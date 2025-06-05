;;; load-balancer.lisp - Enhanced load balancing with effective date smoothing

(in-package :email-scheduler.load-balancer)

;;; Configuration and utilities

(defstruct load-balancer-config
  "Configuration for load balancing operations"
  (daily-cap-percentage 0.07 :type float)
  (ed-soft-limit 15 :type integer)
  (ed-smoothing-window-days 5 :type integer)
  (catch-up-spread-days 7 :type integer)
  (overage-threshold 1.2 :type float)
  (total-contacts 0 :type integer))

(defun make-config-from-json (json-config total-contacts)
  "Create load balancer config from JSON configuration"
  (let ((lb-config (getf json-config :load_balancing)))
    (make-load-balancer-config
     :daily-cap-percentage (getf lb-config :daily_send_percentage_cap 0.07)
     :ed-soft-limit (getf lb-config :ed_daily_soft_limit 15)
     :ed-smoothing-window-days (getf lb-config :ed_smoothing_window_days 5)
     :catch-up-spread-days (getf lb-config :catch_up_spread_days 7)
     :overage-threshold (getf lb-config :overage_threshold 1.2)
     :total-contacts total-contacts)))

;;; Phase 1: Daily Volume Calculation

(defun calculate-daily-caps (config)
  "Calculate daily sending limits based on organization size"
  (let ((total-contacts (load-balancer-config-total-contacts config))
        (daily-percentage (load-balancer-config-daily-cap-percentage config))
        (ed-soft-limit (load-balancer-config-ed-soft-limit config)))
    (values 
     (floor (* total-contacts daily-percentage))  ; Overall daily cap
     (min ed-soft-limit (floor (* total-contacts daily-percentage 0.3)))))) ; ED-specific cap

(defun is-effective-date-email-p (schedule)
  "Check if a schedule is an effective date email"
  (typep (email-scheduler.domain:schedule-email-type schedule) 
         'email-scheduler.domain:effective-date-email))

(defun count-emails-by-date (schedules)
  "Count emails grouped by scheduled date"
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (schedule schedules)
      (let ((date-key (email-scheduler.date-utils:format-date 
                       (email-scheduler.domain:scheduled-date schedule))))
        (incf (gethash date-key counts 0))))
    counts))

(defun count-ed-emails-by-date (schedules)
  "Count effective date emails grouped by scheduled date"
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (schedule schedules)
      (when (is-effective-date-email-p schedule)
        (let ((date-key (email-scheduler.date-utils:format-date 
                         (email-scheduler.domain:scheduled-date schedule))))
          (incf (gethash date-key counts 0)))))
    counts))

;;; Phase 2: Deterministic Jitter Implementation

(defun extract-email-type-string (email-type)
  "Extract string representation of email type for jitter calculation"
  (cond
    ((typep email-type 'email-scheduler.domain:birthday-email) "birthday")
    ((typep email-type 'email-scheduler.domain:effective-date-email) "effective_date")
    ((typep email-type 'email-scheduler.domain:aep-email) "aep")
    ((typep email-type 'email-scheduler.domain:campaign-email) 
     (format nil "campaign_~A" (email-scheduler.domain:campaign-type email-type)))
    ((typep email-type 'email-scheduler.domain:post-window-email) "post_window")
    (t "unknown")))

(defun calculate-deterministic-jitter (contact-id email-type event-year window-days)
  "Generate consistent jitter value for email distribution"
  (let* ((email-type-str (extract-email-type-string email-type))
         (hash-input (format nil "~A-~A-~A" contact-id email-type-str event-year))
         (hash-value (sxhash hash-input)))
    ;; Map hash to window: -floor(window/2) to +floor(window/2)
    (let ((half-window (floor window-days 2)))
      (- (mod hash-value window-days) half-window))))

(defun apply-jitter-to-schedule (schedule window-days)
  "Apply deterministic jitter to a single schedule"
  (let* ((contact-id (email-scheduler.domain:schedule-contact-id schedule))
         (email-type (email-scheduler.domain:schedule-email-type schedule))
         (scheduled-date (email-scheduler.domain:scheduled-date schedule))
         (event-year (local-time:timestamp-year scheduled-date))
         (jitter-days (calculate-deterministic-jitter contact-id email-type event-year window-days)))
    
    ;; Apply jitter to scheduled date
    (setf (email-scheduler.domain:scheduled-date schedule)
          (email-scheduler.date-utils:add-days scheduled-date jitter-days))
    schedule))

;;; Phase 3: Effective Date Smoothing

(defun filter-effective-date-emails (schedules)
  "Filter schedules to only effective date emails"
  (remove-if-not #'is-effective-date-email-p schedules))

(defun smooth-effective-date-emails (schedules config)
  "Apply smoothing to clustered effective date emails"
  (let* ((ed-schedules (filter-effective-date-emails schedules))
         (non-ed-schedules (remove-if #'is-effective-date-email-p schedules))
         (window-days (load-balancer-config-ed-smoothing-window-days config)))
    
    ;; Apply jitter to ED emails only
    (let ((smoothed-ed-schedules 
           (mapcar (lambda (schedule) 
                     (apply-jitter-to-schedule schedule window-days))
                   ed-schedules)))
      
      ;; Combine smoothed ED emails with non-ED emails
      (append smoothed-ed-schedules non-ed-schedules))))

;;; Phase 4: Daily Cap Enforcement

(defun find-overflow-dates (email-counts daily-cap)
  "Find dates that exceed daily capacity"
  (let ((overflow-dates '()))
    (maphash (lambda (date count)
               (when (> count daily-cap)
                 (push (list date count (- count daily-cap)) overflow-dates)))
             email-counts)
    (sort overflow-dates (lambda (a b) (> (third a) (third b)))))) ; Sort by overflow amount

(defun find-available-capacity (email-counts daily-cap start-date days-range)
  "Find dates with available capacity within a range"
  (let ((available-dates '()))
    (dotimes (i days-range)
      (let* ((check-date (email-scheduler.date-utils:add-days start-date (- i (floor days-range 2))))
             (date-key (email-scheduler.date-utils:format-date check-date))
             (current-count (gethash date-key email-counts 0))
             (available-capacity (- daily-cap current-count)))
        (when (> available-capacity 0)
          (push (list date-key check-date available-capacity) available-dates))))
    (sort available-dates (lambda (a b) (> (third a) (third b)))))) ; Sort by capacity

(defun redistribute-overflow-emails (schedules overflow-info config)
  "Redistribute emails from overflow dates to available dates"
  (let* ((daily-cap (nth-value 0 (calculate-daily-caps config)))
         (spread-days (load-balancer-config-catch-up-spread-days config))
         (schedules-by-date (make-hash-table :test 'equal)))
    
    ;; Group schedules by date
    (dolist (schedule schedules)
      (let ((date-key (email-scheduler.date-utils:format-date 
                       (email-scheduler.domain:scheduled-date schedule))))
        (push schedule (gethash date-key schedules-by-date))))
    
    ;; Process each overflow date
    (dolist (overflow-item overflow-info)
      (let* ((overflow-date-key (first overflow-item))
             (overflow-count (second overflow-item))
             (excess-count (third overflow-item))
             (schedules-on-date (gethash overflow-date-key schedules-by-date))
             (sorted-schedules (sort (copy-list schedules-on-date)
                                   (lambda (a b) 
                                     (< (email-scheduler.domain:schedule-priority a)
                                        (email-scheduler.domain:schedule-priority b))))))
        
        ;; Keep highest priority emails, redistribute excess
        (let ((emails-to-keep (subseq sorted-schedules 0 (min daily-cap (length sorted-schedules))))
              (emails-to-move (subseq sorted-schedules daily-cap)))
          
          ;; Update schedules list
          (setf (gethash overflow-date-key schedules-by-date) emails-to-keep)
          
          ;; Find available dates and redistribute
          (when emails-to-move
            (let* ((base-date (email-scheduler.date-utils:parse-date overflow-date-key))
                   (email-counts (count-emails-by-date schedules))
                   (available-dates (find-available-capacity email-counts daily-cap base-date spread-days)))
              
              (redistribute-emails-to-available-dates emails-to-move available-dates email-counts))))))
    
    ;; Flatten schedules back to list
    (let ((result '()))
      (maphash (lambda (date schedules-list) 
                 (declare (ignore date))
                 (setf result (append schedules-list result)))
               schedules-by-date)
      result)))

(defun redistribute-emails-to-available-dates (emails-to-move available-dates email-counts)
  "Redistribute emails to available dates with capacity"
  (let ((remaining-emails emails-to-move))
    (dolist (available-date-info available-dates)
      (when (null remaining-emails) (return))
      
      (let* ((date-key (first available-date-info))
             (target-date (second available-date-info))
             (capacity (third available-date-info))
             (emails-to-place (min capacity (length remaining-emails))))
        
        ;; Move emails to this date
        (dotimes (i emails-to-place)
          (let ((email (pop remaining-emails)))
            (setf (email-scheduler.domain:scheduled-date email) target-date)
            (incf (gethash date-key email-counts 0))))))))

;;; Phase 5: Main Load Balancing Pipeline

(defun enforce-daily-caps-enhanced (schedules config)
  "Enforce daily email caps with intelligent redistribution"
  (let* ((daily-cap (nth-value 0 (calculate-daily-caps config)))
         (ed-daily-cap (nth-value 1 (calculate-daily-caps config)))
         (total-counts (count-emails-by-date schedules))
         (ed-counts (count-ed-emails-by-date schedules)))
    
    ;; First handle general overflow
    (let* ((general-overflow (find-overflow-dates total-counts daily-cap))
           (schedules-after-general (if general-overflow
                                     (redistribute-overflow-emails schedules general-overflow config)
                                     schedules)))
      
      ;; Then handle ED-specific overflow if applicable
      (let* ((updated-ed-counts (count-ed-emails-by-date schedules-after-general))
             (ed-overflow (find-overflow-dates updated-ed-counts ed-daily-cap)))
        
        (if ed-overflow
            (redistribute-overflow-emails schedules-after-general ed-overflow config)
            schedules-after-general)))))

(defun apply-enhanced-load-balancing (schedules config)
  "Apply comprehensive load balancing with all features"
  (let* (;; Phase 1: Apply effective date smoothing
         (smoothed-schedules (smooth-effective-date-emails schedules config))
         
         ;; Phase 2: Enforce daily caps with redistribution
         (capped-schedules (enforce-daily-caps-enhanced smoothed-schedules config)))
    
    ;; Return processed schedules
    capped-schedules))

;;; Main API functions

(defun group-by-date (schedules)
  "Group schedules by their scheduled date"
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (schedule schedules)
      (let ((date-key (email-scheduler.date-utils:format-date 
                       (email-scheduler.domain:scheduled-date schedule))))
        (push schedule (gethash date-key groups))))
    ;; Convert hash table to alist
    (let ((result '()))
      (maphash (lambda (date schedules-list)
                 (push (cons date (reverse schedules-list)) result))
               groups)
      result)))

(defun apply-load-balancing (schedules config &optional total-contacts)
  "Apply comprehensive load balancing to schedules"
  (let* ((total-contacts (or total-contacts 
                             (getf config :total-contacts 100000))) ; Default fallback
         (lb-config (make-config-from-json config total-contacts)))
    (apply-enhanced-load-balancing schedules lb-config)))

(defun smooth-effective-dates (schedules config)
  "Apply smoothing to effective date emails (legacy interface)"
  (let* ((total-contacts (getf config :total-contacts 100000))
         (lb-config (make-config-from-json config total-contacts)))
    (smooth-effective-date-emails schedules lb-config)))

(defun enforce-daily-caps (schedules config)
  "Enforce daily email caps (legacy interface)"
  (let* ((total-contacts (getf config :total-contacts 100000))
         (lb-config (make-config-from-json config total-contacts)))
    (enforce-daily-caps-enhanced schedules lb-config)))

(defun calculate-jitter (contact-id email-type event-year &optional (window-days 5))
  "Calculate deterministic jitter for email distribution"
  (calculate-deterministic-jitter contact-id email-type event-year window-days))

;;; Utility functions
(defun count-schedules-by-date (schedules)
  "Count number of schedules per date"
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (schedule schedules)
      (let ((date-key (email-scheduler.date-utils:format-date 
                       (email-scheduler.domain:scheduled-date schedule))))
        (incf (gethash date-key counts 0))))
    counts))

(defun redistribute-schedules (schedules target-date spread-days)
  "Redistribute schedules around a target date"
  (declare (ignore target-date spread-days))
  ;; Simple implementation - just return schedules
  schedules)

;;; Utility and reporting functions

(defun hash-table-values (hash-table)
  "Get all values from a hash table as a list"
  (let ((values '()))
    (maphash (lambda (key value) 
               (declare (ignore key))
               (push value values))
             hash-table)
    values))

(defun generate-load-balancing-report (original-schedules processed-schedules config)
  "Generate a report of load balancing operations"
  (let* ((original-counts (count-emails-by-date original-schedules))
         (processed-counts (count-emails-by-date processed-schedules))
         (total-contacts (getf config :total-contacts 100000))
         (lb-config (make-config-from-json config total-contacts))
         (daily-cap (nth-value 0 (calculate-daily-caps lb-config)))
         (ed-daily-cap (nth-value 1 (calculate-daily-caps lb-config))))
    
    (list :daily-cap daily-cap
          :ed-daily-cap ed-daily-cap
          :original-peak (reduce #'max (hash-table-values original-counts) :initial-value 0)
          :processed-peak (reduce #'max (hash-table-values processed-counts) :initial-value 0)
          :original-total (length original-schedules)
          :processed-total (length processed-schedules)
          :dates-over-cap-before (count-if (lambda (count) (> count daily-cap)) 
                                          (hash-table-values original-counts))
          :dates-over-cap-after (count-if (lambda (count) (> count daily-cap)) 
                                         (hash-table-values processed-counts)))))

(defun validate-load-balancing-constraints (schedules config)
  "Validate that load balancing constraints are satisfied"
  (let* ((total-contacts (getf config :total-contacts 100000))
         (lb-config (make-config-from-json config total-contacts))
         (daily-cap (nth-value 0 (calculate-daily-caps lb-config)))
         (ed-daily-cap (nth-value 1 (calculate-daily-caps lb-config)))
         (total-counts (count-emails-by-date schedules))
         (ed-counts (count-ed-emails-by-date schedules))
         (violations '()))
    
    ;; Check general daily cap violations
    (maphash (lambda (date count)
               (when (> count daily-cap)
                 (push (list :general-overflow date count daily-cap) violations)))
             total-counts)
    
    ;; Check ED-specific cap violations
    (maphash (lambda (date count)
               (when (> count ed-daily-cap)
                 (push (list :ed-overflow date count ed-daily-cap) violations)))
             ed-counts)
    
    (if violations
        (values nil violations)
        (values t nil))))