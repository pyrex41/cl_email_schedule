;;; frequency-limiter.lisp - Frequency limits checking system

(in-package :email-scheduler.frequency-limiter)

;;; Configuration structure

(defstruct frequency-config
  "Configuration for frequency limiting"
  (max-emails-per-period 5 :type integer)
  (period-days 30 :type integer)
  (exclude-followups-p t :type boolean)
  (priority-based-selection-p t :type boolean))

(defun make-frequency-config-from-json (json-config)
  "Create frequency config from JSON configuration"
  (make-frequency-config
   :max-emails-per-period (getf json-config :max_emails_per_period 5)
   :period-days (getf json-config :period_days 30)
   :exclude-followups-p (getf json-config :exclude_followups t)
   :priority-based-selection-p (getf json-config :priority_based_selection t)))

;;; Phase 1: Contact Email History Analysis

(defun get-contact-email-history (db contact-id days-back)
  "Get recent email history for frequency analysis"
  (sqlite:execute-to-list db
    "SELECT email_type, scheduled_date, status, priority, email_template, campaign_instance_id
     FROM email_schedules 
     WHERE contact_id = ? AND scheduled_date >= date('now', '-? days')
     AND status IN ('sent', 'delivered', 'pre-scheduled', 'scheduled')
     ORDER BY scheduled_date DESC"
    contact-id days-back))

(defun count-emails-in-period (db contact-id config &optional (reference-date (local-time:now)))
  "Count emails for a contact within the frequency period"
  (let* ((period-days (frequency-config-period-days config))
         (cutoff-date (email-scheduler.date-utils:subtract-days reference-date period-days))
         (cutoff-string (email-scheduler.date-utils:format-date cutoff-date)))
    
    (sqlite:execute-single db
      "SELECT COUNT(*) FROM email_schedules 
       WHERE contact_id = ? AND scheduled_date >= ?
       AND status IN ('sent', 'delivered', 'pre-scheduled', 'scheduled')"
      contact-id cutoff-string)))

;;; Phase 2: Follow-up Email Detection

(defun is-followup-email-p (email-type)
  "Check if email type is a follow-up (exempt from frequency limits)"
  (or (stringp email-type) ; String-based check for legacy support
      (when (stringp email-type)
        (alexandria:starts-with-subseq "followup_" email-type))
      (typep email-type 'email-scheduler.domain:follow-up-email)))

(defun is-followup-schedule-p (schedule)
  "Check if a schedule represents a follow-up email"
  (let ((email-type (email-scheduler.domain:schedule-email-type schedule)))
    (is-followup-email-p email-type)))

(defun filter-non-followup-schedules (schedules config)
  "Filter schedules to exclude follow-ups if configured"
  (if (frequency-config-exclude-followups-p config)
      (remove-if #'is-followup-schedule-p schedules)
      schedules))

;;; Phase 3: Priority-Based Email Selection

(defun sort-schedules-by-priority (schedules)
  "Sort schedules by priority (lower number = higher priority)"
  (sort (copy-list schedules)
        (lambda (a b)
          (< (email-scheduler.domain:schedule-priority a)
             (email-scheduler.domain:schedule-priority b)))))

(defun get-schedule-priority-score (schedule)
  "Calculate priority score for a schedule (lower = higher priority)"
  (let ((base-priority (email-scheduler.domain:schedule-priority schedule))
        (email-type (email-scheduler.domain:schedule-email-type schedule)))
    
    ;; Adjust priority based on email type
    (cond
      ;; Anniversary emails have higher priority
      ((typep email-type 'email-scheduler.domain:anniversary-email) 
       (- base-priority 1))
      
      ;; Campaign emails priority depends on campaign type
      ((typep email-type 'email-scheduler.domain:campaign-email)
       (case (email-scheduler.domain:campaign-type email-type)
         (:regulatory_notice (- base-priority 2)) ; Highest priority
         (:rate_increase base-priority)           ; Normal priority
         (:seasonal_promo (+ base-priority 1))   ; Lower priority
         (t base-priority)))
      
      ;; Follow-up emails have lower priority
      ((typep email-type 'email-scheduler.domain:follow-up-email)
       (+ base-priority 2))
      
      (t base-priority))))

(defun select-highest-priority-emails (schedules max-emails)
  "Select the highest priority emails up to the limit"
  (let ((sorted-schedules (sort (copy-list schedules)
                               (lambda (a b)
                                 (< (get-schedule-priority-score a)
                                    (get-schedule-priority-score b))))))
    (subseq sorted-schedules 0 (min max-emails (length sorted-schedules)))))

;;; Phase 4: Contact-Level Frequency Checking

(defun check-contact-frequency-limit (db contact-id proposed-schedules config)
  "Check if adding proposed schedules would exceed frequency limits"
  (let* ((filtered-schedules (filter-non-followup-schedules proposed-schedules config))
         (current-count (count-emails-in-period db contact-id config))
         (new-count (length filtered-schedules))
         (total-count (+ current-count new-count))
         (max-emails (frequency-config-max-emails-per-period config)))
    
    (values 
     (<= total-count max-emails)  ; Within limits?
     total-count                  ; Total count
     max-emails                   ; Limit
     (- total-count max-emails)))) ; Overage amount

(defun apply-frequency-limits-to-contact (db contact-id schedules config)
  "Apply frequency limits to schedules for a single contact"
  (let* ((filtered-schedules (filter-non-followup-schedules schedules config))
         (followup-schedules (remove-if-not #'is-followup-schedule-p schedules)))
    
    (multiple-value-bind (within-limits total-count max-emails overage)
        (check-contact-frequency-limit db contact-id filtered-schedules config)
      
      (if within-limits
          ;; All schedules can be sent
          (values schedules nil 
                  (format nil "Contact ~A: ~A/~A emails (within limit)" 
                          contact-id total-count max-emails))
          
          ;; Need to select subset based on priority
          (let* ((max-new-emails (max 0 (- max-emails 
                                          (count-emails-in-period db contact-id config))))
                 (selected-filtered (if (frequency-config-priority-based-selection-p config)
                                      (select-highest-priority-emails filtered-schedules max-new-emails)
                                      (subseq filtered-schedules 0 (min max-new-emails 
                                                                        (length filtered-schedules)))))
                 (final-schedules (append selected-filtered followup-schedules))
                 (dropped-count (- (length schedules) (length final-schedules))))
            
            (values final-schedules
                    (- (length filtered-schedules) (length selected-filtered))
                    (format nil "Contact ~A: dropped ~A emails (limit ~A/~A exceeded by ~A)" 
                            contact-id dropped-count max-emails total-count overage)))))))

;;; Phase 5: Batch Processing and Integration

(defun group-schedules-by-contact (schedules)
  "Group schedules by contact ID"
  (let ((contact-groups (make-hash-table :test 'equal)))
    (dolist (schedule schedules)
      (let ((contact-id (email-scheduler.domain:schedule-contact-id schedule)))
        (push schedule (gethash contact-id contact-groups))))
    
    ;; Convert to alist for easier processing
    (let ((result '()))
      (maphash (lambda (contact-id contact-schedules)
                 (push (cons contact-id (reverse contact-schedules)) result))
               contact-groups)
      result)))

(defun apply-frequency-limits (db schedules config)
  "Apply frequency limits to a batch of schedules"
  (let* ((freq-config (if (typep config 'frequency-config)
                         config
                         (make-frequency-config-from-json config)))
         (contact-groups (group-schedules-by-contact schedules))
         (final-schedules '())
         (total-dropped 0)
         (processing-log '()))
    
    ;; Process each contact's schedules
    (dolist (contact-group contact-groups)
      (let ((contact-id (car contact-group))
            (contact-schedules (cdr contact-group)))
        
        (multiple-value-bind (allowed-schedules dropped-count log-message)
            (apply-frequency-limits-to-contact db contact-id contact-schedules freq-config)
          
          (setf final-schedules (append allowed-schedules final-schedules))
          (when dropped-count
            (incf total-dropped dropped-count))
          (when log-message
            (push log-message processing-log)))))
    
    (values final-schedules
            total-dropped
            (reverse processing-log))))

;;; Phase 6: Reporting and Validation

(defun generate-frequency-limit-report (original-schedules processed-schedules dropped-count config)
  "Generate a report on frequency limit processing"
  (let* ((freq-config (if (typep config 'frequency-config)
                         config
                         (make-frequency-config-from-json config)))
         (original-contacts (length (remove-duplicates 
                                    (mapcar #'email-scheduler.domain:schedule-contact-id 
                                           original-schedules))))
         (processed-contacts (length (remove-duplicates 
                                     (mapcar #'email-scheduler.domain:schedule-contact-id 
                                            processed-schedules)))))
    
    (list :original-schedules (length original-schedules)
          :processed-schedules (length processed-schedules)
          :dropped-schedules dropped-count
          :original-contacts original-contacts
          :processed-contacts processed-contacts
          :max-emails-per-period (frequency-config-max-emails-per-period freq-config)
          :period-days (frequency-config-period-days freq-config)
          :followups-excluded-p (frequency-config-exclude-followups-p freq-config)
          :drop-rate (if (> (length original-schedules) 0)
                       (* 100.0 (/ dropped-count (length original-schedules)))
                       0.0))))

(defun validate-frequency-limits (db schedules config)
  "Validate that schedules comply with frequency limits"
  (let* ((freq-config (if (typep config 'frequency-config)
                         config
                         (make-frequency-config-from-json config)))
         (contact-groups (group-schedules-by-contact schedules))
         (violations '()))
    
    ;; Check each contact
    (dolist (contact-group contact-groups)
      (let ((contact-id (car contact-group))
            (contact-schedules (cdr contact-group)))
        
        (multiple-value-bind (within-limits total-count max-emails overage)
            (check-contact-frequency-limit db contact-id contact-schedules freq-config)
          
          (unless within-limits
            (push (list :contact-id contact-id
                       :total-emails total-count
                       :max-emails max-emails
                       :overage overage)
                  violations)))))
    
    (if violations
        (values nil violations)
        (values t nil))))

;;; Integration with main scheduler

(defun should-apply-frequency-limits-p (config)
  "Check if frequency limits should be applied based on configuration"
  (and (getf config :max_emails_per_period)
       (> (getf config :max_emails_per_period 0) 0)))

(defun integrate-frequency-limits (db schedules config)
  "Main integration point for frequency limits in scheduling pipeline"
  (if (should-apply-frequency-limits-p config)
      (progn
        (format t "Applying frequency limits to ~A schedules...~%" (length schedules))
        (multiple-value-bind (processed-schedules dropped-count processing-log)
            (apply-frequency-limits db schedules config)
          
          (when (> dropped-count 0)
            (format t "Frequency limits: dropped ~A schedules~%" dropped-count))
          
          ;; Log processing details if configured
          (when (getf config :verbose-logging)
            (dolist (log-entry processing-log)
              (format t "  ~A~%" log-entry)))
          
          processed-schedules))
      
      ;; No frequency limits configured
      schedules))