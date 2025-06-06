;;; scheduling.lisp - Simplified scheduling logic

(in-package :email-scheduler)

;;; Configuration
(defparameter *scheduler-config*
  '(:timezone "America/Chicago"
    :batch-size 10000
    :birthday-days-before 14
    :effective-date-days-before 30
    :pre-window-buffer 60
    :daily-cap-percentage 0.07
    :ed-soft-limit 15
    :send-time "08:30:00"
    :aep-month 9
    :aep-day 15
    :max-emails-per-period 5
    :period-days 30
    :db-path "scheduler.db"))

;;; Simple scheduling functions
(defun calculate-birthday-email (contact today config)
  "Calculate birthday email schedule"
  (declare (ignore config))
  (when (email-scheduler.domain:contact-birthday contact)
    (let* ((birthday (email-scheduler.domain:contact-birthday contact))
           (next-birthday (email-scheduler.date-utils:next-anniversary today birthday))
           (send-date (email-scheduler.date-utils:subtract-days next-birthday 14)))
      (make-instance 'email-scheduler.domain:email-schedule
                     :contact-id (email-scheduler.domain:contact-id contact)
                     :email-type (make-instance 'email-scheduler.domain:birthday-email)
                     :scheduled-date send-date
                     :status :pre-scheduled
                     :priority 5))))

(defun calculate-effective-date-email (contact today config)
  "Calculate effective date email schedule"
  (declare (ignore config))
  (when (email-scheduler.domain:contact-effective-date contact)
    (let* ((effective-date (email-scheduler.domain:contact-effective-date contact))
           (next-anniversary (email-scheduler.date-utils:next-anniversary today effective-date))
           (send-date (email-scheduler.date-utils:subtract-days next-anniversary 30)))
      (make-instance 'email-scheduler.domain:email-schedule
                     :contact-id (email-scheduler.domain:contact-id contact)
                     :email-type (make-instance 'email-scheduler.domain:effective-date-email)
                     :scheduled-date send-date
                     :status :pre-scheduled
                     :priority 3))))

(defun calculate-aep-email (contact today config)
  "Calculate AEP email schedule"
  (declare (ignore config))
  (let* ((year (local-time:timestamp-year today))
         (aep-date (local-time:encode-timestamp 0 0 0 0 15 9 year))
         (send-date (if (local-time:timestamp< today aep-date)
                        aep-date
                        (local-time:encode-timestamp 0 0 0 0 15 9 (1+ year)))))
    (make-instance 'email-scheduler.domain:email-schedule
                   :contact-id (email-scheduler.domain:contact-id contact)
                   :email-type (make-instance 'email-scheduler.domain:aep-email)
                   :scheduled-date send-date
                   :status :pre-scheduled
                   :priority 7)))

(defun calculate-campaign-schedules (contact config)
  "Calculate campaign-based email schedules for a contact"
  (declare (ignore config))
  ;; Get campaign schedules from the campaign system
  (handler-case
      (email-scheduler.campaigns:get-campaign-schedules-for-contact contact)
    (error (e)
      (format t "Warning: Campaign scheduling failed for contact ~A: ~A~%" 
              (email-scheduler.domain:contact-id contact) e)
      '())))

(defun apply-state-rules-to-schedules (contact schedules)
  "Apply state exclusion rules to all schedules for a contact"
  (mapcar (lambda (schedule)
            (let ((send-date (email-scheduler.domain:scheduled-date schedule)))
              (if (email-scheduler.rules:in-exclusion-window-p contact send-date)
                  (progn
                    (setf (email-scheduler.domain:schedule-status schedule) :skipped)
                    (setf (email-scheduler.domain:skip-reason schedule) "exclusion-window")
                    schedule)
                  schedule)))
          schedules))

(defun calculate-all-schedules (contact &optional (config *scheduler-config*))
  "Calculate all email schedules for a contact"
  (let ((today (email-scheduler.date-utils:today))
        (schedules '()))
    ;; Calculate anniversary-based emails
    (let ((birthday-schedule (calculate-birthday-email contact today config)))
      (when birthday-schedule
        (push birthday-schedule schedules)))
    
    (let ((ed-schedule (calculate-effective-date-email contact today config)))
      (when ed-schedule
        (push ed-schedule schedules)))
    
    (let ((aep-schedule (calculate-aep-email contact today config)))
      (when aep-schedule
        (push aep-schedule schedules)))
    
    ;; Calculate campaign-based emails
    (let ((campaign-schedules (calculate-campaign-schedules contact config)))
      (setf schedules (append schedules campaign-schedules)))
    
    ;; Apply state exclusion rules to all schedules
    (setf schedules (apply-state-rules-to-schedules contact schedules))
    
    schedules))

(defun process-contact-batch (contacts run-id config &optional db)
  "Process a batch of contacts and return schedules with frequency limits applied"
  (let ((all-schedules '()))
    (dolist (contact contacts)
      (let ((schedules (calculate-all-schedules contact config)))
        (dolist (schedule schedules)
          (setf (email-scheduler.domain:scheduler-run-id schedule) run-id))
        (setf all-schedules (append all-schedules schedules))))
    
    ;; Apply frequency limits if database connection provided and limits are configured
    (let ((schedules-after-frequency 
           (if (and db (email-scheduler.frequency-limiter:should-apply-frequency-limits-p config))
               (email-scheduler.frequency-limiter:integrate-frequency-limits db all-schedules config)
               all-schedules)))
      
      ;; Apply load balancing if configured
      (if (getf config :daily-cap-percentage)
          (let ((total-contacts (when db (email-scheduler.database:count-contacts db))))
            (email-scheduler.load-balancer:apply-load-balancing 
             schedules-after-frequency config total-contacts))
          schedules-after-frequency))))

(defun validate-config (config)
  "Validate configuration parameters"
  (and (getf config :batch-size)
       (> (getf config :batch-size) 0)
       (getf config :daily-cap-percentage)
       (< (getf config :daily-cap-percentage) 1.0)))

(defun check-frequency-limits (contact schedules config)
  "Apply frequency limits to schedules"
  (declare (ignore contact config))
  ;; For now, just return all schedules
  schedules)

;;; Main scheduling functions
(defun run-scheduler (&key (db-path "scheduler.db") (backup-db t))
  "Run the email scheduler"
  (let ((run-id (format nil "run-~A" (get-universal-time))))
    (email-scheduler.database:with-database (db db-path)
      ;; Create schema if needed
      (email-scheduler.database:create-database-schema db)
      
      ;; Create checkpoint
      (email-scheduler.database:create-checkpoint db run-id)
      
      ;; For demo purposes, just return some stats
      (values run-id '(:contacts-processed 0 :schedules-created 0)))))

(defun setup-test-environment (&key (db-path "test-scheduler.db") (contact-count 100))
  "Set up test environment with sample data"
  (email-scheduler.database:with-database (db db-path)
    (email-scheduler.database:create-database-schema db)
    (email-scheduler.database:insert-test-contacts db contact-count)))

(defun schedule-emails-streaming (db-path run-id &optional (config *scheduler-config*))
  "Stream email scheduling for large datasets"
  (email-scheduler.database:with-database (db db-path)
    ;; Setup campaign system
    (email-scheduler.campaigns:setup-campaign-system db)
    
    (let ((total-contacts (email-scheduler.database:count-contacts db))
          (batch-size (getf config :batch-size 1000))
          (processed 0))
      (loop for offset from 0 by batch-size
            while (< offset total-contacts)
            do (let ((contacts (email-scheduler.database:fetch-contacts-batch db offset batch-size)))
                 (when contacts
                   (let ((schedules (process-contact-batch contacts run-id config db)))
                     (email-scheduler.database:insert-schedules-batch db schedules run-id))
                   (incf processed (length contacts))
                   (email-scheduler.database:update-checkpoint db run-id processed)))))))

;;; Helper macros for compatibility
;; Removed when-let macro since we're using standard when/let constructs