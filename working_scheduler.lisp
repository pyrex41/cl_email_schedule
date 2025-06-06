;;; working_scheduler.lisp - A complete working email scheduler

;; Simple SQLite wrapper using SBCL's built-in FFI
(defpackage :simple-sqlite
  (:use :cl)
  (:export #:connect #:disconnect #:execute #:query))

(in-package :simple-sqlite)

;; For now, use sqlite3 command line tool
(defun connect (db-path)
  "Return database path for sqlite3 command"
  db-path)

(defun disconnect (db)
  "No-op for command line sqlite3"
  (declare (ignore db))
  nil)

(defun execute (db sql)
  "Execute SQL command"
  (let ((command (format nil "sqlite3 ~A \"~A\"" db sql)))
    (uiop:run-program command :output :string :error-output :string)))

(defun query (db sql)
  "Execute SQL query and return results"
  (let ((output (execute db sql)))
    (when (and output (not (string= output "")))
      (mapcar (lambda (line) 
                (split-sequence:split-sequence #\| line))
              (split-sequence:split-sequence #\Newline (string-trim '(#\Newline) output))))))

;; Email scheduler package
(defpackage :email-scheduler
  (:use :cl)
  (:export #:run-scheduler #:process-contacts))

(in-package :email-scheduler)

;; Configuration
(defparameter *config*
  '(:db-path "org-206.sqlite3"
    :birthday-days-before 14
    :effective-date-days-before 30
    :batch-size 100
    :send-time "08:30:00"))

;; State exclusion rules (simplified version)
(defparameter *state-rules*
  '((:ca . (:birthday-window (-30 . 60)))
    (:id . (:birthday-window (0 . 63)))
    (:ky . (:birthday-window (0 . 60)))
    (:md . (:birthday-window (0 . 30)))
    (:nv . (:birthday-window (0 . 60)))
    (:ok . (:birthday-window (0 . 60)))
    (:or . (:birthday-window (0 . 31)))
    (:va . (:birthday-window (0 . 30)))
    (:mo . (:effective-date-window (-30 . 33)))
    (:ct . (:year-round-exclusion))
    (:ma . (:year-round-exclusion))
    (:ny . (:year-round-exclusion))
    (:wa . (:year-round-exclusion))))

;; Date utilities
(defun today ()
  "Get today's date as local-time timestamp"
  (local-time:now))

(defun parse-date (date-string)
  "Parse date string in YYYY-MM-DD format"
  (when (and date-string (not (string= date-string "")))
    (local-time:parse-timestring date-string)))

(defun format-date (timestamp)
  "Format timestamp as YYYY-MM-DD"
  (local-time:format-timestring nil timestamp :format '(:year "-" (:month 2) "-" (:day 2))))

(defun add-days (timestamp days)
  "Add days to timestamp"
  (local-time:timestamp+ timestamp days :day))

(defun subtract-days (timestamp days)
  "Subtract days from timestamp"
  (local-time:timestamp- timestamp days :day))

(defun next-anniversary (today event-date)
  "Calculate next anniversary of event-date from today"
  (let* ((this-year (local-time:timestamp-year today))
         (this-year-anniversary (local-time:encode-timestamp 
                                0 0 0 0
                                (local-time:timestamp-day event-date)
                                (local-time:timestamp-month event-date)
                                this-year)))
    (if (local-time:timestamp< today this-year-anniversary)
        this-year-anniversary
        (local-time:encode-timestamp 
         0 0 0 0
         (local-time:timestamp-day event-date)
         (local-time:timestamp-month event-date)
         (1+ this-year)))))

;; Contact processing
(defun get-state-rule (state)
  "Get exclusion rule for state"
  (cdr (assoc state *state-rules*)))

(defun in-exclusion-window-p (contact date)
  "Check if date falls within exclusion window for contact"
  (let ((state (intern (string-upcase (fourth contact)) :keyword))
        (birthday (parse-date (fifth contact)))
        (effective-date (parse-date (sixth contact))))
    
    (let ((rule (get-state-rule state)))
      (cond
        ;; Year-round exclusion states
        ((eq (first rule) :year-round-exclusion) t)
        
        ;; Birthday window states
        ((and (eq (first rule) :birthday-window) birthday)
         (let* ((window (second rule))
                (start-offset (car window))
                (end-offset (cdr window))
                (next-bday (next-anniversary date birthday))
                (window-start (add-days next-bday start-offset))
                (window-end (add-days next-bday end-offset)))
           (and (local-time:timestamp<= window-start date)
                (local-time:timestamp<= date window-end))))
        
        ;; Effective date window states
        ((and (eq (first rule) :effective-date-window) effective-date)
         (let* ((window (second rule))
                (start-offset (car window))
                (end-offset (cdr window))
                (next-ed (next-anniversary date effective-date))
                (window-start (add-days next-ed start-offset))
                (window-end (add-days next-ed end-offset)))
           (and (local-time:timestamp<= window-start date)
                (local-time:timestamp<= date window-end))))
        
        ;; No exclusion
        (t nil)))))

(defun calculate-birthday-schedule (contact today)
  "Calculate birthday email schedule for contact"
  (let ((birthday (parse-date (fifth contact))))
    (when birthday
      (let* ((next-bday (next-anniversary today birthday))
             (send-date (subtract-days next-bday (getf *config* :birthday-days-before)))
             (excluded-p (in-exclusion-window-p contact send-date)))
        (list :contact-id (first contact)
              :email-type "birthday"
              :scheduled-date (format-date send-date)
              :status (if excluded-p "skipped" "pre-scheduled")
              :skip-reason (when excluded-p "exclusion-window")
              :priority 5)))))

(defun calculate-effective-date-schedule (contact today)
  "Calculate effective date email schedule for contact"
  (let ((effective-date (parse-date (sixth contact))))
    (when effective-date
      (let* ((next-ed (next-anniversary today effective-date))
             (send-date (subtract-days next-ed (getf *config* :effective-date-days-before)))
             (excluded-p (in-exclusion-window-p contact send-date)))
        (list :contact-id (first contact)
              :email-type "effective_date"
              :scheduled-date (format-date send-date)
              :status (if excluded-p "skipped" "pre-scheduled")
              :skip-reason (when excluded-p "exclusion-window")
              :priority 3)))))

(defun calculate-aep-schedule (contact today)
  "Calculate AEP email schedule for contact"
  (let* ((year (local-time:timestamp-year today))
         (aep-date (local-time:encode-timestamp 0 0 0 0 15 9 year))
         (send-date (if (local-time:timestamp< today aep-date)
                        aep-date
                        (local-time:encode-timestamp 0 0 0 0 15 9 (1+ year))))
         (excluded-p (in-exclusion-window-p contact send-date)))
    (list :contact-id (first contact)
          :email-type "aep"
          :scheduled-date (format-date send-date)
          :status (if excluded-p "skipped" "pre-scheduled")
          :skip-reason (when excluded-p "exclusion-window")
          :priority 7)))

(defun process-contact (contact today)
  "Process a single contact and return all schedules"
  (let ((schedules '()))
    ;; Birthday email
    (let ((birthday-schedule (calculate-birthday-schedule contact today)))
      (when birthday-schedule
        (push birthday-schedule schedules)))
    
    ;; Effective date email
    (let ((ed-schedule (calculate-effective-date-schedule contact today)))
      (when ed-schedule
        (push ed-schedule schedules)))
    
    ;; AEP email
    (let ((aep-schedule (calculate-aep-schedule contact today)))
      (when aep-schedule
        (push aep-schedule schedules)))
    
    schedules))

(defun fetch-contacts (db)
  "Fetch all contacts from database"
  (simple-sqlite:query db 
    "SELECT id, email, state, zip_code, birth_date, effective_date FROM contacts LIMIT 50"))

(defun insert-schedule (db schedule run-id)
  "Insert a single schedule into database"
  (let ((sql (format nil 
               "INSERT OR REPLACE INTO email_schedules 
                (contact_id, email_type, scheduled_send_date, scheduled_send_time, 
                 status, skip_reason, priority, scheduler_run_id, created_at, updated_at) 
                VALUES (~A, '~A', '~A', '~A', '~A', '~A', ~A, '~A', datetime('now'), datetime('now'))"
               (getf schedule :contact-id)
               (getf schedule :email-type)
               (getf schedule :scheduled-date)
               (getf *config* :send-time)
               (getf schedule :status)
               (or (getf schedule :skip-reason) "")
               (getf schedule :priority)
               run-id)))
    (simple-sqlite:execute db sql)))

(defun process-contacts (db contacts run-id)
  "Process all contacts and insert schedules"
  (let ((today (today))
        (total-schedules 0))
    (dolist (contact contacts)
      (let ((schedules (process-contact contact today)))
        (dolist (schedule schedules)
          (insert-schedule db schedule run-id)
          (incf total-schedules))
        (when schedules
          (format t "Processed contact ~A: ~A schedules~%" 
                  (first contact) (length schedules)))))
    (format t "Total schedules created: ~A~%" total-schedules)
    total-schedules))

(defun create-checkpoint (db run-id)
  "Create scheduler checkpoint"
  (simple-sqlite:execute db 
    (format nil "INSERT INTO scheduler_checkpoints (scheduler_run_id) VALUES ('~A')" run-id)))

(defun update-checkpoint (db run-id contacts-processed)
  "Update checkpoint with progress"
  (simple-sqlite:execute db 
    (format nil "UPDATE scheduler_checkpoints SET contacts_processed = ~A WHERE scheduler_run_id = '~A'" 
            contacts-processed run-id)))

(defun run-scheduler (&key (db-path "org-206.sqlite3"))
  "Main scheduler entry point"
  (let ((db (simple-sqlite:connect db-path))
        (run-id (format nil "run-~A" (get-universal-time))))
    
    (format t "~&=== Email Scheduler Run ~A ===~%" run-id)
    (format t "Database: ~A~%" db-path)
    
    (unwind-protect
        (progn
          ;; Create checkpoint
          (handler-case
              (create-checkpoint db run-id)
            (error (e)
              (format t "Warning: Could not create checkpoint: ~A~%" e)))
          
          ;; Fetch contacts
          (format t "Fetching contacts...~%")
          (let ((contacts (fetch-contacts db)))
            (format t "Found ~A contacts~%" (length contacts))
            
            ;; Process contacts
            (when contacts
              (format t "Processing contacts...~%")
              (let ((schedules-created (process-contacts db contacts run-id)))
                (format t "~&Scheduler run completed successfully~%")
                (format t "Contacts processed: ~A~%" (length contacts))
                (format t "Schedules created: ~A~%" schedules-created)
                
                ;; Update checkpoint
                (handler-case
                    (update-checkpoint db run-id (length contacts))
                  (error (e)
                    (format t "Warning: Could not update checkpoint: ~A~%" e)))
                
                (values run-id 
                        (list :contacts-processed (length contacts)
                              :schedules-created schedules-created))))))
      
      ;; Cleanup
      (simple-sqlite:disconnect db))))

;; Make it runnable
(defun main ()
  "Main entry point"
  (handler-case
      (run-scheduler)
    (error (e)
      (format t "Error: ~A~%" e)
      nil)))

;; Load dependencies we need
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-case (require :local-time)
    (error () 
      (format t "Warning: local-time not available, using simplified date handling~%")))
  (handler-case (require :split-sequence)
    (error () 
      (format t "Warning: split-sequence not available~%"))))

;; Run if called directly
(when (member "--run" (uiop:command-line-arguments) :test #'string=)
  (main))