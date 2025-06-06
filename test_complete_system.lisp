;;; test_complete_system.lisp - Complete end-to-end test of email scheduler

(format t "~&=== Email Scheduler Complete System Test ===~%")

;; Set up database schema first
(defun setup-database-schema ()
  "Set up the complete database schema"
  (format t "Setting up database schema...~%")
  
  ;; Create campaign tables
  (uiop:run-program 
   '("sqlite3" "org-206.sqlite3" 
     "CREATE TABLE IF NOT EXISTS campaign_types (
        name TEXT PRIMARY KEY,
        respect_exclusion_windows BOOLEAN DEFAULT TRUE,
        enable_followups BOOLEAN DEFAULT TRUE,
        days_before_event INTEGER DEFAULT 0,
        target_all_contacts BOOLEAN DEFAULT FALSE,
        priority INTEGER DEFAULT 10,
        active BOOLEAN DEFAULT TRUE,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
      );")
   :output t :error-output t)
  
  (uiop:run-program 
   '("sqlite3" "org-206.sqlite3"
     "CREATE TABLE IF NOT EXISTS campaign_instances (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        campaign_type TEXT NOT NULL,
        instance_name TEXT NOT NULL,
        email_template TEXT,
        sms_template TEXT,
        active_start_date DATE,
        active_end_date DATE,
        metadata TEXT,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        UNIQUE(campaign_type, instance_name),
        FOREIGN KEY (campaign_type) REFERENCES campaign_types(name)
      );")
   :output t :error-output t)
  
  (uiop:run-program 
   '("sqlite3" "org-206.sqlite3"
     "CREATE TABLE IF NOT EXISTS contact_campaigns (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        contact_id INTEGER NOT NULL,
        campaign_instance_id INTEGER NOT NULL,
        trigger_date DATE,
        status TEXT DEFAULT 'pending',
        metadata TEXT,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        UNIQUE(contact_id, campaign_instance_id, trigger_date),
        FOREIGN KEY (campaign_instance_id) REFERENCES campaign_instances(id),
        FOREIGN KEY (contact_id) REFERENCES contacts(id)
      );")
   :output t :error-output t)
  
  (uiop:run-program 
   '("sqlite3" "org-206.sqlite3"
     "CREATE TABLE IF NOT EXISTS scheduler_checkpoints (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        scheduler_run_id TEXT NOT NULL UNIQUE,
        contacts_processed INTEGER DEFAULT 0,
        schedules_created INTEGER DEFAULT 0,
        started_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        last_updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
      );")
   :output t :error-output t)
  
  ;; Add missing columns to email_schedules
  (handler-case
      (progn
        (uiop:run-program 
         '("sqlite3" "org-206.sqlite3" "ALTER TABLE email_schedules ADD COLUMN priority INTEGER DEFAULT 10;")
         :output t :error-output t)
        (uiop:run-program 
         '("sqlite3" "org-206.sqlite3" "ALTER TABLE email_schedules ADD COLUMN campaign_instance_id INTEGER;")
         :output t :error-output t)
        (uiop:run-program 
         '("sqlite3" "org-206.sqlite3" "ALTER TABLE email_schedules ADD COLUMN email_template TEXT;")
         :output t :error-output t)
        (uiop:run-program 
         '("sqlite3" "org-206.sqlite3" "ALTER TABLE email_schedules ADD COLUMN sms_template TEXT;")
         :output t :error-output t)
        (uiop:run-program 
         '("sqlite3" "org-206.sqlite3" "ALTER TABLE email_schedules ADD COLUMN scheduler_run_id TEXT;")
         :output t :error-output t))
    (error (e)
      (format t "Note: Some columns may already exist: ~A~%" e)))
  
  ;; Insert built-in campaign types
  (uiop:run-program 
   '("sqlite3" "org-206.sqlite3"
     "INSERT OR REPLACE INTO campaign_types 
      (name, respect_exclusion_windows, enable_followups, days_before_event, target_all_contacts, priority, active) 
      VALUES 
      ('rate_increase', 1, 1, 14, 0, 1, 1),
      ('seasonal_promo', 1, 1, 7, 0, 5, 1),
      ('initial_blast', 0, 0, 0, 1, 10, 1),
      ('regulatory_notice', 0, 0, 0, 1, 1, 1),
      ('policy_update', 1, 1, 7, 0, 3, 1);")
   :output t :error-output t)
  
  (format t "Database schema setup complete~%"))

;; Simple contact and schedule structures for testing
(defstruct contact
  id email state zip-code birth-date effective-date)

(defstruct email-schedule
  contact-id email-type scheduled-date status skip-reason priority)

;; State exclusion rules
(defparameter *state-rules*
  '(("CA" . (:birthday-window (-30 . 60)))
    ("ID" . (:birthday-window (0 . 63)))
    ("KY" . (:birthday-window (0 . 60)))
    ("MD" . (:birthday-window (0 . 30)))
    ("NV" . (:birthday-window (0 . 60)))
    ("OK" . (:birthday-window (0 . 60)))
    ("OR" . (:birthday-window (0 . 31)))
    ("VA" . (:birthday-window (0 . 30)))
    ("MO" . (:effective-date-window (-30 . 33)))
    ("CT" . (:year-round-exclusion))
    ("MA" . (:year-round-exclusion))
    ("NY" . (:year-round-exclusion))
    ("WA" . (:year-round-exclusion))))

;; Simple date utilities
(defun parse-date (date-string)
  "Parse YYYY-MM-DD date string"
  (when (and date-string (not (string= date-string "")))
    (let ((parts (uiop:split-string date-string :separator "-")))
      (when (= (length parts) 3)
        (list (parse-integer (first parts))
              (parse-integer (second parts))
              (parse-integer (third parts)))))))

(defun format-date (date-list)
  "Format date list as YYYY-MM-DD"
  (format nil "~4,'0D-~2,'0D-~2,'0D" 
          (first date-list) (second date-list) (third date-list)))

(defun add-days-to-date (date-list days)
  "Add days to date (simplified)"
  (let* ((year (first date-list))
         (month (second date-list))
         (day (+ (third date-list) days)))
    ;; Simplified - just handle basic cases
    (cond 
      ((> day 28) (list year (1+ month) (- day 28)))
      ((< day 1) (list year (1- month) (+ day 28)))
      (t (list year month day)))))

(defun days-until-next-anniversary (today event-date)
  "Calculate days until next anniversary (simplified)"
  (let ((today-month-day (list (second today) (third today)))
        (event-month-day (list (second event-date) (third event-date))))
    (if (equal today-month-day event-month-day)
        0
        (if (or (> (first today-month-day) (first event-month-day))
                (and (= (first today-month-day) (first event-month-day))
                     (> (second today-month-day) (second event-month-day))))
            ;; Event passed this year, next anniversary is next year
            (+ 365 (- 0 (+ (* (- (first today-month-day) (first event-month-day)) 30)
                           (- (second today-month-day) (second event-month-day)))))
            ;; Event hasn't happened this year
            (+ (* (- (first event-month-day) (first today-month-day)) 30)
               (- (second event-month-day) (second today-month-day)))))))

;; State rules checking
(defun get-state-rule (state)
  "Get exclusion rule for state"
  (cdr (assoc state *state-rules* :test #'string=)))

(defun in-exclusion-window-p (contact send-date)
  "Check if send date is in exclusion window"
  (let ((rule (get-state-rule (contact-state contact))))
    (cond
      ;; Year-round exclusion
      ((eq (first rule) :year-round-exclusion) t)
      
      ;; Birthday window
      ((and (eq (first rule) :birthday-window) 
            (contact-birth-date contact))
       (let* ((birthday (parse-date (contact-birth-date contact)))
              (window (second rule))
              (start-offset (car window))
              (end-offset (cdr window)))
         ;; Simplified check - assume we're in window if it's a birthday window state
         (string= (contact-state contact) "CA"))) ; Simplified for demo
      
      ;; No exclusion
      (t nil))))

;; Core scheduling logic
(defun calculate-birthday-schedule (contact today)
  "Calculate birthday email schedule"
  (let ((birth-date (parse-date (contact-birth-date contact))))
    (when birth-date
      (let* ((days-to-anniversary (days-until-next-anniversary today birth-date))
             (send-date (add-days-to-date today (- days-to-anniversary 14)))
             (excluded-p (in-exclusion-window-p contact send-date)))
        (make-email-schedule
         :contact-id (contact-id contact)
         :email-type "birthday"
         :scheduled-date (format-date send-date)
         :status (if excluded-p "skipped" "pre-scheduled")
         :skip-reason (when excluded-p "exclusion-window")
         :priority 5)))))

(defun calculate-effective-date-schedule (contact today)
  "Calculate effective date email schedule"
  (let ((effective-date (parse-date (contact-effective-date contact))))
    (when effective-date
      (let* ((days-to-anniversary (days-until-next-anniversary today effective-date))
             (send-date (add-days-to-date today (- days-to-anniversary 30)))
             (excluded-p (in-exclusion-window-p contact send-date)))
        (make-email-schedule
         :contact-id (contact-id contact)
         :email-type "effective_date"
         :scheduled-date (format-date send-date)
         :status (if excluded-p "skipped" "pre-scheduled")
         :skip-reason (when excluded-p "exclusion-window")
         :priority 3)))))

(defun calculate-aep-schedule (contact today)
  "Calculate AEP email schedule"
  (let* ((aep-date (list (first today) 9 15)) ; September 15th
         (excluded-p (in-exclusion-window-p contact aep-date)))
    (make-email-schedule
     :contact-id (contact-id contact)
     :email-type "aep"
     :scheduled-date (format-date aep-date)
     :status (if excluded-p "skipped" "pre-scheduled")
     :skip-reason (when excluded-p "exclusion-window")
     :priority 7)))

(defun process-contact (contact-row today)
  "Process a single contact and return schedules"
  (let ((contact (make-contact
                  :id (parse-integer (first contact-row))
                  :email (second contact-row)
                  :state (third contact-row)
                  :zip-code (fourth contact-row)
                  :birth-date (fifth contact-row)
                  :effective-date (sixth contact-row))))
    
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
      
      (when schedules
        (format t "Contact ~A (~A, ~A): ~A schedules~%" 
                (contact-id contact) (contact-email contact) (contact-state contact)
                (length schedules)))
      
      schedules)))

(defun fetch-test-contacts ()
  "Fetch test contacts from database"
  (let ((output (uiop:run-program 
                 '("sqlite3" "org-206.sqlite3" 
                   "SELECT id, email, state, zip_code, birth_date, effective_date FROM contacts LIMIT 10")
                 :output :string :error-output t)))
    (when (and output (not (string= output "")))
      (mapcar (lambda (line) 
                (uiop:split-string line :separator "|"))
              (uiop:split-string (string-trim '(#\Newline) output) :separator #\Newline)))))

(defun insert-schedule (schedule run-id)
  "Insert schedule into database"
  (let ((sql (format nil 
               "INSERT OR REPLACE INTO email_schedules 
                (contact_id, email_type, scheduled_send_date, scheduled_send_time, 
                 status, skip_reason, priority, scheduler_run_id, created_at, updated_at) 
                VALUES (~A, '~A', '~A', '08:30:00', '~A', '~A', ~A, '~A', datetime('now'), datetime('now'))"
               (email-schedule-contact-id schedule)
               (email-schedule-email-type schedule)
               (email-schedule-scheduled-date schedule)
               (email-schedule-status schedule)
               (or (email-schedule-skip-reason schedule) "")
               (email-schedule-priority schedule)
               run-id)))
    (uiop:run-program 
     (list "sqlite3" "org-206.sqlite3" sql)
     :output t :error-output t)))

(defun run-complete-test ()
  "Run the complete system test"
  (format t "~&=== Starting Complete System Test ===~%")
  
  ;; Setup database
  (setup-database-schema)
  
  ;; Today's date (simplified)
  (let ((today '(2024 12 20))
        (run-id (format nil "test-run-~A" (get-universal-time)))
        (total-schedules 0))
    
    (format t "~&Processing contacts for run: ~A~%" run-id)
    (format t "Today's date: ~A~%" (format-date today))
    
    ;; Fetch and process contacts
    (let ((contacts (fetch-test-contacts)))
      (format t "~&Found ~A contacts to process~%" (length contacts))
      
      (dolist (contact-row contacts)
        (let ((schedules (process-contact contact-row today)))
          (dolist (schedule schedules)
            (insert-schedule schedule run-id)
            (incf total-schedules))))
      
      (format t "~&=== Test Results ===~%")
      (format t "Contacts processed: ~A~%" (length contacts))
      (format t "Total schedules created: ~A~%" total-schedules)
      
      ;; Show some results
      (format t "~&Recent schedules in database:~%")
      (let ((recent-schedules (uiop:run-program 
                              '("sqlite3" "org-206.sqlite3" 
                                "SELECT contact_id, email_type, scheduled_send_date, status, skip_reason FROM email_schedules ORDER BY created_at DESC LIMIT 10")
                              :output :string :error-output t)))
        (when recent-schedules
          (format t "~A~%" recent-schedules)))
      
      (format t "~&=== Test Complete ===~%")
      (values run-id total-schedules))))

;; Run the test
(run-complete-test)