;;; database.lisp - Simplified database operations using cl-sqlite

(in-package :email-scheduler.database)

;;; Use qualified names instead of importing to avoid name conflicts

;;; Database connection macros
(defmacro with-database ((db path) &body body)
  "Establish database connection and ensure cleanup"
  `(let ((,db (sqlite:connect ,path)))
     (unwind-protect
          (progn ,@body)
       (when ,db
         (sqlite:disconnect ,db)))))

(defmacro with-transaction ((db) &body body)
  "Execute body within a database transaction"
  `(progn
     (sqlite:execute-non-query ,db "BEGIN IMMEDIATE")
     (handler-case
         (prog1 (progn ,@body)
           (sqlite:execute-non-query ,db "COMMIT"))
       (error (e)
         (sqlite:execute-non-query ,db "ROLLBACK")
         (error "Transaction failed: ~A" e)))))

;;; Schema creation
(defun create-database-schema (db)
  "Create database tables"
  (sqlite:execute-non-query db "
    CREATE TABLE IF NOT EXISTS contacts (
      id INTEGER PRIMARY KEY,
      email TEXT NOT NULL,
      zip_code TEXT,
      state TEXT NOT NULL,
      birthday TEXT,
      effective_date TEXT
    )")
  
  (sqlite:execute-non-query db "
    CREATE TABLE IF NOT EXISTS email_schedules (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      contact_id INTEGER NOT NULL,
      email_type TEXT NOT NULL,
      scheduled_date TEXT NOT NULL,
      scheduled_time TEXT DEFAULT '08:30:00',
      status TEXT NOT NULL DEFAULT 'pre-scheduled',
      skip_reason TEXT,
      priority INTEGER DEFAULT 10,
      campaign_instance_id INTEGER,
      email_template TEXT,
      sms_template TEXT,
      scheduler_run_id TEXT NOT NULL,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      actual_send_datetime DATETIME,
      UNIQUE(contact_id, email_type, scheduled_date),
      FOREIGN KEY (contact_id) REFERENCES contacts(id),
      FOREIGN KEY (campaign_instance_id) REFERENCES campaign_instances(id)
    )")
  
  (sqlite:execute-non-query db "
    CREATE TABLE IF NOT EXISTS scheduler_checkpoints (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      scheduler_run_id TEXT NOT NULL UNIQUE,
      contacts_processed INTEGER DEFAULT 0,
      schedules_created INTEGER DEFAULT 0,
      started_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      last_updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )")
  
  ;; Campaign system tables
  (sqlite:execute-non-query db "
    CREATE TABLE IF NOT EXISTS campaign_types (
      name TEXT PRIMARY KEY,
      respect_exclusion_windows BOOLEAN DEFAULT TRUE,
      enable_followups BOOLEAN DEFAULT TRUE,
      days_before_event INTEGER DEFAULT 0,
      target_all_contacts BOOLEAN DEFAULT FALSE,
      priority INTEGER DEFAULT 10,
      active BOOLEAN DEFAULT TRUE,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )")
  
  (sqlite:execute-non-query db "
    CREATE TABLE IF NOT EXISTS campaign_instances (
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
    )")
  
  (sqlite:execute-non-query db "
    CREATE TABLE IF NOT EXISTS contact_campaigns (
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
    )")
  
  (sqlite:execute-non-query db "
    CREATE TABLE IF NOT EXISTS campaign_change_log (
      id INTEGER PRIMARY KEY,
      campaign_instance_id INTEGER NOT NULL,
      field_changed TEXT NOT NULL,
      old_value TEXT,
      new_value TEXT,
      changed_at DATETIME NOT NULL,
      changed_by TEXT,
      requires_rescheduling BOOLEAN DEFAULT TRUE,
      FOREIGN KEY (campaign_instance_id) REFERENCES campaign_instances(id)
    )")
  
  ;; Create indexes
  (sqlite:execute-non-query db "CREATE INDEX IF NOT EXISTS idx_contacts_state ON contacts(state)")
  (sqlite:execute-non-query db "CREATE INDEX IF NOT EXISTS idx_contacts_state_birthday ON contacts(state, birthday)")
  (sqlite:execute-non-query db "CREATE INDEX IF NOT EXISTS idx_contacts_state_effective ON contacts(state, effective_date)")
  (sqlite:execute-non-query db "CREATE INDEX IF NOT EXISTS idx_schedules_date ON email_schedules(scheduled_date)")
  (sqlite:execute-non-query db "CREATE INDEX IF NOT EXISTS idx_schedules_run ON email_schedules(scheduler_run_id)")
  (sqlite:execute-non-query db "CREATE INDEX IF NOT EXISTS idx_schedules_lookup ON email_schedules(contact_id, email_type, scheduled_date)")
  (sqlite:execute-non-query db "CREATE INDEX IF NOT EXISTS idx_schedules_status_date ON email_schedules(status, scheduled_date)")
  (sqlite:execute-non-query db "CREATE INDEX IF NOT EXISTS idx_campaigns_active ON campaign_instances(active_start_date, active_end_date)")
  (sqlite:execute-non-query db "CREATE INDEX IF NOT EXISTS idx_contact_campaigns_lookup ON contact_campaigns(contact_id, campaign_instance_id)"))

;;; Contact operations
(defun insert-test-contacts (db count)
  "Insert test contacts for testing"
  (let ((states '("ca" "ny" "tx" "fl" "wa" "or" "nv" "id" "other")))
    (dotimes (i count)
      (let ((state (nth (mod i (length states)) states))
            (email (format nil "test~D@example.com" (1+ i)))
            (birthday (format nil "198~D-~2,'0D-~2,'0D" 
                             (mod i 10) 
                             (1+ (mod i 12)) 
                             (1+ (mod i 28))))
            (effective-date (format nil "202~D-~2,'0D-01" 
                                   (mod i 5) 
                                   (1+ (mod i 12)))))
        (sqlite:execute-non-query db 
          "INSERT INTO contacts (id, email, zip_code, state, birthday, effective_date) VALUES (?, ?, ?, ?, ?, ?)"
          (1+ i) email "12345" state birthday effective-date)))))

(defun fetch-contacts-batch (db offset limit)
  "Fetch a batch of contacts from database"
  (mapcar (lambda (row)
            (make-instance 'email-scheduler.domain:contact
                          :id (first row)
                          :email (second row)
                          :zip-code (third row)
                          :state (intern (string-upcase (fourth row)) :keyword)
                          :birthday (when (fifth row) 
                                     (local-time:parse-timestring (fifth row)))
                          :effective-date (when (sixth row)
                                           (local-time:parse-timestring (sixth row)))))
          (sqlite:execute-to-list db 
            "SELECT id, email, zip_code, state, birthday, effective_date FROM contacts LIMIT ? OFFSET ?"
            limit offset)))

(defun count-contacts (db)
  "Count total contacts in database"
  (sqlite:execute-single db "SELECT COUNT(*) FROM contacts"))

;;; Schedule operations
(defun insert-schedules-batch (db schedules run-id)
  "Insert batch of email schedules"
  (dolist (schedule schedules)
    (sqlite:execute-non-query db
      "INSERT INTO email_schedules (contact_id, email_type, scheduled_date, status, skip_reason, priority, scheduler_run_id) VALUES (?, ?, ?, ?, ?, ?, ?)"
      (email-scheduler.domain:schedule-contact-id schedule)
      (string-downcase (symbol-name (type-of (email-scheduler.domain:schedule-email-type schedule))))
      (email-scheduler.date-utils:format-date (email-scheduler.domain:scheduled-date schedule))
      (string-downcase (symbol-name (email-scheduler.domain:schedule-status schedule)))
      (email-scheduler.domain:skip-reason schedule)
      (email-scheduler.domain:schedule-priority schedule)
      run-id)))

(defun count-schedules (db &key run-id status)
  "Count schedules with optional filters"
  (cond
    ((and run-id status)
     (sqlite:execute-single db 
       "SELECT COUNT(*) FROM email_schedules WHERE scheduler_run_id = ? AND status = ?"
       run-id status))
    (run-id
     (sqlite:execute-single db 
       "SELECT COUNT(*) FROM email_schedules WHERE scheduler_run_id = ?"
       run-id))
    (status
     (sqlite:execute-single db 
       "SELECT COUNT(*) FROM email_schedules WHERE status = ?"
       status))
    (t
     (sqlite:execute-single db "SELECT COUNT(*) FROM email_schedules"))))

;;; Checkpoint operations
(defun create-checkpoint (db run-id)
  "Create a new scheduler checkpoint"
  (sqlite:execute-non-query db
    "INSERT INTO scheduler_checkpoints (scheduler_run_id) VALUES (?)"
    run-id))

(defun update-checkpoint (db run-id contacts-processed)
  "Update checkpoint with progress"
  (sqlite:execute-non-query db
    "UPDATE scheduler_checkpoints SET contacts_processed = ?, last_updated_at = CURRENT_TIMESTAMP WHERE scheduler_run_id = ?"
    contacts-processed run-id))

;;; Statistics
(defun get-scheduler-stats (db run-id)
  "Get statistics for a scheduler run"
  (let ((checkpoint (sqlite:execute-to-list db
                      "SELECT contacts_processed FROM scheduler_checkpoints WHERE scheduler_run_id = ?"
                      run-id))
        (schedule-counts (sqlite:execute-to-list db
                          "SELECT status, COUNT(*) FROM email_schedules WHERE scheduler_run_id = ? GROUP BY status"
                          run-id)))
    (list :contacts-processed (if checkpoint (first (first checkpoint)) 0)
          :schedules-created (reduce #'+ schedule-counts :key #'second :initial-value 0)
          :pre-scheduled (or (second (find "pre-scheduled" schedule-counts :key #'first :test #'string=)) 0)
          :skipped (or (second (find "skipped" schedule-counts :key #'first :test #'string=)) 0))))

;;; Backup operations
(defun backup-database (source-path)
  "Create a backup of the database"
  (let ((backup-path (format nil "~A.backup.~A" 
                            source-path 
                            (local-time:format-timestring nil (local-time:now) :format local-time:+asctime-format+))))
    ;; Simple file copy for SQLite
    (alexandria:copy-file source-path backup-path)
    backup-path))

;;; Simplified query generation (without SXQL for now)
(defun generate-fetch-contacts-query (offset limit)
  "Generate simple SQL string for fetching contacts"
  (format nil "SELECT id, email, zip_code, state, birthday, effective_date FROM contacts LIMIT ~D OFFSET ~D" 
          limit offset))

(defun generate-insert-schedule-query (schedule run-id)
  "Generate simple SQL string for inserting schedule"
  (declare (ignore schedule run-id))
  "INSERT INTO email_schedules (contact_id, email_type, scheduled_date, status, skip_reason, priority, scheduler_run_id) VALUES (?, ?, ?, ?, ?, ?, ?)")

;;; Campaign database operations
(defun insert-campaign-type (db campaign-type)
  "Insert a campaign type into the database"
  (sqlite:execute-non-query db
    "INSERT OR REPLACE INTO campaign_types 
     (name, respect_exclusion_windows, enable_followups, days_before_event, 
      target_all_contacts, priority, active) VALUES (?, ?, ?, ?, ?, ?, ?)"
    (campaign-type-name campaign-type)
    (campaign-type-respect-exclusion-windows campaign-type)
    (campaign-type-enable-followups campaign-type)
    (campaign-type-days-before-event campaign-type)
    (campaign-type-target-all-contacts campaign-type)
    (campaign-type-priority campaign-type)
    (campaign-type-active campaign-type)))

(defun insert-campaign-instance (db campaign-instance)
  "Insert a campaign instance into the database"
  ;; For now, create a simple version that accepts a property list
  (if (listp campaign-instance)
      ;; Handle property list input
      (sqlite:execute-non-query db
        "INSERT INTO campaign_instances 
         (campaign_type, instance_name, email_template, sms_template, 
          active_start_date, active_end_date, metadata) VALUES (?, ?, ?, ?, ?, ?, ?)"
        (getf campaign-instance :campaign-type)
        (getf campaign-instance :instance-name)
        (getf campaign-instance :email-template)
        (getf campaign-instance :sms-template)
        (getf campaign-instance :active-start-date)
        (getf campaign-instance :active-end-date)
        (getf campaign-instance :metadata))
      ;; Handle object input (implement later when accessors are fixed)
      (error "Object-based campaign instance insertion not yet implemented")))

(defun get-active-campaign-instances-from-db (db &optional (date (local-time:now)))
  "Get all currently active campaign instances from database"
  (let ((date-str (local-time:format-timestring nil date :format '(:year "-" (:month 2) "-" (:day 2)))))
    (mapcar (lambda (row)
              (make-instance 'campaign-instance
                             :id (first row)
                             :campaign-type-name (second row)
                             :instance-name (third row)
                             :email-template (fourth row)
                             :sms-template (fifth row)
                             :active-start-date (when (sixth row) 
                                                  (local-time:parse-timestring (sixth row)))
                             :active-end-date (when (seventh row)
                                               (local-time:parse-timestring (seventh row)))
                             :metadata (eighth row)))
            (sqlite:execute-to-list db
              "SELECT id, campaign_type, instance_name, email_template, sms_template, 
                      active_start_date, active_end_date, metadata 
               FROM campaign_instances 
               WHERE (active_start_date IS NULL OR active_start_date <= ?) 
                 AND (active_end_date IS NULL OR active_end_date >= ?)"
              date-str date-str))))

(defun insert-contact-campaign (db contact-id campaign-instance-id trigger-date &key (status "pending") metadata)
  "Insert a contact-campaign association"
  (sqlite:execute-non-query db
    "INSERT OR IGNORE INTO contact_campaigns 
     (contact_id, campaign_instance_id, trigger_date, status, metadata) VALUES (?, ?, ?, ?, ?)"
    contact-id
    campaign-instance-id
    (when trigger-date
      (local-time:format-timestring nil trigger-date :format '(:year "-" (:month 2) "-" (:day 2))))
    status
    metadata))

(defun get-contact-campaigns (db contact-id)
  "Get all campaign associations for a contact"
  (sqlite:execute-to-list db
    "SELECT campaign_instance_id, trigger_date, status, metadata 
     FROM contact_campaigns WHERE contact_id = ?"
    contact-id))

(defun populate-builtin-campaign-types (db)
  "Populate the database with built-in campaign types"
  ;; This function inserts the campaign types from the campaigns.lisp registry
  ;; For now, we'll insert them directly
  (sqlite:execute-non-query db
    "INSERT OR REPLACE INTO campaign_types 
     (name, respect_exclusion_windows, enable_followups, days_before_event, 
      target_all_contacts, priority, active) VALUES 
     ('rate_increase', 1, 1, 14, 0, 1, 1),
     ('seasonal_promo', 1, 1, 7, 0, 5, 1),
     ('initial_blast', 0, 0, 0, 1, 10, 1),
     ('regulatory_notice', 0, 0, 0, 1, 1, 1),
     ('policy_update', 1, 1, 7, 0, 3, 1)"))