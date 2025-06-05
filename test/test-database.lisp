;;; test-database.lisp

(defpackage #:email-scheduler.test.database
  (:use #:cl #:fiveam #:email-scheduler.domain #:email-scheduler.database))

(in-package :email-scheduler.test.database)

;;; Test suite definition
(def-suite database-tests
  :description "Tests for database operations")

(in-suite database-tests)

;;; Test database path
(defparameter *test-db-path* "test-scheduler.db")

;;; Setup and teardown
(defun setup-test-database ()
  "Set up test database with schema"
  (when (probe-file *test-db-path*)
    (delete-file *test-db-path*))
  
  (with-database (db *test-db-path*)
    (create-database-schema db)))

(defun teardown-test-database ()
  "Clean up test database"
  (when (probe-file *test-db-path*)
    (delete-file *test-db-path*)))

;;; Database connection tests
(test database-connection
  "Test database connection and disconnection"
  (setup-test-database)
  
  ;; Test connection
  (with-database (db *test-db-path*)
    (is (not (null db)))
    
    ;; Test simple query
    (let ((result (dbi:execute (dbi:prepare db "SELECT 1 as test"))))
      (is (= (getf (dbi:fetch result) :test) 1))))
  
  (teardown-test-database))

;;; Schema creation tests
(test schema-creation
  "Test database schema creation"
  (setup-test-database)
  
  (with-database (db *test-db-path*)
    ;; Check that required tables exist
    (let ((tables '("contacts" "email_schedules" "campaign_types" 
                    "campaign_instances" "contact_campaigns" "scheduler_checkpoints")))
      (dolist (table tables)
        (let* ((query (format nil "SELECT name FROM sqlite_master WHERE type='table' AND name='~A'" table))
               (result (dbi:execute (dbi:prepare db query))))
          (is (not (null (dbi:fetch result))))))))
  
  (teardown-test-database))

;;; Contact insertion and retrieval tests
(test contact-operations
  "Test contact insertion and retrieval"
  (setup-test-database)
  
  (with-database (db *test-db-path*)
    ;; Insert test contacts
    (insert-test-contacts db 10)
    
    ;; Check contact count
    (is (= (count-contacts db) 10))
    
    ;; Fetch contacts batch
    (let ((contacts (fetch-contacts-batch db 0 5)))
      (is (= (length contacts) 5))
      
      ;; Check contact structure
      (let ((contact (first contacts)))
        (is (not (null (contact-id contact))))
        (is (not (null (contact-email contact))))
        (is (not (null (contact-state contact)))))))
  
  (teardown-test-database))

;;; Schedule insertion tests
(test schedule-operations
  "Test email schedule insertion and retrieval"
  (setup-test-database)
  
  (with-database (db *test-db-path*)
    ;; Insert test contacts
    (insert-test-contacts db 5)
    
    ;; Create test schedules
    (let ((schedules (list
                      (make-instance 'email-schedule
                                     :contact-id 1
                                     :email-type (make-instance 'birthday-email)
                                     :scheduled-date (local-time:today)
                                     :status :pre-scheduled
                                     :priority 5)
                      (make-instance 'email-schedule
                                     :contact-id 2
                                     :email-type (make-instance 'effective-date-email)
                                     :scheduled-date (local-time:today)
                                     :status :skipped
                                     :skip-reason "exclusion-window"
                                     :priority 3)))
          (run-id "test-run-123"))
      
      ;; Insert schedules
      (insert-schedules-batch db schedules run-id)
      
      ;; Check schedule count
      (is (= (count-schedules db :run-id run-id) 2))
      (is (= (count-schedules db :run-id run-id :status "pre-scheduled") 1))
      (is (= (count-schedules db :run-id run-id :status "skipped") 1))))
  
  (teardown-test-database))

;;; Transaction tests
(test transaction-handling
  "Test database transaction handling"
  (setup-test-database)
  
  (with-database (db *test-db-path*)
    ;; Test successful transaction
    (with-transaction (db)
      (insert-test-contacts db 3))
    
    (is (= (count-contacts db) 3))
    
    ;; Test failed transaction (should rollback)
    (handler-case
        (with-transaction (db)
          (insert-test-contacts db 2)
          (error "Intentional error"))
      (error (e)
        ;; Transaction should have rolled back
        (is (= (count-contacts db) 3)))))
  
  (teardown-test-database))

;;; Checkpoint tests
(test checkpoint-operations
  "Test scheduler checkpoint operations"
  (setup-test-database)
  
  (with-database (db *test-db-path*)
    (let ((run-id "test-checkpoint-run"))
      
      ;; Create checkpoint
      (create-checkpoint db run-id)
      
      ;; Update checkpoint
      (update-checkpoint db run-id 100)
      
      ;; Verify checkpoint exists
      (let* ((query "SELECT * FROM scheduler_checkpoints WHERE scheduler_run_id = ?")
             (stmt (dbi:prepare db query))
             (result (dbi:execute stmt run-id))
             (row (dbi:fetch result)))
        
        (is (not (null row)))
        (is (string= (getf row :scheduler_run_id) run-id))
        (is (= (getf row :contacts_processed) 100)))))
  
  (teardown-test-database))

;;; Batch processing tests
(test batch-processing
  "Test batch processing of large datasets"
  (setup-test-database)
  
  (with-database (db *test-db-path*)
    ;; Insert large number of contacts
    (insert-test-contacts db 1000)
    
    ;; Test batch fetching
    (let ((total-fetched 0)
          (batch-size 100))
      
      (loop for offset from 0 by batch-size
            for contacts = (fetch-contacts-batch db offset batch-size)
            while contacts
            do (incf total-fetched (length contacts)))
      
      (is (= total-fetched 1000))))
  
  (teardown-test-database))

;;; Performance tests
(test database-performance
  "Test database operation performance"
  (setup-test-database)
  
  (with-database (db *test-db-path*)
    (let ((start-time (get-internal-real-time)))
      
      ;; Insert large batch of contacts
      (insert-test-contacts db 5000)
      
      (let ((insert-time (/ (- (get-internal-real-time) start-time) 
                            internal-time-units-per-second)))
        
        ;; Should complete in reasonable time
        (is (< insert-time 10.0))
        
        (format t "~&Inserted 5000 contacts in ~,3F seconds~%" insert-time))
      
      ;; Test batch retrieval performance
      (setf start-time (get-internal-real-time))
      
      (let ((all-contacts '()))
        (loop for offset from 0 by 1000
              for contacts = (fetch-contacts-batch db offset 1000)
              while contacts
              do (setf all-contacts (append all-contacts contacts)))
        
        (let ((fetch-time (/ (- (get-internal-real-time) start-time) 
                             internal-time-units-per-second)))
          
          (is (< fetch-time 5.0))
          (is (= (length all-contacts) 5000))
          
          (format t "~&Fetched 5000 contacts in ~,3F seconds~%" fetch-time)))))
  
  (teardown-test-database))

;;; Error handling tests
(test database-error-handling
  "Test database error handling"
  (setup-test-database)
  
  (with-database (db *test-db-path*)
    ;; Test handling of invalid SQL
    (handler-case
        (dbi:execute (dbi:prepare db "SELECT * FROM non_existent_table"))
      (error (e)
        (is (not (null e)))))
    
    ;; Test handling of constraint violations
    (handler-case
        (progn
          (insert-test-contacts db 1)
          ;; Try to insert duplicate ID
          (let* ((query "INSERT INTO contacts (id, email, state) VALUES (1, 'duplicate@test.com', 'ca')")
                 (stmt (dbi:prepare db query)))
            (dbi:execute stmt)))
      (error (e)
        (is (not (null e))))))
  
  (teardown-test-database))

;;; SQL generation tests
(test sql-generation
  "Test SQL query generation using SXQL"
  
  ;; Test contact fetch query
  (let ((query (generate-fetch-contacts-query 100 50)))
    (is (not (null query)))
    (let ((sql (sxql:yield query)))
      (is (search "SELECT" sql))
      (is (search "FROM contacts" sql))
      (is (search "LIMIT 50" sql))
      (is (search "OFFSET 100" sql))))
  
  ;; Test schedule insert query
  (let* ((schedule (make-instance 'email-schedule
                                  :contact-id 123
                                  :email-type (make-instance 'birthday-email)
                                  :scheduled-date (local-time:today)
                                  :status :pre-scheduled
                                  :priority 5))
         (query (generate-insert-schedule-query schedule "test-run")))
    
    (is (not (null query)))
    (let ((sql (sxql:yield query)))
      (is (search "INSERT INTO email_schedules" sql))
      (is (search "123" sql))
      (is (search "test-run" sql)))))

;;; Data integrity tests
(test data-integrity
  "Test data integrity and constraints"
  (setup-test-database)
  
  (with-database (db *test-db-path*)
    ;; Insert contacts and schedules with foreign key relationships
    (insert-test-contacts db 5)
    
    (let ((schedule (make-instance 'email-schedule
                                   :contact-id 1
                                   :email-type (make-instance 'birthday-email)
                                   :scheduled-date (local-time:today)
                                   :status :pre-scheduled
                                   :priority 5)))
      
      ;; Should succeed with valid contact ID
      (insert-schedules-batch db (list schedule) "test-run")
      (is (= (count-schedules db) 1))
      
      ;; Test unique constraints
      (handler-case
          (insert-schedules-batch db (list schedule) "test-run-2")
        (error (e)
          ;; Should fail due to unique constraint
          (is (not (null e)))))))
  
  (teardown-test-database))

;;; Backup and recovery tests
(test backup-operations
  "Test database backup operations"
  (setup-test-database)
  
  (with-database (db *test-db-path*)
    (insert-test-contacts db 10))
  
  ;; Create backup
  (let ((backup-path (backup-database *test-db-path*)))
    (is (probe-file backup-path))
    
    ;; Verify backup contains data
    (with-database (backup-db backup-path)
      (is (= (count-contacts backup-db) 10)))
    
    ;; Clean up backup
    (delete-file backup-path))
  
  (teardown-test-database))

;;; Statistics tests
(test statistics-operations
  "Test statistics and monitoring operations"
  (setup-test-database)
  
  (with-database (db *test-db-path*)
    (let ((run-id "stats-test-run"))
      
      ;; Insert test data
      (insert-test-contacts db 20)
      
      (let ((schedules (list
                        (make-instance 'email-schedule
                                       :contact-id 1
                                       :email-type (make-instance 'birthday-email)
                                       :scheduled-date (local-time:today)
                                       :status :pre-scheduled
                                       :priority 5)
                        (make-instance 'email-schedule
                                       :contact-id 2
                                       :email-type (make-instance 'effective-date-email)
                                       :scheduled-date (local-time:today)
                                       :status :skipped
                                       :skip-reason "exclusion-window"
                                       :priority 3))))
        
        (insert-schedules-batch db schedules run-id)
        
        ;; Get statistics
        (let ((stats (get-scheduler-stats db run-id)))
          (is (= (getf stats :contacts-processed) 20))
          (is (= (getf stats :schedules-created) 2))
          (is (= (getf stats :pre-scheduled) 1))
          (is (= (getf stats :skipped) 1))))))
  
  (teardown-test-database))

;;; Run all tests
(defun run-database-tests ()
  "Run all database tests and return results"
  (run! 'database-tests))