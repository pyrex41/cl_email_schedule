;;; main.lisp

(in-package :email-scheduler)

;;; Use qualified names to avoid conflicts

;;; Main configuration management
(defun load-config-file (path)
  "Load configuration from JSON file"
  (handler-case
      (let ((config-data (yason:parse (alexandria:read-file-into-string path))))
        (alexandria:plist-hash-table 
         (alexandria:hash-table-plist config-data)))
    (error (e)
      (email-scheduler.conditions:signal-configuration-error path (format nil "Failed to load config: ~A" e)))))

(defun merge-configs (base-config file-config)
  "Merge file configuration with base configuration"
  (let ((merged (copy-list base-config)))
    (loop for (key value) on (alexandria:hash-table-plist file-config) by #'cddr
          do (setf (getf merged (intern (string-upcase key) :keyword)) value))
    merged))

;;; Utility functions
(defun generate-run-id ()
  "Generate a unique run ID"
  (format nil "run-~A" (get-universal-time)))

;;; Main entry points
(defun run-scheduler (&key 
                      (config *scheduler-config*) 
                      (db-path "scheduler.db")
                      (config-file nil)
                      (backup-db t)
                      (create-schema nil))
  "Main entry point for running the email scheduler"
  
  ;; Load additional configuration if specified
  (when config-file
    (let ((file-config (load-config-file config-file)))
      (setf config (merge-configs config file-config))))
  
  ;; Validate configuration
  (unless (validate-config config)
    (email-scheduler.conditions:signal-configuration-error "config" "Invalid scheduler configuration"))
  
  ;; Create database schema if requested
  (when create-schema
    (email-scheduler.database:with-database (db db-path)
      (email-scheduler.database:create-database-schema db)
      (format t "INFO: Database schema created~%")))
  
  ;; Backup database if requested
  (when backup-db
    (email-scheduler.database:backup-database db-path))
  
  (let ((run-id (generate-run-id)))
    (format t "INFO: Starting scheduler run ~A with config: ~S~%" run-id config)
    
    (with-error-handling
      (schedule-emails-streaming db-path run-id :config config)
      
      ;; Get and log statistics
      (email-scheduler.database:with-database (db db-path)
        (let ((stats (email-scheduler.database:get-scheduler-stats db run-id)))
          (format t "INFO: Scheduler run ~A completed successfully. Stats: ~S~%" run-id stats)
          (values run-id stats))))))

(defun setup-test-environment (&key 
                               (db-path "test-scheduler.db")
                               (contact-count 1000))
  "Set up test environment with sample data"
  (format t "INFO: Setting up test environment with ~A contacts~%" contact-count)
  
  (email-scheduler.database:with-database (db db-path)
    ;; Create schema
    (email-scheduler.database:create-database-schema db)
    (format t "INFO: Created database schema~%")
    
    ;; Insert test contacts
    (email-scheduler.database:insert-test-contacts db contact-count)
    (format t "INFO: Inserted ~A test contacts~%" contact-count))
  
  (format t "INFO: Test environment ready at ~A~%" db-path)
  db-path)

(defun run-test-scheduler (&key (contact-count 1000))
  "Run scheduler with test data"
  (let ((db-path (setup-test-environment :contact-count contact-count)))
    (run-scheduler :db-path db-path 
                   :backup-db nil 
                   :create-schema nil)))

;;; Command-line interface
(defun parse-command-line-args (args)
  "Parse command line arguments"
  (let ((config *scheduler-config*)
        (db-path "scheduler.db")
        (config-file nil)
        (test-mode nil)
        (contact-count 1000)
        (setup-only nil))
    
    (loop for arg in args
          do (cond
               ((string= arg "--test")
                (setf test-mode t))
               ((string= arg "--setup")
                (setf setup-only t))
               ((alexandria:starts-with-subseq "--db=" arg)
                (setf db-path (subseq arg 5)))
               ((alexandria:starts-with-subseq "--config=" arg)
                (setf config-file (subseq arg 9)))
               ((alexandria:starts-with-subseq "--contacts=" arg)
                (setf contact-count (parse-integer (subseq arg 11))))
               ((alexandria:starts-with-subseq "--batch-size=" arg)
                (setf (getf config :batch-size) (parse-integer (subseq arg 13))))))
    
    (list :config config
          :db-path db-path
          :config-file config-file
          :test-mode test-mode
          :contact-count contact-count
          :setup-only setup-only)))

(defun main (&optional args)
  "Main entry point for command line usage"
  (let ((parsed-args (parse-command-line-args (or args (uiop:command-line-arguments)))))
    
    ;; Set up logging (using simple format statements)
    
    (cond
      ;; Setup mode
      ((getf parsed-args :setup-only)
       (setup-test-environment 
        :db-path (getf parsed-args :db-path)
        :contact-count (getf parsed-args :contact-count)))
      
      ;; Test mode
      ((getf parsed-args :test-mode)
       (run-test-scheduler :contact-count (getf parsed-args :contact-count)))
      
      ;; Normal mode
      (t
       (run-scheduler 
        :config (getf parsed-args :config)
        :db-path (getf parsed-args :db-path)
        :config-file (getf parsed-args :config-file)
        :create-schema t)))))

;;; Interactive development tools
(defpackage #:email-scheduler.repl
  (:use #:cl #:email-scheduler)
  (:export #:test-contact #:show-rules #:trace-scheduling
           #:explain-exclusion #:preview-schedules
           #:start-repl #:demo))

(in-package :email-scheduler.repl)

(defun test-contact (&key 
                     (email "test@example.com") 
                     (state :ca) 
                     (birthday "1980-03-15") 
                     (effective-date "2020-01-01")
                     (id nil))
  "Create a test contact for REPL experimentation"
  (make-instance 'email-scheduler.domain:contact
                 :id (or id (random 10000))
                 :email email
                 :zip-code "90210"
                 :state state
                 :birthday (when birthday 
                            (local-time:parse-timestring birthday))
                 :effective-date (when effective-date
                                  (local-time:parse-timestring effective-date))))

(defun show-rules (&optional (state :ca))
  "Show exclusion rules for a state"
  (format t "State rules for ~A: [Implementation simplified]~%" state))

(defun show-campaigns (&optional (campaign-name 'rate-increase))
  "Show campaign configuration"
  (format t "Campaign ~A: [Implementation simplified]~%" campaign-name))

(defmacro trace-scheduling (contact)
  "Trace all scheduling decisions for a contact"
  `(let ((*trace-output* *standard-output*))
     (trace email-scheduler:calculate-all-schedules 
            email-scheduler.rules:apply-state-rules
            email-scheduler.rules:in-exclusion-window-p)
     (prog1 (email-scheduler:calculate-all-schedules ,contact)
       (untrace))))

(defun explain-exclusion (contact date-string)
  "Explain why a date is excluded for a contact"
  (declare (ignore contact date-string))
  (format t "~&Date exclusion explanation: [Implementation simplified]~%"))

(defun preview-schedules (contact &optional (config email-scheduler:*scheduler-config*))
  "Preview email schedules for a contact"
  (let ((schedules (email-scheduler:calculate-all-schedules contact config)))
    (format t "~&Contact: ~A (~A)~%" 
            (email-scheduler.domain:contact-email contact)
            (email-scheduler.domain:contact-state contact))
    (if schedules
        (dolist (schedule schedules)
          (format t "  ~A: ~A (~A) - Priority: ~A~%"
                  (type-of (email-scheduler.domain:schedule-email-type schedule))
                  (email-scheduler.date-utils:format-date 
                   (email-scheduler.domain:scheduled-date schedule))
                  (email-scheduler.domain:schedule-status schedule)
                  (email-scheduler.domain:schedule-priority schedule)))
        (format t "  No schedules generated~%"))
    schedules))

(defun demo (&key (state :ca) (show-exclusions t))
  "Run an interactive demo of the scheduler"
  (format t "~&=== Email Scheduler Demo ===~%")
  
  ;; Show state rules
  (when show-exclusions
    (format t "~&State rules for ~A:~%" state)
    (show-rules state)
    (format t "~%"))
  
  ;; Create test contact
  (let ((contact (test-contact :state state)))
    (format t "~&Test contact: ~A~%" contact)
    
    ;; Show birthday exclusion
    (when (email-scheduler.domain:contact-birthday contact)
      (let ((birthday-str (email-scheduler.date-utils:format-date 
                           (email-scheduler.domain:contact-birthday contact))))
        (format t "~&Birthday: ~A~%" birthday-str)
        (explain-exclusion contact birthday-str)))
    
    ;; Preview schedules
    (format t "~&Generated schedules:~%")
    (preview-schedules contact)
    
    ;; Test specific dates
    (format t "~&Testing specific dates:~%")
    (dolist (test-date '("2024-12-25" "2024-07-04" "2024-03-01"))
      (explain-exclusion contact test-date))
    
    contact))

(defun start-repl ()
  "Start an interactive REPL session with useful bindings"
  (format t "~&=== Email Scheduler REPL ===~%")
  (format t "Available functions:~%")
  (format t "  (test-contact) - Create test contact~%")
  (format t "  (show-rules :ca) - Show state rules~%")
  (format t "  (preview-schedules contact) - Preview schedules~%")
  (format t "  (explain-exclusion contact \"2024-03-15\") - Explain exclusion~%")
  (format t "  (demo) - Run interactive demo~%")
  (format t "~%")
  
  ;; Create some useful variables
  (setf *ca-contact* (test-contact :state :ca)
        *ny-contact* (test-contact :state :ny)
        *config* email-scheduler:*scheduler-config*)
  
  (format t "Pre-created contacts: *ca-contact*, *ny-contact*~%")
  (format t "Configuration: *config*~%")
  (format t "~%"))

;;; Benchmarking and performance tools
(defun benchmark-scheduler (&key 
                           (contact-counts '(100 1000 10000))
                           (iterations 3))
  "Benchmark scheduler performance with different contact counts"
  (format t "~&=== Scheduler Benchmark ===~%")
  
  (dolist (count contact-counts)
    (format t "~&Testing with ~A contacts:~%" count)
    
    (let ((times '()))
      (dotimes (i iterations)
        (let ((start-time (get-internal-real-time)))
          
          ;; Run test scheduler
          (email-scheduler:run-test-scheduler :contact-count count)
          
          (let ((end-time (get-internal-real-time))
                (elapsed (/ (- end-time start-time) internal-time-units-per-second)))
            (push elapsed times)
            (format t "  Iteration ~A: ~,2F seconds~%" (1+ i) elapsed))))
      
      (let ((avg-time (/ (reduce #'+ times) (length times))))
        (format t "  Average: ~,2F seconds~%" avg-time)
        (format t "  Rate: ~,0F contacts/second~%" (/ count avg-time)))))
  
  (format t "~&Benchmark complete~%"))

;;; Error simulation for testing condition system
(defun simulate-errors (&key (error-type :database))
  "Simulate various error conditions for testing"
  (case error-type
    (:database
     (email-scheduler.conditions:signal-database-error 
      "SELECT * FROM non_existent_table" 
      "Table doesn't exist"))
    (:invalid-contact
     (email-scheduler.conditions:signal-invalid-contact 
      12345 "Missing required email field"))
    (:configuration
     (email-scheduler.conditions:signal-configuration-error 
      :batch-size "Invalid batch size: -1"))
    (t
     (error "Unknown error type: ~A" error-type))))