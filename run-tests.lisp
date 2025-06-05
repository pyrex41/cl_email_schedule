;;; run-tests.lisp
;;; Main test runner for the email scheduler

(in-package :cl-user)

;;; Load the system
(ql:quickload :email-scheduler)

;;; Load test files
(load "test/test-rules.lisp")
(load "test/test-scheduling.lisp")
(load "test/test-database.lisp")

;;; Define main test suite
(fiveam:def-suite email-scheduler-tests
  :description "Complete test suite for email scheduler")

(fiveam:def-suite-for email-scheduler-tests
  email-scheduler.test.rules:rules-tests
  email-scheduler.test.scheduling:scheduling-tests
  email-scheduler.test.database:database-tests)

;;; Test runner functions
(defun run-all-tests (&key (verbose t))
  "Run all tests and display results"
  (when verbose
    (format t "~&=== Running Email Scheduler Test Suite ===~%"))
  
  (let ((results (fiveam:run! 'email-scheduler-tests)))
    
    (when verbose
      (format t "~&=== Test Results Summary ===~%")
      (format t "Tests run: ~A~%" (length (fiveam:results results)))
      (format t "Passed: ~A~%" (count-if (lambda (r) (typep r 'fiveam:test-passed)) (fiveam:results results)))
      (format t "Failed: ~A~%" (count-if (lambda (r) (typep r 'fiveam:test-failure)) (fiveam:results results)))
      (format t "Errors: ~A~%" (count-if (lambda (r) (typep r 'fiveam:unexpected-test-failure)) (fiveam:results results))))
    
    results))

(defun run-rules-tests ()
  "Run only rules tests"
  (email-scheduler.test.rules:run-rules-tests))

(defun run-scheduling-tests ()
  "Run only scheduling tests"
  (email-scheduler.test.scheduling:run-scheduling-tests))

(defun run-database-tests ()
  "Run only database tests"
  (email-scheduler.test.database:run-database-tests))

(defun run-performance-tests ()
  "Run performance-focused tests"
  (format t "~&=== Performance Tests ===~%")
  
  ;; Run specific performance tests
  (fiveam:run! 'email-scheduler.test.rules:exclusion-calculation-performance)
  (fiveam:run! 'email-scheduler.test.scheduling:scheduling-performance)
  (fiveam:run! 'email-scheduler.test.database:database-performance))

(defun run-integration-tests ()
  "Run integration tests"
  (format t "~&=== Integration Tests ===~%")
  
  ;; Run end-to-end tests
  (fiveam:run! 'email-scheduler.test.rules:rule-dsl-integration)
  (fiveam:run! 'email-scheduler.test.scheduling:end-to-end-scheduling))

;;; Demo and testing utilities
(defun demo-scheduler ()
  "Run a demo of the scheduler with test data"
  (format t "~&=== Email Scheduler Demo ===~%")
  
  ;; Set up test environment
  (let ((db-path "demo-scheduler.db"))
    (format t "Setting up demo environment...~%")
    
    ;; Create test database
    (email-scheduler:setup-test-environment :db-path db-path :contact-count 100)
    
    ;; Run scheduler
    (format t "Running scheduler...~%")
    (multiple-value-bind (run-id stats)
        (email-scheduler:run-scheduler :db-path db-path :backup-db nil)
      
      (format t "Scheduler completed with run ID: ~A~%" run-id)
      (format t "Statistics: ~S~%" stats))
    
    ;; Start REPL for interactive exploration
    (format t "Starting interactive REPL...~%")
    (email-scheduler.repl:start-repl)
    
    db-path))

(defun benchmark-scheduler ()
  "Run comprehensive benchmarks"
  (email-scheduler.repl:benchmark-scheduler 
   :contact-counts '(100 500 1000 5000)
   :iterations 3))

(defun validate-installation ()
  "Validate that the scheduler installation is working"
  (format t "~&=== Validating Email Scheduler Installation ===~%")
  
  (handler-case
      (progn
        ;; Test basic functionality
        (format t "Testing basic DSL functionality...~%")
        (let ((contact (email-scheduler.repl:test-contact)))
          (email-scheduler.repl:preview-schedules contact))
        
        ;; Test rules
        (format t "Testing state rules...~%")
        (email-scheduler.repl:show-rules :ca)
        
        ;; Test database operations
        (format t "Testing database operations...~%")
        (let ((test-db "validation-test.db"))
          (email-scheduler:setup-test-environment :db-path test-db :contact-count 10)
          (when (probe-file test-db)
            (delete-file test-db)))
        
        (format t "✓ Installation validation successful!~%")
        t)
    (error (e)
      (format t "✗ Installation validation failed: ~A~%" e)
      nil)))

;;; Command line interface
(defun main (&optional args)
  "Main entry point for test runner"
  (let ((command (first args)))
    (cond
      ((or (null command) (string= command "all"))
       (run-all-tests))
      ((string= command "rules")
       (run-rules-tests))
      ((string= command "scheduling")
       (run-scheduling-tests))
      ((string= command "database")
       (run-database-tests))
      ((string= command "performance")
       (run-performance-tests))
      ((string= command "integration")
       (run-integration-tests))
      ((string= command "demo")
       (demo-scheduler))
      ((string= command "benchmark")
       (benchmark-scheduler))
      ((string= command "validate")
       (validate-installation))
      (t
       (format t "Usage: sbcl --load run-tests.lisp --eval \"(main '(\"COMMAND\"))\"~%")
       (format t "Commands: all, rules, scheduling, database, performance, integration, demo, benchmark, validate~%")))))

;;; Auto-run tests if this file is loaded directly
(when (and (boundp '*load-pathname*) *load-pathname*)
  (format t "~&Email Scheduler Test Suite Loaded~%")
  (format t "Available commands:~%")
  (format t "  (run-all-tests) - Run complete test suite~%")
  (format t "  (demo-scheduler) - Run interactive demo~%")
  (format t "  (validate-installation) - Validate installation~%")
  (format t "  (benchmark-scheduler) - Run performance benchmarks~%")
  (format t "~%"))