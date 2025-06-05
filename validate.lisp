;;; validate.lisp - Simple validation without test dependencies

;; Load the system
(pushnew (truename ".") ql:*local-project-directories*)
(ql:register-local-projects)

(format t "~&=== Validating Email Scheduler Installation ===~%")

(handler-case
    (progn
      ;; Test system loading
      (format t "Testing system loading...~%")
      (ql:quickload :email-scheduler)
      (format t "✓ System loaded successfully~%")
      
      ;; Test basic functionality
      (format t "Testing basic functionality...~%")
      (eval '(let ((contact (make-instance 'email-scheduler.domain:contact
                                          :id 1
                                          :email "test@example.com"
                                          :zip-code "12345"
                                          :state :other
                                          :birthday (local-time:parse-timestring "1980-03-15")
                                          :effective-date (local-time:parse-timestring "2020-01-01"))))
               (email-scheduler:calculate-all-schedules contact)))
      (format t "✓ Core scheduling logic works~%")
      
      ;; Test database operations
      (format t "Testing database operations...~%")
      (eval '(let ((test-db "validation-test.db"))
               (email-scheduler:setup-test-environment :db-path test-db :contact-count 3)
               (when (probe-file test-db)
                 (delete-file test-db))))
      (format t "✓ Database operations work~%")
      
      (format t "~&✓ Installation validation successful!~%")
      t)
  (error (e)
    (format t "✗ Installation validation failed: ~A~%" e)
    nil))