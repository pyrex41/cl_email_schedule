;;; demo.lisp - Simple demo without test dependencies

;; Load the system
(pushnew (truename ".") ql:*local-project-directories*)
(ql:register-local-projects)
(ql:quickload :email-scheduler)

;; Create a simple demo
(defun demo-scheduler ()
  "Run a simplified demo of the email scheduler"
  (format t "~&=== Email Scheduler Demo ===~%")
  
  ;; Test basic functionality
  (format t "~&1. Testing configuration...~%")
  (format t "   Config loaded: ~A~%" (not (null email-scheduler:*scheduler-config*)))
  
  ;; Test contact creation
  (format t "~&2. Creating test contact...~%")
  (let ((contact (make-instance 'email-scheduler.domain:contact
                               :id 1
                               :email "demo@example.com"
                               :zip-code "90210" 
                               :state :other
                               :birthday (local-time:parse-timestring "1980-03-15")
                               :effective-date (local-time:parse-timestring "2020-01-01"))))
    (format t "   Contact created: ~A (~A)~%" 
            (email-scheduler.domain:contact-email contact)
            (email-scheduler.domain:contact-state contact))
    
    ;; Test scheduling
    (format t "~&3. Testing scheduling logic...~%")
    (let ((schedules (email-scheduler:calculate-all-schedules contact)))
      (format t "   Generated ~A email schedules~%" (length schedules))
      (dolist (schedule schedules)
        (format t "     - ~A: ~A (Priority: ~A)~%"
                (type-of (email-scheduler.domain:schedule-email-type schedule))
                (email-scheduler.date-utils:format-date 
                 (email-scheduler.domain:scheduled-date schedule))
                (email-scheduler.domain:schedule-priority schedule))))
    
    ;; Test database setup
    (format t "~&4. Testing database setup...~%")
    (handler-case
        (progn
          (email-scheduler:setup-test-environment :db-path "demo.db" :contact-count 5)
          (format t "   Database setup successful~%")
          (when (probe-file "demo.db")
            (delete-file "demo.db")
            (format t "   Demo database cleaned up~%")))
      (error (e)
        (format t "   Database setup failed: ~A~%" e)))
    
    ;; Show REPL commands
    (format t "~&5. Available REPL functions:~%")
    (format t "   (email-scheduler.repl:test-contact) - Create test contact~%")
    (format t "   (email-scheduler.repl:preview-schedules contact) - Preview schedules~%")
    (format t "   (email-scheduler.repl:show-rules :ca) - Show state rules~%")
    
    (format t "~&Demo completed successfully!~%")))

;; Run the demo
(demo-scheduler)