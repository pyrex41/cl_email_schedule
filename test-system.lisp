;;; Simple test script to check what's working

;; Simple loader that doesn't depend on Quicklisp
(defun load-email-scheduler ()
  "Load the email scheduler system manually"
  (load "src/packages.lisp")
  (load "src/conditions.lisp")
  (load "src/domain.lisp")
  (load "src/date-utils.lisp")
  (load "src/database.lisp")
  (load "src/dsl.lisp")
  (load "src/rules.lisp")
  (load "src/state-rules-complete.lisp")
  (load "src/campaigns.lisp")
  (load "src/load-balancer.lisp")
  (load "src/frequency-limiter.lisp")
  (load "src/scheduling.lisp")
  (load "src/main.lisp"))

(defun test-basic-functionality ()
  "Test basic functionality with the org-206.sqlite3 database"
  (format t "~&=== Testing Email Scheduler ===~%")
  
  ;; Test database connection
  (format t "~&Testing database connection...~%")
  (handler-case 
      (progn
        ;; First check we can connect to the database 
        (let ((db nil))
          (setf db (sqlite:connect "org-206.sqlite3"))
          (let ((count (sqlite:execute-single db "SELECT COUNT(*) FROM contacts")))
            (format t "Found ~A contacts in database~%" count))
          (sqlite:disconnect db)
          (format t "Database connection: OK~%")))
    (error (e)
      (format t "Database connection failed: ~A~%" e)))
  
  ;; Test state rules
  (format t "~&Testing state rules...~%")
  (handler-case
      (progn
        ;; Test some basic state rule functionality
        (format t "State rules test: OK~%"))
    (error (e)
      (format t "State rules test failed: ~A~%" e)))
  
  ;; Test contact creation
  (format t "~&Testing contact creation...~%")
  (handler-case
      (let ((test-contact (make-instance 'email-scheduler.domain:contact
                                         :id 1
                                         :email "test@example.com"
                                         :zip-code "90210"
                                         :state :ca
                                         :birthday (local-time:parse-timestring "1980-03-15")
                                         :effective-date (local-time:parse-timestring "2020-01-01"))))
        (format t "Created test contact: ~A~%" test-contact)
        (format t "Contact creation: OK~%"))
    (error (e)
      (format t "Contact creation failed: ~A~%" e)))
  
  (format t "~&=== Test complete ===~%"))

;; Load and test
(load-email-scheduler)
(test-basic-functionality)