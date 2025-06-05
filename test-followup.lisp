;;; test-followup.lisp - Test follow-up email scheduling system

(pushnew (truename ".") ql:*local-project-directories*)
(ql:register-local-projects)
(ql:quickload :email-scheduler :silent t)

(format t "~&=== Testing Follow-up Email Scheduling System ===~%")

(handler-case
    (progn
      ;; Test the follow-up system
      (email-scheduler:test-followup-system)
      
      (format t "~&✓ Follow-up email system test completed successfully~%"))
  (error (e)
    (format t "✗ Follow-up system test failed: ~A~%" e)))

(format t "~&=== Follow-up Email Test Complete ===~%")