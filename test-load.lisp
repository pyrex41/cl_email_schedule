;;; test-load.lisp - Simple loading test

(format t "Testing email scheduler loading...~%")

(pushnew (truename ".") ql:*local-project-directories*)
(ql:register-local-projects)

(handler-case
    (progn
      (ql:quickload :email-scheduler :silent t)
      (format t "✓ System loaded successfully~%"))
  (error (e)
    (format t "✗ Failed to load system: ~A~%" e)))

(format t "Done.~%")