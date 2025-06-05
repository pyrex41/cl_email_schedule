;;; test-state-rules.lisp - Test comprehensive state rules implementation

(pushnew (truename ".") ql:*local-project-directories*)
(ql:register-local-projects)
(ql:quickload :email-scheduler :silent t)

(format t "~&=== Testing Complete State Rules Implementation ===~%")

(handler-case
    (progn
      ;; Test state rule coverage
      (format t "~&Testing state rule coverage...~%")
      (if (email-scheduler.rules:test-state-rule-coverage)
          (format t "✓ State rule coverage test passed~%")
          (format t "✗ State rule coverage test failed~%"))
      
      ;; Test specific state rules
      (format t "~&Testing specific state rules...~%")
      
      ;; Test birthday window states
      (let ((ca-rule-type (email-scheduler.rules:get-state-rule-type :ca))
            (ca-params (email-scheduler.rules:get-state-rule-params :ca)))
        (format t "CA rule type: ~A, params: ~A~%" ca-rule-type ca-params)
        (if (and (eq ca-rule-type :birthday-window)
                 (= (getf ca-params :before) 30)
                 (= (getf ca-params :after) 60))
            (format t "✓ CA birthday window rule correct~%")
            (format t "✗ CA birthday window rule incorrect~%")))
      
      ;; Test year-round exclusion states
      (let ((ny-rule-type (email-scheduler.rules:get-state-rule-type :ny)))
        (if (eq ny-rule-type :year-round-exclusion)
            (format t "✓ NY year-round exclusion rule correct~%")
            (format t "✗ NY year-round exclusion rule incorrect~%")))
      
      ;; Test no-exclusion states
      (let ((tx-rule-type (email-scheduler.rules:get-state-rule-type :tx)))
        (if (eq tx-rule-type :no-exclusion)
            (format t "✓ TX no-exclusion rule correct~%")
            (format t "✗ TX no-exclusion rule incorrect~%")))
      
      ;; Test Nevada's special month-start rule
      (let ((nv-params (email-scheduler.rules:get-state-rule-params :nv)))
        (if (getf nv-params :use-month-start)
            (format t "✓ NV month-start rule correct~%")
            (format t "✗ NV month-start rule missing~%")))
      
      ;; Test Missouri's effective date rule
      (let ((mo-rule-type (email-scheduler.rules:get-state-rule-type :mo))
            (mo-params (email-scheduler.rules:get-state-rule-params :mo)))
        (if (and (eq mo-rule-type :effective-date-window)
                 (= (getf mo-params :before) 30)
                 (= (getf mo-params :after) 33))
            (format t "✓ MO effective date rule correct~%")
            (format t "✗ MO effective date rule incorrect~%")))
      
      ;; Test categorization functions
      (format t "~&Testing categorization functions...~%")
      (let ((birthday-states (email-scheduler.rules:get-birthday-window-states))
            (exclusion-states (email-scheduler.rules:get-year-round-exclusion-states))
            (no-exclusion-states (email-scheduler.rules:get-no-exclusion-states)))
        (format t "Birthday window states: ~A (~A states)~%" (length birthday-states) birthday-states)
        (format t "Year-round exclusion states: ~A (~A states)~%" (length exclusion-states) exclusion-states)
        (format t "No exclusion states: ~A states~%" (length no-exclusion-states))
        
        ;; Verify expected counts
        (if (= (length birthday-states) 8)
            (format t "✓ Birthday window states count correct~%")
            (format t "✗ Birthday window states count incorrect: expected 8, got ~A~%" (length birthday-states)))
        
        (if (= (length exclusion-states) 4)
            (format t "✓ Year-round exclusion states count correct~%")
            (format t "✗ Year-round exclusion states count incorrect: expected 4, got ~A~%" (length exclusion-states)))
        
        (if (>= (length no-exclusion-states) 38)
            (format t "✓ No exclusion states count reasonable~%")
            (format t "✗ No exclusion states count too low: expected >=38, got ~A~%" (length no-exclusion-states))))
      
      ;; Show summary (optional - comment out for cleaner output)
      ;; (email-scheduler.rules:show-all-state-rules)
      
      (format t "~&✓ State rules implementation test completed successfully~%"))
  (error (e)
    (format t "✗ State rules test failed: ~A~%" e)))

(format t "~&=== State Rules Test Complete ===~%")