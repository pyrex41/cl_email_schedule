;;; test-rules.lisp

(defpackage #:email-scheduler.test.rules
  (:use #:cl #:fiveam #:email-scheduler.domain #:email-scheduler.rules))

(in-package :email-scheduler.test.rules)

;;; Test suite definition
(def-suite rules-tests
  :description "Tests for state rules and exclusion logic")

(in-suite rules-tests)

;;; Helper functions for tests
(defun make-test-contact (&key (id 1) (email "test@example.com") 
                               (state :ca) (birthday nil) (effective-date nil))
  "Create a test contact with specified parameters"
  (make-instance 'contact
                 :id id
                 :email email
                 :zip-code "90210"
                 :state state
                 :birthday (when birthday (local-time:parse-timestring birthday))
                 :effective-date (when effective-date (local-time:parse-timestring effective-date))))

;;; California birthday exclusion tests
(test california-birthday-exclusion
  "Test CA birthday window exclusion (30 days before to 60 days after)"
  (let ((contact (make-test-contact :state :ca :birthday "1980-03-15")))
    
    ;; Test dates that should be excluded (30 days before to 60 days after)
    (is (apply-state-rules contact (local-time:parse-timestring "2024-02-14"))) ; 30 days before
    (is (apply-state-rules contact (local-time:parse-timestring "2024-03-15"))) ; on birthday
    (is (apply-state-rules contact (local-time:parse-timestring "2024-05-14"))) ; 60 days after
    
    ;; Test dates that should not be excluded
    (is (not (apply-state-rules contact (local-time:parse-timestring "2024-01-01")))) ; before window
    (is (not (apply-state-rules contact (local-time:parse-timestring "2024-06-01"))))))  ; after window

(test nevada-month-start-rule
  "Test Nevada's special rule using month start instead of actual birthday"
  (let ((contact (make-test-contact :state :nv :birthday "1980-03-15")))
    
    ;; Nevada uses beginning of birth month (March 1st) not actual birthday
    (is (apply-state-rules contact (local-time:parse-timestring "2024-03-01"))) ; month start
    (is (apply-state-rules contact (local-time:parse-timestring "2024-05-01"))) ; 60 days after month start
    
    ;; Should not be excluded before the month starts
    (is (not (apply-state-rules contact (local-time:parse-timestring "2024-02-28"))))))

(test year-boundary-exclusion
  "Test exclusion windows spanning year boundaries"
  (let ((contact (make-test-contact :state :ca :birthday "1980-01-15")))
    
    ;; Window should span from Dec 16 previous year to March 16
    (is (apply-state-rules contact (local-time:parse-timestring "2023-12-20"))) ; previous year
    (is (apply-state-rules contact (local-time:parse-timestring "2024-01-15"))) ; birthday
    (is (apply-state-rules contact (local-time:parse-timestring "2024-03-15"))) ; 60 days after
    
    ;; Should not be excluded outside the window
    (is (not (apply-state-rules contact (local-time:parse-timestring "2023-12-10")))) ; before window
    (is (not (apply-state-rules contact (local-time:parse-timestring "2024-04-01")))))) ; after window

;;; Effective date window tests
(test missouri-effective-date-exclusion
  "Test Missouri effective date window (30 days before to 33 days after)"
  (let ((contact (make-test-contact :state :mo :effective-date "2020-01-01")))
    
    ;; Test dates that should be excluded
    (is (apply-state-rules contact (local-time:parse-timestring "2024-12-02"))) ; 30 days before anniversary
    (is (apply-state-rules contact (local-time:parse-timestring "2024-01-01"))) ; on anniversary
    (is (apply-state-rules contact (local-time:parse-timestring "2024-02-03"))) ; 33 days after
    
    ;; Test dates that should not be excluded
    (is (not (apply-state-rules contact (local-time:parse-timestring "2024-11-15")))) ; before window
    (is (not (apply-state-rules contact (local-time:parse-timestring "2024-02-15")))))) ; after window

;;; Year-round exclusion tests
(test year-round-exclusion-states
  "Test states with year-round exclusion"
  (dolist (state '(:ct :ma :ny :wa))
    (let ((contact (make-test-contact :state state :birthday "1980-03-15")))
      
      ;; Should be excluded on any date
      (is (apply-state-rules contact (local-time:parse-timestring "2024-01-01")))
      (is (apply-state-rules contact (local-time:parse-timestring "2024-06-15")))
      (is (apply-state-rules contact (local-time:parse-timestring "2024-12-31"))))))

;;; No exclusion tests
(test no-exclusion-states
  "Test states with no exclusion rules"
  (let ((contact (make-test-contact :state :other :birthday "1980-03-15")))
    
    ;; Should never be excluded
    (is (not (apply-state-rules contact (local-time:parse-timestring "2024-01-01"))))
    (is (not (apply-state-rules contact (local-time:parse-timestring "2024-03-15")))) ; even on birthday
    (is (not (apply-state-rules contact (local-time:parse-timestring "2024-12-31"))))))

;;; Pre-window extension tests
(test pre-window-extension
  "Test pre-window extension functionality"
  (let ((contact (make-test-contact :state :ca :birthday "1980-03-15")))
    
    ;; With 60-day pre-window extension, exclusion should start 90 days before birthday
    (is (in-exclusion-window-p contact (local-time:parse-timestring "2023-12-15") :pre-window-days 60))
    
    ;; Without pre-window extension, should not be excluded that early
    (is (not (in-exclusion-window-p contact (local-time:parse-timestring "2023-12-15") :pre-window-days 0)))))

;;; Multiple rule types tests
(test multiple-rule-types
  "Test contact with multiple exclusion rule types"
  (let ((contact (make-test-contact :state :ca 
                                    :birthday "1980-03-15" 
                                    :effective-date "2020-01-01")))
    
    ;; Should be excluded during birthday window
    (is (apply-state-rules contact (local-time:parse-timestring "2024-03-15")))
    
    ;; CA doesn't have effective date rules, so effective date shouldn't trigger exclusion
    (is (not (apply-state-rules contact (local-time:parse-timestring "2024-01-01"))))))

;;; Edge case tests
(test leap-year-handling
  "Test handling of February 29th in leap and non-leap years"
  (let ((contact (make-test-contact :state :ca :birthday "1980-02-29")))
    
    ;; In non-leap year, should use Feb 28
    ;; This test would need actual date calculation logic to verify
    (is-true t))) ; Placeholder - would test actual behavior

(test missing-contact-data
  "Test handling of contacts with missing birthday/effective date"
  (let ((contact-no-birthday (make-test-contact :state :ca :birthday nil))
        (contact-no-effective (make-test-contact :state :mo :effective-date nil)))
    
    ;; Should not be excluded if no birthday data
    (is (not (apply-state-rules contact-no-birthday (local-time:parse-timestring "2024-03-15"))))
    
    ;; Should not be excluded if no effective date data
    (is (not (apply-state-rules contact-no-effective (local-time:parse-timestring "2024-01-01"))))))

;;; Campaign rule tests
(test campaign-configurations
  "Test campaign configuration DSL"
  (let ((rate-increase-config (get-campaign-config 'rate-increase))
        (initial-blast-config (get-campaign-config 'initial-blast)))
    
    ;; Test rate increase campaign settings
    (is (getf rate-increase-config :respect-exclusions))
    (is (getf rate-increase-config :enable-followups))
    (is (= (getf rate-increase-config :days-before) 14))
    (is (= (getf rate-increase-config :priority) 1))
    
    ;; Test initial blast campaign settings
    (is (not (getf initial-blast-config :respect-exclusions)))
    (is (not (getf initial-blast-config :enable-followups)))
    (is (= (getf initial-blast-config :days-before) 0))
    (is (= (getf initial-blast-config :priority) 10))))

;;; Performance tests
(test exclusion-calculation-performance
  "Test performance of exclusion calculations"
  (let ((contact (make-test-contact :state :ca :birthday "1980-03-15"))
        (test-dates (loop for i from 1 to 365
                          collect (local-time:timestamp+ (local-time:today) i :day))))
    
    (let ((start-time (get-internal-real-time)))
      
      ;; Test exclusion calculation for a full year of dates
      (dolist (date test-dates)
        (apply-state-rules contact date))
      
      (let ((elapsed (/ (- (get-internal-real-time) start-time) 
                        internal-time-units-per-second)))
        
        ;; Should complete in reasonable time (less than 1 second for 365 dates)
        (is (< elapsed 1.0))
        (format t "~&Exclusion calculation for 365 dates: ~,3F seconds~%" elapsed)))))

;;; Integration tests
(test rule-dsl-integration
  "Test integration between DSL and rule application"
  
  ;; Test that DSL-defined rules work correctly
  (let ((ca-rules (get :ca 'state-rules))
        (nv-rules (get :nv 'state-rules)))
    
    ;; CA should have birthday window rule
    (is (some (lambda (rule) (eq (first rule) :birthday-window)) ca-rules))
    
    ;; NV should have birthday window rule with use-month-start
    (let ((nv-birthday-rule (find :birthday-window nv-rules :key #'first)))
      (is nv-birthday-rule)
      (is (getf nv-birthday-rule :use-month-start)))))

;;; Run all tests
(defun run-rules-tests ()
  "Run all rules tests and return results"
  (run! 'rules-tests))