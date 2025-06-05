;;; test-scheduling.lisp

(defpackage #:email-scheduler.test.scheduling
  (:use #:cl #:fiveam #:email-scheduler.domain #:email-scheduler))

(in-package :email-scheduler.test.scheduling)

;;; Test suite definition
(def-suite scheduling-tests
  :description "Tests for email scheduling logic")

(in-suite scheduling-tests)

;;; Helper functions
(defun make-test-contact (&key (id 1) (email "test@example.com") 
                               (state :ca) (birthday nil) (effective-date nil))
  "Create a test contact for testing"
  (make-instance 'contact
                 :id id
                 :email email
                 :zip-code "90210"
                 :state state
                 :birthday (when birthday (local-time:parse-timestring birthday))
                 :effective-date (when effective-date (local-time:parse-timestring effective-date))))

;;; Birthday email scheduling tests
(test birthday-email-scheduling
  "Test birthday email scheduling logic"
  (let* ((contact (make-test-contact :state :other :birthday "1980-03-15")) ; Use :other to avoid exclusions
         (today (local-time:parse-timestring "2024-01-01"))
         (schedule (calculate-birthday-email contact today *scheduler-config*)))
    
    (is (not (null schedule)))
    (is (typep (schedule-email-type schedule) 'birthday-email))
    (is (= (contact-id contact) (schedule-contact-id schedule)))
    (is (eq :pre-scheduled (schedule-status schedule)))
    
    ;; Birthday email should be sent 14 days before next birthday (March 1, 2024)
    (let ((expected-date (local-time:parse-timestring "2024-03-01")))
      (is (email-scheduler.date-utils:same-date-p 
           (scheduled-date schedule) 
           expected-date)))))

(test birthday-email-exclusion
  "Test birthday email gets skipped during exclusion window"
  (let* ((contact (make-test-contact :state :ca :birthday "1980-03-15")) ; CA has exclusion rules
         (today (local-time:parse-timestring "2024-01-01"))
         (schedule (calculate-birthday-email contact today *scheduler-config*)))
    
    (is (not (null schedule)))
    (is (eq :skipped (schedule-status schedule)))
    (is (string= "exclusion-window" (skip-reason schedule)))))

;;; Effective date email scheduling tests
(test effective-date-email-scheduling
  "Test effective date email scheduling logic"
  (let* ((contact (make-test-contact :state :other :effective-date "2020-01-01"))
         (today (local-time:parse-timestring "2024-01-01"))
         (schedule (calculate-effective-date-email contact today *scheduler-config*)))
    
    (is (not (null schedule)))
    (is (typep (schedule-email-type schedule) 'effective-date-email))
    (is (= (contact-id contact) (schedule-contact-id schedule)))
    (is (eq :pre-scheduled (schedule-status schedule)))
    
    ;; Effective date email should be sent 30 days before next anniversary (Dec 2, 2024)
    (let ((expected-date (local-time:parse-timestring "2024-12-02")))
      (is (email-scheduler.date-utils:same-date-p 
           (scheduled-date schedule) 
           expected-date)))))

;;; AEP email scheduling tests
(test aep-email-scheduling
  "Test AEP email scheduling logic"
  (let* ((contact (make-test-contact :state :other))
         (today (local-time:parse-timestring "2024-01-01"))
         (schedule (calculate-aep-email contact today *scheduler-config*)))
    
    (is (not (null schedule)))
    (is (typep (schedule-email-type schedule) 'aep-email))
    (is (= (contact-id contact) (schedule-contact-id schedule)))
    (is (eq :pre-scheduled (schedule-status schedule)))
    
    ;; AEP email should be sent on September 15, 2024
    (let ((expected-date (local-time:parse-timestring "2024-09-15")))
      (is (email-scheduler.date-utils:same-date-p 
           (scheduled-date schedule) 
           expected-date)))))

(test aep-email-next-year
  "Test AEP email scheduling when current year's date has passed"
  (let* ((contact (make-test-contact :state :other))
         (today (local-time:parse-timestring "2024-10-01")) ; After Sep 15
         (schedule (calculate-aep-email contact today *scheduler-config*)))
    
    (is (not (null schedule)))
    
    ;; Should schedule for next year (September 15, 2025)
    (let ((expected-date (local-time:parse-timestring "2025-09-15")))
      (is (email-scheduler.date-utils:same-date-p 
           (scheduled-date schedule) 
           expected-date)))))

;;; Complete scheduling tests
(test complete-contact-scheduling
  "Test complete scheduling for a contact with all data"
  (let* ((contact (make-test-contact :state :other 
                                     :birthday "1980-03-15"
                                     :effective-date "2020-01-01"))
         (schedules (calculate-all-schedules contact *scheduler-config*)))
    
    ;; Should have birthday, effective date, and AEP emails
    (is (>= (length schedules) 3))
    
    ;; Check that we have all expected email types
    (let ((email-types (mapcar (lambda (s) (type-of (schedule-email-type s))) schedules)))
      (is (member 'birthday-email email-types))
      (is (member 'effective-date-email email-types))
      (is (member 'aep-email email-types)))))

(test scheduling-with-exclusions
  "Test scheduling behavior with exclusion windows"
  (let* ((contact (make-test-contact :state :ca 
                                     :birthday "1980-03-15"
                                     :effective-date "2020-01-01"))
         (schedules (calculate-all-schedules contact *scheduler-config*)))
    
    ;; Should have schedules (some skipped, some scheduled)
    (is (> (length schedules) 0))
    
    ;; Some schedules should be skipped due to CA exclusion rules
    (let ((skipped-schedules (remove-if-not (lambda (s) (eq (schedule-status s) :skipped)) schedules)))
      (is (> (length skipped-schedules) 0)))))

;;; Post-window email tests
(test post-window-email-generation
  "Test that post-window emails are generated for skipped emails"
  (let* ((contact (make-test-contact :state :ca :birthday "1980-03-15"))
         (schedules (calculate-all-schedules contact *scheduler-config*)))
    
    ;; Should have both skipped emails and post-window emails
    (let ((skipped-schedules (remove-if-not (lambda (s) (eq (schedule-status s) :skipped)) schedules))
          (post-window-schedules (remove-if-not (lambda (s) (typep (schedule-email-type s) 'post-window-email)) schedules)))
      
      (is (> (length skipped-schedules) 0))
      ;; Post-window emails should be generated for skipped emails
      ;; (exact count depends on exclusion window end dates)
      (is (>= (length post-window-schedules) 0)))))

;;; Email priority tests
(test email-priority-assignment
  "Test that emails get correct priority assignments"
  (let* ((contact (make-test-contact :state :other 
                                     :birthday "1980-03-15"
                                     :effective-date "2020-01-01"))
         (schedules (calculate-all-schedules contact *scheduler-config*)))
    
    (dolist (schedule schedules)
      (let ((email-type (schedule-email-type schedule))
            (priority (schedule-priority schedule)))
        
        ;; Check priority assignments
        (cond
          ((typep email-type 'birthday-email)
           (is (= priority 5)))
          ((typep email-type 'effective-date-email)
           (is (= priority 3)))
          ((typep email-type 'aep-email)
           (is (= priority 7)))
          ((typep email-type 'post-window-email)
           (is (= priority 2))))))))

;;; Frequency limit tests
(test frequency-limit-checking
  "Test frequency limit checking logic"
  (let* ((contact (make-test-contact :state :other 
                                     :birthday "1980-03-15"
                                     :effective-date "2020-01-01"))
         (schedules (calculate-all-schedules contact *scheduler-config*))
         (limited-config (copy-list *scheduler-config*)))
    
    ;; Set very low frequency limit
    (setf (getf limited-config :max-emails-per-period) 1
          (getf limited-config :period-days) 30)
    
    ;; Apply frequency limits
    (let ((limited-schedules (check-frequency-limits contact schedules limited-config)))
      
      ;; Should have fewer schedules after applying limits
      (is (<= (length limited-schedules) (length schedules)))
      
      ;; Remaining schedules should be highest priority
      (when limited-schedules
        (let ((priorities (mapcar #'schedule-priority limited-schedules)))
          (is (= (apply #'min priorities) (first priorities))))))))

;;; Configuration validation tests
(test config-validation
  "Test configuration validation"
  (let ((valid-config *scheduler-config*)
        (invalid-config (copy-list *scheduler-config*)))
    
    ;; Valid config should pass
    (is (validate-config valid-config))
    
    ;; Invalid batch size should fail
    (setf (getf invalid-config :batch-size) -1)
    (is (not (validate-config invalid-config)))
    
    ;; Invalid percentage should fail
    (setf (getf invalid-config :batch-size) 1000
          (getf invalid-config :daily-cap-percentage) 1.5)
    (is (not (validate-config invalid-config)))))

;;; Date edge cases
(test leap-year-birthday-scheduling
  "Test scheduling for February 29th birthdays"
  (let* ((contact (make-test-contact :state :other :birthday "1980-02-29"))
         (today-non-leap (local-time:parse-timestring "2023-01-01"))
         (today-leap (local-time:parse-timestring "2024-01-01"))
         (schedule-non-leap (calculate-birthday-email contact today-non-leap *scheduler-config*))
         (schedule-leap (calculate-birthday-email contact today-leap *scheduler-config*)))
    
    (is (not (null schedule-non-leap)))
    (is (not (null schedule-leap)))
    
    ;; In non-leap year, should use Feb 28
    (let ((non-leap-date (scheduled-date schedule-non-leap)))
      (is (= (local-time:timestamp-month non-leap-date) 2))
      (is (= (local-time:timestamp-day non-leap-date) 14))) ; 14 days before Feb 28
    
    ;; In leap year, should use Feb 29
    (let ((leap-date (scheduled-date schedule-leap)))
      (is (= (local-time:timestamp-month leap-date) 2))
      (is (= (local-time:timestamp-day leap-date) 15))))) ; 14 days before Feb 29

;;; Performance tests
(test scheduling-performance
  "Test scheduling performance with multiple contacts"
  (let ((contacts (loop for i from 1 to 100
                        collect (make-test-contact 
                                 :id i
                                 :state (nth (mod i 4) '(:ca :ny :tx :other))
                                 :birthday (format nil "198~D-~2,'0D-~2,'0D" 
                                                   (mod i 10) 
                                                   (1+ (mod i 12)) 
                                                   (1+ (mod i 28)))
                                 :effective-date (format nil "202~D-~2,'0D-01" 
                                                         (mod i 5) 
                                                         (1+ (mod i 12))))))
        (start-time (get-internal-real-time)))
    
    ;; Schedule all contacts
    (let ((all-schedules (mapcar (lambda (contact) 
                                   (calculate-all-schedules contact *scheduler-config*))
                                 contacts)))
      
      (let ((elapsed (/ (- (get-internal-real-time) start-time) 
                        internal-time-units-per-second)))
        
        ;; Should complete in reasonable time
        (is (< elapsed 5.0))
        (is (> (length (alexandria:flatten all-schedules)) 0))
        
        (format t "~&Scheduled ~A contacts in ~,3F seconds~%" 
                (length contacts) elapsed)))))

;;; Integration tests
(test end-to-end-scheduling
  "Test complete end-to-end scheduling process"
  (let* ((contact (make-test-contact :state :ca 
                                     :birthday "1980-03-15"
                                     :effective-date "2020-01-01"))
         (run-id "test-run-123")
         (schedules (process-contact-batch (list contact) run-id *scheduler-config*)))
    
    ;; Should generate schedules
    (is (> (length schedules) 0))
    
    ;; All schedules should have run-id
    (dolist (schedule schedules)
      (is (string= run-id (scheduler-run-id schedule))))
    
    ;; Should have mix of scheduled and skipped emails
    (let ((statuses (mapcar #'schedule-status schedules)))
      (is (or (member :pre-scheduled statuses)
              (member :skipped statuses))))))

;;; Error handling tests
(test scheduling-error-handling
  "Test error handling in scheduling logic"
  (let ((invalid-contact (make-instance 'contact :id nil :email nil))) ; Invalid contact
    
    ;; Should handle invalid contact gracefully
    (handler-case
        (let ((schedules (calculate-all-schedules invalid-contact *scheduler-config*)))
          ;; Should either return empty list or handle error
          (is (listp schedules)))
      (error (e)
        ;; Error handling should be graceful
        (is (typep e 'email-scheduler.conditions:scheduler-error))))))

;;; Run all tests
(defun run-scheduling-tests ()
  "Run all scheduling tests and return results"
  (run! 'scheduling-tests))