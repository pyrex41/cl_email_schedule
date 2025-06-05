# Common Lisp Email Scheduler DSL Implementation Prompt

## Context

You are implementing a sophisticated email scheduling system in Common Lisp based on the provided business logic documentation. The system must handle complex date calculations, state-based exclusion rules, campaign management, and scale to process up to 3 million contacts efficiently. Leverage Lisp's macro system to create an expressive DSL that makes the business rules clear and modifiable.

## Primary Objectives

1. Create a powerful DSL using macros for expressing scheduling rules
2. Implement CLOS-based domain model with multiple dispatch
3. Use the condition system for robust error handling
4. Build streaming processing for memory efficiency at scale
5. Leverage Lisp's dynamic nature for runtime rule modification

## Technical Requirements

### Core Libraries and Setup

```lisp
;;; email-scheduler.asd
(defsystem "email-scheduler"
  :description "Email scheduling system with DSL"
  :version "1.0.0"
  :depends-on (#:cl-dbi          ; Database interface
               #:sxql            ; SQL DSL
               #:cl-sqlite      ; SQLite driver
               #:local-time     ; Time/date handling
               #:cl-ppcre       ; Regex for parsing
               #:yason          ; JSON parsing
               #:log4cl         ; Logging
               #:bordeaux-threads ; Threading
               #:lparallel      ; Parallel processing
               #:alexandria     ; Utilities
               #:trivial-types  ; Type definitions
               #:fiveam         ; Testing
               #:cl-mock)       ; Mocking
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "conditions" :depends-on ("packages"))
                 (:file "domain" :depends-on ("packages"))
                 (:file "dsl" :depends-on ("domain"))
                 (:file "rules" :depends-on ("dsl"))
                 (:file "scheduling" :depends-on ("rules"))
                 (:file "database" :depends-on ("domain"))
                 (:file "load-balancer" :depends-on ("scheduling"))
                 (:file "main" :depends-on ("scheduling" "database"))))))
```

### Package Structure

```lisp
;;; packages.lisp
(defpackage #:email-scheduler.domain
  (:use #:cl)
  (:export #:contact #:email-schedule #:campaign
           #:state #:email-type #:schedule-status))

(defpackage #:email-scheduler.dsl
  (:use #:cl)
  (:export #:defrule #:defstate #:defcampaign
           #:birthday-window #:effective-date-window
           #:year-round-exclusion #:no-exclusion
           #:schedule-email #:skip-email))

(defpackage #:email-scheduler.rules
  (:use #:cl #:email-scheduler.dsl)
  (:export #:apply-state-rules #:calculate-exclusion-window))

(defpackage #:email-scheduler
  (:use #:cl)
  (:export #:run-scheduler #:configure #:process-contacts))
```

## Implementation Guidelines

### 1. Domain Model (domain.lisp)

```lisp
(in-package :email-scheduler.domain)

;;; Define states as symbols with properties
(deftype us-state ()
  '(member :ca :ct :id :ky :ma :md :mo :nv :ny :ok :or :va :wa :other))

;;; Email type hierarchy using CLOS
(defclass email-type ()
  ((priority :initarg :priority :accessor priority :initform 10)
   (template :initarg :template :accessor template)))

(defclass anniversary-email (email-type) ())

(defclass birthday-email (anniversary-email)
  ((days-before :initform 14 :reader days-before)))

(defclass effective-date-email (anniversary-email)
  ((days-before :initform 30 :reader days-before)))

(defclass aep-email (anniversary-email)
  ((send-month :initform 9 :reader send-month)
   (send-day :initform 15 :reader send-day)))

(defclass post-window-email (anniversary-email) ())

(defclass campaign-email (email-type)
  ((campaign-type :initarg :campaign-type :accessor campaign-type)
   (instance-id :initarg :instance-id :accessor instance-id)
   (respect-exclusions-p :initarg :respect-exclusions-p :accessor respect-exclusions-p)
   (days-before-event :initarg :days-before-event :accessor days-before-event)))

;;; Contact class
(defclass contact ()
  ((id :initarg :id :reader contact-id)
   (email :initarg :email :reader contact-email)
   (zip-code :initarg :zip-code :reader contact-zip)
   (state :initarg :state :accessor contact-state :type (or null us-state))
   (birthday :initarg :birthday :accessor contact-birthday :type (or null local-time:timestamp))
   (effective-date :initarg :effective-date :accessor contact-effective-date :type (or null local-time:timestamp))))

;;; Schedule status
(deftype schedule-status ()
  '(member :pre-scheduled :skipped :scheduled :processing :sent))

;;; Email schedule
(defclass email-schedule ()
  ((contact-id :initarg :contact-id :reader schedule-contact-id)
   (email-type :initarg :email-type :reader schedule-email-type)
   (scheduled-date :initarg :scheduled-date :accessor scheduled-date)
   (scheduled-time :initarg :scheduled-time :accessor scheduled-time)
   (status :initarg :status :accessor schedule-status :type schedule-status)
   (skip-reason :initarg :skip-reason :accessor skip-reason)
   (priority :initarg :priority :accessor schedule-priority)
   (campaign-instance-id :initarg :campaign-instance-id :accessor campaign-instance-id)
   (scheduler-run-id :initarg :scheduler-run-id :reader scheduler-run-id)))

;;; Generic functions for polymorphic behavior
(defgeneric calculate-send-date (email-type contact today))
(defgeneric should-respect-exclusions-p (email-type))
(defgeneric get-template (email-type))
```

### 2. DSL for Rules (dsl.lisp)

```lisp
(in-package :email-scheduler.dsl)

;;; Macro for defining state rules
(defmacro defstate (state-name &body rules)
  "Define exclusion rules for a state"
  `(setf (get ',state-name 'state-rules)
         (list ,@rules)))

;;; DSL functions for rule types
(defun birthday-window (&key before after (use-month-start nil))
  `(:birthday-window :before ,before :after ,after :use-month-start ,use-month-start))

(defun effective-date-window (&key before after)
  `(:effective-date-window :before ,before :after ,after))

(defun year-round-exclusion ()
  `(:year-round-exclusion))

(defun no-exclusion ()
  `(:no-exclusion))

;;; Macro for defining email scheduling rules
(defmacro defrule (name (contact date) &body body)
  "Define a scheduling rule"
  `(defmethod apply-rule ((rule (eql ',name)) ,contact ,date)
     ,@body))

;;; DSL for expressing scheduling decisions
(defmacro schedule-email (email-type date &key (priority 10) template)
  `(make-instance 'email-schedule
                  :email-type ,email-type
                  :scheduled-date ,date
                  :status :pre-scheduled
                  :priority ,priority
                  ,@(when template `(:template ,template))))

(defmacro skip-email (email-type reason &optional resume-date)
  `(make-instance 'email-schedule
                  :email-type ,email-type
                  :status :skipped
                  :skip-reason ,reason
                  ,@(when resume-date `(:resume-date ,resume-date))))

;;; Campaign definition DSL
(defmacro defcampaign (name &body options)
  "Define a campaign type"
  (let ((respect-exclusions (getf options :respect-exclusions t))
        (enable-followups (getf options :enable-followups t))
        (days-before (getf options :days-before 0))
        (priority (getf options :priority 10)))
    `(setf (get ',name 'campaign-config)
           (list :respect-exclusions ,respect-exclusions
                 :enable-followups ,enable-followups
                 :days-before ,days-before
                 :priority ,priority))))

;;; Reader macro for dates
(set-macro-character #\@
  (lambda (stream char)
    (declare (ignore char))
    (let ((date-string (read stream t nil t)))
      `(local-time:parse-timestring ,date-string))))
```

### 3. State Rules Implementation (rules.lisp)

```lisp
(in-package :email-scheduler.rules)

;;; Define state rules using the DSL
(defstate :ca
  (birthday-window :before 30 :after 60))

(defstate :id
  (birthday-window :before 0 :after 63))

(defstate :ky
  (birthday-window :before 0 :after 60))

(defstate :md
  (birthday-window :before 0 :after 30))

(defstate :nv
  (birthday-window :before 0 :after 60 :use-month-start t))

(defstate :ok
  (birthday-window :before 0 :after 60))

(defstate :or
  (birthday-window :before 0 :after 31))

(defstate :va
  (birthday-window :before 0 :after 30))

(defstate :mo
  (effective-date-window :before 30 :after 33))

(defstate :ct (year-round-exclusion))
(defstate :ma (year-round-exclusion))
(defstate :ny (year-round-exclusion))
(defstate :wa (year-round-exclusion))

(defstate :other (no-exclusion))

;;; Campaign definitions using DSL
(defcampaign rate-increase
  :respect-exclusions t
  :enable-followups t
  :days-before 14
  :priority 1)

(defcampaign seasonal-promo
  :respect-exclusions t
  :enable-followups t
  :days-before 7
  :priority 5)

(defcampaign initial-blast
  :respect-exclusions nil
  :enable-followups nil
  :days-before 0
  :priority 10)

;;; Rule application with multiple dispatch
(defgeneric apply-exclusion-rule (rule-type contact date rule-params))

(defmethod apply-exclusion-rule ((rule-type (eql :birthday-window)) contact date params)
  (when (contact-birthday contact)
    (let* ((before (getf params :before))
           (after (getf params :after))
           (use-month-start-p (getf params :use-month-start))
           (birthday (if use-month-start-p
                        (beginning-of-month (contact-birthday contact))
                        (contact-birthday contact))))
      (date-in-window-p date birthday before after))))

(defmethod apply-exclusion-rule ((rule-type (eql :effective-date-window)) contact date params)
  (when (contact-effective-date contact)
    (let ((before (getf params :before))
          (after (getf params :after)))
      (date-in-window-p date (contact-effective-date contact) before after))))

(defmethod apply-exclusion-rule ((rule-type (eql :year-round-exclusion)) contact date params)
  (declare (ignore contact date params))
  t)

(defmethod apply-exclusion-rule ((rule-type (eql :no-exclusion)) contact date params)
  (declare (ignore contact date params))
  nil)

;;; Main rule application
(defun apply-state-rules (contact date)
  "Apply all rules for a contact's state"
  (let ((state-rules (get (contact-state contact) 'state-rules)))
    (loop for rule in state-rules
          for rule-type = (first rule)
          for params = (rest rule)
          when (apply-exclusion-rule rule-type contact date params)
            return (values t rule-type params)
          finally (return (values nil nil nil)))))
```

### 4. Condition System for Error Handling (conditions.lisp)

```lisp
(in-package :email-scheduler)

;;; Define condition hierarchy
(define-condition scheduler-error (error) ())

(define-condition database-error (scheduler-error)
  ((query :initarg :query :reader error-query)
   (db-error :initarg :db-error :reader error-db-error)))

(define-condition invalid-contact-error (scheduler-error)
  ((contact-id :initarg :contact-id :reader error-contact-id)
   (reason :initarg :reason :reader error-reason)))

(define-condition configuration-error (scheduler-error)
  ((config-key :initarg :config-key :reader error-config-key)
   (value :initarg :value :reader error-value)))

(define-condition processing-error (scheduler-error)
  ((batch-id :initarg :batch-id :reader error-batch-id)
   (contacts :initarg :contacts :reader error-contacts)))

;;; Restart definitions
(defun skip-contact (condition)
  (declare (ignore condition))
  (invoke-restart 'skip-contact))

(defun retry-batch (condition)
  (declare (ignore condition))
  (invoke-restart 'retry-batch))

(defun use-default-config (condition)
  (declare (ignore condition))
  (invoke-restart 'use-default))

;;; Handler macros
(defmacro with-error-handling (&body body)
  `(handler-bind
       ((invalid-contact-error #'handle-invalid-contact)
        (database-error #'handle-database-error)
        (configuration-error #'handle-config-error))
     ,@body))

(defmacro with-contact-processing ((contact) &body body)
  `(restart-case
       (progn ,@body)
     (skip-contact ()
       :report "Skip this contact and continue"
       (log:warn "Skipping contact ~A" (contact-id ,contact))
       nil)
     (use-placeholder ()
       :report "Use placeholder data"
       (make-placeholder-schedule ,contact))))
```

### 5. Streaming Scheduler Implementation (scheduling.lisp)

```lisp
(in-package :email-scheduler)

;;; Configuration
(defparameter *scheduler-config*
  '(:timezone "America/Chicago"
    :batch-size 10000
    :birthday-days-before 14
    :effective-date-days-before 30
    :pre-window-buffer 60
    :daily-cap-percentage 0.07
    :ed-soft-limit 15))

;;; Main scheduling logic using streaming
(defun schedule-emails-streaming (db-path run-id &key (config *scheduler-config*))
  "Process contacts in a streaming fashion"
  (with-database (db db-path)
    (with-transaction (db)
      ;; Create audit checkpoint
      (create-checkpoint db run-id)
      
      ;; Clear existing schedules
      (clear-existing-schedules db run-id)
      
      ;; Process in chunks
      (loop with offset = 0
            with chunk-size = (getf config :batch-size)
            for contacts = (fetch-contacts-batch db offset chunk-size)
            while contacts
            do (process-contact-batch contacts db run-id config)
               (incf offset chunk-size)
               (update-checkpoint db run-id offset)))))

;;; Batch processing with parallel execution
(defun process-contact-batch (contacts db run-id config)
  "Process a batch of contacts in parallel"
  (let* ((lparallel:*kernel* (lparallel:make-kernel 4))
         (schedules (lparallel:pmapcar
                     (lambda (contact)
                       (with-contact-processing (contact)
                         (calculate-all-schedules contact config)))
                     contacts)))
    
    ;; Flatten and filter schedules
    (let* ((all-schedules (remove nil (alexandria:flatten schedules)))
           (balanced-schedules (apply-load-balancing all-schedules config)))
      
      ;; Batch insert
      (insert-schedules-batch db balanced-schedules run-id))))

;;; Calculate schedules for a contact
(defun calculate-all-schedules (contact config)
  "Calculate all email schedules for a contact"
  (let ((today (local-time:today))
        (schedules '()))
    
    ;; Anniversary emails
    (when (contact-birthday contact)
      (push (calculate-birthday-email contact today config) schedules))
    
    (when (contact-effective-date contact)
      (push (calculate-effective-date-email contact today config) schedules))
    
    ;; AEP email
    (push (calculate-aep-email contact today config) schedules)
    
    ;; Campaign emails
    (dolist (campaign (get-active-campaigns-for-contact contact))
      (push (calculate-campaign-email contact campaign today config) schedules))
    
    ;; Filter out nils and apply exclusion rules
    (apply-exclusion-rules (remove nil schedules) contact)))

;;; DSL usage in scheduling logic
(defrule birthday-scheduling (contact date)
  (let* ((birthday (contact-birthday contact))
         (next-birthday (next-anniversary date birthday))
         (send-date (subtract-days next-birthday 14)))
    (if (in-exclusion-window-p send-date contact)
        (skip-email 'birthday-email "exclusion-window" 
                    (exclusion-window-end-date send-date contact))
        (schedule-email 'birthday-email send-date :priority 5))))
```

### 6. Database Operations with SQL DSL (database.lisp)

```lisp
(in-package :email-scheduler)

;;; Database macros
(defmacro with-database ((db path) &body body)
  `(let ((,db (dbi:connect :sqlite3 :database-name ,path)))
     (unwind-protect
          (progn ,@body)
       (dbi:disconnect ,db))))

(defmacro with-transaction ((db) &body body)
  `(progn
     (dbi:do-sql ,db "BEGIN IMMEDIATE")
     (handler-case
         (prog1 (progn ,@body)
           (dbi:do-sql ,db "COMMIT"))
       (error (e)
         (dbi:do-sql ,db "ROLLBACK")
         (error e)))))

;;; SQL generation using SXQL
(defun generate-fetch-contacts-query (offset limit)
  (sxql:select (:id :email :zip_code :state :birthday :effective_date)
    (sxql:from :contacts)
    (sxql:order-by :id)
    (sxql:limit limit)
    (sxql:offset offset)))

(defun generate-insert-schedule-query (schedule run-id)
  (sxql:insert-into :email_schedules
    (sxql:set= :contact_id (schedule-contact-id schedule)
               :email_type (string (type-of (schedule-email-type schedule)))
               :scheduled_send_date (format-date (scheduled-date schedule))
               :status (string (schedule-status schedule))
               :skip_reason (skip-reason schedule)
               :priority (schedule-priority schedule)
               :scheduler_run_id run-id)))

;;; Batch operations
(defun insert-schedules-batch (db schedules run-id)
  "Insert schedules in batches of 2000"
  (loop for batch in (alexandria:subdivide schedules 2000)
        do (dolist (schedule batch)
             (let ((query (generate-insert-schedule-query schedule run-id)))
               (dbi:execute (dbi:prepare db (sxql:yield query)))))))

;;; Audit operations
(defun create-checkpoint (db run-id)
  (let ((query (sxql:insert-into :scheduler_checkpoints
                 (sxql:set= :run_timestamp (local-time:now)
                            :scheduler_run_id run-id
                            :status "started"))))
    (dbi:execute (dbi:prepare db (sxql:yield query)))))
```

### 7. Load Balancing Implementation (load-balancer.lisp)

```lisp
(in-package :email-scheduler)

;;; Load balancing with functional approach
(defun apply-load-balancing (schedules config)
  "Apply load balancing to schedules"
  (-> schedules
      (group-by-date)
      (identify-overloaded-days config)
      (smooth-effective-dates config)
      (enforce-daily-caps config)
      (flatten-schedule-groups)))

;;; Smoothing algorithm using DSL
(defmacro define-smoothing-rule (name test-form &body redistribution-logic)
  `(defmethod apply-smoothing ((rule (eql ',name)) schedules config)
     (loop for (date . day-schedules) in schedules
           if ,test-form
             append (progn ,@redistribution-logic)
           else
             append day-schedules)))

(define-smoothing-rule effective-date-smoothing
    (and (> (count-if #'effective-date-email-p day-schedules)
            (getf config :ed-soft-limit))
         (not (past-date-p date)))
  (redistribute-with-jitter day-schedules date 5))

;;; Jitter calculation
(defun calculate-jitter (contact-id email-type year window-days)
  "Deterministic jitter based on hash"
  (let* ((hash-input (format nil "~A-~A-~A" contact-id email-type year))
         (hash-value (sxhash hash-input)))
    (- (mod hash-value window-days) (floor window-days 2))))
```

### 8. Interactive Development Support

```lisp
;;; REPL helpers for development
(defpackage #:email-scheduler.repl
  (:use #:cl #:email-scheduler)
  (:export #:test-contact #:show-rules #:trace-scheduling
           #:explain-exclusion #:preview-schedules))

(in-package :email-scheduler.repl)

(defun test-contact (&key email state birthday effective-date)
  "Create a test contact for REPL experimentation"
  (make-instance 'contact
                 :id (random 10000)
                 :email (or email "test@example.com")
                 :state (or state :ca)
                 :birthday (when birthday 
                            (local-time:parse-timestring birthday))
                 :effective-date (when effective-date
                                  (local-time:parse-timestring effective-date))))

(defmacro trace-scheduling (contact)
  "Trace all scheduling decisions for a contact"
  `(let ((*trace-output* *standard-output*))
     (trace calculate-all-schedules 
            apply-exclusion-rules
            apply-state-rules)
     (prog1 (calculate-all-schedules ,contact *scheduler-config*)
       (untrace))))

(defun explain-exclusion (contact date)
  "Explain why a date is excluded for a contact"
  (multiple-value-bind (excluded-p rule-type params)
      (apply-state-rules contact date)
    (if excluded-p
        (format t "~&Date ~A is excluded for contact in ~A due to ~A rule:~%~S~%"
                date (contact-state contact) rule-type params)
        (format t "~&Date ~A is NOT excluded for contact in ~A~%"
                date (contact-state contact)))))

;;; Pretty printing for schedules
(defmethod print-object ((schedule email-schedule) stream)
  (print-unreadable-object (schedule stream :type t)
    (format stream "~A on ~A (~A)"
            (schedule-email-type schedule)
            (scheduled-date schedule)
            (schedule-status schedule))))
```

### 9. Testing Strategy

```lisp
;;; test/test-rules.lisp
(defpackage #:email-scheduler.test
  (:use #:cl #:fiveam #:email-scheduler))

(in-package :email-scheduler.test)

(def-suite email-scheduler-tests)
(in-suite email-scheduler-tests)

(test california-birthday-exclusion
  "Test CA birthday window exclusion"
  (let ((contact (make-instance 'contact
                               :state :ca
                               :birthday (local-time:parse-timestring "1980-03-15"))))
    ;; Test dates that should be excluded
    (is (apply-state-rules contact (local-time:parse-timestring "2024-02-14"))) ; 30 days before
    (is (apply-state-rules contact (local-time:parse-timestring "2024-03-15"))) ; on birthday
    (is (apply-state-rules contact (local-time:parse-timestring "2024-05-14"))) ; 60 days after
    ;; Test dates that should not be excluded
    (is (not (apply-state-rules contact (local-time:parse-timestring "2024-01-01"))))))

(test year-boundary-exclusion
  "Test exclusion windows spanning year boundaries"
  (let ((contact (make-instance 'contact
                               :state :ca
                               :birthday (local-time:parse-timestring "1980-01-15"))))
    ;; Window should span from Dec 16 previous year to March 16
    (is (apply-state-rules contact (local-time:parse-timestring "2023-12-20")))
    (is (apply-state-rules contact (local-time:parse-timestring "2024-01-15")))
    (is (apply-state-rules contact (local-time:parse-timestring "2024-03-15")))))

;;; Property-based testing
(test birthday-scheduling-properties
  "Property: birthday emails are always scheduled 14 days before birthday"
  (for-all ((month (gen-integer :min 1 :max 12))
            (day (gen-integer :min 1 :max 28)))
    (let* ((birthday (local-time:encode-timestamp 0 0 0 0 day month 1980))
           (contact (make-instance 'contact :birthday birthday))
           (schedule (calculate-birthday-email contact (local-time:today) *scheduler-config*)))
      (when (and schedule (eq (schedule-status schedule) :pre-scheduled))
        (is (= 14 (local-time:timestamp-difference 
                   (scheduled-date schedule) 
                   (next-anniversary (local-time:today) birthday))))))))
```

### 10. Performance Optimization

```lisp
;;; Compiler optimizations
(declaim (optimize (speed 3) (safety 1) (debug 1)))

;;; Type declarations for critical paths
(declaim (ftype (function (contact) (values list &optional)) 
                calculate-all-schedules))

;;; Memory-efficient streaming
(defun process-contacts-lazy (db-path run-id config)
  "Lazy evaluation version for huge datasets"
  (loop with db = (dbi:connect :sqlite3 :database-name db-path)
        with offset = 0
        with chunk-size = (getf config :batch-size)
        for contacts = (fetch-contacts-batch db offset chunk-size)
        while contacts
        do (progn
             ;; Process and immediately write to avoid memory buildup
             (let ((schedules (mapcar #'calculate-all-schedules contacts)))
               (write-schedules-incrementally db schedules run-id))
             ;; Force garbage collection after each batch
             (sb-ext:gc :full t)
             (incf offset chunk-size))
        finally (dbi:disconnect db)))

;;; Memoization for expensive calculations
(let ((exclusion-cache (make-hash-table :test 'equal)))
  (defun cached-apply-state-rules (contact date)
    (let ((key (list (contact-state contact) 
                     (contact-birthday contact)
                     (contact-effective-date contact)
                     date)))
      (alexandria:ensure-gethash key exclusion-cache
                                 (apply-state-rules contact date)))))
```

### 11. Configuration and Deployment

```lisp
;;; config.lisp
(defparameter *config-schema*
  '((:timezone string "America/Chicago")
    (:batch-size integer 10000)
    (:birthday-days-before integer 14)
    (:effective-date-days-before integer 30)
    (:pre-window-buffer integer 60)
    (:daily-cap-percentage float 0.07)
    (:ed-soft-limit integer 15)))

(defun load-config (path)
  "Load configuration with validation"
  (handler-case
      (let ((config (yason:parse (alexandria:read-file-into-string path))))
        (validate-config config *config-schema*)
        config)
    (error (e)
      (error 'configuration-error 
             :config-key path 
             :value (format nil "Failed to load: ~A" e)))))

;;; Main entry point
(defun main (&key (config-path "config/scheduler.json"))
  "Main entry point for the scheduler"
  (let ((config (load-config config-path))
        (run-id (generate-run-id)))
    (log:info "Starting scheduler run ~A" run-id)
    (handler-case
        (progn
          (backup-database (getf config :db-path))
          (schedule-emails-streaming (getf config :db-path) run-id :config config)
          (log:info "Scheduler run ~A completed successfully" run-id))
      (error (e)
        (log:error "Scheduler run ~A failed: ~A" run-id e)
        (rollback-run run-id)))))
```

## Implementation Steps

1. **Phase 1**: Core domain model and basic DSL
2. **Phase 2**: State rules engine with DSL expressions
3. **Phase 3**: Date calculations and exclusion windows
4. **Phase 4**: Basic scheduling without load balancing
5. **Phase 5**: Condition system and error handling
6. **Phase 6**: Database integration with transactions
7. **Phase 7**: Load balancing and smoothing
8. **Phase 8**: Campaign system with DSL
9. **Phase 9**: Performance optimization
10. **Phase 10**: Interactive development tools

## Key Advantages of the Lisp Approach

1. **Powerful DSL**: Macros allow business rules to be expressed naturally
2. **Interactive Development**: REPL allows testing rules immediately
3. **Runtime Flexibility**: Rules can be modified without recompilation
4. **Condition System**: Sophisticated error recovery strategies
5. **Multiple Dispatch**: Clean handling of different email types
6. **Dynamic Typing**: Rapid prototyping with optional optimization

## Success Criteria

1. DSL is readable by non-programmers
2. Rules can be modified at runtime
3. System processes 3M contacts in under 30 minutes
4. Memory usage stays under 1GB via streaming
5. Full audit trail with rollback capability
6. Interactive tools for debugging and testing
7. Comprehensive error recovery via restarts

## Advanced Features to Consider

1. **Reader Macros**: Create custom syntax for dates, rules
2. **MOP (Meta-Object Protocol)**: Dynamic class generation for campaigns
3. **Compiler Macros**: Optimize critical paths
4. **Foreign Function Interface**: Integrate with C libraries for performance
5. **Persistent CLOS**: Direct object persistence to database

Remember: Leverage Lisp's strengths - powerful macros, interactive development, and dynamic nature - to create a system that's both powerful and maintainable.
