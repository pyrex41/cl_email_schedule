;;; packages.lisp

(defpackage #:email-scheduler.domain
  (:use #:cl)
  (:export 
   ;; Types
   #:us-state
   #:schedule-status
   ;; Classes
   #:email-type
   #:anniversary-email
   #:birthday-email
   #:effective-date-email
   #:aep-email
   #:post-window-email
   #:campaign-email
   #:follow-up-email
   #:followup-1-cold
   #:followup-2-clicked-no-hq
   #:followup-3-hq-no-yes
   #:followup-4-hq-with-yes
   #:contact
   #:email-schedule
   #:campaign-type
   #:campaign-instance
   ;; Accessors
   #:priority
   #:template
   #:days-before
   #:send-month
   #:send-day
   #:campaign-type
   #:instance-id
   #:respect-exclusions-p
   #:days-before-event
   #:contact-id
   #:contact-email
   #:contact-zip
   #:contact-state
   #:contact-birthday
   #:contact-effective-date
   #:schedule-contact-id
   #:schedule-email-type
   #:scheduled-date
   #:scheduled-time
   #:schedule-status
   #:skip-reason
   #:schedule-priority
   #:campaign-instance-id
   #:scheduler-run-id
   #:email-template
   #:sms-template
   #:schedule-created-at
   #:schedule-updated-at
   #:actual-send-datetime
   #:schedule-metadata
   ;; Campaign classes
   #:contact-campaign
   ;; Generic functions
   #:calculate-send-date
   #:should-respect-exclusions-p
   #:get-template
   #:format-email-type))

(defpackage #:email-scheduler.date-utils
  (:use #:cl)
  (:export 
   #:next-anniversary
   #:subtract-days
   #:add-days
   #:date-in-window-p
   #:beginning-of-month
   #:past-date-p
   #:format-date
   #:parse-date
   #:same-date-p
   #:days-between
   #:date-add-months
   #:end-of-month
   #:years-between
   #:leap-year-p
   #:days-in-month
   #:today))

(defpackage #:email-scheduler.dsl
  (:use #:cl)
  (:import-from #:email-scheduler.domain
                #:email-schedule)
  (:export 
   #:defrule
   #:defstate
   #:defcampaign
   #:birthday-window
   #:effective-date-window
   #:year-round-exclusion
   #:no-exclusion
   #:schedule-email
   #:skip-email))

(defpackage #:email-scheduler.rules
  (:use #:cl #:email-scheduler.dsl)
  (:import-from #:email-scheduler.domain
                #:contact
                #:contact-state
                #:contact-birthday
                #:contact-effective-date)
  (:import-from #:email-scheduler.date-utils
                #:date-in-window-p
                #:beginning-of-month)
  (:export 
   #:apply-state-rules
   #:calculate-exclusion-window
   #:apply-exclusion-rule
   #:in-exclusion-window-p
   #:exclusion-window-end-date
   ;; Complete state rules
   #:*all-state-rules*
   #:get-birthday-window-states
   #:get-effective-date-window-states
   #:get-year-round-exclusion-states
   #:get-no-exclusion-states
   #:get-state-rule-type
   #:get-state-rule-params
   #:validate-all-state-rules
   #:missing-state-rules
   #:summarize-state-rules
   #:apply-complete-state-rules
   #:test-state-rule-coverage
   #:show-all-state-rules))

(defpackage #:email-scheduler.conditions
  (:use #:cl)
  (:export 
   ;; Conditions
   #:scheduler-error
   #:database-error
   #:invalid-contact-error
   #:configuration-error
   #:processing-error
   ;; Accessors
   #:error-query
   #:error-db-error
   #:error-contact-id
   #:error-reason
   #:error-config-key
   #:error-value
   #:error-batch-id
   #:error-contacts
   ;; Restart functions
   #:skip-contact
   #:retry-batch
   #:use-default-config
   ;; Handler macros
   #:with-error-handling
   #:with-contact-processing
   ;; Signal functions
   #:signal-invalid-contact
   #:signal-database-error
   #:signal-configuration-error
   #:signal-processing-error))

(defpackage #:email-scheduler.database
  (:use #:cl)
  (:export 
   #:with-database
   #:with-transaction
   #:fetch-contacts-batch
   #:insert-schedules-batch
   #:clear-existing-schedules
   #:create-checkpoint
   #:update-checkpoint
   #:get-active-campaigns-for-contact
   #:create-database-schema
   #:insert-test-contacts
   #:count-contacts
   #:count-schedules
   #:get-scheduler-stats
   #:backup-database
   #:generate-fetch-contacts-query
   #:generate-insert-schedule-query
   ;; Campaign database operations
   #:insert-campaign-type
   #:insert-campaign-instance
   #:get-active-campaign-instances-from-db
   #:insert-contact-campaign
   #:get-contact-campaigns
   #:populate-builtin-campaign-types))

(defpackage #:email-scheduler.load-balancer
  (:use #:cl)
  (:export 
   #:apply-load-balancing
   #:group-by-date
   #:smooth-effective-dates
   #:enforce-daily-caps
   #:calculate-jitter
   #:apply-enhanced-load-balancing
   #:generate-load-balancing-report
   #:validate-load-balancing-constraints
   #:make-config-from-json
   #:load-balancer-config))

(defpackage #:email-scheduler.frequency-limiter
  (:use #:cl)
  (:export 
   #:frequency-config
   #:make-frequency-config-from-json
   #:apply-frequency-limits
   #:check-contact-frequency-limit
   #:is-followup-email-p
   #:generate-frequency-limit-report
   #:validate-frequency-limits
   #:integrate-frequency-limits
   #:should-apply-frequency-limits-p))

(defpackage #:email-scheduler
  (:use #:cl)
  (:import-from #:email-scheduler.conditions
                #:with-error-handling
                #:with-contact-processing)
  (:import-from #:email-scheduler.database
                #:with-database
                #:with-transaction)
  (:export 
   #:run-scheduler
   #:configure
   #:process-contacts
   #:schedule-emails-streaming
   #:*scheduler-config*
   #:calculate-all-schedules
   #:calculate-birthday-email
   #:calculate-effective-date-email
   #:calculate-aep-email
   #:calculate-campaign-schedules
   #:process-contact-batch
   #:validate-config
   #:check-frequency-limits
   #:setup-test-environment
   #:run-test-scheduler
   ;; Campaign system
   #:campaign-type
   #:campaign-instance
   #:contact-campaign
   #:register-campaign-type
   #:get-campaign-type
   #:list-campaign-types
   #:create-builtin-campaign-types
   #:get-campaign-schedules-for-contact
   ;; Follow-up email system
   #:*followup-config*
   #:*followup-email-types*
   #:followup-email-type
   #:followup-type-name
   #:followup-priority
   #:followup-description
   #:schedule-followup-emails
   #:run-followup-scheduler
   #:test-followup-system
   #:process-followups-in-batches
   ;; Frequency limits system
   #:apply-frequency-limits
   #:validate-frequency-limits
   #:generate-frequency-limit-report))

(defpackage #:email-scheduler.repl
  (:use #:cl #:email-scheduler)
  (:import-from #:email-scheduler.domain
                #:contact)
  (:import-from #:email-scheduler.rules
                #:apply-state-rules)
  (:export 
   #:test-contact
   #:show-rules
   #:trace-scheduling
   #:explain-exclusion
   #:preview-schedules))

;; Add this import for sqlite support
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-case
      (progn
        (require :sb-posix)
        (require :sb-bsd-sockets))
    (error ())))

(defpackage #:sqlite
  (:use #:cl)
  (:export #:connect #:disconnect #:execute-single #:execute-to-list #:execute-non-query))

(defpackage #:email-scheduler
  (:use #:cl)
  (:import-from #:email-scheduler.domain
                #:contact #:email-schedule #:schedule-status)
  (:import-from #:email-scheduler.database
                #:with-database #:create-database-schema)
  (:import-from #:email-scheduler.campaigns
                #:setup-campaign-system)
  (:import-from #:email-scheduler.frequency-limiter
                #:integrate-frequency-limits)
  (:import-from #:email-scheduler.load-balancer
                #:apply-load-balancing)
  (:export #:run-scheduler
           #:schedule-emails-streaming
           #:setup-test-environment
           #:run-test-scheduler
           #:calculate-all-schedules
           #:*scheduler-config*
           #:main))