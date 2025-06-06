;;; campaigns.lisp - Campaign system implementation

(in-package :email-scheduler)

;;; Campaign Type Domain Model
(defclass campaign-type ()
  ((name
    :initarg :name
    :accessor campaign-type-name
    :type string
    :documentation "Campaign type identifier (e.g., 'rate_increase', 'seasonal_promo')")
   (respect-exclusion-windows
    :initarg :respect-exclusion-windows
    :accessor campaign-type-respect-exclusion-windows
    :type boolean
    :initform t
    :documentation "Whether this campaign type respects state exclusion windows")
   (enable-followups
    :initarg :enable-followups
    :accessor campaign-type-enable-followups
    :type boolean
    :initform t
    :documentation "Whether this campaign type generates follow-up emails")
   (days-before-event
    :initarg :days-before-event
    :accessor campaign-type-days-before-event
    :type integer
    :initform 0
    :documentation "Days before trigger date to send email (positive = before, negative = after)")
   (target-all-contacts
    :initarg :target-all-contacts
    :accessor campaign-type-target-all-contacts
    :type boolean
    :initform nil
    :documentation "Whether this campaign targets all contacts by default")
   (priority
    :initarg :priority
    :accessor campaign-type-priority
    :type integer
    :initform 10
    :documentation "Campaign priority (lower numbers = higher priority)")
   (active
    :initarg :active
    :accessor campaign-type-active
    :type boolean
    :initform t
    :documentation "Whether this campaign type is active for use")
   (created-at
    :initarg :created-at
    :accessor campaign-type-created-at
    :type local-time:timestamp
    :initform (local-time:now)
    :documentation "When this campaign type was created")
   (updated-at
    :initarg :updated-at
    :accessor campaign-type-updated-at
    :type local-time:timestamp
    :initform (local-time:now)
    :documentation "When this campaign type was last updated"))
  (:documentation "Base configuration for a type of campaign that can be reused"))

;;; Campaign Instance Domain Model
(defclass campaign-instance ()
  ((id
    :initarg :id
    :accessor campaign-instance-id
    :type integer
    :documentation "Unique identifier for this campaign instance")
   (campaign-type-name
    :initarg :campaign-type-name
    :accessor campaign-instance-campaign-type-name
    :type string
    :documentation "References campaign-types.name")
   (instance-name
    :initarg :instance-name
    :accessor campaign-instance-instance-name
    :type string
    :documentation "Unique name for this specific campaign instance")
   (email-template
    :initarg :email-template
    :accessor campaign-instance-email-template
    :type (or string null)
    :documentation "Template identifier for email content")
   (sms-template
    :initarg :sms-template
    :accessor campaign-instance-sms-template
    :type (or string null)
    :documentation "Template identifier for SMS content")
   (active-start-date
    :initarg :active-start-date
    :accessor campaign-instance-active-start-date
    :type (or local-time:timestamp null)
    :documentation "When this campaign instance becomes active")
   (active-end-date
    :initarg :active-end-date
    :accessor campaign-instance-active-end-date
    :type (or local-time:timestamp null)
    :documentation "When this campaign instance stops being active")
   (metadata
    :initarg :metadata
    :accessor campaign-instance-metadata
    :type (or string null)
    :initform nil
    :documentation "JSON field for instance-specific configuration overrides")
   (created-at
    :initarg :created-at
    :accessor campaign-instance-created-at
    :type local-time:timestamp
    :initform (local-time:now)
    :documentation "When this campaign instance was created")
   (updated-at
    :initarg :updated-at
    :accessor campaign-instance-updated-at
    :type local-time:timestamp
    :initform (local-time:now)
    :documentation "When this campaign instance was last updated"))
  (:documentation "Specific campaign execution with templates and targeting"))

;;; Contact Campaign Association
(defclass contact-campaign ()
  ((id
    :initarg :id
    :accessor contact-campaign-id
    :type integer
    :documentation "Unique identifier for this association")
   (contact-id
    :initarg :contact-id
    :accessor contact-campaign-contact-id
    :type integer
    :documentation "Reference to the target contact")
   (campaign-instance-id
    :initarg :campaign-instance-id
    :accessor contact-campaign-campaign-instance-id
    :type integer
    :documentation "Reference to the specific campaign instance")
   (trigger-date
    :initarg :trigger-date
    :accessor contact-campaign-trigger-date
    :type (or local-time:timestamp null)
    :documentation "The event date that triggers the campaign")
   (status
    :initarg :status
    :accessor contact-campaign-status
    :type keyword
    :initform :pending
    :documentation "Current state: :pending, :scheduled, :sent, :skipped")
   (metadata
    :initarg :metadata
    :accessor contact-campaign-metadata
    :type (or string null)
    :initform nil
    :documentation "JSON field for contact-specific campaign data")
   (created-at
    :initarg :created-at
    :accessor contact-campaign-created-at
    :type local-time:timestamp
    :initform (local-time:now)
    :documentation "When this association was created")
   (updated-at
    :initarg :updated-at
    :accessor contact-campaign-updated-at
    :type local-time:timestamp
    :initform (local-time:now)
    :documentation "When this association was last updated"))
  (:documentation "Association between a contact and a specific campaign instance"))

;;; Campaign Email Type (extends the domain model)
(defclass campaign-email ()
  ((campaign-type-name
    :initarg :campaign-type-name
    :accessor campaign-email-campaign-type-name
    :type string
    :documentation "The campaign type this email belongs to")
   (campaign-instance-id
    :initarg :campaign-instance-id
    :accessor campaign-email-campaign-instance-id
    :type (or integer null)
    :documentation "Specific campaign instance ID for template resolution"))
  (:documentation "Email type for campaign-based emails"))

;;; Campaign Type Factory and Registry
(defvar *campaign-type-registry* (make-hash-table :test 'equal)
  "Registry of campaign type definitions")

(defun register-campaign-type (campaign-type)
  "Register a campaign type in the registry"
  (setf (gethash (campaign-type-name campaign-type) *campaign-type-registry*)
        campaign-type))

(defun get-campaign-type (name)
  "Get a campaign type by name from the registry"
  (gethash name *campaign-type-registry*))

(defun list-campaign-types ()
  "List all registered campaign types"
  (alexandria:hash-table-values *campaign-type-registry*))

;;; Built-in Campaign Type Definitions
(defun create-builtin-campaign-types ()
  "Create and register built-in campaign types"
  
  ;; Rate Increase Campaign
  (register-campaign-type
   (make-instance 'campaign-type
                  :name "rate_increase"
                  :respect-exclusion-windows t
                  :enable-followups t
                  :days-before-event 14
                  :target-all-contacts nil
                  :priority 1))
  
  ;; Seasonal Promotion Campaign
  (register-campaign-type
   (make-instance 'campaign-type
                  :name "seasonal_promo"
                  :respect-exclusion-windows t
                  :enable-followups t
                  :days-before-event 7
                  :target-all-contacts nil
                  :priority 5))
  
  ;; Initial Blast Campaign
  (register-campaign-type
   (make-instance 'campaign-type
                  :name "initial_blast"
                  :respect-exclusion-windows nil
                  :enable-followups nil
                  :days-before-event 0
                  :target-all-contacts t
                  :priority 10))
  
  ;; Regulatory Notice Campaign
  (register-campaign-type
   (make-instance 'campaign-type
                  :name "regulatory_notice"
                  :respect-exclusion-windows nil
                  :enable-followups nil
                  :days-before-event 0
                  :target-all-contacts t
                  :priority 1))
  
  ;; Policy Update Campaign
  (register-campaign-type
   (make-instance 'campaign-type
                  :name "policy_update"
                  :respect-exclusion-windows t
                  :enable-followups t
                  :days-before-event 7
                  :target-all-contacts nil
                  :priority 3)))

;;; Campaign Scheduling Logic
(defun calculate-campaign-send-date (trigger-date days-before-event)
  "Calculate when to send campaign email based on trigger date and offset"
  (if (zerop days-before-event)
      trigger-date
      (email-scheduler.date-utils:add-days trigger-date (- days-before-event))))

(defun should-respect-exclusion-windows (campaign-type)
  "Check if a campaign type should respect exclusion windows"
  (campaign-type-respect-exclusion-windows campaign-type))

(defun campaign-generates-followups (campaign-type)
  "Check if a campaign type generates follow-up emails"
  (campaign-type-enable-followups campaign-type))

;;; Campaign Instance Management
(defvar *active-campaign-instances* '()
  "Cache of currently active campaign instances")

(defun is-campaign-instance-active (campaign-instance &optional (date (local-time:now)))
  "Check if a campaign instance is currently active"
  (let ((start-date (campaign-instance-active-start-date campaign-instance))
        (end-date (campaign-instance-active-end-date campaign-instance)))
    (and (or (null start-date) (local-time:timestamp<= start-date date))
         (or (null end-date) (local-time:timestamp>= end-date date)))))

(defun get-active-campaign-instances (&optional (date (local-time:now)))
  "Get all currently active campaign instances"
  (remove-if-not (lambda (instance) 
                   (is-campaign-instance-active instance date))
                 *active-campaign-instances*))

;;; Campaign Email Generation
(defun create-campaign-email-schedule (contact-id campaign-instance contact-campaign trigger-date)
  "Create an email schedule for a campaign"
  (let* ((campaign-type-name (campaign-instance-campaign-type-name campaign-instance))
         (campaign-type (get-campaign-type campaign-type-name))
         (send-date (calculate-campaign-send-date 
                     trigger-date 
                     (campaign-type-days-before-event campaign-type)))
         (email-type (make-instance 'campaign-email
                                    :campaign-type-name campaign-type-name
                                    :campaign-instance-id (campaign-instance-id campaign-instance))))
    
    (make-instance 'email-scheduler.domain:email-schedule
                   :contact-id contact-id
                   :email-type email-type
                   :scheduled-date send-date
                   :status :pre-scheduled
                   :priority (campaign-type-priority campaign-type)
                   :campaign-instance-id (campaign-instance-id campaign-instance)
                   :email-template (campaign-instance-email-template campaign-instance)
                   :sms-template (campaign-instance-sms-template campaign-instance))))

;;; Campaign Priority Resolution
(defun resolve-campaign-conflicts (schedules)
  "Resolve conflicts when multiple campaigns target the same contact/date"
  ;; Group schedules by contact-id and scheduled-date
  (let ((schedule-groups (make-hash-table :test 'equal)))
    (dolist (schedule schedules)
      (let ((key (list (email-scheduler.domain:contact-id schedule)
                       (email-scheduler.domain:scheduled-date schedule))))
        (push schedule (gethash key schedule-groups))))
    
    ;; For each group, keep only the highest priority campaign
    (let ((resolved-schedules '()))
      (maphash (lambda (key group)
                 (declare (ignore key))
                 (let ((highest-priority (reduce (lambda (a b)
                                                   (if (< (email-scheduler.domain:schedule-priority a)
                                                          (email-scheduler.domain:schedule-priority b))
                                                       a b))
                                                 group)))
                   (push highest-priority resolved-schedules)))
               schedule-groups)
      resolved-schedules)))

;;; Integration with Main Scheduling
(defun get-campaign-schedules-for-contact (contact &optional (config *scheduler-config*))
  "Calculate all campaign-based email schedules for a contact"
  (let ((db-path (getf config :db-path "scheduler.db")))
    (if (probe-file db-path)
        (email-scheduler.database:with-database (db db-path)
          (calculate-campaign-schedules contact db))
        (progn
          (format t "Warning: Database not found at ~A, skipping campaign schedules~%" db-path)
          '()))))

;;; Email Type Integration
(defmethod email-scheduler.domain:format-email-type ((email-type campaign-email))
  (format nil "campaign_~A" (campaign-email-campaign-type-name email-type)))

;;; Utilities for Testing
(defun create-test-campaign-instance (campaign-type-name instance-name)
  "Create a test campaign instance for development"
  (make-instance 'campaign-instance
                 :id (random 10000)
                 :campaign-type-name campaign-type-name
                 :instance-name instance-name
                 :email-template (format nil "~A_email_template" instance-name)
                 :sms-template (format nil "~A_sms_template" instance-name)
                 :active-start-date (local-time:timestamp- (local-time:now) 30 :day)
                 :active-end-date (local-time:timestamp+ (local-time:now) 30 :day)))

;;; Initialize built-in campaign types when this file is loaded
(create-builtin-campaign-types)

;;; Database Integration Functions
(defun setup-campaign-system (db)
  "Set up the campaign system with database integration"
  (handler-case
      (progn
        ;; Populate built-in campaign types in database
        (email-scheduler.database:populate-builtin-campaign-types db)
        
        ;; Load active campaign instances from database
        (setf *active-campaign-instances* 
              (email-scheduler.database:get-active-campaign-instances-from-db db))
        
        (format t "Campaign system initialized with ~A active instances~%" 
                (length *active-campaign-instances*)))
    (error (e)
      (format t "Warning: Campaign system setup failed: ~A~%" e))))

(defun calculate-campaign-schedules (contact db)
  "Calculate campaign schedules for a contact using database"
  (let ((schedules '())
        (contact-id (email-scheduler.domain:contact-id contact)))
    
    ;; Get campaign instances this contact is enrolled in
    (handler-case
        (let ((contact-campaigns (email-scheduler.database:get-contact-campaigns db contact-id)))
          (dolist (cc-row contact-campaigns)
            (let* ((campaign-instance-id (first cc-row))
                   (trigger-date-str (second cc-row))
                   (trigger-date (when trigger-date-str 
                                   (local-time:parse-timestring trigger-date-str)))
                   (campaign-instance (find campaign-instance-id *active-campaign-instances*
                                            :key #'campaign-instance-id)))
              (when (and campaign-instance trigger-date)
                (let ((schedule (create-campaign-email-schedule 
                                contact-id campaign-instance nil trigger-date)))
                  (push schedule schedules))))))
      (error (e)
        (format t "Warning: Failed to get campaign schedules for contact ~A: ~A~%" 
                contact-id e)))
    
    schedules))

;;; Campaign Management Functions
(defun create-test-campaign-data (db)
  "Create test campaign data for development"
  (handler-case
      (progn
        ;; Create a test rate increase campaign instance
        (let ((rate-increase-instance 
               (list :campaign-type "rate_increase"
                     :instance-name "test_rate_increase_2024"
                     :email-template "rate_increase_email_v1"
                     :sms-template "rate_increase_sms_v1" 
                     :active-start-date "2024-01-01"
                     :active-end-date "2024-12-31"
                     :metadata "{\"test\": true}")))
          (email-scheduler.database:insert-campaign-instance db rate-increase-instance))
        
        ;; Create a test seasonal promo campaign instance
        (let ((promo-instance 
               (list :campaign-type "seasonal_promo"
                     :instance-name "test_spring_promo_2024"
                     :email-template "spring_promo_email_v1"
                     :sms-template "spring_promo_sms_v1"
                     :active-start-date "2024-03-01"
                     :active-end-date "2024-05-31"
                     :metadata "{\"season\": \"spring\"}")))
          (email-scheduler.database:insert-campaign-instance db promo-instance))
        
        ;; Enroll some test contacts in campaigns
        (let ((test-contact-ids '(1 2 3 4 5)))
          (dolist (contact-id test-contact-ids)
            (email-scheduler.database:insert-contact-campaign 
             db contact-id 1 (local-time:timestamp+ (local-time:now) 30 :day))
            (email-scheduler.database:insert-contact-campaign 
             db contact-id 2 (local-time:timestamp+ (local-time:now) 45 :day))))
        
        (format t "Test campaign data created successfully~%"))
    (error (e)
      (format t "Warning: Failed to create test campaign data: ~A~%" e))))