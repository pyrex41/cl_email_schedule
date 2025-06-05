;;; domain.lisp

(in-package :email-scheduler.domain)

;;; Define states as symbols with properties
(deftype us-state ()
  '(member :ca :ct :id :ky :ma :md :mo :nv :ny :ok :or :va :wa :other))

;;; Email type hierarchy using CLOS
(defclass email-type ()
  ((priority :initarg :priority :accessor priority :initform 10
             :documentation "Lower numbers = higher priority")
   (template :initarg :template :accessor template
             :documentation "Email template identifier")))

(defclass anniversary-email (email-type) 
  ()
  (:documentation "Base class for anniversary-based emails"))

(defclass birthday-email (anniversary-email)
  ((days-before :initform 14 :reader days-before
                :documentation "Days before birthday to send email"))
  (:documentation "Email sent before contact's birthday"))

(defclass effective-date-email (anniversary-email)
  ((days-before :initform 30 :reader days-before
                :documentation "Days before effective date to send email"))
  (:documentation "Email sent before policy effective date anniversary"))

(defclass aep-email (anniversary-email)
  ((send-month :initform 9 :reader send-month
               :documentation "Month to send AEP email (September)")
   (send-day :initform 15 :reader send-day
             :documentation "Day of month to send AEP email"))
  (:documentation "Annual Enrollment Period email"))

(defclass post-window-email (anniversary-email) 
  ()
  (:documentation "Email sent after exclusion window ends"))

(defclass campaign-email (email-type)
  ((campaign-type :initarg :campaign-type :accessor campaign-type
                  :documentation "Type of campaign (e.g., rate_increase)")
   (instance-id :initarg :instance-id :accessor instance-id
                :documentation "Campaign instance identifier")
   (respect-exclusions-p :initarg :respect-exclusions-p :accessor respect-exclusions-p
                         :initform t
                         :documentation "Whether to respect state exclusion windows")
   (days-before-event :initarg :days-before-event :accessor days-before-event
                      :initform 0
                      :documentation "Days before trigger date to send"))
  (:documentation "Campaign-based email with flexible configuration"))

;;; Follow-up email types
(defclass follow-up-email (email-type)
  ((parent-email-id :initarg :parent-email-id :accessor parent-email-id
                    :documentation "ID of the initial email this follows up"))
  (:documentation "Base class for follow-up emails"))

(defclass followup-1-cold (follow-up-email) 
  ()
  (:documentation "Cold follow-up for non-engaged contacts"))

(defclass followup-2-clicked-no-hq (follow-up-email) 
  ()
  (:documentation "Follow-up for contacts who clicked but didn't answer health questions"))

(defclass followup-3-hq-no-yes (follow-up-email) 
  ()
  (:documentation "Follow-up for contacts who answered health questions with no conditions"))

(defclass followup-4-hq-with-yes (follow-up-email) 
  ()
  (:documentation "Follow-up for contacts who answered health questions with conditions"))

;;; Contact class
(defclass contact ()
  ((id :initarg :id :reader contact-id
       :documentation "Unique contact identifier")
   (email :initarg :email :reader contact-email
          :documentation "Contact's email address")
   (zip-code :initarg :zip-code :reader contact-zip
             :documentation "US ZIP code")
   (state :initarg :state :accessor contact-state :type (or null us-state)
          :documentation "US state derived from ZIP code")
   (birthday :initarg :birthday :accessor contact-birthday 
             :type (or null local-time:timestamp)
             :initform nil
             :documentation "Contact's date of birth")
   (effective-date :initarg :effective-date :accessor contact-effective-date 
                   :type (or null local-time:timestamp)
                   :initform nil
                   :documentation "Policy effective date"))
  (:documentation "Contact information for email scheduling"))

;;; Schedule status
(deftype schedule-status ()
  '(member :pre-scheduled :skipped :scheduled :processing :sent))

;;; Email schedule
(defclass email-schedule ()
  ((id :initarg :id :reader schedule-id
       :documentation "Unique schedule identifier")
   (contact-id :initarg :contact-id :reader schedule-contact-id
               :documentation "ID of contact to receive email")
   (email-type :initarg :email-type :reader schedule-email-type
               :documentation "Type of email to send")
   (scheduled-date :initarg :scheduled-date :accessor scheduled-date
                   :documentation "Date when email should be sent")
   (scheduled-time :initarg :scheduled-time :accessor scheduled-time
                   :initform "08:30:00"
                   :documentation "Time of day to send email")
   (status :initarg :status :accessor schedule-status :type schedule-status
           :initform :pre-scheduled
           :documentation "Current status of scheduled email")
   (skip-reason :initarg :skip-reason :accessor skip-reason
                :initform nil
                :documentation "Reason if email was skipped")
   (priority :initarg :priority :accessor schedule-priority
             :initform 10
             :documentation "Email priority (lower = higher priority)")
   (campaign-instance-id :initarg :campaign-instance-id :accessor campaign-instance-id
                         :initform nil
                         :documentation "Campaign instance ID if campaign-based")
   (email-template :initarg :email-template :accessor email-template
                   :initform nil
                   :documentation "Template to use for this email")
   (sms-template :initarg :sms-template :accessor sms-template
                 :initform nil
                 :documentation "Template to use for SMS")
   (scheduler-run-id :initarg :scheduler-run-id :accessor scheduler-run-id
                     :documentation "ID of scheduler run that created this schedule")
   (created-at :initarg :created-at :accessor schedule-created-at
               :initform (local-time:now)
               :documentation "When this schedule was created")
   (updated-at :initarg :updated-at :accessor schedule-updated-at
               :initform (local-time:now)
               :documentation "When this schedule was last updated")
   (actual-send-datetime :initarg :actual-send-datetime :accessor actual-send-datetime
                         :initform nil
                         :documentation "When the email was actually sent")
   (metadata :initarg :metadata :accessor schedule-metadata
             :initform nil
             :documentation "Additional metadata as property list"))
  (:documentation "Scheduled email with all necessary information"))

;;; Campaign type configuration
(defclass campaign-type ()
  ((name :initarg :name :reader campaign-name
         :documentation "Campaign type identifier")
   (respect-exclusion-windows :initarg :respect-exclusion-windows 
                              :accessor respect-exclusion-windows-p
                              :initform t
                              :documentation "Whether to respect state exclusion rules")
   (enable-followups :initarg :enable-followups :accessor enable-followups-p
                     :initform t
                     :documentation "Whether to generate follow-up emails")
   (days-before-event :initarg :days-before-event :accessor days-before-event
                      :initform 0
                      :documentation "Default days before event to send")
   (target-all-contacts :initarg :target-all-contacts :accessor target-all-contacts-p
                        :initform nil
                        :documentation "Whether campaign targets all contacts")
   (priority :initarg :priority :accessor priority
             :initform 10
             :documentation "Campaign priority (lower = higher)")
   (active :initarg :active :accessor active-p
           :initform t
           :documentation "Whether campaign type is active"))
  (:documentation "Base campaign type configuration"))

;;; Campaign instance
(defclass campaign-instance ()
  ((id :initarg :id :reader campaign-id
       :documentation "Unique campaign instance ID")
   (campaign-type :initarg :campaign-type :reader campaign-type
                  :documentation "Reference to base campaign type")
   (instance-name :initarg :instance-name :reader instance-name
                  :documentation "Unique name for this campaign instance")
   (email-template :initarg :email-template :accessor email-template
                   :documentation "Email template identifier")
   (sms-template :initarg :sms-template :accessor sms-template
                 :initform nil
                 :documentation "SMS template identifier")
   (active-start-date :initarg :active-start-date :accessor active-start-date
                      :type local-time:timestamp
                      :documentation "When campaign becomes active")
   (active-end-date :initarg :active-end-date :accessor active-end-date
                    :type local-time:timestamp
                    :documentation "When campaign expires")
   (metadata :initarg :metadata :accessor campaign-metadata
             :initform nil
             :documentation "Instance-specific configuration"))
  (:documentation "Specific campaign instance with templates and timing"))

;;; Generic functions for polymorphic behavior
(defgeneric calculate-send-date (email-type contact today)
  (:documentation "Calculate when to send an email based on type and contact info"))

(defgeneric should-respect-exclusions-p (email-type)
  (:documentation "Whether this email type respects exclusion windows"))

(defgeneric get-template (email-type)
  (:documentation "Get the email template for this email type"))

;;; Default methods
(defmethod should-respect-exclusions-p ((email-type anniversary-email))
  t)

(defmethod should-respect-exclusions-p ((email-type campaign-email))
  (respect-exclusions-p email-type))

(defmethod should-respect-exclusions-p ((email-type follow-up-email))
  t)

;;; Pretty printing
(defmethod print-object ((contact contact) stream)
  (print-unreadable-object (contact stream :type t :identity t)
    (format stream "~A (~A)" 
            (contact-email contact)
            (or (contact-state contact) "no state"))))

(defmethod print-object ((schedule email-schedule) stream)
  (print-unreadable-object (schedule stream :type t)
    (format stream "~A on ~A (~A)"
            (type-of (schedule-email-type schedule))
            (scheduled-date schedule)
            (schedule-status schedule))))