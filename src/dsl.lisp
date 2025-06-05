;;; dsl.lisp

(in-package :email-scheduler.dsl)

;;; Macro for defining state rules
(defmacro defstate (state-name &body rules)
  "Define exclusion rules for a state using DSL"
  `(setf (get ',state-name 'state-rules)
         (list ,@rules)))

;;; DSL functions for rule types
(defun birthday-window (&key before after (use-month-start nil))
  "Define a birthday-based exclusion window"
  `(:birthday-window :before ,before :after ,after :use-month-start ,use-month-start))

(defun effective-date-window (&key before after)
  "Define an effective-date-based exclusion window"
  `(:effective-date-window :before ,before :after ,after))

(defun year-round-exclusion ()
  "Define year-round exclusion (no emails ever)"
  `(:year-round-exclusion))

(defun no-exclusion ()
  "Define no exclusion rules"
  `(:no-exclusion))

;;; Macro for defining email scheduling rules
(defmacro defrule (name (contact date) &body body)
  "Define a scheduling rule with name and parameters"
  `(defmethod apply-rule ((rule (eql ',name)) ,contact ,date)
     ,@body))

;;; DSL for expressing scheduling decisions
(defmacro schedule-email (email-type date &key (priority 10) template metadata)
  "Create a scheduled email with given parameters"
  `(make-instance 'email-schedule
                  :email-type ,email-type
                  :scheduled-date ,date
                  :status :pre-scheduled
                  :priority ,priority
                  ,@(when template `(:template ,template))
                  ,@(when metadata `(:metadata ,metadata))))

(defmacro skip-email (email-type reason &optional resume-date)
  "Create a skipped email with reason and optional resume date"
  `(make-instance 'email-schedule
                  :email-type ,email-type
                  :status :skipped
                  :skip-reason ,reason
                  ,@(when resume-date `(:resume-date ,resume-date))))

;;; Campaign definition DSL
(defmacro defcampaign (name &body options)
  "Define a campaign type with configuration options"
  (let ((respect-exclusions (getf options :respect-exclusions t))
        (enable-followups (getf options :enable-followups t))
        (days-before (getf options :days-before 0))
        (priority (getf options :priority 10))
        (target-all (getf options :target-all-contacts nil)))
    `(setf (get ',name 'campaign-config)
           (list :respect-exclusions ,respect-exclusions
                 :enable-followups ,enable-followups
                 :days-before ,days-before
                 :priority ,priority
                 :target-all-contacts ,target-all))))

;;; Reader macro for dates (optional - provides @"2024-03-15" syntax)
(defun enable-date-reader-macro ()
  "Enable the @ reader macro for date literals"
  (set-macro-character #\@
    (lambda (stream char)
      (declare (ignore char))
      (let ((date-string (read stream t nil t)))
        `(local-time:parse-timestring ,date-string)))))

;;; Utility macros for rule conditions
(defmacro when-state (state &body body)
  "Execute body only if contact is in specified state"
  `(when (eq (contact-state contact) ,state)
     ,@body))

(defmacro when-birthday (&body body)
  "Execute body only if contact has a birthday"
  `(when (contact-birthday contact)
     ,@body))

(defmacro when-effective-date (&body body)
  "Execute body only if contact has an effective date"
  `(when (contact-effective-date contact)
     ,@body))

;;; Date arithmetic DSL helpers
(defmacro days-before (date n)
  "Get date N days before given date"
  `(local-time:timestamp- ,date ,n :day))

(defmacro days-after (date n)
  "Get date N days after given date"
  `(local-time:timestamp+ ,date ,n :day))

;;; Configuration helpers
(defmacro with-config ((config-var config) &body body)
  "Bind configuration variable for easy access"
  `(let ((,config-var ,config))
     ,@body))

(defun config-get (config key &optional default)
  "Get configuration value with optional default"
  (getf config key default))

;;; Validation helpers for DSL
(defun validate-state-rule (rule)
  "Validate that a state rule is well-formed"
  (let ((rule-type (first rule)))
    (case rule-type
      (:birthday-window
       (let ((before (getf rule :before))
             (after (getf rule :after)))
         (and (integerp before) (>= before 0)
              (integerp after) (>= after 0))))
      (:effective-date-window
       (let ((before (getf rule :before))
             (after (getf rule :after)))
         (and (integerp before) (>= before 0)
              (integerp after) (>= after 0))))
      ((:year-round-exclusion :no-exclusion) t)
      (t nil))))

(defun validate-campaign-config (config)
  "Validate that a campaign configuration is well-formed"
  (and (booleanp (getf config :respect-exclusions))
       (booleanp (getf config :enable-followups))
       (integerp (getf config :days-before))
       (integerp (getf config :priority))
       (booleanp (getf config :target-all-contacts))))

;;; Introspection helpers
(defun list-states ()
  "List all defined states with their rules"
  (let ((states '()))
    (do-symbols (sym)
      (when (get sym 'state-rules)
        (push (cons sym (get sym 'state-rules)) states)))
    states))

(defun list-campaigns ()
  "List all defined campaign types with their configurations"
  (let ((campaigns '()))
    (do-symbols (sym)
      (when (get sym 'campaign-config)
        (push (cons sym (get sym 'campaign-config)) campaigns)))
    campaigns))

(defun describe-state (state)
  "Describe the rules for a given state"
  (let ((rules (get state 'state-rules)))
    (if rules
        (format t "~&State ~A has rules:~%" state)
        (format t "~&State ~A has no rules defined.~%" state))
    (dolist (rule rules)
      (format t "  ~S~%" rule))))

(defun describe-campaign (campaign)
  "Describe the configuration for a given campaign"
  (let ((config (get campaign 'campaign-config)))
    (if config
        (progn
          (format t "~&Campaign ~A configuration:~%" campaign)
          (format t "  Respect exclusions: ~A~%" (getf config :respect-exclusions))
          (format t "  Enable followups: ~A~%" (getf config :enable-followups))
          (format t "  Days before event: ~A~%" (getf config :days-before))
          (format t "  Priority: ~A~%" (getf config :priority))
          (format t "  Target all contacts: ~A~%" (getf config :target-all-contacts)))
        (format t "~&Campaign ~A has no configuration defined.~%" campaign))))