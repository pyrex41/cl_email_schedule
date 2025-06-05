;;; rules.lisp

(in-package :email-scheduler.rules)

;;; Define state rules using the DSL

;; Birthday window rules
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

;; Effective date window rules
(defstate :mo
  (effective-date-window :before 30 :after 33))

;; Year-round exclusion rules
(defstate :ct (year-round-exclusion))
(defstate :ma (year-round-exclusion))
(defstate :ny (year-round-exclusion))
(defstate :wa (year-round-exclusion))

;; Default rule for other states
(defstate :other (no-exclusion))

;;; Campaign definitions using DSL
(defcampaign rate-increase
  :respect-exclusions t
  :enable-followups t
  :days-before 14
  :priority 1
  :target-all-contacts nil)

(defcampaign seasonal-promo
  :respect-exclusions t
  :enable-followups t
  :days-before 7
  :priority 5
  :target-all-contacts nil)

(defcampaign initial-blast
  :respect-exclusions nil
  :enable-followups nil
  :days-before 0
  :priority 10
  :target-all-contacts t)

;;; Rule application with multiple dispatch
(defgeneric apply-exclusion-rule (rule-type contact date rule-params)
  (:documentation "Apply specific exclusion rule type to contact on given date"))

(defmethod apply-exclusion-rule ((rule-type (eql :birthday-window)) contact date params)
  "Apply birthday window exclusion rule"
  (when (contact-birthday contact)
    (let* ((before (getf params :before))
           (after (getf params :after))
           (use-month-start-p (getf params :use-month-start))
           (birthday (if use-month-start-p
                        (beginning-of-month (contact-birthday contact))
                        (contact-birthday contact))))
      (date-in-window-p date birthday before after))))

(defmethod apply-exclusion-rule ((rule-type (eql :effective-date-window)) contact date params)
  "Apply effective date window exclusion rule"
  (when (contact-effective-date contact)
    (let ((before (getf params :before))
          (after (getf params :after)))
      (date-in-window-p date (contact-effective-date contact) before after))))

(defmethod apply-exclusion-rule ((rule-type (eql :year-round-exclusion)) contact date params)
  "Apply year-round exclusion rule"
  (declare (ignore contact date params))
  t)

(defmethod apply-exclusion-rule ((rule-type (eql :no-exclusion)) contact date params)
  "Apply no exclusion rule"
  (declare (ignore contact date params))
  nil)

;;; Main rule application
(defun apply-state-rules (contact date)
  "Apply all rules for a contact's state, returning exclusion status and details"
  (let ((state-rules (get (contact-state contact) 'state-rules)))
    (loop for rule in state-rules
          for rule-type = (first rule)
          for params = (rest rule)
          when (apply-exclusion-rule rule-type contact date params)
            return (values t rule-type params)
          finally (return (values nil nil nil)))))

(defun in-exclusion-window-p (contact date &key (pre-window-days 60))
  "Check if contact is in exclusion window on given date with pre-window extension"
  (let ((state-rules (get (contact-state contact) 'state-rules)))
    (loop for rule in state-rules
          for rule-type = (first rule)
          for params = (rest rule)
          when (case rule-type
                 (:birthday-window
                  (when (contact-birthday contact)
                    (let* ((before (+ (getf params :before) pre-window-days))
                           (after (getf params :after))
                           (use-month-start-p (getf params :use-month-start))
                           (birthday (if use-month-start-p
                                        (beginning-of-month (contact-birthday contact))
                                        (contact-birthday contact))))
                      (date-in-window-p date birthday before after))))
                 (:effective-date-window
                  (when (contact-effective-date contact)
                    (let ((before (+ (getf params :before) pre-window-days))
                          (after (getf params :after)))
                      (date-in-window-p date (contact-effective-date contact) before after))))
                 (:year-round-exclusion t)
                 (:no-exclusion nil))
            return t
          finally (return nil))))

(defun exclusion-window-end-date (contact reference-date)
  "Calculate when the exclusion window ends for a contact relative to reference date"
  (let ((state-rules (get (contact-state contact) 'state-rules)))
    (loop for rule in state-rules
          for rule-type = (first rule)
          for params = (rest rule)
          when (case rule-type
                 (:birthday-window
                  (when (contact-birthday contact)
                    (let* ((after (getf params :after))
                           (use-month-start-p (getf params :use-month-start))
                           (birthday (if use-month-start-p
                                        (beginning-of-month (contact-birthday contact))
                                        (contact-birthday contact))))
                      (add-days birthday after))))
                 (:effective-date-window
                  (when (contact-effective-date contact)
                    (let ((after (getf params :after)))
                      (add-days (contact-effective-date contact) after))))
                 (:year-round-exclusion nil) ; No end date for year-round exclusion
                 (:no-exclusion nil))
            return it
          finally (return nil))))

(defun calculate-exclusion-window (contact)
  "Calculate the full exclusion window for a contact"
  (let ((state-rules (get (contact-state contact) 'state-rules))
        (windows '()))
    (dolist (rule state-rules)
      (let ((rule-type (first rule))
            (params (rest rule)))
        (case rule-type
          (:birthday-window
           (when (contact-birthday contact)
             (let* ((before (getf params :before))
                    (after (getf params :after))
                    (use-month-start-p (getf params :use-month-start))
                    (birthday (if use-month-start-p
                                 (beginning-of-month (contact-birthday contact))
                                 (contact-birthday contact)))
                    (start (subtract-days birthday before))
                    (end (add-days birthday after)))
               (push (list :birthday-window start end birthday) windows))))
          (:effective-date-window
           (when (contact-effective-date contact)
             (let* ((before (getf params :before))
                    (after (getf params :after))
                    (effective-date (contact-effective-date contact))
                    (start (subtract-days effective-date before))
                    (end (add-days effective-date after)))
               (push (list :effective-date-window start end effective-date) windows))))
          (:year-round-exclusion
           (push (list :year-round-exclusion :always :always :always) windows))
          (:no-exclusion
           ;; No window for no-exclusion rule
           ))))
    windows))

;;; Helper functions for specific rule types
(defun birthday-exclusion-p (contact date &key (pre-window-days 60))
  "Check if date falls in birthday exclusion window"
  (when (contact-birthday contact)
    (let ((rules (get (contact-state contact) 'state-rules)))
      (dolist (rule rules)
        (when (eq (first rule) :birthday-window)
          (let* ((params (rest rule))
                 (before (+ (getf params :before) pre-window-days))
                 (after (getf params :after))
                 (use-month-start-p (getf params :use-month-start))
                 (birthday (if use-month-start-p
                              (beginning-of-month (contact-birthday contact))
                              (contact-birthday contact))))
            (when (date-in-window-p date birthday before after)
              (return t))))))))

(defun effective-date-exclusion-p (contact date &key (pre-window-days 60))
  "Check if date falls in effective date exclusion window"
  (when (contact-effective-date contact)
    (let ((rules (get (contact-state contact) 'state-rules)))
      (dolist (rule rules)
        (when (eq (first rule) :effective-date-window)
          (let* ((params (rest rule))
                 (before (+ (getf params :before) pre-window-days))
                 (after (getf params :after)))
            (when (date-in-window-p date (contact-effective-date contact) before after)
              (return t))))))))

(defun year-round-exclusion-p (contact)
  "Check if contact is in a year-round exclusion state"
  (let ((rules (get (contact-state contact) 'state-rules)))
    (some (lambda (rule) (eq (first rule) :year-round-exclusion)) rules)))

;;; Campaign rule helpers
(defun get-campaign-config (campaign-name)
  "Get configuration for a campaign type"
  (get campaign-name 'campaign-config))

(defun campaign-respects-exclusions-p (campaign-name)
  "Check if campaign respects exclusion windows"
  (let ((config (get-campaign-config campaign-name)))
    (getf config :respect-exclusions t)))

(defun campaign-enables-followups-p (campaign-name)
  "Check if campaign enables follow-up emails"
  (let ((config (get-campaign-config campaign-name)))
    (getf config :enable-followups t)))

(defun campaign-days-before-event (campaign-name)
  "Get days before event for campaign"
  (let ((config (get-campaign-config campaign-name)))
    (getf config :days-before 0)))

(defun campaign-priority (campaign-name)
  "Get priority for campaign"
  (let ((config (get-campaign-config campaign-name)))
    (getf config :priority 10)))

(defun campaign-targets-all-contacts-p (campaign-name)
  "Check if campaign targets all contacts"
  (let ((config (get-campaign-config campaign-name)))
    (getf config :target-all-contacts nil)))