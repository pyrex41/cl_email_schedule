;;; conditions.lisp - Simplified error handling without logging dependencies

(in-package :email-scheduler.conditions)

;;; Define condition hierarchy
(define-condition scheduler-error (error) 
  ()
  (:documentation "Base class for all scheduler errors"))

(define-condition database-error (scheduler-error)
  ((query :initarg :query :reader error-query
          :documentation "SQL query that caused the error")
   (db-error :initarg :db-error :reader error-db-error
             :documentation "Underlying database error"))
  (:report (lambda (condition stream)
             (format stream "Database error executing query: ~A~%Error: ~A"
                     (error-query condition)
                     (error-db-error condition))))
  (:documentation "Database operation failed"))

(define-condition invalid-contact-error (scheduler-error)
  ((contact-id :initarg :contact-id :reader error-contact-id
               :documentation "ID of the invalid contact")
   (reason :initarg :reason :reader error-reason
           :documentation "Reason why contact is invalid"))
  (:report (lambda (condition stream)
             (format stream "Invalid contact ~A: ~A"
                     (error-contact-id condition)
                     (error-reason condition))))
  (:documentation "Contact data is invalid or incomplete"))

(define-condition configuration-error (scheduler-error)
  ((config-key :initarg :config-key :reader error-config-key
               :documentation "Configuration key that is invalid")
   (value :initarg :value :reader error-value
          :documentation "Invalid configuration value"))
  (:report (lambda (condition stream)
             (format stream "Configuration error for key ~A: ~A"
                     (error-config-key condition)
                     (error-value condition))))
  (:documentation "Configuration is invalid or missing"))

(define-condition processing-error (scheduler-error)
  ((batch-id :initarg :batch-id :reader error-batch-id
             :documentation "ID of the batch being processed")
   (contacts :initarg :contacts :reader error-contacts
             :documentation "Contacts that failed to process"))
  (:report (lambda (condition stream)
             (format stream "Processing error in batch ~A: ~A contacts failed"
                     (error-batch-id condition)
                     (length (error-contacts condition)))))
  (:documentation "Batch processing failed"))

;;; Restart definitions
(defun skip-contact (condition)
  "Skip the current contact and continue processing"
  (declare (ignore condition))
  (invoke-restart 'skip-contact))

(defun retry-batch (condition)
  "Retry processing the current batch"
  (declare (ignore condition))
  (invoke-restart 'retry-batch))

(defun use-default-config (condition)
  "Use default configuration value"
  (declare (ignore condition))
  (invoke-restart 'use-default))

(defun abort-processing (condition)
  "Abort all processing"
  (declare (ignore condition))
  (invoke-restart 'abort-processing))

;;; Handler functions
(defun handle-invalid-contact (condition)
  "Handler for invalid contact errors"
  (format *error-output* "WARNING: Invalid contact encountered: ~A~%" condition)
  (skip-contact condition))

(defun handle-database-error (condition)
  "Handler for database errors"
  (format *error-output* "ERROR: Database error: ~A~%" condition)
  ;; For database errors, we might want to retry or abort
  (restart-case (error condition)
    (retry-batch ()
      :report "Retry the current batch"
      (format *error-output* "INFO: Retrying batch after database error~%"))
    (abort-processing ()
      :report "Abort all processing"
      (format *error-output* "ERROR: Aborting processing due to database error~%")
      (error condition))))

(defun handle-config-error (condition)
  "Handler for configuration errors"
  (format *error-output* "WARNING: Configuration error: ~A~%" condition)
  (use-default-config condition))

(defun handle-processing-error (condition)
  "Handler for batch processing errors"
  (format *error-output* "ERROR: Processing error: ~A~%" condition)
  (restart-case (error condition)
    (skip-batch ()
      :report "Skip this batch and continue"
      (format *error-output* "WARNING: Skipping batch ~A due to processing error~%" (error-batch-id condition)))
    (retry-batch ()
      :report "Retry this batch"
      (format *error-output* "INFO: Retrying batch ~A~%" (error-batch-id condition)))
    (abort-processing ()
      :report "Abort all processing"
      (format *error-output* "ERROR: Aborting due to processing error~%")
      (error condition))))

;;; Handler macros
(defmacro with-error-handling (&body body)
  "Wrap body with comprehensive error handling"
  `(handler-bind
       ((invalid-contact-error #'handle-invalid-contact)
        (database-error #'handle-database-error)
        (configuration-error #'handle-config-error)
        (processing-error #'handle-processing-error))
     ,@body))

(defmacro with-contact-processing ((contact) &body body)
  "Wrap contact processing with restart options"
  `(restart-case
       (progn ,@body)
     (skip-contact ()
       :report "Skip this contact and continue"
       (format *error-output* "WARNING: Skipping contact ~A~%" (contact-id ,contact))
       nil)
     (use-placeholder ()
       :report "Use placeholder data for this contact"
       (format *error-output* "INFO: Using placeholder schedule for contact ~A~%" (contact-id ,contact))
       (make-placeholder-schedule ,contact))))

;;; Utility functions for error handling
(defun make-placeholder-schedule (contact)
  "Create a placeholder schedule for invalid contact"
  ;; Import the required symbols locally
  (let ((email-schedule (find-symbol "EMAIL-SCHEDULE" :email-scheduler.domain))
        (contact-id (find-symbol "CONTACT-ID" :email-scheduler.domain)))
    (when (and email-schedule contact-id)
      (make-instance email-schedule
                     :contact-id (funcall contact-id contact)
                     :email-type 'placeholder
                     :scheduled-date (local-time:today)
                     :status :skipped
                     :skip-reason "Invalid contact data"
                     :priority 999))))

(defun valid-contact-p (contact)
  "Check if contact has required data"
  (let ((contact-id (find-symbol "CONTACT-ID" :email-scheduler.domain))
        (contact-email (find-symbol "CONTACT-EMAIL" :email-scheduler.domain))
        (contact-state (find-symbol "CONTACT-STATE" :email-scheduler.domain)))
    (and contact-id contact-email contact-state
         (funcall contact-id contact)
         (funcall contact-email contact)
         (funcall contact-state contact))))

;;; Signal helpers
(defun signal-invalid-contact (contact-id reason)
  "Signal an invalid contact error"
  (error 'invalid-contact-error
         :contact-id contact-id
         :reason reason))

(defun signal-database-error (query db-error)
  "Signal a database error"
  (error 'database-error
         :query query
         :db-error db-error))

(defun signal-configuration-error (config-key value)
  "Signal a configuration error"
  (error 'configuration-error
         :config-key config-key
         :value value))

(defun signal-processing-error (batch-id contacts)
  "Signal a processing error"
  (error 'processing-error
         :batch-id batch-id
         :contacts contacts))