;;; followup-scheduler.lisp - Follow-up email scheduling system

(in-package :email-scheduler)

;;; Follow-up Email Configuration
(defparameter *followup-config*
  '(:days-after-initial 2
    :lookback-days 35
    :default-template-prefix "followup_"
    :behavior-priority-order (:hq-with-yes :hq-no-yes :clicked-no-hq :cold))
  "Configuration for follow-up email scheduling")

;;; Follow-up Email Types (as defined in business logic)
(defclass followup-email-type ()
  ((type-name :initarg :type-name :reader followup-type-name
              :documentation "Type identifier for follow-up email")
   (priority :initarg :priority :reader followup-priority
             :documentation "Priority for behavior-based selection (lower = higher priority)")
   (description :initarg :description :reader followup-description
                :documentation "Human-readable description")
   (requires-behavior :initarg :requires-behavior :reader followup-requires-behavior
                      :documentation "Required user behavior to trigger this follow-up type"))
  (:documentation "Base class for follow-up email types"))

;;; Define the 4 follow-up email types from business logic
(defparameter *followup-email-types*
  (list
   (make-instance 'followup-email-type
                  :type-name "followup_4_hq_with_yes"
                  :priority 1
                  :description "Contact answered health questions with medical conditions"
                  :requires-behavior :answered-hq-with-conditions)
   
   (make-instance 'followup-email-type
                  :type-name "followup_3_hq_no_yes"
                  :priority 2
                  :description "Contact answered health questions with no medical conditions"
                  :requires-behavior :answered-hq-no-conditions)
   
   (make-instance 'followup-email-type
                  :type-name "followup_2_clicked_no_hq"
                  :priority 3
                  :description "Contact clicked a link but didn't answer health questions"
                  :requires-behavior :clicked-no-hq)
   
   (make-instance 'followup-email-type
                  :type-name "followup_1_cold"
                  :priority 4
                  :description "Contact didn't click or answer health questions"
                  :requires-behavior :no-engagement))
  "Registry of all follow-up email types")

;;; Follow-up Eligibility Checking
(defun get-eligible-initial-emails (db &key (lookback-days 35))
  "Get initial emails that are eligible for follow-ups"
  ;; For now, simulate this - in full implementation this would query email_schedules
  ;; and communication logs to find emails with status 'sent' or 'delivered'
  (declare (ignore db lookback-days))
  ;; Return mock data for demonstration
  (list
   (list :contact-id 1 :email-type "birthday" :sent-date "2024-01-15" :campaign-instance-id nil)
   (list :contact-id 2 :email-type "campaign_rate_increase" :sent-date "2024-01-14" :campaign-instance-id 123)
   (list :contact-id 3 :email-type "effective_date" :sent-date "2024-01-13" :campaign-instance-id nil)))

(defun already-has-followup-p (db contact-id initial-email-type)
  "Check if contact already has a follow-up scheduled or sent for this initial email"
  ;; Query email_schedules for existing follow-up emails
  (declare (ignore db contact-id initial-email-type))
  ;; For now, return false - in full implementation this would check the database
  nil)

(defun is-followup-eligible-email-type-p (email-type campaign-enables-followups-p)
  "Check if an email type is eligible for follow-ups"
  (or 
   ;; Anniversary-based emails are always eligible
   (member email-type '("birthday" "effective_date" "aep" "post_window") :test #'string=)
   ;; Campaign-based emails are eligible if the campaign enables follow-ups
   (and (alexandria:starts-with-subseq "campaign_" email-type)
        campaign-enables-followups-p)))

;;; User Behavior Analysis
(defun analyze-contact-behavior (db contact-id initial-email-id)
  "Analyze contact behavior to determine appropriate follow-up type"
  ;; In full implementation, this would query:
  ;; - tracking_clicks table for click behavior
  ;; - contact_events table for health question responses
  ;; - metadata for medical conditions information
  (declare (ignore db contact-id initial-email-id))
  
  ;; For demonstration, simulate different behaviors
  (let ((random-behavior (random 4)))
    (case random-behavior
      (0 :answered-hq-with-conditions)
      (1 :answered-hq-no-conditions)
      (2 :clicked-no-hq)
      (3 :no-engagement))))

(defun get-followup-type-for-behavior (behavior)
  "Get the appropriate follow-up type for a behavior"
  (find behavior *followup-email-types*
        :key #'followup-requires-behavior))

;;; Follow-up Schedule Creation
(defun create-followup-schedule (contact-id initial-email followup-type-obj &key (days-after 2))
  "Create a follow-up email schedule"
  (let* ((initial-sent-date (getf initial-email :sent-date))
         (followup-date (email-scheduler.date-utils:add-days 
                         (local-time:parse-timestring initial-sent-date)
                         days-after))
         (followup-type-name (followup-type-name followup-type-obj))
         (initial-campaign-id (getf initial-email :campaign-instance-id)))
    
    (make-instance 'email-scheduler.domain:email-schedule
                   :contact-id contact-id
                   :email-type (make-instance 'email-scheduler.domain:followup-1-cold)
                   :scheduled-date followup-date
                   :status :pre-scheduled
                   :priority (if initial-campaign-id
                                ;; Inherit priority from campaign
                                (get-campaign-priority initial-campaign-id)
                                ;; Default priority for anniversary-based follow-ups
                                8)
                   :campaign-instance-id initial-campaign-id
                   :email-template followup-type-name
                   :metadata (list :initial-email-type (getf initial-email :email-type)
                                   :initial-email-id (getf initial-email :email-id)
                                   :behavior (followup-requires-behavior followup-type-obj)
                                   :followup-reasoning (followup-description followup-type-obj)))))

(defun get-campaign-priority (campaign-instance-id)
  "Get priority for a campaign instance - placeholder implementation"
  (declare (ignore campaign-instance-id))
  5) ; Default campaign priority

;;; Main Follow-up Scheduling Logic
(defun schedule-followup-emails (db &key (config *followup-config*))
  "Schedule follow-up emails for eligible contacts"
  (let ((lookback-days (getf config :lookback-days 35))
        (days-after (getf config :days-after-initial 2))
        (scheduled-followups '()))
    
    ;; Get eligible initial emails
    (let ((eligible-emails (get-eligible-initial-emails db :lookback-days lookback-days)))
      (format t "Found ~A eligible emails for follow-up processing~%" (length eligible-emails))
      
      ;; Process each eligible email
      (dolist (initial-email eligible-emails)
        (let ((contact-id (getf initial-email :contact-id))
              (email-type (getf initial-email :email-type)))
          
          ;; Check if already has follow-up
          (unless (already-has-followup-p db contact-id email-type)
            
            ;; Check if email type is eligible for follow-ups
            (when (is-followup-eligible-email-type-p email-type t) ; Assume campaign enables follow-ups for now
              
              ;; Analyze contact behavior
              (let* ((behavior (analyze-contact-behavior db contact-id (getf initial-email :email-id)))
                     (followup-type (get-followup-type-for-behavior behavior)))
                
                (when followup-type
                  ;; Create follow-up schedule
                  (let ((followup-schedule (create-followup-schedule 
                                            contact-id initial-email followup-type 
                                            :days-after days-after)))
                    (push followup-schedule scheduled-followups)
                    (format t "Scheduled ~A follow-up for contact ~A (behavior: ~A)~%"
                            (followup-type-name followup-type) contact-id behavior))))))))
    
    (format t "Total follow-ups scheduled: ~A~%" (length scheduled-followups))
    scheduled-followups))

;;; Database Integration Functions
(defun insert-followup-schedules-batch (db followup-schedules run-id)
  "Insert a batch of follow-up email schedules into the database"
  (dolist (schedule followup-schedules)
    ;; Use the existing email schedule insertion logic
    (email-scheduler.database:insert-schedules-batch db (list schedule) run-id)))

(defun get-followup-statistics (db run-id)
  "Get statistics about follow-up emails scheduled in a run"
  (let ((followup-types '("followup_1_cold" "followup_2_clicked_no_hq" 
                          "followup_3_hq_no_yes" "followup_4_hq_with_yes")))
    (loop for followup-type in followup-types
          collect (list followup-type 
                        (email-scheduler.database:count-schedules 
                         db :run-id run-id :status "pre-scheduled")))))

;;; Integration with Main Scheduler
(defun run-followup-scheduler (&key (db-path "scheduler.db") (config *followup-config*))
  "Run the follow-up email scheduler"
  (let ((run-id (format nil "followup-run-~A" (get-universal-time))))
    (email-scheduler.database:with-database (db db-path)
      (format t "Starting follow-up scheduler run ~A~%" run-id)
      
      ;; Schedule follow-up emails
      (let ((followup-schedules (schedule-followup-emails db :config config)))
        
        ;; Insert into database
        (when followup-schedules
          (email-scheduler.database:with-transaction (db)
            (insert-followup-schedules-batch db followup-schedules run-id)))
        
        ;; Get statistics
        (let ((stats (get-followup-statistics db run-id)))
          (format t "Follow-up scheduler run ~A completed~%" run-id)
          (values run-id stats followup-schedules))))))

;;; Campaign-Aware Follow-up Processing
(defun should-campaign-generate-followups-p (campaign-instance-id db)
  "Check if a campaign instance should generate follow-up emails"
  ;; Query campaign_instances table to get campaign_type,
  ;; then query campaign_types table to check enable_followups flag
  (declare (ignore campaign-instance-id db))
  ;; For now, assume true - in full implementation this would query the database
  t)

(defun get-campaign-followup-templates (campaign-instance-id db)
  "Get custom follow-up templates for a campaign instance if any"
  ;; Check campaign_instances.metadata for custom follow-up template overrides
  (declare (ignore campaign-instance-id db))
  ;; For now, return nil to use default templates
  nil)

;;; Follow-up Re-evaluation and Updates
(defun update-followup-type-if-behavior-changed (db contact-id followup-schedule-id)
  "Re-evaluate and potentially update follow-up type if contact behavior has changed"
  ;; This allows for dynamic follow-up type updates before sending
  ;; Check if contact behavior has changed since follow-up was scheduled
  (declare (ignore db contact-id followup-schedule-id))
  ;; Implementation would:
  ;; 1. Re-analyze contact behavior
  ;; 2. Compare with scheduled follow-up type
  ;; 3. Update if higher priority follow-up type is now appropriate
  nil)

;;; Batch Processing for Performance
(defun process-followups-in-batches (db batch-size &key (config *followup-config*))
  "Process follow-up scheduling in batches for better performance"
  (let ((batch-size (or batch-size 1000))
        (total-processed 0)
        (total-scheduled 0))
    
    ;; This would implement batch processing of contacts for follow-up evaluation
    ;; For now, demonstrate the concept
    (format t "Processing follow-ups in batches of ~A~%" batch-size)
    
    ;; Simulate batch processing
    (let ((num-batches 3))
      (dotimes (batch-num num-batches)
        (format t "Processing batch ~A/~A~%" (1+ batch-num) num-batches)
        (let ((batch-followups (schedule-followup-emails db :config config)))
          (incf total-processed batch-size)
          (incf total-scheduled (length batch-followups)))))
    
    (format t "Batch processing complete: ~A contacts processed, ~A follow-ups scheduled~%"
            total-processed total-scheduled)
    (values total-processed total-scheduled)))

;;; Utilities and Testing
(defun simulate-followup-behavior-analysis ()
  "Simulate different contact behaviors for testing"
  (let ((behaviors '(:answered-hq-with-conditions :answered-hq-no-conditions 
                     :clicked-no-hq :no-engagement)))
    (mapcar (lambda (behavior)
              (list behavior (get-followup-type-for-behavior behavior)))
            behaviors)))

(defun test-followup-system ()
  "Test the follow-up scheduling system"
  (format t "~&=== Testing Follow-up Email System ===~%")
  
  ;; Test followup type definitions
  (format t "Follow-up types defined: ~A~%" (length *followup-email-types*))
  (dolist (type *followup-email-types*)
    (format t "  ~A (priority ~A): ~A~%" 
            (followup-type-name type)
            (followup-priority type)
            (followup-description type)))
  
  ;; Test behavior analysis simulation
  (format t "~%Behavior analysis simulation:~%")
  (let ((behavior-tests (simulate-followup-behavior-analysis)))
    (dolist (test behavior-tests)
      (format t "  ~A -> ~A~%" 
              (first test) 
              (when (second test) (followup-type-name (second test))))))
  
  ;; Test with temporary database
  (let ((test-db "test-followup.db"))
    (when (probe-file test-db) (delete-file test-db))
    (unwind-protect
        (progn
          (email-scheduler.database:with-database (db test-db)
            (email-scheduler.database:create-database-schema db)
            (multiple-value-bind (run-id stats schedules)
                (run-followup-scheduler :db-path test-db)
              (format t "~%Test run completed:~%")
              (format t "  Run ID: ~A~%" run-id)
              (format t "  Schedules created: ~A~%" (length schedules))
              (format t "  Statistics: ~A~%" stats))))
      (when (probe-file test-db) (delete-file test-db))))
  
  (format t "~%=== Follow-up System Test Complete ===~%"))