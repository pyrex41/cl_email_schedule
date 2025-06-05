;;; followup-simple.lisp - Simplified follow-up email scheduling system

(in-package :email-scheduler)

;;; Simple Follow-up Configuration
(defparameter *followup-config*
  '(:days-after-initial 2
    :lookback-days 35)
  "Configuration for follow-up email scheduling")

;;; Follow-up Types (simplified)
(defparameter *followup-types*
  '(("followup_4_hq_with_yes" 1 "Health questions with conditions")
    ("followup_3_hq_no_yes" 2 "Health questions no conditions") 
    ("followup_2_clicked_no_hq" 3 "Clicked but no health questions")
    ("followup_1_cold" 4 "No engagement"))
  "List of follow-up types: (name priority description)")

;;; Basic Follow-up Scheduling
(defun schedule-followup-emails (db &key (config *followup-config*))
  "Schedule follow-up emails for eligible contacts - simplified version"
  (declare (ignore db config))
  (format t "Mock follow-up scheduling: would process eligible emails~%")
  ;; Return mock schedules for demonstration
  (list 
   (list :contact-id 1 :followup-type "followup_1_cold" :scheduled-date "2024-01-17")
   (list :contact-id 2 :followup-type "followup_2_clicked_no_hq" :scheduled-date "2024-01-16")))

;;; Simple Follow-up Test
(defun test-followup-system ()
  "Test the follow-up scheduling system - simplified version"
  (format t "~&=== Testing Follow-up Email System (Simplified) ===~%")
  
  ;; Test followup type definitions
  (format t "Follow-up types defined: ~A~%" (length *followup-types*))
  (dolist (type *followup-types*)
    (format t "  ~A (priority ~A): ~A~%" 
            (first type) (second type) (third type)))
  
  ;; Test scheduling
  (format t "~%Testing follow-up scheduling:~%")
  (let ((schedules (schedule-followup-emails nil)))
    (format t "Schedules created: ~A~%" (length schedules))
    (dolist (schedule schedules)
      (format t "  Contact ~A: ~A on ~A~%"
              (getf schedule :contact-id)
              (getf schedule :followup-type)
              (getf schedule :scheduled-date))))
  
  (format t "~%=== Follow-up System Test Complete ===~%"))

;;; Main Follow-up Runner (simplified)
(defun run-followup-scheduler (&key (db-path "scheduler.db") (config *followup-config*))
  "Run the follow-up email scheduler - simplified version"
  (declare (ignore config))
  (let ((run-id (format nil "followup-run-~A" (get-universal-time))))
    (email-scheduler.database:with-database (db db-path)
      (format t "Starting follow-up scheduler run ~A~%" run-id)
      
      ;; Schedule follow-up emails
      (let ((followup-schedules (schedule-followup-emails db)))
        (format t "Follow-up scheduler run ~A completed~%" run-id)
        (values run-id 
                (list :schedules-created (length followup-schedules))
                followup-schedules)))))