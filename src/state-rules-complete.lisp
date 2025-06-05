;;; state-rules-complete.lisp - Complete implementation of all 50 state rules

(in-package :email-scheduler.rules)

;;; This file implements the complete set of state rules as specified in the business logic
;;; Currently the business logic only specifies 12 states with explicit rules,
;;; with the remaining 38 states following the default "no-exclusion" rule.

;;; Birthday Window Rules (30-63 days after birthday)
;;; CA: 30 days before to 60 days after birthday
(defstate :ca
  (birthday-window :before 30 :after 60))

;;; ID: 0 days before to 63 days after birthday  
(defstate :id
  (birthday-window :before 0 :after 63))

;;; KY: 0 days before to 60 days after birthday
(defstate :ky
  (birthday-window :before 0 :after 60))

;;; MD: 0 days before to 30 days after birthday
(defstate :md
  (birthday-window :before 0 :after 30))

;;; NV: 0 days before to 60 days after birthday (uses month start of birthday month)
(defstate :nv
  (birthday-window :before 0 :after 60 :use-month-start t))

;;; OK: 0 days before to 60 days after birthday
(defstate :ok
  (birthday-window :before 0 :after 60))

;;; OR: 0 days before to 31 days after birthday
(defstate :or
  (birthday-window :before 0 :after 31))

;;; VA: 0 days before to 30 days after birthday
(defstate :va
  (birthday-window :before 0 :after 30))

;;; Effective Date Window Rules
;;; MO: 30 days before to 33 days after effective date anniversary
(defstate :mo
  (effective-date-window :before 30 :after 33))

;;; Year-Round Exclusion Rules (no marketing emails sent)
(defstate :ct (year-round-exclusion))
(defstate :ma (year-round-exclusion))
(defstate :ny (year-round-exclusion))
(defstate :wa (year-round-exclusion))

;;; All Other States - No Exclusion Rules
;;; These 38 states follow the default "no-exclusion" rule:
;;; AL, AK, AZ, AR, CO, DE, FL, GA, HI, IL, IN, IA, KS, LA, ME, MI, MN, MS, MT, NE, 
;;; NH, NJ, NM, NC, ND, OH, PA, RI, SC, SD, TN, TX, UT, VT, WV, WI, WY, DC

(defstate :al (no-exclusion))
(defstate :ak (no-exclusion))
(defstate :az (no-exclusion))
(defstate :ar (no-exclusion))
(defstate :co (no-exclusion))
(defstate :de (no-exclusion))
(defstate :fl (no-exclusion))
(defstate :ga (no-exclusion))
(defstate :hi (no-exclusion))
(defstate :il (no-exclusion))
(defstate :in (no-exclusion))
(defstate :ia (no-exclusion))
(defstate :ks (no-exclusion))
(defstate :la (no-exclusion))
(defstate :me (no-exclusion))
(defstate :mi (no-exclusion))
(defstate :mn (no-exclusion))
(defstate :ms (no-exclusion))
(defstate :mt (no-exclusion))
(defstate :ne (no-exclusion))
(defstate :nh (no-exclusion))
(defstate :nj (no-exclusion))
(defstate :nm (no-exclusion))
(defstate :nc (no-exclusion))
(defstate :nd (no-exclusion))
(defstate :oh (no-exclusion))
(defstate :pa (no-exclusion))
(defstate :ri (no-exclusion))
(defstate :sc (no-exclusion))
(defstate :sd (no-exclusion))
(defstate :tn (no-exclusion))
(defstate :tx (no-exclusion))
(defstate :ut (no-exclusion))
(defstate :vt (no-exclusion))
(defstate :wv (no-exclusion))
(defstate :wi (no-exclusion))
(defstate :wy (no-exclusion))
(defstate :dc (no-exclusion))

;;; Fallback rule for any unspecified states
(defstate :other (no-exclusion))

;;; State Rules Registry for Easy Access
(defparameter *all-state-rules* 
  '(;; Birthday window states
    (:ca (:birthday-window :before 30 :after 60))
    (:id (:birthday-window :before 0 :after 63))
    (:ky (:birthday-window :before 0 :after 60))
    (:md (:birthday-window :before 0 :after 30))
    (:nv (:birthday-window :before 0 :after 60 :use-month-start t))
    (:ok (:birthday-window :before 0 :after 60))
    (:or (:birthday-window :before 0 :after 31))
    (:va (:birthday-window :before 0 :after 30))
    
    ;; Effective date window states
    (:mo (:effective-date-window :before 30 :after 33))
    
    ;; Year-round exclusion states
    (:ct (:year-round-exclusion))
    (:ma (:year-round-exclusion))
    (:ny (:year-round-exclusion))
    (:wa (:year-round-exclusion))
    
    ;; No exclusion states (remaining 38 states)
    (:al (:no-exclusion)) (:ak (:no-exclusion)) (:az (:no-exclusion)) (:ar (:no-exclusion))
    (:co (:no-exclusion)) (:de (:no-exclusion)) (:fl (:no-exclusion)) (:ga (:no-exclusion))
    (:hi (:no-exclusion)) (:il (:no-exclusion)) (:in (:no-exclusion)) (:ia (:no-exclusion))
    (:ks (:no-exclusion)) (:la (:no-exclusion)) (:me (:no-exclusion)) (:mi (:no-exclusion))
    (:mn (:no-exclusion)) (:ms (:no-exclusion)) (:mt (:no-exclusion)) (:ne (:no-exclusion))
    (:nh (:no-exclusion)) (:nj (:no-exclusion)) (:nm (:no-exclusion)) (:nc (:no-exclusion))
    (:nd (:no-exclusion)) (:oh (:no-exclusion)) (:pa (:no-exclusion)) (:ri (:no-exclusion))
    (:sc (:no-exclusion)) (:sd (:no-exclusion)) (:tn (:no-exclusion)) (:tx (:no-exclusion))
    (:ut (:no-exclusion)) (:vt (:no-exclusion)) (:wv (:no-exclusion)) (:wi (:no-exclusion))
    (:wy (:no-exclusion)) (:dc (:no-exclusion))
    
    ;; Fallback
    (:other (:no-exclusion)))
  "Complete registry of all US state exclusion rules")

;;; Rule Categorization Functions
(defun get-birthday-window-states ()
  "Get all states with birthday window rules"
  '(:ca :id :ky :md :nv :ok :or :va))

(defun get-effective-date-window-states ()
  "Get all states with effective date window rules"
  '(:mo))

(defun get-year-round-exclusion-states ()
  "Get all states with year-round exclusion rules"
  '(:ct :ma :ny :wa))

(defun get-no-exclusion-states ()
  "Get all states with no exclusion rules"
  '(:al :ak :az :ar :co :de :fl :ga :hi :il :in :ia :ks :la :me :mi :mn :ms :mt 
    :ne :nh :nj :nm :nc :nd :oh :pa :ri :sc :sd :tn :tx :ut :vt :wv :wi :wy :dc :other))

(defun get-state-rule-type (state)
  "Get the type of exclusion rule for a state"
  (cond
    ((member state (get-birthday-window-states)) :birthday-window)
    ((member state (get-effective-date-window-states)) :effective-date-window)
    ((member state (get-year-round-exclusion-states)) :year-round-exclusion)
    ((member state (get-no-exclusion-states)) :no-exclusion)
    (t :no-exclusion))) ; Default fallback

(defun get-state-rule-params (state)
  "Get the parameters for a state's exclusion rule"
  (let ((rule-entry (assoc state *all-state-rules*)))
    (when rule-entry
      (rest (second rule-entry)))))

;;; Enhanced Rule Validation Functions
(defun validate-all-state-rules ()
  "Validate that all 50 states + DC + other have rules defined"
  (let ((defined-states (mapcar #'first *all-state-rules*))
        (expected-count 52)) ; 50 states + DC + other
    (values (>= (length defined-states) expected-count)
            (length defined-states)
            expected-count
            defined-states)))

(defun missing-state-rules ()
  "Check for any missing state rules"
  (let ((defined-states (mapcar #'first *all-state-rules*))
        (all-us-states '(:al :ak :az :ar :ca :co :ct :de :fl :ga :hi :id :il :in :ia :ks :ky :la :me 
                         :md :ma :mi :mn :ms :mo :mt :ne :nv :nh :nj :nm :ny :nc :nd :oh :ok :or :pa 
                         :ri :sc :sd :tn :tx :ut :vt :va :wa :wv :wi :wy :dc :other)))
    (set-difference all-us-states defined-states)))

;;; Rule Summary Functions for Debugging and Documentation
(defun summarize-state-rules ()
  "Generate a summary of all state rules for documentation"
  (format t "~&=== Email Scheduling State Rules Summary ===~%")
  (format t "~&Total states defined: ~A~%" (length *all-state-rules*))
  (format t "~&~%Birthday Window States (~A):~%" (length (get-birthday-window-states)))
  (dolist (state (get-birthday-window-states))
    (let ((params (get-state-rule-params state)))
      (format t "  ~A: ~A days before to ~A days after birthday~A~%"
              state
              (getf params :before)
              (getf params :after)
              (if (getf params :use-month-start) " (uses month start)" ""))))
  
  (format t "~&~%Effective Date Window States (~A):~%" (length (get-effective-date-window-states)))
  (dolist (state (get-effective-date-window-states))
    (let ((params (get-state-rule-params state)))
      (format t "  ~A: ~A days before to ~A days after effective date~%"
              state
              (getf params :before)
              (getf params :after))))
  
  (format t "~&~%Year-Round Exclusion States (~A):~%" (length (get-year-round-exclusion-states)))
  (dolist (state (get-year-round-exclusion-states))
    (format t "  ~A: No marketing emails sent year-round~%" state))
  
  (format t "~&~%No Exclusion States (~A):~%" (length (get-no-exclusion-states)))
  (format t "  ~{~A ~}~%" (get-no-exclusion-states))
  
  (let ((missing (missing-state-rules)))
    (when missing
      (format t "~&~%⚠️  Missing rules for: ~{~A ~}~%" missing)))
  
  (format t "~&~%=== Summary Complete ===~%"))

;;; Integration with Existing Rule System
(defun apply-complete-state-rules (contact date)
  "Apply complete state rules with enhanced error handling"
  (handler-case
      (let ((state (contact-state contact)))
        (if (member state (mapcar #'first *all-state-rules*))
            (apply-state-rules contact date)
            (progn
              ;; Log warning about unknown state and apply default rule
              (format t "Warning: Unknown state ~A, applying default no-exclusion rule~%" state)
              (values nil :no-exclusion nil))))
    (error (e)
      (format t "Error applying state rules for ~A: ~A~%" (contact-state contact) e)
      (values nil :error nil))))

;;; Testing and Validation Utilities
(defun test-state-rule-coverage ()
  "Test that we have complete coverage of state rules"
  (multiple-value-bind (valid-p defined-count expected-count defined-states)
      (validate-all-state-rules)
    (format t "~&State rule coverage test:~%")
    (format t "  Expected: ~A states~%" expected-count)
    (format t "  Defined:  ~A states~%" defined-count)
    (format t "  Valid:    ~A~%" valid-p)
    (when (not valid-p)
      (let ((missing (missing-state-rules)))
        (format t "  Missing:  ~{~A ~}~%" missing)))
    valid-p))

;;; Export summary function for REPL use
(defun show-all-state-rules ()
  "Show all state rules in a formatted table"
  (summarize-state-rules)
  (test-state-rule-coverage))