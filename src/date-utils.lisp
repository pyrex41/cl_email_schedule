;;; date-utils.lisp - Simplified date utilities

(in-package :email-scheduler.date-utils)

;;; Helper functions
(defun today ()
  "Get today's date as a timestamp"
  (local-time:today))

(defun leap-year-p (year)
  "Check if a year is a leap year"
  (or (and (zerop (mod year 4))
           (not (zerop (mod year 100))))
      (zerop (mod year 400))))

(defun days-in-month (month year)
  "Get number of days in a month/year"
  (case month
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    (2 (if (leap-year-p year) 29 28))))

;;; Date arithmetic and utility functions
(defun next-anniversary (today anniversary-date)
  "Calculate the next anniversary date from today"
  (let* ((today-year (local-time:timestamp-year today))
         (anniversary-month (local-time:timestamp-month anniversary-date))
         (anniversary-day (local-time:timestamp-day anniversary-date))
         ;; Handle February 29th in non-leap years
         (actual-day (if (and (= anniversary-month 2) 
                              (= anniversary-day 29)
                              (not (leap-year-p today-year)))
                         28
                         anniversary-day))
         ;; Try this year first
         (this-year-anniversary (local-time:encode-timestamp 
                                 0 0 0 0 actual-day anniversary-month today-year)))
    
    ;; If this year's anniversary has passed, use next year
    (if (local-time:timestamp< this-year-anniversary today)
        (let ((next-year (1+ today-year)))
          (local-time:encode-timestamp 
           0 0 0 0 
           (if (and (= anniversary-month 2) 
                    (= anniversary-day 29)
                    (not (leap-year-p next-year)))
               28
               anniversary-day)
           anniversary-month 
           next-year))
        this-year-anniversary)))

(defun subtract-days (timestamp days)
  "Subtract days from a timestamp"
  (local-time:timestamp- timestamp days :day))

(defun add-days (timestamp days)
  "Add days to a timestamp"
  (local-time:timestamp+ timestamp days :day))

(defun date-in-window-p (date window-start window-end)
  "Check if date falls within a window (inclusive)"
  (and (local-time:timestamp>= date window-start)
       (local-time:timestamp<= date window-end)))

(defun beginning-of-month (timestamp)
  "Get the first day of the month for given timestamp"
  (local-time:encode-timestamp 
   0 0 0 0 1 
   (local-time:timestamp-month timestamp)
   (local-time:timestamp-year timestamp)))

(defun past-date-p (timestamp &optional (reference (local-time:today)))
  "Check if timestamp is in the past relative to reference date"
  (local-time:timestamp< timestamp reference))

(defun format-date (timestamp)
  "Format timestamp as ISO date string"
  (local-time:format-timestring nil timestamp :format '(:year #\- (:month 2) #\- (:day 2))))

(defun parse-date (date-string)
  "Parse ISO date string to timestamp"
  (local-time:parse-timestring date-string))

(defun same-date-p (timestamp1 timestamp2)
  "Check if two timestamps represent the same date (ignoring time)"
  (and (= (local-time:timestamp-year timestamp1) (local-time:timestamp-year timestamp2))
       (= (local-time:timestamp-month timestamp1) (local-time:timestamp-month timestamp2))
       (= (local-time:timestamp-day timestamp1) (local-time:timestamp-day timestamp2))))

(defun days-between (start-date end-date)
  "Calculate number of days between two dates"
  (floor (local-time:timestamp-difference end-date start-date) 86400))

(defun date-add-months (timestamp months)
  "Add months to a timestamp"
  (let* ((year (local-time:timestamp-year timestamp))
         (month (local-time:timestamp-month timestamp))
         (day (local-time:timestamp-day timestamp))
         (new-month (+ month months))
         (new-year year))
    ;; Handle year overflow/underflow
    (loop while (> new-month 12)
          do (incf new-year)
             (decf new-month 12))
    (loop while (< new-month 1)
          do (decf new-year)
             (incf new-month 12))
    ;; Handle day overflow (e.g., Jan 31 + 1 month = Feb 28/29)
    (let ((max-day (days-in-month new-month new-year)))
      (when (> day max-day)
        (setf day max-day)))
    (local-time:encode-timestamp 
     (local-time:timestamp-second timestamp)
     (local-time:timestamp-minute timestamp)
     (local-time:timestamp-hour timestamp)
     day day new-month new-year)))

(defun end-of-month (timestamp)
  "Get the last day of the month for given timestamp"
  (let* ((year (local-time:timestamp-year timestamp))
         (month (local-time:timestamp-month timestamp))
         (last-day (days-in-month month year)))
    (local-time:encode-timestamp 0 0 0 0 last-day month year)))

(defun years-between (start-date end-date)
  "Calculate number of complete years between two dates"
  (let ((start-year (local-time:timestamp-year start-date))
        (end-year (local-time:timestamp-year end-date))
        (start-month (local-time:timestamp-month start-date))
        (end-month (local-time:timestamp-month end-date))
        (start-day (local-time:timestamp-day start-date))
        (end-day (local-time:timestamp-day end-date)))
    (let ((years (- end-year start-year)))
      (when (or (< end-month start-month)
                (and (= end-month start-month)
                     (< end-day start-day)))
        (decf years))
      years)))