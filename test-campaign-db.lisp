;;; test-campaign-db.lisp - Test campaign database functionality

(pushnew (truename ".") ql:*local-project-directories*)
(ql:register-local-projects)
(ql:quickload :email-scheduler :silent t)

(format t "Testing campaign database schema...~%")

(handler-case
    (let ((test-db "test-campaign.db"))
      ;; Remove existing test database
      (when (probe-file test-db)
        (delete-file test-db))
      
      ;; Create new database with campaign tables
      (email-scheduler.database:with-database (db test-db)
        (email-scheduler.database:create-database-schema db)
        (format t "✓ Campaign database schema created successfully~%")
        
        ;; Populate built-in campaign types
        (email-scheduler.database:populate-builtin-campaign-types db)
        (format t "✓ Built-in campaign types populated~%")
        
        ;; Test inserting a campaign instance
        (email-scheduler.database:insert-campaign-instance db
          (list :campaign-type "rate_increase"
                :instance-name "test_rate_increase_2024"
                :email-template "test_template"
                :active-start-date "2024-01-01"
                :active-end-date "2024-12-31"))
        (format t "✓ Test campaign instance created~%")
        
        ;; Test querying active campaigns
        (let ((active-campaigns (email-scheduler.database:get-active-campaign-instances-from-db db)))
          (format t "✓ Found ~A active campaign instances~%" (length active-campaigns)))
        
        ;; Clean up
        (delete-file test-db)
        (format t "✓ Test database cleaned up~%")))
  (error (e)
    (format t "✗ Campaign database test failed: ~A~%" e)))

(format t "Campaign database test complete.~%")