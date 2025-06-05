;;; email-scheduler.asd
(defsystem "email-scheduler"
  :description "Email scheduling system with DSL"
  :version "1.0.0"
  :author "Your Name"
  :license "MIT"
  :depends-on (#:sxql            ; SQL DSL  
               #:local-time      ; Time/date handling
               #:cl-ppcre        ; Regex for parsing
               #:yason           ; JSON parsing
               #:bordeaux-threads ; Threading
               #:lparallel       ; Parallel processing
               #:alexandria      ; Utilities
               #:fiveam          ; Testing
               #:cffi            ; For SQLite FFI
               #:iterate)        ; For cl-sqlite
  :components ((:module "dependencies"
                :components
                ((:module "cl-sqlite"
                  :components
                  ((:file "sqlite-ffi")
                   (:file "cache")
                   (:file "sqlite" :depends-on ("sqlite-ffi" "cache"))))))
               (:module "src"
                :depends-on ("dependencies")
                :components
                ((:file "packages")
                 (:file "conditions" :depends-on ("packages"))
                 (:file "date-utils" :depends-on ("packages"))
                 (:file "domain" :depends-on ("packages"))
                 (:file "dsl" :depends-on ("domain"))
                 (:file "rules" :depends-on ("dsl"))
                 (:file "state-rules-complete" :depends-on ("rules"))
                 (:file "campaigns" :depends-on ("domain" "date-utils"))
                 (:file "followup-simple" :depends-on ("domain" "date-utils" "database"))
                 (:file "scheduling" :depends-on ("rules" "date-utils" "campaigns"))
                 (:file "database" :depends-on ("domain"))
                 (:file "load-balancer" :depends-on ("scheduling"))
                 (:file "frequency-limiter" :depends-on ("domain" "database"))
                 (:file "main" :depends-on ("scheduling" "database" "frequency-limiter")))))
  :in-order-to ((test-op (test-op "email-scheduler/tests"))))

(defsystem "email-scheduler/tests"
  :description "Test suite for email-scheduler"
  :depends-on (#:email-scheduler
               #:fiveam)
  :components ((:module "test"
                :components
                ((:file "test-rules")
                 (:file "test-scheduling")
                 (:file "test-database"))))
  :perform (test-op (op c) (symbol-call :fiveam :run! :email-scheduler-tests)))