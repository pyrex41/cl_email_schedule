# Email Scheduler - Common Lisp Implementation

A sophisticated email scheduling system implemented in Common Lisp, featuring a powerful DSL for expressing business rules, CLOS-based domain modeling, and advanced error handling through the Common Lisp condition system.

## Features

- **Powerful DSL**: Macros for expressing scheduling rules and state-based exclusions
- **CLOS Domain Model**: Object-oriented design with multiple dispatch
- **Condition System**: Sophisticated error handling with restarts
- **Streaming Processing**: Memory-efficient processing of large contact databases
- **Load Balancing**: Intelligent distribution of emails across days
- **Interactive Development**: REPL-friendly design with debugging tools
- **Campaign Management**: Flexible campaign system with template support
- **State Compliance**: Automated compliance with state-specific regulations

## Roswell Integration

This project is fully integrated with [Roswell](https://github.com/roswell/roswell), the Common Lisp environment manager. You can use the project in three ways:

1. **Make commands** (recommended for development)
2. **Roswell script** (great for automation and CI)
3. **Direct Roswell commands** (for maximum control)

### Available Make Commands

```bash
make help       # Show all available commands
make install    # Install dependencies
make test       # Run all tests
make demo       # Interactive demo
make validate   # Validate installation
make schedule   # Run scheduler with test data
make benchmark  # Performance benchmarks
make repl       # Start interactive REPL
make clean      # Clean generated files
```

### Roswell Script

The `roswell/email-scheduler.ros` script provides a convenient CLI interface:

```bash
./roswell/email-scheduler.ros help                                    # Show help
./roswell/email-scheduler.ros test                                    # Run tests
./roswell/email-scheduler.ros demo                                    # Interactive demo
./roswell/email-scheduler.ros schedule --contacts 1000               # Run scheduler
./roswell/email-scheduler.ros repl                                   # Start REPL
```

## Quick Start

### Prerequisites

- [Roswell](https://github.com/roswell/roswell) (Common Lisp environment manager)
- SQLite

Roswell will automatically manage SBCL and Quicklisp for you.

### Installation

1. Clone the repository:
```bash
git clone <repository-url>
cd cl_email_schedule
```

2. Install dependencies and validate:
```bash
make install     # Install SBCL and load system
make validate    # Validate installation
```

**Alternative manual installation:**
```bash
ros install sbcl-bin           # Install SBCL via Roswell
ros run --eval "(pushnew (truename \".\") ql:*local-project-directories*)" --eval "(ql:register-local-projects)" --eval "(ql:quickload :email-scheduler)"
```

### Basic Usage

#### Interactive Demo
```bash
make demo
```

**Or using Roswell directly:**
```bash
ros run --eval "(pushnew (truename \".\") ql:*local-project-directories*)" --load run-tests.lisp --eval "(demo-scheduler)"
```

**Or using the Roswell script:**
```bash
./roswell/email-scheduler.ros demo
```

#### Create Test Contact and Preview Schedules
```bash
# Start interactive REPL
make repl
```

**Or using Roswell directly:**
```bash
ros run --eval "(pushnew (truename \".\") ql:*local-project-directories*)" --eval "(ql:register-local-projects)" --eval "(ql:quickload :email-scheduler)" --eval "(in-package :email-scheduler.repl)"
```

Then in the REPL:
```lisp
;; Create a test contact
(defparameter *contact* 
  (test-contact :email "john@example.com" 
                :state :ca 
                :birthday "1980-03-15"
                :effective-date "2020-01-01"))

;; Preview what emails would be scheduled
(preview-schedules *contact*)

;; Explain why a specific date might be excluded
(explain-exclusion *contact* "2024-03-15")
```

#### Run the Scheduler
```bash
# Quick scheduler run with 100 contacts
make schedule

# Or with custom parameters using the Roswell script
./roswell/email-scheduler.ros schedule --contacts 1000 --db-path "my-scheduler.db"
```

**Or manually in REPL:**
```lisp
;; Set up test environment with 1000 contacts
(email-scheduler:setup-test-environment :contact-count 1000)

;; Run scheduler
(email-scheduler:run-scheduler :db-path "test-scheduler.db")
```

## Architecture

### Core Components

1. **Domain Model** (`src/domain.lisp`)
   - CLOS classes for contacts, email types, and schedules
   - Multiple dispatch for polymorphic behavior

2. **DSL** (`src/dsl.lisp`)
   - Macros for defining state rules and campaigns
   - Reader macros for date literals

3. **Rules Engine** (`src/rules.lisp`)
   - State-specific exclusion window logic
   - Campaign configuration management

4. **Scheduling Logic** (`src/scheduling.lisp`)
   - Anniversary-based email calculation
   - Campaign email processing
   - Exclusion rule application

5. **Database Layer** (`src/database.lisp`)
   - SQLite integration with SXQL DSL
   - Transaction management and audit trails

6. **Load Balancer** (`src/load-balancer.lisp`)
   - Email volume distribution
   - Deterministic jitter for clustering prevention

### State Rules DSL

Define state-specific exclusion rules using the DSL:

```lisp
;; California: 30 days before to 60 days after birthday
(defstate :ca
  (birthday-window :before 30 :after 60))

;; Nevada: Special rule using month start
(defstate :nv
  (birthday-window :before 0 :after 60 :use-month-start t))

;; Connecticut: Year-round exclusion
(defstate :ct (year-round-exclusion))
```

### Campaign DSL

Define campaign types with flexible configuration:

```lisp
(defcampaign rate-increase
  :respect-exclusions t
  :enable-followups t
  :days-before 14
  :priority 1)
```

## Email Types

### Anniversary-Based Emails
- **Birthday**: Sent 14 days before contact's birthday
- **Effective Date**: Sent 30 days before policy anniversary
- **AEP**: Annual Enrollment Period emails (September 15th)
- **Post Window**: Sent after exclusion windows end

### Campaign-Based Emails
- **Rate Increase**: Premium change notifications
- **Seasonal Promotions**: Configurable marketing campaigns
- **Initial Blast**: System introduction emails

### Follow-up Emails
- Intelligent follow-up based on user engagement
- Multiple follow-up types based on behavior analysis

## State Compliance

The system automatically handles state-specific regulations:

### Birthday Window States
- **CA**: 30 days before to 60 days after
- **ID**: 0 days before to 63 days after
- **NV**: 0 days before to 60 days after (month start)
- **KY, OK**: 0 days before to 60 days after
- **MD, VA**: 0 days before to 30 days after
- **OR**: 0 days before to 31 days after

### Effective Date Window States
- **MO**: 30 days before to 33 days after

### Year-Round Exclusion States
- **CT, MA, NY, WA**: No marketing emails sent

## Configuration

Configuration can be provided via JSON file:

```json
{
  "timezone": "America/Chicago",
  "batch_size": 10000,
  "birthday_days_before": 14,
  "effective_date_days_before": 30,
  "daily_cap_percentage": 0.07,
  "load_balancing": {
    "ed_smoothing_window_days": 5,
    "catch_up_spread_days": 7
  }
}
```

## Testing

### Run All Tests
```bash
make test
```

### Run Specific Test Suites
```bash
./roswell/email-scheduler.ros test     # All tests via script
```

**Or using Roswell directly:**
```bash
ros run --eval "(pushnew (truename \".\") ql:*local-project-directories*)" --load run-tests.lisp --eval "(run-rules-tests)"        # State rules and exclusion logic
ros run --eval "(pushnew (truename \".\") ql:*local-project-directories*)" --load run-tests.lisp --eval "(run-scheduling-tests)"   # Email scheduling logic
ros run --eval "(pushnew (truename \".\") ql:*local-project-directories*)" --load run-tests.lisp --eval "(run-database-tests)"     # Database operations
```

### Performance Testing
```bash
make benchmark
```

**Or using Roswell directly:**
```bash
ros run --eval "(pushnew (truename \".\") ql:*local-project-directories*)" --load run-tests.lisp --eval "(run-performance-tests)"
ros run --eval "(pushnew (truename \".\") ql:*local-project-directories*)" --load run-tests.lisp --eval "(benchmark-scheduler)"
```

## Interactive Development

The system provides extensive REPL support for interactive development:

```bash
# Start interactive session
make repl
```

**Or using the Roswell script:**
```bash
./roswell/email-scheduler.ros repl
```

**Or using Roswell directly:**
```bash
ros run --eval "(pushnew (truename \".\") ql:*local-project-directories*)" --eval "(ql:register-local-projects)" --eval "(ql:quickload :email-scheduler)" --eval "(email-scheduler.repl:start-repl)"
```

Then in the REPL:
```lisp
;; Create test contacts
(defparameter *ca-contact* (test-contact :state :ca))
(defparameter *ny-contact* (test-contact :state :ny))

;; Show state rules
(show-rules :ca)
(show-campaigns 'rate-increase)

;; Trace scheduling decisions
(trace-scheduling *ca-contact*)

;; Run demo
(demo)
```

## Database Schema

The system uses SQLite with the following key tables:

- `contacts`: Contact information and demographics
- `email_schedules`: Scheduled emails with status and metadata
- `campaign_types`: Reusable campaign configurations
- `campaign_instances`: Specific campaign executions
- `contact_campaigns`: Campaign targeting relationships
- `scheduler_checkpoints`: Audit trail and recovery points

## Performance

The system is designed to handle large-scale processing:

- **Streaming Processing**: Handles 3M+ contacts without memory exhaustion
- **Parallel Processing**: Uses lparallel for batch operations
- **Load Balancing**: Prevents email clustering and server overload
- **Optimized Queries**: Efficient database operations with proper indexing

### Benchmarks

Typical performance on modern hardware:
- 1,000 contacts: ~0.1 seconds
- 10,000 contacts: ~1.0 seconds
- 100,000 contacts: ~10 seconds

## Error Handling

The system uses Common Lisp's condition system for robust error handling:

```lisp
;; Automatic error recovery
(with-error-handling
  (schedule-emails-streaming db-path run-id))

;; Contact-level error handling with restarts
(with-contact-processing (contact)
  (calculate-schedules contact))
```

Available restarts:
- `skip-contact`: Skip invalid contact and continue
- `retry-batch`: Retry failed batch operation
- `use-default`: Use default configuration values

## Extensions

The system is designed for extensibility:

### Adding New Email Types
```lisp
(defclass custom-email (email-type)
  ((custom-field :initarg :custom-field :accessor custom-field)))

(defmethod calculate-send-date ((email-type custom-email) contact today)
  ;; Custom scheduling logic
  )
```

### Adding New State Rules
```lisp
(defstate :new-state
  (birthday-window :before 15 :after 45)
  (effective-date-window :before 20 :after 20))
```

### Custom Smoothing Rules
```lisp
(define-smoothing-rule custom-smoothing
    (> (count-emails-by-type day-schedules 'custom-email) 10)
  (redistribute-with-jitter schedules date 7))
```

## License

MIT License - see LICENSE file for details.

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

## Support

- Run `(validate-installation)` to check system health
- Use `(demo-scheduler)` for interactive exploration
- Check test suite with `(run-all-tests)`
- Use REPL tools for debugging: `(trace-scheduling contact)`