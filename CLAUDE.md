# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This repository contains a fully implemented email scheduling system in Common Lisp, integrated with Roswell. The system handles complex date calculations, state-based exclusion rules, campaign management, and scales to process up to 3 million contacts efficiently.

## High-Level Architecture

The email scheduling system consists of:

1. **Domain Model**: CLOS-based objects representing contacts, email types, campaigns, and schedules
2. **Rules Engine**: DSL for expressing state-specific exclusion windows and scheduling rules
3. **Scheduling Engine**: Calculates email send dates for anniversary-based and campaign-based emails
4. **Load Balancer**: Distributes emails evenly across days to prevent clustering
5. **Database Layer**: SQLite-based persistence with transaction management and audit trails

## Key Design Principles

- **DSL-Driven**: Uses Common Lisp macros to create an expressive DSL for business rules
- **Multiple Dispatch**: Leverages CLOS for polymorphic behavior across email types
- **Streaming Processing**: Handles large datasets without memory exhaustion
- **Condition System**: Sophisticated error handling with restarts for recovery
- **Interactive Development**: REPL-friendly design for testing and debugging

## Implementation Status

The system is fully implemented with all components working:
1. ✅ Core domain model and basic DSL
2. ✅ State rules engine with DSL expressions
3. ✅ Date calculations and exclusion windows
4. ✅ Complete scheduling with load balancing
5. ✅ Condition system and error handling
6. ✅ Database integration with transactions
7. ✅ Load balancing and smoothing
8. ✅ Campaign system with DSL
9. ✅ Performance optimization
10. ✅ Interactive development tools

## Common Development Tasks

This project is fully implemented and integrated with Roswell. Common development tasks include:

### Using Make Commands (Recommended)
- `make install` - Install dependencies via Roswell
- `make test` - Run all tests
- `make demo` - Run interactive demo
- `make validate` - Validate installation
- `make schedule` - Run scheduler with test data
- `make benchmark` - Run performance benchmarks
- `make repl` - Start interactive REPL

### Using Roswell Script
- `./roswell/email-scheduler.ros test` - Run tests
- `./roswell/email-scheduler.ros demo` - Interactive demo
- `./roswell/email-scheduler.ros schedule --contacts 1000` - Run scheduler

### Direct Roswell Commands
- `ros run --load email-scheduler.asd --eval "(ql:quickload :email-scheduler)"`
- `ros run --load run-tests.lisp --eval "(run-all-tests)"`

### Development Workflow
1. `make install` - Set up environment
2. `make validate` - Ensure everything works
3. `make repl` - Start interactive development
4. `make test` - Run tests after changes

## Important Files

### Design Documentation
- `business_logic.md`: Comprehensive business rules and requirements
- `prompt.md`: Common Lisp implementation guidelines and code examples

### Core Implementation
- `email-scheduler.asd`: ASDF system definition
- `src/`: Main source code directory
- `test/`: Comprehensive test suite
- `config/scheduler.json`: Configuration file

### Roswell Integration
- `Makefile`: Convenient make commands for development
- `roswell/email-scheduler.ros`: Roswell script for CLI usage
- `run-tests.lisp`: Test runner and utilities

### Key Implementation Files
- `src/domain.lisp`: CLOS domain model
- `src/dsl.lisp`: Domain-specific language implementation
- `src/rules.lisp`: State rules and campaign definitions
- `src/scheduling.lisp`: Core scheduling logic
- `src/database.lisp`: Database operations
- `src/load-balancer.lisp`: Load balancing algorithms
- `src/frequency-limiter.lisp`: Frequency limits checking system

## Common Lisp Development Guidelines

### Understanding ASDF and Package Dependencies

This project uses ASDF (Another System Definition Facility) for managing dependencies and compilation order. Understanding this system is crucial for working with the codebase.

#### Package System Architecture

The system is organized into multiple packages with carefully managed dependencies:

1. **Package Definition Order**: Packages must be defined before they are used
2. **File Loading Order**: ASDF manages the compilation/loading order based on `:depends-on` clauses
3. **Symbol Visibility**: Only exported symbols are visible to other packages

#### Key Package Dependencies

```lisp
;; Core foundation packages (no dependencies)
email-scheduler.domain          ; Core CLOS classes and types
email-scheduler.date-utils      ; Date manipulation utilities
email-scheduler.conditions      ; Error handling framework

;; DSL and rules (depends on domain)
email-scheduler.dsl            ; Macro-based DSL
email-scheduler.rules          ; Business rules using DSL

;; Business logic (depends on domain + utilities)
email-scheduler.database       ; SQLite operations
email-scheduler.campaigns      ; Campaign management
email-scheduler.load-balancer  ; Load balancing algorithms
email-scheduler.frequency-limiter ; Frequency limits

;; Main system (depends on all subsystems)
email-scheduler               ; Main entry points and orchestration
```

#### ASDF System Definition Rules

In `email-scheduler.asd`, the `:depends-on` clauses must reflect actual usage:

```lisp
(:file "packages")                                    ; Always first
(:file "domain" :depends-on ("packages"))           ; Core types
(:file "date-utils" :depends-on ("packages"))       ; Utilities
(:file "database" :depends-on ("domain"))           ; Uses domain types
(:file "frequency-limiter" :depends-on ("domain" "database")) ; Uses both
(:file "scheduling" :depends-on ("rules" "date-utils" "campaigns"))
(:file "main" :depends-on ("scheduling" "database" "frequency-limiter"))
```

#### Adding New Files - Checklist

When adding a new file to the system:

1. **Add to ASDF definition** in `email-scheduler.asd`
2. **Add package definition** in `src/packages.lisp` 
3. **Ensure proper `:depends-on`** based on what symbols you import
4. **Export public symbols** from your package
5. **Import required symbols** in dependent packages

#### Package Import/Export Patterns

**Good Practice:**
```lisp
;; In packages.lisp
(defpackage #:email-scheduler.frequency-limiter
  (:use #:cl)
  (:export #:apply-frequency-limits
           #:frequency-config
           #:validate-frequency-limits))

;; In main package
(defpackage #:email-scheduler
  (:use #:cl)
  (:import-from #:email-scheduler.frequency-limiter
                #:apply-frequency-limits))
```

**Avoid:**
- Circular dependencies between packages
- Using internal (non-exported) symbols from other packages
- `:use`ing packages with many exported symbols (prefer `:import-from`)

#### Common Lisp Symbol Resolution

When you see errors like "Package X does not exist" or "Symbol Y not found":

1. **Check package definition order** - packages must be defined before use
2. **Check ASDF dependencies** - files must be loaded in dependency order  
3. **Check exports** - symbols must be exported from source package
4. **Check imports** - symbols must be imported into using package

#### Testing and Validation

- Use `(ql:quickload :email-scheduler)` to test full system loading
- Check compilation errors carefully - they often indicate dependency issues
- Use `make validate` to run system-wide validation
- Individual package testing: load just the dependencies you need

#### Development Workflow for Dependencies

1. **Plan dependencies first** - sketch which packages need what
2. **Update ASDF system definition** - add file with proper `:depends-on`
3. **Add package definition** - define exports early
4. **Implement incrementally** - test loading frequently
5. **Update imports** - add to dependent packages as needed

#### Debugging Dependency Issues

Common patterns and solutions:

```lisp
;; Error: "Package X does not exist"
;; Solution: Check packages.lisp has (defpackage #:x ...)

;; Error: "Symbol Y not found in package X"  
;; Solution: Add Y to (:export ...) list in package X definition

;; Error: "Unmatched parenthesis" during compilation
;; Solution: Use editor with paren matching, check recent edits

;; Error: Circular dependency
;; Solution: Move shared code to a lower-level package
```

This architecture ensures clean separation of concerns while maintaining the ability to compose functionality as needed.