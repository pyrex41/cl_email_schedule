# Email Scheduler Makefile
# Requires Roswell (ros) to be installed

.PHONY: help install test demo validate schedule benchmark repl clean

# Default target
help:
	@echo "Email Scheduler - Common Lisp Implementation"
	@echo ""
	@echo "Available targets:"
	@echo "  install    - Install dependencies"
	@echo "  test       - Run all tests"
	@echo "  demo       - Run interactive demo"
	@echo "  validate   - Validate installation"
	@echo "  schedule   - Run scheduler with test data"
	@echo "  benchmark  - Run performance benchmarks"
	@echo "  repl       - Start interactive REPL"
	@echo "  clean      - Clean up generated files"
	@echo ""
	@echo "Examples:"
	@echo "  make install && make test"
	@echo "  make demo"
	@echo "  make schedule"

# Install dependencies
install:
	@echo "Installing dependencies via Roswell..."
	@echo "Using sbcl-bin for faster installation..."
	ros install sbcl-bin
	@echo "Registering local project and loading system..."
	ros run --eval "(pushnew (truename \".\") ql:*local-project-directories*)" --eval "(ql:register-local-projects)" --eval "(ql:quickload :email-scheduler)" --eval "(quit)"

# Run all tests
test:
	@echo "Running all tests..."
	ros run --eval "(pushnew (truename \".\") ql:*local-project-directories*)" --load run-tests.lisp --eval "(run-all-tests)" --eval "(quit)"

# Run interactive demo
demo:
	@echo "Starting interactive demo..."
	ros run --load demo.lisp

# Validate installation
validate:
	@echo "Validating installation..."
	ros run --load validate.lisp

# Run scheduler with test data
schedule:
	@echo "Running scheduler with test data..."
	./roswell/email-scheduler.ros schedule --contacts 100

# Run performance benchmarks
benchmark:
	@echo "Running performance benchmarks..."
	ros run --eval "(pushnew (truename \".\") ql:*local-project-directories*)" --load run-tests.lisp --eval "(benchmark-scheduler)" --eval "(quit)"

# Start interactive REPL
repl:
	@echo "Starting interactive REPL..."
	ros run --eval "(pushnew (truename \".\") ql:*local-project-directories*)" --eval "(ql:register-local-projects)" --eval "(ql:quickload :email-scheduler)" --eval "(email-scheduler.repl:start-repl)"

# Clean up generated files
clean:
	@echo "Cleaning up generated files..."
	rm -f *.db *.log
	rm -f *.fasl
	rm -rf .ros/
	@echo "Clean complete."

# Quick development setup
dev-setup: install validate
	@echo "Development environment ready!"
	@echo "Try: make demo"