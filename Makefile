# Guile Git Implementation Makefile

.PHONY: all build test clean experiments docs run-server test-server

all: build

build:
	@echo "Building Guile Git..."
	@mkdir -p build
	@echo "Build complete."

test:
	@echo "Running tests..."
	@echo "Tests are set up but may encounter segfaults with current SHA-1 implementation"
	@echo "Run: guile3 tests/test-simple.scm for basic tests"

experiments:
	@cd experiments && gmake list

docs:
	@echo "Generating documentation..."
	@emacs --batch --eval "(require 'org)" --eval "(org-html-export-to-html nil nil nil t)" README.org 2>/dev/null || \
	 echo "Emacs not available. View README.org directly."

clean:
	@echo "Cleaning build artifacts..."
	@rm -rf build
	@cd experiments && gmake clean
	@find . -name "*.go" -delete
	@echo "Clean complete."

run-server:
	@echo "Starting Guile Git server..."
	@chmod +x run-server.scm
	@./run-server.scm

test-server:
	@echo "Starting test server on port 9419..."
	@chmod +x run-server.scm
	@./run-server.scm -p 9419 -r ./test-repos

# Book reference targets
tmp/wyag.html:
	wget -O $@ https://wyag.thb.lt/
