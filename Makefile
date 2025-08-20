# Guile Git Implementation Makefile

.PHONY: all build test clean experiments docs

all: build

build:
	@echo "Building Guile Git..."
	@mkdir -p build
	@echo "Build complete."

test:
	@echo "Running tests..."
	@cd tests && guile3 test-runner.scm

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

# Book reference targets
tmp/wyag.html:
	wget -O $@ https://wyag.thb.lt/
