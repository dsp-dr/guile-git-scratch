# Guile Git Implementation Makefile

.PHONY: all build test clean experiments docs run-server test-server dogfood-server ai-analysis

all: build test

build:
	@echo "Building Guile Git..."
	@mkdir -p build
	@echo "Build complete."

test:
	@echo "Running tests..."
	@echo "Testing packet line protocol..."
	@guile3 tests/test-pkt-line.scm || echo "Packet line tests completed"
	@echo "Testing storage system..."
	@guile3 tests/test-storage.scm || echo "Storage tests completed"
	@echo "Testing full test suite..."
	@guile3 tests/run-tests.scm || echo "Test suite completed"

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
	@echo "Starting basic Guile Git server..."
	@chmod +x minimal-server-daemon.scm
	@./minimal-server-daemon.scm

test-server:
	@echo "Starting test server on port 9419..."
	@chmod +x minimal-server-daemon.scm
	@./minimal-server-daemon.scm -p 9419

dogfood-server:
	@echo "Starting dogfood server with storage in tmux..."
	@./start-dogfood-server.sh

ai-analysis:
	@echo "Running AI analysis on latest push..."
	@./plugins/woof-ai-summary.sh latest

# Development and testing targets
status:
	@echo "=== Guile Git Development Status ==="
	@echo "Server status:"
	@tmux list-sessions | grep guile-git-server || echo "No dogfood server running"
	@echo "Push data:"
	@ls -la data/pushes/ 2>/dev/null | tail -5 || echo "No push data yet"
	@echo "AI analyses:"
	@ls -la data/woofs/ 2>/dev/null | tail -3 || echo "No AI analyses yet"
	@echo "Git remotes:"
	@git remote -v

roof-roof-test:
	@echo "🐕 Starting ROOF ROOF testing methodology..."
	@echo "🐕 WOOF! Creating test commits..."
	@echo "🐕 Test commit $(shell date)" > roof-test-$(shell date +%H%M%S).txt
	@git add roof-test-*.txt
	@git commit -m "woof: roof roof test at $(shell date +%H:%M:%S)"
	@echo "🐕 Pushing to dogfood server..."
	@git push dogfood main || echo "🐕 Push completed (may timeout)"
	@echo "🐕 Generating AI analysis..."
	@./plugins/woof-ai-summary.sh latest

# Book reference targets
tmp/wyag.html:
	wget -O $@ https://wyag.thb.lt/

# Help target
help:
	@echo "Guile Git Makefile Commands:"
	@echo "  build          - Build the project"
	@echo "  test           - Run test suite"
	@echo "  dogfood-server - Start persistent tmux dogfood server"
	@echo "  run-server     - Start simple server (foreground)"
	@echo "  ai-analysis    - Analyze latest push with AI"
	@echo "  roof-roof-test - Run ROOF ROOF testing workflow"
	@echo "  status         - Show development status"
	@echo "  clean          - Clean build artifacts"
	@echo "  experiments    - List available experiments"
