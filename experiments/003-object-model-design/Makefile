.PHONY: run test clean tangle

tangle:
	@echo "Tangling org file to Scheme..."
	@emacs --batch --eval "(require 'org)" --eval "(org-babel-tangle-file \"object-model.org\")" 2>/dev/null || \
	 echo "Note: Emacs not available for tangling. Manually extract code blocks."

run: tangle
	@if [ -f object-model.scm ]; then \
		guile3 -l object-model.scm -c "(display \"Object model loaded successfully\\n\")"; \
	else \
		echo "Please manually extract code from object-model.org to object-model.scm"; \
	fi

test: tangle
	@if [ -f test-object-model.scm ]; then \
		guile3 test-object-model.scm; \
	else \
		echo "Test file not found. Extract from org file."; \
	fi

clean:
	@rm -f object-model.scm test-object-model.scm