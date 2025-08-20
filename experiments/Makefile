# experiments/Makefile
.PHONY: list new run test clean

# List all experiments
list:
	@find . -maxdepth 1 -type d -name "[0-9]*" | sort

# Create new experiment
new:
	@last=$$(find . -maxdepth 1 -type d -name "[0-9]*" | sort -n | tail -1 | cut -d- -f1 | sed 's/\.\///'); \
	next=$$(printf "%03d" $$((10#$$last + 1))); \
	read -p "Experiment name: " name; \
	mkdir -p "$$next-$$name"; \
	cp TEMPLATE/* "$$next-$$name/" 2>/dev/null || true; \
	echo "Created experiment $$next-$$name"

# Run specific experiment
run:
	@if [ -z "$(EXP)" ]; then \
		echo "Usage: gmake run EXP=001-book-analysis"; \
	else \
		cd $(EXP) && gmake run; \
	fi

# Test all experiments
test:
	@for exp in $$(find . -maxdepth 1 -type d -name "[0-9]*" | sort); do \
		echo "Testing $$exp..."; \
		(cd $$exp && gmake test) || exit 1; \
	done

# Clean all experiment artifacts
clean:
	@for exp in $$(find . -maxdepth 1 -type d -name "[0-9]*"); do \
		(cd $$exp && gmake clean 2>/dev/null || true); \
	done