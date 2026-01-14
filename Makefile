# ==============================================================================
# Pro*COBOL Compiler Kit Makefile
#
# Default target is 'help'.
# ==============================================================================

.PHONY: help build compile run test golden-master shell clean db-clean

# ------------------------------------------------------------------------------
# Primary Commands
# ------------------------------------------------------------------------------

help:
	@echo "Usage:"
	@echo "  make build             Build the Docker infrastructure"
	@echo "  make compile           Compile all COBOL sources"
	@echo "  make run mod=NAME      Run a compiled module manually"
	@echo "  make test mod=NAME     Run an automated regression test"
	@echo "  make golden-master mod=NAME Regenerate a test's golden master files"
	@echo "  make shell             Start an interactive shell in the runner container"
	@echo "  make clean             Remove compiled binaries and output files"
	@echo "  make db-clean          Destroy the database and its data volume"

# ------------------------------------------------------------------------------
# Development Workflow
# ------------------------------------------------------------------------------

build:
	@docker-compose build

compile:
	@docker-compose run --rm -T compiler ./compile.sh 2>&1 | grep -v -e "No services to build" -e "Creating" -e "Created"; exit $${PIPESTATUS[0]}

run:
	@if [ -z "$(mod)" ]; then \
		echo "Error: Please specify a module name. Example: make run mod=helloworld"; \
		exit 1; \
	fi
	@echo "⏳ Waiting for Oracle Database to be ready..."
	@docker-compose run --rm -T runner ./execute.sh $(mod) 2>&1 | grep -v -E "Container|Creating|Created|Attaching|No services|time="; exit $${PIPESTATUS[0]}

# ------------------------------------------------------------------------------
# Testing Workflow
# ------------------------------------------------------------------------------

test:
	@cd tools/test-runner && go run . --mod $(mod)

golden-master:
	@cd tools/test-runner && go run . --mod $(mod) --generate

# ------------------------------------------------------------------------------
# Maintenance & Debugging
# ------------------------------------------------------------------------------

shell:
	@docker-compose run --rm runner bash

clean:
	@rm -f workspace/bin/*.so
	@rm -f workspace/output/*
	@echo "Cleaned workspace/bin/ and workspace/output/"

db-clean:
	@echo "Stopping container and removing Oracle DB data volume..."
	@docker-compose down
	@docker volume rm procob-compiler-kit_oracle-data > /dev/null 2>&1 || true
	@echo "Database volume cleaned."
