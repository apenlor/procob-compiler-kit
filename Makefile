.PHONY: build compile run shell clean help

# Default target
help:
	@echo "Usage:"
	@echo "  make build           Build the Docker image"
	@echo "  make compile         Compile all source files in workspace/src"
	@echo "  make run mod=NAME    Run a compiled module (e.g., make run mod=helloworld)"
	@echo "  make shell           Start an interactive shell inside the container"
	@echo "  make clean           Remove compiled binaries"
	@echo "  make db-clean        Stop and remove the Oracle DB container and data"

build:
	@docker-compose build

compile:
	@docker-compose run --rm -T compiler ./compile.sh 2>&1 | grep -v -e "No services to build" -e "Creating" -e "Created"; exit $${PIPESTATUS[0]}

run:
	@if [ -z "$(mod)" ]; then \
		echo "Error: Please specify a module name. Example: make run mod=helloworld"; \
		exit 1; \
	fi
	@docker-compose run --rm -T runner ./execute.sh $(mod) 2>&1 | grep -v -e "No services to build" -e "Creating" -e "Created"; exit $${PIPESTATUS[0]}

shell:
	@docker-compose run --rm runner bash

db-clean:
	@echo "Stopping container and removing Oracle DB data volume..."
	@docker-compose down
	@docker volume rm procob-compiler-kit_oracle-data > /dev/null 2>&1 || true
	@echo "Database volume cleaned."

clean:
	@rm -f workspace/bin/*.so
	@echo "Cleaned workspace/bin/"
