.PHONY: build compile run shell clean help

# Default target
help:
	@echo "Usage:"
	@echo "  make build           Build the Docker image"
	@echo "  make compile         Compile all source files in workspace/src"
	@echo "  make run mod=NAME    Run a compiled module (e.g., make run mod=helloworld)"
	@echo "  make shell           Start an interactive shell inside the container"
	@echo "  make clean           Remove compiled binaries"

build:
	@docker-compose build

compile:
	@docker-compose run --rm -T cobol-compiler ./compile.sh 2>&1 | grep -v -e "No services to build" -e "Creating" -e "Created"; exit $${PIPESTATUS[0]}

run:
	@if [ -z "$(mod)" ]; then \
		echo "Error: Please specify a module name. Example: make run mod=helloworld"; \
		exit 1; \
	fi
	@docker-compose run --rm -T cobol-compiler ./execute.sh $(mod) 2>&1 | grep -v -e "No services to build" -e "Creating" -e "Created"; exit $${PIPESTATUS[0]}

shell:
	@docker-compose run --rm cobol-compiler bash

clean:
	@rm -f workspace/bin/*.so
	@echo "Cleaned workspace/bin/"
