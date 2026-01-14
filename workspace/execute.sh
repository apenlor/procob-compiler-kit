#!/bin/bash

# Wrapper for executing compiled GnuCOBOL modules
# Usage: ./execute.sh MODULE_NAME [ARGS...]

# --- Colors & formatting ---
BLUE="\033[0;34m"
RESET="\033[0m"
BOLD="\033[1m"

log_info() { echo -e "${BLUE}🔵 [INFO]${RESET}  $1"; }

if [ -z "$1" ]; then
    echo "Usage: $0 MODULE_NAME [ARGS...]"
    exit 1
fi

# --- Execution Sandbox ---
# All execution happens inside the /app/bin directory.
cd /app/bin || { echo "❌ [ERROR] Output directory /app/bin not found."; exit 1; }

# --- I/O Management ---
# 1. Clean the output directory for a fresh run.
if [ -d "/app/output" ]; then
    rm -f /app/output/*
fi

# 2. Copy fresh input files from /app/input if the directory exists.
if [ -d "/app/input" ]; then
    # Use `cp -f` to always overwrite with a fresh copy from the input directory,
    # ensuring that each run is idempotent and uses clean data.
    cp -f /app/input/* . 2>/dev/null || true
fi

# --- Execute ---
log_info "Executing module: ${BOLD}$1${RESET}"
cobcrun "$@"
EXIT_CODE=$?
log_info "Execution finished with exit code ${BOLD}$EXIT_CODE${RESET}"


# --- Harvest Outputs ---
# 3. Move all non-binary files (.so) from the sandbox to the output directory.
if [ -d "/app/output" ]; then
    find . -maxdepth 1 -type f ! -name "*.so" ! -name ".gitkeep" -exec mv -t /app/output/ {} +
    log_info "Moved generated files to ${BOLD}/app/output${RESET}"
fi

exit $EXIT_CODE
