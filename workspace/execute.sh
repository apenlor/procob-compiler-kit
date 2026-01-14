#!/bin/bash

# Wrapper for executing compiled GnuCOBOL modules
# Usage: ./execute.sh MODULE_NAME [ARGS...]

# --- Colors & formatting ---
BLUE="\033[0;34m"
RESET="\033[0m"

log_info() { echo -e "${BLUE}🔵 [INFO]${RESET}  $1"; }

if [ -z "$1" ]; then
    echo "Usage: $0 MODULE_NAME [ARGS...]"
    exit 1
fi

# Docker-compose's `depends_on` with healthcheck now handles DB readiness.
# This script can now be simplified.

# Execute
log_info "Executing module: $1"
cobcrun "$@"
