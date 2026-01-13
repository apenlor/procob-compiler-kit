#!/bin/bash

# Wrapper for executing compiled GnuCOBOL modules
# Usage: ./execute.sh MODULE_NAME [ARGS...]

if [ -z "$1" ]; then
    echo "Usage: $0 MODULE_NAME [ARGS...]"
    exit 1
fi

# Configure library path to include compiled modules
# This allows cobcrun to find the .so files
export COB_LIBRARY_PATH=/app/bin:$COB_LIBRARY_PATH

# Explicitly set Oracle library paths (redundancy for safety)
export LD_LIBRARY_PATH=$ORACLE_HOME:$LD_LIBRARY_PATH

# Execute
cobcrun "$@"
