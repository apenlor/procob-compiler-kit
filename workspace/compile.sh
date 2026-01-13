#!/bin/bash

# --- Colors & formatting ---
BOLD="\033[1m"
GREEN="\033[0;32m"
BLUE="\033[0;34m"
YELLOW="\033[0;33m"
RED="\033[0;31m"
RESET="\033[0m"

log_info() { echo -e "${BLUE}🔵 [INFO]${RESET}  $1"; }
log_step() { echo -e "${YELLOW}🛠️  [STEP]${RESET}  $1"; }
log_success() { echo -e "${GREEN}✅ [SUCCESS]${RESET} $1"; }
log_error() { echo -e "${RED}❌ [ERROR]${RESET}   $1"; }

# Ensure binary output directory exists
mkdir -p bin

# Oracle Instant Client configuration
ORACLE_FLAGS="-L$ORACLE_HOME -lclntsh"
COBC_FLAGS="-m -fbinary-byteorder=native -fbinary-size=2-4-8"

log_info "Starting build process..."

# Change to source directory
cd src || { log_error "Source directory 'src' not found"; exit 1; }

# List to track generated .cbl files for cleanup
generated_files=""

# 1. Oracle Pro*COBOL Precompilation
shopt -s nullglob
pco_files=(*.pco)

if [ ${#pco_files[@]} -gt 0 ]; then
    log_step "Precompiling Oracle Pro*COBOL sources..."
    for f in "${pco_files[@]}"; do
        filename=$(basename -- "$f")
        base="${filename%.*}"
        generated_cbl="${base}.cbl"

        echo -e "    ➜ ${BOLD}$f${RESET} -> $generated_cbl"

        # iname=input, oname=output
        procob iname="$f" oname="$generated_cbl" > /dev/null

        if [ $? -eq 0 ]; then
            generated_files="$generated_files $generated_cbl"
        else
            log_error "Precompilation failed for $f"
            exit 1
        fi
    done
fi

# 2. GnuCOBOL Compilation (Batch)
cbl_files=(*.cbl)

if [ ${#cbl_files[@]} -gt 0 ]; then
    log_step "Compiling GnuCOBOL sources..."
    echo -e "    ➜ Sources: ${BOLD}${cbl_files[*]}${RESET}"
    
    # Run cobc in batch mode
    cobc $COBC_FLAGS $ORACLE_FLAGS "${cbl_files[@]}"

    if [ $? -eq 0 ]; then
        # Move generated modules (.so) to bin directory
        mv -f *.so ../bin/ 2>/dev/null
        
        # Remove intermediate precompiled sources
        if [ -n "$generated_files" ]; then
            rm $generated_files
        fi
        
        log_success "Build completed. Artifacts moved to ${BOLD}workspace/bin${RESET}"
    else
        log_error "Compilation failed"
        exit 1
    fi
else
    log_info "No .cbl files found to compile"
fi

