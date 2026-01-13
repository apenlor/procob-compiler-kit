#!/bin/bash

# Ensure binary output directory exists
mkdir -p bin

# Oracle Instant Client configuration
# -L: Library search path
# -l: Link against 'clntsh' (Oracle Client Shared Library)
ORACLE_FLAGS="-L$ORACLE_HOME -lclntsh"
COBC_FLAGS="-m -fbinary-byteorder=native -fbinary-size=2-4-8"

echo "--- Starting build process ---"

# Change to source directory
cd src || exit 1

# List to track generated .cbl files for cleanup
generated_files=""

# 1. Oracle Pro*COBOL Precompilation
# Process files sequentially; 'procob' lacks robust wildcard support
shopt -s nullglob
for f in *.pco; do
    filename=$(basename -- "$f")
    base="${filename%.*}"
    generated_cbl="${base}.cbl"

    echo "Precompiling Oracle COBOL: $f -> $generated_cbl"

    # iname=input, oname=output
    procob iname="$f" oname="$generated_cbl"

    if [ $? -eq 0 ]; then
        generated_files="$generated_files $generated_cbl"
    else
        echo "❌ Error precompiling $f"
        exit 1
    fi
done

# 2. GnuCOBOL Compilation (Batch)
# This picks up both native .cbl files and the ones we just generated
# We use an array to capture the expansion of *.cbl safely
cbl_files=(*.cbl)

if [ ${#cbl_files[@]} -gt 0 ]; then
    echo "Compiling COBOL sources: ${cbl_files[*]}"
    
    # Run cobc in batch mode
    cobc $COBC_FLAGS $ORACLE_FLAGS "${cbl_files[@]}"

    if [ $? -eq 0 ]; then
        echo "✅ Compilation successful"
        
        # Move generated modules (.so) to bin directory
        # Using -f to overwrite existing binaries
        mv -f *.so ../bin/ 2>/dev/null
        
        # Remove intermediate precompiled sources
        if [ -n "$generated_files" ]; then
            echo "Cleaning up generated sources..."
            rm $generated_files
        fi
    else
        echo "❌ Compilation failed"
        exit 1
    fi
else
    echo "⚠️ No .cbl files found to compile"
fi

echo "--- Build complete ---"
