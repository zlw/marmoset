#!/bin/bash
# Runtime output tests - check actual program output
# These tests compile and RUN programs, checking their stdout

set -e

BINDIR="./_build/default/bin"
EXECUTABLE="$BINDIR/main.exe"

if [ ! -f "$EXECUTABLE" ]; then
    echo "Building project..."
    dune build
fi

PASS=0
FAIL=0
TOTAL=0

# Helper function for runtime output testing
test_runtime_output() {
    local name="$1"
    local source="$2"
    local expected_output="$3"
    
    TOTAL=$((TOTAL + 1))
    
    echo -n "TEST [$TOTAL] $name ... "
    
    # Write source to temp file
    tmpfile=$(mktemp)
    echo "$source" > "$tmpfile"
    
    # Build the program
    if ! build_output=$($EXECUTABLE build "$tmpfile" 2>&1); then
        echo "✗ FAIL (compilation failed)"
        echo "  Error: $build_output"
        FAIL=$((FAIL + 1))
        rm -f "$tmpfile"
        return
    fi
    
    # Extract binary name from build output (format: "Built: <name>")
    binary_name=$(echo "$build_output" | grep "^Built: " | sed 's/Built: //')
    
    if [ -z "$binary_name" ]; then
        echo "✗ FAIL (couldn't determine binary name)"
        echo "  Build output: $build_output"
        FAIL=$((FAIL + 1))
        rm -f "$tmpfile"
        return
    fi
    
    # Run the binary and capture output
    if [ -f "./$binary_name" ]; then
        actual_output=$(./"$binary_name" 2>&1)
        
        # Compare output
        if [ "$actual_output" = "$expected_output" ]; then
            echo "✓ PASS"
            PASS=$((PASS + 1))
        else
            echo "✗ FAIL"
            echo "  Expected: '$expected_output'"
            echo "  Got:      '$actual_output'"
            FAIL=$((FAIL + 1))
        fi
        
        # Clean up binary
        rm -f "./$binary_name"
    else
        echo "✗ FAIL (binary not found: $binary_name)"
        FAIL=$((FAIL + 1))
    fi
    
    rm -f "$tmpfile"
}

echo "=========================================="
echo "RUNTIME OUTPUT TESTS"
echo "=========================================="
echo ""

echo "-- BASIC OUTPUT TESTS --"
test_runtime_output "Print integer" \
    'puts(42)' \
    "42"

test_runtime_output "Print string" \
    'puts("hello")' \
    "hello"

test_runtime_output "Print boolean" \
    'puts(true)' \
    "true"

echo ""
echo "-- ENUM OUTPUT TESTS --"

# FIXED: Enum printing now shows readable format with String() method
test_runtime_output "Print enum value with data" \
    'enum option[a] {
  some(a)
  none
}

let x = option.some(42)
puts(x)' \
    "some(42)"

test_runtime_output "Print enum with no data" \
    'enum option[a] {
  some(a)
  none
}

let x: option[int] = option.none()
puts(x)' \
    "none"

echo ""
echo "=========================================="
echo "RESULTS: $PASS passed, $FAIL failed out of $TOTAL tests"
echo "=========================================="

if [ $FAIL -eq 0 ]; then
    echo "✓ ALL TESTS PASSED"
    exit 0
else
    echo "✗ SOME TESTS FAILED"
    exit 1
fi
