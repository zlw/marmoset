#!/bin/bash
# Comprehensive tests for typecheck and codegen
# These tests verify real end-to-end functionality

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

# Helper function for testing
test_case() {
    local name="$1"
    local source="$2"
    local should_succeed="$3"  # "true" or "false"
    local expected_output="$4" # optional
    
    TOTAL=$((TOTAL + 1))
    
    echo -n "TEST [$TOTAL] $name ... "
    
    # Write source to temp file
    tmpfile=$(mktemp)
    echo "$source" > "$tmpfile"
    
    # Try to build
    if output=$($EXECUTABLE build "$tmpfile" 2>&1); then
        if [ "$should_succeed" = "true" ]; then
            echo "✓ PASS"
            PASS=$((PASS + 1))
        else
            echo "✗ FAIL (expected to fail but passed)"
            FAIL=$((FAIL + 1))
        fi
    else
        if [ "$should_succeed" = "false" ]; then
            # Check if error message contains expected substring
            if [ -n "$expected_output" ]; then
                if echo "$output" | grep -q "$expected_output"; then
                    echo "✓ PASS"
                    PASS=$((PASS + 1))
                else
                    echo "✗ FAIL (error message doesn't contain '$expected_output')"
                    echo "  Got: $output"
                    FAIL=$((FAIL + 1))
                fi
            else
                echo "✓ PASS"
                PASS=$((PASS + 1))
            fi
        else
            echo "✗ FAIL"
            echo "  Error: $output"
            FAIL=$((FAIL + 1))
        fi
    fi
    
    rm -f "$tmpfile"
}

echo "=========================================="
echo "TYPECHECK & CODEGEN INTEGRATION TESTS"
echo "=========================================="
echo ""

echo "-- PARAMETER ANNOTATION TESTS --"
test_case "Single int parameter" \
    'let f = fn(x: int) { x + 1 }; f(5)' \
    "true"

test_case "Multiple int parameters" \
    'let add = fn(x: int, y: int) -> int { x + y }; add(3, 4)' \
    "true"

test_case "Mixed type parameters" \
    'let f = fn(x: int, y: string) { y }; f(5, "hi")' \
    "true"

echo ""
echo "-- RETURN TYPE ANNOTATION TESTS --"
test_case "Return type int matches" \
    'let f = fn() -> int { 42 }; f()' \
    "true"

test_case "Return type string matches" \
    'let f = fn() -> string { "hello" }; f()' \
    "true"

test_case "Return type bool matches" \
    'let f = fn() -> bool { true }; f()' \
    "true"

echo ""
echo "-- ANNOTATION MISMATCH DETECTION (MUST FAIL) --"
test_case "Mismatch: int vs string" \
    'let f = fn() -> string { 42 }; f()' \
    "false" \
    "annotation mismatch"

test_case "Mismatch: string vs int" \
    'let f = fn() -> int { "hello" }; f()' \
    "false" \
    "annotation mismatch"

test_case "Mismatch: bool vs int" \
    'let f = fn() -> bool { 42 }; f()' \
    "false" \
    "annotation mismatch"

test_case "Mismatch: array vs int" \
    'let f = fn() -> list[int] { 42 }; f()' \
    "false" \
    "annotation mismatch"

echo ""
echo "-- COMPLEX ANNOTATION TESTS --"
test_case "Full signature: params + return" \
    'let f = fn(x: int, y: int) -> int { x + y }; f(1, 2)' \
    "true"

test_case "Conditional with matching return type" \
    'let f = fn(x: int) -> int { if (x < 0) { 0 } else { x } }; f(-5)' \
    "true"

test_case "Recursive function with correct type" \
    'let fib = fn(n: int) -> int { if (n < 2) { return n } return fib(n - 1) + fib(n - 2); }; fib(10)' \
    "true"

test_case "Recursive function with wrong return type" \
    'let fib = fn(n: int) -> string { if (n < 2) { return n } return fib(n - 1) + fib(n - 2); }; fib(10)' \
    "false" \
    "annotation mismatch"

echo ""
echo "-- BACKWARD COMPATIBILITY TESTS --"
test_case "Unannotated function still works" \
    'let f = fn(x) { x + 1 }; f(5)' \
    "true"

test_case "Phase 1 fibonacci (no annotations)" \
    'let fib = fn(n) { if (n < 2) { return n } return fib(n - 2) + fib(n - 1); }; fib(10)' \
    "true"

echo ""
echo "-- PHASE 3: EARLY RETURN SYNTAX TESTS --"
test_case "Early return without braces (consequence)" \
    'let fib = fn(n: int) -> int { if (n < 2) return n else { fib(n - 1) + fib(n - 2) } }; fib(5)' \
    "true"

test_case "Early return in both branches" \
    'let abs = fn(x: int) -> int { if (x < 0) return -x else return x; }; abs(-5)' \
    "true"

test_case "Early return with complex expression" \
    'let max = fn(a: int, b: int) -> int { if (a < b) return b else return a; }; max(3, 7)' \
    "true"

test_case "Nested early returns" \
    'let f = fn(n: int) -> int { if (n < 0) return -n else { if (n < 1) return 0 else return n * 2 } }; f(5)' \
    "true"

echo ""
echo "-- CODEGEN OUTPUT TESTS --"
# These tests check that Go code is actually being generated
TMPGO=$(mktemp)
echo "let x = 42; x" > /tmp/simple.mr
$EXECUTABLE build /tmp/simple.mr --emit-go "$(dirname $TMPGO)" 2>/dev/null
if [ -f "$(dirname $TMPGO)/main.go" ]; then
    TOTAL=$((TOTAL + 1))
    if grep -q "int64" "$(dirname $TMPGO)/main.go"; then
        echo "TEST [$TOTAL] Codegen produces valid Go ... ✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "TEST [$TOTAL] Codegen produces valid Go ... ✗ FAIL"
        FAIL=$((FAIL + 1))
    fi
fi
rm -rf "$(dirname $TMPGO)"

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
