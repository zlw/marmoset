#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

suite_begin "TYPECHECK & CODEGEN INTEGRATION TESTS - CORE & ANNOTATIONS"
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
test_emit_go_contains "Codegen produces valid Go" "let x = 42; x" "int64"


suite_end
