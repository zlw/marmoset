#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

suite_begin "TYPECHECK & CODEGEN INTEGRATION TESTS - UNIONS"
echo "-- PHASE 4.1: UNION TYPE TESTS --"
test_case "Parse union in parameter type" \
    'let f = fn(x: int | string) -> string { "ok" }; f(5)' \
    "true"

test_case "Parse union in return type" \
    'let f = fn(x: int) -> int | string { if (x > 0) { x } else { "negative" } }; f(5)' \
    "true"

test_case "Parse multi-member union" \
    'let f = fn(x: int | string | bool) { x }; f(true)' \
    "true"

test_case "is operator returns bool" \
    'let x: int | string = 5; if (x is int) { true } else { false }' \
    "true"

test_case "Type narrowing allows int operations" \
    'let f = fn(x: int | string) -> int | string {
       if (x is int) {
           x + 1
       } else {
           x
       }
     }; f(5)' \
    "true"

echo ""
echo "-- PHASE 4.1: EXHAUSTIVENESS & EDGE CASES --"

test_case "Complement narrowing in else branch" \
    'let f = fn(x: int | string) -> string {
       if (x is int) {
           "was int"
       } else {
           x
       }
     }; f("hello")' \
    "true"

test_case "Three-way union exhaustiveness" \
    'let f = fn(x: int | string | bool) -> string {
       if (x is int) {
           "int"
       } else {
           if (x is string) {
               x
           } else {
               "bool"
           }
       }
     }; f(true)' \
    "true"

test_case "Nested type narrowing" \
    'let outer = fn(x: int | string) -> int {
       if (x is int) {
           let inner = fn(y: int | bool) -> int {
               if (y is int) { y } else { 0 }
           };
           inner(x)
       } else {
           0
       }
     }; outer(42)' \
    "true"

test_case "Type narrowing with string operations" \
    'let f = fn(x: int | string) -> int {
       if (x is string) {
           len(x)
       } else {
           x
       }
     }; f(42)' \
    "true"

test_case "Union return from both branches" \
    'let f = fn(b: bool, x: int | string) -> int | string {
       if (b) { x } else { x }
     }; f(true, 5)' \
    "true"

suite_end
