#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

suite_begin "TYPECHECK & CODEGEN INTEGRATION TESTS - UNIONS & ENUMS"
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

echo ""
echo "-- ENUM CONSTRUCTOR TESTS (Phase 4.2) --"
test_case "Simple enum definition" \
    'enum direction { north south east west }' \
    "true"

test_case "Generic enum option[a] with some constructor" \
    'enum option[a] { some(a) none }
     let x = option.some(42)
     x' \
    "true"

test_case "Generic enum option[a] with none constructor" \
    'enum option[a] { some(a) none }
     let x = option.some(42)
     let y = match x {
       option.some(v): v
       option.none: 0
     }
     y' \
    "true"

test_case "Enum list[a] with recursive structure" \
    'enum list[a] { cons(a) nil }
     let x = list.cons(42)
     x' \
    "true"

test_case "Enum with multi-field variant" \
    'enum http_response { ok(int, string) error(int, string) }
     let x = http_response.ok(200, "OK")
     x' \
    "true"

echo ""
echo "-- MATCH EXPRESSION TESTS (Phase 4.2) --"
test_case "Match on simple enum variant" \
    'enum option[a] { some(a) none }
     let x = option.some(42)
     let y = match x {
       option.some(v): v
       option.none: 0
     }
     y' \
    "true"

test_case "Match with wildcard pattern" \
    'enum option[a] { some(a) none }
     let x = option.some(42)
     let y = match x {
       option.some(v): v
       _: 0
     }
     y' \
    "true"

test_case "Match with literal patterns" \
    'let x = 42
     let y = match x {
       0: "zero"
       1: "one"
       _: "other"
     }
     y' \
    "true"

test_case "Match with variable binding" \
    'let x = 42
     let y = match x {
       n: n + 1
     }
     y' \
    "true"

echo ""
echo "-- ENUM TYPE ANNOTATIONS (Phase 4.3) --"
test_case "Function parameter with enum type option[int]" \
    'enum option[a] { some(a) none }
     let unwrap = fn(x: option[int]) -> int {
       match x {
         option.some(v): v
         option.none: 0
       }
     }
     unwrap(option.some(42))' \
    "true"

test_case "Function returning enum type" \
    'enum option[a] { some(a) none }
     let wrap = fn(x: int) -> option[int] {
       option.some(x)
     }
     let y = wrap(100)
     match y {
       option.some(v): v
       option.none: 0
     }' \
    "true"

test_case "Nested enum types option[list[int]]" \
    'enum option[a] { some(a) none }
     let f = fn(x: option[list[int]]) -> int {
       match x {
         option.some(lst): len(lst)
         option.none: 0
       }
     }
     f(option.some([1, 2, 3]))' \
    "true"

echo ""
echo "-- PHASE 4.4: MULTI-FIELD ENUM VARIANTS --"

test_case "Multi-field variant extraction" \
    'enum http_response { ok(int, string) error(int, string) }
     let x = http_response.ok(200, "OK")
     match x {
       http_response.ok(code, _): code
       http_response.error(code, _): code
     }' \
    "true"

test_case "Heterogeneous result type" \
    'enum result[a, b] { ok(a) error(b) }
     let x: result[int, string] = result.ok(42)
     match x {
       result.ok(v): v + 1
       result.error(_): 0
     }' \
    "true"

test_case "Three-field variant" \
    'enum triple { t(int, string, bool) }
     let x = triple.t(1, "two", true)
     match x {
       triple.t(a, _, _): a
     }' \
    "true"

test_case "Mixed field count variants" \
    'enum mixed { none zero(int) pair(int, int) triple(int, int, int) }
     let x = mixed.pair(1, 2)
     match x {
       mixed.none: 0
       mixed.zero(a): a
       mixed.pair(a, _): a
       mixed.triple(a, _, _): a
     }' \
    "true"


suite_end
