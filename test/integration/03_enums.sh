#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

suite_begin "TYPECHECK & CODEGEN INTEGRATION TESTS - ENUMS"
echo "-- ENUM CONSTRUCTOR TESTS (Phase 4.2) --"
test_case "Simple enum definition" \
    'enum direction { north south east west }' \
    "true"

expect_build "Duplicate enum definition is rejected in one program" "Duplicate enum definition: direction" << 'EOF'
enum direction { north south east west }
enum direction { up down }
puts(1)
EOF

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
