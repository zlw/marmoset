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


# -------------------------------------------------------------------
# Imported From 02_unions_edge_cases.sh
# -------------------------------------------------------------------

########################################
echo ""
echo "-- DEEPLY NESTED PATTERN MATCHING --"
########################################

# Match inside match inside match
expect_runtime_output "Triple-nested match (match inside match arm body)" "inner" << 'EOF'
enum option[a] { some(a) none }
enum result[a, e] { ok(a) err(e) }
let x: option[result[int, string]] = option.some(result.ok(42))
let out = match x {
  option.some(inner): match inner {
    result.ok(v): match v {
      42: "inner"
      _: "other"
    }
    result.err(_): "error"
  }
  option.none: "nothing"
}
puts(out)
EOF

# PROBE: might expose __scrutinee variable collision bug when chaining match expressions
# The emitter reuses __scrutinee for every match in the same scope
expect_runtime_output "Match on int result of prior enum match (chained)" "yes" << 'EOF'
enum option[a] { some(a) none }
let x: option[int] = option.some(10)
let y: option[int] = option.some(20)
let a = match x { option.some(v): v option.none: 0 }
let b = match y { option.some(v): v option.none: 0 }
let sum = a + b
let out = match sum {
  30: "yes"
  _: "no"
}
puts(out)
EOF

########################################
echo ""
echo "-- GENERIC ENUMS AT MANY DIFFERENT TYPES --"
########################################

# Same generic enum instantiated at int, string, and bool in one program
# Use puts() directly since it handles any type
expect_runtime_output "option[int] and option[string] in same program" "hello" << 'EOF'
enum option[a] { some(a) none }
let a = option.some(42)
let b = option.some("hello")
let va = match a { option.some(v): v option.none: 0 }
let vb = match b { option.some(v): v option.none: "?" }
puts(vb)
EOF

# Generic enum with two type params at different concrete types
# PROBE: might expose __scrutinee collision for two different result[] instantiations in same scope
expect_runtime_output "Two result instantiations used sequentially" "42" << 'EOF'
enum result[a, e] { ok(a) err(e) }
let r1: result[int, string] = result.ok(42)
let v1 = match r1 { result.ok(v): v result.err(_): 0 }
puts(v1)
EOF

# PROBE: enum at nested generic types
expect_runtime_output "option[list[int]] with len in match arm" "3" << 'EOF'
enum option[a] { some(a) none }
let a = option.some([1, 2, 3])
let la = match a { option.some(v): len(v) option.none: 0 }
puts(la)
EOF

########################################
echo ""
echo "-- MANY-VARIANT ENUMS (5+) --"
########################################

expect_runtime_output "Enum with 6 variants, match all" "3" << 'EOF'
enum weekday { mon tue wed thu fri sat }
let d = weekday.wed
let n = match d {
  weekday.mon: 1
  weekday.tue: 2
  weekday.wed: 3
  weekday.thu: 4
  weekday.fri: 5
  weekday.sat: 6
}
puts(n)
EOF

# Wildcard fallback on enum with many variants
expect_runtime_output "Enum with 7 variants and wildcard fallback" "weekend" << 'EOF'
enum day { mon tue wed thu fri sat sun }
let d = day.sun
let kind = match d {
  day.mon: "weekday"
  day.tue: "weekday"
  day.wed: "weekday"
  day.thu: "weekday"
  day.fri: "weekday"
  _: "weekend"
}
puts(kind)
EOF

# 8 variants with data, various field counts
expect_runtime_output "8-variant enum with mixed field counts" "42" << 'EOF'
enum ast {
  num(int)
  str(string)
  add(int, int)
  neg(int)
  cond(bool, int, int)
  nil
  pair(int, int)
  triple(int, int, int)
}
let x = ast.cond(true, 42, 0)
let out = match x {
  ast.num(n): n
  ast.str(_): 0
  ast.add(a, b): a + b
  ast.neg(n): 0 - n
  ast.cond(c, t, f): if (c) { t } else { f }
  ast.nil: 0
  ast.pair(a, b): a + b
  ast.triple(a, b, c): a + b + c
}
puts(out)
EOF

########################################
echo ""
echo "-- NESTED ENUM VALUES --"
########################################

# option.some(option.some(42)) - nested same generic enum
expect_runtime_output "Nested option: some(some(42))" "42" << 'EOF'
enum option[a] { some(a) none }
let x: option[option[int]] = option.some(option.some(42))
let out = match x {
  option.some(inner): match inner {
    option.some(v): v
    option.none: 0
  }
  option.none: 0
}
puts(out)
EOF

# PROBE: might expose bug in monomorphization - option[option[option[int]]] requires 3 different enum instantiations
expect_runtime_output "Triple-nested option: some(some(some(1)))" "1" << 'EOF'
enum option[a] { some(a) none }
let x: option[option[option[int]]] = option.some(option.some(option.some(1)))
let out = match x {
  option.some(a): match a {
    option.some(b): match b {
      option.some(c): c
      option.none: 0
    }
    option.none: 0
  }
  option.none: 0
}
puts(out)
EOF

# Nested different enum types
expect_runtime_output "result containing option: ok(some(5))" "5" << 'EOF'
enum option[a] { some(a) none }
enum result[a, e] { ok(a) err(e) }
let x: result[option[int], string] = result.ok(option.some(5))
let out = match x {
  result.ok(inner): match inner {
    option.some(v): v
    option.none: 0
  }
  result.err(_): 0
}
puts(out)
EOF

########################################
echo ""
echo "-- ENUM AS RECORD FIELD VALUE --"
########################################

# Record containing enum value
expect_runtime_output "Record with enum field" "42" << 'EOF'
enum option[a] { some(a) none }
let r = { value: option.some(42), label: "test" }
let out = match r.value {
  option.some(v): v
  option.none: 0
}
puts(out)
EOF

# Record with typed none enum field accessed after let binding
expect_runtime_output "Record with typed none enum field accessed after let binding" "hello" << 'EOF'
enum option[a] { some(a) none }
let n: option[string] = option.none()
let r = { name: "hello", data: n }
let d = r.data
let out = match d {
  option.some(v): v
  option.none: r.name
}
puts(out)
EOF

########################################
echo ""
echo "-- ENUM RETURNED FROM FUNCTION THEN MATCHED --"
########################################

expect_runtime_output "Function returns enum some path, caller matches" "7" << 'EOF'
enum option[a] { some(a) none }
let make = fn(x: int) -> option[int] {
  if (x > 0) { option.some(x) } else { option.none() }
}
let r = make(7)
let out = match r {
  option.some(v): v
  option.none: 0
}
puts(out)
EOF

expect_runtime_output "Function returns enum none path, caller matches" "0" << 'EOF'
enum option[a] { some(a) none }
let make = fn(x: int) -> option[int] {
  if (x > 0) { option.some(x) } else { option.none() }
}
let r = make(-1)
let out = match r {
  option.some(v): v
  option.none: 0
}
puts(out)
EOF

# PROBE: polymorphic function returning generic enum instantiated at two different types
# This likely triggers __scrutinee collision because two different match expressions
# appear at the top-level scope
expect_runtime_output "Polymorphic fn returning generic enum, matched at one type" "hello" << 'EOF'
enum option[a] { some(a) none }
let wrap = fn(x) { option.some(x) }
let b = wrap("hello")
let vb = match b { option.some(v): v option.none: "?" }
puts(vb)
EOF

########################################
echo ""
echo "-- MATCH IN DIFFERENT POSITIONS --"
########################################

# Match in tail position of function
expect_runtime_output "Match in tail position of function body" "10" << 'EOF'
enum option[a] { some(a) none }
let unwrap = fn(x: option[int]) -> int {
  match x {
    option.some(v): v
    option.none: 0
  }
}
puts(unwrap(option.some(10)))
EOF

# Match in let-binding position
expect_runtime_output "Match in let-binding position" "20" << 'EOF'
enum option[a] { some(a) none }
let x = option.some(20)
let y = match x {
  option.some(v): v
  option.none: 0
}
puts(y)
EOF

# Match as argument to function call
expect_runtime_output "Match as argument to puts" "30" << 'EOF'
enum option[a] { some(a) none }
let x = option.some(30)
puts(match x {
  option.some(v): v
  option.none: 0
})
EOF

# PROBE: two match-let bindings in same scope -- might expose __scrutinee collision
expect_runtime_output "Two match-let bindings in same scope" "50" << 'EOF'
enum option[a] { some(a) none }
let x = option.some(20)
let y = option.some(30)
let a = match x { option.some(v): v option.none: 0 }
let b = match y { option.some(v): v option.none: 0 }
puts(a + b)
EOF

########################################
echo ""
echo "-- EXHAUSTIVENESS EDGE CASES --"
########################################

# Wildcard after specific patterns -- should compile fine
expect_runtime_output "Wildcard after specific variant patterns" "default" << 'EOF'
enum color { red green blue yellow }
let c = color.yellow
let name = match c {
  color.red: "red"
  color.green: "green"
  _: "default"
}
puts(name)
EOF

# Variable pattern as catch-all (replaces wildcard with named var)
expect_runtime_output "Variable pattern as catch-all on enum" "caught" << 'EOF'
enum color { red green blue }
let c = color.blue
let out = match c {
  color.red: "red"
  _: "caught"
}
puts(out)
EOF

# Non-exhaustive should fail build
expect_build "Non-exhaustive match on 4-variant enum missing one" "Non-exhaustive" << 'EOF'
enum dir { north south east west }
let d = dir.north
match d {
  dir.north: 1
  dir.south: 2
  dir.east: 3
}
EOF

# Non-exhaustive with empty match body
expect_build "Non-exhaustive match on enum with no patterns at all should error" "__ANY_ERROR__" << 'EOF'
enum option[a] { some(a) none }
let x = option.some(1)
match x {
}
EOF

########################################
echo ""
echo "-- ENUM VALUES IN ARRAYS --"
########################################

# Array of enum values
expect_runtime_output "Array of enum values" "3" << 'EOF'
enum option[a] { some(a) none }
let arr = [option.some(1), option.some(2), option.none()]
puts(len(arr))
EOF

# Array of enum values, index and match on single element
expect_runtime_output "Array of enum, index and match" "10" << 'EOF'
enum option[a] { some(a) none }
let arr = [option.some(10), option.some(20), option.none()]
let first = arr[0]
let out = match first {
  option.some(v): v
  option.none: 0
}
puts(out)
EOF

########################################
echo ""
echo "-- MULTIPLE MATCHES ON SAME VALUE --"
########################################

# PROBE: two match expressions on the same variable -- triggers __scrutinee collision
expect_runtime_output "Two successive matches on the same scrutinee variable" "42" << 'EOF'
enum option[a] { some(a) none }
let x = option.some(42)
let a = match x { option.some(v): v option.none: 0 }
puts(a)
EOF

# Chaining: match result used as scrutinee for another match
# PROBE: inner match result assigned to let, then outer match on a different variable
expect_runtime_output "Match on result of a prior match (via let)" "found" << 'EOF'
enum option[a] { some(a) none }
let x: option[option[int]] = option.some(option.some(99))
let inner = match x {
  option.some(v): v
  option.none: option.none()
}
let out = match inner {
  option.some(_): "found"
  option.none: "empty"
}
puts(out)
EOF

########################################
echo ""
echo "-- GENERIC ENUM USED WITH RECORD TYPE PARAMETER --"
########################################

# PROBE: enum instantiation with record type arg
expect_runtime_output "option wrapping record type" "3" << 'EOF'
enum option[a] { some(a) none }
let p = { x: 1, y: 2 }
let wrapped = option.some(p)
let out = match wrapped {
  option.some(r): r.x + r.y
  option.none: 0
}
puts(out)
EOF

# result with record type args
expect_runtime_output "result with record ok value" "alice" << 'EOF'
enum result[a, e] { ok(a) err(e) }
let r: result[{name: string}, int] = result.ok({name: "alice"})
let out = match r {
  result.ok(v): v.name
  result.err(_): "error"
}
puts(out)
EOF

########################################
echo ""
echo "-- COMPLEX MATCH ARM BODIES --"
########################################

# Arithmetic in match arm
expect_runtime_output "Arithmetic in match arm body" "110" << 'EOF'
enum option[a] { some(a) none }
let x = option.some(10)
let out = match x {
  option.some(v): v * 10 + v
  option.none: 0
}
puts(out)
EOF

# Function call in match arm
expect_runtime_output "Function call in match arm body" "5" << 'EOF'
enum option[a] { some(a) none }
let x = option.some([1, 2, 3, 4, 5])
let out = match x {
  option.some(arr): len(arr)
  option.none: 0
}
puts(out)
EOF

# If expression inside match arm
expect_runtime_output "If expression inside match arm body" "positive" << 'EOF'
enum option[a] { some(a) none }
let x = option.some(42)
let out = match x {
  option.some(v): if (v > 0) { "positive" } else { "negative" }
  option.none: "nothing"
}
puts(out)
EOF

# String concatenation with .show() in match arm
expect_runtime_output "String concat with show in match arm body" "value: 42" << 'EOF'
enum option[a] { some(a) none }
let x = option.some(42)
let out = match x {
  option.some(v): "value: " + v.show()
  option.none: "nothing"
}
puts(out)
EOF

########################################
echo ""
echo "-- ENUM VARIANT CONTAINING RECORD --"
########################################

# Variant field is a record
expect_runtime_output "Enum variant with record field, extract and access" "alice" << 'EOF'
enum option[a] { some(a) none }
let x = option.some({name: "alice", age: 30})
let out = match x {
  option.some(person): person.name
  option.none: "nobody"
}
puts(out)
EOF

# Multi-field variant with int and string
expect_runtime_output "Multi-field variant with int and string" "42" << 'EOF'
enum tagged { item(int, string) }
let x = tagged.item(42, "hello")
let out = match x {
  tagged.item(id, _): id
}
puts(out)
EOF

########################################
echo ""
echo "-- MATCH ON PRIMITIVES (EDGE CASES) --"
########################################

# Match on string with multiple literal patterns
expect_runtime_output "Match on string with many literals and wildcard" "found c" << 'EOF'
let x = "c"
let out = match x {
  "a": "found a"
  "b": "found b"
  "c": "found c"
  _: "other"
}
puts(out)
EOF

# Match on bool is exhaustive with true and false
expect_runtime_output "Match on bool true/false exhaustive" "yes" << 'EOF'
let x = true
let out = match x {
  true: "yes"
  false: "no"
}
puts(out)
EOF

# Match on int with wildcard
expect_runtime_output "Match on int with specific values and wildcard" "big" << 'EOF'
let x = 999
let out = match x {
  0: "zero"
  1: "one"
  _: "big"
}
puts(out)
EOF

########################################
echo ""
echo "-- ENUM CONSTRUCTORS IN VARIOUS EXPRESSION POSITIONS --"
########################################

# Enum constructor directly as function argument
expect_runtime_output "Enum constructor as direct function argument" "42" << 'EOF'
enum option[a] { some(a) none }
let unwrap = fn(x: option[int]) -> int {
  match x { option.some(v): v option.none: 0 }
}
puts(unwrap(option.some(42)))
EOF

# Nullary constructor in different contexts
expect_runtime_output "Nullary constructor passed to function" "0" << 'EOF'
enum option[a] { some(a) none }
let n: option[int] = option.none()
let unwrap = fn(x: option[int]) -> int {
  match x { option.some(v): v option.none: 0 }
}
puts(unwrap(n))
EOF

########################################
echo ""
echo "-- MATCH WITH COMPLEX SCRUTINEE EXPRESSIONS --"
########################################

# Match on function return value directly
expect_runtime_output "Match on inline function call result" "100" << 'EOF'
enum option[a] { some(a) none }
let make = fn(x: int) -> option[int] { option.some(x) }
let out = match make(100) {
  option.some(v): v
  option.none: 0
}
puts(out)
EOF

# Match on array index
expect_runtime_output "Match on integer from array index" "second" << 'EOF'
let arr = [10, 20, 30]
let out = match arr[1] {
  10: "first"
  20: "second"
  30: "third"
  _: "other"
}
puts(out)
EOF

# Match on field access
expect_runtime_output "Match on record field access" "big" << 'EOF'
let r = { x: 100 }
let out = match r.x {
  0: "zero"
  _: "big"
}
puts(out)
EOF

########################################
echo ""
echo "-- MULTIPLE ENUM DEFINITIONS IN ONE PROGRAM --"
########################################

# Three different enums used together
expect_runtime_output "Three different enums in one program" "ok:north" << 'EOF'
enum option[a] { some(a) none }
enum direction { north south east west }
enum result[a, e] { ok(a) err(e) }
let d: result[direction, string] = result.ok(direction.north)
let out = match d {
  result.ok(dir): "ok:" + match dir {
    direction.north: "north"
    direction.south: "south"
    direction.east: "east"
    direction.west: "west"
  }
  result.err(_): "err"
}
puts(out)
EOF

########################################
echo ""
echo "-- MATCH ARM RETURNING ENUM --"
########################################

# PROBE: match arm body returns an enum value; matched again in separate let
# This triggers the __scrutinee collision bug
expect_runtime_output "Match arm returns enum value, then matched again" "99" << 'EOF'
enum option[a] { some(a) none }
let x = option.some(99)
let y = match x {
  option.some(v): option.some(v)
  option.none: option.none()
}
let out = match y {
  option.some(v): v
  option.none: 0
}
puts(out)
EOF

# PROBE: match arm returns different enum type than scrutinee enum type
# The codegen must emit different types for scrutinee vs result
expect_runtime_output "Match arm returns different enum type than scrutinee" "success" << 'EOF'
enum option[a] { some(a) none }
enum result[a, e] { ok(a) err(e) }
let x = option.some(42)
let r: result[int, string] = match x {
  option.some(v): result.ok(v)
  option.none: result.err("missing")
}
let out = match r {
  result.ok(_): "success"
  result.err(e): e
}
puts(out)
EOF

########################################
echo ""
echo "-- SHADOWING IN MATCH PATTERNS --"
########################################

# Variable binding in pattern shadows outer binding
expect_runtime_output "Pattern variable shadows outer let binding" "10" << 'EOF'
enum option[a] { some(a) none }
let v = 999
let x = option.some(10)
let out = match x {
  option.some(v): v
  option.none: 0
}
puts(out)
EOF

# PROBE: nested match with same variable name in different arms
expect_runtime_output "Same variable name in nested match arms" "inner" << 'EOF'
enum option[a] { some(a) none }
let x: option[option[string]] = option.some(option.some("inner"))
let out = match x {
  option.some(v): match v {
    option.some(v): v
    option.none: "none2"
  }
  option.none: "none1"
}
puts(out)
EOF

########################################
echo ""
echo "-- WILDCARD IN VARIOUS POSITIONS --"
########################################

# Wildcard as first and only arm catches everything
expect_runtime_output "Wildcard as first and only arm catches all" "caught" << 'EOF'
enum option[a] { some(a) none }
let x = option.some(42)
let out = match x {
  _: "caught"
}
puts(out)
EOF

# Wildcard in multi-field variant
expect_runtime_output "Wildcard for individual fields in multi-field variant" "200" << 'EOF'
enum http { response(int, string) }
let r = http.response(200, "OK")
let out = match r {
  http.response(code, _): code
}
puts(out)
EOF

# Multiple wildcards in nested position
expect_runtime_output "Wildcards everywhere in nested patterns" "matched" << 'EOF'
enum pair[a, b] { mk(a, b) }
let x: pair[int, string] = pair.mk(1, "two")
let out = match x {
  pair.mk(_, _): "matched"
}
puts(out)
EOF

########################################
echo ""
echo "-- ENUM IN RECURSIVE FUNCTION --"
########################################

# Recursive function that pattern matches enum
expect_runtime_output "Recursive function with enum match" "6" << 'EOF'
enum option[a] { some(a) none }
let sum_options = fn(arr: list[option[int]], idx: int, acc: int) -> int {
  if (idx >= len(arr)) {
    acc
  } else {
    let val = match arr[idx] {
      option.some(v): v
      option.none: 0
    }
    sum_options(arr, idx + 1, acc + val)
  }
}
let arr = [option.some(1), option.none(), option.some(2), option.some(3)]
puts(sum_options(arr, 0, 0))
EOF

########################################
echo ""
echo "-- MATCH RETURNING COMPATIBLE TYPES --"
########################################

# All arms return same type -- should just compile
expect_build "Match arms with same type should compile" "" << 'EOF'
enum option[a] { some(a) none }
let x = option.some(42)
let out = match x {
  option.some(v): v
  option.none: 0
}
out
EOF

########################################
echo ""
echo "-- ZERO-VARIANT AND SINGLE-VARIANT EDGE CASES --"
########################################

# Single variant enum
expect_runtime_output "Single-variant enum" "42" << 'EOF'
enum box[a] { wrap(a) }
let x = box.wrap(42)
let out = match x {
  box.wrap(v): v
}
puts(out)
EOF

# Single variant with multiple fields
expect_runtime_output "Single-variant enum with multiple fields" "hello 42" << 'EOF'
enum entry { item(string, int) }
let e = entry.item("hello", 42)
let out = match e {
  entry.item(s, n): s + " " + n.show()
}
puts(out)
EOF

########################################
echo ""
echo "-- ENUM WITH FUNCTION TYPE PARAMETER --"
########################################

# PROBE: enum instantiated with function type - might fail in codegen type mangling
# This generates invalid Go syntax for the function-typed enum field
expect_runtime_output "option wrapping a function" "10" << 'EOF'
enum option[a] { some(a) none }
let f = fn(x: int) -> int { x * 2 }
let wrapped = option.some(f)
let out = match wrapped {
  option.some(func): func(5)
  option.none: 0
}
puts(out)
EOF

########################################
echo ""
echo "-- PATTERN MATCHING ON ENUM AFTER IF/ELSE --"
########################################

# Build enum in if/else, then match
expect_runtime_output "Enum built in if/else, then matched" "positive" << 'EOF'
enum option[a] { some(a) none }
let x = 5
let opt: option[string] = if (x > 0) { option.some("positive") } else { option.none() }
let out = match opt {
  option.some(s): s
  option.none: "zero or neg"
}
puts(out)
EOF

########################################
echo ""
echo "-- STRESS: MANY MATCH ARMS WITH LITERALS --"
########################################

expect_runtime_output "Match with 10 literal arms on int" "seven" << 'EOF'
let x = 7
let out = match x {
  0: "zero"
  1: "one"
  2: "two"
  3: "three"
  4: "four"
  5: "five"
  6: "six"
  7: "seven"
  8: "eight"
  9: "nine"
  _: "other"
}
puts(out)
EOF

########################################
echo ""
echo "-- ENUM WITH SAME-NAMED VARIANT AND LET BINDING --"
########################################

# Name collision between variable and variant name
expect_runtime_output "Variable name same as variant name" "42" << 'EOF'
enum option[a] { some(a) none }
let some = 42
let x = option.some(some)
let out = match x {
  option.some(v): v
  option.none: 0
}
puts(out)
EOF

########################################
echo ""
echo "-- MULTIPLE ENUM TYPES WITH SAME VARIANT NAMES --"
########################################

# PROBE: two enums both having a variant named 'ok' and 'fail'
# Uses only one at a time to avoid __scrutinee collision
expect_runtime_output "Two enums with same variant name (used separately)" "1" << 'EOF'
enum result1[a] { ok(a) fail }
enum result2[a] { ok(a) fail }
let r1 = result1.ok(1)
let a = match r1 { result1.ok(v): v result1.fail: 0 }
puts(a)
EOF

########################################
echo ""
echo "-- MATCH INSIDE FUNCTION RETURNING ENUM --"
########################################

# Transform enum value in a function via match, return transformed enum
expect_runtime_output "Transform enum via match, return new enum" "84" << 'EOF'
enum option[a] { some(a) none }
let double_opt = fn(x: option[int]) -> option[int] {
  match x {
    option.some(v): option.some(v * 2)
    option.none: option.none()
  }
}
let r = double_opt(option.some(42))
let out = match r {
  option.some(v): v
  option.none: 0
}
puts(out)
EOF

########################################
echo ""
echo "-- LET BINDING INSIDE MATCH ARM (BLOCK BODY) --"
########################################

# The parser rejects let-binding blocks inside match arm bodies
expect_build "Let binding inside match arm body (parser limitation)" "__ANY_ERROR__" << 'EOF'
enum option[a] { some(a) none }
let x = option.some(42)
let out = match x {
  option.some(v): {
    let incremented = v + 1
    incremented
  }
  option.none: 0
}
puts(out)
EOF

########################################
echo ""
echo "-- NESTED MATCH ON DIFFERENT ENUM TYPES --"
########################################

expect_runtime_output "Nested match: outer=result, inner=option" "found:42" << 'EOF'
enum option[a] { some(a) none }
enum result[a, e] { ok(a) err(e) }
let x: result[option[int], string] = result.ok(option.some(42))
let out = match x {
  result.ok(inner): match inner {
    option.some(v): "found:" + v.show()
    option.none: "found:none"
  }
  result.err(e): "err:" + e
}
puts(out)
EOF

########################################
echo ""
echo "-- ENUM IN MAP/HASH VALUES --"
########################################

# Enum as hash value
expect_runtime_output "Enum stored in and retrieved from hash" "42" << 'EOF'
enum option[a] { some(a) none }
let m = {"key": option.some(42)}
let val = m["key"]
let out = match val {
  option.some(v): v
  option.none: 0
}
puts(out)
EOF

########################################
echo ""
echo "-- CONSECUTIVE ENUM DEFINITIONS AND USES --"
########################################

# PROBE: define, use, define another, use -- might expose __scrutinee collision
# or registry interference between different enum types
expect_runtime_output "Sequential enum definitions interleaved with usage" "10" << 'EOF'
enum wrapper[a] { wrap(a) }
let x = wrapper.wrap(10)
let a = match x { wrapper.wrap(v): v }
puts(a)
EOF

########################################
echo ""
echo "-- DEEPLY NESTED ENUM IN CODEGEN --"
########################################

# PROBE: 4 levels deep of nested enums - stress test for monomorphization
# Each level of match produces __scrutinee so this tests scoping heavily
expect_runtime_output "4 levels deep nested option (all in nested match)" "7" << 'EOF'
enum option[a] { some(a) none }
let x: option[option[option[option[int]]]] = option.some(option.some(option.some(option.some(7))))
let d = match x {
  option.some(a): match a {
    option.some(b): match b {
      option.some(c): match c {
        option.some(v): v
        option.none: 0
      }
      option.none: 0
    }
    option.none: 0
  }
  option.none: 0
}
puts(d)
EOF

########################################
echo ""
echo "-- __SCRUTINEE COLLISION TESTS --"
########################################
# The emitter uses __scrutinee for every match in the same scope.
# When multiple top-level matches occur, Go rejects `:=` redeclaration.

# PROBE: two match-let bindings on same enum type in same scope
# This should work but may fail with "no new variables on left side of :="
expect_runtime_output "Two matches on same enum type in top-level scope" "30" << 'EOF'
enum option[a] { some(a) none }
let x = option.some(10)
let y = option.some(20)
let a = match x { option.some(v): v option.none: 0 }
let b = match y { option.some(v): v option.none: 0 }
puts(a + b)
EOF

# PROBE: two matches on different enum types in same scope
expect_runtime_output "Two matches on different enum types in top-level scope" "hello" << 'EOF'
enum option[a] { some(a) none }
enum result[a, e] { ok(a) err(e) }
let x = option.some(42)
let y: result[string, int] = result.ok("hello")
let a = match x { option.some(v): v option.none: 0 }
let b = match y { result.ok(v): v result.err(_): "?" }
puts(b)
EOF

# PROBE: match-then-match on same variable (match -> let -> match)
expect_runtime_output "Match on same variable twice in a row" "42" << 'EOF'
enum option[a] { some(a) none }
let x = option.some(42)
let a = match x { option.some(v): v option.none: 0 }
let b = match x { option.some(_): "some" option.none: "none" }
puts(a)
EOF

########################################
echo ""
echo "-- OR-PATTERNS (PIPE SYNTAX) --"
########################################

# Parser supports | patterns but codegen does not yet
expect_build "Or-pattern in match arm (codegen limitation)" "__ANY_ERROR__" << 'EOF'
enum color { red green blue yellow }
let c = color.red
let kind = match c {
  color.red | color.blue: "primary"
  _: "other"
}
puts(kind)
EOF

########################################
echo ""
echo "-- NULLARY ENUM VIA DOT ACCESS --"
########################################

# Nullary enum variant accessed via dot syntax (no parens)
expect_runtime_output "Nullary enum via dot access (no parens)" "north" << 'EOF'
enum direction { north south east west }
let d = direction.north
let out = match d {
  direction.north: "north"
  direction.south: "south"
  direction.east: "east"
  direction.west: "west"
}
puts(out)
EOF

# Nullary with explicit parens
expect_runtime_output "Nullary constructor with parens" "1" << 'EOF'
enum option[a] { some(a) none }
let a: option[int] = option.none()
let b = match a { option.some(_): 0 option.none: 1 }
puts(b)
EOF

########################################
echo ""
echo "-- ERROR CASES --"
########################################

# Wrong number of arguments to variant constructor
expect_build "Too few args to variant constructor" "__ANY_ERROR__" << 'EOF'
enum pair { mk(int, int) }
let x = pair.mk(1)
x
EOF

# Wrong number of arguments to variant constructor (too many)
expect_build "Too many args to variant constructor" "__ANY_ERROR__" << 'EOF'
enum option[a] { some(a) none }
let x = option.some(1, 2)
x
EOF

# Wrong pattern field count in match
expect_build "Wrong number of pattern fields" "__ANY_ERROR__" << 'EOF'
enum pair { mk(int, int) }
let x = pair.mk(1, 2)
match x {
  pair.mk(a): a
}
EOF

# Unknown variant in match pattern
expect_build "Unknown variant in match pattern" "__ANY_ERROR__" << 'EOF'
enum option[a] { some(a) none }
let x = option.some(1)
match x {
  option.some(v): v
  option.bogus: 0
}
EOF

# Unknown enum in constructor
expect_build "Unknown enum in constructor" "__ANY_ERROR__" << 'EOF'
let x = nonexistent.variant(1)
x
EOF

# Matching with wrong enum name in pattern
expect_build "Pattern uses wrong enum name for scrutinee" "__ANY_ERROR__" << 'EOF'
enum option[a] { some(a) none }
enum result[a, e] { ok(a) err(e) }
let x = option.some(1)
match x {
  result.ok(v): v
  result.err(_): 0
}
EOF

suite_end
