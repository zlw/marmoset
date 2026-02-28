#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

suite_begin "TYPECHECK & CODEGEN INTEGRATION TESTS - CODEGEN P0"
echo "-- P0.1: TYPE MAP COMPLETENESS (NO EMITTER RE-INFERENCE) --"

expect_runtime_output "Top-level function calling top-level function compiles and runs" "1" << 'EOF'
let f = fn(x: int) -> int { x }
let g = fn(y: int) -> int { f(y) }
puts(g(1))
EOF

expect_runtime_output "Unannotated higher-order caller handles pure and effectful callbacks" "2
1
3" << 'EOF'
let hof = fn(f) { f(1) }
let pure = fn(x: int) -> int { x + 1 }
let eff = fn(x: int) => int { puts(x); x + 2 }
puts(hof(pure))
puts(hof(eff))
EOF

expect_build "Pure annotated higher-order caller rejects unknown callback calls conservatively" "Pure function (declared with ->) cannot call effectful operations" << 'EOF'
let hof = fn(f) -> int { f(1) }
let eff = fn(x: int) => int { x }
puts(hof(eff))
EOF

expect_runtime_output "Union of pure/effectful callables remains callable in codegen" "1
3" << 'EOF'
let choose = fn(flag: bool) {
  if (flag) {
    fn(x: int) -> int { x + 1 }
  } else {
    fn(x: int) => int { puts(x); x + 2 }
  }
}
let f = choose(false)
puts(f(1))
EOF

expect_runtime_output "Higher-order callback through local alias keeps effectful behavior" "1
3" << 'EOF'
let hof = fn(f) {
  let g = f
  g(1)
}
let eff = fn(x: int) => int { puts(x); x + 2 }
puts(hof(eff))
EOF

expect_build "Union of callable and non-callable value is rejected at call site" "Cannot unify" << 'EOF'
let f = if (true) { fn(x: int) -> int { x + 1 } } else { 0 }
puts(f(1))
EOF

expect_build "Union of callables with mismatched arity is rejected at call site" "Cannot unify" << 'EOF'
let f = if (true) { fn(x: int) -> int { x } } else { fn(x: int, y: int) -> int { x + y } }
puts(f(1))
EOF

expect_runtime_output "Unannotated recursive function with effectful body infers and runs as effectful" "2
1
0" << 'EOF'
let loop = fn(n: int) {
  if (n == 0) {
    0
  } else {
    puts(n)
    loop(n - 1)
  }
}
puts(loop(2))
EOF

expect_build "Pure annotated recursive function with effectful body is rejected" "Pure function (declared with ->) cannot call effectful operations" << 'EOF'
let loop = fn(n: int) -> int {
  if (n == 0) {
    0
  } else {
    puts(n)
    loop(n - 1)
  }
}
puts(loop(2))
EOF

expect_runtime_output "Impl method calling union-param helper resolves using typed env" "int" << 'EOF'
let helper = fn(x: int | string) -> string {
  if (x is int) { "int" } else { "string" }
}
trait show[a] { fn show(x: a) -> string }
impl show for int {
  fn show(x: int) -> string { helper(x) }
}
puts(1.show())
EOF

run_build_ok_not_contains_from_stdin "Successful build emits no missing-type warning text" "missing type for expression id" << 'EOF'
let f = fn(x: int) -> int { x + 1 }
puts(f(1))
EOF

expect_build "Specialized body codegen failure is surfaced cleanly" "Codegen error: Multiple patterns per arm not yet supported in codegen" << 'EOF'
let f = fn(x: int) -> int {
  match x {
    1 | 2: 1
    _: 0
  }
}
puts(f(1))
EOF

run_codegen_deterministic_from_stdin "Codegen output is deterministic for identical input" << 'EOF'
let f = fn(x: int) -> int { x + 1 }
let g = fn(y: int) -> int { f(y) }
puts(g(1))
EOF

test_emit_go_contains "Enum String default branch panics on invalid tag" \
    'enum status { ok fail }
     let x = status.ok
     puts(x)' \
    'panic("unreachable: invalid enum tag")'

run_emit_go_not_contains_from_stdin "Type-check if in let binding avoids IIFE in emitted Go" "func\\(\\) int64" << 'EOF'
let x: int | string = 1
let y = if (x is int) { x + 1 } else { 0 }
puts(y)
EOF

run_emit_go_not_contains_from_stdin "Type-check if in tail position avoids IIFE in emitted Go" "func\\(\\) int64" << 'EOF'
let f = fn(x: int | string) -> int {
  if (x is int) { x + 1 } else { 0 }
}
puts(f(1))
EOF

run_emit_go_not_contains_from_stdin "Match statement avoids IIFE in emitted Go" "func\\(\\) int64" << 'EOF'
match 1 {
  1: 10
  _: 20
}
puts(1)
EOF

expect_build "Tail if without else (unit) compiles" "" << 'EOF'
let f = fn(x: bool) { if (x) { 1 } }
puts(f(true))
EOF

run_emit_go_not_contains_from_stdin "Tail if without else avoids IIFE in emitted Go" "func\\(\\) struct\\{\\}" << 'EOF'
let f = fn(x: bool) { if (x) { 1 } }
puts(f(true))
EOF

expect_build "Type-check if without else in let binding compiles" "" << 'EOF'
let x: int | string = 1
let y = if (x is int) { x + 1 }
puts(y)
EOF

run_emit_go_not_contains_from_stdin "Nested if in tail position avoids IIFE in emitted Go" "func\\(\\) int64" << 'EOF'
let f = fn(a: bool, b: bool) -> int {
  if (a) { if (b) { 1 } else { 2 } } else { 3 }
}
puts(f(true, false))
EOF

run_emit_go_not_contains_from_stdin "Nested match in tail position avoids IIFE in emitted Go" "func\\(\\) int64" << 'EOF'
let f = fn(x: int) -> int {
  match x { 0: match x { 0: 10 _: 20 } _: 30 }
}
puts(f(0))
EOF

run_emit_go_not_contains_from_stdin "Match statement with nested if avoids IIFE in emitted Go" "func\\(\\) int64" << 'EOF'
match 1 {
  1: if (true) { 10 } else { 20 }
  _: 0
}
puts(1)
EOF

run_emit_go_not_contains_from_stdin "Type-check if branch with nested match avoids IIFE in emitted Go" "func\\(\\) int64" << 'EOF'
let x: int | string = 1
let y = if (x is int) {
  match x { 1: 10 _: 20 }
} else {
  0
}
puts(y)
EOF

run_emit_go_not_contains_from_stdin "Nested if containing type-check if avoids IIFE in emitted Go" "func\\(\\) int64" << 'EOF'
let f = fn(x: int | string, y: bool) -> int {
  if (y) {
    if (x is int) { x } else { 0 }
  } else {
    0
  }
}
puts(f(1, true))
EOF

run_emit_go_not_contains_from_stdin "Match in let binding with type-check if arm avoids IIFE in emitted Go" "func\\(\\) int64" << 'EOF'
let x: int | string = 1
let tag = 1
let y = match tag {
  1: if (x is int) { x + 1 } else { 0 }
  _: 0
}
puts(y)
EOF

expect_build "Instantiation fingerprint collision surfaces as clear codegen error" "Codegen error: instantiation fingerprint collision for 'id'" << 'EOF'
enum b { v }
enum a[t] { wrap(t) }
enum a_b { wrap }
let id = fn(x) { x }
let x = id(a_b.wrap)
let y = id(a.wrap(b.v))
puts(1)
EOF

expect_build "Impl instantiation fingerprint collision surfaces as clear codegen error" "Codegen error: impl instantiation fingerprint collision for 'show.show'" << 'EOF'
enum b { v }
enum a[t] { wrap(t) }
enum a_b { wrap }
trait show[x] { fn show(x: x) -> string }
impl show for a_b {
  fn show(x: a_b) -> string { "AB" }
}
impl show for a[b] {
  fn show(x: a[b]) -> string { "AofB" }
}
puts(1)
EOF

expect_build "Duplicate top-level function name/arity is rejected with clear diagnostic" "Duplicate top-level let definition: f" << 'EOF'
let f = fn(x: int) -> int { x + 1 }
let f = fn(x: int) -> int { x + 2 }
puts(f(1))
EOF

expect_runtime_output "Function-parameter shadowing via let compiles and runs" "3" << 'EOF'
let x = 1
let f = fn(x: int) -> int {
  let x = x + 1
  x + 1
}
puts(f(x))
EOF

expect_runtime_output "Closure capture before shadowing keeps outer binding" "11" << 'EOF'
let f = fn(x: int) -> int {
  let g = fn(y: int) -> int { x + y }
  let x = x + 1
  g(0) + x
}
puts(f(5))
EOF



# -------------------------------------------------------------------
# Imported From 06_codegen_edge_cases.sh
# -------------------------------------------------------------------

# ===========================================================================
# GROUP 1: Deeply nested if expressions
# ===========================================================================

expect_runtime_output "Deeply nested if (3 levels) in let binding produces correct value" "3" << 'EOF'
let a = true
let b = false
let c = true
let x = if (a) { if (b) { if (c) { 1 } else { 2 } } else { 3 } } else { 4 }
puts(x)
EOF

expect_runtime_output "Deeply nested if (3 levels) in tail position produces correct value" "3" << 'EOF'
let f = fn(a: bool, b: bool, c: bool) -> int {
  if (a) { if (b) { if (c) { 1 } else { 2 } } else { 3 } } else { 4 }
}
puts(f(true, false, true))
EOF

run_emit_go_not_contains_from_stdin "Deeply nested if (3 levels) in let binding avoids IIFE" "func\\(\\) int64" << 'EOF'
let a = true
let b = false
let c = true
let x = if (a) { if (b) { if (c) { 1 } else { 2 } } else { 3 } } else { 4 }
puts(x)
EOF

run_emit_go_not_contains_from_stdin "Deeply nested if (3 levels) in tail position avoids IIFE" "func\\(\\) int64" << 'EOF'
let f = fn(a: bool, b: bool, c: bool) -> int {
  if (a) { if (b) { if (c) { 1 } else { 2 } } else { 3 } } else { 4 }
}
puts(f(true, false, true))
EOF

# ===========================================================================
# GROUP 2: if-expression as function argument (IIFE still needed)
# ===========================================================================

expect_runtime_output "if-expression as function argument compiles and runs" "11" << 'EOF'
let add = fn(x: int, y: int) -> int { x + y }
let result = add(if (true) { 10 } else { 20 }, 1)
puts(result)
EOF

expect_runtime_output "Nested if as function argument compiles and runs" "2" << 'EOF'
let id = fn(x: int) -> int { x }
let result = id(if (true) { if (false) { 1 } else { 2 } } else { 3 })
puts(result)
EOF

# ===========================================================================
# GROUP 3: match in let binding correctness
# ===========================================================================

expect_runtime_output "Match on int in let binding selects correct arm" "42" << 'EOF'
let x = 2
let y = match x {
  1: 10
  2: 42
  _: 0
}
puts(y)
EOF

run_emit_go_not_contains_from_stdin "Match on int in let binding avoids IIFE" "func\\(\\) int64" << 'EOF'
let x = 2
let y = match x {
  1: 10
  2: 42
  _: 0
}
puts(y)
EOF

# ===========================================================================
# GROUP 4: match in tail position correctness
# ===========================================================================

expect_runtime_output "Match on int in tail position selects correct arm" "42" << 'EOF'
let choose = fn(x: int) -> int {
  match x {
    1: 10
    2: 42
    3: 99
    _: 0
  }
}
puts(choose(2))
EOF

run_emit_go_not_contains_from_stdin "Match on int in tail position avoids IIFE" "func\\(\\) int64" << 'EOF'
let choose = fn(x: int) -> int {
  match x {
    1: 10
    2: 42
    _: 0
  }
}
puts(choose(2))
EOF

# ===========================================================================
# GROUP 5: match with many arms (5+)
# ===========================================================================

expect_runtime_output "Match with 6 literal arms selects correct one" "60" << 'EOF'
let describe = fn(n: int) -> int {
  match n {
    1: 10
    2: 20
    3: 30
    4: 40
    5: 50
    _: 60
  }
}
puts(describe(99))
EOF

expect_runtime_output "Match with 6 arms variable capture in wildcard" "0" << 'EOF'
let score = fn(n: int) -> int {
  match n {
    1: 100
    2: 200
    3: 300
    4: 400
    5: 500
    _: 0
  }
}
puts(score(7))
EOF

# ===========================================================================
# GROUP 6: Nested match inside if
# ===========================================================================

expect_runtime_output "Match nested inside if-then branch produces correct value" "20" << 'EOF'
let f = fn(flag: bool, x: int) -> int {
  if (flag) {
    match x {
      1: 10
      2: 20
      _: 30
    }
  } else {
    0
  }
}
puts(f(true, 2))
EOF

run_emit_go_not_contains_from_stdin "Match nested inside if-then branch avoids IIFE" "func\\(\\) int64" << 'EOF'
let f = fn(flag: bool, x: int) -> int {
  if (flag) {
    match x {
      1: 10
      2: 20
      _: 30
    }
  } else {
    0
  }
}
puts(f(true, 2))
EOF

# ===========================================================================
# GROUP 7: if inside match arm
# ===========================================================================

expect_runtime_output "if-expression inside match arm body produces correct value" "99" << 'EOF'
let f = fn(x: int, flag: bool) -> int {
  match x {
    1: if (flag) { 99 } else { 0 }
    _: 50
  }
}
puts(f(1, true))
EOF

run_emit_go_not_contains_from_stdin "if inside match arm body avoids IIFE" "func\\(\\) int64" << 'EOF'
let f = fn(x: int, flag: bool) -> int {
  match x {
    1: if (flag) { 99 } else { 0 }
    _: 50
  }
}
puts(f(1, true))
EOF

# ===========================================================================
# GROUP 8: Chained let bindings each with if-expression
# ===========================================================================

expect_runtime_output "Three chained let bindings each using if produce correct final value" "6" << 'EOF'
let a = if (true) { 1 } else { 0 }
let b = if (true) { a + 2 } else { 0 }
let c = if (true) { b + 3 } else { 0 }
puts(c)
EOF

run_emit_go_not_contains_from_stdin "Three chained let bindings each using if avoid IIFE" "func\\(\\) int64" << 'EOF'
let a = if (true) { 1 } else { 0 }
let b = if (true) { a + 2 } else { 0 }
let c = if (true) { b + 3 } else { 0 }
puts(c)
EOF

# ===========================================================================
# GROUP 9: if/match with record construction in branches
# ===========================================================================

expect_runtime_output "if-expression returning records in branches" "10" << 'EOF'
let flag = true
let p = if (flag) { { x: 10, y: 20 } } else { { x: 0, y: 0 } }
puts(p.x)
EOF

expect_runtime_output "Match returning records in arms" "99" << 'EOF'
let n = 2
let p = match n {
  1: { x: 10, y: 20 }
  2: { x: 99, y: 88 }
  _: { x: 0, y: 0 }
}
puts(p.x)
EOF

# ===========================================================================
# GROUP 10: if/match with function calls in branches
# ===========================================================================

expect_runtime_output "if-expression with function calls in branches" "11" << 'EOF'
let inc = fn(x: int) -> int { x + 1 }
let dec = fn(x: int) -> int { x - 1 }
let result = if (true) { inc(10) } else { dec(10) }
puts(result)
EOF

expect_runtime_output "Match with function calls in arms" "25" << 'EOF'
let double = fn(x: int) -> int { x * 2 }
let triple = fn(x: int) -> int { x * 3 }
let n = 2
let result = match n {
  1: double(10)
  2: double(10) + 5
  _: triple(10)
}
puts(result)
EOF

# ===========================================================================
# GROUP 11: match on enum with nested if in arm
# ===========================================================================

expect_runtime_output "Enum match with nested if in constructor arm" "100" << 'EOF'
enum option[a] {
  some(a)
  none
}
let val = option.some(42)
let result = match val {
  option.some(v): if (v > 10) { 100 } else { 0 }
  option.none: 0
}
puts(result)
EOF

# ===========================================================================
# GROUP 12: Nested if - inner tail but outer not
# ===========================================================================

expect_runtime_output "Inner if in tail position, outer if in let binding" "5" << 'EOF'
let f = fn(a: bool, b: bool) -> int {
  if (a) { if (b) { 5 } else { 10 } } else { 0 }
}
let x = f(true, true)
puts(x)
EOF

expect_runtime_output "Outer if in let binding with inner match in tail" "30" << 'EOF'
let flag = true
let n = 3
let x = if (flag) {
  match n {
    1: 10
    2: 20
    3: 30
    _: 0
  }
} else {
  0
}
puts(x)
EOF

# ===========================================================================
# GROUP 13: if in recursive function body
# ===========================================================================

expect_runtime_output "Recursive function with if in tail position" "120" << 'EOF'
let factorial = fn(n: int, acc: int) -> int {
  if (n == 0) { acc } else { factorial(n - 1, acc * n) }
}
puts(factorial(5, 1))
EOF

expect_runtime_output "Recursive function with match in tail position" "55" << 'EOF'
let fib_helper = fn(n: int, a: int, b: int) -> int {
  match n {
    0: a
    _: fib_helper(n - 1, b, a + b)
  }
}
puts(fib_helper(10, 0, 1))
EOF

# ===========================================================================
# GROUP 14: Multiple if-expressions in sequence in function body
# ===========================================================================

expect_runtime_output "Multiple if-expressions as sequential statements in function body" "3" << 'EOF'
let f = fn(a: bool, b: bool) => int {
  let x = if (a) { 1 } else { 0 }
  let y = if (b) { 2 } else { 0 }
  x + y
}
puts(f(true, true))
EOF

# ===========================================================================
# GROUP 15: type-check if (x is T) - IIFE still expected for this case
# ===========================================================================

expect_runtime_output "Type-check if in let binding produces correct narrowed value" "2" << 'EOF'
let x: int | string = 1
let y = if (x is int) { x + 1 } else { 0 }
puts(y)
EOF

expect_runtime_output "Type-check if in tail position produces correct narrowed value" "3" << 'EOF'
let f = fn(x: int | string) -> int {
  if (x is int) { x + 2 } else { 0 }
}
puts(f(1))
EOF

expect_runtime_output "Nested type-check if inside regular if produces correct value" "10" << 'EOF'
let f = fn(flag: bool, x: int | string) -> int {
  if (flag) {
    if (x is int) { x + 9 } else { 0 }
  } else {
    0
  }
}
puts(f(true, 1))
EOF

# ===========================================================================
# GROUP 16: match where scrutinee is a complex expression
# ===========================================================================

expect_runtime_output "Match on arithmetic expression as scrutinee" "30" << 'EOF'
let a = 1
let b = 2
let x = match a + b {
  1: 10
  2: 20
  3: 30
  _: 0
}
puts(x)
EOF

expect_runtime_output "Match on function call as scrutinee" "20" << 'EOF'
let double = fn(x: int) -> int { x * 2 }
let x = match double(3) {
  4: 10
  6: 20
  _: 0
}
puts(x)
EOF

# ===========================================================================
# GROUP 17: if/match with string values in branches
# ===========================================================================

expect_runtime_output "if returning strings in branches" "yes" << 'EOF'
let flag = true
let msg = if (flag) { "yes" } else { "no" }
puts(msg)
EOF

expect_runtime_output "Match returning strings in arms" "two" << 'EOF'
let n = 2
let s = match n {
  1: "one"
  2: "two"
  3: "three"
  _: "other"
}
puts(s)
EOF

# ===========================================================================
# GROUP 18: if-expression with boolean operations in condition
# ===========================================================================

expect_runtime_output "if with compound boolean condition (and)" "1" << 'EOF'
let a = true
let b = true
let x = if (a == true) { if (b == true) { 1 } else { 2 } } else { 3 }
puts(x)
EOF

expect_runtime_output "if with negated condition" "20" << 'EOF'
let flag = false
let x = if (!flag) { 20 } else { 10 }
puts(x)
EOF

# ===========================================================================
# GROUP 19: Enum match in let binding (non-tail)
# ===========================================================================

expect_runtime_output "Enum match in let binding extracts correct variant data" "42" << 'EOF'
enum option[a] {
  some(a)
  none
}
let val = option.some(42)
let result = match val {
  option.some(v): v
  option.none: 0
}
puts(result)
EOF

run_emit_go_not_contains_from_stdin "Enum match in let binding avoids IIFE" "func\\(\\)" << 'EOF'
enum option[a] {
  some(a)
  none
}
let val = option.some(42)
let result = match val {
  option.some(v): v
  option.none: 0
}
puts(result)
EOF

# ===========================================================================
# GROUP 20: Statement-position if (discard target) with side effects
# ===========================================================================

expect_runtime_output "Statement-position if with effectful then-branch" "hello" << 'EOF'
let flag = true
if (flag) { puts("hello") }
EOF

expect_runtime_output "Statement-position if with effectful both branches" "world" << 'EOF'
let flag = false
if (flag) { puts("hello") } else { puts("world") }
EOF

# ===========================================================================
# GROUP 21: Deeply nested match (match inside match arm)
# ===========================================================================

expect_runtime_output "Match nested inside match arm produces correct value" "99" << 'EOF'
let f = fn(x: int, y: int) -> int {
  match x {
    1: match y {
      1: 11
      2: 12
      _: 19
    }
    2: match y {
      1: 21
      _: 99
    }
    _: 0
  }
}
puts(f(2, 3))
EOF

run_emit_go_not_contains_from_stdin "Match nested inside match arm avoids IIFE" "func\\(\\) int64" << 'EOF'
let f = fn(x: int, y: int) -> int {
  match x {
    1: match y {
      1: 11
      2: 12
      _: 19
    }
    2: match y {
      1: 21
      _: 99
    }
    _: 0
  }
}
puts(f(2, 3))
EOF

# ===========================================================================
# GROUP 22: if-expression where only one branch has a function call
# ===========================================================================

expect_runtime_output "if with function call only in then branch" "11" << 'EOF'
let inc = fn(x: int) -> int { x + 1 }
let x = if (true) { inc(10) } else { 0 }
puts(x)
EOF

expect_runtime_output "if with function call only in else branch" "9" << 'EOF'
let dec = fn(x: int) -> int { x - 1 }
let x = if (false) { 0 } else { dec(10) }
puts(x)
EOF

# ===========================================================================
# GROUP 23: if/match used to compute a value then used in arithmetic
# ===========================================================================

expect_runtime_output "Value from if-expression used in subsequent arithmetic" "15" << 'EOF'
let base = if (true) { 10 } else { 0 }
let result = base + 5
puts(result)
EOF

expect_runtime_output "Value from match used in subsequent arithmetic" "25" << 'EOF'
let n = 2
let base = match n {
  1: 10
  2: 20
  _: 0
}
let result = base + 5
puts(result)
EOF

# ===========================================================================
# GROUP 24: Match on boolean scrutinee
# ===========================================================================

expect_runtime_output "Match on boolean true selects correct arm" "1" << 'EOF'
let flag = true
let x = match flag {
  true: 1
  false: 0
}
puts(x)
EOF

expect_runtime_output "Match on boolean false selects correct arm" "0" << 'EOF'
let flag = false
let x = match flag {
  true: 1
  false: 0
}
puts(x)
EOF

# ===========================================================================
# GROUP 25: Record construction inside enum match arm
# ===========================================================================

expect_runtime_output "Record construction in enum match arm" "42" << 'EOF'
enum option[a] {
  some(a)
  none
}
let val = option.some(42)
let p = match val {
  option.some(v): { x: v, y: 0 }
  option.none: { x: 0, y: 0 }
}
puts(p.x)
EOF

# ===========================================================================
# GROUP 26: match as function argument (IIFE still needed)
# ===========================================================================

expect_runtime_output "Match as function argument retains IIFE and runs correctly" "20" << 'EOF'
let id = fn(x: int) -> int { x }
let result = id(match 2 { 1: 10 2: 20 _: 0 })
puts(result)
EOF

# ===========================================================================
# GROUP 27: if with inner let bindings inside branches (block handling)
# ===========================================================================

expect_runtime_output "if in tail position with let bindings inside branches" "30" << 'EOF'
let f = fn(flag: bool) -> int {
  if (flag) {
    let a = 10
    let b = 20
    a + b
  } else {
    let c = 5
    c
  }
}
puts(f(true))
EOF

expect_runtime_output "if in tail position with let bindings takes else branch" "5" << 'EOF'
let f = fn(flag: bool) -> int {
  if (flag) {
    let a = 10
    let b = 20
    a + b
  } else {
    let c = 5
    c
  }
}
puts(f(false))
EOF

expect_runtime_output "if in let binding with let bindings inside branches" "30" << 'EOF'
let flag = true
let x = if (flag) {
  let a = 10
  let b = 20
  a + b
} else {
  let c = 5
  c
}
puts(x)
EOF

# ===========================================================================
# GROUP 28: Variable binding in match pattern in let binding context
# ===========================================================================

expect_runtime_output "Match variable binding in let binding" "142" << 'EOF'
let x = 42
let result = match x {
  n: n + 100
}
puts(result)
EOF

expect_runtime_output "Match variable binding in tail position" "142" << 'EOF'
let f = fn(x: int) -> int {
  match x {
    n: n + 100
  }
}
puts(f(42))
EOF

# ===========================================================================
# GROUP 29: Nested match in let binding (__scrutinee scoping)
# ===========================================================================

expect_runtime_output "Nested match in let binding has correct __scrutinee scoping" "230" << 'EOF'
let x = 2
let y = 3
let result = match x {
  1: match y {
    10: 110
    _: 100
  }
  2: match y {
    3: 230
    _: 200
  }
  _: 0
}
puts(result)
EOF

# ===========================================================================
# GROUP 30: if-expression in discard/statement position with side effects
# ===========================================================================

expect_runtime_output "if in discard position executes side effects correctly" "branch_a" << 'EOF'
let x = 1
let _ = if (x == 1) { puts("branch_a") } else { puts("branch_b") }
EOF

# ===========================================================================
# GROUP 31: Deeply nested if (4 levels) in let binding
# ===========================================================================

expect_runtime_output "Deeply nested if (4 levels) in let binding" "4" << 'EOF'
let a = true
let b = true
let c = false
let d = true
let x = if (a) { if (b) { if (c) { if (d) { 1 } else { 2 } } else { 4 } } else { 5 } } else { 6 }
puts(x)
EOF

run_emit_go_not_contains_from_stdin "Deeply nested if (4 levels) in let binding avoids IIFE" "func\\(\\) int64" << 'EOF'
let a = true
let b = true
let c = false
let d = true
let x = if (a) { if (b) { if (c) { if (d) { 1 } else { 2 } } else { 4 } } else { 5 } } else { 6 }
puts(x)
EOF

# ===========================================================================
# GROUP 32: Nested match inside match inside match (3 levels deep)
# ===========================================================================

expect_runtime_output "Triple-nested match in tail position" "321" << 'EOF'
let f = fn(x: int, y: int, z: int) -> int {
  match x {
    1: match y {
      1: match z {
        1: 111
        _: 110
      }
      _: 100
    }
    3: match y {
      2: match z {
        1: 321
        _: 320
      }
      _: 300
    }
    _: 0
  }
}
puts(f(3, 2, 1))
EOF

# ===========================================================================
# GROUP 33: Enum match in tail position with if in arm
# ===========================================================================

expect_runtime_output "Enum match in tail position with if in arm" "100" << 'EOF'
enum option[a] {
  some(a)
  none
}
let check = fn(o: option[int]) -> int {
  match o {
    option.some(v): if (v > 10) { 100 } else { v }
    option.none: 0
  }
}
puts(check(option.some(42)))
EOF

expect_runtime_output "Enum match in tail position if takes else branch" "7" << 'EOF'
enum option[a] {
  some(a)
  none
}
let check = fn(o: option[int]) -> int {
  match o {
    option.some(v): if (v > 10) { 100 } else { v }
    option.none: 0
  }
}
puts(check(option.some(7)))
EOF

# ===========================================================================
# GROUP 34: if-expression with string concatenation in branches
# ===========================================================================

expect_runtime_output "if returning string concatenation in branches" "hello world" << 'EOF'
let flag = true
let msg = if (flag) { "hello" + " " + "world" } else { "goodbye" }
puts(msg)
EOF

# ===========================================================================
# GROUP 35: match on string scrutinee
# ===========================================================================

expect_runtime_output "Match on string scrutinee selects correct arm" "found_hello" << 'EOF'
let s = "hello"
let result = match s {
  "hello": "found_hello"
  "world": "found_world"
  _: "unknown"
}
puts(result)
EOF

expect_runtime_output "Top-level forward ref works with earlier non-capturing value binding" "7" << 'EOF'
let seed = 1
let main_val = add_three(3)
let add_three = fn(x: int) -> int { x + 3 }
puts(main_val + seed)
EOF

suite_end
