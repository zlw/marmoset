#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

suite_begin "EDGE CASE TESTS - RUNTIME BEHAVIOR & OPERATOR STRESS"

# ============================================================
# SECTION A: OPERATOR PRECEDENCE & CHAINING
# ============================================================
echo "-- SECTION A: OPERATOR PRECEDENCE & CHAINING --"

expect_runtime_output "A1: mixed arithmetic 1 + 2 * 3 = 7" "7" << 'EOF'
puts(1 + 2 * 3)
EOF

expect_runtime_output "A2: comparison result stored then compared with ==" "true" << 'EOF'
let a = 1 < 2
puts(a == true)
EOF

expect_runtime_output "A3: nested arithmetic (1 + 2) * (3 + 4) = 21" "21" << 'EOF'
puts((1 + 2) * (3 + 4))
EOF

expect_runtime_output "A4a: unary minus precedence -1 + 2 = 1" "1" << 'EOF'
puts(-1 + 2)
EOF

expect_runtime_output "A4b: unary minus precedence -1 * 2 = -2" "-2" << 'EOF'
puts(-1 * 2)
EOF

expect_runtime_output "A5: unary minus on parenthesized expression -(1 + 2) = -3" "-3" << 'EOF'
puts(-(1 + 2))
EOF

expect_runtime_output "A6: double negation -(-1) = 1" "1" << 'EOF'
puts(-(-1))
EOF

expect_runtime_output "A7: complex precedence 1 + 2 * 3 - 4 / 2 = 5" "5" << 'EOF'
puts(1 + 2 * 3 - 4 / 2)
EOF

expect_runtime_output "A8: comparison in if condition" "positive" << 'EOF'
let x = 5
let r = if (x > 0) { "positive" } else { "non-positive" }
puts(r)
EOF

expect_runtime_output "A9: equality in let binding" "true" << 'EOF'
let b = 1 == 1
puts(b)
EOF

expect_runtime_output "A10: not-equal in if condition" "nonzero" << 'EOF'
let x = 7
let r = if (x != 0) { "nonzero" } else { "zero" }
puts(r)
EOF

# ============================================================
# SECTION B: STRING OPERATIONS
# ============================================================
echo "-- SECTION B: STRING OPERATIONS --"

expect_runtime_output "B11: string triple concat" "hello world" << 'EOF'
puts("hello" + " " + "world")
EOF

expect_runtime_output "B12a: string equality true" "true" << 'EOF'
puts("abc" == "abc")
EOF

expect_runtime_output "B12b: string equality false" "false" << 'EOF'
puts("abc" == "def")
EOF

expect_runtime_output "B13a: string less-than lexicographic" "true" << 'EOF'
puts("a" < "b")
EOF

expect_runtime_output "B13b: string greater-than lexicographic" "true" << 'EOF'
puts("b" > "a")
EOF

expect_runtime_output "B14: string not-equal" "true" << 'EOF'
puts("x" != "y")
EOF

expect_runtime_output "B15a: empty string concat" "x" << 'EOF'
puts("" + "x")
EOF

expect_runtime_output "B15b: empty string equality" "true" << 'EOF'
puts("" == "")
EOF

# NOTE: Marmoset's lexer does NOT process escape sequences (\t, \n, etc.)
# in string literals. A `\t` in source becomes literal backslash + t.
# OCaml's %S formatter then double-escapes it in the Go output.
# So "hello\tworld" in Marmoset becomes Go "hello\\tworld", printing "hello\tworld" literally.
expect_runtime_output "B16: string with backslash literal" 'hello\tworld' << 'EOF'
puts("hello\tworld")
EOF

# ============================================================
# SECTION C: BOOLEAN OPERATIONS
# ============================================================
echo "-- SECTION C: BOOLEAN OPERATIONS --"

expect_runtime_output "C17a: bool eq true==true" "true" << 'EOF'
puts(true == true)
EOF

expect_runtime_output "C17b: bool eq false==false" "true" << 'EOF'
puts(false == false)
EOF

expect_runtime_output "C17c: bool eq true==false" "false" << 'EOF'
puts(true == false)
EOF

expect_runtime_output "C18a: bool negation !true" "false" << 'EOF'
puts(!true)
EOF

expect_runtime_output "C18b: bool negation !false" "true" << 'EOF'
puts(!false)
EOF

expect_runtime_output "C18c: double negation !!true via parens" "true" << 'EOF'
puts(!(!true))
EOF

expect_runtime_output "C19: negation in if condition" "was false" << 'EOF'
let flag = false
let r = if (!flag) { "was false" } else { "was true" }
puts(r)
EOF

# Bool should NOT have num trait, so arithmetic on bool should fail typechecking.
expect_build "C20: bool in arithmetic context should fail" "missing-impl" << 'EOF'
puts(true + false)
EOF

# ============================================================
# SECTION D: FLOAT OPERATIONS
# ============================================================
echo "-- SECTION D: FLOAT OPERATIONS --"

expect_runtime_output "D21a: float addition 1.0 + 2.5 = 3.5" "3.5" << 'EOF'
puts(1.0 + 2.5)
EOF

expect_runtime_output "D21b: float multiplication 3.14 * 2.0 = 6.28" "6.28" << 'EOF'
puts(3.14 * 2.0)
EOF

expect_runtime_output "D22a: float comparison 1.0 < 2.0" "true" << 'EOF'
puts(1.0 < 2.0)
EOF

expect_runtime_output "D22b: float greater-equal 3.14 >= 3.14" "true" << 'EOF'
puts(3.14 >= 3.14)
EOF

# Floating-point precision: 0.1 + 0.2 is NOT exactly 0.3 in IEEE 754.
# This should be false because Go float64 has the same FP representation issue.
expect_runtime_output "D23: float precision 0.1 + 0.2 != 0.3 (FP edge)" "false" << 'EOF'
puts(0.1 + 0.2 == 0.3)
EOF

expect_runtime_output "D24: unary minus on float -3.14" "-3.14" << 'EOF'
puts(-3.14)
EOF

# Float division -- Go float64 division
expect_runtime_output "D25: float division 10.0 / 4.0 = 2.5" "2.5" << 'EOF'
puts(10.0 / 4.0)
EOF

expect_runtime_output "D25b: float subtraction 5.5 - 2.5 = 3" "3" << 'EOF'
puts(5.5 - 2.5)
EOF

# ============================================================
# SECTION E: OPERATORS ON CUSTOM TYPES (via Trait Impls)
# ============================================================
echo "-- SECTION E: OPERATORS ON CUSTOM TYPES --"

# E26: Custom eq impl on enum type, then use ==
expect_runtime_output "E26: custom eq on enum always-true drives ==" "true" << 'EOF'
enum color { red green blue }
impl eq for color {
  fn eq(x: color, y: color) -> bool { true }
}
puts(color.red == color.blue)
EOF

# E27: Custom ord on enum drives < and >
# KNOWN BUG (P1-6): The emitter compares ord_compare_* result against int64(0/2),
# but user-defined ord impls return the `ordering` enum, not int64.
# Builtin primitive ord_compare_* functions return int64, but custom ones
# return the ordering enum struct. This causes a Go type mismatch at compile:
#   invalid operation: ord_compare_rank(...) == int64(0) (mismatched types ordering and int64)
# The existing 04_traits_operator_edge_cases.sh already documents this bug.
# These tests WILL FAIL until the bug is fixed.
expect_runtime_output "E27a: custom ord on enum drives < (KNOWN BUG: ordering vs int64)" "true" << 'EOF'
enum rank { low mid high }
impl eq for rank {
  fn eq(x: rank, y: rank) -> bool { false }
}
impl ord for rank {
  fn compare(x: rank, y: rank) -> ordering {
    ordering.less
  }
}
puts(rank.high < rank.low)
EOF

expect_runtime_output "E27b: custom ord on enum drives > (KNOWN BUG: ordering vs int64)" "false" << 'EOF'
enum rank { low mid high }
impl eq for rank {
  fn eq(x: rank, y: rank) -> bool { false }
}
impl ord for rank {
  fn compare(x: rank, y: rank) -> ordering {
    ordering.less
  }
}
puts(rank.high > rank.low)
EOF

# E28: Custom num impl on enum, use + and -
expect_runtime_output "E28a: custom num + on enum" "42" << 'EOF'
enum boxed { val(int) }
impl num for boxed {
  fn add(x: boxed, y: boxed) -> boxed { x }
  fn sub(x: boxed, y: boxed) -> boxed { x }
  fn mul(x: boxed, y: boxed) -> boxed { x }
  fn div(x: boxed, y: boxed) -> boxed { x }
}
let a = boxed.val(42)
let b = boxed.val(99)
match (a + b) {
  boxed.val(n): puts(n)
}
EOF

expect_runtime_output "E28b: custom num - on enum" "10" << 'EOF'
enum boxed { val(int) }
impl num for boxed {
  fn add(x: boxed, y: boxed) -> boxed { x }
  fn sub(x: boxed, y: boxed) -> boxed { x }
  fn mul(x: boxed, y: boxed) -> boxed { x }
  fn div(x: boxed, y: boxed) -> boxed { x }
}
let a = boxed.val(10)
let b = boxed.val(3)
match (a - b) {
  boxed.val(n): puts(n)
}
EOF

# E29: Custom neg impl on enum, use unary -
expect_runtime_output "E29: custom neg on enum unary minus" "77" << 'EOF'
enum wrapper { val(int) }
impl neg for wrapper {
  fn neg(x: wrapper) -> wrapper { x }
}
let a = wrapper.val(77)
match (-a) {
  wrapper.val(n): puts(n)
}
EOF

# E30: Operator on custom type, result used in another operation
expect_runtime_output "E30: custom + result used in match" "first" << 'EOF'
enum boxed { val(int) }
impl num for boxed {
  fn add(x: boxed, y: boxed) -> boxed { x }
  fn sub(x: boxed, y: boxed) -> boxed { x }
  fn mul(x: boxed, y: boxed) -> boxed { x }
  fn div(x: boxed, y: boxed) -> boxed { x }
}
impl eq for boxed {
  fn eq(x: boxed, y: boxed) -> bool { true }
}
let a = boxed.val(1)
let b = boxed.val(2)
let c = a + b
let d = boxed.val(1)
let result = if (c == d) { "first" } else { "second" }
puts(result)
EOF

# E31: Chained custom operations: a + b + c where + is custom
expect_runtime_output "E31: chained custom + (left-associative)" "10" << 'EOF'
enum boxed { val(int) }
impl num for boxed {
  fn add(x: boxed, y: boxed) -> boxed { x }
  fn sub(x: boxed, y: boxed) -> boxed { x }
  fn mul(x: boxed, y: boxed) -> boxed { x }
  fn div(x: boxed, y: boxed) -> boxed { x }
}
let a = boxed.val(10)
let b = boxed.val(20)
let c = boxed.val(30)
match (a + b + c) {
  boxed.val(n): puts(n)
}
EOF

# E32: Custom == returning false when Go == would return true
# (same enum variant, same data, but custom eq says false)
expect_runtime_output "E32: custom eq returns false despite identical values" "false" << 'EOF'
enum color { red green blue }
impl eq for color {
  fn eq(x: color, y: color) -> bool { false }
}
puts(color.red == color.red)
EOF

# E33: Custom < that gives reverse ordering
# ordering.greater means x > y, so < returns false always
# KNOWN BUG (P1-6): Same ordering enum vs int64 mismatch as E27.
expect_runtime_output "E33: custom ord reverse (KNOWN BUG: ordering vs int64)" "false" << 'EOF'
enum priority { low high }
impl eq for priority {
  fn eq(x: priority, y: priority) -> bool { false }
}
impl ord for priority {
  fn compare(x: priority, y: priority) -> ordering {
    ordering.greater
  }
}
puts(priority.low < priority.high)
EOF

# E34: Operator on custom type inside generic function with trait constraint
expect_runtime_output "E34: generic function with eq constraint uses ==" "true" << 'EOF'
enum color { red green blue }
impl eq for color {
  fn eq(x: color, y: color) -> bool { true }
}
let are_equal = fn[t: eq](a: t, b: t) -> bool {
  a == b
}
puts(are_equal(color.red, color.blue))
EOF

# ============================================================
# SECTION F: INTEGER EDGE CASES
# ============================================================
echo "-- SECTION F: INTEGER EDGE CASES --"

# F35: Large integers (may overflow int64 on some platforms)
# Go int64 max is 9223372036854775807, so 1e9 * 1e9 = 1e18 fits.
expect_runtime_output "F35: large integer multiplication 1e9 * 1e9" "1000000000000000000" << 'EOF'
puts(1000000000 * 1000000000)
EOF

# F36: Integer division truncation toward zero
expect_runtime_output "F36a: int division truncation 7/2 = 3" "3" << 'EOF'
puts(7 / 2)
EOF

expect_runtime_output "F36b: int division 1/3 = 0" "0" << 'EOF'
puts(1 / 3)
EOF

# F37: Negative integer operations
expect_runtime_output "F37a: -5 + 3 = -2" "-2" << 'EOF'
puts(-5 + 3)
EOF

expect_runtime_output "F37b: -5 * -3 = 15" "15" << 'EOF'
puts(-5 * -3)
EOF

expect_runtime_output "F37c: negative minus negative -5 - -3 = -2" "-2" << 'EOF'
puts(-5 - -3)
EOF

# F38: Zero division -- Go panics at runtime on integer zero division.
# This tests that the runtime crashes rather than silently producing wrong output.
# We expect the build to succeed but execution to fail (panic).
# NOTE: The test harness treats execution failure as "build or execution failed",
# so we cannot distinguish build failure from runtime panic cleanly.
# We simply document that this is expected runtime panic behavior.

# F39: Integer comparison chains with equality
expect_runtime_output "F39a: 1 <= 1" "true" << 'EOF'
puts(1 <= 1)
EOF

expect_runtime_output "F39b: 1 >= 1" "true" << 'EOF'
puts(1 >= 1)
EOF

expect_runtime_output "F39c: 0 <= 1" "true" << 'EOF'
puts(0 <= 1)
EOF

expect_runtime_output "F39d: 1 >= 0" "true" << 'EOF'
puts(1 >= 0)
EOF

# F40: Min/max patterns using if-expression
expect_runtime_output "F40a: min of two ints" "3" << 'EOF'
let a = 7
let b = 3
let m = if (a < b) { a } else { b }
puts(m)
EOF

expect_runtime_output "F40b: max of two ints" "7" << 'EOF'
let a = 7
let b = 3
let m = if (a > b) { a } else { b }
puts(m)
EOF

# ============================================================
# SECTION G: puts AND SHOW BEHAVIOR
# ============================================================
echo "-- SECTION G: puts AND SHOW BEHAVIOR --"

# G41: puts on each primitive type
expect_runtime_output "G41a: puts int" "42" << 'EOF'
puts(42)
EOF

expect_runtime_output "G41b: puts string" "hello" << 'EOF'
puts("hello")
EOF

expect_runtime_output "G41c: puts bool" "true" << 'EOF'
puts(true)
EOF

expect_runtime_output "G41d: puts float" "3.14" << 'EOF'
puts(3.14)
EOF

# G42: puts on array literal
expect_runtime_output "G42: puts on array" "[1 2 3]" << 'EOF'
puts([1, 2, 3])
EOF

# G43: show trait on custom enum type, then puts
# puts uses fmt.Println which calls String() method on enums.
# Enum String() methods are auto-generated.
expect_runtime_output "G43a: puts on enum variant no data" "red" << 'EOF'
enum color { red green blue }
puts(color.red)
EOF

expect_runtime_output "G43b: puts on enum variant with data" "some(42)" << 'EOF'
enum option[a] {
  some(a)
  none
}
let x = option.some(42)
puts(x)
EOF

# G44: Multiple puts in sequence
expect_runtime_output "G44: multiple puts in sequence" "1
2
3" << 'EOF'
puts(1)
puts(2)
puts(3)
EOF

# G45: puts inside if-expression branches
expect_runtime_output "G45: puts in if-then branch" "hello" << 'EOF'
if (true) { puts("hello") } else { puts("world") }
EOF

# G46: puts inside match arms
expect_runtime_output "G46: puts inside match arms" "got one" << 'EOF'
enum option[a] {
  some(a)
  none
}
let x = option.some(1)
match (x) {
  option.some(n): puts("got one")
  option.none(): puts("got none")
}
EOF

# G47: Nested puts: puts(puts(42))
# puts(42) prints "42" and returns struct{} (null).
# puts(struct{}{}) then prints the Go representation of struct{}.
# fmt.Println on struct{}{} prints "{}".
expect_runtime_output "G47: nested puts(puts(42))" "42
{}" << 'EOF'
puts(puts(42))
EOF

# ============================================================
# SECTION H: COMPLEX MIXED SCENARIOS
# ============================================================
echo "-- SECTION H: COMPLEX MIXED SCENARIOS --"

# H48: Arithmetic result fed into comparison in same expression
expect_runtime_output "H48: arithmetic result in comparison" "true" << 'EOF'
puts((2 + 3) > 4)
EOF

# H49: Multiple operator types in let-binding chain
expect_runtime_output "H49: let-binding chain with mixed ops" "true" << 'EOF'
let sum = 3 + 4
let product = 2 * 5
let is_greater = sum > product
puts(!is_greater)
EOF

# H50: Function returning comparison result used in if
expect_runtime_output "H50: function returning comparison" "big" << 'EOF'
let is_big = fn(x: int) -> bool { x > 100 }
let r = if (is_big(200)) { "big" } else { "small" }
puts(r)
EOF

# H51: Deeply nested arithmetic
# (10 + 2) * (3 + 5) + 4 = 12 * 8 + 4 = 96 + 4 = 100
expect_runtime_output "H51: deeply nested arithmetic" "100" << 'EOF'
puts((10 + 2) * (3 + 5) + 4)
EOF

# H52: Operator on function call results
expect_runtime_output "H52: operators on function results" "21" << 'EOF'
let double = fn(x: int) -> int { x * 2 }
let triple = fn(x: int) -> int { x * 3 }
puts(double(3) + triple(5))
EOF

# H53: String comparison edge cases
expect_runtime_output "H53a: empty string less than non-empty" "true" << 'EOF'
puts("" < "a")
EOF

expect_runtime_output "H53b: same prefix different length" "true" << 'EOF'
puts("abc" < "abcd")
EOF

# H54: Comparison result passed to function
expect_runtime_output "H54: comparison result as argument" "yes" << 'EOF'
let to_str = fn(b: bool) -> string {
  if (b) { "yes" } else { "no" }
}
puts(to_str(10 > 5))
EOF

# H55: Mix of primitive ops and variables
expect_runtime_output "H55: variable-heavy arithmetic" "55" << 'EOF'
let a = 10
let b = 20
let c = 25
puts(a + b + c)
EOF

# H56: Chained comparisons as separate let bindings
expect_runtime_output "H56: chained comparison bindings" "true" << 'EOF'
let x = 5
let a = x > 0
let b = x < 10
let c = a == b
puts(c)
EOF

# H57: Operator on record field access
expect_runtime_output "H57: arithmetic on record fields" "30" << 'EOF'
type point = { x: int, y: int }
let p: point = { x: 10, y: 20 }
puts(p.x + p.y)
EOF

# H58: Complex expression with multiple record field accesses
expect_runtime_output "H58: comparison on record fields" "true" << 'EOF'
type rect = { w: int, h: int }
let r: rect = { w: 10, h: 5 }
puts(r.w > r.h)
EOF

# H59: Negation on variable from record field
expect_runtime_output "H59: negate record field" "-10" << 'EOF'
type point = { x: int, y: int }
let p: point = { x: 10, y: 20 }
puts(-(p.x))
EOF

# H60: Float negation of variable
expect_runtime_output "H60: negate float variable" "-2.5" << 'EOF'
let f = 2.5
puts(-f)
EOF

# ============================================================
# SECTION I: TYPE REJECTION (operators that should fail)
# ============================================================
echo "-- SECTION I: TYPE REJECTION --"

# I61: String subtraction should fail (string has no num impl)
expect_build "I61: string subtraction fails" "missing-impl" << 'EOF'
puts("a" - "b")
EOF

# I62: String multiplication should fail
expect_build "I62: string multiplication fails" "missing-impl" << 'EOF'
puts("a" * "b")
EOF

# I63: String division should fail
expect_build "I63: string division fails" "missing-impl" << 'EOF'
puts("a" / "b")
EOF

# I64: Negation on string should fail (no neg impl)
expect_build "I64: string negation fails" "missing-impl" << 'EOF'
puts(-"hello")
EOF

# I65: Negation on bool should fail (no neg impl)
expect_build "I65: bool negation with unary minus fails" "missing-impl" << 'EOF'
let x = true
puts(-x)
EOF

# I66: Mixed types in arithmetic should fail
expect_build "I66: int + string type mismatch" "__ANY_ERROR__" << 'EOF'
puts(1 + "hello")
EOF

# I67: Ordering on function type should fail
expect_build "I67: function comparison fails" "missing-impl" << 'EOF'
let f = fn(x: int) -> int { x }
let g = fn(x: int) -> int { x }
puts(f < g)
EOF

# I68: Equality on function type should fail
expect_build "I68: function equality fails" "missing-impl" << 'EOF'
let f = fn(x: int) -> int { x }
let g = fn(x: int) -> int { x }
puts(f == g)
EOF

# ============================================================
# SECTION J: BOOLEAN NEGATION IN COMPLEX CONTEXTS
# ============================================================
echo "-- SECTION J: BOOLEAN NEGATION IN COMPLEX CONTEXTS --"

# J69: Negated comparison in if condition
expect_runtime_output "J69: !(1 < 2) in if" "no" << 'EOF'
let r = if (!(1 < 2)) { "yes" } else { "no" }
puts(r)
EOF

# J70: Negated equality in let binding
expect_runtime_output "J70: let b = !(1 == 2)" "true" << 'EOF'
let b = !(1 == 2)
puts(b)
EOF

# J71: Double negation of comparison
expect_runtime_output "J71: !(!(true)) == true" "true" << 'EOF'
puts(!(!true) == true)
EOF

# J72: Negation of != result
expect_runtime_output "J72: !(1 != 1) == true" "true" << 'EOF'
puts(!(1 != 1))
EOF

# ============================================================
# SECTION K: OPERATOR RESULT AS VARIOUS POSITIONS
# ============================================================
echo "-- SECTION K: OPERATOR RESULTS IN VARIOUS POSITIONS --"

# K73: Operator result as direct function argument
expect_runtime_output "K73: direct arithmetic as function arg" "10" << 'EOF'
let show_int = fn(x: int) -> int { x }
puts(show_int(3 + 7))
EOF

# K74: Operator result in array literal
expect_runtime_output "K74: arithmetic in array" "[3 7 10]" << 'EOF'
puts([1 + 2, 3 + 4, 5 + 5])
EOF

# K75: Comparison in if-else as expression
expect_runtime_output "K75: ternary-style comparison" "bigger" << 'EOF'
let x = 10
let label = if (x * 2 > 15) { "bigger" } else { "smaller" }
puts(label)
EOF

# K76: Multiple operators producing boolean in let-binding
expect_runtime_output "K76: complex bool expression" "false" << 'EOF'
let a = 5 > 3
let b = 2 > 4
puts(a == b)
EOF

# ============================================================
# SECTION L: CUSTOM TYPE + PRIMITIVE IN SAME PROGRAM
# ============================================================
echo "-- SECTION L: CUSTOM TYPE + PRIMITIVE OPS IN SAME PROGRAM --"

# L77: Primitive int + and custom enum + in same program
expect_runtime_output "L77: primitive and custom + coexist" "7
100" << 'EOF'
enum boxed { val(int) }
impl num for boxed {
  fn add(x: boxed, y: boxed) -> boxed { x }
  fn sub(x: boxed, y: boxed) -> boxed { x }
  fn mul(x: boxed, y: boxed) -> boxed { x }
  fn div(x: boxed, y: boxed) -> boxed { x }
}
puts(3 + 4)
let a = boxed.val(100)
let b = boxed.val(200)
match (a + b) {
  boxed.val(n): puts(n)
}
EOF

# L78: Primitive == and custom == coexist
expect_runtime_output "L78: primitive and custom == coexist" "true
false" << 'EOF'
enum color { red green blue }
impl eq for color {
  fn eq(x: color, y: color) -> bool { false }
}
puts(42 == 42)
puts(color.red == color.red)
EOF

# L79: Primitive comparison and custom comparison in same program
expect_runtime_output "L79: primitive < and custom eq in same program" "true
true" << 'EOF'
enum tag { a b }
impl eq for tag {
  fn eq(x: tag, y: tag) -> bool { true }
}
puts(1 < 2)
puts(tag.a == tag.b)
EOF

# ============================================================
# SECTION M: EDGE CASES IN puts OUTPUT FORMAT
# ============================================================
echo "-- SECTION M: puts OUTPUT FORMAT --"

# M80: puts on zero
expect_runtime_output "M80: puts(0)" "0" << 'EOF'
puts(0)
EOF

# M81: puts on negative int
expect_runtime_output "M81: puts(-1)" "-1" << 'EOF'
puts(-1)
EOF

# M82: puts on empty string
expect_runtime_output "M82: puts empty string" "" << 'EOF'
puts("")
EOF

# M83: puts on false
expect_runtime_output "M83: puts(false)" "false" << 'EOF'
puts(false)
EOF

# M84: puts on 0.0
expect_runtime_output "M84: puts(0.0)" "0" << 'EOF'
puts(0.0)
EOF

# M85: puts on negative float
expect_runtime_output "M85: puts(-1.5)" "-1.5" << 'EOF'
puts(-1.5)
EOF

# M86: puts on large int
expect_runtime_output "M86: puts large int" "999999999" << 'EOF'
puts(999999999)
EOF

# ============================================================
# SECTION N: MULTI-STEP COMPUTATION STRESS
# ============================================================
echo "-- SECTION N: MULTI-STEP COMPUTATION STRESS --"

# N87: Fibonacci-like computation with just let bindings
expect_runtime_output "N87: fibonacci via let bindings" "8" << 'EOF'
let a = 1
let b = 1
let c = a + b
let d = b + c
let e = c + d
let f = d + e
puts(f)
EOF

# N88: Accumulator pattern via chained let bindings (no rebinding)
# Marmoset rejects duplicate top-level `let` definitions.
expect_runtime_output "N88: accumulator-style computation" "15" << 'EOF'
let a = 0
let b = a + 1
let c = b + 2
let d = c + 3
let e = d + 4
let f = e + 5
puts(f)
EOF

# N89: Nested if-else with operators
# NOTE: Nested if-else assigned to a let binding may trigger codegen issues
# with undefined `offset` variables in the emitted Go. This tests that path.
expect_runtime_output "N89: nested if with operators" "medium" << 'EOF'
let x = 50
let small = x < 10
let big = x >= 100
let r = if (small) { "small" } else { if (big) { "large" } else { "medium" } }
puts(r)
EOF

# N90: Operator inside recursive-like pattern (using match)
expect_runtime_output "N90: operator inside match on enum" "11" << 'EOF'
enum option[a] {
  some(a)
  none
}
let x = option.some(10)
let r = match (x) {
  option.some(n): n + 1
  option.none(): 0
}
puts(r)
EOF

# ============================================================
# SECTION O: FLOAT PRECISION & FORMATTING
# ============================================================
echo "-- SECTION O: FLOAT PRECISION & FORMATTING --"

# O91: Float that rounds to integer in output
expect_runtime_output "O91: 1.0 + 1.0 = 2" "2" << 'EOF'
puts(1.0 + 1.0)
EOF

# O92: Very small float
expect_runtime_output "O92: small float 0.001" "0.001" << 'EOF'
puts(0.001)
EOF

# O93: Float negative zero behavior
# Go's fmt.Println prints -0.0 as "0" (not "-0"), because the %v formatter
# for float64 uses %g which normalizes negative zero.
expect_runtime_output "O93: negative zero float" "0" << 'EOF'
puts(-0.0)
EOF

# O94: Float comparison with negative
expect_runtime_output "O94: negative float less than positive" "true" << 'EOF'
puts(-1.5 < 1.5)
EOF

# O95: Float not-equal
expect_runtime_output "O95: float not-equal" "true" << 'EOF'
puts(1.0 != 2.0)
EOF

# ============================================================
# SECTION P: GENERIC FUNCTIONS WITH OPERATOR CONSTRAINTS
# ============================================================
echo "-- SECTION P: GENERIC FUNCTIONS WITH OPERATOR CONSTRAINTS --"

# P96: Generic equality function
expect_runtime_output "P96: generic eq function on ints" "true" << 'EOF'
let equals = fn[t: eq](a: t, b: t) -> bool { a == b }
puts(equals(42, 42))
EOF

# P97: Generic equality on strings
expect_runtime_output "P97: generic eq function on strings" "false" << 'EOF'
let equals = fn[t: eq](a: t, b: t) -> bool { a == b }
puts(equals("foo", "bar"))
EOF

# P98: Generic comparison function
expect_runtime_output "P98: generic ord function on ints" "true" << 'EOF'
let is_less = fn[t: ord](a: t, b: t) -> bool { a < b }
puts(is_less(1, 2))
EOF

# P99: Generic arithmetic function
# NOTE: Return type annotation `-> t` where t is a type parameter currently fails
# with "Unknown type constructor: t". So we omit the return annotation.
# BUG: If this test fails, it indicates the generic type parameter `t` cannot be
# used in return type annotations, which is a known limitation.
expect_runtime_output "P99: generic num function on ints" "7" << 'EOF'
let add = fn[t: num](a: t, b: t) { a + b }
puts(add(3, 4))
EOF

# P100: Generic function with multiple operators
expect_runtime_output "P100: generic function using eq and result" "same" << 'EOF'
let check = fn[t: eq](a: t, b: t) -> string {
  if (a == b) { "same" } else { "different" }
}
puts(check(5, 5))
EOF

suite_end
