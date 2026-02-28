#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

suite_begin "EDGE CASE TESTS - OPERATOR DESUGARING (P1-6)"

# ============================================================
# 1. INT COMPARISON OPERATORS (all six: <, >, <=, >=, ==, !=)
# ============================================================
echo "-- INT COMPARISON OPERATORS --"

expect_runtime_output "Int less-than true" "true" << 'EOF'
puts(1 < 2)
EOF

expect_runtime_output "Int less-than false" "false" << 'EOF'
puts(2 < 1)
EOF

expect_runtime_output "Int less-than equal values" "false" << 'EOF'
puts(5 < 5)
EOF

expect_runtime_output "Int greater-than true" "true" << 'EOF'
puts(3 > 2)
EOF

expect_runtime_output "Int greater-than false" "false" << 'EOF'
puts(1 > 2)
EOF

expect_runtime_output "Int less-equal true (strictly less)" "true" << 'EOF'
puts(1 <= 2)
EOF

expect_runtime_output "Int less-equal true (equal)" "true" << 'EOF'
puts(5 <= 5)
EOF

expect_runtime_output "Int less-equal false" "false" << 'EOF'
puts(6 <= 5)
EOF

expect_runtime_output "Int greater-equal true (strictly greater)" "true" << 'EOF'
puts(3 >= 2)
EOF

expect_runtime_output "Int greater-equal true (equal)" "true" << 'EOF'
puts(7 >= 7)
EOF

expect_runtime_output "Int greater-equal false" "false" << 'EOF'
puts(2 >= 3)
EOF

expect_runtime_output "Int equality true" "true" << 'EOF'
puts(42 == 42)
EOF

expect_runtime_output "Int equality false" "false" << 'EOF'
puts(42 == 99)
EOF

expect_runtime_output "Int not-equal true" "true" << 'EOF'
puts(1 != 2)
EOF

expect_runtime_output "Int not-equal false" "false" << 'EOF'
puts(5 != 5)
EOF

# ============================================================
# 2. FLOAT COMPARISON OPERATORS
# ============================================================
echo "-- FLOAT COMPARISON OPERATORS --"

expect_runtime_output "Float less-than true" "true" << 'EOF'
puts(1.5 < 2.5)
EOF

expect_runtime_output "Float less-than false" "false" << 'EOF'
puts(3.0 < 2.0)
EOF

expect_runtime_output "Float greater-than true" "true" << 'EOF'
puts(2.5 > 1.5)
EOF

expect_runtime_output "Float less-equal true (equal)" "true" << 'EOF'
puts(3.14 <= 3.14)
EOF

expect_runtime_output "Float greater-equal true (equal)" "true" << 'EOF'
puts(2.71 >= 2.71)
EOF

expect_runtime_output "Float equality true" "true" << 'EOF'
puts(1.0 == 1.0)
EOF

expect_runtime_output "Float equality false" "false" << 'EOF'
puts(1.0 == 2.0)
EOF

expect_runtime_output "Float not-equal true" "true" << 'EOF'
puts(1.0 != 2.0)
EOF

# ============================================================
# 3. BOOL COMPARISON OPERATORS
#    bools are eq-primitive (direct Go ==) but NOT ord-primitive
#    so < > <= >= route through ord_compare_bool runtime helper
# ============================================================
echo "-- BOOL COMPARISON OPERATORS --"

expect_runtime_output "Bool equality true (both true)" "true" << 'EOF'
puts(true == true)
EOF

expect_runtime_output "Bool equality true (both false)" "true" << 'EOF'
puts(false == false)
EOF

expect_runtime_output "Bool equality false" "false" << 'EOF'
puts(true == false)
EOF

expect_runtime_output "Bool not-equal true" "true" << 'EOF'
puts(true != false)
EOF

expect_runtime_output "Bool not-equal false" "false" << 'EOF'
puts(true != true)
EOF

# Bool ordering: false < true in the builtin ord_compare_bool
expect_runtime_output "Bool less-than: false < true" "true" << 'EOF'
puts(false < true)
EOF

expect_runtime_output "Bool less-than: true < false" "false" << 'EOF'
puts(true < false)
EOF

expect_runtime_output "Bool less-equal: false <= true" "true" << 'EOF'
puts(false <= true)
EOF

expect_runtime_output "Bool less-equal: true <= true (equal)" "true" << 'EOF'
puts(true <= true)
EOF

expect_runtime_output "Bool greater-equal: true >= false" "true" << 'EOF'
puts(true >= false)
EOF

expect_runtime_output "Bool greater-than: true > false" "true" << 'EOF'
puts(true > false)
EOF

# ============================================================
# 4. STRING COMPARISON OPERATORS
# ============================================================
echo "-- STRING COMPARISON OPERATORS --"

expect_runtime_output "String equality true" "true" << 'EOF'
puts("hello" == "hello")
EOF

expect_runtime_output "String equality false" "false" << 'EOF'
puts("hello" == "world")
EOF

expect_runtime_output "String not-equal true" "true" << 'EOF'
puts("a" != "b")
EOF

expect_runtime_output "String not-equal false" "false" << 'EOF'
puts("same" != "same")
EOF

expect_runtime_output "String less-than lexicographic" "true" << 'EOF'
puts("abc" < "abd")
EOF

expect_runtime_output "String greater-than lexicographic" "true" << 'EOF'
puts("z" > "a")
EOF

expect_runtime_output "String less-equal (equal strings)" "true" << 'EOF'
puts("hello" <= "hello")
EOF

expect_runtime_output "String greater-equal (greater string)" "true" << 'EOF'
puts("b" >= "a")
EOF

# ============================================================
# 5. STRING CONCATENATION WITH +
# ============================================================
echo "-- STRING CONCATENATION --"

expect_runtime_output "String concat basic" "helloworld" << 'EOF'
puts("hello" + "world")
EOF

expect_runtime_output "String concat with empty string" "hello" << 'EOF'
puts("hello" + "")
EOF

expect_runtime_output "String concat chained" "abc" << 'EOF'
puts("a" + "b" + "c")
EOF

expect_runtime_output "String concat via variables" "foobar" << 'EOF'
let a = "foo"
let b = "bar"
puts(a + b)
EOF

# ============================================================
# 6. INT ARITHMETIC (+, -, *, /)
# ============================================================
echo "-- INT ARITHMETIC --"

expect_runtime_output "Int addition" "7" << 'EOF'
puts(3 + 4)
EOF

expect_runtime_output "Int subtraction" "2" << 'EOF'
puts(5 - 3)
EOF

expect_runtime_output "Int multiplication" "12" << 'EOF'
puts(3 * 4)
EOF

expect_runtime_output "Int division" "3" << 'EOF'
puts(7 / 2)
EOF

expect_runtime_output "Int division truncation toward zero" "0" << 'EOF'
puts(1 / 3)
EOF

# ============================================================
# 7. FLOAT ARITHMETIC
# ============================================================
echo "-- FLOAT ARITHMETIC --"

expect_runtime_output "Float addition" "3.5" << 'EOF'
puts(1.5 + 2.0)
EOF

expect_runtime_output "Float subtraction" "1.5" << 'EOF'
puts(3.5 - 2.0)
EOF

expect_runtime_output "Float multiplication" "6.25" << 'EOF'
puts(2.5 * 2.5)
EOF

expect_runtime_output "Float division" "2.5" << 'EOF'
puts(5.0 / 2.0)
EOF

# ============================================================
# 8. UNARY MINUS (prefix -)
# ============================================================
echo "-- UNARY MINUS --"

expect_runtime_output "Unary minus on int literal" "-5" << 'EOF'
puts(-5)
EOF

expect_runtime_output "Unary minus on int variable" "-42" << 'EOF'
let x = 42
puts(-x)
EOF

expect_runtime_output "Unary minus on float literal" "-3.14" << 'EOF'
puts(-3.14)
EOF

expect_runtime_output "Unary minus on float variable" "-2.5" << 'EOF'
let x = 2.5
puts(-x)
EOF

expect_runtime_output "Double negation int" "7" << 'EOF'
let x = 7
puts(-(-x))
EOF

# ============================================================
# 9. BOOLEAN NEGATION (prefix !)
# ============================================================
echo "-- BOOLEAN NEGATION --"

expect_runtime_output "Boolean negation true" "false" << 'EOF'
puts(!true)
EOF

expect_runtime_output "Boolean negation false" "true" << 'EOF'
puts(!false)
EOF

expect_runtime_output "Double boolean negation" "true" << 'EOF'
puts(!(!true))
EOF

# ============================================================
# 10. OPERATOR PRECEDENCE (* / bind tighter than + -)
# ============================================================
echo "-- OPERATOR PRECEDENCE --"

expect_runtime_output "Precedence: 1 + 2 * 3 = 7 (mul before add)" "7" << 'EOF'
puts(1 + 2 * 3)
EOF

expect_runtime_output "Precedence: 1 * 2 + 3 = 5 (mul before add)" "5" << 'EOF'
puts(1 * 2 + 3)
EOF

expect_runtime_output "Precedence: 10 - 2 * 3 = 4" "4" << 'EOF'
puts(10 - 2 * 3)
EOF

expect_runtime_output "Precedence: 10 / 2 + 1 = 6" "6" << 'EOF'
puts(10 / 2 + 1)
EOF

expect_runtime_output "Parenthesized override: (1 + 2) * 3 = 9" "9" << 'EOF'
puts((1 + 2) * 3)
EOF

expect_runtime_output "Nested parens: (1 + 2) * (3 - 4) = -3" "-3" << 'EOF'
puts((1 + 2) * (3 - 4))
EOF

expect_runtime_output "Multi-operator: 2 + 3 * 4 - 6 / 2 = 11" "11" << 'EOF'
puts(2 + 3 * 4 - 6 / 2)
EOF

# ============================================================
# 11. COMPARISON IN IF CONDITION
#     Marmoset requires parentheses around if conditions.
# ============================================================
echo "-- COMPARISON IN IF CONDITION --"

expect_runtime_output "Less-than in if condition" "yes" << 'EOF'
let result = if (1 < 2) { "yes" } else { "no" }
puts(result)
EOF

expect_runtime_output "Greater-than in if condition" "no" << 'EOF'
let result = if (1 > 2) { "yes" } else { "no" }
puts(result)
EOF

expect_runtime_output "Equality in if condition" "equal" << 'EOF'
let x = 42
let y = 42
let result = if (x == y) { "equal" } else { "different" }
puts(result)
EOF

expect_runtime_output "Not-equal in if condition" "different" << 'EOF'
let x = 1
let y = 2
let result = if (x != y) { "different" } else { "same" }
puts(result)
EOF

expect_runtime_output "Less-equal in if condition" "yes" << 'EOF'
let result = if (5 <= 5) { "yes" } else { "no" }
puts(result)
EOF

expect_runtime_output "Greater-equal in if condition" "yes" << 'EOF'
let result = if (10 >= 3) { "yes" } else { "no" }
puts(result)
EOF

# ============================================================
# 12. OPERATORS ON FUNCTION CALL RESULTS
# ============================================================
echo "-- OPERATORS ON FUNCTION RESULTS --"

expect_runtime_output "Addition of function call results" "7" << 'EOF'
let f = fn(x: int) -> int { x + 1 }
let g = fn(x: int) -> int { x * 2 }
puts(f(2) + g(2))
EOF

expect_runtime_output "Comparison of function call results" "true" << 'EOF'
let f = fn(x: int) -> int { x * 10 }
let g = fn(x: int) -> int { x + 1 }
puts(f(1) > g(1))
EOF

# ============================================================
# 13. CHAINED/NESTED COMPARISONS
# ============================================================
echo "-- CHAINED / NESTED COMPARISONS --"

expect_runtime_output "Comparison results used as bool operands" "true" << 'EOF'
let a = 1 < 2
let b = 3 > 0
puts(a == b)
EOF

expect_runtime_output "Comparison result stored then printed" "true" << 'EOF'
let x = 10 >= 5
puts(x)
EOF

expect_runtime_output "Comparison result as function argument" "ok" << 'EOF'
let check = fn(b: bool) -> string {
  if (b) { "ok" } else { "fail" }
}
puts(check(3 <= 3))
EOF

# ============================================================
# 14. OPERATOR ON IF-EXPRESSION RESULT
# ============================================================
echo "-- OPERATOR ON IF-EXPRESSION RESULT --"

expect_runtime_output "Arithmetic on if-expression result" "15" << 'EOF'
let x = if (true) { 10 } else { 20 }
puts(x + 5)
EOF

expect_runtime_output "Comparison on if-expression result" "true" << 'EOF'
let x = if (true) { 10 } else { 20 }
puts(x < 15)
EOF

# ============================================================
# 15. CUSTOM TRAIT IMPLS DRIVING OPERATORS
#     These test that non-primitive operator desugaring
#     correctly routes through trait helper functions.
# ============================================================
echo "-- CUSTOM TRAIT IMPLS DRIVING OPERATORS --"

expect_runtime_output "Custom eq impl on enum drives == operator" "true" << 'EOF'
enum color { red green blue }
impl eq for color {
  fn eq(x: color, y: color) -> bool {
    true
  }
}
puts(color.red == color.blue)
EOF

expect_runtime_output "Custom eq impl on enum drives != operator (negated eq)" "false" << 'EOF'
enum color { red green blue }
impl eq for color {
  fn eq(x: color, y: color) -> bool {
    true
  }
}
puts(color.red != color.blue)
EOF

# BUG: Custom ord impl on enum returns ordering enum type,
# but emitter generates `ord_compare_TYPE(...) == int64(0)`
# which causes a Go type mismatch: ordering vs int64.
# This test documents the bug and SHOULD pass once fixed.
expect_runtime_output "Custom ord impl on enum drives < operator" "true" << 'EOF'
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

# Same bug as above for >=
expect_runtime_output "Custom ord impl on enum drives >= operator" "false" << 'EOF'
enum rank { low mid high }
impl eq for rank {
  fn eq(x: rank, y: rank) -> bool { false }
}
impl ord for rank {
  fn compare(x: rank, y: rank) -> ordering {
    ordering.less
  }
}
puts(rank.high >= rank.low)
EOF

expect_runtime_output "Custom num impl drives * operator on enum" "42" << 'EOF'
enum boxed { val(int) }
impl num for boxed {
  fn add(x: boxed, y: boxed) -> boxed { x }
  fn sub(x: boxed, y: boxed) -> boxed { x }
  fn mul(x: boxed, y: boxed) -> boxed { x }
  fn div(x: boxed, y: boxed) -> boxed { x }
}
let a = boxed.val(42)
let b = boxed.val(99)
match (a * b) {
  boxed.val(n): puts(n)
}
EOF

expect_runtime_output "Custom num impl drives - (binary) operator on enum" "7" << 'EOF'
enum boxed { val(int) }
impl num for boxed {
  fn add(x: boxed, y: boxed) -> boxed { x }
  fn sub(x: boxed, y: boxed) -> boxed { x }
  fn mul(x: boxed, y: boxed) -> boxed { x }
  fn div(x: boxed, y: boxed) -> boxed { x }
}
let a = boxed.val(7)
let b = boxed.val(3)
match (a - b) {
  boxed.val(n): puts(n)
}
EOF

expect_runtime_output "Custom num impl drives / operator on enum" "11" << 'EOF'
enum boxed { val(int) }
impl num for boxed {
  fn add(x: boxed, y: boxed) -> boxed { x }
  fn sub(x: boxed, y: boxed) -> boxed { x }
  fn mul(x: boxed, y: boxed) -> boxed { x }
  fn div(x: boxed, y: boxed) -> boxed { y }
}
let a = boxed.val(7)
let b = boxed.val(11)
match (a / b) {
  boxed.val(n): puts(n)
}
EOF

expect_runtime_output "Custom neg impl drives unary - on enum" "99" << 'EOF'
enum wrapper { val(int) }
impl neg for wrapper {
  fn neg(x: wrapper) -> wrapper { x }
}
let a = wrapper.val(99)
match (-a) {
  wrapper.val(n): puts(n)
}
EOF

# ============================================================
# 16. MIXED PRIMITIVE AND NON-PRIMITIVE IN SAME PROGRAM
# ============================================================
echo "-- MIXED PRIMITIVE AND NON-PRIMITIVE OPS --"

expect_runtime_output "Mixed: primitive int + and custom enum + in same program" "10
42" << 'EOF'
enum boxed { val(int) }
impl num for boxed {
  fn add(x: boxed, y: boxed) -> boxed { x }
  fn sub(x: boxed, y: boxed) -> boxed { x }
  fn mul(x: boxed, y: boxed) -> boxed { x }
  fn div(x: boxed, y: boxed) -> boxed { x }
}
puts(3 + 7)
let a = boxed.val(42)
let b = boxed.val(1)
match (a + b) {
  boxed.val(n): puts(n)
}
EOF

expect_runtime_output "Mixed: primitive == and custom enum == in same program" "true
false" << 'EOF'
enum color { red green blue }
impl eq for color {
  fn eq(x: color, y: color) -> bool { false }
}
puts(1 == 1)
puts(color.red == color.green)
EOF

# ============================================================
# 17. OPERATORS ON RECORD FIELD ACCESS
# ============================================================
echo "-- OPERATOR ON RECORD FIELD ACCESS --"

expect_runtime_output "Addition of record fields" "30" << 'EOF'
type point = { x: int, y: int }
let p: point = { x: 10, y: 20 }
puts(p.x + p.y)
EOF

expect_runtime_output "Comparison of record fields" "true" << 'EOF'
type point = { x: int, y: int }
let p: point = { x: 10, y: 20 }
puts(p.x < p.y)
EOF

expect_runtime_output "Multiplication of record fields" "200" << 'EOF'
type rect = { w: int, h: int }
let r: rect = { w: 10, h: 20 }
puts(r.w * r.h)
EOF

# ============================================================
# 18. NEGATIVE INT COMPARISONS
# ============================================================
echo "-- NEGATIVE INT COMPARISONS --"

expect_runtime_output "Negative int less-than zero" "true" << 'EOF'
puts(-5 < 0)
EOF

expect_runtime_output "Zero greater-equal negative" "true" << 'EOF'
puts(0 >= -1)
EOF

expect_runtime_output "Negative int equality" "true" << 'EOF'
let x = -10
let y = -10
puts(x == y)
EOF

# ============================================================
# 19. ZERO COMPARISONS (edge cases for all comparison ops)
# ============================================================
echo "-- ZERO COMPARISONS --"

expect_runtime_output "Zero == zero" "true" << 'EOF'
puts(0 == 0)
EOF

expect_runtime_output "Zero <= zero" "true" << 'EOF'
puts(0 <= 0)
EOF

expect_runtime_output "Zero >= zero" "true" << 'EOF'
puts(0 >= 0)
EOF

expect_runtime_output "Zero < zero is false" "false" << 'EOF'
puts(0 < 0)
EOF

expect_runtime_output "Zero > zero is false" "false" << 'EOF'
puts(0 > 0)
EOF

# ============================================================
# 20. TRAIT REJECTION: OPERATORS REQUIRE CORRECT TRAITS
# ============================================================
echo "-- TRAIT REJECTION FOR OPERATORS --"

expect_build "Negation requires neg trait (bool should fail)" "missing-impl" << 'EOF'
let x = true
puts(-x)
EOF

expect_build "Negation requires neg trait (string should fail)" "missing-impl" << 'EOF'
let x = "hello"
puts(-x)
EOF

expect_build "String subtraction should fail (not num)" "missing-impl" << 'EOF'
puts("a" - "b")
EOF

expect_build "String multiplication should fail (not num)" "missing-impl" << 'EOF'
puts("a" * "b")
EOF

expect_build "String division should fail (not num)" "missing-impl" << 'EOF'
puts("a" / "b")
EOF

expect_build "Ordering on function type should fail" "missing-impl" << 'EOF'
let f = fn(x: int) -> int { x }
let g = fn(x: int) -> int { x }
puts(f < g)
EOF

# ============================================================
# 21. COMPLEX EXPRESSION COMBINATIONS
# ============================================================
echo "-- COMPLEX EXPRESSION COMBINATIONS --"

expect_runtime_output "Operator result in let-binding chain" "13" << 'EOF'
let a = 3 + 4
let b = a + 6
puts(b)
EOF

expect_runtime_output "Multiple operators in single let-binding" "14" << 'EOF'
let x = 2 + 3 * 4
puts(x)
EOF

expect_runtime_output "Nested function calls with arithmetic" "22" << 'EOF'
let double = fn(x: int) -> int { x * 2 }
let inc = fn(x: int) -> int { x + 1 }
puts(double(inc(10)))
EOF

expect_runtime_output "Boolean expression from comparison chain" "true" << 'EOF'
let low = 1 < 10
let high = 100 > 50
let both = low == high
puts(both)
EOF

suite_end
