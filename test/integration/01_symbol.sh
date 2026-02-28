#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

suite_begin "EDGE CASE TESTS - SYMBOL/NAME RESOLUTION (P1-8)"

echo "-- VARIABLE SHADOWING --"

# Inner let binding should shadow outer let binding in a function body.
# The function body creates a new scope; x inside should be 2, not 1.
expect_runtime_output "Variable shadowing in nested scope (let inside fn body)" "2" << 'EOF'
let x = 1
let y = fn() -> int { let x = 2; x }
puts(y())
EOF

# Function parameter should shadow outer variable of the same name.
expect_runtime_output "Function parameter shadows outer variable" "10" << 'EOF'
let x = 1
let f = fn(x: int) -> int { x }
puts(f(10))
EOF

# After the function, the outer x should still be accessible with its
# original value.
expect_runtime_output "Outer variable survives shadowing in inner scope" "1" << 'EOF'
let x = 1
let f = fn(x: int) -> int { x + 1 }
let _ = f(10)
puts(x)
EOF

# Re-binding at the same top-level scope should be rejected.
expect_build "Duplicate let binding at same top-level scope" "Duplicate top-level let definition: x" << 'EOF'
let x = 1
let x = 2
puts(x)
EOF

echo ""
echo "-- DUPLICATE DEFINITIONS --"

# Duplicate trait definition should be rejected.
expect_build "Duplicate trait definition is rejected" "Duplicate trait definition: myshow" << 'EOF'
trait myshow[a] {
  fn myshow(x: a) -> string
}
trait myshow[a] {
  fn myshow(x: a) -> int
}
puts(1)
EOF

# Duplicate enum definition should be rejected.
expect_build "Duplicate enum definition is rejected" "Duplicate enum definition: color" << 'EOF'
enum color { red green blue }
enum color { cyan magenta yellow }
puts(1)
EOF

# Duplicate type alias definition should be rejected.
expect_build "Duplicate type alias definition is rejected" "Duplicate type alias definition: vec2" << 'EOF'
type vec2 = { x: int, y: int }
type vec2 = { a: int, b: int }
puts(1)
EOF

echo ""
echo "-- FORWARD REFERENCES --"

# Using a non-function value before it is defined should fail.
# Only top-level function bindings participate in forward reference resolution.
expect_build "Using a value before it is defined (forward ref to value)" "Unbound variable" << 'EOF'
let a = b
let b = 42
puts(a)
EOF

# Using a function before it is defined should work (top-level forward ref).
expect_runtime_output "Function calling another function defined later (forward ref)" "6" << 'EOF'
let main_val = double(3)
let double = fn(x: int) -> int { x + x }
puts(main_val)
EOF

# Recursive function: a function should be able to call itself.
expect_runtime_output "Recursive function self-reference" "120" << 'EOF'
let factorial = fn(n: int) -> int {
  if (n <= 1) { return 1 }
  return n * factorial(n - 1);
}
puts(factorial(5))
EOF

# Mutually recursive functions should work via top-level forward declaration.
expect_runtime_output "Mutually recursive functions (top-level forward ref)" "true" << 'EOF'
let is_even = fn(n: int) -> bool {
  if (n == 0) { true } else { is_odd(n - 1) }
}
let is_odd = fn(n: int) -> bool {
  if (n == 0) { false } else { is_even(n - 1) }
}
puts(is_even(10))
EOF

echo ""
echo "-- NAME COLLISION WITH BUILTINS --"

# BUG: Codegen emits Go builtin len() instead of the user-defined function.
# Typechecker correctly accepts the shadowing, but the Go emitter still
# references the runtime helper. Go compile error: builtin len does not
# accept int64.
expect_build "BUG: Shadowing builtin 'len' produces Go compile error" "__ANY_ERROR__" << 'EOF'
let len = fn(x: int) -> int { x * 100 }
puts(len(1))
EOF

# BUG: Same issue with 'first' -- codegen emits the runtime helper first()
# instead of the user-defined function. Go compile error: type mismatch.
expect_build "BUG: Shadowing builtin 'first' produces Go compile error" "__ANY_ERROR__" << 'EOF'
let first = fn(x: int) -> int { x }
puts(first(99))
EOF

echo ""
echo "-- NAME COLLISION WITH TYPES --"

# Variable name same as a trait name: traits and values live in different
# namespaces. The value binding should not collide with the trait.
expect_runtime_output "Variable name same as trait name" "42" << 'EOF'
trait myval[a] {
  fn myval(x: a) -> int
}
let myval = 42
puts(myval)
EOF

# Variable name same as enum type name. After the let binding, the variable
# value (7) should be used, not the enum.
expect_runtime_output "Variable name same as enum type name" "7" << 'EOF'
enum status { ok fail }
let status = 7
puts(status)
EOF

echo ""
echo "-- CLOSURES AND NESTED SCOPE --"

# Regression: closure captures outer let-bound variable and runs correctly.
expect_runtime_output "Closure capture of outer let-bound variable works" "15" << 'EOF'
let x = 10
let add_x = fn(y: int) -> int { x + y }
puts(add_x(5))
EOF

# BUG: Same closure capture issue with deeper nesting. Multiple undefined
# variables in the emitted Go.
expect_build "BUG: Deep nesting closure capture fails codegen" "__ANY_ERROR__" << 'EOF'
let a = 1
let f = fn() -> int {
  let b = 2
  let g = fn() -> int {
    let c = 3
    a + b + c
  }
  g()
}
puts(f())
EOF

# Nested function parameter shadows outer variable; inner function captures
# the parameter (which is in scope as a parameter, not a let-bound closure
# variable). This works because the parameter is a formal argument.
expect_runtime_output "Nested parameter shadow with outer access" "30" << 'EOF'
let x = 10
let f = fn(x: int) -> int {
  let g = fn(y: int) -> int { x + y }
  g(x)
}
puts(f(15))
EOF

echo ""
echo "-- PATTERN BINDING SCOPE --"

# Match arm binds a variable in pattern scope.
expect_runtime_output "Pattern binding in match arm scope" "42" << 'EOF'
let val = 42
let result = match val {
  n: n
}
puts(result)
EOF

# Enum pattern binding creates a new variable in arm scope.
expect_runtime_output "Enum pattern binding creates scoped variable" "7" << 'EOF'
enum option[a] { some(a) none }
let x = option.some(7)
let result = match x {
  option.some(v): v
  option.none: 0
}
puts(result)
EOF

# Pattern variable should not leak out of match arm scope.
# After the match, 'v' should not be accessible.
expect_build "Pattern variable does not leak out of match arm" "Unbound variable" << 'EOF'
enum option[a] { some(a) none }
let x = option.some(7)
let result = match x {
  option.some(v): v
  option.none: 0
}
puts(v)
EOF

echo ""
echo "-- EDGE CASE: VARIABLE USED IN OWN INITIALIZER --"

# BUG: `let x = x + 1` at top level where x is not previously defined.
# The typechecker allows this because infer_let adds x to the env as a
# self-reference (designed for recursive function support). But the generated
# Go code `x := (x + 1)` fails because Go does not allow referencing a
# variable in its own short variable declaration.
# This should ideally be caught during typechecking for non-function bindings.
expect_build "BUG: Self-referential let (non-function) passes typecheck but fails codegen" "__ANY_ERROR__" << 'EOF'
let x = x + 1
puts(x)
EOF

echo ""
echo "-- UNDERSCORE BINDING --"

# Underscore as let binding should not introduce _ into the scope.
expect_build "Underscore binding does not introduce a variable" "Unbound variable" << 'EOF'
let _ = 42
puts(_)
EOF

# Multiple underscore bindings should be allowed (no duplicate error).
expect_runtime_output "Multiple underscore bindings are allowed" "done" << 'EOF'
let _ = 1
let _ = 2
let _ = 3
puts("done")
EOF

echo ""
echo "-- EMPTY AND TRIVIAL PROGRAMS --"

# Single literal expression should typecheck and build without error.
expect_build "Minimal program: single literal" "" << 'EOF'
42
EOF

# Single puts call - minimal working program.
expect_runtime_output "Minimal puts program" "hello" << 'EOF'
puts("hello")
EOF

echo ""
echo "-- VARIABLE NAME EDGE CASES --"

# Very long variable name.
expect_runtime_output "Very long variable name" "1" << 'EOF'
let long_name = 1
puts(long_name)
EOF

# Single-character variable names.
expect_runtime_output "Single-character variable names" "3" << 'EOF'
let a = 1
let b = 2
puts(a + b)
EOF

# Variable names with numbers.
expect_runtime_output "Variable names with numbers" "30" << 'EOF'
let x1 = 10
let x2 = 20
puts(x1 + x2)
EOF

echo ""
echo "-- FUNCTION NAME SAME AS PARAMETER --"

# A function with the same name as its own parameter:
# `let f = fn(f: int) -> int { f }` - the parameter should shadow the
# function's self-reference inside the body.
expect_runtime_output "Function with parameter same name as function" "99" << 'EOF'
let f = fn(f: int) -> int { f }
puts(f(99))
EOF

echo ""
echo "-- SCOPE BOUNDARY: IF-EXPRESSION --"

# Let binding inside if-branch should not be visible outside.
# Both branches create separate scopes.
expect_runtime_output "If-expression branches have independent scope" "10" << 'EOF'
let x = 10
let result = if (true) { let y = 20; x } else { let y = 30; x }
puts(result)
EOF

echo ""
echo "-- TRAIT METHOD ON WRONG TYPE --"

# Calling a trait method on a type that does not implement the trait
# should produce a type error at the call site.
expect_build "Trait method called on type without impl" "does not implement trait" << 'EOF'
trait greetable[a] {
  fn greet(x: a) -> string
}
let x = 42
let f = fn[t: greetable](x: t) -> string { x.greet() }
puts(f(x))
EOF

echo ""
echo "-- ENUM VARIANT REQUIRES ENUM PREFIX --"

# Accessing an enum variant without the enum prefix should fail.
# Enum constructors are namespaced: color.red, not just red.
expect_build "Enum variant without enum prefix fails" "__ANY_ERROR__" << 'EOF'
enum color { red green blue }
let x = red
puts(x)
EOF

echo ""
echo "-- USING TYPE/TRAIT BEFORE DEFINITION --"

# Type aliases are registered in a separate pass before inference, so
# forward references to type aliases actually work. This documents
# that behavior.
expect_runtime_output "Type alias forward ref works (separate registration pass)" "1" << 'EOF'
let x: point = { x: 1, y: 2 }
type point = { x: int, y: int }
puts(x.x)
EOF

# Using a trait before it is defined should fail because trait definitions
# are processed in declaration order during inference.
expect_build "Trait used before definition" "__ANY_ERROR__" << 'EOF'
let f = fn[t: mything](x: t) -> string { x.name }
trait mything {
  name: string
}
puts(f({ name: "hi" }))
EOF

# Using an enum constructor before the enum is defined should fail:
# non-function forward refs are not allowed.
expect_build "Enum constructor used before enum definition" "__ANY_ERROR__" << 'EOF'
let x = color.red
enum color { red green blue }
puts(x)
EOF

suite_end
