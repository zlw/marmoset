#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

suite_begin "EDGE CASE TESTS - INHERENT METHODS (P2-10)"

# ===========================================================================
# GROUP 1: Basic inherent methods on various types
# ===========================================================================

expect_runtime_output "Inherent method on type alias (int)" "6" << 'EOF'
type myint = int
impl myint {
  fn triple(x: myint) -> myint {
    x + x + x
  }
}
let n: myint = 2
puts(n.triple())
EOF

expect_runtime_output "Inherent method on enum type" "42" << 'EOF'
enum box { val(int) }
impl box {
  fn unwrap(b: box) -> int {
    match b {
      box.val(n): n
    }
  }
}
let b = box.val(42)
puts(b.unwrap())
EOF

expect_runtime_output "Inherent method on record type alias" "30" << 'EOF'
type point = { x: int, y: int }
impl point {
  fn sum(p: point) -> int {
    p.x + p.y
  }
}
let p: point = { x: 10, y: 20 }
puts(p.sum())
EOF

# ===========================================================================
# GROUP 2: Multiple methods, method interactions
# ===========================================================================

expect_runtime_output "Multiple inherent methods on same type" "15" << 'EOF'
type point = { x: int, y: int }
impl point {
  fn get_x(p: point) -> int { p.x }
  fn get_y(p: point) -> int { p.y }
  fn sum(p: point) -> int { p.x + p.y }
}
let p: point = { x: 7, y: 8 }
puts(p.sum())
EOF

expect_runtime_output "Inherent method calling another inherent method on same type" "20" << 'EOF'
impl int {
  fn twice(x: int) -> int { x + x }
  fn quadruple(x: int) -> int { x.twice().twice() }
}
let n = 5
puts(n.quadruple())
EOF

expect_runtime_output "Inherent method accessing receiver fields" "110" << 'EOF'
type rect = { w: int, h: int }
impl rect {
  fn area(r: rect) -> int {
    r.w * r.h
  }
}
let r: rect = { w: 10, h: 11 }
puts(r.area())
EOF

# ===========================================================================
# GROUP 3: Parameter variations, return type variations
# ===========================================================================

expect_runtime_output "Inherent method with multiple parameters" "7" << 'EOF'
impl int {
  fn clamp(x: int, lo: int, hi: int) -> int {
    if (x < lo) { lo } else { if (x > hi) { hi } else { x } }
  }
}
let n = 7
puts(n.clamp(0, 10))
EOF

expect_runtime_output "Inherent method returning bool from int receiver" "true" << 'EOF'
impl int {
  fn is_positive(x: int) -> bool {
    x > 0
  }
}
let n = 42
puts(n.is_positive())
EOF

expect_runtime_output "Inherent method returning string from int receiver" "hello" << 'EOF'
impl int {
  fn greet(x: int) -> string {
    "hello"
  }
}
let n = 1
puts(n.greet())
EOF

expect_runtime_output "Inherent method returning the receiver type" "10" << 'EOF'
impl int {
  fn double(x: int) -> int {
    x + x
  }
}
let n = 5
puts(n.double())
EOF

# ===========================================================================
# GROUP 4: Complex expressions in method bodies
# ===========================================================================

expect_runtime_output "Inherent method body with if expression" "big" << 'EOF'
impl int {
  fn classify(x: int) -> string {
    if (x > 100) { "big" } else { "small" }
  }
}
let n = 999
puts(n.classify())
EOF

expect_runtime_output "Inherent method body with match expression" "one" << 'EOF'
enum color { red green blue }
impl color {
  fn rank(c: color) -> string {
    match c {
      color.red: "one"
      color.green: "two"
      color.blue: "three"
    }
  }
}
let c = color.red
puts(c.rank())
EOF

expect_runtime_output "Inherent method body with arithmetic" "25" << 'EOF'
type point = { x: int, y: int }
impl point {
  fn dist_sq(p: point) -> int {
    (p.x * p.x) + (p.y * p.y)
  }
}
let p: point = { x: 3, y: 4 }
puts(p.dist_sq())
EOF

# ===========================================================================
# GROUP 5: Dot syntax and chaining
# ===========================================================================

expect_runtime_output "Calling inherent method via dot syntax on let binding" "8" << 'EOF'
impl int {
  fn double(x: int) -> int { x + x }
}
let n = 4
puts(n.double())
EOF

expect_runtime_output "Chaining inherent methods that return same type" "16" << 'EOF'
impl int {
  fn double(x: int) -> int { x + x }
}
let n = 2
puts(n.double().double().double())
EOF

expect_runtime_output "Calling inherent method on non-ambiguous int literal" "6" << 'EOF'
impl int {
  fn triple(x: int) -> int { x + x + x }
}
puts(2.triple())
EOF

# BUG DETECTOR: Integer literal followed by dot-method where method name starts
# with valid float-suffix characters (e.g. "double" starts with "d") causes
# misparsing. The literal "5.d..." may be consumed as a float token.
# Use (5).double() or let-binding as workaround.
expect_runtime_output "Int literal dot-method via parenthesized literal workaround" "10" << 'EOF'
impl int {
  fn double(x: int) -> int { x + x }
}
puts((5).double())
EOF

# ===========================================================================
# GROUP 6: Coexistence with trait methods
# ===========================================================================

expect_runtime_output "Inherent method coexists with trait method (different names)" "hello
42" << 'EOF'
type point = { x: int, y: int }
trait describable[a] {
  fn describe(x: a) -> string
}
impl describable for point {
  fn describe(p: point) -> string { "hello" }
}
impl point {
  fn sum(p: point) -> int { p.x + p.y }
}
let p: point = { x: 20, y: 22 }
puts(p.describe())
puts(p.sum())
EOF

expect_build "Inherent method same name as trait method is ambiguity error" "collides with trait method" << 'EOF'
trait describable[a] {
  fn describe(x: a) -> string
}
impl describable for int {
  fn describe(x: int) -> string { "trait" }
}
impl int {
  fn describe(x: int) -> string { "inherent" }
}
puts(1)
EOF

expect_build "Inherent method does NOT satisfy trait constraints" "does not implement trait" << 'EOF'
trait showable[a] {
  fn display(x: a) -> string
}
type widget = { id: int }
impl widget {
  fn display(w: widget) -> string { "widget" }
}
let render = fn[t: showable](x: t) -> string {
  x.display()
}
let w: widget = { id: 1 }
puts(render(w))
EOF

# Inherent method collides with builtin show trait for int
expect_build "Inherent method collides with builtin show on int" "collides with trait method" << 'EOF'
impl int {
  fn show(x: int) -> string { "my-show" }
}
puts(1)
EOF

# ===========================================================================
# GROUP 7: Usage in various expression contexts
# ===========================================================================

# The impl method parser only supports -> (pure arrow), not => (effectful arrow).
# This is a parse-level limitation: => is not recognized in method signatures.
expect_build "Effectful arrow (=>) in inherent method signature is rejected at parse time" "__ANY_ERROR__" << 'EOF'
impl int {
  fn greet(x: int) => string { "hello" }
}
puts(1)
EOF

expect_runtime_output "Inherent method result used in let binding" "10" << 'EOF'
impl int {
  fn double(x: int) -> int { x + x }
}
let n = 5
let result = n.double()
puts(result)
EOF

expect_runtime_output "Inherent method result used in arithmetic" "14" << 'EOF'
impl int {
  fn double(x: int) -> int { x + x }
}
let n = 3
let a = n.double()
let b = a + 8
puts(b)
EOF

expect_runtime_output "Inherent method result used as function argument" "20" << 'EOF'
impl int {
  fn double(x: int) -> int { x + x }
}
let show_num = fn(n: int) -> int { n }
let n = 10
puts(show_num(n.double()))
EOF

expect_runtime_output "Inherent method on type that also has field access" "35" << 'EOF'
type point = { x: int, y: int }
impl point {
  fn sum(p: point) -> int { p.x + p.y }
}
let p: point = { x: 15, y: 20 }
puts(p.sum())
EOF

# ===========================================================================
# GROUP 8: Records, enums, and multiple types
# ===========================================================================

expect_runtime_output "Inherent method on type alias for record, create and call" "3" << 'EOF'
type pair = { a: int, b: int }
impl pair {
  fn sum(p: pair) -> int { p.a + p.b }
}
let p: pair = { a: 1, b: 2 }
puts(p.sum())
EOF

expect_runtime_output "Two different types each with inherent methods" "30
hello" << 'EOF'
type point = { x: int, y: int }
type label = { text: string }
impl point {
  fn sum(p: point) -> int { p.x + p.y }
}
impl label {
  fn get_text(l: label) -> string { l.text }
}
let p: point = { x: 10, y: 20 }
let l: label = { text: "hello" }
puts(p.sum())
puts(l.get_text())
EOF

expect_runtime_output "Inherent method that creates and returns a new record" "99" << 'EOF'
type box = { value: int }
impl int {
  fn boxed(x: int) -> box { { value: x } }
}
let n = 99
let b = n.boxed()
puts(b.value)
EOF

# ===========================================================================
# GROUP 9: Error cases / negative tests
# ===========================================================================

expect_build "Duplicate inherent method on same type is rejected" "Duplicate inherent method" << 'EOF'
impl int {
  fn ping(x: int) -> int { x }
}
impl int {
  fn ping(x: int) -> int { x + 1 }
}
puts(1)
EOF

expect_build "Inherent method receiver type mismatch is rejected" "does not match impl target type" << 'EOF'
type point = { x: int }
impl point {
  fn bad(x: int) -> int { x }
}
puts(1)
EOF

expect_build "Inherent method missing return type annotation is rejected" "missing a return type annotation" << 'EOF'
impl int {
  fn oops(x: int) { x }
}
puts(1)
EOF

expect_build "Inherent method missing parameter type annotation is rejected" "missing a type annotation" << 'EOF'
impl int {
  fn oops(x) -> int { x }
}
puts(1)
EOF

expect_build "Inherent method with no parameters is rejected" "must declare a receiver parameter" << 'EOF'
impl int {
  fn oops() -> int { 1 }
}
puts(1)
EOF

expect_build "Inherent method return type mismatch is rejected" "__ANY_ERROR__" << 'EOF'
impl int {
  fn bad(x: int) -> string { x + 1 }
}
puts(1)
EOF

expect_build "Calling nonexistent inherent method is rejected" "No method" << 'EOF'
type point = { x: int }
let p: point = { x: 1 }
puts(p.nonexistent())
EOF

expect_runtime_output "Generic inherent impl target on enum type works" "true
false" << 'EOF'
enum result[a, b] {
  success(a)
  failure(b)
}
impl result[a, b] {
  fn is_success(r: result[a, b]) -> bool {
    match r {
      result.success(_): true
      result.failure(_): false
    }
  }
}
let ok = if (true) { result.success(1) } else { result.failure("x") }
let err = if (false) { result.success(1) } else { result.failure("x") }
puts(ok.is_success())
puts(err.is_success())
EOF

expect_runtime_output "Generic inherent method call works with unconstrained enum type arguments" "true" << 'EOF'
enum result[a, b] {
  success(a)
  failure(b)
}
impl result[a, b] {
  fn is_success(r: result[a, b]) -> bool {
    match r {
      result.success(_): true
      result.failure(_): false
    }
  }
}
let ok = result.success(1)
puts(ok.is_success())
EOF

expect_runtime_output "Concrete inherent impl target takes precedence over matching generic target" "concrete
failure" << 'EOF'
enum result[a, b] {
  success(a)
  failure(b)
}
impl result[a, b] {
  fn tag(r: result[a, b]) -> string {
    match r {
      result.success(_): "success"
      result.failure(_): "failure"
    }
  }
}
impl result[int, string] {
  fn tag(r: result[int, string]) -> string { "concrete" }
}
let x = if (true) { result.success(1) } else { result.failure("boom") }
let y = if (false) { result.success(true) } else { result.failure("boom") }
puts(x.tag())
puts(y.tag())
EOF

# ===========================================================================
# GROUP 10: Interaction edge cases and bug detectors
# ===========================================================================

expect_runtime_output "Inherent method on int does not break builtin show" "42" << 'EOF'
impl int {
  fn double(x: int) -> int { x + x }
}
let n = 42
puts(n.show())
EOF

# BUG DETECTOR: Using Go reserved keyword "default" as a parameter name in an
# inherent method causes a Go compilation failure because the codegen emits the
# parameter name verbatim into the generated Go source code.
expect_runtime_output "Inherent method with Go keyword 'default' as param name" "1" << 'EOF'
enum option_int { some(int) none }
impl option_int {
  fn unwrap_or(o: option_int, default: int) -> int {
    match o {
      option_int.some(v): v
      option_int.none: default
    }
  }
}
let x = option_int.some(1)
puts(x.unwrap_or(0))
EOF

# Same bug but with the none path exercised
expect_runtime_output "Inherent method with Go keyword 'default' param (none path)" "99" << 'EOF'
enum option_int { some(int) none }
impl option_int {
  fn unwrap_or(o: option_int, default: int) -> int {
    match o {
      option_int.some(v): v
      option_int.none: default
    }
  }
}
let x = option_int.none
puts(x.unwrap_or(99))
EOF

# Workaround: using a non-keyword parameter name should work
expect_runtime_output "Inherent method on enum with non-keyword param (workaround)" "1" << 'EOF'
enum option_int { some(int) none }
impl option_int {
  fn unwrap_or(o: option_int, fallback: int) -> int {
    match o {
      option_int.some(v): v
      option_int.none: fallback
    }
  }
}
let x = option_int.some(1)
puts(x.unwrap_or(0))
EOF

expect_runtime_output "Inherent method result passed through chained method calls" "21" << 'EOF'
impl int {
  fn double(x: int) -> int { x + x }
  fn add_one(x: int) -> int { x + 1 }
}
let n = 10
puts(n.double().add_one())
EOF

expect_runtime_output "Inherent method on record used in conditional" "positive" << 'EOF'
type point = { x: int, y: int }
impl point {
  fn sum(p: point) -> int { p.x + p.y }
}
let p: point = { x: 5, y: 3 }
if (p.sum() > 0) {
  puts("positive")
} else {
  puts("negative")
}
EOF

expect_runtime_output "Multiple inherent methods called on separate instances" "6
15" << 'EOF'
type point = { x: int, y: int }
impl point {
  fn sum(p: point) -> int { p.x + p.y }
}
let a: point = { x: 1, y: 5 }
let b: point = { x: 7, y: 8 }
puts(a.sum())
puts(b.sum())
EOF

expect_runtime_output "Inherent method on record returns bool" "true" << 'EOF'
type point = { x: int, y: int }
impl point {
  fn is_origin(p: point) -> bool { p.x == 0 }
}
let p: point = { x: 0, y: 5 }
puts(p.is_origin())
EOF

# Ensure separate types with same-name inherent methods don't collide
expect_runtime_output "Same method name on different types via inherent impls" "3
7" << 'EOF'
type point = { x: int, y: int }
type rect = { w: int, h: int }
impl point {
  fn total(p: point) -> int { p.x + p.y }
}
impl rect {
  fn total(r: rect) -> int { r.w + r.h }
}
let p: point = { x: 1, y: 2 }
let r: rect = { w: 3, h: 4 }
puts(p.total())
puts(r.total())
EOF

suite_end
