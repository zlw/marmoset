#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

suite_begin "EDGE CASE TESTS - IMPLS AND GENERIC IMPLS (P2-9)"

echo "-- CONCRETE IMPL EDGE CASES --"

# 1. Impl for record type (via type alias)
expect_runtime_output "Impl for record type via type alias" "point(1,2)" << 'EOF'
type point = { x: int, y: int }
trait show[a] {
  fn show(x: a) -> string
}
impl show for point {
  fn show(p: point) -> string {
    "point(" + p.x.show() + "," + p.y.show() + ")"
  }
}
let p: point = { x: 1, y: 2 }
puts(p.show())
EOF

# 2. Impl for enum type
expect_runtime_output "Impl for enum type" "red" << 'EOF'
enum color { red green blue }
trait show[a] {
  fn show(x: a) -> string
}
impl show for color {
  fn show(c: color) -> string {
    match c {
      color.red: "red"
      color.green: "green"
      color.blue: "blue"
    }
  }
}
let c = color.red
puts(c.show())
EOF

# 3. Multiple impls for same type (different traits)
expect_runtime_output "Multiple impls for same type (different traits)" "42
true" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
trait double[a] {
  fn double(x: a) -> int
}
impl show for int {
  fn show(x: int) -> string { "42" }
}
impl double for int {
  fn double(x: int) -> int { x + x }
}
puts(1.show())
puts(3.double() == 6)
EOF

# 4. Multiple traits, each with impl for same type
expect_runtime_output "Multiple traits each with impl for same type" "hello
true" << 'EOF'
trait greet[a] {
  fn greet(x: a) -> string
}
trait check[a] {
  fn check(x: a) -> bool
}
impl greet for string {
  fn greet(s: string) -> string { "hello" }
}
impl check for string {
  fn check(s: string) -> bool { true }
}
puts("world".greet())
puts("world".check())
EOF

# 5. Impl method that calls another trait method (builtin show)
expect_runtime_output "Impl method calls another trait method" "value:42" << 'EOF'
type box = { value: int }
trait describe[a] {
  fn describe(x: a) -> string
}
impl describe for box {
  fn describe(b: box) -> string {
    "value:" + b.value.show()
  }
}
let b: box = { value: 42 }
puts(b.describe())
EOF

# 6. Impl method that accesses self's fields
expect_runtime_output "Impl method accesses receiver fields" "3" << 'EOF'
type point = { x: int, y: int }
trait sum_fields[a] {
  fn sum_fields(x: a) -> int
}
impl sum_fields for point {
  fn sum_fields(p: point) -> int {
    p.x + p.y
  }
}
let p: point = { x: 1, y: 2 }
puts(p.sum_fields())
EOF

# 7. Impl method with multiple parameters
expect_runtime_output "Impl method with multiple parameters" "true" << 'EOF'
trait eq[a] {
  fn eq(x: a, y: a) -> bool
}
type point = { x: int, y: int }
impl eq for point {
  fn eq(a: point, b: point) -> bool {
    a.x == b.x
  }
}
let p1: point = { x: 1, y: 2 }
let p2: point = { x: 1, y: 99 }
puts(p1.eq(p2))
EOF

# 8. Impl that satisfies a trait with multiple methods
expect_runtime_output "Impl with multiple methods" "hello
goodbye" << 'EOF'
trait dialog[a] {
  fn hello(x: a) -> string
  fn goodbye(x: a) -> string
}
impl dialog for int {
  fn hello(x: int) -> string { "hello" }
  fn goodbye(x: int) -> string { "goodbye" }
}
puts(1.hello())
puts(1.goodbye())
EOF

# 9. Impl method returning a different type than the receiver
expect_runtime_output "Impl method returns different type than receiver" "3" << 'EOF'
type pair = { a: int, b: int }
trait sum[a] {
  fn sum(x: a) -> int
}
impl sum for pair {
  fn sum(p: pair) -> int {
    p.a + p.b
  }
}
let p: pair = { a: 1, b: 2 }
puts(p.sum())
EOF

# 10. Trait method called on type that does NOT have impl (should fail)
expect_build "Trait method on type without impl fails" "No method 'describe' found for type" << 'EOF'
trait describe[a] {
  fn describe(x: a) -> string
}
impl describe for int {
  fn describe(x: int) -> string { "int" }
}
puts("hello".describe())
EOF

# 11. Impl satisfaction through supertrait chain
expect_runtime_output "Supertrait chain: eq -> ord, both satisfied" "0" << 'EOF'
trait eq[a] {
  fn eq(x: a, y: a) -> bool
}
trait ord[a]: eq {
  fn compare(x: a, y: a) -> int
}
type point = { x: int }
impl eq for point {
  fn eq(a: point, b: point) -> bool { a.x == b.x }
}
impl ord for point {
  fn compare(a: point, b: point) -> int { 0 }
}
let p: point = { x: 1 }
puts(p.compare(p))
EOF

echo ""
echo "-- GUARD / REJECTION TESTS --"

# 12. Duplicate impl for same (trait, type) pair (should be rejected)
expect_build "Duplicate impl for same trait and type is rejected" "Duplicate impl" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
impl show for int {
  fn show(x: int) -> string { "first" }
}
impl show for int {
  fn show(x: int) -> string { "second" }
}
puts(1)
EOF

# 13. Impl method that doesn't match trait method signature (wrong return type)
expect_build "Impl method with wrong return type is rejected" "__ANY_ERROR__" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
impl show for int {
  fn show(x: int) -> int { 42 }
}
puts(1)
EOF

# 14. Impl missing required method from trait
expect_build "Impl missing required method is rejected" "does not match trait signature" << 'EOF'
trait dialog[a] {
  fn hello(x: a) -> string
  fn goodbye(x: a) -> string
}
impl dialog for int {
  fn hello(x: int) -> string { "hi" }
}
puts(1)
EOF

# 15. Impl with extra method not in trait
expect_build "Impl with extra method not in trait is rejected" "does not match trait signature" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
impl show for int {
  fn show(x: int) -> string { "int" }
  fn extra(x: int) -> string { "bonus" }
}
puts(1)
EOF

# 16. Impl for field-only trait (should be rejected)
expect_build "Impl for field-only trait is rejected" "field-only and cannot have impl blocks" << 'EOF'
trait named {
  name: string
}
impl named for int {
  fn fake(x: int) -> string { "nope" }
}
puts(1)
EOF

# 17. Impl for undefined trait
expect_build "Impl for undefined trait is rejected" "Cannot implement undefined trait" << 'EOF'
impl nonexistent_trait for int {
  fn do_thing(x: int) -> int { x }
}
puts(1)
EOF

# 18. Impl method with wrong number of parameters
expect_build "Impl method with wrong param count is rejected" "__ANY_ERROR__" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
impl show for int {
  fn show(x: int, y: int) -> string { "int" }
}
puts(1)
EOF

# 19. Supertrait not satisfied should be rejected
expect_build "Impl for supertrait-requiring trait without supertrait impl is rejected" "supertrait" << 'EOF'
trait eq[a] {
  fn eq(x: a, y: a) -> bool
}
trait ord[a]: eq {
  fn compare(x: a, y: a) -> int
}
type foo = { x: int }
impl ord for foo {
  fn compare(a: foo, b: foo) -> int { 0 }
}
puts(1)
EOF

echo ""
echo "-- GENERIC IMPL EDGE CASES --"

# 20. Generic impl with single type param (basic)
expect_runtime_output "Generic impl with single type param" "list" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
impl show[b: show] for list[b] {
  fn show(x: list[b]) -> string { "list" }
}
puts([1, 2, 3].show())
EOF

# 21. Concrete impl overrides generic impl
expect_runtime_output "Concrete impl takes priority over generic impl" "concrete" << 'EOF'
trait label[a] {
  fn label(x: a) -> string
}
impl label[b: show] for list[b] {
  fn label(x: list[b]) -> string { "generic" }
}
impl label for list[int] {
  fn label(x: list[int]) -> string { "concrete" }
}
puts([1, 2, 3].label())
EOF

# 22. Generic impl constraint is enforced at use site
expect_build "Generic impl constraint enforced: element must satisfy constraint" "does not satisfy" << 'EOF'
trait named {
  name: string
}
trait show[a] {
  fn show(x: a) -> string
}
impl show[b: named] for list[b] {
  fn show(x: list[b]) -> string { "ok" }
}
puts([1, 2].show())
EOF

# 23. Generic impl with duplicate type param names
expect_build "Generic impl with duplicate type param names is rejected" "duplicate type parameter" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
impl show[b: show, b: show] for list[b] {
  fn show(x: list[b]) -> string { "ok" }
}
puts([1].show())
EOF

# 24. Generic impl with type param not used in target type
expect_build "Generic impl with unused type param is rejected" "not used in impl target type" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
impl show[b: show] for int {
  fn show(x: int) -> string { "int" }
}
puts(1)
EOF

# 25. Generic impl body calls helper function at concrete use site
expect_runtime_output "Generic impl body calls helper function" "2" << 'EOF'
trait size[a] {
  fn size(x: a) -> int
}
impl size[b: show] for list[b] {
  fn size(x: list[b]) -> int { len(x) }
}
puts([10, 20].size())
EOF

# 26. Generic impl constraint chain: elem must satisfy multi-constraint
expect_runtime_output "Generic impl with show+eq constraint" "ok" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
trait eq[a] {
  fn eq(x: a, y: a) -> bool
}
trait describe[a] {
  fn describe(x: a) -> string
}
impl describe[b: show + eq] for list[b] {
  fn describe(x: list[b]) -> string { "ok" }
}
puts([1, 2].describe())
EOF

echo ""
echo "-- TRICKY / CORNER CASE SCENARIOS --"

# 27. Impl for bool (non-int primitive)
expect_runtime_output "Impl for bool primitive" "yes" << 'EOF'
trait yesno[a] {
  fn yesno(x: a) -> string
}
impl yesno for bool {
  fn yesno(b: bool) -> string {
    if (b) { "yes" } else { "no" }
  }
}
puts(true.yesno())
EOF

# 28. Impl for string
expect_runtime_output "Impl for string primitive" "5" << 'EOF'
trait length[a] {
  fn length(x: a) -> int
}
impl length for string {
  fn length(s: string) -> int { len(s) }
}
puts("hello".length())
EOF

# 29. Method-only trait without type parameter (from existing tests, verify edge)
expect_runtime_output "Method-only trait without type param, multiple impls" "10
20" << 'EOF'
trait ping {
  fn ping(x: int) -> int
}
impl ping for int {
  fn ping(x: int) -> int { x * 10 }
}
puts(1.ping())
puts(2.ping())
EOF

# 30. Two different traits with same method name, only one impl (no ambiguity)
expect_runtime_output "Two traits same method name, only one impl means no ambiguity" "alpha" << 'EOF'
trait alpha_trait[a] {
  fn tag(x: a) -> string
}
trait beta_trait[a] {
  fn tag(x: a) -> string
}
impl alpha_trait for int {
  fn tag(x: int) -> string { "alpha" }
}
puts(1.tag())
EOF

# 31. Two different traits with same method name, both impls (should be ambiguous)
expect_build "Two traits same method name, both impls causes ambiguous dispatch" "Ambiguous" << 'EOF'
trait alpha_trait[a] {
  fn tag(x: a) -> string
}
trait beta_trait[a] {
  fn tag(x: a) -> string
}
impl alpha_trait for int {
  fn tag(x: int) -> string { "alpha" }
}
impl beta_trait for int {
  fn tag(x: int) -> string { "beta" }
}
puts(1.tag())
EOF

# 32. Impl method missing return type annotation
expect_build "Impl method missing return type annotation is rejected" "missing a return type annotation" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
impl show for int {
  fn show(x: int) { "int" }
}
puts(1)
EOF

# 33. Impl method missing parameter type annotation
expect_build "Impl method missing param type annotation is rejected" "missing a type annotation" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
impl show for int {
  fn show(x) -> string { "int" }
}
puts(1)
EOF

# 34. Generic impl drives == operator for list
expect_runtime_output "Generic eq impl drives == for list[int]" "true" << 'EOF'
impl eq[b: eq] for list[b] {
  fn eq(x: list[b], y: list[b]) -> bool {
    len(x) == len(y)
  }
}
puts([1, 2] == [3, 4])
EOF

# 35. Constrained generic function calls method from generic impl
expect_runtime_output "Constrained generic fn calls trait method from generic impl" "ok" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
impl show[b: show] for list[b] {
  fn show(x: list[b]) -> string { "ok" }
}
let render = fn[t: show](x: t) -> string {
  x.show()
};
puts(render([1, 2]))
EOF

suite_end
