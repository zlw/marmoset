#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

suite_begin "TYPECHECK & CODEGEN INTEGRATION TESTS - TRAITS"
echo "-- PHASE 4.3: TRAIT SYSTEM --"

test_case "Basic trait definition" \
    'trait show[a] {
       fn show(x: a) -> string
     }
     42' \
    "true"

test_case "Trait with multiple methods" \
    'trait eq[a] {
       fn eq(x: a, y: a) -> bool
       fn ne(x: a, y: a) -> bool
     }
     42' \
    "true"

expect_build "Duplicate trait definition is rejected in one program" "Duplicate trait definition: ping" << 'EOF'
trait ping[a] {
  fn ping(x: a) -> int
}
trait ping[a] {
  fn pong(x: a) -> int
}
puts(1)
EOF

expect_runtime_output "Method-only trait without type parameter" "42" << 'EOF'
trait ping {
  fn ping(x: int) -> int
}
impl ping for int {
  fn ping(x: int) -> int {
    x
  }
}
puts(42.ping())
EOF

expect_runtime_output "Field-only trait constraint accepts matching record shape" "alice" << 'EOF'
trait named {
  name: string
}
let get_name = fn[t: named](x: t) -> string {
  x.name
};
let person = { name: "alice", age: 42 }
puts(get_name(person))
EOF

expect_build "Field-only trait constraint rejects missing required field" "missing-field" << 'EOF'
trait named {
  name: string
}
let get_name = fn[t: named](x: t) -> string {
  "ok"
};
get_name({ age: 42 })
EOF

expect_build "Mixed field+method constraints still enforce method trait obligations" "does not implement trait shown" << 'EOF'
trait named {
  name: string
}
trait shown[a] {
  fn show(x: a) -> string
}
let get_name = fn[t: named + shown](x: t) -> string {
  x.name
};
let p = { name: "alice" }
puts(get_name(p))
EOF

expect_build "Constrained generic cannot access fields absent from constraints" "not guaranteed by constraints" << 'EOF'
trait named {
  name: string
}
let get_age = fn[t: named](x: t) -> int {
  x.age
};
let p = { name: "alice", age: 42 }
puts(get_age(p))
EOF

expect_runtime_output "Mixed field+method constraints succeed with satisfying impl" "alice" << 'EOF'
type person = { name: string, age: int }
trait named {
  name: string
}
trait shown[a] {
  fn show(x: a) -> string
}
impl shown for person {
  fn show(x: person) -> string {
    x.name
  }
}
let get_name = fn[t: named + shown](x: t) -> string {
  x.name
};
let p: person = { name: "alice", age: 42 }
puts(get_name(p))
EOF

expect_runtime_output "Method trait impl can satisfy field-only supertrait structurally" "alice" << 'EOF'
type person = { name: string, age: int }
trait named {
  name: string
}
trait shown[a]: named {
  fn show(x: a) -> string
}
impl shown for person {
  fn show(x: person) -> string {
    x.name
  }
}
let p: person = { name: "alice", age: 42 }
puts(p.show())
EOF

expect_runtime_output "Field-only trait as type projects records to required shape" "alice" << 'EOF'
trait named {
  name: string
}
let x: named = { name: "alice", age: 42 }
puts(x.name)
EOF

expect_runtime_output "Field-only trait object arrays can be built from annotated values" "acme" << 'EOF'
trait named {
  name: string
}
let person: named = { name: "alice", age: 42 }
let company: named = { name: "acme", employees: 10 }
let xs: list[named] = [person, company]
puts(xs[1].name)
EOF

expect_runtime_output "Field-only trait object includes inherited fields from supertraits" "43" << 'EOF'
trait named {
  name: string
}
trait aged {
  age: int
}
trait person: named + aged {
}
let p: person = { name: "alice", age: 43, active: true }
puts(p.age)
EOF

expect_build "Method trait as type shows dedicated v1 diagnostic" "method and mixed trait objects are not supported in this phase" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
let x: show = 1
EOF

expect_build "Mixed trait as type is rejected" "method and mixed trait objects are not supported in this phase" << 'EOF'
trait named_show[a] {
  name: string
  fn show(x: a) -> string
}
let x: named_show = { name: "alice" }
EOF

expect_build "Generic field-only trait as type is rejected" "generic field-only trait objects are not supported in this phase" << 'EOF'
trait tagged[a] {
  tag: a
}
let x: tagged = { tag: 1 }
EOF

expect_build "Trait with method supertrait is rejected in type position" "method and mixed trait objects are not supported in this phase" << 'EOF'
trait named {
  name: string
}
trait show[a] {
  fn show(x: a) -> string
}
trait named_show: named + show {
}
let x: named_show = { name: "alice" }
EOF

expect_build "Generic field-only supertrait in trait object is rejected explicitly" "generic field-only supertrait 'tagged' is not supported" << 'EOF'
trait tagged[a] {
  tag: a
}
trait tagged_like: tagged {
}
let x: tagged_like = { tag: 1 }
EOF

expect_build "Field-only trait conflicting supertrait fields are rejected" "conflicting types across supertraits" << 'EOF'
trait has_int {
  x: int
}
trait has_string {
  x: string
}
trait bad: has_int + has_string {
}
let x: bad = { x: 1 }
EOF

expect_build "Impl block for field-only trait is rejected" "field-only and cannot have impl blocks" << 'EOF'
trait named {
  name: string
}
impl named for int {
}
puts(1)
EOF

expect_build "Field-only trait object rejects access to non-projected field" "Record field 'age' not found in type" << 'EOF'
trait named {
  name: string
}
let x: named = { name: "alice", age: 1 }
puts(x.age)
EOF

expect_build "Type-alias to field-only trait object still hides non-projected fields" "Record field 'age' not found in type" << 'EOF'
trait named {
  name: string
}
type named_alias = named
let x: named_alias = { name: "alice", age: 1 }
puts(x.age)
EOF

expect_build "Field-only trait object in list still hides non-projected fields" "Record field 'age' not found in type" << 'EOF'
trait named {
  name: string
}
let a: named = { name: "alice", age: 1 }
let b: named = { name: "bob", age: 2 }
let xs: list[named] = [a, b]
puts(xs[0].age)
EOF

expect_build "Field-only trait object returned from annotated function still hides non-projected fields" "Record field 'age' not found in type" << 'EOF'
trait named {
  name: string
}
let mk = fn(age: int) -> named {
  { name: "alice", age: age }
}
let x = mk(1)
puts(x.age)
EOF

expect_build "Field-only trait object does not allow method dispatch" "No method 'show' found for type" << 'EOF'
trait named {
  name: string
}
let x: named = { name: "alice", age: 1 }
puts(x.show())
EOF

test_case "Trait with supertraits" \
    'trait eq[a] {
       fn eq(x: a, y: a) -> bool
     }
     trait ord[a]: eq {
       fn compare(x: a, y: a) -> int
     }
     42' \
    "true"

test_case "Basic impl for primitive" \
    'trait show[a] {
       fn show(x: a) -> string
     }
     impl show for int {
       fn show(x: int) -> string {
         "int"
       }
     }
     42' \
    "true"

test_case "Impl with multiple methods" \
    'trait eq[a] {
       fn eq(x: a, y: a) -> bool
       fn ne(x: a, y: a) -> bool
     }
     impl eq for int {
       fn eq(x: int, y: int) -> bool {
         true
       }
       fn ne(x: int, y: int) -> bool {
         false
       }
     }
     42' \
    "true"

test_case "Multiple impls for different types" \
    'trait show[a] {
       fn show(x: a) -> string
     }
     impl show for int {
       fn show(x: int) -> string {
         "int"
       }
     }
     impl show for bool {
       fn show(x: bool) -> string {
         "bool"
       }
     }
     42' \
    "true"

test_case "Duplicate impl for same trait and type fails" \
    'trait show[a] {
       fn show(x: a) -> string
     }
     impl show for int {
       fn show(x: int) -> string {
         "first"
       }
     }
     impl show for int {
       fn show(x: int) -> string {
         "second"
       }
     }
     42' \
    "false" \
    "Duplicate impl for trait"

expect_build "Ambiguous method dispatch reports conflicting impl sites" "impl sites:" << 'EOF'
trait render_a[a] {
  fn render(x: a) -> string
}
trait render_b[a] {
  fn render(x: a) -> string
}
impl render_a for int {
  fn render(x: int) -> string { "a" }
}
impl render_b for int {
  fn render(x: int) -> string { "b" }
}
let x = 1
puts(x.render())
EOF

test_case "Derive single trait for primitive" \
    'trait eq[a] {
       fn eq(x: a, y: a) -> bool
     }
     derive eq for int;
     42' \
    "true"

test_case "Derive multiple traits" \
    'trait eq[a] {
       fn eq(x: a, y: a) -> bool
     }
     trait show[a] {
       fn show(x: a) -> string
     }
     derive eq, show for int;
     42' \
    "true"

test_case "Derive for multiple types" \
    'trait show[a] {
       fn show(x: a) -> string
     }
     derive show for int;
     derive show for bool;
     derive show for string;
     42' \
    "true"

test_case "Mixed manual and derived impls" \
    'trait show[a] {
       fn show(x: a) -> string
     }
     impl show for int {
       fn show(x: int) -> string {
         "manual"
       }
     }
     derive show for bool;
     42' \
    "true"

expect_runtime_output "Generic impl method resolves for concrete element type" "ok" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
impl show[b: show] for list[b] {
  fn show(x: list[b]) -> string {
    "ok"
  }
}
puts([1, 2, 3].show())
EOF

expect_build "Generic impl constraints are enforced at use sites" "does not satisfy trait named" << 'EOF'
trait named {
  name: string
}
trait show[a] {
  fn show(x: a) -> string
}
impl show[b: named] for list[b] {
  fn show(x: list[b]) -> string {
    "ok"
  }
}
puts([1, 2].show())
EOF

expect_runtime_output "Concrete impl overrides generic impl for same trait/type" "false
true" << 'EOF'
trait eq[a] {
  fn eq(x: a, y: a) -> bool
}
impl eq[b: eq] for list[b] {
  fn eq(x: list[b], y: list[b]) -> bool {
    true
  }
}
impl eq for list[int] {
  fn eq(x: list[int], y: list[int]) -> bool {
    false
  }
}
puts([1] == [2])
puts(["a"] == ["b"])
EOF

expect_runtime_output "Generic impl method body can call helper functions at concrete use sites" "ok" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
let render_len = fn(n: int) -> string {
  "ok"
}
impl show[b: show] for list[b] {
  fn show(x: list[b]) -> string {
    render_len(len(x))
  }
}
puts([1, 2].show())
EOF

expect_runtime_output "Generic eq impl drives == operator for concrete list type" "true" << 'EOF'
impl eq[b: eq] for list[b] {
  fn eq(x: list[b], y: list[b]) -> bool {
    len(x) == len(y)
  }
}
puts([1, 2] == [3, 4])
EOF

echo "-- PHASE 4.3: OPERATOR TRAIT OBLIGATIONS --"

expect_build "Equality requires eq trait (functions should fail)" "missing-impl" << 'EOF'
let same = fn[a](x: a, y: a) -> bool {
  x == y
};
let id1 = fn(n: int) -> int { n };
let id2 = fn(n: int) -> int { n + 1 };
same(id1, id2)
EOF

expect_build "Ordering requires ord trait (arrays should fail)" "missing-impl" << 'EOF'
let less = fn[a](x: a, y: a) -> bool {
  x < y
};
less([1], [2])
EOF

expect_build "Arithmetic requires num trait (bool should fail)" "missing-impl" << 'EOF'
let add = fn[a](x: a, y: a) {
  x + y
};
add(true, false)
EOF

expect_runtime_output "String concatenation still works with builtin operator lowering" "ab" << 'EOF'
puts("a" + "b")
EOF

expect_runtime_output "Bool ordering operators lower through ord trait helpers" "false
true
true" << 'EOF'
puts(true < false)
puts(false <= true)
puts(true >= true)
EOF

expect_runtime_output "Custom eq impl drives == operator on non-primitive types" "false" << 'EOF'
type point = { x: int }
impl eq for point {
  fn eq(x: point, y: point) -> bool {
    false
  }
}
let p1: point = { x: 1 }
let p2: point = { x: 1 }
puts(p1 == p2)
EOF

expect_runtime_output "Custom num impl drives + operator on non-primitive types" "13" << 'EOF'
enum boxed {
  val(int)
}
impl num for boxed {
  fn add(x: boxed, y: boxed) -> boxed { x }
  fn sub(x: boxed, y: boxed) -> boxed { x }
  fn mul(x: boxed, y: boxed) -> boxed { x }
  fn div(x: boxed, y: boxed) -> boxed { x }
}
let a = boxed.val(13)
let b = boxed.val(2)
match (a + b) {
  boxed.val(n): puts(n)
}
EOF

expect_runtime_output "Custom neg impl drives unary - operator on non-primitive types" "5" << 'EOF'
enum boxed {
  val(int)
}
impl neg for boxed {
  fn neg(x: boxed) -> boxed { x }
}
let a = boxed.val(5)
match (-a) {
  boxed.val(n): puts(n)
}
EOF

echo "-- PHASE 4.3: TRAIT METHOD CALLS --"

expect_runtime_output "Basic trait method call on int" "42" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
impl show for int {
  fn show(x: int) -> string {
    "42"
  }
}
let x = 42
puts(x.show())
EOF

expect_runtime_output "Method call with parameter" "true" << 'EOF'
trait eq[a] {
  fn eq(x: a, y: a) -> bool
}
impl eq for int {
  fn eq(x: int, y: int) -> bool {
    x == y
  }
}
let a = 42
let b = 42
puts(a.eq(b))
EOF

expect_runtime_output "Impl method body can return direct record literal" "5" << 'EOF'
type box = { value: int }
trait wrap[a] {
  fn wrap(x: a) -> box
}
impl wrap for int {
  fn wrap(x: int) -> box { { value: x } }
}
let b = 5.wrap()
puts(b.value)
EOF

expect_runtime_output "Method call on string" "hello" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
impl show for string {
  fn show(x: string) -> string {
    x
  }
}
let s = "hello"
puts(s.show())
EOF

expect_runtime_output "Multiple method calls in sequence" "number
number" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
impl show for int {
  fn show(x: int) -> string {
    "number"
  }
}
let x = 1
let y = 2
puts(x.show())
puts(y.show())
EOF

expect_runtime_output "Method call result used in expression" "42" << 'EOF'
trait double[a] {
  fn double(x: a) -> int
}
impl double for int {
  fn double(x: int) -> int {
    x + x
  }
}
let n = 21
let result = n.double()
puts(result)
EOF

run_emit_go_not_contains_from_stdin "Constrained generic method call does not emit any-mangled specialization" "_any" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
impl show for int {
  fn show(x: int) -> string {
    "int"
  }
}
let id = fn[t: show](x: t) -> string {
  x.show()
};
puts(id(1))
EOF

echo "-- BUILTIN TRAITS --"

expect_build "Builtin show trait exists" << 'EOF'
impl show for int {
  fn show(x: int) -> string {
    "test"
  }
}
42
EOF

expect_build "Builtin eq trait exists" << 'EOF'
impl eq for int {
  fn eq(x: int, y: int) -> bool {
    true
  }
}
42
EOF

expect_build "Builtin ord trait exists" << 'EOF'
enum ordering { less equal greater }
impl ord for int {
  fn compare(x: int, y: int) -> ordering {
    ordering.equal
  }
}
42
EOF

echo "-- BUILTIN TRAIT IMPLS FOR PRIMITIVES --"

expect_runtime_output "int implements show (builtin)" "42" << 'EOF'
let x = 42
let s = x.show()
puts(s)
EOF

expect_runtime_output "int implements eq (builtin)" "true" << 'EOF'
let a = 42
let b = 42
let result = a.eq(b)
puts(result)
EOF

expect_runtime_output "string implements show (builtin)" "hello" << 'EOF'
let s = "hello"
puts(s.show())
EOF

expect_runtime_output "bool implements show (builtin)" "true" << 'EOF'
let b = true
puts(b.show())
EOF

echo ""
echo "-- TRAIT SOLVER --"

expect_runtime_output "Trait solver: int implements show" "42" << 'EOF'
let check = fn[a: show](x: a) -> string {
  x.show()
}
let result = check(42)
puts(result)
EOF

expect_build "Trait solver: array lacks show (should fail typecheck)" "does not implement trait" << 'EOF'
let check = fn[a: show](x: a) -> string {
  x.show()
}
let arr = [1, 2, 3]
let result = check(arr)
puts(result)
EOF

expect_runtime_output "Trait solver: multiple constraints work" "42" << 'EOF'
let show_if_equal = fn[a: show + eq](x: a, y: a) -> string {
  if (x.eq(y)) {
    x.show()
  } else {
    "different"
  }
}
let result = show_if_equal(42, 42)
puts(result)
EOF

expect_runtime_output "Trait solver: multiple constraints with different values" "different" << 'EOF'
let show_if_equal = fn[a: show + eq](x: a, y: a) -> string {
  if (x.eq(y)) {
    x.show()
  } else {
    "different"
  }
}
let result = show_if_equal(42, 99)
puts(result)
EOF

expect_build "Supertrait: impl ord requires eq for same type" "supertrait" << 'EOF'
trait eq[a] {
  fn eq(x: a, y: a) -> bool
}
trait ord[a]: eq {
  fn compare(x: a, y: a) -> int
}
type point = { x: int }
impl ord for point {
  fn compare(x: point, y: point) -> int {
    0
  }
}
let p: point = { x: 1 }
puts(p.compare(p))
EOF

expect_runtime_output "Supertrait: methods from supertrait available through ord constraint" "true" << 'EOF'
trait eq[a] {
  fn eq(x: a, y: a) -> bool
}
trait ord[a]: eq {
  fn compare(x: a, y: a) -> int
}
type point = { x: int }
impl eq for point {
  fn eq(x: point, y: point) -> bool {
    x.x == y.x
  }
}
impl ord for point {
  fn compare(x: point, y: point) -> int {
    0
  }
}
let eq_via_ord = fn[a: ord](x: a, y: a) -> bool {
  x.eq(y)
}
let p1: point = { x: 1 }
let p2: point = { x: 1 }
puts(eq_via_ord(p1, p2))
EOF

expect_build "Supertrait: ord constraint also requires eq transitively" "supertrait" << 'EOF'
trait eq[a] {
  fn eq(x: a, y: a) -> bool
}
trait ord[a]: eq {
  fn compare(x: a, y: a) -> int
}
type point = { x: int }
impl ord for point {
  fn compare(x: point, y: point) -> int {
    0
  }
}
let compare_self = fn[a: ord](x: a) -> int {
  x.compare(x)
}
let p: point = { x: 1 }
puts(compare_self(p))
EOF


suite_end
