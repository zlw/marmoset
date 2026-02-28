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

expect_runtime_output "Inherent method call on int" "4" << 'EOF'
impl int {
  fn twice(x: int) -> int {
    x + x
  }
}
puts(2.twice())
EOF

expect_runtime_output "Inherent method call on list[int] receiver" "1" << 'EOF'
impl list[int] {
  fn size(xs: list[int]) -> int {
    1
  }
}
puts([1, 2, 3].size())
EOF

expect_build "Duplicate inherent method for same type is rejected" "Duplicate inherent method 'ping'" << 'EOF'
impl int {
  fn ping(x: int) -> int {
    x
  }
}
impl int {
  fn ping(x: int) -> int {
    x
  }
}
puts(1)
EOF

expect_build "Inherent method receiver type must match impl target type" "does not match impl target type" << 'EOF'
type point = { x: int }
impl point {
  fn bad(x: int) -> int {
    x
  }
}
puts(1)
EOF

expect_build "Inherent method colliding with trait method is rejected" "collides with trait method" << 'EOF'
impl int {
  fn show(x: int) -> string {
    "inherent"
  }
}
puts(1)
EOF

expect_build "Inherent methods do not satisfy trait constraints" "does not implement trait show" << 'EOF'
type point = { x: int }
impl point {
  fn show(p: point) -> string {
    "point"
  }
}
let f = fn[t: show](x: t) -> string {
  x.show()
};
let p: point = { x: 1 }
puts(f(p))
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

test_case "Impl with multiple methods compiles (smoke)" \
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

expect_runtime_output "Generic impl constraints do not overflow with type param name a" "ok" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
impl show[a: show] for list[a] {
  fn show(x: list[a]) -> string {
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



# -------------------------------------------------------------------
# Imported From 04_traits_edge_cases.sh
# -------------------------------------------------------------------

########################################################################
# SECTION 1: FIELD-ONLY TRAIT AS PARAMETER TYPE
########################################################################
echo "-- FIELD-ONLY TRAIT AS PARAMETER TYPE --"

expect_runtime_output "Field-only trait as parameter type annotation" "alice" << 'EOF'
trait named {
  name: string
}
let greet = fn(x: named) -> string {
  x.name
}
puts(greet({ name: "alice", age: 30 }))
EOF

expect_runtime_output "Multiple trait-typed parameters in same function" "alice bob" << 'EOF'
trait named {
  name: string
}
let greet_both = fn(a: named, b: named) -> string {
  a.name + " " + b.name
}
puts(greet_both({ name: "alice", age: 30 }, { name: "bob", role: "admin" }))
EOF

expect_runtime_output "Subsumption: record with extra fields accepted as trait-typed param" "world" << 'EOF'
trait greeter {
  greeting: string
}
let say = fn(g: greeter) -> string {
  g.greeting
}
puts(say({ greeting: "world", volume: 11, language: "en" }))
EOF

expect_build "Param trait annotation: record missing required field is rejected" "__ANY_ERROR__" << 'EOF'
trait named {
  name: string
}
let greet = fn(x: named) -> string {
  x.name
}
puts(greet({ age: 30 }))
EOF

########################################################################
# SECTION 2: FIELD-ONLY TRAIT AS RETURN TYPE (PROJECTION)
########################################################################
echo ""
echo "-- FIELD-ONLY TRAIT AS RETURN TYPE (PROJECTION) --"

# Regression: codegen projects the wider record to the trait return shape.
expect_runtime_output "Fn returning wider record with trait return type" "alice" << 'EOF'
trait named {
  name: string
}
let mk_named = fn(n: string, a: int) -> named {
  { name: n, age: a }
}
let x = mk_named("alice", 30)
puts(x.name)
EOF

expect_build "Return type projection hides non-trait fields at typecheck" "Record field 'age' not found in type" << 'EOF'
trait named {
  name: string
}
let mk_named = fn(n: string, a: int) -> named {
  { name: n, age: a }
}
let x = mk_named("alice", 30)
puts(x.age)
EOF

# Return type with exact field match (no extra fields) should work
expect_runtime_output "Trait return from function with exact field match" "alice" << 'EOF'
trait named {
  name: string
}
let mk = fn() -> named {
  { name: "alice" }
}
let x = mk()
puts(x.name)
EOF

########################################################################
# SECTION 3: TRAIT OBJECT IN IF-EXPRESSIONS
########################################################################
echo ""
echo "-- TRAIT OBJECT IN IF-EXPRESSIONS --"

# Regression: if branches with wider records project correctly to trait return type.
expect_runtime_output "If branches with extra fields and trait return" "young" << 'EOF'
trait named {
  name: string
}
let describe = fn(age: int) -> named {
  if (age < 30) {
    { name: "young", category: "a" }
  } else {
    { name: "old", category: "b" }
  }
}
let x = describe(25)
puts(x.name)
EOF

# Typecheck correctly rejects access to non-projected field from if-expression return
expect_build "If-expression return projection hides branch-specific extra fields" "Record field 'extra1' not found in type" << 'EOF'
trait named {
  name: string
}
let pick = fn(flag: bool) -> named {
  if (flag) {
    { name: "branch_a", extra1: 1 }
  } else {
    { name: "branch_b", extra2: "two" }
  }
}
let x = pick(true)
puts(x.extra1)
EOF

########################################################################
# SECTION 4: TRAIT WITH MULTIPLE FIELDS
########################################################################
echo ""
echo "-- TRAIT WITH MULTIPLE FIELDS --"

expect_runtime_output "Multi-field trait object exposes all declared fields" "alice 30" << 'EOF'
trait person_like {
  name: string
  age: int
}
let x: person_like = { name: "alice", age: 30, active: true }
puts(x.name + " " + x.age.show())
EOF

expect_build "Multi-field trait object hides non-declared fields" "Record field 'active' not found in type" << 'EOF'
trait person_like {
  name: string
  age: int
}
let x: person_like = { name: "alice", age: 30, active: true }
puts(x.active)
EOF

########################################################################
# SECTION 5: METHOD-ONLY AND MIXED TRAITS AS TYPES (SHOULD FAIL)
########################################################################
echo ""
echo "-- METHOD-ONLY AND MIXED TRAITS AS TYPES (SHOULD FAIL) --"

expect_build "Method-only trait as variable type is rejected" "method and mixed trait objects are not supported in this phase" << 'EOF'
trait renderable[a] {
  fn render(x: a) -> string
}
let x: renderable = 42
EOF

# BUG-PARAM-TYPE-TRAIT-KIND: The typechecker does NOT validate trait kind in
# function parameter annotation position. This should produce a typecheck error
# but leaks through to codegen.
expect_build "BUG-PARAM-TYPE-TRAIT-KIND: method-only trait as param type should fail" "method and mixed trait objects are not supported in this phase" << 'EOF'
trait renderable[a] {
  fn render(x: a) -> string
}
let f = fn(x: renderable) -> string {
  "nope"
}
puts(f(42))
EOF

expect_build "Mixed trait (fields + methods) as type is rejected" "method and mixed trait objects are not supported in this phase" << 'EOF'
trait displayable[a] {
  name: string
  fn display(x: a) -> string
}
let x: displayable = { name: "alice" }
EOF

########################################################################
# SECTION 6: GENERIC FIELD-ONLY TRAIT AS TYPE (SHOULD FAIL)
########################################################################
echo ""
echo "-- GENERIC FIELD-ONLY TRAIT AS TYPE (SHOULD FAIL) --"

expect_build "Generic field-only trait as variable type is rejected" "generic field-only trait objects are not supported in this phase" << 'EOF'
trait container[a] {
  value: a
}
let x: container = { value: 42 }
EOF

# BUG-PARAM-TYPE-TRAIT-KIND: Same gap -- generic field-only trait leaks through
# as parameter type annotation without being caught.
expect_build "BUG-PARAM-TYPE-TRAIT-KIND: generic field-only trait as param type should fail" "generic field-only trait objects are not supported in this phase" << 'EOF'
trait container[a] {
  value: a
}
let f = fn(x: container) -> int {
  1
}
puts(f({ value: 42 }))
EOF

########################################################################
# SECTION 7: IMPL FOR FIELD-ONLY TRAIT (SHOULD FAIL)
########################################################################
echo ""
echo "-- IMPL FOR FIELD-ONLY TRAIT (SHOULD FAIL) --"

expect_build "Impl block for field-only trait with alias type is rejected" "field-only and cannot have impl blocks" << 'EOF'
trait named {
  name: string
}
type person = { name: string, age: int }
impl named for person {
}
puts(1)
EOF

########################################################################
# SECTION 8: SUPERTRAIT CHAINS
########################################################################
echo ""
echo "-- SUPERTRAIT CHAINS --"

expect_runtime_output "Supertrait chain inherits parent fields" "alice" << 'EOF'
trait named {
  name: string
}
trait described: named {
  description: string
}
let x: described = { name: "alice", description: "a person", hobby: "chess" }
puts(x.name)
EOF

expect_runtime_output "Supertrait chain: access child's own fields" "a person" << 'EOF'
trait named {
  name: string
}
trait described: named {
  description: string
}
let x: described = { name: "alice", description: "a person", hobby: "chess" }
puts(x.description)
EOF

expect_build "Supertrait chain hides non-declared fields from all levels" "Record field 'hobby' not found in type" << 'EOF'
trait named {
  name: string
}
trait described: named {
  description: string
}
let x: described = { name: "alice", description: "a person", hobby: "chess" }
puts(x.hobby)
EOF

expect_runtime_output "Three-level supertrait chain exposes all ancestor fields" "alice 30 active" << 'EOF'
trait named {
  name: string
}
trait aged: named {
  age: int
}
trait profiled: aged {
  status: string
}
let x: profiled = { name: "alice", age: 30, status: "active", secret: true }
puts(x.name + " " + x.age.show() + " " + x.status)
EOF

########################################################################
# SECTION 9: TRAIT OBJECT PASSED THROUGH FUNCTION LAYERS
########################################################################
echo ""
echo "-- TRAIT OBJECT PASSED THROUGH FUNCTION LAYERS --"

expect_runtime_output "Trait object passed through two function layers" "alice" << 'EOF'
trait named {
  name: string
}
let inner = fn(x: named) -> string {
  x.name
}
let outer = fn(x: named) -> string {
  inner(x)
}
puts(outer({ name: "alice", age: 30 }))
EOF

########################################################################
# SECTION 10: FIELD ACCESS ON TRAIT OBJECT
########################################################################
echo ""
echo "-- FIELD ACCESS ON TRAIT OBJECT --"

expect_runtime_output "Function accessing projected field on trait object" "hello" << 'EOF'
trait greetable {
  greeting: string
}
let get_greeting = fn(g: greetable) -> string {
  g.greeting
}
puts(get_greeting({ greeting: "hello", id: 1 }))
EOF

expect_build "Function cannot access non-projected field on trait param" "Record field 'id' not found in type" << 'EOF'
trait greetable {
  greeting: string
}
let get_id = fn(g: greetable) -> int {
  g.id
}
puts(get_id({ greeting: "hello", id: 1 }))
EOF

########################################################################
# SECTION 11: TRAIT OBJECT IN RECORD FIELD
########################################################################
echo ""
echo "-- TRAIT OBJECT IN RECORD FIELD --"

expect_runtime_output "Trait object stored in a record field via annotation" "bob" << 'EOF'
trait named {
  name: string
}
let person: named = { name: "bob", age: 25 }
let wrapper = { inner: person }
puts(wrapper.inner.name)
EOF

########################################################################
# SECTION 12: TRAIT OBJECT WITH NORMAL RECORD OPS
########################################################################
echo ""
echo "-- TRAIT OBJECT WITH NORMAL RECORD OPS --"

expect_runtime_output "Trait object and normal record operations in same function scope" "alice 42" << 'EOF'
trait named {
  name: string
}
let process = fn(x: named, extra: int) -> string {
  let n = x.name
  let plain = { value: extra }
  n + " " + plain.value.show()
}
puts(process({ name: "alice", age: 30 }, 42))
EOF

########################################################################
# SECTION 13: EMPTY TRAIT (NO FIELDS, NO METHODS)
########################################################################
echo ""
echo "-- EMPTY TRAIT (NO FIELDS, NO METHODS) --"

# BUG-EMPTY-TRAIT-KIND: Empty traits are classified as MethodOnly by trait_kind
# because the (has_fields=false, has_methods=false) case falls through to the
# else branch which returns MethodOnly. They should be FieldOnly.
expect_build "BUG-EMPTY-TRAIT-KIND: empty trait rejected as MethodOnly (should be FieldOnly)" "method and mixed trait objects are not supported" << 'EOF'
trait marker {
}
let x: marker = { anything: 1, goes: "here" }
puts("ok")
EOF

########################################################################
# SECTION 14: OVERLAPPING FIELD NAMES IN SEPARATE TRAITS
########################################################################
echo ""
echo "-- OVERLAPPING FIELD NAMES IN SEPARATE TRAITS --"

expect_runtime_output "Two traits with same field name (same type) used separately" "alice bob" << 'EOF'
trait named {
  name: string
}
trait labeled {
  name: string
}
let a: named = { name: "alice", x: 1 }
let b: labeled = { name: "bob", y: 2 }
puts(a.name + " " + b.name)
EOF

########################################################################
# SECTION 15: CONFLICTING SUPERTRAIT FIELDS
########################################################################
echo ""
echo "-- CONFLICTING SUPERTRAIT FIELDS --"

expect_build "Supertrait chain with conflicting field types is rejected" "conflicting types across supertraits" << 'EOF'
trait has_x_int {
  x: int
}
trait has_x_string {
  x: string
}
trait bad_combo: has_x_int + has_x_string {
}
let x: bad_combo = { x: 1 }
EOF

########################################################################
# SECTION 16: TRAIT OBJECT IN LIST OPERATIONS
########################################################################
echo ""
echo "-- TRAIT OBJECT IN LIST OPERATIONS --"

expect_runtime_output "Heterogeneous records in trait-typed list" "alice bob" << 'EOF'
trait named {
  name: string
}
let a: named = { name: "alice", role: "admin" }
let b: named = { name: "bob", age: 30 }
let xs: list[named] = [a, b]
puts(xs[0].name + " " + xs[1].name)
EOF

expect_build "List of trait objects hides non-projected fields on element access" "Record field 'role' not found in type" << 'EOF'
trait named {
  name: string
}
let a: named = { name: "alice", role: "admin" }
let xs: list[named] = [a]
puts(xs[0].role)
EOF

########################################################################
# SECTION 17: TRAIT WITH METHOD SUPERTRAIT (SHOULD FAIL AS TYPE)
########################################################################
echo ""
echo "-- TRAIT WITH METHOD SUPERTRAIT (SHOULD FAIL AS TYPE) --"

expect_build "Field-only trait with method supertrait is rejected as type" "method and mixed trait objects are not supported in this phase" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
trait describable: show {
  label: string
}
let x: describable = { label: "hi" }
EOF

########################################################################
# SECTION 18: TRAIT OBJECT DOES NOT ALLOW METHOD DISPATCH
########################################################################
echo ""
echo "-- TRAIT OBJECT DOES NOT ALLOW METHOD DISPATCH --"

expect_build "Field-only trait object does not support method dispatch" "No method 'show' found for type" << 'EOF'
trait named {
  name: string
}
let x: named = { name: "alice" }
puts(x.show())
EOF

########################################################################
# SECTION 19: TYPE ALIAS TO TRAIT OBJECT
########################################################################
echo ""
echo "-- TYPE ALIAS TO TRAIT OBJECT --"

expect_runtime_output "Type alias to field-only trait exposes trait fields" "alice" << 'EOF'
trait named {
  name: string
}
type nameable = named
let x: nameable = { name: "alice", age: 30 }
puts(x.name)
EOF

expect_build "Type alias to field-only trait hides non-trait fields" "Record field 'age' not found in type" << 'EOF'
trait named {
  name: string
}
type nameable = named
let x: nameable = { name: "alice", age: 30 }
puts(x.age)
EOF

########################################################################
# SECTION 20: GENERIC FIELD-ONLY SUPERTRAIT (SHOULD FAIL)
########################################################################
echo ""
echo "-- GENERIC FIELD-ONLY SUPERTRAIT (SHOULD FAIL) --"

expect_build "Non-generic trait with generic field-only supertrait is rejected" "generic field-only supertrait" << 'EOF'
trait boxed[a] {
  value: a
}
trait labeled_box: boxed {
  label: string
}
let x: labeled_box = { value: 42, label: "box" }
EOF

########################################################################
# SECTION 21: DIAMOND SUPERTRAIT STRUCTURE
########################################################################
echo ""
echo "-- DIAMOND SUPERTRAIT STRUCTURE --"

expect_runtime_output "Diamond supertrait inherits shared field once" "alice" << 'EOF'
trait named {
  name: string
}
trait worker: named {
  job: string
}
trait resident: named {
  city: string
}
trait citizen: worker + resident {
}
let x: citizen = { name: "alice", job: "eng", city: "sf", extra: true }
puts(x.name)
EOF

expect_runtime_output "Diamond supertrait exposes all inherited fields" "eng sf" << 'EOF'
trait named {
  name: string
}
trait worker: named {
  job: string
}
trait resident: named {
  city: string
}
trait citizen: worker + resident {
}
let x: citizen = { name: "alice", job: "eng", city: "sf", extra: true }
puts(x.job + " " + x.city)
EOF

expect_build "Diamond supertrait hides non-declared fields" "Record field 'extra' not found in type" << 'EOF'
trait named {
  name: string
}
trait worker: named {
  job: string
}
trait resident: named {
  city: string
}
trait citizen: worker + resident {
}
let x: citizen = { name: "alice", job: "eng", city: "sf", extra: true }
puts(x.extra)
EOF

########################################################################
# SECTION 22: TRAIT-TYPED RETURN INFERRED AT CALL SITE
########################################################################
echo ""
echo "-- TRAIT-TYPED RETURN INFERRED AT CALL SITE --"

expect_runtime_output "Trait-typed return value inferred at call site" "alice" << 'EOF'
trait named {
  name: string
}
let extract = fn(x: named) -> string {
  x.name
}
let result = extract({ name: "alice", age: 30, role: "admin" })
puts(result)
EOF

########################################################################
# SECTION 23: DIVERSE FIELD TYPES
########################################################################
echo ""
echo "-- DIVERSE FIELD TYPES --"

expect_runtime_output "Field-only trait with bool field" "true" << 'EOF'
trait togglable {
  active: bool
}
let x: togglable = { active: true, name: "switch" }
puts(x.active)
EOF

expect_runtime_output "Field-only trait with float field" "3.14" << 'EOF'
trait measured {
  value: float
}
let x: measured = { value: 3.14, unit: "m" }
puts(x.value)
EOF

########################################################################
# SECTION 24: DIFFERENT TRAIT-TYPED PARAMS
########################################################################
echo ""
echo "-- DIFFERENT TRAIT-TYPED PARAMS --"

expect_runtime_output "Two different trait-typed params in one function" "alice 42" << 'EOF'
trait named {
  name: string
}
trait scored {
  score: int
}
let describe = fn(n: named, s: scored) -> string {
  n.name + " " + s.score.show()
}
puts(describe({ name: "alice", id: 1 }, { score: 42, rank: "A" }))
EOF

########################################################################
# SECTION 25: FIELD-ONLY TRAIT AS CONSTRAINT AND TYPE IN SAME PROGRAM
########################################################################
echo ""
echo "-- TRAIT AS CONSTRAINT AND TYPE IN SAME PROGRAM --"

expect_runtime_output "Field-only trait as both constraint and type annotation" "alice bob" << 'EOF'
trait named {
  name: string
}
let get_name = fn[t: named](x: t) -> string {
  x.name
}
let p: named = { name: "alice", age: 30 }
puts(p.name + " " + get_name({ name: "bob", role: "admin" }))
EOF

suite_end
