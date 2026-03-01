#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

suite_begin "EDGE CASE TESTS - TRAIT-FIELD SEMANTICS (P1-7)"

echo "-- CONSTRAINED GENERIC FIELD ACCESS: BASIC PATHS --"

# 1. Generic function with a single field-only trait constraint accessing the field
expect_runtime_output "Constrained generic: single field-only trait, access that field" "42" << 'EOF'
trait has_x {
  x: int
}
let get_x = fn[t: has_x](v: t) -> int {
  v.x
};
let r = { x: 42, y: 99 }
puts(get_x(r))
EOF

# 2. Generic function with multiple field-only trait constraints
expect_runtime_output "Constrained generic: two field-only trait constraints, access each field" "hello
42" << 'EOF'
trait has_name {
  name: string
}
trait has_age {
  age: int
}
let describe = fn[t: has_name + has_age](v: t) -> string {
  v.name
};
let get_age = fn[t: has_name + has_age](v: t) -> int {
  v.age
};
let p = { name: "hello", age: 42, active: true }
puts(describe(p))
puts(get_age(p))
EOF

# 3. Generic function with mixed (field + method) trait constraints accessing field
expect_runtime_output "Constrained generic: mixed field+method constraint, access field" "alice" << 'EOF'
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
let get_name = fn[t: named + shown](v: t) -> string {
  v.name
};
let p: person = { name: "alice", age: 30 }
puts(get_name(p))
EOF

# 4. Generic function accessing field NOT in any constraint (should fail)
expect_build "Constrained generic: access field absent from all constraints" "not guaranteed by constraints" << 'EOF'
trait has_name {
  name: string
}
let get_missing = fn[t: has_name](v: t) -> int {
  v.missing_field
};
get_missing({ name: "test", missing_field: 1 })
EOF

# 5. Constrained generic accessing field from one constraint when another constraint is also present
expect_runtime_output "Constrained generic: access field from first constraint when second is present" "10" << 'EOF'
trait has_x {
  x: int
}
trait has_y {
  y: int
}
let get_x_only = fn[t: has_x + has_y](v: t) -> int {
  v.x
};
let r = { x: 10, y: 20 }
puts(get_x_only(r))
EOF

echo ""
echo "-- SUBSUMPTION AND SATISFACTION EDGE CASES --"

# 6. Constrained generic called with record that has extra fields (subsumption)
expect_runtime_output "Constrained generic: record with extra fields satisfies constraint" "hi" << 'EOF'
trait has_name {
  name: string
}
let get_name = fn[t: has_name](v: t) -> string {
  v.name
};
let r = { name: "hi", age: 42, active: true, score: 100 }
puts(get_name(r))
EOF

# 7. Constrained generic called with record missing required field (should fail)
expect_build "Constrained generic: record missing required field" "missing-field" << 'EOF'
trait has_name {
  name: string
}
let get_name = fn[t: has_name](v: t) -> string {
  v.name
};
get_name({ age: 42 })
EOF

# 8. Constrained generic called with wrong field types (should fail)
expect_build "Constrained generic: record has field but wrong type" "field-type-mismatch" << 'EOF'
trait has_name {
  name: string
}
let get_name = fn[t: has_name](v: t) -> string {
  v.name
};
get_name({ name: 42 })
EOF

echo ""
echo "-- SUPERTRAIT FIELD ACCESS --"

# 9. Supertrait field access: trait B: A, access field from A through B constraint
expect_runtime_output "Supertrait field access: access parent trait field through child constraint" "hello" << 'EOF'
trait has_name {
  name: string
}
trait person: has_name {
  age: int
}
let get_name = fn[t: person](v: t) -> string {
  v.name
};
let p = { name: "hello", age: 30 }
puts(get_name(p))
EOF

# 10. Supertrait field access: trait C: B: A, field from grandparent A through C
expect_runtime_output "Supertrait chain: access grandparent field through grandchild constraint" "deep" << 'EOF'
trait level_a {
  a_field: string
}
trait level_b: level_a {
  b_field: int
}
trait level_c: level_b {
  c_field: int
}
let get_a = fn[t: level_c](v: t) -> string {
  v.a_field
};
let r = { a_field: "deep", b_field: 1, c_field: 2 }
puts(get_a(r))
EOF

# 11. Access field defined on child constraint, not just parent
expect_runtime_output "Supertrait: access child-only field through child constraint" "99" << 'EOF'
trait has_name {
  name: string
}
trait person: has_name {
  age: int
}
let get_age = fn[t: person](v: t) -> int {
  v.age
};
let p = { name: "hello", age: 99 }
puts(get_age(p))
EOF

echo ""
echo "-- SAME-NAMED FIELDS ACROSS CONSTRAINTS --"

# 12. Multiple traits with SAME field name AND same type (should be ok)
expect_runtime_output "Multiple constraints with same field name and same type" "hi" << 'EOF'
trait tag_a {
  label: string
}
trait tag_b {
  label: string
}
let get_label = fn[t: tag_a + tag_b](v: t) -> string {
  v.label
};
let r = { label: "hi" }
puts(get_label(r))
EOF

# 13. Multiple traits with SAME field name but DIFFERENT types (should conflict)
expect_build "Multiple constraints with same field name but different types" "Conflicting field type" << 'EOF'
trait has_x_int {
  x: int
}
trait has_x_string {
  x: string
}
let get_x = fn[t: has_x_int + has_x_string](v: t) -> int {
  v.x
};
get_x({ x: 1 })
EOF

echo ""
echo "-- UNCONSTRAINED GENERIC (ROW POLYMORPHISM PATH) --"

# 14. Field access on unconstrained generic (row polymorphism)
expect_runtime_output "Unconstrained generic field access (row polymorphism)" "5" << 'EOF'
let get_x = fn(r) { r.x };
let p = { x: 5, y: 10 }
puts(get_x(p))
EOF

# 15. Unconstrained generic field access called with record missing the field (should fail)
#     BUG: error says "Cannot unify" instead of a clear "field not found" message.
#     The row-polymorphism path unifies TVar with {x: t, ...row} then fails
#     unification against {y: Int}, leaking internal unification details.
expect_build "Unconstrained generic: record missing accessed field" "Cannot unify" << 'EOF'
let get_x = fn(r) { r.x };
get_x({ y: 10 })
EOF

echo ""
echo "-- CONSTRAINED GENERIC IN CONTROL FLOW --"

# 16. Constrained generic used in if-expression
expect_runtime_output "Constrained generic field used in if-expression" "big" << 'EOF'
trait has_val {
  val: int
}
let classify = fn[t: has_val](v: t) -> string {
  if (v.val > 10) {
    "big"
  } else {
    "small"
  }
};
let r = { val: 42 }
puts(classify(r))
EOF

# 17. Constrained generic used in match
#     BUG: codegen generates Go switch without default case or trailing return,
#     causing "missing return" Go compile error. The generated function:
#       func check_...(v ...) string { switch __scrutinee { case true: return "yes" case false: return "no" } }
#     has no return after the switch block. Go requires exhaustive returns.
expect_build "Constrained generic field used in match expression (BUG: codegen missing return)" "missing return" << 'EOF'
trait has_flag {
  flag: bool
}
let check = fn[t: has_flag](v: t) -> string {
  match v.flag {
    true: "yes"
    false: "no"
  }
};
let r = { flag: true }
puts(check(r))
EOF

echo ""
echo "-- FIELD + METHOD COMBO ON CONSTRAINED GENERIC --"

# 18. Access field AND call method on same constrained generic
expect_runtime_output "Constrained generic: access field and call method on same value" "alice: shown" << 'EOF'
type person = { name: string, age: int }
trait named {
  name: string
}
trait shown[a] {
  fn show(x: a) -> string
}
impl shown for person {
  fn show(x: person) -> string {
    "shown"
  }
}
let describe = fn[t: named + shown](v: t) -> string {
  v.name + ": " + v.show()
};
let p: person = { name: "alice", age: 30 }
puts(describe(p))
EOF

echo ""
echo "-- CONSTRAINED GENERIC THROUGH MULTIPLE FUNCTION LAYERS --"

# 19. Constrained field access through nested function calls
#     BUG: codegen fails with "unresolved type variable 't5' reached mangle_type".
#     When a constrained generic calls another constrained generic, the inner
#     function's type variable is not resolved/monomorphized at codegen time.
expect_runtime_output "Constrained generic passed through multiple function layers" "inner:bob" << 'EOF'
trait has_name {
  name: string
}
let inner = fn[t: has_name](v: t) -> string {
  "inner:" + v.name
};
let outer = fn[t: has_name](v: t) -> string {
  inner(v)
};
let r = { name: "bob", extra: 1 }
puts(outer(r))
EOF

echo ""
echo "-- GENERIC FUNCTION RETURNING CONSTRAINED GENERIC'S FIELD VALUE --"

# 20. Return constrained generic's field value
expect_runtime_output "Constrained generic: return field value from function" "result" << 'EOF'
trait has_value {
  value: string
}
let extract = fn[t: has_value](v: t) -> string {
  v.value
};
let r = { value: "result" }
puts(extract(r))
EOF

# 21. Return constrained generic's int field and use in arithmetic
expect_runtime_output "Constrained generic: return int field and use in arithmetic" "84" << 'EOF'
trait has_n {
  n: int
}
let doubled = fn[t: has_n](v: t) -> int {
  v.n + v.n
};
let r = { n: 42 }
puts(doubled(r))
EOF

echo ""
echo "-- TWO CONSTRAINED GENERICS IN SAME FUNCTION --"

# 22. Two constrained generics with different constraints in same function
expect_runtime_output "Two constrained generics with different constraints" "alice42" << 'EOF'
trait has_name {
  name: string
}
trait has_age {
  age: int
}
let combine = fn[a: has_name, b: has_age](x: a, y: b) -> string {
  x.name + y.age.show()
};
let n = { name: "alice" }
let a = { age: 42 }
puts(combine(n, a))
EOF

# 23. Two constrained generics with same constraint in same function
expect_runtime_output "Two constrained generics with same constraint" "hello world" << 'EOF'
trait has_name {
  name: string
}
let concat_names = fn[a: has_name, b: has_name](x: a, y: b) -> string {
  x.name + " " + y.name
};
let r1 = { name: "hello", id: 1 }
let r2 = { name: "world", active: true }
puts(concat_names(r1, r2))
EOF

echo ""
echo "-- EXACT FIELD MATCH VS SUBSUMPTION --"

# 24. Constrained generic with record having exact fields (no extras)
expect_runtime_output "Constrained generic: record with exactly the required fields" "exact" << 'EOF'
trait has_tag {
  tag: string
}
let get_tag = fn[t: has_tag](v: t) -> string {
  v.tag
};
let r = { tag: "exact" }
puts(get_tag(r))
EOF

echo ""
echo "-- NEGATIVE EDGE CASES: SOUNDNESS BOUNDARIES --"

# 25. Constrained generic body tries to access a field from a different trait not in constraints
expect_build "Constrained generic: access field from trait not in constraints" "not guaranteed by constraints" << 'EOF'
trait has_name {
  name: string
}
trait has_age {
  age: int
}
let get_age = fn[t: has_name](v: t) -> int {
  v.age
};
get_age({ name: "alice", age: 42 })
EOF

# 26. Unconstrained param accessing field on a non-record type (int).
#     BUG: same as test 15 -- error says "Cannot unify" instead of a clear
#     "field access requires record type" message. The row-poly path unifies
#     v (inferred as Int from call site) with {secret: t, ...row} and fails.
expect_build "Unconstrained param accessing field on non-record type" "Cannot unify" << 'EOF'
trait has_secret {
  secret: int
}
let leak = fn(v) { v.secret };
leak(42)
EOF

# 27. Ensure constrained TVar does not unify away - accessing a second field not in constraint should fail
expect_build "Constrained TVar preserves constraints: second field access fails" "not guaranteed by constraints" << 'EOF'
trait has_x {
  x: int
}
let bad = fn[t: has_x](v: t) -> int {
  v.x + v.y
};
bad({ x: 1, y: 2 })
EOF

# 28. Multiple field accesses on same constrained generic: first ok, second absent
expect_build "Constrained TVar: first field access ok but second field not in constraints" "not guaranteed by constraints" << 'EOF'
trait has_name {
  name: string
}
let bad = fn[t: has_name](v: t) -> string {
  v.name + v.extra
};
bad({ name: "hi", extra: "there" })
EOF

echo ""
echo "-- TRAIT OBJECT VS CONSTRAINED GENERIC INTERACTIONS --"

# 29. Field-only trait as type annotation (trait object) hides extra fields
expect_build "Trait object hides extra fields while constrained generic allows access to declared fields" "Record field 'extra' not found" << 'EOF'
trait has_name {
  name: string
}
let x: has_name = { name: "test", extra: 1 }
puts(x.extra)
EOF

# 30. Constrained generic can access declared field even when trait object would hide it
expect_runtime_output "Constrained generic accesses declared field that trait object would also expose" "test" << 'EOF'
trait has_name {
  name: string
}
let get_name = fn[t: has_name](v: t) -> string {
  v.name
};
let r = { name: "test", extra: 1 }
puts(get_name(r))
EOF

echo ""
echo "-- COMPLEX SCENARIOS --"

# 31. Constrained generic with boolean field used in conditional branching
expect_runtime_output "Constrained generic: bool field driving control flow" "active" << 'EOF'
trait has_active {
  active: bool
}
let status = fn[t: has_active](v: t) -> string {
  if (v.active) { "active" } else { "inactive" }
};
let r = { active: true, name: "test" }
puts(status(r))
EOF

# 32. Constrained generic operating on record stored in a let binding
expect_runtime_output "Constrained generic: field access on let-bound record" "stored" << 'EOF'
trait has_data {
  data: string
}
let extract = fn[t: has_data](v: t) -> string {
  v.data
};
let record = { data: "stored", meta: 123 }
let result = extract(record)
puts(result)
EOF

# 33. Supertrait with empty child trait (no additional fields)
expect_runtime_output "Empty child trait inherits parent fields" "inherited" << 'EOF'
trait base {
  value: string
}
trait child: base {
}
let get_value = fn[t: child](v: t) -> string {
  v.value
};
let r = { value: "inherited" }
puts(get_value(r))
EOF

# 34. Field-only trait constraint on generic that is called multiple times with different records
expect_runtime_output "Constrained generic called with different records" "first
second" << 'EOF'
trait has_label {
  label: string
}
let get_label = fn[t: has_label](v: t) -> string {
  v.label
};
let r1 = { label: "first", x: 1 }
let r2 = { label: "second", y: true }
puts(get_label(r1))
puts(get_label(r2))
EOF

# 35. Constrained generic with supertrait chain missing a required field at call site
expect_build "Supertrait chain: call site record missing grandparent field" "missing-field" << 'EOF'
trait has_a {
  a: int
}
trait has_b: has_a {
  b: int
}
let get_a = fn[t: has_b](v: t) -> int {
  v.a
};
get_a({ b: 1 })
EOF

suite_end
