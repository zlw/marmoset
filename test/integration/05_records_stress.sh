#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

suite_begin "STRESS EDGE CASE TESTS - RECORDS"

########################################
# SECTION A: TYPE ALIAS + RECORD INTERACTIONS
########################################

# A1: Type alias used as function parameter type
expect_runtime_output "A1: Type alias as function param type" "30" << 'EOF'
type point = { x: int, y: int }
let sum_point = fn(p: point) -> int { p.x + p.y }
let p: point = { x: 10, y: 20 }
puts(sum_point(p))
EOF

# A2: Type alias used as return type
expect_runtime_output "A2: Type alias as function return type" "7" << 'EOF'
type point = { x: int, y: int }
let make_point = fn(a: int, b: int) -> point { { x: a, y: b } }
let p = make_point(3, 4)
puts(p.x + p.y)
EOF

# A3: Two type aliases with same shape -- a generic function should work on both.
# sum_xy(p) = 3, sum_xy(v) = 7, sum_xy({x:1, y:0}) = 1 => 3+7-1 = 9
expect_runtime_output "A3: Two aliases same shape share Go type" "9" << 'EOF'
type point = { x: int, y: int }
type vec2d = { x: int, y: int }
let p: point = { x: 1, y: 2 }
let v: vec2d = { x: 3, y: 4 }
let sum_xy = fn(r) { r.x + r.y }
puts(sum_xy(p) + sum_xy(v) - sum_xy({ x: 1, y: 0 }))
EOF

# A4: Type alias with record containing computed field values.
# make_result(10) => { val: 10, doubled: 20 }; r.val + r.doubled + r.val = 10 + 20 + 10 = 40
expect_runtime_output "A4: Type alias record with computed field values" "40" << 'EOF'
type result = { val: int, doubled: int }
let make_result = fn(n: int) -> result { { val: n, doubled: n * 2 } }
let r = make_result(10)
puts(r.val + r.doubled + r.val)
EOF

# A5: Two aliases with non-identical shapes.
# s.x = 1, b.x = 4, b.y = 5, b.z = 4 => 1 + 4 + 5 + 4 = 14
expect_runtime_output "A5: Two aliases with overlapping but non-identical shapes" "14" << 'EOF'
type small = { x: int }
type big = { x: int, y: int, z: int }
let s: small = { x: 1 }
let b: big = { x: 4, y: 5, z: 4 }
puts(s.x + b.x + b.y + b.z)
EOF

# A6: Function returning type-aliased record, field access on result
expect_runtime_output "A6: Field access on aliased fn return" "42" << 'EOF'
type box = { val: int }
let wrap = fn(n: int) -> box { { val: n } }
puts(wrap(42).val)
EOF

# A6b: Function taking alias param and returning alias, chained
expect_runtime_output "A6b: Alias param fn, chained calls" "100" << 'EOF'
type box = { val: int }
let double_box = fn(b: box) -> box { { val: b.val * 2 } }
let b1: box = { val: 25 }
let b2 = double_box(double_box(b1))
puts(b2.val)
EOF

# A7: Type alias passed through identity function
expect_runtime_output "A7: Type alias through identity function" "3" << 'EOF'
type point = { x: int, y: int }
let id = fn(p: point) -> point { p }
let p: point = { x: 1, y: 2 }
let q = id(p)
puts(q.x + q.y)
EOF

########################################
# SECTION B: SPREAD STRESS TESTS
########################################

# B7: Spread of function-call result
expect_runtime_output "B7: Spread of fn-call result in let binding" "25" << 'EOF'
let make_base = fn() { { x: 5, y: 10, z: 15 } }
let r = { ...make_base(), x: 25 }
puts(r.x)
EOF

# B8: Spread of match result -- the spread base is itself a match expression.
# Both match arms have same shape, so spread should work. The second let
# triggers __spread reuse bug if same scope.
expect_runtime_output "B8: Spread of match result" "110" << 'EOF'
let x = 2
let base = match x {
  1: { a: 10, b: 20 }
  _: { a: 100, b: 200 }
}
let r = { ...base, b: 10 }
puts(r.a + r.b)
EOF

# B9: Spread of if-expression result
expect_runtime_output "B9: Spread of if-expression result" "15" << 'EOF'
let flag = true
let base = if (flag) { { x: 5, y: 10 } } else { { x: 0, y: 0 } }
let r = { ...base, y: 10 }
puts(r.x + r.y)
EOF

# B10: Spread inside function body (polymorphic param).
# BUG: The spread `{ ...base, x: new_x }` inside a polymorphic function body
# loses fields from the base record. The emitter sees the spread's
# override fields (only `x`) and constructs Record_x_int64 instead of
# Record_x_int64_y_int64, so `r.y` fails.
# Root cause: same as existing test 21 -- spread inside polymorphic function
# body loses fields that are not overridden.
expect_runtime_output "B10: Spread in polymorphic fn loses non-override fields" "90" << 'EOF'
let update = fn(base, new_x) {
  let r = { ...base, x: new_x }
  r.x + r.y
}
let b = { x: 0, y: 10 }
puts(update(b, 10) + update(b, 20) + update(b, 30))
EOF

# B10b: Function with TWO spread let-bindings in same body.
# BUG: Two spread bindings inside a polymorphic function body.
# First spread loses fields (z not carried through).
# Additionally, if the function were not polymorphic, the two __spread
# bindings in the same Go scope would trigger the known reuse bug.
expect_runtime_output "B10b: Two spread let-bindings in same fn body" "35" << 'EOF'
let transform = fn(r) {
  let r2 = { ...r, x: 10 }
  let r3 = { ...r2, y: 20 }
  r3.x + r3.y + r3.z
}
puts(transform({ x: 1, y: 2, z: 5 }))
EOF

# B11: Spread inside a recursive function.
# BUG: Spread creates a new record from the base, which has fields {n, acc}.
# But the override adds n and acc as explicit fields, causing the
# row unification to see duplicate fields and fail with a type error.
# The unifier cannot reconcile the spread base row with the override fields.

# B12: Double spread in one record literal.
# Parser should reject this with a clear error message.
expect_build "B12: Double spread rejected at parse time" "multiple spread entries in record literal are not supported yet" << 'EOF'
let a = { x: 1 }
let b = { y: 2 }
let c = { ...a, ...b }
puts(1)
EOF

# B13: Spread creating a record that is immediately spread again.
# BUG: __spread reuse. Two spread let-bindings in same scope
# produce `__spread := a` then `__spread := b`, causing Go error.
expect_runtime_output "B13: Spread result immediately re-spread" "103" << 'EOF'
let a = { x: 1, y: 2, z: 3 }
let b = { ...a, x: 100 }
let c = { ...b, z: 3 }
puts(c.x + c.z)
EOF

# B14: Spread with single override field, rest copied from base
expect_runtime_output "B14: Override field order: one field, spread copies rest" "30" << 'EOF'
let base = { x: 1, y: 20, z: 3 }
let updated = { ...base, y: 30 }
puts(updated.y)
EOF

# B15: Empty override spread -- { ...r } with zero overrides.
# All fields come from the spread base. The __spread variable is used
# for every field.
expect_runtime_output "B15: Spread with zero overrides (identity spread)" "3" << 'EOF'
let base = { x: 1, y: 2 }
let copy = { ...base }
puts(copy.x + copy.y)
EOF

# B16: Spread of single-field record, overriding the only field.
# This is a full override, so __spread should not be emitted.
expect_runtime_output "B16: Spread of single-field record with override" "99" << 'EOF'
let base = { val: 1 }
let updated = { ...base, val: 99 }
puts(updated.val)
EOF

# B16b: Spread of single-field record, adding a new field (structural growth)
expect_runtime_output "B16b: Spread of single-field record adding field" "11" << 'EOF'
let base = { x: 1 }
let bigger = { ...base, y: 10 }
puts(bigger.x + bigger.y)
EOF

# B17: Single spread in a function body (non-polymorphic, typed params).
# When the function has explicit type annotations, the spread should work
# because the emitter knows the exact record shape.
expect_runtime_output "B17: Spread in typed fn body (non-polymorphic)" "12" << 'EOF'
type point = { x: int, y: int }
let update_x = fn(p: point, new_x: int) -> point { { ...p, x: new_x } }
let p: point = { x: 1, y: 2 }
let q = update_x(p, 10)
puts(q.x + q.y)
EOF

########################################
# SECTION C: RECORD + FUNCTION INTERACTIONS
########################################

# C17: Generic function returning record with generic field.
# Works when called at a single type.
expect_runtime_output "C17: Generic fn returning record with generic field" "42" << 'EOF'
let wrap = fn(x) { { val: x } }
let r = wrap(42)
puts(r.val)
EOF

# C17b: Generic fn returning record, called at two different types.
# Monomorphization must generate two instantiations.
expect_runtime_output "C17b: Generic fn returning record called at different types" "42" << 'EOF'
let wrap = fn(x) { { val: x } }
let r1 = wrap(42)
let r2 = wrap("hello")
puts(r1.val)
EOF

# C18: Generic function taking record with generic field.
expect_runtime_output "C18: Generic fn taking record with generic field" "10" << 'EOF'
let get_val = fn(r) { r.val }
puts(get_val({ val: 10 }))
EOF

# C18b: Generic fn taking record, called at two different field types.
# Each call outputs on its own line (puts adds newline).
expect_runtime_output "C18b: Generic fn taking record at different types" "10
hello" << 'EOF'
let get_val = fn(r) { r.val }
let a = get_val({ val: 10 })
let b = get_val({ val: "hello" })
puts(a)
puts(b)
EOF

# C19: Higher-order function with record param.
# BUG: HOF + records causes unresolved TVars during codegen.
# The apply_to_rec function has two polymorphic params (f and r), and the
# monomorphizer cannot resolve the type variable for f's parameter type
# when it's a record.
expect_runtime_output "C19: HOF applying function to record" "10" << 'EOF'
let apply_to_rec = fn(f, r) { f(r) }
let get_x = fn(r) { r.x }
let result = apply_to_rec(get_x, { x: 10, y: 20 })
puts(result)
EOF

# C20: Function taking record, complex field access expression
expect_runtime_output "C20: Complex expression using record fields" "110" << 'EOF'
let compute = fn(r) { r.x * r.y + r.z }
puts(compute({ x: 10, y: 10, z: 10 }))
EOF

# C21: Nested function calls all returning records.
# BUG: The `middle` function calls `inner` which is polymorphic.
# `middle(n).wrapped.val` requires the monomorphizer to resolve
# inner's return type through middle's return type, but the TVar
# chain is lost.
expect_runtime_output "C21: Nested fn calls returning records" "42" << 'EOF'
let inner = fn(n) { { val: n } }
let middle = fn(n) { { wrapped: inner(n) } }
let outer = fn(n) { middle(n).wrapped.val }
puts(outer(42))
EOF

# C22: Function building up record with accumulator pattern (non-recursive).
# Each step creates a new record, not a spread.
expect_runtime_output "C22: Accumulator record pattern (non-recursive)" "15" << 'EOF'
let step1 = fn(acc) { { sum: acc.sum + 1, count: acc.count + 1 } }
let step2 = fn(acc) { { sum: acc.sum + 4, count: acc.count + 1 } }
let step3 = fn(acc) { { sum: acc.sum + 10, count: acc.count + 1 } }
let init = { sum: 0, count: 0 }
let r = step3(step2(step1(init)))
puts(r.sum)
EOF

# C23: Function creating record with field value from another record's field
expect_runtime_output "C23: Field from one record copied to new record" "42" << 'EOF'
let source = { a: 42, b: 99 }
let copy_a = fn(r) { { val: r.a } }
puts(copy_a(source).val)
EOF

# C24: Function that returns record, result used in arithmetic.
# make(5).val = 10, make(3).val = 6, make(1).val = 2 => 10 + 6 + 2 = 18
expect_runtime_output "C24: Fn return record field in arithmetic" "18" << 'EOF'
let make = fn(n) { { val: n * 2 } }
puts(make(5).val + make(3).val + make(1).val)
EOF

# C25: Record field holding a function value, accessed and called via intermediate let.
# BUG: Even with an intermediate binding to extract the function field,
# the type variable for the function field cannot be resolved at codegen.
# r.compute gets type TVar, not a concrete function type.

########################################
# SECTION D: RECORD FIELD ACCESS STRESS
########################################

# D24: Long chain of field access a.b.c.d.e (5 levels)
expect_runtime_output "D24: 5-level deep nested field access" "7" << 'EOF'
let r = { a: { b: { c: { d: { e: 7 } } } } }
puts(r.a.b.c.d.e)
EOF

# D25: Field access on function return value
expect_runtime_output "D25: Field access on fn return value" "99" << 'EOF'
let make = fn(n) { { val: n, extra: 0 } }
puts(make(99).val)
EOF

# D25b: Chained field access on nested fn return
expect_runtime_output "D25b: Chained field access on nested fn return" "7" << 'EOF'
let outer = fn(n) { { inner: { deep: n } } }
puts(outer(7).inner.deep)
EOF

# D26: Field access on match result via let binding
expect_runtime_output "D26: Field access on match result" "200" << 'EOF'
let x = 2
let r = match x {
  1: { val: 100 }
  _: { val: 200 }
}
puts(r.val)
EOF

# D27: Field access on if-expression result
expect_runtime_output "D27: Field access on if-expr result" "10" << 'EOF'
let flag = true
let r = if (flag) { { val: 10 } } else { { val: 20 } }
puts(r.val)
EOF

# D28: Field access on spread result
expect_runtime_output "D28: Field access on spread result" "10" << 'EOF'
let base = { x: 1, y: 2 }
let updated = { ...base, x: 10 }
puts(updated.x)
EOF

# D29: Accessing same field from different record shapes in same function
expect_runtime_output "D29: Same field name from different record shapes" "30" << 'EOF'
let r1 = { val: 10, extra1: true }
let r2 = { val: 20, extra2: "hello" }
puts(r1.val + r2.val)
EOF

# D29b: Generic function accessing common field from different shapes.
# The function must be monomorphized separately for each record shape.
expect_runtime_output "D29b: Generic fn accesses field from different shapes" "30" << 'EOF'
let get_val = fn(r) { r.val }
let a = get_val({ val: 10, extra: 1 })
let b = get_val({ val: 20, extra: "x" })
puts(a + b)
EOF

# D30: Field access on deeply nested function chain
expect_runtime_output "D30: Deeply nested fn chain field access" "42" << 'EOF'
let a = fn() { { inner: { val: 42 } } }
let r = a()
puts(r.inner.val)
EOF

########################################
# SECTION E: RECORD + CONTROL FLOW
########################################

# E30: Record created inside match arm
expect_runtime_output "E30: Record created inside match arm" "42" << 'EOF'
let x = 2
let r = match x {
  1: { val: 10 }
  2: { val: 42 }
  _: { val: 0 }
}
puts(r.val)
EOF

# E31: Record created inside if-branch
expect_runtime_output "E31: Record created inside if-branch" "10" << 'EOF'
let flag = true
let r = if (flag) { { x: 10, y: 20 } } else { { x: 0, y: 0 } }
puts(r.x)
EOF

# E32: Record as match scrutinee -- pattern matching on record fields

# E32b: Record match with multiple arms, different field literal conditions.
# BUG: Record match with literal pattern for one field and binding for another
# generates Go code like `data := __scrutinee.data` but if the arm body
# does not use `data`, Go rejects the unused variable.
# The emitter generates bindings for all non-literal fields in the pattern
# regardless of whether the body uses them.
expect_runtime_output "E32b: Record match with field literal, unused binding" "first" << 'EOF'
let r = { tag: 1, data: 42 }
let out = match r {
  { tag: 1, data: }: "first"
  { tag: 2, data: }: "second"
  _: "other"
}
puts(out)
EOF

# E33: Array of records, field access pattern
expect_runtime_output "E33: Array of records, field access" "60" << 'EOF'
let items = [{ val: 10 }, { val: 20 }, { val: 30 }]
puts(items[0].val + items[1].val + items[2].val)
EOF

# E34: Record with boolean field used in if condition
expect_runtime_output "E34: Record bool field in if condition" "yes" << 'EOF'
let config = { enabled: true, name: "test" }
let out = if (config.enabled) { "yes" } else { "no" }
puts(out)
EOF

# E34b: Nested record boolean check
expect_runtime_output "E34b: Nested record boolean check" "active" << 'EOF'
let data = { settings: { active: true }, label: "hello" }
let result = if (data.settings.active) { "active" } else { "inactive" }
puts(result)
EOF

# E35: Record field used as match scrutinee
expect_runtime_output "E35: Record field used as match scrutinee" "found" << 'EOF'
let config = { mode: 2 }
let out = match config.mode {
  1: "init"
  2: "found"
  _: "unknown"
}
puts(out)
EOF

########################################
# SECTION F: EDGE CASES IN RECORD SHAPE/TYPE
########################################

# F35: Record with 20 fields
expect_runtime_output "F35: Record with 20 fields" "210" << 'EOF'
let r = { a: 1, b: 2, c: 3, d: 4, e: 5, f: 6, g: 7, h: 8, i: 9, j: 10, k: 11, l: 12, m: 13, n: 14, o: 15, p: 16, q: 17, r: 18, s: 19, t: 20 }
puts(r.a + r.b + r.c + r.d + r.e + r.f + r.g + r.h + r.i + r.j + r.k + r.l + r.m + r.n + r.o + r.p + r.q + r.r + r.s + r.t)
EOF

# F36: Record where field names differ by single character
expect_runtime_output "F36: Field names with single-char difference" "6" << 'EOF'
let r = { aa: 1, ab: 2, ac: 3 }
puts(r.aa + r.ab + r.ac)
EOF

# F36b: Field names that are prefixes of each other
expect_runtime_output "F36b: Field names as prefixes of each other" "6" << 'EOF'
let r = { x: 1, xy: 2, xyz: 3 }
puts(r.x + r.xy + r.xyz)
EOF

# F37: Record with field name that is not a Go keyword
expect_runtime_output "F37: Field name 'val' (not a Go keyword)" "42" << 'EOF'
let r = { val: 42 }
puts(r.val)
EOF

# F37b: Field name that matches a Go keyword.
# BUG: go_record_field_name returns the field name as-is (no escaping).
# When the field name is a Go reserved keyword like "range", the emitted
# Go struct definition `struct{range int64}` is syntactically invalid.
# Go reserved words include: break, case, chan, const, continue, default,
# defer, else, fallthrough, for, func, go, goto, if, import, interface,
# map, package, range, return, select, struct, switch, type, var.
expect_runtime_output "F37b: Field name 'range' (Go keyword)" "1" << 'EOF'
let r = { range: 1 }
puts(r.range)
EOF

# F37c: Field name "map" (Go keyword)
# BUG: Same as F37b -- "map" is a Go keyword, emitted as-is.
expect_runtime_output "F37c: Field name 'map' (Go keyword)" "5" << 'EOF'
let r = { map: 5 }
puts(r.map)
EOF

# F37d: Field name "func" (Go keyword)
# BUG: Same as F37b -- "func" is a Go keyword, emitted as-is.
expect_runtime_output "F37d: Field name 'func' (Go keyword)" "7" << 'EOF'
let r = { func: 7 }
puts(r.func)
EOF

# F37e: Field name "type" -- this is a Marmoset keyword too.
# The parser sees "type" and starts parsing a type alias, not a record field.
# This should error at parse time.
expect_build "F37e: Field name 'type' (parser keyword)" "__ANY_ERROR__" << 'EOF'
let r = { type: 7 }
puts(r.type)
EOF

# F37f: Field name "go" (Go keyword)
# BUG: "go" is a Go keyword, emitted as-is in struct fields.
expect_runtime_output "F37f: Field name 'go' (Go keyword)" "1" << 'EOF'
let r = { go: 1 }
puts(r.go)
EOF

# F37g: Field name "var" (Go keyword)
# BUG: "var" is a Go keyword, emitted as-is in struct fields.
expect_runtime_output "F37g: Field name 'var' (Go keyword)" "1" << 'EOF'
let r = { var: 1 }
puts(r.var)
EOF

# F37h: Field name "return" -- this is a Marmoset keyword.
# The parser sees "return" and starts parsing a return statement, not a record field.
# This should error at parse time.
expect_build "F37h: Field name 'return' (Marmoset keyword)" "__ANY_ERROR__" << 'EOF'
let r = { return: 1 }
puts(r.return)
EOF

# F38: Two records with same field names but different types.
# Each puts adds a newline.
expect_runtime_output "F38: Same fields, different types" "hello
42" << 'EOF'
let r1 = { x: "hello", y: true }
let r2 = { x: 42, y: 100 }
puts(r1.x)
puts(r2.x)
EOF

# F39: Record inside record inside record (3 levels) with spread at each level.
# BUG: Three spread let-bindings in the same scope: deep2, mid2, outer2.
# The second __spread := in the same Go scope causes the known reuse bug.
# Additionally, the __spread types differ (deep2 is Record_val_int64,
# mid2 is Record_inner_..._tag_string, outer2 is Record_data_..._id_int64),
# causing type mismatch errors in the reassignment.
expect_runtime_output "F39: 3-level nested records with spread at each" "999" << 'EOF'
let deep = { val: 1 }
let deep2 = { ...deep, val: 999 }
let mid = { inner: deep2, tag: "a" }
let mid2 = { ...mid, tag: "b" }
let outer = { data: mid2, id: 1 }
let outer2 = { ...outer, id: 2 }
puts(outer2.data.inner.val)
EOF

# F40: Record where field value is a function, called via dot syntax.
# BUG: `r.compute(r.base)` is parsed as a method call, not field access + call.
# The typechecker looks for a method named 'compute' on the record type
# and fails because there is no such method -- it's a field holding a function.
# Marmoset does not support calling function-valued fields via dot syntax.

# F40b: Record fn field accessed, then bound and called.
# BUG: Even with intermediate binding, the record field type for a function
# value generates an unresolved TVar during codegen.

# F41: Two records with identical fields in different order should unify.
expect_runtime_output "F41: Records with reordered fields are equivalent" "3" << 'EOF'
let r1 = { x: 1, y: 2 }
let r2 = { y: 2, x: 1 }
puts(r1.x + r2.y)
EOF

########################################
# SECTION G: ADDITIONAL STRESS PATTERNS
########################################

# G41: Many records with different shapes in same scope
expect_runtime_output "G41: Many different record shapes in one scope" "21" << 'EOF'
let r1 = { a: 1 }
let r2 = { a: 2, b: 3 }
let r3 = { a: 4, b: 5, c: 6 }
puts(r1.a + r2.a + r2.b + r3.a + r3.b + r3.c)
EOF

# G42: Record field accessed in arithmetic expression.
# 10 * 5 * (10 / 5) = 50 * 2 = 100
expect_runtime_output "G42: Record fields in complex arithmetic" "100" << 'EOF'
let r = { x: 10, y: 5 }
puts(r.x * r.y * (r.x / r.y))
EOF

# G43: Record returned from if, then field accessed
expect_runtime_output "G43: If-returned record, immediate field access via let" "42" << 'EOF'
let flag = true
let r = if (flag) { { val: 42 } } else { { val: 0 } }
puts(r.val)
EOF

# G44: Record with field value from another record field
expect_runtime_output "G44: Field value from another record's field" "42" << 'EOF'
let source = { data: 42 }
let target = { copied: source.data }
puts(target.copied)
EOF

# G45: Record spread where base has more fields than result uses
# All fields overridden, so __spread should not be emitted.
expect_runtime_output "G45: Spread base with all fields overridden" "30" << 'EOF'
let base = { x: 1, y: 2 }
let full_override = { ...base, x: 10, y: 20 }
puts(full_override.x + full_override.y)
EOF

# G46: Three consecutive spread let-bindings in same scope.
# BUG: __spread reuse. The second spread binding tries to do
# `__spread := r1` but __spread was already declared by the first spread,
# causing Go error "no new variables on left side of :=".
expect_runtime_output "G46: Three spreads in same scope, different fields" "33" << 'EOF'
let r0 = { a: 1, b: 2, c: 3 }
let r1 = { ...r0, a: 10 }
let r2 = { ...r1, b: 20 }
puts(r2.a + r2.b + r2.c)
EOF

# G47: Record with string fields and concatenation
expect_runtime_output "G47: Record string field concatenation" "hello world" << 'EOF'
let r = { first: "hello", sep: " ", last: "world" }
puts(r.first + r.sep + r.last)
EOF

# G48: If branches returning same-shape records
expect_runtime_output "G48: If branches with same-shape record" "20" << 'EOF'
let n = 5
let r = if (n > 3) {
  { val: n * 4 }
} else {
  { val: n * 2 }
}
puts(r.val)
EOF

# G49: Array of nested records.
# items[0].inner.val + items[2].inner.val - items[1].inner.val + items[1].inner.val
# = 10 + 30 - 20 + 20 = 40
expect_runtime_output "G49: Array of nested records" "40" << 'EOF'
let items = [{ inner: { val: 10 } }, { inner: { val: 20 } }, { inner: { val: 30 } }]
puts(items[0].inner.val + items[2].inner.val - items[1].inner.val + items[1].inner.val)
EOF

# G50: Two functions creating records with same shape.
# mk1(2) = {x: 2, y: 3}, mk2(3) = {x: 6, y: 9}
# r1.x + r2.x - r1.y = 2 + 6 - 3 = 5
expect_runtime_output "G50: Two fns creating same-shape records" "5" << 'EOF'
let mk1 = fn(n) { { x: n, y: n + 1 } }
let mk2 = fn(n) { { x: n * 2, y: n * 3 } }
let r1 = mk1(2)
let r2 = mk2(3)
puts(r1.x + r2.x - r1.y)
EOF

# G51: Record with float fields
expect_runtime_output "G51: Record with float fields" "3" << 'EOF'
let r = { x: 1.5, y: 2.5, n: 3 }
puts(r.n)
EOF

# G52: Record with single bool field
expect_runtime_output "G52: Record with single bool field" "yes" << 'EOF'
let r = { ok: true }
let out = if (r.ok) { "yes" } else { "no" }
puts(out)
EOF

# G53: Record field access in function argument position
expect_runtime_output "G53: Record field as function argument" "10" << 'EOF'
let double = fn(n) { n * 2 }
let r = { val: 5 }
puts(double(r.val))
EOF

# G54: Chain of let bindings creating records from previous record fields
expect_runtime_output "G54: Chain of records from previous fields" "16" << 'EOF'
let r1 = { x: 2 }
let r2 = { x: r1.x * 2 }
let r3 = { x: r2.x * 2 }
let r4 = { x: r3.x * 2 }
puts(r4.x)
EOF

# G55: Record pattern matching with wildcard for unused fields.
# Tests that `y: _` and `z: _` don't create unused Go variables.
expect_runtime_output "G55: Record match with wildcard for unused fields" "10" << 'EOF'
let r = { x: 10, y: 20, z: 30 }
let result = match r {
  { x:, y: _, z: _ }: x
  _: 0
}
puts(result)
EOF

# G56: Record spread in expression position (passed directly to fn).
# The spread happens in argument position, uses IIFE path in codegen.
expect_runtime_output "G56: Spread record as function argument" "12" << 'EOF'
let base = { x: 1, y: 2 }
let sum = fn(r) { r.x + r.y }
puts(sum({ ...base, x: 10 }))
EOF

# G57: Function that takes two different record-shaped params.
expect_runtime_output "G57: Function with two different record params" "110" << 'EOF'
let combine = fn(a, b) { a.x + b.y }
puts(combine({ x: 10, z: 0 }, { y: 100, w: 0 }))
EOF

# G58: Record with field value from comparison expression
expect_runtime_output "G58: Record with boolean expr as field value" "true" << 'EOF'
let r = { result: 10 > 5 }
let out = if (r.result) { "true" } else { "false" }
puts(out)
EOF

# G59: Spread where the base record comes from array index
expect_runtime_output "G59: Spread from array-indexed record" "20" << 'EOF'
let arr = [{ x: 1, y: 2 }]
let r = { ...arr[0], x: 20 }
puts(r.x)
EOF

# G60: Type alias with derived trait, used in function
expect_runtime_output "G60: Type alias with derived eq used in function" "true" << 'EOF'
trait eq[a] {
  fn eq(x: a, y: a) -> bool
}
type point = { x: int, y: int }
derive eq for point;
let check_eq = fn(a: point, b: point) -> bool { a.eq(b) }
let p1: point = { x: 1, y: 2 }
let p2: point = { x: 1, y: 2 }
let out = if (check_eq(p1, p2)) { "true" } else { "false" }
puts(out)
EOF

# G61: Record used as hash value (record inside hash)
expect_runtime_output "G61: Record as hash value" "42" << 'EOF'
let data = { "key": { val: 42 } }
puts(data["key"].val)
EOF

# G62: Record with field name "id" (common but could cause Go issues if mangled)
expect_runtime_output "G62: Record with field name 'id'" "100" << 'EOF'
let entity = { id: 100, name: "test" }
puts(entity.id)
EOF

# G63: Spread where the only override is a field that does not exist in the base
# (structural growth via spread). Single spread, no reuse issue.
expect_runtime_output "G63: Spread adds new field not in base" "6" << 'EOF'
let base = { x: 1, y: 2 }
let grown = { ...base, z: 3 }
puts(grown.x + grown.y + grown.z)
EOF

# G64: Field access on let-bound record from complex expression
expect_runtime_output "G64: Record field in ternary-like if" "bigger" << 'EOF'
let r = { threshold: 10 }
let val = 15
let out = if (val > r.threshold) { "bigger" } else { "smaller" }
puts(out)
EOF

# G65: Record with boolean field, negated in condition
expect_runtime_output "G65: Record bool field negated in condition" "off" << 'EOF'
let r = { enabled: false }
let out = if (r.enabled) { "on" } else { "off" }
puts(out)
EOF

# G66: Multiple record shapes used as function arguments in same call expression
expect_runtime_output "G66: Two record args in one function call" "30" << 'EOF'
let add_fields = fn(a, b) { a.x + b.y }
puts(add_fields({ x: 10 }, { y: 20 }))
EOF

# G67: Record spread in tail position of function body
expect_runtime_output "G67: Spread in tail position of fn body" "12" << 'EOF'
type point = { x: int, y: int }
let set_x = fn(p: point, v: int) -> point { { ...p, x: v } }
let p: point = { x: 1, y: 2 }
let q = set_x(p, 10)
puts(q.x + q.y)
EOF

suite_end
