#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

suite_begin "TYPECHECK & CODEGEN INTEGRATION TESTS - RECORDS"
echo "-- PHASE 4.4: RECORDS & ROW POLYMORPHISM --"

expect_runtime_output "Record literal + field access" "30" << 'EOF'
let p = { x: 10, y: 20 }
puts(p.x + p.y)
EOF

expect_runtime_output "Type alias with record annotation" "3" << 'EOF'
type point = { x: int, y: int }
let p: point = { x: 1, y: 2 }
puts(p.x + p.y)
EOF

expect_build "Duplicate type alias definition is rejected in one program" "Duplicate type alias definition: point" << 'EOF'
type point = { x: int, y: int }
type point = { y: int, x: int }
puts(1)
EOF

expect_runtime_output "Record spread update" "12" << 'EOF'
let p = { x: 1, y: 2 }
let p2 = { ...p, x: 10 }
puts(p2.x + p2.y)
EOF

expect_build "Open row in type annotation rejected in v1" "Open row variables" << 'EOF'
let get_x = fn(r: { x: int, ...row }) -> int { r.x }
EOF

expect_runtime_output "Field access without row annotation works" "5" << 'EOF'
let p = { x: 5, y: 10, z: 20 }
let get_x = fn(r) { r.x }
puts(get_x(p))
EOF

expect_runtime_output "Row-tail fields survive pass-through wrappers" "10" << 'EOF'
let inner = fn(r) { r.x + r.y }
let outer = fn(r) { inner(r) }
puts(outer({ x: 3, y: 7 }))
EOF

expect_runtime_output "Higher-order function preserves wide record argument shape" "42" << 'EOF'
let apply = fn(f, r) { f(r) }
let get_x = fn(r) { r.x }
puts(apply(get_x, { x: 42, y: 0 }))
EOF

expect_runtime_output "Spread-returning polymorphic function keeps tail fields" "25" << 'EOF'
let update_x = fn(r, new_x) { { ...r, x: new_x } }
let base = { x: 1, y: 20 }
let result = update_x(base, 5)
puts(result.x + result.y)
EOF

expect_runtime_output "Recursive spread update keeps row fields stable" "10" << 'EOF'
let countdown = fn(r) {
  if (r.n == 0) {
    r.acc
  } else {
    let next = { ...r, n: r.n - 1, acc: r.acc + 1 }
    countdown(next)
  }
}
puts(countdown({ n: 10, acc: 0 }))
EOF

expect_runtime_output "Record function field extracted then called" "25" << 'EOF'
let square = fn(n) { n * n }
let r = { compute: square, base: 5 }
let f = r.compute
puts(f(r.base))
EOF

expect_runtime_output "Record function field called with dot syntax" "25" << 'EOF'
let square = fn(n) { n * n }
let r = { compute: square, base: 5 }
puts(r.compute(r.base))
EOF

expect_runtime_output "Inline record argument to wrapper function" "3" << 'EOF'
let mk = fn(r: { x: int }) { { inner: r } }
let o = mk({ x: 3 })
puts(o.inner.x)
EOF

expect_runtime_output "Local wrapper function inside function body" "5" << 'EOF'
let outer = fn(x) {
  let mk = fn(v) { { inner: v } }
  mk(x)
}
let o = outer(5)
puts(o.inner)
EOF

expect_runtime_output "Typed local wrapper preserves wrapped record fields" "7" << 'EOF'
let outer = fn(r: { x: int }) {
  let mk = fn(v: { x: int }) { { inner: v } }
  mk(r)
}
let o = outer({ x: 7 })
puts(o.inner.x)
EOF

expect_runtime_output "Duplicate record fields are last-write-wins" "2" << 'EOF'
let p = { x: 1, x: 2 }
puts(p.x)
EOF

expect_runtime_output "Case-distinct record fields coexist" "3" << 'EOF'
let p = { x: 1, X: 2 }
puts(p.x + p.X)
EOF

expect_runtime_output "Record spread with all fields overridden" "10" << 'EOF'
let p = { x: 1, X: 2 }
let q = { ...p, x: 4, X: 6 }
puts(q.x + q.X)
EOF

expect_runtime_output "Record spread preserves nested record fields" "7" << 'EOF'
let p = { x: 1, y: { z: 2 } }
let q = { ...p, x: 5 }
puts(q.x + q.y.z)
EOF

expect_runtime_output "Record pattern match" "30" << 'EOF'
let p = { x: 10, y: 20 }
let result = match p {
  { x:, y: }: x + y
  _: 0
}
puts(result)
EOF

expect_runtime_output "Derive eq for record" "true" << 'EOF'
trait eq[a] {
  fn eq(x: a, y: a) -> bool
}
type point = { x: int, y: int }
derive eq for point;
let p1: point = { x: 1, y: 2 }
let p2: point = { x: 1, y: 2 }
puts(p1.eq(p2))
EOF

expect_runtime_output "Derive show for record" "{ x: 1, y: 2 }" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
type point = { x: int, y: int }
derive show for point;
let p: point = { x: 1, y: 2 }
puts(p.show())
EOF

expect_runtime_output "Derive ord for record" "less" << 'EOF'
trait ord[a] {
  fn compare(x: a, y: a) -> int
}
type point = { x: int, y: int }
derive ord for point;
let p1: point = { x: 1, y: 2 }
let p2: point = { x: 1, y: 3 }
puts(p1.compare(p2))
EOF

expect_runtime_output "Derive hash for record" "16370" << 'EOF'
trait hash[a] {
  fn hash(x: a) -> int
}
type point = { x: int, y: int }
derive hash for point;
let p: point = { x: 1, y: 2 }
puts(p.hash())
EOF

expect_runtime_output "Reordered record literal still resolves derived show" "{ x: 1, y: 2 }" << 'EOF'
trait show[a] {
  fn show(x: a) -> string
}
type point = { x: int, y: int }
derive show for point;
let p: point = { y: 2, x: 1 }
puts(p.show())
EOF

expect_runtime_output "Reordered record aliases are assignment-compatible" "3" << 'EOF'
type point = { x: int, y: int }
type vec = { y: int, x: int }
let p: point = { x: 1, y: 2 }
let v: vec = p
puts(v.x + v.y)
EOF

expect_runtime_output "Alias and structural record params interoperate through wrapper" "5" << 'EOF'
type point = { x: int, y: int }
let add = fn(a: point, b: { x: int, y: int }) -> int { a.x + b.y }
let p: point = { x: 1, y: 2 }
let q = { y: 4, x: 3 }
puts(add(p, q))
EOF

expect_runtime_output "Derived eq/hash are stable across field order" "ok" << 'EOF'
trait eq[a] {
  fn eq(x: a, y: a) -> bool
}
trait hash[a] {
  fn hash(x: a) -> int
}
type point = { x: int, y: int }
derive eq, hash for point;
let p1: point = { x: 1, y: 2 }
let p2: point = { y: 2, x: 1 }
let out = if (p1.eq(p2)) {
  if (p1.hash() == p2.hash()) { "ok" } else { "hash_bad" }
} else {
  "eq_bad"
}
puts(out)
EOF

expect_build "Hash literal missing comma reports clear parse error" "expected ',' or '}' after hash literal entry" << 'EOF'
let x = { "a": 1 b: 2 }
EOF

expect_build "Record literal missing comma reports clear parse error" "expected ',' or '}' after record literal entry" << 'EOF'
let x = { a: 1 b: 2 }
EOF

expect_build "Record literal duplicate spread reports clear parse error" "multiple spread entries in record literal are not supported yet" << 'EOF'
let x = { ...a, ...b }
EOF

expect_build "Malformed single dot token errors deterministically" "unexpected Token.Dot found" << 'EOF'
.
EOF

expect_build "Malformed double dot token errors deterministically" "unexpected Token.Illegal found" << 'EOF'
..
EOF

expect_build "Unknown symbol token errors deterministically" "unexpected Token.Illegal found" << 'EOF'
@
EOF



# -------------------------------------------------------------------
# Imported From 05_records_edge_cases.sh
# -------------------------------------------------------------------

########################################
# SECTION A: NESTING DEPTH
########################################

expect_runtime_output "3-level nested record literal and access" "42" << 'EOF'
let r = { a: { b: { c: 42 } } }
puts(r.a.b.c)
EOF

expect_runtime_output "4-level nested record literal and access" "99" << 'EOF'
let r = { w: { x: { y: { z: 99 } } } }
puts(r.w.x.y.z)
EOF

expect_runtime_output "Nested record with mixed int and string fields" "hello" << 'EOF'
let r = { name: "hello", data: { count: 7, flag: true } }
let out = if (r.data.flag) {
  r.name
} else {
  "nope"
}
puts(out)
EOF

########################################
# SECTION B: MANY FIELDS
########################################

expect_runtime_output "Record with 10 fields" "55" << 'EOF'
let r = { a: 1, b: 2, c: 3, d: 4, e: 5, f: 6, g: 7, h: 8, i: 9, j: 10 }
puts(r.a + r.b + r.c + r.d + r.e + r.f + r.g + r.h + r.i + r.j)
EOF

########################################
# SECTION C: SPREAD CHAINS
# BUG: multiple spreads in sequence at top level
# emit __spread := X repeatedly, causing Go error:
# "no new variables on left side of :="
########################################

# This triggers the __spread variable reuse bug in codegen.
# Two consecutive spreads in the same scope produce:
#   __spread := a
#   ...
#   __spread := b   <-- Go error: no new variables on left side of :=
expect_runtime_output "Double spread chain (same shape)" "33" << 'EOF'
let a = { x: 1, y: 2, z: 3 }
let b = { ...a, x: 10 }
let c = { ...b, y: 20 }
puts(c.x + c.y + c.z)
EOF

expect_runtime_output "Triple spread chain" "60" << 'EOF'
let a = { x: 1, y: 2, z: 3 }
let b = { ...a, x: 10 }
let c = { ...b, y: 20 }
let d = { ...c, z: 30 }
puts(d.x + d.y + d.z)
EOF

# BUG: structural growth across spreads. Different shape at each step.
# __spread changes type between steps (Record_x -> Record_x_y -> Record_x_y_z)
expect_runtime_output "Spread chain with field additions (structural growth)" "15" << 'EOF'
let a = { x: 1 }
let b = { ...a, y: 2 }
let c = { ...b, z: 12 }
puts(c.x + c.y + c.z)
EOF

expect_runtime_output "Spread overrides one field, preserves rest" "104" << 'EOF'
let base = { x: 1, y: 2, z: 3 }
let updated = { ...base, y: 100 }
puts(updated.x + updated.y + updated.z)
EOF

# Five consecutive spreads: should expose __spread reuse 5 times
expect_runtime_output "Five-step spread chain (same shape)" "50" << 'EOF'
let r0 = { a: 1, b: 2, c: 3, d: 4, e: 5 }
let r1 = { ...r0, a: 10 }
let r2 = { ...r1, b: 10 }
let r3 = { ...r2, c: 10 }
let r4 = { ...r3, d: 10 }
let r5 = { ...r4, e: 10 }
puts(r5.a + r5.b + r5.c + r5.d + r5.e)
EOF

########################################
# SECTION D: SPREAD WITH NESTED RECORDS
########################################

expect_runtime_output "Spread preserves deeply nested record" "42" << 'EOF'
let base = { tag: "hello", inner: { deep: { val: 42 } } }
let updated = { ...base, tag: "world" }
puts(updated.inner.deep.val)
EOF

expect_runtime_output "Spread preserves nested record, override sibling" "100" << 'EOF'
let base = { a: 1, nested: { v: 99 } }
let next = { ...base, a: 100 }
puts(next.a)
EOF

expect_runtime_output "Spread with nested record value override" "999" << 'EOF'
let base = { outer: { inner: 1 }, tag: 0 }
let updated = { ...base, outer: { inner: 999 } }
puts(updated.outer.inner)
EOF

expect_runtime_output "Spread base with nested record preserved in result" "7" << 'EOF'
let base = { x: 1, nested: { val: 7 } }
let updated = { ...base, x: 100 }
puts(updated.nested.val)
EOF

expect_runtime_output "Spread result nested field access" "42" << 'EOF'
let base = { data: { value: 42 }, meta: "info" }
let updated = { ...base, meta: "updated" }
puts(updated.data.value)
EOF

########################################
# SECTION E: FUNCTIONS AND RECORDS
########################################

# BUG: Polymorphic function passing record through multiple layers
# causes "unresolved type variable" in codegen.
expect_runtime_output "Record passed through two function layers" "10" << 'EOF'
let inner = fn(r) { r.x + r.y }
let outer = fn(r) { inner(r) }
let result = outer({ x: 3, y: 7 })
puts(result)
EOF

# BUG: Generic function creating record, called from another generic function
expect_runtime_output "Record created in inner function, accessed in outer" "5" << 'EOF'
let make = fn(v) { { x: v, y: v } }
let use = fn(v) {
  let r = make(v)
  r.x
}
puts(use(5))
EOF

expect_runtime_output "Function returning record, chained field access" "7" << 'EOF'
let make_pair = fn(a, b) { { first: a, second: b } }
puts(make_pair(3, 7).second)
EOF

expect_runtime_output "Identity function on record" "42" << 'EOF'
let id = fn(r) { r }
let result = id({ x: 42 })
puts(result.x)
EOF

expect_runtime_output "Function takes record, returns new record wrapping it" "9" << 'EOF'
let wrap = fn(r) { { inner: r, extra: r.val + 1 } }
let w = wrap({ val: 8 })
puts(w.extra)
EOF

# BUG: Higher-order function with record argument
# causes "unresolved type variable" in codegen.
expect_runtime_output "Record passed to higher-order function" "42" << 'EOF'
let apply = fn(f, r) { f(r) }
let get_x = fn(r) { r.x }
puts(apply(get_x, { x: 42, y: 0 }))
EOF

# BUG: function body using spread on a polymorphic parameter
# loses fields, generating Record_x_int64 instead of Record_x_int64_y_int64.
# Go error: "__base declared and not used" and "has no field or method y"
expect_runtime_output "Function body uses spread to build return value" "25" << 'EOF'
let update_x = fn(r, new_x) { { ...r, x: new_x } }
let base = { x: 1, y: 20 }
let result = update_x(base, 5)
puts(result.x + result.y)
EOF

########################################
# SECTION F: SHAPE DEDUPLICATION
########################################

expect_runtime_output "Two records with identical shape share Go type" "30" << 'EOF'
let a = { x: 10, y: 20 }
let b = { x: 100, y: 200 }
puts(a.x + a.y)
EOF

expect_runtime_output "Same shape from different code paths" "15" << 'EOF'
let make1 = fn(v) { { x: v, y: v + 1 } }
let make2 = fn(v) { { x: v * 2, y: v * 3 } }
let r1 = make1(5)
let r2 = make2(5)
puts(r1.x + r2.x)
EOF

expect_runtime_output "Three distinct record shapes coexist" "6" << 'EOF'
let r1 = { a: 1 }
let r2 = { b: 2, c: 3 }
let r3 = { d: 4, e: 5, f: 6 }
puts(r1.a + r2.b + r2.c)
EOF

########################################
# SECTION G: MIXED FIELD TYPES
########################################

expect_runtime_output "Record with bool and int fields" "1" << 'EOF'
let r = { flag: true, count: 1 }
let out = if (r.flag) { r.count } else { 0 }
puts(out)
EOF

expect_runtime_output "Record with string fields accessed individually" "hello" << 'EOF'
let r = { first: "hello", second: "world" }
puts(r.first)
EOF

expect_runtime_output "Record with float field" "3" << 'EOF'
let r = { x: 1.5, n: 3 }
puts(r.n)
EOF

expect_runtime_output "Record with string concatenation via +" "helloworld" << 'EOF'
let r = { first: "hello", second: "world" }
puts(r.first + r.second)
EOF

########################################
# SECTION H: SINGLE-FIELD RECORDS
########################################

expect_runtime_output "Single field record" "42" << 'EOF'
let r = { value: 42 }
puts(r.value)
EOF

expect_runtime_output "Single field record passed to function" "7" << 'EOF'
let get = fn(r) { r.value }
puts(get({ value: 7 }))
EOF

########################################
# SECTION I: RECORDS IN CONTROL FLOW
########################################

expect_runtime_output "If expression returns record (true branch)" "1" << 'EOF'
let r = if (true) { { x: 1, y: 2 } } else { { x: 3, y: 4 } }
puts(r.x)
EOF

expect_runtime_output "If expression returns record (false branch)" "4" << 'EOF'
let r = if (false) { { x: 1, y: 2 } } else { { x: 3, y: 4 } }
puts(r.y)
EOF

expect_runtime_output "Nested if returning records" "20" << 'EOF'
let v = 2
let r = if (v == 1) {
  { x: 10 }
} else {
  if (v == 2) {
    { x: 20 }
  } else {
    { x: 30 }
  }
}
puts(r.x)
EOF

expect_runtime_output "Match arms return records with same shape" "200" << 'EOF'
let x = 2
let r = match x {
  1: { val: 100 }
  2: { val: 200 }
  _: { val: 0 }
}
puts(r.val)
EOF

expect_runtime_output "Match on record returns new record" "100" << 'EOF'
let input = { x: 10, y: 20 }
let output = match input {
  { x:, y: }: { val: x * y / 2 }
  _: { val: 0 }
}
puts(output.val)
EOF

expect_runtime_output "Field access on if-expression returning record" "10" << 'EOF'
let x = if (true) { { val: 10 } } else { { val: 20 } }
puts(x.val)
EOF

########################################
# SECTION J: PATTERN MATCHING ON RECORDS
########################################

expect_runtime_output "Record pattern match extracts all fields" "30" << 'EOF'
let p = { x: 10, y: 20 }
let sum = match p {
  { x:, y: }: x + y
  _: 0
}
puts(sum)
EOF

expect_runtime_output "Record match with literal pattern in field" "yes" << 'EOF'
let r = { tag: 1, val: 42 }
let out = match r {
  { tag: 1, val: }: "yes"
  _: "no"
}
puts(out)
EOF

expect_runtime_output "Record match with rest binding" "10" << 'EOF'
let r = { x: 10, y: 20, z: 30 }
let result = match r {
  { x:, ...rest }: x
  _: 0
}
puts(result)
EOF

########################################
# SECTION K: TYPE ALIASES
########################################

expect_runtime_output "Type alias used for two different bindings" "30" << 'EOF'
type point = { x: int, y: int }
let p1: point = { x: 10, y: 20 }
let p2: point = { x: 100, y: 200 }
puts(p1.x + p1.y)
EOF

expect_runtime_output "Type alias record assigned from unaliased record" "3" << 'EOF'
type point = { x: int, y: int }
let raw = { x: 1, y: 2 }
let p: point = raw
puts(p.x + p.y)
EOF

expect_runtime_output "Two aliases for same shape are interchangeable" "3" << 'EOF'
type point = { x: int, y: int }
type vec2d = { x: int, y: int }
let p: point = { x: 1, y: 2 }
let v: vec2d = p
puts(v.x + v.y)
EOF

expect_runtime_output "Type alias record updated via spread" "12" << 'EOF'
type point = { x: int, y: int }
let p: point = { x: 1, y: 2 }
let q = { ...p, x: 10 }
puts(q.x + q.y)
EOF

########################################
# SECTION L: FIELD PUNNING
########################################

expect_runtime_output "Record field punning (shorthand)" "7" << 'EOF'
let x = 3
let y = 4
let r = { x:, y: }
puts(r.x + r.y)
EOF

expect_runtime_output "Record field punning mixed with explicit values" "10" << 'EOF'
let x = 3
let r = { x:, y: 7 }
puts(r.x + r.y)
EOF

########################################
# SECTION M: SPREAD FROM FUNCTION CALL RESULT
########################################

expect_runtime_output "Spread from function return value" "15" << 'EOF'
let make_base = fn() { { x: 5, y: 10 } }
let r = { ...make_base(), x: 15 }
puts(r.x)
EOF

expect_runtime_output "Spread from function call, keep all base fields" "15" << 'EOF'
let make_base = fn() { { x: 5, y: 10 } }
let r = { ...make_base(), z: 0 }
puts(r.x + r.y)
EOF

########################################
# SECTION N: FIELD ACCESS CHAINS
########################################

expect_runtime_output "Chained field access a.b.c" "7" << 'EOF'
let r = { a: { b: { c: 7 } } }
puts(r.a.b.c)
EOF

expect_runtime_output "Chained field access on function return value" "9" << 'EOF'
let mk = fn() { { inner: { val: 9 } } }
puts(mk().inner.val)
EOF

########################################
# SECTION O: SHADOWING AND SCOPING
########################################

expect_runtime_output "Let binding shadows record field name" "99" << 'EOF'
let x = 99
let r = { x: 1, y: 2 }
puts(x)
EOF

expect_runtime_output "Record field access after name shadow" "1" << 'EOF'
let x = 99
let r = { x: 1, y: 2 }
puts(r.x)
EOF

########################################
# SECTION P: SPREAD IN EXPRESSION POSITION
########################################

expect_runtime_output "Spread record passed directly to function" "12" << 'EOF'
let base = { x: 1, y: 2 }
let get_sum = fn(r) { r.x + r.y }
puts(get_sum({ ...base, x: 10 }))
EOF

########################################
# SECTION Q: DUPLICATE FIELDS
########################################

expect_runtime_output "Duplicate field names, last value wins" "3" << 'EOF'
let r = { x: 1, x: 2, x: 3 }
puts(r.x)
EOF

########################################
# SECTION R: COMPUTED FIELD VALUES
########################################

expect_runtime_output "Record fields with arithmetic expressions" "15" << 'EOF'
let a = 5
let r = { x: a * 2, y: a + 0 }
puts(r.x + r.y)
EOF

expect_runtime_output "Record field value from function call" "10" << 'EOF'
let double = fn(n) { n * 2 }
let r = { x: double(5) }
puts(r.x)
EOF

########################################
# SECTION S: ARRAY OF RECORDS
########################################

expect_runtime_output "Array of records, access field" "20" << 'EOF'
let arr = [{ x: 10 }, { x: 20 }, { x: 30 }]
puts(arr[1].x)
EOF

########################################
# SECTION T: DERIVED TRAITS
########################################

expect_runtime_output "Derived eq on records with different field values" "false" << 'EOF'
trait eq[a] {
  fn eq(x: a, y: a) -> bool
}
type point = { x: int, y: int }
derive eq for point;
let p1: point = { x: 1, y: 2 }
let p2: point = { x: 1, y: 3 }
puts(p1.eq(p2))
EOF

########################################
# SECTION U: CODEGEN PROPERTIES
########################################

run_codegen_deterministic_from_stdin "Record shape codegen is deterministic" << 'EOF'
let a = { x: 1, y: 2 }
let b = { x: 3, y: 4 }
let c = { p: "hello", q: true }
puts(a.x + b.y)
EOF

run_emit_go_not_contains_from_stdin "Full override spread has no __spread in Go output" "__spread" << 'EOF'
let base = { x: 1, y: 2 }
let updated = { ...base, x: 10, y: 20 }
puts(updated.x + updated.y)
EOF

########################################
# SECTION V: SPREAD OF SPREAD RESULT
# This is the same underlying bug as SECTION C:
# multiple __spread := in the same Go scope.
########################################

expect_runtime_output "Spread result used as base for another spread" "113" << 'EOF'
let a = { x: 1, y: 2, z: 3 }
let b = { ...a, x: 10 }
let c = { ...b, y: 100 }
puts(c.x + c.y + c.z)
EOF

########################################
# SECTION W: RECORD RETURNED FROM POLYMORPHIC
# FUNCTION CALLED AT DIFFERENT SITES
########################################

expect_runtime_output "Generic record-returning function called twice" "15" << 'EOF'
let wrap = fn(v) { { val: v } }
let a = wrap(5)
let b = wrap(10)
puts(a.val + b.val)
EOF

# Different field sets at different call sites would require
# row polymorphism in codegen; this tests monomorphization correctness.
expect_runtime_output "Record constructor function used multiple times" "8" << 'EOF'
let mk = fn(x, y) { { x: x, y: y } }
let r1 = mk(3, 5)
puts(r1.x + r1.y)
EOF

suite_end
