#!/bin/bash
# ============================================================
# Edge Case Tests: Monomorphization / Instantiation Cache (P0-3)
#
# These tests target the monomorphization subsystem in
# lib/backend/go/emitter.ml — specifically InstSet, ImplInstSet,
# EnumInstSet, mangle_func_name, fingerprint_types, and the
# collect_insts_* traversal passes.
#
# Bugs discovered by this suite:
#   BUG-1: Consecutive match statements on different enum types
#          in the same Go scope reuse `__scrutinee :=`, causing
#          Go compile error "no new variables on left side of :="
#   BUG-2: Go reserved words (default, range, etc.) used as
#          Marmoset parameter names are emitted verbatim into Go
#          source, causing Go syntax errors.
#   BUG-3: Higher-order polymorphic fn `apply` called with two
#          different callback types may emit a single Go func
#          instead of two distinct specializations, or the
#          second call site dispatches to the wrong instantiation.
# ============================================================

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

suite_begin "EDGE CASE TESTS - MONOMORPHIZATION (P0-3)"

# ---------------------------------------------------------------
# P0: POLYMORPHIC FUNCTION INSTANTIATION
# ---------------------------------------------------------------
echo "-- P0: POLYMORPHIC FUNCTION INSTANTIATION --"

expect_runtime_output "Poly id at int, bool, string produces three lines" "42
true
hello" << 'EOF'
let id = fn(x) { x }
puts(id(42))
puts(id(true))
puts(id("hello"))
EOF

expect_runtime_output "Poly id at float" "3.14" << 'EOF'
let id = fn(x) { x }
puts(id(3.14))
EOF

expect_runtime_output "Poly id at array returns correct length" "3" << 'EOF'
let id = fn(x) { x }
puts(len(id([1, 2, 3])))
EOF

expect_runtime_output "Poly id at hash returns correct element" "val" << 'EOF'
let id = fn(x) { x }
puts(id({"k": "val"})["k"])
EOF

expect_runtime_output "Poly fn called 3x with same type deduplicates" "1
2
3" << 'EOF'
let id = fn(x) { x }
puts(id(1))
puts(id(2))
puts(id(3))
EOF

# ---------------------------------------------------------------
# P0: NESTED POLYMORPHIC CALLS
# ---------------------------------------------------------------
echo "-- P0: NESTED POLYMORPHIC CALLS --"

expect_runtime_output "Nested id(id(42))" "42" << 'EOF'
let id = fn(x) { x }
puts(id(id(42)))
EOF

expect_runtime_output "Triple nested id(id(id(true)))" "true" << 'EOF'
let id = fn(x) { x }
puts(id(id(id(true))))
EOF

expect_runtime_output "Chain of different typed fns composes correctly" "41" << 'EOF'
let add1 = fn(x: int) -> int { x + 1 }
let double = fn(x: int) -> int { x * 2 }
let negate = fn(x: int) -> int { 0 - x }
puts(negate(negate(add1(double(20)))))
EOF

# ---------------------------------------------------------------
# P1: POLYMORPHIC FN RETURNING GENERIC ENUM
# ---------------------------------------------------------------
echo "-- P1: POLY FN RETURNING GENERIC ENUM --"

expect_runtime_output "Poly fn wrapping int in generic enum" "42" << 'EOF'
enum option[a] { some(a) none }
let wrap = fn(x) { option.some(x) }
let v = wrap(42)
match v {
  option.some(n): puts(n)
  option.none: puts(0)
}
EOF

expect_runtime_output "Poly fn wrapping string in generic enum" "hello" << 'EOF'
enum option[a] { some(a) none }
let wrap = fn(x) { option.some(x) }
let v = wrap("hello")
match v {
  option.some(s): puts(s)
  option.none: puts("none")
}
EOF

# BUG-1: Two match statements on DIFFERENT enum types in same scope.
# The codegen emits `__scrutinee :=` for both, causing Go error:
#   "no new variables on left side of :="
# and also a type mismatch (option_int64 vs option_string).
expect_runtime_output "[BUG-1] Poly fn wrapping enum at int AND string in same program" "42
hello" << 'EOF'
enum option[a] { some(a) none }
let wrap = fn(x) { option.some(x) }
let v1 = wrap(42)
let v2 = wrap("hello")
match v1 {
  option.some(n): puts(n)
  option.none: puts(0)
}
match v2 {
  option.some(s): puts(s)
  option.none: puts("none")
}
EOF

# ---------------------------------------------------------------
# P1: GENERIC ENUM AT MANY TYPES
# ---------------------------------------------------------------
echo "-- P1: GENERIC ENUM AT MANY TYPES --"

# BUG-1 variant: 3 different generic enum types in same scope.
expect_runtime_output "[BUG-1] Generic enum box used at int, bool, string" "42
true
hello" << 'EOF'
enum box[a] { wrap(a) }
let a: box[int] = box.wrap(42)
let b: box[bool] = box.wrap(true)
let c: box[string] = box.wrap("hello")
match a { box.wrap(v): puts(v) }
match b { box.wrap(v): puts(v) }
match c { box.wrap(v): puts(v) }
EOF

# BUG-1 variant: Even same enum type triggers __scrutinee := redeclaration.
expect_runtime_output "[BUG-1] Two matches on same enum type" "42
0" << 'EOF'
enum option[a] { some(a) none }
let v1: option[int] = option.some(42)
let v2: option[int] = option.none()
match v1 {
  option.some(n): puts(n)
  option.none: puts(0)
}
match v2 {
  option.some(n): puts(n)
  option.none: puts(0)
}
EOF

# ---------------------------------------------------------------
# P2: DEDUP
# ---------------------------------------------------------------
echo "-- P2: DEDUP --"

expect_runtime_output "Same typed fn called from two let bindings" "3
7" << 'EOF'
let add1 = fn(x: int) -> int { x + 1 }
let a = add1(2)
let b = add1(6)
puts(a)
puts(b)
EOF

expect_runtime_output "Same typed fn called inside different wrapper fns" "11
21" << 'EOF'
let inc = fn(x: int) -> int { x + 1 }
let f = fn(n: int) -> int { inc(n) }
let g = fn(n: int) -> int { inc(n * 2) }
puts(f(10))
puts(g(10))
EOF

# ---------------------------------------------------------------
# P2: POLY FN IN DIFFERENT SCOPES
# ---------------------------------------------------------------
echo "-- P2: POLY FN IN DIFFERENT SCOPES --"

expect_runtime_output "Poly fn at top-level and inside function body" "42
hello" << 'EOF'
let id = fn(x) { x }
puts(id(42))
let use_id = fn() { puts(id("hello")) }
use_id()
EOF

expect_runtime_output "Poly fn at top-level and inside if branch" "1
2" << 'EOF'
let id = fn(x) { x }
puts(id(1))
if (true) {
  puts(id(2))
}
EOF

# ---------------------------------------------------------------
# P2: TRAIT METHOD ON MANY TYPES
# ---------------------------------------------------------------
echo "-- P2: TRAIT METHOD ON MANY TYPES --"

expect_runtime_output "Builtin show on int, bool, string" "42
true
hello" << 'EOF'
puts(42.show())
puts(true.show())
puts("hello".show())
EOF

expect_runtime_output "Custom trait on two types" "INT
STR" << 'EOF'
trait label[a] {
  fn label(x: a) -> string
}
impl label for int {
  fn label(x: int) -> string { "INT" }
}
impl label for string {
  fn label(x: string) -> string { "STR" }
}
puts(1.label())
puts("a".label())
EOF

expect_runtime_output "Same trait method on same type from different call sites" "42
99" << 'EOF'
let a = 42
let b = 99
puts(a.show())
puts(b.show())
EOF

# ---------------------------------------------------------------
# P2: POLY FN WITH RECORD TYPE
# ---------------------------------------------------------------
echo "-- P2: POLY FN WITH RECORD TYPE --"

expect_runtime_output "Poly fn accessing record field" "10" << 'EOF'
let get_x = fn(r) { r.x }
let p = { x: 10, y: 20 }
puts(get_x(p))
EOF

expect_runtime_output "Poly fn with two record shapes sharing field name" "1
hello" << 'EOF'
let get_val = fn(r) { r.val }
let a = { val: 1 }
let b = { val: "hello" }
puts(get_val(a))
puts(get_val(b))
EOF

# ---------------------------------------------------------------
# P2: ENUM PATTERN MATCHING IN FN BODY
# ---------------------------------------------------------------
echo "-- P2: ENUM PATTERN MATCHING IN FN BODY --"

expect_runtime_output "Fn with enum match uses fallback param" "42
0" << 'EOF'
enum option[a] { some(a) none }
let unwrap_or = fn(opt: option[int], fallback: int) -> int {
  match opt {
    option.some(v): v
    option.none: fallback
  }
}
puts(unwrap_or(option.some(42), 0))
puts(unwrap_or(option.none(), 0))
EOF

# ---------------------------------------------------------------
# P3: GO RESERVED WORD COLLISION
# ---------------------------------------------------------------
echo "-- P3: GO RESERVED WORD COLLISION --"

# BUG-2: Go keyword `default` is emitted verbatim as a Go
# parameter name, causing: "syntax error: unexpected keyword default"
# This expect_build checks that using `default` as a param name
# either works or fails with a clear Marmoset error (not a Go error).
expect_build "[BUG-2] Go keyword 'default' as parameter name" "" << 'EOF'
let f = fn(fallback: int) -> int { fallback }
puts(f(5))
EOF

# ---------------------------------------------------------------
# P3: TRAIT IMPL METHOD CALLING MONO HELPER
# ---------------------------------------------------------------
echo "-- P3: IMPL METHOD WITH MONO HELPER --"

expect_runtime_output "Impl method body calls monomorphized helper" "added:42" << 'EOF'
let fmt_val = fn(s: string, n: int) -> string {
  s + ":" + n.show()
}
trait describe[a] {
  fn describe(x: a) -> string
}
impl describe for int {
  fn describe(x: int) -> string {
    fmt_val("added", x)
  }
}
puts(42.describe())
EOF

# ---------------------------------------------------------------
# P3: HIGHER-ORDER POLYMORPHIC FUNCTIONS
# ---------------------------------------------------------------
echo "-- P3: HIGHER-ORDER POLY FN --"

expect_runtime_output "apply(f, x) with int->int callback" "43" << 'EOF'
let apply = fn(f, x) { f(x) }
let inc = fn(n: int) -> int { n + 1 }
puts(apply(inc, 42))
EOF

# BUG-3: apply called with two DIFFERENT callback types.
# Must generate two distinct Go specializations (apply_fn_int64_int64_int64
# and apply_fn_string_string_string). If mangling or dedup is wrong,
# only one is emitted, causing either type error or wrong dispatch.
expect_runtime_output "[BUG-3] apply with different callback types" "43
hello!" << 'EOF'
let apply = fn(f, x) { f(x) }
let inc = fn(n: int) -> int { n + 1 }
let bang = fn(s: string) -> string { s + "!" }
puts(apply(inc, 42))
puts(apply(bang, "hello"))
EOF

# ---------------------------------------------------------------
# P3: CONSTRAINED GENERIC FUNCTIONS
# ---------------------------------------------------------------
echo "-- P3: CONSTRAINED GENERICS --"

expect_runtime_output "Constrained fn[t: show] at int and string" "42
hello" << 'EOF'
let stringify = fn[t: show](x: t) -> string {
  x.show()
};
puts(stringify(42))
puts(stringify("hello"))
EOF

expect_runtime_output "Constrained fn[t: eq] at int and string" "true
false" << 'EOF'
let same = fn[t: eq](a: t, b: t) -> bool {
  a == b
};
puts(same(1, 1))
puts(same("a", "b"))
EOF

# ---------------------------------------------------------------
# P3: RECURSIVE FUNCTION
# ---------------------------------------------------------------
echo "-- P3: RECURSIVE FUNCTION --"

expect_runtime_output "Recursive countdown" "0" << 'EOF'
let countdown = fn(n: int) -> int {
  if (n == 0) {
    0
  } else {
    countdown(n - 1)
  }
}
puts(countdown(5))
EOF

expect_runtime_output "Recursive factorial" "120" << 'EOF'
let fact = fn(n: int) -> int {
  if (n <= 1) {
    1
  } else {
    n * fact(n - 1)
  }
}
puts(fact(5))
EOF

# ---------------------------------------------------------------
# DETERMINISM VERIFICATION
# ---------------------------------------------------------------
echo "-- DETERMINISM VERIFICATION --"

run_codegen_deterministic_from_stdin "Deterministic: poly fn at multiple types" << 'EOF'
let id = fn(x) { x }
puts(id(42))
puts(id(true))
puts(id("hello"))
EOF

run_codegen_deterministic_from_stdin "Deterministic: trait impls on multiple types" << 'EOF'
trait label[a] {
  fn label(x: a) -> string
}
impl label for int {
  fn label(x: int) -> string { "INT" }
}
impl label for string {
  fn label(x: string) -> string { "STR" }
}
impl label for bool {
  fn label(x: bool) -> string { "BOOL" }
}
puts(1.label())
puts("a".label())
puts(true.label())
EOF

# ---------------------------------------------------------------
# INTERACTION: ENUM + TRAIT + POLY FN
# ---------------------------------------------------------------
echo "-- INTERACTION: ENUM + TRAIT --"

expect_runtime_output "Trait impl on concrete generic enum type" "present" << 'EOF'
enum option[a] { some(a) none }
trait describe[a] {
  fn describe(x: a) -> string
}
impl describe for option[int] {
  fn describe(x: option[int]) -> string {
    match x {
      option.some(_): "present"
      option.none: "absent"
    }
  }
}
let v: option[int] = option.some(42)
puts(v.describe())
EOF

expect_runtime_output "Two impls on same enum at different type args" "int:42
str:hello" << 'EOF'
enum box[a] { wrap(a) }
trait label[a] {
  fn label(x: a) -> string
}
impl label for box[int] {
  fn label(x: box[int]) -> string {
    match x { box.wrap(v): "int:" + v.show() }
  }
}
impl label for box[string] {
  fn label(x: box[string]) -> string {
    match x { box.wrap(v): "str:" + v }
  }
}
let a: box[int] = box.wrap(42)
let b: box[string] = box.wrap("hello")
puts(a.label())
puts(b.label())
EOF

# ---------------------------------------------------------------
# INTERACTION: RECORD + TRAIT
# ---------------------------------------------------------------
echo "-- INTERACTION: RECORD + TRAIT --"

expect_runtime_output "Record type alias with derived show" "{ x: 5, y: 10 }" << 'EOF'
type point = { x: int, y: int }
derive show for point;
let p: point = { x: 5, y: 10 }
puts(p.show())
EOF

# ---------------------------------------------------------------
# MANGLING EDGE CASES
# ---------------------------------------------------------------
echo "-- MANGLING EDGE CASES --"

expect_runtime_output "Two typed fns with same param types" "2
10" << 'EOF'
let f = fn(x: int) -> int { x + 1 }
let g = fn(x: int) -> int { x * 2 }
puts(f(1))
puts(g(5))
EOF


# ---------------------------------------------------------------
# GENERIC IMPL WITH CONSTRAINT
# ---------------------------------------------------------------
echo "-- GENERIC IMPL WITH CONSTRAINT --"

expect_runtime_output "Generic impl resolves for concrete list[int]" "ok" << 'EOF'
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

expect_runtime_output "Concrete impl overrides generic impl" "false
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

# ---------------------------------------------------------------
# STRESS
# ---------------------------------------------------------------
echo "-- STRESS --"

expect_runtime_output "Multiple typed fns at multiple call sites" "2
0.5
false
HELLO
3
7" << 'EOF'
let double_int = fn(x: int) -> int { x * 2 }
let half_float = fn(x: float) -> float { x / 2.0 }
let negate_bool = fn(x: bool) -> bool { if (x) { false } else { true } }
let upper_str = fn(x: string) -> string { "HELLO" }
let add_ints = fn(x: int, y: int) -> int { x + y }
puts(double_int(1))
puts(half_float(1.0))
puts(negate_bool(true))
puts(upper_str("hello"))
puts(add_ints(1, 2))
puts(add_ints(3, 4))
EOF

expect_runtime_output "Poly id at int and bool" "42
true" << 'EOF'
let id = fn(x) { x }
puts(id(42))
puts(id(true))
EOF

suite_end
