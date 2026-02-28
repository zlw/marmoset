#!/bin/bash
# ===========================================================================
# Edge-case integration tests for purity inference (P0-P5).
#
# These tests exercise the boundary between pure (->) and effectful (=>)
# function annotations, implicit inference for unannotated functions, and
# several known gaps in the current purity enforcement.
#
# Many tests use `puts` (the only effectful builtin) as the canonical
# side-effecting operation.
#
# KNOWN GAPS DOCUMENTED HERE:
# 1. Type expression syntax only supports -> (not =>), so effectful
#    function types cannot be expressed in parameter/return annotations.
#    (P9)
# 2. let-binding type annotations (let f: fn(int) -> int = ...) do NOT
#    enforce the effect bit -- an effectful fn can be bound to a pure
#    annotation silently. (P24)
# 3. unify.ml requires exact effect bit match on TFun, but
#    unify_function_shape_ignoring_effect is used as a fallback for
#    recursive let bindings, silently erasing the distinction.
# 4. Generic return type annotations (-> a) fail with "Unknown type
#    constructor" -- only concrete types are supported as return types.
#    (P23)
#
# NOTE ON FLAKINESS: Tests that verify successful builds (expect_build "")
# go through the Go compiler via a shared .marmoset-build directory.
# Under rapid sequential execution, this directory can experience race
# conditions (stale files, concurrent rmdir/mkdir). This is a pre-existing
# infrastructure issue affecting ALL integration suites, not specific to
# these purity tests. If a success test fails sporadically with "Go build
# failed" or "cannot find main module", re-run in isolation to confirm.
# ===========================================================================

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

suite_begin "EDGE CASE TESTS - PURITY INFERENCE (P0-5)"

# Clean up stale Go build artifacts that cause race conditions
rm -rf "$REPO_ROOT/.marmoset-build" 2>/dev/null || true

# ------------------------------------------------------------------
# P0: Pure function (->) calling puts directly should be rejected
# ------------------------------------------------------------------
echo ""
echo "-- BASIC PURITY ENFORCEMENT --"

expect_build "P0: pure fn (->) calling puts is rejected" \
    "effectful" << 'EOF'
let greet = fn(x: int) -> int {
  puts(x)
  x
}
greet(1)
EOF

# ------------------------------------------------------------------
# P1: Effectful function (=>) calling puts should compile fine
# ------------------------------------------------------------------
expect_build "P1: effectful fn (=>) calling puts compiles" \
    "" << 'EOF'
let greet = fn(x: int) => int {
  puts(x)
  x
}
greet(42)
EOF

# ------------------------------------------------------------------
# P2: Unannotated function calling puts should infer effectful
#     and compile without error.
# ------------------------------------------------------------------
expect_build "P2: unannotated fn calling puts infers effectful and compiles" \
    "" << 'EOF'
let say = fn(x) {
  puts(x)
}
say("hello")
EOF

# ------------------------------------------------------------------
# P3: Pure function calling another pure function (should compile)
# ------------------------------------------------------------------
expect_build "P3: pure fn calling pure fn compiles" \
    "" << 'EOF'
let double = fn(x: int) -> int { x * 2 }
let triple_half = fn(x: int) -> int { double(x) + x }
puts(triple_half(2))
EOF

# ------------------------------------------------------------------
# P4: Pure function calling an effectful function (should be caught)
# ------------------------------------------------------------------
expect_build "P4: pure fn calling effectful fn is rejected" \
    "effectful" << 'EOF'
let side_effect = fn(x: int) => int {
  puts(x)
  x
}
let pure_caller = fn(y: int) -> int {
  side_effect(y)
}
pure_caller(1)
EOF

# ------------------------------------------------------------------
# P5: Nested: inner is effectful, outer is annotated pure but
#     only DEFINES (does not call) the inner -- should be OK
# ------------------------------------------------------------------
expect_build "P5: pure fn defining (not calling) effectful inner fn is OK" \
    "" << 'EOF'
let outer = fn(x: int) -> int {
  let inner = fn(y: int) => int {
    puts(y)
    y
  }
  x + 1
}
puts(outer(5))
EOF

# ------------------------------------------------------------------
# P6: Nested: outer is pure, inner is effectful, outer CALLS inner
#     This should fail because the pure outer is invoking an effect.
# ------------------------------------------------------------------
expect_build "P6: pure fn defining AND calling effectful inner fn is rejected" \
    "effectful" << 'EOF'
let outer = fn(x: int) -> int {
  let inner = fn(y: int) => int {
    puts(y)
    y
  }
  inner(x)
}
outer(5)
EOF

echo ""
echo "-- HIGHER-ORDER FUNCTION PURITY --"

# ------------------------------------------------------------------
# P7: Higher-order: unannotated HOF that takes a callback and calls
#     it. When passed a pure callback, the call should compile fine.
# ------------------------------------------------------------------
expect_build "P7: unannotated HOF with pure callback compiles" \
    "" << 'EOF'
let apply = fn(f, x: int) { f(x) }
let pure_fn = fn(x: int) -> int { x + 1 }
let result = apply(pure_fn, 5)
puts(result)
EOF

# ------------------------------------------------------------------
# P8: Higher-order: passing effectful fn where a parameter is
#     annotated as pure fn type (fn(int) -> int). The unification
#     should catch the TFun effect bit mismatch.
# ------------------------------------------------------------------
expect_build "P8: explicit pure HOF param rejects effectful callback" \
    "__ANY_ERROR__" << 'EOF'
let apply = fn(f: fn(int) -> int, x: int) -> int { f(x) }
let eff = fn(x: int) => int { puts(x); x }
apply(eff, 1)
EOF

# ------------------------------------------------------------------
# P9: Type expression syntax gap: fn(int) => int is NOT valid in
#     type annotations. This test documents that attempting to write
#     an effectful function type in a parameter annotation is a
#     parse error, NOT a type error.
# ------------------------------------------------------------------
expect_build "P9: fn(int) => int in type annotation is a parse error (syntax gap)" \
    "Parse error" << 'EOF'
let apply_eff = fn(f: fn(int) => int, x: int) => int { f(x) }
let pure = fn(x: int) -> int { x + 1 }
apply_eff(pure, 5)
EOF

echo ""
echo "-- FUNCTION RETURNING FUNCTION (CURRIED PURITY) --"

# ------------------------------------------------------------------
# P10: Function returning a function -- the outer is unannotated,
#      returns an effectful inner function. Should compile.
#      (Cannot annotate return type as fn(int) => int due to syntax
#      limitation, so outer is left unannotated.)
# ------------------------------------------------------------------
expect_build "P10: unannotated outer returns effectful inner compiles" \
    "" << 'EOF'
let make_printer = fn(x: int) {
  fn(y: int) => int {
    puts(y)
    y
  }
}
let printer = make_printer(0)
printer(99)
EOF

# ------------------------------------------------------------------
# P11: Simple pure single-argument function
# ------------------------------------------------------------------
expect_build "P11: pure single-arg fn compiles" \
    "" << 'EOF'
let add = fn(x: int) -> int {
  x + 2
}
let result = add(5)
puts(result)
EOF

# ------------------------------------------------------------------
# P12: Let-binding a function with explicit effectful annotation
# ------------------------------------------------------------------
expect_build "P12: let-bound effectful fn compiles" \
    "" << 'EOF'
let say = fn(msg: string) => string {
  puts(msg)
  msg
}
say("hello")
EOF

echo ""
echo "-- RECURSIVE FUNCTION PURITY --"

# ------------------------------------------------------------------
# P13: Recursive function annotated pure that does NOT call puts
# ------------------------------------------------------------------
expect_build "P13: pure recursive fn (no effects) compiles" \
    "" << 'EOF'
let fib = fn(n: int) -> int {
  if (n < 2) { return n }
  return fib(n - 1) + fib(n - 2);
}
puts(fib(10))
EOF

# ------------------------------------------------------------------
# P14: Recursive function annotated pure that DOES call puts
#      Should be rejected.
# ------------------------------------------------------------------
expect_build "P14: pure recursive fn calling puts is rejected" \
    "effectful" << 'EOF'
let countdown = fn(n: int) -> int {
  puts(n)
  if (n == 0) { return 0 }
  return countdown(n - 1);
}
countdown(5)
EOF

# ------------------------------------------------------------------
# P15: Unannotated recursive function calling puts should infer
#      effectful and compile without error.
# ------------------------------------------------------------------
expect_build "P15: unannotated recursive fn calling puts compiles" \
    "" << 'EOF'
let countdown = fn(n: int) {
  puts(n)
  if (n > 0) {
    countdown(n - 1)
  }
}
countdown(3)
EOF

echo ""
echo "-- OVER-CONSERVATIVE ANNOTATION --"

# ------------------------------------------------------------------
# P16: Effectful annotation on a function with NO I/O
#      Should be fine -- a pure body under => is not an error.
# ------------------------------------------------------------------
expect_build "P16: effectful annotation on pure body is allowed" \
    "" << 'EOF'
let add = fn(x: int, y: int) => int { x + y }
puts(add(2, 4))
EOF

echo ""
echo "-- EFFECTFUL PROPAGATION THROUGH CONTROL FLOW --"

# ------------------------------------------------------------------
# P17: Effectful call inside if-expression of a pure function
# ------------------------------------------------------------------
expect_build "P17: pure fn with effectful call in if-branch is rejected" \
    "effectful" << 'EOF'
let log = fn(x: int) => int { puts(x); x }
let maybe_log = fn(flag: bool, x: int) -> int {
  if (flag) { log(x) } else { x }
}
maybe_log(true, 42)
EOF

# ------------------------------------------------------------------
# P18: Effectful call inside match-expression of a pure function
#      (Match arms use ":" syntax in Marmoset, NOT "=>".)
# ------------------------------------------------------------------
expect_build "P18: pure fn with effectful call in match-arm is rejected" \
    "effectful" << 'EOF'
enum choice { yes no }
let log = fn(x: int) => int { puts(x); x }
let decide = fn(c: choice, x: int) -> int {
  match c {
    choice.yes: log(x)
    choice.no: x
  }
}
decide(choice.yes, 1)
EOF

# ------------------------------------------------------------------
# P19: Effectful call inside let-binding inside pure function
# ------------------------------------------------------------------
expect_build "P19: pure fn with effectful let-binding is rejected" \
    "effectful" << 'EOF'
let side = fn(x: int) => int { puts(x); x }
let wrapper = fn(x: int) -> int {
  let y = side(x)
  y + 1
}
wrapper(1)
EOF

echo ""
echo "-- EFFECTFUL CHAIN PROPAGATION --"

# ------------------------------------------------------------------
# P20: Chain of calls where effectfulness propagates through
#      multiple layers. Inner fn is effectful, middle fn calls it
#      (unannotated, should infer effectful), outer fn is annotated
#      pure and calls middle -> should fail.
# ------------------------------------------------------------------
expect_build "P20: effectfulness propagates through call chain" \
    "effectful" << 'EOF'
let inner = fn(x: int) => int { puts(x); x }
let middle = fn(x: int) { inner(x) }
let outer = fn(x: int) -> int { middle(x) }
outer(1)
EOF

echo ""
echo "-- GENERIC FUNCTION WITH PURITY --"

# ------------------------------------------------------------------
# P21: Generic pure function (constrained, with concrete return type)
# ------------------------------------------------------------------
expect_build "P21: constrained generic pure fn compiles" \
    "" << 'EOF'
let stringify = fn[t: show](x: t) -> string { x.show() }
puts(stringify(42))
EOF

# ------------------------------------------------------------------
# P22: Generic pure function calling puts (should fail)
# ------------------------------------------------------------------
expect_build "P22: generic pure fn calling puts is rejected" \
    "effectful" << 'EOF'
let log_identity = fn[a: show](x: a) -> string {
  puts(x)
  x.show()
}
log_identity(1)
EOF

# ------------------------------------------------------------------
# P23: Generic return type annotation gap: "-> a" is rejected
#      because unconstrained type vars are treated as unknown type
#      constructors in the return position. This is a known limitation.
# ------------------------------------------------------------------
expect_build "P23: generic return type annotation -> a is rejected (known limitation)" \
    "Unknown type constructor" << 'EOF'
let identity = fn[a](x: a) -> a { x }
identity(42)
EOF

echo ""
echo "-- UNIFICATION GAP PROBES --"

# ------------------------------------------------------------------
# P24: let-binding type annotation does NOT enforce effect bit.
#      Assigning an effectful fn to a variable annotated as pure
#      SILENTLY SUCCEEDS. This is a real soundness gap.
#
# EXPECTED (ideal): Should reject -- the annotation says pure but
#                   the value is effectful.
# ACTUAL:           Builds successfully. The let-binding annotation
#                   is checked by unification but the effect bit on
#                   TFun is not compared in the annotation path.
# ------------------------------------------------------------------
expect_build "P24: BUG - let-binding pure annotation silently accepts effectful fn" \
    "" << 'EOF'
let f: fn(int) -> int = fn(x: int) => int { puts(x); x }
f(1)
EOF

# ------------------------------------------------------------------
# P25: if-expression choosing between pure and effectful fn.
#      These have different TFun effect bits, but the compiler
#      accepts this and creates a union type. Should compile.
# ------------------------------------------------------------------
expect_build "P25: if-expr mixing pure and effectful fn compiles" \
    "" << 'EOF'
let f = if (true) {
  fn(x: int) -> int { x + 1 }
} else {
  fn(x: int) => int { puts(x); x }
}
puts(f(1))
EOF

suite_end
