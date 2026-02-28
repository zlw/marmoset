#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

suite_begin "TYPECHECK & CODEGEN INTEGRATION TESTS - CODEGEN P0"
echo "-- P0.1: TYPE MAP COMPLETENESS (NO EMITTER RE-INFERENCE) --"

expect_runtime_output "Top-level function calling top-level function compiles and runs" "1" << 'EOF'
let f = fn(x: int) -> int { x }
let g = fn(y: int) -> int { f(y) }
puts(g(1))
EOF

expect_runtime_output "Impl method calling union-param helper resolves using typed env" "int" << 'EOF'
let helper = fn(x: int | string) -> string {
  if (x is int) { "int" } else { "string" }
}
trait show[a] { fn show(x: a) -> string }
impl show for int {
  fn show(x: int) -> string { helper(x) }
}
puts(1.show())
EOF

run_build_ok_not_contains_from_stdin "Successful build emits no missing-type warning text" "missing type for expression id" << 'EOF'
let f = fn(x: int) -> int { x + 1 }
puts(f(1))
EOF

expect_build "Specialized body codegen failure is surfaced cleanly" "Codegen error: Multiple patterns per arm not yet supported in codegen" << 'EOF'
let f = fn(x: int) -> int {
  match x {
    1 | 2: 1
    _: 0
  }
}
puts(f(1))
EOF

run_codegen_deterministic_from_stdin "Codegen output is deterministic for identical input" << 'EOF'
let f = fn(x: int) -> int { x + 1 }
let g = fn(y: int) -> int { f(y) }
puts(g(1))
EOF

test_emit_go_contains "Enum String default branch panics on invalid tag" \
    'enum status { ok fail }
     let x = status.ok
     puts(x)' \
    'panic("unreachable: invalid enum tag")'

run_emit_go_not_contains_from_stdin "Type-check if in let binding avoids IIFE in emitted Go" "func\\(\\) int64" << 'EOF'
let x: int | string = 1
let y = if (x is int) { x + 1 } else { 0 }
puts(y)
EOF

run_emit_go_not_contains_from_stdin "Type-check if in tail position avoids IIFE in emitted Go" "func\\(\\) int64" << 'EOF'
let f = fn(x: int | string) -> int {
  if (x is int) { x + 1 } else { 0 }
}
puts(f(1))
EOF

run_emit_go_not_contains_from_stdin "Match statement avoids IIFE in emitted Go" "func\\(\\) int64" << 'EOF'
match 1 {
  1: 10
  _: 20
}
puts(1)
EOF

expect_build "Tail if without else (unit) compiles" "" << 'EOF'
let f = fn(x: bool) { if (x) { 1 } }
puts(f(true))
EOF

run_emit_go_not_contains_from_stdin "Tail if without else avoids IIFE in emitted Go" "func\\(\\) struct\\{\\}" << 'EOF'
let f = fn(x: bool) { if (x) { 1 } }
puts(f(true))
EOF

expect_build "Type-check if without else in let binding compiles" "" << 'EOF'
let x: int | string = 1
let y = if (x is int) { x + 1 }
puts(y)
EOF

run_emit_go_not_contains_from_stdin "Nested if in tail position avoids IIFE in emitted Go" "func\\(\\) int64" << 'EOF'
let f = fn(a: bool, b: bool) -> int {
  if (a) { if (b) { 1 } else { 2 } } else { 3 }
}
puts(f(true, false))
EOF

run_emit_go_not_contains_from_stdin "Nested match in tail position avoids IIFE in emitted Go" "func\\(\\) int64" << 'EOF'
let f = fn(x: int) -> int {
  match x { 0: match x { 0: 10 _: 20 } _: 30 }
}
puts(f(0))
EOF

run_emit_go_not_contains_from_stdin "Match statement with nested if avoids IIFE in emitted Go" "func\\(\\) int64" << 'EOF'
match 1 {
  1: if (true) { 10 } else { 20 }
  _: 0
}
puts(1)
EOF

run_emit_go_not_contains_from_stdin "Type-check if branch with nested match avoids IIFE in emitted Go" "func\\(\\) int64" << 'EOF'
let x: int | string = 1
let y = if (x is int) {
  match x { 1: 10 _: 20 }
} else {
  0
}
puts(y)
EOF

run_emit_go_not_contains_from_stdin "Nested if containing type-check if avoids IIFE in emitted Go" "func\\(\\) int64" << 'EOF'
let f = fn(x: int | string, y: bool) -> int {
  if (y) {
    if (x is int) { x } else { 0 }
  } else {
    0
  }
}
puts(f(1, true))
EOF

run_emit_go_not_contains_from_stdin "Match in let binding with type-check if arm avoids IIFE in emitted Go" "func\\(\\) int64" << 'EOF'
let x: int | string = 1
let tag = 1
let y = match tag {
  1: if (x is int) { x + 1 } else { 0 }
  _: 0
}
puts(y)
EOF

expect_build "Instantiation fingerprint collision surfaces as clear codegen error" "Codegen error: instantiation fingerprint collision for 'id'" << 'EOF'
enum b { v }
enum a[t] { wrap(t) }
enum a_b { wrap }
let id = fn(x) { x }
let x = id(a_b.wrap)
let y = id(a.wrap(b.v))
puts(1)
EOF

expect_build "Impl instantiation fingerprint collision surfaces as clear codegen error" "Codegen error: impl instantiation fingerprint collision for 'show.show'" << 'EOF'
enum b { v }
enum a[t] { wrap(t) }
enum a_b { wrap }
trait show[x] { fn show(x: x) -> string }
impl show for a_b {
  fn show(x: a_b) -> string { "AB" }
}
impl show for a[b] {
  fn show(x: a[b]) -> string { "AofB" }
}
puts(1)
EOF

expect_build "Duplicate top-level function name/arity is rejected with clear diagnostic" "Codegen error: ambiguous function reference 'f/1'" << 'EOF'
let f = fn(x: int) -> int { x + 1 }
let f = fn(x: int) -> int { x + 2 }
puts(f(1))
EOF


suite_end
