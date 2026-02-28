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

expect_runtime_output "Derive ord for record" "0" << 'EOF'
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


suite_end
