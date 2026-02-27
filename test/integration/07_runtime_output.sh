#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

suite_begin "TYPECHECK & CODEGEN INTEGRATION TESTS - RUNTIME OUTPUT"
echo "-- RUNTIME OUTPUT TESTS --"

expect_runtime_output "Print integer" "42" << 'EOF'
puts(42)
EOF

expect_runtime_output "Print string" "hello" << 'EOF'
puts("hello")
EOF

expect_runtime_output "Print boolean" "true" << 'EOF'
puts(true)
EOF

expect_runtime_output "Print enum value with data" "some(42)" << 'EOF'
enum option[a] {
  some(a)
  none
}

let x = option.some(42)
puts(x)
EOF

expect_runtime_output "Print enum with no data" "none" << 'EOF'
enum option[a] {
  some(a)
  none
}

let x: option[int] = option.none()
puts(x)
EOF

expect_runtime_output "Example monkey source builds and runs" $'Monkey\n1\n[Scheme Lisp JavaScript Clojure]\nThorsten Ball - Writing A Compiler In Go\n55' "$(cat "$REPO_ROOT/examples/monkey.mr")"

suite_end
