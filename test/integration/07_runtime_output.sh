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

expect_runtime_output "Match arm binding can be unused in effectful arm" "got one" << 'EOF'
enum option[a] {
  some(a)
  none
}

let x = option.some(1)
match x {
  option.some(n): puts("got one")
  option.none(): puts("got none")
}
EOF

expect_runtime_output "Example monkey source builds and runs" $'Thorsten Ball - Writing A Compiler In Go\n[1 1 2 3 5 8]' "$(cat "$REPO_ROOT/examples/monkey.mr")"

suite_end
