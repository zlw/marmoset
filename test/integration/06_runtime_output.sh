#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

suite_begin "TYPECHECK & CODEGEN INTEGRATION TESTS - RUNTIME OUTPUT"
echo "-- RUNTIME OUTPUT TESTS --"

run_case_from_stdin "Print integer" "42" << 'EOF'
puts(42)
EOF

run_case_from_stdin "Print string" "hello" << 'EOF'
puts("hello")
EOF

run_case_from_stdin "Print boolean" "true" << 'EOF'
puts(true)
EOF

run_case_from_stdin "Print enum value with data" "some(42)" << 'EOF'
enum option[a] {
  some(a)
  none
}

let x = option.some(42)
puts(x)
EOF

run_case_from_stdin "Print enum with no data" "none" << 'EOF'
enum option[a] {
  some(a)
  none
}

let x: option[int] = option.none()
puts(x)
EOF

suite_end
