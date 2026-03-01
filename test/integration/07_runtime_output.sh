#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

suite_begin "TYPECHECK & CODEGEN INTEGRATION TESTS - RUNTIME OUTPUT"
echo "-- RUNTIME OUTPUT TESTS --"

# Tests migrated to test/fixtures/runtime/:
#   - Print enum value with data          -> print_enum_with_data.mr
#   - Print enum with no data             -> print_enum_no_data.mr
#   - Match arm binding unused            -> match_arm_unused_binding.mr
#   - Primitive match unused binding      -> primitive_match_unused_binding.mr

# Kept in shell: reads external example file, not suitable for fixture model.
expect_runtime_output "Example monkey source builds and runs" $'Thorsten Ball - Writing A Compiler In Go\n[1 1 2 3 5 8]' "$(cat "$REPO_ROOT/examples/monkey.mr")"

suite_end
