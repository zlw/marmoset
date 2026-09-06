#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

SNAPSHOT_ROOT="$REPO_ROOT/test/snapshots/go_tree"
SHIM_RUNTIME_MAIN="$REPO_ROOT/test/fixtures/ffi/shim_runtime/main.mr"
SHIM_FUNCTION_VALUES_MAIN="$REPO_ROOT/test/fixtures/ffi/shim_function_values/main.mr"

suite_begin "FFI TESTS"

run_fixture_expect_output() {
    local name="$1"
    local expected_output="$2"
    local source_file="$3"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local binpath build_output output
    binpath=$(mktemp "$REPO_ROOT/.marmoset/build/marmoset_ffi_bin.XXXXXX")
    rm -f "$binpath"

    if build_output=$($EXECUTABLE build "$source_file" -o "$binpath" 2>&1) && output=$("$binpath" 2>&1); then
        if [ "$output" = "$expected_output" ]; then
            echo "✓ PASS"
            PASS=$((PASS + 1))
        else
            echo "✗ FAIL (expected '$expected_output', got '$output')"
            FAIL=$((FAIL + 1))
        fi
    else
        echo "✗ FAIL (build or execution failed)"
        echo "  Output: $build_output"
        FAIL=$((FAIL + 1))
    fi

    rm -f "$binpath"
}

run_fixture_expect_build_failure() {
    local name="$1"
    local expected_fragment="$2"
    local setup_project="$3"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local root binpath build_output
    root=$(mktemp -d marmoset_ffi_project.XXXXXX)
    binpath=$(mktemp "$REPO_ROOT/.marmoset/build/marmoset_ffi_bin.XXXXXX")
    rm -f "$binpath"

    eval "$setup_project"

    if build_output=$($EXECUTABLE build "$root/main.mr" -o "$binpath" 2>&1); then
        echo "✗ FAIL (expected build failure but build succeeded)"
        FAIL=$((FAIL + 1))
    elif echo "$build_output" | grep -qF "$expected_fragment"; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (missing expected diagnostic '$expected_fragment')"
        echo "  Output: $build_output"
        FAIL=$((FAIL + 1))
    fi

    rm -f "$binpath"
    rm -rf "$root"
}

run_fixture_expect_output \
    "std.bytes shim adapters run through production shims" \
    $'hi\n2\nequal' \
    "$SHIM_RUNTIME_MAIN"

run_fixture_expect_output \
    "exported shim functions work as first-class values" \
    "hi" \
    "$SHIM_FUNCTION_VALUES_MAIN"
rm -f "$REPO_ROOT/.marmoset/build/marmoset_ffi_function_values.tmp"

test_emit_go_tree_snapshot \
    "std.bytes shim adapter Go tree includes runtime, API, and copied shims" \
    "$SHIM_RUNTIME_MAIN" \
    "$SNAPSHOT_ROOT/ffi_shim_runtime.tree"

run_fixture_expect_build_failure \
    "std bytes cannot be constructed in Marmoset" \
    "cannot be constructed" \
    'cat > "$root/main.mr" <<'"'"'EOF'"'"'
import std.bytes
import std.bytes.Bytes
let payload = Bytes(bytes.from_str("x"))
puts(0)
EOF'

run_fixture_expect_build_failure \
    "std bytes cannot expose fields" \
    "has no fields to inspect" \
    'cat > "$root/main.mr" <<'"'"'EOF'"'"'
import std.bytes
let payload = bytes.from_str("x")
puts(payload.data)
EOF'

expect_reject \
    "test shim root is rejected" \
    "allowed root is std" <<'EOF'
extern "test/scalar" = {
  fn upcase(s: Str) -> Str
}
scalar.upcase("hi")
EOF

expect_reject \
    "direct Go package extern ids are rejected" \
    "expected at least two slash-separated segments" <<'EOF'
extern "strings" = {
  fn ToUpper(s: Str) -> Str
}
strings.ToUpper("hi")
EOF

suite_end
