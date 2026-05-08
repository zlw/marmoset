#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

SNAPSHOT_ROOT="$REPO_ROOT/test/snapshots/go_tree"
SHIM_RUNTIME_MAIN="$REPO_ROOT/test/fixtures/ffi/shim_runtime/main.mr"

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

run_fixture_expect_runtime_failure() {
    local name="$1"
    local expected_fragment="$2"
    local setup_project="$3"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local root binpath build_output output
    root=$(mktemp -d marmoset_ffi_project.XXXXXX)
    binpath=$(mktemp "$REPO_ROOT/.marmoset/build/marmoset_ffi_bin.XXXXXX")
    rm -f "$binpath"

    eval "$setup_project"

    if build_output=$($EXECUTABLE build "$root/main.mr" -o "$binpath" 2>&1); then
        if output=$("$binpath" 2>&1); then
            echo "✗ FAIL (expected runtime failure but program succeeded)"
            FAIL=$((FAIL + 1))
        elif echo "$output" | grep -qF "$expected_fragment"; then
            echo "✓ PASS"
            PASS=$((PASS + 1))
        else
            echo "✗ FAIL (missing expected runtime fragment '$expected_fragment')"
            echo "  Output: $output"
            FAIL=$((FAIL + 1))
        fi
    else
        echo "✗ FAIL (build failed)"
        echo "  Output: $build_output"
        FAIL=$((FAIL + 1))
    fi

    rm -f "$binpath"
    rm -rf "$root"
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

copy_shim_fixture_project() {
    local root="$1"
    mkdir -p "$root"
    cp -R "$REPO_ROOT/test/fixtures/ffi/shim_runtime/." "$root/"
}

run_fixture_expect_output \
    "shim adapters run scalar, Option, Result, and owner enum calls" \
    $'HI\n42\n41\nmessage:flipped' \
    "$SHIM_RUNTIME_MAIN"

test_emit_go_tree_snapshot \
    "shim adapter Go tree includes runtime, API, and copied shims" \
    "$SHIM_RUNTIME_MAIN" \
    "$SNAPSHOT_ROOT/ffi_shim_runtime.tree"

run_fixture_expect_runtime_failure \
    "zero-value Result returned by shim is an ABI runtime failure" \
    "invalid zero-value Result returned by shim" \
    'copy_shim_fixture_project "$root"
cat > "$root/main.mr" <<'"'"'EOF'"'"'
import test.result
puts(Result.value_or(result.invalid(), 0))
EOF'

run_fixture_expect_runtime_failure \
    "nil enum returned by shim is an ABI runtime failure" \
    "nil enum returned by shim" \
    'copy_shim_fixture_project "$root"
cat > "$root/main.mr" <<'"'"'EOF'"'"'
import test.status
puts(status.label(status.nil_status()))
EOF'

run_fixture_expect_build_failure \
    "wrong shim signature fails at Go compile time" \
    "cannot use" \
    'mkdir -p "$root/test"
cat > "$root/main.mr" <<'"'"'EOF'"'"'
import test.scalar
puts(scalar.upcase(1))
EOF
cat > "$root/test/scalar.mr" <<'"'"'EOF'"'"'
export upcase
extern "test/scalar" = {
  fn upcase(i: Int) -> Str
}
fn upcase(i: Int) -> Str = scalar.upcase(i)
EOF'

expect_reject \
    "direct Go package extern ids are rejected" \
    "expected at least two slash-separated segments" <<'EOF'
extern "strings" = {
  fn ToUpper(s: Str) -> Str
}
strings.ToUpper("hi")
EOF

suite_end
