#!/bin/bash

set -e

INTEGRATION_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$INTEGRATION_DIR/../.." && pwd)"
EXECUTABLE="$REPO_ROOT/_build/default/bin/main.exe"
BUILD_TARGET="./_build/default/bin/main.exe"

PASS=0
FAIL=0
XFAIL=0
XPASS=0
TOTAL=0

ensure_built() {
    if [ "${MARMOSET_SKIP_BUILD:-0}" = "1" ] && [ -x "$EXECUTABLE" ]; then
        return
    fi

    echo "Building project..."
    (cd "$REPO_ROOT" && dune build "$BUILD_TARGET")
}

suite_begin() {
    local title="$1"

    PASS=0
    FAIL=0
    XFAIL=0
    XPASS=0
    TOTAL=0

    ensure_built

    echo "=========================================="
    echo "$title"
    echo "=========================================="
    echo ""
}

suite_end() {
    echo "=========================================="
    local summary="RESULTS: $PASS passed, $FAIL failed"
    if [ "$XFAIL" -gt 0 ] || [ "$XPASS" -gt 0 ]; then
        summary="$summary, $XFAIL xfail, $XPASS xpass"
    fi
    summary="$summary out of $TOTAL tests"
    echo "$summary"
    echo "=========================================="
    if [ "$FAIL" -gt 0 ] || [ "$XPASS" -gt 0 ]; then
        if [ "$XPASS" -gt 0 ]; then
            echo "✗ SUITE FAILED ($XPASS stale xfail markers — remove them)"
        else
            echo "✗ SOME TESTS FAILED"
        fi
        return 1
    fi
    echo "✓ ALL TESTS PASSED"
    return 0
}

########################################
# New canonical helpers (Phase 2)
#
# 1) expect_run  NAME EXPECTED_STDOUT [SOURCE]
# 2) expect_reject NAME EXPECTED_DIAG [SOURCE]
#
# Both accept source either:
# - as the last argument, or
# - from stdin (heredoc/pipe).
#
# expect_run: build + execute + exact stdout equality.
# expect_reject: build must fail.
#   EXPECTED_DIAG="*"  -> any failing build is accepted.
#   otherwise          -> build output must contain EXPECTED_DIAG substring.
########################################

expect_run() {
    local name="$1"
    local expected_output="$2"
    local source="${3:-}"
    if [ -z "$source" ]; then
        source="$(cat)"
    fi

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local tmpfile
    local binpath
    local test_build_dir="$REPO_ROOT/.marmoset/build"
    mkdir -p "$test_build_dir"
    tmpfile=$(mktemp)
    binpath=$(mktemp "$test_build_dir/marmoset_test_bin.XXXXXX")
    rm -f "$binpath"
    echo "$source" > "$tmpfile"

    local build_output
    local output
    if build_output=$($EXECUTABLE build "$tmpfile" -o "$binpath" 2>&1) && output=$("$binpath" 2>&1); then
        if [ "$output" = "$expected_output" ]; then
            echo "✓ PASS"
            PASS=$((PASS + 1))
        else
            echo "✗ FAIL (expected '$expected_output', got '$output')"
            FAIL=$((FAIL + 1))
        fi
    else
        echo "✗ FAIL (build or execution failed)"
        echo "  Build output: $build_output"
        FAIL=$((FAIL + 1))
    fi

    rm -f "$tmpfile" "$binpath"
}

expect_reject() {
    local name="$1"
    local expected_diag="${2:-*}"
    local source="${3:-}"
    if [ -z "$source" ]; then
        source="$(cat)"
    fi

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local tmpfile
    tmpfile=$(mktemp)
    echo "$source" > "$tmpfile"

    local build_output
    if build_output=$($EXECUTABLE build "$tmpfile" 2>&1); then
        echo "✗ FAIL (expected build failure but build succeeded)"
        FAIL=$((FAIL + 1))
    else
        if [ "$expected_diag" = "*" ] || echo "$build_output" | grep -qF "$expected_diag"; then
            echo "✓ PASS"
            PASS=$((PASS + 1))
        else
            echo "✗ FAIL (missing expected diagnostic '$expected_diag')"
            echo "  Output: $build_output"
            FAIL=$((FAIL + 1))
        fi
    fi

    rm -f "$tmpfile"
}

########################################
# Compatibility helpers for older fixture shells
#
# 1) expect_runtime_output
# 2) expect_build
#
# Both accept source either:
# - as the last argument, or
# - from stdin (heredoc/pipe).
########################################

expect_runtime_output() {
    local name="$1"
    local expected_output="$2"
    local source="${3:-}"
    if [ -z "$source" ]; then
        source="$(cat)"
    fi

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local tmpfile
    local binpath
    local test_build_dir="$REPO_ROOT/.marmoset/build"
    mkdir -p "$test_build_dir"
    tmpfile=$(mktemp)
    binpath=$(mktemp "$test_build_dir/marmoset_test_bin.XXXXXX")
    rm -f "$binpath"
    echo "$source" > "$tmpfile"

    local build_output
    local output
    if build_output=$($EXECUTABLE build "$tmpfile" -o "$binpath" 2>&1) && output=$("$binpath" 2>&1); then
        if [ "$output" = "$expected_output" ]; then
            echo "✓ PASS"
            PASS=$((PASS + 1))
        else
            echo "✗ FAIL (expected '$expected_output', got '$output')"
            FAIL=$((FAIL + 1))
        fi
    else
        echo "✗ FAIL (build or execution failed)"
        echo "  Build output: $build_output"
        FAIL=$((FAIL + 1))
    fi

    rm -f "$tmpfile" "$binpath"
}

expect_build() {
    local name="$1"
    local expected_error_fragment="${2:-}"
    local source="${3:-}"
    if [ -z "$source" ]; then
        source="$(cat)"
    fi

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local tmpfile
    tmpfile=$(mktemp)
    echo "$source" > "$tmpfile"

    local build_output
    if build_output=$($EXECUTABLE build "$tmpfile" 2>&1); then
        if [ -z "$expected_error_fragment" ]; then
            echo "✓ PASS"
            PASS=$((PASS + 1))
        else
            echo "✗ FAIL (expected build failure containing '$expected_error_fragment')"
            echo "  Output: $build_output"
            FAIL=$((FAIL + 1))
        fi
    else
        if [ -z "$expected_error_fragment" ]; then
            echo "✗ FAIL (build failed)"
            echo "  Output: $build_output"
            FAIL=$((FAIL + 1))
        elif [ "$expected_error_fragment" = "__ANY_ERROR__" ] || echo "$build_output" | grep -q "$expected_error_fragment"; then
            echo "✓ PASS"
            PASS=$((PASS + 1))
        else
            echo "✗ FAIL (missing expected error fragment '$expected_error_fragment')"
            echo "  Output: $build_output"
            FAIL=$((FAIL + 1))
        fi
    fi

    rm -f "$tmpfile"
}

test_case() {
    # Legacy convenience helper for inline source strings used in older suites.
    # Prefer expect_build / expect_runtime_output in new tests.
    local name="$1"
    local source="$2"
    local should_succeed="$3"  # "true" or "false"
    local expected_output="$4" # optional error fragment for failing cases

    if [ "$should_succeed" = "true" ]; then
        expect_build "$name" "" "$source"
    else
        expect_build "$name" "${expected_output:-__ANY_ERROR__}" "$source"
    fi
}

run_build_ok_not_contains_from_stdin() {
    local name="$1"
    local forbidden_fragment="$2"
    local source
    source="$(cat)"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local tmpfile
    tmpfile=$(mktemp)
    echo "$source" > "$tmpfile"

    if build_output=$($EXECUTABLE build "$tmpfile" 2>&1); then
        if echo "$build_output" | grep -q "$forbidden_fragment"; then
            echo "✗ FAIL (output contains forbidden fragment '$forbidden_fragment')"
            echo "  Output: $build_output"
            FAIL=$((FAIL + 1))
        else
            echo "✓ PASS"
            PASS=$((PASS + 1))
        fi
    else
        echo "✗ FAIL (build failed)"
        echo "  Output: $build_output"
        FAIL=$((FAIL + 1))
    fi

    rm -f "$tmpfile"
}

run_codegen_deterministic_from_stdin() {
    local name="$1"
    local source
    source="$(cat)"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local tmpfile out1 out2 bin1 bin2
    tmpfile=$(mktemp)
    out1=$(mktemp -d marmoset_emit1.XXXXXX)
    out2=$(mktemp -d marmoset_emit2.XXXXXX)
    bin1=$(mktemp ./marmoset_bin1.XXXXXX)
    bin2=$(mktemp ./marmoset_bin2.XXXXXX)
    rm -f "$bin1" "$bin2"
    echo "$source" > "$tmpfile"

    if build1=$($EXECUTABLE build "$tmpfile" --emit-go "$out1" -o "$bin1" 2>&1) && build2=$($EXECUTABLE build "$tmpfile" --emit-go "$out2" -o "$bin2" 2>&1); then
        if diff -u "$out1/main.go" "$out2/main.go" >/dev/null 2>&1 && diff -u "$out1/runtime.go" "$out2/runtime.go" >/dev/null 2>&1; then
            echo "✓ PASS"
            PASS=$((PASS + 1))
        else
            echo "✗ FAIL (emitted Go is not deterministic)"
            FAIL=$((FAIL + 1))
        fi
    else
        echo "✗ FAIL (build failed)"
        if [ -n "$build1" ]; then
            echo "  First build output: $build1"
        fi
        if [ -n "$build2" ]; then
            echo "  Second build output: $build2"
        fi
        FAIL=$((FAIL + 1))
    fi

    rm -f "$tmpfile" "$bin1" "$bin2"
    rm -rf "$out1" "$out2"
}

run_emit_go_not_contains_from_stdin() {
    local name="$1"
    local forbidden_fragment="$2"
    local source
    source="$(cat)"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local tmpfile outdir binpath
    tmpfile=$(mktemp)
    outdir=$(mktemp -d marmoset_emit.XXXXXX)
    binpath=$(mktemp ./marmoset_bin.XXXXXX)
    rm -f "$binpath"
    echo "$source" > "$tmpfile"

    if build_output=$($EXECUTABLE build "$tmpfile" --emit-go "$outdir" -o "$binpath" 2>&1); then
        if rg -n "$forbidden_fragment" "$outdir/main.go" "$outdir/runtime.go" >/dev/null 2>&1; then
            echo "✗ FAIL (emitted Go contains forbidden fragment '$forbidden_fragment')"
            FAIL=$((FAIL + 1))
        else
            echo "✓ PASS"
            PASS=$((PASS + 1))
        fi
    else
        echo "✗ FAIL (build failed)"
        echo "  Output: $build_output"
        FAIL=$((FAIL + 1))
    fi

    rm -f "$tmpfile" "$binpath"
    rm -rf "$outdir"
}

test_emit_go_contains() {
    local name="$1"
    local source="$2"
    local expected_fragment="$3"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local tmpdir_go
    local tmpfile
    tmpdir_go=$(mktemp -d)
    tmpfile=$(mktemp)
    echo "$source" > "$tmpfile"

    if $EXECUTABLE build "$tmpfile" --emit-go "$tmpdir_go" >/dev/null 2>&1 && [ -f "$tmpdir_go/main.go" ]; then
        if grep -q "$expected_fragment" "$tmpdir_go/main.go"; then
            echo "✓ PASS"
            PASS=$((PASS + 1))
        else
            echo "✗ FAIL"
            FAIL=$((FAIL + 1))
        fi
    else
        echo "✗ FAIL (Go source emission failed)"
        FAIL=$((FAIL + 1))
    fi

    rm -rf "$tmpdir_go" "$tmpfile"
}

test_emit_go_exact_snapshot() {
    local name="$1"
    local source_file="$2"
    local snapshot_file="$3"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local outdir binpath diff_file build_output
    outdir=$(mktemp -d marmoset_emit_snapshot.XXXXXX)
    binpath=$(mktemp "$REPO_ROOT/.marmoset/build/marmoset_snapshot_bin.XXXXXX")
    diff_file=$(mktemp)
    rm -f "$binpath"

    if [ ! -f "$source_file" ]; then
        echo "✗ FAIL (missing source file $source_file)"
        FAIL=$((FAIL + 1))
    elif [ ! -f "$snapshot_file" ]; then
        echo "✗ FAIL (missing snapshot file $snapshot_file)"
        FAIL=$((FAIL + 1))
    elif build_output=$($EXECUTABLE build "$source_file" --emit-go "$outdir" -o "$binpath" 2>&1); then
        if diff -u "$snapshot_file" "$outdir/main.go" > "$diff_file"; then
            echo "✓ PASS"
            PASS=$((PASS + 1))
        else
            echo "✗ FAIL (main.go snapshot drift)"
            sed -n '1,80p' "$diff_file" | sed 's/^/  /'
            FAIL=$((FAIL + 1))
        fi
    else
        echo "✗ FAIL (build failed)"
        echo "  Output: $build_output"
        FAIL=$((FAIL + 1))
    fi

    rm -f "$binpath" "$diff_file"
    rm -rf "$outdir"
}
