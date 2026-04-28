#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

SNAPSHOT_ROOT="$REPO_ROOT/test/snapshots/go_exact"

suite_begin "FFI TESTS"

run_project_expect_output() {
    local name="$1"
    local expected_output="$2"
    local setup_project="$3"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local root binpath build_output output
    root=$(mktemp -d marmoset_ffi_project.XXXXXX)
    binpath=$(mktemp "$REPO_ROOT/.marmoset/build/marmoset_ffi_bin.XXXXXX")
    rm -f "$binpath"

    eval "$setup_project"

    if build_output=$($EXECUTABLE build "$root/main.mr" -o "$binpath" 2>&1) && output=$("$binpath" 2>&1); then
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

    rm -f "$binpath"
    rm -rf "$root"
}

run_project_expect_reject() {
    local name="$1"
    local expected_fragment="$2"
    local setup_project="$3"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local root build_output
    root=$(mktemp -d marmoset_ffi_project.XXXXXX)
    eval "$setup_project"

    if build_output=$($EXECUTABLE build "$root/main.mr" 2>&1); then
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

    rm -rf "$root"
}

test_emit_go_exact_snapshot_emit_only() {
    local name="$1"
    local source_file="$2"
    local snapshot_file="$3"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local outdir binpath diff_file build_output
    outdir=$(mktemp -d marmoset_ffi_emit.XXXXXX)
    binpath=$(mktemp "$REPO_ROOT/.marmoset/build/marmoset_ffi_bin.XXXXXX")
    diff_file=$(mktemp)
    rm -f "$binpath"

    build_output=$($EXECUTABLE build "$source_file" --emit-go "$outdir" -o "$binpath" 2>&1) || true
    if [ ! -f "$source_file" ]; then
        echo "✗ FAIL (missing source file $source_file)"
        FAIL=$((FAIL + 1))
    elif [ ! -f "$snapshot_file" ]; then
        echo "✗ FAIL (missing snapshot file $snapshot_file)"
        FAIL=$((FAIL + 1))
    elif [ ! -f "$outdir/main.go" ]; then
        echo "✗ FAIL (Go emission did not produce main.go)"
        echo "  Output: $build_output"
        FAIL=$((FAIL + 1))
    else
        if diff -u "$snapshot_file" "$outdir/main.go" > "$diff_file"; then
            echo "✓ PASS"
            PASS=$((PASS + 1))
        else
            echo "✗ FAIL (main.go snapshot drift)"
            sed -n '1,80p' "$diff_file" | sed 's/^/  /'
            FAIL=$((FAIL + 1))
        fi
    fi

    rm -f "$binpath" "$diff_file"
    rm -rf "$outdir"
}

run_project_emit_count() {
    local name="$1"
    local setup_project="$2"
    local fragment="$3"
    local expected_count="$4"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local root outdir binpath build_output actual_count
    root=$(mktemp -d marmoset_ffi_project.XXXXXX)
    outdir=$(mktemp -d marmoset_ffi_emit.XXXXXX)
    binpath=$(mktemp "$REPO_ROOT/.marmoset/build/marmoset_ffi_bin.XXXXXX")
    rm -f "$binpath"

    eval "$setup_project"

    if build_output=$($EXECUTABLE build "$root/main.mr" --emit-go "$outdir" -o "$binpath" 2>&1); then
        actual_count=$(grep -oF "$fragment" "$outdir/main.go" | wc -l | tr -d ' ')
        if [ "$actual_count" = "$expected_count" ]; then
            echo "✓ PASS"
            PASS=$((PASS + 1))
        else
            echo "✗ FAIL (expected $expected_count occurrences of '$fragment', got $actual_count)"
            FAIL=$((FAIL + 1))
        fi
    else
        echo "✗ FAIL (build failed)"
        echo "  Output: $build_output"
        FAIL=$((FAIL + 1))
    fi

    rm -f "$binpath"
    rm -rf "$root" "$outdir"
}

test_emit_go_exact_snapshot \
    "single-file strings extern emits stable wrapper/import shape" \
    "$REPO_ROOT/test/fixtures/ffi/ffi01_strings_to_upper.mr" \
    "$SNAPSHOT_ROOT/ffi01_strings_to_upper.main.go"

test_emit_go_exact_snapshot \
    "unused extern emits no import or wrapper in stable Go shape" \
    "$REPO_ROOT/test/fixtures/ffi/ffi04_unused_extern.mr" \
    "$SNAPSHOT_ROOT/ffi04_unused_extern.main.go"

test_emit_go_exact_snapshot \
    "multiple extern imports emit sorted stable Go shape" \
    "$REPO_ROOT/test/fixtures/ffi/ffi05_import_sorting.mr" \
    "$SNAPSHOT_ROOT/ffi05_import_sorting.main.go"

test_emit_go_exact_snapshot_emit_only \
    "duplicate package basenames use stable distinct wrappers and aliases" \
    "$REPO_ROOT/test/fixtures/ffi/ffi06_duplicate_basename_packages.mr" \
    "$SNAPSHOT_ROOT/ffi06_duplicate_basename_packages.main.go"

test_emit_go_exact_snapshot_emit_only \
    "sanitized path collisions use stable distinct wrappers and aliases" \
    "$REPO_ROOT/test/fixtures/ffi/ffi07_sanitized_path_collision.mr" \
    "$SNAPSHOT_ROOT/ffi07_sanitized_path_collision.main.go"

expect_runtime_output \
    "strings.ToUpper wrapper compiles and runs" \
    "HELLO" \
    "$(cat "$REPO_ROOT/test/fixtures/ffi/ffi01_strings_to_upper.mr")"

expect_runtime_output \
    "strings.Contains wrapper returns Bool" \
    "true" \
    "$(cat "$REPO_ROOT/test/fixtures/ffi/ffi02_strings_contains.mr")"

expect_runtime_output \
    "path/filepath alias wrapper compiles and runs" \
    "marmoset.txt" \
    "$(cat "$REPO_ROOT/test/fixtures/ffi/ffi03_path_filepath_alias.mr")"

run_project_expect_output \
    "wrapper module exports ordinary API backed by private extern" \
    "HI" \
    'mkdir -p "$root/lib"
cat > "$root/main.mr" <<'"'"'EOF'"'"'
import lib.str
puts(str.upcase("hi"))
EOF
cat > "$root/lib/str.mr" <<'"'"'EOF'"'"'
export upcase
extern "strings" = {
  fn ToUpper(s: Str) -> Str
}
fn upcase(s: Str) -> Str = strings.ToUpper(s)
EOF'

run_project_expect_reject \
    "importer cannot use wrapper module extern qualifier directly" \
    "Unbound variable: strings" \
    'mkdir -p "$root/lib"
cat > "$root/main.mr" <<'"'"'EOF'"'"'
import lib.str
strings.ToUpper("hi")
EOF
cat > "$root/lib/str.mr" <<'"'"'EOF'"'"'
export upcase
extern "strings" = {
  fn ToUpper(s: Str) -> Str
}
fn upcase(s: Str) -> Str = strings.ToUpper(s)
EOF'

run_project_emit_count \
    "matching wrapper module externs dedupe wrapper" \
    'cat > "$root/main.mr" <<'"'"'EOF'"'"'
import a.up
import b.loud
puts(up("hi"))
puts(loud("bye"))
EOF
cat > "$root/a.mr" <<'"'"'EOF'"'"'
export up
extern "strings" = {
  fn ToUpper(s: Str) -> Str
}
fn up(s: Str) -> Str = strings.ToUpper(s)
EOF
cat > "$root/b.mr" <<'"'"'EOF'"'"'
export loud
extern "strings" as text = {
  fn ToUpper(s: Str) -> Str
}
fn loud(s: Str) -> Str = text.ToUpper(s)
EOF' \
    "func extern__strings__ToUpper(s string) string" \
    "1"

run_project_expect_reject \
    "conflicting wrapper module extern signatures are rejected" \
    "Conflicting extern signatures" \
    'cat > "$root/main.mr" <<'"'"'EOF'"'"'
import a.up
import b.bad
puts(up("hi"))
puts(bad(1))
EOF
cat > "$root/a.mr" <<'"'"'EOF'"'"'
export up
extern "strings" = {
  fn ToUpper(s: Str) -> Str
}
fn up(s: Str) -> Str = strings.ToUpper(s)
EOF
cat > "$root/b.mr" <<'"'"'EOF'"'"'
export bad
extern "strings" as text = {
  fn ToUpper(i: Int) -> Str
}
fn bad(i: Int) -> Str = text.ToUpper(i)
EOF'

run_emit_go_not_contains_from_stdin \
    "unused extern does not emit wrapper" \
    "extern__strings__ToUpper" <<'EOF'
extern "strings" = {
  fn ToUpper(s: Str) -> Str
}
"ok"
EOF

run_emit_go_not_contains_from_stdin \
    "unused extern does not emit import" \
    "mext_strings" <<'EOF'
extern "strings" = {
  fn ToUpper(s: Str) -> Str
}
"ok"
EOF

expect_reject \
    "trusted signature mismatch surfaces as Go build diagnostic" \
    "cannot use" <<'EOF'
extern "strings" = {
  fn ToUpper(i: Int) -> Str
}
strings.ToUpper(42)
EOF

suite_end
