#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

suite_begin "CLI SUBCOMMAND TESTS"

installed_root=""
installed_install_error=""

ensure_installed_toolchain() {
    if [ -n "$installed_root" ] && [ -d "$installed_root/share/marmoset/std" ]; then
        return 0
    fi

    installed_root=$(mktemp -d)
    installed_install_error=""
    if installed_install_error=$(
        cd "$REPO_ROOT" &&
        dune build @install &&
        dune install --prefix "$installed_root" 2>&1
    ); then
        return 0
    fi

    rm -rf "$installed_root"
    installed_root=""
    return 1
}

installed_toolchain_root() {
    printf '%s/share/marmoset' "$installed_root"
}

cleanup() {
    if [ -n "$installed_root" ]; then
        rm -rf "$installed_root"
    fi
}

trap cleanup EXIT

# --- marmoset check ---

TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] check: valid file prints OK ... "
tmpfile=$(mktemp)
echo 'let x = 42;' > "$tmpfile"
if output=$($EXECUTABLE check "$tmpfile" 2>&1) && echo "$output" | grep -q "OK"; then
    echo "✓ PASS"
    PASS=$((PASS + 1))
else
    if [ -n "$installed_install_error" ] && [ -z "${output:-}" ]; then
        echo "✗ FAIL (install failed)"
        printf '%s\n' "$installed_install_error"
    else
        echo "✗ FAIL (output: $output)"
    fi
    FAIL=$((FAIL + 1))
fi
rm -f "$tmpfile"

TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] dune install: does not install CLI binary ... "
if ensure_installed_toolchain && [ ! -e "$installed_root/bin/marmoset" ]; then
    echo "✓ PASS"
    PASS=$((PASS + 1))
else
    echo "✗ FAIL (installed binary unexpectedly present at $installed_root/bin/marmoset)"
    FAIL=$((FAIL + 1))
fi

TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] installed resources: check locates bundled stdlib ... "
tmpdir=$(mktemp -d)
tmpfile="$tmpdir/main.mr"
echo 'let x = 42;' > "$tmpfile"
if ensure_installed_toolchain && output=$(MARMOSET_ROOT="$(installed_toolchain_root)" "$EXECUTABLE" check "$tmpfile" 2>&1) \
    && echo "$output" | grep -q "OK"; then
    echo "✓ PASS"
    PASS=$((PASS + 1))
else
    echo "✗ FAIL (output: $output)"
    FAIL=$((FAIL + 1))
fi
rm -rf "$tmpdir"

TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] installed resources: build locates bundled std shim runtime ... "
tmpdir=$(mktemp -d)
tmpfile="$tmpdir/main.mr"
binpath="$tmpdir/main"
cp "$REPO_ROOT/.tool-versions" "$tmpdir/.tool-versions"
cat > "$tmpfile" <<'EOF'
import std.bytes
puts(bytes.to_str_lossy(bytes.from_str("installed")))
EOF
if ensure_installed_toolchain \
    && build_output=$(cd "$tmpdir" && MARMOSET_ROOT="$(installed_toolchain_root)" "$EXECUTABLE" build "$tmpfile" -o "$binpath" 2>&1) \
    && output=$("$binpath" 2>&1) \
    && [ "$output" = "installed" ]; then
    echo "✓ PASS"
    PASS=$((PASS + 1))
else
    echo "✗ FAIL (build: ${build_output:-}, output: ${output:-})"
    FAIL=$((FAIL + 1))
fi
rm -rf "$tmpdir"

TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] installed resources: test shim root is rejected ... "
tmpdir=$(mktemp -d)
cp "$REPO_ROOT/.tool-versions" "$tmpdir/.tool-versions"
cat > "$tmpdir/main.mr" <<'EOF'
extern "test/scalar" = {
  fn upcase(s: Str) -> Str
}
scalar.upcase("x")
EOF
if ensure_installed_toolchain \
    && build_output=$(cd "$tmpdir" && MARMOSET_ROOT="$(installed_toolchain_root)" "$EXECUTABLE" build "$tmpdir/main.mr" -o "$tmpdir/main" 2>&1); then
    echo "✗ FAIL (expected build failure)"
    FAIL=$((FAIL + 1))
elif echo "${build_output:-}" | grep -qF 'allowed root is std'; then
    echo "✓ PASS"
    PASS=$((PASS + 1))
else
    echo "✗ FAIL (output: ${build_output:-})"
    FAIL=$((FAIL + 1))
fi
rm -rf "$tmpdir"

TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] check: type error exits 1 with message ... "
tmpfile=$(mktemp)
echo '1 + true' > "$tmpfile"
if output=$($EXECUTABLE check "$tmpfile" 2>&1); then
    echo "✗ FAIL (expected non-zero exit)"
    FAIL=$((FAIL + 1))
else
    if echo "$output" | grep -qi "unify\|type\|error"; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (no error message: $output)"
        FAIL=$((FAIL + 1))
    fi
fi
rm -f "$tmpfile"

TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] check: parse error exits 1 ... "
tmpfile=$(mktemp)
echo 'let = ;' > "$tmpfile"
if $EXECUTABLE check "$tmpfile" >/dev/null 2>&1; then
    echo "✗ FAIL (expected non-zero exit)"
    FAIL=$((FAIL + 1))
else
    echo "✓ PASS"
    PASS=$((PASS + 1))
fi
rm -f "$tmpfile"

TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] check: no file prints usage ... "
if output=$($EXECUTABLE check 2>&1); then
    echo "✗ FAIL (expected non-zero exit)"
    FAIL=$((FAIL + 1))
else
    if echo "$output" | grep -q "Usage"; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (no usage: $output)"
        FAIL=$((FAIL + 1))
    fi
fi

# --- marmoset release ---

TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] release: produces binary ... "
tmpfile=$(mktemp)
test_build_dir="$REPO_ROOT/.marmoset/build"
mkdir -p "$test_build_dir"
binpath=$(mktemp "$test_build_dir/marmoset_test_bin.XXXXXX")
rm -f "$binpath"
echo 'puts(1)' > "$tmpfile"
if $EXECUTABLE release "$tmpfile" -o "$binpath" >/dev/null 2>&1 && [ -x "$binpath" ]; then
    echo "✓ PASS"
    PASS=$((PASS + 1))
else
    echo "✗ FAIL"
    FAIL=$((FAIL + 1))
fi
rm -f "$tmpfile" "$binpath"

# --- marmoset (no args) ---

TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] no args: prints usage and exits 1 ... "
if output=$($EXECUTABLE 2>&1); then
    echo "✗ FAIL (expected non-zero exit)"
    FAIL=$((FAIL + 1))
else
    if echo "$output" | grep -q "Usage"; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (no usage: $output)"
        FAIL=$((FAIL + 1))
    fi
fi

# --- marmoset lsp ---

TOTAL=$((TOTAL + 1))
# Build the LSP request sequence: initialize, then shutdown, then exit
init_body='{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}'
initialized_body='{"jsonrpc":"2.0","method":"initialized","params":{}}'
shutdown_body='{"jsonrpc":"2.0","id":2,"method":"shutdown"}'
exit_body='{"jsonrpc":"2.0","method":"exit"}'
init_len=${#init_body}
initialized_len=${#initialized_body}
shutdown_len=${#shutdown_body}
exit_len=${#exit_body}
echo -n "TEST [$TOTAL] installed resources lsp: opens .mr files without stdlib errors ... "
lsp_output=$(mktemp)
tmpdir=$(mktemp -d)
tmpfile="$tmpdir/main.mr"
echo 'let x = 42;' > "$tmpfile"
doc_uri="file://$tmpfile"
did_open_body=$(printf '{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"%s","languageId":"marmoset","version":1,"text":"let x = 42;\\n"}}}' "$doc_uri")
did_open_len=${#did_open_body}
if ensure_installed_toolchain; then
    printf "Content-Length: %d\r\n\r\n%sContent-Length: %d\r\n\r\n%sContent-Length: %d\r\n\r\n%sContent-Length: %d\r\n\r\n%sContent-Length: %d\r\n\r\n%s" \
      "$init_len" "$init_body" "$initialized_len" "$initialized_body" "$did_open_len" "$did_open_body" "$shutdown_len" "$shutdown_body" "$exit_len" "$exit_body" | \
      MARMOSET_ROOT="$(installed_toolchain_root)" "$EXECUTABLE" lsp > "$lsp_output" 2>/dev/null &
    lsp_pid=$!
    sleep 2
    kill "$lsp_pid" 2>/dev/null || true
    wait "$lsp_pid" 2>/dev/null || true
    if grep -q '"method":"textDocument/publishDiagnostics"' "$lsp_output" && ! grep -q 'stdlib-not-found' "$lsp_output"; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        output=$(cat "$lsp_output")
        echo "✗ FAIL (output: $output)"
        FAIL=$((FAIL + 1))
    fi
else
    echo "✗ FAIL (install failed)"
    if [ -n "$installed_install_error" ]; then
        printf '%s\n' "$installed_install_error"
    fi
    FAIL=$((FAIL + 1))
fi
rm -rf "$tmpdir" "$lsp_output"

TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] lsp: responds to initialize ... "
lsp_output=$(mktemp)
printf "Content-Length: %d\r\n\r\n%sContent-Length: %d\r\n\r\n%sContent-Length: %d\r\n\r\n%s" \
  "$init_len" "$init_body" "$shutdown_len" "$shutdown_body" "$exit_len" "$exit_body" | \
  $EXECUTABLE lsp > "$lsp_output" 2>/dev/null &
lsp_pid=$!
# Wait briefly for the server to process, then check output
sleep 2
kill "$lsp_pid" 2>/dev/null || true
wait "$lsp_pid" 2>/dev/null || true
if grep -q '"result"' "$lsp_output"; then
    echo "✓ PASS"
    PASS=$((PASS + 1))
else
    echo "✗ FAIL"
    FAIL=$((FAIL + 1))
fi
rm -f "$lsp_output"

suite_end
