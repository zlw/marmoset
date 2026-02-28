#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

suite_begin "CLI SUBCOMMAND TESTS"

# --- marmoset check ---

TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] check: valid file prints OK ... "
tmpfile=$(mktemp)
echo 'let x = 42;' > "$tmpfile"
if output=$($EXECUTABLE check "$tmpfile" 2>&1) && echo "$output" | grep -q "OK"; then
    echo "✓ PASS"
    PASS=$((PASS + 1))
else
    echo "✗ FAIL (output: $output)"
    FAIL=$((FAIL + 1))
fi
rm -f "$tmpfile"

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

# --- marmoset build --release ---

TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] build --release: produces binary ... "
tmpfile=$(mktemp)
binpath=$(mktemp ./marmoset_test_bin.XXXXXX)
rm -f "$binpath"
echo 'puts(1)' > "$tmpfile"
if $EXECUTABLE build --release "$tmpfile" -o "$binpath" >/dev/null 2>&1 && [ -x "$binpath" ]; then
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
echo -n "TEST [$TOTAL] lsp: responds to initialize ... "
# Build the LSP request sequence: initialize, then shutdown, then exit
init_body='{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}'
shutdown_body='{"jsonrpc":"2.0","id":2,"method":"shutdown"}'
exit_body='{"jsonrpc":"2.0","method":"exit"}'
init_len=${#init_body}
shutdown_len=${#shutdown_body}
exit_len=${#exit_body}
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
