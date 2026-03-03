#!/bin/bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
FIXTURE_ROOT="$REPO_ROOT/test/fixtures"
HARNESS="$REPO_ROOT/test/integration.sh"

# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

TMP_FIXTURE_DIR=""
STUB_EXEC=""
HARNESS_COPY=""
MONKEY_BACKUP=""
MONKEY_FIXTURE="$FIXTURE_ROOT/runtime/monkey_example.mr"

cleanup() {
    if [ -n "$MONKEY_BACKUP" ] && [ -f "$MONKEY_BACKUP" ]; then
        cp "$MONKEY_BACKUP" "$MONKEY_FIXTURE"
        rm -f "$MONKEY_BACKUP"
        MONKEY_BACKUP=""
    fi

    if [ -n "$TMP_FIXTURE_DIR" ] && [ -d "$TMP_FIXTURE_DIR" ]; then
        rm -rf "$TMP_FIXTURE_DIR"
    fi

    if [ -n "$STUB_EXEC" ] && [ -f "$STUB_EXEC" ]; then
        rm -f "$STUB_EXEC"
    fi

    if [ -n "$HARNESS_COPY" ] && [ -f "$HARNESS_COPY" ]; then
        rm -f "$HARNESS_COPY"
    fi
}

trap cleanup EXIT

suite_begin "HARNESS STRICTNESS CANARIES"

TMP_FIXTURE_DIR=$(mktemp -d "$FIXTURE_ROOT/.harness_canary.XXXXXX")

canary_missing="$TMP_FIXTURE_DIR/canary_missing_expected.mr"
canary_line="$TMP_FIXTURE_DIR/canary_line_mismatch.mr"
canary_unexpected="$TMP_FIXTURE_DIR/canary_unexpected.mr"
canary_continuation="$TMP_FIXTURE_DIR/canary_no_span_continuation.mr"
canary_annotation="$TMP_FIXTURE_DIR/canary_annotation_placement.mr"
canary_severity="$TMP_FIXTURE_DIR/canary_severity_mismatch.mr"
canary_mixed_wc="$TMP_FIXTURE_DIR/canary_mixed_wildcard.mr"
canary_wc_requires_extractor="$TMP_FIXTURE_DIR/canary_wildcard_requires_extractor.mr"
canary_colon="$TMP_FIXTURE_DIR/canary_colon_file_id.mr"

cat > "$canary_missing" <<'CANARY'
let x = 1  # error: expected marker that is intentionally absent
CANARY

cat > "$canary_line" <<'CANARY'
let y = 2  # error: line-bound marker
CANARY

cat > "$canary_unexpected" <<'CANARY'
let z = 3  # error: expected core marker
CANARY

cat > "$canary_continuation" <<'CANARY'
let c = 4  # error: missing return
CANARY

cat > "$canary_annotation" <<'CANARY'
# error: detached annotation should fail placement
let detached = 1
CANARY

cat > "$canary_severity" <<'CANARY'
let s = 1  # warning: severity test marker
CANARY

cat > "$canary_mixed_wc" <<'CANARY'
let w = 1  # error: concrete match
let w2 = 2  # error: *
CANARY

cat > "$canary_wc_requires_extractor" <<'CANARY'
let wx = 1  # error: *
CANARY

cat > "$canary_colon" <<'CANARY'
let x = 1  # error: colon path diagnostic
CANARY

STUB_EXEC=$(mktemp)
cat > "$STUB_EXEC" <<'STUB'
#!/bin/bash
set -e

if [ "$#" -lt 2 ] || [ "$1" != "build" ]; then
    echo "stub only supports: build <fixture>" >&2
    exit 2
fi

file="$2"

case "$file" in
    *canary_missing_expected.mr)
        echo "$file:1:1: error test-missing: unrelated emitted diagnostic"
        exit 1
        ;;
    *canary_line_mismatch.mr)
        echo "$file:99:1: error test-line: line-bound marker"
        exit 1
        ;;
    *canary_unexpected.mr)
        echo "$file:1:1: error test-expected: expected core marker"
        echo "$file:4:1: error test-extra: unexpected extra marker"
        exit 1
        ;;
    *canary_mixed_wildcard.mr)
        echo "$file:1:1: error test-concrete: concrete match"
        echo "$file:2:1: error test-extra1: wildcard absorbed extra"
        echo "$file:3:1: error test-extra2: surplus unrelated extra"
        exit 1
        ;;
    *canary_wildcard_requires_extractor.mr)
        echo "toolchain failure without canonical diagnostics"
        exit 1
        ;;
    *canary_severity_mismatch.mr)
        echo "$file:1:1: error test-severity: severity test marker"
        exit 1
        ;;
    *canary_colon_file_id.mr)
        echo "C:\\test\\file.mr:1:1: error test-colon: colon path diagnostic"
        exit 1
        ;;
    *canary_no_span_continuation.mr)
        echo "error build-go-compile: # marmoset_out"
        echo "./main.go:50:1: missing return"
        exit 1
        ;;
    *)
        echo "Built: stub"
        exit 0
        ;;
esac
STUB
chmod +x "$STUB_EXEC"

HARNESS_COPY=$(mktemp "$REPO_ROOT/test/.harness_copy.XXXXXX.sh")
awk -v stub_exec="$STUB_EXEC" '
    /^source "\$INTEGRATION_DIR\/common.sh"/ {
        print
        print "EXECUTABLE=\"" stub_exec "\""
        next
    }
    { print }
' "$HARNESS" > "$HARNESS_COPY"
chmod +x "$HARNESS_COPY"

if ! grep -q 'EXECUTABLE=' "$HARNESS_COPY"; then
    echo "FATAL: stub EXECUTABLE= injection did not land in harness copy" >&2
    exit 2
fi

rel_missing="${canary_missing#$FIXTURE_ROOT/}"
rel_line="${canary_line#$FIXTURE_ROOT/}"
rel_unexpected="${canary_unexpected#$FIXTURE_ROOT/}"
rel_continuation="${canary_continuation#$FIXTURE_ROOT/}"
rel_annotation="${canary_annotation#$FIXTURE_ROOT/}"
rel_severity="${canary_severity#$FIXTURE_ROOT/}"
rel_mixed_wc="${canary_mixed_wc#$FIXTURE_ROOT/}"
rel_wc_requires_extractor="${canary_wc_requires_extractor#$FIXTURE_ROOT/}"
rel_colon="${canary_colon#$FIXTURE_ROOT/}"

TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] reject strictness: missing/line-mismatch/unexpected canaries ... "
if output=$($HARNESS_COPY "$rel_missing" "$rel_line" "$rel_unexpected" 2>&1); then
    echo "✗ FAIL (expected harness failure)"
    FAIL=$((FAIL + 1))
else
    if echo "$output" | grep -q "✗ FAIL (missing expected diagnostics)" \
      && echo "$output" | grep -q "✗ FAIL (line-mismatched diagnostics)" \
      && echo "$output" | grep -q "✗ FAIL (unexpected diagnostics)"; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (did not observe all strictness failure categories)"
        echo "$output" | sed 's/^/  /' | head -n 40
        FAIL=$((FAIL + 1))
    fi
fi

TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] canonical no-span continuation lines are matchable ... "
if output=$($HARNESS_COPY "$rel_continuation" 2>&1); then
    if echo "$output" | grep -q "✓ PASS"; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (continuation-line canary missing pass marker)"
        echo "$output" | sed 's/^/  /' | head -n 40
        FAIL=$((FAIL + 1))
    fi
else
    echo "✗ FAIL (continuation-line canary unexpectedly failed)"
    echo "$output" | sed 's/^/  /' | head -n 40
    FAIL=$((FAIL + 1))
fi

TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] annotation placement guard rejects detached annotation ... "
if output=$($HARNESS "$rel_annotation" 2>&1); then
    echo "✗ FAIL (expected annotation placement failure)"
    FAIL=$((FAIL + 1))
else
    if echo "$output" | grep -q "✗ FAIL (annotation placement)"; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (annotation placement failure marker missing)"
        echo "$output" | sed 's/^/  /' | head -n 40
        FAIL=$((FAIL + 1))
    fi
fi

TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] severity enforcement: warning annotation with error diagnostic fails ... "
if output=$($HARNESS_COPY "$rel_severity" 2>&1); then
    echo "✗ FAIL (expected severity mismatch failure)"
    FAIL=$((FAIL + 1))
else
    if echo "$output" | grep -q "✗ FAIL (severity-mismatched diagnostics)"; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (severity mismatch failure marker missing)"
        echo "$output" | sed 's/^/  /' | head -n 40
        FAIL=$((FAIL + 1))
    fi
fi

TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] mixed wildcard + concrete fails on surplus extras ... "
if output=$($HARNESS_COPY "$rel_mixed_wc" 2>&1); then
    echo "✗ FAIL (expected wildcard-scoped failure)"
    FAIL=$((FAIL + 1))
else
    if echo "$output" | grep -q "✗ FAIL (unexpected diagnostics)"; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (wildcard scope failure marker missing)"
        echo "$output" | sed 's/^/  /' | head -n 40
        FAIL=$((FAIL + 1))
    fi
fi

TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] wildcard-only expectations still require extractor success ... "
if output=$($HARNESS_COPY "$rel_wc_requires_extractor" 2>&1); then
    echo "✗ FAIL (expected extractor failure)"
    FAIL=$((FAIL + 1))
else
    if echo "$output" | grep -q "✗ FAIL (diagnostic extractor failure)"; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (wildcard extractor failure marker missing)"
        echo "$output" | sed 's/^/  /' | head -n 40
        FAIL=$((FAIL + 1))
    fi
fi

TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] colon-bearing file-id header parses correctly ... "
if output=$($HARNESS_COPY "$rel_colon" 2>&1); then
    if echo "$output" | grep -q "✓ PASS"; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (colon file-id canary missing pass marker)"
        echo "$output" | sed 's/^/  /' | head -n 40
        FAIL=$((FAIL + 1))
    fi
else
    echo "✗ FAIL (colon file-id canary unexpectedly failed)"
    echo "$output" | sed 's/^/  /' | head -n 40
    FAIL=$((FAIL + 1))
fi

TOTAL=$((TOTAL + 1))
echo -n "TEST [$TOTAL] monkey drift guard detects fixture/source mismatch ... "
MONKEY_BACKUP=$(mktemp)
cp "$MONKEY_FIXTURE" "$MONKEY_BACKUP"
printf "\n# canary drift marker\n" >> "$MONKEY_FIXTURE"

if output=$($HARNESS runtime/monkey_example.mr 2>&1); then
    echo "✗ FAIL (expected fixture drift failure)"
    FAIL=$((FAIL + 1))
else
    if echo "$output" | grep -q "✗ FAIL (fixture drift)"; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (fixture drift failure marker missing)"
        echo "$output" | sed 's/^/  /' | head -n 40
        FAIL=$((FAIL + 1))
    fi
fi

cp "$MONKEY_BACKUP" "$MONKEY_FIXTURE"
rm -f "$MONKEY_BACKUP"
MONKEY_BACKUP=""

suite_end
