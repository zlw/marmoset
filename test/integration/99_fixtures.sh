#!/bin/bash
# Fixture-based integration test runner.
#
# Discovers .mr fixture files under test/fixtures/, parses inline
# annotations (# output:, # error:, # warning:, # info:), infers
# test mode (run / reject / build-only), and executes accordingly.
#
# Annotation grammar:
#   # output: <text>     — expected stdout line (run mode)
#   # error: <text-or-*> — expected diagnostic (reject mode)
#   # warning: <text>    — expected diagnostic (reject mode)
#   # info: <text>       — expected diagnostic (reject mode)
#
# Mode inference:
#   1. Any diagnostic annotation (error/warning/info) -> reject mode
#   2. Else if any # output: -> run mode
#   3. Else -> build-only mode (build must succeed)
#
# Diagnostic matching:
#   - "# error: *" means "at least one error exists" (wildcard)
#   - Otherwise substring match against build stderr
#   - Two-way strictness: expected-but-missing -> fail,
#     unexpected-diagnostic -> fail (only for non-wildcard reject)

set -e

INTEGRATION_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$INTEGRATION_DIR/common.sh"

FIXTURE_ROOT="$REPO_ROOT/test/fixtures"

suite_begin "Fixture Tests"

# --- Annotation parsing ---

# Parse annotations from a fixture file.
# Sets these arrays:
#   OUTPUT_LINES[i]   — expected stdout lines (for run mode)
#   DIAG_TYPES[i]     — "error" | "warning" | "info"
#   DIAG_VALUES[i]    — expected text or "*"
#   DIAG_LINENOS[i]   — source line number of annotation
# Sets MODE to "reject", "run", or "build-only".
parse_fixture() {
    local file="$1"

    OUTPUT_LINES=()
    DIAG_TYPES=()
    DIAG_VALUES=()
    DIAG_LINENOS=()
    MODE=""

    local lineno=0
    local has_diag=false
    local has_output=false

    while IFS= read -r line || [ -n "$line" ]; do
        lineno=$((lineno + 1))

        # Match "# output: <text>"
        if [[ "$line" =~ ^[^#]*#[[:space:]]*output:[[:space:]]*(.*) ]]; then
            OUTPUT_LINES+=("${BASH_REMATCH[1]}")
            has_output=true
        fi

        # Match "# error: <text>"
        if [[ "$line" =~ ^[^#]*#[[:space:]]*error:[[:space:]]*(.*) ]]; then
            DIAG_TYPES+=("error")
            DIAG_VALUES+=("${BASH_REMATCH[1]}")
            DIAG_LINENOS+=("$lineno")
            has_diag=true
        fi

        # Match "# warning: <text>"
        if [[ "$line" =~ ^[^#]*#[[:space:]]*warning:[[:space:]]*(.*) ]]; then
            DIAG_TYPES+=("warning")
            DIAG_VALUES+=("${BASH_REMATCH[1]}")
            DIAG_LINENOS+=("$lineno")
            has_diag=true
        fi

        # Match "# info: <text>"
        if [[ "$line" =~ ^[^#]*#[[:space:]]*info:[[:space:]]*(.*) ]]; then
            DIAG_TYPES+=("info")
            DIAG_VALUES+=("${BASH_REMATCH[1]}")
            DIAG_LINENOS+=("$lineno")
            has_diag=true
        fi
    done < "$file"

    if $has_diag; then
        MODE="reject"
    elif $has_output; then
        MODE="run"
    else
        MODE="build-only"
    fi
}

# --- Test execution ---

run_fixture() {
    local file="$1"
    local rel_path="${file#$FIXTURE_ROOT/}"
    local name="${rel_path%.mr}"

    parse_fixture "$file"

    case "$MODE" in
        run)
            run_mode "$file" "$name"
            ;;
        reject)
            reject_mode "$file" "$name"
            ;;
        build-only)
            build_only_mode "$file" "$name"
            ;;
    esac
}

run_mode() {
    local file="$1"
    local name="$2"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local test_build_dir="$REPO_ROOT/.marmoset/build"
    mkdir -p "$test_build_dir"
    local binpath
    binpath=$(mktemp "$test_build_dir/marmoset_test_bin.XXXXXX")
    rm -f "$binpath"

    local build_output
    local actual_output
    if build_output=$($EXECUTABLE build "$file" -o "$binpath" 2>&1) && actual_output=$("$binpath" 2>&1); then
        # Build expected stdout from annotations
        local expected_output=""
        local first=true
        for line in "${OUTPUT_LINES[@]}"; do
            if $first; then
                expected_output="$line"
                first=false
            else
                expected_output="$expected_output
$line"
            fi
        done

        if [ "$actual_output" = "$expected_output" ]; then
            echo "✓ PASS"
            PASS=$((PASS + 1))
        else
            echo "✗ FAIL (output mismatch)"
            echo "  Expected: $(echo "$expected_output" | head -5)"
            echo "  Got:      $(echo "$actual_output" | head -5)"
            FAIL=$((FAIL + 1))
        fi
    else
        echo "✗ FAIL (build or execution failed)"
        echo "  Build output: $build_output"
        FAIL=$((FAIL + 1))
    fi

    rm -f "$binpath"
}

reject_mode() {
    local file="$1"
    local name="$2"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local test_build_dir="$REPO_ROOT/.marmoset/build"
    mkdir -p "$test_build_dir"
    local binpath
    binpath=$(mktemp "$test_build_dir/marmoset_test_bin.XXXXXX")
    rm -f "$binpath"

    local build_output
    if build_output=$($EXECUTABLE build "$file" -o "$binpath" 2>&1); then
        echo "✗ FAIL (expected build failure but build succeeded)"
        FAIL=$((FAIL + 1))
        rm -f "$binpath"
        return
    fi
    rm -f "$binpath"

    # Check if we have a pure wildcard reject (all diags are "*")
    local all_wildcard=true
    local i
    for ((i = 0; i < ${#DIAG_VALUES[@]}; i++)); do
        if [ "${DIAG_VALUES[$i]}" != "*" ]; then
            all_wildcard=false
            break
        fi
    done

    if $all_wildcard; then
        # Wildcard: any failure is accepted
        echo "✓ PASS"
        PASS=$((PASS + 1))
        return
    fi

    # --- One-way: each expected diagnostic must be present ---
    local all_found=true
    local missing=""
    for ((i = 0; i < ${#DIAG_VALUES[@]}; i++)); do
        local val="${DIAG_VALUES[$i]}"
        if [ "$val" = "*" ]; then
            continue
        fi
        if ! echo "$build_output" | grep -qF "$val"; then
            all_found=false
            missing="$missing\n  - line ${DIAG_LINENOS[$i]}: '${DIAG_TYPES[$i]}: $val'"
        fi
    done

    if ! $all_found; then
        echo "✗ FAIL (missing expected diagnostics)"
        echo -e "  Missing:$missing"
        echo "  Build output: $(echo "$build_output" | head -10)"
        FAIL=$((FAIL + 1))
        return
    fi

    # --- Two-way: each actual diagnostic line must be covered by an annotation ---
    local unexpected=""
    while IFS= read -r diag_line; do
        [ -z "$diag_line" ] && continue
        local covered=false
        for ((i = 0; i < ${#DIAG_VALUES[@]}; i++)); do
            local val="${DIAG_VALUES[$i]}"
            if [ "$val" = "*" ]; then
                covered=true
                break
            fi
            if echo "$diag_line" | grep -qF "$val"; then
                covered=true
                break
            fi
        done
        if ! $covered; then
            unexpected="$unexpected\n  - $diag_line"
        fi
    done < <(echo "$build_output" | grep -iE '^[^:]*:[0-9]+:[0-9]+.*error|^(Type |Parse )?[Ee]rror|^[Ww]arning|^[Ii]nfo')

    if [ -n "$unexpected" ]; then
        echo "✗ FAIL (unexpected diagnostics)"
        echo -e "  Unexpected:$unexpected"
        FAIL=$((FAIL + 1))
    else
        echo "✓ PASS"
        PASS=$((PASS + 1))
    fi
}

build_only_mode() {
    local file="$1"
    local name="$2"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local test_build_dir="$REPO_ROOT/.marmoset/build"
    mkdir -p "$test_build_dir"
    local binpath
    binpath=$(mktemp "$test_build_dir/marmoset_test_bin.XXXXXX")
    rm -f "$binpath"

    local build_output
    if build_output=$($EXECUTABLE build "$file" -o "$binpath" 2>&1); then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (build failed)"
        echo "  Output: $build_output"
        FAIL=$((FAIL + 1))
    fi

    rm -f "$binpath"
}

# --- Discovery and execution ---

fixtures=()
while IFS= read -r f; do
    fixtures+=("$f")
done < <(find "$FIXTURE_ROOT" -type f -name '*.mr' | LC_ALL=C sort)

if [ ${#fixtures[@]} -eq 0 ]; then
    echo "(no fixtures found — skipping)"
    PASS=0
    FAIL=0
    TOTAL=0
    suite_end
    exit 0
fi

for fixture in "${fixtures[@]}"; do
    run_fixture "$fixture"
done

suite_end
