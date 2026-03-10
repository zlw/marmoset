#!/bin/bash
# Unified integration runner.
#
# Default: run all fixture tests under test/fixtures.
# Optional: run CLI integration suite with `cli` selector.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
INTEGRATION_DIR="$SCRIPT_DIR/integration"
FIXTURE_ROOT="$REPO_ROOT/test/fixtures"
EXECUTABLE="$REPO_ROOT/_build/default/bin/main.exe"
BUILD_TARGET="./_build/default/bin/main.exe"
CLI_SUITE="$INTEGRATION_DIR/08_cli.sh"
HARNESS_SUITE="$INTEGRATION_DIR/09_harness_canaries.sh"
TEST_BUILD_DIR="$REPO_ROOT/.marmoset/build"

source "$INTEGRATION_DIR/common.sh"
mkdir -p "$TEST_BUILD_DIR"

ALL_GROUPS=()
while IFS= read -r d; do
    ALL_GROUPS+=("$d")
done < <(find "$FIXTURE_ROOT" -mindepth 1 -maxdepth 1 -type d -print \
    | sed 's#^.*/##' \
    | LC_ALL=C sort)

if [ "${#ALL_GROUPS[@]}" -eq 0 ]; then
    echo "No fixture groups found in $FIXTURE_ROOT" >&2
    exit 2
fi

print_usage() {
    cat <<USAGE
Usage:
  ./test/integration.sh                 # run all fixture groups
  ./test/integration.sh <selector> [...] 

Selectors:
  all                  # all fixture groups
  cli                  # run 08_cli.sh suite
  harness              # run 09_harness_canaries.sh suite
  <group>              # exact fixture group (e.g. traits, runtime)
  <group-prefix>       # prefix match (e.g. codegen -> codegen/codegen_*)
  <group>/<file>.mr    # single fixture file under test/fixtures
  test/fixtures/...mr  # fixture file path from repo root
USAGE
}

normalize_existing_file() {
    local path="$1"
    local dir
    local base

    dir=$(dirname "$path")
    base=$(basename "$path")
    (cd "$dir" 2>/dev/null && printf "%s/%s\n" "$(pwd)" "$base")
}

resolve_fixture_file_selector() {
    local selector="$1"
    local candidate=""
    local abs=""

    if [[ "$selector" == *.mr ]]; then
        if [ -f "$FIXTURE_ROOT/$selector" ]; then
            candidate="$FIXTURE_ROOT/$selector"
        elif [ -f "$REPO_ROOT/$selector" ]; then
            candidate="$REPO_ROOT/$selector"
        elif [ -f "$selector" ]; then
            candidate="$selector"
        fi
    fi

    if [ -z "$candidate" ]; then
        return 1
    fi

    abs=$(normalize_existing_file "$candidate") || return 1

    case "$abs" in
        "$FIXTURE_ROOT"/*)
            echo "$abs"
            return 0
            ;;
        *)
            return 1
            ;;
    esac
}

resolve_selector() {
    local name="$1"
    local matches=()
    local group

    if [ "$name" = "all" ]; then
        echo "${ALL_GROUPS[*]}"
        return 0
    fi

    if [ "$name" = "cli" ] || [ "$name" = "08_cli" ] || [ "$name" = "08_cli.sh" ]; then
        echo "__CLI__"
        return 0
    fi

    if [ "$name" = "harness" ] || [ "$name" = "09_harness" ] || [ "$name" = "09_harness_canaries.sh" ]; then
        echo "__HARNESS__"
        return 0
    fi

    for group in "${ALL_GROUPS[@]}"; do
        if [ "$group" = "$name" ]; then
            echo "$group"
            return 0
        fi
    done

    for group in "${ALL_GROUPS[@]}"; do
        if [[ "$group" == "$name"_* ]]; then
            matches+=("$group")
        fi
    done

    if [ "${#matches[@]}" -gt 0 ]; then
        echo "${matches[*]}"
        return 0
    fi

    return 1
}

detect_fixture_jobs() {
    local n="${MARMOSET_FIXTURE_JOBS:-}"
    if [ -z "$n" ]; then
        n=$(getconf _NPROCESSORS_ONLN 2>/dev/null || true)
    fi
    if [ -z "$n" ]; then
        n=$(sysctl -n hw.logicalcpu 2>/dev/null || true)
    fi
    if [ -z "$n" ]; then
        n=4
    fi

    case "$n" in
        ''|*[!0-9]*)
            n=1
            ;;
    esac

    if [ "$n" -lt 1 ]; then
        n=1
    fi
    echo "$n"
}

# --- Fixture parsing and execution ---

trim_ws() {
    local s="$1"
    s="${s#"${s%%[![:space:]]*}"}"
    s="${s%"${s##*[![:space:]]}"}"
    echo "$s"
}

record_failed_test() {
    local name="$1"
    local reason="$2"
    FAILED_TEST_NAMES+=("$name")
    FAILED_TEST_REASONS+=("$reason")
}

collect_failure_from_log() {
    local log_file="$1"
    local line
    while IFS= read -r line; do
        if [[ "$line" =~ ^TEST\ \[[0-9]+\]\ (.*)\ \.\.\.\ ✗\ FAIL\ \((.*)\)$ ]]; then
            record_failed_test "${BASH_REMATCH[1]}" "${BASH_REMATCH[2]}"
            return
        fi
    done < "$log_file"
}

print_failed_tests_summary() {
    [ "${#FAILED_TEST_NAMES[@]}" -eq 0 ] && return

    echo "Failing tests:"
    local i
    for i in "${!FAILED_TEST_NAMES[@]}"; do
        echo "  - ${FAILED_TEST_NAMES[$i]} (${FAILED_TEST_REASONS[$i]})"
    done
}

is_output_anchor_line() {
    local line
    line=$(trim_ws "$1")

    if [ -z "$line" ] || [[ "$line" == \#* ]]; then
        return 1
    fi

    if [[ "$line" == puts\(* ]] || [[ "$line" == match\ * ]] || [[ "$line" == if\ * ]] || [[ "$line" == if\(* ]]; then
        return 0
    fi

    if [[ "$line" =~ ^[A-Za-z_][A-Za-z0-9_.]*[[:space:]]*\( ]]; then
        case "$line" in
            let\ *|type\ *|enum\ *|trait\ *|impl\ *|derive\ *|fn\ *|return\ *|else*|while\ *|for\ *|switch\ *)
                return 1
                ;;
        esac
        return 0
    fi

    return 1
}

monkey_fixture_in_sync() {
    local fixture_file="$1"
    local example_file="$REPO_ROOT/examples/monkey.mr"
    local normalized_fixture
    local diff_file

    if [ ! -f "$example_file" ]; then
        echo "  Missing source file: $example_file"
        return 1
    fi

    normalized_fixture=$(mktemp)
    diff_file=$(mktemp)

    # Keep fixture annotations, but guard that the underlying source stays synced
    # with examples/monkey.mr.
    sed -E 's/[[:space:]]*#[[:space:]]*output:[[:space:]].*$//' "$fixture_file" > "$normalized_fixture"

    if diff -u "$example_file" "$normalized_fixture" > "$diff_file"; then
        rm -f "$normalized_fixture" "$diff_file"
        return 0
    fi

    echo "  Fixture/source drift detected against examples/monkey.mr:"
    sed -n '1,40p' "$diff_file" | sed 's/^/  /'
    rm -f "$normalized_fixture" "$diff_file"
    return 1
}

parse_fixture() {
    local file="$1"

    OUTPUT_LINES=()
    DIAG_TYPES=()
    DIAG_VALUES=()
    DIAG_LINENOS=()
    MODE=""
    XFAIL_ENABLED=0
    XFAIL_REASON=""

    local lineno=0
    local has_diag=false
    local has_error_diag=false
    local has_output=false
    local in_leading_block=true
    local -a lines=()

    while IFS= read -r line || [ -n "$line" ]; do
        lineno=$((lineno + 1))
        lines+=("$line")

        # xfail parsing: must appear in leading comment block
        if $in_leading_block; then
            local trimmed_xf
            trimmed_xf=$(trim_ws "$line")
            if [ -z "$trimmed_xf" ]; then
                : # blank lines are OK in leading block
            elif [[ "$trimmed_xf" == \#* ]]; then
                # Comment line — check for xfail annotation
                if [[ "$trimmed_xf" =~ ^#[[:space:]]*xfail:[[:space:]]*(.*)$ ]]; then
                    local reason="${BASH_REMATCH[1]}"
                    reason=$(trim_ws "$reason")
                    if [ -z "$reason" ]; then
                        echo "xfail annotation requires a non-empty reason" >&2
                        return 1
                    fi
                    if [ "$XFAIL_ENABLED" -eq 1 ]; then
                        echo "duplicate xfail annotation (max one per fixture)" >&2
                        return 1
                    fi
                    XFAIL_ENABLED=1
                    XFAIL_REASON="$reason"
                fi
            else
                in_leading_block=false
            fi
        fi

        if [[ "$line" =~ ^[^#]*#[[:space:]]*output:[[:space:]]*(.*) ]]; then
            OUTPUT_LINES+=("${BASH_REMATCH[1]}")
            has_output=true
        fi

        if [[ "$line" =~ ^[^#]*#[[:space:]]*error:[[:space:]]*(.*) ]]; then
            DIAG_TYPES+=("error")
            DIAG_VALUES+=("${BASH_REMATCH[1]}")
            DIAG_LINENOS+=("$lineno")
            has_diag=true
            has_error_diag=true
        fi

        if [[ "$line" =~ ^[^#]*#[[:space:]]*warning:[[:space:]]*(.*) ]]; then
            DIAG_TYPES+=("warning")
            DIAG_VALUES+=("${BASH_REMATCH[1]}")
            DIAG_LINENOS+=("$lineno")
            has_diag=true
        fi

        if [[ "$line" =~ ^[^#]*#[[:space:]]*info:[[:space:]]*(.*) ]]; then
            DIAG_TYPES+=("info")
            DIAG_VALUES+=("${BASH_REMATCH[1]}")
            DIAG_LINENOS+=("$lineno")
            has_diag=true
        fi
    done < "$file"

    local total=${#lines[@]}
    for ((lineno = 1; lineno <= total; lineno++)); do
        local ann_line="${lines[$((lineno - 1))]}"
        if [[ ! "$ann_line" =~ ^[[:space:]]*#[[:space:]]*(output|error|warning|info):[[:space:]]*(.*)$ ]]; then
            continue
        fi

        local ann_type="${BASH_REMATCH[1]}"
        if [ "$ann_type" != "output" ]; then
            echo "line $lineno: '$ann_type' annotation must be inline with code" >&2
            return 1
        fi

        local probe=$((lineno + 1))
        local found_anchor=false
        while [ "$probe" -le "$total" ]; do
            local next_line="${lines[$((probe - 1))]}"
            local trimmed
            trimmed=$(trim_ws "$next_line")

            if [ -z "$trimmed" ]; then
                probe=$((probe + 1))
                continue
            fi

            if [[ "$trimmed" =~ ^#[[:space:]]*(output|error|warning|info):[[:space:]]*(.*)$ ]]; then
                probe=$((probe + 1))
                continue
            fi

            if [[ "$trimmed" == \#* ]]; then
                echo "line $lineno: output annotation is detached from executable line" >&2
                return 1
            fi

            if ! is_output_anchor_line "$trimmed"; then
                echo "line $lineno: output annotation must target an output anchor (puts/match/if/call)" >&2
                return 1
            fi

            found_anchor=true
            break
        done

        if ! $found_anchor; then
            echo "line $lineno: output annotation has no following executable anchor" >&2
            return 1
        fi
    done

    if $has_error_diag; then
        MODE="reject"
    elif $has_diag && $has_output; then
        MODE="run-diagnostics"
    elif $has_diag; then
        MODE="build-diagnostics"
    elif $has_output; then
        MODE="run"
    else
        MODE="build-only"
    fi

    return 0
}

# --- Diagnostic extraction/matching helpers (Phase 7 canonical-only) ---

PARSED_DIAG_EXTRACTOR=()
PARSED_DIAG_SEVERITY=()
PARSED_DIAG_CODE=()
PARSED_DIAG_MESSAGE=()
PARSED_DIAG_FILE=()
PARSED_DIAG_START_LINE=()
PARSED_DIAG_START_COL=()
PARSED_DIAG_END_LINE=()
PARSED_DIAG_END_COL=()
PARSED_DIAG_MATCH_TEXT=()
DIAG_RECORD_SEP=$'\x1f'

to_lower() {
    printf "%s" "$1" | tr '[:upper:]' '[:lower:]'
}

emit_diag_record() {
    local extractor="$1"
    local severity="$2"
    local code="$3"
    local message="$4"
    local file="$5"
    local start_line="$6"
    local start_col="$7"
    local end_line="$8"
    local end_col="$9"
    local match_text="${10}"

    # Keep records single-line/sep-safe for transport.
    local safe_message="${message//$DIAG_RECORD_SEP/ }"
    local safe_match_text="${match_text//$DIAG_RECORD_SEP/ }"
    safe_message="${safe_message//$'\n'/ }"
    safe_match_text="${safe_match_text//$'\n'/ }"

    printf "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s\n" \
        "$extractor" "$DIAG_RECORD_SEP" \
        "$severity" "$DIAG_RECORD_SEP" \
        "$code" "$DIAG_RECORD_SEP" \
        "$safe_message" "$DIAG_RECORD_SEP" \
        "$file" "$DIAG_RECORD_SEP" \
        "$start_line" "$DIAG_RECORD_SEP" \
        "$start_col" "$DIAG_RECORD_SEP" \
        "$end_line" "$DIAG_RECORD_SEP" \
        "$end_col" "$DIAG_RECORD_SEP" \
        "$safe_match_text"
}

try_parse_canonical_header() {
    local line="$1"
    CH_SEVERITY=""
    CH_CODE=""
    CH_MESSAGE=""
    CH_FILE=""
    CH_START_LINE=""
    CH_START_COL=""
    CH_END_LINE=""
    CH_END_COL=""

    if [[ "$line" =~ ^(.+):([0-9]+):([0-9]+)-([0-9]+):([0-9]+):[[:space:]]*([Ee]rror|[Ww]arning|[Ii]nfo)[[:space:]]+([A-Za-z0-9._-]+):[[:space:]]*(.*)$ ]]; then
        CH_FILE="${BASH_REMATCH[1]}"
        CH_START_LINE="${BASH_REMATCH[2]}"
        CH_START_COL="${BASH_REMATCH[3]}"
        CH_END_LINE="${BASH_REMATCH[4]}"
        CH_END_COL="${BASH_REMATCH[5]}"
        CH_SEVERITY="$(to_lower "${BASH_REMATCH[6]}")"
        CH_CODE="$(to_lower "${BASH_REMATCH[7]}")"
        CH_MESSAGE="${BASH_REMATCH[8]}"
        return 0
    fi

    if [[ "$line" =~ ^(.+):([0-9]+):([0-9]+):[[:space:]]*([Ee]rror|[Ww]arning|[Ii]nfo)[[:space:]]+([A-Za-z0-9._-]+):[[:space:]]*(.*)$ ]]; then
        CH_FILE="${BASH_REMATCH[1]}"
        CH_START_LINE="${BASH_REMATCH[2]}"
        CH_START_COL="${BASH_REMATCH[3]}"
        CH_END_LINE=""
        CH_END_COL=""
        CH_SEVERITY="$(to_lower "${BASH_REMATCH[4]}")"
        CH_CODE="$(to_lower "${BASH_REMATCH[5]}")"
        CH_MESSAGE="${BASH_REMATCH[6]}"
        return 0
    fi

    if [[ "$line" =~ ^([Ee]rror|[Ww]arning|[Ii]nfo)[[:space:]]+([A-Za-z0-9._-]+):[[:space:]]*(.*)$ ]]; then
        CH_FILE=""
        CH_START_LINE=""
        CH_START_COL=""
        CH_END_LINE=""
        CH_END_COL=""
        CH_SEVERITY="$(to_lower "${BASH_REMATCH[1]}")"
        CH_CODE="$(to_lower "${BASH_REMATCH[2]}")"
        CH_MESSAGE="${BASH_REMATCH[3]}"
        return 0
    fi

    return 1
}

extract_new_diagnostic_records() {
    local build_output="$1"

    local active=0
    local cur_severity=""
    local cur_code=""
    local cur_message=""
    local cur_file=""
    local cur_start_line=""
    local cur_start_col=""
    local cur_end_line=""
    local cur_end_col=""
    local cur_match_text=""
    local cont_count=0
    local cont_active=0

    while IFS= read -r line || [ -n "$line" ]; do
        line="${line%$'\r'}"

        if try_parse_canonical_header "$line"; then
            if [ "$active" -eq 1 ]; then
                emit_diag_record "canonical" "$cur_severity" "$cur_code" "$cur_message" "$cur_file" "$cur_start_line" \
                    "$cur_start_col" "$cur_end_line" "$cur_end_col" "$cur_match_text"
            fi

            active=1
            cont_count=0
            cont_active=1
            cur_severity="$CH_SEVERITY"
            cur_code="$CH_CODE"
            cur_message="$CH_MESSAGE"
            cur_file="$CH_FILE"
            cur_start_line="$CH_START_LINE"
            cur_start_col="$CH_START_COL"
            cur_end_line="$CH_END_LINE"
            cur_end_col="$CH_END_COL"
            cur_match_text="$line"
            continue
        fi

        if [ "$active" -eq 1 ] && [ "$cont_active" -eq 1 ]; then
            # Keep non-header continuation lines attached to the active diagnostic block.
            # This preserves matcher visibility for toolchain details that are emitted on
            # following lines (for example Go compiler messages under build-go-compile).
            # Stop on blank lines (block boundary) or after 20 continuation lines.
            local trimmed
            trimmed=$(trim_ws "$line")
            if [ -z "$trimmed" ] || [ "$cont_count" -ge 20 ]; then
                cont_active=0
            else
                cur_match_text="$cur_match_text | $trimmed"
                cont_count=$((cont_count + 1))
            fi
        fi
    done <<< "$build_output"

    if [ "$active" -eq 1 ]; then
        emit_diag_record "canonical" "$cur_severity" "$cur_code" "$cur_message" "$cur_file" "$cur_start_line" \
            "$cur_start_col" "$cur_end_line" "$cur_end_col" "$cur_match_text"
    fi
}

clear_parsed_diagnostics() {
    PARSED_DIAG_EXTRACTOR=()
    PARSED_DIAG_SEVERITY=()
    PARSED_DIAG_CODE=()
    PARSED_DIAG_MESSAGE=()
    PARSED_DIAG_FILE=()
    PARSED_DIAG_START_LINE=()
    PARSED_DIAG_START_COL=()
    PARSED_DIAG_END_LINE=()
    PARSED_DIAG_END_COL=()
    PARSED_DIAG_MATCH_TEXT=()
}

parse_build_output_diagnostics() {
    local build_output="$1"
    clear_parsed_diagnostics

    local -a merged_records=()

    local rec
    while IFS= read -r rec || [ -n "$rec" ]; do
        merged_records+=("$rec")
    done < <(extract_new_diagnostic_records "$build_output")

    for rec in "${merged_records[@]}"; do
        [ -z "$rec" ] && continue
        local extractor severity code message file start_line start_col end_line end_col match_text
        IFS="$DIAG_RECORD_SEP" read -r extractor severity code message file start_line start_col end_line end_col match_text <<< "$rec"
        PARSED_DIAG_EXTRACTOR+=("$extractor")
        PARSED_DIAG_SEVERITY+=("$severity")
        PARSED_DIAG_CODE+=("$code")
        PARSED_DIAG_MESSAGE+=("$message")
        PARSED_DIAG_FILE+=("$file")
        PARSED_DIAG_START_LINE+=("$start_line")
        PARSED_DIAG_START_COL+=("$start_col")
        PARSED_DIAG_END_LINE+=("$end_line")
        PARSED_DIAG_END_COL+=("$end_col")
        PARSED_DIAG_MATCH_TEXT+=("$match_text")
    done
}

paths_equal_or_normalized() {
    local left="$1"
    local right="$2"

    [ "$left" = "$right" ] && return 0
    [ -z "$left" ] && return 1
    [ -z "$right" ] && return 1

    local left_norm="$left"
    local right_norm="$right"

    if [ -f "$left" ]; then
        left_norm=$(normalize_existing_file "$left")
    elif [ -f "$REPO_ROOT/$left" ]; then
        left_norm=$(normalize_existing_file "$REPO_ROOT/$left")
    fi

    if [ -f "$right" ]; then
        right_norm=$(normalize_existing_file "$right")
    elif [ -f "$REPO_ROOT/$right" ]; then
        right_norm=$(normalize_existing_file "$REPO_ROOT/$right")
    fi

    [ "$left_norm" = "$right_norm" ]
}

diag_bound_to_fixture_line() {
    local idx="$1"
    local fixture_file="$2"
    local diag_file="${PARSED_DIAG_FILE[$idx]}"
    local diag_line="${PARSED_DIAG_START_LINE[$idx]}"

    [ -z "$diag_file" ] && return 1
    [ -z "$diag_line" ] && return 1

    paths_equal_or_normalized "$diag_file" "$fixture_file"
}

format_diag_location() {
    local idx="$1"
    local file="${PARSED_DIAG_FILE[$idx]}"
    local start_line="${PARSED_DIAG_START_LINE[$idx]}"
    local start_col="${PARSED_DIAG_START_COL[$idx]}"
    local end_line="${PARSED_DIAG_END_LINE[$idx]}"
    local end_col="${PARSED_DIAG_END_COL[$idx]}"

    if [ -z "$file" ]; then
        echo "<no-span>"
        return
    fi

    if [ -z "$start_line" ]; then
        echo "$file"
        return
    fi

    if [ -z "$end_line" ]; then
        echo "$file:$start_line:$start_col"
    else
        echo "$file:$start_line:$start_col-$end_line:$end_col"
    fi
}

print_reject_debug_context() {
    local file="$1"
    local build_output="$2"

    echo "  Expectations:"
    local i
    for ((i = 0; i < ${#DIAG_VALUES[@]}; i++)); do
        echo "    - line ${DIAG_LINENOS[$i]}: '${DIAG_TYPES[$i]}: ${DIAG_VALUES[$i]}'"
    done

    echo "  Parsed diagnostics:"
    if [ "${#PARSED_DIAG_MESSAGE[@]}" -eq 0 ]; then
        echo "    - (none)"
    else
        for ((i = 0; i < ${#PARSED_DIAG_MESSAGE[@]}; i++)); do
            local loc
            loc=$(format_diag_location "$i")
            echo "    - [${PARSED_DIAG_EXTRACTOR[$i]}] ${PARSED_DIAG_SEVERITY[$i]} ${PARSED_DIAG_CODE[$i]} @ $loc: ${PARSED_DIAG_MESSAGE[$i]}"
        done
    fi

    echo "  Output head:"
    echo "$build_output" | sed -n '1,20p' | sed 's/^/    /'
}

run_mode() {
    local file="$1"
    local name="$2"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local binpath="$TEST_BUILD_DIR/marmoset_test_bin.$TOTAL"
    rm -f "$binpath"

    local build_output
    local actual_output
    if build_output=$($EXECUTABLE build "$file" -o "$binpath" 2>&1) && actual_output=$("$binpath" 2>&1); then
        local expected_output=""
        local first=true
        local line
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
            record_failed_test "$name" "output mismatch"
        fi
    else
        echo "✗ FAIL (build or execution failed)"
        echo "  Build output: $build_output"
        FAIL=$((FAIL + 1))
        record_failed_test "$name" "build or execution failed"
    fi

    rm -f "$binpath"
}

reject_mode() {
    local file="$1"
    local name="$2"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local binpath="$TEST_BUILD_DIR/marmoset_test_bin.$TOTAL"
    rm -f "$binpath"

    local build_output
    if build_output=$($EXECUTABLE build "$file" -o "$binpath" 2>&1); then
        echo "✗ FAIL (expected build failure but build succeeded)"
        FAIL=$((FAIL + 1))
        record_failed_test "$name" "expected build failure but build succeeded"
        rm -f "$binpath"
        return
    fi
    rm -f "$binpath"

    if validate_expected_diagnostics "$file" "$build_output"; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        FAIL=$((FAIL + 1))
        record_failed_test "$name" "diagnostic mismatch"
    fi
}

validate_expected_diagnostics() {
    local file="$1"
    local build_output="$2"

    local all_wildcard=true
    local wildcard_count=0
    local i
    for ((i = 0; i < ${#DIAG_VALUES[@]}; i++)); do
        if [ "${DIAG_VALUES[$i]}" = "*" ]; then
            wildcard_count=$((wildcard_count + 1))
        else
            all_wildcard=false
        fi
    done

    parse_build_output_diagnostics "$build_output"

    if [ "${#PARSED_DIAG_MESSAGE[@]}" -eq 0 ]; then
        echo "✗ FAIL (diagnostic extractor failure)"
        print_reject_debug_context "$file" "$build_output"
        return 1
    fi

    if $all_wildcard; then
        return 0
    fi

    local -a diag_used=()
    for ((i = 0; i < ${#PARSED_DIAG_MESSAGE[@]}; i++)); do
        diag_used+=(0)
    done

    local missing=""
    local line_mismatch=""

    local severity_mismatch=""

    # One-to-one expectation matching: duplicates in emitted diagnostics require
    # duplicate fixture expectations (unless wildcard is used).
    # Severity enforcement: annotation kind (error/warning/info) must match
    # the emitted diagnostic severity.
    # Line/file strictness: when a line-bound diagnostic exists for a given
    # expectation, only line-bound matches are accepted (no fallback to
    # diagnostics from unrelated lines/files).
    for ((i = 0; i < ${#DIAG_VALUES[@]}; i++)); do
        local val="${DIAG_VALUES[$i]}"
        [ "$val" = "*" ] && continue

        local expected_line="${DIAG_LINENOS[$i]}"
        local expected_severity="${DIAG_TYPES[$i]}"
        local has_line_bound=false
        local has_line_bound_match=false
        local preferred_idx=-1
        local fallback_idx=-1
        local severity_mismatch_idx=-1
        local j

        for ((j = 0; j < ${#PARSED_DIAG_MESSAGE[@]}; j++)); do
            local diag_text="${PARSED_DIAG_MATCH_TEXT[$j]}"
            if [[ "$diag_text" != *"$val"* ]]; then
                continue
            fi

            # Severity enforcement: text matches but wrong severity → not a match.
            local diag_sev="${PARSED_DIAG_SEVERITY[$j]}"
            if [ "$diag_sev" != "$expected_severity" ]; then
                if [ "${diag_used[$j]}" -eq 0 ] && [ "$severity_mismatch_idx" -lt 0 ]; then
                    severity_mismatch_idx=$j
                fi
                continue
            fi

            if [ "${diag_used[$j]}" -eq 0 ] && [ "$fallback_idx" -lt 0 ]; then
                fallback_idx=$j
            fi

            if diag_bound_to_fixture_line "$j" "$file"; then
                has_line_bound=true
                if [ "${PARSED_DIAG_START_LINE[$j]}" = "$expected_line" ]; then
                    has_line_bound_match=true
                    if [ "${diag_used[$j]}" -eq 0 ] && [ "$preferred_idx" -lt 0 ]; then
                        preferred_idx=$j
                    fi
                fi
            fi
        done

        if $has_line_bound && ! $has_line_bound_match; then
            line_mismatch="$line_mismatch\n  - line $expected_line: '${DIAG_TYPES[$i]}: $val' (diagnostic found on a different source line)"
        fi

        local chosen_idx="$preferred_idx"
        # Line/file strictness: when a line-bound diagnostic exists for this
        # expectation, do NOT fall back to an unrelated match.
        if [ "$chosen_idx" -lt 0 ] && ! $has_line_bound; then
            chosen_idx="$fallback_idx"
        fi

        if [ "$chosen_idx" -lt 0 ]; then
            # Don't report as "missing" if already tracked as line-mismatch.
            if $has_line_bound && ! $has_line_bound_match; then
                : # already recorded in line_mismatch above
            elif [ "$severity_mismatch_idx" -ge 0 ]; then
                local sev_found="${PARSED_DIAG_SEVERITY[$severity_mismatch_idx]}"
                severity_mismatch="$severity_mismatch\n  - line $expected_line: expected '$expected_severity: $val' but diagnostic has severity '$sev_found'"
            else
                missing="$missing\n  - line ${DIAG_LINENOS[$i]}: '${DIAG_TYPES[$i]}: $val'"
            fi
        else
            diag_used[$chosen_idx]=1
        fi
    done

    if [ -n "$missing" ]; then
        echo "✗ FAIL (missing expected diagnostics)"
        echo -e "  Missing:$missing"
        print_reject_debug_context "$file" "$build_output"
        return 1
    fi

    if [ -n "$line_mismatch" ]; then
        echo "✗ FAIL (line-mismatched diagnostics)"
        echo -e "  Mismatch:$line_mismatch"
        print_reject_debug_context "$file" "$build_output"
        return 1
    fi

    if [ -n "$severity_mismatch" ]; then
        echo "✗ FAIL (severity-mismatched diagnostics)"
        echo -e "  Mismatch:$severity_mismatch"
        print_reject_debug_context "$file" "$build_output"
        return 1
    fi

    # Wildcard scoping: each wildcard expectation absorbs one unmatched
    # diagnostic. Remaining unmatched diagnostics are unexpected → FAIL.
    local unmatched_count=0
    local unexpected=""
    for ((i = 0; i < ${#PARSED_DIAG_MESSAGE[@]}; i++)); do
        if [ "${diag_used[$i]}" -eq 0 ]; then
            unmatched_count=$((unmatched_count + 1))
            if [ "$unmatched_count" -gt "$wildcard_count" ]; then
                local loc
                loc=$(format_diag_location "$i")
                unexpected="$unexpected\n  - ${PARSED_DIAG_SEVERITY[$i]} ${PARSED_DIAG_CODE[$i]} @ $loc: ${PARSED_DIAG_MESSAGE[$i]}"
            fi
        fi
    done

    if [ -n "$unexpected" ]; then
        echo "✗ FAIL (unexpected diagnostics)"
        echo -e "  Unexpected:$unexpected"
        print_reject_debug_context "$file" "$build_output"
        return 1
    fi

    return 0
}

build_diagnostics_mode() {
    local file="$1"
    local name="$2"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local binpath="$TEST_BUILD_DIR/marmoset_test_bin.$TOTAL"
    rm -f "$binpath"

    local build_output
    if build_output=$($EXECUTABLE build "$file" -o "$binpath" 2>&1); then
        if validate_expected_diagnostics "$file" "$build_output"; then
            echo "✓ PASS"
            PASS=$((PASS + 1))
        else
            FAIL=$((FAIL + 1))
            record_failed_test "$name" "diagnostic mismatch"
        fi
    else
        echo "✗ FAIL (build failed)"
        echo "  Output: $build_output"
        FAIL=$((FAIL + 1))
        record_failed_test "$name" "build failed"
    fi

    rm -f "$binpath"
}

run_diagnostics_mode() {
    local file="$1"
    local name="$2"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local binpath="$TEST_BUILD_DIR/marmoset_test_bin.$TOTAL"
    rm -f "$binpath"

    local build_output
    local actual_output
    if build_output=$($EXECUTABLE build "$file" -o "$binpath" 2>&1); then
        if ! validate_expected_diagnostics "$file" "$build_output"; then
            FAIL=$((FAIL + 1))
            record_failed_test "$name" "diagnostic mismatch"
            rm -f "$binpath"
            return
        fi

        if actual_output=$("$binpath" 2>&1); then
            local expected_output=""
            local first=true
            local line
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
                record_failed_test "$name" "output mismatch"
            fi
        else
            echo "✗ FAIL (execution failed)"
            FAIL=$((FAIL + 1))
            record_failed_test "$name" "execution failed"
        fi
    else
        echo "✗ FAIL (build failed)"
        echo "  Output: $build_output"
        FAIL=$((FAIL + 1))
        record_failed_test "$name" "build failed"
    fi

    rm -f "$binpath"
}

build_only_mode() {
    local file="$1"
    local name="$2"

    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] $name ... "

    local binpath="$TEST_BUILD_DIR/marmoset_test_bin.$TOTAL"
    rm -f "$binpath"

    local build_output
    if build_output=$($EXECUTABLE build "$file" -o "$binpath" 2>&1); then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL (build failed)"
        echo "  Output: $build_output"
        FAIL=$((FAIL + 1))
        record_failed_test "$name" "build failed"
    fi

    rm -f "$binpath"
}

run_fixture() {
    local file="$1"
    local rel_path="${file#$FIXTURE_ROOT/}"
    local name="${rel_path%.mr}"

    if ! parse_fixture "$file"; then
        TOTAL=$((TOTAL + 1))
        echo -n "TEST [$TOTAL] $name ... "
        echo "✗ FAIL (annotation placement)"
        FAIL=$((FAIL + 1))
        record_failed_test "$name" "annotation placement"
        return
    fi

    if [ "$rel_path" = "runtime/monkey_example.mr" ] && ! monkey_fixture_in_sync "$file"; then
        TOTAL=$((TOTAL + 1))
        echo -n "TEST [$TOTAL] $name ... "
        echo "✗ FAIL (fixture drift)"
        FAIL=$((FAIL + 1))
        record_failed_test "$name" "fixture drift"
        return
    fi

    local pre_pass=$PASS
    local pre_fail=$FAIL

    case "$MODE" in
        run)
            run_mode "$file" "$name"
            ;;
        run-diagnostics)
            run_diagnostics_mode "$file" "$name"
            ;;
        reject)
            reject_mode "$file" "$name"
            ;;
        build-diagnostics)
            build_diagnostics_mode "$file" "$name"
            ;;
        build-only)
            build_only_mode "$file" "$name"
            ;;
    esac

    # xfail reclassification
    if [ "$XFAIL_ENABLED" -eq 1 ]; then
        if [ "$PASS" -gt "$pre_pass" ]; then
            # Test passed but xfail active → XPASS (stale xfail, counts as failure)
            PASS=$pre_pass
            XPASS=$((XPASS + 1))
            echo "  ^ XPASS — test passed but has xfail annotation; remove it"
        elif [ "$FAIL" -gt "$pre_fail" ]; then
            # Test failed and xfail active → XFAIL (expected failure)
            FAIL=$pre_fail
            XFAIL=$((XFAIL + 1))
            echo "  ^ XFAIL ($XFAIL_REASON)"
        fi
    fi
}

run_fixture_isolated() {
    local file="$1"
    local index="$2"

    TOTAL=$((index - 1))
    PASS=0
    FAIL=0
    XFAIL=0
    XPASS=0

    run_fixture "$file"

    # Emit result token for parallel aggregator
    if [ "$XPASS" -gt 0 ]; then
        echo "__RESULT__:XPASS"
        return 1
    elif [ "$XFAIL" -gt 0 ]; then
        echo "__RESULT__:XFAIL"
        return 0
    elif [ "$FAIL" -gt 0 ]; then
        echo "__RESULT__:FAIL"
        return 1
    else
        echo "__RESULT__:PASS"
        return 0
    fi
}

run_fixture_suite() {
    suite_begin "Fixture Tests"
    FAILED_TEST_NAMES=()
    FAILED_TEST_REASONS=()

    if [ "${#SELECTED_FIXTURES[@]}" -eq 0 ]; then
        echo "(no fixtures found — skipping)"
        PASS=0
        FAIL=0
        TOTAL=0
        local empty_rc=0
        suite_end || empty_rc=$?
        return "$empty_rc"
    fi

    local fixture_count=${#SELECTED_FIXTURES[@]}
    local fixture_jobs
    fixture_jobs=$(detect_fixture_jobs)
    if [ "$fixture_jobs" -gt "$fixture_count" ]; then
        fixture_jobs=$fixture_count
    fi

    if [ "$fixture_jobs" -le 1 ] || [ "$fixture_count" -le 1 ]; then
        local fixture
        for fixture in "${SELECTED_FIXTURES[@]}"; do
            run_fixture "$fixture"
        done
    else
        echo "Running $fixture_count fixtures in parallel with $fixture_jobs workers..."
        local log_dir
        log_dir=$(mktemp -d)
        local pids=()
        local logs=()
        local done=()

        PASS=0
        FAIL=0
        XFAIL=0
        XPASS=0
        TOTAL=0

        local next_index=0
        local running=0

        while [ "$TOTAL" -lt "$fixture_count" ]; do
            while [ "$running" -lt "$fixture_jobs" ] && [ "$next_index" -lt "$fixture_count" ]; do
                local fixture="${SELECTED_FIXTURES[$next_index]}"
                local fixture_index=$((next_index + 1))
                local log_file="$log_dir/$fixture_index.log"

                (run_fixture_isolated "$fixture" "$fixture_index") > "$log_file" 2>&1 &
                pids+=($!)
                logs+=("$log_file")
                done+=(0)

                running=$((running + 1))
                next_index=$((next_index + 1))
            done

            local progressed=0
            local i
            for i in "${!pids[@]}"; do
                if [ "${done[$i]}" -eq 1 ]; then
                    continue
                fi

                local pid="${pids[$i]}"
                local log_file="${logs[$i]}"

                if ! kill -0 "$pid" 2>/dev/null; then
                    wait "$pid" || true

                    # Parse __RESULT__ token from worker log
                    local result_token
                    result_token=$(grep -o '__RESULT__:[A-Z]*' "$log_file" 2>/dev/null | tail -1 | cut -d: -f2)
                    case "$result_token" in
                        XFAIL) XFAIL=$((XFAIL + 1)) ;;
                        XPASS) XPASS=$((XPASS + 1)) ;;
                        FAIL)
                            FAIL=$((FAIL + 1))
                            collect_failure_from_log "$log_file"
                            ;;
                        *)     PASS=$((PASS + 1)) ;;
                    esac
                    TOTAL=$((TOTAL + 1))
                    running=$((running - 1))
                    # Print worker log but strip the __RESULT__ token line
                    grep -v '^__RESULT__:' "$log_file" || true

                    done[$i]=1
                    progressed=1
                fi
            done

            if [ "$TOTAL" -lt "$fixture_count" ] && [ "$progressed" -eq 0 ]; then
                sleep 0.05
            fi
        done

        rm -rf "$log_dir"
    fi

    local suite_rc=0
    suite_end || suite_rc=$?
    if [ "$suite_rc" -ne 0 ]; then
        print_failed_tests_summary
    fi
    return "$suite_rc"
}

# --- Target selection ---

run_cli=0
run_harness=0
selected_groups=()
selected_fixture_files=()
if [ "$#" -eq 0 ]; then
    selected_groups=("${ALL_GROUPS[@]}")
else
    for arg in "$@"; do
        fixture_file=$(resolve_fixture_file_selector "$arg" || true)
        if [ -n "$fixture_file" ]; then
            selected_fixture_files+=("$fixture_file")
            continue
        fi

        resolved=$(resolve_selector "$arg") || {
            echo "Unknown selector: $arg" >&2
            print_usage
            exit 2
        }
        for token in $resolved; do
            if [ "$token" = "__CLI__" ]; then
                run_cli=1
            elif [ "$token" = "__HARNESS__" ]; then
                run_harness=1
            else
                selected_groups+=("$token")
            fi
        done
    done
fi

# Deduplicate groups while preserving order.
unique_groups=()
for group in "${selected_groups[@]}"; do
    already_seen=0
    for existing in "${unique_groups[@]}"; do
        if [ "$existing" = "$group" ]; then
            already_seen=1
            break
        fi
    done
    if [ "$already_seen" -eq 0 ]; then
        unique_groups+=("$group")
    fi
done

if [ "${#unique_groups[@]}" -eq 0 ] && [ "${#selected_fixture_files[@]}" -eq 0 ] && [ "$run_cli" -eq 0 ] && [ "$run_harness" -eq 0 ]; then
    echo "No test targets selected." >&2
    print_usage
    exit 2
fi

# Resolve fixtures from groups.
SELECTED_FIXTURES=()
for group in "${unique_groups[@]}"; do
    while IFS= read -r f; do
        SELECTED_FIXTURES+=("$f")
    done < <(find "$FIXTURE_ROOT/$group" -type f -name '*.mr' | LC_ALL=C sort)
done

for fixture in "${selected_fixture_files[@]}"; do
    SELECTED_FIXTURES+=("$fixture")
done

# Deduplicate fixtures while preserving order.
unique_fixtures=()
for fixture in "${SELECTED_FIXTURES[@]}"; do
    already_seen=0
    for existing in "${unique_fixtures[@]}"; do
        if [ "$existing" = "$fixture" ]; then
            already_seen=1
            break
        fi
    done
    if [ "$already_seen" -eq 0 ]; then
        unique_fixtures+=("$fixture")
    fi
done
SELECTED_FIXTURES=("${unique_fixtures[@]}")

# Shared build step for all selected integration targets.
echo "Building project..."
(cd "$REPO_ROOT" && dune build "$BUILD_TARGET")
export MARMOSET_SKIP_BUILD=1

suite_pass=0
suite_fail=0
suite_count=0

if [ "${#SELECTED_FIXTURES[@]}" -gt 0 ]; then
    suite_count=$((suite_count + 1))
    echo ""
    if run_fixture_suite; then
        suite_pass=$((suite_pass + 1))
    else
        suite_fail=$((suite_fail + 1))
    fi
fi

if [ "$run_cli" -eq 1 ]; then
    suite_count=$((suite_count + 1))
    echo ""
    if "$CLI_SUITE"; then
        suite_pass=$((suite_pass + 1))
    else
        suite_fail=$((suite_fail + 1))
    fi
fi

if [ "$run_harness" -eq 1 ]; then
    suite_count=$((suite_count + 1))
    echo ""
    if "$HARNESS_SUITE"; then
        suite_pass=$((suite_pass + 1))
    else
        suite_fail=$((suite_fail + 1))
    fi
fi

if [ "$suite_fail" -ne 0 ]; then
    exit 1
fi
