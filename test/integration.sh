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

source "$INTEGRATION_DIR/common.sh"

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
  <group>              # exact fixture group (e.g. traits, runtime)
  <group-prefix>       # prefix match (e.g. codegen -> codegen/codegen_*)
  <group>/<file>.mr    # single fixture file under test/fixtures
  test/fixtures/...mr  # fixture file path from repo root

Env:
  MARMOSET_STRICT_REJECT=1  # enforce two-way diagnostic coverage in reject mode
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

validate_annotation_placement() {
    local file="$1"
    local -a lines
    while IFS= read -r line || [ -n "$line" ]; do
        lines+=("$line")
    done < "$file"

    local total=${#lines[@]}
    local lineno
    for ((lineno = 1; lineno <= total; lineno++)); do
        local line="${lines[$((lineno - 1))]}"
        if [[ ! "$line" =~ ^[[:space:]]*#[[:space:]]*(output|error|warning|info):[[:space:]]*(.*)$ ]]; then
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

    return 0
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

    local lineno=0
    local has_diag=false
    local has_output=false

    while IFS= read -r line || [ -n "$line" ]; do
        lineno=$((lineno + 1))

        if [[ "$line" =~ ^[^#]*#[[:space:]]*output:[[:space:]]*(.*) ]]; then
            OUTPUT_LINES+=("${BASH_REMATCH[1]}")
            has_output=true
        fi

        if [[ "$line" =~ ^[^#]*#[[:space:]]*error:[[:space:]]*(.*) ]]; then
            DIAG_TYPES+=("error")
            DIAG_VALUES+=("${BASH_REMATCH[1]}")
            DIAG_LINENOS+=("$lineno")
            has_diag=true
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

    if $has_diag; then
        MODE="reject"
    elif $has_output; then
        MODE="run"
    else
        MODE="build-only"
    fi
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
    local strict_reject=false
    case "${MARMOSET_STRICT_REJECT:-0}" in
        1|true|TRUE|yes|YES|on|ON)
            strict_reject=true
            ;;
    esac

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

    local all_wildcard=true
    local i
    for ((i = 0; i < ${#DIAG_VALUES[@]}; i++)); do
        if [ "${DIAG_VALUES[$i]}" != "*" ]; then
            all_wildcard=false
            break
        fi
    done

    if $all_wildcard; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
        return
    fi

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

    if ! $strict_reject; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
        return
    fi

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
    done < <(echo "$build_output" \
        | grep -iE '^[^:]*:[0-9]+:[0-9]+.*error|^(Type |Parse )?[Ee]rror|^[Ww]arning|^[Ii]nfo' \
        | grep -vE '^Error:[[:space:]]+Go build failed$' \
        || true)

    if [ -n "$unexpected" ]; then
        echo "✗ FAIL (unexpected diagnostics)"
        echo -e "  Unexpected:$unexpected"
        FAIL=$((FAIL + 1))
        return
    fi

    echo "✓ PASS"
    PASS=$((PASS + 1))
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

run_fixture() {
    local file="$1"
    local rel_path="${file#$FIXTURE_ROOT/}"
    local name="${rel_path%.mr}"

    if ! validate_annotation_placement "$file"; then
        TOTAL=$((TOTAL + 1))
        echo -n "TEST [$TOTAL] $name ... "
        echo "✗ FAIL (annotation placement)"
        FAIL=$((FAIL + 1))
        return
    fi

    if [ "$rel_path" = "runtime/monkey_example.mr" ] && ! monkey_fixture_in_sync "$file"; then
        TOTAL=$((TOTAL + 1))
        echo -n "TEST [$TOTAL] $name ... "
        echo "✗ FAIL (fixture drift)"
        FAIL=$((FAIL + 1))
        return
    fi

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

run_fixture_isolated() {
    local file="$1"
    local index="$2"

    TOTAL=$((index - 1))
    PASS=0
    FAIL=0

    run_fixture "$file"
    [ "$FAIL" -eq 0 ]
}

run_fixture_suite() {
    suite_begin "Fixture Tests"

    if [ "${#SELECTED_FIXTURES[@]}" -eq 0 ]; then
        echo "(no fixtures found — skipping)"
        PASS=0
        FAIL=0
        TOTAL=0
        suite_end
        return 0
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
                    if wait "$pid"; then
                        PASS=$((PASS + 1))
                    else
                        FAIL=$((FAIL + 1))
                    fi
                    TOTAL=$((TOTAL + 1))
                    running=$((running - 1))
                    cat "$log_file"

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

    suite_end
}

# --- Target selection ---

run_cli=0
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

if [ "${#unique_groups[@]}" -eq 0 ] && [ "${#selected_fixture_files[@]}" -eq 0 ] && [ "$run_cli" -eq 0 ]; then
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

if [ "$suite_fail" -ne 0 ]; then
    exit 1
fi
