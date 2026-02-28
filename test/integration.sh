#!/bin/bash
# Aggregated integration runner for all integration suites in test/integration/.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
INTEGRATION_DIR="$SCRIPT_DIR/integration"
EXECUTABLE="$REPO_ROOT/_build/default/bin/main.exe"
BUILD_TARGET="./_build/default/bin/main.exe"

discover_suites() {
    find "$INTEGRATION_DIR" -maxdepth 1 -type f -name "*.sh" ! -name "common.sh" -print \
        | sed "s#^.*/##" \
        | LC_ALL=C sort
}

ALL_SUITES=()
while IFS= read -r suite; do
    ALL_SUITES+=("$suite")
done < <(discover_suites)

if [ "${#ALL_SUITES[@]}" -eq 0 ]; then
    echo "No integration suites found in $INTEGRATION_DIR" >&2
    exit 2
fi

resolve_suite() {
    local name="$1"
    local matches=()

    if [ "$name" = "all" ]; then
        echo "${ALL_SUITES[*]}"
        return 0
    fi

    # Exact filename.
    if [ -f "$INTEGRATION_DIR/$name" ]; then
        echo "$name"
        return 0
    fi

    # Exact basename without .sh.
    if [ -f "$INTEGRATION_DIR/$name.sh" ]; then
        echo "$name.sh"
        return 0
    fi

    # Group by suite stem prefix:
    #   make integration traits   -> 04_traits*.sh
    #   make integration codegen  -> 06_codegen*.sh
    #   make integration runtime  -> 07_runtime*.sh
    for suite in "${ALL_SUITES[@]}"; do
        local stem="${suite%.sh}"
        local short="$stem"
        if [[ "$stem" == *_* ]]; then
            short="${stem#*_}"
        fi

        if [[ "$short" == "$name" || "$short" == "$name"_* ]]; then
            matches+=("$suite")
        fi
    done

    if [ "${#matches[@]}" -gt 0 ]; then
        echo "${matches[*]}"
        return 0
    fi

    return 1
}

print_usage() {
    cat <<USAGE
Usage:
  ./test/integration.sh                 # run all suites
  ./test/integration.sh <suite> [...]   # run selected suites

Suites:
  all
  <suite.sh>           # exact filename
  <suite-stem>         # exact stem without .sh
  <group-prefix>       # e.g. traits, codegen, runtime
USAGE
}

selected_suites=()
if [ "$#" -eq 0 ]; then
    selected_suites=("${ALL_SUITES[@]}")
else
    for arg in "$@"; do
        resolved=$(resolve_suite "$arg") || {
            echo "Unknown suite: $arg" >&2
            print_usage
            exit 2
        }
        for suite in $resolved; do
            selected_suites+=("$suite")
        done
    done
fi

# Deduplicate while preserving order
unique_suites=()
for suite in "${selected_suites[@]}"; do
    already_seen=0
    for existing in "${unique_suites[@]}"; do
        if [ "$existing" = "$suite" ]; then
            already_seen=1
            break
        fi
    done
    if [ "$already_seen" -eq 0 ]; then
        unique_suites+=("$suite")
    fi
done

echo "Building project..."
(cd "$REPO_ROOT" && dune build "$BUILD_TARGET")
export MARMOSET_SKIP_BUILD=1

suite_pass=0
suite_fail=0

for suite in "${unique_suites[@]}"; do
    echo ""
    if "$INTEGRATION_DIR/$suite"; then
        suite_pass=$((suite_pass + 1))
    else
        suite_fail=$((suite_fail + 1))
    fi
done

echo ""
echo "=========================================="
echo "SUITE RESULTS: $suite_pass passed, $suite_fail failed out of ${#unique_suites[@]} suites"
echo "=========================================="

if [ "$suite_fail" -eq 0 ]; then
    echo "✓ ALL SUITES PASSED"
else
    echo "✗ SOME SUITES FAILED"
    exit 1
fi
