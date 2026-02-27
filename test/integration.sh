#!/bin/bash
# Aggregated integration runner for typecheck + codegen suites.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
INTEGRATION_DIR="$SCRIPT_DIR/integration"
EXECUTABLE="$REPO_ROOT/_build/default/bin/main.exe"
BUILD_TARGET="./_build/default/bin/main.exe"

ALL_SUITES=(
  "01_core_annotations.sh"
  "02_unions.sh"
  "03_enums.sh"
  "04_traits.sh"
  "05_records.sh"
  "06_codegen.sh"
  "07_runtime_output.sh"
)

resolve_suite() {
    local name="$1"
    case "$name" in
        all) echo "${ALL_SUITES[*]}" ;;
        core) echo "01_core_annotations.sh" ;;
        unions) echo "02_unions.sh" ;;
        enums) echo "03_enums.sh" ;;
        traits) echo "04_traits.sh" ;;
        records) echo "05_records.sh" ;;
        codegen) echo "06_codegen.sh" ;;
        runtime) echo "07_runtime_output.sh" ;;
        *.sh)
            if [ -f "$INTEGRATION_DIR/$name" ]; then
                echo "$name"
            else
                return 1
            fi
            ;;
        *)
            return 1
            ;;
    esac
}

print_usage() {
    cat <<USAGE
Usage:
  ./test/integration.sh                 # run all suites
  ./test/integration.sh <suite> [...]   # run selected suites

Suites:
  core
  unions
  enums
  traits
  records
  codegen
  runtime
  all
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
