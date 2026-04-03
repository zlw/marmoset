#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

SNAPSHOT_ROOT="$REPO_ROOT/test/snapshots/go_normalized"
CANARY_FIXTURE="$REPO_ROOT/test/fixtures/codegen_canary/cc01_pre_modules_mixed_feature_canary.mr"

run_canary_structural_assertions() {
    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] mixed-feature canary structural contracts ... "

    local outdir binpath build_output main_go failures dyn_count record_count
    outdir=$(mktemp -d marmoset_emit_canary.XXXXXX)
    binpath=$(mktemp "$REPO_ROOT/.marmoset/build/marmoset_canary_bin.XXXXXX")
    rm -f "$binpath"

    if ! build_output=$($EXECUTABLE build "$CANARY_FIXTURE" --emit-go "$outdir" -o "$binpath" 2>&1); then
        echo "✗ FAIL (build failed)"
        echo "  Output: $build_output"
        FAIL=$((FAIL + 1))
        rm -f "$binpath"
        rm -rf "$outdir"
        return
    fi

    main_go="$outdir/main.go"
    failures=()

    if ! grep -q 'func render_twice_int64_fn_int64_string' "$main_go"; then
        failures+=("missing Int specialization for render_twice")
    fi
    if ! grep -q 'func render_twice_record_name_string_score_int64_closed_fn_record_name_string_score_int64_closed_string' "$main_go"; then
        failures+=("missing Person specialization for render_twice")
    fi
    if ! grep -q 'switch __scrutinee_2.Tag' "$main_go"; then
        failures+=("missing enum-switch lowering for canary match")
    fi
    if ! grep -q 'switch input_typed := input.(type)' "$main_go"; then
        failures+=("missing union narrowing type-switch")
    fi
    if ! grep -q 'func(p Person) string {' "$main_go"; then
        failures+=("missing nested closure in specialized caller")
    fi
    if grep -q ' else if  {' "$main_go"; then
        failures+=("found malformed empty else-if fallback")
    fi

    dyn_count=$(grep -o 'marmosetDyn{typeID: "Int"' "$main_go" | wc -l | tr -d ' ')
    if [ "$dyn_count" != "2" ]; then
        failures+=("expected 2 recorded Dyn packaging sites, found $dyn_count")
    fi

    record_count=$(grep -c '^type Record_box_record_tag_int64_value_marmoset_dyn_closed_owner_record_name_string_score_int64_closed struct' "$main_go")
    if [ "$record_count" != "1" ]; then
        failures+=("expected one interned mixed record-shape type, found $record_count")
    fi

    if [ "${#failures[@]}" -eq 0 ]; then
        echo "✓ PASS"
        PASS=$((PASS + 1))
    else
        echo "✗ FAIL"
        local failure
        for failure in "${failures[@]}"; do
            echo "  $failure"
        done
        FAIL=$((FAIL + 1))
    fi

    rm -f "$binpath"
    rm -rf "$outdir"
}

suite_begin "Pre-Modules Hardening Regression Tests"

test_emit_go_normalized_snapshot \
    "mixed-feature canary normalized main.go stays stable" \
    "$CANARY_FIXTURE" \
    "$SNAPSHOT_ROOT/cc01_pre_modules_mixed_feature_canary.main.go"

run_canary_structural_assertions

expect_reject \
    "duplicate trait definitions stay rejected" \
    "Duplicate trait definition" \
    "$(cat "$REPO_ROOT/test/fixtures/traits/t03_duplicate_trait_definition_is_rejected_in_one_program.mr")"

expect_reject \
    "duplicate impls stay rejected" \
    "Duplicate impl for trait 'show' and type Int" \
    "$(cat "$REPO_ROOT/test/fixtures/traits/t36_duplicate_impl_for_same_trait_and_type_is_rejected.mr")"

expect_reject \
    "conflicting shape guarantees stay rejected" \
    "Conflicting field type" \
    "$(cat "$REPO_ROOT/test/fixtures/traits_field/tf13_multiple_constraints_with_same_field_name_but_different_type.mr")"

expect_run \
    "shape annotations keep guaranteed fields from wider records" \
    "test" \
    "$(cat "$REPO_ROOT/test/fixtures/traits_field/tf29_trait_object_hides_extra_fields_while_constrained_generic_al.mr")"

expect_run \
    "mixed-feature canary runs end to end" \
    "tiny
ada:7:2:2
42:3
milo!={ name: milo!, score: 8 }:42:3
milo!?|milo!?
3|3
int:5
str:ok" \
    "$(cat "$CANARY_FIXTURE")"

suite_end
