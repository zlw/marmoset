#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

SNAPSHOT_ROOT="$REPO_ROOT/test/snapshots/go_normalized"
CANARY_FIXTURE="$REPO_ROOT/test/fixtures/codegen_canary/cc01_pre_modules_mixed_feature_canary.mr"
UNION_FALLBACK_FIXTURE="$REPO_ROOT/test/fixtures/codegen_canary/cc02_union_record_match_catch_all.mr"
SPREAD_DYN_FIXTURE="$REPO_ROOT/test/fixtures/codegen_canary/cc03_record_spread_dyn_projection.mr"
SPREAD_DYN_CONTROL_FLOW_FIXTURE="$REPO_ROOT/test/fixtures/codegen_mono/cm52_record_spread_update_packages_expected_dyn_field.mr"
UNION_VARIABLE_FALLBACK_FIXTURE="$REPO_ROOT/test/fixtures/codegen_canary/cc04_union_record_match_variable_catch_all.mr"
TRAIT_DYN_PACK_FIXTURE="$REPO_ROOT/test/fixtures/vnext_canary/vn114_derived_dyn_pack_inherits_supertrait_methods.mr"
DEFAULT_DYN_BOX_FIXTURE="$REPO_ROOT/test/fixtures/vnext_canary/vn117_generic_record_derived_default_returns_dyn_show.mr"
OVERRIDE_CALLBACK_FIXTURE="$REPO_ROOT/test/fixtures/vnext_canary/vn112_qualified_child_trait_override_generic_call.mr"
POLY_CALLBACK_FIXTURE="$REPO_ROOT/test/fixtures/runtime/p105_local_poly_helper_callback_through_hof_two_types.mr"
MATCH_CLOSURE_FIXTURE="$REPO_ROOT/test/fixtures/codegen_stress/cs49_h50_closure_defined_inside_match_arm.mr"

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
    if ! grep -q 'type marmosetDyn struct{ payload any; witness any }' "$main_go"; then
        failures+=("missing slim Dyn runtime shape without dead typeID metadata")
    fi
    if ! grep -q 'var __marmoset_dyn_witness_marmosetDynWitness_show_int64 = marmosetDynWitness_show{show: __marmoset_dyn_adapter_marmosetDynWitness_show_int64_show}' "$main_go"; then
        failures+=("missing shared Int Dyn witness singleton")
    fi
    if grep -q 'marmosetDyn{payload: (func()' "$main_go" || grep -q 'marmosetDyn{payload: func()' "$main_go"; then
        failures+=("found Dyn packaging that still embeds a control-flow IIFE payload")
    fi

    dyn_count=$(grep -o 'witness: __marmoset_dyn_witness_marmosetDynWitness_show_int64' "$main_go" | wc -l | tr -d ' ')
    if [ "$dyn_count" != "2" ]; then
        failures+=("expected 2 Dyn packaging sites using the shared Int witness, found $dyn_count")
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

run_trait_dyn_pack_structural_assertions() {
    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] derived child-trait Dyn witnesses keep inherited methods ... "

    local outdir binpath build_output main_go failures label_calls
    outdir=$(mktemp -d marmoset_emit_trait_dyn.XXXXXX)
    binpath=$(mktemp "$REPO_ROOT/.marmoset/build/marmoset_trait_dyn_bin.XXXXXX")
    rm -f "$binpath"

    if ! build_output=$($EXECUTABLE build "$TRAIT_DYN_PACK_FIXTURE" --emit-go "$outdir" -o "$binpath" 2>&1); then
        echo "✗ FAIL (build failed)"
        echo "  Output: $build_output"
        FAIL=$((FAIL + 1))
        rm -f "$binpath"
        rm -rf "$outdir"
        return
    fi

    main_go="$outdir/main.go"
    failures=()

    if ! grep -q 'type marmosetDynWitness_Pack struct{label func(any) string; show func(any) string}' "$main_go"; then
        failures+=("missing Pack witness carrying inherited label and show methods")
    fi
    if ! grep -q 'func __marmoset_dyn_adapter_marmosetDynWitness_Pack_record_x_int64_closed_label(__receiver any) string {' "$main_go"; then
        failures+=("missing shared Dyn adapter for inherited label method")
    fi
    if ! grep -q 'var __marmoset_dyn_witness_marmosetDynWitness_Pack_record_x_int64_closed = marmosetDynWitness_Pack{label: __marmoset_dyn_adapter_marmosetDynWitness_Pack_record_x_int64_closed_label, show: __marmoset_dyn_adapter_marmosetDynWitness_Pack_record_x_int64_closed_show}' "$main_go"; then
        failures+=("missing shared Pack witness singleton for Point")
    fi
    if ! grep -q 'func Label_label_record_x_int64_closed(self Point) string {' "$main_go"; then
        failures+=("missing inherited Label helper for derived Point impl")
    fi
    if ! grep -q 'func Pack_label_record_x_int64_closed(self Point) string {' "$main_go"; then
        failures+=("missing Pack forwarding helper for derived Point impl")
    fi

    label_calls=$(grep -c 'apply_record_x_int64_closed_fn_record_x_int64_closed_string(self, func(x Point) string {' "$main_go")
    if [ "$label_calls" != "2" ]; then
        failures+=("expected 2 lambda-backed label calls through apply, found $label_calls")
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

run_default_dyn_box_structural_assertions() {
    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] derived default Dyn boxing keeps concrete payloads ... "

    local outdir binpath build_output main_go failures dyn_count
    outdir=$(mktemp -d marmoset_emit_default_dyn.XXXXXX)
    binpath=$(mktemp "$REPO_ROOT/.marmoset/build/marmoset_default_dyn_bin.XXXXXX")
    rm -f "$binpath"

    if ! build_output=$($EXECUTABLE build "$DEFAULT_DYN_BOX_FIXTURE" --emit-go "$outdir" -o "$binpath" 2>&1); then
        echo "✗ FAIL (build failed)"
        echo "  Output: $build_output"
        FAIL=$((FAIL + 1))
        rm -f "$binpath"
        rm -rf "$outdir"
        return
    fi

    main_go="$outdir/main.go"
    failures=()

    if ! grep -q 'type marmosetDynWitness_show struct{show func(any) string}' "$main_go"; then
        failures+=("missing Dyn[Show] witness shape for derived box method")
    fi
    if ! grep -q 'func __marmoset_dyn_adapter_marmosetDynWitness_show_record_value_int64_closed_show(__receiver any) string {' "$main_go"; then
        failures+=("missing shared Dyn adapter for the derived record impl")
    fi
    if ! grep -q 'var __marmoset_dyn_witness_marmosetDynWitness_show_record_value_int64_closed = marmosetDynWitness_show{show: __marmoset_dyn_adapter_marmosetDynWitness_show_record_value_int64_closed_show}' "$main_go"; then
        failures+=("missing shared Dyn witness singleton for the derived record impl")
    fi
    if ! grep -q 'func Boxed_box_record_value_int64_closed(self Record_value_int64) marmosetDyn {' "$main_go"; then
        failures+=("missing derived Boxed.box helper for concrete record instantiation")
    fi

    dyn_count=$(grep -o 'witness: __marmoset_dyn_witness_marmosetDynWitness_show_record_value_int64_closed' "$main_go" | wc -l | tr -d ' ')
    if [ "$dyn_count" != "1" ]; then
        failures+=("expected 1 direct concrete Dyn packaging site using the shared witness, found $dyn_count")
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

run_spread_dyn_structural_assertions() {
    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] spread updates hoist Dyn payload control flow before packaging ... "

    local outdir binpath build_output main_go failures
    outdir=$(mktemp -d marmoset_emit_spread_dyn.XXXXXX)
    binpath=$(mktemp "$REPO_ROOT/.marmoset/build/marmoset_spread_dyn_bin.XXXXXX")
    rm -f "$binpath"

    if ! build_output=$($EXECUTABLE build "$SPREAD_DYN_CONTROL_FLOW_FIXTURE" --emit-go "$outdir" -o "$binpath" 2>&1); then
        echo "✗ FAIL (build failed)"
        echo "  Output: $build_output"
        FAIL=$((FAIL + 1))
        rm -f "$binpath"
        rm -rf "$outdir"
        return
    fi

    main_go="$outdir/main.go"
    failures=()

    if ! grep -Eq 'var __field_payload_[0-9]+ int64' "$main_go"; then
        failures+=("missing hoisted payload temp for Dyn-packaged spread update")
    fi
    if grep -q 'marmosetDyn{payload: (func() int64 {' "$main_go" || grep -q 'marmosetDyn{payload: func() int64 {' "$main_go"; then
        failures+=("Dyn packaging still embeds an expression-position payload IIFE")
    fi
    if ! grep -q 'witness: __marmoset_dyn_witness_marmosetDynWitness_show_int64' "$main_go"; then
        failures+=("spread update no longer reuses the shared Int Dyn witness")
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

run_override_callback_structural_assertions() {
    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] qualified override calls keep lifted callback helpers ... "

    local outdir binpath build_output main_go failures
    outdir=$(mktemp -d marmoset_emit_override_callback.XXXXXX)
    binpath=$(mktemp "$REPO_ROOT/.marmoset/build/marmoset_override_callback_bin.XXXXXX")
    rm -f "$binpath"

    if ! build_output=$($EXECUTABLE build "$OVERRIDE_CALLBACK_FIXTURE" --emit-go "$outdir" -o "$binpath" 2>&1); then
        echo "✗ FAIL (build failed)"
        echo "  Output: $build_output"
        FAIL=$((FAIL + 1))
        rm -f "$binpath"
        rm -rf "$outdir"
        return
    fi

    main_go="$outdir/main.go"
    failures=()

    if ! grep -Eq '^func __section_.*_int64\(it int64\) int64 \{$' "$main_go"; then
        failures+=("missing lifted helper for placeholder callback passed to Child.cast")
    fi
    if ! grep -q 'func Child_cast_int64__int64(x int64, f func(int64) int64) int64 {' "$main_go"; then
        failures+=("missing specialized override helper for Child.cast")
    fi
    if ! grep -q 'return f(x)' "$main_go"; then
        failures+=("override helper no longer forwards the callback directly")
    fi
    if ! grep -Eq '_ = puts\(Child_cast_int64__int64\(int64\(1\), __section_.*_int64\)\)' "$main_go"; then
        failures+=("main no longer routes the qualified override through the lifted helper")
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

run_poly_callback_structural_assertions() {
    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] local polymorphic callbacks specialize per HOF use ... "

    local outdir binpath build_output main_go failures
    outdir=$(mktemp -d marmoset_emit_poly_callback.XXXXXX)
    binpath=$(mktemp "$REPO_ROOT/.marmoset/build/marmoset_poly_callback_bin.XXXXXX")
    rm -f "$binpath"

    if ! build_output=$($EXECUTABLE build "$POLY_CALLBACK_FIXTURE" --emit-go "$outdir" -o "$binpath" 2>&1); then
        echo "✗ FAIL (build failed)"
        echo "  Output: $build_output"
        FAIL=$((FAIL + 1))
        rm -f "$binpath"
        rm -rf "$outdir"
        return
    fi

    main_go="$outdir/main.go"
    failures=()

    if ! grep -Eq '^func __local_same_.*_int64_int64\(x int64, y int64\) bool \{$' "$main_go"; then
        failures+=("missing Int specialization for lifted local callback")
    fi
    if ! grep -Eq '^func __local_same_.*_string_string\(x string, y string\) bool \{$' "$main_go"; then
        failures+=("missing String specialization for lifted local callback")
    fi
    if ! grep -q 'if apply2_fn_int64_fn_int64_bool_int64_int64(' "$main_go"; then
        failures+=("run() no longer uses the specialized Int higher-order helper")
    fi
    if ! grep -q 'if apply2_fn_string_fn_string_bool_string_string(' "$main_go"; then
        failures+=("run() no longer uses the specialized String higher-order helper")
    fi
    if grep -q '^func apply2_union_' "$main_go"; then
        failures+=("dead union-only higher-order helper is still emitted")
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

run_match_closure_structural_assertions() {
    TOTAL=$((TOTAL + 1))
    echo -n "TEST [$TOTAL] match-arm closures stay branch-local after lowering ... "

    local outdir binpath build_output main_go failures closure_count
    outdir=$(mktemp -d marmoset_emit_match_closure.XXXXXX)
    binpath=$(mktemp "$REPO_ROOT/.marmoset/build/marmoset_match_closure_bin.XXXXXX")
    rm -f "$binpath"

    if ! build_output=$($EXECUTABLE build "$MATCH_CLOSURE_FIXTURE" --emit-go "$outdir" -o "$binpath" 2>&1); then
        echo "✗ FAIL (build failed)"
        echo "  Output: $build_output"
        FAIL=$((FAIL + 1))
        rm -f "$binpath"
        rm -rf "$outdir"
        return
    fi

    main_go="$outdir/main.go"
    failures=()

    if ! grep -q 'switch __scrutinee_0 {' "$main_go"; then
        failures+=("missing switch lowering for the closure-producing match")
    fi
    if ! grep -q 'case int64(1):' "$main_go"; then
        failures+=("missing concrete match arm in lowered closure switch")
    fi
    if ! grep -q 'default:' "$main_go"; then
        failures+=("missing default arm in lowered closure switch")
    fi

    closure_count=$(grep -c 'f = func(x int64) int64 {' "$main_go")
    if [ "$closure_count" != "2" ]; then
        failures+=("expected 2 branch-local closure literals, found $closure_count")
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
run_trait_dyn_pack_structural_assertions
run_default_dyn_box_structural_assertions
run_spread_dyn_structural_assertions
run_override_callback_structural_assertions
run_poly_callback_structural_assertions
run_match_closure_structural_assertions

expect_reject \
    "duplicate trait definitions stay rejected" \
    "Duplicate trait definition" \
    "$(cat "$REPO_ROOT/test/fixtures/traits/t03_duplicate_trait_definition_is_rejected_in_one_program.mr")"

expect_reject \
    "duplicate impls stay rejected" \
    "Duplicate impl registration for trait 'show' and type Int" \
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

expect_run \
    "mixed record and scalar union members still reach catch-all arms" \
    "99" \
    "$(cat "$UNION_FALLBACK_FIXTURE")"

expect_run \
    "mixed record and scalar union members still narrow variable catch-all arms" \
    "6" \
    "$(cat "$UNION_VARIABLE_FALLBACK_FIXTURE")"

expect_run \
    "record spread copies package values into expected Dyn fields" \
    "1" \
    "$(cat "$SPREAD_DYN_FIXTURE")"

suite_end
