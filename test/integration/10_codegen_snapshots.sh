#!/bin/bash

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck source=./common.sh
source "$SCRIPT_DIR/common.sh"

SNAPSHOT_ROOT="$REPO_ROOT/test/snapshots/go_exact"

suite_begin "Generated Go Exact Snapshot Tests"

test_emit_go_exact_snapshot \
    "nested constructor payload patterns lower stably" \
    "$REPO_ROOT/test/fixtures/unions/u92_nested_constructor_payload_pattern_lowers.mr" \
    "$SNAPSHOT_ROOT/u92_nested_constructor_payload_pattern_lowers.main.go"

test_emit_go_exact_snapshot \
    "nested record pattern lowering stays stable" \
    "$REPO_ROOT/test/fixtures/records/r187_nested_record_pattern_match_lowers.mr" \
    "$SNAPSHOT_ROOT/r187_nested_record_pattern_match_lowers.main.go"

test_emit_go_exact_snapshot \
    "recursive nested or-pattern lowering stays stable" \
    "$REPO_ROOT/test/fixtures/unions/u94_recursive_nested_or_pattern_lowers.mr" \
    "$SNAPSHOT_ROOT/u94_recursive_nested_or_pattern_lowers.main.go"

test_emit_go_exact_snapshot \
    "expected Dyn packaging in record spread update stays stable" \
    "$REPO_ROOT/test/fixtures/codegen_mono/cm52_record_spread_update_packages_expected_dyn_field.mr" \
    "$SNAPSHOT_ROOT/cm52_record_spread_update_packages_expected_dyn_field.main.go"

test_emit_go_exact_snapshot \
    "heterogeneous expected Dyn constructor contexts lower stably" \
    "$REPO_ROOT/test/fixtures/codegen_mono/cm54_expected_dyn_payload_flows_into_heterogeneous_constructor_contexts.mr" \
    "$SNAPSHOT_ROOT/cm54_expected_dyn_payload_flows_into_heterogeneous_constructor_contexts.main.go"

test_emit_go_exact_snapshot \
    "derived child-trait Dyn witnesses keep inherited lambda-backed methods" \
    "$REPO_ROOT/test/fixtures/vnext_canary/vn114_derived_dyn_pack_inherits_supertrait_methods.mr" \
    "$SNAPSHOT_ROOT/vn114_derived_dyn_pack_inherits_supertrait_methods.main.go"

test_emit_go_exact_snapshot \
    "derived default methods returning Dyn show package concrete records stably" \
    "$REPO_ROOT/test/fixtures/vnext_canary/vn117_generic_record_derived_default_returns_dyn_show.mr" \
    "$SNAPSHOT_ROOT/vn117_generic_record_derived_default_returns_dyn_show.main.go"

test_emit_go_exact_snapshot \
    "qualified child-trait generic override calls lower through lifted helpers" \
    "$REPO_ROOT/test/fixtures/vnext_canary/vn112_qualified_child_trait_override_generic_call.mr" \
    "$SNAPSHOT_ROOT/vn112_qualified_child_trait_override_generic_call.main.go"

test_emit_go_exact_snapshot \
    "local polymorphic callbacks specialize per higher-order use" \
    "$REPO_ROOT/test/fixtures/runtime/p105_local_poly_helper_callback_through_hof_two_types.mr" \
    "$SNAPSHOT_ROOT/p105_local_poly_helper_callback_through_hof_two_types.main.go"

test_emit_go_exact_snapshot \
    "closures created inside match arms lower stably" \
    "$REPO_ROOT/test/fixtures/codegen_stress/cs49_h50_closure_defined_inside_match_arm.mr" \
    "$SNAPSHOT_ROOT/cs49_h50_closure_defined_inside_match_arm.main.go"

test_emit_go_exact_snapshot \
    "module namespace and direct imports lower to stable internal names" \
    "$REPO_ROOT/test/fixtures/modules_codegen/mc01_namespace_direct_import/main.mr" \
    "$SNAPSHOT_ROOT/mc01_namespace_direct_import.main.go"

test_emit_go_exact_snapshot \
    "module trait and inherent qualification lowers through stable helpers" \
    "$REPO_ROOT/test/fixtures/modules_codegen/mc03_trait_and_inherent_calls/main.mr" \
    "$SNAPSHOT_ROOT/mc03_trait_and_inherent_calls.main.go"

test_emit_go_exact_snapshot \
    "namespace-qualified values and nested type qualification lower stably" \
    "$REPO_ROOT/test/fixtures/modules_codegen/mc04_namespace_values_and_nested_qualifiers/main.mr" \
    "$SNAPSHOT_ROOT/mc04_namespace_values_and_nested_qualifiers.main.go"

test_emit_go_exact_snapshot \
    "module Go-keyword collisions keep emitted identifiers stable" \
    "$REPO_ROOT/test/fixtures/modules_codegen_edge/mce02_go_keyword_collision/main.mr" \
    "$SNAPSHOT_ROOT/mce02_go_keyword_collision.main.go"

test_emit_go_exact_snapshot \
    "module prefix escapes keep colliding paths distinct in Go" \
    "$REPO_ROOT/test/fixtures/modules_codegen_edge/mce05_colliding_module_prefixes/main.mr" \
    "$SNAPSHOT_ROOT/mce05_colliding_module_prefixes.main.go"

test_emit_go_exact_snapshot \
    "sigil-export escapes keep colliding member names distinct in Go" \
    "$REPO_ROOT/test/fixtures/modules_codegen_edge/mce06_sigil_name_escape_collisions/main.mr" \
    "$SNAPSHOT_ROOT/mce06_sigil_name_escape_collisions.main.go"

suite_end
