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

suite_end
