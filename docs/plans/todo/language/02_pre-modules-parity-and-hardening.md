# Pre-Modules Parity And Hardening Plan

## Maintenance

- Last verified: 2026-03-12
- Implementation status: Planning (not started)
- Prerequisites:
  - `docs/plans/todo/language/01_pre-modules-semantics-foundation.md`
  - `docs/plans/done/tooling/01_diagnostics-rework.md`

## Summary

Once the semantic foundation is frozen, harden it before modules land. This plan closes the largest "documented feature vs actual behavior" gaps, tightens checker-to-emitter contracts, and adds drift-prevention checks so modules, prelude, and FFI are built on behavior that is already stable end-to-end.

## In Scope

- supported-feature and codegen parity for patterns, unions, records, and trait-heavy programs
- tighter checker-to-emitter typed artifacts and clearer backend capability boundaries
- row and shape diagnostics plus internal consistency cleanup after the policy is frozen
- targeted expected-type propagation where missing bidirectional typing still causes unstable or surprising behavior
- docs validation, generated-Go snapshots, and regression suites that prevent semantic drift

## Out Of Scope

- redefining the trait/row boundary itself
- module syntax or multi-file compilation
- associated types or user-facing open rows
- broad optimization work or a new IR

## Work Order

### Phase P0. Freeze The Supported Capability Matrix

- Record exactly which pattern forms, narrowing rules, and codegen guarantees are supported.
- Remove "works in some contexts" ambiguity from docs before the hardening sweep starts.

### Phase P1. Close Feature / Codegen Parity Gaps

- Eliminate backend failures for language forms that are already documented as supported.
- Replace accidental backend crashes with deliberate diagnostics where support is still intentionally absent.
- Add focused integration fixtures before each fix.

### Phase P2. Tighten Typed Artifacts

- Move method dispatch, coercion, narrowing, and call-shape decisions out of emitter heuristics.
- Make checker artifacts the source of truth wherever codegen needs semantic meaning.
- Keep the compiler pipeline `parser -> lowering -> typechecker -> emitter`; this is contract hardening, not an IR rewrite.

### Phase P3. Row And Shape Consistency Cleanup

- Improve row-polymorphism and shape mismatch diagnostics.
- Remove context-specific inconsistencies that remain after the foundation plan settles the policy.
- Make sure the same accepted program shapes behave the same way in annotations, inference, and emission.

### Phase P4. Drift-Prevention Gates

- Add docs validation against current behavior.
- Add representative generated-Go snapshots.
- Add regression suites for trait overlap/coherence, record/row edge cases, and mixed-feature codegen paths.

### Phase P5. Targeted Expected-Type Propagation

- Improve bidirectional typing only where it materially affects API clarity or parity.
- Stop once the pre-module surface is predictable; broader inference work belongs in the later post-modules plan.

## Exit Criteria

- Language docs and fixtures agree on the supported semantic matrix.
- No major "documented but not codegen-safe" gaps remain in the current single-file language.
- The emitter consumes typed decisions for the key dispatch, narrowing, and coercion paths it currently relies on.
- Module, prelude, and FFI work can assume the current language is already stable and auditable.

## Related Plans

- `docs/plans/todo/language/01_pre-modules-semantics-foundation.md` freezes the semantics first.
- `docs/plans/todo/language/03_module-system.md` depends on this hardening pass being complete.
- `docs/plans/done/tooling/01_diagnostics-rework.md` is the diagnostics prerequisite, not an active part of this plan.
