# Pre-Modules Semantics Foundation Plan

## Maintenance

- Last verified: 2026-03-12
- Implementation status: Planning (not started)
- Prerequisites:
  - `docs/plans/done/language/02_syntax-rework.md`
  - `docs/plans/done/language/03_syntax-rework-followup.md`

## Summary

Before modules, prelude, FFI, and stdlib freeze public APIs, Marmoset needs a stable semantic foundation for traits vs rows, effects and purity, union narrowing, and nominal API boundaries.

This plan is the first post-syntax language milestone. Its job is to settle the semantics that define how users structure programs and APIs, not to do the broad hardening sweep yet.

## In Scope

- freeze the traits-vs-rows policy, including any accepted `shape` / `trait` split
- freeze the minimal effect and purity model, then enforce it consistently
- freeze the current union-narrowing model and remove context-dependent behavior
- add nominal wrappers / newtypes if they are accepted as part of the pre-module API story
- carry forward any accepted `Dyn[...]` / intersection implications from the syntax follow-up work into one coherent semantics story

## Out Of Scope

- module syntax, imports, exports, or multi-file compilation
- prelude, FFI, or stdlib API work
- full parity and docs drift sweeps
- associated types
- user-facing open-row syntax
- broad optimization or IR work
- rank-N polymorphism, HKTs, dependent types, or a general effects algebra

## Work Order

### Phase P0. Freeze The Semantic Matrix

- Turn the accepted post-syntax language behavior into one explicit semantic matrix.
- Record where the language guarantees identical behavior across expression positions and where boundaries are deliberate.
- Update `docs/SYNTAX.md` and feature docs only when the semantics are actually frozen.

### Phase P1. Traits, Rows, And Shapes

- Decide where field-only traits stop and shape-like typing begins.
- Remove accidental overloading between structural shape use and nominal/method-bearing trait use.
- Make the accepted policy visible in diagnostics and examples.

### Phase P2. Effects And Purity

- Freeze the minimal effect model around `->` and `=>`.
- Enforce the same purity rules across top-level functions, local helpers, methods, lambdas, and higher-order use sites.
- Convert remaining "works here but not there" behavior into either consistent semantics or clear rejection.

### Phase P3. Union Narrowing

- Freeze the supported narrowing matrix for `if`, `match`, and other control-flow sites.
- Remove narrowing behavior that depends on backend accidents or AST shape rather than the checker's semantic model.
- Make later intersection coordination an explicit follow-up, not an implicit side effect.

### Phase P4. Nominal API Boundaries

- Decide whether wrappers / newtypes are part of the pre-module language.
- If accepted, add the minimal canonical representation and typechecker support needed to make them usable for module-facing APIs later.
- Keep the scope focused on API boundaries, not on a larger new nominal type hierarchy.

## Exit Criteria

- The language has one stable pre-module semantics story for traits, rows, effects, and unions.
- The accepted API-boundary vocabulary is settled before module exports and stdlib wrappers are introduced.
- `docs/SYNTAX.md`, feature docs, and targeted integration fixtures all reflect the same guarantees.
- The module-system plan can proceed without depending on unresolved trait/shape/effect semantics.

## Related Plans

- `docs/plans/done/language/03_syntax-rework-followup.md` owns the syntax follow-up features that already landed (`Dyn[...]`, intersections, derive follow-up).
- `docs/plans/todo/language/02_pre-modules-parity-and-hardening.md` hardens the semantics from this plan after they are frozen.
- `docs/plans/todo/language/03_module-system.md` starts only after this foundation is in place.
