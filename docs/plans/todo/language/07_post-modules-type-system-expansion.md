# Post-Modules Type-System Expansion Plan

## Maintenance

- Last verified: 2026-03-12
- Implementation status: Planning (not started)
- Prerequisites:
  - `docs/plans/todo/language/03_module-system.md`
  - `docs/plans/todo/language/04_prelude.md`
  - `docs/plans/todo/language/05_ffi.md`
  - `docs/plans/todo/language/06_stdlib.md`

## Summary

After modules and public APIs exist, revisit the type-system expansions that are hard to judge in a single-file language. This plan is intentionally later: it should be driven by real module, stdlib, and wrapper pressure instead of speculation.

## In Scope

- associated types, if they prove valuable once module signatures and trait metadata are real
- user-facing open-row annotations, if the pre-module work leaves a real API gap
- broader expected-type propagation across public API boundaries
- coherence and orphan-style diagnostics once module boundaries exist
- second-pass union / intersection coordination after real multi-module use
- backend-aware lowering or opportunistic mutation only when representative workloads justify it

## Out Of Scope

- rank-N polymorphism, HKTs, or dependent types
- a fully general effects algebra
- a global optimizing IR
- speculative backend architecture changes without workload evidence

## Work Order

### Phase P0. Capture Real Post-Modules Pressure

- Collect concrete examples from module signatures, stdlib wrappers, and FFI-facing APIs.
- Reject features that still look hypothetical after modules land.

### Phase P1. Associated Types And Coherence Boundaries

- Decide whether associated types solve enough real module-facing problems to justify the complexity.
- If accepted, define how they interact with impl lookup, derived traits, and module signatures.
- Tighten coherence/orphan diagnostics around the same time, when module boundaries make them meaningful.

### Phase P2. Open Rows And Public API Typing

- Decide whether user-facing open-row annotations are still needed once the pre-module trait/shape story is settled.
- If accepted, define a bounded surface syntax and validation model instead of leaking internal row machinery directly.
- Expand expected-type propagation only where it clearly improves public APIs and diagnostics.

### Phase P3. Union / Intersection Second Pass

- Revisit union compatibility and narrowing once intersections and `Dyn[...]` have real multi-module use.
- Tighten function-position behavior and mixed-feature interactions only after actual examples exist.

### Phase P4. Backend-Aware Lowering

- Consider opportunistic mutation, backend-aware lowering, or related performance-driven expansions only when representative workloads justify them.
- Keep source-level semantics stable; this phase is about better lowering choices, not exposing new user-visible mutation semantics by accident.

## Exit Criteria

- Post-module type-system additions are justified by real API pressure instead of speculative design.
- Any accepted feature has a clear interaction story with module signatures, traits, derives, and FFI wrappers.
- The roadmap retains only genuinely deferred explorations after these concrete post-modules items are either planned or rejected.

## Related Plans

- `docs/plans/todo/language/03_module-system.md`, `04_prelude.md`, `05_ffi.md`, and `06_stdlib.md` create the public-API pressure that justifies this later work.
- `docs/ROADMAP.md` should keep more speculative ideas deferred until they are concrete enough to deserve their own plan.
