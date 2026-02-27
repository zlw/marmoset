# Trait Objects, Existentials, and FFI (Future Design Note)

## Maintenance

- Status: Future design note (non-implemented surface)
- Last updated: 2026-02-27
- Source of truth for current behavior: `/Users/zlw/src/marmoset/marmoset-ml/docs/features/traits.md`

## Purpose

This document locks high-level constraints for any future "dynamic trait value" work (existentials / trait objects with methods) so we do not drift into incompatible runtime/ABI decisions.

Current v1 implementation status:
- Field-only trait types are supported as structural projections.
- Method/mixed trait objects are intentionally unsupported.
- No first-class existential IR node is implemented yet.

## Representation Options Considered

1. Interface-only runtime values (Go interface-based dynamic dispatch)
- Pros: straightforward method dispatch semantics.
- Cons: commits ABI and dispatch semantics early; harder to evolve across modules/FFI.

2. Dictionary passing (explicit witness records)
- Pros: predictable monomorphized core; explicit runtime payload.
- Cons: larger call signatures and more lowering complexity.

3. Tagged existential box (`{type_id, payload, witness}`)
- Pros: explicit runtime shape and better room for optimization later.
- Cons: requires dedicated IR and runtime conventions.

## Locked Direction

For future method/mixed trait objects (when implemented), require an explicit existential representation in compiler IR before exposing syntax:
- runtime payload must include witness information (dictionary or vtable-equivalent),
- operations must be explicitly listed as allowed/forbidden,
- codegen must not infer dispatch strategy ad hoc in emitter-only paths.

Until that lands, method/mixed trait objects remain rejected.

## Operation Constraints for Future Existentials

Before enabling existential values, specify for each trait operation category:
- `show`-style formatting: whether dynamic calls are allowed and how failures are surfaced.
- `eq`/`ord`/`hash`: whether cross-concrete comparisons/hashing are valid or rejected.
- downcast/introspection: whether runtime type tests are exposed, and at what layer.

No implicit "best effort" structural method matching is allowed.

## Module and FFI Guardrails

These constraints are locked now to avoid ABI churn:
- One Go package per build remains the backend policy until module backend work is complete.
- Any future external ABI for existential values must use a stable tagged representation (not raw Go interface internals).
- FFI boundaries must reject unsupported trait-object payloads unless an explicit ABI adapter is defined.
- Cross-module method-object identity must be based on stable symbol identity, not local emitter naming heuristics.

## Preconditions to Implement Method/Mixed Trait Objects

All must be completed first:
1. Typed IR node(s) for existential values and trait method dispatch.
2. Coherence and resolution metadata threaded from typechecker into codegen (no re-resolution).
3. ABI document for module boundaries and FFI adapters.
4. Deterministic test matrix for dynamic dispatch, equality/hash policy, and failure modes.
