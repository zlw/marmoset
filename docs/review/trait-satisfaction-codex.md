# Trait Satisfaction Review (Spec vs Implementation)

## Maintenance

- Last verified: 2026-02-27
- Reviewer: Codex
- Source spec: `/Users/zlw/src/marmoset/marmoset-ml/docs/features/trait-satisfaction.md`
- Scope: parser/AST, typechecker, trait solver/registry, Go emitter, integration tests
- Baseline tests: `103 passed, 0 failed` from `test/test_typecheck_and_codegen.sh`

## Summary

The current implementation is solid for v0/v1 method-trait static dispatch on concrete impls, but it diverges from the trait-satisfaction spec in several critical areas:

- field traits and mixed traits are not representable in syntax/AST,
- supertrait semantics are only partially implemented (declaration exists, satisfaction not enforced),
- generic impls are not supported end-to-end and can crash compilation,
- trait-as-type (trait objects) is not implemented,
- operators are not trait-driven.

Additional accepted hardening items (not blockers for core semantics, but should not be dropped):
- missing explicit regression coverage for supertrait obligations,
- emitter still uses local `failwith` paths for method-resolution failures (caught in build pipeline, but brittle),
- derive contract is split between registry metadata and emitter body generation.

This document lists concrete fix items by spec section.

## Section 1: Trait Kinds

### 1.1/1.3 field-only and mixed traits

Status: **Not implemented**.

Evidence:
- `trait_def` stores only methods, no fields: `/Users/zlw/src/marmoset/marmoset-ml/lib/frontend/syntax/ast.ml:36`.
- parser trait body only accepts `fn ...` members: `/Users/zlw/src/marmoset/marmoset-ml/lib/frontend/syntax/parser.ml:553`.
- parser requires `[type_param]` on every trait: `/Users/zlw/src/marmoset/marmoset-ml/lib/frontend/syntax/parser.ml:503`.

Required fix:
- Extend AST/parser/registry to support trait fields and classify trait kind (`field-only`, `method-only`, `mixed`).
- Remove mandatory `[a]` requirement for traits that do not need a type parameter.

## Section 2: Big Satisfaction Rule

### 2.1 fields structural, 2.3 mixed hybrid

Status: **Not implemented** (blocked by missing field members in traits).

Required fix:
- Implement structural field satisfaction and hybrid checks for mixed traits.

### 2.2 methods nominal

Status: **Implemented**.

Evidence:
- method resolution goes through trait registry/impl lookup, not structural probing: `/Users/zlw/src/marmoset/marmoset-ml/lib/frontend/typecheck/infer.ml:675`.

No fix required for this sub-point.

## Section 3: Satisfaction Rules in Detail

### 3.1 field satisfaction

Status: **Not implemented**.

Required fix:
- Add `satisfies_fields(T, Tr)` with record-shape checking + type unification.

### 3.2 method satisfaction with conditional impls

Status: **Partially implemented** for concrete impls only.

Evidence:
- impl lookup is exact-key `(trait, mono_type)` lookup: `/Users/zlw/src/marmoset/marmoset-ml/lib/frontend/typecheck/trait_registry.ml:77`.
- no solver for conditional/generic impl matching.
- generic impl path currently broken:
  - `impl show[b] for list[b]` fails with `Unknown type constructor: b` due impl type conversion not bound to impl type params.
  - failing conversion path starts at `/Users/zlw/src/marmoset/marmoset-ml/lib/frontend/typecheck/infer.ml:1616`.

Required fix:
- Implement proper generic impl instantiation/selection logic (unification + constraint filtering).
- Fix impl type conversion to bind `impl_type_params` when converting `impl_for_type`.
- Replace uncaught `Failure` exception path with typed error reporting.

### 3.3 full trait satisfaction by kind

Status: **Not implemented** (depends on trait kind model).

Required fix:
- Add unified `satisfies_trait` entrypoint combining fields/methods/supertraits according to trait kind.

### 3.4 supertrait satisfaction

Status: **Not enforced**.

Evidence:
- supertraits are validated only for existence at trait definition time: `/Users/zlw/src/marmoset/marmoset-ml/lib/frontend/typecheck/trait_registry.ml:211`.
- solver checks direct impl only: `/Users/zlw/src/marmoset/marmoset-ml/lib/frontend/typecheck/trait_solver.ml:7`.
- program with `trait ord[a]: eq` and only `impl ord for int` currently builds (should be rejected by spec).

Required fix:
- Enforce supertrait obligations when validating impls and when checking constraints.
- Expand method availability via supertrait closure (both concrete and constrained generic receivers).

## Section 4: Coherence / Ambiguity

### 4.1 uniqueness after solver filtering

Status: **Partially implemented** for exact concrete keys only.

Evidence:
- duplicate concrete impl keys rejected: `/Users/zlw/src/marmoset/marmoset-ml/lib/frontend/typecheck/trait_registry.ml:303`.
- overlapping generic/conditional impl ambiguity is not modeled.

Required fix:
- Implement overlap checking based on applicability, not only exact canonical key equality.

### 4.2 ambiguity errors include both impl sites

Status: **Not implemented**.

Evidence:
- ambiguity errors include trait names only; no source locations: `/Users/zlw/src/marmoset/marmoset-ml/lib/frontend/typecheck/trait_registry.ml:114`.

Required fix:
- store source spans for impls and include both candidate sites in ambiguity diagnostics.

### 4.3 field-only traits do not use impl registry

Status: **Not implemented** (field-only traits not supported).

Required fix:
- once field traits exist, ensure they do not register method impl entries.

## Section 5: Constraint Checking

### 5.1 call-site checking

Status: **Implemented**.

Evidence:
- constraints verified at call/instantiation via obligations: `/Users/zlw/src/marmoset/marmoset-ml/lib/frontend/typecheck/infer.ml:1217`.

### 5.2/5.3 semantic reason detail

Status: **Partial**.

Evidence:
- current message: `Type <T> does not implement trait <Tr>` from `/Users/zlw/src/marmoset/marmoset-ml/lib/frontend/typecheck/trait_solver.ml:33`.
- no detailed reason breakdown (missing fields vs missing impl vs supertrait vs ambiguity).

Required fix:
- upgrade constraint failures to structured diagnostics with explicit cause categories.

## Section 6: Method Call Resolution

### 6.1 candidate sources

Status: **Partial**.

Evidence:
- enum constructors and trait methods supported.
- inherent methods are not implemented yet.

Required fix:
- when inherent methods land, add explicit precedence/collision policy in checker.

### 6.2 no structural method probing

Status: **Implemented**.

Evidence:
- record `p.show()` without trait impl fails with `No method 'show' found`.

No fix required for this sub-point.

### 6.3 collisions

Status: **Partial**.

Evidence:
- trait-trait same-name collisions on same receiver type are hard errors via ambiguous resolve.
- inherent-vs-trait collision path not present yet (no inherent methods).

Required fix:
- implement explicit inherent-vs-trait collision policy when inherent methods are added.

### 6.4 dispatch mode

Status: **Static dispatch only**.

Evidence:
- emitter lowers trait method calls to mangled free functions: `/Users/zlw/src/marmoset/marmoset-ml/lib/backend/go/emitter.ml:862`.

Required fix:
- none for static mode itself; trait-object mode remains unimplemented (Section 7).

## Section 7: Trait Objects (Trait as Type)

Status: **Not implemented**.

Evidence:
- trait names in type positions are rejected as unknown type constructors:
  - `/Users/zlw/src/marmoset/marmoset-ml/lib/frontend/typecheck/annotation.ml:61`.
- no field-only trait interface/getter codegen exists.

Required fix:
- implement trait-type representation policy from spec (field-only allowed in v1, method/mixed disallowed).
- add coercion checks and Go interface/getter generation for field-only trait objects.

## Section 8: Operators via Traits

Status: **Not implemented** (current operators are built-in unification rules).

Evidence:
- infix operators currently unify operand types directly, independent of trait impls: `/Users/zlw/src/marmoset/marmoset-ml/lib/frontend/typecheck/infer.ml:825`.
- `==` works on record aliases without `eq` trait requirement.

Required fix:
- route operator typing through trait requirements (`eq`, `ord`, `num`, etc.) per spec.

## Section 9: Records + Row Polymorphism Integration

Status: **Partial**.

Evidence:
- row-polymorphic record typing exists.
- named field-trait constraints (`t: named`) are not desugared to row constraints.

Required fix:
- implement field-trait-to-row-constraint integration once field traits exist in trait defs.

## Section 10: Implementation-Plan Conformance

Status: **Divergent in core APIs**.

Missing pieces from plan:
- trait registry lacks required field storage and trait kind classification.
- no `satisfies_fields` / `satisfies_trait` APIs.
- no trait-object coercion/type path.

Required fix:
- introduce dedicated satisfaction engine module (`Trait_satisfaction`) and use it from:
  - constraint checker,
  - method resolution,
  - trait-object coercion/type checking.

## Section 11: Compliance Checklist

### Current pass

- method trait calls are nominal (no TS-style structural method satisfaction),
- `x.name` field access and `x.show()` method resolution are separated,
- ambiguous trait-method dispatch is rejected.

### Current fail (fix required)

- parser/AST cannot represent field or mixed traits,
- supertraits not semantically enforced,
- trait objects absent,
- operator overloading not trait-based.

## Section 12: Open Decisions Locking

Current code does not explicitly encode the spec's v1 locks; behavior is mostly "feature absent" rather than "policy enforced".

Required fix:
- add explicit diagnostics and tests for locked v1 policies:
  - non-record rejection for field-only trait satisfaction,
  - disallow method/mixed trait objects with dedicated error,
  - mixed traits require method impl even with field match,
  - inherent methods never imply trait satisfaction.

## Additional Accepted Findings

### Test gap: supertrait semantics

Status: **Needs work**.

There is still no focused regression set proving supertrait obligations end-to-end.

Required fix:
- add explicit tests for:
  - rejecting `impl ord for T` when `ord: eq` but no `eq` impl for `T`,
  - allowing supertrait methods through subtrait constraints (`fn[a: ord](x: a) { x.eq(x) }`),
  - rejecting calls where direct trait appears implemented but supertrait chain is unsatisfied.

### Emitter hardening: method resolution failure path

Status: **Needs hardening**.

`emit_expr` still has a direct `failwith` on trait method resolution failure (`/Users/zlw/src/marmoset/marmoset-ml/lib/backend/go/emitter.ml:857`).
In normal build flow this is wrapped and surfaced as a codegen error, but the local `failwith` path is still brittle.

Required fix:
- replace local `failwith` with structured error propagation inside emitter pipeline.

### Derive implementation split

Status: **Architectural smell**.

Trait registry records derived impl signatures, while emitter synthesizes actual record trait bodies separately. Behavior is correct today, but the split can drift.

Required fix:
- define a single source of truth for derive contracts (either shared derive IR or shared derive generator used by both registry validation and codegen).

### Constrained-generic codegen guardrail

Status: **Guardrail needed**.

`mangle_type` maps unresolved type vars to `"any"` (`/Users/zlw/src/marmoset/marmoset-ml/lib/backend/go/emitter.ml:20`).
I did not reproduce a wrong-output case in current pipeline, but this should be pinned with tests.

Required fix:
- add regression tests ensuring constrained generic trait method calls always emit concrete monomorphized symbols (no `_any` fallback in emitted method dispatch paths).

## Prioritized Fix Queue

### P0

1. Add explicit failing/passing supertrait regression tests (implementation validation gate).
2. Fix generic impl handling (currently can crash with uncaught exception on `impl ... for list[b]`).
3. Enforce supertrait semantics in impl validation + constraint solving + method availability.
4. Replace ad-hoc solver checks with a single `satisfies_trait` API used consistently by inference and resolution.

### P1

1. Add trait fields + trait kind classification (field/method/mixed) to AST/parser/registry.
2. Implement structural field satisfaction and mixed-trait hybrid checks.
3. Add structured trait satisfaction diagnostics (with reason categories and impl spans).

### P2

1. Implement field-only trait objects and coercions (or explicitly reject all trait-as-type until this lands).
2. Implement trait-based operator requirements instead of pure type unification for `==`, `<`, `+`, etc.
3. Stop emitter-side trait re-resolution by carrying resolved method identity from typechecker output.
4. Replace emitter-local `failwith` method-resolution failures with structured backend errors.
5. Add constrained-generic codegen guard tests to prevent `_any`-mangled trait dispatch regressions.
6. Consolidate derive contract between registry and emitter (single source of truth).
