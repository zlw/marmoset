# Pre-Modules Parity And Hardening Plan

## Maintenance

- Last verified: 2026-04-03
- Implementation status: In progress
- Prerequisites:
  - `docs/plans/done/language/04_pre-modules-semantics-foundation.md`
  - `docs/plans/done/tooling/01_diagnostics-rework.md`

## Summary

Once the semantic foundation is frozen, harden it before modules land. This plan closes the largest "documented feature vs actual behavior" gaps, finishes the remaining checker-to-emitter contract cleanup, and adds generated-Go drift-prevention gates so modules, prelude, and FFI are built on behavior that is already stable end-to-end.

## In Scope

- full end-to-end parity for the current pattern grammar plus unions, records, and trait-heavy programs
- finishing metadata-first checker-to-emitter contracts using the existing typed artifacts, and removing the remaining emitter-side semantic reconstruction heuristics
- row-polymorphism and user `shape`-constraint diagnostics plus internal consistency cleanup after the policy is frozen
- narrowly scoped expected-type propagation for identified pre-modules pain points, not a general bidirectional-typing rewrite
- generated-Go stability gates, snapshots, structural assertions, and regression suites that prevent semantic drift

## Out Of Scope

- redefining the trait/row boundary itself
- module syntax or multi-file compilation
- associated types or user-facing open rows
- new pattern syntaxes not already in the current grammar, such as tuple or array/list patterns
- broad optimization work or a new IR

## Locked Decisions

- "Current pattern surface" in this plan means the pattern forms already accepted by the current grammar: wildcard, variable, literal, constructor, record, nested combinations of those forms, and or-patterns. Deferred future pattern syntaxes stay out of scope.
- Pattern matching must be fully codegen-safe for every currently accepted pattern form before modules land. "Typechecks but backend rejects" is not an acceptable steady state for the current pattern grammar.
- Checker-to-emitter contract hardening extends the existing typed artifacts and pipeline. This plan does not introduce a new IR or a second semantic ownership layer in codegen.
- Generated-Go regression strategy for this plan is locked to:
  - exact snapshots for small curated codegen fixtures,
  - normalized snapshots plus structural assertions for one or more large end-to-end codegen canaries.
- Expected-type propagation work in this plan remains symptom-driven and narrow. Broader bidirectional typing remains post-modules work.

## Work Order

### Phase P0. Freeze The Supported Capability Matrix

- Publish an explicit capability matrix for the current pattern grammar only:
  - wildcard, variable, literal, constructor, record, nested combinations, and or-patterns,
  - scrutinee classes,
  - narrowing behavior,
  - exhaustiveness guarantees,
  - codegen guarantees.
- Identify every currently accepted-but-not-codegen-safe pattern form and make it visible in one place before implementation starts.
- Remove "works in some contexts" ambiguity from docs before the hardening sweep starts.

### Phase P1. Close Feature / Codegen Parity Gaps

- Eliminate backend failures for language forms that are already documented or parsed as part of the current language surface.
- Full current-grammar pattern matching is the target for this phase. In particular, close the known gaps around:
  - or-pattern / multi-pattern arms,
  - nested constructor payload patterns,
  - nested record field patterns,
  - record patterns inside constructor payloads,
  - recursive combinations of the currently accepted pattern forms.
- Replace accidental backend crashes with deliberate diagnostics only as a temporary step on the way to full support for the current pattern grammar.
- Add one focused integration fixture for each previously unsupported or unstable backend branch before or alongside the fix.

### Phase P2. Tighten Typed Artifacts

- Treat the existing checker artifacts as the baseline contract:
  - explicit call resolution,
  - typed method definitions,
  - resolved method type arguments,
  - trait-object coercion sites,
  - placeholder rewrites.
- Finish removing emitter-side semantic reconstruction for dispatch, coercion, narrowing, and call-shape behavior where checker artifacts can be the source of truth directly.
- Reduce or remove emitter fallbacks that currently re-derive semantic meaning from `env` / `type_map` heuristics when a stronger checker-owned artifact is more appropriate.
- Keep the compiler pipeline `parser -> lowering -> typechecker -> emitter`; this is contract hardening, not an IR rewrite.

### Phase P3. Row And Shape-Constraint Consistency Cleanup

- Treat user `shape` constraints and the underlying row-polymorphic machinery as one consistency surface for diagnostics and behavior.
- Improve row-polymorphism and shape mismatch diagnostics, especially around:
  - missing required fields,
  - field type mismatches,
  - conflicting guaranteed fields,
  - accepted-vs-rejected edge cases that currently differ across contexts.
- Remove context-specific inconsistencies that remain after the foundation plan settles the policy.
- Make sure the same accepted shape-constrained programs behave the same way in annotations, inference, narrowing, and emission.

### Phase P4. Drift-Prevention Gates

- Validate selected README and feature-doc examples against actual current behavior via compile/check/run gates or pinned equivalent example files.
- Add exact generated-Go snapshots for small curated fixtures that isolate one lowering/codegen behavior each.
- Add at least one large end-to-end codegen canary that intentionally mixes:
  - full current-grammar pattern matching,
  - unions and narrowing,
  - records and spread/update,
  - shape-constrained generics,
  - trait-qualified and exact-type-qualified calls,
  - `Dyn[...]` coercion and dispatch,
  - nested closures / lambda lifting,
  - specialization-heavy call paths.
- For the large canary, use normalized generated-Go snapshots plus structural assertions rather than brittle byte-for-byte whole-file matching.
- Structural assertions for the canary should cover the important codegen contracts directly, such as:
  - expected lifted helpers / environment structs,
  - expected specializations,
  - expected match / narrowing lowering shape,
  - record-shape interning behavior,
  - trait-object packaging only at recorded coercion sites,
  - absence of unintended fallback / panic / dynamic lowering paths.
- Add regression suites for:
  - trait duplicate / overlap / conflict diagnostics,
  - record / row / shape edge cases,
  - mixed-feature codegen paths.
- Treat these overlap/conflict gates as groundwork for the later module plan's no-orphan, build-wide coherence model.

### Phase P5. Narrow Expected-Type Propagation Cleanup

- Limit this phase to concrete pre-modules pain points where an expected type is already available, but the checker does not use it early enough and the result is surprising inference, unstable behavior, or avoidable codegen/parity failures.
- In scope for this phase:
  - callback and lambda arguments in known-signature positions,
  - annotated return contexts for `if` / `match` arms when the enclosing expected result type is already known,
  - constructor payloads and wrapper rebuild/update positions where the target payload type is already known,
  - qualified callable and placeholder-section sites that already participate in partial expected-typing but still behave inconsistently.
- Each propagation rule in this phase must be justified by a concrete failing or surprising fixture first, then locked with a regression fixture after implementation.
- This phase does not authorize a general bidirectional-typing rewrite, a broad collection-literal inference redesign, or public-API-driven expected-type work.
- Stop once the identified pre-modules cases are predictable end-to-end. Broader expected-type propagation remains part of `docs/plans/todo/language/06_post-modules-type-system-expansion.md`.

## Exit Criteria

- Language docs, fixtures, and the capability matrix agree on the supported semantic surface.
- No currently accepted pattern form reaches backend failure as part of normal codegen.
- No major "documented but not codegen-safe" gaps remain in the current single-file language.
- The emitter consumes typed decisions for the key dispatch, narrowing, and coercion paths it currently relies on.
- Generated-Go drift gates cover both:
  - exact curated snapshots for small codegen behaviors,
  - normalized + structural canary coverage for large mixed-feature programs.
- Trait duplicate / overlap / conflict diagnostics are stable enough to serve as groundwork for build-wide coherence in the module system plan.
- Module, prelude, and FFI work can assume the current language is already stable and auditable.

## Related Plans

- `docs/plans/done/language/04_pre-modules-semantics-foundation.md` freezes the semantics first.
- `docs/plans/todo/language/02_module-system.md` depends on this hardening pass being complete, especially the overlap/conflict groundwork needed for build-wide coherence.
- `docs/plans/done/tooling/01_diagnostics-rework.md` is the diagnostics prerequisite, not an active part of this plan.
