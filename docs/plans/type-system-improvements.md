# Type-System Improvements Plan

## Maintenance

- Last verified: 2026-03-10
- Implementation status: Planning (not started)
- Update trigger: Any typechecker, trait, record/row, union, effect, pattern-matching, or backend-capability change affecting the items below

## Summary

This document collects the next round of type-system and semantics work that sits between:
- "the language already claims this feature, but it behaves inconsistently across contexts",
- "the compiler technically supports an internal model, but users cannot express it cleanly",
- and "the backend/tooling contract is still loose enough that language behavior can silently drift."

This is not a speculative feature bucket. The primary goal is to finish and harden the language Marmoset already claims to have, then add a small number of high-leverage type-system extensions that become more valuable once modules and public APIs exist.

This plan intentionally complements, rather than replaces:
- `docs/plans/syntax-rework.md`
- `docs/plans/syntax-rework-followup.md`
- `docs/plans/module-system.md`
- `docs/plans/ffi.md`
- `docs/plans/stdlib.md`
- `docs/plans/diagnostics-rework.md`

## Goals

- Close the largest "documented feature vs actual behavior" gaps, especially in pattern matching and backend support.
- Make traits, records, unions, and effects behave more predictably across expression positions, annotations, method resolution, and codegen.
- Tighten the checker-to-emitter contract so codegen consumes typed decisions rather than rediscovering semantics from AST shape.
- Decide where field-only traits stop and explicit row syntax should start, instead of leaving that boundary implicit.
- Add a small number of pragmatic type-system features that improve module-facing APIs: nominal wrappers/newtypes, stronger expected-type propagation, and eventually associated types.
- Keep any future trait-object and general-intersection work aligned with `docs/plans/syntax-rework-followup.md`.

## Non-Goals

- Reopening the core vNext syntax migration or legacy-syntax support model from `docs/plans/syntax-rework.md`.
- Designing a full optimizing mid-level IR. This plan assumes the compiler remains `parser -> lowering -> typechecker -> emitter`, with stronger artifacts between stages.
- Replacing field-only traits with a different constraint system immediately.
- Shipping rank-N polymorphism, HKTs, dependent types, or a fully general effects algebra in this plan.
- Treating stdlib API design as the primary driver. Type-system changes must be justified by language consistency first, then by library value.

## Relationship To Other Plans

- `docs/plans/syntax-rework.md` owns the frontend split into `Surface_ast -> lowering -> Core_ast`.
- `docs/plans/syntax-rework-followup.md` owns:
  - user-trait derive semantics,
  - explicit trait-object syntax (`Dyn[...]`),
  - general intersection types if accepted.
- `docs/plans/module-system.md` owns:
  - imports/exports,
  - per-module checking,
  - namespace-qualified resolution,
  - multi-file compilation boundaries.
- `docs/plans/ffi.md` owns:
  - `extern`,
  - Go interop,
  - wrapper-module patterns.

This plan owns the semantic hardening and typechecker/runtime consistency work that those plans depend on or amplify.

Current milestone reality:
- `docs/plans/syntax-rework.md` is still in progress.
- `docs/plans/syntax-rework-followup.md` is the next planned language-design step after the core syntax migration lands.
- This plan must therefore be read as a post-syntax-rework, pre-modules roadmap unless a specific track explicitly says otherwise.

## Why This Work Exists

Several parts of the language are already close to useful, but still uneven:
- pattern matching is documented broadly, but Go codegen still rejects or degrades several supported forms;
- row-polymorphic behavior exists internally, but user-facing record typing still stops short of the internal model;
- traits are intentionally a mixed structural/nominal system, but the boundaries between field-only, mixed, and method-bearing use cases are still fragile;
- union narrowing works in many cases, but the language does not yet feel "smart everywhere";
- purity/effect checking exists, but the effect model is not yet fully frozen;
- diagnostics have largely moved to `Diagnostic.t`, but there are still expected-path exception/failwith remnants;
- the backend still relies on AST-shape rediscovery in places where it should consume typed artifacts.

Modules, stdlib, and FFI will magnify those inconsistencies. This plan is the work needed so later public APIs are built on stable semantics rather than provisional behavior.

## Work Ordering Recommendation

This plan should not be treated as one monolithic milestone. The correct order is:

### Stage 0. Finish The Active Syntax Migration

Complete the current mainline syntax work first:
- finish `docs/plans/syntax-rework.md`,
- then run the accepted pieces of `docs/plans/syntax-rework-followup.md`.

Reason:
- until syntax and canonical lowering stabilize, this plan would be targeting a moving frontend boundary;
- several items in this plan depend directly on the post-rework `Surface_ast -> lowering -> Core_ast` architecture;
- existential/trait-object and intersection work already has a dedicated owner in the syntax follow-up plan and should not be re-homed here.

### Stage 1. Pre-Modules Architecture And API Foundation

After syntax rework/follow-up, but before modules, settle the language decisions that determine whether Marmoset actually enforces the kind of architecture and API discipline you want:
- Track D0: freeze the traits-vs-rows policy,
- any accepted shape-split work that replaces field-only/mixed-trait overload with a clearer `shape`/`trait` boundary,
- Track E0-E1: freeze and enforce the minimal effect model,
- Track H0-H1: freeze and improve the current union-narrowing model,
- Track C0, and if chosen C1: nominal wrappers/newtypes for API boundaries,
- any accepted existential/trait-object direction from `docs/plans/syntax-rework-followup.md`,
- any other accepted pre-module semantic change required so stdlib/module APIs are built on the right abstraction vocabulary from day one.

Reason:
- module exports, stdlib APIs, and FFI wrappers will freeze public vocabulary;
- if `shape` vs `trait`, trait-object/existential policy, effect semantics, or wrapper boundaries are still unsettled, modules will bake the wrong abstractions into the surface area;
- this is the stage where Marmoset should become "good for architecture and good APIs," not after the stdlib already exists.

### Stage 2. Pre-Modules Parity And Hardening

Once the pre-module semantic model is settled, harden it before modules land:
- Track A: existing-feature parity and supported-feature/codegen parity,
- Track B: diagnostics, checker-to-emitter contracts, docs validation, and drift checks,
- Track D1: row/shape diagnostics and internal consistency,
- any remaining tests/docs needed to ensure the settled design is actually green end-to-end.

Reason:
- parity and hardening should validate the design you actually chose, not the one you are about to replace;
- pattern/codegen parity, backend capability cleanup, and diagnostics hardening become more valuable once the core abstraction model is frozen.

Important nuance:
- some Track B work can and should start early, especially diagnostics cleanup and backend-contract tightening, because it reduces implementation risk in Stage 1;
- but the full parity sweep should happen after the semantic model is settled, so you do not spend time hardening semantics you are about to replace.

### Stage 3. Modules, Prelude, Stdlib, And FFI

After Stages 1 and 2:
- module system,
- prelude/basic stdlib prerequisites already planned,
- minimal FFI milestone,
- stdlib growth on top of those milestones.

Reason:
- by this point, public APIs can be designed against stable semantics rather than migration leftovers or provisional trait/shape rules.

### Stage 4. Later Type-System And Lowering Expansion

After modules/stdlib/FFI are real and there is actual library pressure:
- Track F: better bidirectional typing / expected-type propagation if not already pulled earlier,
- Track D2-D3 if user-facing open rows are accepted,
- Track G0-G1: associated-types decision and implementation,
- Track H2 if union/intersection interaction needs a second pass after syntax-follow-up work lands,
- Track I: opportunistic mutation / backend-aware lowering if real workloads justify it,
- any deeper trait-system work after real usage validates the need.

Reason:
- these are valuable, but they are not the first blockers for architecture/API quality if Stages 1 and 2 are done well;
- optimization-oriented lowering should wait until semantics, typed contracts, and representative workloads are stable enough that performance work is targeting the right shapes.

### Concrete Recommendation

- Do not wait until after modules/stdlib/FFI to fix the semantic foundation. That is too late.
- Do not try to mix those foundational changes into the active syntax migration either. Finish the syntax migration first.
- Recommended order:
  1. finish `syntax-rework`
  2. run accepted `syntax-rework-followup` work
  3. Stage 1 from this plan: pre-modules architecture/API foundation
  4. Stage 2 from this plan: parity and hardening against the settled design
  5. module system
  6. prelude/basic stdlib prerequisites
  7. minimal FFI milestone
  8. stdlib growth
  9. Stage 4 from this plan: later type-system and lowering expansion

## Shared Binding Decisions

### 1. Syntax Rework Must Finish Before This Plan Becomes Active

No track in this document should widen the in-flight syntax migration unless the syntax-rework owner explicitly decides the design correction must happen there. By default:
- finish `docs/plans/syntax-rework.md`,
- then finish the accepted parts of `docs/plans/syntax-rework-followup.md`,
- then activate this plan.

### 2. Architecture/API Foundation Comes Before Full Parity Sweep

The language decisions that define Marmoset's architecture story must be settled before the big hardening pass:
- `shape` vs overloaded field-only/mixed traits,
- trait-object/existential policy if accepted,
- minimal effect model,
- union semantic matrix,
- nominal wrapper/newtype boundary if chosen.

Some diagnostics/contract work may start earlier, but the full parity sweep should validate the settled design, not the transitional one.

### 3. Surface Features Must Map To One Canonical Semantic Story

If the language permits a construct in multiple contexts, the plan must specify:
- whether those contexts are semantically identical,
- whether they lower to the same canonical representation,
- and where any context-specific restrictions are enforced.

Do not add "works here but not there" behavior without explicitly naming the boundary.

### 4. Stronger Checker-To-Emitter Contracts, Not A Big New IR

This plan does not require a new optimizing IR. It does require the checker to keep moving decisions out of emitter heuristics and into typed artifacts. When codegen needs to know what a construct means, that meaning should already have been recorded by the checker whenever practical.

### 5. Traits Remain A Split System For Now

The current language intentionally supports:
- field-only traits as named structural constraints,
- method-only traits as nominal behavior contracts,
- mixed traits as combined contracts.

This plan does not remove that design immediately. It does require every track below to say whether it applies to:
- field-only traits,
- method-only traits,
- mixed traits,
- or only a subset.

### 6. Rows Stay Internal Until Explicitly Accepted

Internal row-polymorphic machinery may continue to exist even if user-facing row syntax remains deferred. Any user-facing row surface change in this plan must be explicitly accepted before parser or docs work begins.

### 7. Public Docs Must Match Actual Guarantees

If docs describe a feature as supported, the compiler must support it end-to-end or the docs must narrow the claim. This applies especially to:
- pattern forms,
- record/trait type-position behavior,
- union narrowing,
- effect annotations,
- and codegen support statements.

## Shared Code Touchpoints

- `docs/SYNTAX.md`
- `docs/features/pattern-matching.md`
- `docs/features/records.md`
- `docs/features/traits.md`
- `docs/features/unions.md`
- `docs/ROADMAP.md`
- `docs/plans/diagnostics-rework.md`
- `lib/frontend/syntax/ast.ml`
- `lib/frontend/syntax/surface_ast.ml`
- `lib/frontend/syntax/parser.ml`
- `lib/frontend/syntax/lower.ml`
- `lib/frontend/typecheck/types.ml`
- `lib/frontend/typecheck/annotation.ml`
- `lib/frontend/typecheck/unify.ml`
- `lib/frontend/typecheck/infer.ml`
- `lib/frontend/typecheck/checker.ml`
- `lib/frontend/typecheck/trait_registry.ml`
- `lib/frontend/typecheck/trait_solver.ml`
- `lib/frontend/typecheck/exhaustiveness.ml`
- `lib/frontend/typecheck/resolution_artifacts.ml`
- `lib/backend/go/emitter.ml`
- `test/fixtures/**`
- `tools/tree-sitter-marmoset/**`
- `tools/vscode-marmoset/syntaxes/marmoset.tmLanguage.json`
- `tools/jetbrains-marmoset/**`

## Track A. Existing-Feature Parity And Backend Support Completion

### Current State

- `docs/features/pattern-matching.md` describes variable, literal, constructor, and record patterns as part of the language model.
- The backend still contains user-triggerable unsupported branches for documented pattern forms:
  - enum match literal patterns unsupported,
  - enum match record patterns unsupported,
  - complex record patterns unsupported,
  - multiple patterns per arm unsupported,
  - some variable-pattern behavior in enum matches still degrades toward wildcard behavior rather than full binding semantics.
- This creates the worst current "spec says yes, backend says maybe" mismatch in the language.

### Binding Decisions

- Track A is not optional. It is existing-feature completion, not a speculative extension.
- The source of truth must be one of:
  - "feature is supported end-to-end" with tests,
  - or "feature is explicitly deferred" with docs narrowed.
- During Track A, prefer implementation over doc retreat when the missing behavior is already consistent with the language model and typechecker.
- Do not expand pattern syntax further in this track. Finish the currently documented forms first.
- If Track A needs a localized match-planning representation to finish pattern/codegen parity cleanly, that is in scope:
  - a backend-neutral decision tree or equivalent per-match plan is allowed,
  - but it must stay focused on match semantics and codegen structure,
  - and it must not turn into a general optimizing IR for unrelated constructs.

### Phase A0. Freeze The Supported Pattern Matrix

Purpose:
- Write one explicit matrix of which pattern forms are expected to work in which scrutinee categories before code changes begin.

Tasks:
- Update `docs/features/pattern-matching.md` to include a support matrix:
  - primitive scrutinee vs enum scrutinee vs record scrutinee,
  - wildcard/variable/literal/constructor/record pattern availability,
  - whether multi-pattern arms are first-class semantics or only sugar.
- Inventory current backend unsupported branches in `lib/backend/go/emitter.ml`.
- For each unsupported branch, classify it:
  - implement in Track A,
  - or narrow docs if it is truly out of scope for v1.
- Add a short "claimed vs actual" checklist to this plan so later reviews can verify closure.

Likely files:
- `docs/features/pattern-matching.md`
- this plan
- `lib/backend/go/emitter.ml`

Exit criteria:
- There is one written support matrix for pattern forms.
- No pattern form remains in an implicit "probably supported" state.

### Phase A1. Implement Missing Match Codegen Paths

Purpose:
- Make documented pattern forms compile and run.

Tasks:
- Extend `lib/backend/go/emitter.ml` match lowering to support all pattern forms declared supported in Phase A0:
  - enum literal-pattern behavior where semantically valid,
  - enum record/constructor sub-patterns as accepted by frontend semantics,
  - variable binding semantics in enum matches,
  - record-pattern lowering parity with frontend binding behavior,
  - multi-pattern arm handling if Phase A0 keeps it in scope.
- Prefer one centralized match-planning step before backend-specific emission:
  - lower canonical match arms into a decision tree or equivalent discriminator-first plan,
  - centralize test ordering, binding extraction, and fallback flow there,
  - then emit Go from that plan rather than scattering pattern behavior across ad hoc branches.
- Keep exhaustiveness and typechecking semantics aligned:
  - do not implement a codegen path for a pattern form the checker rejects,
  - do not accept a checker-valid pattern form that emitter still cannot lower.
- Add or adjust helper functions so codegen pattern lowering is centralized instead of split across many failwith-heavy branches.
- Keep the first version pragmatic:
  - optimize for semantic parity, maintainability, and backend reuse,
  - not for assembly-specific or micro-architecture-specific fast paths.
- Where a pattern form is intentionally unsupported even after Phase A0, replace ad hoc backend failures with a deliberate diagnostic path if the construct is still parseable/typecheckable.

Likely files:
- `lib/backend/go/emitter.ml`
- `lib/frontend/typecheck/infer.ml`
- `lib/frontend/typecheck/exhaustiveness.ml`
- `lib/frontend/syntax/ast.ml` only if a small canonical pattern helper representation is needed

Required tests:
- primitive matches using wildcard, variable, and literal patterns,
- enum matches with constructor and binding patterns,
- enum matches with record-like constructor payload destructuring if supported by Phase A0,
- record matches with field binding and rest behavior where documented,
- multi-pattern arm tests if kept,
- regression tests for variable-binding vs wildcard behavior,
- end-to-end integration tests that compile to Go and run.

Exit criteria:
- Every pattern form left in the docs is green end-to-end.
- `emitter.ml` no longer contains user-reachable "pattern kind not supported" branches for supported forms.

### Phase A2. Supported-Feature / Codegen Parity Sweep

Purpose:
- Eliminate the broader class of "supported in docs, still unsupported in backend" issues outside pattern matching.

Tasks:
- Audit `lib/backend/go/emitter.ml` for user-triggerable unsupported/failwith paths that correspond to documented language features.
- Categorize each case:
  - invariant only, safe to keep as internal bug check,
  - expected-path user error, must become structured diagnostic,
  - missing implementation, must be completed before the feature stays documented as supported.
- Add a backend capability checklist to this plan or `docs/ARCHITECTURE.md`:
  - for each major frontend construct, specify whether codegen guarantees support today.
- Keep this audit focused on existing documented features, not future ideas.

Likely files:
- `lib/backend/go/emitter.ml`
- `docs/ARCHITECTURE.md`
- this plan

Required tests:
- targeted regression tests for every resolved unsupported branch,
- one audit list in docs tied to tests or fixtures.

Exit criteria:
- No documented feature depends on a user-triggerable backend `failwith` to communicate ordinary lack of support.

## Track B. Diagnostics, Contracts, And Semantic Drift Prevention

### Current State

- Major frontend layers now use `Diagnostic.t`, which is good progress.
- Expected-path `failwith` and exception-based behavior still remain in some typechecking code and backend code.
- The checker already produces several useful artifacts:
  - `call_resolution_map`,
  - `method_def_map`,
  - `method_type_args_map`.
- The emitter still rediscovers or reconstructs semantics in places where typed artifacts should be authoritative.
- Docs validation and canonical-source drift checks are still mostly roadmap items rather than enforced CI.

### Binding Decisions

- Track B is mandatory Stage 2 work, with B0-B1 allowed to begin in parallel during Stage 1.
- This track strengthens stage contracts without introducing a new mid-level optimizer IR.
- When a decision can be recorded once in the checker and consumed in emitter, prefer that over emitter-side reclassification.
- Expected-path user failures must use structured diagnostics rather than raw exceptions.

### Phase B0. Finish Diagnostics Closeout For Expected Paths

Purpose:
- Complete the last leg of the diagnostics rework on the user-visible type/backend paths this plan depends on.

Tasks:
- Use `docs/plans/diagnostics-rework.md` as the authoritative cleanup checklist.
- Remove remaining expected-path `failwith` / exception usage from:
  - `lib/frontend/typecheck/annotation.ml`,
  - `lib/frontend/typecheck/infer.ml`,
  - `lib/frontend/typecheck/trait_registry.ml`,
  - `lib/backend/go/emitter.ml`,
  where the error can be triggered by user input rather than an internal invariant violation.
- Tighten the project rule:
  - expected parse/typecheck/validation/codegen failures must become `Diagnostic.t`,
  - `failwith` is reserved for internal impossible states only.
- Update tests so they assert on structured diagnostics, not brittle string transport through exceptions.

Likely files:
- `docs/plans/diagnostics-rework.md`
- `lib/frontend/typecheck/annotation.ml`
- `lib/frontend/typecheck/infer.ml`
- `lib/frontend/typecheck/trait_registry.ml`
- `lib/backend/go/emitter.ml`

Required tests:
- negative annotation/typechecking fixtures,
- negative trait/impl fixtures,
- negative codegen-path fixtures where emitter now reports proper diagnostics,
- regression coverage for formerly exception-based paths.

Exit criteria:
- User-triggerable expected paths in the touched areas no longer use `failwith`/raw exceptions.

### Phase B1. Tighten Checker-To-Emitter Typed Artifacts

Purpose:
- Make codegen consume the checker's semantic decisions rather than rediscovering them from syntax or AST shape.

Tasks:
- Inventory the remaining semantics emitter still infers ad hoc:
  - method dispatch details,
  - narrowing-dependent codegen choices,
  - trait projection packaging,
  - any construct-specific behavior currently reconstructed from AST pattern alone.
- For each case, decide whether the right owner is:
  - `resolution_artifacts.ml`,
  - `checker.ml` result payload,
  - or a more specific typed store.
- For pattern matching specifically, decide whether codegen should consume a dedicated typed match-planning artifact:
  - for example, a per-match decision tree / match plan recording discriminator order, binding extraction, and fallback flow,
  - instead of rebuilding match dispatch semantics directly from raw `AST.pattern` shape inside the emitter.
- Extend typed artifacts as needed, following the existing style of:
  - `call_resolution_map`,
  - `method_def_map`,
  - `method_type_args_map`.
- If a match-planning artifact is introduced, keep it narrow:
  - it is a local checker-to-emitter contract for `match`,
  - not a general-purpose optimizer IR for the whole compiler.
- Update emitter entrypoints so they accept and rely on those artifacts rather than attempting their own classification.
- Document the resulting contract in `docs/ARCHITECTURE.md`:
  - which stage decides what,
  - which typed stores codegen may rely on,
  - which backend assumptions are forbidden.

Likely files:
- `lib/frontend/typecheck/resolution_artifacts.ml`
- `lib/frontend/typecheck/checker.ml`
- `lib/frontend/typecheck/infer.ml`
- `lib/backend/go/emitter.ml`
- `docs/ARCHITECTURE.md`

Required tests:
- artifact snapshot/unit tests for any new stores,
- codegen regressions proving emitter behavior follows checker artifacts,
- negative tests showing emitter no longer silently "guesses" unsupported semantics.

Exit criteria:
- The emitter has an explicit typed contract for the semantics touched by the pre-module foundation and parity stages and no longer needs to re-resolve those cases from raw AST form.

### Phase B2. Canonical Docs Validation And Drift Checks

Purpose:
- Prevent the language spec, feature docs, and real compiler behavior from drifting again.

Tasks:
- Define one canonical-doc validation policy:
  - every canonical doc has maintenance metadata,
  - links resolve,
  - canonical examples are still valid,
  - `docs/SYNTAX.md` and feature docs do not contradict each other on the same surface rule.
- Add CI checks for:
  - markdown link validity,
  - doc metadata presence where required,
  - syntax example extraction/parsing for canonical docs,
  - typechecking or expected-failure validation for marked code blocks where practical.
- Add focused regression suites for:
  - trait solver overlap/coherence cases,
  - record/row unification edge cases,
  - generated-Go snapshots for representative mixed-feature programs.
- Keep the source-of-truth rules explicit:
  - `docs/SYNTAX.md` owns normative surface syntax,
  - feature docs explain semantics and examples,
  - plans explain migration/implementation order.

Likely files:
- `docs/ROADMAP.md`
- `docs/SYNTAX.md`
- `docs/features/*.md`
- `test/ci/**`
- any new docs-validation scripts or harness helpers

Required tests:
- docs validation job in CI,
- representative generated-Go snapshots,
- targeted trait/row regression suite.

Exit criteria:
- Canonical docs drift becomes a failing CI condition, not a manual review surprise.

## Track C. Nominal Wrappers / Newtypes

### Current State

- Structural aliases are currently too transparent at API boundaries.
- A public module can expose a concept like `user_id` or `email_address`, but if it is only an alias, callers can often treat it as the underlying primitive or identical record shape without any nominal distinction.
- This becomes materially more important once the module system exists and library APIs start depending on stable semantic boundaries.

### Binding Decisions

- Track C is a real type-system extension, but a pragmatic one.
- Newtypes/wrappers are about nominal distinction at module/API boundaries, not about replacing structural records everywhere.
- If accepted, the first version should prefer simple, explicit semantics:
  - explicit construction,
  - explicit unwrapping or field access rules,
  - no implicit coercions between wrapper and underlying type.
- Do not entangle Track C with FFI ABI exposure in the same milestone. FFI can treat wrappers as a later policy layer if needed.

### Phase C0. Freeze The Wrapper Model

Purpose:
- Decide one concrete surface and semantic model before implementation.

Tasks:
- Choose and document:
  - declaration form,
  - construction form,
  - whether wrappers can target primitives, records, and existing aliases,
  - whether wrappers support deriving selected traits,
  - whether wrappers are opaque outside their defining module.
- Write examples centered on API-boundary use cases:
  - `UserId`,
  - `EmailAddress`,
  - domain-specific record wrappers where raw structural equivalence is undesirable.
- State explicit non-goals for v1:
  - no zero-cost representation promises unless proven,
  - no auto-coercions,
  - no special FFI behavior in the first pass.

Likely files:
- `docs/SYNTAX.md`
- `docs/features/records.md`
- `docs/features/traits.md`
- this plan

Exit criteria:
- Wrapper semantics are written normatively before parser/typechecker work begins.

### Phase C1. Add Canonical Representation And Typechecking

Purpose:
- Introduce one canonical representation and one predictable compatibility story.

Tasks:
- Extend canonical AST and `Types.mono_type` as needed for wrappers/newtypes.
- Update annotation conversion, unification, pretty-printing, and substitution.
- Define exactly where wrapper transparency is allowed:
  - probably nowhere by default except explicit unwrap/constructor operations.
- Define trait interaction:
  - builtin derives if allowed,
  - explicit impls,
  - field-only trait satisfaction only if wrapper semantics intentionally expose the wrapped record shape.
- Add module-boundary tests once modules land.

Likely files:
- `lib/frontend/syntax/ast.ml`
- `lib/frontend/typecheck/types.ml`
- `lib/frontend/typecheck/annotation.ml`
- `lib/frontend/typecheck/unify.ml`
- `lib/frontend/typecheck/infer.ml`
- `lib/frontend/typecheck/trait_solver.ml`

Required tests:
- wrapper around primitive success/failure cases,
- wrapper around record success/failure cases,
- no implicit assignment/coercion to underlying type,
- explicit constructor/unwrap behavior,
- trait implementation/derive behavior per the frozen spec.

Exit criteria:
- Wrappers are a real nominal boundary rather than an alias spelling variant.

### Phase C2. Module-Facing Validation

Purpose:
- Verify that wrappers solve the API-boundary problems they were added for.

Tasks:
- After modules exist, add cross-module fixtures:
  - exporting a wrapper type,
  - importing code cannot treat it as the raw underlying type without explicit API,
  - private constructors or opaque export behavior if chosen in Phase C0.
- Add examples to stdlib/prelude planning only if wrappers are actually useful there.

Likely files:
- `test/fixtures/**`
- `docs/plans/module-system.md` only if it needs a follow-up note

Exit criteria:
- Wrapper behavior is validated across actual module boundaries, not only within one file.

## Track D. Records, Rows, And Trait/Shape Consistency

### Current State

- The compiler already uses row-polymorphic machinery internally.
- `docs/features/records.md` explicitly defers open row variables in user annotations.
- Field-only traits currently act as named structural constraints and can be used to approximate some row-like use cases.
- Mixed traits combine structural fields and nominal methods, which is powerful but also where semantic complexity concentrates.

### Binding Decisions

- Track D is partly mandatory and partly optional.
- Mandatory:
  - freeze the intended boundary between field-only traits, mixed traits, and user-facing row syntax,
  - improve diagnostics and consistency for the chosen model.
- Optional:
  - exposing open-row surface syntax to users.
- This plan does not force a split between field-only traits and nominal traits immediately, but it does require the docs and semantics to stop being implicit about the split.

### Phase D0. Freeze The Traits-Vs-Rows Policy

Purpose:
- Make the language boundary explicit before more APIs are built on it.

Tasks:
- Write one clear policy section in docs:
  - field-only traits are named structural constraints,
  - method-only traits are nominal behavior contracts,
  - mixed traits are allowed but are the most complex case,
  - open rows remain internal-only unless later accepted below.
- State when library authors should prefer:
  - field-only traits,
  - concrete records,
  - future wrappers/newtypes,
  - or explicit trait objects from the syntax follow-up plan.
- Tighten examples in `docs/features/traits.md` and `docs/features/records.md` so they present a coherent story instead of two partially overlapping ones.

Likely files:
- `docs/features/traits.md`
- `docs/features/records.md`
- this plan

Exit criteria:
- The public language story for records/rows/field traits is explicit and non-contradictory.

### Phase D1. Improve Row/Shape Diagnostics And Internal Consistency

Purpose:
- Make the existing internal row model less surprising even if user-facing row syntax remains deferred.

Tasks:
- Improve diagnostics for:
  - missing fields,
  - extra fields vs projection behavior,
  - recursive row edge cases,
  - field-only supertrait chains,
  - mixed-trait satisfaction failures.
- Add regression tests for the row/trait edge cases already listed in the roadmap.
- Audit projection/coercion behavior so it is deterministic and documented:
  - when a field-only trait in type position narrows visible fields,
  - when emitter inserts projection wrappers,
  - when mixed traits do or do not permit the same structural treatment.

Likely files:
- `lib/frontend/typecheck/annotation.ml`
- `lib/frontend/typecheck/trait_solver.ml`
- `lib/frontend/typecheck/infer.ml`
- `lib/backend/go/emitter.ml`
- `docs/features/traits.md`
- `docs/features/records.md`

Required tests:
- row edge-case diagnostics,
- field-only supertrait chains,
- mixed-trait rejection/satisfaction cases,
- projection wrapper behavior,
- alias-to-field-only-trait visibility tests.

Exit criteria:
- Current field-only/mixed trait behavior is deterministic, documented, and covered by regression tests.

### Phase D2. Accept Or Reject User-Facing Open Row Annotations

Purpose:
- Decide whether field-only traits are sufficient as the main user-facing shape language, or whether explicit open-row syntax is needed.

Tasks:
- Collect real use cases after the pre-module foundation and hardening stages:
  - places where naming every record shape as a trait is noisy,
  - places where field-only traits compose poorly,
  - places where mixed traits are carrying too much semantic weight.
- If rejected:
  - document that field-only traits remain the intended user-facing shape mechanism,
  - keep open rows internal-only.
- If accepted:
  - freeze a concrete annotation syntax,
  - define how it coexists with field-only traits,
  - define how it prints in diagnostics,
  - specify whether it is purely annotation syntax or also participates in inferred type printing.

Likely files:
- `docs/features/records.md`
- `docs/features/traits.md`
- `docs/SYNTAX.md`
- this plan

Exit criteria:
- Open-row surface syntax is either explicitly accepted with a spec or explicitly rejected for the foreseeable roadmap.

### Phase D3. Implement User-Facing Open Rows If Accepted

Purpose:
- Expose the internal row model to users only if D2 accepts it.

Tasks:
- Extend surface syntax, canonical AST, annotation conversion, and pretty-printing for the accepted row syntax.
- Keep the relationship to field-only traits explicit:
  - which problems rows solve better,
  - which problems named field traits still solve better.
- Add focused parser/typechecker/diagnostic tests.

Likely files:
- `docs/SYNTAX.md`
- `lib/frontend/syntax/surface_ast.ml`
- `lib/frontend/syntax/parser.ml`
- `lib/frontend/syntax/lower.ml`
- `lib/frontend/typecheck/annotation.ml`
- `lib/frontend/typecheck/types.ml`
- `lib/frontend/typecheck/unify.ml`

Exit criteria:
- If accepted, open rows are fully documented and typechecked; if not accepted, this phase never starts.

## Track E. Effects And Purity Semantics

### Current State

- The language surface distinguishes pure `->` from effectful `=>`.
- The checker already enforces some purity restrictions, especially pure code calling effectful functions.
- The effect model is still not fully frozen; there are still tests and roadmap notes indicating some `=>` semantics are not fully enforced or specified.
- Stdlib and FFI plans both depend on effect annotations being reliable.

### Binding Decisions

- Track E Stage 1 is mandatory: freeze the semantics already implied by `->` and `=>`.
- This does not require a full algebraic effect system.
- The first goal is consistency:
  - what `=>` means,
  - what counts as effectful,
  - when a pure declaration is rejected,
  - whether "effectful annotation with pure body" remains allowed or is tightened.

### Phase E0. Freeze The Minimal Effect Model

Purpose:
- Turn the current partially implicit model into a written semantic contract.

Tasks:
- Update docs to define:
  - whether effects are a boolean capability or something richer in v1,
  - whether `=>` on a pure body is permitted, warned, or rejected,
  - how effectfulness propagates through function literals, methods, trait defaults, and wrappers,
  - how effect checking interacts with inference and annotations.
- Align `docs/ROADMAP.md`, `docs/SYNTAX.md`, and any feature docs so they say the same thing.
- Add a compact effect-semantics matrix to this plan.

Likely files:
- `docs/SYNTAX.md`
- `docs/ROADMAP.md`
- this plan

Exit criteria:
- The v1 effect model is written explicitly rather than inferred from tests.

### Phase E1. Enforce The Frozen Rules Consistently

Purpose:
- Make checker behavior match the frozen minimal model.

Tasks:
- Update inference/checking so all declaration kinds obey the same effect rule:
  - top-level functions,
  - lambdas,
  - trait methods/defaults,
  - impl methods,
  - inherent methods.
- Remove any lingering "no enforcement yet" behavior once the policy is chosen.
- Ensure codegen does not need its own effect interpretation.

Likely files:
- `lib/frontend/typecheck/infer.ml`
- `lib/frontend/typecheck/types.ml`
- `lib/frontend/syntax/ast.ml` only if an effect bit or helper representation needs cleanup

Required tests:
- pure calling pure,
- pure calling effectful rejection,
- effectful annotation with pure body behavior per the frozen policy,
- trait/default/impl method effect propagation,
- FFI/extern purity prerequisites once extern lands.

Exit criteria:
- `->` and `=>` have one consistent compiler meaning across all declaration sites.

## Track F. Better Bidirectional Typing / Expected-Type Propagation

### Current State

- The compiler already has some checking-against-expected-type behavior.
- In many cases it still primarily infers first and only then checks against an expected type.
- This leaves several contexts more brittle than they need to be:
  - lambdas,
  - empty collection literals,
  - coercion sites,
  - match-arm bodies,
  - ambiguous record/list/hash literals in expected-type contexts.

### Binding Decisions

- Track F is a pragmatic quality-of-implementation improvement, not a new surface feature.
- Prefer pushing expected types downward where they genuinely reduce ambiguity or improve diagnostics.
- Do not turn this into a large inference rewrite if a targeted expected-type pass is enough.

### Phase F0. Freeze Targeted Expected-Type Use Cases

Purpose:
- Limit the scope to the ambiguous cases that matter most.

Tasks:
- Enumerate and prioritize the contexts where expected-type propagation should improve behavior:
  - lambda parameter/result guidance,
  - empty list/hash literals,
  - match-arm alignment,
  - object/trait coercion sites after syntax-follow-up Track B if accepted,
  - wrapper/newtype construction sites if Track C lands.
- Define what "improvement" means for each:
  - accept previously ambiguous valid programs,
  - reject with a clearer diagnostic,
  - or preserve behavior but improve determinism.

Likely files:
- this plan
- `lib/frontend/typecheck/infer.ml`

Exit criteria:
- The scope of the bidirectional-typing pass is frozen and testable.

### Phase F1. Implement Targeted Expected-Type Propagation

Purpose:
- Make the frozen use cases behave predictably.

Tasks:
- Refine `check_expression` / related inference entrypoints so expected types are pushed down earlier in the targeted contexts.
- Keep the changes narrow and measurable:
  - do not regress principal-typing behavior in unrelated code,
  - do not introduce order-sensitive inference.
- Improve diagnostics in the same pass:
  - when an expected type shaped inference,
  - when ambiguity remains,
  - why a literal/lambda did not match the expected type.

Likely files:
- `lib/frontend/typecheck/infer.ml`
- `lib/frontend/typecheck/unify.ml`
- `lib/frontend/typecheck/types.ml`

Required tests:
- empty literal acceptance in expected-type contexts,
- lambda checking with expected function types,
- clearer failures for ambiguous literals,
- match-arm body consistency in expected-result contexts.

Exit criteria:
- The targeted ambiguous cases become more predictable without broad inference regressions.

## Track G. Trait-System Extensions After Module Boundaries Exist

### Current State

- The current trait model is already ambitious: field-only, method-only, and mixed traits; derives; impls; inherent methods.
- `docs/ROADMAP.md` already lists associated types as a deferred trait enhancement.
- Compared with more exotic features such as rank-N, HKTs, or dependent typing, associated types fit the current design more directly and solve more ordinary library problems.

### Binding Decisions

- Track G happens after modules exist.
- Associated types are the first major post-module trait-system extension to evaluate.
- Rank-N, HKTs, and dependent types remain deferred beyond this plan.
- If associated types are accepted, they must integrate with:
  - module signatures,
  - trait objects if those land,
  - derive/impl rules,
  - diagnostics,
  - and codegen-facing typed artifacts where relevant.

### Phase G0. Accept Or Reject Associated Types

Purpose:
- Prevent a large trait feature from landing as abstract compiler ambition without real API pressure.

Tasks:
- Collect concrete module/stdlib use cases:
  - iterator item type,
  - parser output type,
  - collection key/value relationships,
  - builder/result-associated output shapes,
  - any trait where extra generic parameters currently feel artificial.
- Compare associated types against the status quo:
  - extra trait parameters everywhere,
  - ad hoc wrapper types,
  - or postponed trait-object/intersection features.
- If rejected:
  - update the roadmap and stop.
- If accepted:
  - freeze a concrete semantic matrix before parser/typechecker work:
    - declaration syntax,
    - projection syntax,
    - impl syntax,
    - equality/normalization behavior,
    - inference expectations,
    - interaction with trait objects/intersections if those later land.

Likely files:
- `docs/ROADMAP.md`
- `docs/features/traits.md`
- `docs/SYNTAX.md`
- this plan

Exit criteria:
- Associated types are explicitly accepted or rejected with reasons.

### Phase G1. Implement Associated Types If Accepted

Purpose:
- Land one coherent associated-type model rather than a partial parser experiment.

Tasks:
- Extend canonical syntax/types/trait metadata for associated type declarations and impl bindings.
- Update trait registration, trait solving, annotation conversion, normalization, and diagnostics.
- Extend module signatures so exported traits carry associated-type information.
- Add targeted codegen and resolution artifacts only if runtime-relevant information is needed downstream.

Likely files:
- `docs/SYNTAX.md`
- `docs/features/traits.md`
- `lib/frontend/syntax/ast.ml`
- `lib/frontend/syntax/parser.ml`
- `lib/frontend/typecheck/types.ml`
- `lib/frontend/typecheck/trait_registry.ml`
- `lib/frontend/typecheck/trait_solver.ml`
- `lib/frontend/typecheck/annotation.ml`
- `lib/frontend/typecheck/infer.ml`
- `lib/frontend/typecheck/module_sig.ml` after modules land

Required tests:
- trait declaration with associated type,
- impl binding of associated type,
- projection/normalization tests,
- module-signature export/import behavior,
- negative diagnostics for missing/ambiguous associated type bindings.

Exit criteria:
- Associated types, if accepted, work as a coherent trait feature rather than a parser-only addition.

## Track H. Union Narrowing, Compatibility, And Intersection Coordination

### Current State

- Union types already exist and are part of the public language model.
- Narrowing works in many straightforward cases, but the language still does not have a fully frozen "smart everywhere" story for:
  - complement reasoning,
  - nested control-flow refinement,
  - order-independent normalization,
  - interaction with records and traits,
  - and later interaction with general intersections if Track C from `docs/plans/syntax-rework-followup.md` is accepted.
- The roadmap already calls out deeper control-flow precision and compatibility improvements as unfinished work.

### Binding Decisions

- Track H is mandatory for the existing union feature; only the intersection-coordination subphase is conditional.
- The goal is not "more syntax." The goal is one deterministic semantic model for:
  - union normalization,
  - narrowing through `if`/`match`,
  - compatibility/subtyping checks,
  - diagnostics explaining why narrowing succeeded or failed.
- If general intersections are later accepted in the syntax-follow-up plan, Track H must define the union/intersection interaction explicitly rather than allowing ad hoc pairwise behavior to accumulate.

### Phase H0. Freeze The Union Semantic Matrix

Purpose:
- Turn "smart unions" from an aspiration into a testable semantic contract.

Tasks:
- Write one explicit matrix for:
  - normalization rules,
  - duplicate-member elimination,
  - ordering/canonical printing,
  - complement narrowing through `if`, `else`, and `match`,
  - nested-branch refinement,
  - compatibility of unions in function/input/output positions,
  - interaction with record shapes and field-only traits where applicable.
- Update `docs/features/unions.md` so deferred items are either:
  - brought into Track H implementation scope,
  - or left clearly deferred with rationale.
- Add a list of required positive and negative narrowing examples to this plan.

Likely files:
- `docs/features/unions.md`
- `docs/ROADMAP.md`
- this plan

Exit criteria:
- The union normalization/narrowing contract is written down before further implementation work.

### Phase H1. Implement Deterministic Narrowing And Compatibility

Purpose:
- Make the current union feature feel complete and predictable in ordinary code.

Tasks:
- Update type normalization and compatibility logic for deterministic behavior:
  - flatten nested unions,
  - sort/dedupe consistently,
  - keep diagnostics stable and order-insensitive.
- Improve narrowing/refinement in the checker for:
  - `if`/`else`,
  - `match`,
  - nested boolean flow where practical,
  - branch complements when one branch rules out a known alternative.
- Define and implement how unions interact with:
  - record shapes,
  - field-only traits,
  - method-bearing traits when type-directed narrowing is possible,
  - expected-type propagation from Track F where relevant.
- Keep emitter behavior artifact-driven where narrowing affects generated code paths.

Likely files:
- `lib/frontend/typecheck/types.ml`
- `lib/frontend/typecheck/unify.ml`
- `lib/frontend/typecheck/infer.ml`
- `lib/frontend/typecheck/exhaustiveness.ml`
- `lib/frontend/typecheck/resolution_artifacts.ml`
- `lib/backend/go/emitter.ml`

Required tests:
- normalization and pretty-printing stability,
- branch-complement narrowing,
- nested control-flow refinement,
- match-based narrowing,
- union interaction with records and traits,
- deterministic diagnostics for failed narrowing/compatibility cases,
- representative codegen regressions where narrowed knowledge affects emission.

Exit criteria:
- Union behavior is deterministic, documented, and materially smarter across normal control flow.

### Phase H2. Coordinate With General Intersections If Accepted

Purpose:
- Avoid bolting intersections onto unions as an afterthought if `TIntersection` later lands.

Tasks:
- This phase starts only if `docs/plans/syntax-rework-followup.md` Track C is accepted.
- Freeze and implement:
  - union/intersection normalization interaction,
  - compatibility rules for mixed `Union`/`Intersection` positions,
  - narrowing behavior when both are present,
  - diagnostics for unsupported or ambiguous combinations.
- Keep the accepted interaction matrix written in both this plan and the syntax-follow-up plan so the ownership boundary stays clear.

Likely files:
- `docs/features/unions.md`
- `docs/plans/syntax-rework-followup.md`
- `lib/frontend/typecheck/types.ml`
- `lib/frontend/typecheck/unify.ml`
- `lib/frontend/typecheck/infer.ml`

Required tests:
- normalization of mixed union/intersection types,
- narrowing cases that involve intersections,
- compatibility success/failure cases,
- diagnostic determinism for mixed-type interactions.

Exit criteria:
- If intersections land, unions still have one coherent semantic story instead of a second ad hoc compatibility layer.

## Track I. Backend-Aware Lowering And Opportunistic Mutation

### Current State

- Marmoset lowers high-level constructs to Go source and relies on the Go compiler for low-level optimization work.
- Some high-level source patterns are still likely to produce more temporaries, copies, or staged reconstruction than necessary:
  - record spread/update chains,
  - fresh aggregate construction with later field overrides,
  - match lowering that may materialize intermediate values conservatively,
  - other expression-heavy lowering paths where generated Go could use explicit mutable locals without changing language semantics.
- Marmoset source semantics remain value-oriented and effectively immutable by default, so any introduced mutation must remain an implementation detail of generated code, not a user-visible semantic change.

### Binding Decisions

- Track I is optional and performance-driven. It is not a new language feature.
- Do not expose new mutability syntax, new effect rules, or new user-facing guarantees as part of this track.
- Prefer conservative freshness/uniqueness-based rewrites over a full alias-analysis or escape-analysis framework.
- The first version should target the highest-value cases where mutation is clearly unobservable:
  - construction of fresh records/arrays/maps,
  - spread/update lowering on fresh temporaries,
  - builder-style lowering for fresh values in backend codegen,
  - selective match-lowering temporaries if Track A/B work makes those opportunities explicit.
- Do not introduce a whole-program optimizing IR solely to support this track. If extra structure is needed, keep it local to the affected lowering/codegen paths and typed artifacts.
- Every rewrite in this track must preserve:
  - source evaluation order,
  - effect behavior,
  - aliasing-observable semantics,
  - diagnostics stability where relevant.

### Phase I0. Freeze The Safe Opportunistic-Mutation Model

Purpose:
- Define exactly which backend rewrites are allowed before any optimization work begins.

Tasks:
- Write one explicit safety model for opportunistic mutation in generated code:
  - what counts as a fresh value,
  - which rewrites are allowed only for synthetically introduced temporaries,
  - when spread/update chains may collapse into one mutable builder,
  - when effectful subexpressions force the backend to keep a more direct lowering.
- List the first candidate transformations to evaluate:
  - record spread/update compaction,
  - fresh aggregate builder lowering,
  - temporary reuse in match lowering where binding/evaluation order remains explicit.
- Document which analyses are intentionally out of scope for the first version:
  - full alias analysis,
  - whole-program escape analysis,
  - speculative mutation of values whose ownership is not obvious.
- Record where the safety contract lives:
  - emitter-local helper logic,
  - a narrow typed artifact if checker knowledge is required,
  - or both.

Likely files:
- `docs/ARCHITECTURE.md`
- this plan
- `lib/backend/go/emitter.ml`
- `lib/frontend/typecheck/resolution_artifacts.ml` only if a narrow artifact is needed

Exit criteria:
- There is one written safety model for backend-introduced mutation.
- The first implementation target set is explicit and intentionally conservative.

### Phase I1. Implement Selective Opportunistic Mutation

Purpose:
- Reduce avoidable allocation/reconstruction in generated code without changing Marmoset semantics.

Tasks:
- Add a localized lowering/codegen step for the approved cases from Phase I0:
  - rewrite eligible fresh record/aggregate construction into mutable Go locals/builders,
  - collapse eligible spread/update chains into one staged mutable construction path,
  - reuse temporary storage in match lowering only when Track A/B semantics make the ordering and binding behavior explicit.
- Keep the implementation benchmark-driven:
  - start with a small number of representative hot shapes,
  - measure generated-code size/runtime impact,
  - expand only where the win is real.
- Keep a readable non-optimized path or fallback structure where it materially improves maintenance/debuggability.
- Ensure Go emission remains obviously correct:
  - no duplicated effectful evaluation,
  - no accidental sharing changes,
  - no dependence on Go-specific undefined behavior or assembly tricks.

Likely files:
- `lib/backend/go/emitter.ml`
- `docs/ARCHITECTURE.md`
- `test/fixtures/codegen/**`
- `test/fixtures/runtime/**`

Required tests:
- end-to-end regressions for record spread/update cases,
- cases with effectful subexpressions proving evaluation order is preserved,
- alias-sensitive regressions where mutation must not leak through shared values,
- generated-code or integration regressions for representative optimized match/aggregate shapes,
- benchmark notes or representative performance measurements for adopted rewrites.

Exit criteria:
- Approved opportunistic-mutation rewrites are implemented conservatively and covered by semantic regressions.
- Generated Go is simpler/faster for the targeted cases without changing source-language behavior.

## Cross-Track Sequencing

### Hard Prerequisites

- `docs/plans/syntax-rework.md` must complete before this plan becomes the active milestone.
- Any accepted pre-module work from `docs/plans/syntax-rework-followup.md` must be applied before Stage 1 here is considered complete.
- Stage 1 from this plan must complete before the module-system work becomes the mainline milestone.
- Stage 2 from this plan must complete before modules/prelude/FFI are treated as the stable public-platform layer.
- Track G starts only after module signatures exist or are close enough that API-boundary examples are meaningful.
- Trait-object and general-intersection work remain in `docs/plans/syntax-rework-followup.md` and should be coordinated with Track D/F/G/H here, not duplicated.

### Soft Dependencies

- Track B0-B1 should start as early as practical, even before Stage 1 is fully complete, because diagnostics and backend-contract cleanup reduce the risk of all later tracks.
- Track B1 should happen before or alongside any Track A codegen completion, because backend parity work is easier if emitter contracts are already improving.
- If Track A adopts a decision-tree or match-plan representation for pattern lowering, define its ownership and typed contract in Track B1 before or alongside the Track A implementation work.
- Track I should start only after the relevant Track A/B work has stabilized the semantics and typed contracts for the constructs it wants to optimize.
- Track D1 should happen before Track D2, because better diagnostics often clarify whether open rows are actually needed.
- Track C and Track F pair well:
  - wrappers/newtypes introduce more expected-type-sensitive contexts,
  - stronger bidirectional typing makes those contexts nicer to use.
- Track G should not start until module-system signatures are stable enough to carry trait metadata cleanly.

## Highest-Risk Areas

- Finishing pattern/codegen parity without duplicating semantic logic between checker and emitter.
- Letting field-only traits and mixed traits continue to coexist without the docs and diagnostics becoming contradictory.
- Exposing user-facing rows too early and ending up with two shape systems that are both half-blessed.
- Tightening effects semantics without accidentally breaking the stdlib/FFI direction.
- Improving union narrowing without accidentally introducing order-sensitive or brittle inference behavior.
- Adding wrappers or associated types before module boundaries exist strongly enough to validate their real value.
- Introducing backend mutation that accidentally changes evaluation order, aliasing behavior, or effect observability.

## Deliverables Checklist

This plan is only complete when it produces all of the following, not just code changes:
- docs that state the intended semantics explicitly,
- parser/typechecker/codegen behavior aligned with those docs,
- targeted positive and negative tests,
- typed artifact contracts where backend behavior depends on checker decisions,
- CI drift checks for canonical docs and representative generated output.

## Immediate Next Step

Do not start this plan as the mainline milestone until the active syntax work is finished:
1. finish `docs/plans/syntax-rework.md`,
2. run the accepted pieces of `docs/plans/syntax-rework-followup.md`,
3. then start Stage 1 of this plan:
   - settle `shape`/trait/row policy,
   - settle effect semantics,
   - settle union semantics,
   - settle any wrapper/newtype boundary you want before modules,
   - apply any accepted existential/trait-object design from the follow-up plan.
4. while Stage 1 is happening, start Track B0-B1 in parallel where possible.
5. after Stage 1 is frozen, run Stage 2 parity/hardening before modules.

Do not start modules/stdlib/FFI before that pre-module foundation is in place.
