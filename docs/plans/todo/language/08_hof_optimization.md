# Higher-Order Function Optimization Plan

## Maintenance

- Last verified: 2026-04-03
- Implementation status: Planning (not started)
- Update trigger: Any module-system, stdlib collections API, pipe/placeholder semantics, or Go backend codegen change
- Prerequisites:
  - `docs/plans/done/language/06_module-system.md`
  - `docs/plans/done/language/07_prelude.md`
  - `docs/plans/todo/language/03_ffi.md`
  - `docs/plans/todo/language/04_stdlib.md`

## Summary

After modules and stdlib APIs stabilize, optimize a bounded set of standard-library higher-order functions in the Go backend so expressive pipe-heavy Marmoset code does not pay unnecessary closure, indirect-call, and intermediate-allocation costs. This plan is intentionally post-stdlib: it should target real canonical module APIs, not speculative names or temporary library shapes.

## Context

`|>` itself is not the main performance concern. The real Go cost center is callback-heavy higher-order style:

- anonymous sections/lambdas become Go function literals
- generic HOF implementations pay indirect callback calls in hot loops
- chained HOFs allocate intermediate containers unless the backend recognizes and combines them

This plan is about keeping the expressive surface while improving backend lowering.

It is not the place to solve every callable-representation problem. Pre-stdlib backend
cleanup such as hoisting non-capturing sections/lambdas, hardening interface-value or
`Dyn[...]` packaging, removing dead runtime metadata, and trimming generic emitted-code
debt is separate and should happen earlier if needed. This plan starts only once
module-qualified stdlib identities are stable enough to optimize against directly.

## In Scope

- recognizing canonical stdlib HOF identities after modules and stdlib land
- single-call specialization for a bounded initial set of collection/string HOFs
- direct lowering of safe sink operations when the landed stdlib shape justifies it
- straight-line fusion for a small number of safe HOF pipelines
- benchmark coverage and semantic canaries for optimized and fallback paths
- conservative fallback to ordinary stdlib calls whenever a pattern is unsupported or unclear

## Out Of Scope

- optimizing arbitrary user-defined higher-order functions
- introducing new user-visible syntax or semantics
- building a general optimizer IR or arbitrary whole-program optimizer
- speculative rewrites against pre-stdlib names such as temporary `list.filter` vs `list.select` spellings
- mutation-visible or reordering-visible transformations
- pre-stdlib callable-representation work such as closure-hoisting for non-capturing sections
- generic interface-runtime cleanup such as shared witness adapters, dead helper
  suppression, primitive formatting cleanup, or `Dyn[...]` payload trimming

## Locked Decisions

### 1. This Is A Library-Aware Optimization Pass

Match resolved canonical stdlib call targets, not raw surface spelling. The optimizer must key off symbol identity or an equivalent module-qualified resolution artifact, not textual names alone.

### 2. Source Semantics Stay Fixed

Optimization must preserve:

- callback invocation count
- callback evaluation order
- short-circuit behavior
- effect sequencing
- panic / error behavior

If a rewrite cannot preserve those properties clearly, it must fall back.

### 3. Specialization Comes Before Fusion

First make single HOF calls optimize well. Only after those paths are stable should the backend combine multiple HOF stages.

### 4. The Initial Target Set Must Be Small And Measured

Start with the HOFs that are both common and straightforward to lower. Expand only after benchmarks and real stdlib usage justify it.

### 5. Barriers Stay Explicit Until Proven Safe

Operations such as sorting, reversing, deduplication, reduction, and effectful iteration should be treated as optimization barriers by default. They can be revisited later, but they are not the initial fusion surface.

### 6. Unsupported Patterns Must Degrade Gracefully

There must always be a correct fallback to the ordinary stdlib implementation. Unsupported callback shapes, unresolved types, dynamic trait-object paths, or effectful interactions must not block compilation.

## Candidate Surface After Stdlib Lands

The exact names depend on the stdlib plan that actually ships. The initial candidate set should be chosen from the landed APIs, likely including some subset of:

- `list.map`
- `list.select`
- `list.reject`
- `list.each`
- `list.find`
- `list.any?`
- `list.all?`
- `list.none?`
- a join-like sink if the stdlib ends up with a canonical list/string join surface

The optimization plan must use the names that really land in `04_stdlib.md`, not the names that happened to appear in older exploratory docs.

## Target User Model

The user should be able to write:

```mr
posts
|> list.select(Visible.visible_to(_, user))
|> list.map(Card.render_card)
|> str.join("\n")
```

without being forced to manually rewrite it into lower-level loops just to avoid obvious backend overhead.

That does not mean every HOF pipeline becomes free. It means the backend should recognize a bounded set of common, well-understood patterns and lower them competitively while preserving the expressive source form.

## Implementation Plan

### Phase H0. Freeze The Optimization Surface

- Wait for modules and stdlib APIs to land.
- Choose the initial target HOF set from the actual exported stdlib modules.
- Record benchmark scenarios that reflect intended style:
  - direct function reference callbacks
  - qualified callable values
  - non-capturing sections
  - capturing predicates/projections
  - pure pipelines
  - effectful iteration
- Capture baseline generated Go for those scenarios before optimization begins.

### Phase H1. Carry Canonical HOF Identity Into Codegen

- Ensure the backend can distinguish canonical stdlib HOF calls by resolved identity, not by string spelling.
- Reuse or extend resolution artifacts so module-qualified callable identity survives through emission.
- Add a callback-shape classifier for the initial optimization set:
  - direct named function reference
  - qualified callable value
  - hoisted non-capturing section/lambda
  - inline non-capturing function literal
  - capturing function literal
  - effectful callback

**Goal:** by the end of this phase, the emitter can reliably answer “which stdlib HOF is this?” and “what kind of callback is being passed?”.

### Phase H2. Single-Call HOF Specialization

- Lower the initial target HOFs directly in the Go emitter.
- Keep the stdlib implementation as the semantic reference and fallback.

Examples of expected lowering shape:

- `list.map(xs, f)` -> one direct loop allocating the result slice once
- `list.select(xs, p)` / `list.reject(xs, p)` -> one direct loop with append
- `list.each(xs, f)` -> one direct loop preserving effect order
- `list.any?` / `list.all?` / `list.none?` / `list.find` -> short-circuit loops

Rules:

- prefer specialization when callback shape is recognized and types are concrete enough
- fall back when callback shape is too dynamic or semantics are unclear
- reject no valid program solely because optimization did not apply

### Phase H3. Add Conservative Fusion

- Build a small, explicit fusion table for safe straight-line pipelines.
- Start with the simplest useful combinations, for example:
  - `select -> map`
  - `reject -> map`
  - `map -> map`
  - optionally `select -> map -> join` if the sink API is stable and common

Default barriers:

- sort-like operations
- reverse-like operations
- dedup / uniq-like operations
- reductions
- effectful iteration
- dynamic callback cases that obscure ordering or short-circuit behavior

Fusion rules must preserve:

- callback order
- short-circuit rules for sink operations
- exact effect sequencing

If any stage in the candidate pipeline is effectful, ambiguous, or unsupported, stop fusing and fall back to the normal call boundary there.

### Phase H4. Hardening, Benchmarks, And Expansion Decisions

- Add unit and integration tests that compare optimized vs fallback semantics.
- Add perf benchmarks for the representative pipelines from Phase H0.
- Inspect generated Go to confirm:
  - fewer anonymous `func(...)` literals in optimized paths
  - fewer intermediate slice allocations
  - preserved direct-call lowering for named/qualified callables
- Decide whether additional HOFs are worth optimizing only after data exists.

## Exit Criteria

- The backend recognizes a small, explicit set of canonical stdlib HOFs by resolved identity.
- Single-call specialization exists for the initial target set with correct fallback behavior.
- At least one useful fusion family is implemented conservatively and covered by semantic canaries.
- Benchmarks show that common pipe-heavy HOF codegen improves materially on the targeted paths.
- The implementation does not require users to abandon expressive stdlib HOF style in order to get acceptable performance for the optimized cases.

## Risks

1. **Optimizer drift against evolving stdlib APIs.** If stdlib naming/order/module layout is still moving, the optimization logic will churn. This is why the plan is post-stdlib.
2. **Too much ambition too early.** Trying to optimize every HOF or every callback shape will create a fragile pseudo-optimizer. Keep the first target set intentionally small.
3. **Semantic drift under effects.** Fusion in particular can silently break ordering or short-circuit behavior if it is not conservative enough.
4. **Codegen complexity.** Ad hoc loop emitters can become hard to reason about if there is no explicit fallback contract and benchmark discipline.
5. **Mismatch with earlier callable work.** If non-capturing section hoisting does not exist yet, some “obvious” specializations will still route through function literals and be less effective.

## Critical Files

| File | Role |
|------|------|
| `lib/backend/go/emitter.ml` | primary specialization and fusion implementation |
| `lib/frontend/typecheck/resolution_artifacts.ml` | preserve canonical callable identity into codegen if current artifacts are insufficient |
| `lib/frontend/typecheck/infer.ml` | source of method/call resolution metadata consumed by the emitter |
| `docs/plans/done/language/06_module-system.md` | required so canonical module-qualified identities exist |
| `docs/plans/todo/language/04_stdlib.md` | defines the APIs this plan is allowed to optimize |

## Related Plans

- `docs/plans/done/language/06_module-system.md` must land first so optimization can key off real module-qualified identities.
- `docs/plans/todo/language/04_stdlib.md` must land first so the optimizer targets the shipped HOF surface, not speculative names.
- `docs/plans/todo/language/05_post-modules-type-system-expansion.md` remains a separate post-modules track; this plan is backend/performance-focused, not a type-system expansion.
- `docs/plans/todo/language/07_forall-exists.md` owns the earlier interface-runtime cleanup work that makes shared adapters, cleaner callback shapes, and lower-overhead value packaging available before stdlib HOF specialization.
- Pre-stdlib callable representation work such as hoisting non-capturing sections should be tracked separately and can improve the effectiveness of this plan, but is not part of it.
