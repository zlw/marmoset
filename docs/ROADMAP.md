# Marmoset Roadmap

## Maintenance

- Last verified: 2026-04-04
- Implementation status: Canonical (actively maintained)
- Update trigger: Any change to milestone status, plan ordering, or deferred-ideas ownership

## How To Use This File

- `docs/plans/done/**` contains completed milestones and historical implementation records.
- `docs/plans/todo/**` contains concrete planned work.
- This file is the high-level overview and ordering document.
- Only genuinely unplanned or "cool to have later" ideas should remain here as deferred bullets.
- When a deferred idea becomes concrete, give it a plan under `docs/plans/todo/**` and replace the loose roadmap bullet with a link.

## Completed Milestones

### Language

1. [Function model rework](docs/plans/done/language/01_function-model.md)
2. [Syntax rework](docs/plans/done/language/02_syntax-rework.md)
3. [Syntax rework follow-up](docs/plans/done/language/03_syntax-rework-followup.md)
4. [Pre-modules semantics foundation](docs/plans/done/language/04_pre-modules-semantics-foundation.md)
5. [Pre-modules parity and hardening](docs/plans/done/language/05_pre-modules-parity-and-hardening.md)
6. [Module system](docs/plans/done/language/06_module-system.md)

### Tooling

1. [Diagnostics rework](docs/plans/done/tooling/01_diagnostics-rework.md)
2. [Fixture harness migration](docs/plans/done/tooling/02_fixture-harness-migration.md)
3. [Module-aware LSP navigation and completion](docs/plans/done/tooling/03_module-aware-lsp.md)

### CI

1. [GitHub workflow migration](docs/plans/done/ci/01_github-workflow.md)

## Ordered Active Roadmap

### Main Language Track

1. [Prelude](docs/plans/todo/language/02_prelude.md)
2. [FFI](docs/plans/todo/language/03_ffi.md)
3. [Stdlib](docs/plans/todo/language/04_stdlib.md)
4. [Post-modules type-system expansion](docs/plans/todo/language/05_post-modules-type-system-expansion.md)

### Queued Language Work

1. [Function overloading](docs/plans/todo/language/06_function-overloading.md)
2. [Universal constraints and interface value types](docs/plans/todo/language/07_forall-exists.md)

### Post-Stdlib Performance Work

1. [Higher-order function optimization](docs/plans/todo/language/08_hof_optimization.md)

### Parallel Tooling Track

- [OCaml test harness rewrite](docs/plans/todo/tooling/01_ocaml-test-harness.md)

### Parallel CI Track

- [Single OCaml setup in CI via prebuilt runner image](docs/plans/todo/ci/01_single-ocaml-setup-in-ci.md)

## What Is Already Planned

These areas already have a concrete plan or completed milestone and should not also live here as loose deferred bullets:

- declaration-role split (`type` / `shape` / `trait`), transparent vs constructor-bearing type forms, effect freeze, and unions vs named sums:
  [Pre-modules semantics foundation](docs/plans/done/language/04_pre-modules-semantics-foundation.md)
- feature/codegen parity, checker-to-emitter contract hardening, docs validation, generated-Go snapshots, and regression gates:
  [Pre-modules parity and hardening](docs/plans/done/language/05_pre-modules-parity-and-hardening.md)
- imports, exports, per-module checking, and multi-file compilation:
  [Module system](docs/plans/done/language/06_module-system.md)
- core language/library platform milestones:
  [Prelude](docs/plans/todo/language/02_prelude.md),
  [FFI](docs/plans/todo/language/03_ffi.md),
  [Stdlib](docs/plans/todo/language/04_stdlib.md)
- associated types, open rows, post-module coherence work, and later lowering expansion:
  [Post-modules type-system expansion](docs/plans/todo/language/05_post-modules-type-system-expansion.md)
- later planned language work on the stabilized explicit-qualified call model:
  [Function overloading](docs/plans/todo/language/06_function-overloading.md),
  [Universal constraints and interface value types](docs/plans/todo/language/07_forall-exists.md)
- post-stdlib backend optimization work:
  [Higher-order function optimization](docs/plans/todo/language/08_hof_optimization.md)
- current planned tooling / CI work:
  [OCaml test harness rewrite](docs/plans/todo/tooling/01_ocaml-test-harness.md),
  [Single OCaml setup in CI](docs/plans/todo/ci/01_single-ocaml-setup-in-ci.md)

## Deferred Ideas Without A Concrete Plan

### Language Ergonomics And Syntax

- optional record field syntax (`?`)
- let-destructuring syntax
- pattern guards
- tuple patterns, if tuples are added
- array/list patterns
- extra match ergonomics and constructor-polish work
- postponed syntax sugar or future operators that are still too vague for a dedicated plan

### Type-System And Semantics

- canonical field-order policy for hashing, derives, and debug output
- polymorphic literal policy (`1 + 1.0` style questions)
- function-trait policy beyond v1 (`eq` / `ord` / `hash` for functions)
- rank-N polymorphism, HKTs, and dependent-type exploration

### Backend And Performance

- spread/update optimization beyond copy semantics
- escape-analysis-guided lowering and structural-shape canonicalization
- named-sum layout tuning and constructor/match micro-optimizations
- optimized union representations and narrowing codegen
- closure lowering with explicit environment structs
- specialization deduping and explosion-analysis tooling
- selective Go-generics backend experiments
- mid-level IR work only if workload pressure justifies it

### Tooling And Process

- stress/performance benchmark suite with reproducible baselines
- any future documentation/process requirements beyond the currently planned validation gates

### Long-Range Explorations

- concurrency model exploration (channels/goroutines vs effect-based model)
