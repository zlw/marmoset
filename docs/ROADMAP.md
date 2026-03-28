# Marmoset Roadmap

## Maintenance

- Last verified: 2026-03-28
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

### Tooling

1. [Diagnostics rework](docs/plans/done/tooling/01_diagnostics-rework.md)
2. [Fixture harness migration](docs/plans/done/tooling/02_fixture-harness-migration.md)

### CI

1. [GitHub workflow migration](docs/plans/done/ci/01_github-workflow.md)

## Ordered Active Roadmap

### Main Language Track

1. [Pre-modules semantics foundation](docs/plans/todo/language/01_pre-modules-semantics-foundation.md)
2. [Pre-modules parity and hardening](docs/plans/todo/language/02_pre-modules-parity-and-hardening.md)
3. [Module system](docs/plans/todo/language/03_module-system.md)
4. [Prelude](docs/plans/todo/language/04_prelude.md)
5. [FFI](docs/plans/todo/language/05_ffi.md)
6. [Stdlib](docs/plans/todo/language/06_stdlib.md)
7. [Post-modules type-system expansion](docs/plans/todo/language/07_post-modules-type-system-expansion.md)

### Parallel Tooling Track

- [OCaml test harness rewrite](docs/plans/todo/tooling/01_ocaml-test-harness.md)

### Parallel CI Track

- [Single OCaml setup in CI via prebuilt runner image](docs/plans/todo/ci/01_single-ocaml-setup-in-ci.md)

## What Is Already Planned

These areas already have a concrete plan and should not also live here as loose deferred bullets:

- declaration-role split (`type` / `alias` / `shape` / `trait`), named-type identity/construction, effect freeze, and unions vs named sums:
  [Pre-modules semantics foundation](docs/plans/todo/language/01_pre-modules-semantics-foundation.md)
- feature/codegen parity, checker-to-emitter contract hardening, docs validation, generated-Go snapshots, and regression gates:
  [Pre-modules parity and hardening](docs/plans/todo/language/02_pre-modules-parity-and-hardening.md)
- imports, exports, per-module checking, and multi-file compilation:
  [Module system](docs/plans/todo/language/03_module-system.md)
- core language/library platform milestones:
  [Prelude](docs/plans/todo/language/04_prelude.md),
  [FFI](docs/plans/todo/language/05_ffi.md),
  [Stdlib](docs/plans/todo/language/06_stdlib.md)
- associated types, open rows, post-module coherence work, and later lowering expansion:
  [Post-modules type-system expansion](docs/plans/todo/language/07_post-modules-type-system-expansion.md)
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
- enum layout tuning and constructor/match micro-optimizations
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
