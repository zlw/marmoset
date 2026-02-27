# Marmoset Roadmap (Deferred / Future Work)

## Maintenance

- Last verified: 2026-02-27
- Implementation status: Canonical (actively maintained)
- Update trigger: Any language behavior, typechecker, or codegen change affecting this topic

This roadmap tracks deferred decisions and future improvements from current canonical docs and implementation constraints.

## 1. Language and Type-System Enhancements

### 1.1 Records and Rows

- Optional field syntax (`?`) for records (currently `option[T]` workaround).
- Canonical field-order policy for hashing/derives/debug printing.
- Nominal wrapper/newtype capability over structural records.
- Let-destructuring syntax (current recommendation is `match` destructuring).
- Stronger recursive row-polymorphism diagnostics and edge-case handling.
- Spread/update optimization model beyond simple copy semantics.

### 1.2 Traits

- Inherent methods (`impl <type> { ... }`) without trait boilerplate.
- Associated types.
- Default methods and richer override semantics.
- More complete conditional impl resolution and overlap diagnostics.
- Dynamic dispatch mode (explicit trait objects / existentials), if needed.
- First-class existential representation design (`type_id` + payload + witness), before any method/mixed trait-object surface feature.
- Lock capability semantics for existential values (`eq`/`ord`/`hash`/`show`) before enabling dynamic dispatch.
- Better coherence/orphan policy and warnings.
- Potential polymorphic literal behavior policy (`1 + 1.0` style concerns).
- Higher-kinded type design exploration for advanced abstractions.

### 1.3 Enums and Pattern Matching

- Pattern guards.
- Multi-pattern arm sugar improvements.
- Tuple patterns (if tuples are added).
- More exhaustive-check depth for complex nested unions/records.
- Array/list patterns (explicitly deferred in phase docs).
- Constructor ergonomics polish and nested pattern diagnostics.

### 1.4 Unions

- Optimize representation beyond `interface{}` where possible.
- Improve narrowing precision in deeply nested control flow.
- Potential tagged representation for selected unions.
- Variance-aware union compatibility for function positions.
- Better branch-complement narrowing in complex boolean flow.
- Optional specialized codegen when concrete union alternative is statically known.

### 1.5 Functions / Polymorphism

- Implement lambda-lifted closure lowering with explicit environment structs.
- Complete polymorphic function-value support under rank-1 HM.
- Ensure function identifiers used as values lower to specialized symbols deterministically.
- Implement typed empty collection literal emission in all expected-type codegen paths.
- Potential selective use of Go generics as backend strategy toggles.
- Specialization deduping and explosion-control heuristics.
- True rank-N polymorphism (`forall` in function-typed positions) is deferred.
- Function trait policy extension beyond v1 (`eq`/`ord`/`hash` for functions) is deferred.

### 1.6 Effects / Error model

- Effect annotation semantics (`=>`) beyond parsing-level ideas.
- Error propagation ergonomics once effect model is formalized.

## 2. Backend and Performance Roadmap

### 2.1 Record performance

- Spread/update optimization (copy elision / uniqueness-style analysis).
- Escape analysis-guided lowering to reduce allocations.
- Shape canonicalization strategy for repeated structural records.

### 2.2 Enum performance

- Layout tuning and cross-variant packing improvements.
- Constructor/match codegen micro-optimizations.
- Evaluate memory overhead trade-offs for heterogeneous multi-field variants.

### 2.3 Trait call performance

- Improve call-site inlining opportunities in generated Go.
- Avoid unnecessary helper indirection for simple builtins.
- Explicit static-vs-dynamic dispatch selection model (if trait objects land).
- Benchmark dynamic dispatch representation options (interface-only vs dictionary vs tagged existential package) before locking backend strategy.

### 2.4 Union performance

- Reduce boxing/alloc costs in hot paths.
- Optimize narrowing/type-switch patterns.
- Optional generated wrapper representation for selected hot unions.

### 2.5 Monomorphization strategy

- Better specialization for HOF patterns.
- Deduplicate equivalent specializations more aggressively.
- Optional instrumentation to analyze specialization explosion.
- Revisit type-map plumbing hardening where backend type facts are incomplete.

### 2.6 Backend architecture evolution

- Add mid-level IR only when optimization pressure justifies added complexity.
- Keep Go source emission as baseline until IR-backed transforms show clear benefit.
- Preserve generated-source readability as a non-functional requirement.

## 3. Architecture Improvements

- Introduce explicit mid-level IR only when optimization requirements justify complexity.
- Keep stage boundaries strict (parser/typechecker/codegen) to improve maintainability.
- Formalize backend capability matrix (what each construct guarantees at codegen time).
- Continue moving backend decisions from ad-hoc emitter logic to typed metadata contracts.
- Evaluate whole-program assumptions and future module-aware compilation boundaries.

### 3.1 Locked near-term modules/FFI policy

These guardrails are intentionally locked until a dedicated modules+extern design pass lands:
- Backend remains one Go package per build.
- Extern ABI starts with primitives/unit only.
- Records/enums/unions/trait objects stay out of ABI until representation and ownership rules are frozen.
- Trait-object representation is treated as deferred design work, not a soft-inferred backend feature.
- Any future trait-object ABI must use an explicit stable representation; raw Go interface internals are not the public contract.

## 4. Tooling and Testing

- Expand system/e2e test coverage beyond current integration shell suites.
- Add stress/performance benchmark suite with reproducible baselines.
- Add docs validation checks (staleness markers, link checks, canonical-source checks).
- Add regression suites for trait solver overlap and record/row unification edge cases.
- Add generated-Go snapshot tests for representative feature combinations.
- Consolidate repetitive shell test setup helpers and keep one canonical integration harness.

## 5. Documentation Process Improvements

- Every canonical doc should include:
  - `Last verified` date,
  - `Implementation status`,
  - explicit `Chosen vs Alternatives` sections.
- Keep docs focused on current behavior and near-term design decisions.
- Require roadmap entry when a design decision is explicitly deferred.

## 6. Syntax and Surface-Language Follow-ups

- Keep explicit decision records for any syntax-level change proposals.
- Revisit postponed syntax sugar only after core type/codegen stability:
  - postfix return syntax variants,
  - extra match ergonomics,
  - optional future operators that were deferred.
- Keep editor/highlighting support aligned with final syntax choices.

## 7. Out-of-Scope but Preserved Explorations

- Concurrency model exploration (channels/goroutines vs effect-based model) is intentionally not in active implementation scope.
- Modules-system exploratory work should be reintroduced only with a dedicated design pass.
