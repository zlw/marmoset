# Marmoset Architecture

## Maintenance

- Last verified: 2026-02-06
- Implementation status: Canonical (actively maintained)
- Update trigger: Any language behavior, typechecker, or codegen change affecting this topic

This document describes the current high-level architecture and the design choices behind it.

## 1. End-to-End Flow

1. Source text -> Lexer (`lib/frontend/syntax/lexer.ml`)
2. Tokens -> Parser (`lib/frontend/syntax/parser.ml`)
3. AST -> Type annotation conversion + inference + checking (`lib/frontend/typecheck/**`)
4. Typed AST + type map -> Go emitter (`lib/backend/go/emitter.ml`)
5. Generated Go -> `go build` -> native binary

The architecture is intentionally split into a pure frontend (syntax + typing) and a backend code generator.

## 2. Frontend Architecture

### 2.1 Lexer

Responsibilities:
- Tokenize keywords/operators/literals (`fn`, `enum`, `trait`, `impl`, `derive`, `type`, `...`, `.`, `|`, `is`, etc).
- Preserve source offsets for diagnostics.

Alternatives considered:
- Minimal token set + parser-level heuristics.
- Rich token set with explicit operators/keywords.

Chosen:
- Rich token set. It simplifies parser logic for advanced syntax and keeps diagnostics precise.

Pros:
- Cleaner parser states.
- Better error messages.

Cons:
- More lexer maintenance as syntax evolves.

### 2.2 Parser

Responsibilities:
- Build AST for expressions/statements/types.
- Parse advanced constructs:
  - generic params and constraints (`fn[a: show + eq](...)`)
  - type aliases (`type point = { x: int }`)
  - unions (`int | string`)
  - enums/constructors/match patterns
  - records, spread, row-variable type forms (`{ x: int, ...r }`)
  - traits/impl/derive declarations

Alternatives considered:
- Pratt parser for expressions only + separate recursive parser for all type forms.
- Fully recursive-descent with precedence table.

Chosen:
- Recursive-descent with expression precedence table.

Pros:
- Easy to evolve for language-specific syntax.
- Better control over recovery and domain-specific errors.

Cons:
- Parser state threading can be tedious for new constructs.

## 3. Typechecker Architecture

Main modules:
- `types.ml`: type representation + substitution + normalization.
- `unify.ml`: unification and compatibility rules.
- `annotation.ml`: AST type-expression -> internal mono type conversion + alias resolution.
- `infer.ml`: inference/checking (Algorithm W style with extensions).
- `checker.ml`: entry points and formatted diagnostics.
- `trait_registry.ml`, `trait_solver.ml`, `enum_registry.ml`, `exhaustiveness.ml`: feature-specific systems.

### 3.1 Core Representation

Key types in `types.ml`:
- primitives (`TInt`, `TFloat`, `TBool`, `TString`, `TNull`)
- composite (`TArray`, `THash`, `TFun`)
- advanced (`TUnion`, `TEnum`, `TRecord`, `TRowVar`)
- polymorphism (`TVar`, `Forall`)

Chosen style:
- HM-style type variables + substitutions + let-generalization.

Pros:
- Predictable inference model.
- Strong baseline for adding constrained polymorphism.

Cons:
- Advanced features need explicit extensions (unions, rows, traits) beyond plain HM.

### 3.2 Constraint Solving / Inference

Responsibilities:
- Infer expression types.
- Enforce annotation compatibility.
- Resolve trait-method calls by receiver type.
- Narrow unions via `is` checks.
- Validate match exhaustiveness for supported scrutinee classes.

Alternatives considered:
- Bidirectional checker first, with less inference.
- Full HM first, then additive features.

Chosen:
- Full HM baseline with incremental feature-specific checks.

Pros:
- Strong inference ergonomics.
- Incremental feature delivery without rewriting core.

Cons:
- Some advanced constraints are distributed across specialized modules.

## 4. Backend (Go) Architecture

### 4.1 Why Go source emission

Alternatives considered:
- Direct bytecode backend only.
- LLVM/MLIR-style IR and native backend.
- Go source generation.

Chosen:
- Go source generation + `go build`.

Pros:
- Fast implementation velocity.
- Inspectable generated source.
- Mature runtime/toolchain.

Cons:
- Dependent on Go toolchain behavior.
- Some source-level workarounds needed for expression semantics.

### 4.2 Emitter pipeline

Emitter phases:
1. Collect function definitions.
2. Collect concrete instantiations from typed call sites.
3. Emit specialized functions + trait helpers + enum helpers + main body.

Typed data dependency:
- Emitter uses type map from typechecker (no re-inference in codegen path).

Pros:
- Deterministic backend typing decisions.
- Avoids backend-specific inference drift.

Cons:
- Requires full and consistent type-map coverage.

### 4.3 Runtime representation strategy

- Primitives -> native Go primitives.
- Arrays -> typed slices.
- Maps/hashes -> typed maps.
- Unions -> `interface{}` + type assertions/switches.
- Enums -> tagged struct forms with generated constructors and match dispatch.
- Records -> struct-shaped values with field access/update lowering.
- Trait methods -> static free functions with mangled names.

## 5. Error Architecture

- Parser errors: token-level syntax errors.
- Type errors: typed diagnostics with source positions and context.
- Codegen failures: explicit emitter errors when unsupported states are reached.

Design goals:
- keep stage boundaries clear;
- avoid ambiguous mixed-stage failures.

## 6. Testing Architecture

Three layers:
- Inline unit tests in parser/typechecker/emitter modules.
- Integration shell tests (`test/test_typecheck_and_codegen*.sh`) for parse+typecheck+codegen+runtime.
- Future: larger system/e2e suites (see roadmap).

Why this split:
- fast feedback in unit tests;
- high confidence from end-to-end checks.

## 7. Rejected or Deferred Architectural Directions

- Monolithic "single pass" compile+infer+emit path (hard to debug and evolve).
- Full trait-object dynamic dispatch as default (kept static-first for performance/predictability).
- Mandatory user annotations everywhere (hurts ergonomics).
- Full custom IR layer right now (deferred until optimization pressure justifies it).

## 8. Current Authoritative References

- `docs/INDEX.md`
- `docs/ARCHITECTURE.md`
- Feature docs under `docs/features/`
- `docs/ROADMAP.md`

Historical references remain in `docs/archive/`.
