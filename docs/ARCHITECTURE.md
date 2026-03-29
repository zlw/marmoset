# Marmoset Architecture

## Maintenance

- Last verified: 2026-03-10
- Implementation status: Canonical (actively maintained)
- Update trigger: Any language behavior, typechecker, or codegen change affecting this topic

This document describes the current high-level architecture and the design choices behind it.

## 1. End-to-End Flow

1. Source text -> Lexer (`lib/frontend/syntax/lexer.ml`)
2. Tokens -> Surface parser (`lib/frontend/syntax/parser.ml`, `parse_surface`) -> `Surface_ast`
3. `Surface_ast` -> Lowering/canonicalization (`lib/frontend/syntax/lower.ml`) -> canonical `AST.program`
4. Canonical AST -> Type annotation conversion + inference + checking (`lib/frontend/typecheck/**`)
5. Typed canonical AST + type map -> Go emitter (`lib/backend/go/emitter.ml`)
6. Generated Go -> `go build` -> native binary

The architecture is intentionally split into a surface frontend, a canonical typed core, and a backend code generator. The typechecker, emitter, and LSP typed analysis consume only the canonical AST; surface-only syntax is erased at the lowering boundary.

## 2. Frontend Architecture

### 2.1 Lexer

Responsibilities:
- Tokenize the canonical vNext keywords/operators/literals (`fn`, `case`, `override`, `enum`, `trait`, `impl`, `derive`, `type`, `...`, `.`, `|`, `&`, `is`, `=>`, etc).
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

### 2.2 Surface Parser

Responsibilities:
- Build `Surface_ast`, not the canonical downstream AST.
- Parse vNext constructs including:
  - generic params and constraints (`fn[a: Show & Eq](...)`)
  - named types, aliases, and shapes (`type User = { ... }`, `alias Point = { x: Int }`, `shape Named = { name: Str }`)
  - unions (`Int | Str`)
  - enums/constructors/match patterns with `case`
  - records, spread, row-variable type forms (`{ x: Int, ...r }`)
  - trait/impl/derive declarations, top-level `fn`, explicit lambdas, placeholder shorthand, and expression-position blocks
- Own brace disambiguation for record vs map vs block at parse time.

Alternatives considered:
- Pratt parser for expressions only + separate recursive parser for all type forms.
- Fully recursive-descent with precedence table.

Chosen:
- Recursive-descent with expression precedence table plus a dedicated `Surface_ast`.

Pros:
- Keeps surface-syntax/desugaring concerns out of the typechecker and emitter.
- Better control over recovery and domain-specific errors.

Cons:
- Parser state threading can be tedious for new constructs.
- Surface and canonical trees must stay intentionally distinct.

### 2.3 Lowering / Canonicalization

Responsibilities:
- Convert `Surface_ast` into the canonical `Syntax.Ast.AST` representation that downstream stages consume.
- Desugar surface-only forms such as:
  - top-level `fn` declarations into canonical `Let(Function)`,
  - postfix derive clauses into canonical derive nodes,
  - placeholder shorthand into explicit lambdas,
  - expression-position blocks into canonical `BlockExpr`,
  - impl headers and constrained-param shorthand into canonical typechecker-facing forms.
- Preserve stable source spans and expression IDs so diagnostics and type maps still point back to the original source.

Chosen:
- Keep lowering as the only frontend normalization boundary instead of teaching the rest of the compiler about surface syntax.

Pros:
- Resolver, typechecker, emitter, and LSP analysis all see one canonical language.
- Syntax work remains localized to the frontend.

Cons:
- Lowering owns several context-sensitive rewrites and therefore needs strong tests.

## 3. Typechecker Architecture

Main modules:
- `types.ml`: type representation + substitution + normalization.
- `unify.ml`: unification and compatibility rules.
- `annotation.ml`: AST type-expression -> internal mono type conversion + alias resolution.
- `infer.ml`: inference/checking (Algorithm W style with extensions).
- `checker.ml`: entry points and formatted diagnostics.
- `trait_registry.ml`, `trait_solver.ml`, `inherent_registry.ml`, `enum_registry.ml`, `exhaustiveness.ml`: feature-specific systems.

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
- In-process OCaml runtime backend (interpreter/VM).
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
2. Collect concrete instantiations from typed call sites and function-value flows.
3. Compute free-variable capture sets for nested functions.
4. Perform lambda lifting for nested functions with explicit environment parameters.
5. Emit specialized functions + trait helpers + enum helpers + main body.

Typed data dependency:
- Emitter uses type map from typechecker (no re-inference in codegen path).

Pros:
- Deterministic backend typing decisions.
- Avoids backend-specific inference drift.

Cons:
- Requires full and consistent type-map coverage.

Locked function/closure policy (2026-02-27):
- Rank-1 polymorphism with polymorphic function values.
- True rank-N polymorphism deferred.
- Closure lowering uses explicit environment structs plus lifted helpers.
- Function value traits: `eq`/`ord`/`hash` are disallowed; `show` is allowed via stable placeholder rendering.
- Empty collection literals must emit concrete typed forms when expected type is known.

### 4.3 Runtime representation strategy

- Primitives -> native Go primitives.
- Arrays -> typed slices.
- Maps/hashes -> typed maps.
- Unions -> `interface{}` + type assertions/switches.
- Enums -> tagged struct forms with generated constructors and match dispatch.
- Records -> named struct types via shape interning, with type alias support.
- Trait methods -> static free functions with mangled names.

### 4.4 Modules/FFI guardrail policy (current, binding)

Until module and extern features are fully designed:
- One Go package per build is the backend policy.
- Trait impl emission assumes single-package visibility; cross-package impl stitching is out of scope.
- No trait-object ABI is exposed across boundaries.

Initial extern ABI mapping constraints (when extern is enabled):
- Allowed first-wave value types: `Int`, `Float`, `Bool`, `Str`, `Unit`.
- Deferred until representation freeze: records, enums, unions, trait objects, and unconstrained polymorphic values.
- Deferred until ownership rules are explicit: arrays/maps with mutation/aliasing semantics across boundary.

Rationale:
- keeps codegen coherent while modules are introduced,
- avoids locking an unstable runtime layout into public ABI,
- reduces rewrite risk for future IR and dispatch changes.

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
- Integration shell tests (`test/integration/*.sh`) for parse+typecheck+codegen+runtime.
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
