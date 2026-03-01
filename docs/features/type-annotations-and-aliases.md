# Type Annotations and Type Aliases

## Maintenance

- Last verified: 2026-02-28
- Implementation status: Canonical (actively maintained)
- Update trigger: Any language behavior, typechecker, or codegen change affecting this topic

## Scope

This doc covers:

- optional annotations on lets/params/returns,
- generic parameters and constraints syntax,
- type aliases (`type name = ...`) including generic aliases,
- record aliases used as named product types.

## Syntax

```marmoset
let x: int = 5

let add = fn(a: int, b: int) -> int { a + b }

let id = fn[a](x: a) -> a { x }
let show_eq = fn[a: show + eq](x: a) -> string { x.show() }

type point = { x: int, y: int }
type box[a] = { value: a }

let p: point = { x: 1, y: 2 }
let s: box[string] = { value: "ok" }
```

## Sub-Features and Use Cases

- enforce expected boundary types,
- document intent for APIs,
- constrain polymorphism when inference is too broad,
- define reusable type-level names.

## Type-System Semantics

- annotations are checked against inferred types.
- aliases are resolved in annotation conversion.
- generic aliases instantiate with provided type arguments.

## Design Alternatives Considered

### Alternative A: Mandatory annotations everywhere

Pros:
- explicit and easy to read for large teams.

Cons:
- high verbosity,
- reduced HM ergonomics.

### Alternative B: Inference only, no annotations

Pros:
- concise syntax.

Cons:
- weak API boundaries,
- difficult inter-module contracts.

### Alternative C: Optional annotations + HM inference (Chosen)

Pros:
- ergonomic default,
- explicit where needed,
- strong compatibility checks.

Cons:
- dual style requires clear team conventions.

## Codegen: Detailed Design

Annotations and aliases are compile-time only. They do not emit direct runtime entities by default.

### Candidate Approaches

1. Erase annotations/aliases after typecheck (Chosen).
2. Preserve alias metadata in generated code comments.
3. Generate nominal wrappers for aliases by default.

### Approach 1 (Chosen)

Pros:
- zero runtime cost,
- no duplication with backend types.

Cons:
- alias names not present in runtime artifacts.

Pipeline details:

1. Parser records annotations and alias declarations in AST.
2. Annotation conversion resolves alias/type expressions to internal mono types.
3. Typechecker validates annotation compatibility with inferred types.
4. Emitter consumes resolved types from the type map; aliases do not emit runtime entities.

### Approach 3 (Nominal wrappers)

Pros:
- nominal distinction preserved at runtime.

Cons:
- overhead and semantic shift from structural aliasing.

## Why Current Choice

The language currently treats aliases as type-level convenience/abstraction rather than runtime nominal wrappers. This keeps codegen simple and fast.

## Pros and Cons of Current Model

Pros:
- clean integration with inference,
- minimal backend complexity,
- expressive generic APIs.

Cons:
- no runtime nominal identity for aliases,
- alias-heavy code can hide expanded structural complexity.

## Related Docs

- `docs/features/functions-and-polymorphism.md`
- `docs/features/records.md`

## Implementation Touchpoints

- Type-expression parsing and alias statements: `lib/frontend/syntax/parser.ml`
- Annotation conversion and alias registry: `lib/frontend/typecheck/annotation.ml`
- Annotation validation in inference/checker: `lib/frontend/typecheck/infer.ml`, `lib/frontend/typecheck/checker.ml`
