# Type Annotations And Named Types

## Maintenance

- Last verified: 2026-03-28
- Implementation status: Canonical (actively maintained)
- Update trigger: Any parser, typechecker, or codegen change affecting annotations or `type`

## Scope

This doc covers:

- optional annotations on lets, params, and returns,
- generic parameters and constraint syntax,
- transparent naming via plain `type`,
- constructor-bearing nominal wrappers and sums.

## Status Note

The language is being migrated away from the separate `alias` keyword.
The target surface is:

- `type Name = TypeExpr` for transparent names,
- `type Name = Name(Payload)` for nominal wrappers,
- constructor-bearing `type` forms for sums.

## Syntax

```marmoset
let x: Int = 5

fn add(a: Int, b: Int) -> Int = a + b

fn id[a](x: a) -> a = x
fn show_eq[a: Show & Eq](x: a) -> Str = x.show()

type Point = { x: Int, y: Int }
type Reducer[a] = (a, a) -> a

type UserId = UserId(Int)

let p: Point = { x: 1, y: 2 }
let id = UserId(42)
```

## Semantics

- Annotations are checked against inferred types.
- Plain `type Name = TypeExpr` is transparent.
- Constructor-bearing `type` forms introduce nominal identity.
- Exact structural record names do not create a distinct product identity by themselves.
- Trait impls, inherent impls, and derives for exact records attach to the exact structural type named by `type`.

## When To Use Which

Use plain `type Name = TypeExpr` when you want:
- a reusable structural record name,
- a shorthand for a function type,
- a transparent name for an existing type expression.

Use constructor-bearing `type` forms when you want:
- nominal identity,
- derives,
- inherent methods,
- trait impls,
- explicit construction at the call site.

Style convention:
- Prefer plain `type` for transparent helper names such as `Point`, `Reducer`, or other purely structural views.
- Prefer constructor-bearing `type` forms for domain entities and opaque values that need nominal meaning, such as `User`, `Monkey`, `UserId`, or `BananaPile`.

## Codegen

- Annotations are compile-time only.
- Transparent `type` names do not create a distinct runtime type.
- Constructor-bearing wrappers and sums emit nominal runtime representations.

## Related Docs

- `docs/features/records.md`
- `docs/features/traits.md`
- `docs/features/inherent-methods.md`

## Implementation Touchpoints

- Surface parsing: `lib/frontend/syntax/parser.ml`
- Lowering: `lib/frontend/syntax/lower.ml`
- Annotation conversion: `lib/frontend/typecheck/annotation.ml`
- Named-type registry: `lib/frontend/typecheck/type_registry.ml`
- Construction/type inference: `lib/frontend/typecheck/infer.ml`
- Go emission: `lib/backend/go/emitter.ml`
