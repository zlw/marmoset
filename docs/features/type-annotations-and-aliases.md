# Type Annotations, Aliases, And Named Types

## Maintenance

- Last verified: 2026-03-28
- Implementation status: Canonical (actively maintained)
- Update trigger: Any parser, typechecker, or codegen change affecting annotations, `alias`, or `type`

## Scope

This doc covers:

- optional annotations on lets, params, and returns,
- generic parameters and constraint syntax,
- transparent aliases via `alias`,
- nominal named types via `type`.

## Syntax

```marmoset
let x: Int = 5

fn add(a: Int, b: Int) -> Int = a + b

fn id[a](x: a) -> a = x
fn show_eq[a: Show & Eq](x: a) -> Str = x.show()

alias Point = { x: Int, y: Int }
alias Reducer[a] = (a, a) -> a

type UserId = Int
type Box[a] = { value: a }

let p: Point = { x: 1, y: 2 }
let id = UserId(42)
let s = Box(value: "ok")
```

## Semantics

- Annotations are checked against inferred types.
- `alias` is transparent and behaviorless.
- `type` introduces nominal identity.
- Named product and wrapper values require explicit construction.
- Trait impls, inherent impls, and derives attach to `type`, not `alias`.

## When To Use Which

Use `alias` when you want:
- a reusable structural record name,
- a shorthand for a function type,
- a transparent name for an existing type expression.

Use `type` when you want:
- nominal identity,
- derives,
- inherent methods,
- trait impls,
- explicit construction at the call site.

Style convention:
- Prefer `alias` for transparent helper names such as `Point`, `Reducer`, or other purely structural views.
- Prefer `type` for domain entities and values that own behavior or nominal meaning, such as `User`, `Monkey`, or `BananaPile`.

## Codegen

- Annotations are compile-time only.
- `alias` does not create a distinct runtime type.
- `type` emits a distinct runtime representation:
  - named products become distinct Go structs plus constructor calls,
  - named wrappers become distinct Go named types plus wrapper constructors.

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
