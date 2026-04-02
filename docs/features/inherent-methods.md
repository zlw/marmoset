# Inherent Methods

## Maintenance

- Last verified: 2026-04-02
- Implementation status: Canonical (actively maintained)
- Update trigger: Any parser, typechecker, or codegen change affecting inherent impl parsing, method resolution, or lowering

## Scope

Inherent methods attach behavior directly to a target type without defining a trait.

Supported targets:

- primitives,
- exact structural types named with `type`,
- constructor-bearing nominal wrappers,
- constructor-bearing sums,
- concrete type applications.

## Status Note

The data-first rework changes the ownership model for inherent methods:

- `impl Type = { ... }` on an exact structural record is exact-type grouped function registration,
- constructor-bearing wrappers remain nominal and may still carry their own wrapper-side impls,
- wrapper payload access should prefer constructor-pattern matching; structural projection remains explicit and secondary.

## Syntax

```marmoset
type Point = { x: Int, y: Int }

impl Point = {
  fn sum(p: Point) -> Int = p.x + p.y
  fn translate(p: Point, dx: Int, dy: Int) -> Point =
    { ...p, x: p.x + dx, y: p.y + dy }
}

let p: Point = { x: 1, y: 2 }
puts(Point.sum(p))
```

### Method generics

```marmoset
type Box = { v: Int }

impl Box = {
  fn cast[b](bx: Box, f: (Int) -> b) -> b = f(bx.v)
}

let b: Box = { v: 5 }
let s = Box.cast[Str](b, (n: Int) -> Show.show(n))
```

### Recursive inherent methods

```marmoset
type Counter = { n: Int }

impl Counter = {
  fn count_down(c: Counter) -> Str = {
    if (c.n <= 0) {
      "done"
    } else {
      let next: Counter = { n: c.n - 1 }
      Counter.count_down(next)
    }
  }
}
```

## Semantics

- The first parameter is the receiver.
- The receiver type must match the impl target.
- Method annotations may be omitted when inferable.
- Exact structural record returns use plain record syntax.
- Constructor-bearing wrappers use explicit constructors and constructor-aware rebuilds.
- Inherent methods register on `(exact_receiver_type, method_name)`.

## Trait Interaction

Hard rule:

- inherent methods never satisfy trait constraints.

So this still requires an explicit trait impl:

```marmoset
trait Renderable[a] = { fn render(a) -> Str }

type Widget = { id: Int }
impl Widget = { fn render(w: Widget) -> Str = "widget" }

fn use[t: Renderable](x: t) -> Str = Renderable.render(x)
```

## Resolution

- `Type.method(x, ...)` is the canonical inherent-call surface.
- Concrete `x.f(...)` does not search inherent methods, trait impls, or top-level functions.
- Value dot stays literal: `x.f` is field access or direct interface field projection, and `x.f(...)` is callable-field invocation only.
- Trait-constrained values and `Dyn[...]` values use explicit qualified calls such as `Renderable.render(x)`.

## Codegen

Inherent calls lower to static helper functions selected from typechecker metadata.

Examples:

- `Point.sum(p)` -> `inherent_sum_<type_suffix>(p)`
- `Point.translate(p, 1, 2)` -> the same inherent helper with explicit qualification

Field-function fallback remains separate from inherent dispatch and continues to work on structural records when the selected member is a function-valued field.

## Related Docs

- `docs/features/traits.md`
- `docs/features/type-annotations-and-aliases.md`
- `docs/features/records.md`

## Implementation Touchpoints

- Surface parsing: `lib/frontend/syntax/parser.ml`
- Lowering: `lib/frontend/syntax/lower.ml`
- Inherent registry: `lib/frontend/typecheck/inherent_registry.ml`
- Method resolution and collision checks: `lib/frontend/typecheck/infer.ml`, `lib/frontend/typecheck/trait_registry.ml`
- Go emission: `lib/backend/go/emitter.ml`
