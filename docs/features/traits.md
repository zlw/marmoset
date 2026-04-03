# Traits

## Maintenance

- Last verified: 2026-04-03
- Implementation status: Canonical (actively maintained)
- Update trigger: Any parser, typechecker, solver, or codegen change affecting traits, shapes, derives, or method resolution

## Scope

Traits provide:

- nominal method capabilities via explicit impls,
- superconstraints across traits and shapes,
- constrained generics,
- explicit trait-qualified calls,
- explicit trait objects via `Dyn[...]`.

Structural field constraints are handled by `shape`, not `trait`.

## Syntax

### Shapes

```marmoset
shape Named = {
  name: Str
}
```

### Method-only traits

```marmoset
trait Show[a] = {
  fn show(a) -> Str
}
```

### Traits with superconstraints

```marmoset
shape PersonShape = {
  name: Str
}

trait Greeter[a]: Show & PersonShape = {
  fn greet(self: a) -> Str = "hello, " + self.name
}
```

### Impl

```marmoset
type Person = { name: Str } derive Show

impl Greeter[Person] = {
  override fn greet(self: Person) -> Str = "hello, " + self.name
}
```

## Semantics

- Trait bodies are method-only.
- Shapes carry structural record requirements.
- Trait satisfaction is nominal and comes from explicit impls or builtins.
- Shape satisfaction is structural and checked from available fields.
- A trait may depend on both traits and shapes through its superconstraint list.
- Constrained-param shorthand works with either:
  - `fn render(x: Show) -> Str = Show.show(x)`
  - `fn name_of(x: Named) -> Str = x.name`

## Derive

Built-in derives remain specialized for `Eq`, `Show`, `Debug`, `Ord`, and `Hash`.

User-trait derive is default-backed only:
- every required trait method must have a default body after substitution,
- all superconstraints must already be satisfied or be included in the same derive closure when applicable,
- shapes are not derivable because they are structural constraints, not impl-backed behaviors.

## Method Resolution

For dotted access:

1. `x.f` means field access or direct interface field projection.
2. `x.f(...)` means callable-field invocation only.
3. Dot never searches trait impls, inherent impls, or free functions for behavior.
4. Qualified trait calls (`Trait.method(x, ...)`) select trait impls explicitly, including `Dyn[...]` receivers.
5. Qualified inherent calls (`Type.method(x, ...)`) select exact-type grouped functions explicitly.
6. Shapes do not contribute methods.

Field access (`x.name`) is separate from trait qualification (`Show.show(x)`) and exact-type qualification (`Point.sum(x)`).

## Trait Objects

Explicit trait objects use `Dyn[...]`:

```marmoset
let value: Dyn[Show] = 42
let values: List[Dyn[Show]] = [42, "hello", true]
```

Rules:

- `Dyn[...]` accepts trait constraints only.
- Shapes are compile-time structural constraints and are not valid inside `Dyn[...]`.
- Concrete values coerce to `Dyn[...]` at assignment, argument, and return sites.
- Dynamic dispatch uses the explicit witness-carrying runtime package emitted by the Go backend.

## Related Docs

- `docs/features/type-annotations-and-aliases.md`
- `docs/features/inherent-methods.md`
- `docs/features/records.md`

## Implementation Touchpoints

- Surface parsing: `lib/frontend/syntax/parser.ml`
- Lowering: `lib/frontend/syntax/lower.ml`
- Trait registry and impl validation: `lib/frontend/typecheck/trait_registry.ml`
- Trait solving: `lib/frontend/typecheck/trait_solver.ml`
- Shape and named-type registry: `lib/frontend/typecheck/type_registry.ml`
- Method resolution and inference: `lib/frontend/typecheck/infer.ml`
- Go emission: `lib/backend/go/emitter.ml`
