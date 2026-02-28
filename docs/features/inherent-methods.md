# Inherent Methods

## Maintenance

- Last verified: 2026-02-28
- Implementation status: Canonical (actively maintained)
- Update trigger: Any parser/typechecker/codegen/test change affecting inherent impl parsing, method resolution, or lowering

## Scope

Inherent methods attach behavior directly to a target type without defining a trait.

Capabilities:
- inherent impl blocks (`impl <type> { ... }`),
- method-call syntax (`x.m(...)`) with static dispatch,
- support for aliases, primitives, records, enums, and concrete type applications,
- coexistence with trait methods under explicit collision rules.

Non-goals in v1:
- dynamic dispatch,
- module visibility policy,
- using inherent methods as trait evidence.

## Syntax

```marmoset
type point = { x: int, y: int }

impl point {
  fn sum(p: point) -> int { p.x + p.y }
  fn translate(p: point, dx: int, dy: int) -> point {
    { x: p.x + dx, y: p.y + dy }
  }
}

let p: point = { x: 1, y: 2 }
puts(p.sum())
```

Trait impl syntax remains separate:

```marmoset
impl show for point {
  fn show(p: point) -> string { "..." }
}
```

## Type-System Semantics

### Method Signature Rules

For each inherent method:
- at least one parameter is required (receiver parameter),
- receiver parameter type must unify with the impl target type,
- every method parameter must have a type annotation,
- method return type annotation is required,
- method body is inferred and checked against declared return type.

### Registration and Coherence

Inherent methods are stored by canonical `(receiver_type, method_name)`.

Rules:
- duplicate inherent methods for the same `(type, method_name)` are rejected,
- collisions with trait-provided methods for the same `(type, method_name)` are rejected,
- collisions include builtin trait methods (for example, defining inherent `show` on `int` is rejected).

### Trait Interaction

Hard rule:
- inherent methods never satisfy trait constraints.

So this fails unless an explicit trait impl exists:

```marmoset
trait renderable[a] { fn render(x: a) -> string }

type widget = { id: int }
impl widget { fn render(w: widget) -> string { "widget" } }

let use = fn[t: renderable](x: t) -> string { x.render() }
```

## Method Resolution Rules

For `receiver.method(args...)`:
1. If parsed as enum constructor form, treat it as constructor call.
2. If receiver is a constrained type variable, resolve from trait constraints.
3. Otherwise resolve trait methods for concrete receiver type.
4. If no trait method is found, try inherent method lookup.
5. If both trait and inherent candidates exist, raise explicit ambiguity/collision error.

This keeps method dispatch deterministic and avoids silent precedence surprises.

## Codegen: Detailed Design

### Dispatch Model (Chosen)

Inherent calls lower to static helper functions:
- `x.sum()` -> `inherent_sum_<type_suffix>(x)`

The emitter uses method-resolution metadata from typechecking (`expr.id -> TraitMethod | InherentMethod`) and does not re-resolve method source in codegen.

Why this choice:
- consistent with trait helper lowering,
- works uniformly across primitives, aliases, records, and enums,
- avoids runtime/vtable overhead.

## Current Limitations

- Inherent method signatures only support `->` (pure arrow) in parser; `=>` is rejected.
- Generic inherent impl targets are supported when type parameters appear in the impl target (for example `impl result[a, b] { ... }`).
- Inherent generic targets currently do not have explicit per-impl constraint syntax (unlike trait generic impls).
- Generic inherent method call sites still require receiver type arguments to be concretely determined by program context before codegen specialization.
- Receiver shorthand (`self`) is not a dedicated syntax form; receiver is an explicit first parameter.
- No module-scoped visibility/override policy yet (single-program scope today).
- Integer literal dot-call parsing can still be ambiguous in some lexer edge cases; use `(5).method()` when needed.

## Related Docs

- `docs/features/traits.md`
- `docs/features/functions-and-polymorphism.md`
- `docs/ARCHITECTURE.md`
- `docs/ROADMAP.md`

## Implementation Touchpoints

- Inherent impl parsing: `lib/frontend/syntax/parser.ml`
- AST node (`InherentImplDef`): `lib/frontend/syntax/ast.ml`
- Inherent registry: `lib/frontend/typecheck/inherent_registry.ml`
- Method-call resolution and collision checks: `lib/frontend/typecheck/infer.ml`, `lib/frontend/typecheck/trait_registry.ml`
- Inherent helper emission and method-call lowering: `lib/backend/go/emitter.ml`
- Integration coverage: `test/integration/04_traits.sh`, `test/integration/04_traits_inherent.sh`
