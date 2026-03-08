# Inherent Methods

## Maintenance

- Last verified: 2026-03-07
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
- parameter type annotations are optional (inferred from context when omitted),
- return type annotation is optional (inferred from body when omitted),
- method body is inferred and checked against declared return type (if present).

### Registration and Coherence

Inherent methods are stored by canonical `(receiver_type, method_name)`.

Rules:
- duplicate inherent methods for the same `(type, method_name)` are rejected,
- inherent methods can coexist with trait methods for the same `(type, method_name)`,
- for dot calls, inherent methods take precedence over trait methods,
- qualified calls (`Trait.method(x)`) can disambiguate when both exist.

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
1. If receiver is an enum type identifier, parse as enum constructor call.
2. If receiver is a bound variable, resolve as value-dot method call with inherent-first priority.
3. If receiver is a type name, resolve as qualified inherent call (`Type.method(x)`).
4. If receiver type is a constrained type variable, resolve from trait constraints only.
5. Otherwise resolve inherent methods first, then trait methods for concrete receiver type.

Inherent methods take precedence over trait methods on dot calls. Qualified trait calls (`Trait.method(x)`) bypass this precedence and select the named trait directly.

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

- Inherent method signatures support both `->` (pure) and `=>` (effectful) arrow syntax.
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
- Integration coverage: `test/fixtures/traits/`, `test/fixtures/traits_inherent/`
