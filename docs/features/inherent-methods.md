# Inherent Methods

## Maintenance

- Last verified: 2026-03-08
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
type Point = { x: Int, y: Int }

impl Point = {
  fn sum(p: Point) -> Int = p.x + p.y
  fn translate(p: Point, dx: Int, dy: Int) -> Point = {
    { x: p.x + dx, y: p.y + dy }
  }
}

let p: Point = { x: 1, y: 2 }
puts(p.sum())
```

### Method generics

Inherent methods can declare method-level generic parameters:

```marmoset
type Box = { v: Int }

impl Box = {
  fn cast[b](bx: Box, f: (Int) -> b) -> b = {
    f(bx.v)
  }
}

let b: Box = { v: 5 }
let s = b.cast[Str]((n: Int) -> n.show())
```

### Effect markers

Inherent methods use `->` for pure and `=>` for effectful:

```marmoset
impl Logger = {
  fn format(l: Logger, msg: Str) -> Str = l.prefix + ": " + msg
  fn log(l: Logger, msg: Str) => Str = l.prefix + ": " + msg
}
```

Omitting the effect marker infers effectfulness from the body.

### Multi-statement method bodies

Method bodies support let bindings and multiple statements. The last expression is the return value:

```marmoset
impl Acc = {
  fn add_and_show(a: Acc, n: Int) -> Str = {
    let new_total = a.total + n
    let result = "total:" + new_total.show()
    result
  }
}
```

### Recursive inherent methods

Inherent methods can call themselves on other instances of the same type:

```marmoset
impl Counter = {
  fn count_down(c: Counter) -> Str = {
    if (c.n <= 0) { "done" }
    else {
      let next: Counter = { n: c.n - 1 }
      next.count_down()
    }
  }
}
```

Trait impl syntax remains separate:

```marmoset
impl Show[Point] = {
  fn show(p: Point) -> Str = "..."
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
trait Renderable[a] = { fn render(a) -> Str }

type Widget = { id: Int }
impl Widget = { fn render(w: Widget) -> Str = "widget" }

fn use[t: Renderable](x: t) -> Str = x.render()
```

## Method Resolution Rules

For `receiver.method(args...)`:
1. If receiver is a bound variable, resolve as value-dot method call with inherent-first priority.
2. If receiver is an enum type identifier and method is a known variant, resolve as enum constructor call.
3. If receiver is an enum type identifier but method is not a variant, resolve as qualified inherent call on the enum type (or produce unknown-variant diagnostic).
4. If receiver is a type name (primitive, alias, or enum), resolve as qualified inherent call (`Type.method(x)`).
5. If receiver is a trait name, resolve as qualified trait call (`Trait.method(x)`).
6. If receiver type is a constrained type variable, resolve from trait constraints only (inherent methods do not participate).
7. Otherwise resolve inherent methods first, then trait methods for concrete receiver type.

Bound variables take precedence over enum type names in dotted access. This means a local `let color = { red: "x" }` shadows the enum `color` for `color.red`. Inherent methods take precedence over trait methods on dot calls. Qualified trait calls (`Trait.method(x)`) bypass this precedence and select the named trait directly.

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
- Integration coverage: `test/fixtures/traits/`, `test/fixtures/traits_inherent/`, `test/fixtures/function_model/`
