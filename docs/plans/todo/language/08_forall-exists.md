# Universal Constraints And Interface Value Types Plan

## Maintenance

- Last verified: 2026-04-02
- Implementation status: Planning
- Prerequisites:
  - current data-first declaration semantics
  - current trait/shape split
  - current monomorphizing frontend/backend pipeline
  - current Go backend with trait-object-style runtime packaging

## Summary

Marmoset already has a good static story:

- `shape` expresses open structural field constraints,
- `trait` expresses behavioral capabilities,
- constrained binders already mean universal polymorphism,
- monomorphization keeps constrained generic code statically dispatched by default.

The gap is value-level heterogeneity.

Today:

- `fn f[a: HasName](x: a)` works for one concrete `a`,
- but `List[a]` still means one concrete element type for the whole list,
- so `shape` does not currently give us open heterogeneous values like `List[HasName]`,
- and the only existing runtime escape hatch is trait-only `Dyn[...]`.

This plan keeps the current static generic story and adds one new value-level story:

- in binder positions, `shape` and `trait` stay universal constraints,
- in ordinary type positions, `shape` and `trait` names become interface value types,
- closed heterogeneous data stays explicit through enums/tags,
- inferred heterogeneous unions are out of scope for now.

Examples:

```mr
trait Show[a] = { fn show(self: a) -> Str }
shape HasName = { name: Str }

fn render[a: Show](x: a) -> Str = Show.show(x)
fn render(x: Show) -> Str = Show.show(x)

fn greet[a: HasName](x: a) -> Str = x.name
fn greet(x: HasName) -> Str = x.name
```

The first form in each pair is static generic code.
The second form is an interface value type.

This is existential in semantics, but the surface does not expose `exists`, `Pack`, or `dyn`.

## Why Row Polymorphism Alone Is Not Enough

This is the key distinction that caused confusion during design discussion.

Open row polymorphism means:

- one value may have extra fields,
- one unknown row tail stands for those extra fields,
- the same type variable still names one concrete type.

So:

```text
{ name: Str | r }
```

means:

- one concrete record shape,
- with one fixed but unknown extra-field tail `r`.

And:

```text
List[{ name: Str | r }]
```

still means:

- every element has the same concrete record shape,
- not "each element may choose its own different tail".

That is why:

```mr
fn map_names[a: HasName](xs: List[a]) -> List[Str]
```

means:

- there is one concrete element type `a`,
- every element of `xs` has that same `a`,
- `a` satisfies `HasName`.

It does not mean:

- each element may be any different record as long as it has `name`.

That latter meaning requires an interface value type.

## `forall` And Existentials

The plan keeps the semantic distinction, but only one side is explicit in surface syntax.

Universal side:

- current constrained binders already mean implicit `forall`
- caller chooses the concrete type
- compiler may monomorphize/specialize

Example:

```mr
fn render[a: Show](x: a) -> Str = Show.show(x)
```

Conceptually:

```text
forall a. Show(a) => a -> Str
```

Interface value side:

- ordinary type positions like `Show` or `HasName` mean "a value exposing this interface"
- concrete type is hidden at the use site
- implementation may use runtime dispatch/projectors

Example:

```mr
fn render(x: Show) -> Str = Show.show(x)
```

Conceptually:

```text
exists a. packaged value of type a satisfying Show
```

The language does not need an explicit `exists` keyword to implement this.

## In Scope

- allow `trait` names in ordinary type positions as value interface types
- allow `shape` names in ordinary type positions as value interface types
- keep constrained binders as the static universal form
- keep monomorphization as the default implementation strategy for constrained generics
- use interface value types for open heterogeneous collections, returns, params, and stored fields
- lower trait interface values and shape interface values through Go-friendly runtime machinery
- keep closed heterogeneous data explicit through enums/tags
- remove `Dyn[...]` from the long-term user-facing model
- update parser, lowering, annotation conversion, inference, codegen, docs, fixtures, LSP, and editor support

## Out Of Scope

- inferred heterogeneous unions as a convenience feature
- automatic TypeScript-style widening of arbitrary expressions
- replacing monomorphization with always-interface lowering
- higher-kinded polymorphism
- redesigning enums/unions into a different feature family
- mutation or borrowing redesign
- pattern matching on hidden concrete interface payloads
- module/package system redesign

## Locked Decisions

### 1. Keep Generic Binders As The Static Default

Constrained binders remain the shortest and most direct way to write static polymorphism.

Examples:

```mr
fn get_name[a: HasName](x: a) -> Str = x.name
fn render[a: Show](x: a) -> Str = Show.show(x)
fn same[a: HasName](x: a, y: a) -> Bool = x.name == y.name
```

These forms preserve:

- exact type identity,
- same-type relations across positions,
- static dispatch and monomorphization.

### 2. Ordinary Type Positions Become Interface Value Positions

`shape` and `trait` names become valid ordinary type expressions.

Examples:

```mr
fn render(x: Show) -> Str = Show.show(x)
fn greet(x: HasName) -> Str = x.name

let shown: Show = 42
let named: HasName = { name: "Ada", age: 42 }
let xs: List[HasName] = [named]
```

This is the open heterogeneity feature.

### 3. Surface Syntax Stays Minimal

Do not add:

- `Pack[...]`
- `exists`
- `dyn`

for the first iteration.

The surface split is:

```mr
fn render[a: Show](x: a) -> Str = Show.show(x)   # generic/static
fn render(x: Show) -> Str = Show.show(x)     # interface value/open
```

This keeps the surface small:

- constrained binders for the static side,
- plain value-position interface types for the open side.

### 4. Closed Heterogeneity Stays Explicit

Do not infer heterogeneous unions from mixed literals for now.

Closed mixed data should remain explicit through enums/tags.

Example:

```mr
type Userish = {
  Plain({ name: Str })
  Aged({ name: Str, age: Int })
}

let xs: List[Userish] = [
  Plain({ name: "Ada" }),
  Aged({ name: "Linus", age: 42 })
]
```

This keeps the language away from TypeScript-style implicit widening.

### 5. Open Heterogeneity Uses Interface Value Types

Open boundaries should use ordinary value types such as `Show` and `HasName`.

Examples:

```mr
fn make_user(flag: Bool) -> HasName =
  if (flag) {
    { name: "Ada" }
  } else {
    { name: "Linus", age: 42 }
  }

let xs: List[HasName] = [
  { name: "Ada" },
  { name: "Linus", age: 42 }
]
```

This is where the hidden-type packaging semantics matter.

### 6. Interface Coercion Is Type-Directed

Concrete values coerce to interface value types only where an interface type is already expected.

Allowed sites:

- annotated `let` bindings
- annotated parameters
- annotated returns
- collection literals with known interface element types
- branch joins with a known interface result type
- record fields whose declared type is an interface value type

Not allowed:

- global best-effort widening of arbitrary expressions
- guessing interface types for mixed literals with no expected type

This keeps the feature intentional and limits performance surprises.

### 7. Trait Interface Values And Shape Interface Values Share Runtime Machinery

Trait interface values map naturally to Go interface-style runtime representation.

Shape interface values should use the same general strategy by synthesizing accessor methods/projectors in generated Go.

Conceptually:

- `trait Show[a]` becomes a method-bearing runtime interface
- `shape HasName = { name: Str }` becomes a synthesized accessor-bearing runtime interface

Example idea for generated Go shape lowering:

```text
type __marmoset_HasName interface {
  __marmoset_get_name() string
}
```

Concrete record structs can implement those synthesized accessors.

### 8. Static Generic Code Must Stay Static By Default

Even if value-position interface types use efficient Go-style runtime interfaces, constrained generic functions remain distinct.

Examples:

```mr
fn render[a: Show](x: a) -> Str = Show.show(x)
fn keep_first[a: HasName](x: a, y: a) -> a = x
```

These are not the same as:

```mr
fn render(x: Show) -> Str = Show.show(x)
fn keep_first(x: HasName, y: HasName) -> HasName = x
```

The first pair preserves exact type information.
The second pair hides it.

### 9. `Dyn[...]` Becomes Legacy Surface

The current trait-only runtime object form should not remain the canonical public model.

Long term:

- user-facing docs and examples should prefer plain value-position `trait`/`shape` names
- implementation may still reuse or evolve existing `Dyn[...]` machinery internally

## Target User Model

Users should be able to think about the language this way:

1. `shape` describes required data.
2. `trait` describes required behavior.
3. In a binder, `a: HasName` or `a: Show` means static generic code and uses explicit qualified calls for behavior.
4. In a value type position, `HasName` or `Show` means an open interface value; field projection stays direct, but behavior calls remain qualified.
5. Closed mixed data should use enums/tags.
6. Open mixed data should use interface value types.

That is the whole model.

## Examples

### Static Generic Structural Code

```mr
shape HasName = { name: Str }

fn greet[a: HasName](x: a) -> Str = "hi " + x.name
```

### Static Generic Behavioral Code

```mr
trait Show[a] = {
  fn show(self: a) -> Str
}

fn render[a: Show](x: a) -> Str = Show.show(x)
```

### Open Structural Value Interface

```mr
shape HasName = { name: Str }

let xs: List[HasName] = [
  { name: "Ada" },
  { name: "Linus", age: 42 }
]

puts(xs[0].name)
puts(xs[1].name)
```

### Open Behavioral Value Interface

```mr
trait Show[a] = {
  fn show(self: a) -> Str
}

let xs: List[Show] = [42, "hello"]

puts(Show.show(xs[0]))
puts(Show.show(xs[1]))
```

### Mixed Structural And Behavioral Interface

```mr
shape HasName = { name: Str }

trait Render[a]: HasName = {
  fn render(self: a) -> Str = self.name
}

let x: Render & HasName = { name: "Ada" }

puts(x.name)
puts(Render.render(x))
```

### Closed Explicit Mixed Data

```mr
type Userish = {
  Plain({ name: Str })
  Aged({ name: Str, age: Int })
}
```

## Current Implementation Problems

- `shape` lowers to compile-time-only open record constraints today
- `Dyn[...]` is trait-only
- collection literals still unify element types exactly left-to-right before any interface coercion is considered
- interface value coercion is not generalized across traits and shapes
- docs do not currently explain the "one concrete type for all uses" vs "open interface value" split clearly

## Implementation Plan

### Phase 0. Freeze Terminology

- document the difference between:
  - constrained binders
  - interface value types
- stop using `Pack[...]` as the proposed surface
- stop using explicit `exists` syntax in user-facing design docs

Acceptance criteria:

- one language doc explains the split consistently
- future docs/examples use the same story

### Phase 1. Extend Type Syntax

Allow `trait` and `shape` names in ordinary type positions.

Examples:

- `Show`
- `HasName`
- `Show & HasName`
- `List[Show]`
- `{ current: HasName }`

Acceptance criteria:

- parser accepts `trait`/`shape` names in ordinary type positions
- parser rejects clearly invalid value-interface type expressions

### Phase 2. Add Internal Interface Value Types

Introduce a dedicated internal type representation for value-position interfaces.

Recommended direction:

- keep universal constraints as they are today
- add a distinct internal type form for interface values
- normalize intersections deterministically

Acceptance criteria:

- interface value types compare and print deterministically
- `Show & HasName` canonicalizes stably

### Phase 3. Generalize Validation Across Traits And Shapes

Interface value type validation must:

- collect trait method surfaces
- collect shape field requirements
- reject incompatible same-name field requirements
- reject invalid non-interface members

Acceptance criteria:

- `Show & HasName` validates
- incompatible shape intersections fail clearly

### Phase 4. Add Type-Directed Coercions

Insert interface coercions only where the expected type is already an interface value type.

Required sites:

- annotated lets
- parameter passing
- returns
- branch joins with known interface result
- collection literals with known interface element type
- record field initialization into interface-typed fields

Acceptance criteria:

- `let x: HasName = { name: "Ada", age: 42 }` works
- `fn make() -> HasName = ...` works
- `let xs: List[HasName] = [...]` works
- unannotated mixed literals are not silently widened

### Phase 5. Lower Trait Interface Values

Unify or reuse current trait-object machinery for ordinary value-position trait types.

Acceptance criteria:

- `let xs: List[Show] = [42, "hello"]` works
- method calls on those values codegen correctly

### Phase 6. Lower Shape Interface Values

Add synthesized accessor-based lowering for shape interface values.

Acceptance criteria:

- `let xs: List[HasName] = [...]` works
- field access on those values codegen correctly
- shape interface values use shared runtime adapters, not per-value hashmaps or closures

### Phase 7. Support Mixed Trait + Shape Interface Values

Support values such as:

```mr
Render & HasName
```

Acceptance criteria:

- mixed field access + method calls work
- runtime representation composes predictably

### Phase 8. Deprecate `Dyn[...]` In Docs And Fixtures

- keep implementation compatibility only as needed during migration
- move docs and examples to plain value-position interface syntax
- update fixtures to cover both static generic and open interface forms

Acceptance criteria:

- public docs no longer teach `Dyn[...]` as the main model

## Open Questions

- should value-position interface coercions be available in all typed expression contexts, or only in explicitly annotated ones?
- should nominal wrappers/sums satisfy shape interface values through explicit projection only, or is there any wrapper-aware coercion worth allowing?
- do we want any lint that prefers `[a: Trait]` over plain `Trait` when the function could preserve exact type relations?
- do we want any future sugar for naming repeated interface value types in large signatures, or is plain type syntax enough?

## Recommendation

Adopt this layered model:

- exact records and exact nominal types by default
- constrained binders for static polymorphism
- plain value-position `shape`/`trait` types for open heterogeneity
- explicit enums/tags for closed heterogeneity
- no inferred heterogeneous unions for now

This preserves Marmoset's static generic story, gives data-first code a real open-interface story, and avoids both explicit existential jargon and TypeScript-style widening.
