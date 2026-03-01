# Traits

## Maintenance

- Last verified: 2026-02-28
- Implementation status: Canonical (actively maintained)
- Merge note: This document now includes the former `docs/features/trait-satisfaction.md` content.
- Update trigger: Any parser/typechecker/codegen/test change affecting trait declaration, satisfaction, resolution, or lowering

## Scope

Traits provide:
- nominal method capabilities via explicit impls,
- structural field constraints via field-only traits,
- hybrid constraints via mixed traits,
- supertrait closure,
- constrained generics,
- method-call syntax (`x.foo(...)`) lowering,
- field-only trait use in type position.

## Syntax

### Method-only trait

```marmoset
trait show[a] {
  fn show(x: a) -> string
}
```

### Field-only trait

```marmoset
trait named {
  name: string
}
```

### Mixed trait

```marmoset
trait named_show {
  name: string
  fn show(x: { name: string }) -> string
}
```

### Supertraits

```marmoset
trait eq[a] {
  fn eq(x: a, y: a) -> bool
}

trait ord[a]: eq {
  fn compare(x: a, y: a) -> ordering
}
```

### Impl

```marmoset
impl show for int {
  fn show(x: int) -> string {
    "int"
  }
}
```

### Generic impl

```marmoset
impl show[a: show] for list[a] {
  fn show(xs: list[a]) -> string { "list" }
}
```

### Constraint and method call

```marmoset
let show_it = fn[t: show](x: t) -> string {
  x.show()
}
```

### Field-only trait in type position

```marmoset
trait named {
  name: string
}

let p: named = { name: "alice", age: 42 }
puts(p.name)
```

## Trait Kinds

Trait kind is derived from members:
- `FieldOnly`: fields present, methods absent.
- `MethodOnly`: methods present, fields absent.
- `Mixed`: both present.

Supertraits are orthogonal and may themselves be field-only, method-only, or mixed.

## Satisfaction Semantics

### Big rule

- Fields are structural.
- Methods are nominal.
- Mixed traits require both structural field satisfaction and nominal method satisfaction.
- Supertraits are enforced transitively.

### Field satisfaction

A type satisfies required trait fields when:
- receiver type is a record (aliases resolving to records included),
- every required field exists,
- each field type unifies with the required type.

Non-record receivers for field traits are rejected with an explicit diagnostic category.

### Method satisfaction

A type satisfies required trait methods when:
- an explicit impl exists for `(trait_name, concrete_type)` in the registry, or
- a builtin impl exists for that pair.

Structural method probing is forbidden. Having a field/method with the same name does not imply trait satisfaction.

### Supertraits

For `trait child: parent1 + parent2`, satisfying `child` requires satisfying all supertraits recursively.
This applies to:
- impl validation,
- generic constraint checking,
- constrained method availability (`[a: ord]` exposes `eq` methods if `ord: eq`).

### Generic impl behavior

Generic impl blocks are supported:
- impl type params may declare constraints (`impl show[a: show] for list[a]`),
- impl type params must be unique,
- every impl type param must appear in the impl target type,
- concrete impls take precedence over matching generic impls,
- overlapping generic impl matches are rejected as ambiguous with impl-site details.

### Failure categories

Trait-satisfaction diagnostics include structured categories such as:
- `[missing-impl]`
- `[missing-field]`
- `[field-type-mismatch]`
- `[non-record-for-field-trait]`
- `[unknown-trait]`

## Coherence and Ambiguity

- Duplicate impl for the same canonical `(trait, type)` pair is rejected.
- Field-only traits cannot have `impl` blocks (field traits are structural by design).
- If multiple trait impls expose the same method name for the same receiver type, method resolution is a hard ambiguity error and includes impl sites.
- Builtin impls may be overridden once by user impl for the same key.

## Constraint Checking

Constraint checking is enforced at call/instantiation time:
- type-variable obligations are collected from substitutions,
- each obligation runs through `Trait_solver.satisfies_trait`,
- first failing obligation aborts with an explicit trait-satisfaction reason.

Multiple constraints (`[a: show + eq]`) are conjunctive.

## Method Resolution Rules

Method-call resolution for `receiver.method(args...)`:
1. If `receiver` is an enum type identifier, parse as enum constructor call.
2. If receiver type is a constrained type variable, search methods from the expanded constraint set (including supertraits).
3. Otherwise resolve via trait impl registry for the concrete receiver type.
4. If no trait method is found, inherent lookup is attempted for the concrete receiver type.

Resolution guarantees:
- no structural method lookup,
- deterministic ambiguity errors,
- field access (`x.name`) is separate from method resolution (`x.show()`).

Inherent-method interaction:
- if a trait method and an inherent method exist for the same `(type, method_name)`, this is a hard ambiguity error,
- constrained type-variable method resolution uses trait constraints only (inherent methods do not participate).

## Trait-as-Type Policy (v1)

### Allowed

- Field-only traits in type position, if non-generic.
- Field-only trait supertraits composed from field-only chains.

### Rejected

- Method-only traits in type position.
- Mixed traits in type position.
- Field-only traits that are generic.
- Field-only traits whose supertrait closure includes method/mixed traits.

### Internal lowering

Field-only trait types lower to open record requirements assembled from trait fields plus supertrait field closure, with deterministic field merging.

## Operator Requirements via Traits

Operator typing enforces trait obligations:
- unary `-` requires `neg`
- `+ - * /` require `num` (except string concatenation special-case for `+`)
- `< > <= >=` require `ord`
- `== !=` require `eq`

Codegen lowering:
- primitives lower to direct Go operators,
- non-primitives lower to trait helper calls (`num_add_*`, `eq_eq_*`, `ord_compare_*`, `neg_neg_*`) selected from typechecked operator obligations.

## Codegen: Detailed Design

### Method dispatch (chosen)

Trait method calls lower to static helper function calls:
- `x.show()` -> `show_show_<type>(x)`

The emitter uses method-resolution metadata recorded by the typechecker (`expr.id -> trait_name`) and does not re-infer resolution decisions.

### Field-only trait values

v1 uses structural record projection, not runtime trait-object dispatch:
- when expression actual type has a superset of fields and expected type is a narrower record shape (from field-only trait typing), emitter inserts a projection wrapper,
- emitted value is a concrete Go struct literal with required fields copied.

This enables heterogeneous containers through shared projected shape while avoiding trait-object/vtable ABI commitments.

### Derive integration

Derive surface supports selected traits (`eq`, `show`, `debug`, `ord`, `hash`) with registry validation and emitter-side body generation for supported shapes.

## Design Alternatives Considered

### Alternative A: Structural methods (TypeScript-style)

Pros:
- fewer explicit impls.

Cons:
- weak coherence and hard-to-debug accidental matches.

Status:
- rejected.

### Alternative B: Named trait constraints + structural fields (chosen)

Pros:
- explicit method coherence,
- ergonomic record-shape constraints,
- clear typechecker responsibilities.

Cons:
- two satisfaction modes (field vs method) increase conceptual surface.

### Alternative C: Full method trait objects in v1

Pros:
- dynamic dispatch flexibility.

Cons:
- early ABI/runtime commitment,
- higher complexity before module/FFI design is stable.

Status:
- deferred.

## Current Limitations

- Method/mixed trait objects are intentionally unsupported in this phase.
- Generic field-only trait objects are intentionally unsupported in this phase.
- Qualified trait-call syntax is not implemented.

## Deferred: Method/Mixed Trait Objects and Existentials

This is intentionally deferred work, but the design constraints are locked to avoid future ABI drift.

### Representation options considered

1. Interface-only runtime values (Go interface-based dispatch)
2. Dictionary passing (explicit witness records)
3. Tagged existential package (`type_id`, payload, witness)

### Guardrails

- Do not expose method/mixed trait-object syntax until a first-class internal representation exists (typed AST/IR), with explicit witness-carrying semantics.
- Do not treat ad-hoc emitter-only lowering as acceptable for dynamic trait values.
- Do not expose raw Go interface internals as stable module/FFI ABI.
- Lock operation semantics (`eq`, `ord`, `hash`, `show`) for existential values before enabling them.

### Preconditions before implementation

1. Add typed IR/AST support for existential values and dynamic trait dispatch.
2. Thread resolution/coherence metadata from typechecker to codegen without re-resolution.
3. Freeze module/FFI ABI representation for existential payloads/witnesses.
4. Add deterministic tests for dispatch behavior and capability limits.

## Why This Design

The current model keeps method behavior explicit and coherent while preserving structural ergonomics for records. It also keeps codegen simple and performant by using static dispatch and projection-based field trait typing.

## Related Docs

- `docs/features/functions-and-polymorphism.md`
- `docs/features/records.md`
- `docs/features/inherent-methods.md`
- `docs/ROADMAP.md`

## Implementation Touchpoints

- Trait parsing: `lib/frontend/syntax/parser.ml`
- Trait AST shape: `lib/frontend/syntax/ast.ml`
- Trait registry/coherence: `lib/frontend/typecheck/trait_registry.ml`
- Trait satisfaction solver: `lib/frontend/typecheck/trait_solver.ml`
- Trait annotations/type-position policy: `lib/frontend/typecheck/annotation.ml`
- Method-call typing and operator obligations: `lib/frontend/typecheck/infer.ml`
- Builtin trait definitions/impls: `lib/frontend/typecheck/builtins.ml`
- Static dispatch + projection lowering: `lib/backend/go/emitter.ml`
- Integration coverage: `test/integration/04_traits.sh`, `test/integration/04_traits_field.sh`, `test/integration/04_traits_impl.sh`, `test/integration/04_traits_operator.sh`
