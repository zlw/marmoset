# Pre-Modules Semantics Foundation Plan

## Maintenance

- Last verified: 2026-04-02
- Implementation status: Completed milestone; canonical pre-modules semantics baseline
- Update trigger: Any change to declaration roles, exact-vs-constructor type behavior, trait/shape rules, effect/purity policy, or call resolution
- Prerequisites:
  - `docs/plans/done/language/02_syntax-rework.md`
  - `docs/plans/done/language/03_syntax-rework-followup.md`

## Summary

This file now records the pre-modules semantics that actually landed after the
data-first pivot and the later call-model cleanup.

It absorbs the implemented outcomes that had been split across the old
pre-modules, data-first, drop-UFCS, and dot-call planning threads.
Downstream plans should treat this file as the canonical baseline.

The current foundation is:

- plain `type Name = TypeExpr` is transparent and exact,
- constructor-bearing `type` forms introduce explicit nominal wrappers or sums,
- `enum Name = { ... }` remains accepted compatibility sugar for constructor-bearing sum `type` forms,
- `shape` owns named open structural field constraints,
- `trait` owns method-only nominal behavior,
- `Dyn[...]` remains the explicit runtime trait-object surface,
- behavior calls are explicit and qualified,
- value dot is literal field access plus callable-field invocation only,
- broad UFCS is gone.

Representative surface:

```mr
type Point = { x: Int, y: Int }
type UserId = UserId(Int)
type Event = { Click({ x: Int, y: Int }), Quit }

shape HasName = { name: Str }

trait Show[a] = {
  fn show(x: a) -> Str
}

impl Point = {
  fn sum(p: Point) -> Int = p.x + p.y
}

let p: Point = { x: 1, y: 2 }
let shown: Dyn[Show] = 42

Point.sum(p)
Show.show(shown)
```

## In Scope

- freeze the current declaration-role split between transparent `type`, constructor-bearing `type`, `shape`, and `trait`
- freeze the current data-first record/wrapper/sum semantics
- freeze the current behavior-ownership model for derives, trait impls, and exact-type grouped functions
- freeze the current qualified-call model and the removal of general UFCS
- keep the current effect/purity and union/narrowing behavior as part of the pre-modules baseline
- keep docs, fixtures, lowering, typechecking, codegen, and editor tooling aligned on the same model

## Out Of Scope

- module/import/export syntax or multi-file compilation
- prelude, FFI, or stdlib API design
- interface value types beyond the current `Dyn[...]` trait-object surface
- function overloading
- post-modules associated types / open rows / coherence expansion
- broad parity and hardening work that belongs to `05_pre-modules-parity-and-hardening.md`

## Locked Decisions

### 1. Plain `type` Is Transparent; Constructors Are The Nominal Boundary

The current surface is:

- `type Name = TypeExpr`
  - transparent exact name
- `type Name = Name(Payload)`
  - nominal wrapper with explicit constructor
- `type Name = { Variant(...), ... }`
  - nominal sum with explicit constructors
- `enum Name = { ... }`
  - compatibility sugar for the constructor-bearing sum form

Examples:

```mr
type Point = { x: Int, y: Int }
type Reducer[a] = (a, a) -> a
type UserId = UserId(Int)
type User = User({ id: UserId, name: Str })
type Color = { Red, Green, Blue }
```

Consequences:

- `type Point = { x: Int, y: Int }` does not create a separate nominal product identity
- constructor-bearing forms do create an explicit nominal boundary
- wrapper/sum construction and pattern matching stay explicit at the constructor surface

### 2. Exact Records Are Structural Again

Exact product records are structural whether they are anonymous or named with a
plain transparent `type`.

Examples:

```mr
type Point = { x: Int, y: Int }

let a: Point = { x: 1, y: 2 }
let b = { x: 1, y: 2 }
```

Consequences:

- field access, spread/update, and record patterns operate on exact structural records
- named and anonymous exact records with the same fields are interchangeable
- constructor-bearing wrappers and sums do not implicitly collapse back into structural records; wrapper-side construction and matching stay explicit

### 3. `shape` Owns Named Open Structural Constraints

`shape` remains the named open structural contract surface:

```mr
shape HasName = { name: Str }
shape HasXY = { x: Int, y: Int }
```

Rules:

- `shape` means "has at least these fields"
- shapes are satisfied structurally
- shapes do not own derives, impls, or `Dyn[...]`
- field-style trait bodies are rejected; structural field requirements belong here

### 4. `trait` Is Method-Only Nominal Behavior

Traits remain method-only and impl-backed:

```mr
trait Show[a] = {
  fn show(x: a) -> Str
}
```

Rules:

- trait bodies are method-only
- trait satisfaction is nominal and comes from explicit impls or builtins
- traits may depend on other traits and on shapes
- method-only trait names are not ordinary runtime value types; use constrained binders or `Dyn[...]`
- constrained-param shorthand remains available in binder positions
- `Dyn[...]` remains the explicit runtime trait-object surface

### 5. Behavior Ownership Is Explicit

Current ownership rules:

- derives attach to types, not shapes
- trait impls attach to builtins, constructor-bearing nominal types, and exact structural types as supported by the current registries
- `impl Type = { ... }` registers exact-type grouped functions / inherent helpers for that receiver type
- inherent methods never satisfy trait constraints

Because exact structural records are transparent, exact-type grouped behavior is
keyed by the exact receiver type rather than by a separate nominal owner.

### 6. Qualified Calls Are Canonical; Value Dot Is Literal

Broad receiver-first UFCS is no longer part of the language model.

Namespace-qualified forms are explicit:

- `Enum.Variant(...)`
- `Trait.method(x, ...)`
- `Type.method(x, ...)`
- `Module.fn(x, ...)`

Value dot is small and literal:

- `x.f`
  - field access
  - shape/constrained-field projection
- `x.f(...)`
  - callable-field invocation only

It does **not** do receiver-first lookup across:

- trait impls
- exact-type grouped functions
- top-level functions

Examples:

```mr
Show.show(x)
Point.sum(p)
list.map(xs, f)
```

These are canonical.

These are only valid when the receiver really exposes a callable field:

```mr
x.show()
p.sum()
xs.map(f)
```

### 7. Qualified Targets Are First-Class Values

The implemented branch supports first-class qualified callable values such as:

- `Show.show`
- `Point.sum`
- `Option.Some`

That keeps the explicit-qualified call model usable in higher-order code without
reintroducing receiver-first dot fallback.

### 8. Effects, Purity, And Narrowing Stay On The Current Implemented Baseline

The original pre-modules branch also landed real cleanup around:

- may-be-effectful callable types,
- purity checking across callable forms,
- unions and narrowing,
- derive/tooling groundwork.

Those improvements remain part of this milestone.
`05_pre-modules-parity-and-hardening.md` owns hardening and drift prevention, not
semantic redefinition.

## What This Milestone Completed

- replaced the old nominal named-product + `alias` story with the transparent exact `type` / constructor-bearing nominal split
- froze the shape-vs-trait split and kept trait objects explicit through `Dyn[...]`
- removed broad UFCS from the intended language model
- reduced value dot to field access, constrained-field projection, and callable fields
- made trait-qualified and type-qualified calls the canonical behavior surface
- carried qualified-call identity through inference and codegen, including first-class qualified callable values
- updated active feature docs and fixtures to the explicit-qualified behavior model

## Remaining Follow-Up

- `docs/plans/done/language/05_pre-modules-parity-and-hardening.md` hardens and audits this baseline
- `docs/plans/todo/language/02_module-system.md` extends the same qualifier classifier into real module namespaces
- `docs/plans/todo/language/07_function-overloading.md` can build only on plain and qualified calls, not on value-dot fallback
- `docs/plans/todo/language/08_forall-exists.md` stays separate from this baseline because it changes value-level interface packaging rather than the core pre-modules declaration and call model

## Verification

Verified against the current implementation on 2026-04-02 with:

- `dune runtest lib/frontend/typecheck lib/backend/go --force --no-buffer`
- `make integration function_model traits traits_impl traits_inherent records cross_feature codegen_mono`
