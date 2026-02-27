# Inherent Methods

## Maintenance

- Last verified: 2026-02-27
- Implementation status: Blocked (do not implement until trait core prerequisites are complete)
- Update trigger: Any parser/typechecker/codegen change affecting method definitions or method-call resolution

## Scope

This document defines a first-class "method on type" feature that does not require a trait declaration.

Goal:
- let users attach behavior directly to a type in a scalable way.

Non-goals for v1:
- dynamic dispatch / trait objects,
- modules-aware method visibility policy,
- full nominal type system redesign.

## Readiness Gate (Blocking)

This feature is currently blocked by trait-system core gaps documented in:
- `/Users/zlw/src/marmoset/marmoset-ml/docs/review/trait-satisfaction-codex.md`

Required before implementation starts:
1. Generic impl handling must be fixed (no uncaught exceptions in impl type conversion / applicability).
2. Supertrait semantics must be enforced in impl validation, constraint checking, and method availability.
3. A single trait satisfaction API must exist (replace ad-hoc direct checks).

Reason:
- method-call resolution is shared between inherent and trait methods; adding inherent methods before stabilizing trait satisfaction increases ambiguity and regression risk.

## Problem Statement

Today, method syntax (`x.foo()`) is available only through trait impls. That makes simple type-local APIs verbose because users must create a trait even when behavior is not intended for generic constraint reuse.

Current workaround:
- define a one-off trait + impl, or
- use plain functions with receiver as first argument.

These are workable but not scalable for normal type-local method authoring.

## Proposed Syntax (v1)

Add inherent impl blocks:

```marmoset
type point = { x: int, y: int }

impl point {
  fn sum(p: point) -> int { p.x + p.y }
  fn translate(p: point, dx: int, dy: int) -> point {
    { x: p.x + dx, y: p.y + dy }
  }
}

let p = { x: 1, y: 2 }
puts(p.sum())
```

Trait impl syntax remains unchanged:

```marmoset
impl show for point {
  fn show(p: point) -> string { "..." }
}
```

## Method Model

Inherent method call `recv.m(a, b)` lowers to a static function call where `recv` is the first argument.

Method definition rules in v1:
- each inherent method must have at least one parameter (the receiver parameter),
- receiver parameter type must match the impl target type,
- receiver parameter name is user-defined (`self` is not required),
- return type annotation is required (same as trait impl methods in current pipeline),
- method parameter annotations are required (same as trait impl methods in current pipeline).

## Type-System Semantics

### 1. Registration

Add an inherent-method registry keyed by:
- canonicalized receiver type,
- method name.

### 2. Coherence / collisions

v1 coherence policy:
- duplicate inherent methods for the same `(type, method_name)` are rejected at typecheck time,
- if an inherent method and a trait-provided method collide on the same `(type, method_name)`, typecheck rejects the program with an explicit ambiguity error.

Rationale:
- avoids silent shadowing,
- keeps dispatch deterministic,
- avoids requiring new disambiguation syntax in v1.

### 3. Resolution in `MethodCall`

On `receiver.method(args...)`:
1. resolve enum constructor form (`Enum.Variant(...)`) first (existing behavior),
2. resolve inherent method candidates,
3. resolve trait method candidates,
4. if no candidates -> "no method found" error,
5. if multiple candidates after coherence filtering -> ambiguity error.

### 4. Structural vs nominal behavior

This is a blocking design decision and must be locked before implementation.

Option A (recommended v1): nominal ownership for inherent methods
- methods attach to a declared owner type/alias symbol, not to structural shape,
- records remain structural for assignment/comparison where already supported,
- method sets do not leak across unrelated aliases with same shape.

Option B: structural ownership by canonicalized shape
- equivalent record shapes share inherent methods.

Current status:
- **Unresolved.**
- Do not implement inherent-method lookup until this is decided.

Additional hard rule (locked):
- inherent methods must never count toward trait satisfaction.

## Parser / AST Design

## Grammar extension

Support two `impl` forms:
- trait impl: `impl <trait_name> [<generic_params>] for <type_expr> { ... }` (existing),
- inherent impl: `impl <type_expr> { ... }` (new).

## AST extension

Add dedicated AST statement kind (recommended):
- `InherentImplDef` with:
  - target type expression,
  - method impl list.

Why separate node:
- avoids overloading trait impl semantics,
- keeps validation and codegen paths explicit,
- easier future policy (visibility/modules) without trait coupling.

## Typechecker Design

### Registry

Add inherent registry module/state near trait registry:
- register methods,
- lookup candidates by canonical receiver type + method name,
- enforce duplicate/collision checks.

### Validation

For each inherent method:
- infer/check body with receiver + params in env,
- ensure method signature annotations are complete,
- ensure first param type matches impl target type.

### Diagnostics

Required error classes:
- missing receiver parameter,
- receiver type mismatch,
- duplicate inherent method,
- trait/inherent collision for same method name and receiver type,
- unresolved method call,
- ambiguous method call.

## Codegen Design

## Candidate approaches

1. Static free-function lowering (Chosen).
2. Generate Go methods on emitted struct types.
3. Interface/vtable-based dispatch.

## Approach 1 (Chosen)

Lower inherent methods to top-level helper functions with mangled names:
- `point.sum(p)` style internal call shape.

`recv.m(args...)` lowers to:
- `inherent_<method>_<receiver_mangle>(recv, args...)`.

Why chosen:
- consistent with current trait-method lowering strategy,
- works for all receiver kinds (not only structs),
- avoids Go method-set constraints for primitives/unions/aliases.

## Approach 2 (Go native methods)

Pros:
- more idiomatic Go for struct receivers.

Cons:
- not uniform across primitives and non-struct lowered types,
- complicates current uniform mangling and call rewriting model.

## Approach 3 (dynamic dispatch)

Pros:
- flexible runtime polymorphism.

Cons:
- unnecessary runtime overhead for this feature,
- contradicts current static-dispatch direction.

## Why Current Choice

The compiler already resolves calls statically and emits explicit helper functions. Inherent methods are a language-surface extension, not a dispatch model change. Static free-function lowering gives predictable behavior with minimal architectural disruption.

## End-to-End Flow (planned)

1. Parser emits `InherentImplDef` nodes.
2. Typechecker validates and registers inherent methods.
3. Typechecker resolves method calls using unified candidate logic.
4. Codegen emits inherent helper functions.
5. Method call emission lowers to inherent helper call when selected.

## TDD Implementation Plan

## Phase 0: Trait Core Prerequisites (must pass first)

- Complete P0 items in `/Users/zlw/src/marmoset/marmoset-ml/docs/review/trait-satisfaction-codex.md`.
- Add regression tests proving:
  - supertrait obligations are enforced,
  - generic impl matching is safe and non-crashing,
  - trait satisfaction checks are centralized and deterministic.

## Phase A: Parser/AST

- Add parser tests for:
  - `impl <type> { ... }` parse success,
  - malformed inherent impl diagnostics.
- Add AST shape tests for `InherentImplDef`.

## Phase B: Typechecker

- Add red tests for:
  - valid inherent method call,
  - duplicate inherent method rejection,
  - receiver-type mismatch rejection,
  - trait/inherent collision rejection,
  - unresolved inherent method diagnostics.
- Implement registry + resolution + validation.

## Phase C: Codegen

- Add red tests for:
  - emitted helper function naming,
  - method-call lowering correctness,
  - integration runtime outputs.
- Implement helper emission and method-call rewrite path.

## Phase D: Regression matrix

- ensure trait-only behavior is unchanged,
- ensure record canonicalization still yields deterministic method resolution,
- ensure deterministic codegen output for same source.
- ensure inherent methods do not satisfy trait constraints implicitly.

## Migration / Compatibility

- Existing trait method programs continue to work.
- No syntax break for existing `impl <trait> for <type>`.
- New inherent syntax is additive.

## Open Questions (needs decision before implementation start)

1. Structural vs nominal inherent ownership (blocking; see Section "Structural vs nominal behavior").
2. Should v1 allow inherent methods on all types (`int`, unions, enums, records), or only on alias/enum/record targets?
3. Should inherent-vs-trait name collisions be hard errors (proposed), or should one side win by precedence?
4. Do we want receiver shorthand (`fn sum(self)`) in v1, or postpone and require explicit receiver annotations?

## Related Docs

- `/Users/zlw/src/marmoset/marmoset-ml/docs/features/traits.md`
- `/Users/zlw/src/marmoset/marmoset-ml/docs/features/records.md`
- `/Users/zlw/src/marmoset/marmoset-ml/docs/features/functions-and-polymorphism.md`
- `/Users/zlw/src/marmoset/marmoset-ml/docs/ARCHITECTURE.md`
- `/Users/zlw/src/marmoset/marmoset-ml/docs/ROADMAP.md`
