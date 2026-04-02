# Dot Call Resolution Plan

## Maintenance

- Last verified: 2026-04-02
- Implementation status: Planning
- Prerequisites: [drop-ufcs.md](/Users/zlw/src/marmoset/marmoset/docs/plans/todo/language/drop-ufcs.md)

## Summary

Broad UFCS lost.

Dot resolution should now become much smaller:

- namespace dot is explicit qualification,
- value dot is field access plus callable fields only,
- there is no general receiver-first search from `x.f(...)`.

This means:

- `list.map(xs, f)` is the canonical collection call,
- `Show.show(x)` is the canonical trait call,
- `Point.move(p, 1)` is the canonical exact-type grouped call,
- `x.name` only works when `name` is a real field or a shape-interface field surface.

The goal of this plan is to make the checker, docs, fixtures, and diagnostics match that smaller model.

## In Scope

- define the canonical meaning of `x.f`, `x.f(...)`, `Type.f(...)`, `Trait.f(...)`, and `Module.f(...)`
- define how value-position interface types participate without receiver-dot behavior
- remove UFCS-style candidate collection from the long-term dot story
- keep explicit namespace-qualified calls canonical
- update docs, diagnostics, fixtures, LSP wording, and editor-facing examples to match

## Out Of Scope

- removing qualified calls
- changing constructor syntax
- changing `impl` declaration syntax in this pass
- changing trait coherence rules beyond call resolution
- changing `shape` semantics
- redesigning the module namespace classifier beyond aligning it with the smaller dot model

## Locked Decisions

### 1. Split Dot Into Namespace Dot And Value Dot

There are two different surfaces:

- namespace dot:
  - `Enum.Variant`
  - `Trait.method`
  - `Type.method`
  - `Module.fn`
  - later extern/package qualifiers
- value dot:
  - `x.f`
  - `x.f(...)`

These should be specified separately instead of being explained as one giant "method" feature.

### 2. Namespace Dot Is Explicit, Not Sugar

Namespace-dot forms name an explicit target:

```mr
Option.Some(1)
Show.show(x)
Point.move(p, 1)
list.map(xs, f)
```

Rules:

- `Enum.Variant(...)` is a constructor call
- `Trait.method(x, ...)` is an explicit trait call
- `Type.method(x, ...)` is an explicit exact-type grouped call
- `Module.fn(x, ...)` is an explicit module-qualified call

This is the canonical API surface.

### 3. Value Dot Is Direct Receiver Surface Only

Value-dot forms stay literal:

- `x.f`
  - record field access
  - shape-interface field projection
- `x.f(...)`
  - callable field invocation

Important:

- `x.f` never means "search for a function named `f`"
- `x.f(...)` does not fall back to trait lookup, exact-type lookup, or top-level functions

Examples:

```mr
shape HasName = { name: Str }
trait Show[a] = { fn show(x: a) -> Str }

fn greet(x: HasName) -> Str = x.name
fn render(x: Show) -> Str = Show.show(x)
```

### 4. Concrete Receivers Do Not Get Implicit Behavior Lookup

For concrete receivers, dot does not search other callable sources.

These no longer mean the same thing:

```mr
Show.show(user)
user.show()
```

The first is an explicit trait call.
The second is only valid if `user` really has a callable field named `show`.

Likewise:

```mr
Point.move(p, 1)
p.move(1)
```

Only the qualified form is part of the long-term language model.

### 5. Direct Surface Always Wins

The only precedence inside value dot is local receiver surface precedence.

Example:

```mr
type Runner = { run: (Int) -> Int }

fn run(x: Int, y: Int) -> Int = x + y

let r = { run: (x: Int) -> x * 2 }
r.run(10)
```

This is a callable-field invocation.
The top-level `run` function is irrelevant because there is no receiver-first fallback.

### 6. Interface Value Types Keep Values, Not Receiver Methods

Ordinary value-position interface types still matter, but they do not reopen method-like dot calls.

Consequences:

- `x: HasName`
  - `x.name` is still direct interface field projection
- `x: Show`
  - `Show.show(x)` is the behavior-call surface
- `x: Show & HasName`
  - `x.name` stays direct, while behavior still goes through `Show.show(x)`

### 7. Transparent Types Do Not Reintroduce Method Ownership

Transparent structural types may still support explicit grouped calls such as:

```mr
Point.move(p, 1)
```

when an `impl Point = { ... }` block exists.

But that does not imply:

- nominal ownership,
- implicit receiver lookup,
- or `p.move(1)` working by fallback.

`impl` remains grouping and discoverability, not a reason to preserve general method search.

### 8. Diagnostics Should Suggest Qualification, Not Hidden Search Rules

When users write a missing concrete dot call such as:

```mr
p.show()
```

and `Show.show(p)` exists, the diagnostic should point them at the explicit spelling rather than pretending the call almost resolved through UFCS.

## Current Implementation Problems

The current branch still contains broad UFCS machinery from the discarded direction:

- cross-family candidate collection across exact-type impls, traits, and top-level functions
- soft-mismatch bookkeeping for dot-call fallback
- ambiguity reporting among receiver-first candidates

That machinery is now transitional debt rather than the target model.

## Target User Model

Users should be able to think about dot this way:

1. `x.f` means field access or direct interface field projection.
2. `x.f(...)` means callable-field invocation only.
3. `Trait.method(x, ...)`, `Type.method(x, ...)`, and `Module.fn(x, ...)` are the explicit call forms.
4. Dot on concrete values does not search the world.

That is the whole model.

## Implementation Plan

### Phase 0. Freeze The Semantics

- treat [drop-ufcs.md](/Users/zlw/src/marmoset/marmoset/docs/plans/todo/language/drop-ufcs.md) as the controlling plan
- update this plan, module-system docs, and interface-value docs to stop describing dot as UFCS

Acceptance criteria:

- all active docs describe the same dot model

### Phase 1. Simplify Value-Dot Resolution

- keep field access
- keep callable-field invocation
- keep direct interface field surfaces only
- stop treating failed direct lookup as a trigger for cross-family candidate search

Acceptance criteria:

- concrete `x.f(...)` does not probe trait/exact-type/top-level fallback
- interface-value behavior calls still use explicit qualification

### Phase 2. Keep Namespace Dot Explicit

- preserve trait-qualified calls
- preserve type-qualified calls
- preserve module-qualified calls
- keep constructor qualification rules unchanged

Acceptance criteria:

- `Trait.method(x, ...)`, `Type.method(x, ...)`, `Module.fn(x, ...)`, and `Enum.Variant(...)` all remain valid and well-classified

### Phase 3. Update Diagnostics And Tooling

- rewrite missing-dot diagnostics around explicit qualification
- remove stale "method candidate" wording from LSP/hover/error messages
- update fixtures and editor-facing examples

Acceptance criteria:

- docs and diagnostics no longer mention broad UFCS or receiver-first fallback as the intended semantics

## PROGRESS

- 2026-04-02: removed concrete-receiver UFCS fallback from `infer.ml`; value dot now resolves only fields, callable fields, and direct interface surfaces.
- 2026-04-02: removed the remaining interface-method dot surface from `infer.ml`; value dot now resolves only fields, interface field projection, and callable fields.
- 2026-04-02: aligned `Type.method(x, ...)` inference with the qualified trait path so inherent methods now instantiate method-level generics, enforce method-generic constraints, and record resolved method type arguments for codegen.
- 2026-04-02: migrated docs and fixtures to the explicit call model across `function_model`, `traits`, `codegen_mono`, and `traits_inherent`; trait behavior now stays qualified even for `Dyn[...]` and constrained receivers.
- Verification: `dune runtest lib/frontend/typecheck --force --no-buffer`, `make integration function_model`, `make integration codegen_mono`, and `make integration traits_inherent` all passed.
- Caveat: direct interface field projection still exists for shape/value interfaces; only method-like dot calls were removed. The worktree also contains unrelated local edits outside this plan that were left untouched.
