# Dot Call Resolution Plan

## Maintenance

- Last verified: 2026-03-30
- Implementation status: Planning
- Prerequisites: Current data-first declaration semantics

## Summary

Dot resolution currently mixes several different ideas:

- field access,
- function-valued field calls,
- trait dispatch,
- exact-type impl lookup,
- top-level UFCS fallback,
- qualified trait/type calls,
- enum constructor qualification.

The language direction is simpler than the implementation currently suggests:

- qualified calls are the explicit surface,
- value-dot calls are sugar,
- nominal types may have owner-like exact-type behavior because they have distinct identity,
- transparent structural `type` declarations do not own behavior and should not be explained that way.

This plan keeps qualified calls and UFCS, but simplifies the model and the implementation around one consistent rule set.

## In Scope

- define the canonical meaning of `x.f`, `x.f(...)`, `Type.f(...)`, and `Trait.f(...)`
- remove fake OO ownership language from the structural side of the model
- preserve owner-like behavior for nominal wrappers and sums
- make ambiguous receiver-first calls require explicit qualification
- simplify the typechecker resolver around a single dot-call model
- update docs, diagnostics, fixtures, LSP wording, and editor-facing examples to match

## Out Of Scope

- removing qualified calls
- changing constructor syntax
- changing `impl` declaration syntax in this pass
- changing trait coherence rules beyond call resolution
- changing `shape` semantics
- deciding final module-qualified call rules beyond reserving space for them

## Locked Decisions

### 1. Split Dot Into Namespace Dot And Value Dot

There are two different surfaces:

- namespace dot:
  - `Enum.Variant`
  - `Trait.method`
  - `Type.method`
  - later `Module.fn`
- value dot:
  - `x.f`
  - `x.f(...)`

These must be specified separately instead of being treated as one giant "method call" feature.

### 2. Namespace Dot Is Explicit, Not Sugar

Namespace-dot forms name an explicit target:

```mr
Option.Some(1)
Show.show(x)
Point.sum(p)
```

Rules:

- `Enum.Variant(...)` is a constructor call
- `Trait.method(x, ...)` is an explicit trait call
- `Type.method(x, ...)` is an explicit exact-type impl call
- later `Module.fn(x, ...)` should be treated the same way: explicit namespace qualification

This is the canonical, non-sugary API surface.

### 3. Value Dot Is Small And Predictable

Value-dot forms have only two meanings:

- `x.f` means field access
- `x.f(y, z)` means either:
  - call the callable field `x.f`, if `x.f` is a field and has callable type
  - otherwise, treat it as receiver-first call sugar

Important:

- `x.f` by itself never means "look up a method"
- only the call form `x.f(...)` enters UFCS-style resolution

### 4. UFCS Candidate Set Must Stay Short

Once `x.f(...)` is not a callable-field call, the resolver considers only:

- exact-type impl target: `Type.f(x, ...)`
- trait target: `Trait.f(x, ...)`
- plain function target: `f(x, ...)`

That is the full receiver-first candidate set for this milestone.

### 5. Ambiguous UFCS Calls Must Be Rejected

Within receiver-first call resolution:

- 0 matches: error
- 1 match: use it
- 2 or more matches: error and require explicit qualification

Examples:

```mr
type Point = { x: Int, y: Int }

trait Show[a] = {
  fn show(x: a) -> Str
}

impl Show[Point] = {
  fn show(x: Point) -> Str = "trait"
}

impl Point = {
  fn show(x: Point) -> Str = "exact"
}

let p = { x: 1, y: 2 }

p.show()        # error: ambiguous, qualify
Show.show(p)    # ok
Point.show(p)   # ok
```

This replaces hidden precedence among UFCS call targets.

### 6. Callable Field Access Still Wins First

Callable field access is not part of the ambiguity rule above because it starts from `x.f` as field access.

Example:

```mr
type Runner = { run: (Int) -> Int }

fn run(x: Int, y: Int) -> Int = x + y

let r = { run: (x: Int) -> x * 2 }
r.run(10)   # calls the field, not the top-level function
```

This stays true even if a UFCS candidate named `run` also exists.

### 7. Ownership Is Real For Nominal Types, Not For Transparent Types

The implementation already distinguishes these cases internally:

- transparent `type Point = { x: Int, y: Int }`
  - resolves to the underlying exact structural type
  - does not create a distinct impl-owning identity
- constructor-bearing `type UserId = UserId(Int)`
  - has distinct nominal identity
  - impls attach only to that nominal type
- constructor-bearing sum `type Expr = { Num(Int), Add(Expr, Expr) }`
  - also has distinct nominal identity
  - impls attach only to that nominal type

Consequences:

- nominal wrappers and sums may be described in docs as "having their own impls"
- transparent types should be described as exact-type extensions, not ownership
- the call model is still UFCS in both cases

### 8. Qualified Calls Stay Canonical

The language should teach:

- `Trait.method(x, ...)`
- `Type.method(x, ...)`

as the explicit forms.

Dot syntax:

- is ergonomic sugar
- should never be the only way to spell a call
- should never be needed to explain what code means

## Current Implementation Problems

The current resolver carries historical complexity that no longer matches the language story well:

- it still uses "method" terminology for several different kinds of call targets
- dot-call resolution is spread across field access, trait lookup, inherent lookup, qualified calls, and UFCS fallback branches
- exact-type and trait calls currently use implicit precedence on dot calls instead of rejecting ambiguity
- transparent structural exact-type impls can look owner-like in tests and docs even though they are not nominal

## Target User Model

Users should be able to think about calls this way:

1. `x.f` means field access.
2. `x.f(...)` means:
   - field-function call if `f` is a callable field on `x`
   - otherwise receiver-first sugar
3. Receiver-first sugar searches explicit call targets.
4. If more than one explicit call target fits, write the qualification yourself.

That is the whole model.

## Implementation Plan

### Phase 1. Specify And Freeze The Resolver Contract

- document the exact resolution order and ambiguity behavior in language docs
- explicitly separate:
  - namespace classification
  - value-dot callable-field detection
  - UFCS candidate collection
- stop describing the structural side as "inherent ownership"

Acceptance criteria:

- docs use one consistent explanation
- examples show qualified calls as canonical and dot calls as sugar

### Phase 2. Restructure The Typechecker Around A Shared Resolver

Refactor [infer.ml](/Users/zlw/src/marmoset/marmoset/lib/frontend/typecheck/infer.ml) so dot handling becomes one shared flow:

1. classify namespace dot vs value dot
2. for value dot:
   - try field access
   - if call syntax and field is callable, call it
   - otherwise collect UFCS candidates
3. if UFCS candidates are:
   - empty: error
   - singleton: use it
   - multiple: ambiguity error

Implementation notes:

- keep enum constructor qualification separate from UFCS candidate collection
- keep qualified trait and type calls explicit paths, but route them through shared callable checking where possible
- replace ad hoc precedence branches with candidate collection plus ambiguity handling
- narrow the word "method" internally to trait members, or replace it with more neutral naming where practical

Acceptance criteria:

- no hidden precedence between exact-type impls, traits, and free functions in dot-call sugar
- qualified calls still work exactly
- callable fields still short-circuit before UFCS

### Phase 3. Align Diagnostics

Diagnostics should explain the explicit fix:

- ambiguous dot call:
  - say which candidates matched
  - suggest `Trait.method(x, ...)` or `Type.method(x, ...)`
- missing dot call:
  - say no callable field or receiver-first target matched
- structural exact-type impl docs/messages:
  - avoid owner-style wording

Acceptance criteria:

- no diagnostics imply fake nominal ownership for transparent types
- ambiguity errors point users at qualification, not guesswork

### Phase 4. Expand Test Coverage Around Real Scenarios

Add or update fixtures for:

- callable field beats UFCS fallback
- exact-type only
- trait only
- free-function UFCS only
- exact-type plus trait ambiguity
- trait plus free-function ambiguity
- exact-type plus free-function ambiguity
- qualified trait call remains explicit and stable
- qualified type call remains explicit and stable
- nominal wrapper impl does not apply to payload type
- transparent exact-type impl applies by exact structural type, not alias spelling
- enum constructor qualification is unaffected

Also add unit coverage for candidate collection and ambiguity reporting, not only integration fixtures.

Acceptance criteria:

- green tests cover both happy paths and ambiguous/error paths
- ambiguity behavior is asserted directly, not inferred from side effects

### Phase 5. Update Docs, Examples, And LSP Copy

Update:

- feature docs for traits and exact-type impls
- the data-first semantics plan where it references dot behavior
- examples that currently teach dot-first style without qualification
- LSP hover/completion wording where it implies "methods owned by transparent types"

Acceptance criteria:

- canonical examples prefer qualified forms when teaching behavior
- dot-call examples are presented as convenience syntax, not as the core model

## Test Matrix

Minimum required matrix:

- receiver kinds:
  - transparent exact structural type
  - nominal wrapper
  - nominal sum
  - builtin primitive
- call target kinds:
  - callable field
  - exact-type impl
  - trait impl
  - top-level function
- surfaces:
  - dot sugar
  - qualified trait call
  - qualified type call

Important edge cases:

- same method/function name available from multiple UFCS categories
- generic exact-type impl plus more specific exact-type impl
- trait supermethod visibility through qualified trait calls
- zero-arg and multi-arg receiver-first sugar
- structural alias-equivalent records using the same exact-type impl

## Risks

- changing from precedence to ambiguity errors may break existing dot-call code that currently resolves silently
- the current implementation has typechecker, codegen, and LSP assumptions tied to old resolution artifacts
- docs and tests currently use "inherent method" as a blanket term even when the behavior is structural exact-type extension

## Non-Goals For This Pass

This plan does not require:

- removing `impl Type = { ... }`
- inventing new syntax for explicit exact-type extensions
- removing dot-call sugar
- deciding whether modules should participate in value-dot UFCS

## Success Criteria

This work is done when:

- the language explanation fits on one page without special cases
- transparent vs nominal ownership is explained correctly
- dot sugar never depends on hidden precedence among UFCS targets
- every ambiguity has an explicit qualified spelling
- tests cover the real edge cases instead of only the happy path

## PROGRESS

### Progress

- 2026-03-30: Landed a prerequisite slice for this plan: qualified trait methods, exact-type impl functions, and sum constructors are now first-class values. `Show.show`, `MyInt.label`, `Box.is_empty`, and `OptionInt.Some` can be stored, passed, and called as ordinary callables instead of only working in direct qualified-call position.
- 2026-03-30: Added red integration fixtures first for trait/type/enum qualified values, then implemented the typechecker and Go-emitter support and reran the full integration suite green.

### Findings

- The old implementation split namespace qualification by surface form instead of by callable/value intent. Direct `Trait.method(x)` and `Type.method(x)` worked, but bare `Trait.method` and `Type.method` fell through unrelated unbound-variable or constructor-only branches.
- Go codegen had the same blind spot for higher-order use: it only refined let-bound callable RHS types from later calls when the RHS was syntactically a lambda/function, so qualified callable values needed the same refinement path.

### Caveats

- This landed slice does not change dot-call precedence or ambiguity behavior yet. `x.f(...)` still follows the current resolver; the "ambiguous => qualify" cleanup remains future work for this plan.
- Qualification is first-class at the callable/constructor level, not at the namespace level. `Show.show` now works as a value; `Show` by itself still does not.

### Verification

- `make integration enums/e68_unknown_variant_rejected.mr function_model/fm135_qualified_trait_value_first_class.mr function_model/fm136_qualified_named_type_method_value_first_class.mr function_model/fm137_qualified_enum_method_value_first_class.mr function_model/fm138_enum_constructor_value_first_class.mr function_model/fm139_qualified_trait_value_passed_as_argument.mr function_model/fm140_qualified_inherent_generic_value_first_class.mr`
- `dune runtest --root /Users/zlw/src/marmoset/marmoset lib/frontend/typecheck/ --force`
- `dune runtest --root /Users/zlw/src/marmoset/marmoset lib/backend/go/ --force`
- `make integration function_model`
- `make integration`
