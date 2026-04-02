# Function Overloading Plan

## Maintenance

- Last verified: 2026-04-02
- Implementation status: Planning
- Update trigger: Any change to plain-call resolution, namespace qualification, or constrained generic dispatch

## Context

Marmoset currently allows exactly one top-level binding per name.

Today this is rejected:

```mr
fn foo(point: Point, x: Int) -> Str = "int"
fn foo(point: Point, x: Str) -> Str = "str"
```

with a duplicate-definition error from the top-level value namespace.

That keeps the language simple, but it blocks a useful data-first style:

- same operation name across different argument shapes,
- explicit module APIs that do not need awkward suffixes,
- pipe-friendly functions that can still share one conceptual verb.

The design goal here is not to copy Erlang/Elixir literally.
Those languages do runtime clause selection with pattern matching and guards.
Marmoset should stay statically typed and compile-time-resolved.

The real goal is:

- allow same-name callable sets,
- filter candidates by arity and type fit,
- pick one statically when exactly one candidate fits,
- report ambiguity when more than one candidate still fits,
- keep this scoped to plain and qualified calls.

## Summary

Introduce static overload sets for named callables.

Core idea:

- a name such as `foo` may have multiple callable definitions,
- call resolution works over a candidate set instead of one binding,
- candidate viability is determined by full call-shape checking,
- non-fitting candidates are soft mismatches, not immediate hard errors,
- ambiguity is reported only among viable candidates.

Example:

```mr
fn foo(point: Point, x: Int) -> Str = "int"
fn foo(point: Point, x: Str) -> Str = "str"

let p: Point = { ... }

foo(p, 1)    # picks Int overload
foo(p, "1")  # picks Str overload
```

Qualified namespaces should inherit the same behavior:

```mr
json.decode("1")
json.decode(bytes)
```

provided the relevant qualified namespace supports overload sets.

## Non-Goals

This plan does not attempt to add:

- Erlang/Elixir-style pattern-matching clauses in function heads
- guard-based overload selection
- return-type-based overloading
- implicit numeric coercion to make overloads fit
- overloads for local `let` bindings in v1
- trait method overloading in v1
- exact-type grouped function overloading in v1
- overload resolution from concrete value-dot calls such as `x.foo(...)`
- inferred heterogeneous unions as part of this work

## Relationship To Existing Plans

### 1. The Merged Pre-Modules Call Model Comes First

This plan depends on:

- [04_pre-modules-semantics-foundation.md](/Users/zlw/src/marmoset/marmoset/docs/plans/done/language/04_pre-modules-semantics-foundation.md)

Why:

- the merged foundation now freezes the no-UFCS / explicit-qualified call model,
- overloads should not reintroduce receiver-first candidate search through the back door,
- plain and qualified call resolution should stabilize before overload sets are added.

### 2. Interface Value Types Come After The Core Call Model

This plan should land before or alongside [08_forall-exists.md](/Users/zlw/src/marmoset/marmoset/docs/plans/todo/language/08_forall-exists.md), but only after plain and qualified calls already have a stable candidate model.

Why:

- value-position `Show` / `HasName` keeps interface values in the type system without reopening receiver-method lookup,
- overloads should stay clearly separate from field access and qualified trait calls,
- otherwise we risk collapsing explicit interface calls and overload resolution into one problem.

## Locked Decisions

### 1. Overloading Is Static, Not Runtime Clause Matching

Overloads are selected at type-check time.

This is not:

- runtime branch selection,
- pattern matching in function heads,
- or clause ordering semantics.

The compiler should decide the target before codegen.

### 2. Overload Sets Are Name-Based Within A Namespace Bucket

In v1, overload sets apply to:

- top-level functions in one namespace bucket
- module-qualified function namespaces

They do not apply to:

- local lets,
- trait methods within one trait,
- exact-type grouped functions within one impl target,
- enum constructors.

Example:

```mr
fn foo(x: Int) -> Str = "int"
fn foo(x: Str) -> Str = "str"
```

This creates one overload set named `foo`.

### 3. Candidate Fit Uses Full Call Checking

A candidate is viable only if the whole call shape fits:

- name matches,
- arity matches,
- argument types unify,
- generic instantiation succeeds,
- required generic constraints hold,
- namespace qualification fits if relevant.

Name match alone is not enough.

### 4. Soft Mismatch Is The Default Inside Overload Sets

When a candidate exists by name but does not fit the call, it should be dropped from the viable set rather than terminating resolution immediately.

Examples:

```mr
fn foo(x: Int) -> Str = "int"
fn foo(x: Str) -> Str = "str"

foo(1)      # Str overload soft-mismatches, Int overload wins
foo("x")    # Int overload soft-mismatches, Str overload wins
```

This applies inside:

- top-level overload sets
- qualified module overload sets
- any future explicitly qualified overload bucket

It does not apply to value-dot fallback because that fallback is no longer part of the language direction.

### 5. Ambiguity Is Reported Only Among Viable Candidates

If:

- zero candidates fit: error
- one candidate fits: use it
- more than one candidate fits: ambiguity error

Example:

```mr
fn foo(x: Int) -> Str = "a"
fn foo(x: Int) -> Str = "b"
```

This must be rejected as duplicate or ambiguous, not chosen by source order.

### 6. Source Order Must Not Affect Resolution

Resolution should not depend on declaration order.

This avoids function-clause semantics drifting toward Elixir in the wrong way.

### 7. Return Type Alone Never Selects An Overload

Do not select overloads purely from expected return type.

Allowed input to selection:

- argument types
- arity
- namespace qualification when present
- generic constraints that arise from argument fitting

Disallowed:

- "pick the overload whose return type matches the annotation"

That path makes inference and diagnostics much worse.

### 8. Exact-Same Signatures Stay Rejected

Even with overload sets, exact duplicate signatures should still be rejected.

Example:

```mr
fn foo(x: Int) -> Str = "a"
fn foo(x: Int) -> Str = "b"
```

This is not useful overloading. It is duplicate definition.

## User Model

Users should be able to think:

1. A name may refer to an overload set.
2. Calling that name tests which candidates fit the call.
3. Candidates that do not fit simply drop out.
4. If exactly one fits, that is the call target.
5. If several fit, add more type information or disambiguate the call shape.

That is the whole story.

## Examples

### Plain Function Overloading

```mr
fn parse(x: Int) -> Str = "int"
fn parse(x: Str) -> Str = x

parse(1)
parse("ok")
```

### Qualified Overloading

```mr
module json = {
  fn decode(x: Str) -> Result[Int, Str] = ...
  fn decode(x: Bytes) -> Result[Int, Str] = ...
}

json.decode("1")
json.decode(bytes)
```

### Pipe-Friendly Overloading

```mr
fn status_text(x: Int) -> Str = "code:{x}"
fn status_text(x: Bool) -> Str = if (x) { "ok" } else { "fail" }

[1, 2, 3]
|> list.map(status_text)
```

If the expected callable type is `(Int) -> Str`, the overload set can resolve from that expected type plus argument flow.

## Type-System Design

### Candidate Collection

For a call:

```mr
foo(arg1, arg2)
```

or:

```mr
module.foo(arg1, arg2)
```

resolution should work over a candidate set.

Each candidate probe should produce one of:

- `Matched target`
- `SoftMismatch diag`
- `NoCandidate`
- `Fatal diag`

Meaning:

- `Matched target`
  - viable overload candidate
- `SoftMismatch`
  - candidate exists but does not fit this call shape
- `NoCandidate`
  - no candidate by that name in that namespace bucket
- `Fatal`
  - broken internal state / invalid registry / impossible invariant

### Viability Rules

A candidate is viable only if:

- arity matches
- all arguments type-check against its parameters
- generic instantiation succeeds
- trait constraints required by that candidate hold

### Ambiguity Rules

After candidate collection:

- no matches:
  - report the best stored soft-mismatch diagnostic if one exists
  - otherwise generic "no matching overload" error
- one match:
  - use it
- several matches:
  - ambiguity error listing viable candidates

### Specificity

V1 recommendation:

- do not implement a complicated "most specific overload wins" lattice yet
- if more than one candidate fits, report ambiguity

This keeps the model predictable.

Possible future refinement:

- exact concrete overload beats generic overload if uniquely more specific

But that should not be part of the first pass.

## Namespace And Resolution Model

### Top-Level Namespace

The top-level value namespace should map a name to either:

- one callable definition
- or an overload set of callable definitions

The duplicate-definition check must change accordingly.

### Qualified Names

For qualified calls like:

```mr
Module.foo(...)
```

the same candidate-set logic should apply within the qualified namespace bucket when that bucket supports multiple candidates.

V1 may keep:

- trait methods unique within a trait
- exact-type grouped functions unique within an impl target

so the immediate overload work is mostly in top-level and module namespaces.

## Frontend Implementation Plan

### Phase 0. Freeze Resolver Scope

- freeze the scope to plain and qualified calls only
- define the shared candidate result shape used by overload-set probes
- do not thread overload work into value-dot fallback

Acceptance criteria:

- the plan and docs no longer describe UFCS-based overload selection
- zero-match paths preserve useful diagnostics

### Phase 1. Replace Duplicate Top-Level Callable Rejection

Current duplicate top-level let rejection must change for functions/callables that belong to overload sets.

Rules:

- same name + callable definition + different callable signature => same overload set
- same name + exact same callable signature => duplicate-definition error
- non-callable binding collisions remain rejected

Acceptance criteria:

- overloaded top-level functions can be registered
- exact duplicate signatures are still rejected
- function/value collisions remain clear errors

### Phase 2. Introduce Overload Sets In The Environment

Extend the value environment so a name may resolve to:

- a single callable
- or a callable overload set

This needs to work in:

- plain calls
- qualified namespace calls
- higher-order lookup when an expected function type can disambiguate the set

Acceptance criteria:

- the environment can represent and retrieve multiple callable candidates by name
- non-callable lookups are unaffected

### Phase 3. Plain Call Resolution Over Overload Sets

For:

```mr
foo(args...)
```

resolve:

- gather all callable candidates named `foo`
- probe each candidate with soft mismatch
- choose one / ambiguous / none

Acceptance criteria:

- examples with `foo(Int)` and `foo(Str)` work
- mismatched overloads do not block viable ones
- zero-match calls report useful diagnostics

### Phase 4. Qualified Namespace Integration

Extend qualified namespaces so buckets such as `module.foo(...)` may contain multiple candidates.

Acceptance criteria:

- qualified overloads resolve the same way as unqualified overloads
- diagnostics show the qualified namespace and viable signatures clearly

### Phase 5. Diagnostics

Add diagnostics for:

- duplicate exact overload signature
- ambiguous overload call
- no matching overload
- overload set mixed with non-callable binding conflict

Diagnostics should show candidate signatures where practical.

Acceptance criteria:

- error text explains ambiguity and mismatch clearly
- candidate signatures appear in ambiguity and no-match output

### Phase 6. Tests

Add fixtures for:

- overloaded free functions with different arities
- overloaded free functions with different parameter types
- overload ambiguity
- qualified overloads
- duplicate exact signature rejection
- higher-order expected-type disambiguation, if enabled in the same pass

## Risks

1. **Higher-order ambiguity.** Bare references to overloaded names in value position can get tricky quickly.
   - Example: `let f = parse`
   - Recommendation: require expected type or explicit wrapper when needed.

2. **Namespace creep.** It is easy for "top-level only" overloads to turn into a broader mixed dispatch feature.
   - Mitigation: keep trait and exact-type grouped functions unique in v1.

3. **Diagnostics quality.** Overload resolution without clear candidate reporting feels random.
   - Mitigation: surface candidate signatures in errors wherever practical.

## Recommendation

This feature is reasonable if kept narrow:

- top-level functions first
- qualified module namespaces when needed
- no local-let overloading in v1
- no source-order semantics
- no "most specific wins" lattice in v1
- ambiguity on multiple viable candidates

That gives Marmoset an Erlang/Elixir-like "does this call fit?" feel without reintroducing the broad UFCS resolver that the language just decided to remove.
