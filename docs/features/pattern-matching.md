# Pattern Matching

## Maintenance

- Last verified: 2026-04-03
- Implementation status: Canonical (actively maintained)
- Update trigger: Any language behavior, typechecker, or codegen change affecting this topic

## Scope

Pattern matching is expression-oriented branching over structured values.

Supported scrutinee classes include:
- constructor-bearing wrappers and sums,
- selected primitive patterns,
- records.

Pattern forms include:
- wildcard (`_`),
- variable binding,
- literals,
- constructors,
- record patterns (including rest binding forms where supported by semantics),
- nested combinations of the current forms,
- or-patterns.

Out of scope for the current surface:
- tuple patterns,
- array/list patterns,
- guard clauses on arms.

## Status Note

The data-first rework makes constructor patterns the primary unwrap/destructuring
surface for nominal wrappers and sums. Structural projection such as
`{ ...wrapper }` remains available, but docs and examples should prefer `match`
with constructor patterns whenever code is reading or destructuring nominal payloads.

All currently accepted pattern forms in this document are codegen-safe end to
end. The project no longer treats any accepted current-grammar pattern form as
"typechecks but backend rejects."

## Current Capability Matrix

This matrix is the frozen pre-modules support surface for pattern matching.

| Pattern form | Accepted scrutinee classes | Narrowing and bindings | Exhaustiveness behavior | Codegen guarantee |
| --- | --- | --- | --- | --- |
| Wildcard (`_`) | Any scrutinee type | No bindings; no extra narrowing | Catch-all; always contributes full coverage | Fully supported |
| Variable (`name`) | Any scrutinee type | Binds the full scrutinee value in arm scope | Catch-all; always contributes full coverage | Fully supported |
| Literal (`0`, `true`, `"x"`) | Compatible primitive scrutinees and compatible union members | Narrows matched arm to compatible member when the checker can prove it | Closed literal coverage is enforced for `Bool`; open literal domains still require a catch-all arm | Fully supported |
| Constructor (`Option.Some(x)`) | Constructor-bearing sums, wrappers, and compatible union members | Narrows to the matched constructor and binds payload names | Closed constructor sets are exhaustive when every constructor is covered or a catch-all arm exists | Fully supported |
| Record (`{ x:, y: pat, ...rest }`) | Structural records, transparent record types, and compatible union members | Binds named fields/rest and narrows record-like union members | Record-like scrutinees are exhaustive with a record catch-all shape; mixed domains still need coverage for non-record members | Fully supported |
| Nested combinations | Any scrutinee class accepted by the outer pattern form | Constructor payloads and record fields may recursively use the current pattern forms | Exhaustiveness follows the outer pattern domain; nested checks must still typecheck | Fully supported |
| Or-patterns / multi-pattern arms | Any scrutinee class where each alternative is individually valid | All alternatives must bind the same names; bindings/narrowing merge across alternatives | Coverage is the union of the alternatives; closed domains still require full coverage or a catch-all arm | Fully supported |

## Syntax

```marmoset
let y = match x {
  case 0: "zero"
  case n: "other"
}

let v = match opt {
  case Option.Some(x): x
  case Option.None: 0
}

let name = match user {
  case User(name:, ...rest): name
}

let r = match p {
  case { x:, y: }: x + y
  case _: 0
}
```

## Sub-Features and Use Cases

- expression-level branching returning values,
- destructuring payloads from constructor-bearing sums/records,
- replacing fragile index/flag-based branching logic.

## Type-System Semantics

- arm result types are unified/validated,
- pattern bindings extend local environment for arm body inference,
- exhaustiveness checks applied for supported domains,
- union scrutinees narrow through constructor, record, literal, and wildcard/variable coverage where applicable,
- or-pattern alternatives must bind the same names.

Checker responsibilities:
- constructor patterns validated against wrapper/sum constructor signatures,
- literal patterns validated against scrutinee compatibility,
- record patterns validate required fields and bind names in arm scope.

## Design Alternatives Considered

### Alternative A: if/else-only language

Pros:
- minimal syntax and implementation.

Cons:
- poor destructuring ergonomics,
- weaker exhaustiveness guarantees.

### Alternative B: Match expressions with structured patterns (Chosen)

Pros:
- explicit and safe branching,
- payload extraction integrated with typechecker.

Cons:
- substantial parser/typechecker/backend complexity.

### Alternative C: Switch-only primitive model

Pros:
- easy backend mapping for scalars.

Cons:
- inadequate for ADT/record destructuring.

## Codegen: Detailed Design

### Candidate approaches

1. Lower to Go `switch` for named sums/primitives and `if/else` chains for record patterns (Chosen).
2. Build a custom decision-tree IR first.
3. Runtime pattern engine with interpreted matcher.

### Approach 1 (Chosen)

- named-sum match -> switch on tag,
- primitive match -> switch on value,
- record match -> emitted conditional chain with bindings.

Lowering pipeline:
1. Parser builds pattern AST for each arm.
2. Inference validates each pattern and arm body type.
3. Exhaustiveness module checks arm coverage where supported.
4. Emitter lowers to concrete Go control flow (`switch` or ordered `if` chain).

Representative lowering:

Marmoset:
```marmoset
let v = match opt {
  case Option.Some(x): x
  case Option.None: 0
}
```

Representative Go shape:
```go
var v int64
switch opt.Tag {
case 0:
  x := opt.V0
  v = x
default:
  v = int64(0)
}
```

Pros:
- direct mapping to Go control flow,
- easy to inspect generated behavior,
- good runtime performance for common cases.

Cons:
- complex patterns require manual expansion in emitter,
- less room for global optimization than dedicated decision-tree lowering.

### Approach 2 (decision-tree IR)

Pros:
- potentially optimal branching,
- reusable backend logic.

Cons:
- extra IR stage complexity not yet justified.

### Approach 3 (runtime pattern engine)

Pros:
- implementation flexibility.

Cons:
- runtime overhead and weaker static guarantees.

## Why Current Choice

Direct lowering keeps implementation and generated code straightforward while covering the current pattern set.

## Pros and Cons of Current Match Model

Pros:
- expressive branching,
- integration with type narrowing and sum/record destructuring,
- strong compile-time validation opportunities.

Cons:
- tuple/list-pattern work and optimization passes remain future work,
- some exhaustive-analysis corners are still conservative.

## Related Docs

- `docs/features/enums.md`
- `docs/features/records.md`
- `docs/features/unions.md`
- `docs/ROADMAP.md`

## Implementation Touchpoints

- Match/pattern syntax parsing: `lib/frontend/syntax/parser.ml`
- Pattern typing + arm inference: `lib/frontend/typecheck/infer.ml`
- Exhaustiveness checks: `lib/frontend/typecheck/exhaustiveness.ml`
- Match lowering to Go control flow: `lib/backend/go/emitter.ml`
