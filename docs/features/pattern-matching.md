# Pattern Matching

## Maintenance

- Last verified: 2026-02-06
- Implementation status: Canonical (actively maintained)
- Update trigger: Any language behavior, typechecker, or codegen change affecting this topic

## Scope

Pattern matching is expression-oriented branching over structured values.

Supported scrutinee classes include:
- enums,
- selected primitive patterns,
- records.

Pattern forms include:
- wildcard (`_`),
- variable binding,
- literals,
- constructors,
- record patterns (including rest binding forms where supported by semantics).

## Syntax

```marmoset
let y = match x {
  0: "zero"
  n: "other"
}

let v = match opt {
  option.some(x): x
  option.none: 0
}

let r = match p {
  { x:, y: }: x + y
  _: 0
}
```

## Sub-Features and Use Cases

- expression-level branching returning values,
- destructuring payloads from enums/records,
- replacing fragile index/flag-based branching logic.

## Type-System Semantics

- arm result types are unified/validated,
- pattern bindings extend local environment for arm body inference,
- exhaustiveness checks applied for supported domains.

Checker responsibilities:
- constructor patterns validated against enum constructor signatures,
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

1. Lower to Go `switch` for enums/primitives and `if/else` chains for record patterns (Chosen).
2. Build a custom decision-tree IR first.
3. Runtime pattern engine with interpreted matcher.

### Approach 1 (Chosen)

- enum match -> switch on tag,
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
  option.some(x): x
  option.none: 0
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
- integration with type narrowing and enum/record destructuring,
- strong compile-time validation opportunities.

Cons:
- advanced pattern forms and optimization passes remain future work,
- some exhaustive-analysis corners are still conservative.

## Related Docs

- `docs/features/enums.md`
- `docs/features/records.md`
- `docs/features/unions.md`
- `docs/ROADMAP.md`
- `docs/archive/typechecker/phase4/milestone-2.md`

## Implementation Touchpoints

- Match/pattern syntax parsing: `lib/frontend/syntax/parser.ml`
- Pattern typing + arm inference: `lib/frontend/typecheck/infer.ml`
- Exhaustiveness checks: `lib/frontend/typecheck/exhaustiveness.ml`
- Match lowering to Go control flow: `lib/backend/go/emitter.ml`
