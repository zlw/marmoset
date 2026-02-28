# Union Types

## Maintenance

- Last verified: 2026-02-28
- Implementation status: Canonical (actively maintained)
- Update trigger: Any language behavior, typechecker, or codegen change affecting this topic

## Scope

Union types model ad-hoc sum types where a value can be one of several alternatives.

Examples:
- `int | string`
- `point | null`

Primary capabilities:
- union annotations in parameters/lets/returns,
- branch narrowing via `is`,
- control-flow-sensitive typing in `if`/`match` style flow.

## Syntax

```marmoset
let x: int | string = 5

let parse = fn(v: int | string) -> int {
  if (v is int) { v } else { len(v) }
}
```

## Sub-Features and Use Cases

- APIs that accept multiple scalar/domain alternatives.
- Null-safety style modeling (`T | null`) without unsafe unchecked access.
- Progressive migration from loosely typed values toward nominal enums.
- Branch-local logic where each arm can use fully narrowed operations.

## Type-System Semantics

Internal representation:
- `TUnion of mono_type list`

Core rules:
- normalize unions (flatten nested unions and remove duplicates),
- union compatibility is structural over member alternatives,
- direct operations on a union require narrowing first,
- `if (x is T)` narrows true branch to `T` and false branch to complement when known.

Inference behavior:
- `if`/`match` arms with different result types can synthesize unions,
- branch environments can carry narrowed bindings,
- unification keeps union representation deterministic after substitution.

## Design Alternatives Considered

### Alternative A: Enums only, no raw unions

Pros:
- explicit nominal domain modeling.

Cons:
- too verbose for lightweight mixed-value APIs.

### Alternative B: Structural unions (Chosen)

Pros:
- concise and expressive,
- integrates naturally with control-flow narrowing.

Cons:
- backend needs a dynamic representation boundary.

### Alternative C: Full subtyping lattice with rich variance first

Pros:
- strong theoretical expressiveness.

Cons:
- large complexity jump in checker and codegen reasoning.

## Codegen: Detailed Design

### Candidate approaches

1. `interface{}` representation + assertions/switches (Chosen).
2. Auto-generated tagged wrappers per union shape.
3. Fully tagged runtime value object for all values.

### Approach 1 (Chosen)

Runtime representation:
- union-typed values lower to Go `interface{}`.

Lowering pipeline:
1. Typechecker produces union metadata in the type map.
2. Emitter checks expression type; union paths emit `interface{}` variables.
3. `is` checks lower to type assertions or type-switch tests.
4. Narrowed branch expressions emit concrete typed locals where needed.

Representative lowering:

Marmoset:
```marmoset
let f = fn(v: int | string) -> int {
  if (v is int) { v + 1 } else { len(v) }
}
```

Representative Go shape:
```go
func f(v interface{}) int64 {
  if n, ok := v.(int64); ok {
    return n + int64(1)
  } else {
    return int64(len(v.(string)))
  }
}
```

Pros:
- minimal runtime surface,
- easy to map from existing typed AST,
- inspectable generated code.

Cons:
- boxing/dynamic checks in union-heavy hot paths,
- repeated narrowing can duplicate assertion cost.

### Approach 2 (Tagged wrappers)

Pros:
- explicit static tags and payload fields,
- cheaper repeated dispatch once constructed.

Cons:
- many generated wrapper types,
- constructor/bridge boilerplate explosion.

### Approach 3 (Unified tagged runtime)

Pros:
- one uniform dynamic runtime model.

Cons:
- high overhead for programs that are mostly monomorphic.

## Why Current Choice

`interface{}` lowering is the best current trade-off for implementation complexity, generated-code clarity, and delivery speed.

## Pros and Cons of Current Union Model

Pros:
- concise type-level expressiveness,
- predictable narrowing semantics,
- incremental path toward richer optimizations later.

Cons:
- dynamic checks at runtime boundaries,
- advanced union subtyping/variance remains deferred.

## Related Docs

- `docs/features/pattern-matching.md`
- `docs/features/enums.md`
- `docs/ROADMAP.md`

## Implementation Touchpoints

- Union syntax parsing: `lib/frontend/syntax/parser.ml`
- Type representation and normalization: `lib/frontend/typecheck/types.ml`
- Annotation conversion for union forms: `lib/frontend/typecheck/annotation.ml`
- Union unification/compatibility: `lib/frontend/typecheck/unify.ml`
- Narrowing and union inference: `lib/frontend/typecheck/infer.ml`
- Union emission and `is` lowering: `lib/backend/go/emitter.ml`
