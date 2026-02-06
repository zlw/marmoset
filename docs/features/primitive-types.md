# Primitive Types

## Maintenance

- Last verified: 2026-02-06
- Implementation status: Canonical (actively maintained)
- Update trigger: Any language behavior, typechecker, or codegen change affecting this topic

## Scope

Primitive scalar types are the smallest value categories in Marmoset:

- `int`
- `float`
- `bool`
- `string`
- `null`

These are the base of inference, operator typing, trait impls, and codegen mappings.

## Syntax

```marmoset
let i: int = 42
let f: float = 3.14
let b: bool = true
let s: string = "hello"
let n = null
```

## Sub-Features and Use Cases

- Arithmetic over numeric primitives.
- Comparisons and equality.
- String values with indexing support.
- `null` as unit-like no-value result in expression contexts.

Use cases:
- numeric compute,
- boolean branch control,
- string processing,
- side-effect-style expression results via `null`.

## Type-System Semantics

Internal representations include dedicated constructors (`TInt`, `TFloat`, `TBool`, `TString`, `TNull`) rather than modeling primitives as aliases.

Why:
- faster unification dispatch,
- better error diagnostics,
- direct operator compatibility rules.

## Design Alternatives Considered

### Alternative A: Unified numeric type

Model all numbers as one `number`.

Pros:
- simpler surface model.

Cons:
- weaker static guarantees.
- backend ambiguity and coercion complexity.

### Alternative B: Distinct `int` + `float` (Chosen)

Pros:
- explicit semantics.
- simpler operator typing and codegen.

Cons:
- mixed arithmetic requires explicit handling rules.

### Alternative C: No explicit `null`

Use option/result everywhere.

Pros:
- stronger explicitness.

Cons:
- burdens simple expression/statement bridging.

Chosen:
- keep `null` as a primitive result form, while still supporting richer sum types elsewhere.

## Codegen: Detailed Design

### Candidate Approaches

1. Box all primitives into `interface{}`.
2. Map each primitive to native Go scalar (Chosen).
3. Custom runtime tagged value for every value.

### Approach 1 (Boxed)

Pros:
- uniform backend path.

Cons:
- runtime type assertions everywhere.
- extra boxing overhead.

### Approach 2 (Chosen)

Mappings:
- `int` -> `int64`
- `float` -> `float64`
- `bool` -> `bool`
- `string` -> `string`
- `null` -> `struct{}`-style unit representation

Pros:
- fast native ops.
- no boxing for normal primitive paths.
- clean generated code.

Cons:
- requires explicit bridge rules where features use dynamic forms (e.g. unions).

Lowering example:

Marmoset:
```marmoset
let x = 41
puts(x + 1)
```

Representative Go shape:
```go
x := int64(41)
_ = puts((x + int64(1)))
```

### Approach 3 (Tagged runtime value)

Pros:
- uniform semantics across all values.

Cons:
- high runtime overhead.
- large runtime surface area.

## Why Current Choice

Native scalar mapping is the best local optimum for current goals:
- high performance,
- readable generated Go,
- clear static typing at compile-time.

## Pros and Cons of Current Primitive Model

Pros:
- predictable typing and operations,
- good backend performance,
- compact generated code.

Cons:
- conversion boundaries needed for dynamic features,
- `null` semantics still require discipline in API design.

## Related Docs

- `docs/ARCHITECTURE.md`
- `docs/features/functions-and-polymorphism.md`
- `docs/features/unions.md`

## Implementation Touchpoints

- Parser literals: `lib/frontend/syntax/parser.ml`
- Type representation: `lib/frontend/typecheck/types.ml`
- Operator typing/inference: `lib/frontend/typecheck/infer.ml`
- Go emission: `lib/backend/go/emitter.ml`
