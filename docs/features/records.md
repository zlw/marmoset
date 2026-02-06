# Records

## Maintenance

- Last verified: 2026-02-06
- Implementation status: Canonical (actively maintained)
- Update trigger: Any language behavior, typechecker, or codegen change affecting this topic

## Scope

Records provide named product types with structural typing and row-polymorphic forms.

Capabilities:
- record type aliases,
- record literals and field access,
- spread/update,
- row-variable type annotations,
- record pattern matching support.

## Syntax

### Record types and aliases

```marmoset
type point = { x: int, y: int }
type box[a] = { value: a }
```

### Literals and access

```marmoset
let p = { x: 10, y: 20 }
let v = p.x
```

### Punning and spread

```marmoset
let x = 1
let y = 2
let p = { x:, y: }
let p2 = { ...p, x: 10 }
```

### Row-polymorphic annotation shape

```marmoset
let get_x = fn(r: { x: int, ...row }) -> int { r.x }
```

## Sub-Features and Use Cases

- lightweight product modeling without enum boilerplate,
- partial constraints on records ("at least these fields"),
- immutable-style updates through spread.

## Type-System Semantics

Internal types:
- `TRecord(fields, row_opt)`
- `TRowVar(name)`

Core behavior:
- structural compatibility by field set/type,
- row-tail forms for open record constraints,
- field access type resolution with row-tail unification where required.

Inference behavior:
- record literals infer concrete structural record types,
- spread/update preserves known fields and unifies overlaps (last write wins),
- row-polymorphic signatures constrain "at least these fields" without requiring closed shapes.

## Design Alternatives Considered

### Alternative A: Nominal records only

Pros:
- explicit identity and API boundaries.

Cons:
- less flexible reuse,
- more type boilerplate.

### Alternative B: Structural records + rows (Chosen)

Pros:
- expressive partial constraints,
- ergonomic data modeling,
- good fit with inference extensions.

Cons:
- row unification complexity,
- representation choices affect backend simplicity/perf.

### Alternative C: Mutable records at language surface

Pros:
- direct in-place updates.

Cons:
- semantics become more complex with sharing and optimization.

Chosen now:
- immutable-style surface semantics, optimization opportunities deferred.

## Codegen: Detailed Design

### Candidate approaches

1. Lower records to Go struct-shaped values + explicit spread copy/update (Chosen).
2. Lower records to `map[string]interface{}`.
3. Generate nominal struct declarations for every structural form globally.

### Approach 1 (Chosen)

Current style:
- emit struct-shaped record values,
- emit field access as Go field access,
- spread/update lowered through copy-like construction preserving last-write-wins semantics,
- record match lowered to branch chains with extracted bindings.

Lowering pipeline:
1. Parser distinguishes record literals from hash/map literals by key form.
2. Annotation conversion resolves record alias and row-tail syntax into internal types.
3. Inference/unification validates structural compatibility and row constraints.
4. Emitter lowers record operations into struct construction/access/update code.

Representative lowering:

Marmoset:
```marmoset
type point = { x: int, y: int }

let p: point = { x: 1, y: 2 }
let p2 = { ...p, x: 10 }
let s = p2.x + p2.y
```

Representative Go shape:
```go
type Point struct {
  X int64
  Y int64
}

p := Point{X: int64(1), Y: int64(2)}
p2 := Point{X: int64(10), Y: p.Y}
s := p2.X + p2.Y
```

Pros:
- typed field access,
- readable generated code,
- better performance than dynamic map-based representation.

Cons:
- structural-shape coordination complexity,
- spread can incur extra copies without further optimization.

### Approach 2 (`map[string]interface{}`)

Pros:
- very flexible,
- easy shape variability.

Cons:
- dynamic checks everywhere,
- poor static performance characteristics.

### Approach 3 (global nominalization)

Pros:
- cleaner backend type names.

Cons:
- heavy shape deduplication machinery,
- can drift from structural semantics expectations.

## Why Current Choice

Struct-shaped lowering preserves static intent and keeps runtime access efficient while supporting current feature scope.

## Pros and Cons of Current Record Model

Pros:
- expressive structural typing,
- row-polymorphism support for practical APIs,
- performant field operations in generated code.

Cons:
- empty-record/hash ambiguity and advanced row features still need ongoing refinement,
- spread/update optimizations are not fully mature yet.

## Related Docs

- `docs/features/type-annotations-and-aliases.md`
- `docs/features/pattern-matching.md`
- `docs/ROADMAP.md`
- `docs/archive/typechecker/phase4/milestone-4.md`

## Implementation Touchpoints

- Record syntax parsing: `lib/frontend/syntax/parser.ml`
- Record/row type representation: `lib/frontend/typecheck/types.ml`
- Annotation conversion for record/row syntax: `lib/frontend/typecheck/annotation.ml`
- Row-aware unification rules: `lib/frontend/typecheck/unify.ml`
- Record literal/access/spread inference: `lib/frontend/typecheck/infer.ml`
- Record derivation hooks for traits: `lib/frontend/typecheck/trait_registry.ml`
- Record emission (struct shapes/access/update): `lib/backend/go/emitter.ml`
