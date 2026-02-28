# Records

## Maintenance

- Last verified: 2026-02-28
- Implementation status: Canonical (actively maintained)
- Update trigger: Any language behavior, typechecker, or codegen change affecting this topic

## Scope

Records provide named product types with structural typing and row-polymorphic forms.

Capabilities:
- record type aliases,
- record literals and field access,
- spread/update,
- shape interning with named Go types,
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

### Row-polymorphic annotation shape (v1 restriction)

Open row variables (`...row`) in type annotations are rejected in v1.
The inference engine handles row polymorphism internally, but user-written
`...row` syntax in annotations is not supported due to codegen limitations
with multiple call sites.

```marmoset
// v1: use closed annotations or omit annotations for field access
let get_x = fn(r: { x: int }) -> int { r.x }
let get_x = fn(r) { r.x }  // inference handles the row internally
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
- row-polymorphic behavior is handled internally by inference (users cannot write `...row` in annotations in v1).

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

1. Lower records to Go struct-shaped values + explicit spread copy/update.
2. Lower records to `map[string]interface{}`.
3. Generate nominal struct declarations for every structural form globally (Chosen: shape interning).

### Approach 3 (Chosen: shape interning)

Current style:
- emit top-level named struct type definitions for each unique record shape,
- type aliases (e.g., `type point = { x: int, y: int }`) produce Go types with the alias name (e.g., `type Point struct{...}`),
- anonymous record shapes get canonical names (e.g., `Record_x_int64_y_int64`),
- identical shapes are deduplicated (only one type definition per canonical shape),
- field access as Go field access,
- spread/update lowered through copy-like construction preserving last-write-wins semantics,
- record match lowered to branch chains with extracted bindings.

Lowering pipeline:
1. Parser distinguishes record literals from hash/map literals by key form.
2. Annotation conversion resolves record alias syntax into internal types.
3. Inference/unification validates structural compatibility.
4. Emitter interns record shapes, registers type aliases, and emits named type definitions.

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

### Approach 1 (inline anonymous structs, previous)

Pros:
- simple implementation.

Cons:
- no type sharing,
- opaque Go debugging/tooling,
- does not scale to modules.

## Why Current Choice

Shape interning with named Go types preserves static intent, keeps runtime access efficient, supports type aliases for readable generated code, and provides deduplication needed for modules.

## Pros and Cons of Current Record Model

Pros:
- expressive structural typing,
- named Go types improve debuggability and tool integration,
- type alias support for user-defined record names,
- performant field operations in generated code.

Cons:
- open row variables in annotations deferred to post-v1,
- multiple spread entries in one record literal are intentionally rejected in v1,
- empty-record/hash ambiguity needs ongoing refinement.

## Related Docs

- `docs/features/type-annotations-and-aliases.md`
- `docs/features/pattern-matching.md`
- `docs/ROADMAP.md`

## Implementation Touchpoints

- Record syntax parsing: `lib/frontend/syntax/parser.ml`
- Record/row type representation: `lib/frontend/typecheck/types.ml`
- Annotation conversion for record/row syntax: `lib/frontend/typecheck/annotation.ml`
- Row-aware unification rules: `lib/frontend/typecheck/unify.ml`
- Record literal/access/spread inference: `lib/frontend/typecheck/infer.ml`
- Record derivation hooks for traits: `lib/frontend/typecheck/trait_registry.ml`
- Record emission (struct shapes/access/update): `lib/backend/go/emitter.ml`
