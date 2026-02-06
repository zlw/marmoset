# Arrays and Maps (Hashes)

## Maintenance

- Last verified: 2026-02-06
- Implementation status: Canonical (actively maintained)
- Update trigger: Any language behavior, typechecker, or codegen change affecting this topic

## Scope

This doc covers collection features:

- homogeneous arrays: `list[T]` / literals `[ ... ]`
- key-value maps (hashes): `hash[K, V]` / literals `{ "k": v }`

## Syntax

### Arrays

```marmoset
let xs = [1, 2, 3]
let first = xs[0]
let last = xs[-1]
```

### Maps / Hashes

```marmoset
let h = { "x": 10, "y": 20 }
let x = h["x"]
```

### Distinction from records

- string/expression keys -> map/hash
- identifier keys -> record

## Sub-Features and Use Cases

Arrays:
- ordered sequences,
- indexing,
- builtins like `len`, `first`, `last`, `rest`, `push`.

Maps:
- dynamic key lookup,
- flexible dictionaries.

## Type-System Semantics

- arrays infer one element type (`TArray elem`).
- maps infer key/value type (`THash (k, v)`).
- mixed element/key/value types are type errors.

Design decision:
- homogeneous collections only.

Pros:
- simple inference and codegen.

Cons:
- heterogeneous collections require unions or enums.

## Design Alternatives Considered

### Alternative A: Heterogeneous arrays/maps by default

Pros:
- convenience for scripting.

Cons:
- weak static guarantees,
- backend and trait constraints become noisier.

### Alternative B: Homogeneous collections (Chosen)

Pros:
- strong static type properties,
- clear performance profile.

Cons:
- sometimes requires explicit sum types.

### Alternative C: Persistent immutable collection runtime

Pros:
- stronger value semantics.

Cons:
- heavier runtime, more complex codegen.

Chosen now:
- leverage Go slices/maps, with language-level immutability discipline and future optimization opportunities.

## Codegen: Detailed Design

### Candidate Approaches

1. Go slices/maps directly (Chosen).
2. Runtime wrapper types with method dispatch.
3. Persistent structure runtime library.

### Approach 1 (Chosen)

- arrays -> `[]T`
- maps -> `map[K]V`
- index emission uses helper patterns for negative index behavior where needed.

Pros:
- straightforward backend,
- good runtime performance,
- interop with Go idioms.

Cons:
- maps/slices are reference-like in Go; requires frontend semantic discipline.

Lowering examples:

Marmoset:
```marmoset
let xs = [1, 2, 3]
let h = { "k": 7 }
puts(xs[-1])
puts(h["k"])
```

Representative Go shape:
```go
xs := []int64{int64(1), int64(2), int64(3)}
h := map[string]int64{"k": int64(7)}
_ = puts(indexArr(xs, int64(-1)))
_ = puts(h["k"])
```

### Approach 2 (Wrapper runtime)

Pros:
- centralized semantics.

Cons:
- overhead and more emitted runtime scaffolding.

### Approach 3 (Persistent runtime)

Pros:
- stronger immutable behavior guarantees.

Cons:
- significant complexity/perf trade-offs unless deeply optimized.

## Why Current Choice

Direct Go slices/maps balance performance, simplicity, and implementation velocity. This is adequate until stronger immutability/perf transformations are required.

## Pros and Cons of Current Collection Model

Pros:
- efficient generated code,
- simple inference,
- minimal runtime machinery.

Cons:
- deep immutability guarantees are by language semantics, not runtime representation,
- optimization opportunities (copy elision/uniqueness) are deferred.

## Related Docs

- `docs/features/records.md`
- `docs/ROADMAP.md`

## Implementation Touchpoints

- Collection literals/index parsing: `lib/frontend/syntax/parser.ml`
- Collection typing/inference: `lib/frontend/typecheck/infer.ml`
- Builtins (`len/first/last/rest/push`): `lib/frontend/typecheck/builtins.ml`
- Go emission for arrays/maps/indexing: `lib/backend/go/emitter.ml`
