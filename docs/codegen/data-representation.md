# Data Representation in Go Codegen

## Design Decision

Marmoset is immutable at the surface level (no mutation syntax). This lets us use pass-by-reference under the hood without worrying about aliasing bugs.

## Representation by Type

| Marmoset Type | Go Representation | Passing |
|---------------|-------------------|---------|
| Int | int64 | value |
| Float | float64 | value |
| Bool | bool | value |
| String | string | value (but Go strings are immutable refs internally) |
| [T] (Array) | []T | reference (slice header copied, underlying array shared) |
| {K: V} (Hash) | map[K]V | reference (maps are pointers in Go) |
| Record | *Struct | pointer (no copies) |
| Function | func(...) | reference |

## Rationale

### Primitives: Value
Small, cheap to copy. No benefit from pointers.

### Slices/Maps: Already References
Go's natural behavior. Slice header is small, underlying data is shared.

### Records: Always Pointer
- Avoids copying potentially large structs
- Aliasing is safe because Marmoset doesn't allow mutation
- Simpler than heuristics (small=value, big=pointer)

## Future Optimization: Uniqueness Analysis

Like Roc, we could analyze value ownership:
- If a value has one owner → mutate in place
- Otherwise → copy

Example:
```
let arr = [1, 2, 3]
let arr2 = push(arr, 4)  # arr not used after, can mutate in place
```

This gives FP semantics with imperative performance, no user annotations needed.

## Escape Hatch (Future)

For performance-critical code, we might add explicit mutation:
- `mut` keyword or similar
- Only in specific contexts
- Discouraged but available

For now: pure immutable semantics, compiler optimizes where possible.
