# Records

## Maintenance

- Last verified: 2026-03-28
- Implementation status: Canonical (actively maintained)
- Update trigger: Any parser, typechecker, or codegen change affecting records, aliases, spreads, or named products

## Scope

Records provide:

- anonymous structural record literals,
- transparent structural aliases via `alias`,
- nominal named product types via `type`,
- field access,
- spread/update,
- record pattern matching.

## Syntax

### Anonymous and aliased structural records

```marmoset
alias Point = { x: Int, y: Int }
alias Box[a] = { value: a }

let p: Point = { x: 10, y: 20 }
let b: Box[Str] = { value: "ok" }
```

### Nominal named products

```marmoset
type Waypoint = { x: Int, y: Int }

let p = Waypoint(x: 10, y: 20)
let p2 = Waypoint(...p, x: 11)
```

### Access and spread

```marmoset
let x = 1
let y = 2
let p = { x:, y: }
let p2 = { ...p, x: 10 }
let v = p2.x
```

## Semantics

- Anonymous records are structural.
- `alias` names a structural record shape transparently.
- `type` creates a nominal product type with explicit constructor syntax.
- Spread/update works on structural record literals, including named-product values used as spread bases.
- `Type(...base, field: value)` rebuilds a nominal named product while preserving the explicit constructor boundary.
- Field access works on structural records and named products.
- Record pattern matching operates on record values regardless of whether the shape is anonymous, aliased, or named.

## Design Notes

- Use `alias` when you want structural compatibility across sites.
- Use `type` when you want nominal identity, derives, or attached behavior.
- Open row variables are still an internal inference feature; user-written `...row` annotations remain restricted in v1.

## Codegen

- Anonymous structural record shapes are interned into stable Go struct types.
- `alias` reuses the structural representation and remains transparent.
- `type Name = { ... }` emits a distinct Go named type plus explicit constructor lowering.
- Spread/update lowers to copy-style reconstruction that preserves last-write-wins semantics.

## Related Docs

- `docs/features/type-annotations-and-aliases.md`
- `docs/features/traits.md`
- `docs/features/pattern-matching.md`

## Implementation Touchpoints

- Surface parsing: `lib/frontend/syntax/parser.ml`
- Lowering: `lib/frontend/syntax/lower.ml`
- Record and row types: `lib/frontend/typecheck/types.ml`
- Annotation conversion: `lib/frontend/typecheck/annotation.ml`
- Inference/unification: `lib/frontend/typecheck/infer.ml`, `lib/frontend/typecheck/unify.ml`
- Named-type registry: `lib/frontend/typecheck/type_registry.ml`
- Go emission: `lib/backend/go/emitter.ml`
