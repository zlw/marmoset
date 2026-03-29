# Records

## Maintenance

- Last verified: 2026-03-28
- Implementation status: Canonical (actively maintained)
- Update trigger: Any parser, typechecker, or codegen change affecting records, aliases, spreads, or named products

## Scope

Records provide:

- anonymous structural record literals,
- transparent exact-record naming via plain `type Name = { ... }`,
- explicit nominal wrappers and sums only when constructors are present,
- field access,
- spread/update,
- record pattern matching.

## Status Note

The canonical behavior in this file is being migrated to the data-first plan in
`docs/plans/todo/language/data-first-semantics-rework.md`.

During that migration, treat these rules as the target direction:

- exact product records are structural again, even when named with `type`,
- constructor-bearing wrappers and sums remain the nominal boundary,
- wrapper payload destructuring should prefer `match` with constructor patterns,
- bare structural projection like `{ ...wrapper }` is secondary and mainly for reshaping or updating payload data.

## Syntax

### Anonymous and named structural records

```marmoset
type Point = { x: Int, y: Int }
type Box[a] = { value: a }

let p: Point = { x: 10, y: 20 }
let b: Box[Str] = { value: "ok" }
```

### Nominal wrappers

```marmoset
type Waypoint = Waypoint({ x: Int, y: Int })

let p = Waypoint({ x: 10, y: 20 })
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
- `type Name = { ... }` names an exact structural record transparently.
- Constructor-bearing forms such as `type User = User({ ... })` remain nominal.
- Spread/update works on structural record literals and on explicit structural projections of wrapper payloads.
- `Wrapper(...base, field: value)` rebuilds a nominal wrapper while preserving the constructor boundary.
- Record pattern matching remains structural; nominal wrappers should prefer constructor-pattern matching when unwrapping payloads.

## Design Notes

- Use `type Name = { ... }` when you want an exact structural record name.
- Use constructor-bearing `type` forms when you want nominal identity at the constructor boundary.
- Open row variables are still an internal inference feature; user-written `...row` annotations remain restricted in v1.

## Codegen

- Anonymous structural record shapes are interned into stable Go struct types.
- Transparent exact-record names reuse the same structural representation.
- Constructor-bearing wrappers/sums keep explicit nominal lowering.
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
