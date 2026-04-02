# Named Sums (`enum` Compatibility Sugar)

## Maintenance

- Last verified: 2026-03-30
- Implementation status: Canonical (actively maintained)
- Update trigger: Any language behavior, typechecker, or codegen change affecting this topic

## Scope

Constructor-bearing sum types provide algebraic/variant types with named constructors and optional payloads.

Canonical surface:
- `type Name = { Variant, Variant(Payload), ... }`
- `enum Name = { ... }` is still accepted as compatibility sugar for the same construct

Capabilities:
- generic enums,
- nullary and payload variants,
- multi-field variants,
- match-based consumption.

## Syntax

```marmoset
type Option[a] = {
  Some(a),
  None,
}

type Result[a, e] = {
  Success(a),
  Failure(e),
}

type HttpResponse = {
  Ok(Int, Str),
  Error(Int, Str),
}

let x = Option.Some(42)
let n = Option.None
```

Constructor calls are namespaced by the sum type name.

## Sub-Features and Use Cases

- explicit state/domain modeling,
- safer alternatives to ad-hoc sentinel/null protocols,
- strongly-typed branching with payload extraction.

## Type-System Semantics

Named-sum typing model:
- constructor-bearing `type` definitions register constructor metadata and type parameters,
- `enum` declarations lower to the same canonical sum representation,
- constructor calls validate arity and payload types,
- generic parameters instantiate from call-site evidence,
- match checking uses constructor domain information for exhaustiveness.

Namespacing model:
- constructors are qualified (`Option.Some`, `Result.Success`) to avoid cross-enum collisions.

## Design Alternatives Considered

### Alternative A: Union-only model

Pros:
- less syntax surface.

Cons:
- weak naming and domain semantics,
- harder payload discipline.

### Alternative B: Nominal sums with namespaced constructors (Chosen)

Pros:
- clear domain intent,
- payload structure enforced,
- good match ergonomics.

Cons:
- more declaration verbosity.

### Alternative C: Class-like sum encoding

Pros:
- OO familiarity.

Cons:
- heavier runtime model,
- noisier generated code.

## Codegen: Detailed Design

### Candidate approaches

1. Tagged structs with generated constructors and switch-based match lowering (Chosen).
2. Interface hierarchy and type switches.
3. Integer tags + external payload arrays/tables.

### Approach 1 (Chosen)

Runtime representation:
- one generated struct per sum type,
- explicit `Tag` field encodes active variant,
- payload fields represent variant data layout.

Lowering pipeline:
1. Typechecker stores sum metadata in the registry.
2. Emitter generates named-sum type/constructor helpers.
3. Constructor calls lower to helper invocations creating tagged values.
4. Match expressions lower to `switch value.Tag` plus payload extraction.

For heterogeneous and multi-field payloads:
- compute layout mapping so variant field positions map to concrete struct fields,
- preserve typed access for match bindings.

Representative lowering:

Marmoset:
```marmoset
type Option[a] = {
  Some(a),
  None,
}

fn to_int(x: Option[Int]) -> Int = match x {
  case Option.Some(v): v
  case Option.None: 0
}
```

Representative Go shape:
```go
type Option_int64 struct {
  Tag int
  V0  int64
}

func option_some_int64(v int64) Option_int64 { return Option_int64{Tag: 0, V0: v} }
func option_none_int64() Option_int64 { return Option_int64{Tag: 1} }

func to_int(x Option_int64) int64 {
  switch x.Tag {
  case 0:
    v := x.V0
    return v
  default:
    return int64(0)
  }
}
```

Pros:
- predictable runtime behavior,
- explicit and inspectable generated code,
- avoids opaque interface-heavy dispatch for core named-sum operations.

Cons:
- layout logic complexity,
- larger generated types when variants differ significantly.

### Approach 2 (Interface hierarchy)

Pros:
- naturally models per-variant types.

Cons:
- dynamic dispatch overhead,
- more generated boilerplate.

### Approach 3 (external payload tables)

Pros:
- compact tagged storage idea.

Cons:
- unsafe/complex indexing discipline,
- poor maintainability.

## Why Current Choice

Tagged struct lowering gives a strong balance of static structure, runtime efficiency, and manageable backend complexity.

## Pros and Cons of Current Sum Model

Pros:
- expressive ADT-style modeling,
- good match integration,
- performant static-ish codegen.

Cons:
- backend layout logic is non-trivial,
- further optimization opportunities remain.

## Related Docs

- `docs/features/pattern-matching.md`
- `docs/features/unions.md`
- `docs/ROADMAP.md`

## Implementation Touchpoints

- Enum/pattern syntax parsing: `lib/frontend/syntax/parser.ml`
- Enum type constructors and helpers: `lib/frontend/typecheck/types.ml`
- Enum declaration/constructor registry: `lib/frontend/typecheck/enum_registry.ml`
- Enum constructor + match inference: `lib/frontend/typecheck/infer.ml`
- Exhaustiveness validation: `lib/frontend/typecheck/exhaustiveness.ml`
- Enum type/match emission: `lib/backend/go/emitter.ml`
