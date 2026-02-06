# Enums

## Maintenance

- Last verified: 2026-02-06
- Implementation status: Canonical (actively maintained)
- Update trigger: Any language behavior, typechecker, or codegen change affecting this topic

## Scope

Enums provide algebraic/variant types with named constructors and optional payloads.

Capabilities:
- generic enums,
- nullary and payload variants,
- multi-field variants,
- match-based consumption.

## Syntax

```marmoset
enum option[a] {
  some(a)
  none
}

enum result[a, e] {
  success(a)
  failure(e)
}

enum http_response {
  ok(int, string)
  error(int, string)
}

let x = option.some(42)
let n = option.none
```

Constructor calls are namespaced by enum name.

## Sub-Features and Use Cases

- explicit state/domain modeling,
- safer alternatives to ad-hoc sentinel/null protocols,
- strongly-typed branching with payload extraction.

## Type-System Semantics

Enum typing model:
- enum definitions register constructor metadata and type parameters,
- constructor calls validate arity and payload types,
- generic parameters instantiate from call-site evidence,
- match checking uses constructor domain information for exhaustiveness.

Namespacing model:
- constructors are qualified (`option.some`, `result.success`) to avoid cross-enum collisions.

## Design Alternatives Considered

### Alternative A: Union-only model

Pros:
- less syntax surface.

Cons:
- weak naming and domain semantics,
- harder payload discipline.

### Alternative B: Nominal enums with namespaced constructors (Chosen)

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
- one generated struct per enum type,
- explicit `Tag` field encodes active variant,
- payload fields represent variant data layout.

Lowering pipeline:
1. Typechecker stores enum metadata in enum registry.
2. Emitter generates enum type/constructor helpers.
3. Constructor calls lower to helper invocations creating tagged values.
4. Match expressions lower to `switch value.Tag` plus payload extraction.

For heterogeneous and multi-field payloads:
- compute layout mapping so variant field positions map to concrete struct fields,
- preserve typed access for match bindings.

Representative lowering:

Marmoset:
```marmoset
enum option[a] { some(a) none }

let to_int = fn(x: option[int]) -> int {
  match x {
    option.some(v): v
    option.none: 0
  }
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
- avoids opaque interface-heavy dispatch for core enum operations.

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

## Pros and Cons of Current Enum Model

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
- `docs/archive/typechecker/phase4/milestone-2.md`

## Implementation Touchpoints

- Enum/pattern syntax parsing: `lib/frontend/syntax/parser.ml`
- Enum type constructors and helpers: `lib/frontend/typecheck/types.ml`
- Enum declaration/constructor registry: `lib/frontend/typecheck/enum_registry.ml`
- Enum constructor + match inference: `lib/frontend/typecheck/infer.ml`
- Exhaustiveness validation: `lib/frontend/typecheck/exhaustiveness.ml`
- Enum type/match emission: `lib/backend/go/emitter.ml`
