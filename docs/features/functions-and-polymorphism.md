# Functions and Polymorphism

## Maintenance

- Last verified: 2026-02-06
- Implementation status: Canonical (actively maintained)
- Update trigger: Any language behavior, typechecker, or codegen change affecting this topic

## Scope

This doc covers:

- function declaration/expression forms,
- let-polymorphism (HM style),
- generics and constraints in function signatures,
- call-site specialization strategy in codegen.

## Syntax

```marmoset
let inc = fn(x: int) -> int { x + 1 }

let id = fn[a](x: a) -> a { x }

let show_if_equal = fn[a: show + eq](x: a, y: a) -> string {
  if (x.eq(y)) { x.show() } else { "different" }
}
```

## Sub-Features and Use Cases

- first-class functions,
- recursive functions,
- generic utilities,
- constrained generic APIs via traits.

## Type-System Semantics

- HM inference with let-generalization.
- instantiation of polymorphic values at use sites.
- annotation compatibility checks where provided.

## Design Alternatives Considered

### Alternative A: Fully explicit polymorphism only

Pros:
- predictable, explicit generic boundaries.

Cons:
- verbose,
- poor scripting ergonomics.

### Alternative B: HM-only with no generic syntax

Pros:
- concise and elegant in simple cases.

Cons:
- hard to express intentional constraints/contracts.

### Alternative C: HM baseline + optional explicit generic syntax (Chosen)

Pros:
- good ergonomics + control,
- smooth path to constrained polymorphism.

Cons:
- more complex parser/typechecker surface.

## Codegen: Detailed Design

### Candidate approaches

1. Full ahead-of-time monomorphization (Chosen baseline).
2. Backend generics passthrough (Go generics-based).
3. Dynamic dispatch/boxing for polymorphism.

### Approach 1 (Chosen baseline)

- collect concrete call-site instantiations,
- emit specialized function variants with mangled names.

Pros:
- predictable runtime performance,
- static dispatch in generated code.

Cons:
- specialization growth possible,
- higher-order polymorphic cases need careful handling.

Pipeline details:

1. Frontend infers/checks function bodies and call sites.
2. Backend collects function definitions.
3. Backend collects concrete instantiations from typed call sites.
4. Backend emits specialized Go functions and rewrites calls to mangled names.

Representative specialization shape:
```go
func id_int64(x int64) int64 { return x }
func id_string(x string) string { return x }
```

### Approach 2 (Go generics passthrough)

Pros:
- less backend specialization logic.

Cons:
- less direct control/visibility over final specialization strategy.

### Approach 3 (dynamic dispatch)

Pros:
- simpler in some polymorphic edge cases.

Cons:
- runtime overhead and weaker static guarantees.

## Why Current Choice

Static specialization aligns with the current performance and predictability goals while keeping emitted code explicit and analyzable.

## Pros and Cons of Current Model

Pros:
- high performance on specialized paths,
- strong static type guarantees,
- explicit generic constraints where needed.

Cons:
- advanced HOF specialization remains an area for improvement,
- backend complexity grows with feature interactions.

## Related Docs

- `docs/features/traits.md`
- `docs/features/type-annotations-and-aliases.md`
- `docs/ROADMAP.md`

## Implementation Touchpoints

- Function/generic parsing: `lib/frontend/syntax/parser.ml`
- HM inference/generalization/instantiation: `lib/frontend/typecheck/infer.ml`
- Backend specialization and mangling: `lib/backend/go/emitter.ml`
