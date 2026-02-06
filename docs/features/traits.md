# Traits

## Maintenance

- Last verified: 2026-02-06
- Implementation status: Canonical (actively maintained)
- Update trigger: Any language behavior, typechecker, or codegen change affecting this topic

## Scope

Traits provide ad-hoc polymorphism and constrained generic programming.

Capabilities:
- trait declarations,
- supertraits,
- implementations for concrete types,
- derive support for selected traits,
- method-call syntax lowering.

## Syntax

### Trait definition

```marmoset
trait show[a] {
  fn show(x: a) -> string
}

trait ord[a]: eq {
  fn compare(x: a, y: a) -> int
}
```

### Impl

```marmoset
impl show for int {
  fn show(x: int) -> string {
    "int"
  }
}
```

### Derive

```marmoset
derive eq, show, ord, hash for point;
```

### Method call use

```marmoset
let x = 42
puts(x.show())
```

## Sub-Features and Use Cases

- capability constraints in generic functions,
- operator-related behavior via builtin traits,
- reusable behavior across unrelated nominal/structural types,
- auto-derivation for boilerplate reductions.

## Type-System Semantics

Core components:
- trait registry for definitions and impls,
- validation for impl signature compatibility,
- solver checks for constrained type vars,
- method lookup by receiver type.

Resolution model:
- explicit impl registration (user + compiler-provided),
- method calls resolved to concrete impl at compile time in static mode,
- constrained generics require satisfiable trait bounds.

## Design Alternatives Considered

### Alternative A: Where-clause-heavy typeclass syntax

Pros:
- very expressive constraint language.

Cons:
- heavier syntax and parser/typechecker complexity.

### Alternative B: Structural method-only typing without explicit impls

Pros:
- lightweight surface model.

Cons:
- weaker coherence and conflict control.

### Alternative C: Explicit trait + impl registry model (Chosen)

Pros:
- predictable resolution path,
- explicit capabilities,
- strong validation points.

Cons:
- more declarations in user code.

## Codegen: Detailed Design

### Candidate approaches

1. Static free-function dispatch by resolved type (Chosen).
2. Vtable/trait-object dynamic dispatch default.
3. Reflection-based method dispatch.

### Approach 1 (Chosen)

Method calls lower to mangled free functions:
- `x.show()` -> `show_show_int64(x)` (example)

Generated code includes:
- user impl functions,
- builtin primitive impl helpers,
- derived impl helpers (including records for selected traits).

Lowering pipeline:
1. Parser captures trait/impl/derive declarations.
2. Typechecker registers traits and impls in trait registry.
3. Solver resolves constrained calls and validates bounds.
4. Emitter rewrites method-call syntax to concrete helper functions.

Representative lowering:

Marmoset:
```marmoset
trait show[a] {
  fn show(x: a) -> string
}

impl show for int {
  fn show(x: int) -> string { "int" }
}

let x = 42
puts(x.show())
```

Representative Go shape:
```go
func show_show_int64(x int64) string { return "int" }

func main() {
  x := int64(42)
  _ = puts(show_show_int64(x))
}
```

Pros:
- efficient static dispatch,
- transparent generated code,
- easy to test and reason about.

Cons:
- many generated helper names,
- dynamic polymorphism requires explicit future design if needed.

### Approach 2 (trait objects)

Pros:
- runtime polymorphism flexibility.

Cons:
- overhead, complexity, and less predictable performance.

### Approach 3 (reflection)

Pros:
- implementation convenience for prototypes.

Cons:
- weak type safety, high runtime overhead.

## Why Current Choice

Static dispatch aligns with performance goals and existing typechecker resolution model, while preserving generated-code clarity.

## Pros and Cons of Current Trait Model

Pros:
- strong compile-time checking,
- good performance,
- clear linkage between type and behavior.

Cons:
- no first-class trait-object story yet,
- derived behavior coverage is intentionally scoped.

## Related Docs

- `docs/features/functions-and-polymorphism.md`
- `docs/features/records.md`
- `docs/ROADMAP.md`
- `docs/archive/typechecker/phase4/milestone-3.md`

## Implementation Touchpoints

- Trait/impl/derive parsing: `lib/frontend/syntax/parser.ml`
- Trait registry and coherence checks: `lib/frontend/typecheck/trait_registry.ml`
- Constraint solving and impl lookup: `lib/frontend/typecheck/trait_solver.ml`
- Method-call typing and operator desugaring: `lib/frontend/typecheck/infer.ml`
- Typechecker orchestration and diagnostics: `lib/frontend/typecheck/checker.ml`
- Static dispatch helper emission: `lib/backend/go/emitter.ml`
