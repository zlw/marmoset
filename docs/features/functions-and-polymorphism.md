# Functions and Polymorphism

## Maintenance

- Last verified: 2026-02-28
- Implementation status: Canonical (actively maintained)
- Update trigger: Any language behavior, typechecker, or codegen change affecting this topic

## Scope

This doc covers:

- function declaration/expression forms,
- function values (assign, pass, return, store),
- lexical closures,
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
- function values passed to higher-order APIs,
- closure-producing factories (`make_adder` style),
- recursive functions,
- generic utilities,
- constrained generic APIs via traits.

## Locked Decisions (2026-02-27)

1. Polymorphism model:
- Rank-1 HM remains the type-system baseline.
- Polymorphic function values are supported.
- True rank-N polymorphism is explicitly deferred.

2. Closure/backend model:
- Use lambda lifting with explicit environment structs in codegen.
- Nested functions are lowered to lifted helpers that receive an explicit environment parameter.
- Closure values are built from environment construction plus lifted function binding.

3. Capture semantics:
- Hybrid policy for forward compatibility:
- immutable captures are treated as value captures,
- mutable captures (future feature) must use shared reference cells.

4. Function traits/operators:
- Function types cannot satisfy `eq`, `ord`, or `hash`.
- `show` on function values is allowed and returns a stable placeholder representation.

5. Empty collection literals in typed contexts:
- Codegen must emit typed empty literals (`[]T{}`, `map[K]V{}`) when expected type is known.
- Untyped empty literals must be resolved before codegen; unresolved empties are a type/codegen error.

## Type-System Semantics

- HM inference with let-generalization.
- instantiation of polymorphic values at use sites.
- annotation compatibility checks where provided.
- Rank-1 only:
- no explicit `forall` syntax in user type annotations,
- no higher-rank parameter annotations in v1.

Polymorphic function values under rank-1:
- let-bound polymorphic functions can be passed as values.
- each use site instantiates type variables independently.
- function parameters themselves are monomorphic within one instantiated call path.

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

### Alternative D: Full rank-N now (Deferred)

Pros:
- maximum expressive power for function-typed APIs.

Cons:
- large parser/typechecker redesign cost,
- significantly higher inference and diagnostics complexity.

## Codegen: Detailed Design

### Candidate approaches

1. AOT monomorphization + lambda lifting + env structs (Chosen).
2. Backend generics passthrough (Go generics-based).
3. Defunctionalization/runtime callable objects.

### Approach 1 (Chosen)

- collect concrete call-site instantiations,
- emit specialized function variants with mangled names,
- lift nested functions to top-level helpers with explicit env parameters,
- lower closure creation to env construction + lifted binding.

Pros:
- predictable runtime performance,
- static dispatch in generated code.

Cons:
- specialization growth possible,
- backend implementation complexity.

Pipeline details:

1. Frontend infers/checks function bodies and call sites and provides typed map facts.
2. Backend collects function definitions (including nested functions) and free-variable capture sets.
3. Backend collects concrete instantiations from typed call sites and function-value flows.
4. Backend emits lifted helpers:
   `fn_lifted_<mangle>(env <env-type>, args...) -> ret`.
5. Backend emits closure construction sites with explicit env materialization.
6. Backend rewrites direct calls and function-value references to specialized symbols.
7. Backend emits typed empty literals using expected element/key/value types.

Representative specialization shape:
```go
func id_int64(x int64) int64 { return x }
func id_string(x string) string { return x }
```

Representative lifted closure shape:
```go
type __env_make_adder_int64 struct { x int64 }
func make_adder_inner_lifted_int64(env __env_make_adder_int64, y int64) int64 { return env.x + y }
```

Function identifier as value:
- `map(numbers, fib)` must lower to specialized function symbol (`fib_int64`) rather than unspecialized `fib`.
- this rule applies recursively in lifted/nested contexts.

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
- harder to preserve simple static dispatch semantics.

## Why Current Choice

Chosen approach keeps static performance and deterministic lowering while fixing current closure/HOF gaps without introducing a new runtime object model.

## Pros and Cons of Current Model

Pros:
- high performance on specialized paths,
- strong static type guarantees,
- explicit generic constraints where needed,
- closure behavior is explicit in lowered representation.

Cons:
- specialization explosion risk still exists for heavy HOF programs,
- true rank-N polymorphism remains deferred.

## Function Value Semantics (Binding)

Supported:
- assign function values to let bindings,
- pass function values as arguments,
- return function values from functions,
- capture lexical variables in nested function values,
- use let-polymorphic function values under rank-1 instantiation.

Not supported (deferred):
- explicit rank-N polymorphism syntax/typing,
- eq/ord/hash operations over function values.

## Regression Coverage Highlights

- Integration tests validate higher-order call paths across pure/effectful callbacks.
- Integration tests validate closure capture in nested/lifted function scenarios.
- Operator constraints over function types are checked (`eq`/`ord`/`hash` rejected, `show` allowed).
- Codegen tests validate deterministic specialization and typed empty-literal emission for array/map contexts.

## Related Docs

- `docs/features/traits.md`
- `docs/features/type-annotations-and-aliases.md`
- `docs/ROADMAP.md`

## Implementation Touchpoints

- Function/generic parsing: `lib/frontend/syntax/parser.ml`
- HM inference/generalization/instantiation: `lib/frontend/typecheck/infer.ml`
- Backend specialization and mangling: `lib/backend/go/emitter.ml`
