# Marmoset Type System Plan

A gradual approach: start with Hindley-Milner inference, then extend with bidirectional features for advanced type system capabilities.

## Implementation Notes

**Go slow!** This is a learning project. After each implementation step:
- Stop and explain what was done, why, and how
- Wait for questions before moving on
- Suggest questions if concepts are non-obvious
- The process matters more than speed

**Don't assume!** Just because current Marmoset (dynamic, interpreted) doesn't have a feature doesn't mean we don't want it in typed/compiled Marmoset. Features to keep in mind:
- Operator overloading (via traits)
- Method syntax
- Pattern matching
- Partial application
- Named/optional arguments
- Whatever else makes sense for a modern language

## Phase 1: Hindley-Milner Core

Full type inference for basic types. No annotations required.

### Types
- [x] `Int` - 64-bit integers
- [x] `Float` - floating point numbers
- [x] `Bool` - booleans
- [x] `String` - strings
- [x] `Null` - null/unit type
- [x] `T -> U` - function types
- [x] `[T]` - array types (homogeneous)
- [x] `{K: V}` - hash/map types

### Features
- [x] Type variables (`'a`, `'b`, etc.)
- [x] Unification algorithm
- [x] Let-polymorphism (generalization at `let` boundaries)
- [x] Instantiation (fresh type variables when using polymorphic values)
- [x] Occurs check (prevent infinite types)

### Implementation
- [x] `Type` module - type representation
- [x] `Infer` module - constraint generation
- [x] `Unify` module - unification algorithm
- [x] `Typecheck` module - main entry point
- [x] Integration with existing AST

### Example (Phase 1 complete)
```
let id = fn(x) { x };           // inferred: ∀a. a -> a
let add = fn(a, b) { a + b };   // inferred: Int -> Int -> Int
let first = fn(arr) { arr[0] }; // inferred: ∀a. [a] -> a

id(5);        // works: Int
id("hello");  // works: String
```

---

## Phase 2: Bidirectional Extensions

Add optional type annotations. HM still works, but annotations enable advanced features.

### Syntax
- [ ] Let bindings: `let x: Int = 5;`
- [ ] Function params: `fn(x: Int, y: Int) { ... }`
- [ ] Function return: `fn(x: Int): Int { ... }`
- [ ] Generic params: `fn<T>(x: T): T { ... }`

### Features
- [ ] Annotation checking (inferred type must match annotation)
- [ ] Bidirectional mode at annotation boundaries
- [ ] Type ascription expressions: `(expr : Type)`

### Implementation
- [ ] Extend parser for type annotations
- [ ] Extend AST with type annotation nodes
- [ ] Bidirectional `check` mode (when expected type is known)
- [ ] Subsumption checking (is inferred type compatible with annotation?)

### Example (Phase 2 complete)
```
// Annotations optional, but allowed
let id: fn(Int): Int = fn(x) { x };  // constrained to Int -> Int
let add = fn(a: Int, b: Int): Int { a + b };

// Explicit generics
let identity = fn<T>(x: T): T { x };
```

---

## Phase 3: Advanced Types

Features that require annotations (HM can't infer these).

### Union Types
- [ ] Syntax: `Int | String`, `T | Null`
- [ ] Type narrowing in conditionals
- [ ] Exhaustiveness checking

### Enums / Variants (Rust/OCaml-style tagged unions)
- [ ] Syntax: `enum Option<T> { Some(T), None }`
- [ ] Constructors as functions: `Some(5)`
- [ ] Pattern matching to extract values
- [ ] Exhaustiveness checking in match

### Literal Types
- [ ] String literals: `"hello"` as a type
- [ ] Integer literals: `5` as a type
- [ ] Boolean literals: `true`, `false` as types

### Traits / Type Classes
- [ ] Trait definition: `trait Show { fn show(self): String }`
- [ ] Trait implementation: `impl Show for Int { ... }`
- [ ] Trait bounds: `fn<T: Show>(x: T): String`
- [ ] Built-in traits: `Eq`, `Ord`, `Show`, `Add`, etc.

### Example (Phase 3 complete)
```
// Union types
let parse = fn(s: String): Int | Error { ... };
let result = parse("42");
if (result is Int) {
    // result narrowed to Int here
}

// Literal types
let direction: "left" | "right" = "left";

// Traits
trait Show {
    fn show(self): String
}

let print = fn<T: Show>(x: T) { puts(x.show()) };
```

---

## Phase 4: Advanced Features (Future)

Ambitious features for later exploration.

### Higher-Kinded Types (HKT)
- [ ] Type constructors: `Functor`, `Monad`
- [ ] Syntax TBD: `fn<F<_>: Functor, A, B>(fa: F<A>, f: fn(A): B): F<B>`

### Existential Types
- [ ] Pack/unpack existentials
- [ ] Syntax TBD: `exists T. { value: T, show: fn(T): String }`

### Dependent Types (Maybe)
- [ ] Types depending on values
- [ ] Refinement types: `{ x: Int | x > 0 }`

### Row Polymorphism (Maybe)
- [ ] Extensible records
- [ ] Structural typing for hashes

---

## Integration Points

### Where does typechecking run?
- After parsing, before evaluation/compilation
- `parse -> typecheck -> eval` or `parse -> typecheck -> compile -> vm`

### Error Reporting
- Source locations in error messages
- "Expected X, got Y" format
- Suggestions for common mistakes

### Interaction with Existing Code
- Typechecker is optional initially (can run untyped)
- Gradual: untyped code interops with typed code via `any`/`unknown`?

---

## Resources

- [Algorithm W Step by Step](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.65.7733&rep=rep1&type=pdf)
- [Write You a Haskell - Hindley-Milner](http://dev.stephendiehl.com/fun/006_hindley_milner.html)
- [Bidirectional Typing](https://arxiv.org/abs/1908.05839)
- [Simple Easy! (bidirectional dependent types)](https://www.andres-loeh.de/LambdaPi/LambdaPi.pdf)
