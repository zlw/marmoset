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

### Result Handling with `try` (Zig-style)
- [ ] `try expr` - propagate error, like Rust's `?` but prefix
- [ ] `try expr else default` - provide default on error
- [ ] `try expr else |err| handler` - transform/handle error

```
// Propagate error
let user = try fetch_user(id)

// Provide default
let user = try fetch_user(id) else default_user

// Transform error
let user = try fetch_user(id) else |err| custom_error(err)

// Handle inline
let user = try fetch_user(id) else {
    log("failed")
    return err(not_found)
}

// Chaining
fn process(id: int) => result[string, error] {
    let user = try fetch_user(id)
    let order = try fetch_order(user.order_id)
    let item = try fetch_item(order.item_id)
    ok(item.name)
}
```

Why `try` over `?`:
- Reads left-to-right (see `try` before expression)
- `else` clause is natural English
- Harder to miss than suffix `?`
- Zig has proven it works well

### Literal Types
- [ ] String literals: `"hello"` as a type
- [ ] Integer literals: `5` as a type
- [ ] Boolean literals: `true`, `false` as types

### Traits / Type Classes (Rust + TypeScript hybrid)
- [ ] Trait definition with methods: `trait Show { show: (self) -> string }`
- [ ] Trait definition with fields: `trait HasName { name: string }` (TS-like)
- [ ] Trait with both: `trait Entity { id: int, show: (self) -> string }`
- [ ] Trait composition: `trait PrintableEntity : HasName + Show { }`
- [ ] Impl blocks: `impl user { ... }` for methods
- [ ] Trait impl: `impl Show for user { ... }`
- [ ] Foreign type impl: `impl Show for library.foreign_type { ... }`
- [ ] Generic impl: `impl Show for list[a] where a: Show { ... }`
- [ ] Trait bounds: `fn print(x: a) where a: Show`
- [ ] Built-in traits: `Eq`, `Ord`, `Show`, `Add`, etc.

See `approach.md` for full details on the impl-based dispatch model.

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

### Existential Types (Trait Objects)
- [ ] Syntax: `dyn Trait` for type-erased trait objects
- [ ] Runtime dispatch via vtable (compiles to Go interface)
- [ ] Heterogeneous collections: `list[dyn Show]`
- [ ] Static dispatch by default, dynamic only when explicit

```
// Static dispatch (default) - zero cost, monomorphized
fn print(x: a) where a: Show { x.show().puts() }

// Dynamic dispatch (explicit) - runtime vtable
fn print_dyn(x: dyn Show) { x.show().puts() }

// Heterogeneous list
let items: list[dyn Show] = [42, "hello", my_user]
```

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

## Architectural Decisions

Key decisions made about the overall language design and implementation.

### Numeric Types (Int / Float)

**Decision:** Strict typing, no implicit coercion.

- `1 * 1.0` is a **type error** (Int and Float don't mix implicitly)
- Follows OCaml/Rust/Gleam approach, not Ruby/Scala/Python
- When we add typeclasses, numeric literals will become polymorphic (Haskell-style):
  - `1` will have type `Num a => a`, not `Int`
  - `1 * 1.0` will work because both unify to `Float`
  - But `(x: Int) * (y: Float)` will still be an error
- This gives convenience for literals + safety for variables

### Compilation Target

**Decision:** Compile to Go source code.

Why Go:
- Excellent runtime (fast GC, low latency)
- Goroutines/scheduler built-in (for future concurrency)
- Static binaries, easy cross-compilation
- Decent ecosystem

Why not:
- LLVM: Too much work (need GC, everything from scratch)
- JVM: No true native binaries (GraalVM native-image is heavy)
- Rust/OCaml: Would work but Go runtime is simpler

### Monomorphization vs Boxing

**Decision:** Monomorphization (not Go generics).

- Full monomorphization at compile time
- Each polymorphic function generates specialized versions per concrete type
- Go becomes a "typed assembly" target, not a constraint on our type system

Why not Go generics:
- Go generics are limited (no HKT, no existentials, only invariance)
- Go's "GC shape stenciling" isn't full monomorphization
- We'd fight the system constantly

Benefits:
- Best runtime performance (no type switches, no boxing)
- Generated Go can be faster than hand-written Go (we know more than Go's compiler)
- Our type system isn't limited by Go's type system

### Compilation Model

**Decision:** Whole-program compilation.

- Compiler sees all code at once
- Knows every call site, every type instantiation
- Single compilation unit -> single binary

Implications:
- MRS -> MRS: Works naturally (we see all instantiations)
- MRS -> Go: Works (Go functions have concrete types, just need FFI declarations)
- Go -> MRS: Not supported in v1; v2 will have explicit `@export` annotations

Why not separate compilation:
- With monomorphization, you need to know all instantiations
- Would require shipping typed AST/IR for libraries
- Whole-program is simpler, better optimizations
- Can revisit later if needed

### Typeclasses / Traits

**Decision:** Rust-like `impl` blocks with TypeScript-like structural traits.

**Traits:**
- Can have methods AND fields (unlike Rust, like TypeScript)
- Structural satisfaction (like Go interfaces)
- Composition with `:`

**Dispatch:**
- Static by default (monomorphization)
- Dynamic when explicit (`dyn Trait`) - compiles to Go interface

**Impl blocks:**
- One syntax for everything
- `impl Type { ... }` - add methods
- `impl Trait for Type { ... }` - implement trait
- `impl foreign.Type { ... }` - extend foreign types (same syntax!)

**Union types + traits:**
- Union satisfies trait if ALL variants satisfy it
- Monomorphize when concrete type known at compile time
- Runtime dispatch (type switch) when unknown

Comparison:
| Language | Mechanism | Dispatch | Fields in traits? |
|----------|-----------|----------|-------------------|
| Haskell | Typeclasses | Dictionary passing | No |
| Rust | Traits + impl | Monomorphization | No |
| Swift | Protocols | Mix | Yes |
| TypeScript | Interfaces | Structural | Yes |
| **Marmoset** | **Traits + impl** | **Mono + dyn** | **Yes** |

See `approach.md` for full details and rationale.

---

## Phase 5: Effect System (Future)

Track side effects in the type system. Pure functions can't do IO.

### Inspirations to Explore
- **Koka** - algebraic effects, handlers
- **Unison** - abilities (effects with a nice name)
- **Flix** - effect polymorphism
- **Roc** - effects in signature (`->` vs `=>`), not as a type
- **Haskell** - IO monad (proven but verbose)

### Possible Directions

**Option A: Effect as type (Haskell/Flix style)**
```
let readFile: String -> IO[String]
let pure: fn(x: Int): Int { x + 1 }        // no effect
let impure: fn(x: Int): IO[Int] { print(x); x }  // has IO effect
```

**Option B: Arrow annotation (Roc style)**
```
let pure: Int -> Int
let impure: Int => Int   // => means effectful
```

**Option C: Effect polymorphism**
```
let map: fn<e>(f: a -> e b, xs: [a]): e [b]  // effect-polymorphic
```

### Concurrency Effects
- Structured concurrency (no leaked goroutines)
- Async/await or similar
- Compiles down to goroutines + channels
- User never sees the ugly Go parts

TBD: Need to explore options and find the right balance of power vs simplicity.

---

## Language Inspirations

Marmoset sits at the intersection of several languages:

### Type System
| Feature | Inspiration |
|---------|-------------|
| HM inference | OCaml, Haskell, ML family |
| Bidirectional checking | Scala, Rust |
| Typeclasses | Haskell, Rust traits, Scala givens |
| Polymorphic literals | Haskell |
| Strict numerics | OCaml, Rust, Gleam |
| ADTs + pattern matching | ML, Haskell, Rust |

### Runtime / Compilation
| Feature | Inspiration |
|---------|-------------|
| Compile to other lang | Scala->JVM, Flix->JVM, Gleam->BEAM/JS |
| Go as target | Novel (Go runtime without Go language) |
| Monomorphization | Rust, C++ |
| Whole-program compilation | MLton |

### Philosophy
| Feature | Inspiration |
|---------|-------------|
| FP-first, no OO | Flix, Gleam, Elm |
| Effect tracking | Koka, Unison, Flix, Roc |
| Pragmatic FP | Scala, F#, OCaml |

### Closest Relatives
- **Flix**: Scala - OO + effects, on JVM
- **Marmoset**: Scala - OO + effects, on Go

### Design Tension

PL-theory-nerd language... but on Go runtime. Need to balance:

- **Nerdy**: Typeclasses, effects, HM inference, monomorphization
- **Pragmatic**: Simple syntax, good errors, fast compile, easy deploy

There are lots of "balanced" languages already. Maybe it's okay to be a bit nerdy, as long as the developer experience is good. The Go runtime gives us pragmatic deployment benefits even if the type system is sophisticated.

TBD: Find the right balance as we go.

---

## Resources

- [Algorithm W Step by Step](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.65.7733&rep=rep1&type=pdf)
- [Write You a Haskell - Hindley-Milner](http://dev.stephendiehl.com/fun/006_hindley_milner.html)
- [Bidirectional Typing](https://arxiv.org/abs/1908.05839)
- [Simple Easy! (bidirectional dependent types)](https://www.andres-loeh.de/LambdaPi/LambdaPi.pdf)
