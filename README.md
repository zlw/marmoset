## Monkey Language but in OCaml 🐵 🐫
* [Writing Interpreter in Go](https://interpreterbook.com/) / [Writing Compiler in Go](https://compilerbook.com/)'s Monkey Language but in [OCaml](https://ocaml.org/)
* [Marmoset](https://en.wikipedia.org/wiki/Marmoset) is a small monkey, sometimes called Pug Monkey, and I do like pugs 🙈

## Installation

First you need to install OCaml, opam and dune:
* [official instruction](https://ocaml.org/docs/installing-ocaml)

Then you can clone this repository and run the following commands:

```sh
git clone git@github.com:zlw/marmoset.git
```

And then install the dependencies:
```sh
make install
```

## Build

### Dev
faster compilation, slower runtime performance
```sh
make build
```

### Release
slower compilation, faster runtime performance
```sh
make release
```

## Run tests

```sh
make unit
```

## Progress

- [x] Lexer
- [x] Parser
- [x] Evaluator
- [x] Compiler

## Features

| Feature                | Interpreter | Compiler |
|------------------------|:-----------:|:--------:|
| Bindings               | ✅          | ✅       |
| Conditionals           | ✅          | ✅       |
| Strings                | ✅          | ✅       |
| Integers               | ✅          | ✅       |
| Arithmetic +-/*        | ✅          | ✅       |
| Arrays                 | ✅          | ✅       |
| Hashes                 | ✅          | ✅       |
| Functions              | ✅          | ✅       |
| First class functions  | ✅          | ✅       |
| Higher order functions | ✅          | ✅       |
| Closures               | ✅          | ✅       |
| Recursion              | ✅          | ✅       |
| Built-In Functions     | ✅          | ✅       |
| Macros                 | ❌          | ❌       |

## Additional Features (not present in the original implementation)
| Feature                | Interpreter | Compiler |
|------------------------|:-----------:|:--------:|
| Floats                 | ✅          | ✅       |
| Float Arithmetic       | ✅          | ✅       |
| String Indexing        | ✅          | ✅       |
| String Concatenation   | ✅          | ✅       |
| String Equality        | ✅          | ✅       |
| Negative Indexing      | ✅          | ✅       |
| Comments               | ✅          | ✅       |

## Compiler & VM Implementation Notes

The compiler and VM follow "Writing a Compiler in Go" by Thorsten Ball, but with some OCaml-specific considerations.

### Why the code looks "imperative"

While OCaml excels at functional programming, the VM implementation uses mutation in a few places for performance:

- **Compiler**: Uses `Dynarray` (dynamic arrays) and `Buffer` for building bytecode, and mutable fields for tracking compilation state
- **VM**: Uses mutable arrays for the stack, globals, and call frames

This is intentional. A purely functional VM with immutable data structures would allocate more and be slower. The mutation is localized and doesn't leak into the rest of the codebase.

### Performance

Recursive Fibonacci benchmark (`fib(35)`):

| Implementation | Go | OCaml |
|----------------|------|-------|
| Tree-walking interpreter | ~8.8s | ~4.9s |
| Bytecode VM | ~2.8s | ~2.0s |

OCaml's tree-walker is ~1.8x faster than Go's thanks to efficient pattern matching and algebraic data types. The bytecode VM is ~29% faster than Go after optimization:

1. **`[@inline]` hints on hot functions** - The biggest win. OCaml's compiler is conservative about inlining; Go is more aggressive. Adding `[@inline]` to `push`, `pop`, `current_frame`, `execute_binary_op`, etc. gave ~16% speedup.

2. **`Bytes.unsafe_get` / `Array.unsafe_get`** - Skip bounds checking in the VM loop. Safe because we trust our own compiler's bytecode. ~3% speedup.

3. **`Obj.magic` for opcode dispatch** - Convert int to opcode variant without pattern matching through `of_int`. OCaml represents simple variants as integers internally, so this is safe. Preserves exhaustiveness checking in the main dispatch.

### Why OCaml's tree-walker is so fast

OCaml's interpreter is ~1.8x faster than Go's because:
- Pattern matching compiles to efficient jump tables
- Algebraic data types have no runtime type assertions
- The GC is optimized for functional allocation patterns

This means the VM has less relative speedup over the interpreter compared to Go, but in absolute terms both implementations are fast.

## TODO

- [x] Cleanup pyramid of doom in `Parser`
    - [x] Use `Result.bind`
    - [x] Propagate parsing errors instead of crashing
- [ ] Add system tests
  - [ ] test runner
  - [ ] test cases (maybe reuse some from Crafting Interpreters?)
- [x] Add support for negative array indexing
- [x] Add support for string indexing
  - [x] Positive
  - [x] Negative
- [x] Add support for Floats
  - [x] Support Integer/Float arithmetic (currently there's Int/Int and Float/Float)

## Ideas

Monkey supports closures and first class functions. It would be interesting to add some functional programming features to it:
- [ ] immutability
- [ ] static typing with Hindley–Milner style type inference
- [ ] pattern matching

We those in place, it would be a JS-looking language with a ML core 🤔

### Type Safety

Currently, out-of-bounds array access and missing hash keys return `null` (matching canonical Monkey). This is a footgun in dynamic languages.

When adding a static type system, consider:

1. **Option types**: `arr[i]` returns `Option<T>`, forces explicit `match`/`unwrap`
2. **Dependent types**: Prove bounds at compile time (e.g., `Vec<T, N>` where index must be `< N`)
3. **Refinement types**: `arr[i]` where `i : { n : Int | 0 <= n < len(arr) }`
4. **Gradual typing**: Allow both safe (`arr.get(i) -> Option`) and unsafe (`arr[i] -> T`) with different syntax

Interesting type system concepts to explore:
- **Hindley-Milner** type inference (ML, Haskell)
- **Bidirectional type checking** (modern approach, easier to implement)
- **Algebraic data types** with exhaustiveness checking
- **Row polymorphism** for extensible records/hashes
- **Effect systems** for tracking errors, IO, etc.
- **Linear/affine types** for resource management (Rust-style ownership)
- **Dependent types** (Idris, Agda) - types that depend on values

Resources:
- "Types and Programming Languages" (Pierce) - the bible
- "Practical Foundations for Programming Languages" (Harper)
- Bidirectional typing: https://arxiv.org/abs/1908.05839
