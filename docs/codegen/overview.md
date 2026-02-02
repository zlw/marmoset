# Code Generation Overview

## Architecture

```
Source → Lexer → Parser → AST → Typecheck → Typed AST
                                                 │
                                    ┌────────────┴────────────┐
                                    ▼                         ▼
                              backend/vm/               backend/go/
                                    │                         │
                                    ▼                         ▼
                              Bytecode VM              Go source code
                              (interpreted)                   │
                                                              ▼
                                                         go build
                                                              │
                                                              ▼
                                                      Native binary
```

## Backends

### VM Backend (Interpreted)

**Use case:** REPL, scripts, development

**Flow:**
```
Typed AST → Bytecode compiler → Bytecode → VM execution
```

**Characteristics:**
- Fast startup (no external compile step)
- Good for interactive development
- Slower execution than native

**Files:**
```
lib/backend/vm/
├── compiler.ml      # AST → Bytecode
├── code.ml          # Bytecode definitions
├── symbol_table.ml  # Symbol management
└── machine.ml       # Bytecode interpreter

lib/backend/interpreter/
├── value.ml         # Runtime values
├── env.ml           # Environments
├── eval.ml          # Tree-walking interpreter
└── builtins.ml      # Built-in functions (runtime)
```

### Go Backend (Compiled)

**Use case:** Production builds, distribution

**Flow:**
```
Typed AST → Go codegen → Go source → go build → Native binary
```

**Characteristics:**
- Slower build (includes Go compilation)
- Fast execution (native code)
- Single binary output
- Go runtime (GC, goroutines)

**Files:**
```
lib/backend/go/
├── emitter.ml       # AST → Go source
└── runtime.go       # Runtime support (copied to output)
```

## User Experience

### Commands

```bash
# REPL - uses VM
marmoset

# Run script - uses VM
marmoset script.mr

# Build binary - uses Go codegen
marmoset build                           # uses main.mr, outputs ./bin/main
marmoset build -o myapp                  # uses main.mr, outputs ./myapp

# Debug: emit Go source
marmoset build app.mr --emit-go ./debug/ # uses app.mr, outputs Go files to ./debug/
```

### File Visibility

Go files are **hidden by default** (like JVM bytecode for Scala/Clojure):

```bash
$ marmoset build
Building main.mr...
Done: ./bin/main

$ ls
main.mr
bin/main           # just the binary, no .go files
```

Internally uses temp directory:
```
/tmp/marmoset-XXXXX/
├── main.go
├── runtime.go
└── go.mod
→ go build -o /original/path/app
→ rm -rf /tmp/marmoset-XXXXX
```

Use `--emit-go` to inspect generated code when debugging.

## Intermediate Representation

### Current: Typed AST

We use the Typed AST as our shared representation between backends:

```
         Typed AST
              │
   ┌──────────┴──────────┐
   ▼                     ▼
VM backend          Go backend
```

**Why no separate IR (for now):**

| Approach | Pros | Cons |
|----------|------|------|
| Typed AST as IR | Simple, one representation | Harder to optimize |
| Separate IR | Easier optimization | More complexity |

**Real compilers use IR for:**
- Optimization passes
- Analysis (borrow checking, etc.)
- Backend similarity

**We don't need it yet because:**
- Not doing heavy optimization
- Backends are quite different (stack VM vs structured Go)
- Typed AST has all the info we need

### Future: MIR (Maybe)

If we need optimization later, we'd add a middle IR:

```
Typed AST → MIR → Backends
              │
              └── monomorphization
                  inlining
                  dead code elimination
```

## Go Codegen Details

### Strategy: Generate Go Source

**Not** Go AST, **not** Go IR/SSA. Just text.

```ocaml
(* Simplified example *)
let emit_function name params body =
  Printf.sprintf "func %s(%s) {\n%s\n}"
    name
    (emit_params params)
    (emit_body body)
```

**Why source text:**
- Simple to implement
- Debuggable (can read generated Go)
- Go compiler handles optimization
- Cross-compilation for free
- No FFI to Go toolchain needed

**Precedent:**
- Cython: Python → C source → compile
- Nim: Nim → C source → compile  
- Haxe: Haxe → many targets as source

### Monomorphization

Generic functions are specialized at compile time:

```
// Marmoset
fn identity(x: a) -> a { x }
identity(42)
identity("hello")
```

```go
// Generated Go
func identity_int(x int64) int64 { return x }
func identity_string(x string) string { return x }

identity_int(42)
identity_string("hello")
```

No Go generics used. Each concrete type gets its own function.

### Effects (Pure vs Impure)

Pure functions (`->`) become plain Go functions:
```go
func add(a int64, b int64) int64 {
    return a + b
}
```

Impure functions (`=>`) get `context.Context`:
```go
func readFile(ctx context.Context, path string) string {
    // ... IO operations
}
```

User never writes `context.Context` in Marmoset. Compiler adds it.

### Runtime Support

A small Go runtime is bundled with generated code:

```
lib/backend/go/prelude.go
```

Contains:
- Builtin functions (`puts`, `len`, etc.)
- Value representations (if needed)
- Helper functions

This file is copied to the temp build directory.

## Project Structure

```
lib/
├── lib.ml                    # Re-exports for external use
│
├── frontend/
│   ├── syntax/               # Library: Syntax
│   │   ├── lexer.ml
│   │   ├── token.ml
│   │   ├── parser.ml
│   │   └── ast.ml
│   │
│   └── typecheck/            # Library: Typecheck
│       ├── types.ml
│       ├── unify.ml
│       ├── infer.ml
│       ├── checker.ml
│       ├── source_loc.ml
│       └── builtins.ml       # Type signatures
│
└── backend/
    ├── interpreter/          # Library: Interpreter
    │   ├── value.ml
    │   ├── env.ml
    │   ├── eval.ml
    │   └── builtins.ml       # Runtime implementations
    │
    ├── vm/                   # Library: Vm
    │   ├── compiler.ml
    │   ├── code.ml
    │   ├── symbol_table.ml
    │   └── machine.ml
    │
    └── go/                   # Library: Codegen
        ├── emitter.ml
        └── runtime.go
```

## Comparison with Other Languages

| Language | Target | Strategy |
|----------|--------|----------|
| Scala | JVM | Bytecode |
| Clojure | JVM | Bytecode |
| Kotlin | JVM / Native | Bytecode / LLVM |
| Nim | C | Source |
| Cython | C | Source |
| Haxe | Many | Source |
| **Marmoset** | **Go** | **Source** |

We're like "Nim for Go" - generate source, let the target compiler do the heavy lifting.
