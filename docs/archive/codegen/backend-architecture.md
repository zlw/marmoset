# Go Backend Architecture

## Overview

The Go backend compiles Marmoset programs to Go source code by:
1. Type checking the entire program (Hindley-Milner inference)
2. Collecting function definitions and call sites
3. Identifying monomorphization points (where polymorphic functions are used with concrete types)
4. Emitting specialized Go functions for each concrete instantiation
5. Generating the main function with all statements

## Pipeline

```
Source Code
    ↓
Parser → AST
    ↓
Type Checker (infer_program) → typed_env, program_type
    ↓
emit_program_with_typed_env(typed_env, program)
    ├─ Pass 1: collect_funcs_stmt - extract function definitions
    ├─ Pass 2: collect_insts_stmt - find monomorphization points using typed_env
    └─ Pass 3: emit_specialized_func - generate Go code for each instantiation
    ↓
Go Source Code (.go)
    ↓
Go Compiler (go build)
    ↓
Binary
```

## Key Design Decisions

### Single Type Inference Pass
- Type environment is computed once during type checking
- Threaded through codegen to inform monomorphization decisions
- Avoids redundant type inference during code generation
- See commit: 3db76ec

### Statement vs Expression If Handling
- If-expressions (returning values): wrapped in function for Go compatibility
- If-statements (control flow): emitted as regular Go if statements
- Prevents generating `nil` returns for missing else clauses
- See commit: 1e02360

### Type-Based Container Inference
- When indexing a polymorphic container with string key: infer hash type
- When indexing with integer: infer array type  
- Enables functions like `fn(book) { book["title"] }` to work correctly
- See commit: 5714ab4

### Monomorphization Strategy
- Each unique type instantiation of a polymorphic function gets a specialized Go function
- Functions are named with type mangling: `func_<arg_types>_<return_type>`
- Works well for concrete types (Int, String, etc.)
- Breaks down for higher-order functions with polymorphic parameters

## Working Examples

### Basic Types
```marmoset
let x = 42
let y = "hello"
let z = [1, 2, 3]
let w = {"key": "value"}
```
Generates simple Go: `x := int64(42)`, etc.

### Functions
```marmoset
let add = fn(x, y) { x + y }
add(1, 2)
```
Generates monomorphized: `func add_int64_int64(x int64, y int64) int64`

### Polymorphic Functions
```marmoset
let id = fn(x) { x }
id(5)
id("hello")
```
Generates two versions:
- `func id_int64(x int64) int64`
- `func id_string(x string) string`

### Hash Indexing
```marmoset
let get_title = fn(book) { book["title"] }
get_title({"title": "test"})
```
Correctly infers `book` as `{String: String}` and generates proper Go

## Known Limitations

### 1. Polymorphic Higher-Order Functions
Functions that take other functions as parameters with polymorphic types don't monomorphize correctly.

**Example that fails**:
```marmoset
let map = fn(arr, f) {
    let iter = fn(arr, accumulated) {
        if (len(arr) == 0) { accumulated }
        else { iter(rest(arr), push(accumulated, f(first(arr)))) }
    };
    iter(arr, [])
};
```

**Why**: The function parameter `f` has a polymorphic type that depends on array element types. Current monomorphization generates:
```go
func iter_arr_Tt107_arr_Tt116(arr []T107, accumulated []T116) []T107 {
    // f is undefined here - it's a polymorphic parameter
}
```

**Future solution**: 
- Constraint-based monomorphization that tracks polymorphic parameters
- Generate wrapper functions or closures for polymorphic parameters
- Or use Go 1.18+ generics for this case

### 2. Unresolved Type Variables
If monomorphization encounters unresolved type variables (from failed inference), they appear in Go code as `T<number>` or `UNKNOWN`, causing compilation errors.

**Why**: Monomorphization uses the inferred type environment, but some edges cases leave type variables unresolved.

**Future fix**: Reject programs with unresolved type variables at type check time.

### 3. Closure Type Inference
While closures work, they require that captured variables' types are known in context. Polymorphic closures may not monomorphize correctly.

## Type Caching

### Recent Refactor (Commit 3db76ec)
Previously, the codegen re-inferred expression types on-the-fly:
```ocaml
let infer_type env expr =
  match Infer.infer_expression env expr with ...
```

Now, the typed environment from the type checker is passed through and used directly:
```ocaml
let emit_program_with_typed_env (typed_env : Infer.type_env) program =
  (* Use typed_env for all instantiation collection *)
  ignore (List.fold_left (collect_insts_stmt mono_state) typed_env program);
```

Benefits:
- Single inference pass (performance)
- Consistent type information (correctness)
- Easier to debug (all types computed upfront)

## Testing

See `lib/backend/go/emitter.ml` for inline tests:
- 16 tests covering basic codegen
- 3 tests for if statement handling (added after finding bug)
- Tests for polymorphic functions, recursion, closures, indexing

Tests intentionally do NOT cover:
- Polymorphic higher-order functions (known limitation)
- Functions with unresolved type variables

## Future Improvements

### Phase 2: Better Type Inference
- Type annotations on function parameters to give hints
- Explicit type signatures for polymorphic functions
- Better inference for higher-order functions

### Phase 3: Improved Monomorphization
- Constraint-based approach instead of heuristic
- Track polymorphic parameters through recursion
- Generate proper specializations for each concrete usage

### Phase 3+: Consider Go Generics
- Go 1.18+ supports generic functions
- Could compile polymorphic functions directly without specialization
- Trade-off: easier codegen vs. needing recent Go version

### Better Error Messages
- When monomorphization fails, explain why
- Suggest fixes (e.g., "add type annotation to help inference")
- Show which call sites caused the problem
