# FFI Plan

## Maintenance

- Last verified: 2026-02-28
- Implementation status: Planning (not started)
- Update trigger: Any FFI/extern syntax or Go interop change

## Context

Marmoset compiles to Go source code. "FFI" means: declare Go functions/types in Marmoset so the type checker knows about them, then the emitter generates correct Go import statements and function calls. There is no marshaling, no ABI boundary, no CGO — just Go-to-Go.

This is the third milestone in the language evolution. It depends on the module system and basic stdlib being in place, since the FFI pattern uses wrapper modules and prelude types like `result`.

### Prerequisites
- Module system (import/export, per-module compilation, module signatures) — `docs/plans/module-system.md`
- Basic stdlib with prelude (`option`, `result`, core traits, migrate builtins to modules) — plan TBD
- Purity annotations working (`->` pure, `=>` effectful) — extern functions require explicit purity

### Milestone Ordering
1. **Modules** → 2. **Basic stdlib + prelude** → 3. **FFI** → 4. **Full stdlib via FFI**

---

## Locked Decisions

1. **Wrapper module pattern.** Extern declarations live in dedicated wrapper `.mr` files. Wrapper functions provide Marmoset-idiomatic API. Users import the wrapper module, never see `extern` directly.
2. **`extern` block syntax.** `extern "go/import/path" { fn Name(...) -> Type }`. Groups functions by Go package.
3. **Qualified access inside wrappers.** `fmt.Println(s)` — extern functions accessed via Go package qualifier.
4. **Go package qualifier = last path component** (`"encoding/json"` → `json`). Aliasing with `as` supported.
5. **Purity annotation required** on every extern function (`->` or `=>`). Cannot infer purity for external code.
6. **No artificial type restrictions.** If a Go type has a clear Marmoset equivalent, allow it. Wrapper modules handle semantic translation. Unsupported types (channels, `any`) produce clear errors.
7. **Generated Go wrappers** handle type conversion (`int64` ↔ `int`, `void` → `struct{}`).

---

## Syntax Design

### Wrapper Module

```
# lib/fmt.mr — wrapper for Go's fmt package

export println, sprintf

extern "fmt" {
  fn Println(s: string) => unit
  fn Sprintf(format: string, arg: string) -> string
}

let println = fn(s: string) => unit {
  fmt.Println(s)
}

let sprintf = fn(format: string, arg: string) -> string {
  fmt.Sprintf(format, arg)
}
```

### User Code (no extern visible)

```
# main.mr

import lib.fmt

lib.fmt.println("hello")
```

### Richer Wrapper (semantic translation)

```
# lib/strconv.mr — wraps Go's strconv with Marmoset semantics
# result[t, e] comes from prelude (always available, no import needed)

export parse_int

extern "strconv" {
  fn Atoi(s: string) => int           # simplified v1 signature
}

let parse_int = fn(s: string) -> result[int, string] {
  # v2: translate Go's (int, error) → result[int, string]
  # v1: simplified, just forward
  result.ok(strconv.Atoi(s))
}
```

### Aliased Go Package

```
extern "encoding/json" as json {
  fn Marshal(v: string) => string
}

json.Marshal("test")
```

---

## Type Mapping

No artificial restrictions. If a type maps, it maps:

| Go | Marmoset | Notes |
|---|---|---|
| `int`, `int64` | `int` | Wrapper handles int64 ↔ int conversion |
| `float64` | `float` | Direct |
| `string` | `string` | Direct |
| `bool` | `bool` | Direct |
| `void` (no return) | `unit` | Wrapper returns `struct{}{}` |
| `[]T` | `list[T]` | Go slice ↔ Marmoset array |
| `map[K]V` | `hash[K, V]` | Direct |
| `struct{fields}` | `{ field: type, ... }` | Record mapping |
| `interface{methods}` | trait | Trait mapping |
| `(T, error)` | `result[T, string]` | Wrapper translates |
| `(T, bool)` | `option[T]` | Wrapper translates |
| `nil` | `option.none` | Wrapper translates |
| `chan T` | **unsupported** | Clear error |
| `any` / `interface{}` | **unsupported** | Clear error (no universal type) |

In v1, the extern declaration uses Marmoset types. The compiler generates Go wrappers that handle the conversion. Complex translations (error → result, nil → option) happen in the wrapper module's Marmoset code.

---

## Implementation Phases

### Phase F0: Keyword, AST, Parsing
**Goal:** Parser recognizes `extern` blocks. No type checking or codegen.

**Token additions** (`lib/frontend/syntax/token.ml`):
- `Extern` keyword

**AST additions** (`lib/frontend/syntax/ast.ml`):
```ocaml
| ExternBlock of extern_block_def

and extern_block_def = {
  extern_go_path : string;              (* "fmt", "encoding/json" *)
  extern_alias : string option;         (* Some "json" for as json *)
  extern_fns : extern_fn_sig list;
}

and extern_fn_sig = {
  extern_fn_name : string;              (* "Println" — preserves Go PascalCase *)
  extern_fn_params : (string * type_expr) list;
  extern_fn_return_type : type_expr;
  extern_fn_effectful : bool;           (* true for =>, false for -> *)
  extern_fn_pos : int;
  extern_fn_end_pos : int;
}
```

**Parser changes** (`lib/frontend/syntax/parser.ml`):
- Parse `extern "path" { fn Name(params) -> Type; ... }`
- Parse `extern "path" as alias { ... }`
- All params MUST have type annotations (no inference for extern)
- Arrow (`->` or `=>`) REQUIRED

**Tests:**
- Unit: lex `extern`
- Unit: parse `extern "fmt" { fn Println(s: string) => unit }`
- Unit: parse multiple functions in one block
- Unit: parse `extern "encoding/json" as json { ... }`
- Unit: error for missing type annotations
- Unit: error for missing arrow
- Integration: all existing tests pass

**Gate:** `make unit && make integration` green.

---

### Phase F1: Extern Registry and Type Checking
**Goal:** Type checker validates extern declarations, makes extern functions available.

**New file:** `lib/frontend/typecheck/extern_registry.ml`
```ocaml
type extern_func = {
  go_package : string;
  go_func_name : string;
  qualifier : string;
  marmoset_type : Types.poly_type;
  is_effectful : bool;
}

val register_package : string -> string -> extern_fn_sig list -> unit
val lookup_function : qualifier:string -> func_name:string -> extern_func option
val used_packages : unit -> string list   (* Go paths of packages actually called *)
```

**Type checking changes** (`lib/frontend/typecheck/infer.ml`):
- Handle `ExternBlock` in top-level declaration registration
- Register each extern function's type in extern_registry
- Register qualifier as known namespace (not a value)
- Resolve `MethodCall(Identifier "fmt", "Println", args)` — check extern registry before normal method resolution
- Type-check args against declared parameter types
- Track which extern functions are actually called (for import generation)

**Tests:**
- Unit: extern function types registered correctly
- Unit: `fmt.Println("hello")` type-checks with effectful flag
- Unit: `strings.Contains("hello", "ell")` type-checks to `bool`
- Unit: type error for wrong argument type
- Unit: error for unknown extern function name
- Unit: error for using extern qualifier as a value

**Gate:** `make unit && make integration` green.

---

### Phase F2: Code Generation
**Goal:** Extern calls compile to working Go code.

**Emitter changes** (`lib/backend/go/emitter.ml`):

1. **Dynamic import collection.** Replace hardcoded `import "fmt"` with collection of all required Go imports (builtins + used extern packages).

2. **Emit Go wrapper functions** for each used extern function:
   ```go
   func extern__fmt__Println(s string) struct{} {
       fmt.Println(s)
       return struct{}{}
   }
   ```
   Wrappers handle: `int64` → `int` conversion, `void` → `struct{}` return, etc.

3. **Route extern calls** in `emit_expr`: when emitting a call to an extern function, emit call to the wrapper: `extern__fmt__Println(s)`.

4. **Only emit wrappers for functions actually called** (tracked by usage flag in registry).

**Tests:**
- Integration: `extern "fmt" { fn Println(s: string) => unit }` + wrapper → compiles, runs, prints
- Integration: `extern "strings" { fn ToUpper(s: string) -> string }` → "HELLO"
- Integration: multiple extern blocks in one file
- Integration: extern function called inside user-defined function
- Integration: generated Go compiles with `go build`

**Gate:** `make unit && make integration` green.

---

### Phase F3: Integration with Module System
**Goal:** Extern wrapper modules work as regular importable modules.

- Extern declarations in a wrapper `.mr` file
- Wrapper functions exported via `export`
- Other modules import the wrapper module
- Extern registry entries scoped per module (like other registries)
- Go imports collected across all modules for the single `package main` output

**Tests:**
- Multi-file: wrapper module with extern + wrappers, main module imports wrapper
- Extern functions not visible from importing module (only exported wrappers)
- Multiple wrapper modules for different Go packages

**Gate:** `make unit && make integration` green.

---

### Phase F4: Polish and Hardening
**Goal:** Edge cases, error quality.

- Error messages: unsupported Go types, duplicate extern qualifiers, qualifier shadows module name
- Extern qualifier collision with module names → clear error
- Documentation and examples
- Test suite: `test/integration/09_extern_edge_cases.sh`

---

## Go Output Example

**lib/fmt.mr:**
```
export println

extern "fmt" {
  fn Println(s: string) => unit
}

let println = fn(s: string) => unit {
  fmt.Println(s)
}
```

**main.mr:**
```
import lib.fmt

lib.fmt.println("hello world")
```

**Generated main.go:**
```go
package main

import "fmt"

// Extern wrapper
func extern__fmt__Println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

// lib/fmt module wrapper
func lib__fmt__println(s string) struct{} {
    return extern__fmt__Println(s)
}

func main() {
    lib__fmt__println("hello world")
}
```

---

## Relationship to Module System

- `extern` is a statement type (like `trait`, `enum`) — can appear in any module
- Convention: put extern blocks in dedicated wrapper modules
- Extern qualifier namespace is separate from module namespace — error on collision
- Extern registry follows same per-module scoping as other registries
- Go imports from all modules collected into single import block (single Go package)

---

## Future Extensions (v2+)

1. **Multiple return values:** `fn Atoi(s: string) => (int, error)` with automatic result type mapping
2. **Go methods:** `extern "strings" type Builder { fn WriteString(self, s: string) -> int }`
3. **Go interfaces → traits:** automatic mapping
4. **Auto-discovery:** `marmoset gen-extern "fmt"` CLI tool generates wrapper stubs from `go doc`
5. **Variadic functions:** `fn Sprintf(format: string, args: ...string) -> string`
6. **Opaque types:** Go types with no Marmoset equivalent wrapped as opaque handles

---

## Risks

1. **`int64` vs `int`** — mitigated by generated Go wrapper functions that handle conversion
2. **Extern qualifier collision with module names** — mitigated by validation in registry, clear error
3. **`.` disambiguation** — extern qualifiers checked alongside module qualifiers in the resolver
4. **Generated Go size** — wrappers only emitted for functions actually called
5. **Go function signatures evolving** — wrapper modules are user-maintained, updated as needed

---

## Critical Files

| File | Role |
|------|------|
| `lib/frontend/syntax/ast.ml` | Add ExternBlock, extern_block_def, extern_fn_sig |
| `lib/frontend/syntax/token.ml` | Add Extern keyword |
| `lib/frontend/syntax/parser.ml` | Parse extern blocks |
| `lib/frontend/typecheck/infer.ml` | Handle ExternBlock, resolve extern-qualified calls |
| `lib/backend/go/emitter.ml` | Dynamic imports, emit Go wrappers, route extern calls |
| **New:** `lib/frontend/typecheck/extern_registry.ml` | Extern function registry |
