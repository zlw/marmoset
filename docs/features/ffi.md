# FFI

## Maintenance

- Last verified: 2026-05-09
- Implementation status: Shim-first rollout in progress
- Update trigger: Any change to extern syntax, supported FFI types, typechecking, module visibility, or Go codegen

Marmoset interop is now shim-first. An `extern` declaration names a Marmoset shim id such as `std/bytes` or `test/scalar`, not an arbitrary Go import path. Go-specific details stay inside checked-in or project-provided shim packages, and public Marmoset modules expose ordinary Marmoset APIs built from `Result`, `Option`, `Bytes`, opaque handles, enums, records, and effects.

Direct arbitrary Go package externs are historical behavior. New interop and stdlib work should target the shim model instead of extending direct FFI with Go-shaped concepts such as `(T, error)`, pointers, channels, interfaces, variadics, or Go package representation types.

## Syntax

### Historical Direct Externs

```marmoset
extern "strings" = {
  fn ToUpper(s: Str) -> Str
  fn Contains(s: Str, substr: Str) -> Bool
}
```

Use `as` when the package path is not a valid Marmoset qualifier or when a different source qualifier is clearer:

```marmoset
extern "path/filepath" as fp = {
  fn Base(path: Str) -> Str
}
```

Pure extern functions use `->`. Effectful extern functions use `=>`:

```marmoset
extern "fmt" = {
  fn Println(s: Str) => Unit
}
```

### Shim Externs

Shim externs reuse the same top-level form, but the string is a slash-separated shim id. The owner module exports ordinary wrapper functions rather than exporting shim functions directly:

```marmoset
export FileReadError, read

type FileReadError = { NotFound, PermissionDenied, Other(Str) }

extern "std/file" as fs = {
  fn read(path: Str) => Result[Bytes, FileReadError]
}

fn read(path: Str) => Result[Bytes, FileReadError] = {
  fs.read(path)
}
```

Phase one requires shim ids to have an allowed root and at least two segments, such as `std/bytes`, `std/file`, or synthetic `test/scalar`. A single segment such as `strings` is rejected as a malformed shim id.

## Supported Types

### Historical Direct Boundary

FFI v1 supports only scalar boundary types:

| Marmoset | Go |
|---|---|
| `Int` | `int64` |
| `Float` | `float64` |
| `Bool` | `bool` |
| `Str` | `string` |
| `Unit` return | `struct{}` |

`Unit` parameters are rejected. Lists, maps, records, enums, unions, `Dyn`, `Option`, `Result`, functions, and generic extern declarations are not supported at the FFI boundary in v1.

### Shim Boundary

Shim boundaries are classified by resolved Marmoset type identity before Go codegen. The first supported boundary set is:

- `Unit`, `Bool`, `Int`, `Float`, and `Str`
- canonical `std.option.Option[T]`
- canonical `std.result.Result[T, E]`
- closed enums declared by the owning shim module
- opaque `extern type` handles declared by the owning shim module
- canonical immutable `std.bytes.Bytes`

Only canonical `std.bytes.Bytes` maps to the Go `marmoset.Bytes` ABI type. Other `extern type Bytes` declarations remain ordinary opaque handles. Bytes crossing a shim boundary are copied on entry and return so shim code cannot mutate Marmoset-visible byte values through slice aliasing.

Unsupported boundary types fail during typechecking instead of leaking Go representation details into Marmoset source.

## Module Visibility

### Historical Direct Visibility

Extern declarations are module-local. Importing a module that declares an extern does not import its Go package qualifier.

The intended pattern is a small wrapper module:

```marmoset
export upcase

extern "strings" = {
  fn ToUpper(s: Str) -> Str
}

fn upcase(s: Str) -> Str = strings.ToUpper(s)
```

Other modules import and call `upcase` like any ordinary Marmoset API. They cannot call `strings.ToUpper` unless they declare their own extern block.

### Shim Ownership

Shim externs remain module-local, but each shim id has exactly one owning Marmoset module in a project. In phase one, the owner module id must match the shim id with `/` converted to `.`, so `std/file` is owned by `std.file`. Duplicate owners, duplicate blocks for the same shim id, direct export of shim functions, and malformed or missing shim ids produce explicit diagnostics.

## Trust Boundary

### Historical Direct Trust Boundary

Extern signatures are trusted Marmoset declarations. The typechecker verifies Marmoset-side calls against the declared signature, and the Go compiler remains the final authority that the declaration matches the real Go function.

Semantic adapters are explicit. Go patterns such as `(T, error)`, `(T, bool)`, nil returns, or panics are not automatically converted to Marmoset `Result`, `Option`, or effects. Wrap those semantics in ordinary Marmoset or Go code and expose the intended shape deliberately.

### Shim ABI

Shim packages compile against a generated typed Go API package plus a support package named `marmoset`. Go errors, pointers, methods, channels, interfaces, panics, and sentinel values are handled by the shim author and mapped into explicit Marmoset domain types. ABI violations such as invalid zero-value `Result`/`Option`, nil enum interface values, zero handles, stale handles, bytes invariant failures, or shim panics are fatal runtime diagnostics from generated adapters, not domain errors.

## Codegen

### Historical Direct Codegen

The Go backend emits wrappers only for extern functions that are actually called. Imports use deterministic generated aliases such as `mext_strings` or `mext_path_filepath`, and wrapper names are based on the full Go import path plus function name so packages with the same basename do not collide.

### Shim Codegen

Shim codegen emits a complete Go module tree rather than only `main.go` and `runtime.go`. Called shims cause the build tree to include:

```text
go.mod
main.go
runtime.go
marmoset/
api/<shim-id>/api.go
shims/<shim-id>/
```

Generated adapter wrappers call exported Go shim symbols, convert between concrete Marmoset representations and typed ABI values, and copy checked-in runtime support from `runtime/go/marmoset` plus checked-in toolchain shims from `runtime/go/shims`.
