# FFI

## Maintenance

- Last verified: 2026-05-09
- Implementation status: Shim-first interop implemented
- Update trigger: Any change to extern syntax, supported FFI types, typechecking, module visibility, or Go codegen

Marmoset interop is shim-first. An `extern` declaration names a Marmoset shim id such as `std/bytes` or `std/file`, not an arbitrary Go import path. Go-specific details stay inside checked-in or project-provided shim packages, and public Marmoset modules expose ordinary Marmoset APIs built from `Result`, `Option`, `Bytes`, opaque handles, enums, records, and effects.

Direct arbitrary Go package externs have been removed. A declaration such as `extern "strings"` is rejected as a malformed shim id before catalog lookup.

## Syntax

Shim externs use the top-level `extern` form. The string is a slash-separated shim id, and the optional `as` qualifier controls the source qualifier used inside the owning module:

```marmoset
export FileReadError, read

import std.bytes.Bytes

type FileReadError = { NotFound, PermissionDenied, IsDirectory, Other(Str) }

extern "std/file" as file_shim = {
  fn read(path: Str) => Result[Bytes, FileReadError]
}

fn read(path: Str) => Result[Bytes, FileReadError] = {
  file_shim.read(path)
}
```

Phase one requires shim ids to use the `std` root and at least two slash-separated segments. Valid examples include `std/bytes` and `std/file`. Single-segment ids such as `strings` are rejected with `shim-id-invalid`; syntactically valid but unknown ids such as `std/missing` fail with `shim-id-not-found`.

Opaque resource handles use `extern type` in the owning shim module:

```marmoset
export File, open, close

extern type File

extern "std/file" as file_shim = {
  fn open(path: Str) => Result[File, FileOpenError]
  fn close(file: File) => Result[Unit, FileCloseError]
}
```

`extern type` values cannot be constructed, field-inspected, record-pattern matched, or given derived trait implementations in Marmoset. Shim code owns their representation and lifecycle.

## Supported Types

Shim boundaries are classified by resolved Marmoset type identity before Go codegen. The supported boundary set is:

- `Unit`, `Bool`, `Int`, `Float`, and `Str`
- canonical `std.option.Option[T]`
- canonical `std.result.Result[T, E]`
- closed enums declared by the owning shim module
- opaque `extern type` handles declared by the owning shim module
- canonical immutable `std.bytes.Bytes`

Only canonical `std.bytes.Bytes` maps to the Go `marmoset.Bytes` ABI type. Other `extern type Bytes` declarations remain ordinary opaque handles. Bytes crossing a shim boundary are copied on entry and return so shim code cannot mutate Marmoset-visible byte values through slice aliasing.

Unsupported boundary types fail during typechecking instead of leaking Go representation details into Marmoset source.

## Ownership

Extern declarations are module-local. Importing a wrapper module never imports its shim qualifier; callers use the exported Marmoset wrapper API.

Each shim id has exactly one owning Marmoset module in a project. In phase one, the owner module id must match the shim id with `/` converted to `.`, so `std/file` is owned by `std.file`. Duplicate owners, duplicate blocks for the same shim id, direct export of shim functions, qualifier collisions, Go symbol collisions, malformed shim ids, and missing shim ids produce explicit diagnostics.

## Trust Boundary

Shim signatures are trusted Marmoset declarations, but the shim package must compile against generated typed Go API packages. The typechecker verifies Marmoset-side calls against the declared signature, and the Go compiler verifies that checked-in shim functions match the generated ABI.

Go errors, pointers, methods, channels, interfaces, panics, sentinel values, and variadic APIs are handled by the shim author and mapped into explicit Marmoset domain types. ABI violations such as invalid zero-value `Result`/`Option`, nil enum interface values, zero handles, stale handles, bytes invariant failures, or shim panics are fatal runtime diagnostics from generated adapters, not domain errors.

## Codegen

Shim codegen emits a complete Go module tree rather than only `main.go` and `runtime.go`. Called shims cause the build tree to include:

```text
go.mod
main.go
runtime.go
marmoset/
api/<shim-id>/api.go
shims/<shim-id>/
```

Generated adapter wrappers call exported Go shim symbols, convert between concrete Marmoset representations and typed ABI values, recover shim panics as ABI violations, and copy checked-in runtime support from `runtime/go/marmoset` plus checked-in toolchain shims from `runtime/go/shims`.
