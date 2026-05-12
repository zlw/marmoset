# FFI

## Maintenance

- Last verified: 2026-05-12
- Implementation status: Shim-first interop implemented
- Update trigger: Any change to extern syntax, supported FFI types, typechecking, module visibility, or Go codegen

Marmoset interop is shim-first. An `extern` declaration names a Marmoset shim id such as `std/bytes` or `std/file`, not an arbitrary Go import path. Go-specific details stay inside checked-in or project-provided shim packages, and public Marmoset modules expose ordinary Marmoset APIs built from `Result`, `Option`, `Bytes`, opaque handles, enums, records, and effects.

Direct arbitrary Go package externs have been removed. A declaration such as `extern "strings"` is rejected as a malformed shim id before catalog lookup.

## Syntax

Shim externs use the top-level `extern` form. The string is a slash-separated shim id, and the optional `as` qualifier controls the source qualifier used inside the owning module:

```marmoset
export Bytes, DecodeError, Decode, Encode, from_str, to_str

extern type Bytes

type DecodeError = { InvalidUtf8 }

extern "std/bytes" as byte_shim = {
  fn from_str(value: Str) -> Bytes
  fn to_str(input: Bytes) -> Result[Str, DecodeError]
}
```

Phase one requires shim ids to use the `std` root and at least two slash-separated segments. Valid examples include `std/bytes`, `std/file`, `std/io`, and nested packages such as `std/io/err`. Single-segment ids such as `strings` are rejected with `shim-id-invalid`; syntactically valid but unknown ids such as `std/missing` fail with `shim-id-not-found`.

Opaque resources use `extern type` in the owning shim module. Public modules should still expose the highest-level Marmoset API they can. For example, `std.file` exports the opaque `File` resource so users can type scoped callbacks, but keeps raw open/close and byte transport private:

```marmoset
import std.bytes
import std.bytes.Bytes
import std.path
import std.path.PathLike

export File, Mode, Error, UseError,
       read, write, append, open,
       read_all, write_all, flush

extern type File

type Mode = { Read, Write, Append }
type Error = {
  NotFound,
  PermissionDenied,
  AlreadyExists,
  IsDirectory,
  NotDirectory,
  InvalidPath(Str),
  InvalidData(Str),
  AlreadyClosed,
  Other(Str),
}
type UseError[e] = { Open(Error), Use(e), Close(Error), UseAndClose(e, Error) }

extern "std/file" as file_shim = {
  fn read_data(path: Str) => Result[Bytes, Error]
  fn write_data(path: Str, bytes: Bytes) => Result[Unit, Error]
  fn append_data(path: Str, bytes: Bytes) => Result[Unit, Error]
  fn open_handle(path: Str, mode: Mode) => Result[File, Error]
  fn close_handle(file: File) => Result[Unit, Error]
  fn read_all_data(file: File) => Result[Bytes, Error]
  fn write_all_data(file: File, bytes: Bytes) => Result[Unit, Error]
  fn flush_handle(file: File) => Result[Unit, Error]
}

fn read[a: bytes.Decode, p: PathLike](path: p) => Result[a, Error]
fn write[a: bytes.Encode, p: PathLike](path: p, value: a) => Result[Unit, Error]
fn append[a: bytes.Encode, p: PathLike](path: p, value: a) => Result[Unit, Error]
fn open[p: PathLike, a, e](path: p, mode: Mode, body: (File) => Result[a, e]) => Result[a, UseError[e]]
fn read_all[a: bytes.Decode](file: File) => Result[a, Error]
fn write_all[a: bytes.Encode](file: File, value: a) => Result[Unit, Error]
fn flush(file: File) => Result[Unit, Error]
```

`extern type` values cannot be constructed, field-inspected, record-pattern matched, or given derived trait implementations in Marmoset. Shim code owns their representation and lifecycle.

## Supported Types

Shim boundaries are classified by resolved Marmoset type identity before Go codegen. The supported boundary set is:

- `Unit`, `Bool`, `Int`, `Float`, and `Str`
- canonical `std.option.Option[T]`
- canonical `std.result.Result[T, E]`
- canonical immutable `List[T]` when `T` is a supported boundary type
- closed enums declared by the owning shim module
- opaque `extern type` handles declared by the owning shim module
- canonical immutable `std.bytes.Bytes`
- direct callback parameters whose argument and return types are supported shim boundary types

Only canonical `std.bytes.Bytes` maps to the Go `marmoset.Bytes` ABI type. Other `extern type Bytes` declarations remain ordinary opaque handles. Bytes crossing a shim boundary are copied on entry and return so shim code cannot mutate Marmoset-visible byte values through slice aliasing.

Callback parameters lower to typed Go `func(...) ...` values. The supported callback shape is synchronous and non-escaping: the Go shim may call the callback during the shim call, but storing it, calling it later, or invoking it concurrently is outside the supported ABI. Callback return types are converted back through the same adapter rules, including `Result`, `Option`, owner enums, handles, `Bytes`, and `Unit`.

Unsupported boundary types fail during typechecking instead of leaking Go representation details into Marmoset source.

## Ownership

Extern qualifiers are module-local. Importing a shim-backed stdlib module never imports its private shim qualifier. If an extern function name appears in the module's `export` list, callers use that exported module API directly. Direct calls lower through the generated shim adapter, and first-class references such as `let read = file.read` use that same generated adapter as an ordinary function value. Shim owners can also keep extern functions private and wrap them in ordinary Marmoset functions when the public API needs to enforce a lifecycle or combine lower-level operations.

Each shim id has exactly one owning Marmoset module in a project. In phase one, the owner module id must match the shim id with `/` converted to `.`, so `std/file` is owned by `std.file`. Duplicate owners, duplicate blocks for the same shim id, qualifier collisions, Go symbol collisions, malformed shim ids, and missing shim ids produce explicit diagnostics.

## Trust Boundary

Shim signatures are trusted Marmoset declarations, but the shim package must compile against generated typed Go API packages. The typechecker verifies Marmoset-side calls against the declared signature, and the Go compiler verifies that checked-in shim functions match the generated ABI.

Go errors, pointers, methods, channels, interfaces, panics, sentinel values, and variadic APIs are handled by the shim author and mapped into explicit Marmoset domain types. ABI violations such as invalid zero-value `Result`/`Option`, nil enum interface values, zero handles, stale handles, bytes invariant failures, or shim panics are fatal runtime diagnostics from generated adapters, not domain errors.

## Codegen

Shim codegen emits a complete Go module tree rather than a single root Go package. Called or first-class-referenced shims cause the build tree to include:

```text
go.mod
main.go
marmoset/
api/<shim-id>/api.go
shims/<shim-id>/
```

Generated adapter wrappers call exported Go shim symbols, convert between concrete Marmoset representations and typed ABI values, recover shim panics as ABI violations, and copy checked-in runtime support from `runtime/go/marmoset` plus checked-in toolchain shims from `runtime/go/shims`.
