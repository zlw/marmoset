# Shim-First Go Interop Plan

## Maintenance

- Last verified: 2026-05-08
- Implementation status: Planning
- Prerequisites:
  - `docs/plans/done/language/06_module-system.md`
  - `docs/plans/done/language/07_prelude.md`
  - `docs/plans/done/language/08_ffi.md`
- Related plans:
  - `docs/plans/todo/language/05_stdlib.md`
- Update trigger: Any extern syntax change, shim catalog change, Go build layout change, runtime ABI change, stdlib wrapper change, handle/resource design change, or generated Go representation change

## Context

Marmoset currently has FFI v1: trusted direct calls to scalar Go package functions.

```marmoset
extern "strings" = {
  fn ToUpper(s: Str) -> Str
}
```

That implementation is intentionally narrow. It works for simple scalar calls, but it is the wrong long-term abstraction for real Go libraries. Expanding it would push Go-specific concepts into Marmoset source: multi-return tuples, nullable pointers, `error`, raw package paths, methods, channels, interfaces, variadics, and generated Go representation types.

This plan replaces direct Go package externs with shim externs as the single interop surface. A shim is ordinary Go code written against a small typed ABI. The shim handles Go-specific details. The Marmoset module exposes normal Marmoset APIs.

```text
Go package / stdlib / framework / sqlc output
        |
Go shim package written by a wrapper author
        |
typed marmoset Go ABI package plus generated shim API package
        |
generated Marmoset adapter wrapper
        |
public Marmoset module using Result, Option, Bytes, records, handles, callbacks
```

The design deliberately avoids compiler magic for Go `(T, error)` or arbitrary Go tuples. Shims map those details explicitly into Marmoset-owned types such as `Result[T, E]`, `Option[T]`, closed domain enums, immutable `Bytes`, records, and resource handles.

## Goals

1. Remove direct arbitrary Go package externs instead of extending them.
2. Reuse `extern "shim/id" = { ... }` as the shim declaration syntax, since there is no longer a competing direct-Go meaning.
3. Provide a fully typed Go ABI based on a support package named `marmoset`, not a dynamic catch-all value representation.
4. Generate typed API packages from shim boundary signatures so Go shims compile against the declared Marmoset boundary types.
5. Let wrapper modules expose backend-neutral public Marmoset APIs. Go-specific types such as `os.File`, `error`, channels, and pointers must not leak.
6. Add `extern type File` for shim-owned opaque resource handles that Marmoset can name and pass but cannot inspect or construct.
7. Keep Go runtime support and checked-in shims under one runtime tree:

```text
runtime/
  go/
    marmoset/
    shims/
      std/
        file/
```

8. Reconcile the existing FFI, stdlib, architecture, and roadmap docs before implementation proceeds past the build-layout slice.

## Non-Goals

1. Do not keep direct arbitrary Go import-path FFI as a second interop lane.
2. Do not add raw `GoError`, `GoResult`, raw `Ptr`, raw `chan`, raw Go interfaces, or Go package representation types to Marmoset source.
3. Do not add automatic `(T, error) -> Result[T, E]`, `(T, bool) -> Option[T]`, nil-to-option, or arbitrary tuple conversion.
4. Do not expose generated Go type names, Go runtime handles, or shim helper types as public Marmoset API.
5. Do not use a dynamic Go-side ABI value as the primary shim contract.
6. Do not require Go signature discovery, reflection, CGO, plugins, or runtime dynamic loading.
7. Do not implement project-local user shim packaging in this milestone. Phase one supports toolchain shims and test shims only; user shims need a follow-up plan.
8. Do not solve full async/concurrency semantics in this milestone. Channels stay hidden behind later Marmoset-owned abstractions such as `Task`, `Stream`, or `Mailbox`.
9. Do not implement callbacks in this milestone. Callback policy is specified as follow-up design work, not part of the core landing path.

## Current State

- `docs/features/ffi.md` documents FFI v1 as module-local trusted Go package function declarations with scalar boundary types.
- `docs/ARCHITECTURE.md` says emitted builds currently produce one Go package from `main.go` plus `runtime.go`, with no auxiliary Go package tree.
- `bin/main.ml` writes only `main.go`, `runtime.go`, and a hardcoded `go.mod` into `.marmoset/build/<pid>` before running `go build .`. `--emit-go` currently omits `go.mod`.
- `lib/frontend/syntax/ast.ml` stores extern package paths in fields named `extern_go_path`.
- `lib/frontend/typecheck/extern_registry.ml` validates direct extern functions and stores Go import aliases.
- `lib/frontend/typecheck/resolution_artifacts.ml` carries `extern_func` and `extern_call` artifacts from typechecking to the Go emitter.
- `lib/frontend/compiler.ml` currently merges project-wide extern declarations by Go path/function signature. Shim externs need one owner per shim id instead.
- `lib/backend/go/emitter.ml` emits direct extern wrappers and runtime support as string output. It does not copy auxiliary Go packages or generate typed API packages.
- `std/option.mr`, `std/result.mr`, and `std/prelude.mr` are normal Marmoset modules injected through the module pipeline. Canonical stdlib `Result` variants are `Success` and `Failure`; canonical `Option` variants are `Some` and `None`.
- `std/` does not yet contain `bytes.mr`, `file.mr`, or Go shim sources.
- Existing exact Go snapshots under `test/snapshots/go_exact` are single-file snapshots. Shim output needs tree snapshots.
- `docs/plans/todo/language/05_stdlib.md` owns the broader standard-library expansion and now depends on this shim-first interop plan.
- The former direct-FFI multi-return/private-representation draft was deleted as superseded. Tuple work should return only as a separate language-general plan if it becomes independently useful to Marmoset.

## Design Decisions

### Direct FFI Is Removed

`extern` declarations name shim ids, not Go import paths:

```marmoset
extern "std/file" as fs = {
  fn read(path: Str) => Result[Bytes, FileReadError]
}
```

The string is a slash-separated shim id resolved by the compiler. Old direct declarations such as `extern "strings"` no longer import Go package `strings`. Phase one requires at least two shim-id segments under an allowed root, so `extern "strings"` is always rejected with `shim-id-invalid` before catalog lookup and should hint at `std/str` when that shim exists. Syntactically valid but absent ids, such as `extern "std/missing"`, fail with `shim-id-not-found`.

The qualifier defaults to the final path segment. An explicit alias remains useful for conflicts or readability:

```marmoset
extern "std/file" as fs = {
  fn read(path: Str) => Result[Bytes, FileReadError]
}

fn read(path: Str) => Result[Bytes, FileReadError] = {
  fs.read(path)
}
```

Extern functions are not exportable declarations. This is valid because `read` is an ordinary wrapper:

```marmoset
export read

extern "std/file" as fs = {
  fn read(path: Str) => Result[Bytes, FileReadError]
}

fn read(path: Str) => Result[Bytes, FileReadError] = fs.read(path)
```

This is rejected with `module-export-shim` because only the shim function exists:

```marmoset
export read

extern "std/file" as fs = {
  fn read(path: Str) => Result[Bytes, FileReadError]
}
```

If no extern function matches the exported name, the existing unknown-export diagnostic remains correct.

### Shim Catalog, Ownership, And Identity

Phase one supports two shim roots:

- toolchain shims under `runtime/go/shims/<shim-id>/`;
- synthetic `test/*` shims registered by tests.

Project-local user shims are a follow-up plan. This is an intentional cost of removing direct FFI: application authors use shipped stdlib/toolchain shims until user-shim packaging lands.

Shim ids obey these rules in phase one:

- slash-separated, at least two non-empty segments;
- allowed roots are `std` and `test`;
- no `.`, `..`, absolute paths, drive-letter paths, empty segments, or machine paths;
- segments are lowercase snake-case identifiers;
- Go keyword segments are rejected for phase one to keep checked-in shim package names simple;
- the toolchain shim id must exist in the shim catalog before typechecking succeeds.

Each shim id has exactly one owning Marmoset module in a project. For phase one, owner module id must equal the shim id with `/` converted to `.`:

```text
std/file   <-> std.file
std/bytes  <-> std.bytes
test/bytes <-> test.bytes
```

A module `lib.file` cannot declare `extern "std/file"`. That fails with `shim-owner-mismatch`. The same shim id declared by two modules fails with `shim-owner-duplicate`, even if signatures match. Multiple extern blocks for the same shim id in one module are rejected in phase one unless a later review explicitly allows block merging.

API packages are keyed by shim id, not owner module id. Owner module id is metadata for diagnostics and module-surface checks.

### Artifact Model

The direct FFI artifact model must be replaced, not stretched. The post-S2 artifact shape should be equivalent to:

```ocaml
type shim_func = {
  shim_key : string;
  shim_id : string;
  owner_module_id : string;
  source_qualifier : string;
  marmoset_func_name : string;
  go_symbol_name : string;
  param_names : string list;
  param_boundary_types : Shim_boundary.boundary_type list;
  return_boundary_type : Shim_boundary.boundary_type;
  is_effectful : bool;
  source_span : Diagnostic.span;
  boundary_spans : Diagnostic.span list;
}

type shim_call = {
  call_func_key : string;
  call_arg_boundary_types : Shim_boundary.boundary_type list;
  call_return_boundary_type : Shim_boundary.boundary_type;
  call_effectful : bool;
}
```

`Resolution_artifacts.call_resolution` should replace `ExternQualifiedCall` with `ShimQualifiedCall of string`, or rename the existing constructor only if all field names and diagnostics are also migrated away from Go-package language.

The gate for this migration is that shim extern codegen no longer uses `go_path`, `go_import_alias`, or direct Go package wrapper emission.

### Naming And Package Invariants

Checked-in source layout:

```text
runtime/
  go/
    marmoset/
      result.go
      option.go
      unit.go
      bytes.go
      handle.go
    shims/
      std/
        file/
          file.go
        bytes/
          bytes.go
```

Temporary build layout:

```text
.marmoset/build/<pid>/
  go.mod
  main.go
  runtime.go
  marmoset/
    result.go
    option.go
    unit.go
    bytes.go
    handle.go
  api/
    std/
      file/
        api.go
  shims/
    std/
      file/
        file.go
```

Generated imports use deterministic local paths:

```go
import (
    "marmoset_out/marmoset"
    fileapi "marmoset_out/api/std/file"
    mshim_std_file "marmoset_out/shims/std/file"
)
```

Package clauses for generated API packages must be keyword-safe and deterministic, for example `package mapi_std_file`. Adapter import aliases are also generated and collision-safe. Marmoset source never mentions generated Go import paths.

Extern function names are Marmoset names. The emitter maps them to exported Go shim symbols deterministically:

```text
read        -> Read
read_bytes  -> ReadBytes
map_int     -> MapInt
exists?     -> ExistsQ
write!      -> WriteBang
```

The extern parser should accept ordinary Marmoset identifier suffixes that are valid for function names. Collisions after Go symbol mangling fail during typechecking with `shim-symbol-collision`. Required collision tests include `exists?`, `exists_q`, `existsQ`, case-only differences, and Go keywords.

Shim packages must not import other `marmoset_out/shims/...` packages in this milestone. Shims may import `marmoset_out/marmoset` and generated `marmoset_out/api/...` packages only.

### Boundary Type Classifier

The shim boundary must be classified into an explicit typed representation before codegen:

```ocaml
type boundary_type =
  | BUnit
  | BBool
  | BInt
  | BFloat
  | BStr
  | BStdOption of boundary_type
  | BStdResult of boundary_type * boundary_type
  | BOwnerEnum of enum_identity * boundary_type list
  | BStdBytes
  | BExternHandle of extern_type_identity
```

The classifier is keyed by resolved module/type identity after import rewriting, not by source spelling alone. This matters because user code can define or import impostor types named `Option` or `Result`. Only canonical `std.option.Option` and `std.result.Result` map to `BStdOption` and `BStdResult`.

Phase-one allowed boundary types:

- `Unit`
- `Bool`
- `Int`
- `Float`
- `Str`
- canonical `Option[T]` where `T` is supported
- canonical `Result[T, E]` where `T` and `E` are supported
- closed enums declared by the owning shim module, with supported positional payloads

Later phases in this plan add:

- `extern type` handles
- canonical `std.bytes.Bytes`

Rejected until follow-up plans:

- imported user-defined enums and records;
- records, lists, maps, unions, intersections, traits, `Dyn`, generic function values, and callbacks;
- generic shim extern declarations with unresolved type variables.

Unsupported boundary types fail in typechecking with `type-shim-boundary`. Boundary diagnostics should preserve spans for the specific offending param/return type and nested type when possible. If retaining type spans through lowering is too broad, shim-boundary validation must run before spans are discarded.

Required negative tests include local `type Result[a, e] = { Ok(a), Err(e) }`, imported `foo.Result`, alias-shadowed `Option`, imported user enum payloads, and nested unsupported types such as `Result[Int, List[Str]]`.

### Fully Typed Go ABI

Go shims import a support package named `marmoset` plus generated API packages for the shim ids they implement. The ABI is typed Go, so shim mistakes are caught by `go build` as much as possible.

Example Marmoset wrapper surface:

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

Generated API package sketch:

```go
package mapi_std_file

type FileReadError interface {
    isFileReadError()
}

type FileReadErrorNotFound struct{}
func (FileReadErrorNotFound) isFileReadError() {}

type FileReadErrorPermissionDenied struct{}
func (FileReadErrorPermissionDenied) isFileReadError() {}

type FileReadErrorOther struct {
    Field0 string
}
func (FileReadErrorOther) isFileReadError() {}
```

Enum payload fields are positional (`Field0`, `Field1`, ...), because Marmoset enum payloads are positional. Generated adapters must reject nil enum interface values and recursively validate enum payloads before converting into generated Marmoset tagged values.

Shim package sketch:

```go
package file

import (
    "errors"
    "os"

    "marmoset_out/marmoset"
    fileapi "marmoset_out/api/std/file"
)

func Read(path string) marmoset.Result[marmoset.Bytes, fileapi.FileReadError] {
    data, err := os.ReadFile(path)
    if err != nil {
        switch {
        case errors.Is(err, os.ErrNotExist):
            return marmoset.Failure[marmoset.Bytes, fileapi.FileReadError](
                fileapi.FileReadErrorNotFound{},
            )
        case errors.Is(err, os.ErrPermission):
            return marmoset.Failure[marmoset.Bytes, fileapi.FileReadError](
                fileapi.FileReadErrorPermissionDenied{},
            )
        default:
            return marmoset.Failure[marmoset.Bytes, fileapi.FileReadError](
                fileapi.FileReadErrorOther{Field0: err.Error()},
            )
        }
    }

    return marmoset.Success[marmoset.Bytes, fileapi.FileReadError](
        marmoset.BytesCopy(data),
    )
}
```

Generated adapter sketch:

```go
func extern__std_file__read(path string) Result_bytes_file_read_error {
    __ret := mshim_std_file.Read(path)
    return __marmoset_from_abi_Result_bytes_file_read_error("std/file.read", __ret)
}
```

The `marmoset` support package should include:

- `Result[T, E]` with `Success` and `Failure` package-level constructors;
- `Option[T]` with `Some` and `None` package-level constructors;
- `Unit`;
- `Bytes` after the bytes phase;
- `Handle[T]` and `HandleTable[Tag, Value]` after the handle phase.

`Result` and `Option` must have unexported fields and explicit tags. Their zero values are invalid ABI values. Generated adapters diagnose invalid zero-value `Result`/`Option` rather than treating them as domain failures.

Because adapters live outside package `marmoset`, the support package must expose inspection helpers without exposing mutable representation. Required Go 1.18-compatible helper shape:

```go
type ResultState int

const (
    ResultInvalid ResultState = iota
    ResultSuccess
    ResultFailure
)

func InspectResult[T, E any](r Result[T, E]) (ResultState, T, E)

type OptionState int

const (
    OptionInvalid OptionState = iota
    OptionSome
    OptionNone
)

func InspectOption[T any](o Option[T]) (OptionState, T)
```

S4 converters must use these helpers to detect invalid zero values and unwrap payloads. Shims construct values through package-level constructors only; they do not inspect fields directly.

The Go ABI must compile under the generated module's language version. Current `go.mod` uses `go 1.18`, so the support package may use generic types and package-level generic functions, but no generic methods or newer Go-language features.

### Runtime ABI Diagnostics

Domain failures stay in wrapper-owned `Result[T, E]` values. ABI violations are fatal runtime diagnostics from generated adapters or support helpers.

Diagnostics must include:

- shim id and function name;
- expected boundary type;
- actual invalid ABI state when known;
- source span for the extern function when available.

Required fatal ABI cases:

- invalid or zero-value `Result`;
- invalid or zero-value `Option`;
- nil enum interface value;
- enum variant payload that fails recursive boundary validation;
- zero handle returned or passed;
- stale/closed handle where the shim table reports that condition as an ABI violation;
- bytes invariant violation;
- shim panic crossing the adapter boundary.

Do not silently convert ABI violations into domain enum errors.

### Extern Types And Handles

Resource handles use `extern type`:

```marmoset
export File, open, close

extern type File

type FileOpenError = { NotFound, PermissionDenied, Other(Str) }
type FileCloseError = { AlreadyClosed, Other(Str) }

extern "std/file" as fs = {
  fn open(path: Str) => Result[File, FileOpenError]
  fn close(file: File) => Result[Unit, FileCloseError]
}
```

`extern type File` means:

- the declaring module introduces a nominal Marmoset type named `File`;
- the type can be exported and imported like any other public type;
- no Marmoset code can construct, deconstruct, pattern-match, derive equality for, or inspect it;
- generated API packages represent it as `marmoset.Handle[FileTag]`;
- Go shims own the actual Go value and lifecycle policy;
- backend-specific names such as `GoFile` never appear in public Marmoset APIs.

This requires a real frontend type/member kind, not an ordinary wrapper/product type with missing constructors. Add an explicit `NamedExtern` body or `TExtern` representation, plus module-signature/surface metadata so import resolver, constructor classification, pattern matching, derive expansion, hover/completion, and emitter all know it is opaque.

`marmoset.Handle[Tag]` must be unforgeable except for invalid zero values:

- it is a struct with unexported comparable identity;
- `marmoset.NewHandle[Tag]()` is the only valid constructor;
- zero handles are invalid and diagnosed by adapters;
- shims store actual Go values in `marmoset.HandleTable[Tag, Value]` or an equivalent shim-owned map guarded by locking;
- type mismatch between `File` and another handle type fails at Go compile time through distinct tag types;
- stale/closed handle behavior is defined by shim table lookups and wrapper domain errors.

### Bytes And Immutability

`Bytes` is immutable in Marmoset and value-like, not a resource handle.

`std/bytes.mr` is mandatory in this plan and declares the canonical `Bytes` type. The exact declaration mechanism should be `extern type Bytes`, but `Shim_boundary` maps only canonical `std.bytes.Bytes` to `BStdBytes` and `marmoset.Bytes`; ordinary non-canonical extern types map to handles.

Minimum public API needed by this plan:

```marmoset
export Bytes, from_str, to_str_lossy, length, equal?

extern type Bytes

extern "std/bytes" as bytes_abi = {
  fn from_str(s: Str) -> Bytes
  fn to_str_lossy(bytes: Bytes) -> Str
  fn length(bytes: Bytes) -> Int
  fn equal?(a: Bytes, b: Bytes) -> Bool
}

fn from_str(s: Str) -> Bytes = bytes_abi.from_str(s)
fn to_str_lossy(bytes: Bytes) -> Str = bytes_abi.to_str_lossy(bytes)
fn length(bytes: Bytes) -> Int = bytes_abi.length(bytes)
fn equal?(a: Bytes, b: Bytes) -> Bool = bytes_abi.equal?(a, b)
```

Go representation invariant:

- `marmoset.Bytes` is an opaque struct with an unexported `[]byte`;
- `marmoset.BytesCopy([]byte)` copies input;
- `marmoset.BytesFromString(string)` copies/converts into owned bytes;
- `Bytes.Copy() []byte` returns a fresh slice;
- no exported field exposes mutable backing storage.

Rules:

- Go `[]byte` returned to Marmoset as `Bytes` is copied by default.
- Passing `Bytes` into Go code that might mutate receives a copy by default.
- Unsafe borrowing helpers are forbidden in stdlib shims until a separate ownership/performance plan exists.
- Mutable-buffer APIs such as `io.Reader.Read(p []byte)` need a later `ByteBuffer`, `Stream`, or resource-specific abstraction, not immutable `Bytes`.

### Errors And Result Mapping

Go errors never cross into Marmoset. Shims map Go errors into wrapper-owned Marmoset enums:

```go
if errors.Is(err, os.ErrNotExist) {
    return marmoset.Failure[marmoset.Bytes, fileapi.FileReadError](
        fileapi.FileReadErrorNotFound{},
    )
}
```

The first `std/file` proof should use stable domain categories and an explicit `Other(Str)` variant for OS-specific or unclassified errors unless the stdlib plan locks a stricter taxonomy first. `Other(Str)` is still an explicit domain variant, not a raw `GoError` leak.

### Methods, Constants, Channels, Variadics, Generics, Interfaces

Methods stay inside Go shims. Marmoset sees functions:

```marmoset
extern "std/file" as fs = {
  fn close(file: File) => Result[Unit, FileCloseError]
}
```

The Go shim calls `file.Close()`.

Constants and sentinel values stay Go-side in phase one. Prefer zero-arg shim functions or internal Go comparisons over exporting raw constants.

Channels are not exposed. Go shims adapt goroutines/channels to future Marmoset abstractions such as `Task`, `Stream`, or `Mailbox`.

Variadics are not exposed. Shims write fixed-arity functions or accept explicit `List`/record config values once those boundary types exist.

Go generics are not exposed. Shim authors instantiate or wrap generic Go APIs in ordinary Go.

Implementing Go interfaces from Marmoset is deferred. Consuming Go interfaces behind shims is fine; producing Go method sets from Marmoset callbacks requires a separate adapter feature.

### Callback Follow-Up Policy

Callbacks are not part of this milestone. The follow-up callback plan should start with pure, monomorphic, non-concurrent, non-reentrant, non-escaping callbacks only:

- reject effectful `=>` callbacks at the shim boundary in the first callback slice;
- callback handles are invalid after the shim call returns;
- late, concurrent, or reentrant callback invocation is diagnosed;
- panics from Marmoset callbacks are recovered at the adapter boundary and become ABI diagnostics.

This policy should be recorded now so the core shim ABI does not accidentally design against impossible callback requirements.

## Implementation Plan

### Phase S0: Roadmap And Plan Reconciliation Gate

**Goal:** Prevent direct-Go FFI expansion and shim-first interop from being implemented in parallel.

Files:

- `docs/plans/todo/language/05_stdlib.md`
- `docs/features/ffi.md`
- `docs/ARCHITECTURE.md`
- `docs/ROADMAP.md`

Changes:

- Confirm the former direct-FFI multi-return/private-representation draft is not present under `docs/plans/todo/language`.
- Update `05_stdlib.md` so file, bytes, HTTP, DB, and framework wrappers depend on shim phases.
- Update `docs/features/ffi.md` to describe shim externs as the canonical future interop model and direct Go externs as historical/current-v1 until code removal lands.
- Update `docs/ARCHITECTURE.md` with auxiliary Go package emission, generated API packages, shim catalog, and the `runtime/go/marmoset` support package.
- Update `docs/ROADMAP.md` so this shim-first interop plan precedes the stdlib expansion work.

Tests:

- Documentation-only phase. Run `git diff --check`.

Gate:

- Plan-review approval before implementation phases begin.

### Phase S1: Auxiliary Go Package Build Layout

**Goal:** Teach the CLI/build pipeline to emit and build multi-file Go output before any shim semantics depend on it.

Files:

- `bin/main.ml`
- `lib/frontend/compiler.ml`
- `lib/backend/go/emitter.ml`
- `test/integration/common.sh`
- `test/integration/13_ffi.sh`
- `test/snapshots/go_tree/**`

Changes:

- Extend codegen output with `go_mod` and auxiliary Go files:

```ocaml
type go_aux_file = {
  rel_path : string;
  contents : string;
}

type build_output = {
  go_mod : string;
  main_go : string;
  runtime_go : string;
  aux_go_files : go_aux_file list;
  diagnostics : Diagnostic.t list;
}
```

- Have `bin/main.ml` write `go_mod`, `main.go`, `runtime.go`, and `aux_go_files` into the temp build directory.
- Have `--emit-go` write the same complete tree, including `go.mod`.
- Create parent directories for auxiliary files.
- Reject absolute paths, `..`, empty components, paths that normalize outside the build root, duplicate normalized relative paths, and attempts to overwrite `main.go`, `runtime.go`, or `go.mod`.
- Sort auxiliary output deterministically.
- Add recursive tree snapshot helpers under `test/snapshots/go_tree/<case>/` that compare sorted relative file lists and file contents.
- Keep existing single-file snapshots for non-shim `main.go` cases where useful.

Tests:

- Unit tests for path normalization and rejection.
- Emit-go tree snapshot for synthetic auxiliary files.
- Integration test proving `go.mod` appears in `--emit-go`.
- Build failure test where malformed auxiliary Go code produces a classified Go build diagnostic. The malformed file must be in the root `main` package or imported by `main.go`; otherwise `go build .` will not compile it.

Gate:

```bash
dune runtest lib/backend/go
make integration ffi
```

### Phase S2: Shim Extern Syntax, Catalog, Typechecking, And Direct FFI Removal

**Goal:** Reinterpret `extern "id"` as a shim declaration, remove direct Go import-path extern behavior, and produce typed shim artifacts.

Files:

- `lib/frontend/syntax/surface_ast.ml`
- `lib/frontend/syntax/ast.ml`
- `lib/frontend/syntax/parser.ml`
- `lib/frontend/syntax/lower.ml`
- `lib/frontend/typecheck/extern_registry.ml` or replacement `shim_registry.ml`
- `lib/frontend/typecheck/shim_boundary.ml`
- `lib/frontend/typecheck/shim_catalog.ml`
- `lib/frontend/discovery.ml`
- `lib/frontend/typecheck/resolution_artifacts.ml`
- `lib/frontend/typecheck/infer.ml`
- `lib/frontend/typecheck/checker.ml`
- `lib/frontend/compiler.ml`
- `lib/frontend/import_resolver.ml`
- `runtime/go/dune`
- `tools/lsp/lib/source_syntax.ml`
- `tools/lsp/lib/definition.ml`
- `tools/lsp/lib/completions.ml`

Syntax:

```marmoset
extern "std/file" as fs = {
  fn read(path: Str) => Result[Bytes, FileReadError]
}
```

Changes:

- Rename AST and artifact fields from Go-path language to shim-id language.
- Keep the top-level `extern` grammar, but validate the string as a shim id.
- Permit optional `as alias`.
- Extend extern function parsing to accept ordinary Marmoset function identifier suffixes such as `?` and `!`.
- Add `Shim_catalog`:
  - discover checked-in shim ids under `runtime/go/shims/**`;
  - support synthetic `test/*` shim ids in tests;
  - reject unknown ids with `shim-id-not-found`;
  - reject malformed ids with `shim-id-invalid`;
  - add install rules so an installed compiler can locate `runtime/go/marmoset` and `runtime/go/shims`.
- Extend toolchain-root discovery so `runtime/go/marmoset` and `runtime/go/shims` are resolved from the same source tree or installed share root as `std/prelude.mr`. Add tests for source-tree lookup, `MARMOSET_ROOT`, and installed-style `share/marmoset` lookup.
- Add project-wide `validate_project_shim_ownership`:
  - owner module id must match shim id after `/` to `.` conversion;
  - duplicate shim id in multiple modules is rejected even with matching signatures;
  - multiple blocks for the same shim id in one module are rejected in phase one.
- Add deterministic Go symbol mangling and `shim-symbol-collision`.
- Replace direct extern artifacts with `shim_func`, `shim_call`, and `ShimQualifiedCall`.
- Remove direct Go package import wrapper emission for externs.
- Reject exporting shim functions directly as described in the design section.
- Keep shim qualifiers module-local and validate collisions against top-level declarations, imported namespaces, imported direct bindings, and implicit core bindings.
- Add `Shim_boundary` classification keyed by resolved module/type identity.
- Preserve extern parameter/return type spans or run boundary validation before lowering loses nested type spans.
- Ensure pure wrappers cannot call effectful shim functions.
- Replace direct-FFI fixtures with shim fixtures or move historical direct-FFI examples to docs.

Diagnostics:

- `type-shim-boundary`
- `module-export-shim`
- `shim-id-invalid`
- `shim-id-not-found`
- `shim-owner-mismatch`
- `shim-owner-duplicate`
- `shim-symbol-collision`
- `shim-function-duplicate`
- `shim-block-duplicate`
- `module-extern-qualifier-collision` for qualifier collisions unless renamed consistently across module resolver tests

Diagnostic precedence:

- syntax/shape validation runs before catalog lookup;
- single-segment ids such as `strings` are always `shim-id-invalid`;
- well-formed but absent ids are `shim-id-not-found`;
- duplicate blocks in the owner module are `shim-block-duplicate`;
- duplicate functions within one block are `shim-function-duplicate`;
- post-mangling Go symbol collisions are `shim-symbol-collision`.

Tests:

- Parser tests for shim extern declarations with and without aliases.
- Parser tests for extern names with `?` and `!`.
- Typechecker tests for accepted scalar, canonical `Option`, canonical `Result`, and owner-module enum shim signatures.
- Negative tests for local/impostor `Result`, imported `foo.Result`, alias-shadowed `Option`, unsupported nested boundary types, imported user enums, exported shim-only functions, duplicate shim functions, post-mangling symbol collisions, qualifier collisions, malformed shim ids, missing shim ids, owner mismatch, duplicate ownership, old `extern "strings"` direct-FFI assumptions, and pure wrappers calling effectful shims.
- LSP source syntax/navigation tests for the revised extern meaning.

Gate:

```bash
dune runtest lib/frontend/syntax
dune runtest lib/frontend/typecheck
dune runtest tools/lsp/lib
```

### Phase S3: `marmoset` Support Package And Generated API Packages

**Goal:** Add the typed Go support package and generated API package skeletons from shim artifacts.

Files:

- `runtime/go/marmoset/result.go`
- `runtime/go/marmoset/option.go`
- `runtime/go/marmoset/unit.go`
- `runtime/go/marmoset/bytes.go`
- `runtime/go/marmoset/handle.go`
- `runtime/go/dune`
- `lib/backend/go/emitter.ml`
- `test/snapshots/go_tree/**`

Changes:

- Add generic `Result[T, E]`, `Option[T]`, and `Unit` types.
- Add placeholder `Bytes` and `Handle` files only if needed for package layout; boundary use remains rejected until later phases.
- Use package-level generic functions, not generic methods, for Go 1.18 compatibility.
- Generate API packages under `api/<shim-id>/api.go`.
- Generated API packages include only owner-module closed enums supported by `Shim_boundary`.
- Enum variant payloads use `Field0`, `Field1`, ... positional fields.
- Generated API package clauses and import aliases are keyword-safe and deterministic.
- Copy `runtime/go/marmoset` only when a program uses at least one called shim.
- Do not introduce a dynamic ABI value carrier.
- Add a focused Go build/test fixture that compiles generated `marmoset`, `api`, and copied stub shim packages under `go 1.18`.
- The focused package compile fixture must either import the generated stub shim from `main.go` or run an explicit `go test ./...`/package compile helper over the emitted tree. Do not rely on `go build .` to compile unimported packages.

Tests:

- Tree snapshot proving no `runtime/go/marmoset` package is emitted for ordinary programs without shims.
- Tree snapshot proving the support package is emitted once for multiple shim calls.
- Tree snapshot for a generated API package containing a closed enum with nullary and positional-payload variants.
- Compile test for generated API package names with keyword-like or collision-prone source names.
- Go 1.18 compatibility smoke test for support package generics.

Gate:

```bash
dune runtest lib/backend/go
```

S3's integration coverage is tree/compile oriented. If `make integration ffi` still assumes runnable direct-FFI calls after S2, keep that gate in S4 and use backend/tree snapshot tests for S3.

### Phase S4: Typed Adapter Generation For Scalars, Option, Result, And Enums

**Goal:** Generate adapters between concrete Marmoset representations and typed ABI values for the first useful boundary set, then build and run real shim calls.

Files:

- `lib/backend/go/emitter.ml`
- `lib/frontend/typecheck/types.ml`
- `lib/frontend/typecheck/display_names.ml`
- `runtime/go/shims/test/scalar/**`
- `runtime/go/shims/test/result/**`
- `test/integration/13_ffi.sh`
- `test/snapshots/go_tree/**`

Changes:

- Generate outbound conversion from concrete Marmoset values to typed ABI parameters.
- Generate inbound conversion from typed ABI return values to concrete Marmoset values.
- Generate enum conversion keyed by stable enum identity from the owner module.
- Recognize canonical stdlib `Option` and `Result` identities from `Shim_boundary`, not generated names.
- Sort converter emission deterministically.
- Copy called shim packages into the build tree.
- Generate adapter calls using deterministic shim import aliases and Go symbol names.
- Wrap shim calls with panic recovery that reports ABI diagnostics.
- Let Go compile-time errors catch wrong shim signatures. Runtime diagnostics still handle invalid zero-value `Result`/`Option`, nil enum interfaces, invalid payloads, and adapter invariants.

Representative generated wrapper:

```go
func extern__std_file__read(path string) Result_bytes_file_read_error {
    __ret := mshim_std_file.Read(path)
    return __marmoset_from_abi_Result_bytes_file_read_error("std/file.read", __ret)
}
```

Tests:

- Runtime integration for scalar shim calls.
- Runtime integration for `Option[Int]`.
- Runtime integration for `Result[Int, Str]` using `Success` and `Failure`.
- Runtime integration for custom enum error returns.
- Compile-failure fixture where shim return type does not match the generated API signature.
- Runtime-failure fixture where shim returns zero-value `Result`.
- Runtime-failure fixture where shim returns nil enum interface.
- Exact tree snapshots for converter generation and import ordering.

Gate:

```bash
dune runtest lib/backend/go
make integration ffi
make integration snapshots
```

### Phase S5: `extern type` Handles

**Goal:** Support resource wrappers without exposing Go pointers or generated representation details.

Files:

- `lib/frontend/syntax/surface_ast.ml`
- `lib/frontend/syntax/ast.ml`
- `lib/frontend/syntax/parser.ml`
- `lib/frontend/syntax/lower.ml`
- `lib/frontend/typecheck/type_registry.ml`
- `lib/frontend/typecheck/module_sig.ml`
- `lib/frontend/typecheck/derive_expand.ml`
- `lib/frontend/typecheck/structural.ml`
- `lib/frontend/import_resolver.ml`
- `lib/frontend/compiler.ml`
- `lib/frontend/typecheck/shim_boundary.ml`
- `lib/backend/go/emitter.ml`
- `runtime/go/marmoset/handle.go`
- `tools/lsp/lib/hover.ml`
- `tools/lsp/lib/completions.ml`
- `tools/lsp/lib/semantic_tokens.ml`
- `tools/lsp/lib/doc_symbols.ml`
- `tools/lsp/lib/folding_ranges.ml`

Syntax:

```marmoset
extern type File
```

Changes:

- Add an extern named type declaration with no constructor and no Marmoset representation.
- Add explicit frontend representation such as `NamedExtern` or `TExtern`.
- Permit extern types in exported signatures.
- Reject pattern matching, construction, equality derivation, literal conversion, and representation introspection for extern types.
- Generate typed handle aliases in API packages:

```go
type FileTag struct{}
type File = marmoset.Handle[FileTag]
```

- Add `marmoset.Handle[Tag]` and `marmoset.HandleTable[Tag, Value]` with explicit locking or require equivalent shim-owned locking.
- Require lifecycle to be modeled by ordinary effectful functions such as `close(file)`.
- Do not add pointer equality or handle introspection in this phase.

Tests:

- Public module can export `File` and functions using it.
- Importer can pass `File` values but cannot construct or inspect them.
- Attempts to construct, pattern match, derive equality/default/show, or structurally inspect an extern type are rejected with explicit diagnostics.
- Shim can return a `File` handle and later receive it back.
- Zero handle returned by a shim is diagnosed as an ABI violation.
- Type mismatch between two handle types fails at Go compile time.
- Closed/stale handle behavior is defined by shim table code and tested through a synthetic shim.
- LSP hover/completion/semantic tokens/document symbols/folding continue to compile and do not expose representation.

Gate:

```bash
dune runtest lib/frontend
dune runtest tools/lsp/lib
make integration ffi
```

### Phase S6: Immutable Bytes

**Goal:** Add canonical `std.bytes.Bytes` as the first non-scalar value-like shim type.

Files:

- `std/bytes.mr`
- `runtime/go/marmoset/bytes.go`
- `runtime/go/shims/std/bytes/**`
- `runtime/go/shims/test/bytes/**`
- `lib/frontend/typecheck/shim_boundary.ml`
- `lib/backend/go/emitter.ml`
- `docs/features/ffi.md`
- `docs/plans/todo/language/05_stdlib.md`

Changes:

- Add `std/bytes.mr` with the minimal API from the design section.
- Map only canonical `std.bytes.Bytes` to `BStdBytes`.
- Add `marmoset.BytesCopy`, `marmoset.BytesFromString`, and `Bytes.Copy`.
- Require copies on Go-to-Marmoset returns and Marmoset-to-Go parameters by default.
- Add no mutable buffer API in this phase.
- Add compile test proving shims cannot mutate `marmoset.Bytes` through exported fields.
- Use a synthetic `test/bytes` shim for aliasing tests before `std/file` depends on bytes.

Tests:

- `bytes.from_str`, `bytes.to_str_lossy`, `bytes.length`, and `bytes.equal?` work end-to-end.
- A shim mutates the original Go slice after returning; Marmoset `Bytes` remains unchanged.
- Passing `Bytes` to a shim that mutates its copied parameter does not mutate the original Marmoset value.
- Exact tree snapshot for bytes converter helpers.

Gate:

```bash
dune runtest lib/frontend/typecheck
dune runtest lib/backend/go
make integration ffi
```

### Phase S7: Stdlib File Proof Slice

**Goal:** Prove shim-first stdlib development on a small but real file wrapper after core adapters, handles, and bytes are available.

Files:

- `std/file.mr`
- `runtime/go/shims/std/file/**`
- `test/integration/14_stdlib_shims.sh`
- `test/integration.sh`
- `test/integration/common.sh`
- `docs/plans/todo/language/05_stdlib.md`

Initial APIs:

```marmoset
import std.bytes.Bytes

file.read(path: Str) => Result[Bytes, FileReadError]
file.write(path: Str, bytes: Bytes) => Result[Unit, FileWriteError]
file.open(path: Str) => Result[File, FileOpenError]
file.close(file: File) => Result[Unit, FileCloseError]
```

Candidate domain errors for the proof:

```marmoset
type FileReadError = { NotFound, PermissionDenied, IsDirectory, Other(Str) }
type FileWriteError = { NotFound, PermissionDenied, IsDirectory, Other(Str) }
type FileOpenError = { NotFound, PermissionDenied, IsDirectory, Other(Str) }
type FileCloseError = { AlreadyClosed, Other(Str) }
```

These variants may be tightened by the stdlib plan, but S7 must lock the exact variants it tests before implementation starts.

Changes:

- Map Go errors to domain enums in Go shim code.
- Use `errors.Is`, `os.IsNotExist`, and `os.IsPermission` in Go, not Go-shaped error values in Marmoset.
- Keep all public IO functions effectful.
- Record exact domain error variants in docs and tests.
- Ensure no public API contains Go implementation names or `marmoset` ABI terms.
- Add `test/integration/14_stdlib_shims.sh`.
- Wire integration selectors `stdlib`, `stdlib-shims`, and `14_stdlib_shims.sh`.

Tests:

- Read/write round trip with `Bytes`.
- Missing file maps to `FileReadError.NotFound` or `FileOpenError.NotFound`.
- Permission or stable synthetic error fixture maps to the expected enum.
- `open` returns a `File` handle that can be passed to `close`.
- Handle close is idempotent or explicitly reports `AlreadyClosed`, whichever `std/file.mr` defines.
- Public imports can pass `File` values but cannot construct them.

Gate:

```bash
make integration ffi
make integration stdlib-shims
```

Do not use a full integration run as the normal development gate unless the user explicitly asks.

## Testing Strategy

- Syntax and lowering tests live in `lib/frontend/syntax/parser.ml` and `lower.ml`.
- Shim catalog and boundary tests live in `lib/frontend/typecheck/shim_catalog.ml`, `shim_boundary.ml`, `checker.ml`, and project-level compiler tests.
- Build-output tests live in `bin/main.ml` unit helpers where possible and recursive integration snapshots otherwise.
- API-package generation and adapter-generation tests live in `lib/backend/go/emitter.ml` plus tree snapshots under `test/snapshots/go_tree`.
- End-to-end shim tests live under `test/integration/13_ffi.sh` until `test/integration/14_stdlib_shims.sh` is introduced for stdlib shims.
- Stdlib proof tests should use real filesystem temp dirs and stable error fixtures.
- Existing direct-FFI snapshot tests must either be rewritten as shim tests or deliberately moved to historical coverage that is no longer part of current compiler behavior.

Minimum focused verification by final phase:

```bash
dune runtest lib/frontend/syntax
dune runtest lib/frontend/typecheck
dune runtest lib/backend/go
dune runtest tools/lsp/lib
make integration ffi
make integration snapshots
make integration stdlib-shims
```

Full integration remains a user-run gate unless explicitly requested.

## Commit Plan

1. Roadmap/docs reconciliation: mark direct-FFI expansion superseded, move stdlib behind shim interop, update architecture/feature docs.
2. Auxiliary Go package output and recursive tree snapshots.
3. Shim extern syntax/catalog/typechecking, artifact migration, and direct FFI removal.
4. `marmoset` support package skeleton and generated typed API packages.
5. Typed scalar/`Option`/`Result`/enum adapter generation with real shim builds.
6. `extern type` handles and handle ABI.
7. Immutable `Bytes` boundary plus `std.bytes`.
8. `std.file` proof slice and stdlib shim integration suite.

Each commit should add failing tests first, make only the targeted slice pass, and record focused verification in the commit message.

## Risks

1. **Breaking existing direct FFI users.** This is intentional, but docs and roadmap must say the replacement is shim externs.
2. **Generated API churn.** Shims intentionally compile-break when wrapper signatures change. That is good for correctness but needs clear docs and editor support in a follow-up user-shim plan.
3. **Package cycles.** The dependency graph must be one-way: generated `main` imports generated `api` and copied `shims`; shims import generated `api` and `marmoset`; generated `api` imports only `marmoset` in this milestone. Imported user boundary types are rejected to avoid cross-API cycles.
4. **Build layout leaks.** Local import paths and `go.mod` generation must be deterministic and must not leak machine paths.
5. **Boundary scope creep.** Adding too many boundary types at once risks recreating direct FFI complexity. Each type needs explicit copy, ownership, and diagnostic rules.
6. **Copying cost.** Bytes immutability requires copies. Optimize only after correctness is pinned.
7. **Handle lifecycle.** The compiler can enforce opaqueness, but close/idempotency/error policy belongs to each stdlib shim.
8. **User shim packaging.** Project-local shims need conventions for source layout, generated API discovery, and security review. This milestone starts with toolchain/test shims only.
9. **Framework callbacks.** HTTP-style callbacks involve concurrency and effects. They are deliberately follow-up work.

## Resolved Questions

1. Direct FFI is removed instead of expanded.
2. The Go ABI support package is named `marmoset`.
3. The ABI is fully typed with generated API packages.
4. Shim declarations use `extern "std/file" = { ... }`; no additional keyword is added.
5. Resource handles use `extern type File`.
6. Checked-in Go runtime support and shims live under `runtime/go/marmoset` and `runtime/go/shims`.
7. Phase one supports toolchain and synthetic test shims only; project-local shims are a follow-up plan.
8. API packages are keyed by shim id, and phase-one owner module id must match shim id after `/` to `.` conversion.
9. Canonical `Result` ABI uses `Success`/`Failure`, matching `std/result.mr`.
10. Callbacks are not part of this milestone.

## Open Questions

1. Exact stdlib file error taxonomy can still be tightened in `05_stdlib.md`; S7 must lock the variants it tests before implementation starts.
2. A follow-up user-shim packaging plan must decide generated API preview/editor workflow and API package stability guarantees.
3. A follow-up callback plan must validate the pure-only, non-escaping policy against a real framework wrapper.

## Progress

- 2026-05-08 23:06 CEST: Draft plan created after reconsidering direct Go-shaped FFI expansion; scope set to shim-first Go interop, adapter generation, immutable bytes, resource handles, stdlib proof shims, and a later rewrite of existing FFI/stdlib plan ordering.
- 2026-05-08 23:27 CEST: Revised after design decisions: direct FFI removed, `extern` now names shim ids, ABI package renamed to `marmoset`, dynamic ABI dropped in favor of generated typed API packages, handles use `extern type`, and checked-in Go runtime layout moved to `runtime/go/marmoset` plus `runtime/go/shims`.
- 2026-05-08 23:32 CEST: Added the phase-one shim ownership invariant and deterministic Marmoset-name to exported-Go-symbol mapping so lowercase wrapper declarations compile against exported Go shim functions without ad hoc naming.
- 2026-05-08 23:34 CEST: Codex-only plan-review started with parallel frontend, backend/build, ABI semantics, and roadmap/testing review passes.
- 2026-05-08 23:42 CEST: Revised after Codex review passes. Reordered phases, merged roadmap rewrite into S0, added shim catalog and owner-module invariants, replaced direct-FFI artifacts with shim artifacts, locked canonical `Result.Success`/`Failure` ABI, specified enum positional payload ABI, added runtime ABI diagnostics, made `extern type` a real frontend type kind, tightened handle/bytes invariants, moved callbacks to follow-up, and added recursive Go tree snapshot requirements.
- 2026-05-08 23:48 CEST: Final Codex review tightening completed. Added `Result`/`Option` inspector helpers, deterministic shim-id diagnostic precedence, runtime lookup through toolchain discovery, implementable aux-package compile tests, missing `extern type` touchpoints, explicit `std.bytes.Bytes` import in `std.file`, and a tree/compile-only S3 gate.
- 2026-05-08 23:52 CEST: Renumbered from `10_shim-first-go-interop.md` to `04_shim-first-go-interop.md` as the next language milestone, removed the superseded direct-FFI expansion draft from todo, and updated stdlib references to the renumbered `05_stdlib.md`.
- 2026-05-09 00:01 CEST: S0 documentation reconciliation slice started. Confirmed no superseded direct-FFI expansion draft remains under `docs/plans/todo/language`; `05_stdlib.md` and `ROADMAP.md` already point stdlib work behind this shim-first interop plan.
- 2026-05-09 00:03 CEST: S0 docs reconciled in `docs/features/ffi.md` and `docs/ARCHITECTURE.md`; `git diff --check` passed.
- 2026-05-09 00:05 CEST: S0 committed as `3f237f7` (`Document shim-first interop direction`).
- 2026-05-09 00:05 CEST: S1 auxiliary Go package build-layout slice started test-first. Inspecting `bin/main.ml`, `lib/frontend/compiler.ml`, `lib/backend/go/emitter.ml`, `test/integration/common.sh`, and `test/integration/13_ffi.sh`.
- 2026-05-09 00:05 CEST: S1 red observed with `dune runtest lib/backend/go` failing on missing `validate_go_aux_files`; implemented `go_mod`, sorted auxiliary output files, safe relative-path validation, complete CLI tree writing, recursive tree snapshot helpers, and the `--emit-go` go.mod integration check. `dune runtest lib/backend/go`, `git diff --check`, and `make integration ffi` passed.
- 2026-05-09 00:05 CEST: Added S1 malformed root auxiliary Go compile-failure coverage; `dune runtest lib/backend/go` passed with the classifier test.
- 2026-05-09 00:09 CEST: S2 shim extern syntax/catalog/typechecking slice started test-first. Added frontend expectations for shim ids replacing direct Go import paths, owner-module enforcement, synthetic `test/*` catalog entries, boundary classification, shim artifacts, and LSP/backend fallout.
- 2026-05-09 00:23 CEST: S2 implementation reached focused green state locally: parser/syntax accepts shim ids and suffixed extern names, typechecking rejects direct Go package externs and unsupported boundaries, project compilation enforces shim ownership and module-local shim qualifiers, LSP semantic-token fixtures use owner-aware shim documents, and the Go backend now emits temporary placeholder shim adapter wrappers instead of direct package imports.
- 2026-05-09 00:24 CEST: S2 committed as `f6983ac` (`Replace direct FFI artifacts with shim externs`) after `git diff --check`, `dune runtest lib/frontend/syntax`, `dune runtest lib/frontend/typecheck`, `dune runtest lib/frontend`, `dune runtest lib/backend/go`, and `dune runtest tools/lsp/lib`.
- 2026-05-09 00:24 CEST: S3 support-package/API-package slice started test-first. No `runtime/` tree exists yet, so this slice will add `runtime/go/marmoset`, emit it only for called shims, and generate deterministic `api/<shim-id>/api.go` skeletons from owner-module enum boundary metadata.
- 2026-05-09 00:31 CEST: S3 support-package/API-package slice reached focused green state: called shim builds emit checked-in `runtime/go/marmoset` support files and deterministic generated `api/<shim-id>/api.go` skeletons, unused shim declarations emit no auxiliary runtime tree, owner enums appear in the shim API package, and the generated support/API packages compile under Go. Verification passed with `git diff --check`, `dune runtest lib/frontend/typecheck`, `dune runtest lib/backend/go`, and `dune runtest lib/frontend`.
- 2026-05-09 00:31 CEST: S3 committed as `eb27fe7` (`Add shim support and API package output`).
- 2026-05-09 00:31 CEST: S4 typed-adapter slice started test-first with backend expectations for real shim imports/calls and copied `runtime/go/shims` packages replacing placeholder adapter panics.
- 2026-05-09 00:45 CEST: S4 typed-adapter slice reached focused green state: generated wrappers now import copied shim packages, call exported shim symbols, convert scalar, canonical `Option`, canonical `Result`, and owner enum values across the typed ABI, recover shim/adapter panics as ABI failures, copy checked-in test shim packages, and cover Go compile-time signature mismatches plus zero-value `Result`/nil enum runtime failures. Verification passed with `git diff --check`, `dune runtest lib/frontend/typecheck`, `dune runtest lib/backend/go`, `dune runtest lib/frontend`, `make integration ffi`, and `make integration snapshots`.
- 2026-05-09 01:06 CEST: S4 committed as `d287e18` (`Generate typed shim adapters`).
- 2026-05-09 01:06 CEST: S5 extern-type handle slice completed test-first. Red was observed with `make integration ffi` failing because `extern type File` still parsed as an extern signature requiring a quoted shim id. The slice added frontend `extern type` syntax/AST/lowering/import handling, extern named-type registry support, opaque construction/field/pattern/derive diagnostics, `BExternHandle` classification, generated API handle aliases, `marmoset.Handle`/`HandleTable`, typed handle adapter validation, synthetic handle shims, LSP coverage, and FFI integration positives/negatives for handle return/pass/stale/zero/wrong-type behavior. Verification passed with `dune runtest lib/frontend`, `dune runtest tools/lsp/lib`, `dune runtest lib/backend/go`, `make integration ffi`, and `git diff --check`.
- 2026-05-09 01:06 CEST: S5 committed as `d32ab5c` (`Add extern handle support`).
- 2026-05-09 01:06 CEST: S6 immutable Bytes slice started test-first. Added red expectations for canonical `std.bytes.Bytes` boundary classification, `std.bytes` wrapper use, byte-return and byte-parameter copy behavior through a synthetic `test/bytes` shim, noncanonical `extern type Bytes` staying handle-shaped, and direct construction/field inspection rejection.
- 2026-05-09 01:14 CEST: S6 red observed with `dune runtest lib/frontend/typecheck` failing on canonical `std.bytes.Bytes` classification and `make integration ffi` failing because `std.bytes` and `test/bytes` were not yet implemented.
- 2026-05-09 01:14 CEST: S6 reached focused green state: added `std/bytes.mr`, installed it in the toolchain stdlib, mapped only canonical `std__bytes__Bytes` to `BStdBytes`, represented it as `marmoset.Bytes` in Go, copied Bytes on both shim ingress and return, added checked-in `std/bytes` and `test/bytes` Go shims, added a backend Go compile guard proving shim packages cannot assign to `marmoset.Bytes.data`, documented canonical Bytes boundary rules, and regenerated the recursive Go tree snapshot. Verification passed with `dune runtest lib/frontend/typecheck`, `dune runtest lib/backend/go`, and `make integration ffi`.
- 2026-05-09 01:19 CEST: S6 committed as `f5f7d30` (`Add immutable Bytes shim boundary`).
- 2026-05-09 01:19 CEST: S7 std.file proof slice started test-first. Added `14_stdlib_shims.sh` and integration selectors for `stdlib`, `stdlib-shims`, and `14_stdlib_shims.sh`; red was observed with `make integration stdlib-shims` failing because `std.file` was not yet implemented.
- 2026-05-09 01:19 CEST: S7 reached focused green state: added `std/file.mr`, installed it in the toolchain stdlib, added checked-in `std/file` Go shim code with explicit `NotFound`, `PermissionDenied`, `IsDirectory`, `Other(Str)`, and `AlreadyClosed` mappings, tested read/write Bytes round trips, missing/directory error mapping, open/close handle flow, double-close behavior, and public `File` construction rejection. Fixed generated `Option`/`Result` adapter inspectors to avoid unused payload locals for erased payload conversions such as `Result[Unit, E]`. Verification passed with `make integration stdlib-shims`, `make integration ffi`, and `dune runtest lib/backend/go`.
