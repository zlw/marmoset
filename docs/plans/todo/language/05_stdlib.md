# Stdlib Plan (Shim-First)

## Maintenance

- Last verified: 2026-05-12
- Implementation status: In progress (`std.basics`, `std.bytes`, `std.path`, `std.file`, `std.dir`, and trait-shaped `std.io` proof slices landed; broader stdlib expansion pending)
- Update trigger: Any stdlib, shim interop, prelude, module discovery, `Bytes`, resource-handle, or collection representation change
- Prerequisites:
  - `docs/plans/done/language/06_module-system.md`
  - `docs/plans/done/language/07_prelude.md`
  - `docs/plans/done/language/09_shim-first-go-interop.md`

## Context

Marmoset compiles to Go but is not Go's sibling. The stdlib should expose idiomatic Marmoset APIs: module functions, pipe-friendly data-first argument order, immutable-by-default values, Ruby-inspired naming, `?` suffix predicates, and explicit effect/failure surfaces.

The old stdlib draft assumed direct FFI wrappers around Go packages. That is no longer the interop direction. Go-backed stdlib modules now use checked-in Go shims under `runtime/go/shims/std/...`, typed generated ABI packages, and public Marmoset modules under `std/*.mr`.

Dependency order:

```text
Modules -> Prelude -> Shim-first Go interop -> Stdlib
```

## Goals

1. Build stdlib modules as ordinary Marmoset modules with backend-neutral public APIs.
2. Use shim externs for Go-backed modules; do not expose Go package paths, errors, pointers, channels, or ABI helper types.
3. Keep IO effectful and failable operations explicit with `Result[T, DomainError]`.
4. Establish canonical `Bytes` and high-level file surfaces from the shim-first proof and harden them into the broader stdlib.
5. Add pure collection/string helpers with stable module-qualified identities that later optimization plans can target.
6. Keep concurrency, HTTP server callbacks, SQL/sqlc, and project-local shim packaging out of this first stdlib pass unless separate plans make them concrete.

## Non-Goals

1. Do not use direct Go FFI or direct emitter special cases for new stdlib modules.
2. Do not expose raw Go errors. Domain errors are explicit Marmoset enums.
3. Do not expose raw Go handles, pointers, channels, readers, writers, or mutable byte slices.
4. Do not implement a full batteries-included stdlib in one milestone.
5. Do not optimize higher-order functions here; `09_hof_optimization.md` owns optimization after APIs land.

## Locked Decisions

1. **Module-function-first APIs.** Collections and strings use qualified functions such as `list.map(xs, f)` and `str.upcase(s)`. Dot syntax is not the primary stdlib abstraction surface.
2. **Immutable wrappers.** `Bytes`, `List`, and `Map` APIs must not expose Go mutation or aliasing.
3. **All IO is effectful.** Filesystem, terminal, environment, time, and similar operations use `=>`.
4. **Failable IO returns domain errors.** Prefer `Result[T, FileReadError]` over `Result[T, Str]` when the module can define meaningful error categories.
5. **Ruby-inspired naming.** `select`/`reject`, `upcase`/`downcase`, `include?`, `strip`, and related names remain preferred.
6. **`?` suffix for predicates.** This is already relevant to stdlib naming and must be supported by parser/emitter before predicate-heavy modules land.
7. **Separate modules for distinct domains.** `io` is terminal IO; `file` is filesystem IO; `bytes` is immutable byte data; `str` is Unicode/string helpers.
8. **Concurrency model TBD.** No raw Go channels. Future concurrency should use Marmoset-owned abstractions such as `Task`, `Stream`, or `Mailbox`.

## Current State

- `std/prelude.mr`, `std/option.mr`, and `std/result.mr` are toolchain stdlib modules loaded through the normal module pipeline.
- `Option` and `Result` are canonical stdlib nominal types with inherent helper APIs.
- `09_shim-first-go-interop.md` has introduced `extern type`, checked-in Go shims, canonical immutable `std.bytes.Bytes`, and the initial `std.file` proof slice.
- `std.basics` currently owns the implicit terminal output function `puts`; `docs/plans/todo/language/11_io_output_names.md` supersedes that compatibility surface and plans to move canonical line output to `std.io.puts`.
- `std.file` now exposes text-first whole-file helpers, explicit byte helpers, and scoped handle callbacks backed by private shim plumbing.
- `std.path` owns pure path values and `std.dir` exposes filesystem-backed directory operations over high-level `Entry(path.Path, Kind)` values.
- `std.io` now exposes low-level `Read[r, e]`/`Write[w, e]` protocols plus default terminal helpers backed by private singleton extern receiver values. Stderr helpers live in `std.io.err`.
- `docs/features/ffi.md` now documents shim-first interop as the current direction, with direct package externs retained only as historical context.
- Existing list/map/str examples are mostly fixtures and exploratory docs; they are not committed full stdlib modules yet.

## Core Modules

### `std.basics`

Minimal implementation substrate for prelude functions that need shim-backed runtime behavior before broader stdlib modules land.

Note: `docs/plans/todo/language/11_io_output_names.md` supersedes the public
`puts` direction below. `std.basics.puts` is a temporary compatibility surface;
the target public line-output API is `std.io.puts`.

Current API:

```marmoset
export puts

extern "std/basics" = {
  fn puts_str(value: Str) => Unit
}

fn puts[a: Show](value: a) => Unit
```

Rules:

- Public code should keep using implicit `puts`; `std.basics.puts` is the generic `Show`-based function and calls the private `puts_str` shim directly.
- This module should stay small. Domain-specific IO belongs in `std.io`, `std.file`, or other explicit modules.

### `std.bytes`

Canonical immutable byte data. Introduced by `09_shim-first-go-interop.md` and hardened here.

Minimum API:

```marmoset
export Bytes, DecodeError, Decode, Encode,
       from_str, to_str, to_str_lossy, length, equal?

extern type Bytes
type DecodeError = { InvalidUtf8 = "Invalid UTF-8" }

trait Decode[a] = {
  fn decode(bytes: Bytes) -> Result[a, DecodeError]
}

trait Encode[a] = {
  fn encode(value: a) -> Bytes
}

fn from_str(s: Str) -> Bytes
fn to_str(bytes: Bytes) -> Result[Str, DecodeError]
fn to_str_lossy(bytes: Bytes) -> Str
fn length(bytes: Bytes) -> Int
fn equal?(a: Bytes, b: Bytes) -> Bool

impl Decode[Bytes]
impl Encode[Bytes]
impl Decode[Str]
impl Encode[Str]
```

Rules:

- `Bytes` is immutable.
- Go shims copy on ingress and egress.
- `to_str` is strict UTF-8; `to_str_lossy` is explicit when callers want replacement behavior.
- File APIs use `Decode`/`Encode` so public file helpers do not need text and byte variants.
- No mutable buffer API in this milestone.

### `std.path`

Pure path values and path manipulation. This module does not touch the filesystem.

Target API:

```marmoset
export Path,
       join, dirname, basename, extname,
       clean, absolute?, relative?

type Path = Path(Str)

fn join(left: Path, right: Path) -> Path
fn dirname(path: Path) -> Path
fn basename(path: Path) -> Str
fn extname(path: Path) -> Str
fn clean(path: Path) -> Path
fn absolute?(path: Path) -> Bool
fn relative?(path: Path) -> Bool
```

Rules:

- `Path` is a nominal wrapper around a whole path string.
- Raw strings become paths explicitly through the public `Path(value)` constructor; filesystem APIs do not accept `Str` directly.
- Shim ABI conversion unwraps `Path(Str)` to the Go string ABI, so stdlib modules do not need repeated path-to-string plumbing.
- A path is not an IO resource. Do not implement `io.Read` or `io.Write` for `Path`.
- `extname` follows Ruby/Elixir-style empty-string behavior for paths with no extension.

### `std.file`

Filesystem operations. Introduced as a proof slice by `09_shim-first-go-interop.md`, then expanded here.

Target API:

```marmoset
import std.bytes
import std.bytes.Bytes
import std.bytes.DecodeError
import std.io
import std.path

export File, Mode, Error, UseError,
       read, write, append, open,
       read_all, read_line, read_chunk,
       write_all, flush

extern type File

type Mode = { Read, Write, Append }

type Error = {
  NotFound = "File not found",
  PermissionDenied = "You do not have permission to access this file",
  AlreadyExists = "File already exists",
  IsDirectory = "Expected a file but found a directory",
  NotDirectory = "Expected a directory but found a file",
  InvalidPath(Str) = "Path is invalid",
  InvalidData(DecodeError) = "File contains invalid data",
  AlreadyClosed = "File is already closed",
  Other(Str) = "File operation failed",
}

type UseError[e] = {
  Open(Error) = "Could not open file",
  Use(e) = "File callback failed",
  Close(Error) = "Could not close file",
  UseAndClose(e, Error) = "File callback failed and close also failed",
}

fn read[a: bytes.Decode](path: path.Path) => Result[a, Error]
fn write[a: bytes.Encode](path: path.Path, value: a) => Result[Unit, Error]
fn append[a: bytes.Encode](path: path.Path, value: a) => Result[Unit, Error]

fn open[a, e](path: path.Path, mode: Mode, body: (File) => Result[a, e]) => Result[a, UseError[e]]

fn read_all(file: File) => Result[Str, Error]
fn read_line(file: File) => Result[Option[Str], Error]
fn read_chunk(file: File, size: Int) => Result[Option[Bytes], Error]
fn write_all(file: File, value: Str) => Result[Unit, Error]
fn flush(file: File) => Result[Unit, Error]

impl io.Read[File, Error]
impl io.Write[File, Error]
```

Rules:

- Go errors map to domain enums in `runtime/go/shims/std/file`.
- `read`, `write`, and `append` are the simple whole-file APIs. The value type chooses the byte/text conversion through `bytes.Decode`/`bytes.Encode`.
- `File` is an exported opaque Marmoset resource type. Users can name it in annotations and pass it through callbacks, but cannot construct or inspect it.
- `open` is the normal handle-lifecycle API. It opens the file in an explicit `Mode`, calls the callback, then closes the handle after the callback returns.
- `read_all`, `read_line`, `read_chunk`, `write_all`, and `flush` are scoped-file operations that make `open` useful for streaming and incremental IO. `read_line` returns `Option.None` at EOF and strips line terminators; `read_chunk` returns raw bytes so callers do not accidentally split UTF-8 text.
- `read_all`, `write_all`, and `flush` are scoped-file convenience helpers over the `io.Read[File, Error]` / `io.Write[File, Error]` impls; the trait impls own handle behavior.
- Reading, writing, or flushing a closed, leaked, or unknown file value returns `Error.AlreadyClosed`.
- Raw resource operations stay in the private `file_shim` extern block as `open_handle` and `close_handle`; `std.file` does not export a raw close or one-argument open.
- `UseError[e]` flattens the open failure, callback failure, close failure, and combined callback-plus-close failure cases for failable callback APIs.
- `Path` values are explicit at the public boundary; `std.file` does not accept raw `Str`, does not own a private path receiver, and does not implement IO protocols for paths.

### `std.dir`

Directory operations. This module is effectful and filesystem-backed, but it is not file-content IO.

Target API:

```marmoset
import std.path

export Entry, Kind, Error,
       read,
       make, make_all,
       remove, remove_all,
       exists?,
       current

type Kind = { File, Directory, Symlink, Other }
type Entry = Entry(path.Path, Kind)

type Error = {
  NotFound = "Directory not found",
  PermissionDenied = "You do not have permission to access this directory",
  AlreadyExists = "Directory already exists",
  NotDirectory = "Expected a directory but found a file",
  NotEmpty = "Directory is not empty",
  InvalidPath(Str) = "Path is invalid",
  Other(Str) = "Directory operation failed",
}

fn read(path: path.Path) => Result[List[Entry], Error]
fn make(path: path.Path) => Result[Unit, Error]
fn make_all(path: path.Path) => Result[Unit, Error]
fn remove(path: path.Path) => Result[Unit, Error]
fn remove_all(path: path.Path) => Result[Unit, Error]
fn exists?(path: path.Path) => Result[Bool, Error]
fn current() => Result[path.Path, Error]
```

Rules:

- Directory entries carry a `path.Path` plus coarse kind.
- `read` returns immediate children, not a recursive walk.
- Keep `chdir` out of the first pass because it mutates process-global state.
- File-content operations stay in `std.file`; path manipulation stays in `std.path`.

### `std.io`

Terminal IO. All effectful. This should use checked-in shims rather than direct FFI. `std.io` owns the shared IO protocols and the default stdin/stdout surface; `std.io.err` owns stderr helpers.

Current API:

```marmoset
import std.prelude.Show

export Error, Read, Write, read, write, print, puts, flush

type Error = {
  EndOfFile = "Input ended",
  BrokenPipe = "Pipe is broken",
  Interrupted = "I/O operation was interrupted",
  Other(Str) = "I/O operation failed",
}

trait Read[r, e] = {
  fn read(reader: r) => Result[Str, e]
}

trait Write[w, e] = {
  fn write(writer: w, value: Str) => Result[Unit, e]
  fn flush(writer: w) => Result[Unit, e]
  fn print[a: Show](writer: w, value: a) => Result[Unit, e]
  fn puts[a: Show](writer: w, value: a) => Result[Unit, e]
}

extern type Stdin   # private singleton receiver
extern type Stdout  # private singleton receiver

impl Read[Stdin, Error]
impl Write[Stdout, Error]

fn read() => Result[Str, Error]
fn write(value: Str) => Result[Unit, Error]
fn print[a: Show](value: a) => Result[Unit, Error]
fn puts[a: Show](value: a) => Result[Unit, Error]
fn flush() => Result[Unit, Error]
```

Rules:

- `io.read` reads one stdin line without the trailing newline. End-of-input is `Error.EndOfFile`.
- `io.write` writes the string to stdout without adding a newline.
- `io.print` writes `Show.show(value)` to stdout without adding a newline.
- `io.puts` writes `Show.show(value)` to stdout plus a trailing newline.
- `io.flush` is explicit for writers that actually buffer or sync. The current stdout/stderr Go shims write directly to `os.Stdout`/`os.Stderr`, so prompt-style terminal code can call `io.print`, then `io.read` without a flush ceremony.
- `std.io` does not export stdin/stdout handle values or types. It keeps private `Stdin`/`Stdout` extern singleton receivers so default terminal helpers can dogfood `Read[Stdin, Error]` and `Write[Stdout, Error]`.
- `std.io` terminal shims accept the private receiver token at the Go boundary. The Go shim may ignore the token internally, but the Marmoset trait receiver still reaches the implementation boundary.
- `std.io.err` exports `Error`, `write`, `print`, `puts`, and `flush` for stderr. Its error enum omits `EndOfFile`; its private `Stderr` singleton implements `io.Write[Stderr, Error]`.
- File values implement `io.Read[File, FileReadError]` privately inside `std.file`; users call it through `io.Read.read(file)` inside `file.open` callbacks.

### `std.str`

String helpers. Mostly pure; Go-backed helpers use `runtime/go/shims/std/str`.

Candidate API:

```marmoset
str.length("hello")                 # -> Int
str.upcase("hello")                 # -> Str
str.downcase("hello")               # -> Str
str.strip("  hello  ")              # -> Str
str.lstrip("  hello  ")             # -> Str
str.rstrip("  hello  ")             # -> Str
str.split("hello world", " ")       # -> List[Str]
str.include?("hello", "ell")        # -> Bool
str.starts_with?("hello", "he")     # -> Bool
str.ends_with?("hello", "lo")       # -> Bool
str.empty?("hello")                 # -> Bool
str.replace("hello", "l", "r")      # -> Str
str.slice("hello", 1, 3)            # -> Result[Str, StrSliceError]
str.chars("hello")                  # -> List[Str]
str.join(["a", "b", "c"], ", ")     # -> Str
```

Unicode semantics must be explicit per function. `length` should say whether it counts bytes, runes, or grapheme clusters before implementation.

### `std.list`

Pure generic list helpers. These may be implemented in Marmoset where possible and in backend/runtime helpers only when necessary.

Candidate API:

```marmoset
list.map(xs, f)
list.select(xs, predicate)
list.reject(xs, predicate)
list.each(xs, f)          # effectful
list.reduce(xs, init, f)
list.flat_map(xs, f)
list.find(xs, predicate)  # Option[a]
list.any?(xs, predicate)
list.all?(xs, predicate)
list.none?(xs, predicate)
list.empty?(xs)
list.count(xs)
list.reverse(xs)
list.sort(xs)
list.take(xs, n)
list.drop(xs, n)
list.include?(xs, value)
list.uniq(xs)
```

The exact signatures should use pipe-friendly data-first order and canonical `Option`/`Result` where needed.

### `std.map`

Pure immutable map helpers. This may require language/runtime support for map representation beyond the shim-first IO path.

Candidate API:

```marmoset
map.keys(m)
map.values(m)
map.get(m, key)              # Option[v]
map.put(m, key, value)       # Map[k, v]
map.delete(m, key)           # Map[k, v]
map.has_key?(m, key)
map.empty?(m)
map.count(m)
map.merge(m, other)
map.map(m, f)
map.select(m, predicate)
map.each(m, f)               # effectful
```

Mutation must not leak. `put`, `delete`, and `merge` return new values.

## Implementation Phases

### Phase L0: Sync With Shim Proof Outputs

**Goal:** Treat `09_shim-first-go-interop.md` as the source of truth for `std.bytes`, `std.file`, `extern type`, `Bytes`, and Go shim layout.

Changes:

- Import the landed `std.bytes` and `std.file` APIs into this plan's fixtures.
- Lock exact `File*Error` variants after the shim proof.
- Update docs/features for `bytes` and `file` once those modules are canonical.

Gate:

```bash
git diff --check
make integration stdlib-shims
```

### Phase L1: Harden Bytes And File

**Goal:** Move from proof slice to reliable stdlib surface.

Changes:

- Add missing docs for `std.bytes` and `std.file`.
- Add path, permission, directory, stale handle, repeated close, and binary-content fixtures.
- Confirm `std.file` imports `std.bytes.Bytes` explicitly rather than relying on prelude injection.
- Keep Go implementation details confined to `runtime/go/shims/std/file` and `runtime/go/shims/std/bytes`.

Gate:

```bash
make integration stdlib-shims
```

### Phase L2: IO And String Modules

**Goal:** Add common terminal and string APIs using shims only where Go runtime support is needed.

Files:

- `std/io.mr`
- `std/str.mr`
- `runtime/go/shims/std/io/**`
- `runtime/go/shims/std/str/**`
- `test/integration/14_stdlib_shims.sh` or split stdlib suites

Tests:

- Terminal write/print/puts/flush smoke tests.
- Stdin fixture or deterministic test harness path for `read_line`.
- String edge cases: empty strings, Unicode, missing delimiters, prefix/suffix, out-of-range slicing.

Gate:

```bash
make integration stdlib-shims
```

### Phase L3: List Module

**Goal:** Add `std.list` with stable pure/effectful higher-order APIs.

Changes:

- Implement simple helpers in Marmoset where practical.
- Use compiler/runtime support only for operations that cannot be expressed cleanly yet.
- Preserve callback effect sequencing for `each`, `reduce`, predicates, and mapping.
- Record canonical identities for `09_hof_optimization.md`.

Tests:

- Empty, singleton, and multi-element lists.
- Type-polymorphic use.
- Effect order for `each`.
- `find` returns canonical `Option`.

Gate:

```bash
make integration stdlib
```

### Phase L4: Map Module

**Goal:** Add `std.map` with immutable semantics.

Changes:

- Decide whether current map representation is sufficient for public stdlib APIs.
- Ensure copy-on-write semantics for mutating operations.
- Use canonical `Option` for lookup.

Tests:

- Empty map, missing keys, merge conflicts.
- Original map unchanged after `put`, `delete`, and `merge`.
- Generic key/value smoke coverage.

Gate:

```bash
make integration stdlib
```

### Phase L5: Regroup For Application-Oriented Modules

**Goal:** Decide the next stdlib slice based on real app pressure.

Candidates:

- `std/env`
- `std/time`
- `std/json`
- `std/http`
- `std/regex`
- `std/path`

HTTP, SQL, and framework-style wrappers likely need follow-up shim features such as async/stored callbacks, streams, tasks, records, nullable columns, and user-project shim packaging.

## Testing Strategy

- Go-backed stdlib modules use focused integration fixtures that exercise the public Marmoset module, not direct shim functions.
- Shim details are covered by `09_shim-first-go-interop.md`; this plan tests the stdlib API contract.
- Pure helper modules get typechecker and integration coverage for polymorphism, effect sequencing, and edge cases.
- File/bytes tests use temp dirs and binary-safe fixtures.
- Tree snapshots are useful only when stdlib changes affect generated shim/API packages.

## Commit Plan

1. Refactor `std.bytes` with encode/decode traits and strict UTF-8 conversion.
2. Add `std.path` plus path shims and tests.
3. Refactor `std.file` around unified `read`/`write`/`append`, exported opaque `File`, `Mode`, unified `Error`, and `io.Read`/`io.Write` impls for file handles only.
4. Add `std.dir` plus directory shims and tests.
5. Verify LSP, tree-sitter, Zed, VS Code, and JetBrains grammar support for extern blocks, extern types, predicate identifiers, singleton extern values, and multi-parameter trait syntax.
6. Run three bugbash rounds and commit fixes found by the hunt.
7. Regroup and create follow-up plans for app-oriented modules.

## Risks

1. **API sprawl.** Keep the first stdlib small and coherent.
2. **Unicode ambiguity.** String APIs must state byte/rune/grapheme behavior.
3. **Immutable performance.** Bytes and maps require copies; optimize later only with evidence.
4. **Effect leakage.** IO and effectful callbacks must stay explicitly marked.
5. **Shim dependency creep.** Do not add language or ABI features here; push missing interop capabilities into dedicated follow-up plans.

## Related Plans

- `docs/plans/done/language/09_shim-first-go-interop.md` provides the shim interop substrate and the initial `std.bytes`/`std.file` proof.
- `docs/plans/todo/language/09_hof_optimization.md` depends on stable stdlib higher-order function identities.

## Progress

- 2026-05-08 23:52 CEST: Renumbered from `04_stdlib.md` to `05_stdlib.md` and rewritten around shim-first interop. Direct FFI wrapper assumptions were removed; `std.bytes` and `std.file` now depend on `09_shim-first-go-interop.md`.
- 2026-05-09 05:06 CEST: Started the stdlib-basics dogfooding slice from the shim-first interop substrate. Added `std.basics.puts` plus checked-in `runtime/go/shims/std/basics`, removed the backend `puts` runtime special case, moved remaining checked-in helper runtime code under `runtime/go/marmoset`, and made stale direct-print fixtures supply explicit `Show`/rendering. Verification passed with `dune runtest lib/frontend lib/backend/go`, focused former-failure fixtures plus hardening, `git diff --check`, and `make integration all`.
- 2026-05-09 05:07 CEST: Stdlib-basics dogfooding slice committed.
- 2026-05-09 16:05 CEST: Follow-up cleanup moved the public implicit `puts` definition out of `std.prelude` and into `std.basics`; `std.basics` is now a core stdlib module loaded for implicit bindings, with only a private monomorphic `puts_str` adapter at the shim boundary. Verification passed with `dune runtest lib/frontend lib/backend/go`, `make integration runtime/g41a_puts_int.mr prelude hardening snapshots`, `make integration ffi`, `make integration all`, and `git diff --check`.
- 2026-05-09 16:24 CEST: Fixed generic direct shim calls so nested trait arguments prefer the concrete specialized environment over stale unresolved type-map entries. Collapsed `std.basics.puts` to call `basics_shim.puts_str(Show.show(value))` directly and added a regression test for the former `show_show_union_empty` emission.
- 2026-05-09 16:31 CEST: Started the exported-shim-function cleanup slice. Goal: keep shim qualifiers private while letting explicitly exported shim functions serve as the public module API, so identity Marmoset wrappers in `std.bytes` and `std.file` can be deleted.
- 2026-05-09 16:43 CEST: Exported shim functions now serve directly as std module APIs while shim qualifiers remain private and direct-call-only. Removed identity wrappers from `std.bytes`/`std.file`, refreshed the shim-runtime tree snapshot, and verified with `dune runtest lib/frontend`, `dune runtest lib/backend/go`, `make integration ffi stdlib-shims snapshots`, `make integration all`, and `git diff --check`.
- 2026-05-09 16:50 CEST: Started the exported-shim-function-value cleanup slice. Goal: remove the artificial direct-call-only split for exported shim APIs by letting references such as `bytes.from_str` and `file.write` behave as ordinary first-class function values backed by generated shim adapters.
- 2026-05-09 16:56 CEST: Exported shim APIs now work as first-class function values backed by generated adapters. Added explicit function-reference artifacts, preserved effect checking for effectful shim values, fixed shim API package generation to include all declarations for any copied shim package, and verified with `dune runtest lib/frontend`, `dune runtest lib/backend/go`, `make integration ffi`, `make integration stdlib-shims`, `make integration snapshots`, `make integration all`, and `git diff --check`.
- 2026-05-09 23:29 CEST: Fixed the repeated Zed/LSP `std/file` shim-owner diagnostic at the local-dev boundary. The stale error reproduced with the opam-installed `marmoset 85a9d27`; the repo binary accepted `std/file.mr`. Added LSP coverage for direct non-core stdlib shim entries and hardened the then-current Zed launcher. This was superseded on 2026-05-12 by the `MARMOSET_ROOT`-only launcher rule. Verification passed with `dune runtest tools/lsp/lib`, `cargo test --locked`, direct repo/opam `std/file.mr` checks, and `git diff --check`.
- 2026-05-10 00:06 CEST: Started the `std.file` public API redesign slice. Goal: make file IO the stdlib poster-child for high-level Marmoset APIs backed by private Go shim plumbing, with scoped resource helpers as the normal surface and raw open/close kept out of the exported API.
- 2026-05-10 00:08 CEST: Added red stdlib-shim tests for the new managed file API. Current failures are expected: `FileReadError.AlreadyClosed` and `FileScopeError` are missing, and the raw handle lifecycle is still exposed as the public file API.
- 2026-05-10 00:11 CEST: Tried a richer generic use-error helper for callback-returning file APIs, but current exported generic `Result`/error composition is not ready for that stdlib surface. Kept this slice to the simpler managed API (`open` plus `read_all`) and left richer error composition for a dedicated follow-up.
- 2026-05-10 00:14 CEST: Implemented the managed `std.file` surface. Public file handles now flow through callback-based `file.open`, raw handle open/close remain private shim plumbing, `read_all` reports `FileReadError.AlreadyClosed` for stale handles, and verification passed with `make integration stdlib-shims`, `make integration ffi`, `dune runtest lib/frontend lib/backend/go`, and `git diff --check`.
- 2026-05-10 00:18 CEST: Tightened the public/private naming boundary: the user-facing scoped helper is `file.open(path, callback)`, while private Go shim plumbing is named `open_handle`/`close_handle`. Re-ran `make integration stdlib-shims`, `make integration ffi`, `dune runtest lib/frontend lib/backend/go`, and `git diff --check`.
- 2026-05-10 00:24 CEST: Started the generic callback-result compiler hardening slice before building more stdlib API on top of it. Red target: a generic helper that accepts `(Int) -> Result[a, e]` and returns `Result[a, UseError[e]]` must compile and run instead of reaching Go codegen with an unresolved type variable.
- 2026-05-10 00:29 CEST: Generic callback-result hardening reached green. Go enum collection now ignores codegen-unresolved enum instantiations instead of erasing them into `union_empty` artifacts, and the new callback wrapper fixture plus backend unit assertion verify the concrete `Result[a, UseError[e]]` specialization. Verification passed with `dune runtest lib/backend/go`, `make integration codegen_mono/cm55_generic_result_callback_error_wrapper.mr`, `make integration codegen_mono`, and `git diff --check`.
- 2026-05-10 00:30 CEST: Started the high-level `std.file` API slice on top of the callback-result compiler fix. Target: hide the named handle type from importers, add string convenience helpers, and make `file.open` flatten callback failures into `FileUseError[e]` instead of returning nested `Result` values.
- 2026-05-10 00:37 CEST: High-level `std.file` slice reached green. `read`/`write` are now the common text APIs, `read_bytes`/`write_bytes` are explicit binary APIs, `File` remains private, and `file.open` flattens callback failures through `FileUseError[e]`. Verification passed with `make integration stdlib-shims`, `dune runtest lib/frontend`, `dune runtest lib/backend/go`, `make integration ffi`, and `git diff --check`.
- 2026-05-10 00:39 CEST: Started the `std.io` terminal IO slice. Red coverage first: `std.io` must provide `print`, `println`, and `read_line` over stdin with explicit `ReadLineError.EndOfFile`.
- 2026-05-10 00:39 CEST: Implemented `std.io` with generic `Show`-based `print`/`println`, direct shim-backed `read_line`, checked-in `runtime/go/shims/std/io`, and install metadata. Focused stdlib-shim tests now pass.
- 2026-05-10 01:41 CEST: Started callback-interop dogfooding for stdlib shims. Added red coverage for `std.io.map_line` and `std.io.with_line`, where checked-in Go shims must call Marmoset closures synchronously through the generated adapter.
- 2026-05-10 01:45 CEST: Callback-interop dogfooding reached focused green state. `std.io.map_line` covers pure callback return conversion and `std.io.with_line` covers effectful `Unit` callbacks through production shims.
- 2026-05-11 23:31 CEST: Started the trait-shaped IO redesign slice. Target: make `std.io` expose `Read[r, e]`/`Write[w, e]` protocols plus default stdin/stdout helpers, move stderr helpers to `std.io.err`, and keep concrete filesystem APIs under `std.file` while implementing the IO protocols.
- 2026-05-11 23:34 CEST: Added red stdlib-shim coverage for trait-shaped IO. Expected failures observed: `std.io.Read`, `std.io.Error`, and `std.io.err` are missing, while the old `println`/`read_line`/callback helpers are still exported.
- 2026-05-11 23:51 CEST: Trait-shaped IO reached focused green. The compiler now accepts multi-parameter traits/impl heads such as `Read[r, e]` and `impl io.Read[File, FileReadError]`, `std.io` exports `read`/`write`/`flush`/`print` plus IO protocols, stderr lives in `std.io.err`, and `std.file` implements the read protocol for scoped file values. Verification passed with `dune runtest lib/frontend/syntax lib/frontend/typecheck lib/backend/go` and `make integration stdlib-shims`.
- 2026-05-11 23:54 CEST: Final trait-shaped IO quality gate passed with `dune runtest lib/frontend/syntax lib/frontend/typecheck lib/backend/go`, `make integration ffi stdlib-shims`, and `git diff --check`; preparing the slice commit.
- 2026-05-11 23:57 CEST: Started a shim naming cleanup for IO: rename private `write_str` shim entries to `write` so the extern blocks mirror the public helper names.
- 2026-05-11 23:57 CEST: IO shim naming cleanup reached green. Verification passed with `make integration stdlib-shims`, `make integration ffi stdlib-shims`, and `git diff --check`; preparing the cleanup commit.
- 2026-05-12 00:05 CEST: Extended the cleanup so stdio/stderr now expose real `Read`/`Write` receiver values, `std.file` renames its private resource from `Handle` to `File`, and file path helpers route through private `Read[Path, FileReadError]`/`Write[Path, FileWriteError]` impls. Focused `make integration stdlib-shims` is green.
- 2026-05-12 01:35 CEST: Refined the private `std.file.Path` helper from a one-variant sum to the existing nominal wrapper syntax `type Path = Path(Str)`. This exposed and fixed missing wrapper-constructor pattern support in typechecking, exhaustiveness, and Go emission. A trial `Result.map` cleanup for file read conversions was reverted because generic stdlib method specialization still leaks unresolved type variables in codegen; keep that for a dedicated compiler-hardening slice.
- 2026-05-12 01:41 CEST: Started and greened the stdio handle/protocol cleanup. `Stdin`, `Stdout`, and `Stderr` are now opaque extern token types returned by production shims, fake singleton enum constructors are gone, and public `print` helpers delegate through `Write.print` instead of duplicating newline/flush matches. Focused `make integration stdlib-shims` is green; preparing the cleanup commit.
- 2026-05-12 01:59 CEST: Started and greened the phantom receiver/path cleanup. Stdio and stderr shim methods now take the opaque receiver token instead of dropping it in `std.io`, whole-path `std.file.read/write` call private byte shims directly instead of manufacturing a `Path` protocol receiver, and import resolution now lets a real Marmoset wrapper shadow a same-named private extern entry in exported module metadata. Focused `make integration stdlib-shims` is green.
- 2026-05-12 02:35 CEST: Started the stdio token removal cleanup. Red coverage now requires `std.io`/`std.io.err` to keep default terminal IO direct, with no exported `Stdin`/`Stdout`/`Stderr` types and no `stdin`/`stdout`/`stderr` token helpers.
- 2026-05-12 02:35 CEST: Stdio token removal reached focused green. `std.io` now exports only `Error`, `Read`, `Write`, and terminal helpers; `std.io.err` exports only stderr helpers; copied Go shims no longer include Marmoset stdio token APIs. Focused `make integration stdlib-shims` is green.
- 2026-05-12 15:56 CEST: Reworked stdio around private singleton extern receivers instead of public token helpers or direct helper shims. `std.io` now implements `Read[Stdin, Error]`/`Write[Stdout, Error]`, `std.io.err` implements `io.Write[Stderr, Error]`, and public helpers route through those trait impls. The compiler now supports module-local singleton values for private extern types and concrete method-generic default trait specialization from generic wrapper bodies, including interpolation-backed `Write.print`. Verification passed with `dune runtest lib/frontend/typecheck`, `dune runtest lib/backend/go`, `make integration stdlib-shims`, `make integration ffi stdlib-shims`, broader `MARMOSET_ROOT=<temp with HEAD std/option.mr> dune runtest lib/frontend lib/backend/go`, and `git diff --check`.
- 2026-05-12 16:07 CEST: Refactored `std.file` whole-path helpers to use the same private receiver/protocol style as stdio. A private `Path` wrapper now implements `io.Read[Path, FileReadError]` and `io.Write[Path, FileWriteError]`; public `file.read`/`file.write` and byte helpers route through that wrapper while raw byte/open/close operations stay in private shims. Focused verification passed with `make integration stdlib-shims` and `make integration ffi stdlib-shims`.
- 2026-05-12 16:42 CEST: Started the File/Path/Dir poster-child redesign. The plan now targets `bytes.Decode`/`bytes.Encode` instead of public `read_bytes`/`write_bytes`, introduces pure `std.path`, moves `std.file` to unified `Error` plus explicit `Mode`, makes `File` the only filesystem IO resource, and adds a `std.dir` first slice plus editor/LSP and three-round bugbash checkpoints.
- 2026-05-12 16:46 CEST: Started the test-first slice. Red coverage targets `List[T]` shim boundaries for directory listings, `bytes.Decode`/`bytes.Encode`, pure `std.path`, unified trait-based `std.file`, and `std.dir` scalar/listing operations.
- 2026-05-12 16:50 CEST: Red observed with `make integration stdlib-shims`. Expected failures cover missing `bytes.DecodeError`, missing `std.path`/`std.dir`, old `std.file.read_bytes`/`write_bytes` still exported, missing unified `std.file.Error`, and the shape test still using the old file surface.
- 2026-05-12 18:38 CEST: File/Path/Dir implementation reached focused green. `std.bytes` now exposes `Decode`/`Encode`, `std.path` owns pure path values, `std.file` exposes unified trait-based read/write/append/open APIs over exported opaque `File`, `std.dir` covers scalar directory operations and `List[Entry]` shims, and codegen now specializes callback parameter types from declared generic function signatures plus expected return types. Verification passed with `dune build ./_build/default/bin/main.exe` and `make integration stdlib-shims`.
- 2026-05-12 18:41 CEST: Refreshed stale FFI fixtures after removing byte-specific file helpers. The first-class shim-value fixture now uses `std.bytes` APIs directly, and the shim-runtime Go-tree snapshot includes `std.bytes.DecodeError` plus strict decode adapters. Verification passed with `dune runtest lib/frontend`, `dune runtest lib/backend/go`, `git diff --check`, and `make integration ffi stdlib-shims`.
- 2026-05-12 18:47 CEST: Editor/LSP syntax coverage reached green. Added explicit tree-sitter corpus coverage for multi-parameter traits and LSP analysis coverage for multi-parameter traits, predicate identifiers, nominal wrapper constructors, and std-owned extern shim blocks. Verification passed with `dune runtest tools/lsp/lib`, `npm test` in `tools/tree-sitter-marmoset`, `npm run check-grammar` and `npm run compile` in `tools/vscode-marmoset`, `tools/scripts/sync_textmate_grammars.sh --check`, and `cargo test --locked` in `tools/zed-marmoset`.
- 2026-05-12 18:48 CEST: Started three-round bugbash. Round 1 targets stdlib behavior edge cases, Round 2 targets compiler/shim interop specialization edges, and Round 3 targets editor/LSP/tooling syntax coverage.
- 2026-05-12 19:06 CEST: Bugbash Round 1 found a std.dir error-precedence bug: removing a non-empty directory returned `AlreadyExists` on this platform because the shim checked Go's broad existence predicate before `ENOTEMPTY`. Reordered specific filesystem errors before broad existence checks in `std.dir` and `std.file`, and added a regression to the stdlib-shim fixture. Focused `make integration stdlib-shims` is green.
- 2026-05-12 19:09 CEST: Bugbash Round 2 covered generic callback-result specialization through a helper around `file.open`, `io.Read.read(handle)`, explicit `std.path.Path` values, and `List[Entry]` directory shim returns. Folded the generic file-open helper into the permanent stdlib fixture; focused `make integration stdlib-shims` is green.
- 2026-05-12 19:12 CEST: Bugbash Round 3 re-ran editor and LSP coverage for the new syntax surface. Verification passed with `dune runtest tools/lsp/lib`, `npm test` in `tools/tree-sitter-marmoset`, `npm run check-grammar` and `npm run compile` in `tools/vscode-marmoset`, `tools/scripts/sync_textmate_grammars.sh --check`, and `cargo test --locked` in `tools/zed-marmoset`.
- 2026-05-12 19:20 CEST: Broad integration probing exposed a branch-local project-root regression from the stdlib-root split: files under `MARMOSET_ROOT` but outside `MARMOSET_ROOT/std` were being treated as stdlib modules, which broke local module imports from repo-hosted fixtures. Narrowed stdlib module detection to the `std/` subtree, added discovery coverage for projects that live under the toolchain root, refreshed the FFI Go-tree snapshot back to project-local names, and verified module fixture groups plus `make integration ffi stdlib-shims`.
- 2026-05-12 20:25 CEST: Fixed the std.dir nominal-wrapper regression exposed by replacing one-variant directory-entry carrier sums with proper multi-payload wrappers. The parser, type registry, typechecker, Go emitter, shim boundary adapter, generated shim API, and LSP surface traversal now support `type Entry = Entry(path.Path, Kind)` without enum tags. Verification passed with `dune runtest lib/frontend/syntax`, `dune runtest lib/frontend/typecheck`, `dune runtest lib/backend/go`, `dune runtest tools/lsp`, `make integration ffi stdlib-shims`, `test/ci/tree-sitter.sh`, `test/ci/editor-vscode.sh`, `test/ci/editor-nvim.sh`, `test/ci/editor-jetbrains.sh`, and `git diff --check`; `test/ci/editor-zed.sh` was blocked only by the local uncommitted `extension.toml` grammar-source override.
- 2026-05-12 20:43 CEST: Extended shim ABI conversion so named wrappers can cross shim boundaries by shape instead of requiring owner-local raw carrier types. Single-payload wrappers flatten to their payload ABI, multi-payload wrappers keep generated shim API structs with recursively converted fields, and `std.dir` now has its Go shim return `Entry` directly instead of private carrier values. Verification passed with `dune runtest lib/frontend/typecheck`, `dune runtest lib/backend/go`, `make integration ffi stdlib-shims`, and `git diff --check`.
- 2026-05-12 22:35 CEST: Simplified the path-facing stdlib API around explicit `Path(...)` construction. Removed the old structural path protocol, redundant path constructor/stringifier helpers, and duplicated filesystem path-to-string adapters; `std.file` and `std.dir` now take `path.Path` directly while shim ABI flattening keeps Go shims on string parameters. Focused `make integration stdlib-shims` is green.
- 2026-05-12 22:45 CEST: Reproduced the Zed stale-binary failure path: PATH-installed `marmoset` still emitted `unsupported shim boundary type std__path__Path`, while the repo `./marmoset` accepted `std/path.mr`.
- 2026-05-12 22:52 CEST: Corrected the Zed launcher contract to exactly one canonical binary path. `MARMOSET_ROOT` is now required, Zed runs exactly `$MARMOSET_ROOT/marmoset lsp`, and there is no PWD, ancestor, nested-checkout, or PATH fallback. Verification passed with `cargo test --locked` and `cargo fmt --check` in `tools/zed-marmoset`.
- 2026-05-12 23:08 CEST: Extended the same canonical launcher contract across VS Code, Neovim, and JetBrains, and removed Zed's preflight `--version` launchability check so Zed returns `$MARMOSET_ROOT/marmoset lsp` directly. Editor CI now guards against PATH, workspace, stdlib-probing, configurable-command, nested-build, and preflight fallbacks. Verification passed with `test/ci/editor-vscode.sh`, `test/ci/editor-nvim.sh`, `test/ci/editor-jetbrains.sh`, and `cargo fmt --check && cargo test --locked` in `tools/zed-marmoset`; full `test/ci/editor-zed.sh` is blocked in this working tree only by the pre-existing uncommitted local `extension.toml` grammar-source override.
- 2026-05-12 23:32 CEST: Fixed the strict-return/helper-generic regression exposed by `std.file.read_all`. Trait impl return annotations can now specialize return-only helper generics without letting impl-owned generic variables collapse, placeholder rewrites are only recorded after successful inference, and Go emission carries concrete method-generic expected types into qualified inherent callback arguments. Verification passed with `dune runtest lib/frontend/typecheck`, `dune runtest lib/backend/go`, `MARMOSET_ROOT=$PWD dune exec -- bin/main.exe check std/file.mr`, `make integration stdlib-shims`, and `git diff --check`.
- 2026-05-13 00:00 CEST: Started the `std.file` cleanup slice. Target: keep `File` as the public scoped callback value, keep raw handle/path operations private, rename shim byte primitives by path-vs-handle role, make `io.Read[File, Error]` / `io.Write[File, Error]` own handle behavior, and leave module helpers as convenience wrappers.
- 2026-05-13 00:41 CEST: `std.file` cleanup reached focused green. Private file shims now use `read_path`/`write_path`/`append_path` and `read_handle`/`write_handle`; scoped handle helpers delegate through `io.Read`/`io.Write`; decode error adaptation uses the Result failure-wrapping helper; and the Go emitter now return-mangles hoisted placeholder-section callbacks when return type disambiguation is required. Verification passed with `dune runtest lib/backend/go`, `dune runtest lib/frontend`, `dune runtest tools/lsp/lib`, `MARMOSET_ROOT=$PWD dune exec -- bin/main.exe check std/file.mr`, `make integration stdlib-shims`, `make integration ffi`, and `git diff --check`.
- 2026-05-13 01:02 CEST: Started the scoped file-handle operations slice. Target: make `file.open` earn its public API space with line reads, bounded byte chunks, and handle write/flush helpers covered by stdlib-shim integration tests.
- 2026-05-13 01:12 CEST: Scoped file-handle operations reached focused green. `std.file` now exposes `read_line` and `read_chunk`; the Go file shim keeps a buffered reader per scoped handle so line/chunk/all reads share position; EOF is `Option.None`; and line reads strip `\n`/`\r\n` while chunk reads stay byte-oriented. Verification passed with `make integration stdlib-shims`, `MARMOSET_ROOT=$PWD dune exec -- bin/main.exe check std/file.mr`, `dune runtest lib/backend/go`, and `make integration ffi`.
- 2026-05-13 19:09 CEST: Updated the target stdlib error surface for the production error model: `bytes.DecodeError.InvalidUtf8` has a canonical message, and `std.file.Error.InvalidData` now preserves `DecodeError` as a typed cause instead of storing a lossy string.
