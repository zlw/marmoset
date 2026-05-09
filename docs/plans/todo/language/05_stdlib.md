# Stdlib Plan (Shim-First)

## Maintenance

- Last verified: 2026-05-09
- Implementation status: In progress (`std.basics`, `std.bytes`, and `std.file` proof slices landed; broader stdlib expansion pending)
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
4. Establish canonical `Bytes` and `File` surfaces from the shim-first proof and harden them into the broader stdlib.
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
3. **All IO is effectful.** Filesystem, console, environment, time, and similar operations use `=>`.
4. **Failable IO returns domain errors.** Prefer `Result[T, FileReadError]` over `Result[T, Str]` when the module can define meaningful error categories.
5. **Ruby-inspired naming.** `select`/`reject`, `upcase`/`downcase`, `include?`, `strip`, and related names remain preferred.
6. **`?` suffix for predicates.** This is already relevant to stdlib naming and must be supported by parser/emitter before predicate-heavy modules land.
7. **Separate modules for distinct domains.** `console` is terminal IO; `file` is filesystem IO; `bytes` is immutable byte data; `str` is Unicode/string helpers.
8. **Concurrency model TBD.** No raw Go channels. Future concurrency should use Marmoset-owned abstractions such as `Task`, `Stream`, or `Mailbox`.

## Current State

- `std/prelude.mr`, `std/option.mr`, and `std/result.mr` are toolchain stdlib modules loaded through the normal module pipeline.
- `Option` and `Result` are canonical stdlib nominal types with inherent helper APIs.
- `09_shim-first-go-interop.md` has introduced `extern type`, checked-in Go shims, canonical immutable `std.bytes.Bytes`, and the initial `std.file` proof slice.
- `std.basics` now owns the implicit terminal output function `puts`; the compiler no longer lowers `puts` as an emitter special case.
- `docs/features/ffi.md` now documents shim-first interop as the current direction, with direct package externs retained only as historical context.
- Existing list/map/str examples are mostly fixtures and exploratory docs; there is no committed full stdlib module set yet.

## Core Modules

### `std.basics`

Minimal implementation substrate for prelude functions that need shim-backed runtime behavior before broader stdlib modules land.

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
- This module should stay small. Domain-specific IO belongs in `std.console` or other explicit modules.

### `std.bytes`

Canonical immutable byte data. Introduced by `09_shim-first-go-interop.md` and hardened here.

Minimum API:

```marmoset
export Bytes, from_str, to_str_lossy, length, equal?

extern type Bytes

fn from_str(s: Str) -> Bytes
fn to_str_lossy(bytes: Bytes) -> Str
fn length(bytes: Bytes) -> Int
fn equal?(a: Bytes, b: Bytes) -> Bool
```

Rules:

- `Bytes` is immutable.
- Go shims copy on ingress and egress.
- No mutable buffer API in this milestone.

### `std.file`

Filesystem operations. Introduced as a proof slice by `09_shim-first-go-interop.md`, then expanded here.

Initial API:

```marmoset
import std.bytes.Bytes

export File, read, write, open, close

extern type File

type FileReadError = { NotFound, PermissionDenied, IsDirectory, Other(Str) }
type FileWriteError = { NotFound, PermissionDenied, IsDirectory, Other(Str) }
type FileOpenError = { NotFound, PermissionDenied, IsDirectory, Other(Str) }
type FileCloseError = { AlreadyClosed, Other(Str) }

fn read(path: Str) => Result[Bytes, FileReadError]
fn write(path: Str, bytes: Bytes) => Result[Unit, FileWriteError]
fn open(path: Str) => Result[File, FileOpenError]
fn close(file: File) => Result[Unit, FileCloseError]
```

Rules:

- Go errors map to domain enums in `runtime/go/shims/std/file`.
- `File` is an opaque Marmoset handle.
- `FileReadError`, `FileWriteError`, and `FileOpenError` currently use `NotFound`, `PermissionDenied`, `IsDirectory`, and `Other(Str)`.
- `FileCloseError` currently uses `AlreadyClosed` and `Other(Str)`.
- Closing an already-closed or unknown handle returns `FileCloseError.AlreadyClosed`.

### `std.console`

Terminal IO. All effectful. This should use checked-in shims rather than direct FFI.

Candidate API:

```marmoset
export stdout, stdin, errout

fn stdout(value: Str) => Unit
fn stdin() => Result[Str, ConsoleReadError]
fn errout(value: Str) => Unit
```

`stdin` is failable because scanner/readline behavior can fail. The exact error enum should be locked before implementation.

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

### Phase L2: Console And String Modules

**Goal:** Add common terminal and string APIs using shims only where Go runtime support is needed.

Files:

- `std/console.mr`
- `std/str.mr`
- `runtime/go/shims/std/console/**`
- `runtime/go/shims/std/str/**`
- `test/integration/14_stdlib_shims.sh` or split stdlib suites

Tests:

- Console stdout/stderr smoke tests.
- Stdin fixture or deterministic test harness path.
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

HTTP, SQL, and framework-style wrappers likely need follow-up shim features such as callbacks, streams, tasks, records, nullable columns, and user-project shim packaging.

## Testing Strategy

- Go-backed stdlib modules use focused integration fixtures that exercise the public Marmoset module, not direct shim functions.
- Shim details are covered by `09_shim-first-go-interop.md`; this plan tests the stdlib API contract.
- Pure helper modules get typechecker and integration coverage for polymorphism, effect sequencing, and edge cases.
- File/bytes tests use temp dirs and binary-safe fixtures.
- Tree snapshots are useful only when stdlib changes affect generated shim/API packages.

## Commit Plan

1. Sync docs and tests with landed `std.bytes`/`std.file`.
2. Harden `std.bytes` and `std.file`.
3. Add `std.console`.
4. Add `std.str`.
5. Add `std.list`.
6. Add `std.map`.
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
- 2026-05-09 23:29 CEST: Fixed the repeated Zed/LSP `std/file` shim-owner diagnostic at the local-dev boundary. The stale error reproduced with the opam-installed `marmoset 85a9d27`; the repo binary accepted `std/file.mr`. Added LSP coverage for direct non-core stdlib shim entries and hardened the Zed launcher to prefer the repo binary from `PWD` before falling back to PATH. Verification passed with `dune runtest tools/lsp/lib`, `cargo test --locked`, direct repo/opam `std/file.mr` checks, and `git diff --check`.
