# Host-Aligned Stdlib Runtime Model

## Maintenance

- Last verified: 2026-05-15
- Implementation status: Planning
- Area: Language / stdlib / Go backend
- Related plans:
  - `docs/plans/todo/language/05_stdlib.md`
  - `docs/plans/todo/language/11_io_output_names.md`
  - `docs/plans/todo/tooling/02_stdlib_fixture_corpus.md`

## Context

Marmoset compiles to Go. The public stdlib can still feel like Marmoset:
pipe-friendly, `Result`-based, trait-based, and not a direct copy of Go package
names. That does not mean the runtime model should drift away from Go.

This plan follows the Gleam/OCaml/Elixir family rather than the Roc/Elm/Haskell
family:

- Marmoset is **impure FP**.
- `->` functions are pure and should stay strongly protected.
- `=>` functions perform effects directly.
- fallibility remains explicit through `Result`.
- Go shims and platform modules are trusted imperative implementation
  boundaries.

Marmoset should keep FP semantics where they matter, and use Go-backed
practicality where backend work needs it.

The implementation boundary should feel like a Go programmer wrote the shim:
use standard Go resources, buffering primitives, error behavior, and package
contracts. Marmoset names can be pleasant, but the values crossing stdlib and
FFI boundaries should map naturally onto Go stdlib and generated-Go ecosystems
such as `net/http`, gRPC, and sqlc.

The `std.io` output naming slice exposed the distinction:

- public Marmoset names can be `io.print`, `io.puts`, `file.read`, etc.
- stdout/stderr shims should still use normal Go primitives such as
  `fmt.Print`, `fmt.Println`, `os.Stdout.WriteString`, and
  `os.Stderr.WriteString`;
- buffered IO should use Go buffering primitives under the hood instead of
  pretending every operation is an abstract string helper;
- file `flush` must not accidentally mean Go `os.File.Sync` unless the API is
  explicitly about durability.

## Goals

1. Define the stdlib boundary rule: ergonomic Marmoset surface, Go-aligned
   runtime representation.
2. Audit existing stdlib shims for places where the Go primitive does not match
   the Marmoset API contract.
3. Design buffered IO around efficient Go resources while keeping Marmoset code
   pleasant.
4. Preserve natural interop with Go-generated or Go-native libraries:
   `net/http`, gRPC, sqlc, database drivers, filesystem APIs, and networking.
5. Keep the public API explicit about allocation, buffering, flushing, syncing,
   ownership, and closing.
6. Preserve functional semantics for ordinary Marmoset values while allowing
   scoped imperative resources where Go-level performance needs mutation.
7. Pre-decide the target API, stdlib, and shim shapes before implementation
   starts; implementation phases should migrate toward those shapes, not invent
   them while coding.

## Non-Goals

- Do not rename the entire stdlib to Go package names.
- Do not expose raw Go pointers or implementation-only ABI helper types.
- Do not force app-level Marmoset code to look like Go.
- Do not optimize codegen towers in this plan. Backend specialization can come
  later; this plan is about stdlib contracts and shim semantics.
- Do not redesign errors away from `Result`; Go errors should map into
  Marmoset domain errors at the boundary.
- Do not redesign `=>` into Roc-style managed effect descriptions.
- Do not introduce Rust-style lifetimes, ownership, capture checking, or
  locality modes as a prerequisite for stdlib resources.

## Design Principle

Stdlib modules should have two layers:

1. **Host-aligned trusted layer.** Extern/resource types and shims mirror Go's
   real runtime concepts: readers, writers, buffered readers/writers, files,
   requests, responses, contexts, database handles, transactions, and generated
   client/server types. This layer may use mutation, buffers, pooling,
   goroutines, channels, and generated Go directly.
2. **Marmoset convenience layer.** Public helpers provide nicer names,
   data-first composition, `Result`, and common operations. These helpers should
   be thin and should not obscure important runtime behavior.

Function names do not have to copy Go. Representation and semantics should.
The public stdlib should be trait-first for cross-domain resource capabilities:
domain modules construct/configure resources and may expose ergonomic wrappers,
but shared operations are defined by `std.io` traits.

## Locked API And Shim Shape

The following choices are locked for this plan. If implementation uncovers a
real conflict, update this plan first instead of improvising in code.

### Effects

- `->` means pure.
- `=>` means direct effect; it does not build a Roc/Elm-style effect
  description.
- failable effects return `Result`.
- future `Task`, `Mailbox`, and actor APIs are runtime/concurrency
  abstractions, not mandatory wrappers for every effect.

### Shims

- Go-backed stdlib modules use checked-in shims under
  `runtime/go/shims/std/<module>`.
- project-local Go wrappers for sqlc, gRPC, or application libraries use the
  same shim-first pattern under a project/app namespace.
- Marmoset-facing resource types are `extern type`s; Go shims hold the concrete
  Go implementation details.
- values crossing into Marmoset are owned `Str`, owned immutable `Bytes`,
  Marmoset records/enums, or opaque extern resources.
- borrowed Go slices/views are not part of the public API in this plan.
- Go errors map to domain-specific Marmoset error enums at the shim boundary.
  Unknown cases use the module's `Other(Str)`-style variant.

### Data

- `Bytes` remains immutable at the Marmoset layer and maps to owned byte data at
  the boundary.
- raw binary APIs use `Bytes`.
- text APIs use `Str`; conversion between `Bytes` and `Str` stays explicit and
  fallible when UTF-8 validity matters.
- public mutable buffer types are not part of the first backend-ready shape.
  Buffering should first live inside Go-backed resources such as `bufio.Reader`
  and `bufio.Writer`.

### Structural Public Data

Marmoset should be structural-first at the public data layer.

Rules:

- Public business/domain data should prefer Marmoset records, enums, lists,
  maps, `Option`, and `Result`.
- Opaque `extern type`s are for runtime resources, host handles, generated
  clients/servers, streaming bodies, database pools/transactions, and values
  whose structure is unstable, unsafe, or expensive to expose.
- Go-generated data from sqlc/gRPC/http wrappers should become Marmoset
  structural values when it is ordinary application/domain data.
- Do not expose Go structs as opaque externs just to avoid modeling public
  domain data in Marmoset.
- Do not collapse domain data into generic maps/string blobs unless the API is
  explicitly dynamic, such as raw JSON object traversal.
- Resource-like values may expose structural accessor functions, but their
  identity/lifecycle stays opaque.

Examples:

- `sql.User` returned to app code should usually be a Marmoset record.
- `grpc.UserServiceClient`, `db.Pool`, `db.Tx`, `http.Body`, and `net.Conn`
  should stay opaque resources.
- `http.Request` / `http.Response` may be structural wrappers around opaque
  body resources: headers/status/method/path are structural, body is an
  `io.Reader` resource.

### Terminal IO

`std.io` keeps the current terminal convenience surface:

```marmoset
fn read() => Result[Str, io.Error]
fn write(value: Str) => Result[Unit, io.Error]
fn print[a: Show](value: a) => Result[Unit, io.Error]
fn puts[a: Show](value: a) => Result[Unit, io.Error]
fn flush() => Result[Unit, io.Error]
```

Rules:

- `read` reads one stdin line and strips the line ending.
- `write` writes an exact `Str` to stdout.
- `print` renders with `Show` and writes no newline.
- `puts` renders with `Show` and writes one trailing newline.
- stdout/stderr `flush` remains callable for symmetry, but the current Go shims
  are direct writes and therefore do not require flush for prompt visibility.
- `std.io.err` mirrors stdout with `write`, `print`, `puts`, and `flush`.

### Resource Capability Traits

`std.io` owns small, law-like capability traits for effectful resources. These
traits are the canonical extensibility surface across files, buffered files,
network connections, HTTP bodies, database handles, tasks, and generated
wrappers.

Target traits:

```marmoset
trait Reader[r, e] = {
  fn read_chunk(reader: r, size: Int) => Result[Option[Bytes], e]
}

trait Writer[w, e] = {
  fn write(writer: w, bytes: Bytes) => Result[Unit, e]
}

trait Flush[r, e] = {
  fn flush(resource: r) => Result[Unit, e]
}

trait Close[r, e] = {
  fn close(resource: r) => Result[Unit, e]
}

trait Sync[r, e] = {
  fn sync(resource: r) => Result[Unit, e]
}

trait Seek[r, e] = {
  fn seek(resource: r, offset: Int) => Result[Unit, e]
}
```

Text helpers sit above byte streams:

```marmoset
fn read_line[r, e](reader: r) => Result[Option[Str], e]
fn write_str[w, e](writer: w, value: Str) => Result[Unit, e]
```

Rules:

- `Reader` / `Writer` are byte-oriented. Terminal text helpers remain separate
  convenience functions.
- `Flush` means buffered writer flush.
- `Sync` means durable sync, such as Go `os.File.Sync`.
- `Close` is intentionally in `std.io`; it covers effectful resources broadly,
  not only byte streams.
- domain modules may expose convenience functions such as `file.close(file)` or
  `db.Tx.close(tx)`, but those functions should delegate to the `std.io` trait
  implementation rather than defining a second lifecycle contract.

### Buffered Resource Variants

Buffered resources are concrete resources that implement the same `std.io`
traits. The first target API uses parallel domain modules instead of making
callers manually wrap resources through `bufio.reader_from_file(...)`.

Direct file API:

```marmoset
import std.file
import std.io

let f = try file.open(path, file.Mode.Read)
let chunk = try io.Reader.read_chunk(f, 8192)
try io.Close.close(f)
```

Buffered file API:

```marmoset
import std.buf.file
import std.io

let f = try file.open(path, file.Mode.Read)
let line = try io.read_line(f)
try io.Close.close(f)
```

Importing `std.file` or `std.buf.file` selects the direct or buffered concrete
resource type. Both resource types implement the relevant `std.io` traits.

Future `std.net` / `std.buf.net` should follow the same pattern for direct and
buffered network connections.

### Files And Directories

Whole-file helpers stay high-level and typed through `Bytes.Encode` /
`Bytes.Decode`:

```marmoset
fn read[a: Decode](path: path.Path) => Result[a, file.Error]
fn write[a: Encode](path: path.Path, value: a) => Result[Unit, file.Error]
fn append[a: Encode](path: path.Path, value: a) => Result[Unit, file.Error]
fn copy(from: path.Path, to: path.Path) => Result[Unit, file.Error]
```

File handles are opaque resources. The target shape is:

```marmoset
fn open(path: path.Path, mode: file.Mode) => Result[file.File, file.Error]
fn with_open[a, e](path: path.Path, mode: file.Mode, body: (file.File) => Result[a, e]) => Result[a, file.UseError[e]]
fn close(file: file.File) => Result[Unit, file.Error]
fn sync(file: file.File) => Result[Unit, file.Error]
```

Migration notes:

- current callback-shaped `file.open(path, mode, body)` should become
  `file.with_open`.
- current `flush_handle` / `file.flush` should be renamed to `sync` if it keeps
  using Go `File.Sync`.
- file-specific `close` and `sync` helpers delegate to `io.Close` and
  `io.Sync`; shared byte reads/writes go through `io.Reader` and `io.Writer`.
- ordinary buffered flushing belongs to `io.Flush` implementations on buffered
  resources, not `std.file`.
- using a closed file returns `Error.AlreadyClosed` at runtime; this plan does
  not require compile-time lifetime proof.

`std.buf.file` should mirror the same constructor and domain-helper shape where
it makes sense, but returns a buffered file resource backed by Go `bufio`.

Directories remain scalar filesystem helpers over `std.path.Path` values and
domain errors. They should not expose Go `os.FileInfo` or raw directory handles
until a concrete streaming directory API is needed.

### Networking, HTTP, gRPC, And sqlc

- `std.net.Conn` is an opaque extern resource backed by Go networking
  primitives. It implements the byte-oriented `io.Reader`, `io.Writer`, and
  `io.Close` traits. `std.buf.net.Conn` is the buffered counterpart.
- `std.http` should expose Marmoset request/response helpers but keep Go
  `net/http` details inside shims.
- server APIs are direct effects and may block:

```marmoset
fn serve[e](addr: Str, handler: (http.Request) => Result[http.Response, e]) => Result[Unit, http.ServeError[e]]
```

- `Task` / `Mailbox` / actor APIs should wrap goroutines, channels, contexts,
  and cancellation inside Marmoset resources. Raw Go channels are not exposed.
- gRPC and sqlc wrappers use project-local checked-in or generated shims around
  generated Go. Generated Go clients, servers, handles, and streaming resources
  can appear as opaque extern types; generated application/domain data should
  become Marmoset records/enums when it crosses into public Marmoset APIs.
- Do not translate generated Go data into generic maps/string blobs unless the
  domain API explicitly asks for that representation.

## Impure FP Boundary Model

Marmoset should not pretend that buffers, sockets, files, tasks, or database
pools are pure values. They are runtime resources. The stdlib should keep the
ordinary value layer immutable and predictable, while letting trusted Go shims
use imperative machinery internally.

Layering:

- `Bytes`, `Str`, records, enums, and collection values remain immutable value
  data for normal pure and effectful Marmoset code.
- `Buffer`, `Builder`, `Reader`, `Writer`, `File`, `Conn`, `Task`, `Mailbox`,
  database pools, transactions, and generated clients are opaque resources or
  runtime objects. Their operations are effectful and failable where relevant.
- resources may be scoped by callback/helper APIs, explicit close APIs, or
  later `defer`/`use`-style sugar. The compiler does not need to prove lifetime
  soundness before these resources are useful.
- closed, invalid, or misused resources should fail predictably through
  `Result` at the public boundary.
- data returned to Marmoset should be owned immutable values unless an advanced
  API explicitly documents otherwise.
- borrowed views, no-copy slices, locality modes, or escape analysis are
  deferred until a real workload proves that owned values and opaque resources
  are not enough.

This gives Marmoset FP semantics at the value layer and Go-backed performance
at the trusted shim/resource layer. It is the same separation we want for files,
network connections, buffered readers/writers, HTTP bodies, database rows,
generated gRPC/sqlc values, and future concurrency abstractions.

## IO And Buffering Direction

Implementation should follow the locked shape above:

- terminal `std.io` is text/convenience-oriented;
- backend streams are byte-oriented;
- `std.io` owns shared resource capability traits such as `Reader`, `Writer`,
  `Flush`, `Close`, `Sync`, and `Seek`;
- domain modules construct/configure resources and may expose thin convenience
  wrappers that delegate to those traits;
- buffered resource variants live under parallel domain modules such as
  `std.buf.file` and later `std.buf.net`;
- `io.Flush` means buffered-writer flush;
- `io.Sync` means durable sync;
- no public mutable buffer or borrowed slice API is introduced in the first
  backend-ready stdlib.

Representative buffered read:

```marmoset
import std.bytes
import std.buf.file
import std.io

let file = try file.open(path, file.Mode.Read)
let maybe_payload = try io.Reader.read_chunk(file, 8192)
match maybe_payload {
  case Option.Some(payload): try io.puts(bytes.to_str_lossy(payload))
  case Option.None: try io.puts("empty")
}
try io.Close.close(file)
```

Representative buffered write:

```marmoset
import std.buf.file
import std.io

let file = try file.open(path, file.Mode.Write)
try io.write_str(file, "payload")
try io.Flush.flush(file)
try io.Close.close(file)
```

Avoid making common resource code look like this:

```marmoset
try file.open(input, (src) => {
  try file.open(output, (dst) => {
    buffer.with_capacity(8192, (scratch) => {
      copy_loop(src, dst, scratch)
    })
  })
})
```

The target shape should flatten common multi-resource acquisition by moving
common operations into stdlib helpers and exposing explicit handles where
needed. For file-copy-like flows, prefer:

```marmoset
try file.copy(input, output)
```

until a future `defer` / `use` language feature is deliberately planned. The
important constraint is that Marmoset should not become callback soup, but also
should not require Rust-like static resource ownership for the first backend
stdlib.

Convenience helpers may remain pleasant:

```marmoset
try file.read_text(path)
try file.write_text(path, text)
try io.print("Name: ")
let name = try io.read()
try io.puts("Hello, #{name}")
```

But those helpers should lower through normal Go concepts: `os.File`,
`io.Reader`, `io.Writer`, `bufio.Reader`, `bufio.Writer`, `fmt`, and `[]byte`.

## Existing Smells To Audit

- `std.file` handle `flush` currently maps to Go `File.Sync`, which is durability
  sync, not ordinary buffered flush.
- `std.io.flush` for stdout/stderr is a no-op because stdout/stderr are direct
  writes. The public docs should not suggest prompt code needs it.
- `std.io.Read` / `Write` are currently string-shaped terminal/file protocols.
  That may be fine for scripting helpers, but real streaming/networking likely
  needs byte-oriented capability traits.
- `std.bytes.Bytes` is immutable, which is good for app code but not enough for
  zero-copy or low-allocation read loops by itself. The first response should be
  Go-backed opaque resources and owned chunks, not borrowed lifetime APIs.
- scoped file APIs currently compose through nested callbacks. That proves
  cleanup, but it scales poorly for ordinary copy/transform flows; domain
  helpers such as `file.copy` should cover common cases, while explicit handles
  plus `io.Close` cover advanced cases.

## Networking And Generated Go Pressure

The following areas should use host-aligned extern/resource types first:

- `net/http`: request, response, handler/server/client, body reader/writer,
  context, headers.
- gRPC: generated clients/servers/messages should cross the boundary as their
  generated Go types, not as generic maps or string blobs.
- sqlc/database: generated query structs, transactions, rows, and database
  handles should remain Go-shaped at the resource boundary.
- raw networking: TCP connections should implement the same reader/writer
  capabilities as files and buffers.

The Marmoset layer can then add nicer helpers for common application flows
without preventing low-level networking code from staying efficient.

## Implementation Plan

### Phase 0: Audit Current Stdlib Shims

Inspect:

- `runtime/go/shims/std/io`
- `runtime/go/shims/std/io/err`
- `runtime/go/shims/std/file`
- `runtime/go/shims/std/bytes`
- `runtime/go/shims/std/dir`

For each operation, record:

- Go primitive used,
- Marmoset API contract,
- whether the primitive matches the contract,
- allocation/copy behavior,
- close/flush/sync ownership behavior,
- error mapping.

### Phase 1: Align File And Terminal APIs

Migrate existing stdlib modules toward the locked shape:

- keep `std.io` terminal helpers as text/convenience helpers;
- add byte-oriented `std.io` capability traits: `Reader`, `Writer`, `Flush`,
  `Close`, `Sync`, and `Seek`;
- rename callback-shaped `file.open(path, mode, body)` to `file.with_open`;
- add handle-returning `file.open(path, mode)` and `file.close(file)`;
- add `file.copy(from, to)` as the common no-handle copy path;
- keep whole-file `file.read` / `file.write` / `file.append` typed through
  `Bytes.Decode` and `Bytes.Encode`;
- rename file durability from `flush` to `sync` if it still maps to
  `os.File.Sync`.

Tests must cover the migration aliases/removals deliberately so user-facing
examples do not keep drifting.

### Phase 2: Add Buffered Resource Variants

Introduce `std.buf.file` resources from the locked shape. Later `std.buf.net`
should follow the same pattern when networking lands.

Tests:

- buffered writer batches writes and flushes once,
- flush errors map to domain errors,
- buffered reader returns owned line/chunk values,
- direct and buffered file resources both satisfy the relevant `std.io`
  capability traits.

No public mutable `Buffer` API is added in this phase.

### Phase 3: Flatten Runtime Resource Composition

Implement the locked resource ergonomics before broader networking work.
The proof should include at least:

- copying or transforming two files without nested callback indentation;
- explicit close or scoped helper behavior on success, failure, and early
  `try` return;
- predictable runtime behavior if a closed or invalid handle is used;
- readable error propagation when acquisition of the second or third resource
  fails.

Do not require compile-time non-escape proofs in this phase.

### Phase 4: Use The Model In One Host Wrapper

Pick one generated/host-backed integration and prove the resource model works.
Good candidates:

- `net/http` request/response bodies,
- a small gRPC generated client/server fixture,
- a sqlc-generated query wrapper fixture.

## Testing Strategy

Add tests at three levels:

- shim unit/integration tests for exact Go primitive behavior;
- stdlib fixture tests for public Marmoset ergonomics;
- generated-Go or snapshot tests for host-wrapper shape where useful.

Focused commands will depend on the phase, but likely include:

```bash
make integration stdlib-shims
make integration std
make integration ffi
make unit compiler
```

## Deferred Questions

These are intentionally not part of the first backend-ready stdlib shape:

1. Should Marmoset eventually add `defer` or `use` syntax for runtime cleanup,
   after the stdlib helper shape is proven?
2. Do real workloads require a public mutable `Buffer` API, or are owned
   `Bytes` values plus opaque Go-backed resources enough?
3. Should `puts` remain the long-term Marmoset line-output helper, or should a
   future compatibility pass add/prefer `println`?
4. Which resource misuse cases are worth elevating from runtime `Result`
   failures into compiler diagnostics later?
5. Does the `std.buf.<domain>` module-family pattern scale beyond files and
   network connections, or should lower-level explicit buffered wrappers be
   added for unusual composition stacks later?
6. Which generated host values should stay opaque for performance/lifecycle
   reasons, and which should be converted to public structural Marmoset data?

## Progress

- 2026-05-14 23:20 CEST: Drafted after the `std.io` output naming slice exposed
  the distinction between Marmoset-facing names and Go-aligned shim/runtime
  semantics. The immediate concrete follow-up is to audit `flush`/`sync` and
  decide whether core IO traits should be byte-oriented before networking or
  generated-Go wrappers are added.
- 2026-05-15 00:23 CEST: Added the value/resource split: immutable Marmoset
  values keep functional semantics, while mutable buffers/builders/readers and
  writers are scoped effectful resources for Go-level performance. Also added a
  phase to flatten multi-resource code so two-file-plus-buffer workflows do not
  devolve into callback pyramids.
- 2026-05-15 01:08 CEST: Settled the high-level direction after comparing
  Roc-style platform purity with Gleam/OCaml-style impure FP. Marmoset follows
  the impure FP path: pure `->` functions stay protected, effectful `=>`
  functions perform direct effects, shims/platform modules are trusted
  imperative Go boundaries, and resource misuse is handled by API discipline
  plus `Result` rather than a Rust-like ownership/lifetime system.
- 2026-05-15 02:37 CEST: Locked the target API, stdlib, and shim shapes before
  implementation: terminal IO stays text/convenience-oriented, backend streams
  become byte-oriented, file durability is `sync` rather than `flush`, file copy
  is a high-level helper, Go shims return owned values or opaque resources, and
  gRPC/sqlc/http wrappers follow the same typed shim-first boundary.
- 2026-05-17 18:02 CEST: Refined the locked shape around Marmoset's trait
  model. `std.io` now owns cross-domain capability traits such as `Reader`,
  `Writer`, `Flush`, `Close`, `Sync`, and `Seek`; domain modules construct and
  configure resources and may provide thin helper functions that delegate to
  those traits. Buffered resources use parallel domain modules such as
  `std.buf.file` instead of a primary `std.bufio.reader_from_file` wrapper
  ceremony.
- 2026-05-18 00:07 CEST: Locked the structural-first data rule: public
  business/domain data should be Marmoset records/enums/collections, while
  opaque externs are reserved for runtime resources, host handles, generated
  clients/servers, streaming bodies, and values whose structure is unsafe,
  unstable, or too expensive to expose.
