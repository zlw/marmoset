# Host-Aligned Stdlib Runtime Model

## Maintenance

- Last verified: 2026-05-14
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

Questions to settle before adding broader IO APIs:

- Should `std.io.Read` / `std.io.Write` remain text-first, or should the core
  traits become byte-oriented and text helpers sit above them?
- Can performant streaming be handled inside Go-backed shims and opaque
  resources before exposing mutable/borrowed buffer types to Marmoset?
- Should buffered IO live in `std.bufio`, with extern resources backed by
  `*bufio.Reader` and `*bufio.Writer`?
- Should `flush` mean `bufio.Writer.Flush`, while file durability uses a
  separate `sync` operation for `os.File.Sync`?
- Should terminal `print` / `puts` use direct stdout/stderr writes, while
  generic buffered writers expose explicit flushing?
- Which library-level resource helpers give good ergonomics without committing
  to ownership/lifetime syntax?

Likely shape:

```marmoset
import std.bufio
import std.io

let reader = bufio.reader(file)
let writer = bufio.writer(file)

try io.write(writer, bytes)
try io.flush(writer)
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

The target shape should flatten common multi-resource acquisition while keeping
cleanup a library/runtime discipline. Prefer stdlib helpers or later
`defer`/`use` sugar over ownership/lifetime machinery. For example:

```marmoset
resource.use3(file.open(input), file.open(output), buffer.with_capacity(8192), (src, dst, scratch) => {
  try copy_loop(src, dst, scratch)
})
```

The exact helper name and arity story are open. The important constraint is
that Marmoset should not become callback soup, but also should not require
Rust-like static resource ownership for the first backend stdlib.

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
  needs bytes and reusable buffers.
- `std.bytes.Bytes` is immutable, which is good for app code but not enough for
  zero-copy or low-allocation read loops by itself. The first response should be
  Go-backed opaque resources and owned chunks, not borrowed lifetime APIs.
- scoped file APIs currently compose through nested callbacks. That proves
  cleanup, but it scales poorly for ordinary copy/transform flows that need two
  files plus a scratch buffer.

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

### Phase 1: Decide IO Trait Shape

Decide whether to keep the current text-shaped traits as terminal conveniences
or introduce byte-oriented core traits for files, network connections, and
buffered resources.

The plan should explicitly decide:

- how `Str` encoding/decoding is handled,
- when `Bytes` is copied,
- whether a public mutable buffer type is needed now, or whether buffering can
  stay inside Go-backed resources,
- whether `read_line` style helpers belong in `io`, `bufio`, or `file`.

The decision must preserve this split:

- immutable `Bytes` for stable value semantics;
- opaque Go-backed resources for performance-sensitive loops;
- owned value boundaries between resources and normal Marmoset data.

### Phase 2: Separate Flush From Sync

If file durability remains public, give it a name that matches the Go concept.

Candidate direction:

- `flush`: buffered writer flush,
- `sync`: durable file sync,
- terminal stdout/stderr: no public flush requirement for prompt visibility.

### Phase 3: Add Buffered Resources

Introduce a small `std.bufio` proof slice backed by Go `bufio`.

Test:

- buffered writer batches writes and flushes once,
- flush errors map to domain errors,
- buffered reader reads lines/chunks without per-byte allocation where possible,
- file and network-like resources can share reader/writer capabilities.

### Phase 4: Flatten Runtime Resource Composition

Design and test the resource composition API before broader networking work.
The proof should include at least:

- opening two files and one scratch buffer without nested callback indentation;
- cleanup on success, failure, and early `try` return;
- predictable runtime behavior if a closed or invalid handle is used;
- readable error propagation when acquisition of the second or third resource
  fails.

Candidate approaches:

- stdlib `resource.with` / `resource.using` helper;
- later `defer` or `use` sugar with runtime cleanup semantics;
- arity-specific helpers only as a temporary bridge if the language form is not
  ready.

Do not require compile-time non-escape proofs in this phase.

### Phase 5: Use The Model In One Host Wrapper

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

## Open Questions

1. Should the current `io.Write[w, e]` text trait be replaced, renamed, or kept
   as a convenience above byte-oriented writer capabilities?
2. Can networking/file performance stay behind Go-backed opaque resources, or
   do we need a public mutable buffer API before real workloads demand it?
3. Should resource ergonomics use stdlib helpers first, or should a small
   `defer`/`use` syntax be planned after the helper shape is proven?
4. Should `puts` remain the Marmoset line-output helper, or should host-aligned
   output eventually prefer `println` while keeping `puts` as compatibility
   sugar?
5. How much Go stdlib shape should be visible in module names such as `bufio`,
   `http`, `sql`, and `context`?
6. Which resource misuse cases should be compile-time errors, and which should
   remain runtime `Result` failures?

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
