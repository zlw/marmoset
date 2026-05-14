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

## Non-Goals

- Do not rename the entire stdlib to Go package names.
- Do not expose raw Go pointers or implementation-only ABI helper types.
- Do not force app-level Marmoset code to look like Go.
- Do not optimize codegen towers in this plan. Backend specialization can come
  later; this plan is about stdlib contracts and shim semantics.
- Do not redesign errors away from `Result`; Go errors should map into
  Marmoset domain errors at the boundary.

## Design Principle

Stdlib modules should have two layers:

1. **Host-aligned resource layer.** Extern/resource types and shims mirror Go's
   real runtime concepts: readers, writers, buffered readers/writers, files,
   requests, responses, contexts, database handles, transactions, and generated
   client/server types.
2. **Marmoset convenience layer.** Public helpers provide nicer names,
   data-first composition, `Result`, and common operations. These helpers should
   be thin and should not obscure important runtime behavior.

Function names do not have to copy Go. Representation and semantics should.

## IO And Buffering Direction

Questions to settle before adding broader IO APIs:

- Should `std.io.Read` / `std.io.Write` remain text-first, or should the core
  traits become byte-oriented and text helpers sit above them?
- Do we need separate immutable `Bytes` and mutable/borrowed buffer types before
  performant streaming is possible?
- Should buffered IO live in `std.bufio`, with extern resources backed by
  `*bufio.Reader` and `*bufio.Writer`?
- Should `flush` mean `bufio.Writer.Flush`, while file durability uses a
  separate `sync` operation for `os.File.Sync`?
- Should terminal `print` / `puts` use direct stdout/stderr writes, while
  generic buffered writers expose explicit flushing?

Likely shape:

```marmoset
import std.bufio
import std.io

let reader = bufio.reader(file)
let writer = bufio.writer(file)

try io.write(writer, bytes)
try io.flush(writer)
```

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
  zero-copy or low-allocation read loops by itself.

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
- whether a mutable buffer type is needed,
- whether `read_line` style helpers belong in `io`, `bufio`, or `file`.

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

## Open Questions

1. Should the current `io.Write[w, e]` text trait be replaced, renamed, or kept
   as a convenience above byte-oriented writer capabilities?
2. What is the minimum mutable buffer API needed before networking work starts?
3. Should `puts` remain the Marmoset line-output helper, or should host-aligned
   output eventually prefer `println` while keeping `puts` as compatibility
   sugar?
4. How much Go stdlib shape should be visible in module names such as `bufio`,
   `http`, `sql`, and `context`?
5. Do we need explicit ownership/lifetime rules for extern resources before
   exposing HTTP/gRPC/sqlc wrappers?

## Progress

- 2026-05-14 23:20 CEST: Drafted after the `std.io` output naming slice exposed
  the distinction between Marmoset-facing names and Go-aligned shim/runtime
  semantics. The immediate concrete follow-up is to audit `flush`/`sync` and
  decide whether core IO traits should be byte-oriented before networking or
  generated-Go wrappers are added.
