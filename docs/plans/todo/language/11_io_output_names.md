# std.io Output Naming

## Maintenance

- Last verified: 2026-05-14
- Implementation status: In progress
- Area: Language / stdlib
- Supersedes: the `std.io` output naming details in `docs/plans/todo/language/05_stdlib.md`

## Context

`std.io` currently exposes `write`, `flush`, and `print`. Its `print` helper
renders through `Show`, appends a newline, and flushes. That makes the name
carry too many meanings:

- raw string vs `Show` rendering,
- no newline vs newline,
- buffered write vs explicit flush.

The fixture migration plan also needs one canonical output spelling before the
repo-wide `puts(...)` cleanup starts. If fixtures migrate to `io.print(...)`
now, but we later split `print` and `puts`, we create churn and mixed examples.

Current relevant files:

- `std/io.mr`
- `std/io/err.mr`
- `std/basics.mr`
- `runtime/go/shims/std/io/io.go`
- `runtime/go/shims/std/io/err/err.go`
- `runtime/go/shims/std/basics/basics.go`
- `lib/frontend/typecheck/builtins.ml`
- `lib/frontend/compiler.ml`
- `tools/lsp/lib/doc_state.ml`
- editor grammar/highlight metadata under `tools/*marmoset*`
- examples, docs, and fixtures that still call bare `puts(...)`

## Goals

1. Give terminal/file output helpers orthogonal names:
   - `write`: raw `Str`
   - `print`: `Show` rendering, no newline
   - `puts`: `Show` rendering plus trailing newline
   - `flush`: explicit flushing
2. Keep stdout and stderr APIs symmetric.
3. Keep `io.Write[w, e]` useful for stdout, stderr, and file handles without
   forcing a flush after every line.
4. Make `io.puts(...)` the canonical line-oriented output helper for fixtures,
   examples, and docs.
5. Remove the old implicit/bare `puts(...)` surface after the migration.

## Non-Goals

- Do not change `std.io.read` semantics.
- Do not introduce `println`; `puts` is the line-output spelling.
- Do not make `print` or `puts` flush implicitly in this plan.
- Do not remove the runtime/builtin `puts` compatibility surface until fixtures,
  examples, docs, and editor metadata have migrated.
- Do not change Go stdout/stderr shim behavior beyond what is needed to support
  the Marmoset API names.

## Semantics

Target public API:

```marmoset
import std.prelude.Show

export Error, Read, Write, read, write, print, puts, flush

trait Write[w, e] = {
  fn write(writer: w, value: Str) => Result[Unit, e]
  fn flush(writer: w) => Result[Unit, e]

  fn print[a: Show](writer: w, value: a) => Result[Unit, e] =
    Write.write(writer, "#{value}")

  fn puts[a: Show](writer: w, value: a) => Result[Unit, e] =
    Write.write(writer, "#{value}\n")
}

fn write(value: Str) => Result[Unit, Error]
fn print[a: Show](value: a) => Result[Unit, Error]
fn puts[a: Show](value: a) => Result[Unit, Error]
fn flush() => Result[Unit, Error]
```

Stderr mirrors stdout:

```marmoset
import std.io

export Error, write, print, puts, flush

fn write(value: Str) => Result[Unit, Error]
fn print[a: Show](value: a) => Result[Unit, Error]
fn puts[a: Show](value: a) => Result[Unit, Error]
fn flush() => Result[Unit, Error]
```

Rules:

- `write` accepts only `Str`; callers choose exact bytes/text and newline
  placement.
- `print` renders with `Show` and writes no newline.
- `puts` renders with `Show` and writes exactly one trailing newline.
- `flush` is explicit. Callers that need prompt-style behavior write and flush:

```marmoset
try io.write("Name: ")
try io.flush()
let name = try io.read()
try io.puts("Hello, #{name}")
```

## Implementation Plan

### Phase 0: Red Stdlib Tests

Add failing coverage before implementation.

Expected fixture targets once the std fixture group exists:

- `test/fixtures/std/io/io001_write_print_puts_stdout.mr`
- `test/fixtures/std/io/io002_write_print_puts_stderr.mr`
- `test/fixtures/std/io/io003_prompt_requires_explicit_flush.mr`

Until the fixture harness supports std fixture projects, add equivalent focused
coverage in `test/integration/14_stdlib_shims.sh` or the first std fixture
slice from `docs/plans/todo/tooling/02_stdlib_fixture_corpus.md`.

Assertions:

- `io.write("a")`, `io.print(1)`, `io.puts("b")` produce `a1b\n`.
- `io.err.write("a")`, `io.err.print(1)`, `io.err.puts("b")` produce the same
  text on stderr once stderr-specific fixture assertions exist.
- `io.print("x")` does not append a newline.
- `io.puts("x")` appends one newline.
- `io.flush()` remains callable and failable through `Result`.

### Phase 1: Add `puts` Without Removing `print`

Update:

- `std/io.mr`
- `std/io/err.mr`

Changes:

- Change `Write.print` default implementation to write `#{value}` with no
  newline and no flush.
- Add `Write.puts` default implementation writing `#{value}\n`.
- Export top-level `io.puts`.
- Export `std.io.err.puts`.
- Keep `io.print` and `err.print` with the new no-newline semantics.
- Keep `io.write` and `err.write` as raw `Str` helpers.
- Keep `flush` unchanged.

The Go `std/io` and `std/io/err` shims should not need new functions because
`print` and `puts` are Marmoset-level defaults over `write`.

Focused verification:

```bash
make integration stdlib
```

or, after fixture migration starts:

```bash
make integration std/io
```

### Phase 2: Update Docs, Examples, And The Fixture Migration Plan

Update user-facing examples:

- line output -> `try io.puts(...)`
- prompt/no-newline output -> `try io.write(...)` plus `try io.flush()` when the
  following read depends on the prompt being visible
- no-newline `Show` rendering -> `try io.print(...)`

Update:

- `examples/hello_stdin.mr`
- docs under `docs/features`
- `docs/plans/todo/language/05_stdlib.md`
- `docs/plans/todo/tooling/02_stdlib_fixture_corpus.md`
- editor docs/highlight builtin lists if they mention `puts` or `print`

### Phase 3: Migrate Fixtures To `io.puts`

This phase is owned mostly by
`docs/plans/todo/tooling/02_stdlib_fixture_corpus.md`.

Rules:

- Replace line-oriented fixture output with `try io.puts(...)`.
- Use `try io.write(...)` only when the absence of a newline is part of the
  assertion.
- Use `try io.print(...)` only when the fixture specifically asserts
  no-newline `Show` rendering.
- Preserve exact `# output:` expectations.
- Update purity/effect fixtures deliberately because `io.puts` is failable and
  returns `Result[Unit, io.Error]`.

Focused verification:

```bash
make integration std
make integration result purity runtime
```

### Phase 4: Remove Bare `puts`

After fixtures, docs, examples, and editor metadata stop using bare `puts(...)`,
remove the compatibility surface.

Update:

- `std/basics.mr`
- `runtime/go/shims/std/basics/basics.go`
- `lib/frontend/typecheck/builtins.ml`
- `lib/frontend/compiler.ml`
- `tools/lsp/lib/doc_state.ml`
- editor grammar/highlight metadata

Expected end state:

- `puts` is not seeded as a builtin value.
- `std.basics` no longer exports public `puts`.
- no compiler/LSP bootstrap string defines `std.basics.puts`.
- bare `puts(...)` produces a normal unknown-name diagnostic.
- `io.puts(...)` is the standard line-output API.

Add rejection coverage:

```marmoset
puts("hello")  # error: Unknown identifier 'puts'
```

The exact diagnostic text should match the current canonical unknown-name
diagnostic.

## Testing Strategy

Focused commands:

```bash
make integration stdlib
make integration std/io
make integration result purity runtime
make unit compiler
make unit lsp
```

Search gates:

```bash
rg -n '\bputs\(' test/fixtures examples docs std
rg -n 'io\.print|io\.puts|println' docs examples std test
```

The first gate should eventually find no bare fixture/example/doc `puts(...)`
calls except deliberate historical notes or removed-plan progress text.

## Commit Plan

1. Add red tests for `io.write` / `io.print` / `io.puts` semantics.
2. Implement `io.puts`, `err.puts`, and redefine `print` as no-newline.
3. Update docs/examples/plans to use `io.puts` for line output.
4. Migrate fixtures away from bare `puts(...)`.
5. Remove bare builtin/std.basics `puts` and update editor/LSP metadata.

## Risks

- `io.print` changes behavior from newline+flush to no-newline. Any existing
  user code or examples using `io.print` for line output must migrate to
  `io.puts`.
- Removing bare `puts` makes output fixtures depend on stdlib, `Result`, and
  top-level `try`. Keep enough low-level compiler/unit coverage to debug stdlib
  failures.
- If the fixture harness still merges stdout and stderr, stderr tests may pass
  for the wrong reason. Stderr assertions belong with the fixture harness update.

## Progress

- 2026-05-14 22:17 CEST: Draft mini-plan created after inspecting current
  `std.io`, `std.io.err`, `std.basics`, runtime shims, stdlib plan text, docs,
  examples, and fixture usage.
- 2026-05-14 22:38 CEST: Implementation slice started for Phases 0-2: add red
  stdlib-shim coverage for `write`/`print`/`puts`, update `std.io` and
  `std.io.err`, and update user-facing examples/docs. Bare `puts` removal stays
  deferred until the broader fixture migration lands.
- 2026-05-14 22:39 CEST: Red focused test observed with
  `make integration stdlib-shims`: the new stdout/stderr helper case fails
  because `std.io.puts` is not exported yet.
- 2026-05-14 22:42 CEST: Implemented `std.io.puts` and `std.io.err.puts`,
  changed `print` to no-newline/no-flush, and reran `make integration
  stdlib-shims`; all 26 stdlib-shim tests passed.
- 2026-05-14 22:43 CEST: Focused verification passed for
  `examples/hello_stdin.mr`, `make unit compiler`, `make unit lsp`,
  `make integration stdlib-shims`, and `make integration result purity`.
  `make integration runtime` still has the pre-existing
  `runtime/b16_string_backslash_literal` escape-output mismatch, and
  `./test/ci/quality.sh` still reports unrelated repo-wide ocamlformat drift.
