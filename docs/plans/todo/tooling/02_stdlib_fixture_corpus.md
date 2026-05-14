# Stdlib Fixture Corpus and Integration Suite Rebalance

## Maintenance

- Last verified: 2026-05-14
- Implementation status: Planning
- Area: Tooling / integration tests

## Context

The fixture harness migration moved most historical heredoc tests into
`test/fixtures`, but the new stdlib work has grown back into standalone shell
suites. The main examples are:

- `test/integration/12_prelude.sh`
- `test/integration/13_ffi.sh`
- `test/integration/14_stdlib_shims.sh`

Those suites now test normal stdlib behavior, API removals, file/dir fixtures,
`Result`/`Option`/prelude behavior, stdin/stdout/stderr behavior, and some
generated Go shape. That makes stdlib coverage harder to discover and pushes
ordinary language examples into shell.

Current fixture state:

- `test/fixtures` has no `std` top-level group.
- `test/fixtures/result` exists, but it mostly tests language semantics around
  `try`, `wrap`, `try-or`, and Result propagation, not stdlib API ownership.
- `test/integration.sh` discovers top-level fixture groups and recursively runs
  `.mr` files. `modules*` groups are special-cased to run only `main*.mr`.
- `run_mode` currently builds a fixture path and executes the binary from the
  runner's cwd. It does not run from the fixture directory and does not copy a
  fixture project to a temp sandbox.
- Runtime output annotations are `# output:` and are compared against combined
  process output via `2>&1`.
- There are still many bare `puts(...)` calls in fixtures. `puts` is useful as
  a historical runtime primitive, but it is no longer the example we want users
  to copy. New line-oriented user-facing code should use `std.io.puts` and
  handle its `Result`; `std.io.write` is for raw strings without a newline, and
  `std.io.print` is for `Show` rendering without a newline.
- Snapshot tests now exist under `test/integration/10_codegen_snapshots.sh`, so
  generated Go shape checks should not require ad hoc shell test bodies.

There is also an active plan for an OCaml test runner at
`docs/plans/todo/tooling/01_ocaml-test-harness.md`. This plan does not require
that rewrite to land first. The capabilities below may be implemented in the
current bash runner or folded into the OCaml runner if that plan lands first.

## Goals

1. Add a dedicated stdlib fixture corpus under `test/fixtures/std`.
2. Move normal stdlib, prelude, `Option`, and `Result` integration tests out of
   standalone shell suites and into fixture files or fixture projects.
3. Treat fixture projects as realistic tiny projects:
   - relative paths resolve through the process cwd,
   - project fixtures run with cwd set to the fixture project root,
   - mutating project fixtures run in a temp copy.
4. Move generated Go inspection from shell into snapshot tests.
5. Keep shell integration suites only for boundary tests that are actually shell
   concerns: CLI behavior, harness canaries, and any process behavior not yet
   expressible as fixtures after deliberate discussion.
6. Replace fixture usage of bare `puts(...)` with `std.io.puts(...)` plus
   explicit error handling, usually top-level `try`.
7. Audit the existing fixture corpus so stdlib behavior lives in stdlib
   fixtures and language/compiler fixtures focus on language/compiler behavior.
8. Specifically audit the codegen and module fixture families now that snapshot
   tests exist:
   - `codegen`, `codegen_canary`, `codegen_mono`, `codegen_stress`
   - `modules`, `modules_codegen`, `modules_codegen_edge`, `modules_edge`

## Non-Goals

- Do not change `std.file` path semantics. `file.read(Path("data.txt"))`
  should resolve relative to the process cwd, matching normal program behavior.
- Do not introduce `AbsolutePath` / `RelativePath` as part of this plan. That is
  a separate stdlib API design question.
- Do not remove the compiler/runtime `puts` primitive in this plan. This plan
  removes fixture reliance on it.
- Do not weaken fixture expectations while moving tests.
- Do not delete broad stress/regression coverage without recording why the test
  is duplicate, obsolete, or better covered by a smaller fixture/snapshot.

## Design Decisions

### 1. Stdlib Gets Its Own Fixture Group

Create this top-level layout:

```text
test/fixtures/std/
  README.md
  prelude/
  option/
  result/
  error/
  bytes/
  path/
  io/
  file/
  dir/
```

Use two fixture shapes.

Single-file fixtures for pure or simple tests:

```text
test/fixtures/std/result/r001_map_success.mr
test/fixtures/std/path/p001_manipulates_path_values.mr
```

Project fixtures for tests with data files, stdin sidecars, mutating outputs, or
multiple source files:

```text
test/fixtures/std/file/f001_read_text/
  main.mr
  input.txt

test/fixtures/std/file/f002_invalid_utf8/
  main.mr
  invalid.bin

test/fixtures/std/dir/d001_listing/
  main.mr
  listing/
    entry.txt
```

The test name for project fixtures should be the fixture directory path, for
example `std/file/f001_read_text`, not `std/file/f001_read_text/main`.

### 2. Fixture Projects Run Like Tiny Projects

Runtime cwd should be the directory containing the fixture entrypoint.

For a single-file fixture:

```text
test/fixtures/std/path/p001_manipulates_path_values.mr
```

the cwd is:

```text
test/fixtures/std/path
```

For a project fixture:

```text
test/fixtures/std/file/f001_read_text/main.mr
```

the cwd is:

```text
test/fixtures/std/file/f001_read_text
```

This is harness setup, not language semantics. It makes relative `Path("x")`
tests realistic because real users also run a program from some cwd.

### 3. Mutating Fixture Projects Use Temp Copies

Project fixtures that write files, append files, create directories, remove
directories, or intentionally leak/close handles should run in a temp copy.

Add a leading fixture metadata annotation:

```marmoset
# fixture: sandbox
```

Behavior:

1. Copy the whole fixture project directory to a temp directory.
2. Build the copied entrypoint.
3. Execute the binary with cwd set to the copied project root.
4. Normalize diagnostics from the copied path back to the original fixture path
   when matching `# error:` / `# warning:` annotations.
5. Delete the temp copy after the test.

Fixtures without `# fixture: sandbox` run in place with cwd set to the fixture
directory.

### 4. Fixture Entrypoint Discovery Is Explicit For Projects

Update `group_fixture_files` in `test/integration.sh` so a directory containing
`main.mr` may be treated as a project fixture entrypoint.

Rules:

1. For ordinary directories, keep recursive `.mr` discovery.
2. For fixture project directories, discover only `main.mr`.
3. If a project directory contains helper `.mr` files, they are imports, not
   independent tests.
4. Preserve existing `modules*` behavior until the module audit phase decides
   whether the special case should be generalized or removed.

Add selector support for nested suites:

```bash
make integration std
make integration std/file
make integration std/file/f001_read_text
make integration std/file/f001_read_text/main.mr
```

### 5. Runtime Fixtures Need Status, Stdin, Stdout, And Stderr

Extend fixture metadata and output assertions instead of keeping shell tests for
these cases.

Leading metadata:

```marmoset
# stdin-file: stdin.txt
# status: 1
```

Output annotations:

```marmoset
# output: line on stdout
# stderr: line on stderr
```

Compatibility:

- `# output:` remains stdout expectation for existing fixtures.
- `# stderr:` is new.
- Default `# status:` is `0`.
- If `# stdin-file:` is absent, execute with empty stdin.
- Build-reject fixtures keep using inline `# error:`, `# warning:`, and
  `# info:`.

Runtime execution should capture stdout and stderr separately. This allows
`std.io.err` tests to assert the correct stream instead of relying on merged
`2>&1` behavior.

### 6. `std.io.puts` Becomes The Fixture Output Idiom

New and migrated run fixtures should print via stdlib:

```marmoset
import std.io

try io.puts("hello")
```

For values:

```marmoset
import std.io

try io.puts(Show.show(value))
```

or, when the value already has the intended `Show` instance:

```marmoset
try io.puts(value)
```

Harness placement validation must accept output anchors like:

```marmoset
try io.puts("hello")  # output: hello
```

and preceding-line annotations targeting such calls:

```marmoset
# output: hello
try io.puts("hello")
```

The fixture corpus should not use `puts(...)` after the migration except in a
small runtime-only canary if we explicitly decide the builtin still needs an
end-to-end fixture. Prefer an OCaml unit test or compiler runtime test for that
primitive instead of normal fixtures.

### 7. Shell Suite Ownership

Target ownership:

- `08_cli.sh`: stays shell. It tests CLI protocol and process behavior.
- `09_harness_canaries.sh`: stays shell while the runner is bash. If the OCaml
  runner lands, convert these into runner self-tests.
- `10_codegen_snapshots.sh`: stays as the snapshot entrypoint or becomes a
  snapshot manifest runner. Generated Go shape belongs here, not in stdlib shell
  tests.
- `11_pre_modules_hardening.sh`: split. Snapshot/structural checks stay in
  snapshot tests; ordinary reject/run cases become fixtures.
- `12_prelude.sh`: migrate to `test/fixtures/std/prelude`,
  `test/fixtures/std/option`, `test/fixtures/std/result`, and
  `test/fixtures/std/error`, then delete the suite.
- `13_ffi.sh`: migrate existing fixture-backed tests into normal fixtures and
  keep/generated tree checks in snapshots, then delete or reduce the suite to
  true extern boundary checks.
- `14_stdlib_shims.sh`: migrate to `test/fixtures/std/{bytes,path,file,dir,io}`
  and snapshot tests, then delete the suite.

## Implementation Plan

### Phase 0: Baseline And Red Harness Canaries

Add canaries first, before implementation:

1. Cwd canary: a fixture project reads `Path("input.txt")` from its own folder.
   It should fail before cwd support and pass after.
2. Sandbox canary: a fixture project writes/appends/removes a file under its
   fixture directory. The test should pass and leave the tracked source fixture
   unchanged.
3. Stdin canary: a fixture with `# stdin-file:` reads from stdin and prints exact
   output.
4. Runtime status canary: a fixture with `# status: 1` executes a top-level
   `try` failure and verifies the expected output.
5. Stderr canary: a fixture prints to `std.io.err` and verifies `# stderr:`.
6. Nested selector canary: `make integration std/file` selects only that nested
   subtree.
7. Project discovery canary: helper `.mr` files inside a project directory are
   not executed as independent tests.

Suggested files:

- `test/integration/09_harness_canaries.sh`
- temporary canary fixtures under `test/fixtures/.harness_canary.*` as the
  current harness canaries already do

Focused verification:

```bash
make integration harness
```

### Phase 1: Implement Fixture Project Semantics

Touchpoints:

- `test/integration.sh`
  - `resolve_selector`
  - `group_fixture_files`
  - `parse_fixture`
  - `run_mode`
  - `run_diagnostics_mode`
  - `build_only_mode`
  - `run_fixture`
  - diagnostic path normalization helpers
- `test/integration/common.sh` only if shared process helpers are useful

Required behavior:

1. Parse leading metadata:
   - `# fixture: sandbox`
   - `# stdin-file: <relative-path>`
   - `# status: <integer>`
2. Capture stdout and stderr separately in runtime modes.
3. Compare `# output:` against stdout and `# stderr:` against stderr.
4. Feed stdin from the fixture cwd or sandbox copy.
5. Execute the binary with cwd set to the fixture cwd.
6. For sandboxed fixtures, build and run against the temp copy while reporting
   the original fixture name.
7. Preserve current strict reject diagnostics, wildcard semantics, xfail
   semantics, output placement validation, and deterministic reporting.
8. Extend output anchor validation for `try io.puts(...)`,
   `try err.puts(...)`, `try io.print(...)`, `try err.print(...)`,
   `io.puts(...)`, `err.puts(...)`, `io.print(...)`, and `err.print(...)`.

Focused verification:

```bash
make integration harness
make integration result
make integration modules_codegen
```

### Phase 2: Establish Std Fixture Ownership

Add:

- `test/fixtures/std/README.md`
- initial smoke fixtures:
  - `std/io/io001_write_print_puts_stdout.mr`
  - `std/io/io002_write_print_puts_stderr.mr`
  - `std/path/path001_manipulates_values.mr`
  - `std/result/result001_map_bind_wrap_value_or.mr`
  - `std/option/option001_map_bind_value_or.mr`
  - `std/prelude/prelude001_headerless_entry.mr`
  - `std/error/error001_message_context_frames.mr`

The README should define ownership rules:

- std fixtures own stdlib API behavior and examples users should copy.
- language fixtures own parser/resolver/typechecker/codegen behavior.
- codegen snapshots own generated Go shape.
- shell owns process boundary behavior only.

Focused verification:

```bash
make integration std
make integration std/io
```

### Phase 3: Migrate `12_prelude.sh`

Move each test:

- headerless prelude entry -> `std/prelude`
- Option minimal API and absence of predicate helpers -> `std/option`
- Result helpers, `wrap`, and effect-polymorphic `bind` -> `std/result`
- `std.error` canonical messages and frames -> `std/error`
- top-level `try` Result failure -> either `std/result` or a new
  `std/prelude/top_level_try_*` fixture with `# status: 1`
- top-level `try wrap` failure -> `std/result`
- top-level `try` Option.None -> `std/option`

Rewrite outputs from `puts` to:

```marmoset
import std.io

try io.puts(...)
```

After parity is green, delete `test/integration/12_prelude.sh` and remove its
selector branch from `test/integration.sh`.

Focused verification:

```bash
make integration std/prelude std/option std/result std/error
```

### Phase 4: Migrate `14_stdlib_shims.sh`

Move pure/simple tests:

- bytes codecs -> `std/bytes`
- bytes decode error canonical message -> `std/bytes` or `std/file` depending
  on whether the fixture reads invalid data from disk
- path manipulation -> `std/path`
- old std.path structural helper rejection -> `std/path`

Move file fixtures:

- text/bytes read/write/append -> `std/file` project fixture with sandbox
- invalid UTF-8 file read -> `std/file` project fixture with `invalid.bin`
- missing path and directory-as-file errors -> `std/file`
- handle scopes and `io.Read` / `io.Write` protocols -> `std/file`
- raw private shim rejection and old helper rejection -> `std/file`

Move dir fixtures:

- exists/mkdir/mkdir_tree/ls/pwd/rmdir/rmdir_tree -> `std/dir` project fixtures
  with sandbox

Move io fixtures:

- stdout write/print/puts/flush -> `std/io`
- stderr write/print/puts/flush -> `std/io` with `# stderr:`
- stdin line reading and EOF -> `std/io` with `# stdin-file:`
- private shim/token rejection and old helper rejection -> `std/io`

Move generated Go shape:

- `std.io terminal helpers and std.file handles use private shims` becomes a
  snapshot under `test/snapshots/go_exact` or `test/snapshots/go_normalized`.
- Add the snapshot entry to `test/integration/10_codegen_snapshots.sh` or the
  future snapshot manifest.

After parity is green, delete `test/integration/14_stdlib_shims.sh` and remove
the `stdlib` / `stdlib-shims` selector branch, or repoint `stdlib` to
`make integration std`.

Focused verification:

```bash
make integration std/bytes std/path
make integration std/file
make integration std/dir
make integration std/io
make integration snapshots
```

### Phase 5: Migrate `13_ffi.sh`

Current fixture-backed tests already point at:

- `test/fixtures/ffi/shim_runtime/main.mr`
- `test/fixtures/ffi/shim_function_values/main.mr`

Convert their shell output assertions into normal fixture annotations and
`std.io.puts`.

Move rejection tests into fixtures:

- bytes cannot be constructed -> `test/fixtures/ffi/ffi001_bytes_opaque_constructor_rejected.mr`
- bytes cannot expose fields -> `test/fixtures/ffi/ffi002_bytes_opaque_fields_rejected.mr`
- test shim root rejected -> `test/fixtures/ffi/ffi003_test_shim_root_rejected.mr`
- direct Go package extern ids rejected -> `test/fixtures/ffi/ffi004_direct_go_package_rejected.mr`

Keep the Go tree snapshot in snapshot ownership:

- `test/snapshots/go_tree/ffi_shim_runtime.tree`
- `test/integration/10_codegen_snapshots.sh` or snapshot manifest

After parity is green, delete or reduce `test/integration/13_ffi.sh`.

Focused verification:

```bash
make integration ffi
make integration snapshots
```

### Phase 6: Split `11_pre_modules_hardening.sh`

Classify each test:

- Normal reject/run behavior -> fixtures under the appropriate existing group.
- Mixed-feature runtime canary -> `test/fixtures/codegen_canary` only if it
  still catches cross-feature runtime behavior not covered elsewhere.
- Generated Go structural checks -> snapshots.
- Obsolete duplicate checks -> delete with an audit note in the commit message.

Do not keep bespoke shell structural assertions after an equivalent snapshot
exists.

Focused verification:

```bash
make integration vnext_canary codegen_canary cross_feature
make integration snapshots
```

### Phase 7: Replace `puts` In Std Fixtures

This phase should be small after the migration because new std fixtures should
already use `std.io.puts`.

Required checks:

```bash
rg -n '\bputs\(' test/fixtures/std
```

must return no matches.

Focused verification:

```bash
make integration std
```

### Phase 8: Replace `puts` Across All Fixtures

Mechanical migration by group. Prefer small commits by fixture family.

Default rewrite:

```marmoset
puts(value)
```

to:

```marmoset
import std.io

try io.puts(value)
```

Rules:

1. Add `import std.io` once per fixture when needed.
2. Preserve exact output annotations.
3. For fixtures intentionally testing purity/effectfulness, update expected
   effect annotations deliberately. `io.puts` is effectful and returns
   `Result[Unit, io.Error]`.
4. For fixtures where adding stdlib dependencies would obscure the language
   behavior, first consider changing the test to build-only or moving the
   observable behavior to a more focused fixture.
5. Keep a short allowlist only if the team explicitly decides a `puts` runtime
   canary is still needed.

Verification:

```bash
rg -n '\bputs\(' test/fixtures
make integration <changed-groups>
```

### Phase 9: Audit Stdlib-vs-Language Ownership

Audit all fixture groups for tests whose real assertion is now stdlib API
behavior.

Move to `test/fixtures/std` when the test primarily checks:

- `Option` helper API
- `Result` helper API
- `std.error` rendering/context/frame behavior
- `std.bytes`, `std.path`, `std.io`, `std.file`, or `std.dir`
- examples of public stdlib style

Keep in language/compiler groups when the test primarily checks:

- parser syntax
- resolver/module visibility
- type inference and diagnostics
- trait/impl search
- effect inference and purity
- lowering/codegen runtime behavior independent of stdlib API
- regression cases tied to compiler internals

Add `test/fixtures/README.md` if it does not exist, documenting this ownership
rule so new tests do not drift back.

Focused verification:

```bash
make integration std result purity runtime
```

### Phase 10: Audit Codegen Fixture Families

Families:

- `test/fixtures/codegen`
- `test/fixtures/codegen_canary`
- `test/fixtures/codegen_mono`
- `test/fixtures/codegen_stress`

For each fixture, assign one disposition:

1. Keep as runtime codegen regression.
2. Convert to snapshot if the assertion is generated Go shape.
3. Move to a language feature group if it is not really codegen-specific.
4. Delete as duplicate after identifying the smaller fixture or snapshot that
   covers the same behavior.

Specific expectations:

- `codegen_mono` should keep tests that prove monomorphization/specialization
  behavior across multiple concrete instantiations.
- `codegen_stress` should not be a permanent bucket for every generated
  combination. Keep only high-value regression/stress cases with unique failure
  modes.
- `codegen_canary` should contain a very small number of mixed-feature canaries.
  Large canaries that only protect emitted shape should become snapshots.
- Any fixture preserved only because of a previous bug should be named or
  commented as a regression, not as a generic stress sample.

Focused verification:

```bash
make integration codegen codegen_mono codegen_stress codegen_canary
make integration snapshots
```

### Phase 11: Audit Module Fixture Families

Families:

- `test/fixtures/modules`
- `test/fixtures/modules_edge`
- `test/fixtures/modules_codegen`
- `test/fixtures/modules_codegen_edge`

For each fixture, assign one disposition:

1. Keep as module resolver/visibility/import semantics.
2. Convert to snapshot if the assertion is generated namespace/name shape.
3. Move to another language group if the module structure is incidental.
4. Delete as duplicate after identifying equivalent smaller coverage.

Specific expectations:

- `modules` should focus on successful import, alias, namespace, and type
  position semantics.
- `modules_edge` should focus on diagnostics, privacy, ambiguity, shadowing,
  transitive visibility, and import errors.
- `modules_codegen` and `modules_codegen_edge` should shrink if snapshots cover
  emitted naming/escaping behavior.
- If multi-module project fixture discovery becomes general in Phase 1, revisit
  the current `modules*` special case and either generalize it or document why it
  remains.

Focused verification:

```bash
make integration modules modules_edge modules_codegen modules_codegen_edge
make integration snapshots
```

### Phase 12: Remove Obsolete Shell Helpers And Selectors

After the migration:

- Delete suite selectors for removed shell suites from `test/integration.sh`.
- Remove bespoke helpers in `test/integration/common.sh` that no longer have
  callers.
- Keep snapshot helpers until a dedicated snapshot manifest runner replaces
  them.
- Update `Makefile` comments if selector behavior changes.

Required checks:

```bash
rg -n '12_prelude|13_ffi|14_stdlib_shims|run_source_expect|run_project_expect|run_fixture_expect' test
rg -n '\bputs\(' test/fixtures
```

## Testing Strategy

Harness changes must be test-first through `09_harness_canaries.sh`.

Focused commands by phase:

```bash
make integration harness
make integration std
make integration std/file
make integration std/io
make integration ffi
make integration snapshots
make integration codegen codegen_mono codegen_stress codegen_canary
make integration modules modules_edge modules_codegen modules_codegen_edge
```

Do not rely on a full integration run during each phase. Run focused suites for
the changed area, then ask for full final verification when the plan is done.

Minimum final verification before closing the plan:

```bash
make unit
make integration std ffi snapshots
make integration codegen codegen_mono codegen_stress codegen_canary
make integration modules modules_edge modules_codegen modules_codegen_edge
make integration harness cli
./test/ci/quality.sh
```

## Commit Plan

1. Harness canaries for cwd, sandbox, stdin, runtime status, stderr, nested
   selectors, and project entrypoints.
2. Harness implementation for fixture project semantics.
3. Initial `test/fixtures/std` layout and std fixture README.
4. Migrate prelude/Option/Result/error tests from `12_prelude.sh`.
5. Migrate bytes/path/file/dir/io tests from `14_stdlib_shims.sh`.
6. Move std/generated-Go shape checks to snapshots.
7. Migrate FFI shell assertions from `13_ffi.sh`.
8. Split `11_pre_modules_hardening.sh` into fixtures and snapshots.
9. Replace `puts` in std fixtures.
10. Replace `puts` across all remaining fixture groups in small family-based
    commits.
11. Audit and prune codegen fixture families.
12. Audit and prune module fixture families.
13. Remove obsolete shell helpers/selectors and update fixture documentation.

## Risks And Constraints

- Replacing bare `puts` with `std.io.puts` makes many run fixtures depend on stdlib,
  `Result`, FFI shims, and top-level `try`. This is intentional for user-facing
  style, but the migration must keep enough low-level unit/snapshot coverage to
  diagnose compiler failures without assuming stdlib is correct.
- Sandboxed fixture diagnostics will mention temp paths unless path normalization
  maps them back to original fixture paths.
- Recursive fixture discovery can accidentally run helper modules as tests.
  Project entrypoint rules must be implemented before adding multi-file fixture
  projects outside `modules*`.
- Stderr ordering can be flaky if stdout and stderr are merged. Capture streams
  separately and compare them separately.
- Some codegen stress fixtures may be redundant but still encode old bug
  history. Delete only after identifying the replacement coverage in the audit
  note or commit message.
- If `docs/plans/todo/tooling/01_ocaml-test-harness.md` lands first, this plan's
  harness semantics must be implemented in the OCaml runner instead of adding
  more bash-only complexity.

## Open Questions

1. Should `puts` keep exactly one end-to-end fixture as a runtime primitive
   canary, or should it move entirely to unit-level/runtime tests?
2. Should snapshot tests stay as shell wrappers around helper functions, or do
   we want a declarative snapshot manifest as part of this cleanup?
3. Should fixture metadata remain comment-based, or should project fixtures grow
   a small sidecar file if metadata expands further?

## Progress

- 2026-05-14 21:55 CEST: Draft plan created after inspecting the current
  fixture runner, integration shell suites, stdlib test coverage, and existing
  tooling plans.
- 2026-05-14 22:17 CEST: Updated fixture migration direction after
  `docs/plans/todo/language/11_io_output_names.md`: canonical line output is
  `std.io.puts`; `std.io.print` is reserved for no-newline `Show` rendering.
