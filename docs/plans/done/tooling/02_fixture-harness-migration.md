# Test Harness Rework (Detailed)

## Maintenance

- Last verified: 2026-03-12
- Implementation status: Implemented
- Type: Historical implementation plan

## Context

The current integration harness has good coverage but too much mechanical variance:

- ~1,100 integration tests across **19** shell suites in `test/integration/` (`08_cli.sh` included)
- `common.sh` currently exposes **7** test helpers with overlapping behavior:
  - `expect_runtime_output`
  - `expect_build`
  - `test_case`
  - `run_emit_go_not_contains_from_stdin`
  - `run_codegen_deterministic_from_stdin`
  - `test_emit_go_contains`
  - `run_build_ok_not_contains_from_stdin`
- Most tests are heredoc blobs in shell, which are difficult to read, search, and diff.

Current measured helper usage in shell suites (excluding `common.sh` definitions):

| Helper | Count |
|---|---:|
| `expect_runtime_output` | 912 |
| `expect_build` | 196 |
| `test_case` | 57 |
| `run_emit_go_not_contains_from_stdin` | 27 |
| `run_codegen_deterministic_from_stdin` | 7 |
| `test_emit_go_contains` | 2 |
| `run_build_ok_not_contains_from_stdin` | 1 |

The diagnostics rework (`docs/plans/done/tooling/01_diagnostics-rework.md`) will change formatting and introduce stable codes (e.g. `E-TYPE-MISMATCH`), so the next harness should be resilient to both current and future diagnostic output.

## Primary Goals

1. Reduce public test-helper surface in `common.sh` to **2 canonical assertions**:
   - `expect_run`
   - `expect_reject`
   Plus orchestration helpers:
   - `suite_begin`
   - `suite_end`
   - `ensure_built`
2. Move tests from shell heredocs to **fixture files** for readability and diff quality.
3. Preserve or strengthen coverage. No intentional softening.
4. Standardize fixture extension to `.mr` (same as existing source convention).
5. Run legacy and new harnesses side-by-side during migration, then remove legacy only after parity sign-off.

## Non-Goals

- No test-behavior rewrite beyond migration mechanics.
- No bless mode in initial rollout.
- No migration of `08_cli.sh` (CLI protocol/subcommand checks stay shell).
- No immediate deletion of legacy harness while fixture migration is still in progress.

## Migration Invariants (No-Softening Contract)

The migration is complete only if all of these hold:

1. Every existing test intent is represented after migration.
2. Existing "must fail" assertions remain "must fail".
3. Existing broad failure assertions (`__ANY_ERROR__`) remain broad using an explicit fixture form (defined below), not dropped.
4. Output checks remain exact string comparisons (including newlines).
5. Determinism and Go-emission structure checks are preserved (moved to OCaml unit tests where appropriate).
6. Legacy and fixture harnesses must both be runnable during migration for side-by-side review.

---

## Phase 0: Baseline Snapshot (Safety Rail)

Before edits:

1. Capture baseline green status:
   ```bash
   make unit
   make integration
   ```
2. Snapshot helper usage counts (for migration bookkeeping).
3. Keep migration checklists per suite to ensure 1:1 movement from shell test case to fixture/unit test.

---

## Phase 1: Move Go-Inspection Tests from Shell to OCaml Unit Tests

These are structural codegen assertions that are better expressed directly against emitter output.

### Scope

Move **37 tests** currently using these shell helpers:

| Shell helper | Count | Target OCaml pattern |
|---|---:|---|
| `run_emit_go_not_contains_from_stdin` | 27 | `compile_to_build src -> string_not_contains main_go "..."` |
| `run_codegen_deterministic_from_stdin` | 7 | `is_deterministic src` |
| `test_emit_go_contains` | 2 | `compile_string src -> string_contains code "..."` |
| `run_build_ok_not_contains_from_stdin` | 1 | `compile_to_build src -> string_not_contains main_go "..."` |

### Step 1.1: Add local test helpers in `emitter.ml`

Add after `string_contains`:

```ocaml
let string_not_contains s substring =
  not (string_contains s substring)

let is_deterministic source =
  match (compile_to_build source, compile_to_build source) with
  | Ok a, Ok b -> a.main_go = b.main_go && a.runtime_go = b.runtime_go
  | _ -> false
```

### Step 1.2: Convert each shell assertion to `let%test`

Notes:

- Patterns like `func\(\) int64` in shell are escaped literals; convert to plain literal `func() int64`.
- Keep assertion strictness unchanged (or stronger).
- Preserve test intent in test names (may rename for clarity, but keep semantics).

### Step 1.3: Remove the 37 migrated shell tests

Affected suites:

- `06_codegen.sh` (23)
- `06_codegen_stress.sh` (6)
- `06_codegen_mono.sh` (2)
- `05_records.sh` (2)
- `08_cross_feature.sh` (2)
- `04_traits.sh` (1)
- `01_core_annotations.sh` (1)

### Step 1.4: Delete obsolete helper implementations from `common.sh`

Remove:

- `run_emit_go_not_contains_from_stdin`
- `test_emit_go_contains`
- `run_codegen_deterministic_from_stdin`
- `run_build_ok_not_contains_from_stdin`

### Verify

```bash
make unit
make integration
```

---

## Phase 2: Collapse `common.sh` to Canonical Helpers

### Desired end state

`common.sh` publicly exposes only:

- `suite_begin`
- `suite_end`
- `ensure_built`
- `expect_run`
- `expect_reject`

### Step 2.1: Introduce canonical helpers first (non-breaking)

Add:

- `expect_run NAME EXPECTED_STDOUT [SOURCE]`
- `expect_reject NAME EXPECTED_DIAG [SOURCE]`

Behavior contract:

- Both accept source either as 3rd arg or stdin.
- `expect_run`: build + execute + exact stdout equality.
- `expect_reject`: build must fail.
  - If `EXPECTED_DIAG="*"`: any failing build is accepted.
  - Else: failing build output must contain substring `EXPECTED_DIAG`.

`"*"` is the explicit replacement for historical `__ANY_ERROR__`.

### Step 2.2: Add temporary aliases and migrate callsites

Temporary compatibility wrappers:

- `expect_runtime_output -> expect_run`
- `expect_build` wrapper that maps:
  - reject mode -> `expect_reject`
  - success mode -> temporary pass-through behavior (until callers converted)
- `test_case` wrapper preserved until all uses are removed

Then migrate callsites in this order:

1. Replace all `expect_runtime_output` with `expect_run`.
2. Replace all reject-style `expect_build` with `expect_reject`.
3. Replace `test_case` callsites.

### Step 2.3: Convert `test_case` calls (57)

Current expansion:

- `test_case "Name" 'src' "true"` currently means "build succeeds".
- `test_case "Name" 'src' "false" "fragment"` means reject with fragment.

Target:

- Success branch: convert to `expect_run` with explicit expected stdout.
  - Use `""` only when program is silent.
  - If program prints, assert exact printed output.
- Failure branch: `expect_reject`.

Files:

- `01_core_annotations.sh`
- `02_unions.sh`
- `03_enums.sh`
- `04_traits.sh`

### Step 2.4: Convert compile-success `expect_build` calls explicitly (8)

Current compile-success forms in repo:

- 5 calls with explicit empty fragment `""`
- 3 calls with omitted fragment argument

Do **not** bulk-convert all of them to `expect_run "...\"\""` blindly.
Set expected stdout per test program:

- silent programs -> `""`
- programs with `puts` -> exact expected lines

This avoids accidental false failures and keeps/no-softening semantics explicit.

### Step 2.5: Replace `__ANY_ERROR__` sentinel (26 callsites)

During shell migration:

- Replace `__ANY_ERROR__` with `"*"` in `expect_reject`.
- Keep behavior identical: must fail, message unrestricted.

### Step 2.6: Remove compatibility wrappers

After all callers are moved:

- delete `expect_runtime_output`
- delete `expect_build`
- delete `test_case`

### Verify

```bash
make integration
```

---

## Phase 3: Introduce Fixture Model (`.mr`)

### Step 3.1: Fixture file format

Fixtures are plain Marmoset programs with inline comment annotations.
Files use **`.mr`** extension (not `.mar`).

Example run fixture:

```mr
let x = 42
puts(x)  # output: 42
puts("hello")  # output: hello
```

Example reject fixture:

```mr
let x: int = "hello"  # error: Cannot unify
```

Post-diagnostics-rework-compatible reject fixture:

```mr
let x: int = "hello"  # error: E-TYPE-MISMATCH
```

Wildcard reject fixture (parity with historical `__ANY_ERROR__`):

```mr
# error: *
let x = y
```

### Step 3.2: Annotation grammar and semantics

Supported annotations:

- `# output: <text>`
- `# error: <text-or-*>`
- `# warning: <text>`
- `# info: <text>`

Mode inference:

1. Any diagnostic annotation present (`error/warning/info`) -> reject mode
2. Else if any `# output:` present -> run mode
3. Else -> build-only mode (build must succeed)

Diagnostic matching semantics:

- Normal diagnostic annotation:
  - Substring match against diagnostic line text.
  - Prefer line-number-matched diagnostics.
- `# error: *`:
  - Means "at least one error diagnostic exists" (line-agnostic wildcard).
  - Intended as explicit replacement for broad reject assertions.
- Two-way strictness:
  - Expected diagnostic missing -> fail
  - Unexpected diagnostic present -> fail

Output semantics:

- Collect all `# output:` annotations in file order.
- Join with `\n` to expected stdout.
- Compare exact stdout string from executable.

### Step 3.3: Directory structure

Use feature-oriented layout:

```text
test/fixtures/
  annotations/
  purity/
  symbols/
  unions/
  enums/
  expressions/
  traits/
  operators/
  methods/
  records/
  runtime/
  cross_feature/
```

Script-to-directory mapping:

- `01_core_annotations.sh` -> `annotations/`
- `01_purity.sh` -> `purity/`
- `01_symbol.sh` -> `symbols/`
- `02_unions.sh` -> `unions/`
- `03_enums.sh` -> `enums/`
- `04_traits.sh` + `04_traits_field.sh` + `04_traits_impl.sh` -> `traits/`
- `04_traits_operator.sh` -> `operators/`
- `04_traits_inherent.sh` -> `methods/`
- `05_records.sh` + `05_records_stress.sh` -> `records/`
- `06_codegen*.sh` runtime behavior tests -> `expressions/`, `enums/`, `traits/`, `records/`, `runtime/` by feature
- `07_runtime.sh` + `07_runtime_output.sh` -> `runtime/`
- `08_cross_feature.sh` -> `cross_feature/`
- `08_cli.sh` remains shell (not fixtureized)

Naming rules:

- `descriptive_snake_case.mr`
- one scenario per file
- no numeric filename prefixes required

### Step 3.4: Implement fixture runner (`test/integration/99_fixtures.sh`)

Responsibilities:

1. Discover fixtures deterministically:
   - Use `find`, not `**/*.mr` globstar (bash 3.2 portability)
   - `find "$FIXTURE_ROOT" -type f -name '*.mr' | LC_ALL=C sort`
2. Parse annotations:
   - line number
   - annotation type/value
3. Execute fixture in inferred mode:
   - run mode: build + run
   - reject mode: build expecting failure
   - build-only mode: build expecting success
4. Report via suite-level counters (reuse `suite_begin`/`suite_end` conventions).

Important implementation detail:

- Build fixtures by their real path (not copied temp file) so diagnostics with file/line metadata map directly to fixture source.

Reject parsing strategy (current + future compatible):

1. Parse spanful diagnostics when present (`path:line:col...` formats).
2. Keep raw stderr text for substring fallback.
3. Match expected diagnostics first by line, then by wildcard fallback.
4. If future structured format adds `[E-...]`, substring matching still works with `# error: E-...`.

### Step 3.5: Incremental migration order

Start with smaller suites to validate runner behavior, then scale:

1. `07_runtime_output.sh` -> `runtime/` (small, output-only)
2. `01_purity.sh` -> `purity/` (reject-heavy)
3. `01_symbol.sh` -> `symbols/` (mixed modes)
4. `07_runtime.sh` -> `runtime/` (large output suite)
5. `01_core_annotations.sh` -> `annotations/`
6. `02_unions.sh` -> `unions/`
7. `03_enums.sh` -> `enums/`
8. `05_records*.sh` -> `records/`
9. `04_traits*` family -> `traits/`/`methods/`/`operators/`
10. `06_codegen*` runtime fixtures -> feature directories
11. `08_cross_feature.sh` -> `cross_feature/`

Per-suite checklist:

1. Extract each test into `.mr` fixture.
2. Encode assertions via annotations.
3. Run `make integration`.
4. Remove migrated shell cases.
5. Delete shell suite when emptied.

### Step 3.6: Defer bless mode

Still deferred. Manual review of fixture diffs is safer during initial migration.

---

## Phase 4: Shadow-Mode Coexistence and Parity Review

This phase is mandatory and happens before removing legacy harness code.

### Step 4.1: Keep both harnesses runnable in parallel

During migration and after final fixture extraction:

1. Keep legacy suites runnable (`test/integration.sh` over existing shell suites).
2. Keep fixture suites runnable (`test/integration/99_fixtures.sh`, or a dedicated fixture runner wrapper if added).
3. Do not delete legacy helpers/suites until parity sign-off is complete.

Implementation note:

- If needed for cleaner execution UX, add explicit Make targets:
  - `integration-legacy`
  - `integration-fixtures`
  so reviewers can run both independently and compare outcomes.

### Step 4.2: Side-by-side parity review procedure

Run both harnesses from a clean state and compare:

1. Pass/fail status (both must be green).
2. Coverage parity checklist (all legacy test intents mapped to fixture/unit-test counterparts).
3. Known wildcard reject cases (`*`) are intentionally preserved, not silently narrowed.
4. Performance/runtime delta is acceptable (informational, not a blocker unless extreme).

Record parity report in PR description (or adjacent migration notes) including:

- migrated suite list
- unmigrated suite list
- intentional semantic changes (should be empty or explicitly justified)
- residual risks

### Step 4.3: Two-model review gate before deletion

Before deleting any legacy harness pieces, require two independent model reviews of parity:

1. Model review A: migration correctness and no-softening compliance.
2. Model review B: independent re-review of the same diff/checklist.

Deletion of legacy harness code proceeds only if both reviews approve.

---

## Phase 5: Final Cleanup

1. `common.sh` contains only canonical helpers + suite orchestration.
2. `99_fixtures.sh` is the primary language-level integration runner.
3. Legacy shell suites removed once fully migrated (except `08_cli.sh`).
4. Any temporary migration aliases/sentinels removed, except stable `"*"` behavior in fixture + `expect_reject`.

---

## Files to Modify

| File | Planned changes |
|---|---|
| `lib/backend/go/emitter.ml` | Add `string_not_contains`, `is_deterministic`, and 37 migrated `let%test` assertions |
| `test/integration/common.sh` | Collapse helper surface to canonical helpers; remove deprecated codegen helpers; remove aliases after migration |
| `test/integration/01_core_annotations.sh` | Remove migrated codegen checks; replace `test_case`; fixture extraction |
| `test/integration/02_unions.sh` | Replace `test_case`; fixture extraction |
| `test/integration/03_enums.sh` | Replace `test_case`; fixture extraction |
| `test/integration/04_traits*.sh` | Helper migration + fixture extraction to `traits/`/`methods/`/`operators/` |
| `test/integration/05_records*.sh` | Remove moved codegen checks; fixture extraction |
| `test/integration/06_codegen*.sh` | Remove structural Go-inspection tests; migrate remaining runtime behavior to fixtures |
| `test/integration/07_runtime*.sh` | Fixture extraction |
| `test/integration/08_cross_feature.sh` | Remove moved deterministic checks; fixture extraction |
| `test/integration/99_fixtures.sh` (new) | Fixture discovery + parse + execution |
| `test/fixtures/**/*.mr` (new) | New fixture corpus |
| `Makefile` | Optional: add `integration-legacy`/`integration-fixtures` targets for side-by-side review ergonomics |

---

## Verification Matrix (Every Phase Boundary)

Always run:

```bash
make unit
make integration
```

Additional migration checks:

1. Helper count check in `common.sh` (only canonical helpers remain at end).
2. No remaining `test_case` usage.
3. No remaining deprecated codegen shell helpers.
4. No remaining `__ANY_ERROR__`; replaced by `"*"`/fixture wildcard.
5. Fixture discovery determinism validated by repeated runs (same order, same names).
6. Legacy harness run is green.
7. Fixture harness run is green.
8. Side-by-side parity checklist completed and attached.
9. Two independent model reviews completed before legacy deletion.

---

## Definition of Done

Done when all are true:

1. Language-level integration assertions live in `.mr` fixtures.
2. `common.sh` public test assertions are reduced to `expect_run` and `expect_reject`.
3. Go-structure inspection checks are unit tests in `emitter.ml` (not shell ripgrep).
4. `08_cli.sh` remains and passes.
5. Full test suite is green under `make unit && make integration`.
6. Legacy-vs-fixture parity report exists and is approved.
7. Two independent model reviews approved parity before legacy harness removal.
