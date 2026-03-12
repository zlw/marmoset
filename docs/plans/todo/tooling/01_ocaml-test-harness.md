# Rewrite Test Harness Runner in OCaml

## Context

The current integration harness is a bash runner (`test/integration.sh`) that executes fixture-based tests under `test/fixtures/` and supports selector-based invocation (`make integration`, `make integration traits`, `make integration traits/t36_foo.mr`, `make integration cli`).

The current harness behavior is not just "run/reject/build". It also enforces semantics that we must preserve:

- Annotation placement validation (fixtures fail when annotations are detached or malformed).
- Strict reject diagnostics by default:
  - expected diagnostics must be present,
  - line-bound diagnostics (when compiler emits source span for fixture file) must match annotated source line,
  - unexpected diagnostics fail the test (two-way strictness).
- Monkey fixture drift guard (`runtime/monkey_example.mr` must stay in sync with `examples/monkey.mr` after stripping `# output:` comments).
- Deterministic output order in reporting even with parallel execution.

The rewrite target is performance and maintainability, without losing correctness guarantees added during parity hardening.

## Goals

1. Faster end-to-end fixture execution by replacing bash process orchestration with OCaml.
2. Preserve fixture format (`.mr` with `# output:`, `# error:`, `# warning:`, `# info:`).
3. Preserve CLI contract and selector behavior of `make integration` targets.
4. Keep true e2e behavior: still execute `marmoset build` as subprocess.
5. Preserve all current strictness semantics (annotation placement + strict reject diagnostics).
6. Keep output deterministic and debuggable.
7. Add benchmark gates so performance claims are measured, not assumed.

## Non-Goals

- No compiler semantic changes.
- No fixture format redesign.
- No migration of `test/integration/08_cli.sh` to OCaml in this phase.
- No privileged-by-default system configuration (ramdisk must remain optional and safe to skip).
- No silent softening of test strictness.

---

## Compatibility Invariants (Must Hold)

The OCaml runner is only considered parity-safe if all invariants below hold:

1. `make integration` selector contract is unchanged:
   - no args => all fixture groups,
   - `cli`/`08_cli`/`08_cli.sh` => CLI suite,
   - exact group selectors,
   - prefix group selectors,
   - fixture file selectors (`*.mr`) resolved relative to fixture root, repo root, or absolute path.
2. Reject mode remains strict-by-default:
   - missing expected diagnostic => fail,
   - line-mismatched expected diagnostic (when source-located) => fail,
   - unexpected diagnostic => fail,
   - wildcard (`*`) semantics preserved.
3. Annotation placement validation remains enforced before execution.
4. Output mode remains exact stdout line equality.
5. Build-only mode remains "build must succeed".
6. `runtime/monkey_example.mr` drift guard remains enforced.
7. Reporting remains deterministic in fixture-order, not completion-order.
8. Make passthrough behavior still supports extra goals without make errors.

---

## Architecture

### Binary

New executable: `tools/test_runner/main.ml`.

Suggested dune stanza:

```dune
(executable
 (name main)
 (public_name marmoset-test-runner)
 (libraries unix str)
 (package marmoset))
```

The runner remains process-based (no direct dependency on compiler internals for test execution semantics).

### Module Layout

```text
tools/test_runner/
  main.ml          # entry point, CLI args/env
  fixture.ml       # fixture loading, annotation parsing, placement validation
  selector.ml      # selector parsing and fixture resolution
  diag.ml          # diagnostic line extraction and strict matching
  process.ml       # subprocess spawn/capture/timeout
  runner.ml        # mode dispatch: run/reject/build-only
  pool.ml          # worker pool (Domains-based)
  ramdisk.ml       # optional tmp root acceleration
  monkey_guard.ml  # monkey fixture drift check
  report.ml        # deterministic reporting and summary
```

`diag.ml` and `process.ml` are explicit modules to avoid overloading `runner.ml` with parsing/process details.

---

## Phase 0: Behavior Baseline and Canaries

Before implementing the OCaml runner, freeze behavior with executable parity canaries.

### 0.1 Baseline capture

Record baseline run characteristics using current bash runner:

```bash
make integration traits/t36_duplicate_impl_for_same_trait_and_type_fails.mr
make integration runtime/monkey_example.mr
make integration cli
```

Do not rely only on summary lines; capture failing diagnostic sections for strictness checks.

### 0.2 Strictness canaries (must fail under current behavior)

Codify canaries so regressions are caught immediately in the rewrite:

1. Move one expected diagnostic annotation to the wrong source line in a scratch copy -> must fail with line-mismatch.
2. Inject an extra unmatched diagnostic in compiler output fixture scenario -> must fail with unexpected diagnostics.
3. Detach an annotation from executable anchor -> must fail annotation placement.

The OCaml runner must reproduce these failures, not just the happy path pass counts.

### 0.3 Selector canaries

Verify all selector classes before parity sign-off:

```bash
make integration
make integration all
make integration traits
make integration traits t36_duplicate_impl_for_same_trait_and_type_fails.mr
make integration test/fixtures/traits/t36_duplicate_impl_for_same_trait_and_type_fails.mr
make integration cli
```

---

## Phase 1: Fixture Parser and Placement Validation (`fixture.ml`)

### 1.1 Data model

```ocaml
type diag_kind = Error | Warning | Info

type output_ann = {
  line : int;
  text : string;
}

type diag_ann = {
  line : int;
  kind : diag_kind;
  text : string;  (* "*" supported *)
}

type mode = Run | Reject | Build_only

type fixture = {
  abs_path : string;
  rel_path : string;    (* path under test/fixtures, with .mr *)
  group : string;
  name : string;        (* rel_path without .mr *)
  mode : mode;
  outputs : output_ann list;
  diags : diag_ann list;
}
```

### 1.2 Annotation parsing

Regex equivalent to bash:

- `^[^#]*#\s*(output|error|warning|info):\s*(.*)$`

Mode inference (must match current behavior):

1. Any diag annotation => `Reject`
2. Else any output annotation => `Run`
3. Else => `Build_only`

### 1.3 Placement validation (mandatory, not deferred)

Placement validation is required for parity with current harness.

Rules to preserve:

1. `error`/`warning`/`info` annotations must be inline with code intent (not detached comment headers).
2. `output` annotations must resolve to a following executable output anchor, skipping blank lines and other annotations.
3. If an intervening non-annotation comment detaches the annotation from code, fail.
4. If no anchor is found, fail.
5. If anchor exists but is not an allowed output anchor shape, fail.

Current anchor intent to preserve:

- `puts(...)`
- `match ...`
- `if ...`
- callable expressions likely producing output side effects in current rules

Implementation note:

- Keep parser + validator outputs structured (`Placement_error of { line; message }`) so report formatting remains stable.

---

## Phase 2: Subprocess and Execution Engine (`process.ml`, `runner.ml`, `diag.ml`)

### 2.1 Subprocess API (replace `open_process_full` sketch)

Use explicit argv spawning (`Unix.create_process` / `Unix.create_process_env`) with pipe management.

Requirements:

1. No shell interpolation for compiler command.
2. Capture stdout and stderr separately.
3. Drain pipes safely (avoid deadlock on large output).
4. Return exit status, captured streams, and wall-clock duration.
5. Support timeout guard (`MARMOSET_TEST_TIMEOUT_SEC`, default unset/no timeout).

Example process result:

```ocaml
type process_result = {
  exit_code : int;
  stdout : string;
  stderr : string;
  duration_ms : int;
  timed_out : bool;
}
```

### 2.2 Build/run behavior

Build command parity:

```text
marmoset build <fixture_abs_path> -o <worker_out>/<fixture_name>
```

Mode behavior:

- `Run`
1. Build must succeed.
2. Execute produced binary.
3. Compare runtime stdout exactly against expected outputs joined with `\n` in annotation order.
4. Fail with concise diff context on mismatch.

- `Reject`
1. Build must fail.
2. If all expected diagnostic texts are `*`, pass immediately.
3. Otherwise enforce strict diagnostics (Section 2.3).

- `Build_only`
1. Build must succeed.

### 2.3 Strict reject diagnostics (mandatory)

This is a parity-critical area and must not be deferred.

Diagnostic handling pipeline:

1. Extract candidate diagnostic lines from build output using parity-compatible patterns.
2. Filter known non-diagnostic noise lines (for example `Error: Go build failed`) exactly as current harness does.
3. Evaluate in this order:
   - Missing expected diagnostic fragments.
   - Line-bound mismatch checks.
   - Unexpected diagnostic lines.

#### 2.3.1 Missing expected diagnostics

For each non-wildcard expected diagnostic text:

- must appear as fixed-string substring in build output.
- if absent => fail with `missing expected diagnostics` and include annotated line metadata.

#### 2.3.2 Line-bound strictness

For each expected non-wildcard diagnostic:

1. Inspect extracted diagnostics that contain the expected text.
2. If any such line is source-located to the fixture path (`<fixture>:<line>:<col>...`), enforce line equality with annotation line.
3. If fixture-path line-bound diagnostics exist but none match annotated line => fail with `line-mismatched diagnostics`.
4. If diagnostics containing expected text are unbound (no fixture path), do not force mismatch failure for that expectation.

This reproduces current behavior where line strictness is enforced when source location is present.

#### 2.3.3 Unexpected diagnostics (two-way strictness)

For each extracted diagnostic line:

- mark covered if any expected diagnostic fragment matches (fixed-string) or wildcard exists,
- if uncovered => fail with `unexpected diagnostics`.

### 2.4 Diagnostic extractor contract (`diag.ml`)

Define a single extractor function used by reject-mode logic:

```ocaml
val extract_diagnostic_lines : build_output:string -> string list
```

Contract:

- preserve ordering from compiler output,
- include error/warning/info lines matching current harness intent,
- exclude known non-diagnostic noise lines.

### 2.5 Path normalization for line checks

Line-bound matching must be robust across path forms:

- absolute fixture path,
- relative fixture path,
- normalized path separators.

Normalize to canonical absolute path before line comparison.

---

## Phase 3: Optional Ramdisk Strategy (`ramdisk.ml`)

Ramdisk must be optional and safe. Performance optimization cannot become an execution precondition.

### 3.1 Default behavior

Default: use normal temp directory (no privileged mount attempts).

### 3.2 Opt-in behavior

Enable fast tmp root only when explicitly requested:

- `MARMOSET_RAMDISK=1`

Suggested policy:

1. Try unprivileged fast paths first.
2. If unavailable, emit one warning and fall back silently to normal tempdir.
3. Never fail the suite solely due to ramdisk setup failure.

### 3.3 macOS strategy

Opt-in command path:

- `hdiutil attach -nomount ram://<sectors>`
- `diskutil eraseVolume HFS+ MarmosetTests <device>`

Track both mount path and backing device for teardown.

### 3.4 Linux strategy

Prefer non-privileged writable tmpfs-backed locations first:

1. use `/dev/shm` subdir if writable,
2. else fall back to standard tempdir.

Only attempt explicit `mount -t tmpfs ...` behind an additional opt-in flag (for controlled environments), for example:

- `MARMOSET_RAMDISK_ALLOW_MOUNT=1`

### 3.5 Sizing and worker directories

Avoid hard-coded tiny sizes.

- default size target should account for workers and binary churn,
- create isolated per-worker subdirs (`worker_<n>`),
- clean per-test artifacts after completion,
- teardown best-effort at suite end.

---

## Phase 4: Worker Pool and Concurrency (`pool.ml`)

### 4.1 Concurrency model

Use a bounded worker pool with `Domain.spawn` workers and atomic task index.

```ocaml
val run_pool : num_workers:int -> tasks:(unit -> 'a) array -> 'a array
```

Recommended approach:

- tasks stored in array,
- shared atomic next-index counter,
- each worker fetches index via `Atomic.fetch_and_add`,
- write result into pre-sized result array at same index.

This avoids CAS on entire task list and reduces contention.

### 4.2 Worker count detection and validation

`MARMOSET_FIXTURE_JOBS` parsing must be defensive:

1. if env var exists and valid integer >= 1 => use it,
2. else fallback to `Domain.recommended_domain_count ()`,
3. clamp to fixture count,
4. fallback to `1` if detection fails.

### 4.3 Realistic performance model

The workload is largely external-process bound (`marmoset build` + downstream compiler work), not OCaml CPU-bound.

Domains still help with orchestration overhead and predictable scheduling, but expected gains should be treated as moderate unless measured.

### 4.4 Output determinism

Buffer per-fixture result payload and print in input order after all workers complete (or flush in-order as contiguous prefixes become ready).

Do not print completion-order logs.

---

## Phase 5: Selector Logic and Make Contract (`selector.ml`, `Makefile`)

### 5.1 Selector precedence and matching

Preserve current resolution order exactly:

1. no args => all groups,
2. `all` => all groups,
3. `cli` / `08_cli` / `08_cli.sh` => CLI suite,
4. exact group match,
5. prefix group match (`<prefix>_...`),
6. `.mr` fixture file selector (fixture root, repo root, absolute path).

Exact group match must be attempted before prefix match.

### 5.2 File selector normalization

Fixture file selectors must:

- resolve to absolute canonical path,
- verify path stays under fixture root,
- deduplicate while preserving user argument order.

### 5.3 Make passthrough contract (critical)

When switching `integration` to OCaml runner, keep dummy-goal passthrough block so commands like below remain valid and do not trigger "No rule to make target":

```bash
make integration traits
make integration traits/t36_duplicate_impl_for_same_trait_and_type_fails.mr
make integration traits runtime/monkey_example.mr
```

Required Makefile shape:

```makefile
integration: $(RUNNER)
	@$(RUNNER) $(filter-out integration,$(MAKECMDGOALS))

ifneq (,$(filter integration,$(MAKECMDGOALS)))
$(filter-out integration,$(MAKECMDGOALS)):
	@:
endif
```

### 5.4 CLI suite delegation

If CLI suite selected, execute `test/integration/08_cli.sh` as subprocess and integrate pass/fail into overall run status.

---

## Phase 6: Monkey Drift Guard (`monkey_guard.ml`)

Special-case parity requirement for `runtime/monkey_example.mr`:

1. Strip `# output:` suffix comments from fixture source.
2. Compare normalized fixture text with `examples/monkey.mr`.
3. On mismatch, fail with concise diff snippet and remediation hint.

This must run before fixture execution mode dispatch for that fixture.

---

## Phase 7: Reporting (`report.ml`)

### 7.1 Per-test line format

Preserve familiar shape:

```text
TEST [42] codegen/cg01_top_level_function ... ✓ PASS
TEST [43] traits/t36_duplicate_impl ... ✗ FAIL (missing expected diagnostics)
```

### 7.2 Failure detail sections

For failures, include focused sections based on failure kind:

- `annotation placement`
- `fixture drift`
- `build failed`
- `output mismatch`
- `missing expected diagnostics`
- `line-mismatched diagnostics`
- `unexpected diagnostics`

Truncate long payloads with clear note (for example first N lines plus "...truncated...").

### 7.3 Suite summary

```text
==========================================
RESULTS: <pass> passed, <fail> failed out of <total> tests
==========================================
```

Exit code:

- `0` if no failures,
- `1` otherwise.

---

## Phase 8: Integration and Migration Strategy

### Step 1: Introduce OCaml runner alongside bash

Add a side-by-side target first:

```makefile
integration-ocaml: $(RUNNER)
	@$(RUNNER) $(filter-out integration-ocaml,$(MAKECMDGOALS))

ifneq (,$(filter integration-ocaml,$(MAKECMDGOALS)))
$(filter-out integration-ocaml,$(MAKECMDGOALS)):
	@:
endif
```

Keep existing `integration` target pointing to bash until parity + benchmark gates pass.

### Step 2: Parity verification

For a representative selector matrix, compare bash vs OCaml results:

- pass/fail outcome for each fixture,
- failure kinds (`missing`, `line-mismatch`, `unexpected`),
- selector resolution behavior,
- CLI delegation behavior.

Include negative canaries from Phase 0.

### Step 3: Performance verification

Run benchmark protocol (Section "Performance Benchmark Protocol") and capture raw timings.

### Step 4: Switch default

Only after parity and performance gates pass:

- point `make integration` to OCaml runner,
- keep legacy bash runner accessible as `integration-legacy` for one transition window.

### Step 5: Remove legacy harness

After one release cycle (or explicit sign-off window):

- remove `test/integration.sh`,
- keep `test/integration/08_cli.sh`,
- keep minimal `test/integration/common.sh` for CLI suite helpers (`suite_begin`, `suite_end`, `ensure_built`).

---

## Environment Variables

| Variable | Default | Purpose |
|---|---|---|
| `MARMOSET_FIXTURE_JOBS` | auto | Worker count override |
| `MARMOSET_EXECUTABLE` | `_build/default/bin/main.exe` | Compiler path override |
| `MARMOSET_SKIP_BUILD` | unset | Skip shared build bootstrap |
| `MARMOSET_TEST_TIMEOUT_SEC` | unset | Optional timeout per subprocess |
| `MARMOSET_RAMDISK` | unset | Opt-in temporary root acceleration |
| `MARMOSET_RAMDISK_ALLOW_MOUNT` | unset | Opt-in privileged mount attempts where supported |

---

## Performance Benchmark Protocol

Performance claims must be measured against same commit and same machine class.

### Protocol

1. Warm build once (`dune build` / equivalent).
2. Use fixed `MARMOSET_FIXTURE_JOBS` across compared runs.
3. Run each command at least 3 times.
4. Compare median wall-clock time.

### Commands

```bash
time make integration

time make integration-ocaml

time make integration-ocaml traits

time make integration-ocaml runtime/monkey_example.mr
```

### Acceptance gate

- No parity regressions.
- Measurable speedup in median full-suite runtime.
- If speedup is negligible, keep OCaml runner only if maintenance benefits justify complexity.

Do not assert specific multipliers without measured data.

---

## Risk Register

1. **Strictness regression risk (P0)**
   - Mitigation: Phase 0 canaries + mandatory strict reject logic in Phase 2.
2. **Placement validation regression risk (P0)**
   - Mitigation: validator implemented in Phase 1, not deferred.
3. **Make target contract breakage (P1)**
   - Mitigation: preserve passthrough dummy-goal blocks for `integration` and `integration-ocaml`.
4. **Ramdisk portability risk (P1)**
   - Mitigation: opt-in only; fallback is always supported path.
5. **Subprocess deadlock/timeout risk (P1)**
   - Mitigation: explicit pipe draining and timeout support.
6. **Overstated speedup risk (P2)**
   - Mitigation: benchmark gate with recorded medians before default switch.

---

## Files to Create

| File | Purpose |
|---|---|
| `tools/test_runner/dune` | runner build config |
| `tools/test_runner/main.ml` | CLI entrypoint |
| `tools/test_runner/fixture.ml` | parser + placement validation |
| `tools/test_runner/selector.ml` | selector resolution |
| `tools/test_runner/diag.ml` | diagnostic extraction + matching helpers |
| `tools/test_runner/process.ml` | subprocess spawn/capture |
| `tools/test_runner/runner.ml` | mode execution orchestration |
| `tools/test_runner/pool.ml` | worker pool |
| `tools/test_runner/ramdisk.ml` | optional temp acceleration |
| `tools/test_runner/monkey_guard.ml` | monkey drift verification |
| `tools/test_runner/report.ml` | output rendering |

## Files to Modify

| File | Change |
|---|---|
| `Makefile` | add `integration-ocaml`; later switch `integration` target; preserve passthrough blocks |
| `test/integration/common.sh` | only during legacy removal phase (trim to CLI needs) |

## Files to Delete (After Final Sign-Off)

| File | Condition |
|---|---|
| `test/integration.sh` | parity + benchmark gates passed, legacy window complete |

---

## Implementation Checklist (Small-Commit Execution Plan)

This section translates the architecture into an execution sequence that follows `CLAUDE.md` commit constraints:

1. small commits (one logical change per commit),
2. explicit testing in each commit message,
3. no accidental inclusion of local-review/progress/artifact files,
4. no broad staging (`git add .` / `git add -A` are prohibited).

### Commit hygiene rules (applies to every commit)

1. Stage files explicitly by path (`git add <file1> <file2> ...`).
2. Never stage these paths:
   - `docs/review/`
   - temporary local progress artifacts
   - binaries, build outputs, or `/tmp` files
3. Keep each commit focused on exactly one behavior slice.
4. Commit message body must include:
   - what changed,
   - why,
   - how verified (commands + result).
5. If a commit changes behavior, include tests in the same commit (red->green in that slice).

### Test cadence policy during implementation

Because full integration is slow, use sparse full-suite runs and frequent targeted checks.

1. After parser/selector/strictness milestones: run focused subsets.
2. After runner end-to-end milestone: run medium matrix.
3. Before default switch: run full suite once for parity.
4. Before legacy removal: run full suite once again after switch.

Suggested commands by cadence stage:

```bash
# fast focused checks
make integration-ocaml traits
make integration-ocaml runtime/monkey_example.mr
make integration-ocaml codegen/cg01_top_level_function.mr
make integration-ocaml cli

# matrix checks
make integration-ocaml traits runtime codegen

# sparse full checks (milestone only)
make integration-ocaml
make integration
```

### Commit sequence

Each item is one intended commit.

#### C01: Scaffold OCaml runner target without behavior switch

- Scope:
  - create `tools/test_runner/dune`
  - create minimal `tools/test_runner/main.ml` printing usage/placeholder
  - add `integration-ocaml` target + passthrough dummy-goal block in `Makefile`
- Verify:
  - `dune build tools/test_runner/main.exe`
  - `make integration-ocaml` exits non-zero with clear placeholder/usage, but make target resolution works.
- Commit outcome:
  - plumbing only, no test semantics yet.

#### C02: Implement selector discovery + resolution parity

- Scope:
  - add `selector.ml` with exact order: no args/all/cli/exact/prefix/file
  - path normalization and fixture-root containment checks
  - dedup preserving argument order
- Verify:
  - selector unit tests in OCaml (pure functions)
  - manual smoke:
    - `make integration-ocaml traits`
    - `make integration-ocaml traits/t36_duplicate_impl_for_same_trait_and_type_fails.mr`
    - `make integration-ocaml cli`
- Commit outcome:
  - selection behavior parity isolated and testable.

#### C03: Fixture parser (annotation extraction + mode inference)

- Scope:
  - add `fixture.ml` parsing output/diag annotations and line numbers
  - implement mode inference parity (`Reject` > `Run` > `Build_only`)
- Verify:
  - OCaml unit tests for parsing and inference.
- Commit outcome:
  - parser parity without placement enforcement yet.

#### C04: Placement validator parity

- Scope:
  - implement mandatory placement validation in `fixture.ml`
  - encode explicit error variants/messages used by reporting
- Verify:
  - OCaml unit tests:
    - detached annotation fails,
    - non-anchor fails,
    - missing anchor fails,
    - valid anchor passes.
- Commit outcome:
  - P0 placement behavior preserved in isolation.

#### C05: Subprocess layer (argv-safe spawn, capture, timeout)

- Scope:
  - add `process.ml`
  - implement compiler/binary subprocess execution with split stdout/stderr
  - timeout support (`MARMOSET_TEST_TIMEOUT_SEC`)
- Verify:
  - process-level unit tests or harnessed tests for exit code and stream capture.
- Commit outcome:
  - robust process foundation before mode logic.

#### C06: Runner build-only and run modes

- Scope:
  - add initial `runner.ml` with:
    - `Build_only` execution
    - `Run` build+execute+exact stdout comparison
- Verify:
  - focused fixtures:
    - `make integration-ocaml runtime`
    - one known build-only subset.
- Commit outcome:
  - happy-path runtime semantics available.

#### C07: Diagnostic extraction module

- Scope:
  - add `diag.ml` candidate-line extractor + noise filter parity
  - include path normalization helper for line-bound matching
- Verify:
  - unit tests on synthetic build output samples.
- Commit outcome:
  - strict reject machinery primitives done.

#### C08: Reject mode strictness (missing + line-mismatch + unexpected)

- Scope:
  - wire `Reject` mode in `runner.ml` using `diag.ml`
  - enforce full strictness (including two-way unexpected diagnostics)
- Verify:
  - focused reject-heavy subsets:
    - `make integration-ocaml traits`
    - `make integration-ocaml symbols`
  - strictness canary checks from Phase 0.
- Commit outcome:
  - P0 strict reject behavior parity implemented.

#### C09: Monkey drift guard

- Scope:
  - add `monkey_guard.ml`
  - integrate pre-dispatch check for `runtime/monkey_example.mr`
- Verify:
  - `make integration-ocaml runtime/monkey_example.mr`
  - temporary drift injection in scratch copy demonstrates failure (do not commit modified fixture).
- Commit outcome:
  - guard parity restored.

#### C10: Reporting module and deterministic ordering

- Scope:
  - add `report.ml`
  - stable per-test line formatting + failure sections + suite summary
- Verify:
  - run mixed subset twice and confirm identical output order.
- Commit outcome:
  - deterministic, debuggable logs parity.

#### C11: Worker pool implementation

- Scope:
  - add `pool.ml` with bounded workers and atomic index fetch
  - `MARMOSET_FIXTURE_JOBS` parsing + clamping
- Verify:
  - same subset with jobs 1 and jobs N:
    - `MARMOSET_FIXTURE_JOBS=1 make integration-ocaml traits`
    - `MARMOSET_FIXTURE_JOBS=8 make integration-ocaml traits`
  - compare pass/fail equivalence.
- Commit outcome:
  - parallel execution without ordering regressions.

#### C12: Optional ramdisk support (opt-in only)

- Scope:
  - add `ramdisk.ml`
  - support `MARMOSET_RAMDISK=1` flow with safe fallback
  - Linux mount path gated by `MARMOSET_RAMDISK_ALLOW_MOUNT=1`
- Verify:
  - default run path unchanged when env unset,
  - opt-in path logs warning and falls back when unavailable.
- Commit outcome:
  - P1 portability risk mitigated.

#### C13: Full runner wiring in `main.ml`

- Scope:
  - integrate selector + parser + validator + runner + pool + report
  - CLI delegation to `test/integration/08_cli.sh`
- Verify:
  - selector matrix smoke:
    - `make integration-ocaml`
    - `make integration-ocaml traits`
    - `make integration-ocaml traits runtime/monkey_example.mr`
    - `make integration-ocaml cli`
- Commit outcome:
  - full OCaml runner operational side-by-side.

#### C14: Parity canary commit (tests only, no behavior changes)

- Scope:
  - add reproducible parity script/checklist in docs or test helper script
  - include strictness and selector canaries as runnable steps
- Verify:
  - run canary checklist and capture outcomes.
- Commit outcome:
  - repeatable parity validation artifact.

#### C15: Side-by-side parity run and bugfixes (iterative, small commits)

- Scope:
  - run bash vs OCaml across selected groups
  - fix one behavioral discrepancy per commit
- Verify:
  - targeted subset proving each fix.
- Commit outcome:
  - converged semantics before default switch.

Note: C15 is intentionally iterative and may expand into C15a/C15b/... commits; keep each fix isolated.

#### C16: Benchmark capture commit

- Scope:
  - run benchmark protocol and record results in plan-adjacent perf note
  - no behavior changes
- Verify:
  - 3-run medians captured for baseline and ocaml runner.
- Commit outcome:
  - objective performance evidence for switch decision.

#### C17: Switch `make integration` default to OCaml runner

- Scope:
  - change `Makefile` `integration` target to OCaml runner
  - keep passthrough block intact
  - keep `integration-legacy` alias to bash runner
- Verify:
  - `make integration` selector matrix
  - one full run (`make integration`) at milestone.
- Commit outcome:
  - default switched with rollback path retained.

#### C18: Legacy observation window fixes (if needed)

- Scope:
  - only regression fixes discovered post-switch
- Verify:
  - targeted failing selectors + one aggregate check.
- Commit outcome:
  - stabilize post-switch behavior.

#### C19: Remove legacy fixture runner

- Scope:
  - delete `test/integration.sh`
  - trim `test/integration/common.sh` to CLI helpers only
- Verify:
  - `make integration`
  - `make integration cli`
  - one final full run at milestone.
- Commit outcome:
  - legacy removal complete after sign-off.

### Commit message template (recommended)

```text
<Short one-line summary>

What:
- ...

Why:
- ...

Testing:
- <command>  # pass/fail summary
- <command>  # pass/fail summary

Changes:
- <file>: <what changed>
- <file>: <what changed>
```

### Staging checklist before each commit

Run this checklist every time:

1. `git status --short`
2. Confirm only intended files are staged.
3. Confirm no forbidden paths are staged (`docs/review/`, local progress artifacts, binaries, `/tmp`).
4. Re-run the smallest meaningful verification command for this commit.
5. Commit.

---

## Definition of Done

1. `make integration-ocaml` supports full selector matrix identical to current runner.
2. Strict reject behavior is equivalent (missing, line-mismatch, unexpected diagnostics all enforced).
3. Annotation placement validation is enforced equivalently.
4. Monkey drift guard behavior is preserved.
5. Report ordering and summary format remain deterministic and readable.
6. CLI suite delegation works via `08_cli.sh`.
7. Benchmark report exists with raw timings and medians.
8. `make integration` switches to OCaml runner only after parity + benchmark gates pass.
9. Legacy runner removal happens only after transition window sign-off.
