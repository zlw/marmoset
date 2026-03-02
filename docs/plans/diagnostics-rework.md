# Diagnostics Rework Plan (LSP-First, Minimal CLI Rendering)

## Summary
Stabilize diagnostics across parser, typechecker, and codegen so diagnostics are structured, span-accurate, and reliable for LSP consumption.

CLI output is part of the migration, but pretty console presentation is not a goal for this version; a minimal deterministic rendering is sufficient.

This plan explicitly targets the **current** test setup:
- fixture-driven integration via `test/integration.sh` and `test/fixtures/**/*.mr`,
- strict reject semantics (missing expected diagnostics, line-mismatch, unexpected diagnostics),
- annotation placement guardrails and monkey fixture drift guard.

The diagnostics payload (codes/spans/severity) becomes compiler-grade in this version. CLI presentation stays minimal; fixture harness will be adapted to stable output invariants rather than legacy text shape.

## Goals
- Canonical internal diagnostics model shared by syntax/typecheck/codegen/CLI/LSP.
- Uniform frontend error convention: every module in the frontend produces `Diagnostic.t` directly. No bespoke error types per module, no boundary conversion layers.
- Minimal deterministic CLI diagnostics rendering sufficient for developer workflows and tests.
- Consistent error formatting between `marmoset check` and `marmoset build` (currently divergent).
- Stable descriptive diagnostic codes (for example `type-mismatch`, `parse-expected-token`).
- Span-accurate diagnostics carrying `file_id` + byte offsets.
- Keep bail-on-first behavior for now (single emitted diagnostic per failed compilation stage).
- Preserve fixture strictness semantics by updating `test/integration.sh` extraction/matching to the new format.
- Migration order is explicit and enforced: boundary adoption first, internal module convergence second, legacy error-type removal before this rework is considered done.

## Non-Goals
- Multi-error recovery/accumulation.
- Fancy terminal color engine or rich TUI renderer.
- External JSON diagnostics API in this phase.
- Fixture annotation format redesign (`# error:`, `# warning:`, `# info:`, `# output:` stays).
- Codegen/build diagnostics in LSP (LSP only invokes parser + typechecker; it never touches the Go emitter).

## Locked Decisions
- Internal canonical diagnostics core in `lib/diagnostics/`.
- LSP-facing diagnostic stability is prioritized over console presentation.
- No legacy output compatibility requirement for CLI formatting.
- `marmoset build` and `marmoset check` both use the **same** shared diagnostics renderer path (fixing the current divergence where `check` uses `format_error_with_context` with caret markers and `build` uses plain `Error: <msg>`).
- Severity model is `Error | Warning | Info` from day one, even if most emissions are errors.
- Bail-on-first remains across parser/typecheck/codegen in this phase.
- Diagnostic codes are descriptive strings (for example `type-unbound-var`), not numeric.
- Harness adapts to diagnostics; diagnostics do not adapt to harness legacy regexes.
- Harness adaptation lands **before** compiler output changes, not after, so `make integration` stays green throughout the migration.
- Temporary compatibility shims are allowed only during migration and must be removed before rollout completion.

## Current-State Problems
- Parser returns `string list` errors instead of structured diagnostics.
- Parser errors often lack proper source range in CLI output.
- Typechecker has location metadata internally, but user-facing flow is still string-first.
- Frontend error conventions are inconsistent across modules:
  - `annotation.ml` uses `failwith` and `exception Open_row_rejected` (raw exceptions, not result types).
  - `trait_solver.ml` returns `(unit, string) result` with bracket-tagged messages like `[missing-field]`.
  - `infer.ml` uses structured `infer_error` with `error_kind` variants.
  - `checker.ml` wraps everything into its own `error` record.
  - These are all different conventions in the same compiler frontend.
- `marmoset check` and `marmoset build` use different error formatting paths for the same underlying errors.
- Codegen errors are flat `failwith` strings with no source location — cannot be traced back to Marmoset source.
- Build errors (Go compile failures) are opaque external-tool stderr, not structured diagnostics.
- LSP currently fabricates parse diagnostics at zero-range because parser does not provide spans.
- `test/integration.sh` strict diagnostic extraction is coupled to current flat line patterns, not diagnostic blocks.

## Existing Infrastructure to Reuse
- `lib/frontend/typecheck/source_loc.ml`
  - `offset_to_loc`, `to_string`, `to_string_range`, `format_with_context`, `format_with_context_range`.
- `lib/frontend/typecheck/infer.ml`
  - Error sites already have access to `pos`, `end_pos`, `file_id` from AST nodes — these become span data in `Diagnostic.t`. The `error_kind`/`infer_error` types themselves are replaced.
- `lib/frontend/typecheck/checker.ml`
  - `format_loc_prefix` and `format_error_with_context` logic informs the diagnostics renderer. The `checker.error` record itself is replaced.
- Trait-solver error classes already surfaced in messages:
  - `[missing-field]`, `[field-type-mismatch]`, `[impl-resolution]`, etc. — these become diagnostic codes directly.

---

## Target Diagnostic Contract

### Internal Representation
Create `Diagnostics.Diagnostic` as canonical payload:

```ocaml
type severity = Error | Warning | Info

type span =
  | NoSpan
  | Span of { file_id : string; start_pos : int; end_pos : int option }

type label = {
  span : span;
  message : string option;
  primary : bool;
}

type t = {
  code : string;              (* type-mismatch *)
  severity : severity;        (* Error/Warning/Info *)
  message : string;           (* short headline *)
  labels : label list;        (* at most one primary for now *)
  notes : string list;        (* optional contextual notes *)
}
```

### CLI Rendering (Minimal This Version, Library-Driven)
Rendering is delegated to a diagnostics rendering library; this phase does not lock a custom house style.

For this version, use the library's plain/standard profile (no bespoke fancy styling). Richer console presentation is a future-version concern.

Stable invariants we require from CLI output:
- Each diagnostic includes a visible severity and stable code.
- Each diagnostic includes the headline message.
- If a primary span exists, output includes file + line/column location data.
- Output ordering is deterministic for a fixed input.
- File id is rendered from diagnostic payload (no hidden path rewriting in the renderer layer).

Stable header line contract for harness parsing (exactly three forms, no ambiguity):
- Range span: `<file>:<start_line>:<start_col>-<end_line>:<end_col>: <severity> <code>: <message>`
- Point span: `<file>:<line>:<col>: <severity> <code>: <message>` (when `end_pos` is absent)
- No span: `<severity> <code>: <message>`

`file_id` is non-optional in `Span` — if you have an offset, you have a file. `NoSpan` covers locationless errors (codegen, build). This eliminates the ambiguous "offset-but-no-file" case and keeps the header format to exactly three forms.

`file_id` policy for call sites:
- Compiler/LSP parse/typecheck entrypoints require a `file_id` argument.
- If a true path/URI is unknown (for example REPL/scratch input), callers must provide a synthetic stable identifier (for example `<repl>` or `<stdin>`), not `None`.

`severity` is a separate field (`error`/`warning`/`info`), and `code` is unprefixed lowercase kebab-case.
The header line is canonical; any extra context/snippet lines printed by the renderer are non-canonical and ignored by the harness.

The exact visual layout (indentation, pointers, separators, snippet framing) is renderer-library-defined and intentionally not part of this phase contract.

### Compatibility Stance
- Do not preserve old `Error: Type error: ...` format.
- Do not keep old parser `Parse error: ...` text contract.
- Existing tests/harness must be migrated to parse stable diagnostics invariants from the new output.

---

## Implementation Plan

### Phase 0: Baseline and Harness Canaries (Current Setup First)
Before changing compiler output, lock expected strictness behavior in the current fixture world:
- Confirm `make integration` uses `test/integration.sh` (already true).
- Add/update canary checks in harness tests (or documented manual canaries) for:
  - missing expected diagnostic,
  - line-mismatched expected diagnostic,
  - unexpected diagnostic,
  - annotation placement failure,
  - monkey fixture drift failure.

Deliverable:
- Documented baseline from `test/integration.sh` behavior that must remain semantically equivalent after migration.

### Phase 1: Shared Diagnostics Library
Create:
- `lib/diagnostics/dune`
- `lib/diagnostics/source_loc.ml` (moved from `typecheck/`)
- `lib/diagnostics/diagnostic.ml`
- `lib/diagnostics/diagnostic.mli`

Dune graph target:

```text
syntax      -> diagnostics
typecheck   -> syntax, diagnostics
codegen     -> syntax, typecheck, diagnostics
marmoset    -> syntax, typecheck, codegen, diagnostics
```

Add helpers:
- constructors (`error_with_span`, `error_no_span`, `with_note`, `with_secondary_label`),
- offset-to-line/column conversion helpers,
- renderer adapter (`render_cli`) that maps `Diagnostic.t` into the selected rendering library profile,
- text output helper for multiple diagnostics (`render_many_cli`) for future multi-diagnostic support.

### Phase 2: Adapt Harness to Support New Diagnostic Format

**Rationale:** The harness must understand the new diagnostic format *before* any compiler output changes, so `make integration` stays green throughout the entire migration. Adapting the harness last (as originally planned) would leave integration broken across Phases 3–6.

Update fixture harness diagnostics extraction and strictness matching to parse canonical header lines **in addition to** legacy regex lines. The harness accepts both formats during migration; legacy support is removed after all compiler layers emit structured diagnostics.

#### 2.1 Keep Fixture Semantics Unchanged
Unchanged behavior:
- fixture mode inference (`run`, `reject`, `build-only`),
- annotation placement validation,
- monkey fixture drift guard,
- wildcard `# error: *` behavior,
- two-way strictness intent.

#### 2.2 Add Invariant-Oriented Extractor (Alongside Legacy)
Add a new extraction path in `reject_mode` that runs alongside the legacy regex extractor:
- detect diagnostics using the stable header format (`...: <severity> <code>: <message>`),
- extract location from supported span forms (`file:line:col` point spans and `file:line:col-line:col` range spans),
- tolerate renderer-specific context/snippet formatting differences,
- produce structured tuples per diagnostic:
  - severity
  - code
  - headline message
  - file (optional)
  - start_line/start_col/end_line/end_col (optional)
  - raw block text (for debugging)
- **Dual-mode logic:** run both new and legacy extractors during migration and merge as a multiset. Perform deterministic pair-cancel deduplication **only across extractors** (one `new` + one `legacy` entry with the same normalized severity/code/message/location tuple cancels once). Do not dedupe within a single extractor result set. This supports mixed-format output while preserving real duplicate-emission signals.
- **Cardinality rule:** the extractor is designed for `0..N` diagnostics per run from day one (block-oriented parsing, matching, and strictness checks). Current compiler behavior is still bail-on-first in this phase, but harness logic must not assume single-diagnostic output.

#### 2.3 Preserve Strict Reject Semantics Under Both Formats
For each reject fixture:
1. Missing expected diagnostic fragments:
   - non-wildcard fixture expectations must match emitted diagnostic blocks (by substring over normalized textual form).
   - matching is one-to-one (a single emitted diagnostic cannot satisfy multiple expected annotations unless explicitly duplicated in output).
2. Line-bound strictness:
   - when matching diagnostics include location bound to fixture file, enforce annotation line equality.
   - mismatches fail with `line-mismatched diagnostics`.
3. Unexpected diagnostics (two-way strictness):
   - any emitted diagnostic block not covered by an expected fragment/wildcard fails.
   - strictness is count-sensitive: duplicate emitted diagnostics require duplicate expectations (unless wildcard is present), and duplicate expectations require duplicate emitted diagnostics.
4. Parser robustness guard:
   - if build fails and harness cannot parse any diagnostics from output, fail with explicit extractor failure instead of silently passing.

#### 2.4 Harness Output (Failure Readability)
On strictness failures, print:
- fixture expectation line,
- parsed diagnostics summary (`severity`, `code`, `file:line`, message),
- first N lines of raw output for debugging.

### Phase 3: Parser Diagnostics (Structured, Fail-Fast)
Refactor `lib/frontend/syntax/parser.ml`:
- Replace `errors : string list` with `diagnostics : Diagnostic.t list`.
- Convert parse errors (`peek_error`, `no_prefix_parse_fn_error`, explicit `add_error` branches) into coded diagnostics.
- Attach span from parser token positions and parser `file_id`.
- Keep fail-fast result flow; return single-item diagnostic list for now.

API change:

```ocaml
val parse : file_id:string -> string -> (AST.program, Diagnostic.t list) result
```

If callers do not have a real filesystem path/URI, they pass a synthetic stable id (for example `<repl>` / `<stdin>`).

Parser code families:
- `parse-expected-token`
- `parse-unexpected-token`
- `parse-invalid-number`
- `parse-invalid-record`
- `parse-invalid-impl`
- `parse-invalid-pattern`

### Phase 4: Typecheck/Infer Diagnostic Mapping
Refactor:
- `lib/frontend/typecheck/infer.ml`
- `lib/frontend/typecheck/checker.ml`
- `lib/frontend/typecheck/annotation.ml`
- `lib/frontend/typecheck/trait_solver.ml`

#### 4.0 Ordered Execution for Phase 4
Execute this phase in two ordered passes, then a cleanup gate:
1. **4A Boundary pass (stabilize callers first):**
   - `checker.ml` public APIs move to `Diagnostic.t`/`Diagnostic.t list`.
   - Parser/checker/CLI/LSP call paths can consume canonical diagnostics without waiting for all internal helper migrations.
   - Local compatibility shims are allowed inside `typecheck/` in this pass only.
2. **4B Internal convergence pass:**
   - Migrate `infer.ml`, `unify.ml`, `annotation.ml`, and `trait_solver.ml` to `Diagnostic.t` at error origin.
   - Remove string-tag and module-specific error transport inside frontend modules.
3. **4C Cleanup gate (required to close this phase):**
   - Delete compatibility shims and legacy error types.
   - No remaining `infer_error` / `error_kind` / `unify_error` / `checker.error` used in non-test frontend code.

#### 4.1 Infer and Unify Errors (Phase 4B)
Replace `error_kind`, `infer_error`, `unify_error`, and `checker.error` with direct `Diagnostic.t` construction at the error site. Each error site already has access to `file_id`, `pos`, `end_pos` from the AST — attach spans there, not in a later conversion layer. Remove `checker.error` record and `error_of_infer_error` conversion function entirely.

Complete `error_kind` mapping:
- `UnboundVariable` -> `type-unbound-var`
- `UnificationError TypeMismatch` -> `type-mismatch`
- `UnificationError OccursCheck` -> `type-occurs-check`
- `InvalidOperator` -> `type-invalid-operator`
- `NonFunctionCall` -> `type-non-function`
- `IfConditionNotBool` -> `type-if-condition`
- `IfBranchMismatch` -> `type-if-branch-mismatch`
- `IfExpressionWithoutElse` -> `type-if-no-else`
- `ReturnTypeMismatch` -> `type-return-mismatch`
- `PurityViolation` -> `type-purity`
- `ConstructorError` -> `type-constructor`
- `PatternError` -> `type-pattern`
- `MatchError` -> `type-match`
- `ArrayElementMismatch` -> `type-array-element`
- `HashKeyMismatch` -> `type-hash-key`
- `HashValueMismatch` -> `type-hash-value`
- `NotIndexable` -> `type-not-indexable`
- `IndexTypeMismatch` -> `type-index-mismatch`
- `EmptyArrayUnknownType` -> `type-empty-array`
- `EmptyHashUnknownType` -> `type-empty-hash`

#### 4.2 Annotation Validation Errors (Phase 4B)
`annotation.ml` currently uses `failwith` and `exception Open_row_rejected` — raw exceptions that bypass the result-type error path entirely. These must produce `Diagnostic.t` directly:
- Replace `failwith` calls with `Error (Diagnostic.t)` returns.
- Replace `Open_row_rejected` exception with a result-type error path returning `Diagnostic.t`.
- Codes: `type-annotation-mismatch`, `type-annotation-invalid`, `type-open-row-rejected`.

#### 4.3 Trait Solver Errors (Phase 4B)
`trait_solver.ml` currently returns `(unit, string) result` with bracket-tagged messages (`[missing-field]`, `[field-type-mismatch]`, etc.) that get threaded through `infer.ml` as `ConstructorError` strings. This string-embedding pattern must be eliminated:
- Return `Diagnostic.t` directly from trait solver error paths.
- Stop embedding error class information as bracket tags inside message strings — the diagnostic code carries the classification.
- Codes:
  - `[missing-field]` -> `type-trait-missing-field`
  - `[field-type-mismatch]` -> `type-trait-type-mismatch`
  - `[missing-impl]` -> `type-trait-missing-impl`
  - `[impl-resolution]` -> `type-trait-impl-resolution`
  - `[non-record-for-field-trait]` -> `type-trait-non-record`
  - `[impl-specialization]` -> `type-trait-specialization`
  - `[generic-impl-constraint]` -> `type-trait-generic-constraint`
  - `[unknown-trait]` -> `type-trait-unknown`

### Phase 5: Codegen + Build Boundary Diagnostics
Refactor:
- `lib/backend/go/emitter.ml`
- `bin/main.ml`

Changes:
- Keep internal codegen exceptions for now.
- At `compile_string`/`compile_to_build` boundaries, convert exceptions/failures into `Diagnostic.t`.
- Promote build-tool failures to coded diagnostics.
- **Signature changes:** `compile_string` changes from `(string, string) result` to `(string, Diagnostic.t) result`. `compile_to_build` changes from `(build_output, string) result` to `(build_output, Diagnostic.t) result`. Callers in `bin/main.ml` must be updated in lockstep.

**Important limitations:**
- Codegen errors are `NoSpan` — the emitter does not track source positions, so these diagnostics cannot be traced back to specific Marmoset source locations. The diagnostic code and message are the only useful information for the user.
- Go build errors are opaque pass-through from external tooling (`go build` stderr). The `build-go-compile` diagnostic wraps the raw Go error message; it is not a structured Marmoset diagnostic.
- These diagnostics are **CLI-only**. The LSP server never invokes the codegen/build pipeline (it stops at parser + typechecker), so codegen/build diagnostic codes are irrelevant to LSP.

Code families:
- `codegen-unresolved-tvar`
- `codegen-ambiguous-fn`
- `codegen-internal`
- `build-go-compile` (opaque Go error pass-through)
- `build-go-missing`

### Phase 6: CLI Wiring (Build + Check)
Update CLI paths in `bin/main.ml`:
- **Unify error rendering:** Both `build` and `check` must use the same diagnostics renderer path. Currently `check` calls `format_error_with_context` (source line + caret markers) while `build` uses plain `Error: <msg>`. This divergence is eliminated — all compiler errors render through the canonical renderer regardless of subcommand.
- Render diagnostics for parser/typecheck/codegen/build failures via the selected library profile.
- Ensure deterministic ordering (single diagnostic now; list ordering later if expanded).
- Keep CLI scope minimal in this phase; do not block this rework on pretty-print UX work.

No CLI fallback to legacy format.

### Phase 7: Remove Harness Legacy Extractor
After all compiler layers emit structured diagnostics:
- Remove the legacy regex extraction path from `test/integration.sh`.
- Verify all fixtures pass using only the invariant-oriented extractor.
- Full `make integration` green with new-format-only harness.

### Phase 8: LSP Integration on Canonical Diagnostics
Refactor `tools/lsp/lib/doc_state.ml` and related adapters:
- consume `Diagnostic.t` directly from parser/checker.
- thread document identity into analysis: plumb LSP `uri`/path into `Doc_state.analyze`, pass it as `file_id` to parser/typechecker entrypoints, and preserve it through diagnostic spans.
- map to LSP diagnostics using existing location conversion utilities.
- map span semantics consistently:
  - range span -> exact LSP range,
  - point span (end absent) -> 1-character LSP range,
  - no span -> zero/default range.
- stop generating zero-range parser diagnostics except when no span exists.
- LSP scope is **parser + typechecker only** — no codegen/build diagnostics.

---

## Public Interface Changes

### New
- `Diagnostics` shared library.
- `Diagnostics.Diagnostic` canonical payload + renderer.
- `Diagnostics.Source_loc` module.

### Updated
- `Syntax.Parser.parse` returns `Diagnostic.t list` on error.
- `Typecheck.Checker` APIs return `Diagnostic.t` directly (no `checker.error` wrapper).
- `Codegen.Go_emitter.compile_string` returns `(string, Diagnostic.t) result`. `compile_to_build` returns `(build_output, Diagnostic.t) result`.
- CLI build/check render diagnostics via canonical renderer.

---

## Diagnostic Code Taxonomy (Initial)

### Parser (`parse-*`)
- `parse-expected-token`
- `parse-unexpected-token`
- `parse-invalid-number`
- `parse-invalid-record`
- `parse-invalid-impl`
- `parse-invalid-pattern`

### Typechecker — Inference (`type-*`)
- `type-unbound-var`
- `type-mismatch`
- `type-occurs-check`
- `type-invalid-operator`
- `type-non-function`
- `type-if-branch-mismatch`
- `type-if-condition`
- `type-if-no-else`
- `type-return-mismatch`
- `type-purity`
- `type-constructor`
- `type-pattern`
- `type-match`
- `type-array-element`
- `type-hash-key`
- `type-hash-value`
- `type-not-indexable`
- `type-index-mismatch`
- `type-empty-array`
- `type-empty-hash`

### Typechecker — Annotations (`type-annotation-*` and related)
- `type-annotation-mismatch`
- `type-annotation-invalid`
- `type-open-row-rejected`

### Typechecker — Traits (`type-trait-*`)
- `type-trait-missing-field`
- `type-trait-type-mismatch`
- `type-trait-missing-impl`
- `type-trait-impl-resolution`
- `type-trait-non-record`
- `type-trait-specialization`
- `type-trait-generic-constraint`
- `type-trait-unknown`

### Codegen/Build (`codegen-*`, `build-*`) — CLI only, no LSP
- `codegen-unresolved-tvar`
- `codegen-ambiguous-fn`
- `codegen-internal`
- `build-go-compile` (opaque Go error pass-through)
- `build-go-missing`

### Future
- Additional warning/info codes as needed (still unprefixed; severity is carried separately).

---

## Test Plan

### Unit Tests
- Diagnostics constructors populate severity/code/message/span correctly.
- Renderer output is deterministic for same diagnostic input and selected library profile.
- `Source_loc` migration keeps all existing behavior.
- Parser emits coded diagnostic with span and `file_id`.
- All typechecker error sites (infer, unify, annotation, trait solver) produce `Diagnostic.t` directly with correct codes and spans.
- No remaining `error_kind`/`infer_error`/`checker.error` types — replaced by `Diagnostic.t`.
- annotation.ml: no `failwith` or `Open_row_rejected` for expected errors.
- trait_solver.ml: no bracket-tagged string errors.
- Codegen/build boundary converts exceptions to coded diagnostics (all `NoSpan`).

### Integration Tests (Compiler + Harness)
- `make integration` remains green **at every phase boundary** (not just at the end).
- Dual-mode harness (Phase 2) ensures old-format output is still matched during compiler migration.
- `test/integration.sh` reject strictness still enforces:
  - missing expected diagnostics,
  - line mismatches,
  - unexpected diagnostics.
- Annotation placement strictness remains enforced before execution.
- Monkey fixture drift check remains enforced.

### Harness-Specific Canary Matrix
Run (or codify) canaries that must fail when strictness regresses:
1. Move expected `# error:` annotation to wrong source line -> `line-mismatched diagnostics`.
2. Introduce extra emitted diagnostic not covered by expectations -> `unexpected diagnostics`.
3. Remove expected diagnostic fragment from compiler output (or expectation mismatch) -> `missing expected diagnostics`.
4. Introduce detached annotation -> `annotation placement` failure.
5. Break monkey parity -> `fixture drift` failure.

### CLI Coverage
- `marmoset check <file>` reports production diagnostics and exits non-zero on error.
- `marmoset build <file>` reports production diagnostics and exits non-zero on error.
- Valid `build`/`run` behavior unchanged for success paths.

### LSP Coverage
- LSP diagnostics carry real document-backed `file_id` when spans exist (no implicit zero-range fallback due to missing file identity).
- For non-file-backed documents, LSP threads a synthetic stable `file_id` through parse/typecheck.

---

## Rollout and Commit Strategy
1. Add diagnostics library and move `source_loc` (no external behavior change yet).
2. **Adapt harness first:** add dual-mode extractor to `test/integration.sh` that recognizes both legacy and new diagnostic formats. `make integration` green — old compiler output still matches via legacy path.
3. Parser emits `Diagnostic.t` — harness picks up new format automatically. Re-baseline any fixture `# error:` fragments whose message text changed in the same commit. `make integration` green.
4. **Typechecker boundary pass (Phase 4A):** checker-facing APIs emit `Diagnostic.t` to callers; temporary internal shims are allowed while `infer`/`unify`/`annotation`/`trait_solver` are still migrating. Re-baseline affected fixture fragments in the same commit. `make integration` green.
5. **Typechecker internal convergence (Phase 4B + 4C):** migrate `infer`/`unify`/`annotation`/`trait_solver` to `Diagnostic.t` at source and remove legacy types/shims (`infer_error`, `error_kind`, `unify_error`, `checker.error`, bracket-tag string transport, expected-path exceptions). Re-baseline affected fixture fragments in the same commit. `make integration` green.
6. Codegen/build boundaries emit `Diagnostic.t`. Re-baseline affected fixture fragments in the same commit. `make integration` green.
7. CLI unifies `build`/`check` rendering through canonical renderer. Re-baseline affected fixture fragments in the same commit. `make integration` green.
8. Remove legacy extractor from harness. `make integration` green — new-format only.
9. Update LSP to consume canonical diagnostics directly.

**Invariant:** `make integration` stays green at every numbered step. Fixture re-baselining happens in the same commit as the compiler change that alters message text — never as a separate batch step.
**Completion gate:** the rework is not complete until Step 5 deletes frontend legacy error types/shims and frontend modules use `Diagnostic.t` internally.

---

## Frontend Error Convention

**Convention:** Every module in the compiler frontend produces `Diagnostic.t` directly. No bespoke error types per module, no boundary conversion layers.

The previous codebase had four different error representations in the same frontend (string lists, `infer_error`, `checker.error`, `(unit, string) result` with bracket tags). This rework eliminates all of them in favor of one: `Diagnostic.t`.

This convention applies to all modules under `lib/frontend/` and is established by this rework:

| Module | Current Error Style | Target Error Style |
|--------|--------------------|--------------------|
| `syntax/parser.ml` | `string list` | `Diagnostic.t list` |
| `typecheck/infer.ml` | `infer_error` (structured, has spans) | `Diagnostic.t` directly |
| `typecheck/checker.ml` | `checker.error` record | `Diagnostic.t` (no wrapper) |
| `typecheck/annotation.ml` | `failwith` + `exception Open_row_rejected` | `Diagnostic.t` via result type |
| `typecheck/trait_solver.ml` | `(unit, string) result` with bracket tags | `Diagnostic.t` directly |
| `typecheck/unify.ml` | `unify_error` variant | `Diagnostic.t` directly |

**Enforcement order:**
1. Boundary adoption to unblock callers/harness (`checker` outward).
2. Internal migration module-by-module (`infer`/`unify`/`annotation`/`trait_solver`).
3. Mandatory cleanup: remove temporary shims and legacy error types before closing the plan.

**Rules:**
1. No `failwith` or `raise` for expected error conditions (type errors, parse errors, validation failures). Exceptions are reserved for internal invariant violations (bugs).
2. Error channels use `(_, Diagnostic.t) result` or `(_, Diagnostic.t list) result`. No module-specific error types.
3. Every error carries a diagnostic code — no anonymous string-only errors.
4. Span information is attached at the error origin (where AST nodes / tokens are available), not reconstructed at a later boundary.
5. Bracket-tagged string messages (`[missing-field]`, etc.) are eliminated. The diagnostic code carries the classification directly.
6. Compatibility shims are temporary and scoped to migration only; they must not remain after Phase 4 completion.

This convention is documented here as the canonical reference. New frontend modules must follow it.

---

## Risks and Mitigations

### Risk: Harness strictness regression during formatter switch
Mitigation:
- harness adaptation lands **before** compiler output changes (Phase 2),
- dual-mode extractor means old and new formats are both recognized during migration,
- fail fast when diagnostic parsing yields zero blocks on failed build output,
- keep strictness checks block-based and two-way,
- legacy extractor removal (Phase 7) only after all compiler layers confirmed emitting new format.

### Risk: Diagnostic code churn breaks fixture expectations
Mitigation:
- freeze code taxonomy early,
- use code + message-fragment matching in harness (not message-only),
- document code ownership and review policy.

### Risk: Location fidelity drift across parser/typechecker
Mitigation:
- unit tests for span conversion in each layer,
- integration tests asserting fixture line binding behavior.
- (Codegen errors are `NoSpan` by design — no location fidelity to maintain there.)

### Risk: LSP range regressions
Mitigation:
- map from canonical spans in one place,
- keep existing conversion helpers and add regression tests for parser spans.
- LSP scope is parser + typechecker only — codegen/build changes cannot affect LSP.

### Risk: annotation.ml / trait_solver.ml migration to Diagnostic.t
Mitigation:
- Migrate one module at a time with unit test coverage for each error path.
- annotation.ml: replace `failwith`/`Open_row_rejected` with `Error (Diagnostic.t)` returns. If callers currently catch these exceptions, convert the catch sites to result-type handling in the same commit.
- trait_solver.ml: replace `(unit, string) result` with `(unit, Diagnostic.t) result`. Callers in infer.ml that thread string errors through `ConstructorError` are updated to propagate `Diagnostic.t` directly.

---

## Assumptions
- Compiler and LSP remain OCaml/in-repo.
- No multi-error accumulation in this phase.
- No external JSON diagnostics API yet.
- Renderer library choice may evolve; harness parsing is anchored to invariants, not exact ASCII art layout.
- Existing fixture annotations remain the source of reject expectations.
- LSP only calls parser + typechecker (confirmed in code: `doc_state.ml` never imports or calls `Go_emitter`).
- Codegen errors cannot carry source locations without a much larger refactor of `emitter.ml` (4,800+ lines with no position tracking); this is out of scope.
