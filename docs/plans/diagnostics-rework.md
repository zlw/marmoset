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


---

## Merged Refactoring Addendum

Source merged from `docs/plans/diagnostics-refactoring.md` to keep a single canonical diagnostics plan file without losing detail.

# Diagnostics Refactoring Plan (Final)

Date: 2026-03-02
Branch target: `diagnostics-rework`
Supersedes:
- `docs/plans/diagnostics-refactoring-codex.md`
- `docs/plans/diagnostics-refactoring-claude.md`

Inputs reviewed:
- `docs/plans/diagnostics-rework.md` (original migration plan)
- `docs/progress/diagnostics-rework.md` (implementation log)
- `docs/reviews/diagnostics-rework-claude.md`
- `docs/reviews/diagnostics-rework-codex.md`
- Current codebase state in `lib/`, `bin/`, `tools/lsp/`, `test/`

---

## 1) Final Review Summary

The diagnostics rework is mostly complete in architecture:
1. All major pipelines now produce or carry `Diagnostic.t`.
2. CLI has canonical renderer wiring.
3. LSP consumes canonical diagnostics.
4. Harness runs canonical-only extractor path.

However, there are still unresolved correctness and completion-gate gaps:
1. Harness strictness is still too permissive in key matching paths.
2. LSP still fail-opens to empty diagnostics on internal exceptions.
3. Internal frontend layers still contain exception/string transport debt.
4. Parser file identity policy is not fully enforced.
5. Progress doc still overstates completion in several areas.

This final plan closes those gaps with blocker-first execution and explicit phase gates.

---

## 2) Decision Record (Authoritative)

These decisions are locked for this plan:
1. Harness strictness gap is a **HIGH correctness bug**.
2. LSP fail-open behavior is **HIGH**.
3. Checker exception boundary issue is **MEDIUM** and must be fixed.
4. Utility dedup is important, but **not** before blocker fixes.
5. Full integration is **not required on every commit**; targeted tests are default.
6. Full integration is required at explicit closure gates before final completion claim.

---

## 3) Constraints and Working Rules

### 3.1 Process/commit rules
1. Small focused commits, one logical change each.
2. Explicit `git add <file>` only.
3. Do not commit `docs/progress/` or `docs/reviews/`.
4. Do not commit binaries/artifacts/temp files.

### 3.2 Test policy
1. Per commit: run targeted tests for touched scope.
2. Per phase: run phase gate test bundle.
3. Pre-final-closure gate: run full verification (`make unit && make integration`) when requested/approved for closure.

### 3.3 Doc policy
1. Progress log must be updated as issues are closed.
2. Wording must reflect current reality; no phase marked "done" while its completion gate is open.

---

## 4) Canonical Issue Ledger

This ledger deduplicates Claude and Codex findings and maps them to final phases.

### 4.1 Blockers

| ID | Severity | Issue | Primary evidence | Planned phase |
|---|---|---|---|---|
| HR-01 | High | Harness reject matching allows false passes (kind/wildcard/file/line semantics gaps) | `test/integration.sh:328,335,342,744,793,847` | F1 |
| HR-02 | High | LSP fail-opens to `[]` diagnostics on analyzer exception | `tools/lsp/lib/server.ml:57,61,68` | F2 |

### 4.2 Required follow-up

| ID | Severity | Issue | Primary evidence | Planned phase |
|---|---|---|---|---|
| MR-01 | Medium | Checker boundary catches narrowly and depends on message heuristics | `checker.ml:59-80` | F3/F4 |
| MR-02 | Medium | Parser `file_id` still optional, fallback `<unknown>` remains | `parser.ml:99,102,1494` | F3 |
| MR-03 | Medium | Canonical continuation parse can over-merge and is unbounded | `integration.sh:499,523,530` | F1 |
| MR-04 | Medium | Canonical header parsing is fragile for `:` in file ids | `integration.sh:445,458` | F1 |
| MR-05 | Medium | Multi-diagnostic surfaces still collapse to first diagnostic at boundaries | `checker.ml:36-39`, `emitter.ml:4889` | F6 |
| MR-06 | Medium | Internal exception/string transport debt remains in infer/annotation flows | `infer.ml`/`annotation.ml` failwith paths | F4 |

### 4.3 Cleanup and coverage

| ID | Severity | Issue | Primary evidence | Planned phase |
|---|---|---|---|---|
| LR-01 | Low | Canary stub injection lacks explicit verification guard | `09_harness_canaries.sh:112-119` | F1 |
| LR-02 | Low | Primary-span helper duplicated across modules | `checker.ml:20`, `doc_state.ml:41` | F6 |
| LR-03 | Low | Trait messages still carry legacy bracket tags | `trait_solver.ml:45,55,71...` | F5 |
| LR-04 | Low | Diagnostics renderer lacks explicit warning/info coverage | `diagnostic.ml` tests focus on error | F6 |
| LR-05 | Low | Harness dead state remains (`CH_MATCHED`) | `integration.sh:435,446,459,472` | F1 |

### 4.4 Documentation consistency

| ID | Severity | Issue | Planned phase |
|---|---|---|---|
| PD-01 | Medium | Progress metadata scope stale | F7 |
| PD-02 | Medium | Checklist incomplete versus phases actually executed | F7 |
| PD-03 | Medium | Over-strong wording around strictness/completion status | F7 |

### 4.5 Already closed (no action now)

1. LSP URI file identity threading is implemented (`af4984a`).
2. No-span continuation regression for TF17 is fixed (`8e47aaa`).
3. `r106` fixture anchor mismatch is fixed (`964c088`).
4. Legacy dual extractor removal already done (`5fcb7de`).

---

## 5) Plan-Completion Gaps Versus Original `diagnostics-rework.md`

These are explicit original-plan obligations still open:
1. Phase 4 internal convergence gate is not fully closed.
2. Frontend convention "no expected-path exceptions/string transport" is not fully achieved.
3. Mandatory `file_id` entrypoint policy is only partially applied.
4. Harness strictness caveats logged in progress are still unresolved.
5. Progress log/checklist wording does not accurately represent completion status.

This final plan treats these as completion blockers for the rework closure.

---

## 6) Execution Plan (Detailed)

## Phase F0: Baseline, Mapping, and Preflight

### Objective
Create a stable execution baseline and make issue closure trackable.

### Issues addressed
1. Planning hygiene prerequisite for all phases.

### Work items
1. Add issue-status table (`HR/MR/LR/PD`) in progress log with initial status `open`.
2. Record exact test command bundle for each phase in progress log.
3. Snapshot current test baseline commands/results before edits.

### Files in scope
1. `docs/progress/diagnostics-rework.md` (tracking only, uncommitted)

### Risks
1. Doc drift if not updated after each phase.

### Mitigation
1. Mandatory phase-end progress update with commands and results.

### Tests
```bash
bash -n test/integration.sh
dune build
```

### Done criteria
1. Every open issue has mapped phase + acceptance check.

---

## Phase F1: Harness Correctness and Robustness (Blocker)

### Objective
Close harness correctness gaps that can produce false-green results.

### Issues addressed
1. `HR-01`
2. `MR-03`
3. `MR-04`
4. `LR-01`
5. `LR-05`

### Detailed work

#### F1A: Severity-kind enforcement
1. Match expected annotation kind (`error|warning|info`) against parsed diagnostic severity.
2. Treat mismatch as missing expectation.
3. Add canary: expected `# warning:` must fail when only `error` emitted.

#### F1B: Line/file strictness hardening
1. Keep one-to-one matching.
2. Require same-file line binding for line-scoped expectations when file-bound diagnostics are present.
3. Avoid fallback selection that accepts unrelated match text when strict bound exists.

#### F1C: Wildcard semantics tightening
1. Preserve wildcard support.
2. Scope wildcard suppression to intended matching behavior; do not globally disable unexpected-diagnostic checks in mixed expectation fixtures.
3. Add canary: mixed wildcard + concrete should still fail on unrelated extras.

#### F1D: Canonical header parser robustness
1. Replace `([^:]+)` capture approach with colon-tolerant parse strategy (right-anchored numeric tail).
2. Add canary for Windows-like path/file-id containing `:`.

#### F1E: Continuation handling controls
1. Add continuation stop heuristic to reduce unrelated block merge.
2. Add cap on accumulated continuation lines/text.
3. Keep TF17 behavior supported.

#### F1F: Canary harness hardening
1. Verify stub injection succeeded (grep/assert on injected `EXECUTABLE=` line).
2. Hard-fail canary suite if injection fails.
3. Remove dead variable state if unused (`CH_MATCHED`).

### Files in scope
1. `test/integration.sh`
2. `test/integration/09_harness_canaries.sh`

### Commit slices
1. `test(harness): enforce severity-kind and line/file-bound strictness`
2. `test(harness): tighten wildcard semantics and add strictness canaries`
3. `test(harness): colon-safe header parsing and bounded continuation`
4. `test(harness): verify canary stub injection and remove dead state`

### Risks
1. Existing fixtures may fail due to previously masked mismatches.

### Mitigation
1. Rebaseline only when diagnostics are now more correct.
2. Keep fixture changes separate from harness logic commits.

### Tests
```bash
bash -n test/integration.sh
bash -n test/integration/09_harness_canaries.sh
make integration harness
make integration test/fixtures/records/r106_double_spread_rejected.mr
make integration test/fixtures/traits_field/tf17_constrained_generic_field_used_in_match_expression_bug_codeg.mr
```

### Done criteria
1. Severity mismatch canary is enforced.
2. Mixed wildcard canary behavior is strict and deterministic.
3. Colon-bearing header canary parses successfully.
4. No unbounded continuation accumulation path remains.

---

## Phase F2: LSP Fail-Open Elimination (Blocker)

### Objective
Surface internal analysis failures as diagnostics instead of returning empty results.

### Issues addressed
1. `HR-02`

### Detailed work
1. In LSP server exception path, create and publish an internal diagnostic (`lsp-internal` or canonical equivalent).
2. Preserve cache invalidation behavior on exception.
3. Add stderr logging with exception text (and backtrace if available).
4. Ensure this path does not crash normal diagnostic publication.

### Files in scope
1. `tools/lsp/lib/server.ml`
2. `tools/lsp/lib/doc_state.ml` (if helper extraction is needed)

### Commit slices
1. `lsp(server): emit internal diagnostic on analyzer exception`
2. `lsp(server): add exception logging for analysis failures`

### Risks
1. Potential noisy diagnostics if triggered repeatedly on edit loop.

### Mitigation
1. Keep message concise and stable.
2. Keep debounce behavior unchanged.

### Tests
```bash
dune runtest --force tools/lsp/lib
dune build ./tools/lsp/lib/marmoset_lsp.cma
```

### Done criteria
1. Exception path yields visible diagnostic (not `[]`).
2. Existing LSP tests remain green.

---

## Phase F3: Boundary Safety + Mandatory Parser Identity

### Objective
Harden checker boundary and enforce explicit parser identity at all callsites.

### Issues addressed
1. `MR-01` (boundary hardening part)
2. `MR-02`

### Detailed work

#### F3A: Checker catch-all boundary
1. Add catch-all exception mapping to `type-internal` diagnostic at checker top boundary points.
2. Keep existing specific handling where useful (`Open_row_rejected`, existing known families), but ensure unknown exception path is safe.
3. Preserve message clarity and deterministic output.

#### F3B: Parser API hard requirement
1. Change parser signature to require `~file_id:string`.
2. Remove optional fallback flow (`None -> "<unknown>"`).
3. Update parser construction and all callers.

#### F3C: Caller threading rollout
1. Compiler file-based entrypoints use real file path.
2. LSP uses URI string.
3. Tests/repl/synthetic callers use explicit stable synthetic ids (`<test>`, `<stdin>`, `<repl>`, `<codegen>`).

### Files in scope
1. `lib/frontend/typecheck/checker.ml`
2. `lib/frontend/syntax/parser.ml`
3. `lib/backend/go/emitter.ml`
4. `tools/lsp/lib/doc_state.ml`
5. other parser callsites in tests/helpers

### Commit slices
1. `typecheck(checker): add catch-all exn boundary mapping`
2. `syntax(parser): require file_id in parse API`
3. `refactor(callers): thread explicit file_id across all parser entrypoints`

### Risks
1. Signature churn across many files.
2. Missing callsite can fail compile in later commit.

### Mitigation
1. Do API change and callsite updates in same commit or tightly adjacent commits.

### Tests
```bash
dune runtest --force lib/frontend/syntax
dune runtest --force lib/frontend/typecheck
dune runtest --force tools/lsp/lib
dune runtest --force lib/backend/go
```

### Done criteria
1. Parser identity is explicit everywhere.
2. Checker boundary no longer leaks uncaught exceptions from expected paths.

---

## Phase F4: Internal Diagnostic Convergence (Frontend Core)

### Objective
Complete remaining internal migration so expected-path errors originate as `Diagnostic.t`, not exception strings.

### Issues addressed
1. `MR-06`
2. Remaining original Phase-4 completion-gate gaps
3. `MR-01` (deep/internal side)

### Detailed work

#### F4A: `annotation.ml` result transport
1. Replace expected-path `failwith` and `Open_row_rejected` usage with result-based diagnostics.
2. Keep true invariant failures separate and clearly marked.
3. Normalize diagnostic code families for annotation errors.

#### F4B: `infer.ml` cleanup
1. Remove `diag_kind` intermediate transport.
2. Construct diagnostics directly at error origin points.
3. Replace user-triggerable `failwith` paths with structured diagnostics.
4. Preserve location spans where available.

#### F4C: checker simplification
1. Simplify checker-side message-pattern bridge logic as internal flows become structured.
2. Reduce reliance on substring classification.

### Files in scope
1. `lib/frontend/typecheck/annotation.ml`
2. `lib/frontend/typecheck/infer.ml`
3. `lib/frontend/typecheck/checker.ml`
4. optional touch: `lib/frontend/typecheck/unify.ml`

### Commit slices
1. `typecheck(annotation): move expected error paths to Diagnostic.result`
2. `typecheck(infer): remove diag_kind transport and emit Diagnostic.t directly`
3. `typecheck(infer): replace remaining user-triggerable failwith paths`
4. `typecheck(checker): simplify boundary classification after infer/annotation migration`

### Risks
1. Large blast radius in type inference flows.
2. Possible fixture churn due message/code precision improvements.

### Mitigation
1. Split by sub-area (annotation first, infer second, checker cleanup third).
2. Run targeted traits/cross_feature fixtures after each commit.

### Tests
```bash
dune runtest --force lib/frontend/typecheck
make integration traits
make integration cross_feature
make integration test/fixtures/traits_field/tf17_constrained_generic_field_used_in_match_expression_bug_codeg.mr
```

### Done criteria
1. Expected user-facing errors in annotation/infer no longer rely on exception transport.
2. Phase-4 completion gate can be marked closed in progress log.

---

## Phase F5: Trait Diagnostic Fidelity and Message Cleanup

### Objective
Preserve trait-specific diagnostic fidelity end-to-end and remove legacy bracket-tag debt.

### Issues addressed
1. Trait diagnostic downgrade paths
2. `LR-03`

### Detailed work
1. Ensure infer obligation paths keep trait-specific `diag.code` where applicable.
2. Remove bracket tags (`[missing-field]`, etc.) from trait message strings once fixtures rely on code + stable message fragments.
3. Rebaseline affected fixtures in small slices.

### Files in scope
1. `lib/frontend/typecheck/trait_solver.ml`
2. `lib/frontend/typecheck/infer.ml`
3. `test/fixtures/traits*`
4. `test/fixtures/traits_field*`

### Commit slices
1. `typecheck(traits): preserve trait-specific diagnostic codes through infer paths`
2. `typecheck(traits): remove legacy bracket tags from trait messages`
3. `tests(fixtures): rebaseline affected trait fixture expectations`

### Risks
1. Large number of fixture updates possible.

### Mitigation
1. Rebaseline by trait subgroup and verify each subgroup independently.

### Tests
```bash
make integration traits
make integration traits_field
dune runtest --force lib/frontend/typecheck
```

### Done criteria
1. Trait classification is code-first and stable.
2. Legacy bracket tag protocol removed from active trait messages.

---

## Phase F6: Multi-Diagnostic Surfaces and Diagnostics-Lib Cleanup

### Objective
Reduce first-diagnostic truncation and remove duplicated diagnostics helpers/tests gaps.

### Issues addressed
1. `MR-05`
2. `LR-02`
3. `LR-04`

### Detailed work

#### F6A: Boundary list support
1. Promote selected boundaries to diagnostics-list flow where practical.
2. Use deterministic ordering to preserve harness match stability.
3. Apply `render_many_cli` on list-ready CLI surfaces.

#### F6B: shared helper cleanup
1. Export canonical primary-span selector from diagnostics API.
2. Remove local duplicates in checker and doc_state.

#### F6C: coverage additions
1. Add explicit `Warning` and `Info` renderer tests.
2. Add missing parser-code-family tests (`parse-invalid-number`, `parse-invalid-impl`, `parse-invalid-pattern`, `parse-unexpected-token`).

### Files in scope
1. `lib/diagnostics/diagnostic.ml`
2. `lib/diagnostics/diagnostic.mli`
3. `lib/frontend/typecheck/checker.ml`
4. `tools/lsp/lib/doc_state.ml`
5. `bin/main.ml`
6. `lib/backend/go/emitter.ml`
7. `lib/frontend/syntax/parser.ml`

### Commit slices
1. `diagnostics(api): export shared primary-span helper and consume in checker/lsp`
2. `diagnostics(renderer): add warning/info test coverage`
3. `syntax(parser): add missing parse code-family tests`
4. `cli/backend: adopt render_many_cli on list-ready surfaces`

### Risks
1. Output shape drift can affect fixtures.

### Mitigation
1. Keep list transport changes isolated from message text changes.
2. Validate targeted reject fixtures after each boundary change.

### Tests
```bash
dune runtest --force lib/diagnostics
dune runtest --force lib/frontend/syntax
dune runtest --force lib/backend/go
dune runtest --force tools/lsp/lib
```

### Done criteria
1. First-diagnostic truncation reduced on migrated paths.
2. Duplicate span-selection logic removed.
3. Renderer/parser coverage gaps closed.

---

## Phase F7: Progress Doc Reconciliation and Closure Tracking

### Objective
Bring progress docs in sync with actual code and closure status.

### Issues addressed
1. `PD-01`
2. `PD-02`
3. `PD-03`

### Detailed work
1. Update session scope and metadata to reflect actual phase span.
2. Extend checklist through phases 4-8 and follow-up work.
3. Correct strictness/completion wording where over-strong.
4. Add issue closure table:
   - issue id
   - status
   - commit hash
   - tests run
5. Record final gate command outputs (targeted + full gate when run).

### Files in scope
1. `docs/progress/diagnostics-rework.md`

### Commit slices
1. No commit (progress docs remain uncommitted by rule).

### Tests
1. N/A (documentation consistency checks only).

### Done criteria
1. Progress log is accurate and auditable.

---

## Phase F8 (Optional): Classification Layer Ownership Cleanup

### Objective
Move codegen/build classification helpers to cleaner ownership boundaries.

### Issues addressed
1. CLI/backend layering cleanup from reviews.

### Detailed work
1. Move Go-build/classification policy out of `bin/main.ml` into backend library surface.
2. Keep output behavior unchanged.

### Files in scope
1. `bin/main.ml`
2. `lib/backend/go/*`

### Tests
```bash
dune runtest --force lib/backend/go
make integration codegen
```

### Done criteria
1. CLI entrypoint no longer owns backend classification policy.

---

## 7) Cross-Cutting Cleanup Backlog (Execute opportunistically)

These are non-blocking unless encountered in touched code:
1. Consolidate duplicated substring helpers across modules into shared utility.
2. Remove stale identity shims once dead.
3. Keep source location helper layering tidy after parser/file_id migration.
4. Preserve deterministic diagnostic formatting for harness stability.

---

## 8) Detailed Test Matrix

Use this matrix as execution checklist.

| Phase | Unit targets | Integration targets | Gate type |
|---|---|---|---|
| F1 | `bash -n` harness scripts | `make integration harness` + targeted fixtures | required |
| F2 | `dune runtest --force tools/lsp/lib` | optional `make integration cli` smoke | required |
| F3 | `dune runtest --force lib/frontend/syntax lib/frontend/typecheck tools/lsp/lib` | targeted parse/type fixtures | required |
| F4 | `dune runtest --force lib/frontend/typecheck` | `make integration traits cross_feature` subsets | required |
| F5 | `dune runtest --force lib/frontend/typecheck` | `make integration traits traits_field` | required |
| F6 | `dune runtest --force lib/diagnostics lib/frontend/syntax lib/backend/go tools/lsp/lib` | targeted reject slices | required |
| F7 | docs-only | none | required |
| Final closure | `make unit` | `make integration` | closure gate |

Notes:
1. If any phase introduces fixture churn, isolate fixture rebaseline commit from behavior commit.
2. If full integration is too slow during iteration, defer to closure gate.

---

## 9) Commit Plan (Execution-Grade)

Recommended commit sequence:
1. `test(harness): enforce severity/file strictness and mixed wildcard semantics`
2. `test(harness): colon-safe header parsing with bounded continuation`
3. `test(harness): verify canary stub injection and remove dead matcher state`
4. `lsp(server): surface analyzer internal failures as diagnostics`
5. `typecheck(checker): add catch-all exn boundary mapping`
6. `syntax(parser): require file_id and thread explicit IDs`
7. `typecheck(annotation): convert expected-path errors to Diagnostic.result`
8. `typecheck(infer): remove diag_kind and direct-emit diagnostics`
9. `typecheck(infer): replace user-triggerable failwith sites`
10. `typecheck(traits): preserve trait code fidelity through infer obligation paths`
11. `typecheck(traits): remove bracket tags from messages`
12. `tests(fixtures): rebaseline trait-related expectations for message cleanup`
13. `diagnostics(api): export primary-span helper and dedupe checker/lsp usage`
14. `diagnostics(test): add warning/info and parser-code-family coverage`
15. `cli/backend: apply render_many_cli where list boundaries are ready`
16. `docs(progress): reconcile status and issue closure table` (uncommitted tracking update by rule)

Commit hygiene:
1. One logical change per commit.
2. Explicit file adds only.
3. Keep changesets narrow to ease bisectability.

---

## 10) Risk Register

| Risk | Likelihood | Impact | Mitigation |
|---|---|---|---|
| Harness strictness changes expose many stale fixtures | High | Medium | rebaseline in dedicated commits, preserve deterministic matching |
| Parser API hardening breaks many tests at once | High | Medium | do signature + callsite update in tightly coupled commits |
| Infer internal refactor causes subtle type regressions | Medium | High | subphase split, targeted traits/cross_feature runs after each commit |
| LSP internal diagnostic flooding | Low | Medium | keep concise message and existing debounce/cache behavior |
| Multi-diagnostic rollout changes output shape | Medium | Medium | list transport in isolated phase + targeted reject suite checks |

---

## 11) Completion Definition

The refactoring is complete when all are true:
1. `HR-01` and `HR-02` are closed with passing canaries/tests.
2. Parser `file_id` is mandatory and fully threaded.
3. Expected-path exception/string transport debt in annotation/infer is resolved.
4. Trait diagnostics preserve structured classification end-to-end; legacy bracket tags removed.
5. Multi-diagnostic/helper/test coverage items in F6 are closed.
6. Progress log accurately reflects status and issue closure.
7. Final closure gate passes (`make unit && make integration`) when executed for completion.

---

## 12) Immediate Next Step

Start with **Phase F1 / Commit 1**:
1. enforce severity-kind hard matching,
2. tighten file/line-bound selection,
3. add severity mismatch canary,
4. run harness-targeted gate.

