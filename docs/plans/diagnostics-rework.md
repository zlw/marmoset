# Diagnostics Rework Plan (LSP-Ready, No Fancy Rendering Yet)

## Summary
Improve compiler diagnostics so parser/typechecker/codegen all emit structured, span-accurate diagnostics consumable by both CLI and in-repo OCaml LSP code.

Phase 1 focuses on correctness and structure, not fancy terminal rendering.

## Locked Decisions
- Internal canonical diagnostics core in a **shared library** (`lib/diagnostics/`).
- No rich renderer in phase 1.
- **Bail-on-first error** across all layers (parser, typecheck, codegen). Multi-error accumulation is a separate future effort.
- Severity model supports `Error` / `Warning` / `Info` now.
- No requirement to emit standalone warnings yet (infra first).
- No JSON surface now; must be easy to add later.
- Assume `check` command ownership is in another branch.
- Backward message compatibility is not required (pre-release).
- **Descriptive error codes** (e.g., `E-TYPE-UNBOUND-VAR`), not numeric. Can assign numeric aliases later post-1.0.

## Current-State Problems
- Parse errors often lose location in CLI (`Parse error: ...` without line/column).
- Type errors often have spans, but error flow is inconsistent/string-first.
- CLI prints flat strings from compile/build pipeline.
- Parser returns `string list` errors, not structured diagnostics.

## Existing Infrastructure to Reuse
- **`source_loc.ml`** (currently in `typecheck/`): Has `loc` type, `offset_to_loc`, `to_string`, `to_string_range`, `format_with_context`, `format_with_context_range`. All tested. Move to shared `diagnostics` library.
- **`infer.ml` error types**: `error_kind` (20 variants) + `infer_error` record with `pos`, `end_pos`, `file_id`. Map these to `Diagnostic.t` -- this is a mapping exercise, not building from scratch.
- **`checker.ml` error type**: `error` record with `message`, `loc`, `loc_end`, `file_id` plus formatting functions (`format_error`, `format_error_with_context`). These become thin wrappers over `Diagnostic.t`.
- **Trait solver bracket codes**: Already uses descriptive codes like `[missing-field]`, `[field-type-mismatch]`, `[impl-resolution]`. Formalize into the `E-TYPE-*` taxonomy.

---

## Implementation Plan

### Phase 1: Canonical Diagnostics Library

Create a new **shared library** so all layers can depend on it:
- `lib/diagnostics/diagnostic.ml`
- `lib/diagnostics/diagnostic.mli`
- `lib/diagnostics/source_loc.ml` (moved from `typecheck/`)
- `lib/diagnostics/dune` (no dependencies beyond stdlib)

Update dune dependencies:
```
syntax      -> diagnostics
typecheck   -> syntax, diagnostics
codegen     -> syntax, typecheck, diagnostics
```

Define types:
```ocaml
type severity = Error | Warning | Info

type span =
  | NoSpan
  | Span of { file_id : string option; start_pos : int; end_pos : int }

type label = { span : span; message : string option; primary : bool }

type t = {
  code : string;           (* e.g., "E-TYPE-UNBOUND-VAR" *)
  severity : severity;
  message : string;
  labels : label list;
  notes : string list;
}
```

Add helpers:
- Constructors (`error_with_span`, `error_no_span`, `with_note`, `with_secondary_label`)
- Reuse `Source_loc.offset_to_loc` for offset -> line/column conversion
- Plain deterministic renderer:
  - `file:line:col[-line:col]: [CODE] message`
  - Optional note/secondary lines (plain text)
- `to_legacy_string : ?source:string -> t -> string` for backward compat during migration

### Phase 2: Parser Diagnostics (Structured Single-Error)

Refactor `lib/frontend/syntax/parser.ml`:
- Replace `errors : string list` with `diagnostics : Diagnostic.t list`.
- Every parser error must include:
  - Stable descriptive code (e.g., `E-PARSE-EXPECTED-TOKEN`, `E-PARSE-UNEXPECTED-TOKEN`, `E-PARSE-INVALID-NUMBER`)
  - Span from `pos`/`token_end` (byte offsets, already available on tokens)
  - `file_id` (already tracked in parser state, just not attached to errors currently)
- **Keep bail-on-first semantics.** The parser's Result-monad control flow stays unchanged. The single error returned is now a structured `Diagnostic.t` instead of a string.
- Parser recovery (multi-error) is a **separate future effort** requiring:
  - Synchronization strategy (skip to next `let`/`fn`/`type`/`enum`/`trait`/`impl` keyword)
  - Converting from fail-fast `Result` to continue-and-accumulate
  - Deciding AST representation for failed statements
  - This is a major refactor and should not block the diagnostics rework.

Update parse API:
```ocaml
val parse : ?file_id:string -> string -> (AST.program, Diagnostic.t list) result
```

Keep returning a single error (in a list for forward-compat with future multi-error).

### Phase 3: Typechecker/Infer Diagnostics

Refactor:
- `lib/frontend/typecheck/infer.ml`
- `lib/frontend/typecheck/checker.ml`

Changes:
- Map `error_kind` variants to stable descriptive codes:
  - `UnboundVariable` -> `E-TYPE-UNBOUND-VAR`
  - `UnificationError (TypeMismatch _)` -> `E-TYPE-MISMATCH`
  - `UnificationError (OccursCheck _)` -> `E-TYPE-OCCURS-CHECK`
  - `InvalidOperator` -> `E-TYPE-INVALID-OPERATOR`
  - `IfBranchMismatch` -> `E-TYPE-IF-BRANCH-MISMATCH`
  - `IfConditionNotBool` -> `E-TYPE-IF-CONDITION`
  - `ReturnTypeMismatch` -> `E-TYPE-RETURN-MISMATCH`
  - `ConstructorError` -> `E-TYPE-CONSTRUCTOR` (refine subtypes later)
  - `PurityViolation` -> `E-TYPE-PURITY`
  - etc.
- Preserve spans from `pos/end_pos/file_id` (already captured by `error_at`/`error_at_stmt`)
- `checker.ml` error type becomes a thin wrapper: convert `infer_error` -> `Diagnostic.t` directly
- **Bail-on-first stays.** Single error returned, wrapped in `Diagnostic.t`.
- Formalize trait solver bracket codes into `E-TYPE-TRAIT-MISSING-FIELD`, `E-TYPE-TRAIT-TYPE-MISMATCH`, etc.

### Phase 4: Codegen/Build Pipeline Diagnostics

Refactor:
- `lib/backend/go/emitter.ml`
- `bin/main.ml`

Changes:
- **Keep exceptions in codegen.** The emitter continues to use `failwith` internally. At the `compile_string`/`compile_to_build` boundary, catch exceptions and convert to `Diagnostic.t`:
  ```ocaml
  try Ok (emit_program_with_typed_env ...) with
  | Failure msg -> Error [codegen_error_to_diagnostic msg]
  ```
- Normalize codegen failures to descriptive codes: `E-CODEGEN-UNRESOLVED-TVAR`, `E-CODEGEN-AMBIGUOUS-FN`, etc.
- Build failures: `E-BUILD-GO-COMPILE`, `E-BUILD-GO-MISSING`
- CLI renders diagnostics using the deterministic plain renderer and exits non-zero on any `Error`

### Phase 5: LSP-Consumable OCaml API
Expose unified typed diagnostics API (no JSON yet):
- Parser/checker/codegen all return `(result, Diagnostic.t list) result`
- Includes severity, code, primary+secondary labels, notes
- Keep serializer-ready shape so JSON can be added later without rework

---

## Public Interface Changes

### New
- `Diagnostics` library (shared across all compiler layers).
- `Diagnostics.Diagnostic` module with types, constructors, renderer.
- `Diagnostics.Source_loc` module (moved from `Typecheck.Source_loc`).

### Updated
- `Syntax.Parser.parse` returns `Diagnostic.t list` on error instead of `string list`.
- `Typecheck.Checker.check_program` returns `Diagnostic.t` on error instead of `Checker.error`.
- `Codegen.Go_emitter.compile_string` returns `Diagnostic.t list` on error instead of `string`.
- Existing string-returning APIs become thin wrappers via `Diagnostic.to_legacy_string`.

### Diagnostic Code Taxonomy (Descriptive)

**Parser:**
- `E-PARSE-EXPECTED-TOKEN` -- expected X, got Y
- `E-PARSE-UNEXPECTED-TOKEN` -- no parse rule for token
- `E-PARSE-INVALID-NUMBER` -- can't parse number literal
- `E-PARSE-INVALID-RECORD` -- record/hash literal issues
- `E-PARSE-INVALID-IMPL` -- impl definition issues

**Typechecker:**
- `E-TYPE-UNBOUND-VAR` -- unbound variable
- `E-TYPE-MISMATCH` -- unification failure (type mismatch)
- `E-TYPE-OCCURS-CHECK` -- infinite type
- `E-TYPE-INVALID-OPERATOR` -- operator not valid for type
- `E-TYPE-IF-BRANCH-MISMATCH` -- if branches have different types
- `E-TYPE-IF-CONDITION` -- if condition not bool
- `E-TYPE-RETURN-MISMATCH` -- return type annotation violated
- `E-TYPE-PURITY` -- pure function calls effectful operation
- `E-TYPE-CONSTRUCTOR` -- enum/type constructor error
- `E-TYPE-PATTERN` -- pattern matching error
- `E-TYPE-MATCH` -- match expression error
- `E-TYPE-TRAIT-MISSING-FIELD` -- trait field not found
- `E-TYPE-TRAIT-TYPE-MISMATCH` -- trait field type mismatch
- `E-TYPE-TRAIT-MISSING-IMPL` -- no impl for trait
- `E-TYPE-TRAIT-UNKNOWN` -- unknown trait name

**Codegen/Build:**
- `E-CODEGEN-UNRESOLVED-TVAR` -- type variable reached codegen
- `E-CODEGEN-AMBIGUOUS-FN` -- ambiguous function reference
- `E-BUILD-GO-COMPILE` -- go build failed
- `E-BUILD-GO-MISSING` -- go toolchain not found

**Future warnings/info:**
- `W-*` (warnings), `I-*` (info)

---

## Test Plan

### Unit Tests
- `Diagnostic.t` constructors produce correct codes and spans
- `Diagnostic.render` output is deterministic and matches expected format
- `Source_loc.offset_to_loc` (existing tests, moved to new library)
- Parser error includes span + file_id in `Diagnostic.t`
- Type errors carry correct primary range in `Diagnostic.t`
- Error code mapping covers all `error_kind` variants

### Integration Tests
- CLI parse errors include file + line/column/range
- CLI type errors keep/improve location fidelity
- Successful build/run behavior unchanged

### Test Migration Plan
Existing integration tests use `expect_build "name" "error_fragment"` which greps for substrings in error output. Error messages will change format (e.g., from `"Type error: Cannot unify..."` to `"main.mr:5:10: [E-TYPE-MISMATCH] Cannot unify..."`).

Migration strategy:
1. Phase 1 (diagnostics core): No output changes. All tests pass as-is.
2. Phases 2-4: Update `expect_build` error fragments incrementally as each layer migrates. The error message text itself stays similar -- the prefix/format changes.
3. After CLI switch: Do a single pass to update any remaining broken fragments.
4. Consider adding `expect_diagnostic "name" "E-TYPE-MISMATCH"` helper to `common.sh` for code-based matching (more stable than message text).

### Non-Regression
- `make unit` green after each phase
- `make integration` green after each phase (with test fragment updates)

---

## Rollout
1. Create `lib/diagnostics/` library, move `source_loc.ml`, add `diagnostic.ml`. No behavior change -- existing code unaffected.
2. Migrate parser to return structured `Diagnostic.t` on error. Update `compile_string` to convert.
3. Migrate checker/infer to return `Diagnostic.t`. Update `compile_string` to convert.
4. Migrate codegen boundary to catch exceptions and wrap in `Diagnostic.t`.
5. Switch CLI (`bin/main.ml`) to render `Diagnostic.t` via deterministic renderer.
6. Update integration test error fragments as needed.
7. Keep `to_legacy_string` wrappers to minimize merge friction with ongoing work.

---

## Assumptions
- Compiler and LSP remain OCaml/in-repo for now.
- No fancy renderer in phase 1.
- No JSON diagnostics surface in phase 1.
- Severity model includes Error/Warning/Info from day one, even if early emissions are mostly errors.
- Bail-on-first across all layers. Multi-error accumulation is a separate future effort.
- Parser recovery (synchronization + continue after error) is explicitly out of scope.
