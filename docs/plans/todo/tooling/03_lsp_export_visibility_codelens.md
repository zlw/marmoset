# Add LSP Export Visibility CodeLens

## Maintenance

- Last verified: 2026-04-06
- Implementation status: In Progress
- Type: Tooling plan
- Update trigger: Any change to export-header syntax/ordering, `lib/frontend/{discovery,module_context,import_resolver,compiler}.ml`, `tools/lsp/lib/{server,doc_state,cursor_context}.ml`, or editor/LSP capability plumbing around CodeLens and execute-command.

## Context

The current module-aware LSP work already supports hover, completion, definition, inlay hints, document symbols, and code actions. It does not support CodeLens at all, and it has no inline action for changing whether a top-level declaration is exported.

Marmoset visibility is module-level and entirely driven by the single `export ...` header:

- `export` may appear at most once.
- If present, it must be the first top-level statement.
- A declaration is effectively private when its surface name is absent from that header.

That makes export toggling a good fit for always-visible declaration lenses:

- unexported top-level declaration -> `make public`
- exported top-level declaration -> `make private`

The warning on `make private` must be based on real downstream references, not just import headers. `import types.geo` followed by `geo.Point` or `geo.render_point(...)` is still a dependency on `types.geo`, even though the importing file never wrote `import types.geo.Point`.

The repo already has the ingredients to make this principled instead of heuristic:

- `Surface.surface_program` retains exact top-level declaration head refs for `let`, `fn`, `type`, `shape`, and `trait`.
- `Module_context.parsed_module` preserves each module’s `surface_program`, `program`, `exports`, and `imports`.
- `Compiler.entry_analysis` already carries project graph state, navigation metadata, and open-buffer source overrides through `Doc_state.analyze_with_file_id`.
- `cursor_context.ml` already knows how to classify value/type/import/qualified references from the surface tree.

What is missing is the actual CodeLens protocol plumbing, command-driven export edits, and a compiler/LSP helper that can count cross-module dependents of a specific exported declaration.

## Goals

1. Add LSP CodeLens for export visibility on top-level declarations.
2. Support the exact user-facing titles:
   - `make public`
   - `make private`
   - `make private (used by N module)` / `make private (used by N modules)`
3. Offer lenses on top-level `let`, `fn`, `type`, `shape`, and `trait` declarations only.
4. Apply edits by rewriting the module `export` header correctly:
   create it when absent, append names when making public, remove names when making private, and delete the header when it becomes empty.
5. Base the `used by N modules` warning on real cross-module references, including namespace-qualified uses like `geo.Point` and `geo.render_point`.
6. Reuse existing project-root/source-override analysis so unsaved open buffers affect dependent counts and final edits the same way they already affect completion and definition.

## Non-Goals

1. No CodeLens on local lets, lambda params, trait methods, impl methods, variants, or import statements.
2. No general-purpose `textDocument/references` feature in this milestone.
3. No editor-specific custom UI; this stays within standard LSP CodeLens plus execute-command.
4. No export sorting or broader formatting pass. The implementation should preserve existing export order and touch only the minimum necessary text.
5. No semantic change to the module system; the export header remains the single source of truth.
6. No background indexer/daemon. This stays inside the existing analyze-on-request model.

## Current State

### LSP Server

- `tools/lsp/lib/server.ml` advertises hover, definition, completion, inlay hints, document symbols, and code actions.
- It does not override `config_code_lens_options`, `config_list_commands`, `on_req_code_lens`, `on_req_code_lens_resolve`, or `on_req_execute_command`.
- Linol already supports all needed protocol pieces:
  `textDocument/codeLens`, `workspace/executeCommand`, and `workspace/applyEdit` via `notify_back#send_request`.

### Export Metadata

- `lib/frontend/syntax/parser.ml` enforces that `export` is first if present and occurs at most once.
- `lib/frontend/discovery.ml` extracts `exports` from the first `AST.ExportDecl`.
- `lib/frontend/module_context.ml` stores those exports on every parsed module.
- `lib/frontend/import_resolver.ml` validates the export list against actual declarations when building module surfaces.

### Declaration Anchors

- `lib/frontend/syntax/surface_ast.ml` already carries exact top-level name refs for:
  `SLet`, `SFnDecl`, `STypeDef`, `SShapeDef`, and `STraitDef`.
- `SExportDecl` stores only `string list`, but the enclosing `surface_top_stmt` still gives the full span of the export header statement, which is enough for whole-header rewrites.

### Semantic Navigation

- `Compiler.current_navigation`, `Compiler.find_visible_presence`, and `Compiler.find_export_binding` already expose the visible/exported symbol surface needed to decide whether a declaration is currently public.
- `cursor_context.ml` can classify single cursor references precisely, but it has no “collect every reference in this module” API yet.
- There is no helper that counts distinct external modules referencing a particular declaration.

## Design Decisions

### 1. Ship CodeLens-Only V1

This milestone should use CodeLens, not code actions, as the primary UX. The whole point is an always-visible affordance above declarations, similar to TS “show usages”.

Eligible declaration heads are:

- top-level `let`
- top-level `fn`
- `type` declarations, including transparent aliases, wrappers, products, and sums
- `shape`
- `trait`

No lenses should appear on:

- local bindings inside functions or blocks
- trait method signatures
- impl or inherent method implementations
- import/export headers
- expression statements

The CodeLens range should be the declaration head span (`name_ref` / `type_name_ref` / `shape_name_ref`), not the entire statement.

### 2. Use Command-Backed Lenses, Not Resolve-Only Lenses

The simplest robust path is:

1. `textDocument/codeLens` computes fully-populated lenses with titles and commands.
2. Clicking a lens triggers `workspace/executeCommand`.
3. The server recomputes the edit against the freshest open-buffer text and applies it with `workspace/applyEdit`.

This keeps the feature immediately visible in editors without depending on `codeLens/resolve`, and it keeps stale command arguments from blindly rewriting old source.

`textDocument/codeLens` should follow the same cache policy as completion/definition:

- prefer live open-buffer text when it still yields semantic state
- fall back to `last_good` semantic analysis when the current buffer is temporarily parse-broken
- still execute the final edit against the live buffer, not the cached snapshot

Preferred command surface:

- one command: `marmoset.setExportVisibility`
- arguments encode:
  - target document URI
  - declaration surface name
  - declaration kind
  - requested visibility (`public` or `private`)
  - original declaration range for revalidation

On execution, the server must re-read the latest open buffer and re-resolve the target declaration before building the edit. If the declaration disappeared or no longer resolves cleanly, the command should refuse the edit and log/show a short warning instead of guessing.

### 3. Introduce Shared Export-Edit Helpers

The text rewriting logic should live in a dedicated helper module, not inside `server.ml` and not baked into CodeLens generation.

Preferred new module:

- `tools/lsp/lib/export_edits.ml`

Responsibilities:

- locate the current export header span from `Surface.SExportDecl` / `surface_top_stmt`
- decide whether a declaration is exported
- produce the minimal `TextEdit` list for:
  - adding a name to an existing export header
  - inserting a new export header before the first top-level statement
  - removing one name from an export header
  - deleting the whole export header when the last exported name is removed

Editing rules:

- Preserve existing export order.
- `make public` appends the name to the existing header rather than sorting the list.
- If no export header exists, insert `export <Name>\n` immediately before the first top-level statement span. This preserves leading comments/blank lines that occur before the first statement.
- If removing the last exported name, delete the export statement and at most one immediately following newline so the file does not gain an extra empty line.
- Never touch unrelated imports/body statements.

The helper should be reusable later by code actions or future refactors, but this plan does not require a new user-facing code action.

### 4. Count Dependents By Resolved References, Not Import Headers

The warning title must be driven by real downstream references. The implementation should count distinct external modules whose references resolve to the exact declaration target in the active module.

The plan should avoid N-times-full-project rescans for N exported declarations in one file. Instead:

1. Gather all eligible declaration targets in the active file and compute their exact definition identities:
   `{ file_path; start_pos; end_pos }`.
2. Traverse every other module in the project once.
3. Collect that module’s surface references.
4. Resolve each reference through the same compiler/navigation path definition uses today.
5. If the resolved target identity matches one of the active file’s declarations, record the referencing module id in that declaration’s dependent set.

That yields a map:

```ocaml
target_identity -> StringSet.t  (* external module ids *)
```

This must count:

- direct import uses: `import math.add` then `add(...)`
- direct import aliases
- namespace-qualified value uses: `import math` then `math.add(...)`
- namespace-qualified type uses: `import types.geo` then `geo.Point`
- trait/shape/type references where the target is the exported top-level declaration

This must not count:

- same-module references
- modules that only import the namespace but never reference the symbol
- duplicate references from the same external module more than once

To make this practical, `cursor_context.ml` should gain a bulk traversal helper such as `collect_references` or `fold_references`. Repeatedly calling `reference_at` for every offset is the wrong model.

### 5. Keep The “Show Dependents” Follow-Up Unblocked

The already-computed dependent map should keep the full module identities, not just counts, so a later milestone can add:

- a second CodeLens like `show dependents`
- `textDocument/references`
- or a custom “show usages” command

But this plan intentionally stops at the warning count in the `make private` title. There is no good cross-editor dependent-list presentation path in the repo today, and inventing one would dilute the main slice.

## Implementation Plan

### Phase T0: Export Edit Substrate

Touchpoints:

- new `tools/lsp/lib/export_edits.ml`
- `tools/lsp/lib/dune`
- focused tests alongside the new helper

Work:

- Define a small declaration descriptor for exportable top-level heads.
- Extract exportable declarations from `Surface.surface_program`.
- Detect current export visibility from the module export list.
- Build minimal `TextEdit` sequences for `make public` / `make private`.
- Keep whole-header rewrites local to the export statement span.

Exit criteria:

- Helper tests cover add/remove/create/delete-empty cases.
- No server or CodeLens plumbing yet.

### Phase T1: LSP CodeLens And Execute-Command Plumbing

Touchpoints:

- new `tools/lsp/lib/code_lens.ml`
- `tools/lsp/lib/server.ml`
- `tools/lsp/lib/doc_state.ml` if shared request helpers are needed

Work:

- Add `config_code_lens_options` and advertise CodeLens support.
- Add `config_list_commands` entry for `marmoset.setExportVisibility`.
- Implement `on_req_code_lens` for documents with semantic analysis.
- Reuse the existing cached-document `latest`/`last_good` policy so lenses remain available during incomplete edits when possible.
- Convert exportable declarations into `CodeLens.t` values with titles and commands.
- Implement `on_req_execute_command`.
- Use `notify_back#send_request (WorkspaceApplyEdit ...)` to apply the computed edit.
- Recompute against fresh open-buffer text in the command handler, not stale lens text.

Exit criteria:

- `textDocument/codeLens` returns `make public` / `make private` on the right declarations.
- Clicking a lens applies the correct export edit in server tests.

### Phase T2: Dependent Analysis And Warning Titles

Touchpoints:

- `tools/lsp/lib/cursor_context.ml`
- `tools/lsp/lib/definition.ml` or a new shared target-resolution helper if needed
- `lib/frontend/compiler.ml`
- `tools/lsp/lib/code_lens.ml`
- `tools/lsp/lib/server.ml`

Work:

- Add bulk reference collection from `Surface.surface_program`.
- Add a helper that resolves references from arbitrary project modules to exact definition identities.
- Build a per-request dependent map for all exportable declarations in the active file.
- Count distinct external modules per declaration.
- Change exported declaration lens titles from:
  - `make private`
  to:
  - `make private (used by N module)`
  - `make private (used by N modules)`
  when the count is nonzero.

Exit criteria:

- Namespace-qualified type/value usages are counted.
- Same-module references are not counted.
- The active document’s lenses show stable counts under open-buffer source overrides.

### Phase T3: Hardening And Refresh Behavior

Touchpoints:

- `tools/lsp/lib/server.ml`
- `tools/lsp/lib/code_lens.ml`
- editor CI where practical

Work:

- Harden stale-command rejection when the declaration head was renamed or deleted after the lens was rendered.
- Verify that document changes naturally refresh CodeLens in supported clients; only add explicit `workspace/codeLens/refresh` if a concrete client gap is observed.
- Keep logging concise and failure paths non-destructive.

Exit criteria:

- No stale-command mis-edits in tests.
- No manual editor-specific workaround is required for normal refresh behavior.

## Testing Strategy

### Export Edit Helpers

Add focused unit tests for:

- appending a name to an existing export header
- creating a new export header in a file that starts with imports
- creating a new export header in a file with leading comments/blank lines
- removing a middle export name
- removing the final export name and deleting the whole header
- refusing/no-op behavior when the requested visibility already matches reality

### CodeLens Computation

Add LSP-focused tests for:

- exported top-level `fn` -> `make private`
- unexported top-level `fn` -> `make public`
- exported `type` / `shape` / `trait` -> `make private`
- top-level `let` -> correct lens
- no lenses on local `let` bindings
- no lenses on trait methods or impl methods

### Execute Command

Add server tests for:

- `workspace/executeCommand` applies the expected edit through `workspace/applyEdit`
- command execution uses live open-buffer text, not stale lens-time text
- renamed/deleted declarations are rejected instead of rewritten heuristically

### Dependent Analysis

Add project tests for:

- direct import value usage counts as one dependent module
- direct import alias usage counts as one dependent module
- namespace-qualified value usage counts as one dependent module
- namespace-qualified type usage (`geo.Point`) counts as one dependent module
- same external module using the symbol multiple times still counts once
- namespace import without symbol use does not count
- same-module references do not affect the warning title
- unsaved open-buffer overrides in dependent modules change the count seen by CodeLens

## Risks And Mitigations

### 1. Export Header Rewrites Could Disturb Formatting

Mitigation:

- keep edits scoped to the export statement span
- preserve existing order and punctuation style
- test create/remove-last cases explicitly with imports and leading comments

### 2. Dependent Counting Could Become Too Expensive

Mitigation:

- compute one dependent map per active file request, not one full-project scan per declaration
- reuse compiler-owned project/navigation state from the existing analysis result
- defer any workspace-wide references UI until a real need appears

### 3. False Positives From Namespace Imports

Mitigation:

- count only references that resolve to the exact declaration identity
- never treat a bare import header as usage on its own

### 4. Stale Lens Clicks Could Rewrite The Wrong Declaration

Mitigation:

- include declaration range/name metadata in command args
- re-resolve against the live buffer before applying edits
- refuse the edit when revalidation fails

## Commit Plan

1. Add export-edit helpers plus focused tests.
2. Add CodeLens server plumbing and execute-command application tests.
3. Add bulk reference collection, dependent counting, and warning-title tests.

## Future Work

- Add a second lens like `show dependents` once there is a cross-editor presentation path worth standardizing.
- Reuse the same dependent map for a future `textDocument/references` implementation.
- Add export-header hygiene helpers like “remove unused exports” or “sort exports” only after this visibility workflow is proven.

## Progress

- 2026-04-06 02:00 CEST: Draft plan created in `docs/plans/todo/tooling/03_lsp_export_visibility_codelens.md`; awaiting `plan-review`.
- 2026-04-06 02:02 CEST: Implementation started in `/Users/zlw/src/marmoset/marmoset` after reading `CLAUDE.md`, `.ai-rules/references/marmoset-feature-workflows.md`, and the target plan; worktree checked clean aside from this untracked plan file.
- 2026-04-06 02:02 CEST: Reconstructed implementation into test-first slices matching the commit plan: T0 export edit helpers and focused helper tests, T1 CodeLens plus execute-command plumbing and LSP server tests, T2/T3 dependent counting plus stale-command hardening and focused project/server tests.
- 2026-04-06 02:06 CEST: Phase T0 completed: added `tools/lsp/lib/export_edits.ml` with exportable-declaration discovery plus minimal export-header edit generation covering append/create/remove/delete-empty cases and no-op visibility requests.
- 2026-04-06 02:06 CEST: Focused verification for Phase T0 passed with `make unit lsp`.
- 2026-04-06 02:13 CEST: Phase T1 completed: added `tools/lsp/lib/code_lens.ml`, advertised CodeLens plus `marmoset.setExportVisibility`, reused cached `latest`/`last_good` analysis policy for CodeLens requests, and wired execute-command handling through `workspace/applyEdit`.
- 2026-04-06 02:13 CEST: Focused verification for Phase T1 passed with `make unit lsp`.
