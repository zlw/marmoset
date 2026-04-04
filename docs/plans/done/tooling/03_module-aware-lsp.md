# Module-Aware LSP Navigation And Completion

## Maintenance

- Last verified: 2026-04-04
- Implementation status: Implemented
- Type: Historical implementation plan
- Update trigger: Any change to module discovery, import resolution, compiler analysis payloads, or `tools/lsp/lib/*` request handling.

## Context

Modules are now implemented and verified in the compiler, but the LSP is still shaped around the old single-file world.

The current codebase already has important pieces we should reuse instead of rebuilding them in the editor layer:

- `lib/frontend/compiler.ml` can analyze module projects, preserve both `active_file.surface_program` and `active_file.typed_program`, and carry `type_map`, `environment`, `symbol_table`, and `identifier_symbols` for module entries.
- `lib/frontend/compiler.ml` already exposes value-definition helpers such as `find_active_file_symbol`, `find_checked_module_by_file`, and `find_export_binding`.
- `lib/frontend/import_resolver.ml` already computes the exact namespace tree and direct-import binding map we need for `math.add`, `import math.add`, aliases, and import-name collision semantics.
- `lib/frontend/discovery.ml` already owns the filesystem-to-module-id rules, including ambiguity and visibility behavior.

The LSP currently fails to capitalize on that compiler state:

- `tools/lsp/lib/server.ml` advertises hover, document symbols, inlay hints, semantic tokens, signature help, code actions, and completion, but it does not advertise or handle go-to definition.
- `tools/lsp/lib/server.ml` intentionally returns `None` on `"."` completion triggers, so imported module namespaces do not complete at all.
- `tools/lsp/lib/doc_state.ml` calls `Compiler.analyze_entry_with_source`, but `expose_typed_state` hides typed state unless analysis mode is `Standalone`, so module files lose the richer information that hover, inlay hints, semantic tokens, signature help, code actions, and completion should be using.
- `tools/lsp/lib/completions.ml` simply dumps `Infer.type_env` plus keywords. In module mode that would leak rewritten internal names such as `math__add`, so the current implementation is not just incomplete, it is the wrong abstraction.
- The LSP server caches each open document independently. Once modules exist, that cache is stale as soon as an imported open document changes.
- `Discovery.discover_project_with_entry_source` only overrides the entry file source. Unsaved imported files opened in the editor are invisible to analysis from their importers.
- `lib/frontend/syntax/parser.ml` requires an identifier after both import-path dots and expression dots, so `import math.` and `math.` are parse-incomplete states. Completion for those edits cannot depend on a fresh AST or fresh typed analysis always being available.

This milestone now lives in `docs/plans/done/tooling/` because the module-aware LSP work has been implemented and verified.

## Goals

1. Add reliable module-aware go-to definition for the highest-value cases:
   local values, direct imports, namespace-qualified exported members, aliases, and import headers.
2. Make existing type-aware LSP features work in module files without exposing rewritten internal names, including hover, inlay hints, semantic tokens, signature help, and type-driven code actions.
3. Add module/import completion that understands module paths, exported members, aliases, and namespace-qualified expressions, including syntactically incomplete import/header edits.
4. Make module analysis respect unsaved open files across the workspace, not only the active entry document.
5. Keep all user-facing labels and navigation targets in surface syntax and real file locations.

## Non-Goals

1. No rename, references, call hierarchy, or implementation search in this milestone.
2. No generic field/method completion for arbitrary value receivers. This plan only enables dot-completion when the receiver is a module namespace.
3. No full workspace symbol indexer or background project daemon.
4. No new module semantics, visibility rules, or import ambiguity rules.
5. No broad type-annotation navigation pass for every bare type token. This milestone focuses on value/import/navigation surfaces that already have stable source spans.

## Current State

### Compiler Boundary

- `lib/frontend/compiler.ml` already proves that module-mode analyses keep typed state:
  `analyze_entry_with_source keeps surface entry AST alongside typed module project` asserts `type_map <> None` and `environment <> None` in module mode.
- Imported value provenance already survives rewriting:
  `analyze_entry_with_source preserves imported definition provenance for symbol lookup` proves that a direct-imported `add` call in `main.mr` resolves to the exported definition span in `math.mr`.
- The compiler currently discards one critical artifact before the LSP can use it:
  `Import_resolver.rewrite_result.resolved_imports`.
  That artifact already contains:
  - `namespace_roots` for namespace chains,
  - `direct_bindings` for direct imports and aliases,
  - `direct_modules` for visibility-scoped impl/type-impl loading.

### LSP Boundary

- `tools/lsp/lib/server.ml` caches only `Doc_state.analysis_result` per URI and has no cross-document invalidation story.
- `tools/lsp/lib/doc_state.ml` keeps `compiler_analysis`, but it suppresses `type_map`, `environment`, and user generic names for module-mode callers.
- `tools/lsp/lib/hover.ml`, `tools/lsp/lib/inlay_hints.ml`, `tools/lsp/lib/semantic_tokens.ml`, `tools/lsp/lib/signature_help.ml`, and `tools/lsp/lib/code_actions.ml` are surface-AST driven enough to benefit from module typed state once it is exposed.
- `tools/lsp/lib/completions.ml` cannot be reused as-is for modules because top-level module bindings are rewritten to internal names in the environment.
- Bare-dot completion cannot rely on the parser alone, because `parse_import_decl` and `parse_dot_expression` both require an identifier after `.` and therefore reject the intermediate editor states where completion is most valuable.
- The current test surface is good enough to support this work:
  - compiler inline tests already cover module provenance,
  - `tools/lsp/lib/doc_state.ml` already has temp-project helpers,
  - `tools/lsp/lib/editor_scenarios.ml` and `tools/lsp/lib/edge_case_tests.ml` already exercise LSP logic with inline tests.

### Language/Test Surface To Reuse

- `test/fixtures/modules/*.mr` already cover namespace imports, direct imports, aliases, nested modules, and type/trait/shape imports.
- `test/fixtures/modules_edge/*.mr` already cover ambiguity, private exports, collisions, shadowing, and impl visibility.
- Those fixtures should directly inform LSP temp-project tests so the editor plan tracks the real language surface.

## Design Decisions

### 1. Reuse Compiler-Owned Navigation Metadata

The LSP must not build a second symbol/index model beside the compiler. Instead:

- persist `resolved_imports` for each checked module during `compile_project`,
- keep enough module-surface metadata to answer exported-member questions without reparsing the whole project in the LSP,
- expose small helper functions from `lib/frontend/compiler.ml` for:
  - current-module navigation metadata,
  - file-path/source lookup for modules already in the active project,
  - exported member lookup by module id and surface name.

Preferred shape:

```ocaml
type module_navigation = {
  surface : Import_resolver.module_surface;
  resolved_imports : Import_resolver.resolved_imports;
}

type checked_module = {
  module_id : string;
  file_path : string;
  program : AST.program;
  result : Checker.typecheck_result;
  type_var_user_names : (string * string) list;
  signature : Module_sig.module_signature;
  locals : Module_sig.module_locals;
  navigation : module_navigation;
}
```

The exact record name can change during implementation, but the invariant should not: namespace and direct-import resolution must cross the compiler/LSP boundary as compiler-owned facts, not be recomputed ad hoc from editor heuristics.

### 2. Add Multi-Document Source Overrides

Module-aware LSP is wrong unless unsaved imported files participate in the importer's analysis.

Generalize discovery/compiler entrypoints so they accept a whole-source override table, not just the entry file:

```ocaml
val discover_project_with_overrides :
  ?source_root:string ->
  entry_file:string ->
  source_overrides:(string, string) Hashtbl.t ->
  unit ->
  (Module_context.module_graph, Diagnostic.t) result

val analyze_entry_with_overrides :
  ?source_root:string ->
  ?force_modules:bool ->
  entry_file:string ->
  entry_source:string ->
  source_overrides:(string, string) Hashtbl.t ->
  unit ->
  entry_analysis
```

`Discovery.discovery_state` already has `source_overrides`; the public API should expose that capability cleanly instead of keeping it entry-file-only.

Override-table keys must be normalized absolute filesystem paths, not raw document URIs, so discovery/import lookup sees the same file ids that the compiler already uses.

### 3. Keep Surface Syntax As The Editor Truth

The user must never see internal names like `math__add` in completions, hover, or navigation labels.

Implementation rules:

- The surface AST remains the cursor-classification source.
- Typed/project metadata is used only to answer semantic questions.
- Completion items should be built from surface names plus type details recovered through internal-name mappings.
- Definition targets should always resolve to real file/span locations or to a file-start location for modules.

This implies a split completion model:

- use raw `environment` only for locals/builtins whose names are already surface-safe,
- use module navigation metadata for top-level declarations, direct imports, and namespace exports.

### 4. Import-Header Cursor Classification And Completion Stay LSP-Local

`AST.ImportDecl` carries only the path segments and alias text, not per-segment source spans. We should not widen the AST just for this tooling milestone.

Instead, add a small lexer-backed helper in `tools/lsp/lib/` that can classify import-header cursors from either:

- `(source, stmt.pos, stmt.end_pos, cursor_offset)` for already-parsed imports,
- `(source, cursor_offset)` for syntactically incomplete import lines such as `import math.`.

It should recover:

- which import path segment is under the cursor,
- whether the cursor is inside the alias,
- the typed prefix before the cursor,
- the replacement range for import completion.

This is consistent with the current `hover.ml` approach, which already lexes declaration headers for precise token ranges.

### 5. Dot Completion Is Context-Sensitive And Must Survive Incomplete Edits

The existing `"."` trigger should remain unsupported for arbitrary value receivers, but it should work for imported namespaces.

Because bare `math.` does not parse, the server should keep both:

- the latest analysis result for diagnostics/current source,
- the most recent successful typed analysis for the same open document.

Use the same classifier direction already enforced by `Import_resolver.chain_segments_of_expr` and `resolve_namespace_member`:

- on parsed `math.ad|`-style expressions, use the current surface AST plus navigation metadata,
- on bare-dot `math.|` edits, recover receiver segments lexically from the current source and use the last successful analysis only to decide whether the root is a visible namespace or a shadowing value,
- if the receiver chain root is a visible value binding, do not return module completions,
- if the receiver chain resolves through `navigation.resolved_imports.namespace_roots`, return namespace/module-export completions,
- otherwise return `None` and preserve the current "field/method completion not implemented" behavior.

That avoids inventing a fake generic member-completion feature while still making module namespaces useful, while remaining conservative when the current edit is temporarily unparsable.

### 6. Module Targets Are Files, Not Synthetic Symbols

Because Marmoset modules are file-defined and have no `module` keyword, go-to definition for a module path segment should jump to the imported file itself:

- module target => URI for the module file, zero-length range at byte offset `0`,
- exported member target => member definition span from `Module_sig.definition_site`,
- local/direct-imported value target => symbol definition span from `Infer.symbol`.

This keeps navigation behavior stable and easy to explain.

### 7. Cache Invalidation Must Follow Resolved Project Roots

Per-document caching is not sufficient once open documents can import one another.

Add workspace state to `tools/lsp/lib/server.ml`:

```ocaml
type open_doc = {
  text : string;
  file_path : string option;
}

type cached_doc = {
  latest : Doc_state.analysis_result;
  last_good : Doc_state.analysis_result option;
}

open_docs : (Lsp_t.DocumentUri.t, open_doc) Hashtbl.t
analysis_cache : (Lsp_t.DocumentUri.t, cached_doc) Hashtbl.t
```

On open/change/close:

- normalize file-backed URIs through `Lsp_t.DocumentUri.to_path` plus the same path normalization that `Discovery` uses,
- update `open_docs`,
- rebuild the override table for file-backed open docs only,
- choose an explicit `source_root` for each file-backed doc and pass it through `Doc_state` into compiler/discovery entrypoints,
- invalidate cached analyses that share the same project root as the changed document,
- preserve `last_good` when the newest edit only yields parse diagnostics or otherwise loses typed state,
- reanalyze the changed document immediately/debounced,
- opportunistically refresh other open docs in the same project root so diagnostics do not stay stale forever.

`Compiler.entry_analysis` and `Doc_state.analysis_result` should expose the resolved project root actually used for discovery. Do not re-derive that root inside the LSP from `Filename.dirname active_file.file_path`; that heuristic breaks for nested modules opened directly.

### 8. Import Completion Needs A Module Catalog, Not Just The Reachable Graph

The active compiler graph only contains the transitive import closure of the current entry module. Import completion must also suggest not-yet-imported siblings.

Introduce a new `lib/frontend/module_catalog.ml` helper that can:

- reuse `analysis.graph.modules` first for already-reachable modules,
- enumerate `.mr` files under the resolved project root,
- map them to module ids using `Discovery.module_id_of_file`,
- lazily parse candidate files to recover export lists and coarse binding kinds,
- honor open-document source overrides when the candidate module is currently unsaved in the editor.

Keeping this helper in `lib/frontend/` avoids teaching the LSP a second filesystem-to-module-id implementation and keeps future non-LSP tooling free to reuse the same catalog.

For off-graph modules, the catalog should stop at coarse export shape. Do not typecheck not-yet-imported modules just to decorate completion items; omit `detail` when no trusted typed signature is available.

## Implementation Phases

### Phase T0: Project-Aware Module Analysis Foundation

**Goal:** Make module analyses reusable and correct for an LSP server with multiple open documents.

**Files likely touched**

- `lib/frontend/discovery.ml`
- `lib/frontend/compiler.ml`
- `lib/frontend/import_resolver.ml`
- `tools/lsp/lib/doc_state.ml`
- `tools/lsp/lib/server.ml`

**Concrete work**

1. Generalize discovery/compiler entrypoints to accept a source-override table for all open file-backed documents.
   Also thread explicit `source_root` and normalized override keys through `Doc_state.analyze_with_file_id`, rather than recomputing those inputs inside the server on every feature path.
2. Persist `resolved_imports` and current-module surface/navigation data inside checked-module/project analysis results.
3. Keep `compiler_analysis` available in `Doc_state.analysis_result` and expose the compiler-owned resolved `project_root`.
4. Stop suppressing typed state for module-mode callers in `Doc_state`.
   This should be safe because:
   - hover/inlay/semantic-token/signature-help/code-action logic is driven primarily by surface expr ids and type maps,
   - completion will stop iterating raw env bindings blindly.
5. Extend the server to maintain file-backed `open_docs`, `last_good` analyses, and invalidate/recompute analyses across related open documents, not just the changed URI.

**Required invariants**

1. An analysis of `main.mr` must see unsaved edits from an open imported `math.mr`.
2. Cached analysis for `main.mr` must be invalidated when `math.mr` changes.
3. Module-mode typed state exposed through `Doc_state` must still use surface AST ids/ranges, not rewritten display names.
4. Compiler navigation metadata must be derived from the same rewrite/import pass that the compiler already trusts for codegen and typechecking.
5. Override tables must be keyed by normalized file paths that match discovery/compiler file ids exactly; `file://` URI spelling must not cause misses.
6. When an explicit project root is known, opening `collections/list.mr` directly must keep the module id `collections.list`, not silently reinterpret it as `list`.

**Tests**

- `lib/frontend/compiler.ml`
  - add a temp-project test where `main.mr` imports `math.mr`, then `math.mr` is overridden in memory and `main.mr` analysis sees the override.
  - add a regression test that `checked_module.navigation.resolved_imports` preserves alias and namespace data for `import math` / `import math.add as plus`.
  - add a nested-module test proving explicit `source_root` keeps `collections/list.mr` mapped to module id `collections.list` when analyzed directly.
- `tools/lsp/lib/doc_state.ml`
  - module-mode analysis returns `type_map <> None` and `environment <> None`.
  - `project_root` is populated for file-backed module analyses.
  - imported open-file overrides change diagnostics/types for the active importer.
  - file-backed analysis uses normalized override keys rather than raw URIs.
- `tools/lsp/lib/server.ml` or new focused server tests
  - changing one open module invalidates or refreshes analyses for sibling open documents in the same project.
  - parse-broken latest edits keep a `last_good` snapshot for later completion requests.

### Phase T1: Module-Mode Feature Parity For Existing Typed LSP Features

**Goal:** Let the existing hover/inlay/semantic-token/signature-help/code-action stack work in module files before adding new requests.

**Files likely touched**

- `tools/lsp/lib/doc_state.ml`
- `tools/lsp/lib/server.ml`
- `tools/lsp/lib/hover.ml`
- `tools/lsp/lib/inlay_hints.ml`
- `tools/lsp/lib/semantic_tokens.ml`
- `tools/lsp/lib/signature_help.ml`
- `tools/lsp/lib/code_actions.ml`
- `tools/lsp/lib/editor_scenarios.ml`

**Concrete work**

1. Audit which current features only needed `type_map`/surface AST and should start working automatically once module typed state is exposed.
2. Fix any module-specific regressions where rewritten internal names or rewritten top-level env lookups would leak into user-facing strings.
3. Keep `document_symbols` surface-only; it is already safe because it walks the surface AST.
4. Do not try to solve import-header hovers or generic type-position navigation in this phase.

**Required invariants**

1. Hover in module files must render surface names and canonical source syntax, never internal mangled names.
2. Signature help and add-type-annotation code actions in module files must keep using surface names and canonical source syntax.
3. Semantic tokens must continue to classify declarations/uses correctly in mixed single-file and module projects.
4. Existing standalone editor tests must continue to pass unchanged.

**Tests**

- extend `tools/lsp/lib/editor_scenarios.ml` with a temp-project module scenario:
  - hover on `add` from `import math.add`,
  - hover on `math.add` in a namespace import,
  - hover on alias usage from `import math.add as plus`.
- add focused module-mode tests in `tools/lsp/lib/signature_help.ml` covering direct-import and namespace-qualified calls.
- add focused module-mode tests in `tools/lsp/lib/code_actions.ml` ensuring inserted annotations stay in surface syntax for module files.
- extend `tools/lsp/lib/edge_case_tests.ml` with a module-mode regression ensuring semantic tokens still succeed on imported namespace uses.

### Phase T2: Go-To Definition

**Goal:** Add reliable definition lookup for the value and import surfaces that matter most now that modules exist.

**Files likely touched**

- `tools/lsp/lib/server.ml`
- `tools/lsp/lib/lsp_utils.ml`
- `tools/lsp/lib/doc_state.ml`
- `tools/lsp/lib/definition.ml` (new)
- `tools/lsp/lib/import_header.ml` (new)

**Supported targets in this milestone**

1. Local value bindings and parameters.
2. Direct-imported values and aliases:
   - `import math.add`
   - `import math.add as plus`
   - use-sites `add(...)` / `plus(...)`
3. Namespace-qualified exported members:
   - `math.add(...)`
   - `collections.list.map(...)`
4. Import headers themselves:
   - cursor on a module path segment => target module file,
   - cursor on the final exported-member segment of a direct import => exported definition site,
   - cursor on an alias => imported target, not a no-op jump to the alias token.

**Explicitly deferred**

- broad type-annotation navigation for bare `Point`/`Drawable` tokens outside import headers,
- generic references/implementation lookup,
- unresolved/ambiguous/private imports returning anything other than `None`.

**Concrete work**

1. Advertise `definitionProvider` in `server.ml`.
2. Add a definition request handler that:
   - converts LSP position to offset,
   - classifies whether the cursor is in an import header or expression surface,
   - uses `Compiler.find_active_file_symbol` for local/direct-imported value uses,
   - uses persisted `resolved_imports` plus `Import_resolver.resolve_namespace_member` for namespace-qualified targets,
   - converts `Infer.symbol` / `Module_sig.definition_site` / module-file targets into LSP locations.
3. Reuse `analysis.graph.modules` as the preferred source store for target files already in the project, and fall back to reading the file from disk only when needed for range conversion.

**Recommended helper shape**

```ocaml
type definition_target =
  | File_start of string
  | Span of {
      file_path : string;
      start_pos : int;
      end_pos : int;
    }

val find_definition :
  analysis:Doc_state.analysis_result ->
  line:int ->
  character:int ->
  definition_target option
```

**Required invariants**

1. `import math.add` usage jumps to the `add` definition in `math.mr`, not to a rewritten internal call site.
2. `math.add` supports two meaningful targets:
   - cursor on `math` => `math.mr` file start,
   - cursor on `add` => exported definition span.
3. Shadowing still obeys language semantics: a local value named `math` wins over module namespace lookup.
4. Ambiguous/private/broken imports do not fabricate bogus targets.

**Tests**

- new inline tests in `tools/lsp/lib/definition.ml` using `Doc_state.with_temp_project`:
  - direct import usage resolves to exported value span,
  - alias usage resolves to underlying export span,
  - namespace receiver resolves to module file start,
  - namespace member resolves to exported span,
  - shadowed namespace returns local binding target,
  - ambiguous/private import headers return `None`.
- one compiler/LSP regression test proving expr-id preservation still allows surface cursor lookup after import rewriting.

### Phase T3: Module And Import Completion

**Goal:** Make completion useful in module code without leaking compiler-internal names.

**Files likely touched**

- `tools/lsp/lib/completions.ml`
- `tools/lsp/lib/server.ml`
- `tools/lsp/lib/doc_state.ml`
- `tools/lsp/lib/import_header.ml`
- `lib/frontend/module_catalog.ml` (new)

**Completion contexts**

```ocaml
type completion_context =
  | BareIdentifier of { prefix : string }
  | NamespaceMember of {
      receiver_segments : string list;
      prefix : string;
    }
  | ImportPath of {
      typed_segments : string list;
      prefix : string;
      in_alias : bool;
    }
  | Unsupported
```

**Behavior**

Context classification must not require the whole document to parse. `ImportPath` should be source-only, and `NamespaceMember` should be recoverable either from a parsed AST or from a narrow lexical receiver scan on bare-dot edits.

1. `BareIdentifier`
   - include locals/params/builtins from the environment,
   - include current-module top-level surface names by mapping declaration names through `member_presence.internal_name`,
   - include direct imports and aliases from `resolved_imports.direct_bindings`,
   - exclude raw internal names from the final item list.
2. `NamespaceMember`
   - when the current document parses, classify receiver segments through the surface AST and `Import_resolver.chain_segments_of_expr`,
   - on bare-dot edits, recover receiver segments lexically from the current source and consult `last_good` only for root binding kind / namespace visibility,
   - when the receiver is a visible module namespace, offer child modules and exported members,
   - when the receiver is a value or there is no trustworthy semantic snapshot, preserve current behavior and return `None`.
3. `ImportPath`
   - classify import context from source text even when the current buffer does not parse,
   - offer child module path segments by scanning the project root,
   - once a prefix resolves to a module file, offer exported members from that module using either the active graph or the lightweight module catalog,
   - if the same label is both a child module and an exported member, emit one completion item whose detail marks the ambiguity instead of silently hiding one meaning,
   - keep alias editing (`as Foo`) out of the path-completion replacement range.

**Completion item requirements**

1. `label` must be the surface name.
2. `kind` should reflect the binding class:
   - module => `Module`
   - value/function => `Function` or `Variable`
   - type/enum => `Struct` or `Enum`
   - trait/shape => `Interface`
3. `detail` should use `Source_syntax` formatting when a trusted typed signature is available; off-graph catalog results may omit `detail`.
4. `sortText` should keep keywords after semantic items, as the current completion implementation already does.

**Required invariants**

1. No completion item may expose a mangled internal name.
2. `.` completion only activates for namespace contexts, not arbitrary values.
3. `import math.` and similar parse-incomplete states must still produce import-path completions.
4. Bare `math.` completion may consult `last_good`, but if namespace-vs-value facts are not trustworthy it must return `None` rather than guess.
5. Import completion must respect the same module-id/path rules as `Discovery`.
6. Unsaved open module files must influence export completions when they are the completion target.

**Tests**

- `tools/lsp/lib/completions.ml`
  - current-module top-level values are surfaced with source names, not internal names,
  - direct-import alias completions include the alias and omit the internal target name,
  - namespace completion returns exported members for parsed `math.ad|` expressions and for bare-dot `math.|` when `last_good` is available,
  - dot completion on a local record value still returns `None`,
  - import-path completion suggests sibling module segments from the project root even when the buffer currently ends in `.` and does not parse,
  - import completion surfaces exported members and ambiguity details without forcing ad hoc typechecking of off-graph modules.
- add multi-file temp-project scenarios for:
  - `import math.|` => `add`, `Point`, `HasXY`,
  - `import collections.|` => `list`,
  - `math.|` after `import math` => exported members only,
  - open unsaved target module changes export completion immediately.

## Testing Strategy

1. Keep this effort unit-test heavy. The repo already relies on inline OCaml tests in the touched modules, and that is the right granularity for navigation/completion edge cases.
2. Reuse real module fixtures and temp-project helpers instead of synthetic one-off strings whenever filesystem/module-id behavior matters.
3. Verify these boundaries explicitly:
   - override table behavior across multiple open docs,
   - explicit root selection for nested modules,
   - cache invalidation after imported-file edits,
   - parse-broken completion contexts that must fall back to source-only or `last_good` logic,
   - namespace-vs-value precedence,
   - ambiguity/private-export handling,
   - absence of leaked internal names.
4. Focused verification during implementation should include at least:
   - targeted inline tests in `lib/frontend/compiler.ml`,
   - targeted inline tests in `tools/lsp/lib/doc_state.ml`,
   - targeted inline tests in the new definition/completion helpers,
   - `make unit` before claiming the milestone is ready for review.

## Commit Plan

1. Compiler/discovery foundation:
   source overrides for multiple files, resolved project roots, persisted module navigation metadata, and compiler tests.
2. LSP document/workspace state:
   open-doc path normalization, project-root invalidation, `last_good` cache handling, module-mode typed-state exposure, and doc-state/server tests.
3. Definition provider:
   server capability/handler, definition helper module, import-header token classifier, and focused tests.
4. Completion provider:
   surface-name completion engine, parser-tolerant import/dot context classification, module catalog/import completion, namespace dot completion, and focused tests.
5. Final hardening:
   edge-case regressions, cleanup, and `make unit`.

## Risks And Open Questions

### Main Risks

- Raw module-mode environments contain internal names. If completion code iterates them directly, the feature will regress immediately.
- Cross-document invalidation can become too eager if every change nukes all caches. Keep invalidation scoped by resolved project root.
- Parse-incomplete namespace completion can become unsound if it guesses from stale state. Restrict `last_good` usage to root binding-class checks and return `None` when the snapshot is not trustworthy.
- Import-header token classification is intentionally lexer-based. It must stay narrow and well-tested so it does not become a second parser.

### Chosen Tradeoffs

- This plan deliberately does not widen the AST with per-import or per-type-expression spans. That keeps the milestone tooling-focused and avoids parser/lowering churn across the whole compiler.
- Type-position definition for every bare annotation token is deferred because `type_expr` currently has no source spans. The initial milestone should solve the module-navigation surfaces with the highest user value first.
- Import completion requires a module catalog outside the active import closure. That is extra work, but without it the feature would only complete modules that are already imported, which defeats the point.
- Off-graph module-catalog entries only need coarse export shape. The plan intentionally avoids typechecking arbitrary sibling modules just to attach completion detail text.

## Progress

- 2026-04-04 03:37 CEST: Draft plan created in `docs/plans/done/tooling/03_module-aware-lsp.md`; awaiting `plan-review`.
- 2026-04-04 03:48 CEST: Review pass started against the draft plus current compiler/LSP codepaths.
- 2026-04-04 03:52 CEST: Cross-tool Claude review attempt stalled; skipped per user direction and continued with single-model adversarial review.
- 2026-04-04 03:52 CEST: Plan revised after review to cover parser-tolerant completion, explicit root/path normalization, last-good analysis caching, signature-help/code-action parity, and off-graph module-catalog limits.
- 2026-04-04 03:52 CEST: Final review completed; no unresolved questions remain that block implementation.
- 2026-04-04 04:00 CEST: Test-first slice started for Phase T0 foundation: compiler/discovery override tables, compiler-owned project roots/navigation metadata, module-mode typed-state exposure, and server `last_good` cache plumbing.
- 2026-04-04 04:07 CEST: RED observed for the T0 slice via focused `dune runtest lib/frontend tools/lsp/lib --no-buffer -j 1`; failures begin at missing compiler/doc-state contracts such as `entry_analysis.project_root`, override-aware analysis entrypoints, and navigation metadata on checked modules.
- 2026-04-04 04:18 CEST: T0 foundation implementation green with compiler/doc-state/server changes for override-aware analysis, compiler-owned project roots, persisted navigation metadata, module-mode typed state exposure, and `last_good` cache updates.
- 2026-04-04 04:18 CEST: Focused tests green for the T0 slice via `dune runtest lib/frontend tools/lsp/lib --no-buffer -j 1`.
- 2026-04-04 04:19 CEST: Commit created for the T0 foundation slice after a clean rerun of `dune runtest lib/frontend tools/lsp/lib --no-buffer -j 1`.
- 2026-04-04 04:21 CEST: Test-first slice started for Phase T1 module-mode typed-feature parity: hover, signature help, semantic tokens, and add-type-annotation code actions in module files.
- 2026-04-04 04:24 CEST: RED observed for the T1 slice via `dune runtest tools/lsp/lib --no-buffer -j 1`; the remaining gaps were namespace-qualified call labels in `signature_help` and imported nominal type formatting in module-file code actions.
- 2026-04-04 04:29 CEST: T1 typed-feature parity implementation green with module-mode signature-help, code-action, semantic-token, and hover scenario coverage wired through compiler-owned surface metadata.
- 2026-04-04 04:29 CEST: Focused tests green for the T1 slice via `dune runtest tools/lsp/lib --no-buffer -j 1`.
- 2026-04-04 04:34 CEST: Test-first slice started for Phase T2 go-to-definition: import-header classification, module-file jump targets, direct-import alias/value lookup, and namespace-qualified member resolution.
- 2026-04-04 04:36 CEST: RED observed for the T2 slice via `dune runtest tools/lsp/lib --no-buffer -j 1`; missing behavior is isolated to the new definition helper for direct imports, namespace chains, shadowing, and import-header alias/member targets.
- 2026-04-04 04:45 CEST: T2 go-to-definition implementation green with import-header token classification, lexical namespace-chain fallback, compiler-owned definition-target conversion, and server `definitionProvider` wiring.
- 2026-04-04 04:45 CEST: Focused tests green for the T2 slice via `dune runtest tools/lsp/lib --no-buffer -j 1`.
- 2026-04-04 04:47 CEST: Test-first slice started for Phase T3 completion: module catalog scanning, source-only import-path classification, namespace dot completion, and last-good fallback for parse-broken module edits.
- 2026-04-04 04:48 CEST: RED observed for the T3 slice via `dune runtest tools/lsp/lib --no-buffer -j 1`; the remaining gaps are advanced completion contexts for current-module names, direct-import aliases, namespace receivers, trailing-dot import paths, and source-override-aware export catalogs.
- 2026-04-04 05:08 CEST: T3 completion implementation green with source-only current-module/import navigation fallback, parser-tolerant project-root recovery for import catalogs, and override-aware export completion for parse-broken module edits.
- 2026-04-04 05:08 CEST: Focused tests green for the T3 slice via `dune runtest lib/frontend tools/lsp/lib --no-buffer -j 1`.
- 2026-04-04 05:11 CEST: Commit created for the T3 completion slice after the focused frontend/LSP suites passed cleanly.
- 2026-04-04 05:14 CEST: Final broader verification completed via `make unit`; no regressions were observed across the unit suites.
- 2026-04-04 06:37 CEST: Moved the completed milestone to `docs/plans/done/tooling/03_module-aware-lsp.md` and updated `docs/ROADMAP.md` after PR review feedback.
- 2026-04-04 06:39 CEST: Post-PR hygiene verification green via `make ci-quality-lint-fmt-doc` and `make unit`.
