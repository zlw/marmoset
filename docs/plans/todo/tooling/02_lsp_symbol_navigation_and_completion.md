# Broaden LSP Symbol Navigation And Completion

## Maintenance

- Last verified: 2026-04-05
- Implementation status: Completed
- Type: Follow-on tooling plan
- Update trigger: Any change to `lib/frontend/syntax/{surface_ast,parser,lower}.ml`, compiler analysis payloads, import resolution, method/constructor metadata, or `tools/lsp/lib/{definition,completions,doc_state}.ml`.

## Context

`docs/plans/done/tooling/03_module-aware-lsp.md` implemented the module-aware LSP baseline: module/import go-to-definition, direct-import and namespace completion, cross-document source overrides, and the first pass at surface-safe labels.

That milestone intentionally stopped short of broad symbol navigation and completion:

- `tools/lsp/lib/definition.ml` resolves local value symbols, direct imports, module aliases, namespace-qualified module exports, and import-header segments, but not bare type names, trait names, shape names, constructor names, generic parameters, impl headers, derive headers, or qualified methods.
- `tools/lsp/lib/completions.ml` only distinguishes `BareIdentifier`, `NamespaceMember`, and `ImportPath`. It has no notion of type-position completion, constraint-position completion, constructor completion, or trait/type qualifier completion.
- The parser already has a separate `Surface_ast` layer in `lib/frontend/syntax/surface_ast.ml`, but the compiler/discovery pipeline discards that layer and keeps only lowered `Ast.AST.program`.
- `Ast.AST.type_expr`, `variant_def`, `method_sig`, `method_impl`, and import path segments are spanless, so the current LSP falls back to local lexer tricks in places like `tools/lsp/lib/hover.ml` and `tools/lsp/lib/import_header.ml`.

The next LSP milestone should not stack more lexer-only heuristics on top of that gap. The long-term fix is to preserve structured source-aware syntax for editor features, then make go-to-definition and completion consume compiler-owned navigation facts.

Because this repo has no user-facing compatibility obligation for internal LSP plumbing, the implementation should prefer deletion over coexistence whenever a new path supersedes an old one.

## Goals

1. Implement go-to-definition for all symbol classes with stable, user-meaningful definition sites:
   local values, top-level functions, imported values, modules, named types, transparent type aliases, enums, shapes, traits, constructors, generic type parameters, import aliases, qualified trait methods, and inherent methods.
2. Implement semantic completion for the same language surface in all relevant contexts:
   expression positions, type annotation positions, constraint positions, qualified roots/members, and import headers.
3. Keep all labels and locations in surface syntax and real file positions, never rewritten internal names.
4. Reuse compiler-owned project state so open unsaved files, imports, and module visibility behave the same way in definition/completion as they do in diagnostics and hover.
5. Use one principled source model for editor features. Lexer recovery remains allowed only for truly incomplete edits that cannot produce an AST yet, such as trailing-dot states.

## Non-Goals

1. No rename, references, call hierarchy, implementation search, or workspace-symbol index in this milestone.
2. No generic record-field completion or record-field go-to-definition on arbitrary value receivers.
3. No background daemon or incremental parser project. This work stays inside the existing analyze-on-request model.
4. No synthetic definitions for primitive builtins like `Int`, `Str`, or `Unit` that have no real source file.
5. No second LSP-only resolver that recomputes module/import/type visibility independently from the compiler.
6. No backward-compatibility layer for obsolete LSP internals. This plan should replace old cursor/completion/definition paths, not preserve them behind aliases, feature flags, or fallback branches.
6. No backward-compatibility scaffolding that keeps old parsed-buffer definition/completion pipelines alive once the replacement path is green.

## In-Scope Symbol Surface

This plan treats "everything that makes sense" as the following concrete surface:

- Values:
  local `let` bindings, top-level `let` bindings, top-level `fn` declarations, lambda parameters, imported values, imported aliases.
- Types and constraints:
  named types, transparent aliases, enums, shapes, traits, generic type parameters, trait-object members inside `Dyn[...]`, shorthand constraint names in parameter annotations, generic constraints, and trait supertraits.
- Constructor surfaces:
  bare wrapper-constructor heads like `UserId(...)`, and qualified sum variants like `Option.Some(...)` and `Option.Some` in patterns.
- Qualified roots and members:
  module namespaces (`math.add`), enum constructors (`Option.Some`), qualified trait calls (`Greeter.greet`), and qualified inherent calls (`Point.rename`).
- Header surfaces:
  import path segments, import aliases, `impl` heads, `derive` heads, parameter annotations, return annotations, type aliases, shape fields, trait method signatures, and constructor payload types.

Definition targets follow these rules:

- Modules jump to the start of the defining file.
- User-defined symbols jump to the exact source span of the definition name.
- Clicking a qualified root jumps to the root declaration:
  `Option` -> sum type, `Greeter` -> trait, `Point` -> type alias/named type.
- Clicking a qualified member jumps to the member declaration:
  `Some` -> constructor, `greet` -> trait method, `rename` -> inherent method.
- Clicking a declaration head returns that declaration's own span rather than `None`.
- Qualified trait method calls jump to the trait method signature, not an arbitrary impl.
- Resolved inherent method calls jump to the concrete or generic inherent method definition when unique.
- Shape names are navigable only in constraint positions; `Shape.member` is not a valid member-namespace surface.
- Primitive builtins with no source remain non-navigable.

## Current State

### LSP Definition And Completion

- `tools/lsp/lib/definition.ml` walks lowered expression trees and resolves:
  `Compiler.find_active_file_symbol`, `Compiler.find_export_binding`, `Import_resolver.resolve_namespace_member`, and import-header token scans.
- `tools/lsp/lib/completions.ml` builds items from:
  `Infer.type_env`, `checked_module.navigation.surface`, `resolved_imports.direct_bindings`, and module catalog scans.
- `tools/lsp/lib/import_header.ml` exists specifically because import path segments and aliases do not have AST-level source ranges.
- `tools/lsp/lib/hover.ml` already proves that token-level header parsing was needed as a workaround for missing structured spans in declaration/type surfaces.

### Syntax And Compiler Boundary

- `lib/frontend/syntax/parser.ml` already has `parse_surface_program` and public `parse_surface`, so the parser can produce a rich surface tree now.
- `lib/frontend/syntax/surface_ast.ml` keeps surface-only constructs, but its `surface_type_expr`, import data, variant data, method data, and declaration heads carry names only, not positioned name refs.
- `lib/frontend/syntax/lower.ml` lowers surface syntax into `Ast.AST`.
- `lib/frontend/discovery.ml` and `lib/frontend/module_context.ml` only preserve lowered `AST.program`.
- `lib/frontend/compiler.ml` exposes `checked_module.navigation` and `active_file.surface_program`, but that `surface_program` field is actually a lowered `AST.program`, not the parser surface tree.

### Available Semantic Metadata

- `lib/frontend/typecheck/module_sig.ml` already has `definition_site` for exported values, enums, named types, transparent aliases, shapes, and traits.
- `lib/frontend/import_resolver.ml` already records per-export/per-declaration presence facts for those same module-level symbol classes.
- `lib/frontend/typecheck/infer.ml` already records value symbol definitions and method resolution kinds.
- `lib/frontend/typecheck/resolution_artifacts.ml` already snapshots typed method signatures, but not method source sites.
- `lib/frontend/typecheck/enum_registry.ml` and `lib/frontend/typecheck/trait_registry.ml` know about constructors and methods semantically, but their exposed source metadata is not strong enough for precise LSP navigation today.
- The current language surface is stricter than a generic “constructor completion” label suggests:
  wrapper construction is bare (`UserId(42)`), sum variants stay qualified (`Option.Some(42)`), shapes are valid only in constraint positions, and `Dyn[...]` accepts traits only.

## Design Decisions

### 1. Use A Proper Source-Aware Surface AST, Not A Lexer-Only Sidecar

The main editor source of truth should be the parser surface tree, not ad hoc token rescans.

This plan widens `lib/frontend/syntax/surface_ast.ml` so it can carry positioned name references for all editor-relevant syntax. The exact record names can vary, but the shape should be close to:

```ocaml
type name_ref = {
  text : string;
  pos : int;
  end_pos : int;
  file_id : string option;
}

type surface_type_expr = {
  ste_desc : surface_type_expr_desc;
  ste_pos : int;
  ste_end_pos : int;
  ste_file_id : string option;
}
```

Use positioned refs for:

- import path segments and aliases,
- top-level declaration heads,
- type names and generic parameter names,
- trait supertraits and shorthand constraints,
- enum variant names,
- trait method names,
- inherent/impl method names,
- record type field names,
- derive trait names.

This is still an AST-first design, but it avoids forcing source spans through every core typechecker path. LSP is the consumer that needs precise surface positions; the semantic core can stay largely unchanged.

Additional invariants:

- parser-allocated expression ids must remain stable across the surface and lowered trees for nodes that survive lowering (`Identifier`, `Call`, `MethodCall`, `TypeApply`, `FieldAccess`, and other direct expression carriers),
- method ids (`method_sig_id`, `impl_method_id`) must continue to match the method metadata exposed after typechecking,
- source-aware import/type/header refs should be added in a way that does not change existing parse/lower semantics for codegen or typechecking.

### 2. Parse Once And Keep Both Surface And Lowered Trees

We should not double-parse files just to satisfy LSP.

Add a parser entrypoint that returns both the surface tree and the lowered tree from one parse run. `lib/frontend/syntax/parser.ml` already has the building blocks. The new public parser result should be used by:

- `lib/frontend/discovery.ml`
- `lib/frontend/module_context.ml`
- `lib/frontend/compiler.ml`
- `lib/frontend/module_catalog.ml`

Preferred compiler boundary changes:

```ocaml
type parsed_module = {
  module_id : string;
  file_path : string;
  source : string;
  surface_program : Surface.surface_program;
  program : AST.program;
  exports : string list;
  imports : import_info list;
}

type file_analysis = {
  ...
  surface_program : Surface.surface_program option;
  lowered_program : AST.program option;
  typed_program : AST.program option;
  ...
}
```

If `typed_program` remains identical to `lowered_program`, rename aggressively enough that future tooling code stops treating lowered AST as "surface".

`tools/lsp/lib/doc_state.ml` should preserve both trees explicitly:

- keep the existing `program`-style field for lowered AST consumers such as semantic tokens and signature help,
- add a separate `surface_program` field for definition/completion/navigation work,
- do not silently repurpose the current lowered `program` field into a surface tree,
- once callers are migrated, delete misleading compatibility aliases rather than keeping parallel “program means two things” access patterns.

Keeping both trees here is intentional, not backward-compatibility scaffolding: lowered AST remains the right substrate for typed/semantic features, while surface AST is the right substrate for source-precise navigation.

### 3. Split Cursor Classification From Semantic Lookup

Trying to make one new frontend module own both cursor positioning and semantic visibility is likely to create circular dependencies around `Compiler`, `Module_catalog`, and `Import_resolver`.

Use a split design instead:

- add a new LSP-local `tools/lsp/lib/cursor_context.ml` that consumes `Surface.surface_program` and returns only syntactic cursor facts:
  declaration heads, type refs, generic params, qualified roots/members, import refs, and wrapper-constructor call heads,
- keep semantic visibility, resolved imports, and project/module lookup in frontend/compiler helpers and existing compiler artifacts,
- build or cache any offset-to-scope tables in `Doc_state.analysis_result` or the server document cache, not inside `Compiler.checked_module`.

The cursor-context layer should build source-aware references from:

- `Surface.surface_program`,
- lowered `AST.program` when ids or typed artifacts must be joined back to semantic data,
- positioned import/header refs,
- a lexical `scope_index`.

Preferred cursor-classification boundary:

```ocaml
type cursor_context_input = {
  surface_program : Surface.surface_program;
  lowered_program : AST.program;
  scope_index : scope_index;
}
```

`cursor_context.ml` should answer two questions:

1. What syntactic symbol/reference/completion context is under the cursor?
2. Which lexical scope binds the cursor first?

It must not answer semantic visibility questions by itself.

The leaf cursor module should expose compiler-agnostic helpers, and `lib/frontend/compiler.ml` should expose semantic wrapper helpers for LSP consumption. For example:

```ocaml
val reference_at :
  source:string ->
  input:cursor_context_input ->
  offset:int ->
  reference option

val completion_context_at :
  source:string ->
  input:cursor_context_input ->
  offset:int ->
  completion_context option

val visible_symbols_at :
  analysis:Compiler.entry_analysis ->
  file_path:string ->
  scope:scope_lookup ->
  completion_context ->
  visible_symbol list
```

This keeps the AST-first cursor model while leaving symbol visibility compiler-owned.

### 4. No Legacy Dual-Path LSP Plumbing

This migration should be replacement-oriented, not compatibility-oriented.

When a new path is introduced, the superseded path should be removed in the same implementation slice unless it is still required for one narrow incomplete-buffer case explicitly listed in this plan.

Concrete deletion expectations:

- `tools/lsp/lib/completions.ml` should not keep the current `BareIdentifier` / `NamespaceMember` / `ImportPath` classifier alive in parallel with the new context model,
- `tools/lsp/lib/definition.ml` should not keep raw parsed-buffer import-header and namespace-chain scanners as first-class logic once `cursor_context` plus compiler semantic lookup is in place,
- `tools/lsp/lib/import_header.ml` should either be narrowed to parse-incomplete recovery only or deleted if `cursor_context` fully subsumes it,
- ambiguous analysis fields or helper names that still imply lowered AST is “surface” should be renamed and old spellings deleted once the migration commit is green,
- no feature flags, no compatibility booleans, and no “old LSP mode” branches should be added for this work.

The only acceptable surviving fallback is the narrow lexical recovery path for syntactically incomplete edits such as trailing-dot states. That is an editor-state necessity, not backward compatibility.

Acceptance rule:

- once the new cursor-context plus compiler-wrapper path is green for a feature slice, the superseded parsed-buffer path is deleted in that same slice.

### 5. Extend Source Metadata For Constructors And Methods

Broad symbol navigation is incomplete unless constructors and methods have definition sites.

Add explicit source metadata in the semantic layer where it is currently missing.

Important ownership rule:

- temporary global registries may carry source metadata while typechecking,
- the compiler must snapshot any constructor/method navigation metadata into per-project/per-module artifacts before returning `entry_analysis`,
- LSP code must not read global registries directly.

Add explicit source metadata in the semantic layer where it is currently missing:

- extend `lib/frontend/typecheck/enum_registry.ml` variant definitions with optional `Module_sig.definition_site`,
- extend `lib/frontend/typecheck/trait_registry.ml` method signatures with optional definition sites,
- extend `lib/frontend/typecheck/resolution_artifacts.ml` typed method artifacts with optional definition sites and enough ownership metadata to distinguish trait vs inherent method targets.

That metadata should be populated when lowering/typechecking project modules, then copied into per-analysis compiler artifacts rather than left resident only in global registries.

This enables:

- `Option.Some` -> variant definition,
- `UserId(42)` -> wrapper type definition,
- `Greeter.greet` -> trait method definition,
- `Point.rename` -> inherent method definition,
- `x.greet()` on a constrained generic receiver -> trait method definition,
- `point.rename()` on a resolved inherent call -> inherent method definition.

The compiler-facing navigation payload should include dedicated constructor/method indexes rather than forcing LSP to reverse-engineer those targets from registries:

- sum-constructor entries keyed by `(type_name, constructor_name)`,
- wrapper constructors resolved through the defining type binding rather than a synthetic member namespace,
- trait method entries keyed by `(trait_name, method_name)`,
- inherent method entries keyed by `(receiver_type_key, method_name)` or callable key,
- enough range data to jump both from roots and from members.

Preferred ownership boundary:

- module-local declarations live on `Module_sig.module_locals` or sibling compiler-owned module data,
- merged lookup tables live on per-analysis compiler artifacts,
- global registries remain implementation detail, not LSP API.

### 6. Completion Contexts Must Be Explicit

Replace the current three-way completion context split with source-aware contexts that reflect the language:

- `ValueIdentifier`
- `TypeIdentifier`
- `ConstraintIdentifier`
- `ImportPath`
- `ModuleMember`
- `EnumConstructorMember`
- `TraitMethodMember`
- `InherentMethodMember`
- `Unsupported`

Context classification rules:

- expression/bare identifier positions offer values, functions, visible module roots, direct imports, callable wrapper-constructor names such as `UserId`, and keywords,
- type positions offer builtin types, named types, transparent aliases, enums, and in-scope type parameters,
- constraint positions offer only visible traits and shapes,
- `Dyn[...]` offers only traits,
- sum-variant constructor names are never offered as bare identifiers; they are only offered after a qualifying sum root such as `Option.`,
- `Type.` roots split by kind:
  constructor-bearing sums offer constructors,
  any type with visible inherent impls may offer inherent methods,
  wrapper constructors remain bare call heads rather than `Type.member` items,
  transparent aliases and exact named records do not gain constructors,
  shapes do not produce member completion,
- `Trait.` roots offer trait methods,
- module namespace roots offer child module segments and exported members,
- import headers offer child module segments and exported members of a fully resolved module path.

No context should ever offer rewritten internal names.

### 7. Scope Tracking Is Explicit And Lexical

Generic-parameter navigation/completion is too vague unless the plan defines how scope is recovered.

Build a lexical `scope_index` from the surface tree with at least these scope classes:

- value scopes: top-level lets/fns, blocks, lambda params, method params,
- type scopes: type parameters from `type`, `trait`, `fn`, `impl`, inherent methods, trait methods,
- constraint scopes: traits and shapes visible in shorthand/constraint positions,
- declaration-head scopes: exact positioned refs for declaration names and import aliases.

The scope index must honor shadowing and containment by offset range. Completion and definition should resolve:

- nearest matching generic/type param first,
- then current-module declarations/imports,
- then visible project/module namespaces.

### 8. Qualified-Root Resolution Order Must Be Stable

`foo.bar` is ambiguous in this language surface. The LSP must resolve it using a fixed order that matches compiler reality:

1. If `foo` is a visible local/value symbol, treat the expression as a value receiver. Do not offer module/type namespace completions.
2. Else if `foo` is a visible imported/current module namespace, treat it as a module path.
3. Else if `foo` is a visible constructor-bearing sum type in a type-qualified position, offer sum constructors and any visible inherent methods.
4. Else if `foo` is a visible constructor-bearing wrapper type, offer inherent methods only; wrapper construction stays a bare root call, not a member result.
5. Else if `foo` is a visible exact named/transparent type with inherent impls, offer inherent methods only.
6. Else if `foo` is a visible trait in a trait-qualified position, offer trait methods.
7. Else if `foo` is a visible shape name, return no member result.
8. Else return no qualified-member result.

This prevents local shadowing bugs and stops the LSP from offering module completions on ordinary value receivers.

### 9. Lexer Recovery Stays Narrow And Honest

Even after the AST upgrade, the parser still cannot represent syntactically incomplete states like:

- `math.`
- `import math.`
- `Trait.`
- partially typed import aliases or member prefixes before the next identifier token exists

For those cases only, keep a small lexical recovery layer in `tools/lsp/lib/`, alongside `cursor_context.ml`. It should:

- classify just enough of the trailing-dot/prefix state to choose a completion context,
- use the last successful semantic snapshot for visibility facts,
- never become the primary source for definition or completion in fully parsable buffers.

That keeps incomplete-edit completion useful without turning the whole feature into a text scanner.

## Implementation Phases

### Phase 0: Preserve Source-Aware Surface Syntax

Primary files:

- `lib/frontend/syntax/surface_ast.ml`
- `lib/frontend/syntax/parser.ml`
- `lib/frontend/syntax/lower.ml`
- `lib/frontend/module_context.ml`
- `lib/frontend/discovery.ml`
- `lib/frontend/compiler.ml`
- `tools/lsp/lib/doc_state.ml`
- `tools/lsp/lib/import_header.ml`

Work:

1. Add positioned refs and positioned surface type nodes to `Surface_ast`.
2. Update parser constructors so import segments, declaration heads, methods, variants, type references, and generic params record exact source spans.
3. Keep lowering behavior identical. `lower.ml` should ignore the new source metadata except where it must preserve ids or file ids.
4. Change discovery/compiler parsing flow to retain `Surface.surface_program` alongside lowered AST.
5. Rename misleading fields so LSP code can distinguish surface syntax from lowered/typed AST, and remove the old misnamed aliases in the same slice.
6. Preserve cross-tree ids so cursor classification can join surface refs back to typed maps and symbol tables safely.
7. Keep `import_header.ml` only as the parse-incomplete helper. Parsed import buffers must not continue to round-trip through token-only header parsing, and any old parsed-buffer callers should be removed rather than left as fallback branches.

Required tests:

- parser inline tests for positioned import aliases/segments,
- parser inline tests for positioned type references in params, returns, `Dyn[...]`, and `impl` heads,
- parser inline tests for positioned trait method names, inherent method names, and enum variant names,
- parser/lowering tests proving expression ids and method ids stay aligned across surface and lowered trees,
- compiler/discovery tests proving surface trees survive analysis for entry modules and imported modules.

### Phase 1: Build Cursor Context And Per-Analysis Navigation Artifacts

Primary files:

- `tools/lsp/lib/cursor_context.ml` (new)
- `lib/frontend/compiler.ml`
- `lib/frontend/module_catalog.ml`
- `lib/frontend/typecheck/module_sig.ml`
- `lib/frontend/typecheck/resolution_artifacts.ml`
- `lib/frontend/typecheck/{enum_registry,trait_registry}.ml`

Work:

1. Build a cursor-context walker from the surface program, plus a lexical scope index for value/type/constraint visibility.
2. Keep that cursor index out of `Compiler.checked_module` to avoid new compiler/module dependency cycles; cache it in doc/LSP state only if profiling later shows it is necessary.
3. Extend constructor and method metadata with definition sites and snapshot them into per-analysis compiler artifacts so semantic lookups do not depend on global registries.
4. Add compiler helpers for:
   - visible type names in current scope,
   - visible constraint names in current scope,
   - wrapper-constructor visibility in bare expression positions,
   - sum-constructor lookup by type name,
   - trait/inherent method lookup by qualified root and method name,
   - reference classification for import headers and declaration heads.

Required tests:

- compiler inline tests for imported type/trait/shape visibility,
- compiler inline tests for generic type-param shadowing and nearest-scope lookup,
- cursor-context inline tests for nested value scopes, nested generic scopes, and declaration-head self sites,
- compiler inline tests for constructor source-site lookup,
- compiler inline tests for trait/inherent method source-site lookup,
- compiler inline tests proving wrapper constructors are visible in bare expression positions while sum variants are not,
- module catalog tests proving off-graph surface data still contributes correct import completions.

### Phase 2: Implement Broad Go-To-Definition

Primary files:

- `tools/lsp/lib/definition.ml`
- `tools/lsp/lib/server.ml`
- `tools/lsp/lib/editor_scenarios.ml`
- `tools/lsp/lib/edge_case_tests.ml`

Work:

1. Replace direct raw-AST/header-scanner logic in `definition.ml` with `cursor_context` classification plus compiler semantic lookup helpers.
2. Delete the parsed-buffer definition helpers that become redundant once `cursor_context` is authoritative; do not keep them as “legacy” branches.
3. Add definition handling for:
   - qualified roots (`Option`, `Greeter`, `Point`) separately from qualified members,
   - declaration heads,
   - type references in annotations,
   - generic params,
   - constraint references,
   - wrapper-constructor call heads,
   - constructors,
   - qualified trait methods,
   - qualified inherent methods,
   - import aliases and segments using structured import data.
4. Preserve existing module/import/value behavior by routing it through the same reference model, not by keeping the old raw-scanner path beside it.
5. Keep module targets at file start and non-source builtins as `None`.

Required tests:

- direct/imported top-level function definition,
- named type / transparent alias / enum / shape / trait definition from annotations,
- generic type parameter definition from parameter/return annotations,
- qualified root definition for `Option`, `Greeter`, and `Point`,
- wrapper-constructor call definition (`UserId(...)` -> type head),
- constructor definition from expression and pattern positions,
- qualified trait method definition,
- qualified inherent method definition,
- import alias and path-segment definition,
- definition requested on a declaration head returns that declaration span,
- shape-qualified member surface returns no definition,
- builtin primitive type returns no definition.

### Phase 3: Implement Broad Semantic Completion

Primary files:

- `tools/lsp/lib/completions.ml`
- `tools/lsp/lib/import_header.ml`
- `tools/lsp/lib/server.ml`
- `tools/lsp/lib/editor_scenarios.ml`
- `tools/lsp/lib/edge_case_tests.ml`

Work:

1. Replace the current completion-context classifier with the explicit context model from this plan.
2. Remove the current compatibility-shaped classifier and any translation shims once the new context model is green. `BareIdentifier`, `NamespaceMember`, and `ImportPath` should not survive as a parallel internal API.
3. Add semantic providers for:
   - value positions,
   - type positions,
   - constraint positions,
   - wrapper-constructor heads,
   - module members,
   - enum constructors,
   - trait-qualified methods,
   - inherent-qualified methods,
   - import headers.
4. Use typed signatures/detail formatters for label kinds and details.
5. Keep last-good semantic fallback only for incomplete trailing-dot/import states.
6. Preserve the current guarantee that completions do not leak rewritten internal names.

Required tests:

- expression completion includes visible functions/imports but not type-only names,
- expression completion includes callable wrapper constructors like `UserId`,
- expression completion does not offer bare constructor names like `Some`,
- type-position completion includes visible type names and type params but not values,
- type-position completion excludes shapes,
- constraint-position completion includes traits/shapes but not ordinary types,
- `Dyn[...]` completion includes traits only,
- `Option.` completion includes constructors,
- `Greeter.` completion includes trait methods,
- `Point.` completion includes inherent methods,
- `Point.` where `Point` is a transparent exact-record name includes methods but no constructor items,
- `Named.` where `Named` is a shape yields no member completion,
- `import math.` completion includes child modules and exports with correct kinds,
- parsed `import math.add` and parsed `math.add` completion/definition still work with `last_good = None`, proving the structured path is primary rather than a fallback,
- incomplete `math.` / `import math.` uses last-good semantics without stale-name leaks,
- shadowed module root suppresses namespace/type completions.

## Testing Strategy

Focused verification should follow the repo TDD rules from `CLAUDE.md`.

Unit-level coverage:

- parser and lowering inline tests for new surface spans and preserved ids,
- compiler inline tests for surface-tree retention and navigation lookup helpers,
- registry/artifact inline tests for constructor/method definition sites,
- LSP inline tests in `tools/lsp/lib/{definition,completions,editor_scenarios,edge_case_tests}.ml`.

Scenario coverage that must exist before implementation is considered done:

- local file with top-level functions, generics, traits, shapes, and inherent impls,
- temp project with imported value/type/trait/shape/enum surfaces,
- wrapper constructor scenario such as `type UserId = UserId(Int)` proving bare-expression completion/definition,
- qualified constructor, trait method, and inherent method calls,
- direct-import completion vs namespace completion,
- exact-record inherent scenario proving transparent `type Point = { ... }` gets `Point.` method completion but no constructor items,
- `Dyn[Show]` and `Dyn[Show & Eq]` scenarios proving trait-only completion and definition behavior,
- unsaved source override changing imported types/traits and completion/definition following the override,
- incomplete trailing-dot states using last-good semantics conservatively.

Reuse the existing language fixture surface where possible instead of inventing synthetic editor-only syntax:

- `test/fixtures/modules/*.mr` for namespace/direct-import/export cases,
- `test/fixtures/modules_edge/*.mr` for ambiguity, private exports, collisions, and shadowing,
- `tools/lsp/lib/editor_scenarios.ml` for dense single-file mixed-surface scenarios.

Focused command cadence during implementation:

- `make unit lsp`
- `make unit compiler`
- targeted dune inline-test invocations for touched parser/typecheck modules when needed

Do not run the full integration suite for this plan unless explicitly requested after the feature work is complete.

## Commit Plan

1. `tooling: preserve source-aware surface AST through compiler analysis`
   - surface AST positions
   - parser/discovery/compiler/doc_state boundary
   - parser/compiler tests
2. `tooling: add cursor context and per-analysis navigation artifacts`
   - new cursor-context helper
   - constructor/method source metadata in compiler artifacts
   - compiler lookup tests
   - removal of redundant parsed-buffer aliases introduced by the old surface/lowered naming
3. `tooling: broaden LSP go-to-definition across symbol classes`
   - definition resolver rewrite
   - deletion of superseded parsed-buffer definition scanners
   - definition regressions
4. `tooling: add semantic completion for types, methods, constructors, and imports`
   - completion context rewrite
   - deletion of superseded completion-context variants and shims
   - incomplete-edit fallback hardening
   - completion regressions

Each commit should stay green against the focused tests it introduces.

## Risks

1. Surface AST widening touches the parser broadly. The mitigation is to keep lowering semantics unchanged and lock parser behavior with new inline tests before LSP code changes.
2. Keeping both surface and lowered trees increases analysis payload size. That is acceptable for the current architecture, but the plan should avoid duplicate reparsing and avoid storing redundant ad hoc indexes beside the compiler-owned one.
3. The new cursor-context/compiler-wrapper split can still create dependency drift if cursor classification starts owning semantic visibility. Keep `cursor_context.ml` syntax-only and route symbol visibility through compiler helpers.
4. Method navigation can become unsound if it guesses impl targets for polymorphic calls. This plan avoids that by sending trait-dispatched calls to trait method declarations unless an inherent target is uniquely resolved.
5. Surface/lowered id drift would break joins between cursor classification and typed artifacts. Lock id preservation with explicit parser/lowering tests before LSP rewrites.
6. Constructor completion is easy to get wrong because wrapper constructors and sum variants use different surfaces. Keep those paths separate in both context classification and tests.
7. Incomplete-edit completion can regress if lexical recovery grows beyond narrow trailing-dot/import classification. Keep that layer intentionally small and backed by last-good semantic state only.
8. Legacy-path drift is easy to reintroduce if old classifiers or token-only parsed-buffer helpers are left in place “just in case”. Delete superseded branches as soon as the new path is green.
9. Mutable type registries are easy to misuse from editor code. LSP lookups should flow through per-analysis project artifacts on `Compiler.entry_analysis` and `checked_module`, not arbitrary global registry reads.

## Progress

- 2026-04-05 02:52 CEST: Draft plan created in `docs/plans/todo/tooling/02_lsp_symbol_navigation_and_completion.md`; awaiting `plan-review`.
- 2026-04-05 03:15 CEST: Plan-review pass started in Codex; validating the draft against current parser, compiler, resolver, and LSP ownership boundaries.
- 2026-04-05 03:16 CEST: Codex-only counterpart review started via a same-tool subagent; Claude cross-review intentionally skipped per request.
- 2026-04-05 03:21 CEST: Plan revised after review to separate cursor classification from compiler semantic ownership, keep constructor/method sites in per-analysis artifacts, and pin wrapper-vs-sum constructor completion semantics.
- 2026-04-05 03:22 CEST: Codex-only counterpart review received; it confirmed the cursor/compiler split, per-analysis metadata ownership, wrapper-vs-sum constructor behavior, and explicit scope-index coverage.
- 2026-04-05 03:22 CEST: Final Codex review completed; plan tightened for implementation.
- 2026-04-05 03:29 CEST: Deep Codex-only review pass started with an explicit check for actionability and accidental backward-compatibility scaffolding.
- 2026-04-05 03:30 CEST: Plan revised again to forbid legacy parsed-buffer compatibility branches, require deletion of superseded classifiers/helpers in the same slice, and clarify that dual surface/lowered trees are intentional rather than compatibility aliases.
- 2026-04-05 03:31 CEST: Deep Codex-only counterpart review received; it confirmed the plan is actionable and that the remaining dual structures are intentional feature substrates rather than backward-compatibility baggage.
- 2026-04-05 03:33 CEST: Final deep Codex pass completed; section numbering and progress tracking were tightened after the second review.
- 2026-04-05 03:35 CEST: Feature implementation started in Codex. Executable slices locked to the plan phases: (1) preserve positioned surface syntax through parser/discovery/compiler/doc-state, (2) add cursor context plus per-analysis constructor/method navigation artifacts, (3) rewrite definition onto cursor-context/compiler lookup and delete superseded scanners, (4) rewrite completion contexts/providers and keep only narrow incomplete-edit recovery.
- 2026-04-05 03:59 CEST: Slice 1 (parse once, retain both surface and lowered trees) entered test-first execution. The slice boundary is `syntax/parser` -> `discovery` -> `module_context` -> `compiler` -> `tools/lsp/doc_state`, with existing compiler/doc-state tests being rewritten to require distinct `surface_program` and `lowered_program` payloads.
- 2026-04-05 04:01 CEST: Red state captured for Slice 1. `dune build` now fails on the missing `Syntax.Parser.parse_with_surface` entrypoint, after which the new compiler/doc-state tests will require explicit `surface_program` plus `lowered_program` fields instead of the old lowered-AST `surface_program` alias.
- 2026-04-05 04:06 CEST: Slice 1 implementation is green under focused verification. `dune build` passes, and `dune runtest lib/frontend/syntax lib/frontend tools/lsp/lib` passes after wiring `parse_with_surface` through discovery/compiler/module-catalog/doc-state, exporting `Surface_ast` from `lib/lib.ml`, and splitting compiler/doc-state payloads into real `surface_program` plus explicit `lowered_program`/`program`.
- 2026-04-05 04:09 CEST: Slice 2 (cursor context plus compiler-owned navigation lookups) entered test-first execution. The planned implementation path is to classify references from `parsed_module.surface_program`, keep local/generic binding lookup lexical, and derive exact constructor/method definition sites from compiler-owned checked modules plus parsed surface trees rather than adding new LSP reads of global registries.
- 2026-04-05 04:18 CEST: Red state captured for Slice 2. The new cursor-context definition tests exposed that `Qualified_member` lookups for `Option.Some` were still short-circuiting through the root type symbol and landing on the enclosing type span instead of the exact variant head.
- 2026-04-05 04:28 CEST: Slice 2 is green under focused and broader verification. `tools/lsp/lib/cursor_context.ml` now classifies declaration/import/type/qualified references from the real surface tree, `tools/lsp/lib/definition.ml` resolves them through compiler-owned exact sites plus surface-backed same-file variant lookup, and both `dune runtest tools/lsp/lib` and `dune runtest lib/frontend/syntax lib/frontend tools/lsp/lib` pass with `dune build`.
- 2026-04-05 04:31 CEST: Slice 3 (broad go-to-definition rewrite) entered test-first execution. The next failures were expected to come from method-bearing qualified members and generic-constraint references, using compiler method-resolution artifacts plus surface method ids instead of raw AST guesses.
- 2026-04-05 04:34 CEST: Red state captured for Slice 3. The new qualified-method and constraint-definition tests showed two remaining gaps: `Qualified_member` references needed the full access expression id to join against compiler call-resolution artifacts, and generic constraint refs like `fn show[t: Named]` were not yet classified by `cursor_context`.
- 2026-04-05 04:37 CEST: Slice 3 is green under focused and broader verification. `cursor_context` now carries qualified access expr ids and classifies generic constraint refs, `compiler.ml` can map resolved trait/inherent method callables back to exact surface method heads, and `definition.ml` now covers declaration heads, constraints, wrapper constructors, enum constructors, and qualified trait/inherent methods. `dune runtest tools/lsp/lib`, `dune runtest lib/frontend/syntax lib/frontend tools/lsp/lib`, and `dune build` all pass.
- 2026-04-05 04:43 CEST: Slice 4 (broad semantic completion rewrite) entered test-first execution. The slice scope was to replace the old bare-identifier/namespace/import classifier with surface-aware value/type/constraint/qualified-member contexts, while keeping only the narrow trailing-dot lexical recovery path for incomplete edits.
- 2026-04-05 04:56 CEST: Red state captured for Slice 4. The new completion tests showed two concrete regressions in incomplete-buffer flows: visible type/member lookup was still depending on checked-module-only navigation, and `Dyn[...]` constraint detection in `completions.ml` had a non-advancing backward scan that could spin indefinitely on non-identifier characters.
- 2026-04-05 05:01 CEST: Slice 4 is green under focused and broader verification. `completions.ml` now classifies value/type/constraint/import/module/enum/trait/inherent contexts from the surface tree, incomplete-buffer visibility falls back through module-graph navigation for type and method lookup, and the `Dyn[...]` constraint detector no longer loops. `dune runtest tools/lsp/lib`, `dune runtest lib/frontend/syntax lib/frontend tools/lsp/lib`, and `dune build` all pass.
