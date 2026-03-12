# Syntax Rework Follow-up Plan

## Maintenance

- Last verified: 2026-03-12
- Implementation status: Implemented
- Type: Historical implementation plan

## Summary
Track language work intentionally deferred out of the core vNext syntax rework.

This document assumes `docs/plans/done/language/02_syntax-rework.md` has completed through Phase 7 and that `docs/SYNTAX.md` is the stable vNext surface spec.

All work in this document happens on the post-migration frontend/compiler pipeline:

`source -> lexer/parser -> Surface_ast -> lowering/canonicalization -> Core_ast -> resolver/typechecker/codegen`

This is follow-up language work, not another syntax migration. Do not reintroduce legacy syntax, dual-syntax branching below lowering, or temporary compatibility paths removed by the main plan.

## Goals
- Finish the semantics of user-trait derive on top of the canonical `AST.DeriveDef` shape established by the core syntax rework.
- Implement explicit trait-object syntax without reusing bare trait names.
- Implement general intersection types without weakening the existing constrained-param shorthand or trait-bound grammar.
- Keep `docs/SYNTAX.md`, compiler behavior, tests, LSP output, tree-sitter grammar, and editor grammars synchronized.

## Non-Goals
- Reopening the vNext syntax decisions already locked in `docs/plans/done/language/02_syntax-rework.md`.
- Restoring the previous bare field-only trait-as-type surface syntax.
- Mixing follow-up feature work into the legacy-syntax migration path.
- Shipping new parser syntax without matching tests, docs, tree-sitter, and editor-grammar updates.

## Preconditions
- `docs/plans/done/language/02_syntax-rework.md` Phase 7 exit criteria are met.
- Legacy syntax support has been removed from parser and lowering.
- `docs/SYNTAX.md` is the only normative surface-syntax snapshot.
- `UpperCamel` casing is already enforced for vNext type-space names.
- Bare trait names in parameter/type position already mean constrained-param shorthand, not trait-object syntax.
- `&` is still constraint-only in the mainline language until the intersection track below explicitly lands.

## Shared Binding Decisions

### 1. Canonical Layer Ownership
- Parser work still targets `Surface_ast`.
- Lowering still owns canonicalization into `Core_ast` (`Syntax.Ast.AST`).
- Type-system extensions still land in `lib/frontend/typecheck/types.ml`.
- No follow-up feature may bypass lowering by teaching downstream compiler passes to understand raw new surface syntax directly.

### 2. Work Order
- Run Track A first. It depends only on the canonical derive shape already reserved by the main plan and does not require new surface syntax.
- Run Track B second.
- Run Track C last. Track B lands first because trait-object representation and general intersections interact in type normalization and subtyping.

### 3. Stop Conditions
- Track A, Track B, and Track C are all in scope for this follow-up plan.
- C0 still exists to freeze the semantic matrix for intersections before implementation starts; it is not an accept/reject gate anymore.

### 4. Tooling Policy
- Any accepted surface-syntax change in this document must update all of the following in the same track, not later:
  - parser tests,
  - fixture corpus,
  - `tools/tree-sitter-marmoset/**`,
  - `tools/vscode-marmoset/syntaxes/marmoset.tmLanguage.json`,
  - `tools/jetbrains-marmoset/**`,
  - LSP formatting / signature / symbol output if surface forms are printed back to users.

### 5. Docs Policy
- `docs/SYNTAX.md` remains the authoritative surface spec.
- This document is the implementation/migration plan for deferred language work.
- `docs/features/traits.md` remains the design/behavior explainer and must be updated whenever trait semantics change.

## Shared Code Touchpoints
- `lib/frontend/syntax/ast.ml`
- `lib/frontend/syntax/surface_ast.ml`
- `lib/frontend/syntax/parser.ml`
- `lib/frontend/syntax/lower.ml`
- `lib/frontend/typecheck/types.ml`
- `lib/frontend/typecheck/annotation.ml`
- `lib/frontend/typecheck/trait_registry.ml`
- `lib/frontend/typecheck/trait_solver.ml`
- `lib/frontend/typecheck/infer.ml`
- `lib/frontend/typecheck/resolution_artifacts.ml`
- `lib/backend/go/emitter.ml`
- `docs/SYNTAX.md`
- `docs/features/traits.md`
- `test/fixtures/traits/`
- `test/fixtures/traits_field/`
- `test/fixtures/traits_impl/`
- `test/fixtures/codegen_mono/`

## Track A. Full User-Trait Derive Semantics

### Current State
- The main syntax plan already reserves canonical derive syntax and lowers both legacy and postfix derive to `AST.DeriveDef`.
- `docs/SYNTAX.md` currently treats built-in derive semantics as normative and explicitly defers user-trait derive semantics.
- `Trait_registry` still hardcodes derivable traits through `derive_kind_of_trait_name` and `derive_impl` in `lib/frontend/typecheck/trait_registry.ml`.
- Derive processing currently lives in `infer_top_level_trait_related_stmt` (`lib/frontend/typecheck/infer.ml:3469`), which calls `Trait_registry.derive_impl` for each requested trait. The general `infer_statement` dispatch treats `DeriveDef` as a no-op. There is almost no derive-specific semantic validation beyond what `Trait_registry.derive_impl` checks internally.
- `trait_registry.method_sig` does not currently preserve trait default bodies, so default-backed derive cannot be implemented correctly without extending registry data in `lib/frontend/typecheck/trait_registry.ml`.
- The emitter only has builtin-record derive body generation today in `lib/backend/go/emitter.ml`.

### Binding Decisions
- Track A does not add new surface syntax. It reuses the canonical `derive Trait1, Trait2` surface already locked in `docs/SYNTAX.md`.
- Built-in derives (`Eq`, `Show`, `Debug`, `Ord`, `Hash`) remain supported through the existing specialized code-generation path.
- User-trait derive v1 is default-backed only:
  - a user trait is derivable only when every trait method has a default body after type substitution,
  - traits with any required method remain non-derivable in this rollout unless they are one of the builtin derivable traits above.
- Field-only traits are not derivable in Track A. They are already satisfied structurally; `derive FieldOnlyTrait` is a deterministic error, not a no-op.
- Supertraits are not auto-derived implicitly from trait definitions alone. A derive request succeeds only if every supertrait is already implemented for the target type or appears in the same derive clause.
- When the same `derive` statement requests multiple traits, processing order is graph-driven, not source-order-sensitive. The implementation must topologically sort the requested traits with respect to their supertrait closure.
- Derived impls must be recorded with provenance:
  - `ExplicitImpl`
  - `BuiltinDerivedImpl`
  - `DefaultDerivedImpl`
- Default-backed user derives are not executed directly out of `Trait_registry.impl_def`. The registry remains signature/provenance-oriented metadata.
- Track A introduces a dedicated canonical-to-canonical derive-expansion pass:
  - `expand_user_derives : AST.program -> (AST.program, Diagnostic.t) result`
  - this pass runs before ordinary impl validation, impl registration, type inference of impl bodies, and codegen
  - internally it performs an initial pre-scan of the canonical program to collect trait definitions, default bodies, supertrait links, and explicit impl keys needed for derive planning and duplicate/conflict checks
  - it rewrites each source `AST.DeriveDef` into:
    - a residual `AST.DeriveDef` containing builtin derive traits only, if the source statement requested any builtin derives,
    - zero or more synthetic `AST.ImplDef` nodes for default-backed user derives, inserted immediately after the source derive statement.
- Synthetic `AST.ImplDef` nodes own executable method bodies for user-trait derives. The trait registry keeps default bodies for planning and validation, but emitter and ordinary impl-body typechecking consume the synthetic impl nodes.
- Default-backed user derive expansion must deep-clone each default method body. It must not reuse the exact original `AST.expression` nodes from the trait definition, because derived impls need distinct expression ids and may need rewritten nested type annotations.
- Expansion substitutes the trait type parameter and any method-level generic type references in both:
  - the generated impl signature,
  - any cloned type annotations nested inside the generated method body.
- When converting a trait default body expression into `AST.method_impl.impl_method_body`, use one deterministic rule:
  - `Some (BlockExpr stmts)` -> `AST.Block stmts`
  - any other `Some expr` -> `AST.Block [AST.ExpressionStmt expr]`
- The expander allocates fresh synthetic ids for every cloned expression and every generated `impl_method_id`. The initial allocation point is `max_expr_id(program) + 1`, computed once before expansion begins, so generated ids cannot collide with parser-allocated ids already present in the canonical AST.
- Derive diagnostics must report the specific blocking reason and name the target trait/type. Required cases:
  - undefined trait,
  - trait not derivable,
  - field-only trait derive is redundant/unsupported,
  - missing supertrait impl/derive closure,
  - required method without default body,
  - duplicate explicit or duplicate derived impl,
  - default body typecheck failure after substitution.

### Phase A0. Freeze User-Derive Spec And Diagnostics
Purpose:
- Convert user-trait derive from "reserved syntax" into a written, normative semantic contract before compiler work begins.

Tasks:
- Update `docs/SYNTAX.md`:
  - state that builtin derives remain special-cased,
  - state that user-trait derive v1 is default-backed only,
  - state the supertrait-closure rule,
  - state the field-only-trait rejection rule,
  - list the deterministic failure cases above.
- Update `docs/features/traits.md`:
  - replace the current "selected traits only" derive text,
  - explain builtin derive vs default-backed user derive,
  - explain why field-only traits are not derived.
- Add a derive-specific diagnostics matrix to this document or `docs/SYNTAX.md` so failure behavior is testable before code changes begin.

Likely files:
- `docs/SYNTAX.md`
- `docs/features/traits.md`
- this plan

Exit criteria:
- The user-trait derive rules above are written normatively.
- No remaining doc says user-trait derive semantics are merely "reserved" or "TBD."

### Phase A1. Extend Canonical Trait/Impl Metadata
Purpose:
- Preserve enough canonical information to synthesize default-backed impls correctly and diagnose them consistently.

Tasks:
- Extend `Trait_registry.method_sig` in `lib/frontend/typecheck/trait_registry.ml` to retain default method bodies:
  - add `method_default_impl : AST.expression option`,
  - keep existing signature/effect/generic metadata.
- Extend registry impl metadata to retain provenance:
  - either add `impl_origin` to `impl_def`,
  - or add a parallel origin registry keyed by `(trait_name, impl_for_type)`,
  - prefer storing provenance with the impl so diagnostics and emitter do not need parallel lookups.
- Introduce a post-parse synthetic-id allocator for canonical rewrites:
  - either `lib/frontend/typecheck/synthetic_ids.ml`,
  - or a private helper inside the derive-expansion module,
  - but the API must support `fresh_expr_id` and `fresh_method_id` for cloned derive bodies.
- Make trait-definition registration in `lib/frontend/typecheck/infer.ml` preserve default bodies when building registry `trait_def` values.
- Keep explicit impl validation strict:
  - explicit impls still typecheck against the trait contract,
  - generated default-derived impls are fully materialized before validation so existing signature validation logic remains usable.
- Add the derive-expansion pass skeleton and wire-point, even if it still returns the input program unchanged in this phase:
  - module path: `lib/frontend/typecheck/derive_expand.ml` (recommended),
  - public API: `expand_user_derives : AST.program -> (AST.program, Diagnostic.t) result`.
- Do not add parser or lowering changes in Track A. This track operates entirely on canonical AST produced by the main syntax plan.

Likely files:
- `lib/frontend/typecheck/trait_registry.ml`
- `lib/frontend/typecheck/infer.ml`
- `lib/frontend/typecheck/derive_expand.ml`
- `lib/frontend/typecheck/synthetic_ids.ml` or equivalent helper
- `lib/frontend/syntax/ast.ml` only if canonical metadata needs a small companion type for impl provenance

Required tests:
- trait registration preserves default bodies,
- impl provenance is recorded deterministically,
- explicit impl validation behavior does not regress when defaults exist.

Exit criteria:
- The trait registry can answer, for any trait method, whether a default body exists and what canonical expression it contains.
- The impl registry can distinguish explicit impls from builtin-derived and default-derived impls.

### Phase A2. Implement Derive Planning, Validation, And Registry Insertion
Purpose:
- Replace the builtin-only derive path with one planner that handles builtin derives and default-backed user derives coherently.

Tasks:
- Introduce a derive planner in `trait_registry.ml` and `derive_expand.ml`:
  - pre-scan the canonical program for `AST.TraitDef` and explicit `AST.ImplDef` ownership before rewriting derive statements,
  - compute the requested derive set from one `AST.DeriveDef`,
  - split requested traits into builtin derives vs user derives,
  - expand/check supertrait closure,
  - topologically order requested user traits,
  - reject cycles or missing supertrait definitions deterministically.
- Split validation into explicit stages:
  - `validate_derive_target_type`,
  - `validate_derivable_trait`,
  - `validate_derive_supertraits`,
  - `validate_default_backed_trait`,
  - `validate_duplicate_impl_conflicts`.
- Keep builtin derives on the existing specialized path, but run them through the same planner/duplicate checks.
- Implement default-backed user derive synthesis:
  - synthesize a canonical `AST.ImplDef` for each accepted user trait derive,
  - instantiate the trait type parameter with the concrete target type in the generated impl signature,
  - instantiate method-level generics in the generated impl signature and cloned body-local type annotations,
  - clone every default method body into the synthesized impl using fresh ids,
  - convert cloned default expressions to canonical impl statements using the binding rule above,
  - leave builtin derives as residual `AST.DeriveDef` entries so the existing builtin derive/emitter path can keep working during Track A,
  - tag the generated impl registration as `DefaultDerivedImpl`.
- Make residual-program rewriting explicit:
  - source `type Point = { x: Int } derive Show, Printable` where `Show` is builtin and `Printable` is default-backed user trait becomes
    - `AST.DeriveDef { derive_traits = [Show]; ... }`
    - followed immediately by one synthetic `AST.ImplDef` for `Printable[Point]`.
- Ensure the derive planner is idempotent for one program:
  - a repeated request for the same trait/type pair fails deterministically,
  - explicit impls always block duplicate derives,
  - existing "replace builtin impl once" behavior remains explicit and tested if still desired.
- Update `Infer` entrypoints so they invoke `Derive_expand.expand_user_derives` once per canonical program before ordinary top-level trait/impl processing begins.
- After expansion, keep the existing invariant:
  - builtin derives are still processed via `AST.DeriveDef`,
  - user-trait derives are processed through the ordinary impl path because they now exist as synthetic `AST.ImplDef` nodes in the expanded program.

Likely files:
- `lib/frontend/typecheck/trait_registry.ml`
- `lib/frontend/typecheck/infer.ml`
- `lib/frontend/typecheck/derive_expand.ml`
- `lib/frontend/typecheck/annotation.ml` if derive target-type validation needs canonical type conversion changes

Required tests:
- builtin derive still succeeds for existing builtin cases,
- user trait with all-default methods derives successfully,
- user trait with one required method fails with a precise error,
- derive of field-only trait fails deterministically,
- derive succeeds when supertraits are already implemented,
- derive succeeds when supertraits are in the same derive list in any order,
- derive fails when a required supertrait is missing,
- duplicate derive / derive-vs-explicit-impl conflicts fail deterministically.

Exit criteria:
- One derive planner handles builtin derives and default-backed user derives.
- Derive behavior is fully determined by canonical trait/impl metadata, not by parser surface shape.

### Phase A3. Typecheck, Codegen, Docs, And Fixture Integration
Purpose:
- Make user-trait derive a fully supported language feature instead of a registry side effect.

Tasks:
- Ensure default-backed generated impls participate in normal trait resolution and method dispatch exactly like explicit impls.
- Keep emitter behavior coherent:
  - builtin-derived impl bodies may still use specialized emission,
  - default-derived impls emit through the regular impl-method path because derive expansion has already materialized them as synthetic `AST.ImplDef` nodes inside the canonical program.
- Add end-to-end fixture coverage for:
  - default-backed user derive success,
  - derive failure diagnostics,
  - method dispatch through derived impls,
  - codegen for default-derived methods.
- Update docs/examples to use `UpperCamel` names for user-defined derivable traits.

Likely files:
- `lib/frontend/typecheck/infer.ml`
- `lib/frontend/typecheck/trait_solver.ml`
- `lib/backend/go/emitter.ml`
- `docs/SYNTAX.md`
- `docs/features/traits.md`
- `test/fixtures/traits/`
- `test/fixtures/traits_impl/`
- `test/fixtures/codegen_mono/`

Exit criteria:
- User-trait derive works end-to-end for default-backed traits.
- Builtin derive behavior remains green.
- Docs and fixtures describe one derive model, not two partially overlapping ones.

## Track B. Explicit Trait-Object Syntax

### Current State
- The previous field-only trait-as-type path is implemented in annotation conversion today and projects to record types in `lib/frontend/typecheck/annotation.ml`.
- Method-only and mixed trait objects are intentionally unsupported in the current compiler and docs in `lib/frontend/typecheck/annotation.ml` and `docs/features/traits.md`.
- The traits design doc already states that method/mixed trait objects should not be exposed until a first-class internal representation exists and names that as typed AST/IR work in `docs/features/traits.md`.
- The main syntax plan deliberately removes the old bare field-only trait-as-type feature so bare trait names can remain constrained-param shorthand.

### Binding Decisions
- Track B is accepted. `Dyn[ConstraintExpr]` is the trait-object syntax for Marmoset.
- Bare trait names remain constrained-param shorthand permanently. They must never be overloaded back into trait-object syntax.
- The only surface syntax added is:
  - `Dyn[ConstraintExpr]`
  - examples: `Dyn[Show]`, `Dyn[Show & Eq]`
- `Dyn[...]` is the trait-object surface form for v1. No alternate spellings (`Trait`, `dyn Trait`, `exists Trait`, etc.) are added in this track.
- `Dyn[...]` uses the existing `ConstraintExpr` grammar inside the brackets. That preserves the existing meaning of `&` inside trait bounds and avoids overloading bare trait names.
- `Dyn[...]` is legal only when every trait in the enclosed `ConstraintExpr` is `MethodOnly` or `Mixed`.
  - `Dyn[Named]` is a type error if `Named` is field-only.
  - `Dyn[Show & Named]` is also a type error; mixing field-only traits into `Dyn[...]` is not allowed.
  - field-only traits continue to use the existing structural projection model outside `Dyn[...]`.
- Implicit coercion: concrete values coerce to `Dyn[...]` at type-directed sites (assignment, argument passing, return position). No manual wrapping call is needed:
  - `let x: Dyn[Show] = 42` — implicit coercion from `int` to `Dyn[Show]`.
  - `let items: List[Dyn[Show]] = [42, "hello", true]` — each element coerced implicitly.
  - `fn print_all(xs: List[Dyn[Show]])` — callers pass concrete values, coercion inserted by compiler.
- Method dispatch on `Dyn[...]` values is dynamic and requires no explicit unwrapping. Calling `.show()` on a `Dyn[Show]` dispatches through the witness table.
- Motivating use cases:
  - Heterogeneous collections: `List[Dyn[Show]]` holding values of different concrete types.
  - Extensibility: one library defining a trait, another providing impls, a third consuming trait objects without knowing concrete types.
- Runtime representation is witness-carrying existential packaging, not raw Go interface exposure:
  - payload value,
  - witness table,
  - type identity / specialization key as needed for dynamic operations.
- Track B must introduce an explicit typed representation before parser syntax is exposed. Do not rely on emitter-only tricks.
- Track B does not introduce a new canonical AST expression node for coercion. Packaging is represented as typed resolution metadata attached to existing expression ids; the emitter performs packaging only at recorded coercion sites.
- The explicit canonical artifacts for Track B are:
  - `Types.TTraitObject of string list` for the type itself,
  - `Resolution_artifacts.trait_object_coercion` keyed by source `expr.id` for each inserted packaging site,
  - `Infer.method_resolution = ... | DynamicTraitMethod of string` to record which trait witness services a dynamic dot-call.
- `Dyn[...]` exists for method-bearing trait values only. It is not an alternate spelling for the existing field-only projection path.

### Phase B0. Freeze The Dyn Spec
Purpose:
- Lock down the `Dyn[ConstraintExpr]` syntax and coercion model in docs before implementation starts.

Tasks:
- Update `docs/SYNTAX.md`:
  - add `Dyn[ConstraintExpr]` to `TypeExpr` grammar,
  - document implicit coercion rules (assignment, argument, return),
  - document dynamic method dispatch on `Dyn[...]` values,
  - document that `Dyn[...]` rejects field-only traits and mixed constraint sets containing field-only traits,
  - give examples: `Dyn[Show]`, `Dyn[Show & Eq]`, `List[Dyn[Show]]`.
- Update `docs/features/traits.md`:
  - replace deferred-object wording with the accepted `Dyn[...]` model,
  - document that bare trait names remain constrained-param shorthand only.
- Update this plan: remove any remaining "optional" or "accept/reject" language from Track B.

Likely files:
- `docs/SYNTAX.md`
- `docs/features/traits.md`
- this plan

Exit criteria:
- `Dyn[ConstraintExpr]` is fully documented with coercion rules and examples.
- No ambiguous "maybe later" wording remains in trait-object docs.

### Phase B1. Introduce Typed Representation Before Surface Syntax
Purpose:
- Add a real internal model for trait objects before parser or lowering work begins.

Tasks:
- Extend `Types.mono_type` in `lib/frontend/typecheck/types.ml` with a canonical trait-object type, for example:
  - `TTraitObject of string list`
- Canonicalize the enclosed trait list for every `TTraitObject`:
  - sort deterministically by trait name,
  - deduplicate repeated traits,
  - reject an empty trait list before the type is constructed.
- Update all `Types` helpers:
  - canonicalization,
  - pretty-printing,
  - substitution,
  - free-variable collection,
  - normalization/equality helpers.
- Extend `Resolution_artifacts` and checker/emitter plumbing with explicit trait-object metadata:
  - `type trait_object_coercion = { target_traits : string list; source_type : Types.mono_type }`
  - `Infer` gets a global `trait_object_coercion_store : (int, Resolution_artifacts.trait_object_coercion) Hashtbl.t`
  - `Checker.typecheck_result` snapshots that store as `trait_object_coercion_map`
  - emitter state carries the same map alongside `call_resolution_map` and `method_type_args_map`
- Extend typed resolution for dynamic dispatch:
  - `Infer.method_resolution` gains `DynamicTraitMethod of string`
  - the string records the concrete trait witness chosen for the dynamic call (`Show`, `Eq`, etc.), not just the method name.
- Decide and document the runtime layout used by the Go backend:
  - package shape,
  - witness-table shape,
  - method lookup path.
- Do not add parser syntax yet. This phase is type-system/runtime scaffolding only.

Likely files:
- `lib/frontend/typecheck/types.ml`
- `lib/frontend/typecheck/checker.ml`
- `lib/frontend/typecheck/resolution_artifacts.ml`
- `lib/frontend/typecheck/infer.ml`
- `lib/backend/go/emitter.ml`
- `docs/features/traits.md`

Required tests:
- `Types.to_string` and normalization handle trait-object types,
- `TTraitObject` canonicalization sorts/dedupes trait lists and rejects empty sets,
- substitutions preserve trait-object bounds,
- resolution metadata can represent a dynamic trait call site without emitter re-resolution,
- checker/emitter plumbing snapshots and carries `trait_object_coercion_map`,
- `DynamicTraitMethod` can represent one selected witness trait for a dynamic call.

Exit criteria:
- The compiler has a first-class internal trait-object representation.
- Dynamic trait dispatch metadata exists before any surface syntax reaches users.

### Phase B2. Add `Dyn[...]` Surface Syntax And Lowering
Purpose:
- Expose explicit trait-object syntax on top of the typed representation from Phase B1.

Tasks:
- Update `docs/SYNTAX.md`:
  - add `Dyn[ConstraintExpr]` to `TypeExpr`,
  - give examples for single-trait and multi-trait object types,
  - keep bare trait names as constrained-param shorthand,
  - state that field-only traits are rejected inside `Dyn[...]`.
- Extend `Surface_ast` and canonical `AST.type_expr` with an explicit trait-object type form.
- Update parser/lowering:
  - parse `Dyn[Show]` and `Dyn[Show & Eq]`,
  - lower surface trait-object types to one canonical `AST` type form,
  - keep brace/expr lowering rules unchanged.
- Update syntax tooling:
  - tree-sitter grammar and corpus tests,
  - VS Code TextMate grammar,
  - JetBrains grammar copy,
  - any syntax snapshots used by editor tests.

Likely files:
- `docs/SYNTAX.md`
- `lib/frontend/syntax/surface_ast.ml`
- `lib/frontend/syntax/ast.ml`
- `lib/frontend/syntax/parser.ml`
- `lib/frontend/syntax/lower.ml`
- `tools/tree-sitter-marmoset/**`
- `tools/vscode-marmoset/syntaxes/marmoset.tmLanguage.json`
- `tools/jetbrains-marmoset/**`

Required tests:
- parser success/failure cases for `Dyn[...]`,
- lowering tests from surface trait-object types to canonical type AST,
- parser/type-annotation failure cases for `Dyn[FieldOnlyTrait]` and `Dyn[Show & FieldOnlyTrait]`,
- tree-sitter corpus tests,
- grammar-sync checks.

Exit criteria:
- `Dyn[...]` is fully documented and parseable.
- All syntax tooling recognizes it.

### Phase B3. Typechecking, Dispatch, And Coercions
Purpose:
- Make trait-object values typecheck and behave predictably.

Tasks:
- Update annotation conversion so canonical trait-object types convert to `Types.TTraitObject`.
- Define implicit coercion rules from concrete values to `Dyn[...]`:
  - coercion is inserted automatically at type-directed sites: assignment, argument passing, return position,
  - only traits with method-bearing witness tables are eligible,
  - `Dyn[...]` is rejected before coercion if any enclosed trait is `FieldOnly`,
  - the compiler records one `trait_object_coercion` artifact at each coercion site keyed by the source expression id,
  - the emitter packages through witness-table construction only when such an artifact exists,
  - coercion does not happen in arbitrary expression contexts — only where a `Dyn[...]` type is expected.
- Update trait resolution and inference:
  - method calls on `Dyn[...]` use `DynamicTraitMethod trait_name` resolution metadata,
  - field access on `Dyn[...]` is rejected unless the language later adds explicit object-field exposure rules,
  - narrowing/compatibility behavior is written and tested explicitly.
- Keep coherence rules explicit:
  - object creation depends on a concrete impl being available,
  - dynamic calls do not bypass trait registry coherence.

Likely files:
- `lib/frontend/typecheck/annotation.ml`
- `lib/frontend/typecheck/trait_registry.ml`
- `lib/frontend/typecheck/trait_solver.ml`
- `lib/frontend/typecheck/infer.ml`
- `lib/frontend/typecheck/resolution_artifacts.ml`

Required tests:
- coercion from concrete value to `Dyn[Show]`,
- coercion through assignment, argument passing, and return-position separately,
- dynamic method call success/failure,
- `Dyn[FieldOnlyTrait]` rejection,
- `Dyn[Show & FieldOnlyTrait]` rejection,
- missing impl diagnostics,
- no fallback to legacy field-only trait-as-type behavior,
- emitter packaging occurs only at recorded coercion sites.

Exit criteria:
- The typechecker can represent, check, and resolve trait-object values without special parser-era hacks.

### Phase B4. Codegen, Docs, And End-To-End Verification
Purpose:
- Land runtime support and freeze the user-visible model.

Tasks:
- Emit witness tables and trait-object package values in the Go backend.
- Ensure dynamic dispatch uses `DynamicTraitMethod` plus `trait_object_coercion_map` rather than re-inferring method resolution or coercion sites in the emitter.
- Add end-to-end fixtures for:
  - trait-object creation,
  - storage in collections,
  - dynamic method dispatch,
  - negative capability checks.
- Update `docs/features/traits.md` to replace the current deferred-object wording with the final accepted model.

Likely files:
- `lib/backend/go/emitter.ml`
- `docs/features/traits.md`
- `test/fixtures/traits/`
- `test/fixtures/codegen_mono/`

Exit criteria:
- Trait-object syntax, typechecking, and runtime behavior are all green.
- Docs no longer describe trait objects as deferred.

## Track C. General Intersection Types

### Current State
- `docs/SYNTAX.md` explicitly says full intersection types are not part of vNext.
- `&` is currently reserved for constraint composition only.
- `Types.mono_type` currently has `TUnion` but no intersection form in `lib/frontend/typecheck/types.ml`.
- Trait constraints, constrained-param shorthand, and supertrait expansion already depend on `&` remaining unambiguous in constraint positions.

### Binding Decisions
- Track C is accepted for this follow-up plan.
- The surface syntax is general `TypeExpr & TypeExpr`.
- The canonical type representation is `TIntersection of mono_type list`.
- Intersections are compile-time-only types, not runtime wrapper values.
- Normalization must flatten nested intersections, canonicalize members, sort deterministically, and deduplicate identical members.
- Track C must not change the meaning of `ConstraintExpr`. Constraint grammar stays as-is even if `TypeExpr` later gains intersections.
- Implement Track B first and then define the interaction of `TIntersection` with `TTraitObject`.

### Phase C0. Freeze Intersection Semantics
Purpose:
- Prevent a high-churn type-system feature from landing without a written semantic matrix.

Tasks:
- Freeze the semantic matrix before parser work starts:
  - interaction with unions,
  - interaction with records,
  - interaction with trait objects after Track B,
  - interaction with subtyping/compatibility,
  - interaction with narrowing and diagnostics.
- Document concrete motivating use cases that are not already satisfied by:
  - constrained-param shorthand,
  - multiple trait bounds,
  - `Dyn[...]`.

Likely files:
- `docs/SYNTAX.md`
- `docs/features/traits.md`
- this plan

Exit criteria:
- The semantic matrix above is written down before parser work starts.

### Phase C1. Add Canonical Representation And Syntax
Purpose:
- Introduce one canonical type form for intersections and one surface syntax for it.

Tasks:
- Extend canonical `AST.type_expr` and `Types.mono_type` with an intersection representation.
- Update `Types` helpers:
  - canonicalization,
  - pretty-printing,
  - substitution,
  - free-variable collection,
  - normalization.
- Update `docs/SYNTAX.md` grammar and examples.
- Extend `Surface_ast`, parser, and lowering to parse and lower `A & B` in type-expression position while preserving existing `ConstraintExpr` parsing behavior.
- Update syntax tooling if Track C is accepted:
  - tree-sitter,
  - TextMate grammar,
  - editor sync scripts/tests.

Likely files:
- `docs/SYNTAX.md`
- `lib/frontend/syntax/surface_ast.ml`
- `lib/frontend/syntax/ast.ml`
- `lib/frontend/syntax/parser.ml`
- `lib/frontend/syntax/lower.ml`
- `lib/frontend/typecheck/types.ml`
- `tools/tree-sitter-marmoset/**`
- `tools/vscode-marmoset/syntaxes/marmoset.tmLanguage.json`
- `tools/jetbrains-marmoset/**`

Required tests:
- parser precedence tests,
- lowering tests for nested intersections,
- normalization tests for flatten/sort/dedupe,
- syntax-tooling tests.

Exit criteria:
- Intersections have one canonical syntax and one canonical type representation.

### Phase C2. Compatibility, Unification, And Narrowing
Purpose:
- Make intersections mean something coherent in the typechecker instead of merely existing in the parser.

Tasks:
- Define and implement compatibility/subtyping rules for `TIntersection`.
- Update unification and normalization to avoid unstable or order-sensitive behavior.
- Update narrowing/pattern-analysis behavior if intersection-typed values participate in control-flow refinement.
- Define interaction with:
  - unions,
  - records,
  - trait objects,
  - constrained type variables.
- Keep diagnostics deterministic. Every rejected intersection must explain which pairing or rule is unsupported.

Likely files:
- `lib/frontend/typecheck/annotation.ml`
- `lib/frontend/typecheck/unify.ml`
- `lib/frontend/typecheck/infer.ml`
- `lib/frontend/typecheck/trait_solver.ml`
- `lib/frontend/typecheck/types.ml`

Required tests:
- subtyping/compatibility success and failure cases,
- unification stability tests,
- interaction with unions and records,
- interaction with trait objects if Track B was accepted.

Exit criteria:
- The typechecker has a coherent, deterministic model for intersections.

### Phase C3. End-To-End Verification And Docs
Purpose:
- Finalize the feature or reject it cleanly after implementation evidence.

Tasks:
- Add end-to-end fixtures for accepted intersection use cases.
- Confirm intersections erase cleanly in codegen unless a separate runtime representation is explicitly introduced later.
- Update `docs/SYNTAX.md` and any feature docs to reflect the final accepted or rejected state.

Likely files:
- `docs/SYNTAX.md`
- `lib/backend/go/emitter.ml`
- `test/fixtures/**`

Exit criteria:
- Intersections are either fully documented and green end-to-end, or explicitly rejected and removed from the implementation branch.

## Verification Strategy
- Every accepted track must have:
  - parser/lowering tests for any new syntax,
  - unit tests for new canonical type representations,
  - inference/typechecking tests for acceptance and rejection paths,
  - end-to-end fixtures,
  - docs updates in the same branch.
- Do not rely on one broad "language smoke test" to validate these tracks. Each track needs targeted positive and negative cases tied to the semantic rules above.

## Highest-Risk Areas
- Extending trait metadata enough to synthesize default-backed derives without making registry and emitter ownership ambiguous.
- Introducing trait-object runtime values without accidental dependence on Go-interface internals.
- Adding `TIntersection` without destabilizing normalization, subtyping, and diagnostics.

## Immediate Next Step
Complete Track A Phase A0 before touching compiler code for follow-up features:
- freeze user-trait derive semantics,
- update `docs/SYNTAX.md` and `docs/features/traits.md`,
- then start Track A Phase A1 on canonical trait/impl metadata.
