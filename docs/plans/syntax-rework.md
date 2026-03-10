# Syntax Rework Plan

## Summary
Rework Marmoset's surface syntax to the vNext style described in `docs/SYNTAX.md`, while keeping the compiler, fixtures, docs, LSP, and editor tooling aligned during migration.

This revision turns the plan into an implementation document, not just a policy document. It defines the migration architecture, the lowering boundary, the staged rollout, and the verification gates required to land the work without letting parser changes sprawl into the rest of the compiler.

Deferred post-rollout work remains in `docs/plans/syntax-rework-followup.md`.

## Goals
- Adopt the vNext declaration, type, trait, impl, derive, match, and lambda syntax in `docs/SYNTAX.md`.
- Introduce a hard frontend boundary: `lexer/parser -> Surface_ast -> lowering/canonicalization -> Core_ast -> resolver/typechecker/codegen`.
- Keep legacy syntax working during migration without letting dual syntax leak into the typechecker or emitter.
- Keep compiler behavior, docs, examples, tests, LSP, tree-sitter, and editor grammars in sync.
- Make rollout sequencing and removal of legacy syntax explicit.

## Non-Goals
- Changing the runtime model or backend strategy.
- Designing modules/imports beyond what is already locked in `docs/plans/module-system.md`.
- Adding a general optimization IR.
- Depending on a formatter to make rollout possible.

## Current State

### Compiler Surface Today
- Top-level functions are mostly written as `let name = fn(...) { ... }`, not `fn name(...) = ...`.
- Traits and constraint lists use `+`, not `&`.
- Trait impls use `impl Trait for Type { ... }`.
- Derive is a standalone statement: `derive eq, show for point;`.
- Match arms are `pattern: expr`, without `case`.
- Function types use `fn(int) -> string`, not `(int) -> string`.
- Field-only trait-as-type behavior still exists in annotation conversion.

### Frontend Constraints
- The lexer does not currently tokenize `override`, `case`, `&`, `&&`, `||`, `%`, or identifier suffixes like `?` / `!`.
- The parser does not have a top-level function declaration form.
- The parser does not parse trait default method bodies or vNext impl headers.
- The current AST and parser still reflect the legacy statement/expression split:
  - top-level functions are discovered through `Let(Function)`,
  - `match` arm bodies are expressions only,
  - braces in expression position currently mean record/hash literals,
  - type arrows do not carry effect information in type AST.

### Downstream Coupling
- Symbol predeclaration and top-level forward references are keyed off `AST.Let` containing `AST.Function`.
- The typechecker and emitter consume the parser AST directly today.
- Derive lowering and built-in derive behavior currently expect standalone derive declarations.
- Annotation conversion still hardcodes lowercase primitive/type spellings and legacy trait-as-type behavior.

### Tooling Blast Radius
- `test/fixtures` currently contains about 1296 `.mr` files using current syntax.
- Tree-sitter, VS Code, JetBrains, Neovim, Zed, and LSP surface printers all encode the current syntax in different ways.
- Existing editor CI scripts do not yet prove grammar parity strongly enough for a syntax migration.

## Inputs Already Locked Elsewhere
- `docs/plans/function-model.md`
  - keeps `.` as the universal qualifier,
  - keeps `->` vs `=>`,
  - locks method generics and method call type-arg syntax.
- `docs/plans/module-system.md`
  - depends on `.` as the unified qualifier,
  - will later add import/export syntax to the same parser family.
- `docs/plans/diagnostics-rework.md`
  - already treats harness adaptation as a first-class migration concern,
  - should shape migration diagnostics and staged verification.

## Binding Decisions

### 1. Canonical Casing
- Marmoset must have one canonical casing table for language entities.
- Canonical casing is:
  - `UpperCamel` for types, traits, enums, enum variants, and primitive type names,
  - lowercase or `snake_case` for values and functions,
  - lowercase for generic type parameters.
- `docs/SYNTAX.md` is the reference for this table.
- Phase 0 is not complete until `docs/SYNTAX.md`, the authoritative examples, and the vNext canary suite all use this exact casing.

### 2. Backward Compatibility
- Legacy syntax remains supported during migration.
- Dual syntax is allowed only in the surface parser and lowering pipeline.
- Resolver, typechecker, emitter, LSP typed analysis, and editor-facing semantic logic must consume only canonical lowered syntax.
- Legacy syntax support is temporary and will be removed in the final cleanup phase.

### 3. Frontend Boundary
- Introduce two explicit frontend representations:
  - `Surface_ast` (`lib/frontend/syntax/surface_ast.ml`): parser output; preserves source-level syntax, including legacy/vNext distinctions and temporary migration-only forms.
  - `Core_ast`: the canonical downstream AST, which continues to live in the existing `Syntax.Ast.AST` module.
- This is a frontend normalization boundary, not a general compiler IR.
- `Core_ast` is not frozen. The module name stays and downstream still consumes `AST.program`, but the canonical AST is allowed the small number of real structural changes required by vNext:
  - `AST.TArrow` records purity/effect explicitly,
  - `AST.expr_kind` gains one canonical `BlockExpr` form for expression-position blocks,
  - `AST.method_impl` gains `impl_method_override : bool`.
- Surface-only constructs must not be encoded as `AST.expr_kind`, `AST.type_expr`, or `AST.pattern`.
- `Surface_ast` may reuse small leaf payload types or helper records where structure is identical, but parser output must remain statically distinct from canonical `AST`.
- `Core_ast` does not over-unify semantically different method families:
  - trait declarations stay distinct from impls,
  - trait impls stay distinct from inherent impls,
  - trait impl methods and inherent methods may share a common canonical method-body record where their structure is identical,
  - trait declaration methods remain separate because signatures, defaults, and `override`-related validation have different semantics.

### 4. Canonical Lowering Target
- Top-level `fn name[...] (...) [-> T | => T] = expr_or_block` lowers to the same canonical top-level function form as legacy `let name = fn(...) { ... }`.
- Legacy and vNext impl syntax both lower to one canonical impl representation.
- Postfix derive lowers to one canonical derive representation before typechecking.
- Explicit lambdas and placeholder lambdas both lower to canonical function expressions.
- `case`-based match syntax lowers to one canonical match-arm representation.
- Parser owns brace disambiguation from `docs/SYNTAX.md`: it decides record vs map vs block at parse time, and lowering never re-decides brace meaning.
- Effectful function types lower to a type AST that records purity/effect explicitly.
- Constrained-param shorthand lowers to explicit generic binders and explicit parameter types.
- Legacy field-only trait-as-type stays legacy-only during migration. It must not be confused with vNext constrained-param shorthand in the lowered representation.
- Method lowering is only partially unified:
  - trait impl methods and inherent methods may lower to the same canonical method-body record,
  - trait declaration methods lower to a distinct canonical method-signature/default-method form,
  - trait impl parent nodes and inherent impl parent nodes remain distinct in `Core_ast`,
  - `override` survives lowering only on `AST.method_impl` for trait impl methods; trait declarations keep default bodies but do not carry `override`.

### 5. Semicolon Policy
- Parser accepts optional semicolons during migration.
- Canonical vNext style remains semicolon-free.
- Rollout does not depend on a formatter. Until a formatter exists, docs, canaries, tests, and compiler diagnostics carry the policy.

### 6. Derive Scope For First Rollout
- Syntax rework only needs parser/lowering support for user-trait derive syntax.
- Full user-trait derive semantics remain follow-up work in `docs/plans/syntax-rework-followup.md`.

## Immediate Architecture Choice
The central implementation choice for this plan is:

`source -> lexer -> parser -> Surface_ast -> lower/canonicalize -> Core_ast -> resolve/typecheck -> codegen`

This boundary exists to prevent the rest of the compiler from learning about:
- `fn name(...) = ...` vs `let name = fn(...) { ... }`,
- postfix derive vs standalone derive,
- `case` vs legacy match arms,
- placeholder shorthand vs explicit lambdas,
- legacy-only trait-as-type vs vNext constrained-param shorthand,
- temporary migration-only syntax.

## Concrete Representations

### Core_ast Is The Canonical Downstream AST

`Core_ast` continues to be the existing `Syntax.Ast.AST` module in `lib/frontend/syntax/ast.ml`. The name `AST` stays during migration. Stability here means downstream APIs still traffic in `AST.program`, `AST.statement`, and `AST.expression`; it does not mean every downstream pattern match remains untouched.

The canonical AST deltas locked by this plan are:
- `AST.TArrow` grows an explicit effect bit.
- `AST.expr_kind` gains one canonical `BlockExpr` variant for expression-position blocks.
- `AST.method_impl` gains `impl_method_override : bool`.
- No surface-only `Placeholder`, `PlaceholderLambda`, `case`, or brace-disambiguation sentinel variants may be added to `AST.expr_kind`.

### Surface_ast Is New

`Surface_ast` is a new module (`lib/frontend/syntax/surface_ast.ml`) produced by the parser. It owns every syntax-divergent or migration-only construct that must disappear before typechecking. `Surface_ast` is a fully separate type family from `AST` — the parser produces surface types, and lowering converts the entire tree (declarations, expressions, types, patterns) into canonical `AST` types.

Binding rules for `Surface_ast`:
- `Surface.surface_program` is the parser result.
- `Surface_ast` has its own declaration, expression, type, and pattern types. It must not reuse `AST.expression`, `AST.type_expr`, or `AST.pattern` as its tree types.
- Top-level `fn` declarations, postfix derive, explicit arrow lambdas, placeholder shorthand, and expression-position blocks exist only in `Surface_ast` before lowering.
- Trait declarations preserve default method bodies; `override` exists only on impl members, never on trait signatures.
- Parser applies `docs/SYNTAX.md` brace disambiguation and emits surface record, map, or block nodes directly.
- Legacy and vNext impl headers coexist only in `Surface_ast`; `Core_ast` sees one canonical impl representation.
- Small leaf payload types (e.g., `literal_value`, `generic_param`) may be shared with `AST` where the structure is identical and no surface-only variant exists.

Key Surface_ast types (OCaml sketch):

```ocaml
(* lib/frontend/syntax/surface_ast.ml *)
module Surface = struct

  (* ── Surface type expressions ── *)
  type surface_type_expr =
    | STVar of string
    | STCon of string
    | STApp of string * surface_type_expr list
    | STArrow of surface_type_expr list * surface_type_expr * bool
        (* bool = is_effectful; covers both (a) -> b and (a) => b *)
    | STUnion of surface_type_expr list
    | STRecord of surface_record_type_field list * surface_type_expr option

  and surface_record_type_field = {
    sf_name : string;
    sf_type : surface_type_expr;
  }

  and surface_variant_def = {
    sv_name : string;
    sv_fields : surface_type_expr list;
  }

  (* ── Surface patterns ── *)
  type surface_pattern_kind =
    | SPWildcard
    | SPVariable of string
    | SPLiteral of AST.literal_value  (* shared leaf *)
    | SPConstructor of string * string * surface_pattern list
    | SPRecord of surface_record_pat_field list * string option

  and surface_pattern = {
    sp_pat : surface_pattern_kind;
    sp_pos : int;
    sp_end_pos : int;
    sp_file_id : string option;
  }

  and surface_record_pat_field = {
    sp_field_name : string;
    sp_field_pattern : surface_pattern option;
  }

  (* ── Surface expressions ── *)
  type surface_expr_kind =
    (* — Carried over from AST, structurally identical — *)
    | SEIdentifier of string
    | SEInteger of int64
    | SEFloat of float
    | SEBoolean of bool
    | SEString of string
    | SEArray of surface_expr list
    | SEIndex of surface_expr * surface_expr
    | SEHash of (surface_expr * surface_expr) list
    | SEPrefix of string * surface_expr
    | SEInfix of surface_expr * string * surface_expr
    | SETypeCheck of surface_expr * surface_type_expr
    | SEIf of surface_expr * surface_stmt * surface_stmt option
    | SECall of surface_expr * surface_expr list
    | SEEnumConstructor of string * string * surface_expr list
    | SEMatch of surface_expr * surface_match_arm list
    | SERecordLit of surface_record_field list * surface_expr option
    | SEFieldAccess of surface_expr * string
    | SEMethodCall of {
        se_receiver : surface_expr;
        se_method : string;
        se_type_args : surface_type_expr list option;
        se_args : surface_expr list;
      }
    (* — Legacy function expression (fn(...) { ... }) — *)
    | SEFunction of {
        se_generics : AST.generic_param list option;  (* shared leaf *)
        se_params : (string * surface_type_expr option) list;
        se_return_type : surface_type_expr option;
        se_is_effectful : bool;
        se_body : surface_stmt;
      }
    (* — vNext surface-only forms — *)
    | SEArrowLambda of {
        se_lambda_params : (string * surface_type_expr option) list;
        se_lambda_is_effectful : bool;
        se_lambda_body : surface_expr_or_block;
      }
        (* (x) -> expr  or  (x, y) => expr *)
    | SEPlaceholder
        (* _ in expression position; rewritten to SEArrowLambda or rejected in lowering *)
    | SEBlockExpr of surface_block
        (* { let x = 1; x + 2 } in expression position;
           parser has already decided this is a block, not a record/hash *)

  and surface_expr = {
    se_id : int;
    se_expr : surface_expr_kind;
    se_pos : int;
    se_end_pos : int;
    se_file_id : string option;
  }

  and surface_block = {
    sb_stmts : surface_stmt list;
    sb_pos : int;
    sb_end_pos : int;
    sb_file_id : string option;
  }

  and surface_expr_or_block =
    | SEOBExpr of surface_expr
    | SEOBBlock of surface_block

  and surface_record_field = {
    se_field_name : string;
    se_field_value : surface_expr option;  (* None = punning *)
  }

  and surface_match_arm = {
    se_patterns : surface_pattern list;
    se_arm_body : surface_expr_or_block;
        (* vNext arms use case keyword and allow block bodies;
           legacy arms allow expression only *)
  }

  (* ── Surface statements ── *)
  and surface_stmt_kind =
    | SSLet of {
        ss_name : string;
        ss_value : surface_expr;
        ss_type_annotation : surface_type_expr option;
      }
    | SSReturn of surface_expr
    | SSExpressionStmt of surface_expr
    | SSBlock of surface_block

  and surface_stmt = {
    ss_stmt : surface_stmt_kind;
    ss_pos : int;
    ss_end_pos : int;
    ss_file_id : string option;
  }

  (* ── Surface top-level declarations ── *)
  type surface_generic_param = AST.generic_param  (* shared leaf *)

  type surface_method_sig = {
    sm_id : int;
    sm_name : string;
    sm_generics : surface_generic_param list option;
    sm_params : (string * surface_type_expr) list;
    sm_return_type : surface_type_expr;
    sm_effect : AST.effect_annotation;  (* shared leaf *)
    sm_default_impl : surface_expr_or_block option;
  }

  type surface_method_impl = {
    smi_id : int;
    smi_name : string;
    smi_generics : surface_generic_param list option;
    smi_params : (string * surface_type_expr option) list;
    smi_return_type : surface_type_expr option;
    smi_effect : AST.effect_annotation option;
    smi_override : bool;
    smi_body : surface_expr_or_block;
  }

  type top_decl =
    | SLet of {
        name : string;
        value : surface_expr;
        type_annotation : surface_type_expr option;
      }
    | SFnDecl of {
        name : string;
        generics : surface_generic_param list option;
        params : (string * surface_type_expr option) list;
        return_type : surface_type_expr option;
        is_effectful : bool;
        body : surface_expr_or_block;
      }
    | SEnumDef of {
        name : string;
        type_params : string list;
        variants : surface_variant_def list;
        derive : string list;
      }
    | STypeDef of {
        alias_name : string;
        alias_type_params : string list;
        alias_body : surface_type_expr;
        derive : string list;
      }
    | STraitDef of {
        name : string;
        type_param : string option;
        supertraits : string list;
        fields : surface_record_type_field list;
        methods : surface_method_sig list;
      }
    | SImplDef of {
        impl_type_params : surface_generic_param list;
        impl_trait_name : string;
        impl_for_type : surface_type_expr;
        impl_methods : surface_method_impl list;
      }
    | SInherentImplDef of {
        inherent_for_type : surface_type_expr;
        inherent_methods : surface_method_impl list;
      }
    | SDeriveDef of {
        derive_traits : AST.derive_trait list;  (* shared leaf *)
        derive_for_type : surface_type_expr;
      }
    | SExpressionStmt of surface_expr
    | SReturn of surface_expr
    | SBlock of surface_block

  type surface_program = top_decl list
end
```

Design notes:
- Every syntax-divergent form (`SEArrowLambda`, `SEPlaceholder`, `SEBlockExpr`, `SFnDecl`, postfix derive, `smi_override`) lives only in `Surface_ast`. None of these can appear in `AST.expr_kind` or `AST.stmt_kind`.
- Shared leaf types (`literal_value`, `generic_param`, `effect_annotation`, `derive_trait`) are reused from `AST` because they have no surface-only variants and no migration-only concerns.
- `surface_variant_def` is separate from `AST.variant_def` because enum payload types participate in surface-to-core lowering through `surface_type_expr`.
- `surface_expr_or_block` is used for fn declaration bodies, trait default method bodies, impl method bodies, and match arm bodies wherever vNext allows either an expression or a braced block.
- `surface_block` carries explicit brace-span metadata. This is required so lowering can synthesize canonical `AST.BlockExpr` nodes and canonical block statements even for empty blocks.
- The sketch is a starting point. Field names and minor structural choices may evolve during implementation. The binding requirement is that surface and canonical types are statically distinct type families.

### Shared ID Supply

Introduce `lib/frontend/syntax/id_supply.ml`:

```ocaml
module Id_supply : sig
  type t = { mutable next_id : int }
  val create : int -> t
  val fresh : t -> int
end
```

Binding rules:
- Parser state stores `id_supply : Id_supply.t` instead of a raw `next_id : int`.
- `fresh_id` in `parser.ml` delegates to `Id_supply.fresh`.
- `parse_surface` returns both the surface program and the post-parse `Id_supply.t`.
- Lowering consumes that same `Id_supply.t` when it needs to mint synthetic canonical nodes.
- Synthetic IDs must therefore be globally unique with respect to parser-assigned IDs from the same source file.

### Lowering Module Signature

```ocaml
(* lib/frontend/syntax/lower.ml *)
val lower_expr : Id_supply.t -> Surface.surface_expr -> AST.expression
val lower_type_expr : Surface.surface_type_expr -> AST.type_expr
val lower_pattern : Surface.surface_pattern -> AST.pattern
val lower_stmt : Id_supply.t -> Surface.surface_stmt -> AST.statement
val lower_expr_or_block_to_expr : Id_supply.t -> Surface.surface_expr_or_block -> AST.expression
val lower_expr_or_block_to_stmt : Id_supply.t -> Surface.surface_expr_or_block -> AST.statement
val lower_top_decl : Id_supply.t -> Surface.top_decl -> AST.statement list
val lower_program : Id_supply.t -> Surface.surface_program -> AST.program
(** Exact canonical rules:
    - SFnDecl -> Let(Function)
    - postfix derive -> EnumDef/TypeAlias + DeriveDef
    - explicit arrow lambdas and placeholder shorthand -> Function
    - surface block expr -> canonical AST.BlockExpr
    - parser-proven record/map nodes -> AST.RecordLit / AST.Hash
    - legacy and vNext impl headers -> canonical ImplDef / InherentImplDef

    Expr-or-block lowering is context-sensitive and must be implemented by two helpers:

    lower_expr_or_block_to_stmt:
    - SEOBExpr e -> AST.Block [ AST.ExpressionStmt (lower_expr e) ]
      This is a binding constraint. Canonical function and impl bodies remain statement-bodied,
      and expression-bodied vNext forms are wrapped in a one-statement block so existing
      infer_block / emit_func_body tail-expression behavior remains correct.
    - SEOBBlock b -> AST.Block (List.map (lower_stmt id_supply) b.sb_stmts)

    lower_expr_or_block_to_expr:
    - SEOBExpr e -> lower_expr e
    - SEOBBlock b ->
        AST.{ id = Id_supply.fresh id_supply; expr = BlockExpr lowered_stmts;
              pos = b.sb_pos; end_pos = b.sb_end_pos; file_id = b.sb_file_id }
      This is used for trait default methods and match-arm bodies, where canonical AST still
      expects an expression. *)
```

### Parser API Strategy

`parser.ml` should expose two entrypoints:
- `parse_surface : file_id:string -> string -> (parse_surface_result, Diagnostic.t list) result`
- `parse : file_id:string -> string -> (AST.program, Diagnostic.t list) result`

Where:

```ocaml
type parse_surface_result = {
  program : Surface.surface_program;
  id_supply : Id_supply.t;
}
```

`Parser.parse` stays the default public entrypoint and is implemented as:
- call `parse_surface`,
- pass `result.id_supply` and `result.program` to `Lower.lower_program`,
- return the lowered `AST.program`.

This keeps the boundary real without forcing a repo-wide parser return-type migration in Phase 0.5.

Current direct `Parser.parse` callers that must either remain on the lowered API or be migrated intentionally include:
- `lib/frontend/typecheck/checker.ml`
- `lib/frontend/typecheck/infer.ml` (tests/helpers)
- `lib/backend/go/emitter.ml`
- `tools/lsp/lib/doc_state.ml`
- `tools/lsp/lib/doc_symbols.ml`
- `tools/lsp/lib/folding_ranges.ml`
- `tools/lsp/lib/selection_ranges.ml`
- `tools/lsp/lib/edge_case_tests.ml`

Do not scatter ad hoc `parse_surface |> lower_program` chains across the repo.

### expr_id Strategy

`Infer.type_map` is keyed by `AST.expression.id` values in the lowered program. Since Surface and Core are separate type families, lowering always creates new `AST.expression` nodes.

ID assignment strategy:
- **1:1 lowerings** (most nodes): copy `se_id` from the surface node to `AST.expression.id`. This preserves the source-position link so diagnostics from infer/emitter point at the right source spans without extra bookkeeping.
- **Synthetic nodes** (placeholder lambda desugaring creates a `Function` node with no surface counterpart; `SEOBBlock` in expression context creates `AST.BlockExpr`): mint fresh IDs with `Id_supply.fresh result.id_supply`, where `result` is the output of `parse_surface`.
- **Span copying**: `se_pos`, `se_end_pos`, `se_file_id` are copied alongside the ID for the same reason.

The design must not depend on physical identity equality between Surface and Core expressions — they are always different OCaml values — but it does depend on ID value equality for diagnostics traceability.

### Top-Level fn Parse Strategy

The parser distinguishes `fn name(...)` (declaration) from `fn(...)` (lambda) using one-token lookahead:

- In `parse_statement`, when `curr_token` is `Token.Function`:
  - If `peek_token` is `Token.Ident`, parse as `SFnDecl` (vNext top-level function declaration).
  - Otherwise, fall through to expression parsing, which handles `fn(...)` lambdas as today.
- This requires no new keywords or tokens. The existing `Token.Function` token (which represents `fn`) is sufficient.

### Forward Reference Preservation

`predeclare_top_level_lets` in `infer.ml` scans for `AST.Let { value = { expr = Function _ } }` to enable forward references. Because `SFnDecl` lowers to exactly `AST.Let { name; value = Function { ... } }`, forward reference resolution works with zero changes to `infer.ml`.

This is a binding constraint: the lowering of `fn name(...)` must produce `Let(Function)`, not a new `FnDecl` AST node.

## Normalization Contract
The lowering pass must make the following transformations explicit and tested.

### Top-Level Functions
- vNext `fn name[...] (params) -> T = expr_or_block` lowers to `AST.Let { name; value = AST.Function { generics; params; return_type; is_effectful; body } }`.
- Legacy `let name = fn[...] (params) -> T { ... }` lowers to the same `AST.Let(Function)` form.
- Canonical body rule for function-like statement-bodied forms:
  - `SEOBExpr e` lowers to `AST.Block [ AST.ExpressionStmt (lower_expr e) ]`
  - `SEOBBlock b` lowers to `AST.Block (List.map lower_stmt b.sb_stmts)`
  - This rule is binding for top-level functions, legacy `fn(...) { ... }` literals, trait impl methods, and inherent impl methods.
- This is a binding constraint: `predeclare_top_level_lets` in `infer.ml` depends on scanning for `Let(Function)` to enable forward references. No new top-level function AST node may be introduced.

### Lambdas
- Explicit lambda syntax lowers to canonical function expressions.
- Placeholder shorthand lowers to canonical single-argument pure lambdas.
- Placeholder rewrite is rejected for zero-placeholder and multi-placeholder cases according to the spec.

### Derive
- Postfix derive on `type` and `enum` lowers to the canonical derive representation expected by downstream code.
- Legacy standalone derive lowers to the same canonical representation during migration.

### Traits And Impls
- Legacy `impl Trait for Type { ... }` and vNext `impl[...] Trait[Type] = { ... }` lower to one canonical impl shape.
- Trait default methods survive lowering on trait declarations as:
  - `None` for required methods,
  - `Some (lower_expr_or_block_to_expr ...)` for defaulted methods.
- Impl-member `override` survives lowering on `AST.method_impl`.
- Trait impl methods and inherent methods may share one canonical method-body record after lowering.
- Trait declaration methods must stay separate from impl methods in canonical form.
- Trait impls and inherent impls remain distinct canonical parent nodes even if they share child method-body structure.

### Match And Blocks
- `case` arms lower to canonical match arms.
- Canonical body rule for expression-bodied forms:
  - `SEOBExpr e` lowers to `lower_expr e`
  - `SEOBBlock b` lowers to a fresh `AST.BlockExpr (List.map lower_stmt b.sb_stmts)` carrying `b` span metadata
  - This rule is binding for match-arm bodies and trait default method bodies.
- Block bodies used in expression position lower to a first-class canonical block-expression form.
- Parser applies the brace disambiguation rule from `docs/SYNTAX.md`; lowering consumes that already-resolved surface form without re-deciding it.

### Types And Constraints
- Function types lower to a type form with an explicit effect bit.
- `&` constraint composition lowers to an explicit constraint representation.
- Constrained-param shorthand lowers to explicit generic binders.
- Lowering must be the only place where legacy field-only trait-as-type and vNext shorthand are disambiguated.

## Phase Plan

### Phase 0. Freeze Spec, Casing, And Canary Examples
Purpose:
- Remove contradictions before compiler work starts.

Tasks:
- Reconcile `docs/SYNTAX.md` with the locked migration architecture in this plan.
- Collapse all docs/examples/canaries to the locked `UpperCamel` type namespace.
- Replace the conflicting `examples/new-syntax*.mr` files with one authoritative vNext example set.
- Add a short migration-policy section to `docs/SYNTAX.md` or this plan:
  - legacy syntax remains accepted temporarily,
  - dual syntax exists only above lowering,
  - removal happens in the final cleanup phase.
- Record that semicolon policy does not depend on a formatter.

Likely files:
- `docs/SYNTAX.md`
- `docs/plans/syntax-rework.md`
- `examples/new-syntax*.mr`

Exit criteria:
- `docs/SYNTAX.md`, this plan, and the authoritative vNext examples agree on casing, derive placement, trait/impl grammar, match syntax, and lambda syntax.
- There is exactly one public vNext example set.
- The compatibility/removal story is written down explicitly.

### Phase 0.5. Introduce Surface/Core Boundary
Purpose:
- Create the hard boundary that keeps syntax migration out of downstream compiler logic.

Concrete implementation strategy:
- `Core_ast` remains the existing `Syntax.Ast.AST` module and is the only AST seen below the syntax frontend. It may receive the targeted structural changes locked in "Concrete Representations."
- `Surface_ast` is a new module at `lib/frontend/syntax/surface_ast.ml`. It defines `Surface.top_decl`, `Surface.surface_expr`, and related types as specified above.
- `Id_supply` is a new module at `lib/frontend/syntax/id_supply.ml`.
- `Lower` is a new module at `lib/frontend/syntax/lower.ml` with signature `val lower_program : Id_supply.t -> Surface.surface_program -> AST.program`.
- `parser.ml` grows `parse_surface`, which returns both the surface program and the shared `Id_supply.t`. Existing `Parser.parse` remains the public lowered entrypoint and internally performs `parse_surface` followed by `Lower.lower_program result.id_supply result.program`.
- In this phase, the only `Surface.top_decl` variants actually emitted by `parse_surface` are the legacy ones. vNext-only variants (`SFnDecl`, postfix derive, arrow lambdas, placeholder nodes, block expressions) are defined but not yet parsed.
- The lowering pass in this phase is a near-identity transform: each `S*` variant maps directly to its `AST.*` counterpart.

Tasks:
1. **Create `lib/frontend/syntax/surface_ast.ml`** with the full surface type family from "Concrete Representations":
   - `surface_type_expr`, `surface_record_type_field`, `surface_variant_def`, `surface_pattern`, `surface_expr`, `surface_block`, `surface_stmt`, `top_decl`, `surface_program`.
   - Include vNext-only variants (`SFnDecl`, `SEArrowLambda`, `SEPlaceholder`, `SEBlockExpr`, `smi_override`) even though they won't be parsed yet — they must exist so the types are complete.
   - Share leaf types (`literal_value`, `generic_param`, `effect_annotation`, `derive_trait`) from `AST`.
2. **Create `lib/frontend/syntax/id_supply.ml`**:
   - `type t = { mutable next_id : int }`
   - `create : int -> t`
   - `fresh : t -> int`
3. **Create `lib/frontend/syntax/lower.ml`** implementing `lower_program` for legacy variants only:
   - `lower_expr : Id_supply.t -> surface_expr -> AST.expression` — recursive walk converting `SE*` variants to `AST.*` variants. In this phase every `SE*` variant has a direct `AST` counterpart; vNext-only variants (`SEArrowLambda`, `SEPlaceholder`, `SEBlockExpr`) raise an internal error if reached.
   - `lower_type_expr : surface_type_expr -> AST.type_expr` — converts `ST*` to `AST.T*`.
   - `lower_pattern : surface_pattern -> AST.pattern` — converts `SP*` to `AST.P*`.
   - `lower_stmt : Id_supply.t -> surface_stmt -> AST.statement` — converts `SS*` to `AST.*`.
   - `lower_expr_or_block_to_stmt : Id_supply.t -> surface_expr_or_block -> AST.statement`
     - `SEOBExpr e -> AST.Block [AST.ExpressionStmt (lower_expr id_supply e)]`
     - `SEOBBlock b -> AST.Block (List.map (lower_stmt id_supply) b.sb_stmts)`
   - `lower_expr_or_block_to_expr : Id_supply.t -> surface_expr_or_block -> AST.expression`
     - `SEOBExpr e -> lower_expr id_supply e`
     - `SEOBBlock b -> fresh `AST.BlockExpr` using `Id_supply.fresh id_supply`
   - `lower_top_decl : Id_supply.t -> top_decl -> AST.statement list` — converts each `S*` declaration to one or more `AST` statements (e.g., `SEnumDef` with derive produces `[EnumDef; DeriveDef]`).
   - `lower_program : Id_supply.t -> surface_program -> AST.program` — maps `lower_top_decl` over declarations, flattens.
   - Copy `se_id` → `AST.expression.id` and spans for 1:1 lowerings. Use `Id_supply.fresh` for synthetic nodes only.
4. **Refactor parser internals** so all expression/type/pattern/statement-producing functions return `Surface.*` types instead of `AST.*` types:
   - `mk_expr` → `mk_surface_expr` producing `surface_expr`.
   - `mk_stmt` → `mk_surface_stmt` producing `surface_stmt`.
   - Parser state field `next_id : int` → `id_supply : Id_supply.t`.
   - `fresh_id` delegates to `Id_supply.fresh`.
   - `parse_expression` and all `parse_*` expression helpers return `surface_expr`.
   - `parse_type_expr`, `parse_type_atom` return `surface_type_expr`.
   - `parse_pattern` returns `surface_pattern`.
   - `parse_statement` returns `surface_stmt` (for block-level statements).
   - `parse_block_statement` returns `surface_block`.
   - `parse_program` wraps top-level parsed forms in `Surface.top_decl` variants and returns `surface_program`.
   - This is the bulk of Phase 0.5 work — the parser is ~3100 lines and most functions change their return type.
   - Strategy: start from leaf parsers (literals, identifiers, type atoms), work outward to compound expressions, then statements, then top-level. Each layer compiles once its callees are converted.
5. **Expose two parser entrypoints**:
   - `parse_surface : file_id:string -> string -> (parse_surface_result, Diagnostic.t list) result`
   - `type parse_surface_result = { program : Surface.surface_program; id_supply : Id_supply.t }`
   - `parse : file_id:string -> string -> (AST.program, Diagnostic.t list) result` — existing public API, implemented as `let* r = parse_surface ... in Ok (Lower.lower_program r.id_supply r.program)`.
6. **Audit current direct `Parser.parse` callers** (`checker`, `infer` tests/helpers, `emitter`, `doc_state`, `doc_symbols`, `folding_ranges`, `selection_ranges`, `edge_case_tests`) and confirm they stay on `Parser.parse`. No caller should need `parse_surface` in this phase.
7. If any module truly needs both layers, add one shared helper inside the syntax frontend rather than open-coding `parse_surface |> Lower.lower_program` at each callsite.
8. **Add unit tests in `lower.ml`** showing legacy syntax round-trips through Surface → Lower → AST identically. Test at minimum:
   - `let x = 42` → `AST.Let`
   - `let f = fn(x: int) -> int { x + 1 }` → `AST.Let(Function)`
   - `enum color { red, green, blue }` → `AST.EnumDef`
   - `type point = { x: int, y: int }` → `AST.TypeAlias`
   - `trait show[a] { fn show(x: a) -> string }` → `AST.TraitDef`
   - `impl show for int { fn show(x: int) -> string { ... } }` → `AST.ImplDef`
   - `impl point { fn sum(p: point) -> int { ... } }` → `AST.InherentImplDef`
   - `derive eq, show for color` → `AST.DeriveDef`
   - Match expressions, if expressions, record literals, hash literals, method calls.
9. **Update `dune` files** to include `surface_ast`, `id_supply`, and `lower` modules.

Files changed:
- `lib/frontend/syntax/surface_ast.ml` (new)
- `lib/frontend/syntax/id_supply.ml` (new)
- `lib/frontend/syntax/lower.ml` (new)
- `lib/frontend/syntax/parser.ml` (`parse_surface` + lowered `parse`)
- `lib/frontend/syntax/dune` (add new modules)
- targeted callsites only if any require explicit surface access

Exit criteria:
- `Parser.parse_surface` returns `parse_surface_result`.
- `Parser.parse` still returns `AST.program`.
- `Lower.lower_program result.id_supply result.program` converts it to `AST.program`.
- No non-syntax module consumes `Surface_ast` by default.
- All existing unit tests (`make unit`) pass unchanged.
- All existing integration tests (`make integration`) pass unchanged.
- Lowering unit tests verify that each legacy `Surface.top_decl` variant produces the expected `AST.stmt_kind`.

### Phase 1. Extend Lexer And Parser For Legacy + vNext Surface Syntax
Purpose:
- Parse the full vNext surface while preserving temporary legacy support.

Tasks:

**1a. Lexer token additions** (`token.ml`, `lexer.ml`):
- Add tokens: `Override`, `Case`, `Ampersand` (`&`), `AmpAmp` (`&&`), `PipePipe` (`||`), `Percent` (`%`).
- Allow `?` and `!` as trailing characters in identifier tokens (e.g., `admin?`, `panic!`).
- No new keywords beyond `override` and `case`. All other changes are punctuation/operator tokens.

**1b. Top-level `fn` declaration** (`parser.ml`):
- In `parse_statement`, when `curr_token` is `Token.Function` and `peek_token` is `Token.Ident`, parse as `SFnDecl`:
  - consume `fn`, identifier (name), optional `[generics]`, `(params)`, optional `->`/`=>` plus return type, `=`, body (expression or block).
  - Parse the right-hand side into `surface_expr_or_block`:
    - `= expr` -> `SEOBExpr expr`
    - `= { ... }` -> `SEOBBlock block`
  - Emit `Surface.SFnDecl { name; generics; params; return_type; is_effectful; body }`.
- When `peek_token` is not `Token.Ident`, fall through to expression parsing (legacy `fn(...)` lambda). No ambiguity.

**1c. Trait grammar rewrite** (`parser.ml`):
- Parse vNext trait headers: `trait Name[a]: Constraint & Constraint = { ... }`.
  - `&` replaces `+` for constraint composition.
  - `= {` replaces bare `{`.
- Parse trait default method bodies: `fn method_name(params) -> T = expr_or_block`.
  - Required methods keep `sm_default_impl = None`.
  - Defaulted methods parse the body as `surface_expr_or_block` and store `sm_default_impl = Some ...`.
- Do not introduce `override` on trait signatures; `override` is an impl-member-only concept.
- Keep legacy `trait name[a]: c + c { ... }` parseable during migration.

**1d. Impl grammar rewrite** (`parser.ml`):
- Parse vNext impl headers: `impl[a: Show] Show[List[a]] = { ... }` and `impl List[a] = { ... }`.
  - Binder list is `[generic_params]` before the trait/type name.
  - `= {` replaces bare `{`.
- Parse impl members as:
  - optional leading `override`,
  - `fn` name, params, optional purity arrow plus return type,
  - `= expr_or_block` parsed into `surface_expr_or_block`,
  - stored in `surface_method_impl` with `smi_override` and `smi_body`.
- Distinguish trait impl (`impl ... TraitName[Type] = { ... }`) from inherent impl (`impl ... Type = { ... }`) by checking whether the target has `TraitName[...]` shape.
- Keep legacy `impl Trait for Type { ... }` parseable during migration.

**1e. Type/enum/postfix-derive** (`parser.ml`, `surface_ast.ml`):
- After parsing `enum Name = { ... }` or `type Name = ...`, check for trailing `derive Trait1, Trait2` and store in `SEnumDef.derive` / `STypeDef.derive`.
- Keep legacy standalone `derive trait1, trait2 for Type` parseable.

**1f. Lambda, block-expression, and match** (`parser.ml`, `ast.ml`):
- Parse explicit lambda: `(params) -> expr` or `(params) => expr` in expression position. This is disambiguated from parenthesized expressions by checking for `->` / `=>` after the closing `)` and emitted as a surface lambda node.
- Parse placeholder `_` in expression position as a surface placeholder node. Placeholder-lambda rewrite happens in lowering, not in the parser.
- Parse `case`-based match arms into a surface match-arm form whose body is `surface_expr_or_block`:
  - `case pat: expr` -> `SEOBExpr expr`
  - `case pat: { ... }` -> `SEOBBlock block`
  - legacy `pat: expr` remains parseable during migration and maps to the same surface shape without the `case` token.
- Parser owns brace disambiguation from `docs/SYNTAX.md` §13.3 and must emit one of:
  - surface record literal,
  - surface map/hash literal,
  - surface block expression.
- Surface block expressions lower to canonical `AST.BlockExpr`. Lowering never revisits the brace decision.
- Keep legacy match arms (without `case`) and legacy `fn(...)` lambdas parseable.

**1g. Type expression changes** (`parser.ml`, `ast.ml`):
- Parse `(int, string) -> bool` as a function type (replaces legacy `fn(int, string) -> bool`).
- Parse `(int) => bool` as an effectful function type.
- Add effect bit to `AST.TArrow`: change `TArrow of type_expr list * type_expr` to `TArrow of type_expr list * type_expr * bool` where the bool is `true` for effectful.
- Keep legacy `fn(int) -> bool` type syntax parseable during migration.
- Parse `&` in constraint positions (generic param bounds, supertrait lists).

Files changed:
- `lib/frontend/syntax/token.ml`
- `lib/frontend/syntax/lexer.ml`
- `lib/frontend/syntax/parser.ml`
- `lib/frontend/syntax/ast.ml` (canonical `BlockExpr`, `TArrow` effect bit, override flag on `method_impl`)
- `lib/frontend/syntax/surface_ast.ml`

Exit criteria:
- Parser can represent every locked legacy and vNext surface form needed during migration.
- Parser tests cover both successful and failing cases for each rewritten grammar family.
- Parser tests include explicit ambiguity/failure cases around braces, placeholder shorthand, impl headers, and derive placement.
- Each sub-workstream (1a–1g) has its own test group.

### Phase 2. Lowering, Canonicalization, And Migration Diagnostics
Purpose:
- Make all surface rewrites explicit and testable before typechecking.

Tasks:
- Lower legacy and vNext top-level functions to one canonical top-level function form.
- Lower explicit lambdas and placeholder shorthand to canonical function expressions.
- Lower postfix derive and legacy standalone derive to one canonical derive form.
- Lower legacy and vNext impl syntax to one canonical impl form.
- Lower `case` arms and surface block bodies to canonical match/block-expression forms.
- Respect parser-owned brace disambiguation; lowering must not re-decide record vs map vs block.
- Lower constrained-param shorthand to explicit generic binders.
- Introduce explicit canonical casing validation/canonicalization.
- Preserve enough surface information to emit good migration diagnostics where forms are deprecated or removed.
- Keep the legacy-only field-only trait-as-type path separate from vNext shorthand until legacy syntax is removed.

Required tests:
- Lowering tests in this phase are AST-structure tests only. They call `Lower.lower_program` (or narrower lowering helpers) and assert on the resulting canonical AST shape. They do not run `Checker.check_program*`, `Infer.infer_program`, `Resolver`, or emitter/codegen on vNext inputs that lower to newly introduced canonical nodes such as `AST.BlockExpr`; downstream semantic/codegen coverage for those nodes starts in Phase 3.
- old syntax and new syntax lower to identical `Core_ast` for equivalent programs,
- placeholder rewrite success/failure cases,
- parser-owned brace disambiguation success/failure cases,
- expr-or-block lowering in every canonical context:
  - `fn name(...) = expr` -> `AST.Function.body = AST.Block [ExpressionStmt ...]`
  - `fn name(...) = { ... }` -> `AST.Function.body = AST.Block ...`
  - trait default `= expr` -> `method_default_impl = Some expr`
  - trait default `= { ... }` -> `method_default_impl = Some(BlockExpr ...)`
  - impl method `= expr` -> `impl_method_body = AST.Block [ExpressionStmt ...]`
  - impl method `= { ... }` -> `impl_method_body = AST.Block ...`
  - `case pat: { ... }` -> `AST.match_arm.body = BlockExpr ...`
- constrained-param shorthand success/failure cases,
- derive lowering,
- impl header lowering,
- casing validation,
- deterministic diagnostics for ambiguous or invalid shorthand.

Likely files:
- `lib/frontend/syntax/lower.ml` (extend lowering for vNext forms)
- `lib/frontend/syntax/ast.ml` (canonical block-expression handling)
- parser/lowering unit tests

Exit criteria:
- Resolver/typechecker never need to branch on legacy vs vNext syntax.
- All syntax migration semantics are visible in lowering tests.
- Phase 2 test coverage does not depend on infer/emitter understanding newly introduced canonical nodes such as `AST.BlockExpr`; AST-shape assertions are sufficient here, and end-to-end semantic/codegen coverage for those nodes begins in Phase 3.
- Ambiguous shorthand forms have deterministic diagnostics.

### Phase 3. Adapt Resolver, Typechecker, Registries, And Emitter To Core Syntax
Purpose:
- Teach downstream compiler passes about the canonical syntax model and only the canonical syntax model.

Tasks:
- Update symbol predeclaration and top-level function discovery to depend on canonical function declarations only.
- Update annotation conversion for:
  - effectful function types,
  - canonical primitive/type naming,
  - the eventual removal of vNext bare trait-as-type behavior.
- Introduce canonical block-expression handling in inference and codegen.
- Add trait default method and `override` support to registries, inference, validation, and emission.
- Keep derive support working against canonical derive nodes.
- Ensure match arms accept expression or block bodies consistently via canonical lowered form.

Likely files:
- `lib/frontend/typecheck/annotation.ml`
- `lib/frontend/typecheck/infer.ml`
- `lib/frontend/typecheck/checker.ml`
- `lib/frontend/typecheck/trait_registry.ml`
- `lib/frontend/typecheck/resolution_artifacts.ml`
- `lib/backend/go/emitter.ml`

Exit criteria:
- Downstream code consumes only `Core_ast`.
- Effectful function types survive parsing and lowering without losing effect information.
- Override/default-method behavior matches the final syntax policy.
- Typechecker/codegen tests cover block expressions, derive, override, and canonical function declarations.

### Phase 4. Strengthen Verification And CI Before Bulk Migration
Purpose:
- Add the missing gates needed to safely migrate a large corpus and multiple toolchains.

Tasks:
- Add a dedicated vNext canary suite covering every rewritten syntax family.
- Keep the legacy regression suite green in parallel.
- Add paired legacy/vNext semantic-equivalence tests where both surfaces should behave the same.
- Strengthen CI/editor verification:
  - add `test/ci/tree-sitter.sh` running `tools/tree-sitter-marmoset` generation/test checks,
  - add `test/ci/grammar-sync.sh` verifying TextMate grammar copies are synchronized,
  - add `test/ci/lsp-syntax.sh` or equivalent golden-test entrypoint for user-facing syntax printers,
  - extend harness coverage so canary suites are first-class, not ad hoc examples.
- Document exactly which CI tasks must pass before bulk fixture conversion begins.

Likely files:
- `test/integration.sh`
- new vNext canary fixture directory under `test/`
- `test/ci/editor-*.sh`
- `test/ci/tree-sitter.sh` (new)
- `test/ci/grammar-sync.sh` (new)
- `test/ci/lsp-syntax.sh` (new)
- tree-sitter corpus tests
- LSP tests under `tools/lsp/lib/`

Exit criteria:
- Legacy suite is green.
- vNext canary suite is green.
- `tree-sitter test` is green.
- Grammar sync checks are green.
- LSP syntax-output tests are green.

### Phase 5. Tooling And LSP Alignment
Purpose:
- Bring editor tooling and LSP output into alignment with compiler behavior.

Tasks:
- Declare grammar ownership explicitly:
  - `tools/tree-sitter-marmoset/grammar.js` is the source of truth for structural editor grammar,
  - `tools/vscode-marmoset/syntaxes/marmoset.tmLanguage.json` is the source of truth for TextMate-based editors,
  - `tools/jetbrains-marmoset/src/main/resources/textmate/syntaxes/marmoset.tmLanguage.json` is a synced copy generated from the VS Code grammar,
  - copied/generated grammar artifacts must be synced mechanically and checked in CI.
- Add one mechanical sync path for TextMate grammars:
  - `tools/scripts/sync_textmate_grammars.sh` copies the VS Code TextMate grammar to the JetBrains location,
  - `test/ci/grammar-sync.sh` fails if the synced copy differs from source.
- Regenerate and update:
  - tree-sitter generated files and corpus tests,
  - Neovim queries,
  - VS Code grammar/config,
  - JetBrains TextMate grammar copy,
  - Zed pinned grammar commit.
- Update LSP-facing syntax renderers and labels:
  - hover,
  - signature help,
  - document symbols,
  - code actions that print source syntax,
  - keyword completions where needed.

Likely files:
- `tools/tree-sitter-marmoset/**`
- `tools/nvim-marmoset/**`
- `tools/scripts/sync_textmate_grammars.sh`
- `tools/vscode-marmoset/**`
- `tools/jetbrains-marmoset/**`
- `tools/zed-marmoset/**`
- `tools/lsp/lib/{hover,signature_help,doc_symbols,code_actions,completions,doc_state}.ml`

Exit criteria:
- Tree-sitter grammar and corpus reflect vNext syntax.
- TextMate grammars are synchronized and checked.
- Zed points at the correct grammar revision.
- LSP-facing labels and syntax printers match compiler-accepted syntax.

### Phase 6. Migrate Docs, Examples, And Fixture Corpus In Batches
Purpose:
- Convert the repo to one public syntax story without losing coverage during the transition.

Tasks:
- Migrate in batches, not one giant change:
  1. authoritative examples and feature docs,
  2. syntax-focused tests and canaries,
  3. trait/impl/derive fixtures,
  4. cross-feature and codegen fixtures,
  5. CLI snippets and remaining docs.
- Keep both suites green while batches are in progress.
- Prefer scripted or mechanical rewrites where practical, followed by targeted manual cleanup.
- Keep a migration checklist covering:
  - `examples/**`,
  - `docs/features/**`,
  - `test/fixtures/**`,
  - CLI output/help snippets,
  - syntax strings embedded in LSP tests.

Exit criteria:
- Public docs and examples use only the chosen vNext syntax.
- Fixture corpus migration is complete.
- Repo-wide syntax examples no longer contradict the canonical spec.

### Phase 7. Remove Legacy Syntax And Cleanup
Purpose:
- Delete temporary compatibility code once the migration is complete.

Tasks:
- Remove legacy parser/lowering branches.
- Remove legacy-only diagnostics and compatibility tests.
- Delete legacy fixtures or legacy fixture variants once no longer needed.
- Update `docs/ARCHITECTURE.md` to describe the new frontend pipeline.
- Add a compact migration note if public users need an upgrade guide.

Exit criteria:
- Resolver/typechecker/emitter no longer know legacy syntax ever existed.
- Legacy parser support is gone.
- Architecture docs and public docs describe the same language and the same pipeline.

## Verification Strategy
- Parser tests for every new and legacy surface form supported during migration.
- Lowering tests proving legacy and vNext forms map to the same canonical syntax where intended.
- Typechecker tests for:
  - effectful function types,
  - block expressions,
  - default methods,
  - `override`,
  - derive behavior,
  - shorthand desugaring.
- Codegen tests covering canonical top-level functions, impls, and block-expression forms.
- vNext canary suite plus legacy regression suite run in parallel until migration is complete.
- Tree-sitter corpus tests and grammar-sync checks in CI.
- LSP syntax-output tests for hover, signature help, document symbols, and code actions.

## Highest-Risk Areas
- Defining the canonical `Core_ast` narrowly enough to reduce churn, but clearly enough to stop syntax leakage.
- Brace ambiguity between block expressions and record/hash literals.
- Legacy trait-as-type compatibility versus vNext constrained-param shorthand.
- Override/default-method work requiring parser, lowering, registry, inference, and emitter changes together.
- Migration size of the fixture corpus and grammar/tooling drift.

## Immediate Next Step
Complete Phase 0 and Phase 0.5 before starting parser feature work:
- freeze spec/casing/examples,
- define `Surface_ast` and `Core_ast`,
- write the normalization contract in code,
- insert lowering between parsing and typechecking,
- only then start rewriting grammar families one by one.
