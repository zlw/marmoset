open Ast

(* Surface AST — the parser output before lowering/canonicalization.
   All surface-only and vNext-only forms live here.  Downstream
   compiler passes (resolver, typechecker, emitter) never see this
   type family; they consume only canonical Core_ast (Ast.AST). *)

module Surface = struct
  type name_ref = {
    text : string;
    pos : int;
    end_pos : int;
    file_id : string option;
  }

  (* ── Surface type expressions ── *)
  type surface_type_expr = {
    ste_desc : surface_type_expr_desc;
    ste_pos : int;
    ste_end_pos : int;
    ste_file_id : string option;
  }

  and surface_type_expr_desc =
    | STVar of name_ref
    | STCon of name_ref
    | STApp of name_ref * surface_type_expr list
    | STConstraintShorthand of name_ref list (* Bare trait names in parameter position, e.g. Named & Aged *)
    | STTraitObject of name_ref list (* Dyn[Show] or Dyn[Show & Eq] *)
    | STArrow of surface_type_expr list * surface_type_expr * bool
      (* bool = is_effectful; covers both (a) -> b and (a) => b *)
    | STUnion of surface_type_expr list
    | STIntersection of surface_type_expr list
    | STRecord of surface_record_type_field list * surface_type_expr option

  and surface_record_type_field = {
    sf_name : string;
    sf_name_ref : name_ref;
    sf_type : surface_type_expr;
  }

  and surface_variant_def = {
    sv_name : string;
    sv_name_ref : name_ref;
    sv_fields : surface_type_expr list;
  }

  and surface_type_def_kind =
    | STTransparent of surface_type_expr
    | STNamedProduct of surface_record_type_field list
    | STNamedWrapper of surface_type_expr
    | STNamedSum of surface_variant_def list

  and surface_generic_param = {
    sg_name : string;
    sg_name_ref : name_ref;
    sg_constraints : string list;
    sg_constraint_refs : name_ref list;
  }

  and surface_derive_trait = {
    sdt_name : string;
    sdt_name_ref : name_ref;
    sdt_constraints : surface_generic_param list;
  }

  and surface_value_param = {
    svp_name : string;
    svp_name_ref : name_ref;
    svp_type : surface_type_expr option;
  }

  and surface_typed_param = {
    stp_name : string;
    stp_name_ref : name_ref;
    stp_type : surface_type_expr;
  }

  (* ── Surface patterns ── *)
  and surface_pattern_kind =
    | SPWildcard
    | SPVariable of name_ref
    | SPLiteral of AST.literal_value (* shared leaf *)
    | SPConstructor of {
        sp_enum_name : string;
        sp_enum_name_ref : name_ref;
        sp_constructor_name : string;
        sp_constructor_name_ref : name_ref;
        sp_args : surface_pattern list;
      }
    | SPRecord of surface_record_pat_field list * string option

  and surface_pattern = {
    sp_pat : surface_pattern_kind;
    sp_pos : int;
    sp_end_pos : int;
    sp_file_id : string option;
  }

  and surface_record_pat_field = {
    sp_field_name : string;
    sp_field_name_ref : name_ref;
    sp_field_pattern : surface_pattern option;
  }

  (* ── Surface expressions ── *)
  type surface_expr_kind =
    (* — Carried over from Core_ast, structurally identical — *)
    | SEIdentifier of name_ref
    | SEInteger of int64
    | SEFloat of float
    | SEBoolean of bool
    | SEString of string
    | SEArray of surface_expr list
    | SEIndex of surface_expr * surface_expr
    | SETypeApply of surface_expr * surface_type_expr list
    | SEHash of (surface_expr * surface_expr) list
    | SEPrefix of string * surface_expr
    | SEInfix of surface_expr * string * surface_expr
    | SETypeCheck of surface_expr * surface_type_expr
    | SEIf of surface_expr * surface_stmt * surface_stmt option
    | SECall of surface_expr * surface_expr list
    | SEEnumConstructor of name_ref * name_ref * surface_expr list
    | SEMatch of surface_expr * surface_match_arm list
    | SERecordLit of surface_record_field list * surface_expr option
    | SEFieldAccess of surface_expr * name_ref
    | SEMethodCall of {
        se_receiver : surface_expr;
        se_method : string;
        se_method_ref : name_ref;
        se_type_args : surface_type_expr list option;
        se_args : surface_expr list;
      }
    (* — vNext surface-only forms — *)
    | SEArrowLambda of {
        se_lambda_params : surface_value_param list;
        se_lambda_is_effectful : bool;
        se_lambda_body : surface_expr_or_block;
      }
      (* (x) -> expr  or  (x, y) => expr *)
    | SEPlaceholder (* _ in expression position; rewritten to SEArrowLambda or rejected in lowering *)
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
    se_field_name_ref : name_ref;
    se_field_value : surface_expr option; (* None = punning *)
  }

  and surface_match_arm = {
    se_patterns : surface_pattern list;
    se_arm_body : surface_expr_or_block; (* vNext arms use case and allow block or expression bodies *)
  }

  (* ── Surface statements (block-level only) ── *)
  and surface_stmt_kind =
    | SSLet of {
        ss_name : string;
        ss_name_ref : name_ref;
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
  and surface_method_sig = {
    sm_id : int;
    sm_name : string;
    sm_name_ref : name_ref;
    sm_generics : surface_generic_param list option;
    sm_params : surface_typed_param list;
    sm_return_type : surface_type_expr;
    sm_effect : AST.effect_annotation; (* shared leaf *)
    sm_default_impl : surface_expr_or_block option;
  }

  and surface_method_impl = {
    smi_id : int;
    smi_name : string;
    smi_name_ref : name_ref;
    smi_generics : surface_generic_param list option;
    smi_params : surface_value_param list;
    smi_return_type : surface_type_expr option;
    smi_effect : AST.effect_annotation option;
    smi_override : bool;
    smi_body : surface_expr_or_block;
  }

  and top_decl =
    | SExportDecl of string list
    | SImportDecl of {
        import_path : string list;
        import_path_refs : name_ref list;
        import_alias : string option;
        import_alias_ref : name_ref option;
      }
    | SLet of {
        name : string;
        name_ref : name_ref;
        value : surface_expr;
        type_annotation : surface_type_expr option;
      }
    | SFnDecl of {
        name : string;
        name_ref : name_ref;
        generics : surface_generic_param list option;
        params : surface_value_param list;
        return_type : surface_type_expr option;
        is_effectful : bool;
        body : surface_expr_or_block;
      }
    | STypeDef of {
        type_name : string;
        type_name_ref : name_ref;
        type_type_params : string list;
        type_type_param_refs : name_ref list;
        type_body : surface_type_def_kind;
        derive : surface_derive_trait list;
      }
    | SShapeDef of {
        shape_name : string;
        shape_name_ref : name_ref;
        shape_type_params : string list;
        shape_type_param_refs : name_ref list;
        shape_fields : surface_record_type_field list;
      }
    | STraitDef of {
        name : string;
        name_ref : name_ref;
        type_param : string option;
        type_param_ref : name_ref option;
        supertraits : string list;
        supertrait_refs : name_ref list;
        methods : surface_method_sig list;
      }
    | SAmbiguousImplDef of {
        impl_type_params : surface_generic_param list;
        impl_head_type : surface_type_expr;
        impl_methods : surface_method_impl list;
      }
    | SInherentImplDef of {
        inherent_for_type : surface_type_expr;
        inherent_methods : surface_method_impl list;
      }
    | SExpressionStmt of surface_expr
    | SReturn of surface_expr
    | SBlock of surface_block

  (* Positioned wrapper for top-level declarations.
     Carries the source span so that lower_program can reconstruct AST.statement positions. *)
  type surface_top_stmt = {
    std_decl : top_decl;
    std_pos : int;
    std_end_pos : int;
    std_file_id : string option;
  }

  type surface_program = surface_top_stmt list

  (* Smart constructor helpers *)
  let mk_name_ref ?(pos = 0) ?end_pos ?(file_id = None) text =
    let end_pos = Option.value end_pos ~default:pos in
    { text; pos; end_pos; file_id }

  let mk_surface_type ?(pos = 0) ?end_pos ?(file_id = None) ste_desc =
    let end_pos = Option.value end_pos ~default:pos in
    { ste_desc; ste_pos = pos; ste_end_pos = end_pos; ste_file_id = file_id }

  let mk_surface_expr ?(id = 0) ?(pos = 0) ?end_pos ?(file_id = None) expr =
    let end_pos = Option.value end_pos ~default:pos in
    { se_id = id; se_expr = expr; se_pos = pos; se_end_pos = end_pos; se_file_id = file_id }

  let mk_surface_stmt ?(pos = 0) ?end_pos ?(file_id = None) stmt =
    let end_pos = Option.value end_pos ~default:pos in
    { ss_stmt = stmt; ss_pos = pos; ss_end_pos = end_pos; ss_file_id = file_id }

  let mk_surface_pat ?(pos = 0) ?end_pos ?(file_id = None) pat =
    let end_pos = Option.value end_pos ~default:pos in
    { sp_pat = pat; sp_pos = pos; sp_end_pos = end_pos; sp_file_id = file_id }
end
