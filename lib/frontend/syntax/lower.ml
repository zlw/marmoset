open Ast
open Surface_ast

(* Lower — converts Surface_ast to Core_ast (Ast.AST).
   In Phase 0.5 this is a near-identity transform: every legacy Surface.*
   variant maps directly to its AST.* counterpart.  vNext-only Surface
   variants (SEArrowLambda, SEPlaceholder, SEBlockExpr, SFnDecl, etc.)
   raise an internal error if reached, because they are not yet parsed. *)

let failwith_unimplemented variant =
  failwith (Printf.sprintf "Lower: vNext-only surface form '%s' is not yet parsed in Phase 0.5" variant)

(* ── Type expressions ── *)

let rec lower_type_expr (st : Surface.surface_type_expr) : AST.type_expr =
  match st with
  | Surface.STVar s -> AST.TVar s
  | Surface.STCon s -> AST.TCon s
  | Surface.STApp (name, args) -> AST.TApp (name, List.map lower_type_expr args)
  | Surface.STArrow (params, ret, _effectful) ->
      (* In Phase 0.5 the effect bit is stored but TArrow doesn't carry it yet.
         Phase 1 / 1g will add the effect bit to AST.TArrow. *)
      AST.TArrow (List.map lower_type_expr params, lower_type_expr ret)
  | Surface.STUnion members -> AST.TUnion (List.map lower_type_expr members)
  | Surface.STRecord (fields, row) ->
      let lower_field f =
        AST.{ field_name = f.Surface.sf_name; field_type = lower_type_expr f.Surface.sf_type }
      in
      AST.TRecord (List.map lower_field fields, Option.map lower_type_expr row)

(* ── Patterns ── *)

let rec lower_pattern (sp : Surface.surface_pattern) : AST.pattern =
  let kind =
    match sp.sp_pat with
    | Surface.SPWildcard -> AST.PWildcard
    | Surface.SPVariable s -> AST.PVariable s
    | Surface.SPLiteral lv -> AST.PLiteral lv
    | Surface.SPConstructor (enum_name, variant_name, pats) ->
        AST.PConstructor (enum_name, variant_name, List.map lower_pattern pats)
    | Surface.SPRecord (fields, rest) ->
        let lower_field f =
          AST.
            {
              pat_field_name = f.Surface.sp_field_name;
              pat_field_pattern = Option.map lower_pattern f.Surface.sp_field_pattern;
            }
        in
        AST.PRecord (List.map lower_field fields, rest)
  in
  AST.{ pat = kind; pos = sp.sp_pos; end_pos = sp.sp_end_pos; file_id = sp.sp_file_id }

(* ── Expressions ── *)

let rec lower_expr (id_supply : Id_supply.Id_supply.t) (se : Surface.surface_expr) : AST.expression =
  let pos = se.se_pos
  and end_pos = se.se_end_pos
  and file_id = se.se_file_id
  and id = se.se_id in
  let expr =
    match se.se_expr with
    | Surface.SEIdentifier s -> AST.Identifier s
    | Surface.SEInteger i -> AST.Integer i
    | Surface.SEFloat f -> AST.Float f
    | Surface.SEBoolean b -> AST.Boolean b
    | Surface.SEString s -> AST.String s
    | Surface.SEArray elems -> AST.Array (List.map (lower_expr id_supply) elems)
    | Surface.SEIndex (container, idx) -> AST.Index (lower_expr id_supply container, lower_expr id_supply idx)
    | Surface.SEHash pairs ->
        AST.Hash (List.map (fun (k, v) -> (lower_expr id_supply k, lower_expr id_supply v)) pairs)
    | Surface.SEPrefix (op, e) -> AST.Prefix (op, lower_expr id_supply e)
    | Surface.SEInfix (l, op, r) -> AST.Infix (lower_expr id_supply l, op, lower_expr id_supply r)
    | Surface.SETypeCheck (e, t) -> AST.TypeCheck (lower_expr id_supply e, lower_type_expr t)
    | Surface.SEIf (cond, cons, alt) ->
        AST.If
          ( lower_expr id_supply cond,
            lower_stmt id_supply cons,
            Option.map (lower_stmt id_supply) alt )
    | Surface.SECall (f, args) -> AST.Call (lower_expr id_supply f, List.map (lower_expr id_supply) args)
    | Surface.SEEnumConstructor (enum_name, variant_name, args) ->
        AST.EnumConstructor (enum_name, variant_name, List.map (lower_expr id_supply) args)
    | Surface.SEMatch (scrutinee, arms) ->
        AST.Match (lower_expr id_supply scrutinee, List.map (lower_match_arm id_supply) arms)
    | Surface.SERecordLit (fields, spread) ->
        let lower_field f =
          AST.{ field_name = f.Surface.se_field_name; field_value = Option.map (lower_expr id_supply) f.Surface.se_field_value }
        in
        AST.RecordLit (List.map lower_field fields, Option.map (lower_expr id_supply) spread)
    | Surface.SEFieldAccess (receiver, field) -> AST.FieldAccess (lower_expr id_supply receiver, field)
    | Surface.SEMethodCall { se_receiver; se_method; se_type_args; se_args } ->
        AST.MethodCall
          {
            mc_receiver = lower_expr id_supply se_receiver;
            mc_method = se_method;
            mc_type_args = Option.map (List.map lower_type_expr) se_type_args;
            mc_args = List.map (lower_expr id_supply) se_args;
          }
    | Surface.SEFunction { se_generics; se_params; se_return_type; se_is_effectful; se_body } ->
        AST.Function
          {
            generics = se_generics;
            params = List.map (fun (n, t) -> (n, Option.map lower_type_expr t)) se_params;
            return_type = Option.map lower_type_expr se_return_type;
            is_effectful = se_is_effectful;
            body = lower_stmt id_supply se_body;
          }
    | Surface.SEArrowLambda _ -> failwith_unimplemented "SEArrowLambda"
    | Surface.SEPlaceholder -> failwith_unimplemented "SEPlaceholder"
    | Surface.SEBlockExpr _ -> failwith_unimplemented "SEBlockExpr"
  in
  AST.{ id; expr; pos; end_pos; file_id }

(* lower_stmt converts a block-level surface_stmt to AST.statement *)
and lower_stmt (id_supply : Id_supply.Id_supply.t) (ss : Surface.surface_stmt) : AST.statement =
  let pos = ss.ss_pos
  and end_pos = ss.ss_end_pos
  and file_id = ss.ss_file_id in
  let stmt =
    match ss.ss_stmt with
    | Surface.SSLet { ss_name; ss_value; ss_type_annotation } ->
        AST.Let
          {
            name = ss_name;
            value = lower_expr id_supply ss_value;
            type_annotation = Option.map lower_type_expr ss_type_annotation;
          }
    | Surface.SSReturn e -> AST.Return (lower_expr id_supply e)
    | Surface.SSExpressionStmt e -> AST.ExpressionStmt (lower_expr id_supply e)
    | Surface.SSBlock block -> AST.Block (List.map (lower_stmt id_supply) block.sb_stmts)
  in
  AST.{ stmt; pos; end_pos; file_id }

and lower_match_arm (id_supply : Id_supply.Id_supply.t) (arm : Surface.surface_match_arm) : AST.match_arm =
  let body = lower_expr_or_block_to_expr id_supply arm.se_arm_body in
  AST.{ patterns = List.map lower_pattern arm.se_patterns; body }

(* lower_expr_or_block_to_expr:
   Used for match-arm bodies and trait default method bodies.
   - SEOBExpr e  -> lower_expr e
   - SEOBBlock b -> fresh AST.BlockExpr (not yet in AST; use ExpressionStmt trick for now)
     In Phase 0.5 all match arm bodies are expressions; block arm bodies are Phase 1 / 1f. *)
and lower_expr_or_block_to_expr (id_supply : Id_supply.Id_supply.t) (eob : Surface.surface_expr_or_block) :
    AST.expression =
  match eob with
  | Surface.SEOBExpr e -> lower_expr id_supply e
  | Surface.SEOBBlock _b ->
      (* Phase 0.5: block arm bodies not yet parsed; this branch should not be reached *)
      failwith_unimplemented "SEOBBlock in expression context"

(* lower_expr_or_block_to_stmt:
   Used for function bodies, impl method bodies.
   - SEOBExpr e  -> AST.Block [AST.ExpressionStmt (lower_expr e)]
   - SEOBBlock b -> AST.Block (List.map lower_stmt b.sb_stmts) *)
and lower_expr_or_block_to_stmt (id_supply : Id_supply.Id_supply.t) (eob : Surface.surface_expr_or_block) :
    AST.statement =
  match eob with
  | Surface.SEOBExpr e ->
      let lowered = lower_expr id_supply e in
      AST.mk_stmt ~pos:lowered.pos ~end_pos:lowered.end_pos ~file_id:lowered.file_id
        (AST.Block [ AST.mk_stmt ~pos:lowered.pos ~end_pos:lowered.end_pos ~file_id:lowered.file_id (AST.ExpressionStmt lowered) ])
  | Surface.SEOBBlock b ->
      AST.mk_stmt ~pos:b.sb_pos ~end_pos:b.sb_end_pos ~file_id:b.sb_file_id
        (AST.Block (List.map (lower_stmt id_supply) b.sb_stmts))

(* ── Top-level declarations ── *)

let lower_method_sig (sm : Surface.surface_method_sig) : AST.method_sig =
  AST.
    {
      method_sig_id = sm.sm_id;
      method_name = sm.sm_name;
      method_generics = sm.sm_generics;
      method_params = List.map (fun (n, t) -> (n, lower_type_expr t)) sm.sm_params;
      method_return_type = lower_type_expr sm.sm_return_type;
      method_effect = sm.sm_effect;
      method_default_impl = None;
      (* Phase 0.5: default impls not yet parsed; always None *)
    }

let lower_method_impl (id_supply : Id_supply.Id_supply.t) (smi : Surface.surface_method_impl) : AST.method_impl =
  AST.
    {
      impl_method_id = smi.smi_id;
      impl_method_name = smi.smi_name;
      impl_method_generics = smi.smi_generics;
      impl_method_params = List.map (fun (n, t) -> (n, Option.map lower_type_expr t)) smi.smi_params;
      impl_method_return_type = Option.map lower_type_expr smi.smi_return_type;
      impl_method_effect = smi.smi_effect;
      impl_method_body = lower_expr_or_block_to_stmt id_supply smi.smi_body;
    }

let lower_variant (sv : Surface.surface_variant_def) : AST.variant_def =
  AST.{ variant_name = sv.sv_name; variant_fields = List.map lower_type_expr sv.sv_fields }

let lower_top_decl (id_supply : Id_supply.Id_supply.t) (ts : Surface.surface_top_stmt) : AST.statement list =
  let pos = ts.Surface.std_pos
  and end_pos = ts.Surface.std_end_pos
  and file_id = ts.Surface.std_file_id in
  match ts.Surface.std_decl with
  | Surface.SLet { name; value; type_annotation } ->
      [
        AST.mk_stmt ~pos ~end_pos ~file_id
          (AST.Let
             {
               name;
               value = lower_expr id_supply value;
               type_annotation = Option.map lower_type_expr type_annotation;
             });
      ]
  | Surface.SFnDecl { name; generics; params; return_type; is_effectful; body } ->
      let fn_body = lower_expr_or_block_to_stmt id_supply body in
      let fn_expr =
        AST.mk_expr
          (AST.Function
             {
               generics;
               params = List.map (fun (n, t) -> (n, Option.map lower_type_expr t)) params;
               return_type = Option.map lower_type_expr return_type;
               is_effectful;
               body = fn_body;
             })
      in
      [ AST.mk_stmt ~pos ~end_pos ~file_id (AST.Let { name; value = fn_expr; type_annotation = None }) ]
  | Surface.SEnumDef { name; type_params; variants; derive = _ } ->
      (* Postfix derive on enums is Phase 1/1e; ignored in Phase 0.5 *)
      [ AST.mk_stmt ~pos ~end_pos ~file_id (AST.EnumDef { name; type_params; variants = List.map lower_variant variants }) ]
  | Surface.STypeDef { alias_name; alias_type_params; alias_body; derive = _ } ->
      (* Postfix derive on types is Phase 1/1e; ignored in Phase 0.5 *)
      [ AST.mk_stmt ~pos ~end_pos ~file_id (AST.TypeAlias { alias_name; alias_type_params; alias_body = lower_type_expr alias_body }) ]
  | Surface.STraitDef { name; type_param; supertraits; fields; methods } ->
      let lower_field f = AST.{ field_name = f.Surface.sf_name; field_type = lower_type_expr f.Surface.sf_type } in
      let td =
        AST.
          {
            name;
            type_param;
            supertraits;
            fields = List.map lower_field fields;
            methods = List.map lower_method_sig methods;
          }
      in
      [ AST.mk_stmt ~pos ~end_pos ~file_id (AST.TraitDef td) ]
  | Surface.SImplDef { impl_type_params; impl_trait_name; impl_for_type; impl_methods } ->
      let idef =
        AST.
          {
            impl_type_params;
            impl_trait_name;
            impl_for_type = lower_type_expr impl_for_type;
            impl_methods = List.map (lower_method_impl id_supply) impl_methods;
          }
      in
      [ AST.mk_stmt ~pos ~end_pos ~file_id (AST.ImplDef idef) ]
  | Surface.SInherentImplDef { inherent_for_type; inherent_methods } ->
      let iid =
        AST.
          {
            inherent_for_type = lower_type_expr inherent_for_type;
            inherent_methods = List.map (lower_method_impl id_supply) inherent_methods;
          }
      in
      [ AST.mk_stmt ~pos ~end_pos ~file_id (AST.InherentImplDef iid) ]
  | Surface.SDeriveDef { derive_traits; derive_for_type } ->
      [ AST.mk_stmt ~pos ~end_pos ~file_id (AST.DeriveDef { derive_traits; derive_for_type = lower_type_expr derive_for_type }) ]
  | Surface.SExpressionStmt e ->
      [ AST.mk_stmt ~pos ~end_pos ~file_id (AST.ExpressionStmt (lower_expr id_supply e)) ]
  | Surface.SReturn e ->
      [ AST.mk_stmt ~pos ~end_pos ~file_id (AST.Return (lower_expr id_supply e)) ]
  | Surface.SBlock block ->
      [ AST.mk_stmt ~pos ~end_pos ~file_id
          (AST.Block (List.map (lower_stmt id_supply) block.sb_stmts)) ]

let lower_program (id_supply : Id_supply.Id_supply.t) (prog : Surface.surface_program) : AST.program =
  List.concat_map (lower_top_decl id_supply) prog

(* ── Tests ── *)

(* Helper: wrap a top_decl in a surface_top_stmt with default positions for tests *)
let mk_test_ts decl =
  Surface.{ std_decl = decl; std_pos = 0; std_end_pos = 0; std_file_id = None }

let%test "lower SLet integer" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let value = Surface.mk_surface_expr ~id:0 ~pos:4 (Surface.SEInteger 42L) in
  let decl = Surface.SLet { name = "x"; value; type_annotation = None } in
  let result = lower_top_decl id_supply (mk_test_ts decl) in
  match result with
  | [ { AST.stmt = AST.Let { name = "x"; value = { AST.expr = AST.Integer 42L; _ }; _ }; _ } ] -> true
  | _ -> false

let%test "lower SEnumDef" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let decl =
    Surface.SEnumDef
      {
        name = "Color";
        type_params = [];
        variants =
          [
            Surface.{ sv_name = "Red"; sv_fields = [] };
            Surface.{ sv_name = "Green"; sv_fields = [] };
            Surface.{ sv_name = "Blue"; sv_fields = [] };
          ];
        derive = [];
      }
  in
  let result = lower_top_decl id_supply (mk_test_ts decl) in
  match result with
  | [
      {
        AST.stmt =
          AST.EnumDef
            {
              name = "Color";
              type_params = [];
              variants = [ { AST.variant_name = "Red"; _ }; { AST.variant_name = "Green"; _ }; { AST.variant_name = "Blue"; _ } ];
            };
        _;
      };
    ] ->
      true
  | _ -> false

let%test "lower STypeDef" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let decl = Surface.STypeDef { alias_name = "Point"; alias_type_params = []; alias_body = Surface.STCon "Int"; derive = [] } in
  let result = lower_top_decl id_supply (mk_test_ts decl) in
  match result with
  | [ { AST.stmt = AST.TypeAlias { alias_name = "Point"; alias_type_params = []; alias_body = AST.TCon "Int" }; _ } ] ->
      true
  | _ -> false

let%test "lower SDeriveDef" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let decl =
    Surface.SDeriveDef
      {
        derive_traits = [ AST.{ derive_trait_name = "Eq"; derive_trait_constraints = [] } ];
        derive_for_type = Surface.STCon "Point";
      }
  in
  let result = lower_top_decl id_supply (mk_test_ts decl) in
  match result with
  | [
      {
        AST.stmt =
          AST.DeriveDef
            {
              derive_traits = [ { AST.derive_trait_name = "Eq"; _ } ];
              derive_for_type = AST.TCon "Point";
            };
        _;
      };
    ] ->
      true
  | _ -> false

let%test "lower_expr_or_block_to_stmt wraps SEOBExpr in Block[ExpressionStmt]" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let e = Surface.mk_surface_expr ~id:1 ~pos:0 (Surface.SEInteger 5L) in
  let result = lower_expr_or_block_to_stmt id_supply (Surface.SEOBExpr e) in
  match result.stmt with
  | AST.Block [ { AST.stmt = AST.ExpressionStmt { AST.expr = AST.Integer 5L; _ }; _ } ] -> true
  | _ -> false

let%test "lower_expr_or_block_to_stmt preserves SEOBBlock" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let e = Surface.mk_surface_expr ~id:1 ~pos:0 (Surface.SEInteger 7L) in
  let block =
    Surface.
      {
        sb_stmts =
          [ Surface.mk_surface_stmt ~pos:0 (Surface.SSExpressionStmt e) ];
        sb_pos = 0;
        sb_end_pos = 5;
        sb_file_id = None;
      }
  in
  let result = lower_expr_or_block_to_stmt id_supply (Surface.SEOBBlock block) in
  match result.stmt with
  | AST.Block [ { AST.stmt = AST.ExpressionStmt { AST.expr = AST.Integer 7L; _ }; _ } ] -> true
  | _ -> false

let%test "lower match arm body (legacy expression arm)" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let e = Surface.mk_surface_expr ~id:1 ~pos:0 (Surface.SEString "yes") in
  let arm =
    Surface.
      {
        se_patterns =
          [ Surface.mk_surface_pat ~pos:0 (Surface.SPLiteral (AST.LBool true)) ];
        se_arm_body = SEOBExpr e;
      }
  in
  let result = lower_match_arm id_supply arm in
  match result.body.expr with
  | AST.String "yes" -> true
  | _ -> false
