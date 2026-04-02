open Ast
open Surface_ast
module StringSet = Set.Make (String)

(* Lower — converts Surface_ast to the canonical Ast.AST tree consumed
   downstream. Surface-only forms such as arrow lambdas, placeholders,
   block expressions, and vNext declarations must disappear here. *)

let failwith_unimplemented variant =
  failwith (Printf.sprintf "Lower: unexpected surface-only form '%s' after parsing" variant)

type lower_context = {
  constraint_names : StringSet.t;
  type_names : StringSet.t;
}

let builtin_trait_names = [ "Eq"; "Show"; "Debug"; "Ord"; "Hash"; "Num"; "Rem"; "Neg" ]

let builtin_type_names =
  [ "Int"; "Float"; "Bool"; "Str"; "String"; "Unit"; "Null"; "List"; "Map"; "Option"; "Result"; "Ordering" ]

let empty_lower_context =
  {
    constraint_names = List.fold_left (fun acc name -> StringSet.add name acc) StringSet.empty builtin_trait_names;
    type_names = List.fold_left (fun acc name -> StringSet.add name acc) StringSet.empty builtin_type_names;
  }

let lower_context_of_program (prog : Surface.surface_program) : lower_context =
  List.fold_left
    (fun ({ constraint_names; type_names } as acc) (ts : Surface.surface_top_stmt) ->
      match ts.std_decl with
      | Surface.STraitDef { name; _ } | Surface.SShapeDef { shape_name = name; _ } ->
          { acc with constraint_names = StringSet.add name constraint_names }
      | Surface.STypeDef { type_name = name; _ } -> { acc with type_names = StringSet.add name type_names }
      | _ -> acc)
    empty_lower_context prog

let trait_shorthand_constraint (ctx : lower_context) (st : Surface.surface_type_expr) : string list option =
  match st with
  | Surface.STConstraintShorthand traits -> Some traits
  | Surface.STCon name when StringSet.mem name ctx.constraint_names && not (StringSet.mem name ctx.type_names) ->
      Some [ name ]
  | _ -> None

let fresh_shorthand_generic_name (used : StringSet.t) : string =
  let rec loop n =
    let candidate = Printf.sprintf "g%d" n in
    if StringSet.mem candidate used then
      loop (n + 1)
    else
      candidate
  in
  loop 0

let bound_type_vars_of_generics (generics : AST.generic_param list option) : StringSet.t =
  match generics with
  | None -> StringSet.empty
  | Some gs -> List.fold_left (fun acc (g : AST.generic_param) -> StringSet.add g.name acc) StringSet.empty gs

let bound_type_vars_of_names (names : string list) : StringSet.t =
  List.fold_left (fun acc name -> StringSet.add name acc) StringSet.empty names

let is_lower_ident (name : string) : bool =
  String.length name > 0
  &&
  match String.get name 0 with
  | 'a' .. 'z' -> true
  | _ -> false

let known_type_name (ctx : lower_context) (name : string) : bool =
  StringSet.mem name ctx.type_names || StringSet.mem name ctx.constraint_names

let infer_impl_type_params (ctx : lower_context) (impl_for_type : Surface.surface_type_expr) :
    AST.generic_param list =
  let rec collect seen rev_names te =
    match te with
    | Surface.STVar name ->
        if StringSet.mem name seen || known_type_name ctx name then
          (seen, rev_names)
        else
          (StringSet.add name seen, name :: rev_names)
    | Surface.STCon name ->
        if (not (is_lower_ident name)) || StringSet.mem name seen || known_type_name ctx name then
          (seen, rev_names)
        else
          (StringSet.add name seen, name :: rev_names)
    | Surface.STConstraintShorthand _ -> (seen, rev_names)
    | Surface.STTraitObject _ -> (seen, rev_names)
    | Surface.STApp (_name, args) ->
        List.fold_left (fun (seen_acc, names_acc) arg -> collect seen_acc names_acc arg) (seen, rev_names) args
    | Surface.STArrow (params, ret, _) ->
        let seen, rev_names =
          List.fold_left
            (fun (seen_acc, names_acc) param -> collect seen_acc names_acc param)
            (seen, rev_names) params
        in
        collect seen rev_names ret
    | Surface.STUnion members ->
        List.fold_left
          (fun (seen_acc, names_acc) member -> collect seen_acc names_acc member)
          (seen, rev_names) members
    | Surface.STIntersection members ->
        List.fold_left
          (fun (seen_acc, names_acc) member -> collect seen_acc names_acc member)
          (seen, rev_names) members
    | Surface.STRecord (fields, row) -> (
        let seen, rev_names =
          List.fold_left
            (fun (seen_acc, names_acc) (field : Surface.surface_record_type_field) ->
              collect seen_acc names_acc field.sf_type)
            (seen, rev_names) fields
        in
        match row with
        | None -> (seen, rev_names)
        | Some row_var -> collect seen rev_names row_var)
  in
  let _, rev_names = collect StringSet.empty [] impl_for_type in
  List.rev_map (fun name -> AST.{ name; constraints = [] }) rev_names

(* ── Type expressions ── *)

let rec lower_type_expr_with_bound_vars (bound_type_vars : StringSet.t) (st : Surface.surface_type_expr) :
    AST.type_expr =
  match st with
  | Surface.STVar s -> AST.TVar s
  | Surface.STCon s when StringSet.mem s bound_type_vars -> AST.TVar s
  | Surface.STCon s -> AST.TCon s
  | Surface.STConstraintShorthand _ -> failwith "Lower: constrained-param shorthand escaped parameter lowering"
  | Surface.STTraitObject traits -> AST.TTraitObject traits
  | Surface.STApp (name, args) -> AST.TApp (name, List.map (lower_type_expr_with_bound_vars bound_type_vars) args)
  | Surface.STArrow (params, ret, effectful) ->
      AST.TArrow
        ( List.map (lower_type_expr_with_bound_vars bound_type_vars) params,
          lower_type_expr_with_bound_vars bound_type_vars ret,
          effectful )
  | Surface.STUnion members -> AST.TUnion (List.map (lower_type_expr_with_bound_vars bound_type_vars) members)
  | Surface.STIntersection members ->
      AST.TIntersection (List.map (lower_type_expr_with_bound_vars bound_type_vars) members)
  | Surface.STRecord (fields, row) ->
      let lower_field f =
        AST.
          {
            field_name = f.Surface.sf_name;
            field_type = lower_type_expr_with_bound_vars bound_type_vars f.Surface.sf_type;
          }
      in
      AST.TRecord (List.map lower_field fields, Option.map (lower_type_expr_with_bound_vars bound_type_vars) row)

let lower_type_expr (st : Surface.surface_type_expr) : AST.type_expr =
  lower_type_expr_with_bound_vars StringSet.empty st

let lower_callable_signature
    (ctx : lower_context)
    ?(extra_bound_names = StringSet.empty)
    (generics : AST.generic_param list option)
    (params : (string * Surface.surface_type_expr option) list) :
    AST.generic_param list option * (string * AST.type_expr option) list =
  let used_generic_names =
    match generics with
    | None -> extra_bound_names
    | Some gs -> List.fold_left (fun acc (g : AST.generic_param) -> StringSet.add g.name acc) extra_bound_names gs
  in
  let rec lower_params used rev_generics rev_params = function
    | [] ->
        let shorthand_generics = List.rev rev_generics in
        let lowered_params = List.rev rev_params in
        let all_generics =
          match (generics, shorthand_generics) with
          | None, [] -> None
          | None, _ -> Some shorthand_generics
          | Some existing, [] -> Some existing
          | Some existing, _ -> Some (existing @ shorthand_generics)
        in
        (all_generics, lowered_params)
    | (name, None) :: rest -> lower_params used rev_generics ((name, None) :: rev_params) rest
    | (name, Some annot) :: rest -> (
        match trait_shorthand_constraint ctx annot with
        | Some constraints ->
            let generic_name = fresh_shorthand_generic_name used in
            let generic = AST.{ name = generic_name; constraints } in
            lower_params (StringSet.add generic_name used) (generic :: rev_generics)
              ((name, Some (AST.TVar generic_name)) :: rev_params)
              rest
        | None ->
            let annot = lower_type_expr_with_bound_vars used annot in
            lower_params used rev_generics ((name, Some annot) :: rev_params) rest)
  in
  lower_params used_generic_names [] [] params

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

let rec placeholder_count_expr (expr : AST.expression) : int =
  match expr.expr with
  | AST.Identifier "_" -> 1
  | AST.Identifier _ | AST.Integer _ | AST.Float _ | AST.Boolean _ | AST.String _ -> 0
  | AST.Prefix (_, inner) | AST.TypeCheck (inner, _) | AST.FieldAccess (inner, _) -> placeholder_count_expr inner
  | AST.Infix (left, _, right) -> placeholder_count_expr left + placeholder_count_expr right
  | AST.TypeApply (callee, _) -> placeholder_count_expr callee
  | AST.If (cond, cons, alt) -> (
      placeholder_count_expr cond
      + placeholder_count_stmt cons
      +
      match alt with
      | None -> 0
      | Some stmt -> placeholder_count_stmt stmt)
  | AST.Function _ -> 0
  | AST.Call (fn_expr, args) ->
      placeholder_count_expr fn_expr + List.fold_left (fun acc arg -> acc + placeholder_count_expr arg) 0 args
  | AST.Array elements -> List.fold_left (fun acc arg -> acc + placeholder_count_expr arg) 0 elements
  | AST.Hash pairs ->
      List.fold_left
        (fun acc (key, value) -> acc + placeholder_count_expr key + placeholder_count_expr value)
        0 pairs
  | AST.Index (container, index) -> placeholder_count_expr container + placeholder_count_expr index
  | AST.EnumConstructor (_, _, args) -> List.fold_left (fun acc arg -> acc + placeholder_count_expr arg) 0 args
  | AST.Match (scrutinee, arms) ->
      placeholder_count_expr scrutinee
      + List.fold_left (fun acc arm -> acc + placeholder_count_expr arm.AST.body) 0 arms
  | AST.RecordLit (fields, spread) -> (
      List.fold_left
        (fun acc (field : AST.record_field) ->
          acc
          +
          match field.field_value with
          | None -> 0
          | Some value -> placeholder_count_expr value)
        0 fields
      +
      match spread with
      | None -> 0
      | Some spread_expr -> placeholder_count_expr spread_expr)
  | AST.MethodCall { mc_receiver; mc_args; _ } ->
      placeholder_count_expr mc_receiver
      + List.fold_left (fun acc arg -> acc + placeholder_count_expr arg) 0 mc_args
  | AST.BlockExpr stmts -> placeholder_count_stmt_list stmts

and placeholder_count_stmt (stmt : AST.statement) : int =
  match stmt.stmt with
  | AST.Let { value; _ } -> placeholder_count_expr value
  | AST.Return expr | AST.ExpressionStmt expr -> placeholder_count_expr expr
  | AST.Block stmts -> placeholder_count_stmt_list stmts
  | AST.EnumDef _ | AST.TypeDef _ | AST.ShapeDef _ | AST.TraitDef _ | AST.ImplDef _ | AST.InherentImplDef _
  | AST.DeriveDef _ | AST.TypeAlias _ ->
      0

and placeholder_count_stmt_list (stmts : AST.statement list) : int =
  List.fold_left (fun acc stmt -> acc + placeholder_count_stmt stmt) 0 stmts

let is_placeholder_identifier (expr : AST.expression) : bool =
  match expr.expr with
  | AST.Identifier "_" -> true
  | _ -> false

let rec replace_placeholder_with_expr (replacement : AST.expression) (expr : AST.expression) : AST.expression =
  match expr.expr with
  | AST.Identifier "_" -> replacement
  | AST.Identifier _ | AST.Integer _ | AST.Float _ | AST.Boolean _ | AST.String _ -> expr
  | AST.Prefix (op, inner) ->
      { expr with expr = AST.Prefix (op, replace_placeholder_with_expr replacement inner) }
  | AST.Infix (left, op, right) ->
      {
        expr with
        expr =
          AST.Infix
            (replace_placeholder_with_expr replacement left, op, replace_placeholder_with_expr replacement right);
      }
  | AST.TypeCheck (inner, typ) ->
      { expr with expr = AST.TypeCheck (replace_placeholder_with_expr replacement inner, typ) }
  | AST.TypeApply (callee, type_args) ->
      { expr with expr = AST.TypeApply (replace_placeholder_with_expr replacement callee, type_args) }
  | AST.If (cond, cons, alt) ->
      {
        expr with
        expr =
          AST.If
            ( replace_placeholder_with_expr replacement cond,
              replace_placeholder_with_stmt replacement cons,
              Option.map (replace_placeholder_with_stmt replacement) alt );
      }
  | AST.Function _ -> expr
  | AST.Call (fn_expr, args) ->
      {
        expr with
        expr =
          AST.Call
            ( replace_placeholder_with_expr replacement fn_expr,
              List.map (replace_placeholder_with_expr replacement) args );
      }
  | AST.Array elements ->
      { expr with expr = AST.Array (List.map (replace_placeholder_with_expr replacement) elements) }
  | AST.Hash pairs ->
      {
        expr with
        expr =
          AST.Hash
            (List.map
               (fun (key, value) ->
                 (replace_placeholder_with_expr replacement key, replace_placeholder_with_expr replacement value))
               pairs);
      }
  | AST.Index (container, index) ->
      {
        expr with
        expr =
          AST.Index
            (replace_placeholder_with_expr replacement container, replace_placeholder_with_expr replacement index);
      }
  | AST.EnumConstructor (enum_name, variant_name, args) ->
      {
        expr with
        expr =
          AST.EnumConstructor (enum_name, variant_name, List.map (replace_placeholder_with_expr replacement) args);
      }
  | AST.Match (scrutinee, arms) ->
      {
        expr with
        expr =
          AST.Match
            ( replace_placeholder_with_expr replacement scrutinee,
              List.map
                (fun (arm : AST.match_arm) ->
                  { arm with body = replace_placeholder_with_expr replacement arm.body })
                arms );
      }
  | AST.RecordLit (fields, spread) ->
      {
        expr with
        expr =
          AST.RecordLit
            ( List.map
                (fun (field : AST.record_field) ->
                  {
                    field with
                    field_value = Option.map (replace_placeholder_with_expr replacement) field.field_value;
                  })
                fields,
              Option.map (replace_placeholder_with_expr replacement) spread );
      }
  | AST.FieldAccess (receiver, field_name) ->
      { expr with expr = AST.FieldAccess (replace_placeholder_with_expr replacement receiver, field_name) }
  | AST.MethodCall { mc_receiver; mc_method; mc_type_args; mc_args } ->
      {
        expr with
        expr =
          AST.MethodCall
            {
              mc_receiver = replace_placeholder_with_expr replacement mc_receiver;
              mc_method;
              mc_type_args;
              mc_args = List.map (replace_placeholder_with_expr replacement) mc_args;
            };
      }
  | AST.BlockExpr stmts ->
      { expr with expr = AST.BlockExpr (List.map (replace_placeholder_with_stmt replacement) stmts) }

and replace_placeholder_with_stmt (replacement : AST.expression) (stmt : AST.statement) : AST.statement =
  let rewritten =
    match stmt.stmt with
    | AST.Let ({ value; _ } as let_binding) ->
        AST.Let { let_binding with value = replace_placeholder_with_expr replacement value }
    | AST.Return expr -> AST.Return (replace_placeholder_with_expr replacement expr)
    | AST.ExpressionStmt expr -> AST.ExpressionStmt (replace_placeholder_with_expr replacement expr)
    | AST.Block stmts -> AST.Block (List.map (replace_placeholder_with_stmt replacement) stmts)
    | AST.EnumDef _ | AST.TypeDef _ | AST.ShapeDef _ | AST.TraitDef _ | AST.ImplDef _ | AST.InherentImplDef _
    | AST.DeriveDef _ | AST.TypeAlias _ ->
        stmt.stmt
  in
  { stmt with stmt = rewritten }

let lower_pipe_expr (lhs : AST.expression) (rhs : AST.expression) : AST.expr_kind =
  let placeholder_count = placeholder_count_expr rhs in
  match rhs.expr with
  | AST.MethodCall { mc_receiver; mc_args; _ }
    when placeholder_count = 1
         && (is_placeholder_identifier mc_receiver || List.exists is_placeholder_identifier mc_args) ->
      (replace_placeholder_with_expr lhs rhs).expr
  | AST.MethodCall { mc_receiver; mc_method; mc_type_args; mc_args } ->
      AST.MethodCall { mc_receiver; mc_method; mc_type_args; mc_args = lhs :: mc_args }
  | AST.Call (callee, args)
    when placeholder_count = 1 && (is_placeholder_identifier callee || List.exists is_placeholder_identifier args)
    ->
      (replace_placeholder_with_expr lhs rhs).expr
  | AST.Call (callee, args) -> AST.Call (callee, lhs :: args)
  | _ when placeholder_count = 1 -> (replace_placeholder_with_expr lhs rhs).expr
  | _ -> AST.Call (rhs, [ lhs ])

let rec lower_expr_with_ctx (ctx : lower_context) (id_supply : Id_supply.Id_supply.t) (se : Surface.surface_expr)
    : AST.expression =
  let pos = se.se_pos and end_pos = se.se_end_pos and file_id = se.se_file_id and id = se.se_id in
  let expr =
    match se.se_expr with
    | Surface.SEIdentifier s -> AST.Identifier s
    | Surface.SEInteger i -> AST.Integer i
    | Surface.SEFloat f -> AST.Float f
    | Surface.SEBoolean b -> AST.Boolean b
    | Surface.SEString s -> AST.String s
    | Surface.SEArray elems -> AST.Array (List.map (lower_expr_with_ctx ctx id_supply) elems)
    | Surface.SEIndex (container, idx) ->
        AST.Index (lower_expr_with_ctx ctx id_supply container, lower_expr_with_ctx ctx id_supply idx)
    | Surface.SETypeApply (callee, type_args) ->
        AST.TypeApply (lower_expr_with_ctx ctx id_supply callee, List.map lower_type_expr type_args)
    | Surface.SEHash pairs ->
        AST.Hash
          (List.map
             (fun (k, v) -> (lower_expr_with_ctx ctx id_supply k, lower_expr_with_ctx ctx id_supply v))
             pairs)
    | Surface.SEPrefix (op, e) -> AST.Prefix (op, lower_expr_with_ctx ctx id_supply e)
    | Surface.SEInfix (l, "|>", r) ->
        let lhs = lower_expr_with_ctx ctx id_supply l in
        let rhs = lower_expr_with_ctx ctx id_supply r in
        lower_pipe_expr lhs rhs
    | Surface.SEInfix (l, op, r) ->
        AST.Infix (lower_expr_with_ctx ctx id_supply l, op, lower_expr_with_ctx ctx id_supply r)
    | Surface.SETypeCheck (e, t) -> AST.TypeCheck (lower_expr_with_ctx ctx id_supply e, lower_type_expr t)
    | Surface.SEIf (cond, cons, alt) ->
        AST.If
          ( lower_expr_with_ctx ctx id_supply cond,
            lower_stmt_with_ctx ctx id_supply cons,
            Option.map (lower_stmt_with_ctx ctx id_supply) alt )
    | Surface.SECall (f, args) ->
        AST.Call (lower_expr_with_ctx ctx id_supply f, List.map (lower_expr_with_ctx ctx id_supply) args)
    | Surface.SEEnumConstructor (enum_name, variant_name, args) ->
        AST.EnumConstructor (enum_name, variant_name, List.map (lower_expr_with_ctx ctx id_supply) args)
    | Surface.SEMatch (scrutinee, arms) ->
        AST.Match
          (lower_expr_with_ctx ctx id_supply scrutinee, List.map (lower_match_arm_with_ctx ctx id_supply) arms)
    | Surface.SERecordLit (fields, spread) ->
        let lower_field f =
          AST.
            {
              field_name = f.Surface.se_field_name;
              field_value = Option.map (lower_expr_with_ctx ctx id_supply) f.Surface.se_field_value;
            }
        in
        AST.RecordLit (List.map lower_field fields, Option.map (lower_expr_with_ctx ctx id_supply) spread)
    | Surface.SEFieldAccess (receiver, field) ->
        AST.FieldAccess (lower_expr_with_ctx ctx id_supply receiver, field)
    | Surface.SEMethodCall { se_receiver; se_method; se_type_args; se_args } ->
        AST.MethodCall
          {
            mc_receiver = lower_expr_with_ctx ctx id_supply se_receiver;
            mc_method = se_method;
            mc_type_args = Option.map (List.map lower_type_expr) se_type_args;
            mc_args = List.map (lower_expr_with_ctx ctx id_supply) se_args;
          }
    | Surface.SEArrowLambda { se_lambda_params; se_lambda_is_effectful; se_lambda_body } ->
        (* Lower arrow lambda to canonical Function form *)
        let generics, params = lower_callable_signature ctx None se_lambda_params in
        let fn_body = lower_expr_or_block_to_stmt_with_ctx ctx id_supply se_lambda_body in
        AST.Function
          {
            origin = AST.ExplicitLambda;
            generics;
            params;
            return_type = None;
            is_effectful = se_lambda_is_effectful;
            body = fn_body;
          }
    | Surface.SEPlaceholder -> failwith_unimplemented "SEPlaceholder"
    | Surface.SEBlockExpr block -> AST.BlockExpr (List.map (lower_stmt_with_ctx ctx id_supply) block.sb_stmts)
  in
  AST.{ id; expr; pos; end_pos; file_id }

(* lower_stmt converts a block-level surface_stmt to AST.statement *)
and lower_stmt_with_ctx (ctx : lower_context) (id_supply : Id_supply.Id_supply.t) (ss : Surface.surface_stmt) :
    AST.statement =
  let pos = ss.ss_pos and end_pos = ss.ss_end_pos and file_id = ss.ss_file_id in
  let stmt =
    match ss.ss_stmt with
    | Surface.SSLet { ss_name; ss_value; ss_type_annotation } ->
        AST.Let
          {
            name = ss_name;
            value = lower_expr_with_ctx ctx id_supply ss_value;
            type_annotation = Option.map lower_type_expr ss_type_annotation;
          }
    | Surface.SSReturn e -> AST.Return (lower_expr_with_ctx ctx id_supply e)
    | Surface.SSExpressionStmt e -> AST.ExpressionStmt (lower_expr_with_ctx ctx id_supply e)
    | Surface.SSBlock block -> AST.Block (List.map (lower_stmt_with_ctx ctx id_supply) block.sb_stmts)
  in
  AST.{ stmt; pos; end_pos; file_id }

and lower_match_arm_with_ctx
    (ctx : lower_context) (id_supply : Id_supply.Id_supply.t) (arm : Surface.surface_match_arm) : AST.match_arm =
  let body = lower_expr_or_block_to_expr_with_ctx ctx id_supply arm.se_arm_body in
  AST.{ patterns = List.map lower_pattern arm.se_patterns; body }

(* lower_expr_or_block_to_expr:
   Used for match-arm bodies and trait default method bodies.
   - SEOBExpr e  -> lower_expr e
   - SEOBBlock b -> fresh AST.BlockExpr *)
and lower_expr_or_block_to_expr_with_ctx
    (ctx : lower_context) (id_supply : Id_supply.Id_supply.t) (eob : Surface.surface_expr_or_block) :
    AST.expression =
  match eob with
  | Surface.SEOBExpr e -> lower_expr_with_ctx ctx id_supply e
  | Surface.SEOBBlock block ->
      AST.mk_expr ~id:(Id_supply.Id_supply.fresh id_supply) ~pos:block.sb_pos ~end_pos:block.sb_end_pos
        ~file_id:block.sb_file_id
        (AST.BlockExpr (List.map (lower_stmt_with_ctx ctx id_supply) block.sb_stmts))

(* lower_expr_or_block_to_stmt:
   Used for function bodies, impl method bodies.
   - SEOBExpr e  -> AST.Block [AST.ExpressionStmt (lower_expr e)]
   - SEOBBlock b -> AST.Block (List.map lower_stmt b.sb_stmts) *)
and lower_expr_or_block_to_stmt_with_ctx
    (ctx : lower_context) (id_supply : Id_supply.Id_supply.t) (eob : Surface.surface_expr_or_block) :
    AST.statement =
  match eob with
  | Surface.SEOBExpr e ->
      let lowered = lower_expr_with_ctx ctx id_supply e in
      AST.mk_stmt ~pos:lowered.pos ~end_pos:lowered.end_pos ~file_id:lowered.file_id
        (AST.Block
           [
             AST.mk_stmt ~pos:lowered.pos ~end_pos:lowered.end_pos ~file_id:lowered.file_id
               (AST.ExpressionStmt lowered);
           ])
  | Surface.SEOBBlock b ->
      AST.mk_stmt ~pos:b.sb_pos ~end_pos:b.sb_end_pos ~file_id:b.sb_file_id
        (AST.Block (List.map (lower_stmt_with_ctx ctx id_supply) b.sb_stmts))

let lower_expr (id_supply : Id_supply.Id_supply.t) (se : Surface.surface_expr) : AST.expression =
  lower_expr_with_ctx empty_lower_context id_supply se

let lower_stmt (id_supply : Id_supply.Id_supply.t) (ss : Surface.surface_stmt) : AST.statement =
  lower_stmt_with_ctx empty_lower_context id_supply ss

let lower_match_arm (id_supply : Id_supply.Id_supply.t) (arm : Surface.surface_match_arm) : AST.match_arm =
  lower_match_arm_with_ctx empty_lower_context id_supply arm

let lower_expr_or_block_to_expr (id_supply : Id_supply.Id_supply.t) (eob : Surface.surface_expr_or_block) :
    AST.expression =
  lower_expr_or_block_to_expr_with_ctx empty_lower_context id_supply eob

let lower_expr_or_block_to_stmt (id_supply : Id_supply.Id_supply.t) (eob : Surface.surface_expr_or_block) :
    AST.statement =
  lower_expr_or_block_to_stmt_with_ctx empty_lower_context id_supply eob

(* ── Top-level declarations ── *)

let lower_method_sig (id_supply : Id_supply.Id_supply.t) (sm : Surface.surface_method_sig) : AST.method_sig =
  let method_generics, method_params =
    lower_callable_signature empty_lower_context sm.sm_generics
      (List.map (fun (name, typ) -> (name, Some typ)) sm.sm_params)
  in
  let bound_type_vars = bound_type_vars_of_generics method_generics in
  AST.
    {
      method_sig_id = sm.sm_id;
      method_name = sm.sm_name;
      method_generics;
      method_params =
        List.map
          (fun (name, typ_opt) ->
            match typ_opt with
            | Some typ -> (name, typ)
            | None -> failwith "Lower: trait method parameter unexpectedly lost its annotation")
          method_params;
      method_return_type = lower_type_expr_with_bound_vars bound_type_vars sm.sm_return_type;
      method_effect = sm.sm_effect;
      method_default_impl = Option.map (lower_expr_or_block_to_expr id_supply) sm.sm_default_impl;
    }

let lower_method_impl (id_supply : Id_supply.Id_supply.t) (smi : Surface.surface_method_impl) : AST.method_impl =
  let method_generics, method_params =
    lower_callable_signature empty_lower_context smi.smi_generics smi.smi_params
  in
  let bound_type_vars = bound_type_vars_of_generics method_generics in
  AST.
    {
      impl_method_id = smi.smi_id;
      impl_method_name = smi.smi_name;
      impl_method_generics = method_generics;
      impl_method_params = method_params;
      impl_method_return_type = Option.map (lower_type_expr_with_bound_vars bound_type_vars) smi.smi_return_type;
      impl_method_effect = smi.smi_effect;
      impl_method_override = smi.smi_override;
      impl_method_body = lower_expr_or_block_to_stmt id_supply smi.smi_body;
    }

let lower_variant_with_bound_vars (bound_type_vars : StringSet.t) (sv : Surface.surface_variant_def) :
    AST.variant_def =
  AST.
    {
      variant_name = sv.sv_name;
      variant_fields = List.map (lower_type_expr_with_bound_vars bound_type_vars) sv.sv_fields;
    }

let lower_record_type_field_with_bound_vars
    (bound_type_vars : StringSet.t) (f : Surface.surface_record_type_field) : AST.record_type_field =
  AST.
    {
      field_name = f.Surface.sf_name;
      field_type = lower_type_expr_with_bound_vars bound_type_vars f.Surface.sf_type;
    }

let lower_top_decl_with_ctx
    (ctx : lower_context) (id_supply : Id_supply.Id_supply.t) (ts : Surface.surface_top_stmt) : AST.statement list
    =
  let pos = ts.Surface.std_pos and end_pos = ts.Surface.std_end_pos and file_id = ts.Surface.std_file_id in
  let lower_trait_impl_decl
      (impl_type_params : AST.generic_param list)
      (impl_trait_name : string)
      (impl_for_type : Surface.surface_type_expr)
      (impl_methods : Surface.surface_method_impl list) =
    let impl_type_params =
      match impl_type_params with
      | [] -> infer_impl_type_params ctx impl_for_type
      | _ -> impl_type_params
    in
    let impl_bound_type_vars =
      List.fold_left
        (fun acc (p : AST.generic_param) -> StringSet.add p.name acc)
        StringSet.empty impl_type_params
    in
    let lower_method_impl_with_impl_bindings (smi : Surface.surface_method_impl) =
      let method_generics, method_params =
        lower_callable_signature ctx ~extra_bound_names:impl_bound_type_vars smi.smi_generics smi.smi_params
      in
      let method_bound_type_vars =
        StringSet.union impl_bound_type_vars (bound_type_vars_of_generics method_generics)
      in
      AST.
        {
          impl_method_id = smi.smi_id;
          impl_method_name = smi.smi_name;
          impl_method_generics = method_generics;
          impl_method_params = method_params;
          impl_method_return_type =
            Option.map (lower_type_expr_with_bound_vars method_bound_type_vars) smi.smi_return_type;
          impl_method_effect = smi.smi_effect;
          impl_method_override = smi.smi_override;
          impl_method_body = lower_expr_or_block_to_stmt_with_ctx ctx id_supply smi.smi_body;
        }
    in
    let idef =
      AST.
        {
          impl_type_params;
          impl_trait_name;
          impl_for_type = lower_type_expr_with_bound_vars impl_bound_type_vars impl_for_type;
          impl_methods = List.map lower_method_impl_with_impl_bindings impl_methods;
        }
    in
    [ AST.mk_stmt ~pos ~end_pos ~file_id (AST.ImplDef idef) ]
  in
  let lower_inherent_impl_decl
      (inherent_for_type : Surface.surface_type_expr) (inherent_methods : Surface.surface_method_impl list) =
    let lower_inherent_method_with_ctx (smi : Surface.surface_method_impl) =
      let method_generics, method_params = lower_callable_signature ctx smi.smi_generics smi.smi_params in
      let method_bound_type_vars = bound_type_vars_of_generics method_generics in
      AST.
        {
          impl_method_id = smi.smi_id;
          impl_method_name = smi.smi_name;
          impl_method_generics = method_generics;
          impl_method_params = method_params;
          impl_method_return_type =
            Option.map (lower_type_expr_with_bound_vars method_bound_type_vars) smi.smi_return_type;
          impl_method_effect = smi.smi_effect;
          impl_method_override = smi.smi_override;
          impl_method_body = lower_expr_or_block_to_stmt_with_ctx ctx id_supply smi.smi_body;
        }
    in
    let iid =
      AST.
        {
          inherent_for_type = lower_type_expr inherent_for_type;
          inherent_methods = List.map lower_inherent_method_with_ctx inherent_methods;
        }
    in
    [ AST.mk_stmt ~pos ~end_pos ~file_id (AST.InherentImplDef iid) ]
  in
  match ts.Surface.std_decl with
  | Surface.SLet { name; value; type_annotation } ->
      [
        AST.mk_stmt ~pos ~end_pos ~file_id
          (AST.Let
             {
               name;
               value = lower_expr_with_ctx ctx id_supply value;
               type_annotation = Option.map lower_type_expr type_annotation;
             });
      ]
  | Surface.SFnDecl { name; generics; params; return_type; is_effectful; body } ->
      let generics, params = lower_callable_signature ctx generics params in
      let bound_type_vars = bound_type_vars_of_generics generics in
      let fn_body = lower_expr_or_block_to_stmt_with_ctx ctx id_supply body in
      let fn_expr =
        AST.mk_expr ~id:(Id_supply.Id_supply.fresh id_supply) ~pos ~end_pos ~file_id
          (AST.Function
             {
               origin = AST.DeclaredFunction;
               generics;
               params;
               return_type = Option.map (lower_type_expr_with_bound_vars bound_type_vars) return_type;
               is_effectful;
               body = fn_body;
             })
      in
      [ AST.mk_stmt ~pos ~end_pos ~file_id (AST.Let { name; value = fn_expr; type_annotation = None }) ]
  | Surface.STypeDef { type_name; type_type_params; type_body; derive } ->
      let bound_type_vars = bound_type_vars_of_names type_type_params in
      let type_stmt =
        match type_body with
        | Surface.STTransparent transparent_body ->
            AST.mk_stmt ~pos ~end_pos ~file_id
              (AST.TypeAlias
                 {
                   alias_name = type_name;
                   alias_type_params = type_type_params;
                   alias_body = lower_type_expr_with_bound_vars bound_type_vars transparent_body;
                 })
        | Surface.STNamedProduct fields ->
            AST.mk_stmt ~pos ~end_pos ~file_id
              (AST.TypeDef
                 {
                   type_name;
                   type_type_params;
                   type_body =
                     AST.NamedTypeProduct
                       (List.map (lower_record_type_field_with_bound_vars bound_type_vars) fields);
                 })
        | Surface.STNamedWrapper wrapper_body ->
            AST.mk_stmt ~pos ~end_pos ~file_id
              (AST.TypeDef
                 {
                   type_name;
                   type_type_params;
                   type_body = AST.NamedTypeWrapper (lower_type_expr_with_bound_vars bound_type_vars wrapper_body);
                 })
        | Surface.STNamedSum variants ->
            AST.mk_stmt ~pos ~end_pos ~file_id
              (AST.EnumDef
                 {
                   name = type_name;
                   type_params = type_type_params;
                   variants = List.map (lower_variant_with_bound_vars bound_type_vars) variants;
                 })
      in
      let derive_stmts =
        if derive = [] then
          []
        else
          let for_type =
            if type_type_params = [] then
              AST.TCon type_name
            else
              AST.TApp (type_name, List.map (fun p -> AST.TVar p) type_type_params)
          in
          [
            AST.mk_stmt ~pos ~end_pos ~file_id
              (AST.DeriveDef { derive_traits = derive; derive_for_type = for_type });
          ]
      in
      type_stmt :: derive_stmts
  | Surface.SShapeDef { shape_name; shape_type_params; shape_fields } ->
      let bound_type_vars = bound_type_vars_of_names shape_type_params in
      let shape_stmt =
        AST.mk_stmt ~pos ~end_pos ~file_id
          (AST.ShapeDef
             {
               shape_name;
               shape_type_params;
               shape_fields = List.map (lower_record_type_field_with_bound_vars bound_type_vars) shape_fields;
             })
      in
      [ shape_stmt ]
  | Surface.STraitDef { name; type_param; supertraits; methods } ->
      let trait_bound_type_vars =
        match type_param with
        | None -> StringSet.empty
        | Some param -> StringSet.singleton param
      in
      let lower_method_sig_with_trait_bindings (sm : Surface.surface_method_sig) =
        let method_generics, method_params =
          lower_callable_signature ctx ~extra_bound_names:trait_bound_type_vars sm.sm_generics
            (List.map (fun (name, typ) -> (name, Some typ)) sm.sm_params)
        in
        let method_bound_type_vars =
          StringSet.union trait_bound_type_vars (bound_type_vars_of_generics method_generics)
        in
        AST.
          {
            method_sig_id = sm.sm_id;
            method_name = sm.sm_name;
            method_generics;
            method_params =
              List.map
                (fun (name, typ_opt) ->
                  match typ_opt with
                  | Some typ -> (name, typ)
                  | None -> failwith "Lower: trait method parameter unexpectedly lost its annotation")
                method_params;
            method_return_type = lower_type_expr_with_bound_vars method_bound_type_vars sm.sm_return_type;
            method_effect = sm.sm_effect;
            method_default_impl =
              Option.map (lower_expr_or_block_to_expr_with_ctx ctx id_supply) sm.sm_default_impl;
          }
      in
      let td =
        AST.{ name; type_param; supertraits; methods = List.map lower_method_sig_with_trait_bindings methods }
      in
      [ AST.mk_stmt ~pos ~end_pos ~file_id (AST.TraitDef td) ]
  | Surface.SAmbiguousImplDef { impl_type_params; impl_head_type; impl_methods } -> (
      match impl_head_type with
      | Surface.STApp (head_name, [ impl_for_type ]) when not (StringSet.mem head_name ctx.type_names) ->
          lower_trait_impl_decl impl_type_params head_name impl_for_type impl_methods
      | _ -> lower_inherent_impl_decl impl_head_type impl_methods)
  | Surface.SInherentImplDef { inherent_for_type; inherent_methods } ->
      lower_inherent_impl_decl inherent_for_type inherent_methods
  | Surface.SExpressionStmt e ->
      [ AST.mk_stmt ~pos ~end_pos ~file_id (AST.ExpressionStmt (lower_expr_with_ctx ctx id_supply e)) ]
  | Surface.SReturn e -> [ AST.mk_stmt ~pos ~end_pos ~file_id (AST.Return (lower_expr_with_ctx ctx id_supply e)) ]
  | Surface.SBlock block ->
      [
        AST.mk_stmt ~pos ~end_pos ~file_id
          (AST.Block (List.map (lower_stmt_with_ctx ctx id_supply) block.sb_stmts));
      ]

let lower_top_decl (id_supply : Id_supply.Id_supply.t) (ts : Surface.surface_top_stmt) : AST.statement list =
  lower_top_decl_with_ctx empty_lower_context id_supply ts

let lower_program (id_supply : Id_supply.Id_supply.t) (prog : Surface.surface_program) : AST.program =
  let ctx = lower_context_of_program prog in
  List.concat_map (lower_top_decl_with_ctx ctx id_supply) prog

(* ── Tests ── *)

(* Helper: wrap a top_decl in a surface_top_stmt with default positions for tests *)
let mk_test_ts decl = Surface.{ std_decl = decl; std_pos = 0; std_end_pos = 0; std_file_id = None }

let%test "lower SLet integer" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let value = Surface.mk_surface_expr ~id:0 ~pos:4 (Surface.SEInteger 42L) in
  let decl = Surface.SLet { name = "x"; value; type_annotation = None } in
  let result = lower_top_decl id_supply (mk_test_ts decl) in
  match result with
  | [ { AST.stmt = AST.Let { name = "x"; value = { AST.expr = AST.Integer 42L; _ }; _ }; _ } ] -> true
  | _ -> false

let%test "lower STNamedSum type definition" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let decl =
    Surface.STypeDef
      {
        type_name = "Color";
        type_type_params = [];
        type_body =
          Surface.STNamedSum
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
           variants =
             [
               { AST.variant_name = "Red"; _ };
               { AST.variant_name = "Green"; _ };
               { AST.variant_name = "Blue"; _ };
             ];
         };
     _;
   };
  ] ->
      true
  | _ -> false

let%test "lower transparent type definition" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let decl =
    Surface.STypeDef
      {
        type_name = "Point";
        type_type_params = [];
        type_body = Surface.STTransparent (Surface.STCon "Int");
        derive = [];
      }
  in
  let result = lower_top_decl id_supply (mk_test_ts decl) in
  match result with
  | [
   { AST.stmt = AST.TypeAlias { alias_name = "Point"; alias_type_params = []; alias_body = AST.TCon "Int" }; _ };
  ] ->
      true
  | _ -> false

let%test "transparent type params lower lowercase names to TVars in type bodies" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let decl =
    Surface.STypeDef
      {
        type_name = "Reducer";
        type_type_params = [ "a" ];
        type_body =
          Surface.STTransparent
            (Surface.STArrow ([ Surface.STCon "a"; Surface.STCon "a" ], Surface.STCon "a", false));
        derive = [];
      }
  in
  let result = lower_top_decl id_supply (mk_test_ts decl) in
  match result with
  | [
   {
     AST.stmt =
       AST.TypeAlias
         {
           alias_name = "Reducer";
           alias_type_params = [ "a" ];
           alias_body = AST.TArrow ([ AST.TVar "a"; AST.TVar "a" ], AST.TVar "a", false);
         };
     _;
   };
  ] ->
      true
  | _ -> false

let%test "lower trait object transparent type" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let decl =
    Surface.STypeDef
      {
        type_name = "Printer";
        type_type_params = [];
        type_body = Surface.STTransparent (Surface.STTraitObject [ "Show"; "Eq" ]);
        derive = [];
      }
  in
  let result = lower_top_decl id_supply (mk_test_ts decl) in
  match result with
  | [ { AST.stmt = AST.TypeAlias { alias_body = AST.TTraitObject [ "Show"; "Eq" ]; _ }; _ } ] -> true
  | _ -> false

let%test "lower intersection transparent type" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let decl =
    Surface.STypeDef
      {
        type_name = "NamedAge";
        type_type_params = [];
        type_body =
          Surface.STTransparent
            (Surface.STIntersection
               [
                 Surface.STRecord ([ Surface.{ sf_name = "name"; sf_type = Surface.STCon "Str" } ], None);
                 Surface.STRecord ([ Surface.{ sf_name = "age"; sf_type = Surface.STCon "Int" } ], None);
               ]);
        derive = [];
      }
  in
  let result = lower_top_decl id_supply (mk_test_ts decl) in
  match result with
  | [ { AST.stmt = AST.TypeAlias { alias_body = AST.TIntersection [ AST.TRecord _; AST.TRecord _ ]; _ }; _ } ] ->
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
        sb_stmts = [ Surface.mk_surface_stmt ~pos:0 (Surface.SSExpressionStmt e) ];
        sb_pos = 0;
        sb_end_pos = 5;
        sb_file_id = None;
      }
  in
  let result = lower_expr_or_block_to_stmt id_supply (Surface.SEOBBlock block) in
  match result.stmt with
  | AST.Block [ { AST.stmt = AST.ExpressionStmt { AST.expr = AST.Integer 7L; _ }; _ } ] -> true
  | _ -> false

let%test "lower match arm body (expression arm)" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let e = Surface.mk_surface_expr ~id:1 ~pos:0 (Surface.SEString "yes") in
  let arm =
    Surface.
      {
        se_patterns = [ Surface.mk_surface_pat ~pos:0 (Surface.SPLiteral (AST.LBool true)) ];
        se_arm_body = SEOBExpr e;
      }
  in
  let result = lower_match_arm id_supply arm in
  match result.body.expr with
  | AST.String "yes" -> true
  | _ -> false

(* ── Surface lowering tests ── *)

let%test "lower SEArrowLambda to canonical Function" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let body_expr = Surface.mk_surface_expr ~id:1 ~pos:5 (Surface.SEInteger 42L) in
  let lambda =
    Surface.mk_surface_expr ~id:2 ~pos:0
      (Surface.SEArrowLambda
         {
           se_lambda_params = [ ("x", None) ];
           se_lambda_is_effectful = false;
           se_lambda_body = Surface.SEOBExpr body_expr;
         })
  in
  match (lower_expr id_supply lambda).expr with
  | AST.Function { params = [ ("x", None) ]; is_effectful = false; body = { stmt = AST.Block [ _ ]; _ }; _ } ->
      true
  | _ -> false

let%test "arrow lambda shorthand param lowers with program trait context" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let trait_decl =
    mk_test_ts (Surface.STraitDef { name = "Named"; type_param = None; supertraits = []; methods = [] })
  in
  let body_expr = Surface.mk_surface_expr ~id:1 ~pos:0 (Surface.SEIdentifier "x") in
  let lambda_expr =
    Surface.mk_surface_expr ~id:2 ~pos:0
      (Surface.SEArrowLambda
         {
           se_lambda_params = [ ("x", Some (Surface.STCon "Named")) ];
           se_lambda_is_effectful = false;
           se_lambda_body = Surface.SEOBExpr body_expr;
         })
  in
  let program = lower_program id_supply [ trait_decl; mk_test_ts (Surface.SExpressionStmt lambda_expr) ] in
  match List.rev program with
  | { AST.stmt = AST.ExpressionStmt { AST.expr = AST.Function fn; _ }; _ } :: _ -> (
      match (fn.generics, fn.params) with
      | Some [ { AST.name = "g0"; constraints = [ "Named" ] } ], [ ("x", Some (AST.TVar "g0")) ] -> true
      | _ -> false)
  | _ -> false

let%test "lower SEBlockExpr to AST.BlockExpr" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let e = Surface.mk_surface_expr ~id:1 ~pos:0 (Surface.SEInteger 7L) in
  let block =
    Surface.
      {
        sb_stmts = [ Surface.mk_surface_stmt ~pos:0 (Surface.SSExpressionStmt e) ];
        sb_pos = 0;
        sb_end_pos = 5;
        sb_file_id = None;
      }
  in
  let se = Surface.mk_surface_expr ~id:2 ~pos:0 (Surface.SEBlockExpr block) in
  match (lower_expr id_supply se).expr with
  | AST.BlockExpr [ { stmt = AST.ExpressionStmt { expr = AST.Integer 7L; _ }; _ } ] -> true
  | _ -> false

let%test "lower pipe desugars rhs callable into Call" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let se =
    Surface.mk_surface_expr ~id:3 ~pos:0
      (Surface.SEInfix
         ( Surface.mk_surface_expr ~id:1 ~pos:0 (Surface.SEIdentifier "x"),
           "|>",
           Surface.mk_surface_expr ~id:2 ~pos:5 (Surface.SEIdentifier "f") ))
  in
  match (lower_expr id_supply se).expr with
  | AST.Call ({ AST.expr = AST.Identifier "f"; _ }, [ { AST.expr = AST.Identifier "x"; _ } ]) -> true
  | _ -> false

let%test "lower pipe prepends lhs into existing rhs call" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let rhs_call =
    Surface.mk_surface_expr ~id:4 ~pos:5
      (Surface.SECall
         ( Surface.mk_surface_expr ~id:2 ~pos:5 (Surface.SEIdentifier "f"),
           [ Surface.mk_surface_expr ~id:3 ~pos:7 (Surface.SEInteger 1L) ] ))
  in
  let se =
    Surface.mk_surface_expr ~id:5 ~pos:0
      (Surface.SEInfix (Surface.mk_surface_expr ~id:1 ~pos:0 (Surface.SEIdentifier "x"), "|>", rhs_call))
  in
  match (lower_expr id_supply se).expr with
  | AST.Call
      ( { AST.expr = AST.Identifier "f"; _ },
        [ { AST.expr = AST.Identifier "x"; _ }; { AST.expr = AST.Integer 1L; _ } ] ) ->
      true
  | _ -> false

let%test "lower pipe prepends lhs into qualified method call args" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let rhs_method =
    Surface.mk_surface_expr ~id:4 ~pos:5
      (Surface.SEMethodCall
         {
           se_receiver = Surface.mk_surface_expr ~id:2 ~pos:5 (Surface.SEIdentifier "Box");
           se_method = "map";
           se_type_args = None;
           se_args = [ Surface.mk_surface_expr ~id:3 ~pos:13 (Surface.SEInteger 1L) ];
         })
  in
  let se =
    Surface.mk_surface_expr ~id:5 ~pos:0
      (Surface.SEInfix (Surface.mk_surface_expr ~id:1 ~pos:0 (Surface.SEIdentifier "x"), "|>", rhs_method))
  in
  match (lower_expr id_supply se).expr with
  | AST.MethodCall
      {
        mc_receiver = { AST.expr = AST.Identifier "Box"; _ };
        mc_method = "map";
        mc_args = [ { AST.expr = AST.Identifier "x"; _ }; { AST.expr = AST.Integer 1L; _ } ];
        _;
      } ->
      true
  | _ -> false

let%test "lower pipe substitutes lhs into projection section rhs" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let rhs =
    Surface.mk_surface_expr ~id:3 ~pos:8 ~end_pos:18
      (Surface.SEFieldAccess
         (Surface.mk_surface_expr ~id:2 ~pos:8 ~end_pos:8 (Surface.SEIdentifier "_"), "updated_at"))
  in
  let se =
    Surface.mk_surface_expr ~id:4 ~pos:0 ~end_pos:18
      (Surface.SEInfix (Surface.mk_surface_expr ~id:1 ~pos:0 ~end_pos:3 (Surface.SEIdentifier "post"), "|>", rhs))
  in
  match (lower_expr id_supply se).expr with
  | AST.FieldAccess ({ AST.expr = AST.Identifier "post"; pos; end_pos; _ }, "updated_at") ->
      pos = 0 && end_pos = 3
  | _ -> false

let%test "lower pipe substitutes lhs into infix section rhs" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let rhs =
    Surface.mk_surface_expr ~id:4 ~pos:5
      (Surface.SEInfix
         ( Surface.mk_surface_expr ~id:2 ~pos:5 (Surface.SEIdentifier "_"),
           "+",
           Surface.mk_surface_expr ~id:3 ~pos:9 (Surface.SEInteger 1L) ))
  in
  let se =
    Surface.mk_surface_expr ~id:5 ~pos:0
      (Surface.SEInfix (Surface.mk_surface_expr ~id:1 ~pos:0 (Surface.SEIdentifier "x"), "|>", rhs))
  in
  match (lower_expr id_supply se).expr with
  | AST.Infix ({ AST.expr = AST.Identifier "x"; _ }, "+", { AST.expr = AST.Integer 1L; _ }) -> true
  | _ -> false

let%test "lower SEOBBlock in match arm -> AST.BlockExpr" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let e = Surface.mk_surface_expr ~id:1 ~pos:0 (Surface.SEString "block") in
  let block =
    Surface.
      {
        sb_stmts = [ Surface.mk_surface_stmt ~pos:0 (Surface.SSExpressionStmt e) ];
        sb_pos = 0;
        sb_end_pos = 5;
        sb_file_id = None;
      }
  in
  let arm =
    Surface.
      {
        se_patterns = [ Surface.mk_surface_pat ~pos:0 Surface.SPWildcard ];
        se_arm_body = Surface.SEOBBlock block;
      }
  in
  let result = lower_match_arm id_supply arm in
  match result.body.expr with
  | AST.BlockExpr [ { stmt = AST.ExpressionStmt { expr = AST.String "block"; _ }; _ } ] -> true
  | _ -> false

let%test "STNamedSum with postfix derive generates DeriveDef" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let decl =
    Surface.STypeDef
      {
        type_name = "Color";
        type_type_params = [];
        type_body = Surface.STNamedSum [];
        derive =
          [
            AST.{ derive_trait_name = "Eq"; derive_trait_constraints = [] };
            AST.{ derive_trait_name = "Show"; derive_trait_constraints = [] };
          ];
      }
  in
  let result = lower_top_decl id_supply (mk_test_ts decl) in
  match result with
  | [
   { AST.stmt = AST.EnumDef { name = "Color"; _ }; _ };
   {
     AST.stmt =
       AST.DeriveDef
         {
           derive_traits = [ { derive_trait_name = "Eq"; _ }; { derive_trait_name = "Show"; _ } ];
           derive_for_type = AST.TCon "Color";
         };
     _;
   };
  ] ->
      true
  | _ -> false

let%test "STypeDef with postfix derive generates DeriveDef" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let decl =
    Surface.STypeDef
      {
        type_name = "MyInt";
        type_type_params = [];
        type_body = Surface.STNamedWrapper (Surface.STCon "Int");
        derive = [ AST.{ derive_trait_name = "Eq"; derive_trait_constraints = [] } ];
      }
  in
  let result = lower_top_decl id_supply (mk_test_ts decl) in
  match result with
  | [
   { AST.stmt = AST.TypeDef { type_name = "MyInt"; _ }; _ };
   {
     AST.stmt =
       AST.DeriveDef { derive_traits = [ { derive_trait_name = "Eq"; _ } ]; derive_for_type = AST.TCon "MyInt" };
     _;
   };
  ] ->
      true
  | _ -> false

let%test "STypeDef lowering preserves derive target params" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let decl =
    Surface.STypeDef
      {
        type_name = "Box";
        type_type_params = [ "t" ];
        type_body =
          Surface.STTransparent
            (Surface.STRecord ([ Surface.{ sf_name = "value"; sf_type = Surface.STVar "t" } ], None));
        derive =
          [
            AST.
              {
                derive_trait_name = "Forwarder";
                derive_trait_constraints = [ AST.{ name = "u"; constraints = [] } ];
              };
          ];
      }
  in
  let result = lower_top_decl id_supply (mk_test_ts decl) in
  match result with
  | [
   { AST.stmt = AST.TypeAlias { alias_name = "Box"; _ }; _ };
   {
     AST.stmt =
       AST.DeriveDef
         {
           derive_traits =
             [ { derive_trait_name = "Forwarder"; derive_trait_constraints = [ { name = "u"; _ } ] } ];
           derive_for_type = AST.TApp ("Box", [ AST.TVar "t" ]);
         };
     _;
   };
  ] ->
      true
  | _ -> false

let%test "lower_method_impl carries override flag" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let body_expr = Surface.mk_surface_expr ~id:1 ~pos:0 (Surface.SEInteger 1L) in
  let smi =
    Surface.
      {
        smi_id = 1;
        smi_name = "foo";
        smi_generics = None;
        smi_params = [];
        smi_return_type = None;
        smi_effect = None;
        smi_override = true;
        smi_body = SEOBExpr body_expr;
      }
  in
  let result = lower_method_impl id_supply smi in
  result.AST.impl_method_override = true

let%test "SFnDecl with expr body -> Block[ExpressionStmt]" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let body_expr = Surface.mk_surface_expr ~id:1 ~pos:5 (Surface.SEInteger 99L) in
  let decl =
    Surface.SFnDecl
      {
        name = "answer";
        generics = None;
        params = [];
        return_type = None;
        is_effectful = false;
        body = Surface.SEOBExpr body_expr;
      }
  in
  let result = lower_top_decl id_supply (mk_test_ts decl) in
  match result with
  | [
   {
     AST.stmt =
       AST.Let
         {
           name = "answer";
           value =
             {
               AST.expr =
                 AST.Function
                   {
                     body =
                       { stmt = AST.Block [ { stmt = AST.ExpressionStmt { expr = AST.Integer 99L; _ }; _ } ]; _ };
                     _;
                   };
               _;
             };
           _;
         };
     _;
   };
  ] ->
      true
  | _ -> false

let%test "bare trait param shorthand lowers to fresh constrained generic" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let body_expr = Surface.mk_surface_expr ~id:1 ~pos:5 (Surface.SEIdentifier "x") in
  let decl =
    Surface.SFnDecl
      {
        name = "who_dis";
        generics = None;
        params = [ ("x", Some (Surface.STCon "JungleDweller")) ];
        return_type = Some (Surface.STCon "Str");
        is_effectful = false;
        body = Surface.SEOBExpr body_expr;
      }
  in
  let prog =
    lower_program id_supply
      [
        mk_test_ts
          (Surface.STraitDef { name = "JungleDweller"; type_param = Some "a"; supertraits = []; methods = [] });
        mk_test_ts decl;
      ]
  in
  match prog with
  | [
   { AST.stmt = AST.TraitDef _; _ };
   {
     AST.stmt =
       AST.Let
         {
           name = "who_dis";
           value =
             {
               AST.expr =
                 AST.Function
                   {
                     generics = Some [ { AST.name = "g0"; constraints = [ "JungleDweller" ] } ];
                     params = [ ("x", Some (AST.TVar "g0")) ];
                     return_type = Some (AST.TCon "Str");
                     _;
                   };
               _;
             };
           _;
         };
     _;
   };
  ] ->
      true
  | _ -> false

let%test "multiple shorthand params lower to independent generics" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let body_expr = Surface.mk_surface_expr ~id:1 ~pos:5 (Surface.SEBoolean true) in
  let decl =
    Surface.SFnDecl
      {
        name = "same_species";
        generics = None;
        params = [ ("x", Some (Surface.STCon "Eq")); ("y", Some (Surface.STCon "Eq")) ];
        return_type = Some (Surface.STCon "Bool");
        is_effectful = false;
        body = Surface.SEOBExpr body_expr;
      }
  in
  let prog = lower_program id_supply [ mk_test_ts decl ] in
  match prog with
  | [
   {
     AST.stmt =
       AST.Let
         {
           value =
             {
               AST.expr =
                 AST.Function
                   {
                     generics =
                       Some
                         [
                           { AST.name = "g0"; constraints = [ "Eq" ] };
                           { AST.name = "g1"; constraints = [ "Eq" ] };
                         ];
                     params = [ ("x", Some (AST.TVar "g0")); ("y", Some (AST.TVar "g1")) ];
                     _;
                   };
               _;
             };
           _;
         };
     _;
   };
  ] ->
      true
  | _ -> false

let%test "ampersand shorthand param lowers to one constrained generic with multiple traits" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let body_expr = Surface.mk_surface_expr ~id:1 ~pos:5 (Surface.SEIdentifier "x") in
  let decl =
    Surface.SFnDecl
      {
        name = "describe";
        generics = None;
        params = [ ("x", Some (Surface.STConstraintShorthand [ "Named"; "Aged" ])) ];
        return_type = Some (Surface.STCon "Str");
        is_effectful = false;
        body = Surface.SEOBExpr body_expr;
      }
  in
  let prog = lower_program id_supply [ mk_test_ts decl ] in
  match prog with
  | [
   {
     AST.stmt =
       AST.Let
         {
           value =
             {
               AST.expr =
                 AST.Function
                   {
                     generics = Some [ { AST.name = "g0"; constraints = [ "Named"; "Aged" ] } ];
                     params = [ ("x", Some (AST.TVar "g0")) ];
                     _;
                   };
               _;
             };
           _;
         };
     _;
   };
  ] ->
      true
  | _ -> false

let%test "explicit generics and shorthand params stay independent" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let body_expr = Surface.mk_surface_expr ~id:1 ~pos:5 (Surface.SEString "") in
  let decl =
    Surface.SFnDecl
      {
        name = "pair";
        generics = Some [ AST.{ name = "a"; constraints = [ "Named" ] } ];
        params = [ ("left", Some (Surface.STCon "a")); ("right", Some (Surface.STCon "Named")) ];
        return_type = Some (Surface.STCon "Str");
        is_effectful = false;
        body = Surface.SEOBExpr body_expr;
      }
  in
  let prog =
    lower_program id_supply
      [
        mk_test_ts (Surface.STraitDef { name = "Named"; type_param = None; supertraits = []; methods = [] });
        mk_test_ts decl;
      ]
  in
  match prog with
  | [
   { AST.stmt = AST.TraitDef _; _ };
   {
     AST.stmt =
       AST.Let
         {
           value =
             {
               AST.expr =
                 AST.Function
                   {
                     generics =
                       Some
                         [
                           { AST.name = "a"; constraints = [ "Named" ] };
                           { AST.name = "g0"; constraints = [ "Named" ] };
                         ];
                     params = [ ("left", Some (AST.TVar "a")); ("right", Some (AST.TVar "g0")) ];
                     _;
                   };
               _;
             };
           _;
         };
     _;
   };
  ] ->
      true
  | _ -> false

let%test "explicit impl binders lower lowercase names in impl target and method signature" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let method_body = Surface.mk_surface_expr ~id:1 ~pos:0 (Surface.SEString "") in
  let decl =
    Surface.SAmbiguousImplDef
      {
        impl_type_params = [ AST.{ name = "a"; constraints = [] } ];
        impl_head_type = Surface.STApp ("Show", [ Surface.STApp ("Option", [ Surface.STCon "a" ]) ]);
        impl_methods =
          [
            Surface.
              {
                smi_id = 1;
                smi_name = "show";
                smi_generics = None;
                smi_params = [ ("x", Some (Surface.STApp ("Option", [ Surface.STCon "a" ]))) ];
                smi_return_type = Some (Surface.STCon "Str");
                smi_effect = None;
                smi_override = false;
                smi_body = Surface.SEOBExpr method_body;
              };
          ];
      }
  in
  let result = lower_top_decl id_supply (mk_test_ts decl) in
  match result with
  | [
   {
     AST.stmt =
       AST.ImplDef
         {
           impl_type_params = [ { AST.name = "a"; constraints = [] } ];
           impl_trait_name = "Show";
           impl_for_type = AST.TApp ("Option", [ AST.TVar "a" ]);
           impl_methods =
             [
               {
                 AST.impl_method_params = [ ("x", Some (AST.TApp ("Option", [ AST.TVar "a" ]))) ];
                 impl_method_return_type = Some (AST.TCon "Str");
                 _;
               };
             ];
         };
     _;
   };
  ] ->
      true
  | _ -> false

let%test "omitted impl binders are inferred from free vars in impl target" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let method_body = Surface.mk_surface_expr ~id:1 ~pos:0 (Surface.SEString "") in
  let decl =
    Surface.SAmbiguousImplDef
      {
        impl_type_params = [];
        impl_head_type = Surface.STApp ("Show", [ Surface.STApp ("List", [ Surface.STCon "a" ]) ]);
        impl_methods =
          [
            Surface.
              {
                smi_id = 1;
                smi_name = "show";
                smi_generics = None;
                smi_params = [ ("self", Some (Surface.STApp ("List", [ Surface.STCon "a" ]))) ];
                smi_return_type = Some (Surface.STCon "Str");
                smi_effect = None;
                smi_override = false;
                smi_body = Surface.SEOBExpr method_body;
              };
          ];
      }
  in
  let result = lower_top_decl_with_ctx empty_lower_context id_supply (mk_test_ts decl) in
  match result with
  | [
   {
     AST.stmt =
       AST.ImplDef
         {
           impl_type_params = [ { AST.name = "a"; constraints = [] } ];
           impl_trait_name = "Show";
           impl_for_type = AST.TApp ("List", [ AST.TVar "a" ]);
           impl_methods =
             [
               {
                 AST.impl_method_params = [ ("self", Some (AST.TApp ("List", [ AST.TVar "a" ]))) ];
                 impl_method_return_type = Some (AST.TCon "Str");
                 _;
               };
             ];
         };
     _;
   };
  ] ->
      true
  | _ -> false

let%test "ambiguous vNext impl head lowers to inherent impl when head names a type" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let ctx = { empty_lower_context with type_names = StringSet.add "Result" empty_lower_context.type_names } in
  let decl =
    Surface.SAmbiguousImplDef
      {
        impl_type_params = [];
        impl_head_type = Surface.STApp ("Result", [ Surface.STCon "a"; Surface.STCon "b" ]);
        impl_methods = [];
      }
  in
  let result = lower_top_decl_with_ctx ctx id_supply (mk_test_ts decl) in
  match result with
  | [
   {
     AST.stmt =
       AST.InherentImplDef
         { inherent_for_type = AST.TApp ("Result", [ AST.TCon "a"; AST.TCon "b" ]); inherent_methods = [] };
     _;
   };
  ] ->
      true
  | _ -> false

let%test "ambiguous vNext impl head lowers to trait impl when head names a trait" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let ctx =
    { empty_lower_context with constraint_names = StringSet.add "Show" empty_lower_context.constraint_names }
  in
  let decl =
    Surface.SAmbiguousImplDef
      {
        impl_type_params = [];
        impl_head_type = Surface.STApp ("Show", [ Surface.STApp ("Option", [ Surface.STCon "a" ]) ]);
        impl_methods = [];
      }
  in
  let result = lower_top_decl_with_ctx ctx id_supply (mk_test_ts decl) in
  match result with
  | [
   {
     AST.stmt =
       AST.ImplDef
         {
           impl_trait_name = "Show";
           impl_type_params = [ { AST.name = "a"; constraints = [] } ];
           impl_for_type = AST.TApp ("Option", [ AST.TVar "a" ]);
           impl_methods = [];
         };
     _;
   };
  ] ->
      true
  | _ -> false

let%test "ambiguous vNext impl head defaults unknown app head to trait impl" =
  let id_supply = Id_supply.Id_supply.create 0 in
  let decl =
    Surface.SAmbiguousImplDef
      {
        impl_type_params = [];
        impl_head_type = Surface.STApp ("NonexistentTrait", [ Surface.STCon "Int" ]);
        impl_methods = [];
      }
  in
  let result = lower_top_decl_with_ctx empty_lower_context id_supply (mk_test_ts decl) in
  match result with
  | [ { AST.stmt = AST.ImplDef { impl_trait_name = "NonexistentTrait"; impl_for_type = AST.TCon "Int"; _ }; _ } ]
    ->
      true
  | _ -> false
