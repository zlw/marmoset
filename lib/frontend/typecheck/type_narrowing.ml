module AST = Syntax.Ast.AST

type path = {
  root : string;
  fields : string list;
}
[@@deriving show, eq]

let rec path_of_expr (expr : AST.expression) : path option =
  match expr.expr with
  | AST.Identifier name -> Some { root = name; fields = [] }
  | AST.FieldAccess (receiver, field_name) -> (
      match path_of_expr receiver with
      | Some path -> Some { path with fields = path.fields @ [ field_name ] }
      | None -> None)
  | _ -> None

let is_identifier_path (path : path) : bool = path.fields = []
let to_string (path : path) : string = String.concat "." (path.root :: path.fields)

let temp_name (path : path) (suffix : string) : string =
  String.concat "_" (path.root :: path.fields @ [ suffix ])

let replacement_identifier (name : string) (expr : AST.expression) : AST.expression =
  { expr with expr = AST.Identifier name }

let substitute_path_in_expr
    ?(descend_into_functions = false)
    (target : path)
    ~(replace : AST.expression -> AST.expression)
    (expr : AST.expression) : AST.expression =
  let rec subst_expr (expr : AST.expression) =
    if path_of_expr expr = Some target then
      replace expr
    else
      match expr.expr with
      | AST.Identifier _ | AST.Integer _ | AST.Float _ | AST.Boolean _ | AST.String _ -> expr
      | AST.Prefix (op, operand) -> { expr with expr = AST.Prefix (op, subst_expr operand) }
      | AST.Infix (left, op, right) -> { expr with expr = AST.Infix (subst_expr left, op, subst_expr right) }
      | AST.TypeCheck (inner, type_ann) -> { expr with expr = AST.TypeCheck (subst_expr inner, type_ann) }
      | AST.If (cond, cons, alt) ->
          { expr with expr = AST.If (subst_expr cond, subst_stmt cons, Option.map subst_stmt alt) }
      | AST.Function _ when not descend_into_functions -> expr
      | AST.Function fn_expr ->
          { expr with expr = AST.Function { fn_expr with body = subst_stmt fn_expr.body } }
      | AST.Call (fn_expr, args) ->
          { expr with expr = AST.Call (subst_expr fn_expr, List.map subst_expr args) }
      | AST.Array elements -> { expr with expr = AST.Array (List.map subst_expr elements) }
      | AST.Hash pairs ->
          {
            expr with
            expr = AST.Hash (List.map (fun (key, value) -> (subst_expr key, subst_expr value)) pairs);
          }
      | AST.Index (container, index) -> { expr with expr = AST.Index (subst_expr container, subst_expr index) }
      | AST.EnumConstructor (enum_name, variant_name, args) ->
          { expr with expr = AST.EnumConstructor (enum_name, variant_name, List.map subst_expr args) }
      | AST.Match (scrutinee, arms) ->
          {
            expr with
            expr =
              AST.Match
                ( subst_expr scrutinee,
                  List.map
                    (fun (arm : AST.match_arm) -> { arm with body = subst_expr arm.body })
                    arms );
          }
      | AST.RecordLit (fields, spread) ->
          {
            expr with
            expr =
              AST.RecordLit
                ( List.map
                    (fun (field : AST.record_field) ->
                      { field with field_value = Option.map subst_expr field.field_value })
                    fields,
                  Option.map subst_expr spread );
          }
      | AST.FieldAccess (receiver, field_name) -> { expr with expr = AST.FieldAccess (subst_expr receiver, field_name) }
      | AST.MethodCall { mc_receiver; mc_method; mc_type_args; mc_args } ->
          {
            expr with
            expr =
              AST.MethodCall
                {
                  mc_receiver = subst_expr mc_receiver;
                  mc_method;
                  mc_type_args;
                  mc_args = List.map subst_expr mc_args;
                };
          }
      | AST.BlockExpr stmts -> { expr with expr = AST.BlockExpr (List.map subst_stmt stmts) }
  and subst_stmt (stmt : AST.statement) =
    match stmt.stmt with
    | AST.Let ({ value; _ } as let_binding) -> { stmt with stmt = AST.Let { let_binding with value = subst_expr value } }
    | AST.Return expr -> { stmt with stmt = AST.Return (subst_expr expr) }
    | AST.ExpressionStmt expr -> { stmt with stmt = AST.ExpressionStmt (subst_expr expr) }
    | AST.Block stmts -> { stmt with stmt = AST.Block (List.map subst_stmt stmts) }
    | AST.EnumDef _ | AST.TypeDef _ | AST.ShapeDef _ | AST.TraitDef _ | AST.ImplDef _ | AST.InherentImplDef _
    | AST.DeriveDef _ | AST.TypeAlias _ ->
        stmt
  in
  subst_expr expr

let substitute_path_in_stmt
    ?(descend_into_functions = false)
    (target : path)
    ~(replace : AST.expression -> AST.expression)
    (stmt : AST.statement) : AST.statement =
  let rec subst_stmt (stmt : AST.statement) =
    match stmt.stmt with
    | AST.Let ({ value; _ } as let_binding) ->
        {
          stmt with
          stmt =
            AST.Let
              { let_binding with value = substitute_path_in_expr ~descend_into_functions target ~replace value };
        }
    | AST.Return expr ->
        {
          stmt with
          stmt = AST.Return (substitute_path_in_expr ~descend_into_functions target ~replace expr);
        }
    | AST.ExpressionStmt expr ->
        {
          stmt with
          stmt = AST.ExpressionStmt (substitute_path_in_expr ~descend_into_functions target ~replace expr);
        }
    | AST.Block stmts -> { stmt with stmt = AST.Block (List.map subst_stmt stmts) }
    | AST.EnumDef _ | AST.TypeDef _ | AST.ShapeDef _ | AST.TraitDef _ | AST.ImplDef _ | AST.InherentImplDef _
    | AST.DeriveDef _ | AST.TypeAlias _ ->
        stmt
  in
  subst_stmt stmt

let normalize_if_nonempty (members : Types.mono_type list) : Types.mono_type option =
  match members with
  | [] -> None
  | [ single ] -> Some single
  | many -> Some (Types.normalize_union many)

let compute_narrowed_type (current_type : Types.mono_type) (narrow_type : Types.mono_type) :
    Types.mono_type option =
  let current_type = Types.canonicalize_mono_type current_type in
  let narrow_type = Types.canonicalize_mono_type narrow_type in
  if Annotation.is_subtype_of current_type narrow_type then
    Some current_type
  else
    match current_type with
    | Types.TUnion members ->
        members
        |> List.map Types.canonicalize_mono_type
        |> List.filter (fun member -> Annotation.is_subtype_of member narrow_type)
        |> normalize_if_nonempty
    | _ -> None

let compute_complement_type (current_type : Types.mono_type) (narrow_type : Types.mono_type) :
    Types.mono_type option =
  let current_type = Types.canonicalize_mono_type current_type in
  let narrow_type = Types.canonicalize_mono_type narrow_type in
  match current_type with
  | Types.TUnion members ->
      members
      |> List.map Types.canonicalize_mono_type
      |> List.filter (fun member -> not (Annotation.is_subtype_of member narrow_type))
      |> normalize_if_nonempty
  | _ when Annotation.is_subtype_of current_type narrow_type -> None
  | _ -> Some current_type

let static_outcome
    (current_type_opt : Types.mono_type option)
    (narrow_type : Types.mono_type) :
    [ `Always_true | `Always_false | `Runtime_check of Types.mono_type option ] =
  match current_type_opt with
  | None -> `Runtime_check None
  | Some current_type -> (
      match compute_narrowed_type current_type narrow_type with
      | Some narrowed when Types.canonicalize_mono_type narrowed = Types.canonicalize_mono_type current_type ->
          `Always_true
      | _ -> (
          match compute_complement_type current_type narrow_type with
          | Some complement
            when Types.canonicalize_mono_type complement = Types.canonicalize_mono_type current_type ->
              `Always_false
          | complement_type_opt -> `Runtime_check complement_type_opt))

let%test "path_of_expr extracts nested field paths" =
  let expr =
    AST.mk_expr ~id:3
      (AST.FieldAccess
         ( AST.mk_expr ~id:2 (AST.FieldAccess (AST.mk_expr ~id:1 (AST.Identifier "box"), "value")),
           "name" ))
  in
  path_of_expr expr = Some { root = "box"; fields = [ "value"; "name" ] }

let%test "substitute_path_in_expr rewrites matching field access" =
  let expr =
    AST.mk_expr ~id:4
      (AST.MethodCall
         {
           mc_receiver =
             AST.mk_expr ~id:2 (AST.FieldAccess (AST.mk_expr ~id:1 (AST.Identifier "box"), "value"));
           mc_method = "show";
           mc_type_args = None;
           mc_args = [];
         })
  in
  let target = { root = "box"; fields = [ "value" ] } in
  match
    substitute_path_in_expr target ~replace:(replacement_identifier "box_value_typed") expr
  with
  | { AST.expr = AST.MethodCall { mc_receiver = { AST.expr = AST.Identifier "box_value_typed"; _ }; _ }; _ } ->
      true
  | _ -> false

let%test "compute_narrowed_type filters union members" =
  compute_narrowed_type (Types.TUnion [ Types.TInt; Types.TString ]) Types.TInt = Some Types.TInt

let%test "compute_complement_type filters union members" =
  compute_complement_type (Types.TUnion [ Types.TInt; Types.TString ]) Types.TInt = Some Types.TString
