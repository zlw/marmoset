module AST = Syntax.Ast.AST

type t = {
  mutable next_expr_id : int;
  mutable next_method_id : int;
}

let create_from_program (program : AST.program) : t =
  let max_expr_id = ref 0 in
  let max_method_id = ref 0 in
  let rec visit_expr (expr : AST.expression) =
    max_expr_id := max !max_expr_id expr.id;
    match expr.expr with
    | AST.Identifier _ | AST.Integer _ | AST.Float _ | AST.Boolean _ | AST.String _ -> ()
    | AST.Array items -> List.iter visit_expr items
    | AST.Index (container, index) ->
        visit_expr container;
        visit_expr index
    | AST.Hash pairs ->
        List.iter
          (fun (key, value) ->
            visit_expr key;
            visit_expr value)
          pairs
    | AST.Prefix (_, operand) -> visit_expr operand
    | AST.Infix (left, _, right) ->
        visit_expr left;
        visit_expr right
    | AST.TypeCheck (subject, _) -> visit_expr subject
    | AST.If (condition, consequence, alternative) ->
        visit_expr condition;
        visit_stmt consequence;
        Option.iter visit_stmt alternative
    | AST.Function fn_body -> visit_stmt fn_body.body
    | AST.Call (callee, args) ->
        visit_expr callee;
        List.iter visit_expr args
    | AST.EnumConstructor (_, _, args) -> List.iter visit_expr args
    | AST.Match (scrutinee, arms) ->
        visit_expr scrutinee;
        List.iter (fun (arm : AST.match_arm) -> visit_expr arm.body) arms
    | AST.RecordLit (fields, spread) ->
        List.iter
          (fun (field : AST.record_field) -> Option.iter visit_expr field.field_value)
          fields;
        Option.iter visit_expr spread
    | AST.FieldAccess (receiver, _) -> visit_expr receiver
    | AST.MethodCall { mc_receiver; mc_args; _ } ->
        visit_expr mc_receiver;
        List.iter visit_expr mc_args
    | AST.BlockExpr stmts -> List.iter visit_stmt stmts
  and visit_trait_method (m : AST.method_sig) =
    max_method_id := max !max_method_id m.method_sig_id;
    Option.iter visit_expr m.method_default_impl
  and visit_impl_method (m : AST.method_impl) =
    max_method_id := max !max_method_id m.impl_method_id;
    visit_stmt m.impl_method_body
  and visit_stmt (stmt : AST.statement) =
    match stmt.stmt with
    | AST.Let let_binding -> visit_expr let_binding.value
    | AST.Return expr | AST.ExpressionStmt expr -> visit_expr expr
    | AST.Block stmts -> List.iter visit_stmt stmts
    | AST.EnumDef _ | AST.TypeAlias _ | AST.DeriveDef _ -> ()
    | AST.TraitDef trait_def -> List.iter visit_trait_method trait_def.methods
    | AST.ImplDef impl_def -> List.iter visit_impl_method impl_def.impl_methods
    | AST.InherentImplDef impl_def -> List.iter visit_impl_method impl_def.inherent_methods
  in
  List.iter visit_stmt program;
  { next_expr_id = !max_expr_id + 1; next_method_id = !max_method_id + 1 }

let fresh_expr_id (supply : t) : int =
  let fresh = supply.next_expr_id in
  supply.next_expr_id <- fresh + 1;
  fresh

let fresh_method_id (supply : t) : int =
  let fresh = supply.next_method_id in
  supply.next_method_id <- fresh + 1;
  fresh

let%test "create_from_program seeds expr and method ids independently" =
  let default_expr = AST.mk_expr ~id:7 (AST.Integer 1L) in
  let program =
    [
      AST.mk_stmt
        (AST.TraitDef
           {
             name = "Show";
             type_param = Some "a";
             supertraits = [];
             fields = [];
             methods =
               [
                 {
                   method_sig_id = 11;
                   method_name = "show";
                   method_generics = None;
                   method_params = [ ("x", AST.TVar "a") ];
                   method_return_type = AST.TCon "Str";
                   method_effect = AST.Pure;
                   method_default_impl = Some default_expr;
                 };
               ];
           });
      AST.mk_stmt
        (AST.ImplDef
           {
             impl_type_params = [];
             impl_trait_name = "Show";
             impl_for_type = AST.TCon "Int";
             impl_methods =
               [
                 {
                   impl_method_id = 13;
                   impl_method_name = "show";
                   impl_method_generics = None;
                   impl_method_params = [ ("x", Some (AST.TCon "Int")) ];
                   impl_method_return_type = Some (AST.TCon "Str");
                   impl_method_effect = Some AST.Pure;
                   impl_method_override = false;
                   impl_method_body =
                     AST.mk_stmt (AST.ExpressionStmt (AST.mk_expr ~id:9 (AST.String "ok")));
                 };
               ];
           });
    ]
  in
  let supply = create_from_program program in
  fresh_expr_id supply = 10 && fresh_method_id supply = 14

let%test "fresh ids remain monotonic" =
  let supply = { next_expr_id = 3; next_method_id = 5 } in
  fresh_expr_id supply = 3
  && fresh_expr_id supply = 4
  && fresh_method_id supply = 5
  && fresh_method_id supply = 6
