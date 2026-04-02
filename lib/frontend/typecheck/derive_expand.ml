module AST = Syntax.Ast.AST
module Constraints = Constraints
module Diagnostic = Diagnostics.Diagnostic
module Type_registry = Type_registry
module Trait_solver = Trait_solver
module StringSet = Set.Make (String)

let ( let* ) = Result.bind

type trait_summary = {
  name : string;
  type_param : string option;
  supertraits : Constraints.t list;
  methods : AST.method_sig list;
}

let default_derived_impl_store : (string * string, unit) Hashtbl.t = Hashtbl.create 32
let clear_default_derived_impl_store () : unit = Hashtbl.clear default_derived_impl_store

let error_at_stmt ~code ~message (stmt : AST.statement) : Diagnostic.t =
  match stmt.file_id with
  | Some file_id ->
      Diagnostic.error_with_span ~code ~message ~file_id ~start_pos:stmt.pos ~end_pos:stmt.end_pos ()
  | None -> Diagnostic.error_no_span ~code ~message

let collect_free_type_vars_in_order (type_expr : AST.type_expr) : string list =
  let seen = Hashtbl.create 8 in
  let rec go acc = function
    | AST.TVar name ->
        if Hashtbl.mem seen name then
          acc
        else (
          Hashtbl.replace seen name ();
          acc @ [ name ])
    | AST.TCon _ | AST.TTraitObject _ -> acc
    | AST.TApp (_, args) | AST.TUnion args | AST.TIntersection args -> List.fold_left go acc args
    | AST.TArrow (params, ret, _) -> go (List.fold_left go acc params) ret
    | AST.TRecord (fields, row_var) -> (
        let acc' =
          List.fold_left
            (fun inner_acc (field : AST.record_type_field) -> go inner_acc field.field_type)
            acc fields
        in
        match row_var with
        | None -> acc'
        | Some row -> go acc' row)
  in
  go [] type_expr

let type_expr_key_with_binders (binder_names : string list) (type_expr : AST.type_expr) : string =
  let bound_names : (string, string) Hashtbl.t = Hashtbl.create 8 in
  let next_var = ref 0 in
  List.iteri
    (fun idx name ->
      Hashtbl.replace bound_names name (Printf.sprintf "__impl_param_%d" idx);
      next_var := idx + 1)
    binder_names;
  let rec go = function
    | AST.TVar name -> (
        match Hashtbl.find_opt bound_names name with
        | Some canonical_name -> "Var(" ^ canonical_name ^ ")"
        | None ->
            let canonical_name = Printf.sprintf "__impl_param_%d" !next_var in
            incr next_var;
            Hashtbl.replace bound_names name canonical_name;
            "Var(" ^ canonical_name ^ ")")
    | AST.TCon name -> "Con(" ^ name ^ ")"
    | AST.TTraitObject traits -> "Dyn(" ^ String.concat "&" (List.sort_uniq String.compare traits) ^ ")"
    | AST.TApp (name, args) -> "App(" ^ name ^ "[" ^ String.concat "," (List.map go args) ^ "])"
    | AST.TArrow (params, ret, is_effectful) ->
        let arrow =
          if is_effectful then
            "=>"
          else
            "->"
        in
        "Arrow(" ^ String.concat "," (List.map go params) ^ arrow ^ go ret ^ ")"
    | AST.TUnion members ->
        "Union(" ^ String.concat "," (List.sort_uniq String.compare (List.map go members)) ^ ")"
    | AST.TIntersection members ->
        "Intersection(" ^ String.concat "," (List.sort_uniq String.compare (List.map go members)) ^ ")"
    | AST.TRecord (fields, row_var) ->
        let field_keys =
          fields
          |> List.map (fun (field : AST.record_type_field) -> field.field_name ^ ":" ^ go field.field_type)
          |> List.sort String.compare
        in
        let row_key =
          match row_var with
          | None -> ""
          | Some row -> ",..." ^ go row
        in
        "Record(" ^ String.concat "," field_keys ^ row_key ^ ")"
  in
  go type_expr

let type_expr_key (type_expr : AST.type_expr) : string =
  type_expr_key_with_binders (collect_free_type_vars_in_order type_expr) type_expr

let render_type_expr (type_expr : AST.type_expr) : string =
  let rec render_parens = function
    | (AST.TArrow _ | AST.TUnion _) as type_expr -> "(" ^ render type_expr ^ ")"
    | type_expr -> render type_expr
  and render = function
    | AST.TVar name -> name
    | AST.TCon name -> name
    | AST.TTraitObject traits -> "Dyn[" ^ String.concat " & " traits ^ "]"
    | AST.TApp (name, args) -> name ^ "[" ^ String.concat ", " (List.map render args) ^ "]"
    | AST.TArrow (params, ret, is_effectful) ->
        let arrow =
          if is_effectful then
            " => "
          else
            " -> "
        in
        let params =
          match params with
          | [ single ] -> render_parens single
          | many -> "(" ^ String.concat ", " (List.map render many) ^ ")"
        in
        params ^ arrow ^ render ret
    | AST.TUnion members -> String.concat " | " (List.map render members)
    | AST.TIntersection members -> String.concat " & " (List.map render_parens members)
    | AST.TRecord (fields, row_var) ->
        let field_parts =
          List.map
            (fun (field : AST.record_type_field) -> field.field_name ^ ": " ^ render field.field_type)
            fields
        in
        let row_part =
          match row_var with
          | None -> ""
          | Some row when field_parts = [] -> "..." ^ render row
          | Some row -> ", ..." ^ render row
        in
        "{ " ^ String.concat ", " field_parts ^ row_part ^ " }"
  in
  render type_expr

let derived_impl_key ~(trait_name : string) ~(for_type : AST.type_expr) : string * string =
  (Trait_registry.canonical_trait_name trait_name, type_expr_key for_type)

let is_default_derived_impl ~(trait_name : string) ~(for_type : AST.type_expr) : bool =
  Hashtbl.mem default_derived_impl_store (derived_impl_key ~trait_name ~for_type)

let record_default_derived_impl ~(trait_name : string) ~(for_type : AST.type_expr) : unit =
  Hashtbl.replace default_derived_impl_store (derived_impl_key ~trait_name ~for_type) ()

let register_program_trait (traits : (string, trait_summary) Hashtbl.t) (trait_def : AST.trait_def) : unit =
  let key = Trait_registry.canonical_trait_name trait_def.name in
  Hashtbl.replace traits key
    {
      name = trait_def.name;
      type_param = trait_def.type_param;
      supertraits = Constraints.of_names trait_def.supertraits;
      methods = trait_def.methods;
    }

let impl_key_of_explicit_impl (impl_def : AST.impl_def) : string * string =
  let binder_names = List.map (fun (p : AST.generic_param) -> p.name) impl_def.impl_type_params in
  ( Trait_registry.canonical_trait_name impl_def.impl_trait_name,
    type_expr_key_with_binders binder_names impl_def.impl_for_type )

let pre_scan_program (program : AST.program) :
    (string, trait_summary) Hashtbl.t * (string * string, unit) Hashtbl.t =
  let traits : (string, trait_summary) Hashtbl.t = Hashtbl.create 32 in
  let explicit_impls : (string * string, unit) Hashtbl.t = Hashtbl.create 32 in
  List.iter
    (fun (stmt : AST.statement) ->
      match stmt.stmt with
      | AST.TraitDef trait_def -> register_program_trait traits trait_def
      | AST.TypeDef type_def -> Type_registry.predeclare_named_type type_def
      | AST.TypeAlias alias_def -> Annotation.register_type_alias alias_def
      | AST.ShapeDef shape_def -> Type_registry.predeclare_shape shape_def
      | AST.ImplDef impl_def -> Hashtbl.replace explicit_impls (impl_key_of_explicit_impl impl_def) ()
      | _ -> ())
    program;
  (traits, explicit_impls)

let global_impl_exists (trait_name : string) (for_type : AST.type_expr) : bool =
  match Annotation.type_expr_to_mono_type for_type with
  | Ok mono_type -> Trait_registry.implements_trait trait_name mono_type
  | Error _ -> false

let rec substitute_type_expr_vars (subst : (string * AST.type_expr) list) (type_expr : AST.type_expr) :
    AST.type_expr =
  match type_expr with
  | AST.TVar name -> (
      match List.assoc_opt name subst with
      | Some replacement -> replacement
      | None -> type_expr)
  | AST.TCon _ | AST.TTraitObject _ -> type_expr
  | AST.TApp (name, args) -> AST.TApp (name, List.map (substitute_type_expr_vars subst) args)
  | AST.TArrow (params, ret, is_effectful) ->
      AST.TArrow
        (List.map (substitute_type_expr_vars subst) params, substitute_type_expr_vars subst ret, is_effectful)
  | AST.TUnion members -> AST.TUnion (List.map (substitute_type_expr_vars subst) members)
  | AST.TIntersection members -> AST.TIntersection (List.map (substitute_type_expr_vars subst) members)
  | AST.TRecord (fields, row_var) ->
      AST.TRecord
        ( List.map
            (fun (field : AST.record_type_field) ->
              { field with field_type = substitute_type_expr_vars subst field.field_type })
            fields,
          Option.map (substitute_type_expr_vars subst) row_var )

let source_record_fields (type_expr : AST.type_expr) : AST.record_type_field list option =
  let product_fields_of_named_type (type_def : AST.named_type_def) (args : AST.type_expr list) :
      AST.record_type_field list option =
    match type_def.type_body with
    | AST.NamedTypeWrapper _ -> None
    | AST.NamedTypeProduct fields ->
        if List.length type_def.type_type_params <> List.length args then
          None
        else
          let subst = List.combine type_def.type_type_params args in
          Some
            (List.map
               (fun (field : AST.record_type_field) ->
                 { field with field_type = substitute_type_expr_vars subst field.field_type })
               fields)
  in
  match type_expr with
  | AST.TRecord (fields, _row_var) -> Some fields
  | AST.TCon name -> (
      match Type_registry.lookup_named_type_source name with
      | Some type_def -> product_fields_of_named_type type_def []
      | None -> None)
  | AST.TApp (name, args) -> (
      match Type_registry.lookup_named_type_source name with
      | Some type_def -> product_fields_of_named_type type_def args
      | None -> None)
  | _ -> None

let ast_fields_to_mono (fields : AST.record_type_field list) : Types.record_field_type list option =
  let rec go acc = function
    | [] -> Some (List.rev acc)
    | (field : AST.record_type_field) :: rest -> (
        match Annotation.type_expr_to_mono_type field.field_type with
        | Error _ -> None
        | Ok typ -> go ({ Types.name = field.field_name; typ } :: acc) rest)
  in
  go [] fields

let fallback_shape_fields (shape_name : string) : Types.record_field_type list option =
  match Type_registry.lookup_shape_source shape_name with
  | Some shape_def when shape_def.shape_type_params = [] -> ast_fields_to_mono shape_def.shape_fields
  | Some _ -> None
  | None -> (
      match Type_registry.lookup_shape shape_name with
      | Some shape_def when shape_def.shape_type_params = [] -> Some shape_def.shape_fields
      | Some _ -> None
      | None -> None)

let fallback_actual_fields (for_type : AST.type_expr) : Types.record_field_type list option =
  match source_record_fields for_type with
  | Some actual_fields -> ast_fields_to_mono actual_fields
  | None -> (
      match Annotation.type_expr_to_mono_type for_type with
      | Ok mono_type -> Structural.fields_of_type mono_type
      | Error _ -> None)

let source_level_shape_satisfied (shape_name : string) (for_type : AST.type_expr) : bool =
  match (fallback_shape_fields shape_name, fallback_actual_fields for_type) with
  | Some required_fields, Some actual_fields ->
      List.for_all
        (fun (required : Types.record_field_type) ->
          match
            List.find_opt (fun (field : Types.record_field_type) -> field.name = required.name) actual_fields
          with
          | None -> false
          | Some actual -> Structural.field_types_compatible actual.typ required.typ)
        required_fields
  | _ -> false

let superconstraint_satisfied (constraint_ref : Constraints.t) (for_type : AST.type_expr) : bool =
  match Annotation.type_expr_to_mono_type for_type with
  | Error _ -> false
  | Ok mono_type -> (
      match constraint_ref with
      | Constraints.ShapeConstraint shape_name ->
          Result.is_ok (Trait_solver.check_constraint_ref mono_type constraint_ref)
          || source_level_shape_satisfied shape_name for_type
      | Constraints.TraitConstraint trait_name -> Trait_registry.implements_trait trait_name mono_type)

let validate_target_type_params
    ~(stmt : AST.statement) ~(derive_trait : AST.derive_trait) ~(free_type_vars : string list) :
    (AST.generic_param list, Diagnostic.t) result =
  match derive_trait.derive_trait_constraints with
  | [] -> Ok (List.map (fun name -> AST.{ name; constraints = [] }) free_type_vars)
  | constraints ->
      let expected = List.sort String.compare free_type_vars in
      let actual = constraints |> List.map (fun (p : AST.generic_param) -> p.name) |> List.sort String.compare in
      if expected = actual then
        Ok constraints
      else
        Error
          (error_at_stmt ~code:"derive-invalid-target-params"
             ~message:
               (Printf.sprintf "Derive for %s specifies generic parameters [%s], but target type uses [%s]"
                  derive_trait.derive_trait_name (String.concat ", " actual) (String.concat ", " expected))
             stmt)

let substitute_type_expr
    ~(trait_subst_name : string option)
    ~(target_type : AST.type_expr)
    ~(bound_names : StringSet.t)
    (type_expr : AST.type_expr) : AST.type_expr =
  let rec go bound = function
    | AST.TVar name -> (
        match trait_subst_name with
        | Some subst_name when name = subst_name && not (StringSet.mem name bound) -> target_type
        | _ -> AST.TVar name)
    | AST.TCon _ as type_expr -> type_expr
    | AST.TTraitObject _ as type_expr -> type_expr
    | AST.TApp (name, args) -> AST.TApp (name, List.map (go bound) args)
    | AST.TArrow (params, ret, is_effectful) -> AST.TArrow (List.map (go bound) params, go bound ret, is_effectful)
    | AST.TUnion members -> AST.TUnion (List.map (go bound) members)
    | AST.TIntersection members -> AST.TIntersection (List.map (go bound) members)
    | AST.TRecord (fields, row_var) ->
        AST.TRecord
          ( List.map
              (fun (field : AST.record_type_field) -> AST.{ field with field_type = go bound field.field_type })
              fields,
            Option.map (go bound) row_var )
  in
  go bound_names type_expr

let clone_default_body
    ~(trait_subst_name : string option)
    ~(target_type : AST.type_expr)
    ~(method_generics : AST.generic_param list)
    (supply : Synthetic_ids.t)
    (default_expr : AST.expression) : AST.statement =
  let method_bound_names =
    method_generics |> List.map (fun (gp : AST.generic_param) -> gp.name) |> StringSet.of_list
  in
  let substitute_type = substitute_type_expr ~trait_subst_name ~target_type ~bound_names:method_bound_names in
  let rec clone_expr bound_names (expr : AST.expression) : AST.expression =
    let clone_expr = clone_expr bound_names in
    let expr_kind =
      match expr.expr with
      | (AST.Identifier _ | AST.Integer _ | AST.Float _ | AST.Boolean _ | AST.String _) as literal -> literal
      | AST.Array items -> AST.Array (List.map clone_expr items)
      | AST.Index (container, index) -> AST.Index (clone_expr container, clone_expr index)
      | AST.TypeApply (callee, type_args) -> AST.TypeApply (clone_expr callee, List.map substitute_type type_args)
      | AST.Hash pairs -> AST.Hash (List.map (fun (key, value) -> (clone_expr key, clone_expr value)) pairs)
      | AST.Prefix (op, operand) -> AST.Prefix (op, clone_expr operand)
      | AST.Infix (left, op, right) -> AST.Infix (clone_expr left, op, clone_expr right)
      | AST.TypeCheck (subject, type_expr) -> AST.TypeCheck (clone_expr subject, substitute_type type_expr)
      | AST.If (condition, consequence, alternative) ->
          AST.If
            ( clone_expr condition,
              clone_stmt bound_names consequence,
              Option.map (clone_stmt bound_names) alternative )
      | AST.Function { origin; generics; params; return_type; is_effectful; body } ->
          let generic_names =
            match generics with
            | None -> []
            | Some gs -> List.map (fun (gp : AST.generic_param) -> gp.name) gs
          in
          let bound_names' = List.fold_left (fun acc name -> StringSet.add name acc) bound_names generic_names in
          AST.Function
            {
              origin;
              generics;
              params =
                List.map
                  (fun (name, typ) ->
                    ( name,
                      Option.map
                        (substitute_type_expr ~trait_subst_name ~target_type ~bound_names:bound_names')
                        typ ))
                  params;
              return_type =
                Option.map
                  (substitute_type_expr ~trait_subst_name ~target_type ~bound_names:bound_names')
                  return_type;
              is_effectful;
              body = clone_stmt bound_names' body;
            }
      | AST.Call (callee, args) -> AST.Call (clone_expr callee, List.map clone_expr args)
      | AST.EnumConstructor (enum_name, variant_name, args) ->
          AST.EnumConstructor (enum_name, variant_name, List.map clone_expr args)
      | AST.Match (scrutinee, arms) ->
          AST.Match
            ( clone_expr scrutinee,
              List.map (fun (arm : AST.match_arm) -> AST.{ arm with body = clone_expr arm.body }) arms )
      | AST.RecordLit (fields, spread) ->
          AST.RecordLit
            ( List.map
                (fun (field : AST.record_field) ->
                  AST.{ field with field_value = Option.map clone_expr field.field_value })
                fields,
              Option.map clone_expr spread )
      | AST.FieldAccess (receiver, field_name) -> AST.FieldAccess (clone_expr receiver, field_name)
      | AST.MethodCall { mc_receiver; mc_method; mc_type_args; mc_args } ->
          AST.MethodCall
            {
              mc_receiver = clone_expr mc_receiver;
              mc_method;
              mc_type_args = Option.map (List.map substitute_type) mc_type_args;
              mc_args = List.map clone_expr mc_args;
            }
      | AST.BlockExpr stmts -> AST.BlockExpr (List.map (clone_stmt bound_names) stmts)
    in
    AST.mk_expr ~id:(Synthetic_ids.fresh_expr_id supply) ~pos:expr.pos ~end_pos:expr.end_pos ~file_id:expr.file_id
      expr_kind
  and clone_stmt bound_names (stmt : AST.statement) : AST.statement =
    let stmt_kind =
      match stmt.stmt with
      | AST.Let let_binding ->
          AST.Let
            {
              name = let_binding.name;
              value = clone_expr bound_names let_binding.value;
              type_annotation = Option.map substitute_type let_binding.type_annotation;
            }
      | AST.Return expr -> AST.Return (clone_expr bound_names expr)
      | AST.ExpressionStmt expr -> AST.ExpressionStmt (clone_expr bound_names expr)
      | AST.Block stmts -> AST.Block (List.map (clone_stmt bound_names) stmts)
      | AST.EnumDef _ | AST.TypeDef _ | AST.ShapeDef _ | AST.TraitDef _ | AST.ImplDef _ | AST.InherentImplDef _
      | AST.DeriveDef _ | AST.TypeAlias _ ->
          stmt.stmt
    in
    AST.mk_stmt ~pos:stmt.pos ~end_pos:stmt.end_pos ~file_id:stmt.file_id stmt_kind
  in
  let cloned_expr = clone_expr method_bound_names default_expr in
  match cloned_expr.expr with
  | AST.BlockExpr stmts ->
      AST.mk_stmt ~pos:default_expr.pos ~end_pos:default_expr.end_pos ~file_id:default_expr.file_id
        (AST.Block stmts)
  | _ ->
      AST.mk_stmt ~pos:default_expr.pos ~end_pos:default_expr.end_pos ~file_id:default_expr.file_id
        (AST.Block
           [
             AST.mk_stmt ~pos:cloned_expr.pos ~end_pos:cloned_expr.end_pos ~file_id:cloned_expr.file_id
               (AST.ExpressionStmt cloned_expr);
           ])

let synthesize_user_impl
    ~(stmt : AST.statement)
    ~(supply : Synthetic_ids.t)
    ~(trait_summary : trait_summary)
    ~(derive_trait : AST.derive_trait)
    ~(for_type : AST.type_expr) : (AST.statement, Diagnostic.t) result =
  let free_type_vars = collect_free_type_vars_in_order for_type in
  let* impl_type_params = validate_target_type_params ~stmt ~derive_trait ~free_type_vars in
  let methods =
    List.map
      (fun (method_sig : AST.method_sig) ->
        let method_generics = Option.value method_sig.method_generics ~default:[] in
        let method_bound_names =
          method_generics |> List.map (fun (gp : AST.generic_param) -> gp.name) |> StringSet.of_list
        in
        let substitute_type =
          substitute_type_expr ~trait_subst_name:trait_summary.type_param ~target_type:for_type
            ~bound_names:method_bound_names
        in
        let default_expr =
          match method_sig.method_default_impl with
          | Some expr -> expr
          | None -> failwith "impossible: synthesize_user_impl called for required method"
        in
        {
          AST.impl_method_id = Synthetic_ids.fresh_method_id supply;
          impl_method_name = method_sig.method_name;
          impl_method_generics = method_sig.method_generics;
          impl_method_params =
            List.map (fun (name, typ) -> (name, Some (substitute_type typ))) method_sig.method_params;
          impl_method_return_type = Some (substitute_type method_sig.method_return_type);
          impl_method_effect = Some method_sig.method_effect;
          impl_method_override = false;
          impl_method_body =
            clone_default_body ~trait_subst_name:trait_summary.type_param ~target_type:for_type ~method_generics
              supply default_expr;
        })
      trait_summary.methods
  in
  record_default_derived_impl ~trait_name:trait_summary.name ~for_type;
  Ok
    (AST.mk_stmt ~pos:stmt.pos ~end_pos:stmt.end_pos ~file_id:stmt.file_id
       (AST.ImplDef
          {
            impl_type_params;
            impl_trait_name = trait_summary.name;
            impl_for_type = for_type;
            impl_methods = methods;
          }))

let ensure_user_trait_derivable
    ~(traits : (string, trait_summary) Hashtbl.t) ~(stmt : AST.statement) ~(trait_name : string) :
    (trait_summary, Diagnostic.t) result =
  let canonical_name = Trait_registry.canonical_trait_name trait_name in
  match Hashtbl.find_opt traits canonical_name with
  | None ->
      Error
        (error_at_stmt ~code:"derive-undefined-trait"
           ~message:(Printf.sprintf "Cannot derive undefined trait: %s" trait_name)
           stmt)
  | Some summary ->
      let required_methods =
        summary.methods
        |> List.filter (fun (method_sig : AST.method_sig) -> method_sig.method_default_impl = None)
      in
      if required_methods <> [] then
        let method_names =
          required_methods
          |> List.map (fun (method_sig : AST.method_sig) -> method_sig.method_name)
          |> String.concat ", "
        in
        Error
          (error_at_stmt ~code:"derive-required-method"
             ~message:
               (Printf.sprintf "Trait '%s' cannot be derived because these methods have no default body: %s"
                  summary.name method_names)
             stmt)
      else
        Ok summary

let expand_one_derive
    ~(traits : (string, trait_summary) Hashtbl.t)
    ~(explicit_impls : (string * string, unit) Hashtbl.t)
    ~(planned_impls : (string * string, unit) Hashtbl.t)
    ~(supply : Synthetic_ids.t)
    (stmt : AST.statement)
    (derive_def : AST.derive_def) : (AST.statement list, Diagnostic.t) result =
  let target_display = render_type_expr derive_def.derive_for_type in
  let seen_traits : (string, unit) Hashtbl.t = Hashtbl.create 8 in
  let rec reject_duplicate = function
    | [] -> Ok ()
    | (trait_ref : AST.derive_trait) :: rest ->
        let canonical_name = Trait_registry.canonical_trait_name trait_ref.derive_trait_name in
        if Hashtbl.mem seen_traits canonical_name then
          Error
            (error_at_stmt ~code:"derive-duplicate-impl"
               ~message:
                 (Printf.sprintf "Trait '%s' already has an explicit or derived impl for %s"
                    trait_ref.derive_trait_name target_display)
               stmt)
        else (
          Hashtbl.replace seen_traits canonical_name ();
          reject_duplicate rest)
  in
  let* () = reject_duplicate derive_def.derive_traits in
  let builtin_traits, user_traits =
    List.partition
      (fun (trait_ref : AST.derive_trait) ->
        Trait_registry.derive_kind_of_trait_name trait_ref.derive_trait_name <> None)
      derive_def.derive_traits
  in
  if user_traits = [] then
    Ok [ stmt ]
  else
    let target_key = type_expr_key derive_def.derive_for_type in
    let requested_user_trait_names =
      user_traits
      |> List.map (fun (trait_ref : AST.derive_trait) ->
             Trait_registry.canonical_trait_name trait_ref.derive_trait_name)
      |> StringSet.of_list
    in
    let requested_builtin_trait_names =
      builtin_traits
      |> List.map (fun (trait_ref : AST.derive_trait) ->
             Trait_registry.canonical_trait_name trait_ref.derive_trait_name)
      |> StringSet.of_list
    in
    let requested_user_traits : (string, AST.derive_trait) Hashtbl.t = Hashtbl.create 8 in
    List.iter
      (fun (trait_ref : AST.derive_trait) ->
        Hashtbl.replace requested_user_traits
          (Trait_registry.canonical_trait_name trait_ref.derive_trait_name)
          trait_ref)
      user_traits;
    let impl_exists trait_name =
      let key = (Trait_registry.canonical_trait_name trait_name, target_key) in
      Hashtbl.mem explicit_impls key
      || Hashtbl.mem planned_impls key
      || global_impl_exists trait_name derive_def.derive_for_type
    in
    let rec visit ordered visiting visited trait_name =
      if StringSet.mem trait_name visited then
        Ok (ordered, visited)
      else if StringSet.mem trait_name visiting then
        Error
          (error_at_stmt ~code:"derive-cycle"
             ~message:(Printf.sprintf "Derive request for %s has a cyclic supertrait dependency" trait_name)
             stmt)
      else
        let* summary = ensure_user_trait_derivable ~traits ~stmt ~trait_name in
        let visiting' = StringSet.add trait_name visiting in
        let* ordered', visited' =
          List.fold_left
            (fun acc superconstraint ->
              let* ordered_acc, visited_acc = acc in
              match superconstraint with
              | Constraints.TraitConstraint supertrait ->
                  if StringSet.mem supertrait requested_user_trait_names then
                    visit ordered_acc visiting' visited_acc supertrait
                  else if StringSet.mem supertrait requested_builtin_trait_names || impl_exists supertrait then
                    Ok (ordered_acc, visited_acc)
                  else
                    Error
                      (error_at_stmt ~code:"derive-missing-supertrait"
                         ~message:
                           (Printf.sprintf
                              "Trait '%s' cannot be derived for %s because required supertrait '%s' is not implemented and is not requested in the same derive clause"
                              summary.name target_display supertrait)
                         stmt)
              | Constraints.ShapeConstraint shape_name ->
                  if superconstraint_satisfied superconstraint derive_def.derive_for_type then
                    Ok (ordered_acc, visited_acc)
                  else
                    Error
                      (error_at_stmt ~code:"derive-missing-supertrait"
                         ~message:
                           (Printf.sprintf
                              "Trait '%s' cannot be derived for %s because required shape superconstraint '%s' is not satisfied by the target type"
                              summary.name target_display shape_name)
                         stmt))
            (Ok (ordered, visited))
            summary.supertraits
        in
        Ok (trait_name :: ordered', StringSet.add trait_name visited')
    in
    let* ordered_user_trait_names, _visited =
      StringSet.fold
        (fun trait_name acc ->
          let* ordered, visited = acc in
          visit ordered StringSet.empty visited trait_name)
        requested_user_trait_names
        (Ok ([], StringSet.empty))
    in
    let ordered_user_trait_names = List.rev ordered_user_trait_names in
    let* generated_impls =
      List.fold_left
        (fun acc trait_name ->
          let* generated = acc in
          let derive_trait =
            match Hashtbl.find_opt requested_user_traits trait_name with
            | Some trait_ref -> trait_ref
            | None -> failwith "impossible: requested user trait disappeared during derive planning"
          in
          let key = (trait_name, target_key) in
          if Hashtbl.mem explicit_impls key || Hashtbl.mem planned_impls key then
            Error
              (error_at_stmt ~code:"derive-duplicate-impl"
                 ~message:
                   (Printf.sprintf "Trait '%s' already has an explicit or derived impl for %s"
                      derive_trait.derive_trait_name target_display)
                 stmt)
          else
            let* trait_summary = ensure_user_trait_derivable ~traits ~stmt ~trait_name in
            Hashtbl.replace planned_impls key ();
            let* generated_impl =
              synthesize_user_impl ~stmt ~supply ~trait_summary ~derive_trait ~for_type:derive_def.derive_for_type
            in
            Ok (generated @ [ generated_impl ]))
        (Ok []) ordered_user_trait_names
    in
    let residual_builtin_stmt =
      match builtin_traits with
      | [] -> []
      | _ ->
          [
            AST.mk_stmt ~pos:stmt.pos ~end_pos:stmt.end_pos ~file_id:stmt.file_id
              (AST.DeriveDef { derive_traits = builtin_traits; derive_for_type = derive_def.derive_for_type });
          ]
    in
    Ok (residual_builtin_stmt @ generated_impls)

let expand_user_derives (program : AST.program) : (AST.program, Diagnostic.t) result =
  clear_default_derived_impl_store ();
  let traits, explicit_impls = pre_scan_program program in
  let planned_impls : (string * string, unit) Hashtbl.t = Hashtbl.create 32 in
  let supply = Synthetic_ids.create_from_program program in
  let rec go rev_stmts = function
    | [] -> Ok (List.rev rev_stmts)
    | (stmt : AST.statement) :: rest -> (
        match stmt.stmt with
        | AST.DeriveDef derive_def -> (
            match expand_one_derive ~traits ~explicit_impls ~planned_impls ~supply stmt derive_def with
            | Error _ as error_result -> error_result
            | Ok rewritten -> go (List.rev_append rewritten rev_stmts) rest)
        | _ -> go (stmt :: rev_stmts) rest)
  in
  go [] program

let%test "expand_user_derives preserves builtin-only derive" =
  clear_default_derived_impl_store ();
  let stmt =
    AST.mk_stmt
      (AST.DeriveDef
         {
           derive_traits = [ AST.{ derive_trait_name = "Show"; derive_trait_constraints = [] } ];
           derive_for_type = AST.TCon "Point";
         })
  in
  match expand_user_derives [ stmt ] with
  | Ok [ { AST.stmt = AST.DeriveDef _; _ } ] -> true
  | _ -> false

let%test "expand_user_derives rewrites user derive into synthetic impl" =
  clear_default_derived_impl_store ();
  let trait_stmt =
    AST.mk_stmt
      (AST.TraitDef
         {
           name = "Printable";
           type_param = Some "a";
           supertraits = [];
           methods =
             [
               {
                 method_sig_id = 10;
                 method_name = "render";
                 method_generics = None;
                 method_params = [ ("self", AST.TVar "a") ];
                 method_return_type = AST.TCon "Str";
                 method_effect = AST.Pure;
                 method_default_impl = Some (AST.mk_expr ~id:21 (AST.String "ok"));
               };
             ];
         })
  in
  let derive_stmt =
    AST.mk_stmt
      (AST.DeriveDef
         {
           derive_traits = [ AST.{ derive_trait_name = "Printable"; derive_trait_constraints = [] } ];
           derive_for_type = AST.TCon "Point";
         })
  in
  match expand_user_derives [ trait_stmt; derive_stmt ] with
  | Ok
      [
        _;
        {
          AST.stmt =
            AST.ImplDef { impl_trait_name; impl_for_type = AST.TCon "Point"; impl_methods = [ method_impl ]; _ };
          _;
        };
      ] ->
      impl_trait_name = "Printable"
      && method_impl.impl_method_return_type = Some (AST.TCon "Str")
      && is_default_derived_impl ~trait_name:"Printable" ~for_type:(AST.TCon "Point")
  | _ -> false

let%test "expand_user_derives keeps builtin residual before synthetic impl" =
  clear_default_derived_impl_store ();
  let trait_stmt =
    AST.mk_stmt
      (AST.TraitDef
         {
           name = "Printable";
           type_param = Some "a";
           supertraits = [];
           methods =
             [
               {
                 method_sig_id = 11;
                 method_name = "render";
                 method_generics = None;
                 method_params = [ ("self", AST.TVar "a") ];
                 method_return_type = AST.TCon "Str";
                 method_effect = AST.Pure;
                 method_default_impl = Some (AST.mk_expr ~id:22 (AST.String "ok"));
               };
             ];
         })
  in
  let derive_stmt =
    AST.mk_stmt
      (AST.DeriveDef
         {
           derive_traits =
             [
               AST.{ derive_trait_name = "Show"; derive_trait_constraints = [] };
               AST.{ derive_trait_name = "Printable"; derive_trait_constraints = [] };
             ];
           derive_for_type = AST.TCon "Point";
         })
  in
  match expand_user_derives [ trait_stmt; derive_stmt ] with
  | Ok
      [
        _;
        { AST.stmt = AST.DeriveDef { derive_traits = [ { derive_trait_name = "Show"; _ } ]; _ }; _ };
        { AST.stmt = AST.ImplDef _; _ };
      ] ->
      true
  | _ -> false

let%test "expand_user_derives rejects required-method traits" =
  clear_default_derived_impl_store ();
  let trait_stmt =
    AST.mk_stmt
      (AST.TraitDef
         {
           name = "Needful";
           type_param = Some "a";
           supertraits = [];
           methods =
             [
               {
                 method_sig_id = 12;
                 method_name = "need";
                 method_generics = None;
                 method_params = [ ("self", AST.TVar "a") ];
                 method_return_type = AST.TCon "Str";
                 method_effect = AST.Pure;
                 method_default_impl = None;
               };
             ];
         })
  in
  let derive_stmt =
    AST.mk_stmt
      (AST.DeriveDef
         {
           derive_traits = [ AST.{ derive_trait_name = "Needful"; derive_trait_constraints = [] } ];
           derive_for_type = AST.TCon "Point";
         })
  in
  match expand_user_derives [ trait_stmt; derive_stmt ] with
  | Error diag -> diag.code = "derive-required-method"
  | Ok _ -> false

let%test "expand_user_derives orders supertraits before dependent user traits" =
  clear_default_derived_impl_store ();
  let base_trait =
    AST.mk_stmt
      (AST.TraitDef
         {
           name = "Base";
           type_param = Some "a";
           supertraits = [];
           methods =
             [
               {
                 method_sig_id = 13;
                 method_name = "base";
                 method_generics = None;
                 method_params = [ ("self", AST.TVar "a") ];
                 method_return_type = AST.TCon "Str";
                 method_effect = AST.Pure;
                 method_default_impl = Some (AST.mk_expr ~id:30 (AST.String "base"));
               };
             ];
         })
  in
  let child_trait =
    AST.mk_stmt
      (AST.TraitDef
         {
           name = "Child";
           type_param = Some "a";
           supertraits = [ "Base" ];
           methods =
             [
               {
                 method_sig_id = 14;
                 method_name = "child";
                 method_generics = None;
                 method_params = [ ("self", AST.TVar "a") ];
                 method_return_type = AST.TCon "Str";
                 method_effect = AST.Pure;
                 method_default_impl = Some (AST.mk_expr ~id:31 (AST.String "child"));
               };
             ];
         })
  in
  let derive_stmt =
    AST.mk_stmt
      (AST.DeriveDef
         {
           derive_traits =
             [
               AST.{ derive_trait_name = "Child"; derive_trait_constraints = [] };
               AST.{ derive_trait_name = "Base"; derive_trait_constraints = [] };
             ];
           derive_for_type = AST.TCon "Point";
         })
  in
  match expand_user_derives [ base_trait; child_trait; derive_stmt ] with
  | Ok
      [
        _;
        _;
        { AST.stmt = AST.ImplDef { impl_trait_name = "Base"; _ }; _ };
        { AST.stmt = AST.ImplDef { impl_trait_name = "Child"; _ }; _ };
      ] ->
      true
  | _ -> false

let%test "expand_user_derives rejects duplicate traits in one derive clause" =
  clear_default_derived_impl_store ();
  let trait_stmt =
    AST.mk_stmt
      (AST.TraitDef
         {
           name = "Printable";
           type_param = Some "a";
           supertraits = [];
           methods =
             [
               {
                 method_sig_id = 15;
                 method_name = "render";
                 method_generics = None;
                 method_params = [ ("self", AST.TVar "a") ];
                 method_return_type = AST.TCon "Str";
                 method_effect = AST.Pure;
                 method_default_impl = Some (AST.mk_expr ~id:32 (AST.String "ok"));
               };
             ];
         })
  in
  let derive_stmt =
    AST.mk_stmt
      (AST.DeriveDef
         {
           derive_traits =
             [
               AST.{ derive_trait_name = "Printable"; derive_trait_constraints = [] };
               AST.{ derive_trait_name = "Printable"; derive_trait_constraints = [] };
             ];
           derive_for_type = AST.TCon "Point";
         })
  in
  match expand_user_derives [ trait_stmt; derive_stmt ] with
  | Error diag -> diag.code = "derive-duplicate-impl"
  | Ok _ -> false

let%test "expand_user_derives accepts shape superconstraints satisfied by transparent type targets" =
  clear_default_derived_impl_store ();
  Annotation.clear_type_aliases ();
  Type_registry.clear ();
  let shape_stmt =
    AST.mk_stmt
      (AST.ShapeDef
         {
           shape_name = "Named";
           shape_type_params = [];
           shape_fields = [ { AST.field_name = "name"; field_type = AST.TCon "Str" } ];
         })
  in
  let trait_stmt =
    AST.mk_stmt
      (AST.TraitDef
         {
           name = "Greeter";
           type_param = Some "a";
           supertraits = [ "Named" ];
           methods =
             [
               {
                 method_sig_id = 16;
                 method_name = "greet";
                 method_generics = None;
                 method_params = [ ("self", AST.TVar "a") ];
                 method_return_type = AST.TCon "Str";
                 method_effect = AST.Pure;
                 method_default_impl =
                   Some
                     (AST.mk_expr ~id:33 (AST.FieldAccess (AST.mk_expr ~id:34 (AST.Identifier "self"), "name")));
               };
             ];
         })
  in
  let type_stmt =
    AST.mk_stmt
      (AST.TypeAlias
         {
           alias_name = "User";
           alias_type_params = [];
           alias_body = AST.TRecord ([ { field_name = "name"; field_type = AST.TCon "Str" } ], None);
         })
  in
  let derive_stmt =
    AST.mk_stmt
      (AST.DeriveDef
         {
           derive_traits = [ AST.{ derive_trait_name = "Greeter"; derive_trait_constraints = [] } ];
           derive_for_type = AST.TCon "User";
         })
  in
  match expand_user_derives [ shape_stmt; trait_stmt; type_stmt; derive_stmt ] with
  | Ok [ _; _; _; { AST.stmt = AST.ImplDef { impl_trait_name = "Greeter"; _ }; _ } ] -> true
  | _ -> false
