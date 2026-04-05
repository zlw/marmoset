module Ast = Marmoset.Lib.Ast
module Parser = Marmoset.Lib.Parser
module Surface = Marmoset.Lib.Surface_ast.Surface

type binding_kind =
  | Value_binding
  | Type_binding

type scope_binding = {
  binding_kind : binding_kind;
  binding_name : string;
  binding_ref : Surface.name_ref;
  scope_start : int;
  scope_end : int;
}

type scope_index = scope_binding list

type declaration_kind =
  | Import_alias_decl
  | Let_decl
  | Fn_decl
  | Type_decl
  | Shape_decl
  | Trait_decl
  | Trait_method_decl
  | Inherent_method_decl
  | Variant_decl
  | Generic_decl

type reference =
  | Import_alias of {
      alias_ref : Surface.name_ref;
      import_path : string list;
    }
  | Import_path_segment of {
      segment_ref : Surface.name_ref;
      import_path : string list;
      segment_index : int;
    }
  | Declaration_head of {
      name_ref : Surface.name_ref;
      declaration_kind : declaration_kind;
    }
  | Value_identifier of {
      name_ref : Surface.name_ref;
      expr_id : int;
      binding : scope_binding option;
    }
  | Type_identifier of {
      name_ref : Surface.name_ref;
      binding : scope_binding option;
    }
  | Constraint_identifier of {
      name_ref : Surface.name_ref;
    }
  | Qualified_root of {
      root_ref : Surface.name_ref;
      root_expr_id : int option;
      member_ref : Surface.name_ref;
      access_expr_id : int;
    }
  | Qualified_member of {
      root_ref : Surface.name_ref;
      root_expr_id : int option;
      member_ref : Surface.name_ref;
      access_expr_id : int;
    }

type cursor_context_input = {
  surface_program : Surface.surface_program;
  lowered_program : Ast.AST.program;
  scope_index : scope_index;
}

let name_ref_contains ~(offset : int) (ref_ : Surface.name_ref) : bool = offset >= ref_.pos && offset <= ref_.end_pos

let binding_of_name ~(scope_index : scope_index) ~(binding_kind : binding_kind) ~(name : string) ~(offset : int) :
    scope_binding option =
  match
    scope_index
    |> List.filter (fun (binding : scope_binding) ->
           binding.binding_kind = binding_kind
           && String.equal binding.binding_name name
           && offset >= binding.scope_start
           && offset <= binding.scope_end)
    |> List.sort (fun left right ->
           compare (left.scope_end - left.scope_start) (right.scope_end - right.scope_start))
  with
  | binding :: _ -> Some binding
  | [] -> None

let add_binding
    (acc : scope_index)
    ~(binding_kind : binding_kind)
    ~(binding_name : string)
    ~(binding_ref : Surface.name_ref)
    ~(scope_start : int)
    ~(scope_end : int) : scope_index =
  { binding_kind; binding_name; binding_ref; scope_start; scope_end } :: acc

let add_value_param_bindings acc ~scope_start ~scope_end (params : Surface.surface_value_param list) =
  List.fold_left
    (fun acc (param : Surface.surface_value_param) ->
      add_binding acc ~binding_kind:Value_binding ~binding_name:param.svp_name ~binding_ref:param.svp_name_ref
        ~scope_start ~scope_end)
    acc params

let add_typed_param_bindings acc ~scope_start ~scope_end (params : Surface.surface_typed_param list) =
  List.fold_left
    (fun acc (param : Surface.surface_typed_param) ->
      add_binding acc ~binding_kind:Value_binding ~binding_name:param.stp_name ~binding_ref:param.stp_name_ref
        ~scope_start ~scope_end)
    acc params

let add_generic_param_bindings acc ~scope_start ~scope_end (params : Surface.surface_generic_param list) =
  List.fold_left
    (fun acc (param : Surface.surface_generic_param) ->
      add_binding acc ~binding_kind:Type_binding ~binding_name:param.sg_name ~binding_ref:param.sg_name_ref
        ~scope_start ~scope_end)
    acc params

let rec build_scope_index_expr (acc : scope_index) (expr : Surface.surface_expr) : scope_index =
  match expr.se_expr with
  | Surface.SEIdentifier _ | Surface.SEInteger _ | Surface.SEFloat _ | Surface.SEBoolean _ | Surface.SEString _
  | Surface.SEPlaceholder ->
      acc
  | Surface.SEArray exprs -> List.fold_left build_scope_index_expr acc exprs
  | Surface.SEIndex (left, right) | Surface.SEInfix (left, _, right) ->
      build_scope_index_expr (build_scope_index_expr acc left) right
  | Surface.SETypeApply (callee, type_args) ->
      List.fold_left build_scope_index_type (build_scope_index_expr acc callee) type_args
  | Surface.SEHash pairs ->
      List.fold_left
        (fun acc (left, right) -> build_scope_index_expr (build_scope_index_expr acc left) right)
        acc pairs
  | Surface.SEPrefix (_, inner) | Surface.SETypeCheck (inner, _) | Surface.SEFieldAccess (inner, _) ->
      build_scope_index_expr acc inner
  | Surface.SEIf (cond, then_, else_) ->
      let acc = build_scope_index_expr acc cond in
      let acc = build_scope_index_stmt acc then_ in
      Option.fold ~none:acc ~some:(build_scope_index_stmt acc) else_
  | Surface.SECall (callee, args) ->
      List.fold_left build_scope_index_expr (build_scope_index_expr acc callee) args
  | Surface.SEEnumConstructor (_, _, args) -> List.fold_left build_scope_index_expr acc args
  | Surface.SEMatch (scrutinee, arms) ->
      let acc = build_scope_index_expr acc scrutinee in
      List.fold_left
        (fun acc (arm : Surface.surface_match_arm) -> build_scope_index_expr_or_block acc arm.se_arm_body)
        acc arms
  | Surface.SERecordLit (fields, spread) ->
      let acc =
        List.fold_left
          (fun acc (field : Surface.surface_record_field) ->
            Option.fold ~none:acc ~some:(build_scope_index_expr acc) field.se_field_value)
          acc fields
      in
      Option.fold ~none:acc ~some:(build_scope_index_expr acc) spread
  | Surface.SEMethodCall { se_receiver; se_args; _ } ->
      List.fold_left build_scope_index_expr (build_scope_index_expr acc se_receiver) se_args
  | Surface.SEArrowLambda { se_lambda_params; se_lambda_body; _ } ->
      let acc =
        add_value_param_bindings acc ~scope_start:expr.se_pos ~scope_end:expr.se_end_pos se_lambda_params
      in
      build_scope_index_expr_or_block acc se_lambda_body
  | Surface.SEBlockExpr block -> build_scope_index_block acc block

and build_scope_index_expr_or_block (acc : scope_index) = function
  | Surface.SEOBExpr expr -> build_scope_index_expr acc expr
  | Surface.SEOBBlock block -> build_scope_index_block acc block

and build_scope_index_stmt (acc : scope_index) (stmt : Surface.surface_stmt) : scope_index =
  match stmt.ss_stmt with
  | Surface.SSLet { ss_name; ss_name_ref; ss_value; ss_type_annotation } ->
      let acc =
        add_binding acc ~binding_kind:Value_binding ~binding_name:ss_name ~binding_ref:ss_name_ref
          ~scope_start:stmt.ss_pos ~scope_end:stmt.ss_end_pos
      in
      let acc = Option.fold ~none:acc ~some:(build_scope_index_type acc) ss_type_annotation in
      build_scope_index_expr acc ss_value
  | Surface.SSReturn expr | Surface.SSExpressionStmt expr -> build_scope_index_expr acc expr
  | Surface.SSBlock block -> build_scope_index_block acc block

and build_scope_index_block (acc : scope_index) (block : Surface.surface_block) : scope_index =
  List.fold_left build_scope_index_stmt acc block.sb_stmts

and build_scope_index_type (acc : scope_index) (type_expr : Surface.surface_type_expr) : scope_index =
  match type_expr.ste_desc with
  | Surface.STVar _ | Surface.STCon _ -> acc
  | Surface.STApp (_, args) | Surface.STUnion args | Surface.STIntersection args ->
      List.fold_left build_scope_index_type acc args
  | Surface.STConstraintShorthand _ | Surface.STTraitObject _ -> acc
  | Surface.STArrow (params, ret, _) ->
      build_scope_index_type (List.fold_left build_scope_index_type acc params) ret
  | Surface.STRecord (fields, row) ->
      let acc =
        List.fold_left
          (fun acc (field : Surface.surface_record_type_field) -> build_scope_index_type acc field.sf_type)
          acc fields
      in
      Option.fold ~none:acc ~some:(build_scope_index_type acc) row

let build_scope_index (program : Surface.surface_program) : scope_index =
  List.fold_left
    (fun acc (stmt : Surface.surface_top_stmt) ->
      match stmt.std_decl with
      | Surface.SExportDecl _ | Surface.SImportDecl _ -> acc
      | Surface.SLet { name; name_ref; value; type_annotation } ->
          let acc =
            add_binding acc ~binding_kind:Value_binding ~binding_name:name ~binding_ref:name_ref
              ~scope_start:stmt.std_pos ~scope_end:stmt.std_end_pos
          in
          let acc = Option.fold ~none:acc ~some:(build_scope_index_type acc) type_annotation in
          build_scope_index_expr acc value
      | Surface.SFnDecl { generics; params; return_type; body; _ } ->
          let acc =
            Option.fold ~none:acc
              ~some:(add_generic_param_bindings acc ~scope_start:stmt.std_pos ~scope_end:stmt.std_end_pos)
              generics
          in
          let acc = add_value_param_bindings acc ~scope_start:stmt.std_pos ~scope_end:stmt.std_end_pos params in
          let acc = Option.fold ~none:acc ~some:(build_scope_index_type acc) return_type in
          build_scope_index_expr_or_block acc body
      | Surface.STypeDef { type_type_param_refs; type_type_params; type_body; derive; _ } ->
          let acc =
            List.fold_left2
              (fun acc binding_name binding_ref ->
                add_binding acc ~binding_kind:Type_binding ~binding_name ~binding_ref ~scope_start:stmt.std_pos
                  ~scope_end:stmt.std_end_pos)
              acc type_type_params type_type_param_refs
          in
          let acc =
            match type_body with
            | Surface.STTransparent type_expr | Surface.STNamedWrapper type_expr -> build_scope_index_type acc type_expr
            | Surface.STNamedProduct fields ->
                List.fold_left
                  (fun acc (field : Surface.surface_record_type_field) -> build_scope_index_type acc field.sf_type)
                  acc fields
            | Surface.STNamedSum variants ->
                List.fold_left
                  (fun acc (variant : Surface.surface_variant_def) ->
                    List.fold_left build_scope_index_type acc variant.sv_fields)
                  acc variants
          in
          List.fold_left
            (fun acc (derive_trait : Surface.surface_derive_trait) ->
              add_generic_param_bindings acc ~scope_start:stmt.std_pos ~scope_end:stmt.std_end_pos
                derive_trait.sdt_constraints)
            acc derive
      | Surface.SShapeDef { shape_type_params; shape_type_param_refs; shape_fields; _ } ->
          let acc =
            List.fold_left2
              (fun acc binding_name binding_ref ->
                add_binding acc ~binding_kind:Type_binding ~binding_name ~binding_ref ~scope_start:stmt.std_pos
                  ~scope_end:stmt.std_end_pos)
              acc shape_type_params shape_type_param_refs
          in
          List.fold_left
            (fun acc (field : Surface.surface_record_type_field) -> build_scope_index_type acc field.sf_type)
            acc shape_fields
      | Surface.STraitDef { type_param; type_param_ref; methods; _ } ->
          let acc =
            match (type_param, type_param_ref) with
            | Some binding_name, Some binding_ref ->
                add_binding acc ~binding_kind:Type_binding ~binding_name ~binding_ref ~scope_start:stmt.std_pos
                  ~scope_end:stmt.std_end_pos
            | _ -> acc
          in
          List.fold_left
            (fun acc (method_ : Surface.surface_method_sig) ->
              let acc =
                Option.fold ~none:acc
                  ~some:(add_generic_param_bindings acc ~scope_start:stmt.std_pos ~scope_end:stmt.std_end_pos)
                  method_.sm_generics
              in
              let acc = add_typed_param_bindings acc ~scope_start:stmt.std_pos ~scope_end:stmt.std_end_pos method_.sm_params in
              let acc = build_scope_index_type acc method_.sm_return_type in
              Option.fold ~none:acc ~some:(build_scope_index_expr_or_block acc) method_.sm_default_impl)
            acc methods
      | Surface.SAmbiguousImplDef { impl_type_params; impl_head_type; impl_methods } ->
          let acc =
            add_generic_param_bindings acc ~scope_start:stmt.std_pos ~scope_end:stmt.std_end_pos impl_type_params
          in
          let acc = build_scope_index_type acc impl_head_type in
          List.fold_left
            (fun acc (method_ : Surface.surface_method_impl) ->
              let acc =
                Option.fold ~none:acc
                  ~some:(add_generic_param_bindings acc ~scope_start:stmt.std_pos ~scope_end:stmt.std_end_pos)
                  method_.smi_generics
              in
              let acc = add_value_param_bindings acc ~scope_start:stmt.std_pos ~scope_end:stmt.std_end_pos method_.smi_params in
              let acc = Option.fold ~none:acc ~some:(build_scope_index_type acc) method_.smi_return_type in
              build_scope_index_expr_or_block acc method_.smi_body)
            acc impl_methods
      | Surface.SInherentImplDef { inherent_for_type; inherent_methods } ->
          let acc = build_scope_index_type acc inherent_for_type in
          List.fold_left
            (fun acc (method_ : Surface.surface_method_impl) ->
              let acc =
                Option.fold ~none:acc
                  ~some:(add_generic_param_bindings acc ~scope_start:stmt.std_pos ~scope_end:stmt.std_end_pos)
                  method_.smi_generics
              in
              let acc = add_value_param_bindings acc ~scope_start:stmt.std_pos ~scope_end:stmt.std_end_pos method_.smi_params in
              let acc = Option.fold ~none:acc ~some:(build_scope_index_type acc) method_.smi_return_type in
              build_scope_index_expr_or_block acc method_.smi_body)
            acc inherent_methods
      | Surface.SExpressionStmt expr | Surface.SReturn expr -> build_scope_index_expr acc expr
      | Surface.SBlock block -> build_scope_index_block acc block)
    [] program

let first_some a b =
  match a with
  | Some _ -> a
  | None -> b

let identifier_root_of_expr (expr : Surface.surface_expr) : (Surface.name_ref * int option) option =
  match expr.se_expr with
  | Surface.SEIdentifier name_ref -> Some (name_ref, Some expr.se_id)
  | Surface.SETypeApply (callee, _) -> (
      match callee.se_expr with
      | Surface.SEIdentifier name_ref -> Some (name_ref, Some callee.se_id)
      | _ -> None)
  | _ -> None

let declaration_ref_if_contains ~(offset : int) ~(name_ref : Surface.name_ref) ~(declaration_kind : declaration_kind) :
    reference option =
  if name_ref_contains ~offset name_ref then
    Some (Declaration_head { name_ref; declaration_kind })
  else
    None

let type_reference_of_name ~(scope_index : scope_index) ~(offset : int) (name_ref : Surface.name_ref) : reference option =
  if name_ref_contains ~offset name_ref then
    let binding =
      binding_of_name ~scope_index ~binding_kind:Type_binding ~name:name_ref.text ~offset
    in
    Some (Type_identifier { name_ref; binding })
  else
    None

let constraint_reference_of_name ~(offset : int) (name_ref : Surface.name_ref) : reference option =
  if name_ref_contains ~offset name_ref then
    Some (Constraint_identifier { name_ref })
  else
    None

let reference_in_generic_param ~(offset : int) (param : Surface.surface_generic_param) : reference option =
  first_some
    (declaration_ref_if_contains ~offset ~name_ref:param.sg_name_ref ~declaration_kind:Generic_decl)
    (List.find_map (constraint_reference_of_name ~offset) param.sg_constraint_refs)

let value_reference_of_expr ~(scope_index : scope_index) ~(offset : int) (expr : Surface.surface_expr)
    (name_ref : Surface.name_ref) : reference option =
  if name_ref_contains ~offset name_ref then
    let binding =
      binding_of_name ~scope_index ~binding_kind:Value_binding ~name:name_ref.text ~offset
    in
    Some (Value_identifier { name_ref; expr_id = expr.se_id; binding })
  else
    None

let rec reference_in_type ~(scope_index : scope_index) ~(offset : int) (type_expr : Surface.surface_type_expr) :
    reference option =
  match type_expr.ste_desc with
  | Surface.STVar name_ref | Surface.STCon name_ref -> type_reference_of_name ~scope_index ~offset name_ref
  | Surface.STApp (name_ref, args) ->
      first_some
        (type_reference_of_name ~scope_index ~offset name_ref)
        (List.find_map (reference_in_type ~scope_index ~offset) args)
  | Surface.STConstraintShorthand refs | Surface.STTraitObject refs ->
      List.find_map (constraint_reference_of_name ~offset) refs
  | Surface.STArrow (params, ret, _) ->
      first_some
        (List.find_map (reference_in_type ~scope_index ~offset) params)
        (reference_in_type ~scope_index ~offset ret)
  | Surface.STUnion members | Surface.STIntersection members ->
      List.find_map (reference_in_type ~scope_index ~offset) members
  | Surface.STRecord (fields, row) ->
      first_some
        (List.find_map
           (fun (field : Surface.surface_record_type_field) -> reference_in_type ~scope_index ~offset field.sf_type)
           fields)
        (Option.bind row (reference_in_type ~scope_index ~offset))

let rec reference_in_expr ~(scope_index : scope_index) ~(offset : int) (expr : Surface.surface_expr) : reference option =
  match expr.se_expr with
  | Surface.SEIdentifier name_ref -> value_reference_of_expr ~scope_index ~offset expr name_ref
  | Surface.SEInteger _ | Surface.SEFloat _ | Surface.SEBoolean _ | Surface.SEString _ | Surface.SEPlaceholder -> None
  | Surface.SEArray exprs -> List.find_map (reference_in_expr ~scope_index ~offset) exprs
  | Surface.SEIndex (left, right) | Surface.SEInfix (left, _, right) ->
      first_some (reference_in_expr ~scope_index ~offset left) (reference_in_expr ~scope_index ~offset right)
  | Surface.SETypeApply (callee, type_args) ->
      first_some
        (reference_in_expr ~scope_index ~offset callee)
        (List.find_map (reference_in_type ~scope_index ~offset) type_args)
  | Surface.SEHash pairs ->
      List.find_map
        (fun (left, right) ->
          first_some (reference_in_expr ~scope_index ~offset left) (reference_in_expr ~scope_index ~offset right))
        pairs
  | Surface.SEPrefix (_, inner) -> reference_in_expr ~scope_index ~offset inner
  | Surface.SETypeCheck (inner, type_expr) ->
      first_some (reference_in_expr ~scope_index ~offset inner) (reference_in_type ~scope_index ~offset type_expr)
  | Surface.SEIf (cond, then_, else_) ->
      first_some
        (reference_in_expr ~scope_index ~offset cond)
        (first_some
           (reference_in_stmt ~scope_index ~offset then_)
           (Option.bind else_ (reference_in_stmt ~scope_index ~offset)))
  | Surface.SECall (callee, args) ->
      first_some
        (reference_in_expr ~scope_index ~offset callee)
        (List.find_map (reference_in_expr ~scope_index ~offset) args)
  | Surface.SEEnumConstructor (root_ref, member_ref, args) ->
      if name_ref_contains ~offset member_ref then
        Some (Qualified_member { root_ref; root_expr_id = None; member_ref; access_expr_id = expr.se_id })
      else if name_ref_contains ~offset root_ref then
        Some (Qualified_root { root_ref; root_expr_id = None; member_ref; access_expr_id = expr.se_id })
      else
        List.find_map (reference_in_expr ~scope_index ~offset) args
  | Surface.SEMatch (scrutinee, arms) ->
      first_some
        (reference_in_expr ~scope_index ~offset scrutinee)
        (List.find_map
           (fun (arm : Surface.surface_match_arm) -> reference_in_expr_or_block ~scope_index ~offset arm.se_arm_body)
           arms)
  | Surface.SERecordLit (fields, spread) ->
      first_some
        (List.find_map
           (fun (field : Surface.surface_record_field) ->
             Option.bind field.se_field_value (reference_in_expr ~scope_index ~offset))
           fields)
        (Option.bind spread (reference_in_expr ~scope_index ~offset))
  | Surface.SEFieldAccess (receiver, member_ref) ->
      if name_ref_contains ~offset member_ref then
        Option.map
          (fun (root_ref, root_expr_id) ->
            Qualified_member { root_ref; root_expr_id; member_ref; access_expr_id = expr.se_id })
          (identifier_root_of_expr receiver)
      else
        first_some
          (Option.map
             (fun (root_ref, root_expr_id) ->
               Qualified_root { root_ref; root_expr_id; member_ref; access_expr_id = expr.se_id })
             (match identifier_root_of_expr receiver with
             | Some ((root_ref, _) as root_info) when name_ref_contains ~offset root_ref -> Some root_info
             | _ -> None))
          (reference_in_expr ~scope_index ~offset receiver)
  | Surface.SEMethodCall { se_receiver; se_method_ref; se_type_args; se_args; _ } ->
      if name_ref_contains ~offset se_method_ref then
        Option.map
          (fun (root_ref, root_expr_id) ->
            Qualified_member
              { root_ref; root_expr_id; member_ref = se_method_ref; access_expr_id = expr.se_id })
          (identifier_root_of_expr se_receiver)
      else
        first_some
          (Option.map
             (fun (root_ref, root_expr_id) ->
               Qualified_root { root_ref; root_expr_id; member_ref = se_method_ref; access_expr_id = expr.se_id })
             (match identifier_root_of_expr se_receiver with
             | Some ((root_ref, _) as root_info) when name_ref_contains ~offset root_ref -> Some root_info
             | _ -> None))
          (first_some
             (reference_in_expr ~scope_index ~offset se_receiver)
             (first_some
                (Option.bind se_type_args (List.find_map (reference_in_type ~scope_index ~offset)))
                (List.find_map (reference_in_expr ~scope_index ~offset) se_args)))
  | Surface.SEArrowLambda { se_lambda_body; _ } -> reference_in_expr_or_block ~scope_index ~offset se_lambda_body
  | Surface.SEBlockExpr block -> reference_in_block ~scope_index ~offset block

and reference_in_expr_or_block ~(scope_index : scope_index) ~(offset : int) = function
  | Surface.SEOBExpr expr -> reference_in_expr ~scope_index ~offset expr
  | Surface.SEOBBlock block -> reference_in_block ~scope_index ~offset block

and reference_in_stmt ~(scope_index : scope_index) ~(offset : int) (stmt : Surface.surface_stmt) : reference option =
  match stmt.ss_stmt with
  | Surface.SSLet { ss_name_ref; ss_type_annotation; ss_value; _ } ->
      first_some
        (declaration_ref_if_contains ~offset ~name_ref:ss_name_ref ~declaration_kind:Let_decl)
        (first_some
           (Option.bind ss_type_annotation (reference_in_type ~scope_index ~offset))
           (reference_in_expr ~scope_index ~offset ss_value))
  | Surface.SSReturn expr | Surface.SSExpressionStmt expr -> reference_in_expr ~scope_index ~offset expr
  | Surface.SSBlock block -> reference_in_block ~scope_index ~offset block

and reference_in_block ~(scope_index : scope_index) ~(offset : int) (block : Surface.surface_block) : reference option =
  List.find_map (reference_in_stmt ~scope_index ~offset) block.sb_stmts

and reference_in_method_sig ~(scope_index : scope_index) ~(offset : int) (method_ : Surface.surface_method_sig) :
    reference option =
  first_some
    (declaration_ref_if_contains ~offset ~name_ref:method_.sm_name_ref ~declaration_kind:Trait_method_decl)
    (first_some
       (Option.bind method_.sm_generics (List.find_map (reference_in_generic_param ~offset)))
       (first_some
          (List.find_map
             (fun (param : Surface.surface_typed_param) ->
               first_some
                 (declaration_ref_if_contains ~offset ~name_ref:param.stp_name_ref ~declaration_kind:Let_decl)
                 (reference_in_type ~scope_index ~offset param.stp_type))
             method_.sm_params)
          (first_some
             (reference_in_type ~scope_index ~offset method_.sm_return_type)
             (Option.bind method_.sm_default_impl (reference_in_expr_or_block ~scope_index ~offset)))))

and reference_in_method_impl ~(scope_index : scope_index) ~(offset : int) (method_ : Surface.surface_method_impl) :
    reference option =
  first_some
    (declaration_ref_if_contains ~offset ~name_ref:method_.smi_name_ref ~declaration_kind:Inherent_method_decl)
    (first_some
       (Option.bind method_.smi_generics (List.find_map (reference_in_generic_param ~offset)))
       (first_some
          (List.find_map
             (fun (param : Surface.surface_value_param) ->
               first_some
                 (declaration_ref_if_contains ~offset ~name_ref:param.svp_name_ref ~declaration_kind:Let_decl)
                 (Option.bind param.svp_type (reference_in_type ~scope_index ~offset)))
             method_.smi_params)
          (first_some
             (Option.bind method_.smi_return_type (reference_in_type ~scope_index ~offset))
             (reference_in_expr_or_block ~scope_index ~offset method_.smi_body))))

let reference_in_top_stmt ~(scope_index : scope_index) ~(offset : int) (stmt : Surface.surface_top_stmt) :
    reference option =
  match stmt.std_decl with
  | Surface.SExportDecl _ -> None
  | Surface.SImportDecl { import_path; import_path_refs; import_alias_ref; _ } ->
      first_some
        (Option.bind import_alias_ref (fun alias_ref ->
             if name_ref_contains ~offset alias_ref then
               Some (Import_alias { alias_ref; import_path })
             else
               None))
        (List.find_mapi
           (fun segment_index segment_ref ->
             if name_ref_contains ~offset segment_ref then
               Some (Import_path_segment { segment_ref; import_path; segment_index })
             else
               None)
           import_path_refs)
  | Surface.SLet { name_ref; type_annotation; value; _ } ->
      first_some
        (declaration_ref_if_contains ~offset ~name_ref ~declaration_kind:Let_decl)
        (first_some
           (Option.bind type_annotation (reference_in_type ~scope_index ~offset))
           (reference_in_expr ~scope_index ~offset value))
  | Surface.SFnDecl { name_ref; generics; params; return_type; body; _ } ->
      first_some
        (declaration_ref_if_contains ~offset ~name_ref ~declaration_kind:Fn_decl)
        (first_some
           (Option.bind generics (List.find_map (reference_in_generic_param ~offset)))
           (first_some
              (List.find_map
                 (fun (param : Surface.surface_value_param) ->
                   first_some
                     (declaration_ref_if_contains ~offset ~name_ref:param.svp_name_ref ~declaration_kind:Let_decl)
                     (Option.bind param.svp_type (reference_in_type ~scope_index ~offset)))
                 params)
              (first_some
                 (Option.bind return_type (reference_in_type ~scope_index ~offset))
                 (reference_in_expr_or_block ~scope_index ~offset body))))
  | Surface.STypeDef { type_name_ref; type_type_param_refs; type_body; derive; _ } ->
      let type_param_ref =
        List.find_map
          (fun ref_ -> declaration_ref_if_contains ~offset ~name_ref:ref_ ~declaration_kind:Generic_decl)
          type_type_param_refs
      in
      first_some
        (declaration_ref_if_contains ~offset ~name_ref:type_name_ref ~declaration_kind:Type_decl)
        (first_some type_param_ref
           (first_some
              (match type_body with
              | Surface.STTransparent type_expr | Surface.STNamedWrapper type_expr ->
                  reference_in_type ~scope_index ~offset type_expr
              | Surface.STNamedProduct fields ->
                  List.find_map
                    (fun (field : Surface.surface_record_type_field) ->
                      reference_in_type ~scope_index ~offset field.sf_type)
                    fields
              | Surface.STNamedSum variants ->
                  List.find_map
                    (fun (variant : Surface.surface_variant_def) ->
                      first_some
                        (declaration_ref_if_contains ~offset ~name_ref:variant.sv_name_ref
                           ~declaration_kind:Variant_decl)
                        (List.find_map (reference_in_type ~scope_index ~offset) variant.sv_fields))
                    variants)
              (List.find_map
                 (fun (trait_ : Surface.surface_derive_trait) ->
                   first_some
                     (constraint_reference_of_name ~offset trait_.sdt_name_ref)
                     (List.find_map
                        (fun (param : Surface.surface_generic_param) ->
                          declaration_ref_if_contains ~offset ~name_ref:param.sg_name_ref
                            ~declaration_kind:Generic_decl)
                        trait_.sdt_constraints))
                 derive)))
  | Surface.SShapeDef { shape_name_ref; shape_type_param_refs; shape_fields; _ } ->
      first_some
        (declaration_ref_if_contains ~offset ~name_ref:shape_name_ref ~declaration_kind:Shape_decl)
        (first_some
           (List.find_map
              (fun ref_ -> declaration_ref_if_contains ~offset ~name_ref:ref_ ~declaration_kind:Generic_decl)
              shape_type_param_refs)
           (List.find_map
              (fun (field : Surface.surface_record_type_field) ->
                reference_in_type ~scope_index ~offset field.sf_type)
              shape_fields))
  | Surface.STraitDef { name_ref; type_param_ref; supertrait_refs; methods; _ } ->
      first_some
        (declaration_ref_if_contains ~offset ~name_ref ~declaration_kind:Trait_decl)
        (first_some
           (Option.bind type_param_ref (fun ref_ ->
                declaration_ref_if_contains ~offset ~name_ref:ref_ ~declaration_kind:Generic_decl))
           (first_some
              (List.find_map (constraint_reference_of_name ~offset) supertrait_refs)
              (List.find_map (reference_in_method_sig ~scope_index ~offset) methods)))
  | Surface.SAmbiguousImplDef { impl_type_params; impl_head_type; impl_methods } ->
      first_some
        (List.find_map (reference_in_generic_param ~offset) impl_type_params)
        (first_some
           (reference_in_type ~scope_index ~offset impl_head_type)
           (List.find_map (reference_in_method_impl ~scope_index ~offset) impl_methods))
  | Surface.SInherentImplDef { inherent_for_type; inherent_methods } ->
      first_some
        (reference_in_type ~scope_index ~offset inherent_for_type)
        (List.find_map (reference_in_method_impl ~scope_index ~offset) inherent_methods)
  | Surface.SExpressionStmt expr | Surface.SReturn expr -> reference_in_expr ~scope_index ~offset expr
  | Surface.SBlock block -> reference_in_block ~scope_index ~offset block

let reference_at ~source:_ ~(input : cursor_context_input) ~(offset : int) : reference option =
  List.find_map (reference_in_top_stmt ~scope_index:input.scope_index ~offset) input.surface_program

let nth_substring_offset ~(source : string) ~(needle : string) ~(occurrence : int) : int option =
  let rec scan start remaining =
    if remaining <= 0 then
      None
    else if start + String.length needle > String.length source then
      None
    else if String.sub source start (String.length needle) = needle then
      if remaining = 1 then
        Some start
      else
        scan (start + 1) (remaining - 1)
    else
      scan (start + 1) remaining
  in
  scan 0 occurrence

let reference_for_needle ?(occurrence = 1) ?(offset_in_needle = 0) ~(source : string) ~(needle : string) :
    unit -> reference option =
 fun () ->
  match Parser.parse_with_surface ~file_id:"<test>" source with
  | Error _ -> None
  | Ok parse_result -> (
      match nth_substring_offset ~source ~needle ~occurrence with
      | None -> None
      | Some base ->
          let input =
            {
              surface_program = parse_result.surface_program;
              lowered_program = parse_result.program;
              scope_index = build_scope_index parse_result.surface_program;
            }
          in
          reference_at ~source ~input ~offset:(base + offset_in_needle))

let%test "reference_at classifies import aliases from the surface tree" =
  match reference_for_needle ~source:"import math.add as plus\nplus(1, 2)\n" ~needle:"plus" () with
  | Some (Import_alias { import_path; alias_ref }) -> import_path = [ "math"; "add" ] && alias_ref.text = "plus"
  | _ -> false

let%test "reference_at classifies trait method declaration heads" =
  match reference_for_needle ~source:"trait Greeter[a] = { fn greet(x: a) -> Str }\n" ~needle:"greet" () with
  | Some (Declaration_head { declaration_kind = Trait_method_decl; name_ref }) -> name_ref.text = "greet"
  | _ -> false

let%test "reference_at binds annotation type vars to the nearest generic scope" =
  match reference_for_needle ~source:"fn id[t](x: t) -> t = x\n" ~needle:"t" ~occurrence:3 () with
  | Some (Type_identifier { name_ref; binding = Some binding }) ->
      name_ref.text = "t" && binding.binding_kind = Type_binding && binding.binding_name = "t"
  | _ -> false

let%test "reference_at classifies enum constructor members" =
  match
    reference_for_needle
      ~source:"type Option[a] = { Some(a), None }\nlet x = Option.Some(42)\n"
      ~needle:"Option.Some" ~offset_in_needle:7 ()
  with
  | Some (Qualified_member { root_ref; root_expr_id = Some _; member_ref; access_expr_id = _ }) ->
      root_ref.text = "Option" && member_ref.text = "Some"
  | _ -> false
