(* Go code generation from Marmoset AST with monomorphization *)

module AST = Syntax.Ast.AST
module Types = Typecheck.Types
module Infer = Typecheck.Infer
module Annotation = Typecheck.Annotation

(* ============================================================
   Type Mangling - convert types to Go-safe identifiers
   ============================================================ *)

let rec mangle_type (t : Types.mono_type) : string =
  match t with
  | Types.TInt -> "int64"
  | Types.TFloat -> "float64"
  | Types.TBool -> "bool"
  | Types.TString -> "string"
  | Types.TNull -> "unit"
  | Types.TVar _name -> "any" (* Unresolved type variable - use 'any' in mangled names *)
  | Types.TFun (arg, ret) -> "fn_" ^ mangle_type arg ^ "_" ^ mangle_type ret
  | Types.TArray elem -> "arr_" ^ mangle_type elem
  | Types.THash (key, value) -> "map_" ^ mangle_type key ^ "_" ^ mangle_type value
  | Types.TUnion _ -> "union" (* Phase 4.1: unions will be interface{} *)
  | Types.TEnum (name, []) -> name
  | Types.TEnum (name, args) -> name ^ "_" ^ String.concat "_" (List.map mangle_type args)

(* Generate mangled function name: name_type1_type2_... *)
(* For functions with union parameters, don't mangle - use base name *)
let mangle_func_name name (param_types : Types.mono_type list) : string =
  (* Check if any param is a union - if so, don't mangle *)
  let has_union =
    List.exists
      (function
        | Types.TUnion _ -> true
        | _ -> false)
      param_types
  in
  if has_union || param_types = [] then
    name
  else
    name ^ "_" ^ (List.map mangle_type param_types |> String.concat "_")

(* ============================================================
   Function Registry - track function definitions and instantiations
   ============================================================ *)

(* A function definition: name, params, body, and its polymorphic type *)
type func_def = {
  name : string;
  params : AST.expression list;
  body : AST.statement;
  poly_type : Types.poly_type;
  captures : string list; (* variables captured from outer scope - for closures *)
}

(* An instantiation: concrete types for a polymorphic function *)
type instantiation = {
  func_name : string;
  concrete_types : Types.mono_type list; (* param types *)
  return_type : Types.mono_type;
}

module InstSet = Set.Make (struct
  type t = instantiation

  let compare a b =
    let c = String.compare a.func_name b.func_name in
    if c <> 0 then
      c
    else
      compare a.concrete_types b.concrete_types
end)

(* Set to track enum type instantiations *)
module EnumInstSet = Set.Make (struct
  type t = string * Types.mono_type list (* enum_name, type_args *)

  let compare = compare
end)

type mono_state = {
  mutable func_defs : func_def list;
  mutable instantiations : InstSet.t;
  mutable enum_insts : EnumInstSet.t; (* Track which enum types we need to generate *)
  mutable name_counter : int;
  concrete_only : bool; (* Phase 4.3: Rust-style (true) vs TypeScript-style (false) codegen *)
}

let create_mono_state ?(concrete_only = true) () =
  {
    func_defs = [];
    instantiations = InstSet.empty;
    enum_insts = EnumInstSet.empty;
    name_counter = 0;
    concrete_only;
  }

(* ============================================================
   Enum Layout Types (Phase 4.4: Multi-field support)
   ============================================================ *)

(* Types for enum field layout analysis *)
type field_mapping = {
  data_field_name : string; (* e.g., "Data0" *)
  data_field_type : Types.mono_type;
  go_type : string; (* e.g., "int64" *)
}

type variant_field_map = (int * field_mapping) list (* position -> mapping *)

type enum_layout = {
  fields : field_mapping list; (* All DataN fields in order *)
  variant_maps : (string * variant_field_map) list; (* variant_name -> mappings *)
}

(* Get size/alignment of a type for sorting (used for optimal field layout) *)
let type_size (t : Types.mono_type) : int =
  match t with
  | Types.TInt | Types.TFloat | Types.TArray _ | Types.THash _ | Types.TEnum _ | Types.TString ->
      8 (* 64-bit types, pointers *)
  | Types.TBool -> 1
  | Types.TNull -> 0
  | Types.TVar _ -> 8 (* Treat as pointer *)
  | Types.TFun _ -> 8 (* Function pointer *)
  | Types.TUnion _ -> 8 (* Interface *)

(* ============================================================
   Type to Go Type String and Enum Layout Analysis
   (Mutually recursive because analyze_enum_layout needs type_to_go)
   ============================================================ *)

let rec type_to_go (state : mono_state) (t : Types.mono_type) : string =
  match t with
  | Types.TInt -> "int64"
  | Types.TFloat -> "float64"
  | Types.TBool -> "bool"
  | Types.TString -> "string"
  | Types.TNull -> "struct{}"
  | Types.TVar _name ->
      (* Phase 4.3: In Rust-style mode, unresolved type variables are an error *)
      (* However, we allow interface{} fallback during enum layout analysis *)
      if state.concrete_only then
        failwith
          (Printf.sprintf
             "Cannot generate code for unresolved type variable. Add type annotation to resolve this type.")
      else
        "interface{}"
        (* TypeScript-style fallback *)
  | Types.TFun (arg, ret) -> emit_func_type state arg ret
  | Types.TArray elem -> "[]" ^ type_to_go state elem
  | Types.THash (key, value) -> "map[" ^ type_to_go state key ^ "]" ^ type_to_go state value
  | Types.TUnion _ -> "interface{}" (* Phase 4.1: unions compile to interface{} *)
  | Types.TEnum (name, args) -> mangle_type (Types.TEnum (name, args))

and emit_func_type state arg ret =
  let rec collect_args = function
    | Types.TFun (a, r) ->
        let args, final_ret = collect_args r in
        (a :: args, final_ret)
    | t -> ([], t)
  in
  let args, final_ret = collect_args (Types.TFun (arg, ret)) in
  let args_str = List.map (type_to_go state) args |> String.concat ", " in
  Printf.sprintf "func(%s) %s" args_str (type_to_go state final_ret)

(* Analyze enum fields and build layout mapping *)
and analyze_enum_layout
    (state : mono_state) (enum_def : Typecheck.Enum_registry.enum_def) (type_args : Types.mono_type list) :
    enum_layout =
  (* Find the maximum number of fields across all variants *)
  let max_fields =
    List.fold_left
      (fun acc (v : Typecheck.Enum_registry.variant_def) -> max acc (List.length v.fields))
      0 enum_def.variants
  in

  (* For each position 0..max_fields-1, collect ALL unique types used at that position *)
  let position_type_lists =
    List.init max_fields (fun pos ->
        (* Get all types used at this position across all variants *)
        let types_at_pos =
          List.filter_map
            (fun (v : Typecheck.Enum_registry.variant_def) ->
              if pos < List.length v.fields then
                let subst = List.combine enum_def.type_params type_args in
                Some (Types.apply_substitution subst (List.nth v.fields pos))
              else
                None)
            enum_def.variants
        in
        (* Get unique types at this position *)
        List.sort_uniq compare types_at_pos)
  in

  (* Flatten all position types into a list of (position, type) pairs *)
  let all_position_types =
    List.concat (List.mapi (fun pos types -> List.map (fun t -> (pos, t)) types) position_type_lists)
  in

  (* Create field mappings - one field per (position, type) pair *)
  (* Use sequential Data0, Data1, Data2, ... naming - one for each unique (pos, type) pair *)
  let field_mappings_with_pos =
    List.mapi
      (fun idx (pos, field_type) ->
        let field_name = Printf.sprintf "Data%d" idx in
        ( pos,
          field_type,
          { data_field_name = field_name; data_field_type = field_type; go_type = type_to_go state field_type } ))
      all_position_types
  in

  (* Create a list of unique field mappings *)
  let field_mappings =
    List.map (fun (_, _, fm) -> fm) field_mappings_with_pos
    |> List.sort_uniq (fun a b -> compare a.data_field_name b.data_field_name)
  in

  (* Build mapping from (variant, position) to field_mapping *)
  let variant_maps =
    List.map
      (fun (v : Typecheck.Enum_registry.variant_def) ->
        let subst = List.combine enum_def.type_params type_args in
        let substituted_fields = List.map (Types.apply_substitution subst) v.fields in
        let mappings =
          List.mapi
            (fun pos field_type ->
              (* Find the field mapping for this (pos, type) pair *)
              let _, _, mapping =
                List.find (fun (p, t, _) -> p = pos && t = field_type) field_mappings_with_pos
              in
              (pos, mapping))
            substituted_fields
        in
        (v.name, mappings))
      enum_def.variants
  in

  { fields = field_mappings; variant_maps }

(* ============================================================
   Pass 1: Collect function definitions
   ============================================================ *)

let rec collect_funcs_stmt (state : mono_state) (stmt : AST.statement) : unit =
  match stmt.stmt with
  | AST.Let let_binding -> (
      match let_binding.value.expr with
      | AST.Function f ->
          (* Phase 2: Convert new param format to expressions *)
          let param_exprs = List.map (fun (pname, _) -> AST.mk_expr (AST.Identifier pname)) f.params in
          (* For now, assume no captures - we'll handle closures later *)
          state.func_defs <-
            {
              name = let_binding.name;
              params = param_exprs;
              body = f.body;
              poly_type = Types.Forall ([], Types.TNull);
              (* filled in later *)
              captures = [];
            }
            :: state.func_defs;
          collect_funcs_stmt state f.body
      | _ -> collect_funcs_expr state let_binding.value)
  | AST.Return e -> collect_funcs_expr state e
  | AST.ExpressionStmt e -> collect_funcs_expr state e
  | AST.Block stmts -> List.iter (collect_funcs_stmt state) stmts
  | AST.EnumDef _ -> () (* Enums are compile-time only *)
  | AST.TraitDef _ | AST.ImplDef _ | AST.DeriveDef _ -> () (* Traits are compile-time only *)

and collect_funcs_expr (state : mono_state) (expr : AST.expression) : unit =
  match expr.expr with
  | AST.Integer _ | AST.Float _ | AST.Boolean _ | AST.String _ | AST.Identifier _ -> ()
  | AST.Prefix (_, e) -> collect_funcs_expr state e
  | AST.Infix (l, _, r) ->
      collect_funcs_expr state l;
      collect_funcs_expr state r
  | AST.TypeCheck (e, _) -> collect_funcs_expr state e
  | AST.If (cond, cons, alt) ->
      collect_funcs_expr state cond;
      collect_funcs_stmt state cons;
      Option.iter (collect_funcs_stmt state) alt
  | AST.Call (f, args) ->
      collect_funcs_expr state f;
      List.iter (collect_funcs_expr state) args
  | AST.Array elements -> List.iter (collect_funcs_expr state) elements
  | AST.Hash pairs ->
      List.iter
        (fun (k, v) ->
          collect_funcs_expr state k;
          collect_funcs_expr state v)
        pairs
  | AST.Index (container, index) ->
      collect_funcs_expr state container;
      collect_funcs_expr state index
  | AST.Function f ->
      (* Phase 2: Anonymous function - not a top-level let binding *)
      collect_funcs_stmt state f.body
  | AST.EnumConstructor (_, _, args) -> List.iter (collect_funcs_expr state) args
  | AST.Match (scrutinee, arms) ->
      collect_funcs_expr state scrutinee;
      List.iter (fun arm -> collect_funcs_expr state arm.AST.body) arms

(* ============================================================
   Pass 2: Collect instantiations at call sites
   ============================================================ *)

(* Get the type of an expression from the type map *)
let get_type (type_map : Infer.type_map) (expr : AST.expression) : Types.mono_type =
  match Hashtbl.find_opt type_map expr.id with
  | Some t -> t
  | None ->
      (* Fallback to TNull if type not found - shouldn't happen with proper type checking *)
      Printf.eprintf "Warning: No type found for expression id %d\n%!" expr.id;
      Types.TNull

(* Check if a type contains unresolved type variables *)
let rec has_type_vars (t : Types.mono_type) : bool =
  match t with
  | Types.TVar _ -> true
  | Types.TFun (arg, ret) -> has_type_vars arg || has_type_vars ret
  | Types.TArray elem -> has_type_vars elem
  | Types.THash (k, v) -> has_type_vars k || has_type_vars v
  | Types.TEnum (_, args) -> List.exists has_type_vars args
  | Types.TUnion types -> List.exists has_type_vars types
  | _ -> false

(* Track an enum instantiation *)
let track_enum_inst state (t : Types.mono_type) =
  match t with
  | Types.TEnum (name, args) ->
      (* In Rust-style mode, skip instantiations with unresolved type variables *)
      (* In TypeScript-style mode, track all instantiations (type vars become interface{}) *)
      if state.concrete_only && List.exists has_type_vars args then
        ()
        (* Skip - unresolved type variables in Rust-style mode *)
      else
        state.enum_insts <- EnumInstSet.add (name, args) state.enum_insts
  | _ -> ()

let is_user_func (state : mono_state) (name : string) : bool =
  List.exists (fun fd -> fd.name = name) state.func_defs

(* Extract parameter types from a function type *)
let rec extract_param_types n = function
  | Types.TFun (arg, ret) when n > 0 ->
      let rest, final = extract_param_types (n - 1) ret in
      (arg :: rest, final)
  | t -> ([], t)

let rec collect_insts_stmt
    (state : mono_state) (type_map : Infer.type_map) (env : Infer.type_env) (stmt : AST.statement) :
    Infer.type_env =
  match stmt.stmt with
  | AST.Let let_binding ->
      (* Use the type from the environment if it exists (from type checking),
         otherwise get from type_map *)
      let expr_type =
        match Infer.TypeEnv.find_opt let_binding.name env with
        | Some (Types.Forall (_, t)) -> t
        | None -> get_type type_map let_binding.value
      in
      let env_with_binding = Infer.TypeEnv.add let_binding.name (Types.Forall ([], expr_type)) env in
      (* Pass the expected type to collect_insts_expr so it can use it for EnumConstructors *)
      collect_insts_expr ~expected_type:(Some expr_type) state type_map env_with_binding let_binding.value;
      env_with_binding
  | AST.Return e ->
      collect_insts_expr state type_map env e;
      env
  | AST.ExpressionStmt e ->
      collect_insts_expr state type_map env e;
      env
  | AST.Block stmts -> List.fold_left (collect_insts_stmt state type_map) env stmts
  | AST.EnumDef _ -> env (* Enums are compile-time only *)
  | AST.TraitDef _ | AST.ImplDef _ | AST.DeriveDef _ -> env (* Traits are compile-time only *)

and collect_insts_expr
    ?(expected_type : Types.mono_type option = None)
    (state : mono_state)
    (type_map : Infer.type_map)
    (env : Infer.type_env)
    (expr : AST.expression) : unit =
  match expr.expr with
  | AST.Integer _ | AST.Float _ | AST.Boolean _ | AST.String _ | AST.Identifier _ -> ()
  | AST.Prefix (_, e) -> collect_insts_expr state type_map env e
  | AST.Infix (l, _, r) ->
      collect_insts_expr state type_map env l;
      collect_insts_expr state type_map env r
  | AST.TypeCheck (e, _) -> collect_insts_expr state type_map env e
  | AST.If (cond, cons, alt) ->
      collect_insts_expr state type_map env cond;
      ignore (collect_insts_stmt state type_map env cons);
      Option.iter (fun s -> ignore (collect_insts_stmt state type_map env s)) alt
  | AST.Call (func, args) -> (
      (* Collect from subexpressions first *)
      collect_insts_expr state type_map env func;
      List.iter (collect_insts_expr state type_map env) args;
      (* Check if this is a call to a user-defined function *)
      match func.expr with
      | AST.Identifier name when is_user_func state name ->
          (* Look up the function's declared type from the environment *)
          let func_param_types =
            match Infer.TypeEnv.find_opt name env with
            | Some (Types.Forall (_, func_type)) ->
                (* Extract parameter types from the function type *)
                let num_args = List.length args in
                let declared_param_types, _ = extract_param_types num_args func_type in
                declared_param_types
            | None ->
                (* Fallback to argument types if function not in env *)
                List.map (get_type type_map) args
          in
          (* Check if any declared param is a union type *)
          let has_union_param =
            List.exists
              (function
                | Types.TUnion _ -> true
                | _ -> false)
              func_param_types
          in
          (* If function has union params, use declared types; otherwise use argument types *)
          let param_types =
            if has_union_param then
              func_param_types
            else
              List.map (get_type type_map) args
          in
          let return_type = get_type type_map expr in
          let inst = { func_name = name; concrete_types = param_types; return_type } in
          state.instantiations <- InstSet.add inst state.instantiations
      | _ -> ())
  | AST.Array elements -> List.iter (collect_insts_expr state type_map env) elements
  | AST.Hash pairs ->
      List.iter
        (fun (k, v) ->
          collect_insts_expr state type_map env k;
          collect_insts_expr state type_map env v)
        pairs
  | AST.Index (container, index) ->
      collect_insts_expr state type_map env container;
      collect_insts_expr state type_map env index
  | AST.Function f ->
      (* Phase 2: Create env with params for the body *)
      let func_type = get_type type_map expr in
      let param_types, _ = extract_param_types (List.length f.params) func_type in
      let param_names = List.map fst f.params in
      let body_env =
        List.fold_left2
          (fun acc name typ -> Infer.TypeEnv.add name (Types.Forall ([], typ)) acc)
          env param_names param_types
      in
      ignore (collect_insts_stmt state type_map body_env f.body)
  | AST.EnumConstructor (_, _, args) ->
      List.iter (collect_insts_expr state type_map env) args;
      (* Use expected_type if available (from let binding context), otherwise get from type_map *)
      let enum_type =
        match expected_type with
        | Some t -> t
        | None -> get_type type_map expr
      in
      track_enum_inst state enum_type
  | AST.Match (scrutinee, arms) ->
      collect_insts_expr state type_map env scrutinee;
      (* Track scrutinee enum type if it's an enum *)
      let scrutinee_type = get_type type_map scrutinee in
      track_enum_inst state scrutinee_type;
      List.iter (fun arm -> collect_insts_expr state type_map env arm.AST.body) arms

(* ============================================================
   Code Generation State
   ============================================================ *)

type emit_state = {
  mutable indent : int;
  mono : mono_state;
}

let create_emit_state mono = { indent = 1; mono }
let indent_str state = String.make (state.indent * 4) ' '

(* ============================================================
   Expression Emission
   ============================================================ *)

let rec emit_expr
    ?(expected_type : Types.mono_type option = None)
    (state : emit_state)
    (type_map : Infer.type_map)
    (env : Infer.type_env)
    (expr : AST.expression) : string =
  match expr.expr with
  | AST.Integer i -> Printf.sprintf "int64(%Ld)" i
  | AST.Float f -> Printf.sprintf "float64(%g)" f
  | AST.Boolean true -> "true"
  | AST.Boolean false -> "false"
  | AST.String s -> Printf.sprintf "%S" s
  | AST.Identifier name -> name
  | AST.Prefix (op, operand) -> emit_prefix state type_map env op operand
  | AST.Infix (left, op, right) -> emit_infix state type_map env left op right
  | AST.TypeCheck (expr, type_ann) -> emit_type_check state type_map env expr type_ann
  | AST.If (cond, cons, alt) -> emit_if state type_map env expr cond cons alt
  | AST.Call (func, args) -> emit_call state type_map env func args
  | AST.Array elements -> emit_array state type_map env elements
  | AST.Hash pairs -> emit_hash state type_map env pairs
  | AST.Index (container, index) -> emit_index state type_map env container index
  | AST.Function f ->
      (* Phase 2: Convert params to expressions for backward compat *)
      let param_exprs = List.map (fun (name, _) -> AST.mk_expr (AST.Identifier name)) f.params in
      emit_function_expr state type_map env expr param_exprs f.body
  | AST.EnumConstructor (enum_name, variant_name, args) ->
      (* Use expected_type if available (from let binding context), otherwise get from type_map *)
      let enum_type =
        match expected_type with
        | Some t -> t
        | None -> get_type type_map expr
      in
      let type_args =
        match enum_type with
        | Types.TEnum (_, args) -> args
        | _ -> []
      in
      (* Generate the mangled constructor name *)
      let go_type_name = mangle_type (Types.TEnum (enum_name, type_args)) in
      let constructor_name = Printf.sprintf "%s_%s" go_type_name variant_name in
      (* Emit arguments *)
      let arg_strs = List.map (emit_expr state type_map env) args in
      if arg_strs = [] then
        Printf.sprintf "%s()" constructor_name
      else
        Printf.sprintf "%s(%s)" constructor_name (String.concat ", " arg_strs)
  | AST.Match (scrutinee, arms) -> emit_match state type_map env expr scrutinee arms

(* ============================================================
     Match Expression Codegen
     ============================================================ *)

and emit_match state type_map env match_expr scrutinee arms =
  (* Get the type of the scrutinee *)
  let scrutinee_type = get_type type_map scrutinee in

  (* Get the type of the entire match expression from the type_map *)
  let match_result_type = get_type type_map match_expr in

  match scrutinee_type with
  | Types.TEnum (enum_name, type_args) ->
      (* Enum match: switch on Tag field *)
      emit_match_enum state type_map env scrutinee scrutinee_type enum_name type_args arms match_result_type
  | Types.TInt | Types.TString | Types.TBool ->
      (* Primitive match: switch on value directly *)
      emit_match_primitive state type_map env scrutinee scrutinee_type arms match_result_type
  | _ -> failwith (Printf.sprintf "Match on type %s not yet supported" (Types.to_string scrutinee_type))

and emit_match_enum state type_map env scrutinee scrutinee_type enum_name type_args arms match_result_type =
  (* Generate mangled enum type name *)
  let go_type_name = mangle_type scrutinee_type in

  (* Look up the enum definition *)
  let enum_def_opt = Typecheck.Enum_registry.lookup enum_name in
  let enum_def =
    match enum_def_opt with
    | Some def -> def
    | None -> failwith (Printf.sprintf "Unknown enum: %s" enum_name)
  in

  (* Emit scrutinee to a temporary variable *)
  let scrutinee_str = emit_expr state type_map env scrutinee in

  (* Convert match result type to Go type *)
  let match_result_go_type = type_to_go state.mono match_result_type in

  (* Generate a Go anonymous function that returns the match result *)
  (* This allows match to be used as an expression *)
  let match_body =
    (* For each match arm, generate a case *)
    let cases =
      List.map
        (fun (arm : AST.match_arm) ->
          (* For simplicity, only handle single pattern per arm for now *)
          match arm.patterns with
          | [ pattern ] -> emit_match_arm_enum state type_map env go_type_name enum_def type_args pattern arm.body
          | [] -> failwith "Match arm must have at least one pattern"
          | _ -> failwith "Multiple patterns per arm not yet supported in codegen")
        arms
    in
    String.concat "\n" cases
  in

  (* Check if any arm has a wildcard pattern - if so, no panic needed *)
  let has_wildcard =
    List.exists
      (fun (arm : AST.match_arm) ->
        List.exists
          (fun (pat : AST.pattern) ->
            match pat.pat with
            | AST.PWildcard -> true
            | AST.PVariable _ -> true (* Variable patterns also act as wildcards *)
            | _ -> false)
          arm.patterns)
      arms
  in

  (* Generate: func() T { __scrutinee := <expr>; switch __scrutinee.Tag { cases... } }() *)
  (* Note: If no wildcard, add panic for Go's control flow analysis *)
  if has_wildcard then
    Printf.sprintf "(func() %s {\n\t__scrutinee := %s\n\tswitch __scrutinee.Tag {\n%s\n\t}\n})()"
      match_result_go_type scrutinee_str match_body
  else
    Printf.sprintf
      "(func() %s {\n\t__scrutinee := %s\n\tswitch __scrutinee.Tag {\n%s\n\tdefault:\n\t\tpanic(\"unreachable: exhaustive match\")\n\t}\n})()"
      match_result_go_type scrutinee_str match_body

and emit_match_primitive state type_map env scrutinee scrutinee_type arms match_result_type =
  (* Emit scrutinee to a temporary variable *)
  let scrutinee_str = emit_expr state type_map env scrutinee in

  (* Convert match result type to Go type *)
  let match_result_go_type = type_to_go state.mono match_result_type in

  (* Generate a Go anonymous function that returns the match result *)
  (* This allows match to be used as an expression *)
  let match_body =
    (* For each match arm, generate a case *)
    let cases =
      List.map
        (fun (arm : AST.match_arm) ->
          (* For simplicity, only handle single pattern per arm for now *)
          match arm.patterns with
          | [ pattern ] -> emit_match_arm_primitive state type_map env scrutinee_type pattern arm.body
          | [] -> failwith "Match arm must have at least one pattern"
          | _ -> failwith "Multiple patterns per arm not yet supported in codegen")
        arms
    in
    String.concat "\n" cases
  in

  (* Generate: func() T { __scrutinee := <expr>; switch __scrutinee { cases... } }() *)
  (* Note: Primitive matches have explicit default case from wildcard patterns, no panic needed *)
  Printf.sprintf "(func() %s {\n\t__scrutinee := %s\n\tswitch __scrutinee {\n%s\n\t}\n})()" match_result_go_type
    scrutinee_str match_body

and emit_match_arm_primitive state type_map env _scrutinee_type pattern body =
  match pattern.AST.pat with
  | AST.PWildcard ->
      let body_str = emit_expr state type_map env body in
      Printf.sprintf "\tdefault:\n\t\treturn %s" body_str
  | AST.PVariable var_name ->
      (* Variable pattern binds the scrutinee *)
      let body_str = emit_expr state type_map env body in
      Printf.sprintf "\tdefault:\n\t\t%s := __scrutinee\n\t\treturn %s" var_name body_str
  | AST.PLiteral lit ->
      (* Literal pattern - match exact value *)
      let lit_str =
        match lit with
        | AST.LInt n -> Printf.sprintf "int64(%Ld)" n
        | AST.LString s -> Printf.sprintf "\"%s\"" (String.escaped s)
        | AST.LBool b ->
            if b then
              "true"
            else
              "false"
      in
      let body_str = emit_expr state type_map env body in
      Printf.sprintf "\tcase %s:\n\t\treturn %s" lit_str body_str
  | AST.PConstructor _ -> failwith "Constructor patterns not valid for primitive match"

and emit_match_arm_enum state type_map env go_type_name enum_def type_args pattern body =
  match pattern.AST.pat with
  | AST.PWildcard ->
      (* Wildcard matches everything - use default case *)
      let body_str = emit_expr state type_map env body in
      Printf.sprintf "\tdefault:\n\t\treturn %s" body_str
  | AST.PLiteral _lit ->
      (* Literal patterns - not for enums, shouldn't happen *)
      failwith "Literal patterns not supported for enum match"
  | AST.PVariable _var_name ->
      (* Variable pattern - binds the entire enum value *)
      (* For now, treat as wildcard since we don't track variable bindings in Go codegen *)
      let body_str = emit_expr state type_map env body in
      Printf.sprintf "\tdefault:\n\t\treturn %s" body_str
  | AST.PConstructor (enum_name_pat, variant_name, field_patterns) ->
      (* Constructor pattern - match specific variant and extract fields *)
      (* Find the variant definition *)
      let variant_opt =
        List.find_opt (fun (v : Typecheck.Enum_registry.variant_def) -> v.name = variant_name) enum_def.variants
      in

      let variant =
        match variant_opt with
        | Some v -> v
        | None -> failwith (Printf.sprintf "Unknown variant %s.%s" enum_name_pat variant_name)
      in

      (* Generate case for this tag *)
      let tag_constant = Printf.sprintf "%s_%s_tag" go_type_name variant_name in

      (* Get layout for field mapping *)
      let layout = analyze_enum_layout state.mono enum_def type_args in

      (* Extract field bindings with layout mapping *)
      let bindings = emit_pattern_bindings layout variant_name field_patterns variant.fields in

      (* Generate the case *)
      let binding_strs =
        List.map
          (fun (var_name, data_field_name, _field_type) ->
            Printf.sprintf "\t\t%s := __scrutinee.%s" var_name data_field_name)
          bindings
      in

      let bindings_code =
        if bindings = [] then
          ""
        else
          String.concat "\n" binding_strs ^ "\n"
      in

      (* Emit body with bindings *)
      let body_str = emit_expr state type_map env body in

      Printf.sprintf "\tcase %s:\n%s\t\treturn %s" tag_constant bindings_code body_str

and emit_pattern_bindings
    (layout : enum_layout)
    (variant_name : string)
    (field_patterns : AST.pattern list)
    (_field_types : Types.mono_type list) : (string * string * Types.mono_type) list =
  (* Get the field mapping for this variant *)
  let variant_field_map = List.assoc variant_name layout.variant_maps in

  (* Match patterns with field positions *)
  match field_patterns with
  | [] -> []
  | patterns ->
      List.mapi
        (fun pos pat ->
          match pat.AST.pat with
          | AST.PVariable var_name ->
              (* Find the mapping for this position *)
              let _pos, mapping = List.find (fun (p, _) -> p = pos) variant_field_map in
              Some (var_name, mapping.data_field_name, mapping.data_field_type)
          | AST.PWildcard -> None
          | _ -> failwith "Complex field patterns not yet supported")
        patterns
      |> List.filter_map Fun.id

and emit_prefix state type_map env op operand =
  let operand_str = emit_expr state type_map env operand in
  match op with
  | "!" -> Printf.sprintf "!(%s)" operand_str
  | "-" -> Printf.sprintf "-(%s)" operand_str
  | _ -> failwith ("Unknown prefix operator: " ^ op)

and emit_infix state type_map env left op right =
  let left_str = emit_expr state type_map env left in
  let right_str = emit_expr state type_map env right in
  let go_op =
    match op with
    | "+" -> "+"
    | "-" -> "-"
    | "*" -> "*"
    | "/" -> "/"
    | "<" -> "<"
    | ">" -> ">"
    | "==" -> "=="
    | "!=" -> "!="
    | "<=" -> "<="
    | ">=" -> ">="
    | _ -> failwith ("Unknown infix operator: " ^ op)
  in
  Printf.sprintf "(%s %s %s)" left_str go_op right_str

and emit_type_check state type_map env expr type_ann =
  (* Use runtime helper function for type checking *)
  let expr_str = emit_expr state type_map env expr in
  let check_type = Annotation.type_expr_to_mono_type type_ann in
  let go_type_name =
    match check_type with
    | Types.TInt -> "int64"
    | Types.TString -> "string"
    | Types.TBool -> "bool"
    | Types.TFloat -> "float64"
    | _ -> failwith "Type check for complex types not yet implemented"
  in
  Printf.sprintf "typeIs(%s, \"%s\")" expr_str go_type_name

(* Helper: Substitute identifier references in expressions *)
and substitute_identifier_in_expr old_name new_name expr =
  let rec subst_expr e =
    match e.AST.expr with
    | AST.Identifier id when id = old_name -> { e with expr = AST.Identifier new_name }
    | AST.Prefix (op, operand) -> { e with expr = AST.Prefix (op, subst_expr operand) }
    | AST.Infix (left, op, right) -> { e with expr = AST.Infix (subst_expr left, op, subst_expr right) }
    | AST.TypeCheck (expr, type_ann) -> { e with expr = AST.TypeCheck (subst_expr expr, type_ann) }
    | AST.If (cond, cons, alt) ->
        { e with expr = AST.If (subst_expr cond, subst_stmt cons, Option.map subst_stmt alt) }
    | AST.Call (func, args) -> { e with expr = AST.Call (subst_expr func, List.map subst_expr args) }
    | AST.Array elements -> { e with expr = AST.Array (List.map subst_expr elements) }
    | AST.Hash pairs -> { e with expr = AST.Hash (List.map (fun (k, v) -> (subst_expr k, subst_expr v)) pairs) }
    | AST.Index (container, index) -> { e with expr = AST.Index (subst_expr container, subst_expr index) }
    | AST.Function _ ->
        (* Don't substitute inside function bodies - would need scope tracking *)
        e
    | _ -> e
  and subst_stmt s =
    match s.AST.stmt with
    | AST.Let let_binding -> { s with stmt = AST.Let { let_binding with value = subst_expr let_binding.value } }
    | AST.Return e -> { s with stmt = AST.Return (subst_expr e) }
    | AST.ExpressionStmt e -> { s with stmt = AST.ExpressionStmt (subst_expr e) }
    | AST.Block stmts -> { s with stmt = AST.Block (List.map subst_stmt stmts) }
    | AST.EnumDef _ -> s (* Enum defs don't contain expressions to substitute *)
    | AST.TraitDef _ | AST.ImplDef _ | AST.DeriveDef _ -> s (* Trait defs don't contain expressions *)
  in
  subst_expr expr

(* Helper: Compute complement type (all union members except narrow_type) *)
and compute_complement_type (current_type : Types.mono_type) (narrow_type : Types.mono_type) :
    Types.mono_type option =
  match current_type with
  | Types.TUnion members -> (
      let remaining = List.filter (fun t -> t <> narrow_type) members in
      match remaining with
      | [] -> None (* No members left *)
      | [ single ] -> Some single (* Single type remains *)
      | multiple -> Some (Types.TUnion multiple)
      (* Multiple types remain *))
  | _ when current_type = narrow_type -> None (* Narrowing to same type, no complement *)
  | _ -> Some current_type (* Not a union, complement is the whole type *)

(* Emit a Go type switch for type narrowing *)
and emit_type_switch state type_map env _if_expr var_name narrow_type complement_type_opt cons alt result_type =
  let result_type_str = type_to_go state.mono result_type in
  let narrow_type_str = type_to_go state.mono narrow_type in
  let ind = indent_str state in
  let inner_ind = ind ^ "    " in

  (* Create narrowed variable name *)
  let narrowed_var = var_name ^ "_typed" in

  (* Emit branch with variable substitution *)
  let emit_branch_with_var var_to_use narrow_env stmt =
    (* Always add unused variable marker to avoid Go compiler warnings *)
    let var_marker = inner_ind ^ "        _ = " ^ var_to_use ^ "\n" in
    match stmt.AST.stmt with
    | AST.Block stmts -> (
        match List.rev stmts with
        | [] ->
            var_marker ^ inner_ind ^ "        return "
            ^ (if result_type = Types.TNull then
                 "struct{}{}"
               else
                 "nil")
            ^ "\n"
        | last :: rest ->
            let prefix, env' = emit_stmts state type_map narrow_env (List.rev rest) in
            let last_str =
              match last.stmt with
              | AST.ExpressionStmt e ->
                  (* Substitute variable references in expression before emitting *)
                  let e_subst = substitute_identifier_in_expr var_name var_to_use e in
                  inner_ind ^ "        return " ^ emit_expr state type_map env' e_subst ^ "\n"
              | AST.Return e ->
                  let e_subst = substitute_identifier_in_expr var_name var_to_use e in
                  inner_ind ^ "        return " ^ emit_expr state type_map env' e_subst ^ "\n"
              | _ -> fst (emit_stmt state type_map env' last)
            in
            var_marker ^ prefix ^ last_str)
    | AST.ExpressionStmt e ->
        let e_subst = substitute_identifier_in_expr var_name var_to_use e in
        var_marker ^ inner_ind ^ "        return " ^ emit_expr state type_map narrow_env e_subst ^ "\n"
    | _ -> var_marker ^ fst (emit_stmt state type_map narrow_env stmt)
  in

  (* Emit branch with complement type assertion if needed *)
  let emit_branch_with_complement var_to_use original_var complement_type_opt narrow_env stmt =
    (* If we have a complement type, we need to emit type assertion in default case *)
    let type_assertion_prefix =
      match complement_type_opt with
      | Some complement_type when complement_type <> Types.TUnion [] ->
          let complement_type_str = type_to_go state.mono complement_type in
          inner_ind ^ "        " ^ var_to_use ^ " := " ^ original_var ^ ".(" ^ complement_type_str ^ ")\n"
      | _ -> ""
    in
    (* Always add unused variable marker if we declared it *)
    let var_marker =
      match complement_type_opt with
      | Some _ when complement_type_opt <> Some (Types.TUnion []) ->
          inner_ind ^ "        _ = " ^ var_to_use ^ "\n"
      | _ -> ""
    in
    match stmt.AST.stmt with
    | AST.Block stmts -> (
        match List.rev stmts with
        | [] ->
            type_assertion_prefix ^ var_marker ^ inner_ind ^ "        return "
            ^ (if result_type = Types.TNull then
                 "struct{}{}"
               else
                 "nil")
            ^ "\n"
        | last :: rest ->
            let prefix, env' = emit_stmts state type_map narrow_env (List.rev rest) in
            let last_str =
              match last.stmt with
              | AST.ExpressionStmt e ->
                  (* Substitute variable references in expression before emitting *)
                  let e_subst = substitute_identifier_in_expr original_var var_to_use e in
                  inner_ind ^ "        return " ^ emit_expr state type_map env' e_subst ^ "\n"
              | AST.Return e ->
                  let e_subst = substitute_identifier_in_expr original_var var_to_use e in
                  inner_ind ^ "        return " ^ emit_expr state type_map env' e_subst ^ "\n"
              | _ -> fst (emit_stmt state type_map env' last)
            in
            type_assertion_prefix ^ var_marker ^ prefix ^ last_str)
    | AST.ExpressionStmt e ->
        let e_subst = substitute_identifier_in_expr original_var var_to_use e in
        type_assertion_prefix
        ^ var_marker
        ^ inner_ind
        ^ "        return "
        ^ emit_expr state type_map narrow_env e_subst
        ^ "\n"
    | _ -> type_assertion_prefix ^ var_marker ^ fst (emit_stmt state type_map narrow_env stmt)
  in

  (* Emit consequence branch with narrowed variable *)
  let narrowed_env = Infer.TypeEnv.add narrowed_var (Types.Forall ([], narrow_type)) env in
  let cons_str = emit_branch_with_var narrowed_var narrowed_env cons in

  (* Emit alternative branch with complement type assertion if available *)
  let alt_str =
    match alt with
    | Some alt_stmt ->
        (* Create complement variable name and environment if we have a complement type *)
        let complement_var = var_name ^ "_complement" in
        let complement_env =
          match complement_type_opt with
          | Some complement_type -> Infer.TypeEnv.add complement_var (Types.Forall ([], complement_type)) env
          | None -> env
        in
        emit_branch_with_complement complement_var var_name complement_type_opt complement_env alt_stmt
    | None ->
        inner_ind ^ "        return "
        ^ (if result_type = Types.TNull then
             "struct{}{}"
           else
             "nil")
        ^ "\n"
  in

  (* Generate type switch *)
  Printf.sprintf "func() %s {\n%s    switch %s := %s.(type) {\n%s    case %s:\n%s%s    default:\n%s%s    }\n%s}()"
    result_type_str ind narrowed_var var_name ind narrow_type_str cons_str ind alt_str ind ind

and emit_if state type_map env if_expr cond cons alt =
  let result_type = get_type type_map if_expr in
  let result_type_str = type_to_go state.mono result_type in
  let ind = indent_str state in
  let inner_ind = ind ^ "    " in

  (* Check if condition is a type check (x is T) *)
  let type_check_info =
    match cond.expr with
    | AST.TypeCheck (var_expr, type_ann) -> (
        match var_expr.expr with
        | AST.Identifier var_name ->
            let narrow_type = Annotation.type_expr_to_mono_type type_ann in
            Some (var_name, narrow_type)
        | _ -> None)
    | _ -> None
  in

  match type_check_info with
  | Some (var_name, narrow_type) ->
      (* Look up the original type of the variable to compute complement *)
      let complement_type_opt =
        match Infer.TypeEnv.find_opt var_name env with
        | Some (Types.Forall ([], original_type)) -> compute_complement_type original_type narrow_type
        | _ -> None
      in
      (* Emit type switch instead of if *)
      emit_type_switch state type_map env if_expr var_name narrow_type complement_type_opt cons alt result_type
  | None ->
      (* Normal if-expression *)
      let cond_str = emit_expr state type_map env cond in

      let emit_branch stmt =
        match stmt.AST.stmt with
        | AST.Block stmts -> (
            match List.rev stmts with
            | [] ->
                inner_ind ^ "    return "
                ^ (if result_type = Types.TNull then
                     "struct{}{}"
                   else
                     "nil")
                ^ "\n"
            | last :: rest ->
                let prefix, env' = emit_stmts state type_map env (List.rev rest) in
                let last_str =
                  match last.stmt with
                  | AST.ExpressionStmt e -> inner_ind ^ "    return " ^ emit_expr state type_map env' e ^ "\n"
                  | AST.Return e -> inner_ind ^ "    return " ^ emit_expr state type_map env' e ^ "\n"
                  | _ -> fst (emit_stmt state type_map env' last)
                in
                prefix ^ last_str)
        | AST.ExpressionStmt e -> inner_ind ^ "    return " ^ emit_expr state type_map env e ^ "\n"
        | _ -> fst (emit_stmt state type_map env stmt)
      in

      let cons_str = emit_branch cons in
      let alt_str =
        match alt with
        | Some alt_stmt -> emit_branch alt_stmt
        | None ->
            inner_ind ^ "    return "
            ^ (if result_type = Types.TNull then
                 "struct{}{}"
               else
                 "nil")
            ^ "\n"
      in

      Printf.sprintf "func() %s {\n%s    if %s {\n%s%s    } else {\n%s%s    }\n%s}()" result_type_str ind cond_str
        cons_str ind alt_str ind ind

and emit_call state type_map env func args =
  (* Check for builtin function calls that need special handling *)
  match func.expr with
  | AST.Identifier "len" when List.length args = 1 ->
      let arg_str = emit_expr state type_map env (List.hd args) in
      Printf.sprintf "int64(len(%s))" arg_str
  | AST.Identifier "puts" ->
      let args_str = List.map (emit_expr state type_map env) args |> String.concat ", " in
      Printf.sprintf "puts(%s)" args_str
  | AST.Identifier "first" when List.length args = 1 ->
      let arg_str = emit_expr state type_map env (List.hd args) in
      Printf.sprintf "first(%s)" arg_str
  | AST.Identifier "last" when List.length args = 1 ->
      let arg_str = emit_expr state type_map env (List.hd args) in
      Printf.sprintf "last(%s)" arg_str
  | AST.Identifier "rest" when List.length args = 1 ->
      let arg_str = emit_expr state type_map env (List.hd args) in
      Printf.sprintf "rest(%s)" arg_str
  | AST.Identifier "push" when List.length args = 2 ->
      let arr_str = emit_expr state type_map env (List.hd args) in
      let val_str = emit_expr state type_map env (List.nth args 1) in
      Printf.sprintf "push(%s, %s)" arr_str val_str
  | AST.Identifier name when is_user_func state.mono name ->
      (* User-defined function - look up declared parameter types to check for unions *)
      let func_param_types =
        match Infer.TypeEnv.find_opt name env with
        | Some (Types.Forall (_, func_type)) ->
            let num_args = List.length args in
            let declared_param_types, _ = extract_param_types num_args func_type in
            declared_param_types
        | None -> List.map (get_type type_map) args
      in
      (* Check if any declared param is a union type *)
      let has_union_param =
        List.exists
          (function
            | Types.TUnion _ -> true
            | _ -> false)
          func_param_types
      in
      (* If function has union params, use declared types for name mangling; otherwise use argument types *)
      let param_types =
        if has_union_param then
          func_param_types
        else
          List.map (get_type type_map) args
      in
      let mangled_name = mangle_func_name name param_types in
      let args_str = List.map (emit_expr state type_map env) args |> String.concat ", " in
      Printf.sprintf "%s(%s)" mangled_name args_str
  | _ ->
      let func_str = emit_expr state type_map env func in
      let args_str = List.map (emit_expr state type_map env) args |> String.concat ", " in
      Printf.sprintf "%s(%s)" func_str args_str

and emit_array state type_map env elements =
  match elements with
  | [] -> "[]interface{}{}"
  | first :: _ ->
      let elem_type = get_type type_map first in
      let elem_type_str = type_to_go state.mono elem_type in
      let elems_str = List.map (emit_expr state type_map env) elements |> String.concat ", " in
      Printf.sprintf "[]%s{%s}" elem_type_str elems_str

and emit_hash state type_map env pairs =
  match pairs with
  | [] -> "map[interface{}]interface{}{}"
  | (first_key, first_val) :: _ ->
      let key_type = get_type type_map first_key in
      let val_type = get_type type_map first_val in
      let pairs_str =
        List.map (fun (k, v) -> emit_expr state type_map env k ^ ": " ^ emit_expr state type_map env v) pairs
        |> String.concat ", "
      in
      Printf.sprintf "map[%s]%s{%s}" (type_to_go state.mono key_type) (type_to_go state.mono val_type) pairs_str

and emit_index state type_map env container index =
  let container_str = emit_expr state type_map env container in
  let container_type = get_type type_map container in
  let index_str = emit_expr state type_map env index in
  (* Check for literal integer (positive or negative via prefix) *)
  let literal_int =
    match index.expr with
    | AST.Integer i -> Some i
    | AST.Prefix ("-", { expr = AST.Integer i; _ }) -> Some (Int64.neg i)
    | _ -> None
  in
  match container_type with
  | Types.TString -> (
      match literal_int with
      (* Literal positive: direct access *)
      | Some i when i >= 0L -> Printf.sprintf "string(%s[%Ld])" container_str i
      (* Literal negative: transform to len-based *)
      | Some i -> Printf.sprintf "string(%s[len(%s)%Ld])" container_str container_str i
      (* Variable: use runtime helper *)
      | None -> Printf.sprintf "indexStr(%s, %s)" container_str index_str)
  | Types.TArray _ -> (
      match literal_int with
      (* Literal positive: direct access *)
      | Some i when i >= 0L -> Printf.sprintf "%s[%Ld]" container_str i
      (* Literal negative: transform to len-based *)
      | Some i -> Printf.sprintf "%s[len(%s)%Ld]" container_str container_str i
      (* Variable: use runtime helper *)
      | None -> Printf.sprintf "indexArr(%s, %s)" container_str index_str)
  | Types.THash _ -> Printf.sprintf "%s[%s]" container_str index_str
  | _ -> Printf.sprintf "%s[%s]" container_str index_str

and emit_function_expr state type_map env func_expr params body =
  (* Inline anonymous function - no monomorphization needed *)
  let func_type = get_type type_map func_expr in
  let param_types, return_type = extract_param_types (List.length params) func_type in

  let param_names =
    List.map
      (fun (p : AST.expression) ->
        match p.expr with
        | AST.Identifier name -> name
        | _ -> failwith "Function parameter must be identifier")
      params
  in

  let params_with_types =
    List.map2 (fun name typ -> name ^ " " ^ type_to_go state.mono typ) param_names param_types
  in
  let params_str = String.concat ", " params_with_types in
  let return_type_str = type_to_go state.mono return_type in

  (* Extend environment with parameter bindings for the body *)
  let body_env =
    List.fold_left2
      (fun acc name typ -> Infer.TypeEnv.add name (Types.Forall ([], typ)) acc)
      env param_names param_types
  in

  let body_str = emit_func_body state type_map body_env body in

  Printf.sprintf "func(%s) %s {\n%s%s}" params_str return_type_str body_str (indent_str state)

and emit_func_body state type_map env stmt =
  let ind = indent_str state in
  let inner_ind = ind ^ "    " in
  match stmt.AST.stmt with
  | AST.Block stmts -> (
      match List.rev stmts with
      | [] -> inner_ind ^ "return\n"
      | last :: rest ->
          let prefix, env' = emit_stmts state type_map env (List.rev rest) in
          let last_str =
            match last.stmt with
            | AST.ExpressionStmt e -> inner_ind ^ "return " ^ emit_expr state type_map env' e ^ "\n"
            | AST.Return e -> inner_ind ^ "return " ^ emit_expr state type_map env' e ^ "\n"
            | _ -> fst (emit_stmt state type_map env' last)
          in
          prefix ^ last_str)
  | AST.ExpressionStmt e -> inner_ind ^ "return " ^ emit_expr state type_map env e ^ "\n"
  | _ -> fst (emit_stmt state type_map env stmt)

(* ============================================================
   Statement Emission
   ============================================================ *)

and emit_stmt (state : emit_state) (type_map : Infer.type_map) (env : Infer.type_env) (stmt : AST.statement) :
    string * Infer.type_env =
  let ind = indent_str state in
  match stmt.stmt with
  | AST.Let let_binding -> (
      (* Use the type from the environment if it exists, otherwise get from type_map *)
      let expr_type =
        match Infer.TypeEnv.find_opt let_binding.name env with
        | Some (Types.Forall (_, t)) -> t
        | None -> get_type type_map let_binding.value
      in
      match let_binding.value.expr with
      | AST.Function _ when is_user_func state.mono let_binding.name ->
          (* Skip - this is a top-level function, emitted separately *)
          let env' = Infer.TypeEnv.add let_binding.name (Types.Forall ([], expr_type)) env in
          ("", env')
      | _ ->
          (* Pass expected_type so EnumConstructors get the correct concrete type *)
          let expr_str = emit_expr ~expected_type:(Some expr_type) state type_map env let_binding.value in
          let env' = Infer.TypeEnv.add let_binding.name (Types.Forall ([], expr_type)) env in
          (Printf.sprintf "%s%s := %s\n" ind let_binding.name expr_str, env'))
  | AST.Return expr ->
      let expr_str = emit_expr state type_map env expr in
      (Printf.sprintf "%sreturn %s\n" ind expr_str, env)
  | AST.ExpressionStmt expr -> (
      match expr.expr with
      (* Handle if statements in statement context *)
      | AST.If (cond, cons, alt) ->
          let cond_str = emit_expr state type_map env cond in
          let cons_code, _ = emit_stmt state type_map env cons in
          let code =
            match alt with
            | None -> Printf.sprintf "%sif %s {\n%s%s}\n" ind cond_str cons_code ind
            | Some alt_stmt ->
                let alt_code, _ = emit_stmt state type_map env alt_stmt in
                Printf.sprintf "%sif %s {\n%s%s} else {\n%s%s}\n" ind cond_str cons_code ind alt_code ind
          in
          (code, env)
      | _ ->
          let expr_str = emit_expr state type_map env expr in
          (Printf.sprintf "%s_ = %s\n" ind expr_str, env))
  | AST.Block stmts ->
      let code, env' = emit_stmts state type_map env stmts in
      (code, env')
  | AST.EnumDef _ ->
      (* Enum definitions are compile-time only *)
      ("", env)
  | AST.TraitDef _ | AST.ImplDef _ | AST.DeriveDef _ ->
      (* Trait definitions/impls/derives are compile-time only *)
      ("", env)

and emit_stmts state type_map env stmts =
  let rec loop env acc = function
    | [] -> (String.concat "" (List.rev acc), env)
    | stmt :: rest ->
        let code, env' = emit_stmt state type_map env stmt in
        loop env' (code :: acc) rest
  in
  loop env [] stmts

(* ============================================================
   Generate Monomorphized Functions
   ============================================================ *)

let emit_specialized_func (state : emit_state) (inst : instantiation) : string =
  (* Find the function definition *)
  let func_def = List.find (fun fd -> fd.name = inst.func_name) state.mono.func_defs in

  let param_names =
    List.map
      (fun (p : AST.expression) ->
        match p.expr with
        | AST.Identifier name -> name
        | _ -> failwith "Function parameter must be identifier")
      func_def.params
  in

  let mangled_name = mangle_func_name inst.func_name inst.concrete_types in

  let params_with_types =
    List.map2 (fun name typ -> name ^ " " ^ type_to_go state.mono typ) param_names inst.concrete_types
  in
  let params_str = String.concat ", " params_with_types in
  let return_type_str = type_to_go state.mono inst.return_type in

  (* Create env with parameter types for the body *)
  let base_env = Typecheck.Builtins.prelude_env () in
  (* Build the concrete function type from param types and return type *)
  let func_type =
    List.fold_right (fun param_t acc -> Types.TFun (param_t, acc)) inst.concrete_types inst.return_type
  in
  (* Add the function itself for recursive calls *)
  let env_with_func = Infer.TypeEnv.add inst.func_name (Types.Forall ([], func_type)) base_env in
  let body_env =
    List.fold_left2
      (fun acc name typ -> Infer.TypeEnv.add name (Types.Forall ([], typ)) acc)
      env_with_func param_names inst.concrete_types
  in

  (* Create a type_map for the function body by running inference *)
  let func_body_type_map = Infer.create_type_map () in
  let _ = Infer.infer_statement func_body_type_map body_env func_def.body in

  (* Save and reset indent for top-level function *)
  let saved_indent = state.indent in
  state.indent <- 0;
  let body_str = emit_func_body state func_body_type_map body_env func_def.body in
  state.indent <- saved_indent;

  Printf.sprintf "func %s(%s) %s {\n%s}\n" mangled_name params_str return_type_str body_str

(* ============================================================
   Enum Type Generation
   ============================================================ *)

(* Generate Go struct and constructors for an enum type *)
(* Returns: (generated_code, needs_fmt_import) *)
let emit_enum_type (state : mono_state) (enum_name : string) (type_args : Types.mono_type list) : string * bool =
  let go_type_name = mangle_type (Types.TEnum (enum_name, type_args)) in

  (* Look up enum definition *)
  match Typecheck.Enum_registry.lookup enum_name with
  | None -> ("", false)
  | Some enum_def ->
      (* Analyze layout for multi-field and heterogeneous support *)
      let layout = analyze_enum_layout state enum_def type_args in

      (* Generate struct type with optimal field ordering *)
      let struct_def =
        if layout.fields = [] then
          (* Nullary enum - only Tag field *)
          Printf.sprintf "type %s struct {\n\tTag int8\n}\n\n" go_type_name
        else
          (* Generate DataN fields, sorted by size (already sorted in layout) *)
          let fields_str =
            List.map (fun fm -> Printf.sprintf "\t%s %s" fm.data_field_name fm.go_type) layout.fields
            |> String.concat "\n"
          in
          Printf.sprintf "type %s struct {\n%s\n\tTag int8\n}\n\n" go_type_name fields_str
      in

      (* Generate tag constants *)
      let tag_constants =
        String.concat "\n"
          (List.mapi
             (fun i (v : Typecheck.Enum_registry.variant_def) ->
               Printf.sprintf "const %s_%s_tag = %d" go_type_name v.name i)
             enum_def.variants)
        ^ "\n\n"
      in

      (* Generate constructors for each variant *)
      let constructors =
        String.concat "\n"
          (List.map
             (fun (v : Typecheck.Enum_registry.variant_def) ->
               let constructor_name = Printf.sprintf "%s_%s" go_type_name v.name in
               if v.fields = [] then
                 (* Nullary constructor *)
                 Printf.sprintf "func %s() %s {\n\treturn %s{Tag: %s_%s_tag}\n}\n" constructor_name go_type_name
                   go_type_name go_type_name v.name
               else
                 (* Constructor with fields - use layout mapping *)
                 let subst = List.combine enum_def.type_params type_args in
                 let field_types = List.map (Types.apply_substitution subst) v.fields in
                 let params =
                   List.mapi (fun i t -> Printf.sprintf "v%d %s" i (type_to_go state t)) field_types
                   |> String.concat ", "
                 in

                 (* Get field mappings for this variant *)
                 let variant_field_map = List.assoc v.name layout.variant_maps in
                 let field_inits =
                   List.map
                     (fun (pos, mapping) -> Printf.sprintf "%s: v%d" mapping.data_field_name pos)
                     variant_field_map
                   |> String.concat ", "
                 in

                 Printf.sprintf "func %s(%s) %s {\n\treturn %s{Tag: %s_%s_tag, %s}\n}\n" constructor_name params
                   go_type_name go_type_name go_type_name v.name field_inits)
             enum_def.variants)
        ^ "\n"
      in

      (* Generate String() method for pretty printing *)
      let string_method =
        let cases =
          List.map
            (fun (v : Typecheck.Enum_registry.variant_def) ->
              let variant_field_map =
                if List.mem_assoc v.name layout.variant_maps then
                  List.assoc v.name layout.variant_maps
                else
                  []
              in
              if v.fields = [] then
                (* Nullary variant - just print name *)
                Printf.sprintf "\tcase %s_%s_tag:\n\t\treturn \"%s\"" go_type_name v.name v.name
              else
                (* Variant with fields - print name and fields *)
                let field_formats = List.map (fun _ -> "%v") v.fields |> String.concat ", " in
                let field_args =
                  List.map (fun (_, mapping) -> Printf.sprintf "e.%s" mapping.data_field_name) variant_field_map
                  |> String.concat ", "
                in
                Printf.sprintf "\tcase %s_%s_tag:\n\t\treturn fmt.Sprintf(\"%s(%s)\", %s)" go_type_name v.name
                  v.name field_formats field_args)
            enum_def.variants
          |> String.concat "\n"
        in
        Printf.sprintf
          "func (e %s) String() string {\n\tswitch e.Tag {\n%s\n\tdefault:\n\t\treturn \"<unknown>\"\n\t}\n}\n\n"
          go_type_name cases
      in

      (* Check if any variant has fields (needs fmt.Sprintf) *)
      let needs_fmt =
        List.exists (fun (v : Typecheck.Enum_registry.variant_def) -> v.fields <> []) enum_def.variants
      in
      (struct_def ^ tag_constants ^ constructors ^ string_method, needs_fmt)

(* ============================================================
    Program Emission
    ============================================================ *)

let emit_program_with_typed_env (type_map : Infer.type_map) (typed_env : Infer.type_env) (program : AST.program) :
    string =
  let mono_state = create_mono_state () in

  (* Pass 1: Collect function definitions *)
  List.iter (collect_funcs_stmt mono_state) program;

  (* Pass 2: Collect instantiations using the already-typed environment and type_map *)
  ignore (List.fold_left (collect_insts_stmt mono_state type_map) typed_env program);

  let emit_state = create_emit_state mono_state in

  (* Generate enum types *)
  let enum_results =
    EnumInstSet.elements mono_state.enum_insts
    |> List.map (fun (name, args) -> emit_enum_type mono_state name args)
  in
  let enum_types = List.map fst enum_results |> String.concat "\n" in
  let needs_fmt = List.exists snd enum_results in

  (* Generate specialized functions *)
  let specialized_funcs =
    InstSet.elements mono_state.instantiations
    |> List.map (emit_specialized_func emit_state)
    |> String.concat "\n"
  in

  (* Emit main body *)
  let main_body, _ = emit_stmts emit_state type_map typed_env program in

  (* Build final output *)
  let has_enums = enum_types <> "" in
  let imports =
    if has_enums && needs_fmt then
      "import \"fmt\"\n\n"
    else
      ""
  in
  let type_defs =
    if enum_types = "" then
      ""
    else
      enum_types ^ "\n"
  in
  let top_funcs =
    if specialized_funcs = "" then
      ""
    else
      specialized_funcs ^ "\n"
  in

  Printf.sprintf "package main\n\n%s%s%sfunc main() {\n%s}\n" imports type_defs top_funcs main_body

let emit_program (program : AST.program) : string =
  let base_env = Typecheck.Builtins.prelude_env () in
  match Infer.infer_program ~env:base_env program with
  | Error _ ->
      (* If inference fails, create an empty type_map *)
      let empty_type_map = Infer.create_type_map () in
      emit_program_with_typed_env empty_type_map base_env program
  | Ok (typed_env, type_map, _result_type) -> emit_program_with_typed_env type_map typed_env program

(* ============================================================
   Runtime
   ============================================================ *)

let runtime_go =
  {|// Marmoset Runtime - builtin functions for generated Go code
package main

import (
	"fmt"
	"reflect"
)

// puts prints values to stdout, returns struct{}
func puts[T any](v T) struct{} {
	fmt.Println(v)
	return struct{}{}
}

// typeIs checks if a value is of a specific Go type
func typeIs[T any](v T, typeName string) bool {
	return reflect.TypeOf(v).String() == typeName
}

// indexArr handles negative indexing for arrays
func indexArr[T any](arr []T, i int64) T {
	if i < 0 {
		i = int64(len(arr)) + i
	}
	return arr[i]
}

// indexStr handles negative indexing for strings
func indexStr(s string, i int64) string {
	if i < 0 {
		i = int64(len(s)) + i
	}
	return string(s[i])
}

// first returns the first element of an array, or zero value if empty
func first[T any](arr []T) T {
	if len(arr) == 0 {
		var zero T
		return zero
	}
	return arr[0]
}

// last returns the last element of an array, or zero value if empty
func last[T any](arr []T) T {
	if len(arr) == 0 {
		var zero T
		return zero
	}
	return arr[len(arr)-1]
}

// rest returns all but the first element, or empty array if empty
func rest[T any](arr []T) []T {
	if len(arr) == 0 {
		return []T{}
	}
	return arr[1:]
}

// push appends an element to an array and returns the new array
func push[T any](arr []T, v T) []T {
	return append(arr, v)
}
|}

(* ============================================================
    Main Entry Point
    ============================================================ *)

let compile_string (source : string) : (string, string) result =
  match Syntax.Parser.parse source with
  | Error errors -> Error ("Parse error: " ^ String.concat ", " errors)
  | Ok program -> (
      let env = Typecheck.Builtins.prelude_env () in
      match Typecheck.Checker.check_program_with_annotations ~env program with
      | Error err -> Error ("Type error: " ^ Typecheck.Checker.format_error err)
      | Ok { environment = typed_env; type_map; _ } -> Ok (emit_program_with_typed_env type_map typed_env program)
      )

type build_output = {
  main_go : string;
  runtime_go : string;
}

let compile_to_build (source : string) : (build_output, string) result =
  match compile_string source with
  | Error e -> Error e
  | Ok main_go -> Ok { main_go; runtime_go }

let get_runtime () = runtime_go

(* ============================================================
   Tests
   ============================================================ *)

let string_contains s substring =
  let len_sub = String.length substring in
  let len_s = String.length s in
  if len_sub > len_s then
    false
  else
    let rec check i =
      if i + len_sub > len_s then
        false
      else if String.sub s i len_sub = substring then
        true
      else
        check (i + 1)
    in
    check 0

let%test "emit integer" =
  match compile_string "42" with
  | Ok code -> string_contains code "int64(42)"
  | Error _ -> false

let%test "emit addition" =
  match compile_string "1 + 2" with
  | Ok code -> string_contains code "(int64(1) + int64(2))"
  | Error _ -> false

let%test "emit let binding" =
  match compile_string "let x = 5; x" with
  | Ok code -> string_contains code "x := int64(5)" && string_contains code "_ = x"
  | Error _ -> false

let%test "emit boolean" =
  match compile_string "true" with
  | Ok code -> string_contains code "_ = true"
  | Error _ -> false

let%test "emit string" =
  match compile_string "\"hello\"" with
  | Ok code -> string_contains code "\"hello\""
  | Error _ -> false

let%test "emit comparison" =
  match compile_string "1 < 2" with
  | Ok code -> string_contains code "(int64(1) < int64(2))"
  | Error _ -> false

let%test "monomorphized function" =
  match compile_string "let double = fn(x) { x * 2 }; double(5)" with
  | Ok code ->
      (* Should have top-level func double_int64 *)
      string_contains code "func double_int64(x int64) int64"
      &&
      (* Call should use mangled name *)
      string_contains code "double_int64(int64(5))"
  | Error _ -> false

let%test "polymorphic function multiple instantiations" =
  match compile_string "let id = fn(x) { x }; id(5); id(true)" with
  | Ok code ->
      string_contains code "func id_int64(x int64) int64"
      && string_contains code "func id_bool(x bool) bool"
      && string_contains code "id_int64(int64(5))"
      && string_contains code "id_bool(true)"
  | Error _ -> false

let%test "emit array index with literal" =
  match compile_string "let a = [1,2,3]; a[0]" with
  | Ok code -> string_contains code "a[0]" && not (string_contains code "int(")
  | Error _ -> false

let%test "emit array index with variable" =
  match compile_string "let a = [1,2,3]; let i = 1; a[i]" with
  | Ok code -> string_contains code "indexArr(a, i)"
  | Error _ -> false

let%test "emit if inside function body" =
  match compile_string "let abs = fn(x) { if (x < 0) { -x } else { x } }; abs(5)" with
  | Ok code -> string_contains code "func abs_int64(x int64) int64" && string_contains code "if (x < int64(0))"
  | Error _ -> false

let%test "emit recursive function" =
  match compile_string "let fact = fn(n) { if (n < 2) { 1 } else { n * fact(n - 1) } }; fact(5)" with
  | Ok code ->
      string_contains code "func fact_int64(n int64) int64" && string_contains code "fact_int64((n - int64(1)))"
  | Error _ -> false

let%test "emit closure (function returning function)" =
  match compile_string "let make_adder = fn(x) { fn(y) { x + y } }; let add5 = make_adder(5); add5(10)" with
  | Ok code ->
      string_contains code "func make_adder_int64(x int64) func(int64) int64"
      && string_contains code "return func(y int64) int64"
  | Error _ -> false

let%test "emit string indexing returns string" =
  match compile_string "let s = \"hello\"; s[0]" with
  | Ok code -> string_contains code "string(s[0])"
  | Error _ -> false

let%test "emit negative array index" =
  match compile_string "let a = [1,2,3]; a[-1]" with
  | Ok code -> string_contains code "a[len(a)-1]"
  | Error _ -> false

let%test "emit negative string index" =
  match compile_string "let s = \"hello\"; s[-2]" with
  | Ok code -> string_contains code "s[len(s)-2]"
  | Error _ -> false

let%test "if statement without else in function followed by return" =
  match compile_string "let fib = fn(n) { if (n < 2) { return n }; return n + 1 }; fib(5)" with
  | Ok code ->
      (* Should emit as regular if statement, not wrapped in function *)
      string_contains code "if (n < int64(2))"
      && (not (string_contains code "func() int64"))
      && string_contains code "return n"
  | Error _ -> false

let%test "if statement with else in function followed by return" =
  match compile_string "let test = fn(x) { if (x > 0) { return 1 } else { return -1 }; return 0 }; test(5)" with
  | Ok code ->
      (* Should compile without nil errors *)
      string_contains code "if (x > int64(0))" && string_contains code "} else {"
  | Error _ -> false

let%test "fibonacci with if statement and subsequent return" =
  match compile_string "let fib = fn(n) { if (n < 2) { return n }; return fib(n - 1) + fib(n - 2) }; fib(10)" with
  | Ok code ->
      string_contains code "func fib_int64(n int64) int64"
      && string_contains code "if (n < int64(2))"
      && not (string_contains code "return nil")
  | Error _ -> false
