(* Go code generation from Marmoset AST with monomorphization *)

module AST = Syntax.Ast.AST
module Types = Typecheck.Types
module Infer = Typecheck.Infer
module Annotation = Typecheck.Annotation
module Unify = Typecheck.Unify

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
  | Types.TVar name ->
      failwith
        (Printf.sprintf
           "Codegen error: unresolved type variable '%s' reached mangle_type. All type variables must be resolved \
            before code generation."
           name)
  | Types.TFun (arg, ret, _) -> "fn_" ^ mangle_type arg ^ "_" ^ mangle_type ret
  | Types.TArray elem -> "arr_" ^ mangle_type elem
  | Types.THash (key, value) -> "map_" ^ mangle_type key ^ "_" ^ mangle_type value
  | Types.TRecord (fields, row) ->
      let fields = Types.normalize_record_fields fields in
      let field_bits =
        fields
        |> List.map (fun (f : Types.record_field_type) -> f.name ^ "_" ^ mangle_type f.typ)
        |> String.concat "_"
      in
      let row_bit =
        match row with
        | None -> "closed"
        | Some _ -> "open"
      in
      "record_" ^ field_bits ^ "_" ^ row_bit
  | Types.TRowVar name -> "row_" ^ name
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

let fingerprint_types (types : Types.mono_type list) : string =
  if types = [] then
    "unit"
  else
    String.concat "__" (List.map mangle_type types)

(* ============================================================
   Function Registry - track function definitions and instantiations
   ============================================================ *)

(* A function definition: name, params, body, and its polymorphic type *)
type func_def = {
  name : string;
  params : AST.expression list;
  body : AST.statement;
  func_expr_id : int;
  poly_type : Types.poly_type;
  captures : string list; (* variables captured from outer scope - for closures *)
}

(* An instantiation: concrete types for a polymorphic function *)
type instantiation = {
  func_name : string;
  module_path : string; (* Future-proof symbol namespace for modules *)
  func_expr_id : int; (* Stable symbol identity within a program *)
  func_arity : int;
  concrete_only_mode : bool;
  concrete_types : Types.mono_type list; (* param types *)
  type_fingerprint : string;
  return_type : Types.mono_type;
}

module InstSet = Set.Make (struct
  type t = instantiation

  let compare a b =
    let c = String.compare a.module_path b.module_path in
    if c <> 0 then
      c
    else
      let c = compare a.func_expr_id b.func_expr_id in
      if c <> 0 then
        c
      else
        let c = String.compare a.func_name b.func_name in
        if c <> 0 then
          c
        else
          let c = compare a.func_arity b.func_arity in
          if c <> 0 then
            c
        else
          let c = compare a.concrete_only_mode b.concrete_only_mode in
          if c <> 0 then
            c
          else
            String.compare a.type_fingerprint b.type_fingerprint
end)

type impl_instantiation = {
  trait_name : string;
  method_name : string;
  module_path : string;
  concrete_only_mode : bool;
  for_type : Types.mono_type;
  type_fingerprint : string;
}

type impl_inst_payload = {
  param_names : string list;
  param_types : Types.mono_type list;
  return_type : Types.mono_type;
  body_expr : AST.expression;
}

type impl_template_method = {
  method_name : string;
  param_names : string list;
  param_types : Types.mono_type list;
  return_type : Types.mono_type;
  body_expr : AST.expression;
}

type impl_template = {
  trait_name : string;
  impl_type_params : AST.generic_param list;
  for_type : Types.mono_type;
  methods : impl_template_method list;
}

module ImplInstSet = Set.Make (struct
  type t = impl_instantiation

  let compare a b =
    let c = String.compare a.module_path b.module_path in
    if c <> 0 then
      c
    else
      let c = compare a.concrete_only_mode b.concrete_only_mode in
      if c <> 0 then
        c
      else
        let c = String.compare a.trait_name b.trait_name in
        if c <> 0 then
          c
        else
          let c = String.compare a.method_name b.method_name in
          if c <> 0 then
            c
          else
            String.compare a.type_fingerprint b.type_fingerprint
end)

(* Set to track enum type instantiations *)
module EnumInstSet = Set.Make (struct
  type t = string * Types.mono_type list (* enum_name, type_args *)

  let compare = compare
end)

type mono_state = {
  mutable func_defs : func_def list;
  mutable impl_templates : impl_template list;
  mutable instantiations : InstSet.t;
  mutable impl_instantiations : ImplInstSet.t;
  impl_inst_payloads : (string, impl_inst_payload) Hashtbl.t;
  mutable enum_insts : EnumInstSet.t; (* Track which enum types we need to generate *)
  mutable name_counter : int;
  module_path : string; (* namespace identity for future module-aware builds *)
  concrete_only : bool; (* Phase 4.3: Rust-style (true) vs TypeScript-style (false) codegen *)
  record_shapes : (string, string) Hashtbl.t;
      (* shape_name -> Go struct body, e.g. "Record_x_int64_y_int64" -> "X int64; Y int64" *)
  type_alias_shapes : (string, string) Hashtbl.t;
      (* canonical_shape_name -> alias_display_name, e.g. "Record_x_int64_y_int64" -> "Point" *)
}

let create_mono_state ?(module_path = "main") ?(concrete_only = true) () =
  if module_path <> "main" then
    failwith
      "Module codegen policy: single Go package per build (module_path must be 'main' until module backend ships)";
  {
    func_defs = [];
    impl_templates = [];
    instantiations = InstSet.empty;
    impl_instantiations = ImplInstSet.empty;
    impl_inst_payloads = Hashtbl.create 32;
    enum_insts = EnumInstSet.empty;
    name_counter = 0;
    module_path;
    concrete_only;
    record_shapes = Hashtbl.create 16;
    type_alias_shapes = Hashtbl.create 16;
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
  | Types.TRecord _ -> 8 (* Lowered as pointer/interface for now *)
  | Types.TRowVar _ -> 8
  | Types.TFun _ -> 8 (* Function pointer *)
  | Types.TUnion _ -> 8 (* Interface *)

(* ============================================================
   Record Shape Interning
   ============================================================ *)

(* Compute canonical shape name for a closed record type.
   Fields must be pre-normalized (alphabetical order).
   Returns shape name like "Record_x_int64_y_int64" *)
let record_shape_name (fields : Types.record_field_type list) : string =
  let field_bits =
    fields
    |> List.map (fun (f : Types.record_field_type) -> f.name ^ "_" ^ mangle_type f.typ)
    |> String.concat "_"
  in
  "Record_" ^ field_bits

(* Convert mono_type to Go type string for shape registration.
   Needs to be defined before intern_record_shape since it's called during registration.
   Mirrors type_to_go but without mono_state dependency for shape body generation. *)
let rec mangle_type_to_go (t : Types.mono_type) : string =
  match t with
  | Types.TInt -> "int64"
  | Types.TFloat -> "float64"
  | Types.TBool -> "bool"
  | Types.TString -> "string"
  | Types.TNull -> "struct{}"
  | Types.TVar _ -> "interface{}"
  | Types.TFun _ as t ->
      let rec collect_args = function
        | Types.TFun (a, r, _) ->
            let args, final_ret = collect_args r in
            (a :: args, final_ret)
        | ret -> ([], ret)
      in
      let args, final_ret = collect_args t in
      let args_str = List.map mangle_type_to_go args |> String.concat ", " in
      Printf.sprintf "func(%s) %s" args_str (mangle_type_to_go final_ret)
  | Types.TArray elem -> "[]" ^ mangle_type_to_go elem
  | Types.THash (key, value) -> "map[" ^ mangle_type_to_go key ^ "]" ^ mangle_type_to_go value
  | Types.TRecord (fields, _row) ->
      let fields = Types.normalize_record_fields fields in
      record_shape_name fields
  | Types.TRowVar _ -> "interface{}"
  | Types.TUnion _ -> "interface{}"
  | Types.TEnum (name, args) -> mangle_type (Types.TEnum (name, args))

let go_record_field_name (name : string) : string = name

(* Register a record shape and return its display name.
   If a type alias is registered for this shape, uses the alias name.
   If already registered, returns existing name (dedup). *)
let intern_record_shape (state : mono_state) (fields : Types.record_field_type list) : string =
  let fields = Types.normalize_record_fields fields in
  let canonical_name = record_shape_name fields in
  (* Determine display name: alias name if registered, otherwise canonical *)
  let display_name =
    match Hashtbl.find_opt state.type_alias_shapes canonical_name with
    | Some alias_name -> alias_name
    | None -> canonical_name
  in
  (if not (Hashtbl.mem state.record_shapes display_name) then
     let go_fields =
       List.map
         (fun (f : Types.record_field_type) ->
           let go_name = go_record_field_name f.name in
           Printf.sprintf "%s %s" go_name (mangle_type_to_go f.typ))
         fields
     in
     Hashtbl.replace state.record_shapes display_name (String.concat "; " go_fields));
  display_name

(* Register a type alias for a record shape.
   e.g., "type point = { x: int, y: int }" registers "Record_x_int64_y_int64" -> "Point" *)
let register_type_alias_shape (state : mono_state) (alias_name : string) (fields : Types.record_field_type list) :
    unit =
  let fields = Types.normalize_record_fields fields in
  let canonical_name = record_shape_name fields in
  let go_alias_name =
    String.uppercase_ascii (String.sub alias_name 0 1) ^ String.sub alias_name 1 (String.length alias_name - 1)
  in
  Hashtbl.replace state.type_alias_shapes canonical_name go_alias_name

(* Emit all registered record shape type definitions *)
let emit_record_shape_defs (state : mono_state) : string =
  Hashtbl.fold
    (fun name body acc -> Printf.sprintf "type %s struct{%s}\n" name body :: acc)
    state.record_shapes []
  |> List.sort String.compare
  |> String.concat ""

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
  | Types.TFun (arg, ret, _) -> emit_func_type state arg ret
  | Types.TArray elem -> "[]" ^ type_to_go state elem
  | Types.THash (key, value) -> "map[" ^ type_to_go state key ^ "]" ^ type_to_go state value
  | Types.TRecord (fields, _row) ->
      let fields = Types.normalize_record_fields fields in
      intern_record_shape state fields
  | Types.TRowVar _ -> "interface{}"
  | Types.TUnion _ -> "interface{}" (* Phase 4.1: unions compile to interface{} *)
  | Types.TEnum (name, args) -> mangle_type (Types.TEnum (name, args))

and emit_func_type state arg ret =
  let rec collect_args = function
    | Types.TFun (a, r, _) ->
        let args, final_ret = collect_args r in
        (a :: args, final_ret)
    | t -> ([], t)
  in
  let args, final_ret = collect_args (Types.TFun (arg, ret, false)) in
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
              func_expr_id = let_binding.value.id;
              poly_type = Types.Forall ([], Types.TNull);
              (* filled in later *)
              captures = [];
            }
            :: state.func_defs;
          ()
      | _ -> collect_funcs_expr state let_binding.value)
  | AST.Return e -> collect_funcs_expr state e
  | AST.ExpressionStmt e -> collect_funcs_expr state e
  | AST.Block stmts -> List.iter (collect_funcs_stmt state) stmts
  | AST.EnumDef _ -> () (* Enums are compile-time only *)
  | AST.TraitDef _ | AST.ImplDef _ | AST.DeriveDef _ | AST.TypeAlias _ ->
      () (* Traits/type aliases are compile-time only *)

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
  | AST.Function _ ->
      (* Nested/anonymous functions are emitted as local closures, not monomorphized top-level defs. *)
      ()
  | AST.EnumConstructor (_, _, args) -> List.iter (collect_funcs_expr state) args
  | AST.Match (scrutinee, arms) ->
      collect_funcs_expr state scrutinee;
      List.iter (fun arm -> collect_funcs_expr state arm.AST.body) arms
  | AST.RecordLit (fields, spread) -> (
      List.iter
        (fun field ->
          match field.AST.field_value with
          | Some expr -> collect_funcs_expr state expr
          | None -> ())
        fields;
      match spread with
      | Some expr -> collect_funcs_expr state expr
      | None -> ())
  | AST.FieldAccess (expr, _field) -> collect_funcs_expr state expr
  | AST.MethodCall (receiver, _method_name, args) ->
      collect_funcs_expr state receiver;
      List.iter (collect_funcs_expr state) args

(* ============================================================
   Pass 2: Collect instantiations at call sites
   ============================================================ *)

(* Get the type of an expression from the type map *)
let get_type (type_map : Infer.type_map) (expr : AST.expression) : Types.mono_type =
  match Hashtbl.find_opt type_map expr.id with
  | Some t -> t
  | None ->
      failwith
        (Printf.sprintf
           "Codegen error: missing type for expression id %d (%s). Type map should be complete before emission."
           expr.id (AST.type_of expr))

(* Check if a type contains unresolved type variables *)
let rec has_type_vars (t : Types.mono_type) : bool =
  match t with
  | Types.TVar _ -> true
  | Types.TFun (arg, ret, _) -> has_type_vars arg || has_type_vars ret
  | Types.TArray elem -> has_type_vars elem
  | Types.THash (k, v) -> has_type_vars k || has_type_vars v
  | Types.TEnum (_, args) -> List.exists has_type_vars args
  | Types.TUnion types -> List.exists has_type_vars types
  | Types.TRecord (fields, row) ->
      List.exists (fun (f : Types.record_field_type) -> has_type_vars f.typ) fields
      || (match row with
          | Some r -> has_type_vars r
          | None -> false)
  | Types.TRowVar _ -> true
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

let lookup_func_def_for_call (state : mono_state) (name : string) (arity : int) : func_def option =
  let candidates = List.filter (fun fd -> fd.name = name && List.length fd.params = arity) state.func_defs in
  match candidates with
  | [] -> None
  | [ fd ] -> Some fd
  | many ->
      let ids =
        many |> List.map (fun (fd : func_def) -> string_of_int fd.func_expr_id) |> String.concat ", "
      in
      failwith
        (Printf.sprintf
           "Codegen error: ambiguous function reference '%s/%d': multiple function definitions share this name/arity (expr ids: %s). Rename one definition or use distinct helper names until symbol-based resolution ships."
           name arity ids)

(* Extract parameter types from a function type *)
let rec extract_param_types n = function
  | Types.TFun (arg, ret, _) when n > 0 ->
      let rest, final = extract_param_types (n - 1) ret in
      (arg :: rest, final)
  | t -> ([], t)

let rec extract_param_types_exact n = function
  | t when n = 0 -> Some ([], t)
  | Types.TFun (arg, ret, _) -> (
      match extract_param_types_exact (n - 1) ret with
      | Some (rest, final) -> Some (arg :: rest, final)
      | None -> None)
  | _ -> None

let rec collect_callable_signatures_exact (arity : int) (t : Types.mono_type) :
    (Types.mono_type list * Types.mono_type) list =
  match t with
  | Types.TFun _ -> (
      match extract_param_types_exact arity t with
      | Some sig_ -> [ sig_ ]
      | None -> [])
  | Types.TUnion members -> List.concat_map (collect_callable_signatures_exact arity) members
  | _ -> []

let callable_signature_exact (arity : int) (t : Types.mono_type) : (Types.mono_type list * Types.mono_type) option =
  match collect_callable_signatures_exact arity t with
  | [] -> None
  | (params0, ret0) :: rest ->
      let all_match = List.for_all (fun (params, ret) -> params = params0 && ret = ret0) rest in
      if all_match then
        Some (params0, ret0)
      else
        None

let extract_all_param_types (t : Types.mono_type) : Types.mono_type list * Types.mono_type =
  let rec go acc = function
    | Types.TFun (arg, ret, _) -> go (arg :: acc) ret
    | final_ret -> (List.rev acc, final_ret)
  in
  go [] t

let add_param_bindings
    (env : Infer.type_env) (param_names : string list) (param_types : Types.mono_type list) : Infer.type_env =
  let rec go acc names types =
    match (names, types) with
    | name :: names_tail, typ :: types_tail ->
        go (Infer.TypeEnv.add name (Types.Forall ([], typ)) acc) names_tail types_tail
    | _ -> acc
  in
  go env param_names param_types

let impl_type_bindings (impl_type_params : AST.generic_param list) : (string * Types.mono_type) list =
  List.map (fun (p : AST.generic_param) -> (p.name, Types.TVar p.name)) impl_type_params

let type_expr_to_mono_type_with_impl_bindings
    (impl_type_params : AST.generic_param list)
    (type_expr : AST.type_expr) : Types.mono_type =
  let bindings = impl_type_bindings impl_type_params in
  Annotation.type_expr_to_mono_type_with bindings type_expr

let register_impl_template (state : mono_state) (impl : AST.impl_def) : impl_template =
  let for_type = type_expr_to_mono_type_with_impl_bindings impl.impl_type_params impl.impl_for_type in
  let methods =
    List.map
      (fun (m : AST.method_impl) ->
        let param_names, param_types =
          List.split
            (List.map
               (fun (param_name, param_type_opt) ->
                 match param_type_opt with
                 | Some param_type ->
                     (param_name, type_expr_to_mono_type_with_impl_bindings impl.impl_type_params param_type)
                 | None ->
                     failwith
                       (Printf.sprintf
                          "Codegen error: impl %s.%s parameter '%s' is missing a type annotation"
                          impl.impl_trait_name m.impl_method_name param_name))
               m.impl_method_params)
        in
        let return_type =
          match m.impl_method_return_type with
          | Some ret_ann -> type_expr_to_mono_type_with_impl_bindings impl.impl_type_params ret_ann
          | None ->
              failwith
                (Printf.sprintf "Codegen error: impl %s.%s is missing a return type annotation" impl.impl_trait_name
                   m.impl_method_name)
        in
        {
          method_name = m.impl_method_name;
          param_names;
          param_types;
          return_type;
          body_expr = m.impl_method_body;
        })
      impl.impl_methods
  in
  let template =
    {
      trait_name = impl.impl_trait_name;
      impl_type_params = impl.impl_type_params;
      for_type = Types.canonicalize_mono_type for_type;
      methods;
    }
  in
  state.impl_templates <- template :: state.impl_templates;
  template

let specialize_signature
    (declared_param_types : Types.mono_type list)
    (declared_return_type : Types.mono_type)
    (actual_param_types : Types.mono_type list) : (Types.mono_type list * Types.mono_type) option =
  let rec unify_params subst actual_types expected_types =
    match (actual_types, expected_types) with
    | [], [] -> Some subst
    | actual :: rest_actual, expected :: rest_expected -> (
        let actual' = Types.apply_substitution subst actual in
        let expected' = Types.apply_substitution subst expected in
        match Unify.unify actual' expected' with
        | Ok new_subst -> unify_params (Types.compose_substitution new_subst subst) rest_actual rest_expected
        | Error _ -> None)
    | _ -> None
  in
  match unify_params Types.empty_substitution declared_param_types actual_param_types with
  | None -> None
  | Some subst ->
      let specialized_params = List.map (Types.apply_substitution subst) declared_param_types in
      let specialized_return = Types.apply_substitution subst declared_return_type in
      Some (specialized_params, specialized_return)

let infer_return_type_from_signature
    (declared_param_types : Types.mono_type list)
    (declared_return_type : Types.mono_type)
    (concrete_param_types : Types.mono_type list) : Types.mono_type option =
  match specialize_signature declared_param_types declared_return_type concrete_param_types with
  | Some (_specialized_params, specialized_return) -> Some specialized_return
  | None -> None

let same_instantiation_identity (a : instantiation) (b : instantiation) : bool =
  a.module_path = b.module_path
  && a.func_expr_id = b.func_expr_id
  && a.func_name = b.func_name
  && a.func_arity = b.func_arity
  && a.concrete_only_mode = b.concrete_only_mode
  && a.type_fingerprint = b.type_fingerprint

let add_instantiation (state : mono_state) (inst : instantiation) : unit =
  let existing =
    InstSet.elements state.instantiations |> List.find_opt (fun candidate -> same_instantiation_identity candidate inst)
  in
  match existing with
  | Some existing_inst when existing_inst.concrete_types <> inst.concrete_types ->
      failwith
        (Printf.sprintf
           "Codegen error: instantiation fingerprint collision for '%s' (expr id %d, fingerprint %s): %s vs %s"
           inst.func_name inst.func_expr_id inst.type_fingerprint
           (String.concat ", " (List.map Types.to_string existing_inst.concrete_types))
           (String.concat ", " (List.map Types.to_string inst.concrete_types)))
  | Some existing_inst when existing_inst.return_type <> inst.return_type ->
      failwith
        (Printf.sprintf
           "Codegen error: inconsistent return type for instantiation '%s' (expr id %d, fingerprint %s): %s vs %s"
           inst.func_name inst.func_expr_id inst.type_fingerprint (Types.to_string existing_inst.return_type)
           (Types.to_string inst.return_type))
  | _ -> state.instantiations <- InstSet.add inst state.instantiations

let impl_inst_payload_key (inst : impl_instantiation) : string =
  Printf.sprintf "%s|%b|%s|%s|%s" inst.module_path inst.concrete_only_mode inst.trait_name inst.method_name
    inst.type_fingerprint

let add_impl_instantiation (state : mono_state) (inst : impl_instantiation) (payload : impl_inst_payload) : unit =
  let payload_key = impl_inst_payload_key inst in
  let existing_payload_opt = Hashtbl.find_opt state.impl_inst_payloads payload_key in
  (match existing_payload_opt with
  | Some _existing when
      (match ImplInstSet.find_opt inst state.impl_instantiations with
      | Some existing_inst -> existing_inst.for_type <> inst.for_type
      | None -> false) ->
      failwith
        (Printf.sprintf "Codegen error: impl instantiation fingerprint collision for '%s.%s' (%s): %s vs %s"
           inst.trait_name inst.method_name inst.type_fingerprint
           (match ImplInstSet.find_opt inst state.impl_instantiations with
           | Some existing_inst -> Types.to_string existing_inst.for_type
           | None -> "<missing>")
           (Types.to_string inst.for_type))
  | Some existing when existing.param_types <> payload.param_types ->
      failwith
        (Printf.sprintf
           "Codegen error: inconsistent parameter types for impl instantiation '%s.%s' (%s): [%s] vs [%s]"
           inst.trait_name inst.method_name inst.type_fingerprint
           (String.concat ", " (List.map Types.to_string existing.param_types))
           (String.concat ", " (List.map Types.to_string payload.param_types)))
  | Some existing when existing.return_type <> payload.return_type ->
      failwith
        (Printf.sprintf
           "Codegen error: inconsistent return type for impl instantiation '%s.%s' (%s): %s vs %s"
           inst.trait_name inst.method_name inst.type_fingerprint (Types.to_string existing.return_type)
           (Types.to_string payload.return_type))
  | Some _ -> ()
  | None -> Hashtbl.replace state.impl_inst_payloads payload_key payload);
  state.impl_instantiations <- ImplInstSet.add inst state.impl_instantiations

let select_impl_template_for_type
    (state : mono_state)
    (trait_name : string)
    (method_name : string)
    (for_type : Types.mono_type) : (impl_template_method * Types.substitution) option =
  let for_type' = Types.canonicalize_mono_type for_type in
  let templates =
    List.filter
      (fun (template : impl_template) ->
        template.trait_name = trait_name && List.exists (fun m -> m.method_name = method_name) template.methods)
      state.impl_templates
  in
  let method_from_template (template : impl_template) : impl_template_method =
    match List.find_opt (fun (m : impl_template_method) -> m.method_name = method_name) template.methods with
    | Some m -> m
    | None ->
        failwith
          (Printf.sprintf "Codegen error: impl template for trait '%s' is missing method '%s'" trait_name method_name)
  in
  let concrete_matches =
    List.filter
      (fun (template : impl_template) ->
        template.impl_type_params = [] && Types.canonicalize_mono_type template.for_type = for_type')
      templates
  in
  match concrete_matches with
  | template :: [] -> Some (method_from_template template, Types.empty_substitution)
  | _ :: _ :: _ ->
      failwith
        (Printf.sprintf
           "Codegen error: ambiguous concrete impl templates for trait '%s', method '%s', type %s"
           trait_name method_name (Types.to_string for_type'))
  | [] ->
      let generic_matches =
        List.filter_map
          (fun (template : impl_template) ->
            if template.impl_type_params = [] then
              None
            else
              match Unify.unify (Types.canonicalize_mono_type template.for_type) for_type' with
              | Error _ -> None
              | Ok subst -> Some (template, subst))
          templates
      in
      (match generic_matches with
      | [] -> None
      | [ (template, subst) ] -> Some (method_from_template template, subst)
      | many ->
          let patterns =
            many
            |> List.map (fun (template, _subst) -> Types.to_string template.for_type)
            |> List.sort_uniq String.compare
            |> String.concat ", "
          in
          failwith
            (Printf.sprintf
               "Codegen error: ambiguous generic impl templates for trait '%s', method '%s', type %s (matching \
                patterns: %s)"
               trait_name method_name (Types.to_string for_type') patterns))

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
  | AST.TraitDef _ | AST.DeriveDef _ | AST.TypeAlias _ -> env
  | AST.ImplDef impl ->
      ignore (register_impl_template state impl);
      if impl.impl_type_params = [] then (
        let for_type = Annotation.type_expr_to_mono_type impl.impl_for_type in
        let for_type_fingerprint = fingerprint_types [ for_type ] in
        List.iter
          (fun (m : AST.method_impl) ->
            let method_param_names, method_param_types =
              List.split
                (List.map
                   (fun (param_name, param_type_opt) ->
                     match param_type_opt with
                     | Some param_type -> (param_name, Annotation.type_expr_to_mono_type param_type)
                     | None ->
                         failwith
                           (Printf.sprintf
                              "Codegen error: impl %s.%s parameter '%s' is missing a type annotation"
                              impl.impl_trait_name m.impl_method_name param_name))
                   m.impl_method_params)
            in
            let return_type =
              match m.impl_method_return_type with
              | Some ret_ann -> Annotation.type_expr_to_mono_type ret_ann
              | None ->
                  failwith
                    (Printf.sprintf "Codegen error: impl %s.%s is missing a return type annotation"
                       impl.impl_trait_name m.impl_method_name)
            in
            let impl_inst =
              {
                trait_name = impl.impl_trait_name;
                method_name = m.impl_method_name;
                module_path = state.module_path;
                concrete_only_mode = state.concrete_only;
                for_type;
                type_fingerprint = for_type_fingerprint;
              }
            in
            let payload =
              {
                param_names = method_param_names;
                param_types = method_param_types;
                return_type;
                body_expr = m.impl_method_body;
              }
            in
            add_impl_instantiation state impl_inst payload;
            let method_env = add_param_bindings env method_param_names method_param_types in
            collect_insts_expr state type_map method_env m.impl_method_body)
          impl.impl_methods);
      env

and register_impl_method_use
    (state : mono_state)
    (type_map : Infer.type_map)
    (env : Infer.type_env)
    ~(trait_name : string)
    ~(method_name : string)
    ~(for_type : Types.mono_type) : unit =
  let for_type' = Types.canonicalize_mono_type for_type in
  if state.concrete_only && has_type_vars for_type' then
    ()
  else
    match select_impl_template_for_type state trait_name method_name for_type' with
    | None -> ()
    | Some (template_method, method_subst) ->
        let param_types =
          List.map
            (fun t -> Types.canonicalize_mono_type (Types.apply_substitution method_subst t))
            template_method.param_types
        in
        let return_type =
          Types.canonicalize_mono_type (Types.apply_substitution method_subst template_method.return_type)
        in
        if state.concrete_only && List.exists has_type_vars (for_type' :: return_type :: param_types) then
          failwith
            (Printf.sprintf
               "Codegen error: unresolved type variables after impl specialization for '%s.%s' on %s"
               trait_name method_name (Types.to_string for_type'));
        let impl_inst =
          {
            trait_name;
            method_name;
            module_path = state.module_path;
            concrete_only_mode = state.concrete_only;
            for_type = for_type';
            type_fingerprint = fingerprint_types [ for_type' ];
          }
        in
        if ImplInstSet.mem impl_inst state.impl_instantiations then
          ()
        else (
          let payload =
            {
              param_names = template_method.param_names;
              param_types;
              return_type;
              body_expr = template_method.body_expr;
            }
          in
          add_impl_instantiation state impl_inst payload;
          let method_env = add_param_bindings env payload.param_names payload.param_types in
          collect_insts_expr state type_map method_env payload.body_expr)

and collect_insts_expr
    ?(expected_type : Types.mono_type option = None)
    (state : mono_state)
    (type_map : Infer.type_map)
    (env : Infer.type_env)
    (expr : AST.expression) : unit =
  match expr.expr with
  | AST.Integer _ | AST.Float _ | AST.Boolean _ | AST.String _ -> ()
  | AST.Identifier name when is_user_func state name -> (
      let inferred_func_type =
        match Infer.TypeEnv.find_opt name env with
        | Some (Types.Forall (_, t)) -> t
        | None -> get_type type_map expr
      in
      let inferred_params, inferred_ret = extract_all_param_types inferred_func_type in
      let selected_params, selected_ret =
        match expected_type with
        | Some expected_func_type -> (
            let expected_params, expected_ret = extract_all_param_types expected_func_type in
            if expected_params <> [] then
              (expected_params, expected_ret)
            else
              (inferred_params, inferred_ret))
        | None -> (inferred_params, inferred_ret)
      in
      let arity = List.length selected_params in
      let has_unresolved = List.exists has_type_vars (selected_ret :: selected_params) in
      if arity > 0 && not has_unresolved then
        match lookup_func_def_for_call state name arity with
        | None -> ()
        | Some func_def ->
            let inst =
              {
                func_name = func_def.name;
                module_path = state.module_path;
                func_expr_id = func_def.func_expr_id;
                func_arity = arity;
                concrete_only_mode = state.concrete_only;
                concrete_types = selected_params;
                type_fingerprint = fingerprint_types selected_params;
                return_type = selected_ret;
              }
            in
            add_instantiation state inst)
  | AST.Identifier _ -> ()
  | AST.Prefix (op, e) ->
      collect_insts_expr state type_map env e;
      (match op with
      | "-" ->
          let operand_type = Types.canonicalize_mono_type (get_type type_map e) in
          (match operand_type with
          | Types.TInt | Types.TFloat -> ()
          | _ ->
              register_impl_method_use state type_map env ~trait_name:"neg" ~method_name:"neg"
                ~for_type:operand_type)
      | _ -> ())
  | AST.Infix (l, op, r) ->
      collect_insts_expr state type_map env l;
      collect_insts_expr state type_map env r;
      let left_type = Types.canonicalize_mono_type (get_type type_map l) in
      let is_num_primitive =
        match left_type with
        | Types.TInt | Types.TFloat -> true
        | _ -> false
      in
      let is_eq_primitive =
        match left_type with
        | Types.TInt | Types.TFloat | Types.TBool | Types.TString -> true
        | _ -> false
      in
      let is_ord_primitive =
        match left_type with
        | Types.TInt | Types.TFloat | Types.TString -> true
        | _ -> false
      in
      (match op with
      | "+" ->
          if left_type = Types.TString || is_num_primitive then
            ()
          else
            register_impl_method_use state type_map env ~trait_name:"num" ~method_name:"add" ~for_type:left_type
      | "-" ->
          if is_num_primitive then
            ()
          else
            register_impl_method_use state type_map env ~trait_name:"num" ~method_name:"sub" ~for_type:left_type
      | "*" ->
          if is_num_primitive then
            ()
          else
            register_impl_method_use state type_map env ~trait_name:"num" ~method_name:"mul" ~for_type:left_type
      | "/" ->
          if is_num_primitive then
            ()
          else
            register_impl_method_use state type_map env ~trait_name:"num" ~method_name:"div" ~for_type:left_type
      | "==" | "!=" ->
          if is_eq_primitive then
            ()
          else
            register_impl_method_use state type_map env ~trait_name:"eq" ~method_name:"eq" ~for_type:left_type
      | "<" | ">" | "<=" | ">=" ->
          if is_ord_primitive then
            ()
          else
            register_impl_method_use state type_map env ~trait_name:"ord" ~method_name:"compare" ~for_type:left_type
      | _ -> ())
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
      | AST.Identifier name when is_user_func state name -> (
          let arity = List.length args in
          let func_def_opt = lookup_func_def_for_call state name arity in
          let arg_types = List.map (get_type type_map) args in
          (* Look up the function's declared type from the environment *)
          let declared_signature_opt =
            match Infer.TypeEnv.find_opt name env with
            | Some (Types.Forall (_, func_type)) ->
                extract_param_types_exact arity func_type
            | None -> None
          in
          let declared_param_types, declared_return_type_opt =
            match declared_signature_opt with
            | Some (params, ret) -> (params, Some ret)
            | None -> (arg_types, None)
          in
          let specialized_declared_signature =
            match (declared_signature_opt, declared_return_type_opt) with
            | Some (_params, _ret), Some declared_return ->
                specialize_signature declared_param_types declared_return arg_types
            | _ -> None
          in
          (* Check if any declared param is a union type *)
          let has_union_param =
            List.exists
              (function
                | Types.TUnion _ -> true
                | _ -> false)
              declared_param_types
          in
          (* If function has union params, use declared types; otherwise use argument types *)
          let concrete_param_types =
            if has_union_param then
              (match specialized_declared_signature with
              | Some (specialized_params, _specialized_return) -> specialized_params
              | None -> declared_param_types)
            else
              arg_types
          in
          let return_type =
            match declared_return_type_opt with
            | Some declared_return_type -> (
                match specialized_declared_signature with
                | Some (_specialized_params, specialized_return) -> specialized_return
                | None -> (
                    match
                      infer_return_type_from_signature declared_param_types declared_return_type concrete_param_types
                    with
                    | Some inferred_ret -> inferred_ret
                    | None -> get_type type_map expr))
            | None -> get_type type_map expr
          in
          match func_def_opt with
          | None -> ()
          | Some func_def ->
              let inst =
                {
                  func_name = func_def.name;
                  module_path = state.module_path;
                  func_expr_id = func_def.func_expr_id;
                  func_arity = arity;
                  concrete_only_mode = state.concrete_only;
                  concrete_types = concrete_param_types;
                  type_fingerprint = fingerprint_types concrete_param_types;
                  return_type;
                }
              in
              add_instantiation state inst)
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
      let param_names = List.map fst f.params in
      let arity = List.length param_names in
      let func_type = get_type type_map expr in
      let param_types =
        match extract_param_types_exact arity func_type with
        | Some (types, _) -> types
        | None -> (
            match expected_type with
            | Some expected_func_type -> (
                match extract_param_types_exact arity expected_func_type with
                | Some (types, _) -> types
                | None -> [])
            | None -> [])
      in
      let body_env = add_param_bindings env param_names param_types in
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
  | AST.RecordLit (fields, spread) -> (
      List.iter
        (fun field ->
          match field.AST.field_value with
          | Some expr -> collect_insts_expr state type_map env expr
          | None -> ())
        fields;
      match spread with
      | Some expr -> collect_insts_expr state type_map env expr
      | None -> ())
  | AST.FieldAccess (receiver, _field) -> (
      (* Check if this is a nullary enum constructor *)
      match receiver.expr with
      | AST.Identifier enum_name when Typecheck.Enum_registry.lookup enum_name <> None ->
          (* This is a nullary enum constructor - track it *)
          let enum_type = get_type type_map expr in
          track_enum_inst state enum_type
      | _ ->
          (* Real field access - collect insts in receiver *)
          collect_insts_expr state type_map env receiver)
  | AST.MethodCall (receiver, method_name, args) -> (
      (* Check if this is an enum constructor and register it *)
      match receiver.expr with
      | AST.Identifier enum_name when Typecheck.Enum_registry.lookup enum_name <> None ->
          (* This is an enum constructor - track it like EnumConstructor *)
          let enum_type = get_type type_map expr in
          track_enum_inst state enum_type;
          (* Also collect instantiations in arguments *)
          List.iter (fun arg -> collect_insts_expr state type_map env arg) args
      | _ ->
          (* Real method call - collect insts in receiver and args *)
          collect_insts_expr state type_map env receiver;
          List.iter (fun arg -> collect_insts_expr state type_map env arg) args;
          let receiver_type = Types.canonicalize_mono_type (get_type type_map receiver) in
          let trait_name =
            match Typecheck.Infer.lookup_method_resolution expr.id with
            | Some name -> name
            | None ->
                failwith
                  (Printf.sprintf
                     "Codegen error: missing resolved trait metadata for method call id %d ('%s'). Typechecker \
                      must record method resolution before emission."
                     expr.id method_name)
          in
          register_impl_method_use state type_map env ~trait_name ~method_name ~for_type:receiver_type)

(* ============================================================
   Code Generation State
   ============================================================ *)

type emit_state = {
  mutable indent : int;
  mono : mono_state;
}

let create_emit_state mono = { indent = 1; mono }
let indent_str state = String.make (state.indent * 4) ' '

let with_indent_delta (state : emit_state) (delta : int) (f : unit -> 'a) : 'a =
  state.indent <- state.indent + delta;
  Fun.protect ~finally:(fun () -> state.indent <- state.indent - delta) f

type emit_target =
  | ReturnTarget
  | AssignTarget of string
  | DiscardTarget

let target_prefix = function
  | ReturnTarget -> "return "
  | AssignTarget name -> name ^ " = "
  | DiscardTarget -> "_ = "

let emit_record_struct_type (state : emit_state) (fields : Types.record_field_type list) : string =
  let fields = Types.normalize_record_fields fields in
  intern_record_shape state.mono fields

let maybe_project_to_expected_record_type
    (state : emit_state)
    (type_map : Infer.type_map)
    (env : Infer.type_env)
    (expr : AST.expression)
    (expected_type : Types.mono_type option)
    (expr_str : string) : string =
  let type_from_env_or_map (e : AST.expression) : Types.mono_type option =
    match e.expr with
    | AST.Identifier name -> (
        match Infer.TypeEnv.find_opt name env with
        | Some (Types.Forall (_, t)) -> Some t
        | None -> Hashtbl.find_opt type_map e.id)
    | _ -> Hashtbl.find_opt type_map e.id
  in
  let rec all_some = function
    | [] -> Some []
    | x :: xs -> (
        match (x, all_some xs) with
        | Some v, Some rest -> Some (v :: rest)
        | _ -> None)
  in
  match expected_type with
  | None -> expr_str
  | Some (Types.TRecord (expected_fields, _expected_row)) -> (
      let actual_type_opt =
        match expr.expr with
        | AST.Call ({ expr = AST.Identifier func_name; _ }, args) -> (
            match (Infer.TypeEnv.find_opt func_name env, all_some (List.map type_from_env_or_map args)) with
            | Some (Types.Forall (_, func_type)), Some arg_types -> (
                match extract_param_types_exact (List.length args) func_type with
                | Some (declared_params, declared_ret) -> (
                    match infer_return_type_from_signature declared_params declared_ret arg_types with
                    | Some inferred_ret -> Some inferred_ret
                    | None -> type_from_env_or_map expr)
                | None -> type_from_env_or_map expr)
            | _ -> type_from_env_or_map expr)
        | _ -> type_from_env_or_map expr
      in
      match actual_type_opt with
      | Some (Types.TRecord (actual_fields, actual_row)) ->
          let expected_fields = Types.normalize_record_fields expected_fields in
          let actual_fields = Types.normalize_record_fields actual_fields in
          if expected_fields = actual_fields then
            expr_str
          else
            let expected_struct_type = emit_record_struct_type state expected_fields in
            let actual_type = Types.TRecord (actual_fields, actual_row) in
            let actual_go_type = type_to_go state.mono actual_type in
            let assignments =
              expected_fields
              |> List.map (fun (f : Types.record_field_type) ->
                     let go_name = go_record_field_name f.name in
                     Printf.sprintf "%s: __src.%s" go_name go_name)
              |> String.concat ", "
            in
            Printf.sprintf "(func(__src %s) %s { return %s{%s} })(%s)" actual_go_type expected_struct_type
              expected_struct_type assignments expr_str
      | _ -> expr_str)
  | Some _ -> expr_str

let rec find_last_record_field_expr (field_name : string) (fields : AST.record_field list) : AST.expression option
    =
  match fields with
  | [] -> None
  | field :: rest ->
      let rest_result = find_last_record_field_expr field_name rest in
      let current_result =
        if field.field_name = field_name then
          match field.field_value with
          | Some expr -> Some expr
          | None -> Some (AST.mk_expr (AST.Identifier field.field_name))
        else
          None
      in
      match rest_result with
      | Some _ -> rest_result
      | None -> current_result

let rec copy_specialized_expr_types
    (source_map : Infer.type_map)
    (target_map : Infer.type_map)
    (specialization_subst : Types.substitution)
    (expr : AST.expression) : unit =
  (match Hashtbl.find_opt source_map expr.id with
  | Some ty -> Hashtbl.replace target_map expr.id (Types.apply_substitution specialization_subst ty)
  | None -> ());
  match expr.expr with
  | AST.Integer _ | AST.Float _ | AST.Boolean _ | AST.String _ | AST.Identifier _ -> ()
  | AST.Prefix (_, operand) -> copy_specialized_expr_types source_map target_map specialization_subst operand
  | AST.Infix (left, _, right) ->
      copy_specialized_expr_types source_map target_map specialization_subst left;
      copy_specialized_expr_types source_map target_map specialization_subst right
  | AST.TypeCheck (e, _) -> copy_specialized_expr_types source_map target_map specialization_subst e
  | AST.If (condition, consequence, alternative) ->
      copy_specialized_expr_types source_map target_map specialization_subst condition;
      copy_specialized_stmt_types source_map target_map specialization_subst consequence;
      Option.iter (copy_specialized_stmt_types source_map target_map specialization_subst) alternative
  | AST.Call (f, args) ->
      copy_specialized_expr_types source_map target_map specialization_subst f;
      List.iter (copy_specialized_expr_types source_map target_map specialization_subst) args
  | AST.Array elements ->
      List.iter (copy_specialized_expr_types source_map target_map specialization_subst) elements
  | AST.Hash pairs ->
      List.iter
        (fun (k, v) ->
          copy_specialized_expr_types source_map target_map specialization_subst k;
          copy_specialized_expr_types source_map target_map specialization_subst v)
        pairs
  | AST.Index (container, index) ->
      copy_specialized_expr_types source_map target_map specialization_subst container;
      copy_specialized_expr_types source_map target_map specialization_subst index
  | AST.Function f -> copy_specialized_stmt_types source_map target_map specialization_subst f.body
  | AST.EnumConstructor (_, _, args) ->
      List.iter (copy_specialized_expr_types source_map target_map specialization_subst) args
  | AST.Match (scrutinee, arms) ->
      copy_specialized_expr_types source_map target_map specialization_subst scrutinee;
      List.iter
        (fun (arm : AST.match_arm) ->
          copy_specialized_expr_types source_map target_map specialization_subst arm.body)
        arms
  | AST.RecordLit (fields, spread) ->
      List.iter
        (fun (field : AST.record_field) ->
          match field.field_value with
          | Some field_expr -> copy_specialized_expr_types source_map target_map specialization_subst field_expr
          | None -> ())
        fields;
      Option.iter (copy_specialized_expr_types source_map target_map specialization_subst) spread
  | AST.FieldAccess (receiver, _) ->
      copy_specialized_expr_types source_map target_map specialization_subst receiver
  | AST.MethodCall (receiver, _, args) ->
      copy_specialized_expr_types source_map target_map specialization_subst receiver;
      List.iter (copy_specialized_expr_types source_map target_map specialization_subst) args

and copy_specialized_stmt_types
    (source_map : Infer.type_map)
    (target_map : Infer.type_map)
    (specialization_subst : Types.substitution)
    (stmt : AST.statement) : unit =
  match stmt.stmt with
  | AST.Let let_binding ->
      copy_specialized_expr_types source_map target_map specialization_subst let_binding.value
  | AST.Return expr | AST.ExpressionStmt expr ->
      copy_specialized_expr_types source_map target_map specialization_subst expr
  | AST.Block stmts -> List.iter (copy_specialized_stmt_types source_map target_map specialization_subst) stmts
  | AST.EnumDef _ | AST.TraitDef _ | AST.ImplDef _ | AST.DeriveDef _ | AST.TypeAlias _ -> ()

(* ============================================================
   Expression Emission
   ============================================================ *)

let rec emit_expr
    ?(expected_type : Types.mono_type option = None)
    (state : emit_state)
    (type_map : Infer.type_map)
    (env : Infer.type_env)
    (expr : AST.expression) : string =
  let emitted =
    match expr.expr with
    | AST.Integer i -> Printf.sprintf "int64(%Ld)" i
    | AST.Float f -> Printf.sprintf "float64(%g)" f
    | AST.Boolean true -> "true"
    | AST.Boolean false -> "false"
    | AST.String s -> Printf.sprintf "%S" s
    | AST.Identifier name -> (
        match expected_type with
        | Some expected_func_type when is_user_func state.mono name -> (
            let expected_param_types, _ = extract_all_param_types expected_func_type in
            let param_types =
              if expected_param_types <> [] then
                expected_param_types
              else
                match Infer.TypeEnv.find_opt name env with
                | Some (Types.Forall (_, inferred_func_type)) ->
                    fst (extract_all_param_types inferred_func_type)
                | None -> (
                    match Hashtbl.find_opt type_map expr.id with
                    | Some inferred_func_type -> fst (extract_all_param_types inferred_func_type)
                    | None -> [])
            in
            match param_types with
            | [] -> name
            | _ -> mangle_func_name name param_types)
        | _ -> name)
    | AST.Prefix (op, operand) -> emit_prefix state type_map env op operand
    | AST.Infix (left, op, right) -> emit_infix state type_map env left op right
    | AST.TypeCheck (expr, type_ann) -> emit_type_check state type_map env expr type_ann
    | AST.If (cond, cons, alt) -> emit_if state type_map env expr cond cons alt
    | AST.Call (func, args) -> emit_call state type_map env func args
    | AST.Array elements ->
        let expected_elem_type =
          match expected_type with
          | Some (Types.TArray elem_type) -> Some elem_type
          | _ -> None
        in
        emit_array ?expected_elem_type state type_map env elements
    | AST.Hash pairs ->
        let expected_hash_types =
          match expected_type with
          | Some (Types.THash (k, v)) -> Some (k, v)
          | _ -> None
        in
        emit_hash ?expected_hash_types state type_map env pairs
    | AST.Index (container, index) -> emit_index state type_map env container index
    | AST.Function f ->
        (* Phase 2: Convert params to expressions for backward compat *)
        let param_exprs = List.map (fun (name, _) -> AST.mk_expr (AST.Identifier name)) f.params in
        let arity = List.length param_exprs in
        let inferred_func_type = get_type type_map expr in
        let should_use_expected =
          match extract_param_types_exact arity inferred_func_type with
          | Some (params, ret) -> List.exists has_type_vars (ret :: params)
          | None -> true
        in
        let selected_func_type =
          if should_use_expected then
            match expected_type with
            | Some t when Option.is_some (extract_param_types_exact arity t) -> t
            | _ -> inferred_func_type
          else
            inferred_func_type
        in
        emit_function_expr ~func_type_override:selected_func_type state type_map env expr param_exprs f.body
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
    | AST.RecordLit (fields, spread) -> (
        let record_type = get_type type_map expr in
        let result_fields, _result_row =
          match record_type with
          | Types.TRecord (f, row) -> (f, row)
          | _ ->
              failwith
                (Printf.sprintf "Record literal expected record type, got %s" (Types.to_string record_type))
        in
        let struct_type = emit_record_struct_type state result_fields in
        let emit_field_assignment field base_var_opt =
          let go_name = go_record_field_name field.Types.name in
          match find_last_record_field_expr field.Types.name fields with
          | Some field_expr ->
              let field_str = emit_expr state type_map env field_expr in
              Printf.sprintf "%s: %s" go_name field_str
          | None -> (
              match base_var_opt with
              | Some base_var -> Printf.sprintf "%s: %s.%s" go_name base_var go_name
              | None ->
                  failwith
                    (Printf.sprintf "Record field '%s' not provided and no spread base available" field.Types.name)
              )
        in
        match spread with
        | None ->
            let assignments = List.map (fun f -> emit_field_assignment f None) result_fields in
            Printf.sprintf "%s{%s}" struct_type (String.concat ", " assignments)
        | Some base_expr ->
            let base_str = emit_expr state type_map env base_expr in
            let assignments = List.map (fun f -> emit_field_assignment f (Some "__base")) result_fields in
            Printf.sprintf "(func() %s { __base := %s; return %s{%s} })()" struct_type base_str struct_type
              (String.concat ", " assignments))
    | AST.FieldAccess (receiver, variant_name) -> (
        (* Check if this is a nullary enum constructor *)
        match receiver.expr with
        | AST.Identifier enum_name when Typecheck.Enum_registry.lookup enum_name <> None ->
            (* This is a nullary enum constructor - emit like EnumConstructor with no args *)
            let enum_type = get_type type_map expr in
            let type_args =
              match enum_type with
              | Types.TEnum (_, args) -> args
              | _ -> []
            in
            (* Generate the mangled constructor name *)
            let go_type_name = mangle_type (Types.TEnum (enum_name, type_args)) in
            let constructor_name = Printf.sprintf "%s_%s" go_type_name variant_name in
            Printf.sprintf "%s()" constructor_name
        | _ ->
            (* Real field access *)
            let receiver_str = emit_expr state type_map env receiver in
            Printf.sprintf "(%s).%s" receiver_str (go_record_field_name variant_name))
    | AST.MethodCall (receiver, variant_name, args) -> (
        (* Check if this is an enum constructor *)
        match receiver.expr with
        | AST.Identifier enum_name when Typecheck.Enum_registry.lookup enum_name <> None ->
            (* This is an enum constructor - emit like EnumConstructor *)
            (* Get the enum type from the type_map (MethodCall node itself should be typed) *)
            let enum_type = get_type type_map expr in
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
        | _ ->
            (* Real method call - emit as trait method call *)
            (* Get receiver type *)
            let receiver_type = get_type type_map receiver in

            (* Use method-resolution metadata from typechecking; codegen must not re-resolve. *)
            let trait_name =
              match Typecheck.Infer.lookup_method_resolution expr.id with
              | Some name -> name
              | None ->
                  failwith
                    (Printf.sprintf
                       "Codegen error: missing resolved trait metadata for method call id %d ('%s'). Typechecker must record method resolution before emission."
                       expr.id variant_name)
            in
            (* Generate mangled function name: trait_method_type *)
            (* e.g., show_show_int64 for show trait, show method, int64 type *)
            let type_suffix = mangle_type receiver_type in
            let func_name = Printf.sprintf "%s_%s_%s" trait_name variant_name type_suffix in

            (* Emit receiver and arguments *)
            let receiver_str = emit_expr state type_map env receiver in
            let arg_strs = List.map (emit_expr state type_map env) args in
            let all_args = receiver_str :: arg_strs in

            Printf.sprintf "%s(%s)" func_name (String.concat ", " all_args))
  in
  maybe_project_to_expected_record_type state type_map env expr expected_type emitted

(* ============================================================
     Match Expression Codegen
     ============================================================ *)

and emit_match ?target state type_map env match_expr scrutinee arms =
  (* Get the type of the scrutinee *)
  let scrutinee_type = get_type type_map scrutinee in

  (* Get the type of the entire match expression from the type_map *)
  let match_result_type = get_type type_map match_expr in

  match scrutinee_type with
  | Types.TEnum (enum_name, type_args) ->
      emit_match_enum ?target state type_map env scrutinee scrutinee_type enum_name type_args arms
        match_result_type
  | Types.TInt | Types.TString | Types.TBool ->
      emit_match_primitive ?target state type_map env scrutinee scrutinee_type arms match_result_type
  | Types.TRecord _ -> emit_match_record ?target state type_map env scrutinee arms match_result_type
  | _ -> failwith (Printf.sprintf "Match on type %s not yet supported" (Types.to_string scrutinee_type))

and emit_match_record ?target state type_map env scrutinee arms match_result_type =
  let scrutinee_str = emit_expr state type_map env scrutinee in
  let match_result_go_type = type_to_go state.mono match_result_type in
  let result_prefix =
    match target with
    | Some t -> target_prefix t
    | None -> "return "
  in
  let emit_lit = function
    | AST.LInt n -> Printf.sprintf "int64(%Ld)" n
    | AST.LString s -> Printf.sprintf "%S" s
    | AST.LBool b ->
        if b then
          "true"
        else
          "false"
  in
  let emit_record_arm pattern body is_first =
    let body_code =
      match target with
      | Some t -> with_indent_delta state 2 (fun () -> emit_expr_to_target state type_map env body t)
      | None ->
          let body_str = emit_expr state type_map env body in
          "\t\t" ^ result_prefix ^ body_str ^ "\n"
    in
    let branch_prefix_if =
      if is_first then
        "\tif"
      else
        " else if"
    in
    let branch_prefix_else =
      if is_first then
        "\tif true"
      else
        " else"
    in
    match pattern.AST.pat with
    | AST.PWildcard ->
        Printf.sprintf "%s {\n%s\t}" branch_prefix_else body_code
    | AST.PVariable name ->
        Printf.sprintf "%s {\n\t\t%s := __scrutinee\n%s\t}" branch_prefix_else name body_code
    | AST.PRecord (fields, rest) ->
        let cond_parts, bind_lines =
          List.fold_left
            (fun (conds, binds) (f : AST.record_pattern_field) ->
              let access = Printf.sprintf "__scrutinee.%s" (go_record_field_name f.pat_field_name) in
              match f.pat_field_pattern with
              | None -> (conds, Printf.sprintf "%s := %s" f.pat_field_name access :: binds)
              | Some p -> (
                  match p.AST.pat with
                  | AST.PWildcard -> (conds, binds)
                  | AST.PVariable name -> (conds, Printf.sprintf "%s := %s" name access :: binds)
                  | AST.PLiteral lit -> (Printf.sprintf "(%s == %s)" access (emit_lit lit) :: conds, binds)
                  | _ -> failwith "Complex record patterns in codegen are not yet supported"))
            ([], []) fields
        in
        let bind_lines =
          match rest with
          | None -> bind_lines
          | Some rest_name -> Printf.sprintf "%s := __scrutinee" rest_name :: bind_lines
        in
        let cond_str =
          match List.rev cond_parts with
          | [] -> "true"
          | cs -> String.concat " && " cs
        in
        let binds_block =
          match List.rev bind_lines with
          | [] -> ""
          | lines -> String.concat "\n" (List.map (fun l -> "\t\t" ^ l) lines) ^ "\n"
        in
        Printf.sprintf "%s %s {\n%s%s\t}" branch_prefix_if cond_str binds_block body_code
    | _ -> failwith "Only record, wildcard, or variable patterns are supported for record match codegen"
  in
  let arm_blocks =
    List.mapi
      (fun idx (arm : AST.match_arm) ->
        match arm.patterns with
        | [ pattern ] -> emit_record_arm pattern arm.body (idx = 0)
        | [] -> failwith "Match arm must have at least one pattern"
        | _ -> failwith "Multiple patterns per arm not yet supported in codegen")
      arms
  in
  match target with
  | Some ReturnTarget | Some DiscardTarget ->
      (* Panic needed after if-chain for Go control flow analysis when no wildcard *)
      let ind = indent_str state in
      Printf.sprintf "%s__scrutinee := %s\n%s\n%spanic(\"non-exhaustive record match\")\n" ind scrutinee_str
        (String.concat "" arm_blocks) ind
  | Some (AssignTarget _) ->
      (* No panic for assignment — execution continues after the if-chain *)
      let ind = indent_str state in
      Printf.sprintf "%s__scrutinee := %s\n%s\n" ind scrutinee_str (String.concat "" arm_blocks)
  | None ->
      Printf.sprintf "(func() %s {\n\t__scrutinee := %s\n%s\n\tpanic(\"non-exhaustive record match\")\n})()"
        match_result_go_type scrutinee_str (String.concat "" arm_blocks)

and emit_match_enum ?target state type_map env scrutinee scrutinee_type enum_name type_args arms match_result_type
    =
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

  match target with
  | Some t ->
      let emit_case_target (arm : AST.match_arm) =
        match arm.patterns with
        | [ pattern ] -> (
            let emit_arm_body () = with_indent_delta state 2 (fun () -> emit_expr_to_target state type_map env arm.body t) in
            match pattern.AST.pat with
            | AST.PWildcard ->
                let ind = indent_str state in
                Printf.sprintf "%sdefault:\n%s" (ind ^ "    ") (emit_arm_body ())
            | AST.PVariable var_name ->
                let ind = indent_str state in
                Printf.sprintf "%sdefault:\n%s%s := __scrutinee\n%s" (ind ^ "    ") (ind ^ "        ") var_name
                  (emit_arm_body ())
            | AST.PConstructor (enum_name_pat, variant_name, field_patterns) ->
                let variant_opt =
                  List.find_opt (fun (v : Typecheck.Enum_registry.variant_def) -> v.name = variant_name) enum_def.variants
                in
                let variant =
                  match variant_opt with
                  | Some v -> v
                  | None -> failwith (Printf.sprintf "Unknown variant %s.%s" enum_name_pat variant_name)
                in
                let tag_constant = Printf.sprintf "%s_%s_tag" go_type_name variant_name in
                let layout = analyze_enum_layout state.mono enum_def type_args in
                let bindings = emit_pattern_bindings layout variant_name field_patterns variant.fields in
                let ind = indent_str state in
                let binding_lines =
                  List.map
                    (fun (var_name, data_field_name, _field_type) ->
                      Printf.sprintf "%s%s := __scrutinee.%s\n" (ind ^ "        ") var_name data_field_name)
                    bindings
                  |> String.concat ""
                in
                Printf.sprintf "%scase %s:\n%s%s" (ind ^ "    ") tag_constant binding_lines (emit_arm_body ())
            | AST.PLiteral _ -> failwith "Literal patterns not supported for enum match"
            | AST.PRecord _ -> failwith "Record patterns not yet implemented in enum match")
        | [] -> failwith "Match arm must have at least one pattern"
        | _ -> failwith "Multiple patterns per arm not yet supported in codegen"
      in
      let match_body = String.concat "" (List.map emit_case_target arms) in
      let ind = indent_str state in
      if has_wildcard then
        Printf.sprintf "%s__scrutinee := %s\n%sswitch __scrutinee.Tag {\n%s\n%s}\n" ind scrutinee_str ind
          match_body ind
      else
        Printf.sprintf
          "%s__scrutinee := %s\n%sswitch __scrutinee.Tag {\n%s\n%sdefault:\n%s\tpanic(\"unreachable: exhaustive match\")\n%s}\n"
          ind scrutinee_str ind match_body ind ind ind
  | None ->
      let result_prefix = "return " in
      let match_body =
        let cases =
          List.map
            (fun (arm : AST.match_arm) ->
              match arm.patterns with
              | [ pattern ] ->
                  emit_match_arm_enum ~result_prefix state type_map env go_type_name enum_def type_args pattern
                    arm.body
              | [] -> failwith "Match arm must have at least one pattern"
              | _ -> failwith "Multiple patterns per arm not yet supported in codegen")
            arms
        in
        String.concat "\n" cases
      in
      if has_wildcard then
        Printf.sprintf "(func() %s {\n\t__scrutinee := %s\n\tswitch __scrutinee.Tag {\n%s\n\t}\n})()"
          match_result_go_type scrutinee_str match_body
      else
        Printf.sprintf
          "(func() %s {\n\t__scrutinee := %s\n\tswitch __scrutinee.Tag {\n%s\n\tdefault:\n\t\tpanic(\"unreachable: exhaustive match\")\n\t}\n})()"
          match_result_go_type scrutinee_str match_body

and emit_match_primitive ?target state type_map env scrutinee scrutinee_type arms match_result_type =
  (* Emit scrutinee to a temporary variable *)
  let scrutinee_str = emit_expr state type_map env scrutinee in

  (* Convert match result type to Go type *)
  let match_result_go_type = type_to_go state.mono match_result_type in

  match target with
  | Some t ->
      let emit_case_target (arm : AST.match_arm) =
        match arm.patterns with
        | [ pattern ] -> (
            let emit_arm_body () = with_indent_delta state 2 (fun () -> emit_expr_to_target state type_map env arm.body t) in
            match pattern.AST.pat with
            | AST.PWildcard ->
                let ind = indent_str state in
                Printf.sprintf "%sdefault:\n%s" (ind ^ "    ") (emit_arm_body ())
            | AST.PVariable var_name ->
                let ind = indent_str state in
                Printf.sprintf "%sdefault:\n%s%s := __scrutinee\n%s" (ind ^ "    ") (ind ^ "        ") var_name
                  (emit_arm_body ())
            | AST.PLiteral lit ->
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
                let ind = indent_str state in
                Printf.sprintf "%scase %s:\n%s" (ind ^ "    ") lit_str (emit_arm_body ())
            | AST.PConstructor _ -> failwith "Constructor patterns not valid for primitive match"
            | AST.PRecord _ -> failwith "Record patterns not valid for primitive match")
        | [] -> failwith "Match arm must have at least one pattern"
        | _ -> failwith "Multiple patterns per arm not yet supported in codegen"
      in
      let match_body = String.concat "" (List.map emit_case_target arms) in
      let ind = indent_str state in
      Printf.sprintf "%s__scrutinee := %s\n%sswitch __scrutinee {\n%s\n%s}\n" ind scrutinee_str ind match_body ind
  | None ->
      let result_prefix = "return " in
      let match_body =
        let cases =
          List.map
            (fun (arm : AST.match_arm) ->
              match arm.patterns with
              | [ pattern ] ->
                  emit_match_arm_primitive ~result_prefix state type_map env scrutinee_type pattern arm.body
              | [] -> failwith "Match arm must have at least one pattern"
              | _ -> failwith "Multiple patterns per arm not yet supported in codegen")
            arms
        in
        String.concat "\n" cases
      in
      Printf.sprintf "(func() %s {\n\t__scrutinee := %s\n\tswitch __scrutinee {\n%s\n\t}\n})()"
        match_result_go_type scrutinee_str match_body

and emit_match_arm_primitive ?(result_prefix = "return ") state type_map env _scrutinee_type pattern body =
  match pattern.AST.pat with
  | AST.PWildcard ->
      let body_str = emit_expr state type_map env body in
      Printf.sprintf "\tdefault:\n\t\t%s%s" result_prefix body_str
  | AST.PVariable var_name ->
      (* Variable pattern binds the scrutinee *)
      let body_str = emit_expr state type_map env body in
      Printf.sprintf "\tdefault:\n\t\t%s := __scrutinee\n\t\t%s%s" var_name result_prefix body_str
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
      Printf.sprintf "\tcase %s:\n\t\t%s%s" lit_str result_prefix body_str
  | AST.PConstructor _ -> failwith "Constructor patterns not valid for primitive match"
  | AST.PRecord _ -> failwith "Record patterns not valid for primitive match"

and emit_match_arm_enum
    ?(result_prefix = "return ") state type_map env go_type_name enum_def type_args pattern body =
  match pattern.AST.pat with
  | AST.PWildcard ->
      (* Wildcard matches everything - use default case *)
      let body_str = emit_expr state type_map env body in
      Printf.sprintf "\tdefault:\n\t\t%s%s" result_prefix body_str
  | AST.PLiteral _lit ->
      (* Literal patterns - not for enums, shouldn't happen *)
      failwith "Literal patterns not supported for enum match"
  | AST.PVariable _var_name ->
      (* Variable pattern - binds the entire enum value *)
      (* For now, treat as wildcard since we don't track variable bindings in Go codegen *)
      let body_str = emit_expr state type_map env body in
      Printf.sprintf "\tdefault:\n\t\t%s%s" result_prefix body_str
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

      Printf.sprintf "\tcase %s:\n%s\t\t%s%s" tag_constant bindings_code result_prefix body_str
  | AST.PRecord _ -> failwith "Record patterns not yet implemented in enum match"

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
  let operand_type = Types.canonicalize_mono_type (get_type type_map operand) in
  match op with
  | "!" -> Printf.sprintf "!(%s)" operand_str
  | "-" -> (
      match operand_type with
      | Types.TInt | Types.TFloat -> Printf.sprintf "-(%s)" operand_str
      | _ ->
          let type_suffix = mangle_type operand_type in
          Printf.sprintf "neg_neg_%s(%s)" type_suffix operand_str)
  | _ -> failwith ("Unknown prefix operator: " ^ op)

and emit_infix state type_map env left op right =
  let left_str = emit_expr state type_map env left in
  let right_str = emit_expr state type_map env right in
  let left_type = Types.canonicalize_mono_type (get_type type_map left) in
  let type_suffix = mangle_type left_type in
  let is_num_primitive =
    match left_type with
    | Types.TInt | Types.TFloat -> true
    | _ -> false
  in
  let is_eq_primitive =
    match left_type with
    | Types.TInt | Types.TFloat | Types.TBool | Types.TString -> true
    | _ -> false
  in
  let is_ord_primitive =
    match left_type with
    | Types.TInt | Types.TFloat | Types.TString -> true
    | _ -> false
  in
  match op with
  | "+" ->
      if left_type = Types.TString then
        Printf.sprintf "(%s + %s)" left_str right_str
      else if is_num_primitive then
        Printf.sprintf "(%s + %s)" left_str right_str
      else
        Printf.sprintf "num_add_%s(%s, %s)" type_suffix left_str right_str
  | "-" ->
      if is_num_primitive then
        Printf.sprintf "(%s - %s)" left_str right_str
      else
        Printf.sprintf "num_sub_%s(%s, %s)" type_suffix left_str right_str
  | "*" ->
      if is_num_primitive then
        Printf.sprintf "(%s * %s)" left_str right_str
      else
        Printf.sprintf "num_mul_%s(%s, %s)" type_suffix left_str right_str
  | "/" ->
      if is_num_primitive then
        Printf.sprintf "(%s / %s)" left_str right_str
      else
        Printf.sprintf "num_div_%s(%s, %s)" type_suffix left_str right_str
  | "==" ->
      if is_eq_primitive then
        Printf.sprintf "(%s == %s)" left_str right_str
      else
        Printf.sprintf "eq_eq_%s(%s, %s)" type_suffix left_str right_str
  | "!=" ->
      if is_eq_primitive then
        Printf.sprintf "(%s != %s)" left_str right_str
      else
        Printf.sprintf "!eq_eq_%s(%s, %s)" type_suffix left_str right_str
  | "<" ->
      if is_ord_primitive then
        Printf.sprintf "(%s < %s)" left_str right_str
      else
        Printf.sprintf "(ord_compare_%s(%s, %s) == int64(0))" type_suffix left_str right_str
  | ">" ->
      if is_ord_primitive then
        Printf.sprintf "(%s > %s)" left_str right_str
      else
        Printf.sprintf "(ord_compare_%s(%s, %s) == int64(2))" type_suffix left_str right_str
  | "<=" ->
      if is_ord_primitive then
        Printf.sprintf "(%s <= %s)" left_str right_str
      else
        Printf.sprintf "(ord_compare_%s(%s, %s) != int64(2))" type_suffix left_str right_str
  | ">=" ->
      if is_ord_primitive then
        Printf.sprintf "(%s >= %s)" left_str right_str
      else
        Printf.sprintf "(ord_compare_%s(%s, %s) != int64(0))" type_suffix left_str right_str
  | _ -> failwith ("Unknown infix operator: " ^ op)

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

(* Helper: Substitute identifier references in expressions/statements. *)
and make_identifier_substituter old_name new_name =
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
    | AST.RecordLit (fields, spread) ->
        let fields' =
          List.map
            (fun (f : AST.record_field) ->
              {
                f with
                field_value =
                  (match f.field_value with
                  | Some v -> Some (subst_expr v)
                  | None -> None);
              })
            fields
        in
        let spread' = Option.map subst_expr spread in
        { e with expr = AST.RecordLit (fields', spread') }
    | AST.FieldAccess (recv, field) -> { e with expr = AST.FieldAccess (subst_expr recv, field) }
    | AST.MethodCall (recv, name, args) ->
        { e with expr = AST.MethodCall (subst_expr recv, name, List.map subst_expr args) }
    | AST.Match (scrutinee, arms) ->
        let arms' =
          List.map
            (fun (arm : AST.match_arm) ->
              (* Keep patterns unchanged; substitute only in arm body. *)
              { arm with body = subst_expr arm.body })
            arms
        in
        { e with expr = AST.Match (subst_expr scrutinee, arms') }
    | AST.Function _ ->
        (* Don't substitute inside function bodies - would need scope tracking. *)
        e
    | _ -> e
  and subst_stmt s =
    match s.AST.stmt with
    | AST.Let let_binding -> { s with stmt = AST.Let { let_binding with value = subst_expr let_binding.value } }
    | AST.Return e -> { s with stmt = AST.Return (subst_expr e) }
    | AST.ExpressionStmt e -> { s with stmt = AST.ExpressionStmt (subst_expr e) }
    | AST.Block stmts -> { s with stmt = AST.Block (List.map subst_stmt stmts) }
    | AST.EnumDef _ -> s
    | AST.TraitDef _ | AST.ImplDef _ | AST.DeriveDef _ | AST.TypeAlias _ -> s
  in
  (subst_expr, subst_stmt)

and substitute_identifier_in_expr old_name new_name expr =
  let subst_expr, _ = make_identifier_substituter old_name new_name in
  subst_expr expr

and substitute_identifier_in_stmt old_name new_name stmt =
  let _, subst_stmt = make_identifier_substituter old_name new_name in
  subst_stmt stmt

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
  let emit_return_expr branch_env e =
    if result_type = Types.TNull then
      inner_ind
      ^ "        _ = "
      ^ emit_expr state type_map branch_env e
      ^ "\n"
      ^ inner_ind
      ^ "        return struct{}{}\n"
    else
      inner_ind ^ "        return " ^ emit_expr state type_map branch_env e ^ "\n"
  in

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
                  emit_return_expr env' e_subst
              | AST.Return e ->
                  let e_subst = substitute_identifier_in_expr var_name var_to_use e in
                  emit_return_expr env' e_subst
              | _ -> fst (emit_stmt state type_map env' last)
            in
            var_marker ^ prefix ^ last_str)
    | AST.ExpressionStmt e ->
        let e_subst = substitute_identifier_in_expr var_name var_to_use e in
        var_marker ^ emit_return_expr narrow_env e_subst
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
                  emit_return_expr env' e_subst
              | AST.Return e ->
                  let e_subst = substitute_identifier_in_expr original_var var_to_use e in
                  emit_return_expr env' e_subst
              | _ -> fst (emit_stmt state type_map env' last)
            in
            type_assertion_prefix ^ var_marker ^ prefix ^ last_str)
    | AST.ExpressionStmt e ->
        let e_subst = substitute_identifier_in_expr original_var var_to_use e in
        type_assertion_prefix ^ var_marker ^ emit_return_expr narrow_env e_subst
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

and emit_type_switch_to_target
    state
    type_map
    env
    var_name
    narrow_type
    complement_type_opt
    result_type
    cons
    alt
    target =
  let ind = indent_str state in
  let inner_ind = ind ^ "    " in
  let value_target =
    if result_type = Types.TNull then
      DiscardTarget
    else
      target
  in
  let narrow_type_str = type_to_go state.mono narrow_type in
  let narrowed_var = var_name ^ "_typed" in
  let emit_branch_to_target branch_env stmt =
    match stmt.AST.stmt with
    | AST.Block stmts -> (
        match List.rev stmts with
        | [] -> ""
        | last :: rest ->
            let stmts_prefix, env' = emit_stmts state type_map branch_env (List.rev rest) in
            let last_str =
              match last.stmt with
              | AST.ExpressionStmt e -> with_indent_delta state 1 (fun () -> emit_expr_to_target state type_map env' e value_target)
              | AST.Return e -> inner_ind ^ "    return " ^ emit_expr state type_map env' e ^ "\n"
              | _ -> fst (emit_stmt state type_map env' last)
            in
            stmts_prefix ^ last_str)
    | AST.ExpressionStmt e -> with_indent_delta state 1 (fun () -> emit_expr_to_target state type_map branch_env e value_target)
    | AST.Return e -> inner_ind ^ "    return " ^ emit_expr state type_map branch_env e ^ "\n"
    | _ -> fst (emit_stmt state type_map branch_env stmt)
  in
  let cons_stmt = substitute_identifier_in_stmt var_name narrowed_var cons in
  let narrowed_env = Infer.TypeEnv.add narrowed_var (Types.Forall ([], narrow_type)) env in
  let cons_str =
    let marker = inner_ind ^ "        _ = " ^ narrowed_var ^ "\n" in
    marker ^ emit_branch_to_target narrowed_env cons_stmt
  in
  let default_branch =
    match alt with
    | None -> ""
    | Some alt_stmt -> (
        match complement_type_opt with
        | Some complement_type when complement_type <> Types.TUnion [] ->
            let complement_var = var_name ^ "_complement" in
            let complement_type_str = type_to_go state.mono complement_type in
            let alt_stmt' = substitute_identifier_in_stmt var_name complement_var alt_stmt in
            let complement_env = Infer.TypeEnv.add complement_var (Types.Forall ([], complement_type)) env in
            let assertion = inner_ind ^ "        " ^ complement_var ^ " := " ^ var_name ^ ".(" ^ complement_type_str ^ ")\n" in
            let marker = inner_ind ^ "        _ = " ^ complement_var ^ "\n" in
            let body = emit_branch_to_target complement_env alt_stmt' in
            ind ^ "    default:\n" ^ assertion ^ marker ^ body
        | _ ->
            let body = emit_branch_to_target env alt_stmt in
            ind ^ "    default:\n" ^ body)
  in
  let switch_code =
    Printf.sprintf "%sswitch %s := %s.(type) {\n%scase %s:\n%s%s%s}\n" ind narrowed_var var_name ind narrow_type_str
      cons_str default_branch ind
  in
  let unit_tail =
    if result_type <> Types.TNull then
      ""
    else
      match target with
      | ReturnTarget -> ind ^ "return struct{}{}\n"
      | AssignTarget name -> ind ^ name ^ " = struct{}{}\n"
      | DiscardTarget -> ""
  in
  switch_code ^ unit_tail

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
      let emit_return_expr branch_env e =
        if result_type = Types.TNull then
          inner_ind
          ^ "    _ = "
          ^ emit_expr state type_map branch_env e
          ^ "\n"
          ^ inner_ind
          ^ "    return struct{}{}\n"
        else
          inner_ind ^ "    return " ^ emit_expr state type_map branch_env e ^ "\n"
      in

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
                  | AST.ExpressionStmt e -> emit_return_expr env' e
                  | AST.Return e -> emit_return_expr env' e
                  | _ -> fst (emit_stmt state type_map env' last)
                in
                prefix ^ last_str)
        | AST.ExpressionStmt e -> emit_return_expr env e
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

and emit_expr_to_target state type_map env expr target =
  let ind = indent_str state in
  match expr.AST.expr with
  | AST.If (cond, cons, alt) -> with_indent_delta state 1 (fun () -> emit_if_to_target state type_map env expr cond cons alt target)
  | AST.Match (scrutinee, arms) ->
      with_indent_delta state 1 (fun () -> emit_match ~target state type_map env expr scrutinee arms)
  | _ -> ind ^ target_prefix target ^ emit_expr state type_map env expr ^ "\n"

and emit_if_to_target state type_map env if_expr (cond : AST.expression) cons alt target =
  let result_type = get_type type_map if_expr in
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
      let complement_type_opt =
        match Infer.TypeEnv.find_opt var_name env with
        | Some (Types.Forall ([], original_type)) -> compute_complement_type original_type narrow_type
        | _ -> None
      in
      emit_type_switch_to_target state type_map env var_name narrow_type complement_type_opt result_type cons alt target
  | None ->
      let ind = indent_str state in
      let inner_ind = ind ^ "    " in
      let cond_str = emit_expr state type_map env cond in
      let value_target =
        if result_type = Types.TNull then
          DiscardTarget
        else
          target
      in
      let emit_branch stmt =
        match stmt.AST.stmt with
        | AST.Block stmts -> (
            match List.rev stmts with
            | [] -> ""
            | last :: rest ->
                let stmts_prefix, env' = emit_stmts state type_map env (List.rev rest) in
                let last_str =
                  match last.stmt with
                  | AST.ExpressionStmt e -> with_indent_delta state 1 (fun () -> emit_expr_to_target state type_map env' e value_target)
                  | AST.Return e -> inner_ind ^ "    return " ^ emit_expr state type_map env' e ^ "\n"
                  | _ -> fst (emit_stmt state type_map env' last)
                in
                stmts_prefix ^ last_str)
        | AST.ExpressionStmt e -> with_indent_delta state 1 (fun () -> emit_expr_to_target state type_map env e value_target)
        | AST.Return e -> inner_ind ^ "    return " ^ emit_expr state type_map env e ^ "\n"
        | _ -> fst (emit_stmt state type_map env stmt)
      in
      let cons_str = emit_branch cons in
      let if_code =
        match alt with
        | Some alt_stmt ->
            let alt_str = emit_branch alt_stmt in
            Printf.sprintf "%sif %s {\n%s%s} else {\n%s%s}\n" ind cond_str cons_str ind alt_str ind
        | None -> Printf.sprintf "%sif %s {\n%s%s}\n" ind cond_str cons_str ind
      in
      let unit_tail =
        if result_type <> Types.TNull then
          ""
        else
          match target with
          | ReturnTarget -> ind ^ "return struct{}{}\n"
          | AssignTarget name -> ind ^ name ^ " = struct{}{}\n"
          | DiscardTarget -> ""
      in
      if_code ^ unit_tail

and emit_call state type_map env func args =
  let callee_type_opt =
    match func.expr with
    | AST.Identifier name -> (
        match Infer.TypeEnv.find_opt name env with
        | Some (Types.Forall (_, t)) -> Some t
        | None -> Hashtbl.find_opt type_map func.id)
    | _ -> Hashtbl.find_opt type_map func.id
  in
  let call_signature_opt =
    match callee_type_opt with
    | Some callee_type -> callable_signature_exact (List.length args) callee_type
    | None -> None
  in
  let call_param_types =
    match call_signature_opt with
    | Some (params, _ret) -> params
    | None -> (
        match callee_type_opt with
        | Some callee_type -> fst (extract_param_types (List.length args) callee_type)
        | None -> [])
  in
  let emit_args_with_expected_types (expected_types : Types.mono_type list) : string =
    List.mapi
      (fun i arg ->
        let expected =
          if i < List.length expected_types then
            Some (List.nth expected_types i)
          else
            None
        in
        emit_expr ~expected_type:expected state type_map env arg)
      args
    |> String.concat ", "
  in
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
      let args_str = emit_args_with_expected_types param_types in
      Printf.sprintf "%s(%s)" mangled_name args_str
  | _ ->
      let func_str = emit_expr state type_map env func in
      let args_str = emit_args_with_expected_types call_param_types in
      (match (callee_type_opt, call_signature_opt) with
      | Some (Types.TUnion _), Some (params, ret) ->
          let callable_type = List.fold_right (fun p acc -> Types.TFun (p, acc, false)) params ret in
          let callable_go_type = type_to_go state.mono callable_type in
          Printf.sprintf "(%s.(%s))(%s)" func_str callable_go_type args_str
      | _ -> Printf.sprintf "%s(%s)" func_str args_str)

and emit_array ?expected_elem_type state type_map env elements =
  match elements with
  | [] ->
      let elem_type_str =
        match expected_elem_type with
        | Some t -> type_to_go state.mono t
        | None -> "interface{}"
      in
      Printf.sprintf "[]%s{}" elem_type_str
  | first :: _ ->
      let elem_type =
        match expected_elem_type with
        | Some t -> t
        | None -> get_type type_map first
      in
      let elem_type_str = type_to_go state.mono elem_type in
      let emit_elem e = emit_expr ~expected_type:(Some elem_type) state type_map env e in
      let elems_str = List.map emit_elem elements |> String.concat ", " in
      Printf.sprintf "[]%s{%s}" elem_type_str elems_str

and emit_hash ?expected_hash_types state type_map env pairs =
  match pairs with
  | [] -> (
      match expected_hash_types with
      | Some (key_t, val_t) ->
          Printf.sprintf "map[%s]%s{}" (type_to_go state.mono key_t) (type_to_go state.mono val_t)
      | None -> "map[interface{}]interface{}{}")
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

and emit_function_expr ?func_type_override state type_map env func_expr params body =
  (* Inline anonymous function - no monomorphization needed *)
  let inferred_func_type = get_type type_map func_expr in
  let func_type =
    match func_type_override with
    | Some t -> t
    | None -> inferred_func_type
  in
  let arity = List.length params in
  let param_types, return_type =
    match extract_param_types_exact arity func_type with
    | Some types -> types
    | None ->
        failwith
          (Printf.sprintf
             "Codegen error: function literal expr id %d expected %d parameters but inferred type %s"
             func_expr.id arity (Types.to_string func_type))
  in

  let param_names =
    List.map
      (fun (p : AST.expression) ->
        match p.expr with
        | AST.Identifier name -> name
        | _ -> failwith "Function parameter must be identifier")
      params
  in

  if List.length param_names <> List.length param_types then
    failwith
      (Printf.sprintf
         "Codegen error: function literal expr id %d parameter name/type arity mismatch (%d names vs %d types)"
         func_expr.id (List.length param_names) (List.length param_types));

  let params_with_types =
    List.map2 (fun name typ -> name ^ " " ^ type_to_go state.mono typ) param_names param_types
  in
  let params_str = String.concat ", " params_with_types in
  let return_type_str = type_to_go state.mono return_type in

  (* Extend environment with parameter bindings for the body *)
  let body_env = add_param_bindings env param_names param_types in

  let body_type_map =
    match Unify.unify inferred_func_type func_type with
    | Ok subst ->
        let specialized_type_map = Infer.create_type_map () in
        copy_specialized_stmt_types type_map specialized_type_map subst body;
        specialized_type_map
    | Error _ -> type_map
  in

  let body_str = emit_func_body state body_type_map body_env body in

  Printf.sprintf "func(%s) %s {\n%s%s}" params_str return_type_str body_str (indent_str state)

and emit_func_body state type_map env stmt =
  let ind = indent_str state in
  let inner_ind = ind ^ "    " in
  (* Emit a tail-position expression, flattening if/match to avoid IIFE *)
  let emit_tail_expr env' e =
    match e.AST.expr with
    | AST.If (cond, cons, alt) ->
        state.indent <- state.indent + 1;
        let r = emit_if_to_target state type_map env' e cond cons alt ReturnTarget in
        state.indent <- state.indent - 1;
        r
    | AST.Match (scrutinee, arms) ->
        state.indent <- state.indent + 1;
        let r = emit_match ~target:ReturnTarget state type_map env' e scrutinee arms in
        state.indent <- state.indent - 1;
        r
    | _ -> inner_ind ^ "return " ^ emit_expr state type_map env' e ^ "\n"
  in
  match stmt.AST.stmt with
  | AST.Block stmts -> (
      match List.rev stmts with
      | [] -> inner_ind ^ "return\n"
      | last :: rest ->
          let rest_stmts = List.rev rest in
          let emit_prefix_with_tail_lookahead (tail_stmt : AST.statement) =
            let rec go env_acc code_acc remaining =
              match remaining with
              | [] -> (String.concat "" (List.rev code_acc), env_acc)
              | stmt :: tl ->
                  let following = tl @ [ tail_stmt ] in
                  let code, env' = emit_stmt ~following_stmts:following state type_map env_acc stmt in
                  go env' (code :: code_acc) tl
            in
            go env [] rest_stmts
          in
          let prefix, env' = emit_prefix_with_tail_lookahead last in
          let last_str =
            match last.stmt with
            | AST.ExpressionStmt e -> emit_tail_expr env' e
            | AST.Return e -> emit_tail_expr env' e
            | _ -> fst (emit_stmt state type_map env' last)
          in
          prefix ^ last_str)
  | AST.ExpressionStmt e -> emit_tail_expr env e
  | _ -> fst (emit_stmt state type_map env stmt)

(* ============================================================
   Statement Emission
   ============================================================ *)

and type_from_env_or_map
    (env : Infer.type_env)
    (type_map : Infer.type_map)
    (expr : AST.expression) : Types.mono_type option =
  match expr.expr with
  | AST.Identifier id -> (
      match Infer.TypeEnv.find_opt id env with
      | Some (Types.Forall (_, t)) -> Some t
      | None -> Hashtbl.find_opt type_map expr.id)
  | _ -> Hashtbl.find_opt type_map expr.id

and collect_local_call_arg_types_expr
    (name : string)
    (type_map : Infer.type_map)
    (env : Infer.type_env)
    (expr : AST.expression) : Types.mono_type list list =
  let all_some (xs : 'a option list) : 'a list option =
    let rec go acc = function
      | [] -> Some (List.rev acc)
      | None :: _ -> None
      | Some x :: rest -> go (x :: acc) rest
    in
    go [] xs
  in
  let nested =
    match expr.expr with
    | AST.Integer _ | AST.Float _ | AST.Boolean _ | AST.String _ | AST.Identifier _ -> []
    | AST.Prefix (_, e) -> collect_local_call_arg_types_expr name type_map env e
    | AST.Infix (l, _, r) ->
        collect_local_call_arg_types_expr name type_map env l @ collect_local_call_arg_types_expr name type_map env r
    | AST.TypeCheck (e, _) -> collect_local_call_arg_types_expr name type_map env e
    | AST.If (cond, cons, alt) ->
        collect_local_call_arg_types_expr name type_map env cond
        @ collect_local_call_arg_types_stmt name type_map env cons
        @
        (match alt with
        | Some s -> collect_local_call_arg_types_stmt name type_map env s
        | None -> [])
    | AST.Call (callee, args) ->
        collect_local_call_arg_types_expr name type_map env callee
        @ List.concat_map (collect_local_call_arg_types_expr name type_map env) args
    | AST.Array elements -> List.concat_map (collect_local_call_arg_types_expr name type_map env) elements
    | AST.Hash pairs ->
        List.concat_map
          (fun (k, v) ->
            collect_local_call_arg_types_expr name type_map env k
            @ collect_local_call_arg_types_expr name type_map env v)
          pairs
    | AST.Index (container, index) ->
        collect_local_call_arg_types_expr name type_map env container
        @ collect_local_call_arg_types_expr name type_map env index
    | AST.Function _ ->
        (* Function literals introduce a nested scope; skip usage inference across scope boundaries. *)
        []
    | AST.EnumConstructor (_, _, args) ->
        List.concat_map (collect_local_call_arg_types_expr name type_map env) args
    | AST.Match (scrutinee, arms) ->
        collect_local_call_arg_types_expr name type_map env scrutinee
        @ List.concat_map
            (fun (arm : AST.match_arm) -> collect_local_call_arg_types_expr name type_map env arm.body)
            arms
    | AST.RecordLit (fields, spread) ->
        List.concat_map
          (fun (field : AST.record_field) ->
            match field.field_value with
            | Some v -> collect_local_call_arg_types_expr name type_map env v
            | None -> [])
          fields
        @
        (match spread with
        | Some s -> collect_local_call_arg_types_expr name type_map env s
        | None -> [])
    | AST.FieldAccess (receiver, _) -> collect_local_call_arg_types_expr name type_map env receiver
    | AST.MethodCall (receiver, _, args) ->
        collect_local_call_arg_types_expr name type_map env receiver
        @ List.concat_map (collect_local_call_arg_types_expr name type_map env) args
  in
  match expr.expr with
  | AST.Call ({ expr = AST.Identifier callee_name; _ }, args) when callee_name = name -> (
      match List.map (type_from_env_or_map env type_map) args |> all_some with
      | Some arg_types -> arg_types :: nested
      | None -> nested)
  | _ -> nested

and collect_local_call_arg_types_stmt
    (name : string)
    (type_map : Infer.type_map)
    (env : Infer.type_env)
    (stmt : AST.statement) : Types.mono_type list list =
  match stmt.stmt with
  | AST.Let { value; _ } -> collect_local_call_arg_types_expr name type_map env value
  | AST.Return e | AST.ExpressionStmt e -> collect_local_call_arg_types_expr name type_map env e
  | AST.Block stmts -> List.concat_map (collect_local_call_arg_types_stmt name type_map env) stmts
  | AST.ImplDef impl ->
      List.concat_map
        (fun (m : AST.method_impl) -> collect_local_call_arg_types_expr name type_map env m.impl_method_body)
        impl.impl_methods
  | AST.EnumDef _ | AST.TraitDef _ | AST.DeriveDef _ | AST.TypeAlias _ -> []

and resolve_local_function_type_from_calls
    (name : string)
    (declared_type : Types.mono_type)
    (type_map : Infer.type_map)
    (env : Infer.type_env)
    (following_stmts : AST.statement list) : Types.mono_type option =
  let declared_params, _ = extract_all_param_types declared_type in
  let declared_arity = List.length declared_params in
  if declared_arity = 0 then
    None
  else
    let call_arg_types =
      List.concat_map (collect_local_call_arg_types_stmt name type_map env) following_stmts
      |> List.filter (fun arg_types -> List.length arg_types = declared_arity)
    in
    let unify_param_lists subst arg_types =
      let rec go subst_acc params args =
        match (params, args) with
        | [], [] -> Some subst_acc
        | param :: params_tail, arg :: args_tail ->
            let param' = Types.apply_substitution subst_acc param in
            let arg' = Types.apply_substitution subst_acc arg in
            (match Unify.unify param' arg' with
            | Ok new_subst -> go (Types.compose_substitution new_subst subst_acc) params_tail args_tail
            | Error _ -> None)
        | _ -> None
      in
      go subst declared_params arg_types
    in
    let rec refine subst = function
      | [] -> Some subst
      | arg_types :: rest -> (
          match unify_param_lists subst arg_types with
          | Some subst' -> refine subst' rest
          | None -> None)
    in
    match refine Types.empty_substitution call_arg_types with
    | Some subst ->
        let resolved = Types.apply_substitution subst declared_type in
        if has_type_vars resolved then
          None
        else
          Some resolved
    | None -> None

and emit_stmt
    ?(following_stmts : AST.statement list = [])
    (state : emit_state)
    (type_map : Infer.type_map)
    (env : Infer.type_env)
    (stmt : AST.statement) :
    string * Infer.type_env =
  let ind = indent_str state in
  match stmt.stmt with
  | AST.Let let_binding -> (
      if let_binding.name = "_" then
        (* Blank binding is a discard, not a named variable declaration. *)
        (match let_binding.value.expr with
        | AST.If (cond, cons, alt) ->
            let code = emit_if_to_target state type_map env let_binding.value cond cons alt DiscardTarget in
            (code, env)
        | AST.Match (scrutinee, arms) ->
            let code = emit_match ~target:DiscardTarget state type_map env let_binding.value scrutinee arms in
            (code, env)
        | _ ->
            let expr_type = get_type type_map let_binding.value in
            let expr_str = emit_expr ~expected_type:(Some expr_type) state type_map env let_binding.value in
            (Printf.sprintf "%s_ = %s\n" ind expr_str, env))
      else
      (* Use the type from the environment if it exists, otherwise get from type_map *)
      let expr_type =
        match Infer.TypeEnv.find_opt let_binding.name env with
        | Some (Types.Forall (_, t)) -> t
        | None -> get_type type_map let_binding.value
      in
      let expr_type =
        match let_binding.value.expr with
        | AST.Function _ when has_type_vars expr_type -> (
            match resolve_local_function_type_from_calls let_binding.name expr_type type_map env following_stmts with
            | Some resolved -> resolved
            | None -> expr_type)
        | _ -> expr_type
      in
      match let_binding.value.expr with
      | AST.Function _ when is_user_func state.mono let_binding.name ->
          (* Skip - this is a top-level function, emitted separately *)
          let env' = Infer.TypeEnv.add let_binding.name (Types.Forall ([], expr_type)) env in
          ("", env')
      | AST.Function _ ->
          (* Emit local function values with split declaration+assignment.
             This keeps the name in scope inside the function literal body for recursion/captures. *)
          let expr_str = emit_expr ~expected_type:(Some expr_type) state type_map env let_binding.value in
          let go_type = type_to_go state.mono expr_type in
          let binding_code =
            Printf.sprintf "%svar %s %s\n%s%s = %s\n%s_ = %s\n" ind let_binding.name go_type ind let_binding.name
              expr_str ind let_binding.name
          in
          let env' = Infer.TypeEnv.add let_binding.name (Types.Forall ([], expr_type)) env in
          (binding_code, env')
      | AST.If (cond, cons, alt) ->
          let go_type = type_to_go state.mono expr_type in
          let decl = Printf.sprintf "%svar %s %s\n" ind let_binding.name go_type in
          let if_code =
            emit_if_to_target state type_map env let_binding.value cond cons alt (AssignTarget let_binding.name)
          in
          let env' = Infer.TypeEnv.add let_binding.name (Types.Forall ([], expr_type)) env in
          (decl ^ if_code, env')
      | AST.Match (scrutinee, arms) ->
          let go_type = type_to_go state.mono expr_type in
          let decl = Printf.sprintf "%svar %s %s\n" ind let_binding.name go_type in
          let match_code =
            emit_match ~target:(AssignTarget let_binding.name) state type_map env let_binding.value scrutinee arms
          in
          let env' = Infer.TypeEnv.add let_binding.name (Types.Forall ([], expr_type)) env in
          (decl ^ match_code, env')
      | AST.RecordLit (fields, Some base_expr) ->
          (* Record spread without IIFE *)
          let record_type = get_type type_map let_binding.value in
          let result_fields, _result_row =
            match record_type with
            | Types.TRecord (f, row) -> (f, row)
            | _ ->
                failwith
                  (Printf.sprintf "Record literal expected record type, got %s" (Types.to_string record_type))
          in
          let struct_type = emit_record_struct_type state result_fields in
          let base_str = emit_expr state type_map env base_expr in
          let uses_spread_base =
            List.exists
              (fun (field : Types.record_field_type) -> Option.is_none (find_last_record_field_expr field.name fields))
              result_fields
          in
          let spread_decl =
            if uses_spread_base then
              Printf.sprintf "%s__spread := %s\n" ind base_str
            else
              ""
          in
          let emit_field_assignment field =
            let go_name = go_record_field_name field.Types.name in
            match find_last_record_field_expr field.Types.name fields with
            | Some field_expr ->
                let field_str = emit_expr state type_map env field_expr in
                Printf.sprintf "%s: %s" go_name field_str
            | None ->
                if uses_spread_base then
                  Printf.sprintf "%s: __spread.%s" go_name go_name
                else
                  failwith
                    (Printf.sprintf "Record field '%s' not provided and spread base disabled" field.Types.name)
          in
          let assignments = List.map emit_field_assignment result_fields in
          let binding_code =
            Printf.sprintf "%s%s := %s{%s}\n%s_ = %s\n" ind let_binding.name struct_type
              (String.concat ", " assignments) ind let_binding.name
          in
          let env' = Infer.TypeEnv.add let_binding.name (Types.Forall ([], expr_type)) env in
          (spread_decl ^ binding_code, env')
      | _ ->
          (* Pass expected_type so EnumConstructors get the correct concrete type *)
          let expr_str = emit_expr ~expected_type:(Some expr_type) state type_map env let_binding.value in
          let binding_code =
            match let_binding.type_annotation with
            | Some _ ->
                let go_type = type_to_go state.mono expr_type in
                Printf.sprintf "%svar %s %s = %s\n%s_ = %s\n" ind let_binding.name go_type expr_str ind
                  let_binding.name
            | None -> Printf.sprintf "%s%s := %s\n%s_ = %s\n" ind let_binding.name expr_str ind let_binding.name
          in
          let env' = Infer.TypeEnv.add let_binding.name (Types.Forall ([], expr_type)) env in
          (binding_code, env'))
  | AST.Return expr ->
      let expr_str = emit_expr state type_map env expr in
      (Printf.sprintf "%sreturn %s\n" ind expr_str, env)
  | AST.ExpressionStmt expr -> (
      match expr.expr with
      | AST.If (cond, cons, alt) ->
          let code = emit_if_to_target state type_map env expr cond cons alt DiscardTarget in
          (code, env)
      | AST.Match (scrutinee, arms) ->
          let code = emit_match ~target:DiscardTarget state type_map env expr scrutinee arms in
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
  | AST.TraitDef _ | AST.ImplDef _ | AST.DeriveDef _ | AST.TypeAlias _ ->
      (* Trait definitions/impls/derives/type aliases are compile-time only *)
      ("", env)

and emit_stmts state type_map env stmts =
  let rec loop env acc = function
    | [] -> (String.concat "" (List.rev acc), env)
    | stmt :: rest ->
        let code, env' = emit_stmt ~following_stmts:rest state type_map env stmt in
        loop env' (code :: acc) rest
  in
  loop env [] stmts

(* ============================================================
   Generate Monomorphized Functions
   ============================================================ *)

let emit_specialized_func
    (state : emit_state) (global_type_map : Infer.type_map) (typed_env : Infer.type_env) (inst : instantiation) :
    string =
  (* Resolve by symbol id (not just name) to avoid scope/module collisions. *)
  let func_def =
    List.find
      (fun (fd : func_def) ->
        fd.func_expr_id = inst.func_expr_id && fd.name = inst.func_name && List.length fd.params = inst.func_arity)
      state.mono.func_defs
  in

  let param_names =
    List.map
      (fun (p : AST.expression) ->
        match p.expr with
        | AST.Identifier name -> name
        | _ -> failwith "Function parameter must be identifier")
      func_def.params
  in

  let mangled_name = mangle_func_name inst.func_name inst.concrete_types in

  if List.length param_names <> List.length inst.concrete_types then
    failwith
      (Printf.sprintf
         "Codegen error: specialization for '%s' has arity mismatch (%d params vs %d concrete types)"
         inst.func_name (List.length param_names) (List.length inst.concrete_types));

  let params_with_types =
    List.map2 (fun name typ -> name ^ " " ^ type_to_go state.mono typ) param_names inst.concrete_types
  in
  let params_str = String.concat ", " params_with_types in
  let return_type_str = type_to_go state.mono inst.return_type in

  let has_arity t = Option.is_some (extract_param_types_exact inst.func_arity t) in
  let type_from_map = Hashtbl.find_opt global_type_map func_def.func_expr_id in
  let type_from_env =
    match Infer.TypeEnv.find_opt inst.func_name typed_env with
    | Some (Types.Forall (_, t)) -> Some t
    | None -> None
  in

  let generic_func_type =
    match type_from_map with
    | Some t when has_arity t -> t
    | _ -> (
        match type_from_env with
        | Some t when has_arity t -> t
        | _ ->
            let map_type_desc =
              match type_from_map with
              | Some t -> Types.to_string t
              | None -> "<missing>"
            in
            let env_type_desc =
              match type_from_env with
              | Some t -> Types.to_string t
              | None -> "<missing>"
            in
            failwith
              (Printf.sprintf
                 "Codegen error: missing arity-%d function type for '%s' (expr id %d). type_map=%s, env=%s"
                 inst.func_arity inst.func_name func_def.func_expr_id map_type_desc env_type_desc))
  in

  (* Build the concrete function type from parameter types and return type,
     preserving the inferred function-effect arrow kind. *)
  let arrow_is_effectful =
    match generic_func_type with
    | Types.TFun (_, _, eff) -> eff
    | _ -> false
  in
  let mk_fun =
    if arrow_is_effectful then
      Types.tfun_eff
    else
      Types.tfun
  in
  let concrete_func_type = List.fold_right (fun param_t acc -> mk_fun param_t acc) inst.concrete_types inst.return_type in

  let specialization_subst =
    match Unify.unify generic_func_type concrete_func_type with
    | Ok subst -> subst
    | Error err ->
        failwith
          (Printf.sprintf "Codegen error: failed to specialize function '%s' to concrete type %s: %s"
             inst.func_name (Types.to_string concrete_func_type) (Typecheck.Unify.error_to_string err))
  in

  let specialized_type_map = Infer.create_type_map () in
  copy_specialized_stmt_types global_type_map specialized_type_map specialization_subst func_def.body;

  (* Add function and parameters with concrete types for recursive calls/body emission *)
  let env_with_func = Infer.TypeEnv.add inst.func_name (Types.Forall ([], concrete_func_type)) typed_env in
  let body_env = add_param_bindings env_with_func param_names inst.concrete_types in

  (* Save and reset indent for top-level function *)
  let saved_indent = state.indent in
  state.indent <- 0;
  let body_str = emit_func_body state specialized_type_map body_env func_def.body in
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
          "func (e %s) String() string {\n\tswitch e.Tag {\n%s\n\tdefault:\n\t\tpanic(\"unreachable: invalid enum tag\")\n\t}\n}\n\n"
          go_type_name cases
      in

      (* Check if any variant has fields (needs fmt.Sprintf) *)
      let needs_fmt =
        List.exists (fun (v : Typecheck.Enum_registry.variant_def) -> v.fields <> []) enum_def.variants
      in
      (struct_def ^ tag_constants ^ constructors ^ string_method, needs_fmt)

(* ============================================================
   Generate Trait Implementation Functions
   ============================================================ *)

let emit_cached_impl_method
    (state : emit_state)
    (type_map : Infer.type_map)
    (typed_env : Infer.type_env)
    (impl_inst : impl_instantiation) : string =
  let payload_key = impl_inst_payload_key impl_inst in
  let payload =
    match Hashtbl.find_opt state.mono.impl_inst_payloads payload_key with
    | Some p -> p
    | None ->
        failwith
          (Printf.sprintf "Codegen error: missing payload for impl instantiation '%s.%s' (%s)"
             impl_inst.trait_name impl_inst.method_name impl_inst.type_fingerprint)
  in
  if List.length payload.param_names <> List.length payload.param_types then
    failwith
      (Printf.sprintf "Codegen error: impl %s.%s has %d params but %d param types"
         impl_inst.trait_name impl_inst.method_name (List.length payload.param_names)
         (List.length payload.param_types));

  let type_suffix = mangle_type impl_inst.for_type in
  let func_name = Printf.sprintf "%s_%s_%s" impl_inst.trait_name impl_inst.method_name type_suffix in
  let params_str =
    List.map2
      (fun name typ -> Printf.sprintf "%s %s" name (type_to_go state.mono typ))
      payload.param_names payload.param_types
    |> String.concat ", "
  in
  let return_type_str = type_to_go state.mono payload.return_type in
  let method_env = add_param_bindings typed_env payload.param_names payload.param_types in
  let inferred_body_type = get_type type_map payload.body_expr in
  if not (Annotation.check_annotation payload.return_type inferred_body_type) then
    failwith
      (Printf.sprintf "Codegen error: impl %s.%s return type mismatch: expected %s, got %s"
         impl_inst.trait_name impl_inst.method_name (Types.to_string payload.return_type)
         (Types.to_string inferred_body_type));
  let body_str = emit_expr state type_map method_env payload.body_expr in
  Printf.sprintf "func %s(%s) %s {\n\treturn %s\n}\n" func_name params_str return_type_str body_str

(* Extract all ImplDef statements from a program *)
let collect_impl_defs (program : AST.program) : AST.impl_def list =
  List.filter_map
    (fun (stmt : AST.statement) ->
      match stmt.stmt with
      | AST.ImplDef impl -> Some impl
      | _ -> None)
    program

let builtin_impl_keys : (string * string) list =
  [
    ("show", "int64");
    ("show", "bool");
    ("show", "string");
    ("show", "float64");
    ("debug", "int64");
    ("debug", "bool");
    ("debug", "string");
    ("debug", "float64");
    ("eq", "int64");
    ("eq", "bool");
    ("eq", "string");
    ("eq", "float64");
    ("ord", "int64");
    ("ord", "bool");
    ("ord", "string");
    ("ord", "float64");
    ("hash", "int64");
    ("hash", "bool");
    ("hash", "string");
    ("num", "int64");
    ("num", "float64");
    ("neg", "int64");
    ("neg", "float64");
  ]

let emit_record_derived_impl
    (state : emit_state) (derive_kind : Typecheck.Trait_registry.derive_kind) (record_type : Types.mono_type) :
    string option =
  let type_suffix = mangle_type record_type in
  let type_str = type_to_go state.mono record_type in
  let fields, _row =
    match record_type with
    | Types.TRecord (fields, row) -> (Types.normalize_record_fields fields, row)
    | _ -> ([], None)
  in
  let go_field_access prefix (f : Types.record_field_type) = prefix ^ "." ^ go_record_field_name f.name in
  match derive_kind with
  | Typecheck.Trait_registry.DeriveEq ->
      let body =
        match fields with
        | [] -> "true"
        | _ ->
            fields
            |> List.map (fun f -> Printf.sprintf "(%s == %s)" (go_field_access "x" f) (go_field_access "y" f))
            |> String.concat " && "
      in
      Some (Printf.sprintf "func eq_eq_%s(x, y %s) bool {\n\treturn %s\n}\n" type_suffix type_str body)
  | Typecheck.Trait_registry.DeriveShow | Typecheck.Trait_registry.DeriveDebug ->
      let fn_prefix =
        if derive_kind = Typecheck.Trait_registry.DeriveShow then
          "show_show_"
        else
          "debug_debug_"
      in
      let format_str, args_str =
        match fields with
        | [] -> ("{}", "")
        | _ ->
            let pieces = List.map (fun (f : Types.record_field_type) -> Printf.sprintf "%s: %%v" f.name) fields in
            let args = List.map (go_field_access "x") fields in
            ("{ " ^ String.concat ", " pieces ^ " }", String.concat ", " args)
      in
      let return_expr =
        if args_str = "" then
          Printf.sprintf "%S" format_str
        else
          Printf.sprintf "fmt.Sprintf(%S, %s)" format_str args_str
      in
      Some
        (Printf.sprintf "func %s%s(x %s) string {\n\treturn %s\n}\n" fn_prefix type_suffix type_str return_expr)
  | Typecheck.Trait_registry.DeriveOrd ->
      let compare_pair (f : Types.record_field_type) =
        let x_access = go_field_access "x" f in
        let y_access = go_field_access "y" f in
        match f.typ with
        | Types.TBool ->
            (Printf.sprintf "(!%s && %s)" x_access y_access, Printf.sprintf "(%s == %s)" x_access y_access)
        | _ -> (Printf.sprintf "(%s < %s)" x_access y_access, Printf.sprintf "(%s == %s)" x_access y_access)
      in
      let body =
        match fields with
        | [] -> "\treturn int64(1)\n"
        | _ ->
            fields
            |> List.map compare_pair
            |> List.map (fun (less_expr, eq_expr) ->
                   Printf.sprintf "\tif %s { return int64(0) }\n\tif !%s { return int64(2) }\n" less_expr eq_expr)
            |> String.concat ""
      in
      Some
        (Printf.sprintf "func ord_compare_%s(x, y %s) int64 {\n%s\treturn int64(1)\n}\n" type_suffix type_str body)
  | Typecheck.Trait_registry.DeriveHash ->
      let hash_steps =
        fields
        |> List.map (fun (f : Types.record_field_type) ->
               Printf.sprintf "\th = h*31 + hash_hash_%s(%s)\n" (mangle_type f.typ) (go_field_access "x" f))
        |> String.concat ""
      in
      Some
        (Printf.sprintf "func hash_hash_%s(x %s) int64 {\n\th := int64(17)\n%s\treturn h\n}\n" type_suffix
           type_str hash_steps)

let emit_registry_derived_impls (state : emit_state) (program : AST.program) : string =
  let user_impls = collect_impl_defs program in
  let user_impl_set =
    List.fold_left
      (fun acc (impl : AST.impl_def) ->
        let for_type = type_expr_to_mono_type_with_impl_bindings impl.impl_type_params impl.AST.impl_for_type in
        if has_type_vars for_type then
          acc
        else
          (impl.AST.impl_trait_name, mangle_type for_type) :: acc)
      [] user_impls
  in
  let should_emit trait_name type_suffix =
    (not (List.mem (trait_name, type_suffix) user_impl_set))
    && not (List.mem (trait_name, type_suffix) builtin_impl_keys)
  in
  Typecheck.Trait_registry.all_impls ()
  |> List.filter_map (fun (impl : Typecheck.Trait_registry.impl_def) ->
         if has_type_vars impl.impl_for_type then
           None
         else
           let type_suffix = mangle_type impl.impl_for_type in
           if should_emit impl.impl_trait_name type_suffix then
             match Typecheck.Trait_registry.derive_kind_for_impl impl with
             | Some derive_kind -> (
                 match impl.impl_for_type with
                 | Types.TRecord _ -> emit_record_derived_impl state derive_kind impl.impl_for_type
                 | _ -> None)
             | None -> None
           else
             None)
  |> String.concat "\n"

(* ============================================================
    Builtin Trait Implementations - Go Code Generation
    ============================================================ *)

(* Emit Go code for builtin trait implementations for primitives *)
(* Only emit builtins that are NOT overridden by user impls *)
let emit_builtin_impls (program : AST.program) : string =
  (* Collect user-defined impls from program *)
  let user_impls = collect_impl_defs program in

  (* Build set of (trait_name, for_type) pairs that user defined *)
  let user_impl_set =
    List.fold_left
      (fun acc (impl : AST.impl_def) ->
        let for_type = type_expr_to_mono_type_with_impl_bindings impl.impl_type_params impl.AST.impl_for_type in
        if has_type_vars for_type then
          acc
        else
          let key = (impl.AST.impl_trait_name, mangle_type for_type) in
          key :: acc)
      [] user_impls
  in

  (* Helper to check if an impl is user-defined *)
  let is_user_defined trait_name type_name = List.mem (trait_name, type_name) user_impl_set in

  (* Define all possible builtin impls with their keys *)
  let all_builtins =
    [
      (* show trait implementations *)
      (("show", "int64"), "func show_show_int64(x int64) string {\n\treturn fmt.Sprintf(\"%d\", x)\n}");
      (("show", "bool"), "func show_show_bool(x bool) string {\n\treturn fmt.Sprintf(\"%t\", x)\n}");
      (("show", "string"), "func show_show_string(x string) string {\n\treturn x\n}");
      (("show", "float64"), "func show_show_float64(x float64) string {\n\treturn fmt.Sprintf(\"%g\", x)\n}");
      (* debug trait implementations *)
      (("debug", "int64"), "func debug_debug_int64(x int64) string {\n\treturn fmt.Sprintf(\"%d\", x)\n}");
      (("debug", "bool"), "func debug_debug_bool(x bool) string {\n\treturn fmt.Sprintf(\"%t\", x)\n}");
      (("debug", "string"), "func debug_debug_string(x string) string {\n\treturn fmt.Sprintf(\"%q\", x)\n}");
      (("debug", "float64"), "func debug_debug_float64(x float64) string {\n\treturn fmt.Sprintf(\"%g\", x)\n}");
      (* eq trait implementations *)
      (("eq", "int64"), "func eq_eq_int64(x, y int64) bool {\n\treturn x == y\n}");
      (("eq", "bool"), "func eq_eq_bool(x, y bool) bool {\n\treturn x == y\n}");
      (("eq", "string"), "func eq_eq_string(x, y string) bool {\n\treturn x == y\n}");
      (("eq", "float64"), "func eq_eq_float64(x, y float64) bool {\n\treturn x == y\n}");
      (* ord trait implementations - returns ordering enum *)
      ( ("ord", "int64"),
        "func ord_compare_int64(x, y int64) int64 {\n\tif x < y { return 0 } else if x == y { return 1 } else { return 2 }\n}"
      );
      ( ("ord", "bool"),
        "func ord_compare_bool(x, y bool) int64 {\n\tif !x && y { return 0 } else if x == y { return 1 } else { return 2 }\n}"
      );
      ( ("ord", "string"),
        "func ord_compare_string(x, y string) int64 {\n\tif x < y { return 0 } else if x == y { return 1 } else { return 2 }\n}"
      );
      ( ("ord", "float64"),
        "func ord_compare_float64(x, y float64) int64 {\n\tif x < y { return 0 } else if x == y { return 1 } else { return 2 }\n}"
      );
      (* hash trait implementations *)
      (("hash", "int64"), "func hash_hash_int64(x int64) int64 {\n\treturn x\n}");
      (("hash", "bool"), "func hash_hash_bool(x bool) int64 {\n\tif x { return 1 } else { return 0 }\n}");
      ( ("hash", "string"),
        "func hash_hash_string(x string) int64 {\n\tvar h int64 = 0\n\tfor _, c := range x { h = h*31 + int64(c) }\n\treturn h\n}"
      );
      (* num trait implementations *)
      ( ("num", "int64"),
        "func num_add_int64(x, y int64) int64 {\n\treturn x + y\n}\nfunc num_sub_int64(x, y int64) int64 {\n\treturn x - y\n}\nfunc num_mul_int64(x, y int64) int64 {\n\treturn x * y\n}\nfunc num_div_int64(x, y int64) int64 {\n\treturn x / y\n}"
      );
      ( ("num", "float64"),
        "func num_add_float64(x, y float64) float64 {\n\treturn x + y\n}\nfunc num_sub_float64(x, y float64) float64 {\n\treturn x - y\n}\nfunc num_mul_float64(x, y float64) float64 {\n\treturn x * y\n}\nfunc num_div_float64(x, y float64) float64 {\n\treturn x / y\n}"
      );
      (* neg trait implementations *)
      (("neg", "int64"), "func neg_neg_int64(x int64) int64 {\n\treturn -x\n}");
      (("neg", "float64"), "func neg_neg_float64(x float64) float64 {\n\treturn -x\n}");
    ]
  in

  (* Filter out user-defined impls *)
  let needed_impls =
    List.filter (fun ((trait_name, type_name), _code) -> not (is_user_defined trait_name type_name)) all_builtins
  in

  let impl_codes = List.map snd needed_impls in
  String.concat "\n\n" impl_codes

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

  (* Pass 3: Register type alias shapes for records *)
  List.iter
    (fun (stmt : AST.statement) ->
      match stmt.stmt with
      | AST.TypeAlias alias_def -> (
          let mono_type = Annotation.type_expr_to_mono_type alias_def.alias_body in
          match mono_type with
          | Types.TRecord (fields, _row) -> register_type_alias_shape mono_state alias_def.alias_name fields
          | _ -> ())
      | _ -> ())
    program;

  (* Ensure builtin enums are always generated if referenced *)
  (* Check if ordering enum is used in any impl *)
  let ordering_used =
    List.exists
      (fun stmt ->
        match stmt.AST.stmt with
        | AST.ImplDef impl -> impl.impl_trait_name = "ord"
        | _ -> false)
      program
  in
  if ordering_used then
    mono_state.enum_insts <- EnumInstSet.add ("ordering", []) mono_state.enum_insts;

  let emit_state = create_emit_state mono_state in

  (* Generate enum types *)
  let enum_results =
    EnumInstSet.elements mono_state.enum_insts
    |> List.map (fun (name, args) -> emit_enum_type mono_state name args)
  in
  let enum_types = List.map fst enum_results |> String.concat "\n" in

  (* Generate specialized functions *)
  let specialized_funcs =
    InstSet.elements mono_state.instantiations
    |> List.map (emit_specialized_func emit_state type_map typed_env)
    |> String.concat "\n"
  in

  (* Generate builtin trait impl functions *)
  let builtin_impl_funcs = emit_builtin_impls program in

  (* Generate trait impl functions *)
  let impl_funcs =
    ImplInstSet.elements mono_state.impl_instantiations
    |> List.map (emit_cached_impl_method emit_state type_map typed_env)
    |> String.concat "\n"
  in
  let derived_impl_funcs = emit_registry_derived_impls emit_state program in

  (* Emit main body *)
  let main_body, _ = emit_stmts emit_state type_map typed_env program in

  (* Generate record shape type definitions *)
  let record_shape_defs = emit_record_shape_defs mono_state in

  (* Build final output *)
  (* Always import fmt since builtin impls use it *)
  let imports = "import \"fmt\"\n\n" in
  let type_defs =
    let enum_section =
      if enum_types = "" then
        ""
      else
        enum_types ^ "\n"
    in
    let record_section =
      if record_shape_defs = "" then
        ""
      else
        record_shape_defs ^ "\n"
    in
    enum_section ^ record_section
  in
  let specialized_funcs_str =
    if specialized_funcs = "" then
      ""
    else
      specialized_funcs ^ "\n"
  in
  let builtin_impl_funcs_str = builtin_impl_funcs ^ "\n" in
  let impl_funcs_str =
    if impl_funcs = "" then
      ""
    else
      impl_funcs ^ "\n"
  in
  let derived_impl_funcs_str =
    if derived_impl_funcs = "" then
      ""
    else
      derived_impl_funcs ^ "\n"
  in
  let top_funcs = specialized_funcs_str ^ builtin_impl_funcs_str ^ impl_funcs_str ^ derived_impl_funcs_str in

  Printf.sprintf "package main\n\n%s%s%sfunc main() {\n%s}\n" imports type_defs top_funcs main_body

let emit_program (program : AST.program) : string =
  let env = Typecheck.Builtins.prelude_env () in
  match Typecheck.Checker.check_program_with_annotations ~env program with
  | Error err ->
      failwith
        (Printf.sprintf "Codegen error: cannot emit untyped program: %s" (Typecheck.Checker.format_error err))
  | Ok { environment = typed_env; type_map; _ } -> emit_program_with_typed_env type_map typed_env program

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

let compile_string ?file_id (source : string) : (string, string) result =
  match Syntax.Parser.parse ?file_id source with
  | Error errors -> Error ("Parse error: " ^ String.concat ", " errors)
  | Ok program -> (
      let env = Typecheck.Builtins.prelude_env () in
      match Typecheck.Checker.check_program_with_annotations ~source ~env program with
      | Error err -> Error ("Type error: " ^ Typecheck.Checker.format_error err)
      | Ok { environment = typed_env; type_map; _ } -> (
          let normalize_codegen_error msg =
            let prefix = "Codegen error: " in
            let prefix_len = String.length prefix in
            if String.length msg >= prefix_len && String.sub msg 0 prefix_len = prefix then
              msg
            else
              prefix ^ msg
          in
          try Ok (emit_program_with_typed_env type_map typed_env program) with
          | Failure msg -> Error (normalize_codegen_error msg)
          | exn -> Error (normalize_codegen_error (Printexc.to_string exn))))

type build_output = {
  main_go : string;
  runtime_go : string;
}

let compile_to_build ?file_id (source : string) : (build_output, string) result =
  match compile_string ?file_id source with
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

let%test "emit bool ordering via ord helper" =
  match compile_string "true < false" with
  | Ok code -> string_contains code "ord_compare_bool(true, false)"
  | Error _ -> false

let%test "emit non-primitive equality via eq helper" =
  match
    compile_string
      "type point = { x: int }\nimpl eq for point {\n  fn eq(x: point, y: point) -> bool { false }\n}\nlet a: point = { x: 1 }\nlet b: point = { x: 1 }\na == b"
  with
  | Ok code -> string_contains code "func eq_eq_record_x_int64_closed" && string_contains code "return false"
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

let%test "instantiation identity includes module path" =
  let mk module_path =
    {
      func_name = "f";
      module_path;
      func_expr_id = 1;
      func_arity = 1;
      concrete_only_mode = true;
      concrete_types = [ Types.TInt ];
      type_fingerprint = fingerprint_types [ Types.TInt ];
      return_type = Types.TInt;
    }
  in
  let set = InstSet.empty |> InstSet.add (mk "main") |> InstSet.add (mk "pkg/math") in
  InstSet.cardinal set = 2

let%test "module codegen policy rejects non-main module path" =
  match
    try
      let _ = create_mono_state ~module_path:"pkg/math" () in
      None
    with Failure msg -> Some msg
  with
  | None -> false
  | Some msg -> string_contains msg "single Go package"

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

let%test "underscore let binding emits discard assignment" =
  match compile_string "let f = fn(x: int) -> int { let _ = x; x }; f(1)" with
  | Ok code -> string_contains code "_ = x" && not (string_contains code "_ := x")
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

let%test "emit record literal and field access" =
  match compile_string "let p = { x: 10, y: 20 }; p.x + p.y" with
  | Ok code ->
      (* Record literals should use named shape types, not inline structs *)
      string_contains code "Record_x_int64_y_int64"
      && string_contains code "(p).x"
      && string_contains code "(p).y"
  | Error _ -> false

let%test "record shape type definition is emitted" =
  match compile_string "let p = { x: 10, y: 20 }; p.x" with
  | Ok code ->
      (* A top-level type definition should exist for the record shape *)
      string_contains code "type Record_x_int64_y_int64 struct"
  | Error _ -> false

let%test "same record shape reuses type name" =
  match compile_string "let p = { x: 1, y: 2 }; let q = { x: 10, y: 20 }; p.x + q.y" with
  | Ok code ->
      (* Both should use the same named type — only one type definition *)
      let count =
        let needle = "type Record_x_int64_y_int64 struct" in
        let rec count_from i acc =
          if i + String.length needle > String.length code then
            acc
          else if String.sub code i (String.length needle) = needle then
            count_from (i + 1) (acc + 1)
          else
            count_from (i + 1) acc
        in
        count_from 0 0
      in
      count = 1
  | Error _ -> false

let%test "different record shapes get different type names" =
  match compile_string "let p = { x: 1, y: 2 }; let q = { a: 10, b: 20 }; p.x + q.a" with
  | Ok code ->
      string_contains code "type Record_x_int64_y_int64 struct"
      && string_contains code "type Record_a_int64_b_int64 struct"
  | Error _ -> false

let%test "record field ordering is canonical in shape name" =
  match compile_string "let p = { y: 2, x: 1 }; p.x" with
  | Ok code ->
      (* Fields sorted alphabetically: x before y *)
      string_contains code "Record_x_int64_y_int64"
  | Error _ -> false

let%test "function returning wrapped scalar record compiles" =
  match compile_string "let mk = fn(i) { { inner: i } }; let o = mk(1); o.inner" with
  | Ok code ->
      string_contains code "func mk_int64(i int64)"
      && string_contains code "type Record_inner_int64 struct"
  | Error _ -> false

let%test "function returning wrapped record compiles" =
  match compile_string
          "let mk = fn(i: { x: int }) { { inner: i } }; let i = { x: 1 }; let o = mk(i); o.inner.x"
  with
  | Ok code ->
      string_contains code "type Record_x_int64 struct"
      && string_contains code "type Record_inner_"
      && string_contains code "func mk_record_x_int64_closed("
  | Error _ -> false

let%test "function wrapping inline record literal argument compiles" =
  match compile_string "let mk = fn(r: { x: int }) { { inner: r } }; let o = mk({ x: 3 }); o.inner.x" with
  | Ok code -> string_contains code "func mk_record_x_int64_closed(" && string_contains code "type Record_inner_"
  | Error _ -> false

let%test "duplicate record fields use last write" =
  match compile_string "let p = { x: 1, x: 2 }; p.x" with
  | Ok code -> string_contains code "Record_x_int64{x: int64(2)}"
  | Error _ -> false

let%test "emit record spread" =
  match compile_string "let p = { x: 1, y: 2 }; let p2 = { ...p, x: 10 }; p2.x + p2.y" with
  | Ok code -> string_contains code "__spread := p" && string_contains code "Record_x_int64_y_int64"
  | Error _ -> false

let%test "record spread with full field override avoids unused temp" =
  match compile_string "let p = { x: 1, X: 2 }; let q = { ...p, x: 4, X: 6 }; q.x + q.X" with
  | Ok code -> not (string_contains code "__spread :=")
  | Error _ -> false

let%test "emit record match pattern" =
  match compile_string "let p = { x: 10, y: 20 }; match p { { x:, y: }: x + y _: 0 }" with
  | Ok code ->
      string_contains code "__scrutinee :="
      && string_contains code "if true"
      && string_contains code "x := __scrutinee.x"
  | Error _ -> false

let%test "type alias emits named Go type" =
  match compile_string "type point = { x: int, y: int }; let p: point = { x: 1, y: 2 }; p.x" with
  | Ok code ->
      (* Should emit "type Point struct{...}" using alias name, not Record_... *)
      string_contains code "type Point struct"
  | Error _ -> false

let%test "type alias used in record literal" =
  match compile_string "type point = { x: int, y: int }; let p: point = { x: 1, y: 2 }; p.x" with
  | Ok code ->
      (* Record literal should use alias name *)
      string_contains code "Point{x:"
  | Error _ -> false

let%test "case-distinct record fields compile to distinct Go fields" =
  match compile_string "let p = { x: 1, X: 2 }; p.x + p.X" with
  | Ok code -> string_contains code "type Record_X_int64_x_int64 struct{X int64; x int64}"
  | Error _ -> false

let%test "type alias replaces generated shape name" =
  match compile_string "type point = { x: int, y: int }; let p: point = { x: 1, y: 2 }; p.x" with
  | Ok code ->
      (* Should NOT emit Record_x_int64_y_int64 when alias exists *)
      not (string_contains code "Record_x_int64_y_int64")
  | Error _ -> false

let%test "emitter method calls use typechecker resolution metadata" =
  let source =
    {|
trait ping {
  fn ping(x: int) -> int
}
impl ping for int {
  fn ping(x: int) -> int { x }
}
let v = 1
v.ping()
|}
  in
  match Syntax.Parser.parse source with
  | Error _ -> false
  | Ok program -> (
      let env = Typecheck.Builtins.prelude_env () in
      match Typecheck.Checker.check_program_with_annotations ~source ~env program with
      | Error _ -> false
      | Ok { environment = typed_env; type_map; _ } -> (
          (* If emitter re-resolves methods from the registry, this clear would break codegen. *)
          Typecheck.Trait_registry.clear ();
          match try Some (emit_program_with_typed_env type_map typed_env program) with _ -> None with
          | Some code -> string_contains code "ping_ping_int64"
          | None -> false))

(* ============================================================
   Phase 2: IIFE Removal Tests
   ============================================================ *)

let%test "if-expression in let binding emits without IIFE" =
  match compile_string "let x = if (true) { 1 } else { 2 }; x" with
  | Ok code ->
      (* Should NOT contain func() - no IIFE *)
      (not (string_contains code "func()"))
      (* Should contain var x int64 pattern *)
      && string_contains code "var "
  | Error _ -> false

let%test "if-expression in tail position emits without IIFE" =
  match compile_string "let f = fn(x) { if (x > 0) { x } else { -x } }; f(5)" with
  | Ok code ->
      (* Inside the function body, should NOT wrap in func() *)
      (* Should have direct if with return *)
      string_contains code "if (x > int64(0))" && not (string_contains code "func() int64")
  | Error _ -> false

let%test "match in let binding emits without IIFE" =
  match compile_string "let x = 5; let y = match x { 1: 10 _: 20 }; y" with
  | Ok code ->
      (* Should NOT contain func() for the match *)
      (not (string_contains code "func()")) && string_contains code "switch"
  | Error _ -> false

let%test "match in tail position emits without IIFE" =
  match compile_string "let f = fn(x) { match x { 1: 10 2: 20 _: 30 } }; f(5)" with
  | Ok code -> (not (string_contains code "func() int64")) && string_contains code "switch"
  | Error _ -> false

let%test "if-expression in function argument still uses IIFE" =
  match compile_string "puts(if (true) { 1 } else { 2 })" with
  | Ok code ->
      (* IIFE still needed when if is a function argument *)
      string_contains code "func()" && string_contains code "if true"
  | Error _ -> false

let%test "type-check if in let binding emits without IIFE" =
  match compile_string "let x: int | string = 1; let y = if (x is int) { x + 1 } else { 0 }; y" with
  | Ok code -> not (string_contains code "func() int64") && string_contains code "switch x_typed := x.(type)"
  | Error _ -> false

let%test "type-check if in tail position emits without IIFE" =
  match compile_string "let f = fn(x: int | string) -> int { if (x is int) { x + 1 } else { 0 } }; f(1)" with
  | Ok code -> not (string_contains code "func() int64") && string_contains code "switch x_typed := x.(type)"
  | Error _ -> false

let%test "type-check if in function argument still uses IIFE" =
  match compile_string "let x: int | string = 1; puts(if (x is int) { x } else { 0 })" with
  | Ok code -> string_contains code "func()" && string_contains code "switch x_typed := x.(type)"
  | Error _ -> false

let%test "if without else in tail position emits unit return without IIFE" =
  match compile_string "let f = fn(x: bool) { if (x) { 1 } }; f(true)" with
  | Ok code ->
      not (string_contains code "func() struct{}")
      && string_contains code "if x"
      && string_contains code "return struct{}{}"
  | Error _ -> false

let%test "type-check if without else in let binding emits unit assignment" =
  match compile_string "let x: int | string = 1; let y = if (x is int) { x + 1 }; y" with
  | Ok code ->
      not (string_contains code "func()")
      && string_contains code "switch x_typed := x.(type)"
      && string_contains code "y = struct{}{}"
  | Error _ -> false

let%test "type-check statement if preserves explicit return without IIFE" =
  match compile_string "let f = fn(x: int | string) -> int { if (x is int) { return 1 }; return 0 }; f(1)" with
  | Ok code ->
      not (string_contains code "func() int64")
      && string_contains code "switch x_typed := x.(type)"
      && string_contains code "return int64(1)"
  | Error _ -> false

let%test "match expression statement emits without IIFE" =
  match compile_string "match 1 { 1: 10 _: 20 }" with
  | Ok code -> not (string_contains code "func() int64") && string_contains code "switch __scrutinee"
  | Error _ -> false

let%test "nested if in tail position emits without IIFE" =
  match compile_string "let f = fn(a: bool, b: bool) -> int { if (a) { if (b) { 1 } else { 2 } } else { 3 } }; f(true, false)" with
  | Ok code -> not (string_contains code "func() int64")
  | Error _ -> false

let%test "nested match in tail position emits without IIFE" =
  match compile_string "let f = fn(x: int) -> int { match x { 0: match x { 0: 10 _: 20 } _: 30 } }; f(0)" with
  | Ok code -> not (string_contains code "func() int64")
  | Error _ -> false

let%test "if branch containing match in let binding emits without IIFE" =
  match compile_string "let x = 1; let y = if (x == 1) { match x { 1: 10 _: 20 } } else { 0 }; y" with
  | Ok code -> not (string_contains code "func() int64")
  | Error _ -> false

let%test "match statement with nested if emits without IIFE" =
  match compile_string "match 1 { 1: if (true) { 10 } else { 20 } _: 0 }" with
  | Ok code -> not (string_contains code "func() int64")
  | Error _ -> false

let%test "record spread emits without IIFE" =
  match compile_string "let p = { x: 1, y: 2 }; let p2 = { ...p, x: 10 }; p2" with
  | Ok code ->
      (* Should NOT use IIFE for spread *)
      (not (string_contains code "(func()")) && string_contains code "__spread"
  | Error _ -> false

(* ============================================================
   Problem 3: Monomorphization Cache Hardening Tests
   ============================================================ *)

let%test "has_type_vars detects TVar in record fields" =
  has_type_vars (Types.TRecord ([ { Types.name = "x"; typ = Types.TVar "a" } ], None))

let%test "has_type_vars detects TRowVar in record row" =
  has_type_vars (Types.TRecord ([ { Types.name = "x"; typ = Types.TInt } ], Some (Types.TRowVar "r")))

let%test "has_type_vars returns false for concrete record" =
  not (has_type_vars (Types.TRecord ([ { Types.name = "x"; typ = Types.TInt } ], None)))

let%test "has_type_vars detects standalone TRowVar" =
  has_type_vars (Types.TRowVar "r")

let%test "mangle_type errors on TVar" =
  match try Some (mangle_type (Types.TVar "a")) with Failure _ -> None with
  | Some _ -> false (* Should have raised *)
  | None -> true

let%test "mangle_type errors on nested TVar" =
  match try Some (mangle_type (Types.TArray (Types.TVar "x"))) with Failure _ -> None with
  | Some _ -> false
  | None -> true

let%test "fingerprint_types is stable and explicit" =
  fingerprint_types [ Types.TInt; Types.TArray Types.TBool ] = "int64__arr_bool"

let%test "instantiation identity ignores return type" =
  let mk return_type =
    {
      func_name = "id";
      module_path = "main";
      func_expr_id = 99;
      func_arity = 1;
      concrete_only_mode = true;
      concrete_types = [ Types.TInt ];
      type_fingerprint = fingerprint_types [ Types.TInt ];
      return_type;
    }
  in
  let set = InstSet.empty |> InstSet.add (mk Types.TInt) |> InstSet.add (mk Types.TBool) in
  InstSet.cardinal set = 1

let%test "add_instantiation rejects conflicting return type for same identity" =
  let state = create_mono_state () in
  let mk return_type =
    {
      func_name = "id";
      module_path = "main";
      func_expr_id = 100;
      func_arity = 1;
      concrete_only_mode = true;
      concrete_types = [ Types.TInt ];
      type_fingerprint = fingerprint_types [ Types.TInt ];
      return_type;
    }
  in
  add_instantiation state (mk Types.TInt);
  match
    try
      add_instantiation state (mk Types.TBool);
      None
    with Failure msg -> Some msg
  with
  | None -> false
  | Some msg -> string_contains msg "inconsistent return type for instantiation"

let%test "add_instantiation rejects fingerprint collision with different concrete types" =
  let state = create_mono_state () in
  let t1 = Types.TEnum ("a_b", []) in
  let t2 = Types.TEnum ("a", [ Types.TEnum ("b", []) ]) in
  if fingerprint_types [ t1 ] <> fingerprint_types [ t2 ] then
    false
  else
    let mk concrete_type =
      {
        func_name = "id";
        module_path = "main";
        func_expr_id = 101;
        func_arity = 1;
        concrete_only_mode = true;
        concrete_types = [ concrete_type ];
        type_fingerprint = fingerprint_types [ concrete_type ];
        return_type = Types.TNull;
      }
    in
    add_instantiation state (mk t1);
    match
      try
        add_instantiation state (mk t2);
        None
      with Failure msg -> Some msg
    with
    | None -> false
    | Some msg -> string_contains msg "instantiation fingerprint collision"

let%test "add_impl_instantiation rejects fingerprint collision with different for_type" =
  let state = create_mono_state () in
  let t1 = Types.TEnum ("a_b", []) in
  let t2 = Types.TEnum ("a", [ Types.TEnum ("b", []) ]) in
  if fingerprint_types [ t1 ] <> fingerprint_types [ t2 ] then
    false
  else
    let mk_inst for_type =
      {
        trait_name = "show";
        method_name = "show";
        module_path = "main";
        concrete_only_mode = true;
        for_type;
        type_fingerprint = fingerprint_types [ for_type ];
      }
    in
    let payload =
      { param_names = [ "x" ]; param_types = [ Types.TInt ]; return_type = Types.TString; body_expr = AST.mk_expr (AST.String "x") }
    in
    add_impl_instantiation state (mk_inst t1) payload;
    match
      try
        add_impl_instantiation state (mk_inst t2) payload;
        None
      with Failure msg -> Some msg
    with
    | None -> false
    | Some msg -> string_contains msg "impl instantiation fingerprint collision"

let%test "duplicate top-level function name and arity is rejected" =
  match
    compile_string
      "let f = fn(x: int) -> int { x + 1 }; let f = fn(x: int) -> int { x + 2 }; let y = f(1); puts(y)"
  with
  | Ok _ -> false
  | Error msg -> string_contains msg "ambiguous function reference 'f/1'"

let%test "collect_insts registers impl methods in cache" =
  let source =
    {|
trait show[a] {
  fn show(x: a) -> string
}
impl show for int {
  fn show(x: int) -> string { "int!" }
}
puts(1.show())
|}
  in
  Fun.protect
    ~finally:(fun () ->
      Typecheck.Trait_registry.clear ();
      Typecheck.Builtins.init_builtin_impls ())
    (fun () ->
      match Syntax.Parser.parse source with
      | Error _ -> false
      | Ok program -> (
          let env = Typecheck.Builtins.prelude_env () in
          match Typecheck.Checker.check_program_with_annotations ~source ~env program with
          | Error _ -> false
          | Ok { environment = typed_env; type_map; _ } ->
              let mono_state = create_mono_state () in
              List.iter (collect_funcs_stmt mono_state) program;
              ignore (List.fold_left (collect_insts_stmt mono_state type_map) typed_env program);
              ImplInstSet.cardinal mono_state.impl_instantiations = 1
              && Hashtbl.length mono_state.impl_inst_payloads = 1))

let%test "trait impl methods are emitted exactly once" =
  (* Builtin show_show_int64 should appear exactly once as a function definition *)
  match compile_string "puts(42)" with
  | Ok code ->
      let count_occurrences haystack needle =
        let nl = String.length needle in
        let rec loop pos acc =
          match String.index_from_opt haystack pos needle.[0] with
          | None -> acc
          | Some i ->
              if i + nl <= String.length haystack && String.sub haystack i nl = needle then
                loop (i + 1) (acc + 1)
              else
                loop (i + 1) acc
        in
        loop 0 0
      in
      (* Each builtin trait function should be defined exactly once *)
      count_occurrences code "func show_show_int64(" = 1 && count_occurrences code "func eq_eq_int64(" = 1
  | Error _ -> false
