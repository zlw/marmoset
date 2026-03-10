(* Go code generation from Marmoset AST with monomorphization *)

module AST = Syntax.Ast.AST
module Types = Typecheck.Types
module Infer = Typecheck.Infer
module Annotation = Typecheck.Annotation
module Unify = Typecheck.Unify
module Diagnostic = Diagnostics.Diagnostic
module String_utils = Diagnostics.String_utils
module StringSet = Set.Make (String)

module StringPairOrd = struct
  type t = string * string

  let compare = compare
end

module StringPairSet = Set.Make (StringPairOrd)

(* Unwrap annotation Result — emitter processes type-checked code so annotation
   errors here are internal failures *)
let annotation_exn r =
  match r with
  | Ok t -> t
  | Error (d : Diagnostic.t) -> failwith d.message

let merge_record_fields_for_row
    (base_fields : Types.record_field_type list) (extra_fields : Types.record_field_type list) :
    Types.record_field_type list =
  let by_name : (string, Types.record_field_type) Hashtbl.t =
    Hashtbl.create (List.length base_fields + List.length extra_fields)
  in
  List.iter (fun (f : Types.record_field_type) -> Hashtbl.replace by_name f.name f) base_fields;
  List.iter (fun (f : Types.record_field_type) -> Hashtbl.replace by_name f.name f) extra_fields;
  Hashtbl.to_seq_keys by_name
  |> List.of_seq
  |> List.sort String.compare
  |> List.map (fun name ->
         match Hashtbl.find_opt by_name name with
         | Some field -> field
         | None -> failwith "merge_record_fields_for_row: impossible missing field")

let rec normalize_record_row_type (fields : Types.record_field_type list) (row : Types.mono_type option) :
    Types.record_field_type list * Types.mono_type option =
  match row with
  | Some (Types.TRecord (row_fields, row_tail)) ->
      let merged = merge_record_fields_for_row fields row_fields in
      normalize_record_row_type merged row_tail
  | _ -> (Types.normalize_record_fields fields, row)

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
           "Codegen error: unresolved type variable '%s' reached mangle_type. All type variables must be resolved before code generation."
           name)
  | Types.TFun (arg, ret, _) -> "fn_" ^ mangle_type arg ^ "_" ^ mangle_type ret
  | Types.TArray elem -> "arr_" ^ mangle_type elem
  | Types.THash (key, value) -> "map_" ^ mangle_type key ^ "_" ^ mangle_type value
  | Types.TRecord (fields, row) ->
      let fields, row = normalize_record_row_type fields row in
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

let rec mangle_type_for_shape (t : Types.mono_type) : string =
  match t with
  | Types.TInt -> "int64"
  | Types.TFloat -> "float64"
  | Types.TBool -> "bool"
  | Types.TString -> "string"
  | Types.TNull -> "unit"
  | Types.TVar _ | Types.TRowVar _ -> "any"
  | Types.TFun (arg, ret, _) -> "fn_" ^ mangle_type_for_shape arg ^ "_" ^ mangle_type_for_shape ret
  | Types.TArray elem -> "arr_" ^ mangle_type_for_shape elem
  | Types.THash (key, value) -> "map_" ^ mangle_type_for_shape key ^ "_" ^ mangle_type_for_shape value
  | Types.TRecord (fields, row) ->
      let fields, row = normalize_record_row_type fields row in
      let field_bits =
        fields
        |> List.map (fun (f : Types.record_field_type) -> f.name ^ "_" ^ mangle_type_for_shape f.typ)
        |> String.concat "_"
      in
      let row_bit =
        match row with
        | None -> "closed"
        | Some _ -> "open"
      in
      "record_" ^ field_bits ^ "_" ^ row_bit
  | Types.TUnion _ -> "union"
  | Types.TEnum (name, []) -> name
  | Types.TEnum (name, args) -> name ^ "_" ^ String.concat "_" (List.map mangle_type_for_shape args)

let rec erase_unresolved_type_vars_for_codegen (t : Types.mono_type) : Types.mono_type =
  match t with
  | Types.TVar _ | Types.TRowVar _ -> Types.TUnion []
  | Types.TFun (arg, ret, is_effectful) ->
      Types.TFun
        (erase_unresolved_type_vars_for_codegen arg, erase_unresolved_type_vars_for_codegen ret, is_effectful)
  | Types.TArray elem -> Types.TArray (erase_unresolved_type_vars_for_codegen elem)
  | Types.THash (key, value) ->
      Types.THash (erase_unresolved_type_vars_for_codegen key, erase_unresolved_type_vars_for_codegen value)
  | Types.TRecord (fields, row) ->
      let fields' =
        List.map
          (fun (f : Types.record_field_type) -> { f with typ = erase_unresolved_type_vars_for_codegen f.typ })
          fields
      in
      let row' = Option.map erase_unresolved_type_vars_for_codegen row in
      Types.TRecord (fields', row')
  | Types.TUnion members -> Types.TUnion (List.map erase_unresolved_type_vars_for_codegen members)
  | Types.TEnum (name, args) -> Types.TEnum (name, List.map erase_unresolved_type_vars_for_codegen args)
  | Types.TInt | Types.TFloat | Types.TBool | Types.TString | Types.TNull -> t

let normalize_type_for_codegen ~(concrete_only : bool) (t : Types.mono_type) : Types.mono_type =
  let t = Types.canonicalize_mono_type t in
  if concrete_only then
    Types.canonicalize_mono_type (erase_unresolved_type_vars_for_codegen t)
  else
    t

let go_keywords =
  [
    "break";
    "default";
    "func";
    "interface";
    "select";
    "case";
    "defer";
    "go";
    "map";
    "struct";
    "chan";
    "else";
    "goto";
    "package";
    "switch";
    "const";
    "fallthrough";
    "if";
    "range";
    "type";
    "continue";
    "for";
    "import";
    "return";
    "var";
  ]

let go_keywords_set = List.fold_left (fun s k -> StringSet.add k s) StringSet.empty go_keywords

let go_safe_ident (name : string) : string =
  let buffer = Buffer.create (String.length name + 8) in
  String.iter
    (function
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as c -> Buffer.add_char buffer c
      | '?' -> Buffer.add_string buffer "_q"
      | '!' -> Buffer.add_string buffer "_bang"
      | c -> Buffer.add_string buffer (Printf.sprintf "_u%04x" (Char.code c)))
    name;
  let sanitized = Buffer.contents buffer in
  let sanitized =
    if sanitized = "" then
      "_"
    else
      match sanitized.[0] with
      | '0' .. '9' -> "_" ^ sanitized
      | _ -> sanitized
  in
  if StringSet.mem sanitized go_keywords_set then
    sanitized ^ "_"
  else
    sanitized

let canonical_codegen_trait_name (trait_name : string) : string =
  Typecheck.Trait_registry.canonical_trait_name trait_name

(* Generate mangled function name: name_type1_type2_... *)
let mangle_func_name name (param_types : Types.mono_type list) : string =
  let go_name = go_safe_ident name in
  if param_types = [] then
    go_name
  else
    go_name ^ "_" ^ (List.map mangle_type param_types |> String.concat "_")

let fingerprint_types (types : Types.mono_type list) : string =
  if types = [] then
    "unit"
  else
    String.concat "__" (List.map mangle_type types)

let trait_method_func_name (trait_name : string) (method_name : string) (type_suffix : string) : string =
  let trait_name = canonical_codegen_trait_name trait_name in
  Printf.sprintf "%s_%s_%s" (go_safe_ident trait_name) (go_safe_ident method_name) type_suffix

let inherent_method_func_name (method_name : string) (type_suffix : string) : string =
  Printf.sprintf "inherent_%s_%s" (go_safe_ident method_name) type_suffix

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
  method_type_args : Types.mono_type list; (* Phase 6.4: resolved method-level type args *)
  type_fingerprint : string;
}

type impl_inst_payload = {
  param_names : string list;
  param_types : Types.mono_type list;
  return_type : Types.mono_type;
  body_stmt : AST.statement;
  specialization_subst : Types.substitution;
}

type impl_template_method = {
  method_name : string;
  param_names : string list;
  param_types : Types.mono_type list;
  return_type : Types.mono_type;
  body_stmt : AST.statement;
  method_generic_names : string list; (* Phase 6.4: names of method-level type params *)
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
  record_field_func_aliases : (string, string) Hashtbl.t; (* "recordVar.fieldName" -> user function name *)
  value_func_aliases : (string, string) Hashtbl.t;
      (* local value alias (e.g. let f = r.compute) -> user function name *)
  mutable top_level_value_bindings : StringSet.t;
      (* top-level non-function let bindings available to later top-level functions *)
  call_resolution_map : (int, Infer.method_resolution) Hashtbl.t;
      (* Phase 5: explicit call-resolution metadata from typechecker *)
  method_type_args_map : (int, Types.mono_type list) Hashtbl.t;
      (* Phase 6.4: resolved method-level type args per call site *)
  method_def_map : (int, Typecheck.Resolution_artifacts.typed_method_def) Hashtbl.t;
      (* Phase 6.3: typed method definitions keyed by method id *)
  placeholder_rewrite_map : Infer.placeholder_rewrite_map;
      (* Placeholder shorthand rewrites keyed by original expression id *)
}

let create_mono_state
    ?(module_path = "main")
    ?(concrete_only = true)
    ?(call_resolution_map = Hashtbl.create 0)
    ?(method_type_args_map = Hashtbl.create 0)
    ?(method_def_map = Hashtbl.create 0)
    ?(placeholder_rewrite_map = Hashtbl.create 0)
    () =
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
    record_field_func_aliases = Hashtbl.create 32;
    value_func_aliases = Hashtbl.create 32;
    top_level_value_bindings = StringSet.empty;
    call_resolution_map;
    method_type_args_map;
    method_def_map;
    placeholder_rewrite_map;
  }

let placeholder_rewritten_expr (rewrite_map : Infer.placeholder_rewrite_map) (expr : AST.expression) : AST.expression =
  match Hashtbl.find_opt rewrite_map expr.id with
  | Some rewritten -> rewritten
  | None -> expr

let lookup_method_def_exn
    (method_def_map : (int, Typecheck.Resolution_artifacts.typed_method_def) Hashtbl.t)
    ~(method_id : int)
    ~(owner : string)
    ~(method_name : string) : Typecheck.Resolution_artifacts.typed_method_def =
  match Hashtbl.find_opt method_def_map method_id with
  | Some def -> def
  | None ->
      failwith
        (Printf.sprintf "Codegen error: missing typed method metadata for %s.%s (method id %d)" owner
           method_name method_id)

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
    |> List.map (fun (f : Types.record_field_type) -> f.name ^ "_" ^ mangle_type_for_shape f.typ)
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
      let fields, _ = normalize_record_row_type fields _row in
      record_shape_name fields
  | Types.TRowVar _ -> "interface{}"
  | Types.TUnion _ -> "interface{}"
  | Types.TEnum _ -> mangle_type_for_shape t

let go_record_field_name (name : string) : string = go_safe_ident name

let merge_record_fields
    (base_fields : Types.record_field_type list) (override_fields : Types.record_field_type list) :
    Types.record_field_type list =
  let by_name : (string, Types.record_field_type) Hashtbl.t =
    Hashtbl.create (List.length base_fields + List.length override_fields)
  in
  List.iter (fun (f : Types.record_field_type) -> Hashtbl.replace by_name f.name f) base_fields;
  List.iter (fun (f : Types.record_field_type) -> Hashtbl.replace by_name f.name f) override_fields;
  Hashtbl.to_seq_keys by_name
  |> List.of_seq
  |> List.sort String.compare
  |> List.map (fun name ->
         match Hashtbl.find_opt by_name name with
         | Some field -> field
         | None -> failwith "merge_record_fields: impossible missing field")

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
  | Types.TRecord (fields, row) ->
      let fields, _ = normalize_record_row_type fields row in
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
                let subst = Types.substitution_of_list (List.combine enum_def.type_params type_args) in
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
        let subst = Types.substitution_of_list (List.combine enum_def.type_params type_args) in
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

let rec captures_top_level_values_expr
    (top_level_values : StringSet.t) (bound : StringSet.t) (expr : AST.expression) : StringSet.t =
  let merge_sets = List.fold_left StringSet.union StringSet.empty in
  match expr.expr with
  | AST.Integer _ | AST.Float _ | AST.Boolean _ | AST.String _ -> StringSet.empty
  | AST.Identifier name ->
      if StringSet.mem name top_level_values && not (StringSet.mem name bound) then
        StringSet.singleton name
      else
        StringSet.empty
  | AST.Prefix (_, e) | AST.TypeCheck (e, _) -> captures_top_level_values_expr top_level_values bound e
  | AST.Infix (l, _, r) ->
      StringSet.union
        (captures_top_level_values_expr top_level_values bound l)
        (captures_top_level_values_expr top_level_values bound r)
  | AST.If (cond, cons, alt) ->
      merge_sets
        [
          captures_top_level_values_expr top_level_values bound cond;
          captures_top_level_values_stmt top_level_values bound cons;
          (match alt with
          | Some s -> captures_top_level_values_stmt top_level_values bound s
          | None -> StringSet.empty);
        ]
  | AST.Call (f, args) ->
      merge_sets
        (captures_top_level_values_expr top_level_values bound f
        :: List.map (captures_top_level_values_expr top_level_values bound) args)
  | AST.Array elements -> merge_sets (List.map (captures_top_level_values_expr top_level_values bound) elements)
  | AST.Hash pairs ->
      merge_sets
        (List.concat_map
           (fun (k, v) ->
             [
               captures_top_level_values_expr top_level_values bound k;
               captures_top_level_values_expr top_level_values bound v;
             ])
           pairs)
  | AST.Index (container, index) ->
      StringSet.union
        (captures_top_level_values_expr top_level_values bound container)
        (captures_top_level_values_expr top_level_values bound index)
  | AST.Function f ->
      let param_bound = List.fold_left (fun acc (name, _) -> StringSet.add name acc) bound f.params in
      captures_top_level_values_stmt top_level_values param_bound f.body
  | AST.EnumConstructor (_, _, args) ->
      merge_sets (List.map (captures_top_level_values_expr top_level_values bound) args)
  | AST.Match (scrutinee, arms) ->
      merge_sets
        (captures_top_level_values_expr top_level_values bound scrutinee
        :: List.map
             (fun (arm : AST.match_arm) -> captures_top_level_values_expr top_level_values bound arm.body)
             arms)
  | AST.RecordLit (fields, spread) ->
      merge_sets
        (List.concat_map
           (fun (field : AST.record_field) ->
             match field.field_value with
             | Some v -> [ captures_top_level_values_expr top_level_values bound v ]
             | None -> [])
           fields
        @
        match spread with
        | Some s -> [ captures_top_level_values_expr top_level_values bound s ]
        | None -> [])
  | AST.FieldAccess (receiver, _) -> captures_top_level_values_expr top_level_values bound receiver
  | AST.MethodCall { mc_receiver; mc_args; _ } ->
      merge_sets
        (captures_top_level_values_expr top_level_values bound mc_receiver
        :: List.map (captures_top_level_values_expr top_level_values bound) mc_args)
  | AST.BlockExpr stmts -> merge_sets (List.map (captures_top_level_values_stmt top_level_values bound) stmts)

and captures_top_level_values_stmt (top_level_values : StringSet.t) (bound : StringSet.t) (stmt : AST.statement) :
    StringSet.t =
  match stmt.stmt with
  | AST.Let let_binding -> captures_top_level_values_expr top_level_values bound let_binding.value
  | AST.Return e | AST.ExpressionStmt e -> captures_top_level_values_expr top_level_values bound e
  | AST.Block stmts ->
      let _, captured =
        List.fold_left
          (fun (bound_acc, captured_acc) (s : AST.statement) ->
            let captures_here = captures_top_level_values_stmt top_level_values bound_acc s in
            let bound_next =
              match s.stmt with
              | AST.Let let_binding -> StringSet.add let_binding.name bound_acc
              | _ -> bound_acc
            in
            (bound_next, StringSet.union captured_acc captures_here))
          (bound, StringSet.empty) stmts
      in
      captured
  | AST.EnumDef _ | AST.TraitDef _ | AST.ImplDef _ | AST.InherentImplDef _ | AST.DeriveDef _ | AST.TypeAlias _ ->
      StringSet.empty

let top_level_function_captures
    (state : mono_state) (params : (string * AST.type_expr option) list) (body : AST.statement) : string list =
  let bound_params = List.fold_left (fun acc (name, _) -> StringSet.add name acc) StringSet.empty params in
  captures_top_level_values_stmt state.top_level_value_bindings bound_params body |> StringSet.elements

let rec collect_funcs_stmt ?(top_level = false) (state : mono_state) (stmt : AST.statement) : unit =
  match stmt.stmt with
  | AST.Let let_binding -> (
      match let_binding.value.expr with
      | AST.Function f when top_level ->
          let captures = top_level_function_captures state f.params f.body in
          let should_stay_local = captures <> [] in
          if should_stay_local then
            collect_funcs_expr state let_binding.value
          else
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
              :: state.func_defs
      | AST.Function _ ->
          (* Non-top-level named function bindings remain local closures. *)
          collect_funcs_expr state let_binding.value
      | _ ->
          if top_level then
            state.top_level_value_bindings <- StringSet.add let_binding.name state.top_level_value_bindings;
          (* Phase 2: Convert new param format to expressions *)
          collect_funcs_expr state let_binding.value)
  | AST.Return e -> collect_funcs_expr state e
  | AST.ExpressionStmt e -> collect_funcs_expr state e
  | AST.Block stmts -> List.iter (collect_funcs_stmt ~top_level:false state) stmts
  | AST.EnumDef _ -> () (* Enums are compile-time only *)
  | AST.TraitDef _ | AST.ImplDef _ | AST.InherentImplDef _ | AST.DeriveDef _ | AST.TypeAlias _ ->
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
      collect_funcs_stmt ~top_level:false state cons;
      Option.iter (collect_funcs_stmt ~top_level:false state) alt
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
  | AST.MethodCall { mc_receiver; mc_args; _ } ->
      collect_funcs_expr state mc_receiver;
      List.iter (collect_funcs_expr state) mc_args
  | AST.BlockExpr stmts -> List.iter (collect_funcs_stmt state) stmts

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
  | Types.TRecord (fields, row) -> (
      List.exists (fun (f : Types.record_field_type) -> has_type_vars f.typ) fields
      ||
      match row with
      | Some r -> has_type_vars r
      | None -> false)
  | Types.TRowVar _ -> true
  | _ -> false

let rec has_open_record_rows (t : Types.mono_type) : bool =
  match t with
  | Types.TRecord (fields, row) ->
      let normalized_fields, normalized_row = normalize_record_row_type fields row in
      Option.is_some normalized_row
      || List.exists (fun (f : Types.record_field_type) -> has_open_record_rows f.typ) normalized_fields
  | Types.TFun (arg, ret, _) -> has_open_record_rows arg || has_open_record_rows ret
  | Types.TArray elem -> has_open_record_rows elem
  | Types.THash (k, v) -> has_open_record_rows k || has_open_record_rows v
  | Types.TEnum (_, args) | Types.TUnion args -> List.exists has_open_record_rows args
  | _ -> false

let has_unresolved_codegen_type (t : Types.mono_type) : bool = has_type_vars t || has_open_record_rows t

let enum_allows_unresolved_erasure (enum_name : string) : bool =
  Typecheck.Inherent_registry.all_methods ()
  |> List.exists (fun (for_type, _method_sig) ->
         match for_type with
         | Types.TEnum (name, args) -> name = enum_name && List.exists has_type_vars args
         | _ -> false)

(* Track an enum instantiation *)
let track_enum_inst state (t : Types.mono_type) =
  match t with
  | Types.TEnum (name, args) ->
      if state.concrete_only && List.exists has_type_vars args then
        if enum_allows_unresolved_erasure name then
          let erased_args = List.map erase_unresolved_type_vars_for_codegen args in
          state.enum_insts <- EnumInstSet.add (name, erased_args) state.enum_insts
        else
          ()
      else
        state.enum_insts <- EnumInstSet.add (name, args) state.enum_insts
  | _ -> ()

let is_user_func (state : mono_state) (name : string) : bool =
  List.exists (fun fd -> fd.name = name) state.func_defs

let unique_instantiated_func_name (state : mono_state) (name : string) : string option =
  let candidates =
    InstSet.elements state.instantiations |> List.filter (fun (inst : instantiation) -> inst.func_name = name)
  in
  match candidates with
  | [ inst ] when not (List.exists has_type_vars inst.concrete_types) ->
      Some (mangle_func_name name inst.concrete_types)
  | _ -> None

let matches_instantiated_param (param_type : Types.mono_type) (arg_type : Types.mono_type) : bool =
  match param_type with
  | Types.TUnion members -> List.exists (fun member -> member = arg_type) members
  | _ -> (
      match Unify.unify (Types.canonicalize_mono_type param_type) (Types.canonicalize_mono_type arg_type) with
      | Ok _ -> true
      | Error _ -> false)

let instantiated_func_name_for_args (state : mono_state) (name : string) (arg_types : Types.mono_type list) :
    string option =
  let candidates =
    InstSet.elements state.instantiations
    |> List.filter (fun (inst : instantiation) ->
           inst.func_name = name
           && List.length inst.concrete_types = List.length arg_types
           && List.for_all2 matches_instantiated_param inst.concrete_types arg_types)
  in
  match candidates with
  | [ inst ] -> Some (mangle_func_name name inst.concrete_types)
  | _ -> None

let lookup_func_def_for_call (state : mono_state) (name : string) (arity : int) : func_def option =
  let candidates = List.filter (fun fd -> fd.name = name && List.length fd.params = arity) state.func_defs in
  match candidates with
  | [] -> None
  | [ fd ] -> Some fd
  | many ->
      let ids = many |> List.map (fun (fd : func_def) -> string_of_int fd.func_expr_id) |> String.concat ", " in
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

let callable_signature_exact (arity : int) (t : Types.mono_type) : (Types.mono_type list * Types.mono_type) option
    =
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

let add_param_bindings (env : Infer.type_env) (param_names : string list) (param_types : Types.mono_type list) :
    Infer.type_env =
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
    (impl_type_params : AST.generic_param list) (type_expr : AST.type_expr) : Types.mono_type =
  let bindings = impl_type_bindings impl_type_params in
  annotation_exn (Annotation.type_expr_to_mono_type_with bindings type_expr)

let is_known_inherent_type_name (name : string) : bool =
  Annotation.builtin_primitive_type name <> None
  || Annotation.builtin_type_constructor_name name <> None
  || Annotation.lookup_enum_by_source_name name <> None
  || Annotation.lookup_type_alias name <> None
  || Typecheck.Trait_registry.lookup_trait name <> None

let rec collect_inherent_target_generic_names ~(in_head : bool) (te : AST.type_expr) (acc : StringSet.t) :
    StringSet.t =
  match te with
  | AST.TCon name ->
      if in_head || is_known_inherent_type_name name then
        acc
      else
        StringSet.add name acc
  | AST.TVar name ->
      if in_head then
        acc
      else
        StringSet.add name acc
  | AST.TApp (_con_name, args) ->
      List.fold_left (fun acc' arg -> collect_inherent_target_generic_names ~in_head:false arg acc') acc args
  | AST.TArrow (params, ret, _) ->
      let acc' =
        List.fold_left
          (fun acc' param -> collect_inherent_target_generic_names ~in_head:false param acc')
          acc params
      in
      collect_inherent_target_generic_names ~in_head:false ret acc'
  | AST.TUnion members ->
      List.fold_left
        (fun acc' member -> collect_inherent_target_generic_names ~in_head:false member acc')
        acc members
  | AST.TRecord (fields, _row_var) ->
      List.fold_left
        (fun acc' (field : AST.record_type_field) ->
          collect_inherent_target_generic_names ~in_head:false field.field_type acc')
        acc fields

let inherent_type_bindings (inherent_for_type : AST.type_expr) : (string * Types.mono_type) list =
  collect_inherent_target_generic_names ~in_head:true inherent_for_type StringSet.empty
  |> StringSet.elements
  |> List.map (fun name -> (name, Types.TVar name))

let register_impl_template (state : mono_state) (impl : AST.impl_def) : impl_template =
  let for_type = type_expr_to_mono_type_with_impl_bindings impl.impl_type_params impl.impl_for_type in
  let explicit_methods =
    List.map
      (fun (m : AST.method_impl) ->
        let method_generic_names =
          match m.impl_method_generics with
          | None -> []
          | Some gps -> List.map (fun (gp : AST.generic_param) -> gp.name) gps
        in
        let def =
          lookup_method_def_exn state.method_def_map ~method_id:m.impl_method_id ~owner:impl.impl_trait_name
            ~method_name:m.impl_method_name
        in
        let param_names, param_types, return_type = (def.md_param_names, def.md_param_types, def.md_return_type) in
        {
          method_name = m.impl_method_name;
          param_names;
          param_types;
          return_type;
          body_stmt = m.impl_method_body;
          method_generic_names;
        })
      impl.impl_methods
  in
  (* Also include trait default methods not explicitly provided in this impl *)
  let default_methods =
    match Typecheck.Trait_registry.lookup_trait impl.impl_trait_name with
    | None -> []
    | Some trait_def ->
        let explicit_names = List.map (fun (m : AST.method_impl) -> m.impl_method_name) impl.impl_methods in
        let concrete_for_type = Types.canonicalize_mono_type for_type in
        let substitute =
          match trait_def.trait_type_param with
          | None -> fun t -> t
          | Some tp ->
              let subst = Types.substitution_singleton tp concrete_for_type in
              fun t -> Types.apply_substitution subst t
        in
        List.filter_map
          (fun (m : Typecheck.Trait_registry.method_sig) ->
            match m.method_default_impl with
            | None -> None
            | Some default_expr ->
                if List.mem m.method_name explicit_names then
                  None
                else
                  let param_names = List.map fst m.method_params in
                  let param_types = List.map (fun (_, t) -> substitute t) m.method_params in
                  let return_type = substitute m.method_return_type in
                  let body_stmt =
                    AST.mk_stmt ~pos:default_expr.pos ~end_pos:default_expr.end_pos ~file_id:default_expr.file_id
                      (AST.Block
                         [
                           AST.mk_stmt ~pos:default_expr.pos ~end_pos:default_expr.end_pos
                             ~file_id:default_expr.file_id (AST.ExpressionStmt default_expr);
                         ])
                  in
                  Some
                    {
                      method_name = m.method_name;
                      param_names;
                      param_types;
                      return_type;
                      body_stmt;
                      method_generic_names = List.map fst m.method_generics;
                    })
          trait_def.trait_methods
  in
  let methods = explicit_methods @ default_methods in
  let template =
    {
      trait_name = canonical_codegen_trait_name impl.impl_trait_name;
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

let add_instantiation (state : mono_state) (inst : instantiation) : unit =
  if state.concrete_only && List.exists has_type_vars inst.concrete_types then
    ()
  else
    let existing = InstSet.find_opt inst state.instantiations in
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
             inst.func_name inst.func_expr_id inst.type_fingerprint
             (Types.to_string existing_inst.return_type)
             (Types.to_string inst.return_type))
    | _ -> state.instantiations <- InstSet.add inst state.instantiations

let impl_inst_payload_key (inst : impl_instantiation) : string =
  Printf.sprintf "%s|%b|%s|%s|%s" inst.module_path inst.concrete_only_mode inst.trait_name inst.method_name
    inst.type_fingerprint

let add_impl_instantiation (state : mono_state) (inst : impl_instantiation) (payload : impl_inst_payload) : unit =
  let payload_key = impl_inst_payload_key inst in
  let existing_payload_opt = Hashtbl.find_opt state.impl_inst_payloads payload_key in
  (match existing_payload_opt with
  | Some _existing
    when match ImplInstSet.find_opt inst state.impl_instantiations with
         | Some existing_inst -> existing_inst.for_type <> inst.for_type
         | None -> false ->
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
        (Printf.sprintf "Codegen error: inconsistent return type for impl instantiation '%s.%s' (%s): %s vs %s"
           inst.trait_name inst.method_name inst.type_fingerprint
           (Types.to_string existing.return_type)
           (Types.to_string payload.return_type))
  | Some _ -> ()
  | None -> Hashtbl.replace state.impl_inst_payloads payload_key payload);
  state.impl_instantiations <- ImplInstSet.add inst state.impl_instantiations

let field_alias_key (record_name : string) (field_name : string) : string = record_name ^ "." ^ field_name

let register_user_func_call_instantiation
    (state : mono_state)
    (type_map : Infer.type_map)
    (env : Infer.type_env)
    ~(target_name : string)
    ~(args : AST.expression list)
    ~(call_expr : AST.expression) : unit =
  let args = List.map (placeholder_rewritten_expr state.placeholder_rewrite_map) args in
  let arity = List.length args in
  let func_def_opt = lookup_func_def_for_call state target_name arity in
  let arg_types = List.map (get_type type_map) args in
  let declared_signature_opt =
    match Infer.TypeEnv.find_opt target_name env with
    | Some (Types.Forall (_, func_type)) -> extract_param_types_exact arity func_type
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
  let has_union_param =
    List.exists
      (function
        | Types.TUnion _ -> true
        | _ -> false)
      declared_param_types
  in
  let concrete_param_types =
    match specialized_declared_signature with
    | Some (specialized_params, _specialized_return) when not (List.exists has_type_vars specialized_params) ->
        specialized_params
    | None ->
        if has_union_param then
          declared_param_types
        else
          arg_types
    | Some _ ->
        if has_union_param then
          declared_param_types
        else
          arg_types
  in
  let inferred_call_return = get_type type_map call_expr in
  let return_type =
    match declared_return_type_opt with
    | Some declared_return_type ->
        let candidate_return =
          match specialized_declared_signature with
          | Some (_specialized_params, specialized_return) -> specialized_return
          | None -> (
              match
                infer_return_type_from_signature declared_param_types declared_return_type concrete_param_types
              with
              | Some inferred_ret -> inferred_ret
              | None -> inferred_call_return)
        in
        if has_unresolved_codegen_type candidate_return then
          inferred_call_return
        else
          candidate_return
    | None -> inferred_call_return
  in
  match func_def_opt with
  | None -> ()
  | Some func_def ->
      if state.concrete_only && List.exists has_type_vars concrete_param_types then
        ()
      else
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
        add_instantiation state inst

let select_impl_template_for_type
    (state : mono_state) (trait_name : string) (method_name : string) (for_type : Types.mono_type) :
    (impl_template_method * Types.substitution) option =
  let trait_name = canonical_codegen_trait_name trait_name in
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
          (Printf.sprintf "Codegen error: impl template for trait '%s' is missing method '%s'" trait_name
             method_name)
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
        (Printf.sprintf "Codegen error: ambiguous concrete impl templates for trait '%s', method '%s', type %s"
           trait_name method_name (Types.to_string for_type'))
  | [] -> (
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
      match generic_matches with
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
               "Codegen error: ambiguous generic impl templates for trait '%s', method '%s', type %s (matching patterns: %s)"
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
      (match let_binding.value.expr with
      | AST.Identifier target_name when is_user_func state target_name ->
          Hashtbl.replace state.value_func_aliases let_binding.name target_name
      | AST.FieldAccess ({ expr = AST.Identifier record_name; _ }, field_name) -> (
          match Hashtbl.find_opt state.record_field_func_aliases (field_alias_key record_name field_name) with
          | Some target_name -> Hashtbl.replace state.value_func_aliases let_binding.name target_name
          | None -> Hashtbl.remove state.value_func_aliases let_binding.name)
      | AST.RecordLit (fields, _) ->
          List.iter
            (fun (field : AST.record_field) ->
              match field.field_value with
              | Some { expr = AST.Identifier target_name; _ } when is_user_func state target_name ->
                  Hashtbl.replace state.record_field_func_aliases
                    (field_alias_key let_binding.name field.field_name)
                    target_name
              | _ -> ())
            fields;
          Hashtbl.remove state.value_func_aliases let_binding.name
      | _ -> Hashtbl.remove state.value_func_aliases let_binding.name);
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
        let for_type = annotation_exn (Annotation.type_expr_to_mono_type impl.impl_for_type) in
        let for_type_fingerprint = fingerprint_types [ for_type ] in
        let explicit_method_names =
          List.map (fun (m : AST.method_impl) -> m.impl_method_name) impl.impl_methods
        in
        List.iter
          (fun (m : AST.method_impl) ->
            (* Skip eager instantiation for methods with method-level generics;
                they'll be instantiated on-demand via register_impl_method_use per call site *)
            let has_method_generics =
              match m.impl_method_generics with
              | Some (_ :: _) -> true
              | _ -> false
            in
            if not has_method_generics then (
              let def =
                lookup_method_def_exn state.method_def_map ~method_id:m.impl_method_id ~owner:impl.impl_trait_name
                  ~method_name:m.impl_method_name
              in
              let method_param_names, method_param_types, return_type =
                (def.md_param_names, def.md_param_types, def.md_return_type)
              in
              let impl_inst =
                {
                  trait_name = canonical_codegen_trait_name impl.impl_trait_name;
                  method_name = m.impl_method_name;
                  module_path = state.module_path;
                  concrete_only_mode = state.concrete_only;
                  for_type;
                  method_type_args = [];
                  type_fingerprint = for_type_fingerprint;
                }
              in
              let payload : impl_inst_payload =
                {
                  param_names = method_param_names;
                  param_types = method_param_types;
                  return_type;
                  body_stmt = m.impl_method_body;
                  specialization_subst = Types.empty_substitution;
                }
              in
              add_impl_instantiation state impl_inst payload;
              let method_env = add_param_bindings env method_param_names method_param_types in
              ignore (collect_insts_stmt state type_map method_env m.impl_method_body)))
          impl.impl_methods;
        (* Also instantiate trait default methods not explicitly provided in this impl *)
        match Typecheck.Trait_registry.lookup_trait impl.impl_trait_name with
        | None -> ()
        | Some trait_def ->
            let substitute =
              match trait_def.trait_type_param with
              | None -> fun t -> t
              | Some tp ->
                  let subst = Types.substitution_singleton tp for_type in
                  fun t -> Types.apply_substitution subst t
            in
            List.iter
              (fun (m : Typecheck.Trait_registry.method_sig) ->
                match m.method_default_impl with
                | None -> () (* No default, skip *)
                | Some default_expr ->
                    if List.mem m.method_name explicit_method_names then
                      ()
                      (* Explicitly provided, skip *)
                    else if m.method_generics <> [] then
                      ()
                      (* Method-level generics: specialized per call site, not eagerly *)
                    else
                      let param_names = List.map fst m.method_params in
                      let param_types = List.map (fun (_, t) -> substitute t) m.method_params in
                      let return_type = substitute m.method_return_type in
                      let body_stmt =
                        AST.mk_stmt ~pos:default_expr.pos ~end_pos:default_expr.end_pos
                          ~file_id:default_expr.file_id
                          (AST.Block
                             [
                               AST.mk_stmt ~pos:default_expr.pos ~end_pos:default_expr.end_pos
                                 ~file_id:default_expr.file_id (AST.ExpressionStmt default_expr);
                             ])
                      in
                      let impl_inst =
                        {
                          trait_name = canonical_codegen_trait_name impl.impl_trait_name;
                          method_name = m.method_name;
                          module_path = state.module_path;
                          concrete_only_mode = state.concrete_only;
                          for_type;
                          method_type_args = [];
                          type_fingerprint = for_type_fingerprint;
                        }
                      in
                      let specialization_subst =
                        match trait_def.trait_type_param with
                        | None -> Types.empty_substitution
                        | Some tp -> Types.substitution_singleton tp for_type
                      in
                      let payload : impl_inst_payload =
                        { param_names; param_types; return_type; body_stmt; specialization_subst }
                      in
                      add_impl_instantiation state impl_inst payload)
              trait_def.trait_methods);
      env
  | AST.InherentImplDef impl ->
      let bindings = inherent_type_bindings impl.inherent_for_type in
      let _for_type = annotation_exn (Annotation.type_expr_to_mono_type_with bindings impl.inherent_for_type) in
      List.iter
        (fun (m : AST.method_impl) ->
          (* Skip body collection for methods with method-level generics;
             they're specialized per call site in emit_inherent_methods *)
          let has_method_generics =
            match m.impl_method_generics with
            | Some (_ :: _) -> true
            | _ -> false
          in
          if not has_method_generics then
            let def =
              lookup_method_def_exn state.method_def_map ~method_id:m.impl_method_id
                ~owner:(Types.to_string (annotation_exn (Annotation.type_expr_to_mono_type_with bindings impl.inherent_for_type)))
                ~method_name:m.impl_method_name
            in
            let method_param_names, method_param_types = (def.md_param_names, def.md_param_types) in
            let method_env = add_param_bindings env method_param_names method_param_types in
            ignore (collect_insts_stmt state type_map method_env m.impl_method_body))
        impl.inherent_methods;
      env

and register_impl_method_use
    (state : mono_state)
    (type_map : Infer.type_map)
    (env : Infer.type_env)
    ~(trait_name : string)
    ~(method_name : string)
    ~(for_type : Types.mono_type)
    ~(method_type_args : Types.mono_type list) : unit =
  let trait_name = canonical_codegen_trait_name trait_name in
  let for_type' = Types.canonicalize_mono_type for_type in
  if state.concrete_only && has_type_vars for_type' then
    ()
  else
    match select_impl_template_for_type state trait_name method_name for_type' with
    | None -> ()
    | Some (template_method, impl_subst) ->
        (* Phase 6.4: Build method-generic substitution from template's generic names + call-site type args *)
        let method_generic_subst =
          if template_method.method_generic_names = [] || method_type_args = [] then
            Types.SubstMap.empty
          else
            List.fold_left2
              (fun acc name concrete -> Types.SubstMap.add name concrete acc)
              Types.SubstMap.empty template_method.method_generic_names method_type_args
        in
        let combined_subst = Types.SubstMap.union (fun _k _a b -> Some b) impl_subst method_generic_subst in
        let param_types =
          List.map
            (fun t -> Types.canonicalize_mono_type (Types.apply_substitution combined_subst t))
            template_method.param_types
        in
        let return_type =
          Types.canonicalize_mono_type (Types.apply_substitution combined_subst template_method.return_type)
        in
        if state.concrete_only && List.exists has_type_vars (for_type' :: return_type :: param_types) then
          failwith
            (Printf.sprintf "Codegen error: unresolved type variables after impl specialization for '%s.%s' on %s"
               trait_name method_name (Types.to_string for_type'));
        let impl_inst =
          {
            trait_name;
            method_name;
            module_path = state.module_path;
            concrete_only_mode = state.concrete_only;
            for_type = for_type';
            method_type_args;
            type_fingerprint = fingerprint_types (for_type' :: method_type_args);
          }
        in
        if ImplInstSet.mem impl_inst state.impl_instantiations then
          ()
        else
          let payload : impl_inst_payload =
            {
              param_names = template_method.param_names;
              param_types;
              return_type;
              body_stmt = template_method.body_stmt;
              specialization_subst = combined_subst;
            }
          in
          add_impl_instantiation state impl_inst payload;
          let method_env = add_param_bindings env payload.param_names payload.param_types in
          ignore (collect_insts_stmt state type_map method_env payload.body_stmt)

and collect_insts_expr
    ?(expected_type : Types.mono_type option = None)
    (state : mono_state)
    (type_map : Infer.type_map)
    (env : Infer.type_env)
    (expr : AST.expression) : unit =
  let expr = placeholder_rewritten_expr state.placeholder_rewrite_map expr in
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
              let inferred_arity = List.length inferred_params in
              match callable_signature_exact inferred_arity expected_func_type with
              | Some (callable_params, callable_ret) -> (callable_params, callable_ret)
              | None -> (inferred_params, inferred_ret))
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
  | AST.Prefix (op, e) -> (
      collect_insts_expr state type_map env e;
      match op with
      | "-" -> (
          let operand_type = Types.canonicalize_mono_type (get_type type_map e) in
          match operand_type with
          | Types.TInt | Types.TFloat -> ()
          | _ ->
              register_impl_method_use state type_map env ~trait_name:"neg" ~method_name:"neg"
                ~for_type:operand_type ~method_type_args:[])
      | _ -> ())
  | AST.Infix (l, op, r) -> (
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
      match op with
      | "+" ->
          if left_type = Types.TString || is_num_primitive then
            ()
          else
            register_impl_method_use state type_map env ~trait_name:"num" ~method_name:"add" ~for_type:left_type
              ~method_type_args:[]
      | "-" ->
          if is_num_primitive then
            ()
          else
            register_impl_method_use state type_map env ~trait_name:"num" ~method_name:"sub" ~for_type:left_type
              ~method_type_args:[]
      | "*" ->
          if is_num_primitive then
            ()
          else
            register_impl_method_use state type_map env ~trait_name:"num" ~method_name:"mul" ~for_type:left_type
              ~method_type_args:[]
      | "/" ->
          if is_num_primitive then
            ()
          else
            register_impl_method_use state type_map env ~trait_name:"num" ~method_name:"div" ~for_type:left_type
              ~method_type_args:[]
      | "==" | "!=" ->
          if is_eq_primitive then
            ()
          else
            register_impl_method_use state type_map env ~trait_name:"eq" ~method_name:"eq" ~for_type:left_type
              ~method_type_args:[]
      | "<" | ">" | "<=" | ">=" ->
          if is_ord_primitive then
            ()
          else
            register_impl_method_use state type_map env ~trait_name:"ord" ~method_name:"compare"
              ~for_type:left_type ~method_type_args:[]
      | _ -> ())
  | AST.TypeCheck (e, _) -> collect_insts_expr state type_map env e
  | AST.If (cond, cons, alt) ->
      collect_insts_expr state type_map env cond;
      ignore (collect_insts_stmt state type_map env cons);
      Option.iter (fun s -> ignore (collect_insts_stmt state type_map env s)) alt
  | AST.Call (func, args) -> (
      let args = List.map (placeholder_rewritten_expr state.placeholder_rewrite_map) args in
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
            | Some (Types.Forall (_, func_type)) -> extract_param_types_exact arity func_type
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
          (* Prefer specialized declared params when available (important for
             generic/HOF call sites where raw arg types may still contain TVars). *)
          let concrete_param_types =
            match specialized_declared_signature with
            | Some (specialized_params, _specialized_return)
              when not (List.exists has_type_vars specialized_params) ->
                specialized_params
            | None ->
                if has_union_param then
                  declared_param_types
                else
                  arg_types
            | Some _ ->
                if has_union_param then
                  declared_param_types
                else
                  arg_types
          in
          (* Revisit args with expected concrete parameter types so function-valued
             arguments can register concrete instantiations. *)
          let rec revisit_with_expected remaining_args remaining_expected =
            match (remaining_args, remaining_expected) with
            | [], _ -> ()
            | arg :: rest_args, expected_ty :: rest_expected ->
                collect_insts_expr ~expected_type:(Some expected_ty) state type_map env arg;
                revisit_with_expected rest_args rest_expected
            | arg :: rest_args, [] ->
                collect_insts_expr state type_map env arg;
                revisit_with_expected rest_args []
          in
          revisit_with_expected args concrete_param_types;
          let inferred_call_return = get_type type_map expr in
          let return_type =
            match declared_return_type_opt with
            | Some declared_return_type ->
                let candidate_return =
                  match specialized_declared_signature with
                  | Some (_specialized_params, specialized_return) -> specialized_return
                  | None -> (
                      match
                        infer_return_type_from_signature declared_param_types declared_return_type
                          concrete_param_types
                      with
                      | Some inferred_ret -> inferred_ret
                      | None -> inferred_call_return)
                in
                if has_unresolved_codegen_type candidate_return then
                  inferred_call_return
                else
                  candidate_return
            | None -> inferred_call_return
          in
          match func_def_opt with
          | None -> ()
          | Some func_def ->
              if state.concrete_only && List.exists has_type_vars concrete_param_types then
                ()
              else
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
      | AST.Identifier alias_name -> (
          match Hashtbl.find_opt state.value_func_aliases alias_name with
          | Some target_name when is_user_func state target_name ->
              register_user_func_call_instantiation state type_map env ~target_name ~args ~call_expr:expr
          | _ -> ())
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
  | AST.MethodCall { mc_receiver = receiver; mc_method = method_name; mc_args = args; _ } -> (
      (* Check if this is an enum constructor and register it *)
      match receiver.expr with
      | AST.Identifier enum_name when Typecheck.Enum_registry.lookup enum_name <> None ->
          (* This is an enum constructor - track it like EnumConstructor *)
          let enum_type = get_type type_map expr in
          track_enum_inst state enum_type;
          (* Also collect instantiations in arguments *)
          List.iter (fun arg -> collect_insts_expr state type_map env arg) args
      | _ -> (
          (* Real method call - collect insts in receiver and args *)
          List.iter (fun arg -> collect_insts_expr state type_map env arg) args;
          let method_resolution = Hashtbl.find_opt state.call_resolution_map expr.id in
          match method_resolution with
          | Some (Infer.QualifiedTraitMethod trait_name) ->
              (* Qualified: first arg is receiver *)
              let for_type = Types.canonicalize_mono_type (get_type type_map (List.hd args)) in
              let method_type_args =
                match Hashtbl.find_opt state.method_type_args_map expr.id with
                | Some args -> args
                | None -> []
              in
              register_impl_method_use state type_map env ~trait_name ~method_name ~for_type ~method_type_args
          | Some (Infer.TraitMethod trait_name) ->
              collect_insts_expr state type_map env receiver;
              let for_type = Types.canonicalize_mono_type (get_type type_map receiver) in
              let method_type_args =
                match Hashtbl.find_opt state.method_type_args_map expr.id with
                | Some args -> args
                | None -> []
              in
              register_impl_method_use state type_map env ~trait_name ~method_name ~for_type ~method_type_args
          | Some Infer.InherentMethod -> collect_insts_expr state type_map env receiver
          | Some Infer.QualifiedInherentMethod -> ()
          | None -> (
              collect_insts_expr state type_map env receiver;
              match receiver.expr with
              | AST.Identifier record_name -> (
                  match
                    Hashtbl.find_opt state.record_field_func_aliases (field_alias_key record_name method_name)
                  with
                  | Some target_name when is_user_func state target_name ->
                      register_user_func_call_instantiation state type_map env ~target_name ~args ~call_expr:expr
                  | _ ->
                      (* Parsed as MethodCall but typechecked as field-function call fallback. *)
                      let field_expr =
                        AST.mk_expr ~pos:expr.pos ~end_pos:expr.end_pos ~file_id:expr.file_id
                          (AST.FieldAccess (receiver, method_name))
                      in
                      let synthetic_call =
                        AST.mk_expr ~pos:expr.pos ~end_pos:expr.end_pos ~file_id:expr.file_id
                          (AST.Call (field_expr, args))
                      in
                      collect_insts_expr state type_map env synthetic_call)
              | _ ->
                  let field_expr =
                    AST.mk_expr ~pos:expr.pos ~end_pos:expr.end_pos ~file_id:expr.file_id
                      (AST.FieldAccess (receiver, method_name))
                  in
                  let synthetic_call =
                    AST.mk_expr ~pos:expr.pos ~end_pos:expr.end_pos ~file_id:expr.file_id
                      (AST.Call (field_expr, args))
                  in
                  collect_insts_expr state type_map env synthetic_call)))
  | AST.BlockExpr stmts ->
      ignore (List.fold_left (fun e stmt -> collect_insts_stmt state type_map e stmt) env stmts)

(* ============================================================
   Code Generation State
   ============================================================ *)

type emit_state = {
  mutable indent : int;
  mutable current_return_type : Types.mono_type option;
  mono : mono_state;
}

let create_emit_state mono = { indent = 1; current_return_type = None; mono }
let indent_str state = String.make (state.indent * 4) ' '

let fresh_temp_name (state : emit_state) (prefix : string) : string =
  let n = state.mono.name_counter in
  state.mono.name_counter <- n + 1;
  Printf.sprintf "%s_%d" prefix n

let with_indent_delta (state : emit_state) (delta : int) (f : unit -> 'a) : 'a =
  state.indent <- state.indent + delta;
  Fun.protect ~finally:(fun () -> state.indent <- state.indent - delta) f

let with_return_type (state : emit_state) (return_type : Types.mono_type) (f : unit -> 'a) : 'a =
  let previous = state.current_return_type in
  state.current_return_type <- Some return_type;
  Fun.protect ~finally:(fun () -> state.current_return_type <- previous) f

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
  | Some (Types.TRecord (expected_fields, expected_row)) -> (
      let expected_fields, _ = normalize_record_row_type expected_fields expected_row in
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
          let actual_fields, actual_row = normalize_record_row_type actual_fields actual_row in
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
  | field :: rest -> (
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
      | None -> current_result)

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
  | AST.MethodCall { mc_receiver; mc_args; _ } ->
      copy_specialized_expr_types source_map target_map specialization_subst mc_receiver;
      List.iter (copy_specialized_expr_types source_map target_map specialization_subst) mc_args
  | AST.BlockExpr stmts ->
      List.iter (copy_specialized_stmt_types source_map target_map specialization_subst) stmts

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
  | AST.EnumDef _ | AST.TraitDef _ | AST.ImplDef _ | AST.InherentImplDef _ | AST.DeriveDef _ | AST.TypeAlias _ ->
      ()

(* ============================================================
   Expression Emission
   ============================================================ *)

let rec emit_expr
    ?(expected_type : Types.mono_type option = None)
    (state : emit_state)
    (type_map : Infer.type_map)
    (env : Infer.type_env)
    (expr : AST.expression) : string =
  let expr = placeholder_rewritten_expr state.mono.placeholder_rewrite_map expr in
  let emitted =
    match expr.expr with
    | AST.Integer i -> Printf.sprintf "int64(%Ld)" i
    | AST.Float f -> Printf.sprintf "float64(%g)" f
    | AST.Boolean true -> "true"
    | AST.Boolean false -> "false"
    | AST.String s -> Printf.sprintf "%S" s
    | AST.Identifier name ->
        if is_user_func state.mono name then
          let inferred_param_types =
            match Infer.TypeEnv.find_opt name env with
            | Some (Types.Forall (_, inferred_func_type)) -> fst (extract_all_param_types inferred_func_type)
            | None -> (
                match Hashtbl.find_opt type_map expr.id with
                | Some inferred_func_type -> fst (extract_all_param_types inferred_func_type)
                | None -> [])
          in
          let param_types =
            match expected_type with
            | Some expected_func_type ->
                let expected_param_types, _ = extract_all_param_types expected_func_type in
                if expected_param_types <> [] then
                  expected_param_types
                else
                  inferred_param_types
            | None -> inferred_param_types
          in
          if List.exists has_type_vars param_types then
            match unique_instantiated_func_name state.mono name with
            | Some resolved_name -> resolved_name
            | None -> go_safe_ident name
          else
            mangle_func_name name param_types
        else
          go_safe_ident name
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
        let enum_type =
          match enum_type with
          | Types.TEnum (_, type_args) -> Types.TEnum (enum_name, type_args)
          | _ -> Types.TEnum (enum_name, [])
        in
        let enum_type = Types.canonicalize_mono_type enum_type in
        let enum_type =
          if state.mono.concrete_only && has_type_vars enum_type && enum_allows_unresolved_erasure enum_name then
            normalize_type_for_codegen ~concrete_only:state.mono.concrete_only enum_type
          else
            enum_type
        in
        (* Generate the mangled constructor name *)
        let go_type_name = mangle_type enum_type in
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
        let declared_fields, _result_row =
          match record_type with
          | Types.TRecord (f, row) -> normalize_record_row_type f row
          | _ ->
              failwith
                (Printf.sprintf "Record literal expected record type, got %s" (Types.to_string record_type))
        in
        let result_fields =
          match spread with
          | None -> declared_fields
          | Some base_expr -> (
              match type_from_env_or_map env type_map base_expr |> Option.map Types.canonicalize_mono_type with
              | Some (Types.TRecord (base_fields, base_row)) ->
                  let base_fields, _ = normalize_record_row_type base_fields base_row in
                  merge_record_fields base_fields declared_fields
              | _ -> declared_fields)
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
            let uses_spread_base =
              List.exists
                (fun (field : Types.record_field_type) ->
                  Option.is_none (find_last_record_field_expr field.name fields))
                result_fields
            in
            let assignments =
              List.map
                (fun f ->
                  emit_field_assignment f
                    (if uses_spread_base then
                       Some "__base"
                     else
                       None))
                result_fields
            in
            if uses_spread_base then
              Printf.sprintf "(func() %s { __base := %s; return %s{%s} })()" struct_type base_str struct_type
                (String.concat ", " assignments)
            else
              (* Preserve spread-base evaluation semantics even when all fields are overridden. *)
              Printf.sprintf "(func() %s { _ = %s; return %s{%s} })()" struct_type base_str struct_type
                (String.concat ", " assignments))
    | AST.FieldAccess (receiver, variant_name) -> (
        (* Check if this is a nullary enum constructor — bound variables shadow enum names *)
        match receiver.expr with
        | AST.Identifier enum_name
          when Typecheck.Enum_registry.lookup enum_name <> None && Infer.TypeEnv.find_opt enum_name env = None ->
            (* This is a nullary enum constructor - emit like EnumConstructor with no args *)
            let enum_type =
              match expected_type with
              | Some t -> t
              | None -> get_type type_map expr
            in
            let enum_type =
              match enum_type with
              | Types.TEnum (_, type_args) -> Types.TEnum (enum_name, type_args)
              | _ -> Types.TEnum (enum_name, [])
            in
            let enum_type = Types.canonicalize_mono_type enum_type in
            let enum_type =
              if state.mono.concrete_only && has_type_vars enum_type && enum_allows_unresolved_erasure enum_name
              then
                normalize_type_for_codegen ~concrete_only:state.mono.concrete_only enum_type
              else
                enum_type
            in
            (* Generate the mangled constructor name *)
            let go_type_name = mangle_type enum_type in
            let constructor_name = Printf.sprintf "%s_%s" go_type_name variant_name in
            Printf.sprintf "%s()" constructor_name
        | _ ->
            (* Real field access *)
            let receiver_str = emit_expr state type_map env receiver in
            Printf.sprintf "(%s).%s" receiver_str (go_record_field_name variant_name))
    | AST.MethodCall { mc_receiver = receiver; mc_method = variant_name; mc_args = args; _ } -> (
        (* Check if this is an enum constructor — bound variables shadow enum names *)
        match receiver.expr with
        | AST.Identifier enum_name
          when Typecheck.Enum_registry.lookup enum_name <> None && Infer.TypeEnv.find_opt enum_name env = None ->
            (* This is an enum constructor - emit like EnumConstructor *)
            (* Use expected_type when available, otherwise fall back to inferred constructor type. *)
            let enum_type =
              match expected_type with
              | Some t -> t
              | None -> get_type type_map expr
            in
            let enum_type =
              match enum_type with
              | Types.TEnum (_, type_args) -> Types.TEnum (enum_name, type_args)
              | _ -> Types.TEnum (enum_name, [])
            in
            let enum_type = Types.canonicalize_mono_type enum_type in
            let enum_type =
              if state.mono.concrete_only && has_type_vars enum_type && enum_allows_unresolved_erasure enum_name
              then
                normalize_type_for_codegen ~concrete_only:state.mono.concrete_only enum_type
              else
                enum_type
            in
            (* Generate the mangled constructor name *)
            let go_type_name = mangle_type enum_type in
            let constructor_name = Printf.sprintf "%s_%s" go_type_name variant_name in
            (* Emit arguments *)
            let arg_strs = List.map (emit_expr state type_map env) args in
            if arg_strs = [] then
              Printf.sprintf "%s()" constructor_name
            else
              Printf.sprintf "%s(%s)" constructor_name (String.concat ", " arg_strs)
        | _ -> (
            (* Real method call - emit using typechecker-selected method source *)
            (* Use method-resolution metadata from typechecking; codegen must not re-resolve. *)
            let method_resolution = Hashtbl.find_opt state.mono.call_resolution_map expr.id in
            let method_type_args =
              match Hashtbl.find_opt state.mono.method_type_args_map expr.id with
              | Some args -> args
              | None -> []
            in
            let type_suffix_with_mta for_type =
              if method_type_args = [] then
                mangle_type for_type
              else
                fingerprint_types (for_type :: method_type_args)
            in
            match method_resolution with
            | Some (Infer.QualifiedTraitMethod trait_name) ->
                (* Qualified call: Trait.method(receiver, args...) — first arg is the receiver *)
                let first_arg_type =
                  normalize_type_for_codegen ~concrete_only:state.mono.concrete_only
                    (get_type type_map (List.hd args))
                in
                let type_suffix = type_suffix_with_mta first_arg_type in
                let func_name = trait_method_func_name trait_name variant_name type_suffix in
                let arg_strs = List.map (emit_expr state type_map env) args in
                Printf.sprintf "%s(%s)" func_name (String.concat ", " arg_strs)
            | Some Infer.QualifiedInherentMethod ->
                (* Qualified call: Type.method(receiver, args...) — first arg is the receiver *)
                let first_arg_type =
                  normalize_type_for_codegen ~concrete_only:state.mono.concrete_only
                    (get_type type_map (List.hd args))
                in
                let type_suffix = type_suffix_with_mta first_arg_type in
                let func_name = inherent_method_func_name variant_name type_suffix in
                let arg_strs = List.map (emit_expr state type_map env) args in
                Printf.sprintf "%s(%s)" func_name (String.concat ", " arg_strs)
            | Some (Infer.TraitMethod trait_name) ->
                (* Dot call: receiver.method(args...) — receiver is separate *)
                let receiver_type =
                  normalize_type_for_codegen ~concrete_only:state.mono.concrete_only (get_type type_map receiver)
                in
                let type_suffix = type_suffix_with_mta receiver_type in
                let func_name = trait_method_func_name trait_name variant_name type_suffix in
                let receiver_str = emit_expr state type_map env receiver in
                let arg_strs = List.map (emit_expr state type_map env) args in
                let all_args = receiver_str :: arg_strs in
                Printf.sprintf "%s(%s)" func_name (String.concat ", " all_args)
            | Some Infer.InherentMethod ->
                (* Dot call: receiver.method(args...) — receiver is separate *)
                let receiver_type =
                  normalize_type_for_codegen ~concrete_only:state.mono.concrete_only (get_type type_map receiver)
                in
                let type_suffix = type_suffix_with_mta receiver_type in
                let func_name = inherent_method_func_name variant_name type_suffix in
                let receiver_str = emit_expr state type_map env receiver in
                let arg_strs = List.map (emit_expr state type_map env) args in
                let all_args = receiver_str :: arg_strs in
                Printf.sprintf "%s(%s)" func_name (String.concat ", " all_args)
            | None ->
                (* Parsed as MethodCall but typechecked as field-function call fallback. *)
                let receiver_str = emit_expr state type_map env receiver in
                let arg_strs = List.map (emit_expr state type_map env) args in
                Printf.sprintf "((%s).%s)(%s)" receiver_str (go_record_field_name variant_name)
                  (String.concat ", " arg_strs)))
    | AST.BlockExpr stmts -> (
        (* Emit as immediately-invoked closure: func() T { stmts; return last }() *)
        let result_type = get_type type_map expr in
        let go_type = type_to_go state.mono result_type in
        match List.rev stmts with
        | [] -> Printf.sprintf "(func() %s { return struct{}{} })()" go_type
        | last :: rest ->
            let body_str =
              with_indent_delta state 2 (fun () ->
                  let stmts_str, env' = emit_stmts state type_map env (List.rev rest) in
                  let last_str =
                    match last.stmt with
                    | AST.ExpressionStmt e ->
                        let inner_ind = indent_str state in
                        inner_ind ^ "return " ^ emit_expr_for_expected_type state type_map env' result_type e ^ "\n"
                    | AST.Return e ->
                        let inner_ind = indent_str state in
                        inner_ind ^ "return " ^ emit_expr_for_expected_type state type_map env' result_type e ^ "\n"
                    | _ -> fst (emit_stmt state type_map env' last)
                  in
                  stmts_str ^ last_str)
            in
            let ind = indent_str state in
            Printf.sprintf "(func() %s {\n%s%s})()" go_type body_str ind)
  in
  maybe_project_to_expected_record_type state type_map env expr expected_type emitted

and emit_expr_for_expected_type
    (state : emit_state)
    (type_map : Infer.type_map)
    (env : Infer.type_env)
    (expected_type : Types.mono_type)
    (expr : AST.expression) : string =
  emit_expr ~expected_type:(Some expected_type) state type_map env expr

and emit_expr_for_current_return
    (state : emit_state)
    (type_map : Infer.type_map)
    (env : Infer.type_env)
    (expr : AST.expression) : string =
  match state.current_return_type with
  | Some expected_type -> emit_expr_for_expected_type state type_map env expected_type expr
  | None -> emit_expr state type_map env expr

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
  let scrutinee_var = fresh_temp_name state "__scrutinee" in
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
          let body_str = emit_expr_for_expected_type state type_map env match_result_type body in
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
    | AST.PWildcard -> Printf.sprintf "%s {\n%s\t}" branch_prefix_else body_code
    | AST.PVariable name ->
        let go_name = go_safe_ident name in
        Printf.sprintf "%s {\n\t\t%s := %s\n\t\t_ = %s\n%s\t}" branch_prefix_else go_name scrutinee_var go_name
          body_code
    | AST.PRecord (fields, rest) ->
        let cond_parts, bind_lines =
          List.fold_left
            (fun (conds, binds) (f : AST.record_pattern_field) ->
              let access = Printf.sprintf "%s.%s" scrutinee_var (go_record_field_name f.pat_field_name) in
              match f.pat_field_pattern with
              | None -> (conds, Printf.sprintf "%s := %s" (go_safe_ident f.pat_field_name) access :: binds)
              | Some p -> (
                  match p.AST.pat with
                  | AST.PWildcard -> (conds, binds)
                  | AST.PVariable name -> (conds, Printf.sprintf "%s := %s" (go_safe_ident name) access :: binds)
                  | AST.PLiteral lit -> (Printf.sprintf "(%s == %s)" access (emit_lit lit) :: conds, binds)
                  | _ -> failwith "Complex record patterns in codegen are not yet supported"))
            ([], []) fields
        in
        let bind_lines =
          match rest with
          | None -> bind_lines
          | Some rest_name -> Printf.sprintf "%s := %s" (go_safe_ident rest_name) scrutinee_var :: bind_lines
        in
        let cond_str =
          match List.rev cond_parts with
          | [] -> "true"
          | cs -> String.concat " && " cs
        in
        let binds_block =
          let lines = List.rev bind_lines in
          let markers = List.map (fun line -> "\t\t_ = " ^ String.sub line 0 (String.index line ' ')) lines in
          match lines with
          | [] -> ""
          | _ -> String.concat "\n" (List.map (fun l -> "\t\t" ^ l) lines @ markers) ^ "\n"
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
      Printf.sprintf "%s%s := %s\n%s\n%spanic(\"non-exhaustive record match\")\n" ind scrutinee_var scrutinee_str
        (String.concat "" arm_blocks) ind
  | Some (AssignTarget _) ->
      (* No panic for assignment — execution continues after the if-chain *)
      let ind = indent_str state in
      Printf.sprintf "%s%s := %s\n%s\n" ind scrutinee_var scrutinee_str (String.concat "" arm_blocks)
  | None ->
      Printf.sprintf "(func() %s {\n\t%s := %s\n%s\n\tpanic(\"non-exhaustive record match\")\n})()"
        match_result_go_type scrutinee_var scrutinee_str (String.concat "" arm_blocks)

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
  let scrutinee_var = fresh_temp_name state "__scrutinee" in

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
            let emit_arm_body () =
              with_indent_delta state 2 (fun () -> emit_expr_to_target state type_map env arm.body t)
            in
            match pattern.AST.pat with
            | AST.PWildcard ->
                let ind = indent_str state in
                Printf.sprintf "%sdefault:\n%s" (ind ^ "    ") (emit_arm_body ())
            | AST.PVariable var_name ->
                let ind = indent_str state in
                let go_var_name = go_safe_ident var_name in
                Printf.sprintf "%sdefault:\n%s%s := %s\n%s_ = %s\n%s" (ind ^ "    ") (ind ^ "        ")
                  go_var_name scrutinee_var (ind ^ "        ") go_var_name (emit_arm_body ())
            | AST.PConstructor (enum_name_pat, variant_name, field_patterns) ->
                let variant_opt =
                  List.find_opt
                    (fun (v : Typecheck.Enum_registry.variant_def) -> v.name = variant_name)
                    enum_def.variants
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
                      let go_name = go_safe_ident var_name in
                      Printf.sprintf "%s%s := %s.%s\n%s_ = %s\n" (ind ^ "        ") go_name scrutinee_var
                        data_field_name (ind ^ "        ") go_name)
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
        Printf.sprintf "%s%s := %s\n%sswitch %s.Tag {\n%s\n%s}\n" ind scrutinee_var scrutinee_str ind
          scrutinee_var match_body ind
      else
        Printf.sprintf
          "%s%s := %s\n%sswitch %s.Tag {\n%s\n%sdefault:\n%s\tpanic(\"unreachable: exhaustive match\")\n%s}\n" ind
          scrutinee_var scrutinee_str ind scrutinee_var match_body ind ind ind
  | None ->
      let result_prefix = "return " in
      let match_body =
        let cases =
          List.map
            (fun (arm : AST.match_arm) ->
              match arm.patterns with
              | [ pattern ] ->
                  emit_match_arm_enum ~result_prefix ~scrutinee_var ~expected_type:match_result_type state
                    type_map env go_type_name enum_def type_args pattern arm.body
              | [] -> failwith "Match arm must have at least one pattern"
              | _ -> failwith "Multiple patterns per arm not yet supported in codegen")
            arms
        in
        String.concat "\n" cases
      in
      if has_wildcard then
        Printf.sprintf "(func() %s {\n\t%s := %s\n\tswitch %s.Tag {\n%s\n\t}\n})()" match_result_go_type
          scrutinee_var scrutinee_str scrutinee_var match_body
      else
        Printf.sprintf
          "(func() %s {\n\t%s := %s\n\tswitch %s.Tag {\n%s\n\tdefault:\n\t\tpanic(\"unreachable: exhaustive match\")\n\t}\n})()"
          match_result_go_type scrutinee_var scrutinee_str scrutinee_var match_body

and emit_match_primitive ?target state type_map env scrutinee scrutinee_type arms match_result_type =
  (* Emit scrutinee to a temporary variable *)
  let scrutinee_str = emit_expr state type_map env scrutinee in
  let scrutinee_var = fresh_temp_name state "__scrutinee" in

  (* Convert match result type to Go type *)
  let match_result_go_type = type_to_go state.mono match_result_type in

  match target with
  | Some t ->
      let emit_case_target (arm : AST.match_arm) =
        match arm.patterns with
        | [ pattern ] -> (
            let emit_arm_body () =
              with_indent_delta state 2 (fun () -> emit_expr_to_target state type_map env arm.body t)
            in
            match pattern.AST.pat with
            | AST.PWildcard ->
                let ind = indent_str state in
                Printf.sprintf "%sdefault:\n%s" (ind ^ "    ") (emit_arm_body ())
            | AST.PVariable var_name ->
                let ind = indent_str state in
                let go_var_name = go_safe_ident var_name in
                Printf.sprintf "%sdefault:\n%s%s := %s\n%s_ = %s\n%s" (ind ^ "    ") (ind ^ "        ")
                  go_var_name scrutinee_var (ind ^ "        ") go_var_name (emit_arm_body ())
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
      Printf.sprintf "%s%s := %s\n%sswitch %s {\n%s\n%s}\n" ind scrutinee_var scrutinee_str ind scrutinee_var
        match_body ind
  | None ->
      let result_prefix = "return " in
      let match_body =
        let cases =
          List.map
            (fun (arm : AST.match_arm) ->
              match arm.patterns with
              | [ pattern ] ->
                  emit_match_arm_primitive ~result_prefix ~scrutinee_var ~expected_type:match_result_type state
                    type_map env scrutinee_type pattern arm.body
              | [] -> failwith "Match arm must have at least one pattern"
              | _ -> failwith "Multiple patterns per arm not yet supported in codegen")
            arms
        in
        String.concat "\n" cases
      in
      Printf.sprintf "(func() %s {\n\t%s := %s\n\tswitch %s {\n%s\n\t}\n})()" match_result_go_type scrutinee_var
        scrutinee_str scrutinee_var match_body

and emit_match_arm_primitive
    ?(result_prefix = "return ")
    ?(scrutinee_var = "__scrutinee")
    ~expected_type
    state
    type_map
    env
    _scrutinee_type
    pattern
    body
    =
  match pattern.AST.pat with
  | AST.PWildcard ->
      let body_str = emit_expr_for_expected_type state type_map env expected_type body in
      Printf.sprintf "\tdefault:\n\t\t%s%s" result_prefix body_str
  | AST.PVariable var_name ->
      (* Variable pattern binds the scrutinee *)
      let body_str = emit_expr_for_expected_type state type_map env expected_type body in
      let go_var_name = go_safe_ident var_name in
      Printf.sprintf "\tdefault:\n\t\t%s := %s\n\t\t_ = %s\n\t\t%s%s" go_var_name scrutinee_var go_var_name
        result_prefix body_str
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
      let body_str = emit_expr_for_expected_type state type_map env expected_type body in
      Printf.sprintf "\tcase %s:\n\t\t%s%s" lit_str result_prefix body_str
  | AST.PConstructor _ -> failwith "Constructor patterns not valid for primitive match"
  | AST.PRecord _ -> failwith "Record patterns not valid for primitive match"

and emit_match_arm_enum
    ?(result_prefix = "return ")
    ?(scrutinee_var = "__scrutinee")
    ~expected_type
    state
    type_map
    env
    go_type_name
    enum_def
    type_args
    pattern
    body =
  match pattern.AST.pat with
  | AST.PWildcard ->
      (* Wildcard matches everything - use default case *)
      let body_str = emit_expr_for_expected_type state type_map env expected_type body in
      Printf.sprintf "\tdefault:\n\t\t%s%s" result_prefix body_str
  | AST.PLiteral _lit ->
      (* Literal patterns - not for enums, shouldn't happen *)
      failwith "Literal patterns not supported for enum match"
  | AST.PVariable _var_name ->
      (* Variable pattern - binds the entire enum value *)
      (* For now, treat as wildcard since we don't track variable bindings in Go codegen *)
      let body_str = emit_expr_for_expected_type state type_map env expected_type body in
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
            let go_name = go_safe_ident var_name in
            Printf.sprintf "\t\t%s := %s.%s\n\t\t_ = %s" go_name scrutinee_var data_field_name go_name)
          bindings
      in

      let bindings_code =
        if bindings = [] then
          ""
        else
          String.concat "\n" binding_strs ^ "\n"
      in

      (* Emit body with bindings *)
      let body_str = emit_expr_for_expected_type state type_map env expected_type body in

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
        Printf.sprintf "((ord_compare_%s(%s, %s)).Tag == ordering_less_tag)" type_suffix left_str right_str
  | ">" ->
      if is_ord_primitive then
        Printf.sprintf "(%s > %s)" left_str right_str
      else
        Printf.sprintf "((ord_compare_%s(%s, %s)).Tag == ordering_greater_tag)" type_suffix left_str right_str
  | "<=" ->
      if is_ord_primitive then
        Printf.sprintf "(%s <= %s)" left_str right_str
      else
        Printf.sprintf "((ord_compare_%s(%s, %s)).Tag != ordering_greater_tag)" type_suffix left_str right_str
  | ">=" ->
      if is_ord_primitive then
        Printf.sprintf "(%s >= %s)" left_str right_str
      else
        Printf.sprintf "((ord_compare_%s(%s, %s)).Tag != ordering_less_tag)" type_suffix left_str right_str
  | _ -> failwith ("Unknown infix operator: " ^ op)

and emit_type_check state type_map env expr type_ann =
  (* Use runtime helper function for type checking *)
  let expr_str = emit_expr state type_map env expr in
  let check_type = annotation_exn (Annotation.type_expr_to_mono_type type_ann) in
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
    | AST.MethodCall { mc_receiver; mc_method; mc_type_args; mc_args } ->
        {
          e with
          expr =
            AST.MethodCall
              {
                mc_receiver = subst_expr mc_receiver;
                mc_method;
                mc_type_args;
                mc_args = List.map subst_expr mc_args;
              };
        }
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
    | AST.TraitDef _ | AST.ImplDef _ | AST.InherentImplDef _ | AST.DeriveDef _ | AST.TypeAlias _ -> s
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
      ^ emit_expr_for_expected_type state type_map branch_env result_type e
      ^ "\n"
      ^ inner_ind
      ^ "        return struct{}{}\n"
    else
      inner_ind ^ "        return " ^ emit_expr_for_expected_type state type_map branch_env result_type e ^ "\n"
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
    state type_map env var_name narrow_type complement_type_opt result_type cons alt target =
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
              | AST.ExpressionStmt e ->
                  with_indent_delta state 1 (fun () -> emit_expr_to_target state type_map env' e value_target)
              | AST.Return e -> inner_ind ^ "    return " ^ emit_expr_for_current_return state type_map env' e ^ "\n"
              | _ -> fst (emit_stmt state type_map env' last)
            in
            stmts_prefix ^ last_str)
    | AST.ExpressionStmt e ->
        with_indent_delta state 1 (fun () -> emit_expr_to_target state type_map branch_env e value_target)
    | AST.Return e -> inner_ind ^ "    return " ^ emit_expr_for_current_return state type_map branch_env e ^ "\n"
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
            let assertion =
              inner_ind ^ "        " ^ complement_var ^ " := " ^ var_name ^ ".(" ^ complement_type_str ^ ")\n"
            in
            let marker = inner_ind ^ "        _ = " ^ complement_var ^ "\n" in
            let body = emit_branch_to_target complement_env alt_stmt' in
            ind ^ "    default:\n" ^ assertion ^ marker ^ body
        | _ ->
            let body = emit_branch_to_target env alt_stmt in
            ind ^ "    default:\n" ^ body)
  in
  let switch_code =
    Printf.sprintf "%sswitch %s := %s.(type) {\n%scase %s:\n%s%s%s}\n" ind narrowed_var var_name ind
      narrow_type_str cons_str default_branch ind
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
            let narrow_type = annotation_exn (Annotation.type_expr_to_mono_type type_ann) in
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
          ^ emit_expr_for_expected_type state type_map branch_env result_type e
          ^ "\n"
          ^ inner_ind
          ^ "    return struct{}{}\n"
        else
          inner_ind ^ "    return " ^ emit_expr_for_expected_type state type_map branch_env result_type e ^ "\n"
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
  | AST.If (cond, cons, alt) ->
      with_indent_delta state 1 (fun () -> emit_if_to_target state type_map env expr cond cons alt target)
  | AST.Match (scrutinee, arms) ->
      with_indent_delta state 1 (fun () -> emit_match ~target state type_map env expr scrutinee arms)
  | AST.BlockExpr stmts -> (
      (* Emit each statement except the last, then emit the last expression to target *)
      match List.rev stmts with
      | [] -> ind ^ target_prefix target ^ "struct{}{}\n"
      | last :: rest ->
          let stmts_prefix, env' = emit_stmts state type_map env (List.rev rest) in
          let last_str =
            match last.stmt with
            | AST.ExpressionStmt e ->
                with_indent_delta state 1 (fun () -> emit_expr_to_target state type_map env' e target)
            | AST.Return e -> ind ^ "    return " ^ emit_expr_for_current_return state type_map env' e ^ "\n"
            | _ -> fst (emit_stmt state type_map env' last)
          in
          stmts_prefix ^ last_str)
  | _ ->
      let expr_str =
        match target with
        | ReturnTarget -> emit_expr_for_current_return state type_map env expr
        | AssignTarget _ -> (
            match Hashtbl.find_opt type_map expr.id with
            | Some expected_type -> emit_expr_for_expected_type state type_map env expected_type expr
            | None -> emit_expr state type_map env expr)
        | DiscardTarget -> emit_expr state type_map env expr
      in
      ind ^ target_prefix target ^ expr_str ^ "\n"

and emit_if_to_target state type_map env if_expr (cond : AST.expression) cons alt target =
  let result_type = get_type type_map if_expr in
  let type_check_info =
    match cond.expr with
    | AST.TypeCheck (var_expr, type_ann) -> (
        match var_expr.expr with
        | AST.Identifier var_name ->
            let narrow_type = annotation_exn (Annotation.type_expr_to_mono_type type_ann) in
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
      emit_type_switch_to_target state type_map env var_name narrow_type complement_type_opt result_type cons alt
        target
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
                  | AST.ExpressionStmt e ->
                      with_indent_delta state 1 (fun () -> emit_expr_to_target state type_map env' e value_target)
                  | AST.Return e -> inner_ind ^ "    return " ^ emit_expr_for_current_return state type_map env' e ^ "\n"
                  | _ -> fst (emit_stmt state type_map env' last)
                in
                stmts_prefix ^ last_str)
        | AST.ExpressionStmt e ->
            with_indent_delta state 1 (fun () -> emit_expr_to_target state type_map env e value_target)
        | AST.Return e -> inner_ind ^ "    return " ^ emit_expr_for_current_return state type_map env e ^ "\n"
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
  let args = List.map (placeholder_rewritten_expr state.mono.placeholder_rewrite_map) args in
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
    let rec emit acc remaining_args remaining_expected =
      match (remaining_args, remaining_expected) with
      | [], _ -> String.concat ", " (List.rev acc)
      | arg :: rest_args, expected :: rest_expected ->
          emit (emit_expr ~expected_type:(Some expected) state type_map env arg :: acc) rest_args rest_expected
      | arg :: rest_args, [] -> emit (emit_expr state type_map env arg :: acc) rest_args []
    in
    emit [] args expected_types
  in
  (* Check for builtin function calls that need special handling *)
  match func.expr with
  | AST.Identifier "len"
    when match args with
         | [ _ ] -> true
         | _ -> false ->
      let arg_str = emit_expr state type_map env (List.hd args) in
      Printf.sprintf "int64(len(%s))" arg_str
  | AST.Identifier "puts" ->
      let args_str = List.map (emit_expr state type_map env) args |> String.concat ", " in
      Printf.sprintf "puts(%s)" args_str
  | AST.Identifier "first"
    when match args with
         | [ _ ] -> true
         | _ -> false ->
      let arg_str = emit_expr state type_map env (List.hd args) in
      Printf.sprintf "first(%s)" arg_str
  | AST.Identifier "last"
    when match args with
         | [ _ ] -> true
         | _ -> false ->
      let arg_str = emit_expr state type_map env (List.hd args) in
      Printf.sprintf "last(%s)" arg_str
  | AST.Identifier "rest"
    when match args with
         | [ _ ] -> true
         | _ -> false ->
      let arg_str = emit_expr state type_map env (List.hd args) in
      Printf.sprintf "rest(%s)" arg_str
  | AST.Identifier "push"
    when match args with
         | [ _; _ ] -> true
         | _ -> false ->
      let arr_str = emit_expr state type_map env (List.hd args) in
      let val_str = emit_expr state type_map env (List.nth args 1) in
      Printf.sprintf "push(%s, %s)" arr_str val_str
  | AST.Identifier name when is_user_func state.mono name ->
      let arg_types = List.map (get_type type_map) args in
      (* User-defined function - look up declared parameter types to check for unions *)
      let func_param_types =
        match Infer.TypeEnv.find_opt name env with
        | Some (Types.Forall (_, func_type)) ->
            let num_args = List.length args in
            let declared_param_types, _ = extract_param_types num_args func_type in
            declared_param_types
        | None -> arg_types
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
          arg_types
      in
      let mangled_name =
        if List.exists has_type_vars param_types then
          match instantiated_func_name_for_args state.mono name arg_types with
          | Some resolved_name -> resolved_name
          | None -> (
              match unique_instantiated_func_name state.mono name with
              | Some resolved_name -> resolved_name
              | None -> go_safe_ident name)
        else
          mangle_func_name name param_types
      in
      let args_str = emit_args_with_expected_types param_types in
      Printf.sprintf "%s(%s)" mangled_name args_str
  | _ -> (
      let func_str = emit_expr state type_map env func in
      let args_str = emit_args_with_expected_types call_param_types in
      match (callee_type_opt, call_signature_opt) with
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
          (Printf.sprintf "Codegen error: function literal expr id %d expected %d parameters but inferred type %s"
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
  let go_param_names = List.map go_safe_ident param_names in

  if List.length param_names <> List.length param_types then
    failwith
      (Printf.sprintf
         "Codegen error: function literal expr id %d parameter name/type arity mismatch (%d names vs %d types)"
         func_expr.id (List.length param_names) (List.length param_types));

  let params_with_types =
    List.map2 (fun name typ -> name ^ " " ^ type_to_go state.mono typ) go_param_names param_types
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

  let body_str = with_return_type state return_type (fun () -> emit_func_body state body_type_map body_env body) in

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
    | _ -> inner_ind ^ "return " ^ emit_expr_for_current_return state type_map env' e ^ "\n"
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
              | [] -> (String.concat "" (List.rev code_acc), env_acc, false)
              | ({ AST.stmt = AST.Let let_binding; _ } as stmt) :: tl
                when let_binding.name <> "_" && Infer.TypeEnv.mem let_binding.name env_acc ->
                  (* A same-block shadow would emit `x := ...` against an existing Go binding.
                     Lower the remaining function body into a nested Go block so `:=` can
                     introduce the shadow safely while preserving lexical scope. *)
                  let block_stmts = stmt :: (tl @ [ tail_stmt ]) in
                  let block_stmt =
                    AST.mk_stmt ~pos:stmt.pos ~end_pos:tail_stmt.end_pos ~file_id:stmt.file_id
                      (AST.Block block_stmts)
                  in
                  let outer_ind = indent_str state in
                  let env_for_shadow_block = Infer.TypeEnv.remove let_binding.name env_acc in
                  state.indent <- state.indent + 1;
                  let block_body = emit_func_body state type_map env_for_shadow_block block_stmt in
                  state.indent <- state.indent - 1;
                  let block_code = outer_ind ^ "{\n" ^ block_body ^ outer_ind ^ "}\n" in
                  (String.concat "" (List.rev code_acc) ^ block_code, env_acc, true)
              | stmt :: tl ->
                  let following = tl @ [ tail_stmt ] in
                  let code, env' = emit_stmt ~following_stmts:following state type_map env_acc stmt in
                  go env' (code :: code_acc) tl
            in
            go env [] rest_stmts
          in
          let prefix, env', tail_handled = emit_prefix_with_tail_lookahead last in
          if tail_handled then
            prefix
          else
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

and type_from_env_or_map (env : Infer.type_env) (type_map : Infer.type_map) (expr : AST.expression) :
    Types.mono_type option =
  match expr.expr with
  | AST.Identifier id -> (
      match Infer.TypeEnv.find_opt id env with
      | Some (Types.Forall (_, t)) -> Some t
      | None -> Hashtbl.find_opt type_map expr.id)
  | _ -> Hashtbl.find_opt type_map expr.id

and collect_local_call_arg_types_expr
    (name : string) (type_map : Infer.type_map) (env : Infer.type_env) (expr : AST.expression) :
    Types.mono_type list list =
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
        collect_local_call_arg_types_expr name type_map env l
        @ collect_local_call_arg_types_expr name type_map env r
    | AST.TypeCheck (e, _) -> collect_local_call_arg_types_expr name type_map env e
    | AST.If (cond, cons, alt) -> (
        collect_local_call_arg_types_expr name type_map env cond
        @ collect_local_call_arg_types_stmt name type_map env cons
        @
        match alt with
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
    | AST.RecordLit (fields, spread) -> (
        List.concat_map
          (fun (field : AST.record_field) ->
            match field.field_value with
            | Some v -> collect_local_call_arg_types_expr name type_map env v
            | None -> [])
          fields
        @
        match spread with
        | Some s -> collect_local_call_arg_types_expr name type_map env s
        | None -> [])
    | AST.FieldAccess (receiver, _) -> collect_local_call_arg_types_expr name type_map env receiver
    | AST.MethodCall { mc_receiver; mc_args; _ } ->
        collect_local_call_arg_types_expr name type_map env mc_receiver
        @ List.concat_map (collect_local_call_arg_types_expr name type_map env) mc_args
    | AST.BlockExpr stmts -> List.concat_map (collect_local_call_arg_types_stmt name type_map env) stmts
  in
  match expr.expr with
  | AST.Call ({ expr = AST.Identifier callee_name; _ }, args) when callee_name = name -> (
      match List.map (type_from_env_or_map env type_map) args |> all_some with
      | Some arg_types -> arg_types :: nested
      | None -> nested)
  | _ -> nested

and collect_local_call_arg_types_stmt
    (name : string) (type_map : Infer.type_map) (env : Infer.type_env) (stmt : AST.statement) :
    Types.mono_type list list =
  match stmt.stmt with
  | AST.Let { value; _ } -> collect_local_call_arg_types_expr name type_map env value
  | AST.Return e | AST.ExpressionStmt e -> collect_local_call_arg_types_expr name type_map env e
  | AST.Block stmts -> List.concat_map (collect_local_call_arg_types_stmt name type_map env) stmts
  | AST.ImplDef impl ->
      List.concat_map
        (fun (m : AST.method_impl) -> collect_local_call_arg_types_stmt name type_map env m.impl_method_body)
        impl.impl_methods
  | AST.InherentImplDef impl ->
      List.concat_map
        (fun (m : AST.method_impl) -> collect_local_call_arg_types_stmt name type_map env m.impl_method_body)
        impl.inherent_methods
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
        | param :: params_tail, arg :: args_tail -> (
            let param' = Types.apply_substitution subst_acc param in
            let arg' = Types.apply_substitution subst_acc arg in
            match Unify.unify param' arg' with
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
    (stmt : AST.statement) : string * Infer.type_env =
  let ind = indent_str state in
  match stmt.stmt with
  | AST.Let let_binding -> (
      if let_binding.name = "_" then
        (* Blank binding is a discard, not a named variable declaration. *)
        match let_binding.value.expr with
        | AST.If (cond, cons, alt) ->
            let code = emit_if_to_target state type_map env let_binding.value cond cons alt DiscardTarget in
            (code, env)
        | AST.Match (scrutinee, arms) ->
            let code = emit_match ~target:DiscardTarget state type_map env let_binding.value scrutinee arms in
            (code, env)
        | _ ->
            let expr_type = get_type type_map let_binding.value in
            let expr_str = emit_expr ~expected_type:(Some expr_type) state type_map env let_binding.value in
            (Printf.sprintf "%s_ = %s\n" ind expr_str, env)
      else
        (* Use the type from the environment if it exists, otherwise get from type_map *)
        let go_binding_name = go_safe_ident let_binding.name in
        let expr_type =
          match Infer.TypeEnv.find_opt let_binding.name env with
          | Some (Types.Forall (_, t)) -> t
          | None -> get_type type_map let_binding.value
        in
        let expr_type =
          match let_binding.value.expr with
          | AST.Function _ when has_type_vars expr_type -> (
              match
                resolve_local_function_type_from_calls let_binding.name expr_type type_map env following_stmts
              with
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
              Printf.sprintf "%svar %s %s\n%s%s = %s\n%s_ = %s\n" ind go_binding_name go_type ind go_binding_name
                expr_str ind go_binding_name
            in
            let env' = Infer.TypeEnv.add let_binding.name (Types.Forall ([], expr_type)) env in
            (binding_code, env')
        | AST.If (cond, cons, alt) ->
            let go_type = type_to_go state.mono expr_type in
            let decl = Printf.sprintf "%svar %s %s\n" ind go_binding_name go_type in
            let if_code =
              emit_if_to_target state type_map env let_binding.value cond cons alt (AssignTarget go_binding_name)
            in
            let used_marker = Printf.sprintf "%s_ = %s\n" ind go_binding_name in
            let env' = Infer.TypeEnv.add let_binding.name (Types.Forall ([], expr_type)) env in
            (decl ^ if_code ^ used_marker, env')
        | AST.Match (scrutinee, arms) ->
            let go_type = type_to_go state.mono expr_type in
            let decl = Printf.sprintf "%svar %s %s\n" ind go_binding_name go_type in
            let match_code =
              emit_match ~target:(AssignTarget go_binding_name) state type_map env let_binding.value scrutinee
                arms
            in
            let used_marker = Printf.sprintf "%s_ = %s\n" ind go_binding_name in
            let env' = Infer.TypeEnv.add let_binding.name (Types.Forall ([], expr_type)) env in
            (decl ^ match_code ^ used_marker, env')
        | AST.RecordLit (fields, Some base_expr) ->
            (* Record spread without IIFE *)
            let record_type = get_type type_map let_binding.value in
            let declared_fields, _result_row =
              match record_type with
              | Types.TRecord (f, row) -> normalize_record_row_type f row
              | _ ->
                  failwith
                    (Printf.sprintf "Record literal expected record type, got %s" (Types.to_string record_type))
            in
            let result_fields =
              match type_from_env_or_map env type_map base_expr |> Option.map Types.canonicalize_mono_type with
              | Some (Types.TRecord (base_fields, base_row)) ->
                  let base_fields, _ = normalize_record_row_type base_fields base_row in
                  merge_record_fields base_fields declared_fields
              | _ -> declared_fields
            in
            let struct_type = emit_record_struct_type state result_fields in
            let base_str = emit_expr state type_map env base_expr in
            let spread_var = fresh_temp_name state "__spread" in
            let uses_spread_base =
              List.exists
                (fun (field : Types.record_field_type) ->
                  Option.is_none (find_last_record_field_expr field.name fields))
                result_fields
            in
            let spread_decl =
              if uses_spread_base then
                Printf.sprintf "%s%s := %s\n" ind spread_var base_str
              else
                Printf.sprintf "%s_ = %s\n" ind base_str
            in
            let emit_field_assignment field =
              let go_name = go_record_field_name field.Types.name in
              match find_last_record_field_expr field.Types.name fields with
              | Some field_expr ->
                  let field_str = emit_expr state type_map env field_expr in
                  Printf.sprintf "%s: %s" go_name field_str
              | None ->
                  if uses_spread_base then
                    Printf.sprintf "%s: %s.%s" go_name spread_var go_name
                  else
                    failwith
                      (Printf.sprintf "Record field '%s' not provided and spread base disabled" field.Types.name)
            in
            let assignments = List.map emit_field_assignment result_fields in
            let binding_code =
              Printf.sprintf "%s%s := %s{%s}\n%s_ = %s\n" ind go_binding_name struct_type
                (String.concat ", " assignments) ind go_binding_name
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
                  Printf.sprintf "%svar %s %s = %s\n%s_ = %s\n" ind go_binding_name go_type expr_str ind
                    go_binding_name
              | None -> Printf.sprintf "%s%s := %s\n%s_ = %s\n" ind go_binding_name expr_str ind go_binding_name
            in
            let env' = Infer.TypeEnv.add let_binding.name (Types.Forall ([], expr_type)) env in
            (binding_code, env'))
  | AST.Return expr ->
      let expr_str = emit_expr_for_current_return state type_map env expr in
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
  | AST.TraitDef _ | AST.ImplDef _ | AST.InherentImplDef _ | AST.DeriveDef _ | AST.TypeAlias _ ->
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

let rec terminal_expr_of_stmt (stmt : AST.statement) : AST.expression option =
  match stmt.stmt with
  | AST.ExpressionStmt e | AST.Return e -> Some e
  | AST.Block stmts -> (
      match List.rev stmts with
      | [] -> None
      | last :: _ -> terminal_expr_of_stmt last)
  | _ -> None

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
      (Printf.sprintf "Codegen error: specialization for '%s' has arity mismatch (%d params vs %d concrete types)"
         inst.func_name (List.length param_names) (List.length inst.concrete_types));

  let params_with_types =
    List.map2
      (fun name typ -> go_safe_ident name ^ " " ^ type_to_go state.mono typ)
      param_names inst.concrete_types
  in
  let params_str = String.concat ", " params_with_types in

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
  let concrete_func_type =
    List.fold_right (fun param_t acc -> mk_fun param_t acc) inst.concrete_types inst.return_type
  in

  let specialization_subst =
    match Unify.unify generic_func_type concrete_func_type with
    | Ok subst -> subst
    | Error err ->
        failwith
          (Printf.sprintf "Codegen error: failed to specialize function '%s' to concrete type %s: %s"
             inst.func_name (Types.to_string concrete_func_type) err.message)
  in

  let copied_specialized_type_map = Infer.create_type_map () in
  copy_specialized_stmt_types global_type_map copied_specialized_type_map specialization_subst func_def.body;

  (* Re-infer the specialized body with concrete parameters to recover the most precise
     concrete types for nested calls and function return when substitution leaves row vars open. *)
  let provisional_env_with_func =
    Infer.TypeEnv.add inst.func_name (Types.Forall ([], concrete_func_type)) typed_env
  in
  let provisional_body_env = add_param_bindings provisional_env_with_func param_names inst.concrete_types in
  let specialized_type_map, inferred_body_type_opt =
    let inferred_map = Infer.create_type_map () in
    let infer_state = Infer.create_inference_state () in
    match
      Infer.with_inference_state infer_state (fun () ->
          Infer.infer_statement inferred_map provisional_body_env func_def.body)
    with
    | Ok (subst, inferred_body_type) ->
        Infer.apply_substitution_type_map subst inferred_map;
        let inferred_body_type =
          Types.canonicalize_mono_type (Types.apply_substitution subst inferred_body_type)
        in
        (inferred_map, Some inferred_body_type)
    | Error _ -> (copied_specialized_type_map, None)
  in
  let terminal_return_type_opt =
    match terminal_expr_of_stmt func_def.body with
    | None -> None
    | Some terminal_expr -> (
        match Hashtbl.find_opt specialized_type_map terminal_expr.id with
        | Some t -> Some (Types.canonicalize_mono_type t)
        | None ->
            type_from_env_or_map provisional_body_env specialized_type_map terminal_expr
            |> Option.map Types.canonicalize_mono_type)
  in
  let effective_return_type =
    if not (has_unresolved_codegen_type inst.return_type) then
      inst.return_type
    else
      let candidates = List.filter_map Fun.id [ inferred_body_type_opt; terminal_return_type_opt ] in
      match List.find_opt (fun t -> not (has_unresolved_codegen_type t)) candidates with
      | Some t -> t
      | None -> inst.return_type
  in
  let concrete_func_type =
    List.fold_right (fun param_t acc -> mk_fun param_t acc) inst.concrete_types effective_return_type
  in
  let return_type_str = type_to_go state.mono effective_return_type in

  (* Add function and parameters with concrete types for recursive calls/body emission *)
  let env_with_func = Infer.TypeEnv.add inst.func_name (Types.Forall ([], concrete_func_type)) typed_env in
  let body_env = add_param_bindings env_with_func param_names inst.concrete_types in
  (* Collect impl uses from the concrete body so generic trait-method calls
     register the needed concrete impl instantiations before final emission. *)
  ignore (collect_insts_stmt state.mono specialized_type_map body_env func_def.body);

  (* Save and reset indent for top-level function *)
  let saved_indent = state.indent in
  state.indent <- 0;
  let body_str =
    with_return_type state effective_return_type (fun () ->
        emit_func_body state specialized_type_map body_env func_def.body)
  in
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
                 let subst = Types.substitution_of_list (List.combine enum_def.type_params type_args) in
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
                match List.assoc_opt v.name layout.variant_maps with
                | Some m -> m
                | None -> []
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
    (state : emit_state) (type_map : Infer.type_map) (typed_env : Infer.type_env) (impl_inst : impl_instantiation)
    : string =
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
      (Printf.sprintf "Codegen error: impl %s.%s has %d params but %d param types" impl_inst.trait_name
         impl_inst.method_name (List.length payload.param_names) (List.length payload.param_types));

  let type_suffix =
    if impl_inst.method_type_args = [] then
      mangle_type impl_inst.for_type
    else
      impl_inst.type_fingerprint
  in
  let func_name = trait_method_func_name impl_inst.trait_name impl_inst.method_name type_suffix in
  let params_str =
    List.map2
      (fun name typ -> Printf.sprintf "%s %s" (go_safe_ident name) (type_to_go state.mono typ))
      payload.param_names payload.param_types
    |> String.concat ", "
  in
  let return_type_str = type_to_go state.mono payload.return_type in
  let method_env = add_param_bindings typed_env payload.param_names payload.param_types in
  let copied_specialized_type_map () =
    if Types.SubstMap.is_empty payload.specialization_subst then
      type_map
    else
      let copied = Hashtbl.create (Hashtbl.length type_map) in
      Hashtbl.iter
        (fun expr_id mono ->
          let substituted = Types.apply_substitution payload.specialization_subst mono in
          let final =
            if impl_inst.method_type_args = [] then
              substituted
            else
              erase_unresolved_type_vars_for_codegen substituted
          in
          Hashtbl.replace copied expr_id final)
        type_map;
      copied
  in
  (* Generic impl-method specializations can leave fresh inference vars in the
     global type_map. Re-infer the concrete body when possible so nested block
     expressions and calls recover precise concrete types. *)
  let effective_type_map =
    if impl_inst.method_type_args = [] then
      copied_specialized_type_map ()
    else
      let inferred_map = Infer.create_type_map () in
      let infer_state = Infer.create_inference_state () in
      match Infer.with_inference_state infer_state (fun () -> Infer.infer_statement inferred_map method_env payload.body_stmt) with
      | Ok (subst, _body_type) ->
          Infer.apply_substitution_type_map subst inferred_map;
          inferred_map
      | Error _ ->
          copied_specialized_type_map ()
  in
  let body_str =
    with_return_type state payload.return_type (fun () ->
        emit_func_body state effective_type_map method_env payload.body_stmt)
  in
  Printf.sprintf "func %s(%s) %s {\n%s}\n" func_name params_str return_type_str body_str

let emit_inherent_method
    (state : emit_state)
    (type_map : Infer.type_map)
    (typed_env : Infer.type_env)
    (bindings : (string * Types.mono_type) list)
    (for_type : Types.mono_type)
    ~(method_type_args : Types.mono_type list)
    (method_impl : AST.method_impl) : string =
  (* Build impl-level substitution from bindings *)
  let impl_substitution : Types.substitution =
    List.fold_left
      (fun acc (name, ty) ->
        match ty with
        | Types.TVar n when n = name -> acc
        | _ -> Types.SubstMap.add name ty acc)
      Types.SubstMap.empty bindings
  in
  (* Build method-generic substitution from method type params + call-site type args *)
  let method_generic_names =
    match method_impl.impl_method_generics with
    | None -> []
    | Some gps -> List.map (fun (gp : AST.generic_param) -> gp.name) gps
  in
  let method_generic_subst =
    if method_generic_names = [] || method_type_args = [] then
      Types.SubstMap.empty
    else
      List.fold_left2
        (fun acc name concrete -> Types.SubstMap.add name concrete acc)
        Types.SubstMap.empty method_generic_names method_type_args
  in
  (* Also map internal TVars (from body inference) to concrete types.
     E.g. if the body uses t0 for method generic b, and b -> String,
     then also add t0 -> String so type_map entries are properly resolved. *)
  let internal_var_subst =
    match Typecheck.Inherent_registry.lookup_method for_type method_impl.impl_method_name with
    | Some method_sig when method_type_args <> [] ->
        List.fold_left
          (fun acc (generic_name, internal_name) ->
            match List.assoc_opt generic_name (List.combine method_generic_names method_type_args) with
            | Some concrete -> Types.SubstMap.add internal_name concrete acc
            | None -> acc)
          Types.SubstMap.empty method_sig.method_generic_internal_vars
    | _ -> Types.SubstMap.empty
  in
  let combined_subst =
    Types.SubstMap.union
      (fun _k _a b -> Some b)
      (Types.SubstMap.union (fun _k _a b -> Some b) impl_substitution method_generic_subst)
      internal_var_subst
  in
  let def =
    lookup_method_def_exn state.mono.method_def_map ~method_id:method_impl.impl_method_id
      ~owner:(Types.to_string for_type) ~method_name:method_impl.impl_method_name
  in
  let method_param_names = def.md_param_names in
  let method_param_types = List.map (fun t -> Types.apply_substitution combined_subst t) def.md_param_types in
  let return_type = Types.apply_substitution combined_subst def.md_return_type in
  if List.length method_param_names <> List.length method_param_types then
    failwith
      (Printf.sprintf "Codegen error: inherent method %s has %d params but %d param types"
         method_impl.impl_method_name (List.length method_param_names) (List.length method_param_types));
  let for_type = Types.canonicalize_mono_type for_type in
  let type_suffix =
    if method_type_args = [] then
      mangle_type for_type
    else
      fingerprint_types (for_type :: method_type_args)
  in
  let func_name = inherent_method_func_name method_impl.impl_method_name type_suffix in
  let params_str =
    List.map2
      (fun name typ -> Printf.sprintf "%s %s" (go_safe_ident name) (type_to_go state.mono typ))
      method_param_names method_param_types
    |> String.concat ", "
  in
  let return_type_str = type_to_go state.mono return_type in
  let method_env = add_param_bindings typed_env method_param_names method_param_types in
  let method_type_map : Infer.type_map =
    if Types.SubstMap.is_empty combined_subst then
      type_map
    else
      let inferred_map = Infer.create_type_map () in
      let infer_state = Infer.create_inference_state () in
      match
        Infer.with_inference_state infer_state (fun () ->
            Infer.infer_statement inferred_map method_env method_impl.impl_method_body)
      with
      | Ok (subst, _body_type) ->
          Infer.apply_substitution_type_map subst inferred_map;
          inferred_map
      | Error _ ->
          let copied = Hashtbl.create (Hashtbl.length type_map) in
          Hashtbl.iter
            (fun expr_id mono ->
              let substituted = Types.apply_substitution combined_subst mono in
              let final =
                if method_type_args <> [] then
                  erase_unresolved_type_vars_for_codegen substituted
                else
                  substituted
              in
              Hashtbl.replace copied expr_id final)
            type_map;
          copied
  in
  (* For method-generic specializations, register any new enum instantiations
     discovered in the specialized type_map (e.g. opt[String] from opt[b]) *)
  if method_type_args <> [] then (
    track_enum_inst state.mono return_type;
    List.iter
      (fun (_name, pty) -> track_enum_inst state.mono pty)
      (List.combine method_param_names method_param_types);
    Hashtbl.iter (fun _id ty -> track_enum_inst state.mono ty) method_type_map);
  let body_str =
    with_return_type state return_type (fun () ->
        emit_func_body state method_type_map method_env method_impl.impl_method_body)
  in
  Printf.sprintf "func %s(%s) %s {\n%s}\n" func_name params_str return_type_str body_str

(* Extract all ImplDef statements from a program *)
let collect_impl_defs (program : AST.program) : AST.impl_def list =
  List.filter_map
    (fun (stmt : AST.statement) ->
      match stmt.stmt with
      | AST.ImplDef impl -> Some impl
      | _ -> None)
    program

let collect_inherent_impl_defs (program : AST.program) : AST.inherent_impl_def list =
  List.filter_map
    (fun (stmt : AST.statement) ->
      match stmt.stmt with
      | AST.InherentImplDef impl -> Some impl
      | _ -> None)
    program

type inherent_call_site = {
  call_method_name : string;
  call_receiver_type : Types.mono_type;
  call_method_type_args : Types.mono_type list;
}

let add_inherent_call_site_if_new (sites : inherent_call_site list) (site : inherent_call_site) :
    inherent_call_site list =
  if
    List.exists
      (fun s ->
        s.call_method_name = site.call_method_name
        && s.call_receiver_type = site.call_receiver_type
        && s.call_method_type_args = site.call_method_type_args)
      sites
  then
    sites
  else
    site :: sites

let collect_inherent_call_sites
    ~(concrete_only : bool)
    (call_resolution_map : (int, Infer.method_resolution) Hashtbl.t)
    (method_type_args_map : (int, Types.mono_type list) Hashtbl.t)
    (placeholder_rewrite_map : Infer.placeholder_rewrite_map)
    (type_map : Infer.type_map)
    (program : AST.program) : inherent_call_site list =
  let rec collect_expr (acc : inherent_call_site list) (expr : AST.expression) : inherent_call_site list =
    let expr = placeholder_rewritten_expr placeholder_rewrite_map expr in
    match expr.expr with
    | AST.Identifier _ | AST.Integer _ | AST.Float _ | AST.Boolean _ | AST.String _ -> acc
    | AST.Array elems -> List.fold_left collect_expr acc elems
    | AST.Index (a, b) -> collect_expr (collect_expr acc a) b
    | AST.Hash pairs ->
        List.fold_left
          (fun acc' (k, v) ->
            let acc'' = collect_expr acc' k in
            collect_expr acc'' v)
          acc pairs
    | AST.Prefix (_, e) -> collect_expr acc e
    | AST.Infix (l, _, r) -> collect_expr (collect_expr acc l) r
    | AST.TypeCheck (e, _type_ann) -> collect_expr acc e
    | AST.If (condition, consequence, alternative) -> (
        let acc' = collect_expr acc condition in
        let acc'' = collect_stmt acc' consequence in
        match alternative with
        | None -> acc''
        | Some alt -> collect_stmt acc'' alt)
    | AST.Function { body; _ } -> collect_stmt acc body
    | AST.Call (callee, args) ->
        let acc' = collect_expr acc callee in
        List.fold_left collect_expr acc' args
    | AST.EnumConstructor (_enum_name, _variant_name, args) -> List.fold_left collect_expr acc args
    | AST.Match (scrutinee, arms) ->
        let acc' = collect_expr acc scrutinee in
        List.fold_left (fun acc' (arm : AST.match_arm) -> collect_expr acc' arm.body) acc' arms
    | AST.RecordLit (fields, spread) -> (
        let acc' =
          List.fold_left
            (fun acc' (field : AST.record_field) ->
              match field.field_value with
              | None -> acc'
              | Some v -> collect_expr acc' v)
            acc fields
        in
        match spread with
        | None -> acc'
        | Some s -> collect_expr acc' s)
    | AST.FieldAccess (receiver, _field_name) -> collect_expr acc receiver
    | AST.MethodCall { mc_receiver = receiver; mc_method = method_name; mc_args = args; _ } -> (
        let acc' = collect_expr acc receiver in
        let acc'' = List.fold_left collect_expr acc' args in
        match Hashtbl.find_opt call_resolution_map expr.id with
        | Some Infer.InherentMethod ->
            let receiver_type =
              normalize_type_for_codegen ~concrete_only
                (Types.canonicalize_mono_type (get_type type_map receiver))
            in
            let mta =
              match Hashtbl.find_opt method_type_args_map expr.id with
              | Some a -> a
              | None -> []
            in
            add_inherent_call_site_if_new acc''
              { call_method_name = method_name; call_receiver_type = receiver_type; call_method_type_args = mta }
        | Some Infer.QualifiedInherentMethod ->
            let receiver_type =
              normalize_type_for_codegen ~concrete_only
                (Types.canonicalize_mono_type (get_type type_map (List.hd args)))
            in
            let mta =
              match Hashtbl.find_opt method_type_args_map expr.id with
              | Some a -> a
              | None -> []
            in
            add_inherent_call_site_if_new acc''
              { call_method_name = method_name; call_receiver_type = receiver_type; call_method_type_args = mta }
        | Some (Infer.TraitMethod _) | Some (Infer.QualifiedTraitMethod _) | None -> acc'')
    | AST.BlockExpr stmts -> List.fold_left collect_stmt acc stmts
  and collect_stmt (acc : inherent_call_site list) (stmt : AST.statement) : inherent_call_site list =
    match stmt.stmt with
    | AST.Let let_binding -> collect_expr acc let_binding.value
    | AST.Return e | AST.ExpressionStmt e -> collect_expr acc e
    | AST.Block stmts -> List.fold_left collect_stmt acc stmts
    | AST.EnumDef _ | AST.TypeAlias _ | AST.DeriveDef _ -> acc
    | AST.TraitDef trait_def ->
        List.fold_left
          (fun acc' (m : AST.method_sig) ->
            match m.method_default_impl with
            | None -> acc'
            | Some e -> collect_expr acc' e)
          acc trait_def.methods
    | AST.ImplDef impl ->
        List.fold_left
          (fun acc' (m : AST.method_impl) -> collect_stmt acc' m.impl_method_body)
          acc impl.impl_methods
    | AST.InherentImplDef impl ->
        List.fold_left
          (fun acc' (m : AST.method_impl) -> collect_stmt acc' m.impl_method_body)
          acc impl.inherent_methods
  in
  List.fold_left collect_stmt [] program
  |> List.sort (fun a b ->
         let c = String.compare a.call_method_name b.call_method_name in
         if c <> 0 then
           c
         else
           compare a.call_receiver_type b.call_receiver_type)

let builtin_impl_keys : StringPairSet.t =
  StringPairSet.of_list
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
    | Types.TRecord (fields, row) -> normalize_record_row_type fields row
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
        | [] -> "\treturn ordering_equal()\n"
        | _ ->
            fields
            |> List.map compare_pair
            |> List.map (fun (less_expr, eq_expr) ->
                   Printf.sprintf "\tif %s { return ordering_less() }\n\tif !%s { return ordering_greater() }\n"
                     less_expr eq_expr)
            |> String.concat ""
      in
      Some
        (Printf.sprintf "func ord_compare_%s(x, y %s) ordering {\n%s\treturn ordering_equal()\n}\n" type_suffix
           type_str body)
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

let emit_enum_derived_impl (derive_kind : Typecheck.Trait_registry.derive_kind) (enum_type : Types.mono_type) :
    string option =
  let type_suffix = mangle_type enum_type in
  let type_str =
    match enum_type with
    | Types.TEnum (name, []) -> name
    | _ -> type_suffix
  in
  match derive_kind with
  | Typecheck.Trait_registry.DeriveEq ->
      Some (Printf.sprintf "func eq_eq_%s(x, y %s) bool {\n\treturn x.Tag == y.Tag\n}\n" type_suffix type_str)
  | Typecheck.Trait_registry.DeriveShow ->
      Some (Printf.sprintf "func show_show_%s(x %s) string {\n\treturn x.String()\n}\n" type_suffix type_str)
  | Typecheck.Trait_registry.DeriveDebug ->
      Some (Printf.sprintf "func debug_debug_%s(x %s) string {\n\treturn x.String()\n}\n" type_suffix type_str)
  | Typecheck.Trait_registry.DeriveOrd ->
      Some
        (Printf.sprintf
           "func ord_compare_%s(x, y %s) ordering {\n\tif x.Tag < y.Tag { return ordering_less() }\n\tif x.Tag > y.Tag { return ordering_greater() }\n\treturn ordering_equal()\n}\n"
           type_suffix type_str)
  | Typecheck.Trait_registry.DeriveHash ->
      Some (Printf.sprintf "func hash_hash_%s(x %s) int64 {\n\treturn int64(x.Tag)\n}\n" type_suffix type_str)

let emit_registry_derived_impls (state : emit_state) (program : AST.program) : string =
  let user_impls = collect_impl_defs program in
  let user_impl_set =
    List.fold_left
      (fun acc (impl : AST.impl_def) ->
        let for_type = type_expr_to_mono_type_with_impl_bindings impl.impl_type_params impl.AST.impl_for_type in
        if has_type_vars for_type then
          acc
        else
          StringPairSet.add (canonical_codegen_trait_name impl.AST.impl_trait_name, mangle_type for_type) acc)
      StringPairSet.empty user_impls
  in
  let should_emit trait_name type_suffix =
    (not (StringPairSet.mem (trait_name, type_suffix) user_impl_set))
    && not (StringPairSet.mem (trait_name, type_suffix) builtin_impl_keys)
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
                 | Types.TEnum _ -> emit_enum_derived_impl derive_kind impl.impl_for_type
                 | _ -> None)
             | None -> None
           else
             None)
  |> String.concat "\n"

let emit_inherent_methods
    (state : emit_state) (type_map : Infer.type_map) (typed_env : Infer.type_env) (program : AST.program) : string
    =
  let inherent_impls = collect_inherent_impl_defs program in
  let call_sites =
    collect_inherent_call_sites ~concrete_only:state.mono.concrete_only state.mono.call_resolution_map
      state.mono.method_type_args_map state.mono.placeholder_rewrite_map type_map program
  in

  let concrete_method_targets : (string * Types.mono_type) list =
    inherent_impls
    |> List.concat_map (fun (impl : AST.inherent_impl_def) ->
           let bindings = inherent_type_bindings impl.inherent_for_type in
           let for_type =
             annotation_exn (Annotation.type_expr_to_mono_type_with bindings impl.inherent_for_type)
           in
           let for_type = Types.canonicalize_mono_type for_type in
           if has_type_vars for_type then
             []
           else
             List.map (fun (m : AST.method_impl) -> (m.impl_method_name, for_type)) impl.inherent_methods)
  in
  let has_concrete_target (method_name : string) (for_type : Types.mono_type) : bool =
    List.exists (fun (m, t) -> m = method_name && t = for_type) concrete_method_targets
  in

  let emitted_targets : (string * Types.mono_type * Types.mono_type list) list ref = ref [] in
  let should_emit_target (method_name : string) (for_type : Types.mono_type) (mta : Types.mono_type list) : bool =
    if List.exists (fun (m, t, a) -> m = method_name && t = for_type && a = mta) !emitted_targets then
      false
    else (
      emitted_targets := (method_name, for_type, mta) :: !emitted_targets;
      true)
  in

  (* Determine which method type arg combos are needed for each method on a concrete type *)
  let method_type_arg_combos_for (method_name : string) (for_type : Types.mono_type) : Types.mono_type list list =
    let combos =
      call_sites
      |> List.filter (fun site ->
             site.call_method_name = method_name
             && site.call_receiver_type = for_type
             && site.call_method_type_args <> [])
      |> List.map (fun site -> site.call_method_type_args)
    in
    (* Deduplicate *)
    List.sort_uniq compare combos
  in

  let emit_for_impl (impl : AST.inherent_impl_def) : string list =
    let bindings = inherent_type_bindings impl.inherent_for_type in
    let for_type_pattern =
      annotation_exn (Annotation.type_expr_to_mono_type_with bindings impl.inherent_for_type)
      |> Types.canonicalize_mono_type
    in
    if not (has_type_vars for_type_pattern) then
      impl.inherent_methods
      |> List.concat_map (fun (m : AST.method_impl) ->
             let has_method_generics = m.impl_method_generics <> None && m.impl_method_generics <> Some [] in
             if has_method_generics then
               (* Emit one specialization per distinct method type args combo *)
               let combos = method_type_arg_combos_for m.impl_method_name for_type_pattern in
               combos
               |> List.filter_map (fun mta ->
                      if should_emit_target m.impl_method_name for_type_pattern mta then
                        Some
                          (emit_inherent_method state type_map typed_env bindings for_type_pattern
                             ~method_type_args:mta m)
                      else
                        None)
             else if should_emit_target m.impl_method_name for_type_pattern [] then
               [ emit_inherent_method state type_map typed_env bindings for_type_pattern ~method_type_args:[] m ]
             else
               [])
    else
      impl.inherent_methods
      |> List.concat_map (fun (m : AST.method_impl) ->
             call_sites
             |> List.filter (fun site -> site.call_method_name = m.impl_method_name)
             |> List.filter_map (fun site ->
                    if has_concrete_target m.impl_method_name site.call_receiver_type then
                      None
                    else
                      match Unify.unify for_type_pattern site.call_receiver_type with
                      | Error _ -> None
                      | Ok subst ->
                          let specialized_bindings =
                            bindings
                            |> List.map (fun (name, _ty) ->
                                   ( name,
                                     Types.canonicalize_mono_type
                                       (Types.apply_substitution subst (Types.TVar name)) ))
                          in
                          if
                            should_emit_target m.impl_method_name site.call_receiver_type
                              site.call_method_type_args
                          then
                            Some
                              (emit_inherent_method state type_map typed_env specialized_bindings
                                 site.call_receiver_type ~method_type_args:site.call_method_type_args m)
                          else
                            None))
  in

  inherent_impls |> List.concat_map emit_for_impl |> String.concat "\n"

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
          StringPairSet.add (canonical_codegen_trait_name impl.AST.impl_trait_name, mangle_type for_type) acc)
      StringPairSet.empty user_impls
  in

  (* Helper to check if an impl is user-defined *)
  let is_user_defined trait_name type_name = StringPairSet.mem (trait_name, type_name) user_impl_set in

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
        "func ord_compare_int64(x, y int64) ordering {\n\tif x < y { return ordering_less() } else if x == y { return ordering_equal() } else { return ordering_greater() }\n}"
      );
      ( ("ord", "bool"),
        "func ord_compare_bool(x, y bool) ordering {\n\tif !x && y { return ordering_less() } else if x == y { return ordering_equal() } else { return ordering_greater() }\n}"
      );
      ( ("ord", "string"),
        "func ord_compare_string(x, y string) ordering {\n\tif x < y { return ordering_less() } else if x == y { return ordering_equal() } else { return ordering_greater() }\n}"
      );
      ( ("ord", "float64"),
        "func ord_compare_float64(x, y float64) ordering {\n\tif x < y { return ordering_less() } else if x == y { return ordering_equal() } else { return ordering_greater() }\n}"
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

let emit_program_with_typed_env
    ~(call_resolution_map : (int, Infer.method_resolution) Hashtbl.t)
    ~(method_type_args_map : (int, Types.mono_type list) Hashtbl.t)
    ~(method_def_map : (int, Typecheck.Resolution_artifacts.typed_method_def) Hashtbl.t)
    ?(placeholder_rewrite_map = Hashtbl.create 0)
    (type_map : Infer.type_map)
    (typed_env : Infer.type_env)
    (program : AST.program) : string =
  let mono_state =
    create_mono_state ~call_resolution_map ~method_type_args_map ~method_def_map ~placeholder_rewrite_map ()
  in

  (* Pass 1: Collect function definitions *)
  List.iter (collect_funcs_stmt ~top_level:true mono_state) program;

  (* Pass 2: Collect instantiations using the already-typed environment and type_map *)
  ignore (List.fold_left (collect_insts_stmt mono_state type_map) typed_env program);

  (* Pass 3: Register type alias shapes for records *)
  List.iter
    (fun (stmt : AST.statement) ->
      match stmt.stmt with
      | AST.TypeAlias alias_def -> (
          let mono_type = annotation_exn (Annotation.type_expr_to_mono_type alias_def.alias_body) in
          match mono_type with
          | Types.TRecord (fields, _row) -> register_type_alias_shape mono_state alias_def.alias_name fields
          | _ -> ())
      | _ -> ())
    program;

  (* ordering is a builtin enum used by ord helpers and operator desugaring. *)
  mono_state.enum_insts <- EnumInstSet.add ("ordering", []) mono_state.enum_insts;

  let emit_state = create_emit_state mono_state in

  (* Generate specialized functions (enum types are generated AFTER all function
     body emissions so that method-generic specializations can register new
     enum instantiations like opt[String] via track_enum_inst) *)
  let specialized_funcs =
    let rec emit_pending (emitted : InstSet.t) (acc : string list) =
      let pending =
        InstSet.elements mono_state.instantiations |> List.filter (fun inst -> not (InstSet.mem inst emitted))
      in
      match pending with
      | [] -> String.concat "\n" (List.rev acc)
      | _ ->
          let emitted', acc' =
            List.fold_left
              (fun (emitted_acc, code_acc) inst ->
                let code = emit_specialized_func emit_state type_map typed_env inst in
                (InstSet.add inst emitted_acc, code :: code_acc))
              (emitted, acc) pending
          in
          emit_pending emitted' acc'
    in
    emit_pending InstSet.empty []
  in

  (* Generate builtin trait impl functions *)
  let builtin_impl_funcs = emit_builtin_impls program in

  (* Generate trait impl functions *)
  let impl_funcs =
    ImplInstSet.elements mono_state.impl_instantiations
    |> List.map (emit_cached_impl_method emit_state type_map typed_env)
    |> String.concat "\n"
  in
  let inherent_funcs = emit_inherent_methods emit_state type_map typed_env program in
  let derived_impl_funcs = emit_registry_derived_impls emit_state program in

  (* Emit main body *)
  let main_body, _ = emit_stmts emit_state type_map typed_env program in

  (* Generate enum types AFTER all function body emissions so that
     method-generic specializations can register new enum instantiations *)
  let enum_results =
    EnumInstSet.elements mono_state.enum_insts
    |> List.map (fun (name, args) -> emit_enum_type mono_state name args)
  in
  let enum_types = List.map fst enum_results |> String.concat "\n" in

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
  let inherent_funcs_str =
    if inherent_funcs = "" then
      ""
    else
      inherent_funcs ^ "\n"
  in
  let derived_impl_funcs_str =
    if derived_impl_funcs = "" then
      ""
    else
      derived_impl_funcs ^ "\n"
  in
  let top_funcs =
    specialized_funcs_str ^ builtin_impl_funcs_str ^ impl_funcs_str ^ inherent_funcs_str ^ derived_impl_funcs_str
  in

  Printf.sprintf "package main\n\n%s%s%sfunc main() {\n%s}\n" imports type_defs top_funcs main_body

let emit_program (program : AST.program) : string =
  let env = Typecheck.Builtins.prelude_env () in
  match Typecheck.Checker.check_program_with_annotations ~env program with
  | Error (err :: _) ->
      failwith
        (Printf.sprintf "Codegen error: cannot emit untyped program: %s" (Typecheck.Checker.format_error err))
  | Error [] -> failwith "Codegen error: cannot emit untyped program"
  | Ok
      {
        environment = typed_env;
        type_map;
        call_resolution_map;
        method_type_args_map;
        method_def_map;
        placeholder_rewrite_map;
        _;
      } ->
      emit_program_with_typed_env ~call_resolution_map ~method_type_args_map ~method_def_map
        ~placeholder_rewrite_map type_map typed_env program

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

let classify_go_build_failure ~(exit_code : int) ~(output : string) : Diagnostic.t =
  ignore exit_code;
  let message =
    if output = "" then
      "Go build failed"
    else
      output
  in
  let lower = String.lowercase_ascii message in
  let code =
    if
      String_utils.contains_substring ~needle:"go: command not found" lower
      || String_utils.contains_substring ~needle:"go: not found" lower
      || String_utils.contains_substring ~needle:"'go' is not recognized" lower
      || String_utils.contains_substring ~needle:"executable file not found" lower
    then
      "build-go-missing"
    else
      "build-go-compile"
  in
  Diagnostic.error_no_span ~code ~message

let normalize_codegen_failure_message (msg : string) : string =
  let prefix = "Codegen error: " in
  let prefix_len = String.length prefix in
  if String.length msg >= prefix_len && String.sub msg 0 prefix_len = prefix then
    msg
  else
    prefix ^ msg

let classify_codegen_failure_code (normalized_message : string) : string =
  let lower = String.lowercase_ascii normalized_message in
  if String_utils.contains_substring ~needle:"ambiguous function reference" lower then
    "codegen-ambiguous-fn"
  else if
    String_utils.contains_substring ~needle:"unresolved type variable" lower
    || String_utils.contains_substring ~needle:"unresolved type variables" lower
  then
    "codegen-unresolved-tvar"
  else
    "codegen-internal"

let diagnostic_of_codegen_failure_message (msg : string) : Diagnostic.t =
  let normalized_message = normalize_codegen_failure_message msg in
  Diagnostic.error_no_span ~code:(classify_codegen_failure_code normalized_message) ~message:normalized_message

let compile_string ~file_id (source : string) : (string * Diagnostic.t list, Diagnostic.t list) result =
  match Syntax.Parser.parse ~file_id source with
  | Error errors -> Error errors
  | Ok program -> (
      let env = Typecheck.Builtins.prelude_env () in
      match Typecheck.Checker.check_program_with_annotations ~env program with
      | Error errs -> Error errs
      | Ok
          {
            environment = typed_env;
            type_map;
            call_resolution_map;
            method_type_args_map;
            method_def_map;
            placeholder_rewrite_map;
            diagnostics;
            _;
          } -> (
          try
            Ok
              ( emit_program_with_typed_env ~call_resolution_map ~method_type_args_map ~method_def_map
                  ~placeholder_rewrite_map type_map typed_env program,
                diagnostics )
          with
          | Failure msg -> Error [ diagnostic_of_codegen_failure_message msg ]
          | exn ->
              let msg = normalize_codegen_failure_message (Printexc.to_string exn) in
              Error [ Diagnostic.error_no_span ~code:"codegen-internal" ~message:msg ]))

type build_output = {
  main_go : string;
  runtime_go : string;
  diagnostics : Diagnostic.t list;
}

let compile_to_build ~file_id (source : string) : (build_output, Diagnostic.t list) result =
  match compile_string ~file_id source with
  | Error e -> Error e
  | Ok (main_go, diagnostics) -> Ok { main_go; runtime_go; diagnostics }

let get_runtime () = runtime_go

(* ============================================================
   Tests
   ============================================================ *)

let string_contains s substring = String_utils.contains_substring ~needle:substring s
let string_not_contains s substring = not (string_contains s substring)

let is_deterministic source =
  let build () =
    Fun.protect
      ~finally:(fun () ->
        Typecheck.Trait_registry.clear ();
        Typecheck.Builtins.init_builtin_impls ())
      (fun () -> compile_to_build ~file_id:"<codegen>" source)
  in
  match (build (), build ()) with
  | Ok a, Ok b -> a.main_go = b.main_go && a.runtime_go = b.runtime_go
  | _ -> false

let capture_stderr f =
  let original_stderr = Unix.dup Unix.stderr in
  let read_fd, write_fd = Unix.pipe () in
  Unix.dup2 write_fd Unix.stderr;
  Unix.close write_fd;
  let result = try Ok (f ()) with exn -> Error exn in
  flush stderr;
  Unix.dup2 original_stderr Unix.stderr;
  Unix.close original_stderr;

  let buffer = Buffer.create 256 in
  let chunk = Bytes.create 4096 in
  let rec drain_pipe () =
    match Unix.read read_fd chunk 0 (Bytes.length chunk) with
    | 0 -> ()
    | n ->
        Buffer.add_string buffer (Bytes.sub_string chunk 0 n);
        drain_pipe ()
  in
  drain_pipe ();
  Unix.close read_fd;

  let stderr_output = Buffer.contents buffer in
  match result with
  | Ok value -> (value, stderr_output)
  | Error exn -> raise exn

let%test "emit integer" =
  match compile_string ~file_id:"<codegen>" "42" with
  | Ok (code, _) -> string_contains code "int64(42)"
  | Error _ -> false

let%test "emit addition" =
  match compile_string ~file_id:"<codegen>" "1 + 2" with
  | Ok (code, _) -> string_contains code "(int64(1) + int64(2))"
  | Error _ -> false

let%test "emit let binding" =
  match compile_string ~file_id:"<codegen>" "let x = 5; x" with
  | Ok (code, _) -> string_contains code "x := int64(5)" && string_contains code "_ = x"
  | Error _ -> false

let%test "emit boolean" =
  match compile_string ~file_id:"<codegen>" "true" with
  | Ok (code, _) -> string_contains code "_ = true"
  | Error _ -> false

let%test "emit string" =
  match compile_string ~file_id:"<codegen>" "\"hello\"" with
  | Ok (code, _) -> string_contains code "\"hello\""
  | Error _ -> false

let%test "emit comparison" =
  match compile_string ~file_id:"<codegen>" "1 < 2" with
  | Ok (code, _) -> string_contains code "(int64(1) < int64(2))"
  | Error _ -> false

let%test "emit bool ordering via ord helper" =
  match compile_string ~file_id:"<codegen>" "true < false" with
  | Ok (code, _) -> string_contains code "ord_compare_bool(true, false)"
  | Error _ -> false

let%test "emit non-primitive equality via eq helper" =
  match
    compile_string ~file_id:"<codegen>"
      "type Point = { x: Int }\nimpl Eq[Point] = {\n  fn eq(x: Point, y: Point) -> Bool = false\n}\nlet a: Point = { x: 1 }\nlet b: Point = { x: 1 }\na == b"
  with
  | Ok (code, _) -> string_contains code "func eq_eq_" && string_contains code "return false"
  | Error _ -> false

let%test "monomorphized function" =
  match compile_string ~file_id:"<codegen>" "fn double(x: Int) -> Int = x * 2\ndouble(5)" with
  | Ok (code, _) ->
      (* Should have top-level func double_int64 *)
      string_contains code "func double_int64(x int64) int64"
      &&
      (* Call should use mangled name *)
      string_contains code "double_int64(int64(5))"
  | Error _ -> false

let%test "placeholder shorthand callback compiles through codegen" =
  Typecheck.Trait_registry.clear ();
  Typecheck.Enum_registry.clear ();
  Typecheck.Inherent_registry.clear ();
  match
    compile_string ~file_id:"<codegen>"
      "fn apply[a, b](x: a, f: (a) -> b) -> b = f(x)\n\
       let result = apply(21, _ * 2)\n\
       result"
  with
  | Ok (_, _) -> true
  | Error _ -> false

let%test "identifier suffix methods compile through codegen" =
  Typecheck.Trait_registry.clear ();
  Typecheck.Enum_registry.clear ();
  Typecheck.Inherent_registry.clear ();
  match
    compile_string ~file_id:"<codegen>"
      "type Monkey = { bananas_eaten: Int, is_alpha: Bool }\n\
       impl Monkey = {\n\
         fn hungry?(self: Monkey) -> Bool = self.bananas_eaten < 3\n\
         fn alpha!(self: Monkey) -> Monkey = { ...self, is_alpha: true }\n\
       }\n\
       let m: Monkey = { bananas_eaten: 1, is_alpha: false }\n\
       puts(m.hungry?())\n\
       puts(m.alpha!().is_alpha)"
  with
  | Ok (code, _) ->
      string_contains code "func inherent_hungry_q_record_bananas_eaten_int64_is_alpha_bool_closed("
      && string_contains code "func inherent_alpha_bang_record_bananas_eaten_int64_is_alpha_bool_closed("
      && string_contains code "inherent_hungry_q_record_bananas_eaten_int64_is_alpha_bool_closed(m)"
      && string_contains code "inherent_alpha_bang_record_bananas_eaten_int64_is_alpha_bool_closed(m)"
  | Error _ -> false

let%test "generic empty array return uses function return type during build" =
  Typecheck.Trait_registry.clear ();
  Typecheck.Enum_registry.clear ();
  Typecheck.Inherent_registry.clear ();
  match
    compile_to_build ~file_id:"<codegen>"
      "fn wrap_empty[a, b](x: a, sample: b) -> List[b] = []\n\
       let ints = wrap_empty(true, 1)\n\
       let strs = wrap_empty(true, \"banana\")\n\
       puts(len(ints))\n\
       puts(len(strs))"
  with
  | Ok out ->
      string_contains out.main_go "return []int64{}" && string_contains out.main_go "return []string{}"
  | Error _ -> false

let%test "polymorphic function multiple instantiations" =
  match compile_string ~file_id:"<codegen>" "fn id[a](x: a) -> a = x\nid(5)\nid(true)" with
  | Ok (code, _) ->
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
  match compile_string ~file_id:"<codegen>" "let a = [1,2,3]; a[0]" with
  | Ok (code, _) -> string_contains code "a[0]" && not (string_contains code "int(")
  | Error _ -> false

let%test "emit array index with variable" =
  match compile_string ~file_id:"<codegen>" "let a = [1,2,3]; let i = 1; a[i]" with
  | Ok (code, _) -> string_contains code "indexArr(a, i)"
  | Error _ -> false

let%test "emit if inside function body" =
  match compile_string ~file_id:"<codegen>" "fn abs(x: Int) -> Int = { if (x < 0) { -x } else { x } }\nabs(5)" with
  | Ok (code, _) ->
      string_contains code "func abs_int64(x int64) int64" && string_contains code "if (x < int64(0))"
  | Error _ -> false

let%test "emit recursive function" =
  match
    compile_string ~file_id:"<codegen>"
      "fn fact(n: Int) -> Int = { if (n < 2) { 1 } else { n * fact(n - 1) } }\nfact(5)"
  with
  | Ok (code, _) ->
      string_contains code "func fact_int64(n int64) int64" && string_contains code "fact_int64((n - int64(1)))"
  | Error _ -> false

let%test "emit closure (function returning function)" =
  match
    compile_string ~file_id:"<codegen>"
      "fn make_adder(n: Int) = (x: Int) -> n + x\nlet add10 = make_adder(10)\nadd10(5)"
  with
  | Ok (code, _) ->
      string_contains code "func make_adder_int64(n int64) func(int64) int64"
      && string_contains code "return func("
  | Error _ -> false

let%test "emit string indexing returns string" =
  match compile_string ~file_id:"<codegen>" "let s = \"hello\"; s[0]" with
  | Ok (code, _) -> string_contains code "string(s[0])"
  | Error _ -> false

let%test "emit negative array index" =
  match compile_string ~file_id:"<codegen>" "let a = [1,2,3]; a[-1]" with
  | Ok (code, _) -> string_contains code "a[len(a)-1]"
  | Error _ -> false

let%test "emit negative string index" =
  match compile_string ~file_id:"<codegen>" "let s = \"hello\"; s[-2]" with
  | Ok (code, _) -> string_contains code "s[len(s)-2]"
  | Error _ -> false

let%test "underscore let binding emits discard assignment" =
  match compile_string ~file_id:"<codegen>" "fn f(x: Int) -> Int = { let _ = x; x }\nf(1)" with
  | Ok (code, _) -> string_contains code "_ = x" && not (string_contains code "_ := x")
  | Error _ -> false

let%test "if statement without else in function followed by return" =
  match
    compile_string ~file_id:"<codegen>" "fn fib(n: Int) -> Int = { if (n < 2) { return n }; return n + 1 }\nfib(5)"
  with
  | Ok (code, _) ->
      (* Should emit as regular if statement, not wrapped in function *)
      string_contains code "if (n < int64(2))"
      && (not (string_contains code "func() int64"))
      && string_contains code "return n"
  | Error _ -> false

let%test "if statement with else in function followed by return" =
  match
    compile_string ~file_id:"<codegen>"
      "fn test(x: Int) -> Int = { if (x > 0) { return 1 } else { return -1 }; return 0 }\ntest(5)"
  with
  | Ok (code, _) ->
      (* Should compile without nil errors *)
      string_contains code "if (x > int64(0))" && string_contains code "} else {"
  | Error _ -> false

let%test "fibonacci with if statement and subsequent return" =
  match
    compile_string ~file_id:"<codegen>"
      "fn fib(n: Int) -> Int = { if (n < 2) { return n }; return fib(n - 1) + fib(n - 2) }\nfib(10)"
  with
  | Ok (code, _) ->
      string_contains code "func fib_int64(n int64) int64"
      && string_contains code "if (n < int64(2))"
      && not (string_contains code "return nil")
  | Error _ -> false

let%test "emit record literal and field access" =
  match compile_string ~file_id:"<codegen>" "let p = { x: 10, y: 20 }; p.x + p.y" with
  | Ok (code, _) ->
      (* Record literals should use named shape types, not inline structs *)
      string_contains code "Record_x_int64_y_int64"
      && string_contains code "(p).x"
      && string_contains code "(p).y"
  | Error _ -> false

let%test "record shape type definition is emitted" =
  match compile_string ~file_id:"<codegen>" "let p = { x: 10, y: 20 }; p.x" with
  | Ok (code, _) ->
      (* A top-level type definition should exist for the record shape *)
      string_contains code "type Record_x_int64_y_int64 struct"
  | Error _ -> false

let%test "same record shape reuses type name" =
  match compile_string ~file_id:"<codegen>" "let p = { x: 1, y: 2 }; let q = { x: 10, y: 20 }; p.x + q.y" with
  | Ok (code, _) ->
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
  match compile_string ~file_id:"<codegen>" "let p = { x: 1, y: 2 }; let q = { a: 10, b: 20 }; p.x + q.a" with
  | Ok (code, _) ->
      string_contains code "type Record_x_int64_y_int64 struct"
      && string_contains code "type Record_a_int64_b_int64 struct"
  | Error _ -> false

let%test "record field ordering is canonical in shape name" =
  match compile_string ~file_id:"<codegen>" "let p = { y: 2, x: 1 }; p.x" with
  | Ok (code, _) ->
      (* Fields sorted alphabetically: x before y *)
      string_contains code "Record_x_int64_y_int64"
  | Error _ -> false

let%test "function returning wrapped scalar record compiles" =
  match compile_string ~file_id:"<codegen>" "fn mk(i: Int) -> { inner: Int } = { inner: i }\nlet o = mk(1)\no.inner" with
  | Ok (code, _) ->
      string_contains code "func mk_int64(i int64)" && string_contains code "type Record_inner_int64 struct"
  | Error _ -> false

let%test "function returning wrapped record compiles" =
  match
    compile_string ~file_id:"<codegen>"
      "fn mk(i: { x: Int }) -> { inner: { x: Int } } = { inner: i }\nlet i = { x: 1 }\nlet o = mk(i)\no.inner.x"
  with
  | Ok (code, _) ->
      string_contains code "type Record_x_int64 struct"
      && string_contains code "type Record_inner_"
      && string_contains code "func mk_record_x_int64_closed("
  | Error _ -> false

let%test "function wrapping inline record literal argument compiles" =
  match
    compile_string ~file_id:"<codegen>"
      "fn mk(r: { x: Int }) -> { inner: { x: Int } } = { inner: r }\nlet o = mk({ x: 3 })\no.inner.x"
  with
  | Ok (code, _) ->
      string_contains code "func mk_record_x_int64_closed(" && string_contains code "type Record_inner_"
  | Error _ -> false

let%test "duplicate record fields use last write" =
  match compile_string ~file_id:"<codegen>" "let p = { x: 1, x: 2 }; p.x" with
  | Ok (code, _) -> string_contains code "Record_x_int64{x: int64(2)}"
  | Error _ -> false

let%test "emit record spread" =
  match compile_string ~file_id:"<codegen>" "let p = { x: 1, y: 2 }; let p2 = { ...p, x: 10 }; p2.x + p2.y" with
  | Ok (code, _) -> string_contains code "__spread_0 := p" && string_contains code "Record_x_int64_y_int64"
  | Error _ -> false

let%test "record spread with full field override avoids unused temp" =
  match compile_string ~file_id:"<codegen>" "let p = { x: 1, X: 2 }; let q = { ...p, x: 4, X: 6 }; q.x + q.X" with
  | Ok (code, _) -> not (string_contains code "__spread :=")
  | Error _ -> false

let%test "emit record match pattern" =
  match
    compile_string ~file_id:"<codegen>"
      "let p = { x: 10, y: 20 }\nmatch p {\n  case { x:, y: }: x + y\n  case _: 0\n}"
  with
  | Ok (code, _) ->
      string_contains code "__scrutinee_0 :="
      && string_contains code "if true"
      && string_contains code "x := __scrutinee_0.x"
  | Error _ -> false

let%test "type alias emits named Go type" =
  match
    compile_string ~file_id:"<codegen>" "type Point = { x: Int, y: Int }\nlet p: Point = { x: 1, y: 2 }\np.x"
  with
  | Ok (code, _) ->
      (* Should emit "type Point struct{...}" using alias name, not Record_... *)
      string_contains code "type Point struct"
  | Error _ -> false

let%test "type alias used in record literal" =
  match
    compile_string ~file_id:"<codegen>" "type Point = { x: Int, y: Int }\nlet p: Point = { x: 1, y: 2 }\np.x"
  with
  | Ok (code, _) ->
      (* Record literal should use alias name *)
      string_contains code "Point{x:"
  | Error _ -> false

let%test "case-distinct record fields compile to distinct Go fields" =
  match compile_string ~file_id:"<codegen>" "let p = { x: 1, X: 2 }; p.x + p.X" with
  | Ok (code, _) -> string_contains code "type Record_X_int64_x_int64 struct{X int64; x int64}"
  | Error _ -> false

let%test "type alias replaces generated shape name" =
  match
    compile_string ~file_id:"<codegen>" "type Point = { x: Int, y: Int }\nlet p: Point = { x: 1, y: 2 }\np.x"
  with
  | Ok (code, _) ->
      (* Should NOT emit Record_x_int64_y_int64 when alias exists *)
      not (string_contains code "Record_x_int64_y_int64")
  | Error _ -> false

let%test "emitter method calls use typechecker resolution metadata" =
  let source =
    {|
trait Ping[a] = {
  fn ping(x: a) -> Int
}
impl Ping[Int] = {
  fn ping(x: Int) -> Int = x
}
let v = 1
v.ping()
|}
  in
  match Syntax.Parser.parse ~file_id:"<codegen>" source with
  | Error _ -> false
  | Ok program -> (
      let env = Typecheck.Builtins.prelude_env () in
          match Typecheck.Checker.check_program_with_annotations ~env program with
          | Error _ -> false
          | Ok { environment = typed_env; type_map; call_resolution_map; method_type_args_map; method_def_map; _ } -> (
              (* If emitter re-resolves methods from the registry, this clear would break codegen. *)
              Typecheck.Trait_registry.clear ();
              match
                try
                  Some
                    (emit_program_with_typed_env ~call_resolution_map ~method_type_args_map ~method_def_map type_map
                       typed_env program)
                with _ -> None
              with
              | Some code -> string_contains code "func Ping_ping_int64("
          | None -> false))

let%test "emitter lowers inherent methods to inherent_* helpers" =
  match
    compile_string ~file_id:"<codegen>" "impl Int = { fn double(x: Int) -> Int = x + x }\nlet v = 2\nv.double()"
  with
  | Ok (code, _) ->
      string_contains code "func inherent_double_int64(" && string_contains code "inherent_double_int64(v)"
  | Error _ -> false

let%test "emitter inherent method calls use typechecker resolution metadata" =
  let source = {|
impl Int = {
  fn ping(x: Int) -> Int = x
}
let v = 1
v.ping()
|} in
  match Syntax.Parser.parse ~file_id:"<codegen>" source with
  | Error _ -> false
  | Ok program -> (
      match Typecheck.Checker.check_program_with_annotations program with
      | Error _ -> false
      | Ok { environment = typed_env; type_map; call_resolution_map; method_type_args_map; method_def_map; _ } -> (
          Typecheck.Inherent_registry.clear ();
          match
            try
              Some
                (emit_program_with_typed_env ~call_resolution_map ~method_type_args_map ~method_def_map type_map
                   typed_env program)
            with _ -> None
          with
          | Some code -> string_contains code "inherent_ping_int64"
          | None -> false))

let%test "canonical builtin names in concrete inherent targets do not get treated as generic binders" =
  let source =
    {|
enum Result[a, b] = {
  Success(a),
  Failure(b),
}
impl Result[a, b] = {
  fn tag(r: Result[a, b]) -> Str = {
    match r {
      case Result.Success(_): "success"
      case Result.Failure(_): "failure"
    }
  }
}
impl Result[Int, Str] = {
  fn tag(r: Result[Int, Str]) -> Str = "concrete"
}
let x: Result[Int, Str] = Result.Success(1)
x.tag()
|}
  in
  match compile_string ~file_id:"<codegen>" source with
  | Ok (code, _) ->
      string_contains code "func inherent_tag_Result_int64_string("
      && string_contains code "return \"concrete\""
  | Error _ -> false

(* ============================================================
   Phase 2: IIFE Removal Tests
   ============================================================ *)

let%test "if-expression in let binding emits without IIFE" =
  match compile_string ~file_id:"<codegen>" "let x = if (true) { 1 } else { 2 }; x" with
  | Ok (code, _) ->
      (* Should NOT contain func() - no IIFE *)
      (not (string_contains code "func()"))
      (* Should contain var x int64 pattern *)
      && string_contains code "var "
  | Error _ -> false

let%test "if-expression in tail position emits without IIFE" =
  match compile_string ~file_id:"<codegen>" "fn f(x: Int) -> Int = { if (x > 0) { x } else { -x } }\nf(5)" with
  | Ok (code, _) ->
      (* Inside the function body, should NOT wrap in func() *)
      (* Should have direct if with return *)
      string_contains code "if (x > int64(0))" && not (string_contains code "func() int64")
  | Error _ -> false

let%test "match in let binding emits without IIFE" =
  match compile_string ~file_id:"<codegen>" "let x = 5\nlet y = match x { case 1: 10 case _: 20 }\ny" with
  | Ok (code, _) ->
      (* Should NOT contain func() for the match *)
      (not (string_contains code "func()")) && string_contains code "switch"
  | Error _ -> false

let%test "match in tail position emits without IIFE" =
  match compile_string ~file_id:"<codegen>" "fn f(x: Int) -> Int = { match x { case 1: 10 case 2: 20 case _: 30 } }\nf(5)" with
  | Ok (code, _) -> (not (string_contains code "func() int64")) && string_contains code "switch"
  | Error _ -> false

let%test "if-expression in function argument still uses IIFE" =
  match compile_string ~file_id:"<codegen>" "puts(if (true) { 1 } else { 2 })" with
  | Ok (code, _) ->
      (* IIFE still needed when if is a function argument *)
      string_contains code "func()" && string_contains code "if true"
  | Error _ -> false

let%test "type-check if in let binding emits without IIFE" =
  match
    compile_string ~file_id:"<codegen>" "let x: Int | Str = 1\nlet y = if (x is Int) { x + 1 } else { 0 }\ny"
  with
  | Ok (code, _) ->
      (not (string_contains code "func() int64")) && string_contains code "switch x_typed := x.(type)"
  | Error _ -> false

let%test "type-check if in tail position emits without IIFE" =
  match
    compile_string ~file_id:"<codegen>"
      "fn f(x: Int | Str) -> Int = { if (x is Int) { x + 1 } else { 0 } }\nf(1)"
  with
  | Ok (code, _) ->
      (not (string_contains code "func() int64")) && string_contains code "switch x_typed := x.(type)"
  | Error _ -> false

let%test "type-check if in function argument still uses IIFE" =
  match compile_string ~file_id:"<codegen>" "let x: Int | Str = 1\nputs(if (x is Int) { x } else { 0 })" with
  | Ok (code, _) -> string_contains code "func()" && string_contains code "switch x_typed := x.(type)"
  | Error _ -> false

let%test "if without else in tail position emits unit return without IIFE" =
  match compile_string ~file_id:"<codegen>" "fn f(x: Bool) -> Unit = { if (x) { 1 } }\nf(true)" with
  | Ok (code, _) ->
      (not (string_contains code "func() struct{}"))
      && string_contains code "if x"
      && string_contains code "return struct{}{}"
  | Error _ -> false

let%test "type-check if without else in let binding emits unit assignment" =
  match compile_string ~file_id:"<codegen>" "let x: Int | Str = 1\nlet y = if (x is Int) { x + 1 }\ny" with
  | Ok (code, _) ->
      (not (string_contains code "func()"))
      && string_contains code "switch x_typed := x.(type)"
      && string_contains code "y = struct{}{}"
  | Error _ -> false

let%test "type-check statement if preserves explicit return without IIFE" =
  match
    compile_string ~file_id:"<codegen>"
      "fn f(x: Int | Str) -> Int = { if (x is Int) { return 1 }; return 0 }\nf(1)"
  with
  | Ok (code, _) ->
      (not (string_contains code "func() int64"))
      && string_contains code "switch x_typed := x.(type)"
      && string_contains code "return int64(1)"
  | Error _ -> false

let%test "match expression statement emits without IIFE" =
  match compile_string ~file_id:"<codegen>" "match 1 { case 1: 10 case _: 20 }" with
  | Ok (code, _) -> (not (string_contains code "func() int64")) && string_contains code "switch __scrutinee"
  | Error _ -> false

let%test "nested if in tail position emits without IIFE" =
  match
    compile_string ~file_id:"<codegen>"
      "fn f(a: Bool, b: Bool) -> Int = { if (a) { if (b) { 1 } else { 2 } } else { 3 } }\nf(true, false)"
  with
  | Ok (code, _) -> not (string_contains code "func() int64")
  | Error _ -> false

let%test "nested match in tail position emits without IIFE" =
  match
    compile_string ~file_id:"<codegen>"
      "fn f(x: Int) -> Int = { match x { case 0: match x { case 0: 10 case _: 20 } case _: 30 } }\nf(0)"
  with
  | Ok (code, _) -> not (string_contains code "func() int64")
  | Error _ -> false

let%test "if branch containing match in let binding emits without IIFE" =
  match
    compile_string ~file_id:"<codegen>"
      "let x = 1\nlet y = if (x == 1) { match x { case 1: 10 case _: 20 } } else { 0 }\ny"
  with
  | Ok (code, _) -> not (string_contains code "func() int64")
  | Error _ -> false

let%test "match statement with nested if emits without IIFE" =
  match compile_string ~file_id:"<codegen>" "match 1 { case 1: if (true) { 10 } else { 20 } case _: 0 }" with
  | Ok (code, _) -> not (string_contains code "func() int64")
  | Error _ -> false

let%test "record spread emits without IIFE" =
  match compile_string ~file_id:"<codegen>" "let p = { x: 1, y: 2 }; let p2 = { ...p, x: 10 }; p2" with
  | Ok (code, _) ->
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

let%test "has_type_vars detects standalone TRowVar" = has_type_vars (Types.TRowVar "r")

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
        method_type_args = [];
        type_fingerprint = fingerprint_types [ for_type ];
      }
    in
    let payload : impl_inst_payload =
      {
        param_names = [ "x" ];
        param_types = [ Types.TInt ];
        return_type = Types.TString;
        body_stmt = AST.mk_stmt (AST.ExpressionStmt (AST.mk_expr (AST.String "x")));
        specialization_subst = Types.empty_substitution;
      }
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
    compile_string ~file_id:"<codegen>"
      "fn f(x: Int) -> Int = x + 1\nfn f(x: Int) -> Int = x + 2\nlet y = f(1)\nputs(y)"
  with
  | Ok _ | Error [] -> false
  | Error (diag :: _) ->
      string_contains diag.message "ambiguous function reference 'f/1'"
      || string_contains diag.message "Duplicate top-level let definition: f"

let%test "codegen diagnostic classifier tags unresolved-type-variable failures" =
  let diag = diagnostic_of_codegen_failure_message "Cannot generate code for unresolved type variable." in
  diag.code = "codegen-unresolved-tvar" && string_contains diag.message "Codegen error:"

let%test "codegen diagnostic classifier tags ambiguous-function failures" =
  let diag =
    diagnostic_of_codegen_failure_message
      "Codegen error: ambiguous function reference 'f/1': multiple function definitions share this name/arity"
  in
  diag.code = "codegen-ambiguous-fn"

let%test "compile_string returns structured parser diagnostic on parse failure" =
  match compile_string ~file_id:"<codegen>" "let x = " with
  | Ok _ -> false
  | Error (diag :: _) -> string_contains diag.code "parse-"
  | Error [] -> false

let%test "compile_string preserves checker diagnostic on type failure" =
  match compile_string ~file_id:"<codegen>" "let x: Int = true\nx" with
  | Ok _ -> false
  | Error (diag :: _) -> string_contains diag.code "type-"
  | Error [] -> false

let%test "collect_insts registers impl methods in cache" =
  let source =
    {|
impl Show[Int] = {
  fn show(self: Int) -> Str = "int!"
}
puts(1.show())
|}
  in
  Fun.protect
    ~finally:(fun () ->
      Typecheck.Trait_registry.clear ();
      Typecheck.Builtins.init_builtin_impls ())
    (fun () ->
      match Syntax.Parser.parse ~file_id:"<codegen>" source with
      | Error _ -> false
      | Ok program -> (
          let env = Typecheck.Builtins.prelude_env () in
          match Typecheck.Checker.check_program_with_annotations ~env program with
          | Error _ -> false
          | Ok { environment = typed_env; type_map; call_resolution_map; method_type_args_map; method_def_map; _ } ->
              let mono_state = create_mono_state ~call_resolution_map ~method_type_args_map ~method_def_map () in
              List.iter (collect_funcs_stmt mono_state) program;
              ignore (List.fold_left (collect_insts_stmt mono_state type_map) typed_env program);
              ImplInstSet.cardinal mono_state.impl_instantiations = 1
              && Hashtbl.length mono_state.impl_inst_payloads = 1))

let%test "canonical builtin trait impl header emits canonical helper name" =
  let source =
    {|impl Show[Int] = { fn show(self: Int) -> Str = "int!" }
puts(1.show())|}
  in
  Fun.protect
    ~finally:(fun () ->
      Typecheck.Trait_registry.clear ();
      Typecheck.Builtins.init_builtin_impls ())
    (fun () ->
      Typecheck.Trait_registry.clear ();
      match compile_string ~file_id:"<codegen>" source with
      | Ok (code, _) ->
          string_contains code "func show_show_int64("
          && string_contains code {|return "int!"|}
          && string_not_contains code "func Show_show_int64("
      | Error _ -> false)

let%test "canonical generic builtin impl emits specialized helper name" =
  let source =
    {|impl[a: Show] Show[List[a]] = { fn show(self: List[a]) -> Str = "list" }
puts([1, 2].show())|}
  in
  Fun.protect
    ~finally:(fun () ->
      Typecheck.Trait_registry.clear ();
      Typecheck.Builtins.init_builtin_impls ())
    (fun () ->
      Typecheck.Trait_registry.clear ();
      match compile_string ~file_id:"<codegen>" source with
      | Ok (code, _) ->
          string_contains code "func show_show_arr_int64("
          && string_not_contains code "func Show_show_arr_int64("
      | Error _ -> false)

let%test "trait impl methods are emitted exactly once" =
  (* Builtin show_show_int64 should appear exactly once as a function definition *)
  match compile_string ~file_id:"<codegen>" "puts(42)" with
  | Ok (code, _) ->
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

(* ============================================================
   Migrated Go-inspection tests (from shell harness)
   ============================================================ *)

(* --- run_emit_go_not_contains_from_stdin: IIFE avoidance --- *)

let%test "type-check if in let binding avoids IIFE" =
  match
    compile_string ~file_id:"<codegen>"
      {|let x: Int | Str = 1
let y = if (x is Int) { x + 1 } else { 0 }
puts(y)|}
  with
  | Ok (code, _) -> string_not_contains code "func() int64"
  | Error _ -> false

let%test "type-check if in tail position avoids IIFE" =
  match
    compile_string ~file_id:"<codegen>"
      {|fn f(x: Int | Str) -> Int = {
  if (x is Int) { x + 1 } else { 0 }
}
puts(f(1))|}
  with
  | Ok (code, _) -> string_not_contains code "func() int64"
  | Error _ -> false

let%test "match statement avoids IIFE" =
  match compile_string ~file_id:"<codegen>" {|match 1 {
  case 1: 10
  case _: 20
}
puts(1)|} with
  | Ok (code, _) -> string_not_contains code "func() int64"
  | Error _ -> false

let%test "tail if without else avoids IIFE" =
  match compile_string ~file_id:"<codegen>" {|fn f(x: Bool) -> Unit = { if (x) { 1 } }
puts(f(true))|} with
  | Ok (code, _) -> string_not_contains code "func() struct{}"
  | Error _ -> false

let%test "nested if in tail position avoids IIFE" =
  match
    compile_string ~file_id:"<codegen>"
      {|fn f(a: Bool, b: Bool) -> Int = {
  if (a) { if (b) { 1 } else { 2 } } else { 3 }
}
puts(f(true, false))|}
  with
  | Ok (code, _) -> string_not_contains code "func() int64"
  | Error _ -> false

let%test "nested match in tail position avoids IIFE" =
  match
    compile_string ~file_id:"<codegen>"
      {|fn f(x: Int) -> Int = {
  match x { case 0: match x { case 0: 10 case _: 20 } case _: 30 }
}
puts(f(0))|}
  with
  | Ok (code, _) -> string_not_contains code "func() int64"
  | Error _ -> false

let%test "match with nested if avoids IIFE" =
  match compile_string ~file_id:"<codegen>" {|match 1 {
  case 1: if (true) { 10 } else { 20 }
  case _: 0
}
puts(1)|} with
  | Ok (code, _) -> string_not_contains code "func() int64"
  | Error _ -> false

let%test "type-check if branch with nested match avoids IIFE" =
  match
    compile_string ~file_id:"<codegen>"
      {|let x: Int | Str = 1
let y = if (x is Int) {
  match x { case 1: 10 case _: 20 }
} else {
  0
}
puts(y)|}
  with
  | Ok (code, _) -> string_not_contains code "func() int64"
  | Error _ -> false

let%test "nested if containing type-check if avoids IIFE" =
  match
    compile_string ~file_id:"<codegen>"
      {|fn f(x: Int | Str, y: Bool) -> Int = {
  if (y) {
    if (x is Int) { x } else { 0 }
  } else {
    0
  }
}
puts(f(1, true))|}
  with
  | Ok (code, _) -> string_not_contains code "func() int64"
  | Error _ -> false

let%test "match in let binding with type-check if arm avoids IIFE" =
  match
    compile_string ~file_id:"<codegen>"
      {|let x: Int | Str = 1
let tag = 1
let y = match tag {
  case 1: if (x is Int) { x + 1 } else { 0 }
  case _: 0
}
puts(y)|}
  with
  | Ok (code, _) -> string_not_contains code "func() int64"
  | Error _ -> false

let%test "deeply nested if (3 levels) in let binding avoids IIFE" =
  match
    compile_string ~file_id:"<codegen>"
      {|let a = true
let b = false
let c = true
let x = if (a) { if (b) { if (c) { 1 } else { 2 } } else { 3 } } else { 4 }
puts(x)|}
  with
  | Ok (code, _) -> string_not_contains code "func() int64"
  | Error _ -> false

let%test "deeply nested if (3 levels) in tail position avoids IIFE" =
  match
    compile_string ~file_id:"<codegen>"
      {|fn f(a: Bool, b: Bool, c: Bool) -> Int = {
  if (a) { if (b) { if (c) { 1 } else { 2 } } else { 3 } } else { 4 }
}
puts(f(true, false, true))|}
  with
  | Ok (code, _) -> string_not_contains code "func() int64"
  | Error _ -> false

let%test "match on int in let binding avoids IIFE" =
  match compile_string ~file_id:"<codegen>" {|let x = 2
let y = match x {
  case 1: 10
  case 2: 42
  case _: 0
}
puts(y)|} with
  | Ok (code, _) -> string_not_contains code "func() int64"
  | Error _ -> false

let%test "match on int in tail position avoids IIFE" =
  match
    compile_string ~file_id:"<codegen>"
      {|fn choose(x: Int) -> Int = {
  match x {
    case 1: 10
    case 2: 42
    case _: 0
  }
}
puts(choose(2))|}
  with
  | Ok (code, _) -> string_not_contains code "func() int64"
  | Error _ -> false

let%test "match nested inside if-then branch avoids IIFE" =
  match
    compile_string ~file_id:"<codegen>"
      {|fn f(flag: Bool, x: Int) -> Int = {
  if (flag) {
    match x {
      case 1: 10
      case 2: 20
      case _: 30
    }
  } else {
    0
  }
}
puts(f(true, 2))|}
  with
  | Ok (code, _) -> string_not_contains code "func() int64"
  | Error _ -> false

let%test "if inside match arm body avoids IIFE" =
  match
    compile_string ~file_id:"<codegen>"
      {|fn f(x: Int, flag: Bool) -> Int = {
  match x {
    case 1: if (flag) { 99 } else { 0 }
    case _: 50
  }
}
puts(f(1, true))|}
  with
  | Ok (code, _) -> string_not_contains code "func() int64"
  | Error _ -> false

let%test "three chained let bindings each using if avoid IIFE" =
  match
    compile_string ~file_id:"<codegen>"
      {|let a = if (true) { 1 } else { 0 }
let b = if (true) { a + 2 } else { 0 }
let c = if (true) { b + 3 } else { 0 }
puts(c)|}
  with
  | Ok (code, _) -> string_not_contains code "func() int64"
  | Error _ -> false

let%test "enum match in let binding avoids IIFE" =
  match
    compile_string ~file_id:"<codegen>"
      {|enum Option[a] = {
  Some(a),
  None,
}
let value = Option.Some(42)
let result = match value {
  case Option.Some(v): v
  case Option.None: 0
}
puts(result)|}
  with
  | Ok (code, _) -> string_not_contains code "func()"
  | Error _ -> false

let%test "match nested inside match arm avoids IIFE" =
  match
    compile_string ~file_id:"<codegen>"
      {|fn f(x: Int, y: Int) -> Int = {
  match x {
    case 1: match y {
      case 1: 11
      case 2: 12
      case _: 19
    }
    case 2: match y {
      case 1: 21
      case _: 99
    }
    case _: 0
  }
}
puts(f(2, 3))|}
  with
  | Ok (code, _) -> string_not_contains code "func() int64"
  | Error _ -> false

let%test "deeply nested if (4 levels) in let binding avoids IIFE" =
  match
    compile_string ~file_id:"<codegen>"
      {|let a = true
let b = true
let c = false
let d = true
let x = if (a) { if (b) { if (c) { if (d) { 1 } else { 2 } } else { 4 } } else { 5 } } else { 6 }
puts(x)|}
  with
  | Ok (code, _) -> string_not_contains code "func() int64"
  | Error _ -> false

(* --- stress IIFE tests --- *)

let%test "I61: 4-deep nested if in let avoids IIFE" =
  match
    compile_string ~file_id:"<codegen>"
      {|let a = true
let b = true
let c = true
let d = true
let x = if (a) { if (b) { if (c) { if (d) { 1 } else { 2 } } else { 3 } } else { 4 } } else { 5 }
puts(x)|}
  with
  | Ok (code, _) -> string_not_contains code "func() int64"
  | Error _ -> false

let%test "I62: match in tail position avoids IIFE" =
  match
    compile_string ~file_id:"<codegen>"
      {|fn f(x: Int) -> Int = {
  match x { case 1: 10 case 2: 20 case _: 0 }
}
puts(f(1))|}
  with
  | Ok (code, _) -> string_not_contains code "func() int64"
  | Error _ -> false

let%test "I63: if-match-if chain in tail avoids IIFE" =
  match
    compile_string ~file_id:"<codegen>"
      {|fn f(flag: Bool, n: Int, check: Bool) -> Int = {
  if (flag) {
    match n {
      case 1: if (check) { 99 } else { 0 }
      case _: 50
    }
  } else {
    0
  }
}
puts(f(true, 1, true))|}
  with
  | Ok (code, _) -> string_not_contains code "func() int64"
  | Error _ -> false

let%test "I64: nested match in let binding avoids IIFE" =
  match
    compile_string ~file_id:"<codegen>"
      {|let x = 2
let y = 3
let result = match x {
  case 1: match y { case 10: 110 case _: 100 }
  case 2: match y { case 3: 230 case _: 200 }
  case _: 0
}
puts(result)|}
  with
  | Ok (code, _) -> string_not_contains code "func() int64"
  | Error _ -> false

let%test "K66: full-override spread avoids __spread" =
  match
    compile_string ~file_id:"<codegen>" {|let p = { x: 1, y: 2 }
let q = { ...p, x: 10, y: 20 }
puts(q.x)|}
  with
  | Ok (code, _) -> string_not_contains code "__spread"
  | Error _ -> false

let%test "full override spread has no __spread in Go output (records)" =
  match
    compile_string ~file_id:"<codegen>"
      {|let base = { x: 1, y: 2 }
let updated = { ...base, x: 10, y: 20 }
puts(updated.x + updated.y)|}
  with
  | Ok (code, _) -> string_not_contains code "__spread"
  | Error _ -> false

let%test "constrained generic method call does not emit any-mangled specialization" =
  let source =
    {|impl Show[Int] = {
  fn show(self: Int) -> Str = {
    "int"
  }
}
fn id[t: Show](x: t) -> Str = {
  x.show()
}
puts(id(1))|}
  in
  Fun.protect
    ~finally:(fun () ->
      Typecheck.Trait_registry.clear ();
      Typecheck.Builtins.init_builtin_impls ())
    (fun () ->
      match compile_string ~file_id:"<codegen>" source with
      | Ok (code, _) -> string_not_contains code "_any"
      | Error _ -> false)

(* --- run_build_ok_not_contains_from_stdin --- *)

let%test "successful build emits no missing-type warning text" =
  let result, stderr_output =
    capture_stderr (fun () ->
        compile_string ~file_id:"<codegen>" {|fn f(x: Int) -> Int = x + 1
puts(f(1))|})
  in
  match result with
  | Ok _ -> string_not_contains stderr_output "missing type for expression id"
  | Error _ -> false

(* --- test_emit_go_contains --- *)

let%test "enum String default branch panics on invalid tag" =
  match compile_string ~file_id:"<codegen>" {|enum Status = { Ok, Fail }
let x = Status.Ok
puts(x)|} with
  | Ok (code, _) -> string_contains code {|panic("unreachable: invalid enum tag")|}
  | Error _ -> false

let%test "codegen produces valid Go (int64)" =
  match compile_string ~file_id:"<codegen>" "let x = 42; x" with
  | Ok (code, _) -> string_contains code "int64"
  | Error _ -> false

(* --- run_codegen_deterministic_from_stdin --- *)

let%test "codegen deterministic for identical input" =
  is_deterministic {|fn f(x: Int) -> Int = x + 1
fn g(y: Int) -> Int = f(y)
puts(g(1))|}

let%test "J65: complex program deterministic across builds" =
  is_deterministic
    {|enum Option[a] = { Some(a), None }
fn id[a](x: a) -> a = x
fn f(n: Int) -> Int = { match n { case 1: 10 case 2: 20 case _: 0 } }
fn g(flag: Bool) -> Int = { if (flag) { 1 } else { 0 } }
puts(id(42))
puts(id("hello"))
puts(f(2))
puts(g(true))
let opt = Option.Some(42)
match opt {
  case Option.Some(v): puts(v)
  case Option.None: puts(0)
}|}

let%test "record shape codegen is deterministic" =
  is_deterministic
    {|let a = { x: 1, y: 2 }
let b = { x: 3, y: 4 }
let c = { p: "hello", q: true }
puts(a.x + b.y)|}

let%test "deterministic: poly fn at multiple types" =
  is_deterministic {|fn id[a](x: a) -> a = x
puts(id(42))
puts(id(true))
puts(id("hello"))|}

let%test "deterministic: trait impls on multiple types" =
  is_deterministic
    {|trait Label[a] = {
  fn label(x: a) -> Str
}
impl Label[Int] = {
  fn label(x: Int) -> Str = "INT"
}
impl Label[Str] = {
  fn label(x: Str) -> Str = "STR"
}
impl Label[Bool] = {
  fn label(x: Bool) -> Str = "BOOL"
}
puts(1.label())
puts("a".label())
puts(true.label())|}

let%test "Q81: codegen deterministic for records + enums + traits" =
  is_deterministic
    {|type Point = { x: Int, y: Int }
enum Status = { Ok, Fail }
trait Named = { name: Str }
fn get_name[t: Named](x: t) -> Str = x.name
let p = { name: "alice", score: 42 }
let s = Status.Ok
puts(get_name(p))|}

let%test "Q82: codegen deterministic for inherent methods + operators" =
  is_deterministic
    {|type Point = { x: Int, y: Int }
impl Point = {
  fn sum(p: Point) -> Int = p.x + p.y
}
let p: Point = { x: 3, y: 4 }
let result = p.sum() + 10
puts(result)|}
