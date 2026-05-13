open Types
module AST = Syntax.Ast.AST

type named_type_body =
  | NamedProduct of record_field_type list
  | NamedWrapper of mono_type list
  | NamedExtern of { extern_type_owner_module_id : string }

type named_type_def = {
  named_type_name : string;
  named_type_params : string list;
  named_type_body : named_type_body;
}

type shape_def = {
  shape_name : string;
  shape_type_params : string list;
  shape_fields : record_field_type list;
}

let named_type_source_registry : (string, AST.named_type_def) Hashtbl.t = Hashtbl.create 64
let extern_type_source_registry : (string, AST.extern_type_def) Hashtbl.t = Hashtbl.create 16
let shape_source_registry : (string, AST.shape_def) Hashtbl.t = Hashtbl.create 64
let named_type_registry : (string, named_type_def) Hashtbl.t = Hashtbl.create 64
let shape_registry : (string, shape_def) Hashtbl.t = Hashtbl.create 64
let current_module_id : string option ref = ref None

let clear () : unit =
  Hashtbl.clear named_type_source_registry;
  Hashtbl.clear extern_type_source_registry;
  Hashtbl.clear shape_source_registry;
  Hashtbl.clear named_type_registry;
  Hashtbl.clear shape_registry;
  current_module_id := None

let set_current_module_id module_id = current_module_id := Some module_id

let owner_module_id_of_internal_name name =
  let rec find_last idx last =
    if idx + 1 >= String.length name then
      last
    else if name.[idx] = '_' && name.[idx + 1] = '_' then
      find_last (idx + 2) (Some idx)
    else
      find_last (idx + 1) last
  in
  match find_last 0 None with
  | None -> None
  | Some idx when idx = 0 -> None
  | Some idx ->
      let prefix = String.sub name 0 idx in
      let buffer = Buffer.create (String.length prefix) in
      let rec copy pos =
        if pos >= String.length prefix then
          ()
        else if pos + 1 < String.length prefix && prefix.[pos] = '_' && prefix.[pos + 1] = '_' then (
          Buffer.add_char buffer '.';
          copy (pos + 2))
        else (
          Buffer.add_char buffer prefix.[pos];
          copy (pos + 1))
      in
      copy 0;
      Some (Buffer.contents buffer)

let current_or_inferred_owner_module_id type_name =
  Option.value !current_module_id
    ~default:(Option.value (owner_module_id_of_internal_name type_name) ~default:"<unknown>")

let predeclare_named_type (def : AST.named_type_def) : unit =
  Hashtbl.replace named_type_source_registry def.type_name def

let predeclare_extern_type (def : AST.extern_type_def) : unit =
  Hashtbl.replace extern_type_source_registry def.extern_type_name def

let predeclare_shape (def : AST.shape_def) : unit = Hashtbl.replace shape_source_registry def.shape_name def

let lookup_named_type_source (name : string) : AST.named_type_def option =
  Hashtbl.find_opt named_type_source_registry name

let lookup_extern_type_source (name : string) : AST.extern_type_def option =
  Hashtbl.find_opt extern_type_source_registry name

let lookup_shape_source (name : string) : AST.shape_def option = Hashtbl.find_opt shape_source_registry name

let register_named_type (def : named_type_def) : unit =
  let def' =
    {
      def with
      named_type_body =
        (match def.named_type_body with
        | NamedProduct fields ->
            NamedProduct
              (fields
              |> List.map (fun (f : record_field_type) -> { f with typ = canonicalize_mono_type f.typ })
              |> normalize_record_fields)
        | NamedWrapper payload_types -> NamedWrapper (List.map canonicalize_mono_type payload_types)
        | NamedExtern metadata -> NamedExtern metadata);
    }
  in
  Hashtbl.replace named_type_registry def'.named_type_name def'

let register_extern_type (def : AST.extern_type_def) : unit =
  register_named_type
    {
      named_type_name = def.extern_type_name;
      named_type_params = [];
      named_type_body =
        NamedExtern { extern_type_owner_module_id = current_or_inferred_owner_module_id def.extern_type_name };
    }

let register_shape (def : shape_def) : unit =
  let def' =
    {
      def with
      shape_fields =
        def.shape_fields
        |> List.map (fun (f : record_field_type) -> { f with typ = canonicalize_mono_type f.typ })
        |> normalize_record_fields;
    }
  in
  Hashtbl.replace shape_registry def'.shape_name def'

let lookup_named_type (name : string) : named_type_def option = Hashtbl.find_opt named_type_registry name
let lookup_shape (name : string) : shape_def option = Hashtbl.find_opt shape_registry name
let all_named_types () : named_type_def list = Hashtbl.to_seq_values named_type_registry |> List.of_seq

let is_named_type_name (name : string) : bool =
  Hashtbl.mem named_type_source_registry name
  || Hashtbl.mem extern_type_source_registry name
  || Hashtbl.mem named_type_registry name

let is_extern_type_name (name : string) : bool =
  match lookup_named_type name with
  | Some { named_type_body = NamedExtern _; _ } -> true
  | _ -> Hashtbl.mem extern_type_source_registry name

let is_shape_name (name : string) : bool =
  Hashtbl.mem shape_source_registry name || Hashtbl.mem shape_registry name

let named_type_arity (name : string) : int option =
  match lookup_named_type name with
  | Some def -> Some (List.length def.named_type_params)
  | None -> (
      match lookup_named_type_source name with
      | Some def -> Some (List.length def.type_type_params)
      | None ->
          if Hashtbl.mem extern_type_source_registry name then
            Some 0
          else
            None)

let extern_type_owner_module_id (name : string) : string option =
  match lookup_named_type name with
  | Some { named_type_body = NamedExtern { extern_type_owner_module_id }; _ } -> Some extern_type_owner_module_id
  | _ -> None

let shape_arity (name : string) : int option =
  match lookup_shape name with
  | Some def -> Some (List.length def.shape_type_params)
  | None -> (
      match lookup_shape_source name with
      | Some def -> Some (List.length def.shape_type_params)
      | None -> None)

let instantiate_type_params (params : string list) (args : mono_type list) : (substitution, string) result =
  if List.length params <> List.length args then
    Error (Printf.sprintf "Expected %d type argument(s), got %d" (List.length params) (List.length args))
  else
    Ok (substitution_of_list (List.combine params args))

let instantiate_named_product_fields (name : string) (args : mono_type list) :
    (record_field_type list, string) result option =
  match lookup_named_type name with
  | None -> None
  | Some def -> (
      match instantiate_type_params def.named_type_params args with
      | Error msg -> Some (Error (Printf.sprintf "Named type %s: %s" name msg))
      | Ok subst -> (
          match def.named_type_body with
          | NamedProduct fields ->
              Some
                (Ok
                   (fields
                   |> List.map (fun (f : record_field_type) -> { f with typ = apply_substitution subst f.typ })
                   |> normalize_record_fields))
          | NamedWrapper _ ->
              Some (Error (Printf.sprintf "Named type %s is a wrapper type, not a product type" name))
          | NamedExtern _ -> Some (Error (Printf.sprintf "Extern type %s has no fields to inspect" name))))

let instantiate_named_wrapper_representation (name : string) (args : mono_type list) :
    (mono_type list, string) result option =
  match lookup_named_type name with
  | None -> None
  | Some def -> (
      match instantiate_type_params def.named_type_params args with
      | Error msg -> Some (Error (Printf.sprintf "Named type %s: %s" name msg))
      | Ok subst -> (
          match def.named_type_body with
          | NamedProduct _ ->
              Some (Error (Printf.sprintf "Named type %s is a product type, not a wrapper type" name))
          | NamedWrapper payload_types -> Some (Ok (List.map (apply_substitution subst) payload_types))
          | NamedExtern _ ->
              Some (Error (Printf.sprintf "Extern type %s has no Marmoset representation to construct" name))))

let instantiate_shape_fields (name : string) (args : mono_type list) :
    (record_field_type list, string) result option =
  match lookup_shape name with
  | None -> None
  | Some def -> (
      match instantiate_type_params def.shape_type_params args with
      | Error msg -> Some (Error (Printf.sprintf "Shape %s: %s" name msg))
      | Ok subst ->
          Some
            (Ok
               (def.shape_fields
               |> List.map (fun (f : record_field_type) -> { f with typ = apply_substitution subst f.typ })
               |> normalize_record_fields)))
