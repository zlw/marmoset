open Types
module AST = Syntax.Ast.AST

type named_type_body =
  | NamedProduct of record_field_type list
  | NamedWrapper of mono_type

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
let shape_source_registry : (string, AST.shape_def) Hashtbl.t = Hashtbl.create 64
let named_type_registry : (string, named_type_def) Hashtbl.t = Hashtbl.create 64
let shape_registry : (string, shape_def) Hashtbl.t = Hashtbl.create 64

let clear () : unit =
  Hashtbl.clear named_type_source_registry;
  Hashtbl.clear shape_source_registry;
  Hashtbl.clear named_type_registry;
  Hashtbl.clear shape_registry

let predeclare_named_type (def : AST.named_type_def) : unit =
  Hashtbl.replace named_type_source_registry def.type_name def

let predeclare_shape (def : AST.shape_def) : unit = Hashtbl.replace shape_source_registry def.shape_name def

let lookup_named_type_source (name : string) : AST.named_type_def option = Hashtbl.find_opt named_type_source_registry name
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
        | NamedWrapper mono -> NamedWrapper (canonicalize_mono_type mono));
    }
  in
  Hashtbl.replace named_type_registry def'.named_type_name def'

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

let is_named_type_name (name : string) : bool =
  Hashtbl.mem named_type_source_registry name || Hashtbl.mem named_type_registry name

let is_shape_name (name : string) : bool =
  Hashtbl.mem shape_source_registry name || Hashtbl.mem shape_registry name

let named_type_arity (name : string) : int option =
  match lookup_named_type name with
  | Some def -> Some (List.length def.named_type_params)
  | None -> (
      match lookup_named_type_source name with
      | Some def -> Some (List.length def.type_type_params)
      | None -> None)

let shape_arity (name : string) : int option =
  match lookup_shape name with
  | Some def -> Some (List.length def.shape_type_params)
  | None -> (
      match lookup_shape_source name with
      | Some def -> Some (List.length def.shape_type_params)
      | None -> None)

let instantiate_type_params (params : string list) (args : mono_type list) : (substitution, string) result =
  if List.length params <> List.length args then
    Error
      (Printf.sprintf "Expected %d type argument(s), got %d" (List.length params) (List.length args))
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
              Some (Error (Printf.sprintf "Named type %s is a wrapper type, not a product type" name))))

let instantiate_named_wrapper_representation (name : string) (args : mono_type list) :
    (mono_type, string) result option =
  match lookup_named_type name with
  | None -> None
  | Some def -> (
      match instantiate_type_params def.named_type_params args with
      | Error msg -> Some (Error (Printf.sprintf "Named type %s: %s" name msg))
      | Ok subst -> (
          match def.named_type_body with
          | NamedProduct _ ->
              Some (Error (Printf.sprintf "Named type %s is a product type, not a wrapper type" name))
          | NamedWrapper mono -> Some (Ok (apply_substitution subst mono))))

let instantiate_shape_fields (name : string) (args : mono_type list) : (record_field_type list, string) result option =
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
