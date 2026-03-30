open Types

let fields_of_type (typ : mono_type) : record_field_type list option =
  match canonicalize_mono_type typ with
  | TRecord (fields, _row) -> Some fields
  | TNamed (name, args) -> (
      match Type_registry.instantiate_named_product_fields name args with
      | Some (Ok fields) -> Some fields
      | Some (Error _) | None -> None)
  | _ -> None

let lookup_field_type (typ : mono_type) (field_name : string) : mono_type option =
  match fields_of_type typ with
  | None -> None
  | Some fields ->
      List.find_opt (fun (field : record_field_type) -> field.name = field_name) fields
      |> Option.map (fun (field : record_field_type) -> field.typ)

let rec required_fields_satisfied
    (actual_fields : record_field_type list) (required_fields : record_field_type list) : bool =
  List.for_all
    (fun (required : record_field_type) ->
      match List.find_opt (fun (field : record_field_type) -> field.name = required.name) actual_fields with
      | None -> false
      | Some actual -> field_types_compatible actual.typ required.typ)
    required_fields

and type_satisfies_required_fields (typ : mono_type) (required_fields : record_field_type list) : bool =
  match canonicalize_mono_type typ with
  | TIntersection members ->
      List.for_all (fun member -> type_satisfies_required_fields member required_fields) members
  | concrete -> (
      match fields_of_type concrete with
      | Some actual_fields -> required_fields_satisfied actual_fields required_fields
      | None -> false)

and field_types_compatible (actual : mono_type) (expected : mono_type) : bool =
  Result.is_ok (Unify.unify (canonicalize_mono_type actual) (canonicalize_mono_type expected))

let shape_fields (shape_name : string) : record_field_type list option =
  match Type_registry.instantiate_shape_fields shape_name [] with
  | Some (Ok fields) -> Some fields
  | Some (Error _) | None -> None

let type_satisfies_shape (typ : mono_type) (shape_name : string) : bool =
  match shape_fields shape_name with
  | Some required_fields -> type_satisfies_required_fields typ required_fields
  | None -> false

let%test "fields_of_type returns product fields for named products" =
  Type_registry.clear ();
  Type_registry.register_named_type
    {
      Type_registry.named_type_name = "User";
      named_type_params = [];
      named_type_body = Type_registry.NamedProduct [ { name = "name"; typ = TString } ];
    };
  match fields_of_type (TNamed ("User", [])) with
  | Some [ { name = "name"; typ = TString } ] -> true
  | _ -> false

let%test "type_satisfies_shape accepts named products structurally" =
  Type_registry.clear ();
  Type_registry.register_named_type
    {
      Type_registry.named_type_name = "User";
      named_type_params = [];
      named_type_body = Type_registry.NamedProduct [ { name = "name"; typ = TString } ];
    };
  Type_registry.register_shape
    {
      Type_registry.shape_name = "Named";
      shape_type_params = [];
      shape_fields = [ { name = "name"; typ = TString } ];
    };
  type_satisfies_shape (TNamed ("User", [])) "Named"
