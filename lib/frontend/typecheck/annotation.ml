(* Phase 2: Type Annotation Conversion and Checking
   
   This module converts parsed type expressions (AST.type_expr) to internal
   monomorphic types (Types.mono_type) for checking and integration with the
   Hindley-Milner type inference system.
   
   Key functions:
   - type_expr_to_mono_type: Convert parsed types to mono types
   - check_annotation: Verify annotation matches inferred type
   - extract_constraints: Extract trait constraints for Phase 3
*)

module Diagnostic = Diagnostics.Diagnostic
module Constraints = Constraints

let ( let* ) = Result.bind

let map_result f xs =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | x :: rest -> (
        match f x with
        | Error e -> Error e
        | Ok y -> go (y :: acc) rest)
  in
  go [] xs

let iter_result f xs =
  let rec go = function
    | [] -> Ok ()
    | x :: rest -> (
        match f x with
        | Error e -> Error e
        | Ok () -> go rest)
  in
  go xs

(* Convert a parsed type expression to an internal mono_type *)
type type_alias_info = {
  alias_type_params : string list;
  alias_body : Syntax.Ast.AST.type_expr;
}

let type_alias_registry : (string, type_alias_info) Hashtbl.t = Hashtbl.create 64

let register_type_alias (alias_def : Syntax.Ast.AST.type_alias_def) : unit =
  Hashtbl.replace type_alias_registry alias_def.alias_name
    { alias_type_params = alias_def.alias_type_params; alias_body = alias_def.alias_body }

let register_type_alias_info ~(name : string) (info : type_alias_info) : unit =
  Hashtbl.replace type_alias_registry name info

let clear_type_aliases () : unit = Hashtbl.clear type_alias_registry
let lookup_type_alias (name : string) : type_alias_info option = Hashtbl.find_opt type_alias_registry name
let fresh_shape_row_counter = ref 0

let fresh_shape_row_var () : Types.mono_type =
  let n = !fresh_shape_row_counter in
  fresh_shape_row_counter := n + 1;
  Types.TRowVar ("shape_row" ^ string_of_int n)

let builtin_primitive_type (name : string) : Types.mono_type option =
  match name with
  | "Int" -> Some Types.TInt
  | "Float" -> Some Types.TFloat
  | "Bool" -> Some Types.TBool
  | "Str" -> Some Types.TString
  | "Unit" -> Some Types.TNull
  | _ -> None

let builtin_type_constructor_name (name : string) : string option =
  match name with
  | "List" -> Some "list"
  | "Map" -> Some "map"
  | _ -> None

let lookup_enum_by_source_name (name : string) : Enum_registry.enum_def option =
  match Enum_registry.lookup name with
  | Some _ as enum_def -> enum_def
  | None -> (
      match builtin_type_constructor_name name with
      | Some builtin_name -> Enum_registry.lookup builtin_name
      | None -> None)

let trait_type_position_error (name : string) : string =
  Printf.sprintf "Trait '%s' cannot be used as a type; use a constrained type parameter or Dyn[...] instead" name

let type_position_error_for_constructor (name : string) : string =
  if Trait_registry.lookup_trait name <> None then
    trait_type_position_error name
  else
    "Unknown type constructor: " ^ name

let trait_object_traits_allowed (traits : string list) : bool =
  traits <> []
  && List.for_all
       (fun trait_name ->
         match Constraints.of_name trait_name with
         | Constraints.ShapeConstraint _ -> false
         | Constraints.TraitConstraint canonical_name -> Trait_registry.lookup_trait canonical_name <> None)
       traits

let normalized_trait_object_membership (traits : string list) : string list =
  traits
  |> List.concat_map Trait_registry.trait_with_supertraits
  |> List.map Trait_registry.canonical_trait_name
  |> List.sort_uniq String.compare

let intersection_has_mixed_dyn_members (members : Types.mono_type list) : bool =
  let has_dyn =
    List.exists
      (function
        | Types.TTraitObject _ -> true
        | _ -> false)
      members
  in
  let has_non_dyn =
    List.exists
      (function
        | Types.TTraitObject _ -> false
        | _ -> true)
      members
  in
  has_dyn && has_non_dyn

let is_callable_type = function
  | Types.TFun _ -> true
  | _ -> false

let open_record_of_shape_fields (fields : Types.record_field_type list) : Types.mono_type =
  Types.TRecord (Types.normalize_record_fields fields, Some (fresh_shape_row_var ()))

let record_fields_satisfied_with
    (subtype : Types.mono_type -> Types.mono_type -> bool)
    (actual_fields : Types.record_field_type list)
    (expected_fields : Types.record_field_type list) : bool =
  let field_lookup fields name = List.find_opt (fun (f : Types.record_field_type) -> f.name = name) fields in
  List.for_all
    (fun (expected_f : Types.record_field_type) ->
      match field_lookup actual_fields expected_f.name with
      | None -> false
      | Some actual_f -> subtype actual_f.typ expected_f.typ)
    expected_fields

let rec type_satisfies_required_fields_with
    (subtype : Types.mono_type -> Types.mono_type -> bool)
    (typ : Types.mono_type)
    (required_fields : Types.record_field_type list) : bool =
  match Types.canonicalize_mono_type typ with
  | Types.TIntersection members ->
      List.for_all (fun member -> type_satisfies_required_fields_with subtype member required_fields) members
  | concrete -> (
      match Structural.fields_of_type concrete with
      | Some actual_fields -> record_fields_satisfied_with subtype actual_fields required_fields
      | None -> false)

let rec member_is_subtype_of (actual : Types.mono_type) (expected : Types.mono_type) : bool =
  match (actual, expected) with
  | Types.TVar a, Types.TVar b -> a = b
  | Types.TRowVar a, Types.TRowVar b -> a = b
  | Types.TTraitObject actual_traits, Types.TTraitObject expected_traits ->
      let actual_members = normalized_trait_object_membership actual_traits in
      let expected_members = normalized_trait_object_membership expected_traits in
      trait_object_traits_allowed actual_traits
      && trait_object_traits_allowed expected_traits
      && List.for_all
           (fun trait_name -> List.mem (Trait_registry.canonical_trait_name trait_name) actual_members)
           expected_members
  | concrete, Types.TTraitObject expected_traits ->
      trait_object_traits_allowed expected_traits
      && List.for_all
           (fun trait_name -> Trait_solver.satisfies_trait_bool concrete trait_name)
           (Types.normalize_trait_object_traits expected_traits)
  | Types.TInt, Types.TInt -> true
  | Types.TFloat, Types.TFloat -> true
  | Types.TBool, Types.TBool -> true
  | Types.TString, Types.TString -> true
  | Types.TNull, Types.TNull -> true
  | Types.TNamed (name1, args1), Types.TNamed (name2, args2) ->
      name1 = name2 && List.length args1 = List.length args2 && List.for_all2 member_is_subtype_of args1 args2
  | Types.TArray elem1, Types.TArray elem2 -> member_is_subtype_of elem1 elem2
  | Types.THash (k1, v1), Types.THash (k2, v2) -> member_is_subtype_of k1 k2 && member_is_subtype_of v1 v2
  | Types.TNamed _, Types.TRecord (_expected_fields, None) -> false
  | actual_named, Types.TRecord (expected_fields, Some _row) ->
      type_satisfies_required_fields_with member_is_subtype_of actual_named expected_fields
  | Types.TRecord (fields1, row1), Types.TRecord (fields2, row2) -> (
      let field_lookup fields name = List.find_opt (fun (f : Types.record_field_type) -> f.name = name) fields in
      let fields_ok =
        List.for_all
          (fun (expected_f : Types.record_field_type) ->
            match field_lookup fields1 expected_f.name with
            | None -> false
            | Some actual_f -> member_is_subtype_of actual_f.typ expected_f.typ)
          fields2
      in
      if not fields_ok then
        false
      else
        let has_extra_fields =
          List.exists (fun (f : Types.record_field_type) -> field_lookup fields2 f.name = None) fields1
        in
        match row2 with
        | Some _ -> true
        | None ->
            if has_extra_fields then
              match row1 with
              | Some _ -> false
              | None -> false
            else
              true)
  | Types.TFun (p1, r1, eff1), Types.TFun (p2, r2, eff2) ->
      eff1 = eff2 && member_is_subtype_of p2 p1 && member_is_subtype_of r1 r2
  | Types.TEnum (name1, args1), Types.TEnum (name2, args2) ->
      name1 = name2 && List.length args1 = List.length args2 && List.for_all2 member_is_subtype_of args1 args2
  | Types.TUnion members1, Types.TUnion members2 ->
      List.for_all (fun m1 -> List.exists (member_is_subtype_of m1) members2) members1
  | concrete, Types.TUnion members -> List.exists (member_is_subtype_of concrete) members
  | Types.TUnion members, concrete -> List.for_all (fun member -> member_is_subtype_of member concrete) members
  | _ -> false

let rec merged_record_intersection_type (members : Types.mono_type list) : (Types.mono_type, Diagnostic.t) result
    =
  let intersection_error message = Error (Diagnostic.error_no_span ~code:"type-annotation-invalid" ~message) in
  let merged_fields : (string, Types.mono_type) Hashtbl.t = Hashtbl.create 8 in
  let choose_field_type field_name existing incoming =
    if member_is_subtype_of existing incoming then
      Ok existing
    else if member_is_subtype_of incoming existing then
      Ok incoming
    else
      match (Types.canonicalize_mono_type existing, Types.canonicalize_mono_type incoming) with
      | Types.TRecord (_, None), Types.TRecord (_, None) -> merged_record_intersection_type [ existing; incoming ]
      | _ ->
          intersection_error
            (Printf.sprintf "Record intersection field '%s' has incompatible types %s and %s" field_name
               (Types.to_string existing) (Types.to_string incoming))
  in
  let rec add_fields = function
    | [] -> Ok ()
    | Types.TRecord (fields, None) :: rest ->
        let rec add_record_fields = function
          | [] -> add_fields rest
          | (field : Types.record_field_type) :: field_rest -> (
              match Hashtbl.find_opt merged_fields field.name with
              | None ->
                  Hashtbl.replace merged_fields field.name field.typ;
                  add_record_fields field_rest
              | Some existing -> (
                  match choose_field_type field.name existing field.typ with
                  | Error _ as err -> err
                  | Ok merged ->
                      Hashtbl.replace merged_fields field.name merged;
                      add_record_fields field_rest))
        in
        add_record_fields fields
    | Types.TRecord (_, Some _) :: _ -> intersection_error "Open-record intersections are not supported"
    | _ -> intersection_error "Internal error: expected only record members in record intersection merge"
  in
  let* () = add_fields members in
  let merged_fields =
    Hashtbl.to_seq_keys merged_fields
    |> List.of_seq
    |> List.sort String.compare
    |> List.map (fun name ->
           match Hashtbl.find_opt merged_fields name with
           | Some typ -> { Types.name; typ }
           | None -> failwith "merged_record_intersection_type: impossible missing field")
  in
  Ok (Types.TRecord (Types.normalize_record_fields merged_fields, None))

let validate_record_intersection_members (members : Types.mono_type list) : (unit, Diagnostic.t) result =
  let* _ = merged_record_intersection_type members in
  Ok ()

let validate_intersection_type (members : Types.mono_type list) : (Types.mono_type, Diagnostic.t) result =
  let normalized = Types.normalize_intersection members in
  match normalized with
  | Types.TIntersection normalized_members -> (
      if intersection_has_mixed_dyn_members normalized_members then
        Error
          (Diagnostic.error_no_span ~code:"type-annotation-invalid"
             ~message:
               "Intersection types may not mix Dyn[...] members with non-Dyn members. Split the annotation or use a single Dyn[...] trait set.")
      else
        match normalized_members with
        | many
          when List.for_all
                 (function
                   | Types.TRecord _ -> true
                   | _ -> false)
                 many ->
            let* () = validate_record_intersection_members many in
            Ok (Types.TIntersection many)
        | many when List.exists is_callable_type many ->
            Error
              (Diagnostic.error_no_span ~code:"type-annotation-invalid"
                 ~message:
                   "General callable intersections are not supported unless normalization leaves a single callable type.")
        | many ->
            Error
              (Diagnostic.error_no_span ~code:"type-annotation-invalid"
                 ~message:
                   (Printf.sprintf
                      "Unsupported intersection: %s. Supported forms are compatible record meets, Dyn[...] trait-set merging, and intersections that simplify to a single member."
                      (Types.to_string (Types.TIntersection many)))))
  | Types.TFun _ as callable -> Ok callable
  | other -> Ok other

let rec type_expr_to_mono_type_with
    (type_bindings : (string * Types.mono_type) list) (te : Syntax.Ast.AST.type_expr) :
    (Types.mono_type, Diagnostic.t) result =
  let convert_shape_fields_with
      (shape_bindings : (string * Types.mono_type) list) (shape_fields : Syntax.Ast.AST.record_type_field list) :
      (Types.record_field_type list, Diagnostic.t) result =
    map_result
      (fun (f : Syntax.Ast.AST.record_type_field) ->
        let* typ = type_expr_to_mono_type_with shape_bindings f.field_type in
        Ok { Types.name = f.field_name; typ })
      shape_fields
  in
  let shape_type_to_mono_type_with
      (shape_name : string)
      (shape_type_params : string list)
      (shape_fields : Syntax.Ast.AST.record_type_field list)
      (arg_types : Types.mono_type list) : (Types.mono_type, Diagnostic.t) result =
    let ann_error msg = Error (Diagnostic.error_no_span ~code:"type-annotation-invalid" ~message:msg) in
    if List.length shape_type_params <> List.length arg_types then
      ann_error
        (Printf.sprintf "Shape %s expects %d type argument(s), got %d" shape_name (List.length shape_type_params)
           (List.length arg_types))
    else
      let shape_bindings = List.combine shape_type_params arg_types in
      let* fields = convert_shape_fields_with (shape_bindings @ type_bindings) shape_fields in
      Ok (open_record_of_shape_fields fields)
  in
  let ann_error msg = Error (Diagnostic.error_no_span ~code:"type-annotation-invalid" ~message:msg) in
  match te with
  | Syntax.Ast.AST.TVar name -> (
      match List.assoc_opt name type_bindings with
      | Some ty -> Ok ty
      | None -> Ok (Types.TVar name))
  | Syntax.Ast.AST.TCon name ->
      let* bound_or_unbound =
        match List.assoc_opt name type_bindings with
        | Some ty -> Ok (Some ty)
        | None -> Ok None
      in
      let lookup_enum_or_trait () =
        match lookup_enum_by_source_name name with
        | Some enum_def when enum_def.type_params = [] -> Ok (Types.TEnum (enum_def.name, []))
        | Some enum_def -> ann_error (Printf.sprintf "Type %s expects type arguments" enum_def.name)
        | None ->
            if Trait_registry.lookup_trait name <> None then
              ann_error (trait_type_position_error name)
            else
              ann_error (type_position_error_for_constructor name)
      in
      let lookup_alias_or_enum () =
        match lookup_type_alias name with
        | Some alias_info when alias_info.alias_type_params = [] ->
            type_expr_to_mono_type_with type_bindings alias_info.alias_body
        | Some _ -> ann_error (Printf.sprintf "Type %s expects type arguments" name)
        | None -> lookup_enum_or_trait ()
      in
      let lookup_shape_instantiation () =
        match Type_registry.instantiate_shape_fields name [] with
        | Some (Ok fields) -> Ok (open_record_of_shape_fields fields)
        | Some (Error msg) -> ann_error msg
        | None -> lookup_alias_or_enum ()
      in
      let lookup_shape_source () =
        match Type_registry.lookup_shape_source name with
        | Some shape_def when shape_def.shape_type_params = [] ->
            shape_type_to_mono_type_with name shape_def.shape_type_params shape_def.shape_fields []
        | Some shape_def ->
            ann_error
              (Printf.sprintf "Shape %s expects %d type argument(s)" name
                 (List.length shape_def.shape_type_params))
        | None -> lookup_shape_instantiation ()
      in
      let lookup_named_type () =
        match Type_registry.named_type_arity name with
        | Some 0 -> Ok (Types.TNamed (name, []))
        | Some expected_arity ->
            ann_error (Printf.sprintf "Named type %s expects %d type argument(s)" name expected_arity)
        | None -> lookup_shape_source ()
      in
      (match bound_or_unbound with
      | Some ty -> Ok ty
      | None -> (
          match builtin_primitive_type name with
          | Some primitive -> Ok primitive
          | None -> lookup_named_type ()))
  | Syntax.Ast.AST.TApp (con_name, type_args) ->
      let ann_error msg = Error (Diagnostic.error_no_span ~code:"type-annotation-invalid" ~message:msg) in
      let* arg_types = map_result (type_expr_to_mono_type_with type_bindings) type_args in
      (match builtin_type_constructor_name con_name with
      | Some "list" -> (
          match arg_types with
          | [ elem_type ] -> Ok (Types.TArray elem_type)
          | _ -> ann_error ("List type expects 1 argument, got " ^ string_of_int (List.length arg_types)))
      | Some "map" -> (
          match arg_types with
          | [ key_type; value_type ] -> Ok (Types.THash (key_type, value_type))
          | _ -> ann_error ("Map type expects 2 arguments, got " ^ string_of_int (List.length arg_types)))
      | _ -> (
          match Type_registry.named_type_arity con_name with
          | Some expected_arity ->
              let actual_arity = List.length arg_types in
              if expected_arity <> actual_arity then
                ann_error
                  (Printf.sprintf "Named type %s expects %d type argument(s), got %d" con_name expected_arity
                     actual_arity)
              else
                Ok (Types.TNamed (con_name, arg_types))
          | None -> (
              match Type_registry.lookup_shape_source con_name with
              | Some shape_def ->
                  shape_type_to_mono_type_with con_name shape_def.shape_type_params shape_def.shape_fields
                    arg_types
              | None -> (
                  match Type_registry.instantiate_shape_fields con_name arg_types with
                  | Some (Ok fields) -> Ok (open_record_of_shape_fields fields)
                  | Some (Error msg) -> ann_error msg
                  | None -> (
                      match lookup_type_alias con_name with
                      | Some alias_info ->
                          let expected_arity = List.length alias_info.alias_type_params in
                          let actual_arity = List.length arg_types in
                          if expected_arity <> actual_arity then
                            ann_error
                              (Printf.sprintf "Type %s expects %d type argument(s), got %d" con_name
                                 expected_arity actual_arity)
                          else
                            let alias_bindings = List.combine alias_info.alias_type_params arg_types in
                            type_expr_to_mono_type_with (alias_bindings @ type_bindings) alias_info.alias_body
                      | None -> (
                          match lookup_enum_by_source_name con_name with
                          | Some enum_def ->
                              let expected_arity = List.length enum_def.type_params in
                              let actual_arity = List.length arg_types in
                              if expected_arity <> actual_arity then
                                ann_error
                                  (Printf.sprintf "Type %s expects %d type argument(s), got %d" enum_def.name
                                     expected_arity actual_arity)
                              else
                                Ok (Types.TEnum (enum_def.name, arg_types))
                          | None -> ann_error (type_position_error_for_constructor con_name)))))))
  | Syntax.Ast.AST.TArrow (param_types, return_type, is_effectful) ->
      let* param_mono = map_result (type_expr_to_mono_type_with type_bindings) param_types in
      let* return_mono = type_expr_to_mono_type_with type_bindings return_type in
      let mk_fun arg ret =
        if is_effectful then
          Types.tfun_eff arg ret
        else
          Types.tfun arg ret
      in
      Ok (List.fold_right mk_fun param_mono return_mono)
  | Syntax.Ast.AST.TTraitObject traits -> Ok (Types.canonicalize_mono_type (Types.TTraitObject traits))
  | Syntax.Ast.AST.TUnion type_exprs ->
      let* mono_types = map_result (type_expr_to_mono_type_with type_bindings) type_exprs in
      Ok (Types.normalize_union mono_types)
  | Syntax.Ast.AST.TIntersection type_exprs ->
      let* mono_types = map_result (type_expr_to_mono_type_with type_bindings) type_exprs in
      validate_intersection_type mono_types
  | Syntax.Ast.AST.TRecord (fields, row_var) -> (
      match row_var with
      | Some _ ->
          Error
            (Diagnostic.error_no_span ~code:"type-open-row-rejected"
               ~message:
                 "Open row variables (e.g., '...row') are not supported in type annotations. Use a closed record type annotation (e.g., '{ x: int, y: int }') or omit the annotation.")
      | None ->
          let* field_types =
            map_result
              (fun (f : Syntax.Ast.AST.record_type_field) ->
                let* typ = type_expr_to_mono_type_with type_bindings f.field_type in
                Ok { Types.name = f.field_name; typ })
              fields
          in
          Ok (Types.canonicalize_mono_type (Types.TRecord (field_types, None))))

and type_expr_to_mono_type (te : Syntax.Ast.AST.type_expr) : (Types.mono_type, Diagnostic.t) result =
  type_expr_to_mono_type_with [] te

(* Check if two types are equal (for annotation verification) *)
let rec mono_types_equal (t1 : Types.mono_type) (t2 : Types.mono_type) : bool =
  match (t1, t2) with
  | Types.TVar a, Types.TVar b -> a = b
  | Types.TInt, Types.TInt -> true
  | Types.TFloat, Types.TFloat -> true
  | Types.TBool, Types.TBool -> true
  | Types.TString, Types.TString -> true
  | Types.TNull, Types.TNull -> true
  | Types.TArray elem1, Types.TArray elem2 -> mono_types_equal elem1 elem2
  | Types.THash (k1, v1), Types.THash (k2, v2) -> mono_types_equal k1 k2 && mono_types_equal v1 v2
  | Types.TRecord (fields1, row1), Types.TRecord (fields2, row2) -> (
      let compare_field_names a b = String.compare a.Types.name b.Types.name in
      let sort_fields fields = List.sort (fun a b -> compare_field_names a b) fields in
      let fields1' = sort_fields fields1 in
      let fields2' = sort_fields fields2 in
      List.length fields1' = List.length fields2'
      && List.for_all2
           (fun f1 f2 -> f1.Types.name = f2.Types.name && mono_types_equal f1.Types.typ f2.Types.typ)
           fields1' fields2'
      &&
      match (row1, row2) with
      | None, None -> true
      | Some r1, Some r2 -> mono_types_equal r1 r2
      | _ -> false)
  | Types.TRowVar r1, Types.TRowVar r2 -> r1 = r2
  | Types.TFun (p1, r1, eff1), Types.TFun (p2, r2, eff2) ->
      eff1 = eff2 && mono_types_equal p1 p2 && mono_types_equal r1 r2
  | Types.TTraitObject traits1, Types.TTraitObject traits2 ->
      Types.normalize_trait_object_traits traits1 = Types.normalize_trait_object_traits traits2
  | Types.TUnion t1s, Types.TUnion t2s ->
      List.length t1s = List.length t2s && List.for_all2 mono_types_equal t1s t2s
  | Types.TIntersection t1s, Types.TIntersection t2s ->
      List.length t1s = List.length t2s && List.for_all2 mono_types_equal t1s t2s
  | Types.TEnum (name1, args1), Types.TEnum (name2, args2) ->
      name1 = name2 && List.length args1 = List.length args2 && List.for_all2 mono_types_equal args1 args2
  | Types.TNamed (name1, args1), Types.TNamed (name2, args2) ->
      name1 = name2 && List.length args1 = List.length args2 && List.for_all2 mono_types_equal args1 args2
  | _ -> false

(* Check if actual_type is a subtype of expected_type.
   This is used for return type checking where we need to ensure the actual
   return value can safely be used where the expected type is expected.
   
   Key rules:
   - A concrete type is subtype of itself (int <: int)
   - A concrete type is subtype of a union containing it (int <: int | string)
   - A union is subtype of another union if ALL members are in the target union
   - A union is subtype of a concrete ONLY if it contains only that type
*)
let rec is_subtype_of (actual : Types.mono_type) (expected : Types.mono_type) : bool =
  match (actual, expected) with
  (* Type variables: only equal names are known-compatible at subtyping layer.
     Unresolved-variable compatibility should be handled by callers with
     explicit unification when appropriate. *)
  | Types.TVar a, Types.TVar b -> a = b
  | Types.TRowVar a, Types.TRowVar b -> a = b
  | Types.TIntersection _, Types.TIntersection members ->
      List.for_all (fun member -> is_subtype_of actual member) members
  | Types.TRecord (actual_fields, _), Types.TIntersection members ->
      List.for_all
        (function
          | Types.TRecord (expected_fields, _) ->
              record_fields_satisfied_with is_subtype_of actual_fields expected_fields
          | member -> is_subtype_of actual member)
        members
  | actual, Types.TIntersection members -> List.for_all (fun member -> is_subtype_of actual member) members
  | Types.TIntersection members, expected -> List.exists (fun member -> is_subtype_of member expected) members
  | Types.TTraitObject actual_traits, Types.TTraitObject expected_traits ->
      let actual_members = normalized_trait_object_membership actual_traits in
      let expected_members = normalized_trait_object_membership expected_traits in
      trait_object_traits_allowed actual_traits
      && trait_object_traits_allowed expected_traits
      && List.for_all
           (fun trait_name -> List.mem (Trait_registry.canonical_trait_name trait_name) actual_members)
           expected_members
  | Types.TUnion members, Types.TTraitObject expected_traits ->
      trait_object_traits_allowed expected_traits
      && List.for_all (fun member -> is_subtype_of member (Types.TTraitObject expected_traits)) members
  | concrete, Types.TTraitObject expected_traits ->
      trait_object_traits_allowed expected_traits
      && List.for_all
           (fun trait_name -> Trait_solver.satisfies_trait_bool concrete trait_name)
           (Types.normalize_trait_object_traits expected_traits)
  (* Same primitive types *)
  | Types.TInt, Types.TInt -> true
  | Types.TFloat, Types.TFloat -> true
  | Types.TBool, Types.TBool -> true
  | Types.TString, Types.TString -> true
  | Types.TNull, Types.TNull -> true
  | Types.TNamed (name1, args1), Types.TNamed (name2, args2) ->
      name1 = name2 && List.length args1 = List.length args2 && List.for_all2 is_subtype_of args1 args2
  | actual_named, Types.TRecord (expected_fields, Some _row) ->
      type_satisfies_required_fields_with is_subtype_of actual_named expected_fields
  | Types.TNamed _, Types.TRecord (_, None) -> false
  (* Arrays: covariant in element type *)
  | Types.TArray elem1, Types.TArray elem2 -> is_subtype_of elem1 elem2
  (* Hashes: covariant in key and value types *)
  | Types.THash (k1, v1), Types.THash (k2, v2) -> is_subtype_of k1 k2 && is_subtype_of v1 v2
  | Types.TRecord (fields1, row1), Types.TRecord (fields2, row2) -> (
      let field_lookup fields name = List.find_opt (fun (f : Types.record_field_type) -> f.name = name) fields in
      let fields_ok =
        List.for_all
          (fun (expected_f : Types.record_field_type) ->
            match field_lookup fields1 expected_f.name with
            | None -> false
            | Some actual_f -> is_subtype_of actual_f.typ expected_f.typ)
          fields2
      in
      if not fields_ok then
        false
      else
        let has_extra_fields =
          List.exists (fun (f : Types.record_field_type) -> field_lookup fields2 f.name = None) fields1
        in
        match row2 with
        | Some _ -> true
        | None ->
            if has_extra_fields then
              match row1 with
              | Some _ -> false
              | None -> false
            else
              true)
  (* Functions: contravariant in params, covariant in return *)
  | Types.TFun (p1, r1, actual_effectful), Types.TFun (p2, r2, expected_effectful) ->
      ((not actual_effectful) || expected_effectful) && is_subtype_of p2 p1 && is_subtype_of r1 r2
  (* Enums: same name, subtypes for all args *)
  | Types.TEnum (name1, args1), Types.TEnum (name2, args2) ->
      name1 = name2 && List.length args1 = List.length args2 && List.for_all2 is_subtype_of args1 args2
  (* Union is subtype of another union if ALL members are subtypes - must come before single-sided cases *)
  | Types.TUnion members1, Types.TUnion members2 ->
      List.for_all (fun m1 -> List.exists (is_subtype_of m1) members2) members1
  (* Concrete type is subtype of union if it's a member *)
  | concrete, Types.TUnion members -> List.exists (is_subtype_of concrete) members
  (* Union is subtype of concrete ONLY if all members equal that concrete type *)
  | Types.TUnion members, concrete -> List.for_all (fun m -> is_subtype_of m concrete) members
  (* Different concrete types *)
  | _ -> false

(* Check if annotation matches inferred type *)
let check_annotation (annot_type : Types.mono_type) (inferred_type : Types.mono_type) : bool =
  (* For return type annotations, we check if the inferred type is a subtype 
     of the annotated type. This means:
     - int matches int (equal)
     - int matches int | string (widening is ok)
     - int | string does NOT match int (narrowing is not ok) *)
  is_subtype_of inferred_type annot_type

(* Extract constraint names from generic parameters *)
let extract_constraints (params : Syntax.Ast.AST.generic_param list) : string list list =
  List.map (fun p -> p.Syntax.Ast.AST.constraints) params

(* Format a mono_type as a string for error messages *)
let rec format_mono_type (t : Types.mono_type) : string =
  match t with
  | Types.TVar name -> name
  | Types.TInt -> "Int"
  | Types.TFloat -> "Float"
  | Types.TBool -> "Bool"
  | Types.TString -> "Str"
  | Types.TNull -> "Unit"
  | Types.TArray elem_type -> Printf.sprintf "List[%s]" (format_mono_type elem_type)
  | Types.THash (key_type, value_type) ->
      Printf.sprintf "Map[%s, %s]" (format_mono_type key_type) (format_mono_type value_type)
  | Types.TRecord (fields, row) ->
      let field_strs =
        List.map (fun (f : Types.record_field_type) -> f.name ^ ": " ^ format_mono_type f.typ) fields
      in
      let row_str =
        match row with
        | None -> ""
        | Some r ->
            if field_strs = [] then
              "..." ^ format_mono_type r
            else
              ", ..." ^ format_mono_type r
      in
      Printf.sprintf "{ %s%s }" (String.concat ", " field_strs) row_str
  | Types.TRowVar name -> name
  | Types.TFun (param_type, return_type, eff) ->
      let arrow =
        if eff then
          " => "
        else
          " -> "
      in
      Printf.sprintf "%s%s%s" (format_mono_type param_type) arrow (format_mono_type return_type)
  | Types.TTraitObject traits -> Printf.sprintf "Dyn[%s]" (String.concat " & " traits)
  | Types.TUnion types -> String.concat " | " (List.map format_mono_type types)
  | Types.TIntersection types -> String.concat " & " (List.map format_mono_type types)
  | Types.TEnum (name, []) -> name
  | Types.TEnum (name, args) -> Printf.sprintf "%s[%s]" name (String.concat ", " (List.map format_mono_type args))
  | Types.TNamed (name, []) -> name
  | Types.TNamed (name, args) ->
      Printf.sprintf "%s[%s]" name (String.concat ", " (List.map format_mono_type args))

(* ============================================================
   Phase 4.3: Tests for Enum Type Annotations
   ============================================================ *)

(* Test helper: register a test enum *)
let setup_test_enums () =
  Enum_registry.clear ();
  clear_type_aliases ();
  (* Register Option[a] *)
  Enum_registry.register
    {
      name = "Option";
      type_params = [ "a" ];
      variants = [ { name = "Some"; fields = [ Types.TVar "a" ] }; { name = "None"; fields = [] } ];
    };
  (* Register Result[a, b] *)
  Enum_registry.register
    {
      name = "Result";
      type_params = [ "a"; "b" ];
      variants = [ { name = "Success"; fields = [ Types.TVar "a" ] }; { name = "Failure"; fields = [ Types.TVar "b" ] } ];
    }

let%test "enum annotation Option[Int]" =
  setup_test_enums ();
  let te = Syntax.Ast.AST.TApp ("Option", [ Syntax.Ast.AST.TCon "Int" ]) in
  type_expr_to_mono_type te = Ok (Types.TEnum ("Option", [ Types.TInt ]))

let%test "enum annotation Result[Str, Int]" =
  setup_test_enums ();
  let te = Syntax.Ast.AST.TApp ("Result", [ Syntax.Ast.AST.TCon "Str"; Syntax.Ast.AST.TCon "Int" ]) in
  type_expr_to_mono_type te = Ok (Types.TEnum ("Result", [ Types.TString; Types.TInt ]))

let%test "enum annotation nested Option[List[Int]]" =
  setup_test_enums ();
  let te = Syntax.Ast.AST.TApp ("Option", [ Syntax.Ast.AST.TApp ("List", [ Syntax.Ast.AST.TCon "Int" ]) ]) in
  type_expr_to_mono_type te = Ok (Types.TEnum ("Option", [ Types.TArray Types.TInt ]))

let%test "canonical primitive annotation Int" = type_expr_to_mono_type (Syntax.Ast.AST.TCon "Int") = Ok Types.TInt

let%test "canonical primitive annotation Str" =
  type_expr_to_mono_type (Syntax.Ast.AST.TCon "Str") = Ok Types.TString

let%test "canonical List annotation" =
  let te = Syntax.Ast.AST.TApp ("List", [ Syntax.Ast.AST.TCon "Int" ]) in
  type_expr_to_mono_type te = Ok (Types.TArray Types.TInt)

let%test "Dyn trait object annotation converts to TTraitObject" =
  type_expr_to_mono_type (Syntax.Ast.AST.TTraitObject [ "Show"; "Eq" ]) = Ok (Types.TTraitObject [ "Eq"; "Show" ])

let%test "canonical Map annotation" =
  let te = Syntax.Ast.AST.TApp ("Map", [ Syntax.Ast.AST.TCon "Str"; Syntax.Ast.AST.TCon "Int" ]) in
  type_expr_to_mono_type te = Ok (Types.THash (Types.TString, Types.TInt))

let%test "canonical builtin enum annotation Option[Int]" =
  setup_test_enums ();
  let te = Syntax.Ast.AST.TApp ("Option", [ Syntax.Ast.AST.TCon "Int" ]) in
  type_expr_to_mono_type te = Ok (Types.TEnum ("Option", [ Types.TInt ]))

let%test "unknown enum Foo[Int] fails" =
  setup_test_enums ();
  let te = Syntax.Ast.AST.TApp ("Foo", [ Syntax.Ast.AST.TCon "Int" ]) in
  match type_expr_to_mono_type te with
  | Error d -> String.length d.message > 0
  | Ok _ -> false

let%test "Option with wrong arity fails" =
  setup_test_enums ();
  let te = Syntax.Ast.AST.TApp ("Option", [ Syntax.Ast.AST.TCon "Int"; Syntax.Ast.AST.TCon "Str" ]) in
  match type_expr_to_mono_type te with
  | Error d -> String.length d.message > 0
  | Ok _ -> false

let%test "open row in record annotation is rejected" =
  let te =
    Syntax.Ast.AST.TRecord
      ( [ { Syntax.Ast.AST.field_name = "x"; field_type = Syntax.Ast.AST.TCon "Int" } ],
        Some (Syntax.Ast.AST.TCon "r") )
  in
  match type_expr_to_mono_type te with
  | Error d -> d.code = "type-open-row-rejected"
  | Ok _ -> false

let%test "closed record annotation still works" =
  let te =
    Syntax.Ast.AST.TRecord ([ { Syntax.Ast.AST.field_name = "x"; field_type = Syntax.Ast.AST.TCon "Int" } ], None)
  in
  type_expr_to_mono_type te = Ok (Types.TRecord ([ { Types.name = "x"; typ = Types.TInt } ], None))

let%test "transparent type annotation resolves non-generic type" =
  clear_type_aliases ();
  register_type_alias
    {
      Syntax.Ast.AST.alias_name = "Point";
      alias_type_params = [];
      alias_body =
        Syntax.Ast.AST.TRecord
          ( [
              { field_name = "x"; field_type = Syntax.Ast.AST.TCon "Int" };
              { field_name = "y"; field_type = Syntax.Ast.AST.TCon "Int" };
            ],
            None );
    };
  type_expr_to_mono_type (Syntax.Ast.AST.TCon "Point")
  = Ok (Types.TRecord ([ { Types.name = "x"; typ = Types.TInt }; { Types.name = "y"; typ = Types.TInt } ], None))

let%test "transparent type annotation resolves generic type application" =
  clear_type_aliases ();
  register_type_alias
    {
      Syntax.Ast.AST.alias_name = "Box";
      alias_type_params = [ "a" ];
      alias_body =
        Syntax.Ast.AST.TRecord ([ { field_name = "value"; field_type = Syntax.Ast.AST.TCon "a" } ], None);
    };
  type_expr_to_mono_type (Syntax.Ast.AST.TApp ("Box", [ Syntax.Ast.AST.TCon "Str" ]))
  = Ok (Types.TRecord ([ { Types.name = "value"; typ = Types.TString } ], None))

let setup_trait_annotation_tests () =
  Trait_registry.clear ();
  Trait_registry.register_trait
    { trait_name = "Named"; trait_type_param = None; trait_supertraits = []; trait_methods = [] };
  Trait_registry.register_trait
    {
      trait_name = "Show";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [
          Trait_registry.mk_method_sig ~name:"show"
            ~params:[ ("x", Types.TVar "a") ]
            ~return_type:Types.TString ();
        ];
    }

let%test "trait annotation is rejected in type position" =
  setup_trait_annotation_tests ();
  match type_expr_to_mono_type (Syntax.Ast.AST.TCon "Named") with
  | Error d ->
      Diagnostics.String_utils.contains_substring ~needle:"constrained type parameter or Dyn[...]" d.message
  | Ok _ -> false

let%test "method trait annotation is rejected in type position" =
  setup_trait_annotation_tests ();
  match type_expr_to_mono_type (Syntax.Ast.AST.TCon "Show") with
  | Error d ->
      Diagnostics.String_utils.contains_substring ~needle:"constrained type parameter or Dyn[...]" d.message
  | Ok _ -> false

let%test "trait type constructor application is rejected in type position" =
  setup_trait_annotation_tests ();
  match type_expr_to_mono_type (Syntax.Ast.AST.TApp ("Show", [ Syntax.Ast.AST.TCon "Int" ])) with
  | Error d ->
      Diagnostics.String_utils.contains_substring ~needle:"constrained type parameter or Dyn[...]" d.message
  | Ok _ -> false

let%test "is_subtype_of: TVar only matches same TVar name" =
  is_subtype_of (Types.TVar "a") (Types.TVar "a") && not (is_subtype_of (Types.TVar "a") (Types.TVar "b"))

let%test "is_subtype_of: concrete does not subtype unresolved TVar" =
  not (is_subtype_of Types.TInt (Types.TVar "a"))

let%test "intersection annotation rejects mixed Dyn and non-Dyn members" =
  match
    type_expr_to_mono_type
      (Syntax.Ast.AST.TIntersection [ Syntax.Ast.AST.TTraitObject [ "Show" ]; Syntax.Ast.AST.TCon "Int" ])
  with
  | Error d ->
      d.code = "type-annotation-invalid"
      && Diagnostics.String_utils.contains_substring ~needle:"Dyn[...]" d.message
      && Diagnostics.String_utils.contains_substring ~needle:"non-Dyn" d.message
  | Ok _ -> false

let%test "intersection annotation rejects general callable intersections" =
  match
    type_expr_to_mono_type
      (Syntax.Ast.AST.TIntersection
         [
           Syntax.Ast.AST.TArrow ([ Syntax.Ast.AST.TCon "Int" ], Syntax.Ast.AST.TCon "Int", false);
           Syntax.Ast.AST.TCon "Bool";
         ])
  with
  | Error d ->
      d.code = "type-annotation-invalid"
      && Diagnostics.String_utils.contains_substring ~needle:"callable intersections" d.message
  | Ok _ -> false

let%test "intersection annotation preserves compatible record members" =
  match
    type_expr_to_mono_type
      (Syntax.Ast.AST.TIntersection
         [
           Syntax.Ast.AST.TRecord ([ { field_name = "x"; field_type = Syntax.Ast.AST.TCon "Int" } ], None);
           Syntax.Ast.AST.TRecord ([ { field_name = "y"; field_type = Syntax.Ast.AST.TCon "Str" } ], None);
         ])
  with
  | Ok
      (Types.TIntersection
         [
           Types.TRecord ([ { Types.name = "x"; typ = Types.TInt } ], None);
           Types.TRecord ([ { Types.name = "y"; typ = Types.TString } ], None);
         ]) ->
      true
  | _ -> false

let%test "intersection annotation rejects unsupported non-record intersections" =
  match
    type_expr_to_mono_type
      (Syntax.Ast.AST.TIntersection
         [
           Syntax.Ast.AST.TCon "Int";
           Syntax.Ast.AST.TUnion [ Syntax.Ast.AST.TCon "Int"; Syntax.Ast.AST.TCon "Str" ];
         ])
  with
  | Error d ->
      d.code = "type-annotation-invalid"
      && Diagnostics.String_utils.contains_substring ~needle:"Unsupported intersection" d.message
  | _ -> false

let%test "is_subtype_of: concrete record satisfies record intersection" =
  let actual =
    Types.TRecord ([ { Types.name = "x"; typ = Types.TInt }; { Types.name = "y"; typ = Types.TString } ], None)
  in
  let expected =
    Types.TIntersection
      [
        Types.TRecord ([ { Types.name = "x"; typ = Types.TInt } ], None);
        Types.TRecord ([ { Types.name = "y"; typ = Types.TString } ], None);
      ]
  in
  is_subtype_of actual expected

let%test "is_subtype_of: intersection is subtype of one guaranteed member" =
  let actual =
    Types.TIntersection
      [
        Types.TRecord ([ { Types.name = "x"; typ = Types.TInt } ], None);
        Types.TRecord ([ { Types.name = "y"; typ = Types.TString } ], None);
      ]
  in
  let expected = Types.TRecord ([ { Types.name = "x"; typ = Types.TInt } ], None) in
  is_subtype_of actual expected

let%test "is_subtype_of: identical intersections are compatible" =
  let intersection =
    Types.TIntersection
      [
        Types.TRecord ([ { Types.name = "x"; typ = Types.TInt } ], None);
        Types.TRecord ([ { Types.name = "x"; typ = Types.TInt }; { Types.name = "y"; typ = Types.TString } ], None);
      ]
  in
  is_subtype_of intersection intersection

(* Phase 3: effectful function type annotation *)
let%test "Phase3: TArrow pure converts to TFun false" =
  match
    type_expr_to_mono_type
      (Syntax.Ast.AST.TArrow ([ Syntax.Ast.AST.TCon "Int" ], Syntax.Ast.AST.TCon "Int", false))
  with
  | Ok (Types.TFun (Types.TInt, Types.TInt, false)) -> true
  | _ -> false

let%test "Phase3: TArrow effectful converts to TFun true" =
  match
    type_expr_to_mono_type
      (Syntax.Ast.AST.TArrow ([ Syntax.Ast.AST.TCon "Int" ], Syntax.Ast.AST.TCon "Int", true))
  with
  | Ok (Types.TFun (Types.TInt, Types.TInt, true)) -> true
  | _ -> false

let%test "is_subtype_of: pure function is subtype of effectful function" =
  is_subtype_of (Types.tfun Types.TInt Types.TInt) (Types.tfun_eff Types.TInt Types.TInt)

let%test "is_subtype_of: effectful function is not subtype of pure function" =
  not (is_subtype_of (Types.tfun_eff Types.TInt Types.TInt) (Types.tfun Types.TInt Types.TInt))
