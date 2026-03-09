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
let fresh_trait_object_row_counter = ref 0

let register_type_alias (alias_def : Syntax.Ast.AST.type_alias_def) : unit =
  Hashtbl.replace type_alias_registry alias_def.alias_name
    { alias_type_params = alias_def.alias_type_params; alias_body = alias_def.alias_body }

let clear_type_aliases () : unit =
  Hashtbl.clear type_alias_registry;
  fresh_trait_object_row_counter := 0

let lookup_type_alias (name : string) : type_alias_info option = Hashtbl.find_opt type_alias_registry name

let fresh_trait_object_row_var () : Types.mono_type =
  let n = !fresh_trait_object_row_counter in
  fresh_trait_object_row_counter := n + 1;
  Types.TRowVar (Printf.sprintf "trait_obj_row_%d" n)

let field_only_trait_object_type (trait_name : string) : (Types.mono_type, Diagnostic.t) result =
  let trait_chain = Trait_registry.trait_with_supertraits trait_name in
  let field_tbl : (string, Types.mono_type) Hashtbl.t = Hashtbl.create 8 in
  let merge_field (owner_trait : string) (field : Types.record_field_type) : (unit, Diagnostic.t) result =
    match Hashtbl.find_opt field_tbl field.name with
    | None ->
        Hashtbl.replace field_tbl field.name (Types.canonicalize_mono_type field.typ);
        Ok ()
    | Some existing_type ->
        let expected = Types.canonicalize_mono_type existing_type in
        let got = Types.canonicalize_mono_type field.typ in
        if expected <> got then
          Error
            (Diagnostic.error_no_span ~code:"type-annotation-invalid"
               ~message:
                 (Printf.sprintf
                    "Trait '%s' cannot be used as a type: field '%s' has conflicting types across supertraits (%s vs %s, from '%s')"
                    trait_name field.name (Types.to_string expected) (Types.to_string got) owner_trait))
        else
          Ok ()
  in
  let gather_trait_fields (name : string) : (unit, Diagnostic.t) result =
    match Trait_registry.trait_kind name with
    | Some Trait_registry.FieldOnly -> (
        let* trait_def =
          match Trait_registry.lookup_trait name with
          | Some def -> Ok def
          | None ->
              Error
                (Diagnostic.error_no_span ~code:"type-annotation-invalid"
                   ~message:("Unknown trait in registry: " ^ name))
        in
        if trait_def.trait_type_param <> None then
          Error
            (Diagnostic.error_no_span ~code:"type-annotation-invalid"
               ~message:
                 (Printf.sprintf
                    "Trait '%s' cannot be used as a type: generic field-only supertrait '%s' is not supported in this phase"
                    trait_name name))
        else
          match Trait_registry.lookup_trait_fields name with
          | None -> Ok ()
          | Some fields -> iter_result (merge_field name) fields)
    | Some Trait_registry.MethodOnly | Some Trait_registry.Mixed ->
        Error
          (Diagnostic.error_no_span ~code:"type-annotation-invalid"
             ~message:
               (Printf.sprintf
                  "Trait '%s' cannot be used as a type: supertrait '%s' requires methods and trait-object method dispatch is not supported in this phase"
                  trait_name name))
    | None ->
        Error
          (Diagnostic.error_no_span ~code:"type-annotation-invalid"
             ~message:("Unknown trait in supertrait closure: " ^ name))
  in
  let* () = iter_result gather_trait_fields trait_chain in
  let fields =
    Hashtbl.to_seq_keys field_tbl
    |> List.of_seq
    |> List.sort String.compare
    |> List.map (fun name ->
           match Hashtbl.find_opt field_tbl name with
           | Some typ -> { Types.name; typ }
           | None -> failwith "field_only_trait_object_type: impossible missing field")
  in
  Ok (Types.canonicalize_mono_type (Types.TRecord (fields, Some (fresh_trait_object_row_var ()))))

let type_position_error_for_constructor (name : string) : string =
  match Trait_registry.trait_kind name with
  | Some Trait_registry.FieldOnly ->
      Printf.sprintf "Trait '%s' cannot be used here as a type constructor with arguments" name
  | Some Trait_registry.MethodOnly | Some Trait_registry.Mixed ->
      Printf.sprintf
        "Trait '%s' cannot be used as a type: method and mixed trait objects are not supported in this phase" name
  | None -> "Unknown type constructor: " ^ name

let rec type_expr_to_mono_type_with
    (type_bindings : (string * Types.mono_type) list) (te : Syntax.Ast.AST.type_expr) :
    (Types.mono_type, Diagnostic.t) result =
  let ann_error msg = Error (Diagnostic.error_no_span ~code:"type-annotation-invalid" ~message:msg) in
  match te with
  | Syntax.Ast.AST.TVar name -> (
      match List.assoc_opt name type_bindings with
      | Some ty -> Ok ty
      | None -> Ok (Types.TVar name))
  | Syntax.Ast.AST.TCon name -> (
      match List.assoc_opt name type_bindings with
      | Some ty -> Ok ty
      | None -> (
          match name with
          | "int" -> Ok Types.TInt
          | "float" -> Ok Types.TFloat
          | "bool" -> Ok Types.TBool
          | "string" -> Ok Types.TString
          | "unit" -> Ok Types.TNull
          | other -> (
              match Enum_registry.lookup other with
              | Some enum_def ->
                  if enum_def.type_params = [] then
                    Ok (Types.TEnum (other, []))
                  else
                    ann_error (Printf.sprintf "Enum %s expects type arguments" other)
              | None -> (
                  match lookup_type_alias other with
                  | Some alias_info ->
                      if alias_info.alias_type_params = [] then
                        type_expr_to_mono_type_with type_bindings alias_info.alias_body
                      else
                        ann_error (Printf.sprintf "Type alias %s expects type arguments" other)
                  | None -> (
                      match Trait_registry.trait_kind other with
                      | Some Trait_registry.FieldOnly ->
                          let* trait_def =
                            match Trait_registry.lookup_trait other with
                            | Some def -> Ok def
                            | None -> ann_error ("Unknown trait in registry: " ^ other)
                          in
                          if trait_def.trait_type_param <> None then
                            ann_error
                              (Printf.sprintf
                                 "Trait '%s' cannot be used as a type: generic field-only trait objects are not supported in this phase"
                                 other)
                          else
                            field_only_trait_object_type other
                      | Some Trait_registry.MethodOnly | Some Trait_registry.Mixed ->
                          ann_error (type_position_error_for_constructor other)
                      | None -> ann_error (type_position_error_for_constructor other))))))
  | Syntax.Ast.AST.TApp (con_name, type_args) -> (
      let ann_error msg = Error (Diagnostic.error_no_span ~code:"type-annotation-invalid" ~message:msg) in
      let* arg_types = map_result (type_expr_to_mono_type_with type_bindings) type_args in
      match con_name with
      | "list" -> (
          match arg_types with
          | [ elem_type ] -> Ok (Types.TArray elem_type)
          | _ -> ann_error ("list type expects 1 argument, got " ^ string_of_int (List.length arg_types)))
      | "map" -> (
          match arg_types with
          | [ key_type; value_type ] -> Ok (Types.THash (key_type, value_type))
          | _ -> ann_error ("map type expects 2 arguments, got " ^ string_of_int (List.length arg_types)))
      | _ -> (
          match Enum_registry.lookup con_name with
          | Some enum_def ->
              let expected_arity = List.length enum_def.type_params in
              let actual_arity = List.length arg_types in
              if expected_arity <> actual_arity then
                ann_error
                  (Printf.sprintf "Enum %s expects %d type argument(s), got %d" con_name expected_arity
                     actual_arity)
              else
                Ok (Types.TEnum (con_name, arg_types))
          | None -> (
              match lookup_type_alias con_name with
              | Some alias_info ->
                  let expected_arity = List.length alias_info.alias_type_params in
                  let actual_arity = List.length arg_types in
                  if expected_arity <> actual_arity then
                    ann_error
                      (Printf.sprintf "Type alias %s expects %d type argument(s), got %d" con_name expected_arity
                         actual_arity)
                  else
                    let alias_bindings = List.combine alias_info.alias_type_params arg_types in
                    type_expr_to_mono_type_with (alias_bindings @ type_bindings) alias_info.alias_body
              | None ->
                  if Trait_registry.lookup_trait con_name <> None then
                    ann_error
                      (Printf.sprintf
                         "Trait '%s' cannot be used here as a type constructor with arguments in this phase"
                         con_name)
                  else
                    ann_error (type_position_error_for_constructor con_name))))
  | Syntax.Ast.AST.TArrow (param_types, return_type, _) ->
      let* param_mono = map_result (type_expr_to_mono_type_with type_bindings) param_types in
      let* return_mono = type_expr_to_mono_type_with type_bindings return_type in
      Ok (List.fold_right (fun param_type ret_type -> Types.tfun param_type ret_type) param_mono return_mono)
  | Syntax.Ast.AST.TUnion type_exprs ->
      let* mono_types = map_result (type_expr_to_mono_type_with type_bindings) type_exprs in
      Ok (Types.normalize_union mono_types)
  | Syntax.Ast.AST.TRecord (fields, row_var) -> (
      match row_var with
      | Some _ ->
          Error
            (Diagnostic.error_no_span ~code:"type-open-row-rejected"
               ~message:
                 "Open row variables (e.g., '...row') in type annotations are not supported in v1. Use a closed record type annotation (e.g., '{ x: int, y: int }') or omit the annotation.")
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
  | Types.TFun (p1, r1, _), Types.TFun (p2, r2, _) -> mono_types_equal p1 p2 && mono_types_equal r1 r2
  | Types.TUnion t1s, Types.TUnion t2s ->
      List.length t1s = List.length t2s && List.for_all2 mono_types_equal t1s t2s
  | Types.TEnum (name1, args1), Types.TEnum (name2, args2) ->
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
  (* Same primitive types *)
  | Types.TInt, Types.TInt -> true
  | Types.TFloat, Types.TFloat -> true
  | Types.TBool, Types.TBool -> true
  | Types.TString, Types.TString -> true
  | Types.TNull, Types.TNull -> true
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
  | Types.TFun (p1, r1, _), Types.TFun (p2, r2, _) -> is_subtype_of p2 p1 && is_subtype_of r1 r2
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
  | Types.TInt -> "int"
  | Types.TFloat -> "float"
  | Types.TBool -> "bool"
  | Types.TString -> "string"
  | Types.TNull -> "unit"
  | Types.TArray elem_type -> Printf.sprintf "list[%s]" (format_mono_type elem_type)
  | Types.THash (key_type, value_type) ->
      Printf.sprintf "map[%s, %s]" (format_mono_type key_type) (format_mono_type value_type)
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
  | Types.TUnion types -> String.concat " | " (List.map format_mono_type types)
  | Types.TEnum (name, []) -> name
  | Types.TEnum (name, args) -> Printf.sprintf "%s[%s]" name (String.concat ", " (List.map format_mono_type args))

(* ============================================================
   Phase 4.3: Tests for Enum Type Annotations
   ============================================================ *)

(* Test helper: register a test enum *)
let setup_test_enums () =
  Enum_registry.clear ();
  clear_type_aliases ();
  (* Register option[a] *)
  Enum_registry.register
    {
      name = "option";
      type_params = [ "a" ];
      variants = [ { name = "some"; fields = [ Types.TVar "a" ] }; { name = "none"; fields = [] } ];
    };
  (* Register result[a, b] *)
  Enum_registry.register
    {
      name = "result";
      type_params = [ "a"; "b" ];
      variants = [ { name = "ok"; fields = [ Types.TVar "a" ] }; { name = "err"; fields = [ Types.TVar "b" ] } ];
    }

let%test "enum annotation option[int]" =
  setup_test_enums ();
  let te = Syntax.Ast.AST.TApp ("option", [ Syntax.Ast.AST.TCon "int" ]) in
  type_expr_to_mono_type te = Ok (Types.TEnum ("option", [ Types.TInt ]))

let%test "enum annotation result[string, int]" =
  setup_test_enums ();
  let te = Syntax.Ast.AST.TApp ("result", [ Syntax.Ast.AST.TCon "string"; Syntax.Ast.AST.TCon "int" ]) in
  type_expr_to_mono_type te = Ok (Types.TEnum ("result", [ Types.TString; Types.TInt ]))

let%test "enum annotation nested option[list[int]]" =
  setup_test_enums ();
  let te = Syntax.Ast.AST.TApp ("option", [ Syntax.Ast.AST.TApp ("list", [ Syntax.Ast.AST.TCon "int" ]) ]) in
  type_expr_to_mono_type te = Ok (Types.TEnum ("option", [ Types.TArray Types.TInt ]))

let%test "unknown enum foo[int] fails" =
  setup_test_enums ();
  let te = Syntax.Ast.AST.TApp ("foo", [ Syntax.Ast.AST.TCon "int" ]) in
  match type_expr_to_mono_type te with
  | Error d -> String.length d.message > 0
  | Ok _ -> false

let%test "option with wrong arity fails" =
  setup_test_enums ();
  let te = Syntax.Ast.AST.TApp ("option", [ Syntax.Ast.AST.TCon "int"; Syntax.Ast.AST.TCon "string" ]) in
  match type_expr_to_mono_type te with
  | Error d -> String.length d.message > 0
  | Ok _ -> false

let%test "open row in record annotation is rejected" =
  let te =
    Syntax.Ast.AST.TRecord
      ( [ { Syntax.Ast.AST.field_name = "x"; field_type = Syntax.Ast.AST.TCon "int" } ],
        Some (Syntax.Ast.AST.TCon "r") )
  in
  match type_expr_to_mono_type te with
  | Error d -> d.code = "type-open-row-rejected"
  | Ok _ -> false

let%test "closed record annotation still works" =
  let te =
    Syntax.Ast.AST.TRecord ([ { Syntax.Ast.AST.field_name = "x"; field_type = Syntax.Ast.AST.TCon "int" } ], None)
  in
  type_expr_to_mono_type te = Ok (Types.TRecord ([ { Types.name = "x"; typ = Types.TInt } ], None))

let%test "type alias annotation resolves non-generic alias" =
  clear_type_aliases ();
  register_type_alias
    {
      Syntax.Ast.AST.alias_name = "point";
      alias_type_params = [];
      alias_body =
        Syntax.Ast.AST.TRecord
          ( [
              { field_name = "x"; field_type = Syntax.Ast.AST.TCon "int" };
              { field_name = "y"; field_type = Syntax.Ast.AST.TCon "int" };
            ],
            None );
    };
  type_expr_to_mono_type (Syntax.Ast.AST.TCon "point")
  = Ok (Types.TRecord ([ { Types.name = "x"; typ = Types.TInt }; { Types.name = "y"; typ = Types.TInt } ], None))

let%test "type alias annotation resolves generic alias application" =
  clear_type_aliases ();
  register_type_alias
    {
      Syntax.Ast.AST.alias_name = "box";
      alias_type_params = [ "a" ];
      alias_body =
        Syntax.Ast.AST.TRecord ([ { field_name = "value"; field_type = Syntax.Ast.AST.TCon "a" } ], None);
    };
  type_expr_to_mono_type (Syntax.Ast.AST.TApp ("box", [ Syntax.Ast.AST.TCon "string" ]))
  = Ok (Types.TRecord ([ { Types.name = "value"; typ = Types.TString } ], None))

let setup_trait_object_annotation_tests () =
  Trait_registry.clear ();
  Trait_registry.register_trait
    { trait_name = "named"; trait_type_param = None; trait_supertraits = []; trait_methods = [] };
  Trait_registry.set_trait_fields "named" [ { Types.name = "name"; typ = Types.TString } ];
  Trait_registry.register_trait
    {
      trait_name = "show";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [
          Trait_registry.mk_method_sig ~name:"show"
            ~params:[ ("x", Types.TVar "a") ]
            ~return_type:Types.TString ();
        ];
    }

let%test "field-only trait annotation lowers to open record requirement" =
  setup_trait_object_annotation_tests ();
  match type_expr_to_mono_type (Syntax.Ast.AST.TCon "named") with
  | Ok (Types.TRecord ([ { Types.name = "name"; typ = Types.TString } ], Some (Types.TRowVar _))) -> true
  | _ -> false

let%test "method trait annotation is rejected in type position" =
  setup_trait_object_annotation_tests ();
  match type_expr_to_mono_type (Syntax.Ast.AST.TCon "show") with
  | Error d -> String.length d.message > 0 && String.contains d.message 'm'
  | Ok _ -> false

let%test "generic field-only supertrait is rejected in type position" =
  Trait_registry.clear ();
  Trait_registry.register_trait
    { trait_name = "tagged"; trait_type_param = Some "a"; trait_supertraits = []; trait_methods = [] };
  Trait_registry.set_trait_fields "tagged" [ { Types.name = "tag"; typ = Types.TVar "a" } ];
  Trait_registry.register_trait
    { trait_name = "tagged_like"; trait_type_param = None; trait_supertraits = [ "tagged" ]; trait_methods = [] };
  let contains_substring s sub = Diagnostics.String_utils.contains_substring ~needle:sub s in
  match type_expr_to_mono_type (Syntax.Ast.AST.TCon "tagged_like") with
  | Error d -> contains_substring d.message "generic field-only supertrait"
  | Ok _ -> false

let%test "is_subtype_of: TVar only matches same TVar name" =
  is_subtype_of (Types.TVar "a") (Types.TVar "a") && not (is_subtype_of (Types.TVar "a") (Types.TVar "b"))

let%test "is_subtype_of: concrete does not subtype unresolved TVar" =
  not (is_subtype_of Types.TInt (Types.TVar "a"))
