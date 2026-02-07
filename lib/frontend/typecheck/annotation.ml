(* Phase 2: Type Annotation Conversion and Checking
   
   This module converts parsed type expressions (AST.type_expr) to internal
   monomorphic types (Types.mono_type) for checking and integration with the
   Hindley-Milner type inference system.
   
   Key functions:
   - type_expr_to_mono_type: Convert parsed types to mono types
   - check_annotation: Verify annotation matches inferred type
   - extract_constraints: Extract trait constraints for Phase 3
*)

(* Convert a parsed type expression to an internal mono_type *)
type type_alias_info = {
  alias_type_params : string list;
  alias_body : Syntax.Ast.AST.type_expr;
}

let type_alias_registry : (string, type_alias_info) Hashtbl.t = Hashtbl.create 64

let register_type_alias (alias_def : Syntax.Ast.AST.type_alias_def) : unit =
  Hashtbl.replace type_alias_registry alias_def.alias_name
    { alias_type_params = alias_def.alias_type_params; alias_body = alias_def.alias_body }

let clear_type_aliases () : unit = Hashtbl.clear type_alias_registry
let lookup_type_alias (name : string) : type_alias_info option = Hashtbl.find_opt type_alias_registry name

let rec type_expr_to_mono_type_with
    (type_bindings : (string * Types.mono_type) list)
    (te : Syntax.Ast.AST.type_expr) : Types.mono_type =
  match te with
  | Syntax.Ast.AST.TVar name ->
      (* Type variables represent generic parameters *)
      (match List.assoc_opt name type_bindings with
      | Some ty -> ty
      | None -> Types.TVar name)
  | Syntax.Ast.AST.TCon name -> (
      match List.assoc_opt name type_bindings with
      | Some ty -> ty
      | None -> (
          match name with
          | "int" -> Types.TInt
          | "float" -> Types.TFloat
          | "bool" -> Types.TBool
          | "string" -> Types.TString
          | "unit" | "null" -> Types.TNull
          | other -> (
              match Enum_registry.lookup other with
              | Some enum_def ->
                  if enum_def.type_params = [] then
                    Types.TEnum (other, [])
                  else
                    failwith (Printf.sprintf "Enum %s expects type arguments" other)
              | None -> (
                  match lookup_type_alias other with
                  | Some alias_info ->
                      if alias_info.alias_type_params = [] then
                        type_expr_to_mono_type_with type_bindings alias_info.alias_body
                      else
                        failwith (Printf.sprintf "Type alias %s expects type arguments" other)
                  | None -> failwith ("Unknown type constructor: " ^ other)))))
  | Syntax.Ast.AST.TApp (con_name, type_args) -> (
      (* Generic type application: list[int], map[string, int], option[a] *)
      let arg_types = List.map (type_expr_to_mono_type_with type_bindings) type_args in
      match con_name with
      | "list" -> (
          match arg_types with
          | [ elem_type ] -> Types.TArray elem_type
          | _ -> failwith ("list type expects 1 argument, got " ^ string_of_int (List.length arg_types)))
      | "map" -> (
          match arg_types with
          | [ key_type; value_type ] -> Types.THash (key_type, value_type)
          | _ -> failwith ("map type expects 2 arguments, got " ^ string_of_int (List.length arg_types)))
      | _ -> (
          (* Phase 4.3: Check if this is a registered enum type *)
          match Enum_registry.lookup con_name with
          | Some enum_def ->
              (* Verify arity matches *)
              let expected_arity = List.length enum_def.type_params in
              let actual_arity = List.length arg_types in
              if expected_arity <> actual_arity then
                failwith
                  (Printf.sprintf "Enum %s expects %d type argument(s), got %d" con_name expected_arity
                     actual_arity)
              else
                Types.TEnum (con_name, arg_types)
          | None -> (
              match lookup_type_alias con_name with
              | Some alias_info ->
                  let expected_arity = List.length alias_info.alias_type_params in
                  let actual_arity = List.length arg_types in
                  if expected_arity <> actual_arity then
                    failwith
                      (Printf.sprintf "Type alias %s expects %d type argument(s), got %d" con_name expected_arity
                         actual_arity)
                  else
                    let alias_bindings = List.combine alias_info.alias_type_params arg_types in
                    type_expr_to_mono_type_with (alias_bindings @ type_bindings) alias_info.alias_body
              | None -> failwith ("Unknown type constructor: " ^ con_name))))
  | Syntax.Ast.AST.TArrow (param_types, return_type) ->
      (* Function type: (int, string) -> bool *)
      let param_mono = List.map (type_expr_to_mono_type_with type_bindings) param_types in
      let return_mono = type_expr_to_mono_type_with type_bindings return_type in
      (* Build nested function types: (a, b, c) -> d becomes a -> b -> c -> d *)
      List.fold_right (fun param_type ret_type -> Types.TFun (param_type, ret_type)) param_mono return_mono
  | Syntax.Ast.AST.TUnion type_exprs ->
      (* Union types (Phase 4.1): int | string | bool *)
      let mono_types = List.map (type_expr_to_mono_type_with type_bindings) type_exprs in
      Types.normalize_union mono_types
  | Syntax.Ast.AST.TRecord (fields, row_var) ->
      let field_types =
        List.map
          (fun (f : Syntax.Ast.AST.record_type_field) ->
            { Types.name = f.field_name; typ = type_expr_to_mono_type_with type_bindings f.field_type })
          fields
      in
      let row_type =
        match row_var with
        | None -> None
        | Some (Syntax.Ast.AST.TCon name) -> Some (Types.TRowVar name)
        | Some (Syntax.Ast.AST.TVar name) -> Some (Types.TRowVar name)
        | Some other -> Some (type_expr_to_mono_type_with type_bindings other)
      in
      Types.canonicalize_mono_type (Types.TRecord (field_types, row_type))

and type_expr_to_mono_type (te : Syntax.Ast.AST.type_expr) : Types.mono_type = type_expr_to_mono_type_with [] te

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
  | Types.TRecord (fields1, row1), Types.TRecord (fields2, row2) ->
      let compare_field_names a b = String.compare a.Types.name b.Types.name in
      let sort_fields fields = List.sort (fun a b -> compare_field_names a b) fields in
      let fields1' = sort_fields fields1 in
      let fields2' = sort_fields fields2 in
      List.length fields1' = List.length fields2'
      &&
      List.for_all2
        (fun f1 f2 -> f1.Types.name = f2.Types.name && mono_types_equal f1.Types.typ f2.Types.typ)
        fields1' fields2'
      &&
      (match (row1, row2) with
      | None, None -> true
      | Some r1, Some r2 -> mono_types_equal r1 r2
      | _ -> false)
  | Types.TRowVar r1, Types.TRowVar r2 -> r1 = r2
  | Types.TFun (p1, r1), Types.TFun (p2, r2) -> mono_types_equal p1 p2 && mono_types_equal r1 r2
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
  (* Type variables: use equality *)
  | Types.TVar a, Types.TVar b -> a = b
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
  | Types.TRecord (fields1, row1), Types.TRecord (fields2, row2) ->
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
        (match row2 with
        | Some _ -> true
        | None ->
            if has_extra_fields then
              match row1 with
              | Some _ -> false
              | None -> false
            else
              true)
  | Types.TRowVar a, Types.TRowVar b -> a = b
  (* Functions: contravariant in params, covariant in return *)
  | Types.TFun (p1, r1), Types.TFun (p2, r2) -> is_subtype_of p2 p1 && is_subtype_of r1 r2
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
  | Types.TNull -> "null"
  | Types.TArray elem_type -> Printf.sprintf "list[%s]" (format_mono_type elem_type)
  | Types.THash (key_type, value_type) ->
      Printf.sprintf "map[%s, %s]" (format_mono_type key_type) (format_mono_type value_type)
  | Types.TRecord (fields, row) ->
      let field_strs = List.map (fun (f : Types.record_field_type) -> f.name ^ ": " ^ format_mono_type f.typ) fields in
      let row_str =
        match row with
        | None -> ""
        | Some r -> if field_strs = [] then "..." ^ format_mono_type r else ", ..." ^ format_mono_type r
      in
      Printf.sprintf "{ %s%s }" (String.concat ", " field_strs) row_str
  | Types.TRowVar name -> name
  | Types.TFun (param_type, return_type) ->
      (* Format as a->b->c (right-associative) *)
      Printf.sprintf "%s -> %s" (format_mono_type param_type) (format_mono_type return_type)
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
  let result = type_expr_to_mono_type te in
  result = Types.TEnum ("option", [ Types.TInt ])

let%test "enum annotation result[string, int]" =
  setup_test_enums ();
  let te = Syntax.Ast.AST.TApp ("result", [ Syntax.Ast.AST.TCon "string"; Syntax.Ast.AST.TCon "int" ]) in
  let result = type_expr_to_mono_type te in
  result = Types.TEnum ("result", [ Types.TString; Types.TInt ])

let%test "enum annotation nested option[list[int]]" =
  setup_test_enums ();
  let te = Syntax.Ast.AST.TApp ("option", [ Syntax.Ast.AST.TApp ("list", [ Syntax.Ast.AST.TCon "int" ]) ]) in
  let result = type_expr_to_mono_type te in
  result = Types.TEnum ("option", [ Types.TArray Types.TInt ])

(* This test should fail with unknown type constructor *)
let%test "unknown enum foo[int] fails" =
  setup_test_enums ();
  let te = Syntax.Ast.AST.TApp ("foo", [ Syntax.Ast.AST.TCon "int" ]) in
  try
    let _ = type_expr_to_mono_type te in
    false (* Should have thrown *)
  with Failure msg -> String.length msg > 0 (* Should have error message *)

(* This test should fail with arity mismatch *)
let%test "option with wrong arity fails" =
  setup_test_enums ();
  let te = Syntax.Ast.AST.TApp ("option", [ Syntax.Ast.AST.TCon "int"; Syntax.Ast.AST.TCon "string" ]) in
  try
    let _ = type_expr_to_mono_type te in
    false (* Should have thrown *)
  with Failure msg -> String.length msg > 0 (* Should have error message *)

let%test "record annotation converts to record mono type" =
  let te =
    Syntax.Ast.AST.TRecord
      ([ { Syntax.Ast.AST.field_name = "x"; field_type = Syntax.Ast.AST.TCon "int" } ], Some (Syntax.Ast.AST.TCon "r"))
  in
  type_expr_to_mono_type te = Types.TRecord ([ { Types.name = "x"; typ = Types.TInt } ], Some (Types.TRowVar "r"))

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
  = Types.TRecord ([ { Types.name = "x"; typ = Types.TInt }; { Types.name = "y"; typ = Types.TInt } ], None)

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
  = Types.TRecord ([ { Types.name = "value"; typ = Types.TString } ], None)
