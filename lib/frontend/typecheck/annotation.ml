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
let rec type_expr_to_mono_type (te : Syntax.Ast.AST.type_expr) : Types.mono_type =
  match te with
  | Syntax.Ast.AST.TVar name ->
      (* Type variables represent generic parameters *)
      Types.TVar name
  | Syntax.Ast.AST.TCon name -> (
      (* Built-in type constructors *)
      match name with
      | "int" -> Types.TInt
      | "float" -> Types.TFloat
      | "bool" -> Types.TBool
      | "string" -> Types.TString
      | "unit" | "null" -> Types.TNull
      | other -> (
          (* Check if it's a registered enum type (like ordering) *)
          match Enum_registry.lookup other with
          | Some enum_def ->
              (* Non-generic enum *)
              if enum_def.type_params = [] then
                Types.TEnum (other, [])
              else
                failwith (Printf.sprintf "Enum %s expects type arguments" other)
          | None -> failwith ("Unknown type constructor: " ^ other)))
  | Syntax.Ast.AST.TApp (con_name, type_args) -> (
      (* Generic type application: list[int], map[string, int], option[a] *)
      let arg_types = List.map type_expr_to_mono_type type_args in
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
          | None -> failwith ("Unknown type constructor: " ^ con_name)))
  | Syntax.Ast.AST.TArrow (param_types, return_type) ->
      (* Function type: (int, string) -> bool *)
      let param_mono = List.map type_expr_to_mono_type param_types in
      let return_mono = type_expr_to_mono_type return_type in
      (* Build nested function types: (a, b, c) -> d becomes a -> b -> c -> d *)
      List.fold_right (fun param_type ret_type -> Types.TFun (param_type, ret_type)) param_mono return_mono
  | Syntax.Ast.AST.TUnion type_exprs ->
      (* Union types (Phase 4.1): int | string | bool *)
      let mono_types = List.map type_expr_to_mono_type type_exprs in
      Types.normalize_union mono_types
  | Syntax.Ast.AST.TRecord (_fields, _row_var) ->
      (* Phase 4.4: Record types - not yet implemented *)
      failwith "Record types not yet implemented"

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
