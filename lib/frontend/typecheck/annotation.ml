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
      | other -> failwith ("Unknown type constructor: " ^ other))
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

(* Check if annotation matches inferred type *)
let check_annotation (annot_type : Types.mono_type) (inferred_type : Types.mono_type) : bool =
  (* Phase 4.1: Use unification for union types *)
  (* This allows int to match int | string (widening) *)
  match Unify.unify annot_type inferred_type with
  | Ok _ -> true
  | Error _ -> false

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
