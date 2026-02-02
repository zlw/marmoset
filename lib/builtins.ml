(* ============================================================
   Builtin Type Signatures (for type inference)
   ============================================================
   
   These are the types for the prelude - the built-in functions
   available in every Marmoset program.
   
   Note: Some of these are more permissive than ideal because
   we don't have union types or type classes yet. For example,
   `len` works on both String and [a], but we type it as a -> Int.
   We'll tighten these when we add unions/traits.
*)

open Types

(* Helper to create a poly_type with quantified variables *)
let forall vars mono = Forall (vars, mono)

(* Helper for monomorphic types *)
let mono m = Forall ([], m)

(* Builtin type signatures *)
let builtin_types : (string * poly_type) list =
  [
    (* len : a -> Int
       Actually works on String and [a], but we use a -> Int for now.
       TODO: When we have unions: (String | [a]) -> Int *)
    ("len", forall [ "a" ] (TFun (TVar "a", TInt)));
    (* first : [a] -> a
       Returns first element of array, or Null if empty.
       TODO: Should return Option a or a? when we have those *)
    ("first", forall [ "a" ] (TFun (TArray (TVar "a"), TVar "a")));
    (* last : [a] -> a
       Returns last element of array, or Null if empty. *)
    ("last", forall [ "a" ] (TFun (TArray (TVar "a"), TVar "a")));
    (* rest : [a] -> [a]
       Returns all but the first element, or Null if empty.
       TODO: Should return [a] or Null - needs union types *)
    ("rest", forall [ "a" ] (TFun (TArray (TVar "a"), TArray (TVar "a"))));
    (* push : ([a], a) -> [a]
       Appends element to array, returns new array. *)
    ("push", forall [ "a" ] (TFun (TArray (TVar "a"), TFun (TVar "a", TArray (TVar "a")))));
    (* puts : a -> Null
       Prints any value to stdout, returns Null. *)
    ("puts", forall [ "a" ] (TFun (TVar "a", TNull)));
  ]

(* Create a type environment with all builtins *)
let prelude_env () : Infer.type_env =
  List.fold_left (fun env (name, poly) -> Infer.TypeEnv.add name poly env) Infer.empty_env builtin_types

(* ============================================================
   Builtin Implementations (for runtime)
   ============================================================ *)

let builtin_len args =
  match args with
  | [ Value.String s ] -> Value.Integer (Int64.of_int (String.length s))
  | [ Value.Array a ] -> Value.Integer (Int64.of_int (List.length a))
  | [ _ ] -> Error ("argument to `len` not supported, got " ^ Value.type_of (List.hd args))
  | _ ->
      let msg = Printf.sprintf "wrong number of arguments. got=%d, want=1" (List.length args) in
      Error msg

let builtin_first args =
  match args with
  | [ Value.Array a ] -> (
      match a with
      | [] -> Value.Null
      | _ -> List.hd a)
  | [ _ ] -> Error ("argument to `first` not supported, got " ^ Value.type_of (List.hd args))
  | _ ->
      let msg = Printf.sprintf "wrong number of arguments. got=%d, want=1" (List.length args) in
      Error msg

let builtin_last args =
  match args with
  | [ Value.Array a ] -> (
      match a with
      | [] -> Value.Null
      | _ -> List.hd (List.rev a))
  | [ _ ] -> Error ("argument to `last` not supported, got " ^ Value.type_of (List.hd args))
  | _ ->
      let msg = Printf.sprintf "wrong number of arguments. got=%d, want=1" (List.length args) in
      Error msg

let builtin_rest args =
  match args with
  | [ Value.Array a ] -> (
      match a with
      | [] -> Value.Null
      | _ -> Value.Array (List.tl a))
  | [ _ ] -> Error ("argument to `rest` not supported, got " ^ Value.type_of (List.hd args))
  | _ ->
      let msg = Printf.sprintf "wrong number of arguments. got=%d, want=1" (List.length args) in
      Error msg

let builtin_push args =
  match args with
  | [ Value.Array a; v ] -> Value.Array (a @ [ v ])
  | [ _; _ ] -> Error ("argument to `push` not supported, got " ^ Value.type_of (List.hd args))
  | _ ->
      let msg = Printf.sprintf "wrong number of arguments. got=%d, want=2" (List.length args) in
      Error msg

let builtin_puts args =
  List.iter (fun arg -> print_endline (Value.to_string arg)) args;
  Value.Null

let builtin = function
  | "len" -> Some builtin_len
  | "first" -> Some builtin_first
  | "last" -> Some builtin_last
  | "rest" -> Some builtin_rest
  | "push" -> Some builtin_push
  | "puts" -> Some builtin_puts
  | _ -> None

(* ============================================================
   Tests
   ============================================================ *)

let%test "builtin_types has all builtins" =
  let names = List.map fst builtin_types in
  List.for_all (fun name -> List.mem name names) [ "len"; "first"; "last"; "rest"; "push"; "puts" ]

let%test "prelude_env creates valid environment" =
  let env = prelude_env () in
  Infer.TypeEnv.cardinal env = 6
