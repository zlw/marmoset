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
   Tests
   ============================================================ *)

let%test "builtin_types has all builtins" =
  let names = List.map fst builtin_types in
  List.for_all (fun name -> List.mem name names) [ "len"; "first"; "last"; "rest"; "push"; "puts" ]

let%test "prelude_env creates valid environment" =
  let env = prelude_env () in
  Infer.TypeEnv.cardinal env = 6
