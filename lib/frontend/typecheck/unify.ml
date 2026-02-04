(* Unification algorithm for Hindley-Milner type inference *)

open Types

(* Unification errors *)
type unify_error =
  | TypeMismatch of mono_type * mono_type (* Can't unify these two types *)
  | OccursCheck of string * mono_type (* Type variable occurs in the type (infinite type) *)

(* Convert error to human-readable string *)
let error_to_string = function
  | TypeMismatch (t1, t2) -> "Cannot unify " ^ to_string t1 ^ " with " ^ to_string t2
  | OccursCheck (var, mono) -> "Infinite type: " ^ var ^ " occurs in " ^ to_string mono

(* Main unification function.
   
   Given two types, find a substitution that makes them equal.
   Returns Ok substitution on success, Error on failure.
   
   The algorithm:
   1. If both types are identical, return empty substitution
   2. If one is a type variable, bind it to the other (with occurs check)
   3. If both are compound types, unify their parts
   4. Otherwise, the types don't match - return error
*)
let rec unify (type1 : mono_type) (type2 : mono_type) : (substitution, unify_error) result =
  match (type1, type2) with
  (* Identical types - nothing to do *)
  | TInt, TInt -> Ok empty_substitution
  | TFloat, TFloat -> Ok empty_substitution
  | TBool, TBool -> Ok empty_substitution
  | TString, TString -> Ok empty_substitution
  | TNull, TNull -> Ok empty_substitution
  (* Same type variable - nothing to do *)
  | TVar a, TVar b when a = b -> Ok empty_substitution
  (* Type variable on left - bind it *)
  | TVar var, other -> bind_variable var other
  (* Type variable on right - bind it *)
  | other, TVar var -> bind_variable var other
  (* Function types - unify argument and return types *)
  | TFun (arg1, ret1), TFun (arg2, ret2) -> unify_two_pairs (arg1, arg2) (ret1, ret2)
  (* Array types - unify element types *)
  | TArray elem1, TArray elem2 -> unify elem1 elem2
  (* Hash types - unify key and value types *)
  | THash (key1, val1), THash (key2, val2) -> unify_two_pairs (key1, key2) (val1, val2)
  (* Both unions - check equality (must come before single-sided union cases) *)
  | TUnion members1, TUnion members2 -> unify_union_with_union members1 members2
  (* Union on right - concrete type must match at least one member *)
  | concrete, TUnion members -> unify_concrete_with_union concrete members
  (* Union on left - same as right case (symmetrical) *)
  | TUnion members, concrete -> unify_concrete_with_union concrete members
  (* No match - types are incompatible *)
  | _, _ -> Error (TypeMismatch (type1, type2))

(* Helper: Check if concrete type matches any union member *)
and unify_concrete_with_union (concrete : mono_type) (members : mono_type list) :
    (substitution, unify_error) result =
  match members with
  | [] -> Error (TypeMismatch (concrete, TUnion members))
  | first :: rest -> (
      (* Try to unify with first member *)
      match unify concrete first with
      | Ok subst -> Ok subst
      | Error _ ->
          (* Failed, try remaining members *)
          unify_concrete_with_union concrete rest)

(* Helper: Unify two union types (all members of left must be in right) *)
and unify_union_with_union (members1 : mono_type list) (members2 : mono_type list) :
    (substitution, unify_error) result =
  (* For simplicity: unions must be equal *)
  (* Full subtyping would check if members1 ⊆ members2 *)
  if List.length members1 = List.length members2 then
    (* Try to unify element-wise *)
    let rec unify_all subst ms1 ms2 =
      match (ms1, ms2) with
      | [], [] -> Ok subst
      | m1 :: rest1, m2 :: rest2 -> (
          let m1' = apply_substitution subst m1 in
          let m2' = apply_substitution subst m2 in
          match unify m1' m2' with
          | Ok subst' ->
              let composed = compose_substitution subst subst' in
              unify_all composed rest1 rest2
          | Error e -> Error e)
      | _ -> Error (TypeMismatch (TUnion members1, TUnion members2))
    in
    unify_all empty_substitution members1 members2
  else
    Error (TypeMismatch (TUnion members1, TUnion members2))

(* Bind a type variable to a type.
   Performs the occurs check to prevent infinite types. *)
and bind_variable (var : string) (mono : mono_type) : (substitution, unify_error) result =
  (* Occurs check: make sure var doesn't appear in mono *)
  if occurs_in var mono then
    Error (OccursCheck (var, mono))
  else
    Ok [ (var, mono) ]

(* Unify two pairs of types and compose the resulting substitutions.
   Used for function types (arg, ret) and hash types (key, value). *)
and unify_two_pairs (t1a, t2a) (t1b, t2b) : (substitution, unify_error) result =
  (* First, unify the first pair *)
  match unify t1a t2a with
  | Error e -> Error e
  | Ok subst1 -> (
      (* Apply first substitution to second pair before unifying *)
      let t1b' = apply_substitution subst1 t1b in
      let t2b' = apply_substitution subst1 t2b in
      (* Unify the second pair *)
      match unify t1b' t2b' with
      | Error e -> Error e
      | Ok subst2 ->
          (* Compose the substitutions *)
          Ok (compose_substitution subst1 subst2))

(* ============================================================
   Tests
   ============================================================ *)

(* Helper to check if unification succeeds and produces expected substitution *)
let unifies_to type1 type2 expected_subst =
  match unify type1 type2 with
  | Error _ -> false
  | Ok subst ->
      (* Check that applying the substitution makes both types equal *)
      let t1_applied = apply_substitution subst type1 in
      let t2_applied = apply_substitution subst type2 in
      t1_applied = t2_applied
      (* Also check we got the expected substitution (order matters for our tests) *)
      && subst = expected_subst

(* Helper to check if unification fails *)
let fails_to_unify type1 type2 =
  match unify type1 type2 with
  | Error _ -> true
  | Ok _ -> false

(* Tests for identical types *)
let%test "unify identical primitives" =
  unifies_to TInt TInt []
  && unifies_to TBool TBool []
  && unifies_to TString TString []
  && unifies_to TFloat TFloat []
  && unifies_to TNull TNull []

let%test "unify identical type variables" = unifies_to (TVar "a") (TVar "a") []

(* Tests for type variable binding *)
let%test "unify TVar with primitive" =
  unifies_to (TVar "a") TInt [ ("a", TInt) ] && unifies_to TInt (TVar "a") [ ("a", TInt) ]

let%test "unify two different TVars" =
  (* a unified with b binds a to b (left variable gets bound) *)
  unifies_to (TVar "a") (TVar "b") [ ("a", TVar "b") ]

(* Tests for compound types *)
let%test "unify simple functions" =
  (* (a -> b) unified with (Int -> Bool) *)
  let t1 = TFun (TVar "a", TVar "b") in
  let t2 = TFun (TInt, TBool) in
  match unify t1 t2 with
  | Error _ -> false
  | Ok subst -> apply_substitution subst (TVar "a") = TInt && apply_substitution subst (TVar "b") = TBool

let%test "unify functions with shared variable" =
  (* (a -> a) unified with (Int -> b) should give {a -> Int, b -> Int} *)
  let t1 = TFun (TVar "a", TVar "a") in
  let t2 = TFun (TInt, TVar "b") in
  match unify t1 t2 with
  | Error _ -> false
  | Ok subst ->
      let t1' = apply_substitution subst t1 in
      let t2' = apply_substitution subst t2 in
      t1' = t2' && t1' = TFun (TInt, TInt)

let%test "unify arrays" = unifies_to (TArray (TVar "a")) (TArray TInt) [ ("a", TInt) ]

let%test "unify hashes" =
  let t1 = THash (TVar "k", TVar "v") in
  let t2 = THash (TString, TInt) in
  match unify t1 t2 with
  | Error _ -> false
  | Ok subst -> apply_substitution subst (TVar "k") = TString && apply_substitution subst (TVar "v") = TInt

(* Tests for type mismatches *)
let%test "fail to unify different primitives" =
  fails_to_unify TInt TString && fails_to_unify TBool TInt && fails_to_unify TFloat TString

let%test "fail to unify function with primitive" = fails_to_unify (TFun (TInt, TBool)) TInt
let%test "fail to unify array with hash" = fails_to_unify (TArray TInt) (THash (TString, TInt))

(* Tests for occurs check *)
let%test "occurs check prevents infinite type" =
  (* Can't unify a with (a -> Int) - would create infinite type *)
  fails_to_unify (TVar "a") (TFun (TVar "a", TInt))

let%test "occurs check in nested structure" =
  (* Can't unify a with [a] *)
  fails_to_unify (TVar "a") (TArray (TVar "a"))

let%test "occurs check in hash" =
  (* Can't unify a with {a: Int} *)
  fails_to_unify (TVar "a") (THash (TVar "a", TInt))

(* More complex tests *)
let%test "unify nested functions" =
  (* (a -> b -> c) unified with (Int -> String -> Bool) *)
  let t1 = TFun (TVar "a", TFun (TVar "b", TVar "c")) in
  let t2 = TFun (TInt, TFun (TString, TBool)) in
  match unify t1 t2 with
  | Error _ -> false
  | Ok subst ->
      apply_substitution subst (TVar "a") = TInt
      && apply_substitution subst (TVar "b") = TString
      && apply_substitution subst (TVar "c") = TBool

let%test "unify higher-order function" =
  (* ((a -> b) -> a -> b) unified with ((Int -> String) -> Int -> c) *)
  let t1 = TFun (TFun (TVar "a", TVar "b"), TFun (TVar "a", TVar "b")) in
  let t2 = TFun (TFun (TInt, TString), TFun (TInt, TVar "c")) in
  match unify t1 t2 with
  | Error _ -> false
  | Ok subst ->
      let t1' = apply_substitution subst t1 in
      let t2' = apply_substitution subst t2 in
      t1' = t2' && apply_substitution subst (TVar "c") = TString

let%test "unify array of functions" =
  let t1 = TArray (TFun (TVar "a", TVar "a")) in
  let t2 = TArray (TFun (TInt, TVar "b")) in
  match unify t1 t2 with
  | Error _ -> false
  | Ok subst -> apply_substitution subst (TVar "a") = TInt && apply_substitution subst (TVar "b") = TInt

(* Union type unification tests *)

let%test "unify concrete with union member" =
  (* int unifies with int | string *)
  match unify TInt (TUnion [ TInt; TString ]) with
  | Ok _ -> true
  | Error _ -> false

let%test "unify union with concrete member" =
  (* int | string unifies with int *)
  match unify (TUnion [ TInt; TString ]) TInt with
  | Ok _ -> true
  | Error _ -> false

let%test "fail unify concrete not in union" =
  (* bool does NOT unify with int | string *)
  fails_to_unify TBool (TUnion [ TInt; TString ])

let%test "unify equal unions" =
  let union1 = TUnion [ TInt; TString ] in
  let union2 = TUnion [ TInt; TString ] in
  match unify union1 union2 with
  | Ok _ -> true
  | Error _ -> false

let%test "unify union with type variable" =
  (* 'a unifies with int | string, binding 'a to union *)
  match unify (TVar "a") (TUnion [ TInt; TString ]) with
  | Ok subst -> apply_substitution subst (TVar "a") = TUnion [ TInt; TString ]
  | Error _ -> false
