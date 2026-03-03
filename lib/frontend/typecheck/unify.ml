(* Unification algorithm for Hindley-Milner type inference *)

open Types
module Diagnostic = Diagnostics.Diagnostic

let type_mismatch (t1 : mono_type) (t2 : mono_type) : Diagnostic.t =
  Diagnostic.error_no_span ~code:"type-mismatch" ~message:("Cannot unify " ^ to_string t1 ^ " with " ^ to_string t2)

let occurs_check (var : string) (mono : mono_type) : Diagnostic.t =
  Diagnostic.error_no_span ~code:"type-occurs-check"
    ~message:("Infinite type: " ^ var ^ " occurs in " ^ to_string mono)

let fresh_row_counter = ref 0

let fresh_row_var () =
  let n = !fresh_row_counter in
  fresh_row_counter := n + 1;
  TRowVar ("row" ^ string_of_int n)

(* Main unification function.
   
   Given two types, find a substitution that makes them equal.
   Returns Ok substitution on success, Error on failure.
   
   The algorithm:
   1. If both types are identical, return empty substitution
   2. If one is a type variable, bind it to the other (with occurs check)
   3. If both are compound types, unify their parts
   4. Otherwise, the types don't match - return error
*)
let rec unify (type1 : mono_type) (type2 : mono_type) : (substitution, Diagnostic.t) result =
  match (type1, type2) with
  (* Identical types - nothing to do *)
  | TInt, TInt -> Ok empty_substitution
  | TFloat, TFloat -> Ok empty_substitution
  | TBool, TBool -> Ok empty_substitution
  | TString, TString -> Ok empty_substitution
  | TNull, TNull -> Ok empty_substitution
  | TRowVar a, TRowVar b when a = b -> Ok empty_substitution
  (* Same type variable - nothing to do *)
  | TVar a, TVar b when a = b -> Ok empty_substitution
  (* Row variable on left - bind it *)
  | TRowVar var, other -> bind_variable var other
  (* Row variable on right - bind it *)
  | other, TRowVar var -> bind_variable var other
  (* Type variable on left - bind it *)
  | TVar var, other -> bind_variable var other
  (* Type variable on right - bind it *)
  | other, TVar var -> bind_variable var other
  (* Function types - effects must match exactly. *)
  | TFun (arg1, ret1, eff1), TFun (arg2, ret2, eff2) ->
      if eff1 = eff2 then
        unify_two_pairs (arg1, arg2) (ret1, ret2)
      else
        Error (type_mismatch type1 type2)
  (* Array types - unify element types *)
  | TArray elem1, TArray elem2 -> unify elem1 elem2
  (* Hash types - unify key and value types *)
  | THash (key1, val1), THash (key2, val2) -> unify_two_pairs (key1, key2) (val1, val2)
  (* Record types - unify fields and row tails *)
  | TRecord (fields1, row1), TRecord (fields2, row2) -> unify_records fields1 row1 fields2 row2
  (* Enum types - unify name and all type arguments *)
  | TEnum (name1, args1), TEnum (name2, args2) ->
      if name1 <> name2 then
        Error (type_mismatch type1 type2)
      else if List.length args1 <> List.length args2 then
        Error (type_mismatch type1 type2)
      else
        (* Unify all type arguments *)
        unify_list args1 args2
  (* Both unions - check equality (must come before single-sided union cases) *)
  | TUnion members1, TUnion members2 -> unify_union_with_union members1 members2
  (* Union on right - concrete type must match at least one member (widening) *)
  | concrete, TUnion members -> unify_concrete_with_union concrete members
  (* Union on left - concrete on right: concrete must match at least one member *)
  (* This is symmetrical because unification is about finding substitution, not subtyping *)
  | TUnion members, concrete -> unify_concrete_with_union concrete members
  (* No match - types are incompatible *)
  | _, _ -> Error (type_mismatch type1 type2)

and unify_records
    (fields1 : record_field_type list)
    (row1 : mono_type option)
    (fields2 : record_field_type list)
    (row2 : mono_type option) : (substitution, Diagnostic.t) result =
  let table_of_fields (fields : record_field_type list) : (string, record_field_type) Hashtbl.t =
    let tbl = Hashtbl.create (List.length fields) in
    List.iter (fun (field : record_field_type) -> Hashtbl.replace tbl field.name field) fields;
    tbl
  in
  let fields1_by_name = table_of_fields fields1 in
  let fields2_by_name = table_of_fields fields2 in
  let common_pairs =
    List.filter_map
      (fun f1 ->
        match Hashtbl.find_opt fields2_by_name f1.name with
        | Some f2 -> Some (f1.typ, f2.typ)
        | None -> None)
      fields1
  in
  let only1 = List.filter (fun f -> not (Hashtbl.mem fields2_by_name f.name)) fields1 in
  let only2 = List.filter (fun f -> not (Hashtbl.mem fields1_by_name f.name)) fields2 in
  let rec unify_common subst = function
    | [] -> Ok subst
    | (t1, t2) :: rest -> (
        let t1' = apply_substitution subst t1 in
        let t2' = apply_substitution subst t2 in
        match unify t1' t2' with
        | Error e -> Error e
        | Ok subst2 ->
            let composed = compose_substitution subst subst2 in
            unify_common composed rest)
  in
  match unify_common empty_substitution common_pairs with
  | Error e -> Error e
  | Ok subst_common -> (
      let only1' = List.map (fun f -> { f with typ = apply_substitution subst_common f.typ }) only1 in
      let only2' = List.map (fun f -> { f with typ = apply_substitution subst_common f.typ }) only2 in
      let row1' = Option.map (apply_substitution subst_common) row1 in
      let row2' = Option.map (apply_substitution subst_common) row2 in
      let compose_with_common subst_tail = Ok (compose_substitution subst_common subst_tail) in
      match (row1', row2') with
      | None, None ->
          if only1' = [] && only2' = [] then
            Ok subst_common
          else
            Error (type_mismatch (TRecord (fields1, row1)) (TRecord (fields2, row2)))
      | Some r1, None -> (
          if only1' <> [] then
            Error (type_mismatch (TRecord (fields1, row1)) (TRecord (fields2, row2)))
          else
            match unify r1 (TRecord (only2', None)) with
            | Error e -> Error e
            | Ok subst2 -> compose_with_common subst2)
      | None, Some r2 -> (
          if only2' <> [] then
            Error (type_mismatch (TRecord (fields1, row1)) (TRecord (fields2, row2)))
          else
            match unify r2 (TRecord (only1', None)) with
            | Error e -> Error e
            | Ok subst2 -> compose_with_common subst2)
      | Some r1, Some r2 -> (
          if only1' = [] && only2' = [] then
            match unify r1 r2 with
            | Error e -> Error e
            | Ok subst2 -> compose_with_common subst2
          else if r1 = r2 then
            (* Avoid recursive row equations like {x, ...r} ~ {y, ...r}. *)
            Error (type_mismatch (TRecord (fields1, row1)) (TRecord (fields2, row2)))
          else if only1' = [] then
            match unify r1 (TRecord (only2', Some r2)) with
            | Error e -> Error e
            | Ok subst2 -> compose_with_common subst2
          else if only2' = [] then
            match unify r2 (TRecord (only1', Some r1)) with
            | Error e -> Error e
            | Ok subst2 -> compose_with_common subst2
          else
            let shared = fresh_row_var () in
            match unify r1 (TRecord (only2', Some shared)) with
            | Error e -> Error e
            | Ok subst2 -> (
                let r2' = apply_substitution subst2 r2 in
                let shared' = apply_substitution subst2 shared in
                let only1'' = List.map (fun f -> { f with typ = apply_substitution subst2 f.typ }) only1' in
                match unify r2' (TRecord (only1'', Some shared')) with
                | Error e -> Error e
                | Ok subst3 ->
                    let subst23 = compose_substitution subst2 subst3 in
                    compose_with_common subst23)))

(* Helper: Check if concrete type matches any union member *)
and unify_concrete_with_union (concrete : mono_type) (members : mono_type list) :
    (substitution, Diagnostic.t) result =
  match members with
  | [] -> Error (type_mismatch concrete (TUnion members))
  | first :: rest -> (
      (* Try to unify with first member *)
      match unify concrete first with
      | Ok subst -> Ok subst
      | Error _ ->
          (* Failed, try remaining members *)
          unify_concrete_with_union concrete rest)

(* Helper: ALL union members must unify with concrete type.
   This is for when a union is on the LEFT side - you can't assign string|int to a string slot
   unless all members are compatible with string (which is impossible for int). *)
and unify_union_all_with_concrete (members : mono_type list) (concrete : mono_type) :
    (substitution, Diagnostic.t) result =
  let rec unify_all subst = function
    | [] -> Ok subst
    | member :: rest -> (
        let member' = apply_substitution subst member in
        let concrete' = apply_substitution subst concrete in
        match unify member' concrete' with
        | Error _ -> Error (type_mismatch (TUnion members) concrete)
        | Ok subst' ->
            let composed = compose_substitution subst subst' in
            unify_all composed rest)
  in
  unify_all empty_substitution members

(* Helper: Unify two union types (all members of left must be in right) *)
and unify_union_with_union (members1 : mono_type list) (members2 : mono_type list) :
    (substitution, Diagnostic.t) result =
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
      | _ -> Error (type_mismatch (TUnion members1) (TUnion members2))
    in
    unify_all empty_substitution members1 members2
  else
    Error (type_mismatch (TUnion members1) (TUnion members2))

(* Bind a type variable to a type.
   Performs the occurs check to prevent infinite types. *)
and bind_variable (var : string) (mono : mono_type) : (substitution, Diagnostic.t) result =
  (* Occurs check: make sure var doesn't appear in mono *)
  if occurs_in var mono then
    Error (occurs_check var mono)
  else
    Ok [ (var, mono) ]

(* Unify two pairs of types and compose the resulting substitutions.
   Used for function types (arg, ret) and hash types (key, value). *)
and unify_two_pairs (t1a, t2a) (t1b, t2b) : (substitution, Diagnostic.t) result =
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

(* Unify two lists of types element-wise.
   Used for enum type arguments. *)
and unify_list (list1 : mono_type list) (list2 : mono_type list) : (substitution, Diagnostic.t) result =
  match (list1, list2) with
  | [], [] -> Ok empty_substitution
  | t1 :: rest1, t2 :: rest2 -> (
      match unify t1 t2 with
      | Error e -> Error e
      | Ok subst1 -> (
          (* Apply substitution to remaining elements *)
          let rest1' = List.map (apply_substitution subst1) rest1 in
          let rest2' = List.map (apply_substitution subst1) rest2 in
          (* Unify remaining elements *)
          match unify_list rest1' rest2' with
          | Error e -> Error e
          | Ok subst2 -> Ok (compose_substitution subst1 subst2)))
  | _, _ ->
      Error
        (Diagnostic.error_no_span ~code:"type-mismatch"
           ~message:
             (Printf.sprintf "Type argument list length mismatch: expected %d but got %d" (List.length list1)
                (List.length list2)))

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
  let t1 = tfun (TVar "a") (TVar "b") in
  let t2 = tfun TInt TBool in
  match unify t1 t2 with
  | Error _ -> false
  | Ok subst -> apply_substitution subst (TVar "a") = TInt && apply_substitution subst (TVar "b") = TBool

let%test "unify functions with shared variable" =
  (* (a -> a) unified with (Int -> b) should give {a -> Int, b -> Int} *)
  let t1 = tfun (TVar "a") (TVar "a") in
  let t2 = tfun TInt (TVar "b") in
  match unify t1 t2 with
  | Error _ -> false
  | Ok subst ->
      let t1' = apply_substitution subst t1 in
      let t2' = apply_substitution subst t2 in
      t1' = t2' && t1' = tfun TInt TInt

let%test "fail unify pure and effectful function types" = fails_to_unify (tfun TInt TInt) (tfun_eff TInt TInt)
let%test "unify arrays" = unifies_to (TArray (TVar "a")) (TArray TInt) [ ("a", TInt) ]

let%test "unify hashes" =
  let t1 = THash (TVar "k", TVar "v") in
  let t2 = THash (TString, TInt) in
  match unify t1 t2 with
  | Error _ -> false
  | Ok subst -> apply_substitution subst (TVar "k") = TString && apply_substitution subst (TVar "v") = TInt

let%test "unify closed records same fields" =
  let r1 = TRecord ([ { name = "x"; typ = TInt }; { name = "y"; typ = TString } ], None) in
  let r2 = TRecord ([ { name = "y"; typ = TString }; { name = "x"; typ = TInt } ], None) in
  match unify r1 r2 with
  | Ok _ -> true
  | Error _ -> false

let%test "unify open record with closed record" =
  let r1 = TRecord ([ { name = "x"; typ = TInt } ], Some (TRowVar "r")) in
  let r2 = TRecord ([ { name = "x"; typ = TInt }; { name = "y"; typ = TString } ], None) in
  match unify r1 r2 with
  | Ok subst -> apply_substitution subst (TRowVar "r") = TRecord ([ { name = "y"; typ = TString } ], None)
  | Error _ -> false

let%test "fail unify open records sharing row var with conflicting extra fields" =
  let row = Some (TRowVar "r") in
  let r1 = TRecord ([ { name = "x"; typ = TInt } ], row) in
  let r2 = TRecord ([ { name = "y"; typ = TString } ], row) in
  fails_to_unify r1 r2

let%test "fail unify missing required record field" =
  let r1 = TRecord ([ { name = "x"; typ = TInt }; { name = "y"; typ = TString } ], None) in
  let r2 = TRecord ([ { name = "x"; typ = TInt } ], None) in
  fails_to_unify r1 r2

(* Tests for type mismatches *)
let%test "fail to unify different primitives" =
  fails_to_unify TInt TString && fails_to_unify TBool TInt && fails_to_unify TFloat TString

let%test "fail to unify function with primitive" = fails_to_unify (tfun TInt TBool) TInt
let%test "fail to unify array with hash" = fails_to_unify (TArray TInt) (THash (TString, TInt))

(* Tests for occurs check *)
let%test "occurs check prevents infinite type" =
  (* Can't unify a with (a -> Int) - would create infinite type *)
  fails_to_unify (TVar "a") (tfun (TVar "a") TInt)

let%test "occurs check in nested structure" =
  (* Can't unify a with [a] *)
  fails_to_unify (TVar "a") (TArray (TVar "a"))

let%test "occurs check in hash" =
  (* Can't unify a with {a: Int} *)
  fails_to_unify (TVar "a") (THash (TVar "a", TInt))

(* More complex tests *)
let%test "unify nested functions" =
  (* (a -> b -> c) unified with (Int -> String -> Bool) *)
  let t1 = tfun (TVar "a") (tfun (TVar "b") (TVar "c")) in
  let t2 = tfun TInt (tfun TString TBool) in
  match unify t1 t2 with
  | Error _ -> false
  | Ok subst ->
      apply_substitution subst (TVar "a") = TInt
      && apply_substitution subst (TVar "b") = TString
      && apply_substitution subst (TVar "c") = TBool

let%test "unify higher-order function" =
  (* ((a -> b) -> a -> b) unified with ((Int -> String) -> Int -> c) *)
  let t1 = tfun (tfun (TVar "a") (TVar "b")) (tfun (TVar "a") (TVar "b")) in
  let t2 = tfun (tfun TInt TString) (tfun TInt (TVar "c")) in
  match unify t1 t2 with
  | Error _ -> false
  | Ok subst ->
      let t1' = apply_substitution subst t1 in
      let t2' = apply_substitution subst t2 in
      t1' = t2' && apply_substitution subst (TVar "c") = TString

let%test "unify array of functions" =
  let t1 = TArray (tfun (TVar "a") (TVar "a")) in
  let t2 = TArray (tfun TInt (TVar "b")) in
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
  (* int | string unifies with int (symmetrical) *)
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
