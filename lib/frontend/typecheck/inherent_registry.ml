(* Inherent Method Registry - tracks methods attached directly to concrete and generic types *)

open Types
module Trait_registry = Trait_registry

type method_sig = Trait_registry.method_sig

type generic_method_entry = {
  for_type_pattern : mono_type;
  method_sig : method_sig;
}

let concrete_registry : (mono_type * string, method_sig) Hashtbl.t = Hashtbl.create 64
let generic_registry : (mono_type * string, generic_method_entry) Hashtbl.t = Hashtbl.create 64

let clear () : unit =
  Hashtbl.clear concrete_registry;
  Hashtbl.clear generic_registry

let canonical_method_key (for_type : mono_type) (method_name : string) : mono_type * string =
  (canonicalize_mono_type for_type, method_name)

let canonicalize_method_sig (m : method_sig) : method_sig =
  {
    m with
    method_params = List.map (fun (name, ty) -> (name, canonicalize_mono_type ty)) m.method_params;
    method_return_type = canonicalize_mono_type m.method_return_type;
  }

let rec has_type_vars (t : mono_type) : bool =
  match t with
  | TVar _ | TRowVar _ -> true
  | TFun (arg, ret, _) -> has_type_vars arg || has_type_vars ret
  | TArray elem -> has_type_vars elem
  | THash (k, v) -> has_type_vars k || has_type_vars v
  | TRecord (fields, row) -> (
      List.exists (fun (f : Types.record_field_type) -> has_type_vars f.typ) fields
      ||
      match row with
      | None -> false
      | Some r -> has_type_vars r)
  | TTraitObject _ -> false
  | TEnum (_, args) | TUnion args | TIntersection args -> List.exists has_type_vars args
  | TInt | TFloat | TBool | TString | TNull -> false

let apply_subst_to_method_sig (subst : Types.substitution) (m : method_sig) : method_sig =
  (* Scope isolation: remove method-level generic names from subst *)
  let safe_subst = List.fold_left (fun s (name, _) -> Types.SubstMap.remove name s) subst m.method_generics in
  canonicalize_method_sig
    {
      m with
      method_params = List.map (fun (name, ty) -> (name, apply_substitution safe_subst ty)) m.method_params;
      method_return_type = apply_substitution safe_subst m.method_return_type;
    }

let register_method ~(for_type : mono_type) (method_sig : method_sig) : (unit, string) result =
  let for_type' = canonicalize_mono_type for_type in
  let method_sig' = canonicalize_method_sig method_sig in
  let key = (for_type', method_sig'.method_name) in
  if has_type_vars for_type' then (
    match Hashtbl.find_opt generic_registry key with
    | Some _ ->
        Error
          (Printf.sprintf "Duplicate inherent method '%s' for generic target %s" method_sig'.method_name
             (to_string for_type'))
    | None ->
        Hashtbl.replace generic_registry key { for_type_pattern = for_type'; method_sig = method_sig' };
        Ok ())
  else
    match Hashtbl.find_opt concrete_registry key with
    | Some _ ->
        Error
          (Printf.sprintf "Duplicate inherent method '%s' for type %s" method_sig'.method_name
             (to_string for_type'))
    | None ->
        Hashtbl.replace concrete_registry key method_sig';
        Ok ()

let remove_method ~(for_type : mono_type) (method_name : string) : unit =
  let for_type' = canonicalize_mono_type for_type in
  let key = (for_type', method_name) in
  Hashtbl.remove concrete_registry key;
  Hashtbl.remove generic_registry key

let resolve_method (for_type : mono_type) (method_name : string) : (method_sig option, string) result =
  let for_type' = canonicalize_mono_type for_type in
  let concrete_key = (for_type', method_name) in
  match Hashtbl.find_opt concrete_registry concrete_key with
  | Some method_sig -> Ok (Some method_sig)
  | None -> (
      let matches : (mono_type * method_sig) list =
        Hashtbl.fold
          (fun (pattern_type, registered_name) entry acc ->
            if registered_name <> method_name then
              acc
            else
              match Unify.unify pattern_type for_type' with
              | Error _ -> acc
              | Ok subst ->
                  let specialized = apply_subst_to_method_sig subst entry.method_sig in
                  (pattern_type, specialized) :: acc)
          generic_registry []
      in
      match matches with
      | [] -> Ok None
      | [ (_pattern, method_sig) ] -> Ok (Some method_sig)
      | many ->
          let matching_patterns =
            many
            |> List.map (fun (pattern, _method_sig) -> to_string pattern)
            |> List.sort_uniq String.compare
            |> String.concat ", "
          in
          Error
            (Printf.sprintf "Ambiguous inherent method '%s' for type %s (matching inherent impl targets: %s)"
               method_name (to_string for_type') matching_patterns))

let lookup_method (for_type : mono_type) (method_name : string) : method_sig option =
  match resolve_method for_type method_name with
  | Ok resolved -> resolved
  | Error _ -> None

let all_methods () : (mono_type * method_sig) list =
  let concrete =
    Hashtbl.fold
      (fun (for_type, _method_name) method_sig acc -> (for_type, method_sig) :: acc)
      concrete_registry []
  in
  Hashtbl.fold
    (fun (_for_type, _method_name) (entry : generic_method_entry) acc ->
      (entry.for_type_pattern, entry.method_sig) :: acc)
    generic_registry concrete

let%test "register and lookup inherent method" =
  clear ();
  let method_sig : method_sig =
    Trait_registry.mk_method_sig ~name:"sum" ~params:[ ("p", TInt) ] ~return_type:TInt ()
  in
  match register_method ~for_type:TInt method_sig with
  | Error _ -> false
  | Ok () -> (
      match lookup_method TInt "sum" with
      | Some m -> m.method_name = "sum" && m.method_return_type = TInt
      | None -> false)

let%test "register_method rejects duplicates for same concrete type and method name" =
  clear ();
  let method_sig : method_sig =
    Trait_registry.mk_method_sig ~name:"sum" ~params:[ ("p", TInt) ] ~return_type:TInt ()
  in
  match register_method ~for_type:TInt method_sig with
  | Error _ -> false
  | Ok () -> (
      match register_method ~for_type:TInt method_sig with
      | Ok () -> false
      | Error msg ->
          let needle = "Duplicate inherent method 'sum'" in
          let len_msg = String.length msg in
          let len_needle = String.length needle in
          let rec has_needle i =
            if i + len_needle > len_msg then
              false
            else if String.sub msg i len_needle = needle then
              true
            else
              has_needle (i + 1)
          in
          has_needle 0)

let%test "lookup_method canonicalizes record field ordering" =
  clear ();
  let for_type_registered = TRecord ([ { name = "x"; typ = TInt }; { name = "y"; typ = TInt } ], None) in
  let for_type_query = TRecord ([ { name = "y"; typ = TInt }; { name = "x"; typ = TInt } ], None) in
  let method_sig : method_sig =
    Trait_registry.mk_method_sig ~name:"sum" ~params:[ ("p", for_type_registered) ] ~return_type:TInt ()
  in
  match register_method ~for_type:for_type_registered method_sig with
  | Error _ -> false
  | Ok () -> Option.is_some (lookup_method for_type_query "sum")

let%test "generic inherent method resolves for concrete receiver" =
  clear ();
  let pattern = TEnum ("result", [ TVar "a"; TVar "b" ]) in
  let method_sig : method_sig =
    Trait_registry.mk_method_sig ~name:"is_success" ~params:[ ("r", pattern) ] ~return_type:TBool ()
  in
  match register_method ~for_type:pattern method_sig with
  | Error _ -> false
  | Ok () -> (
      match resolve_method (TEnum ("result", [ TInt; TString ])) "is_success" with
      | Error _ -> false
      | Ok None -> false
      | Ok (Some m) -> m.method_return_type = TBool)

let%test "concrete inherent method takes precedence over generic" =
  clear ();
  let generic_pattern = TEnum ("result", [ TVar "a"; TVar "b" ]) in
  let generic_sig : method_sig =
    Trait_registry.mk_method_sig ~name:"tag" ~params:[ ("r", generic_pattern) ] ~return_type:TString ()
  in
  let concrete_type = TEnum ("result", [ TInt; TString ]) in
  let concrete_sig : method_sig =
    Trait_registry.mk_method_sig ~name:"tag" ~params:[ ("r", concrete_type) ] ~return_type:TString ()
  in
  match register_method ~for_type:generic_pattern generic_sig with
  | Error _ -> false
  | Ok () -> (
      match register_method ~for_type:concrete_type concrete_sig with
      | Error _ -> false
      | Ok () -> (
          match resolve_method concrete_type "tag" with
          | Error _ -> false
          | Ok None -> false
          | Ok (Some m) -> m.method_params = concrete_sig.method_params))

let%test "ambiguous generic inherent methods are rejected at resolve-time" =
  clear ();
  let m1_pattern = TEnum ("result", [ TVar "a"; TVar "b" ]) in
  let m2_pattern = TEnum ("result", [ TVar "x"; TVar "y" ]) in
  let m1 : method_sig =
    Trait_registry.mk_method_sig ~name:"name" ~params:[ ("r", m1_pattern) ] ~return_type:TString ()
  in
  let m2 : method_sig =
    Trait_registry.mk_method_sig ~name:"name" ~params:[ ("r", m2_pattern) ] ~return_type:TString ()
  in
  match register_method ~for_type:m1_pattern m1 with
  | Error _ -> false
  | Ok () -> (
      match register_method ~for_type:m2_pattern m2 with
      | Error _ -> false
      | Ok () -> (
          match resolve_method (TEnum ("result", [ TInt; TString ])) "name" with
          | Ok _ -> false
          | Error msg ->
              let needle = "Ambiguous inherent method" in
              let len_msg = String.length msg in
              let len_needle = String.length needle in
              let rec has_needle i =
                if i + len_needle > len_msg then
                  false
                else if String.sub msg i len_needle = needle then
                  true
                else
                  has_needle (i + 1)
              in
              has_needle 0))
