(* Inherent Method Registry - tracks methods attached directly to concrete types *)

open Types
module Trait_registry = Trait_registry

type method_sig = Trait_registry.method_sig

let inherent_registry : ((mono_type * string), method_sig) Hashtbl.t = Hashtbl.create 64

let clear () : unit = Hashtbl.clear inherent_registry

let canonical_method_key (for_type : mono_type) (method_name : string) : mono_type * string =
  (canonicalize_mono_type for_type, method_name)

let register_method ~(for_type : mono_type) (method_sig : method_sig) : (unit, string) result =
  let for_type' = canonicalize_mono_type for_type in
  let key = (for_type', method_sig.method_name) in
  match Hashtbl.find_opt inherent_registry key with
  | Some _ ->
      Error
        (Printf.sprintf "Duplicate inherent method '%s' for type %s" method_sig.method_name
           (to_string for_type'))
  | None ->
      Hashtbl.replace inherent_registry key method_sig;
      Ok ()

let lookup_method (for_type : mono_type) (method_name : string) : method_sig option =
  let key = canonical_method_key for_type method_name in
  Hashtbl.find_opt inherent_registry key

let all_methods () : (mono_type * method_sig) list =
  Hashtbl.fold
    (fun (for_type, _method_name) method_sig acc -> (for_type, method_sig) :: acc)
    inherent_registry []

let%test "register and lookup inherent method" =
  clear ();
  let method_sig : method_sig =
    { method_name = "sum"; method_params = [ ("p", TInt) ]; method_return_type = TInt }
  in
  match register_method ~for_type:TInt method_sig with
  | Error _ -> false
  | Ok () -> (
      match lookup_method TInt "sum" with
      | Some m -> m.method_name = "sum" && m.method_return_type = TInt
      | None -> false)

let%test "register_method rejects duplicates for same type and method name" =
  clear ();
  let method_sig : method_sig =
    { method_name = "sum"; method_params = [ ("p", TInt) ]; method_return_type = TInt }
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
    {
      method_name = "sum";
      method_params = [ ("p", for_type_registered) ];
      method_return_type = TInt;
    }
  in
  match register_method ~for_type:for_type_registered method_sig with
  | Error _ -> false
  | Ok () -> Option.is_some (lookup_method for_type_query "sum")
