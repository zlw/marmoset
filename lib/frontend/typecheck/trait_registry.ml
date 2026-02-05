(* Trait Registry - tracks defined traits, implementations, and derivations *)

open Types
module AST = Syntax.Ast.AST

(* A method signature in a trait *)
type method_sig = {
  method_name : string;
  method_params : (string * mono_type) list; (* param name and type *)
  method_return_type : mono_type;
}

(* A trait definition *)
type trait_def = {
  trait_name : string;
  trait_type_param : string option; (* trait show[a] has Some "a" *)
  trait_supertraits : string list; (* trait ord: eq has ["eq"] *)
  trait_methods : method_sig list;
}

(* A trait implementation *)
type impl_def = {
  impl_trait_name : string;
  impl_type_params : AST.generic_param list; (* impl eq[a: eq] has [{ name = "a"; constraints = ["eq"] }] *)
  impl_for_type : mono_type; (* The type being implemented for *)
  impl_methods : method_sig list; (* Actual method implementations *)
}

(* Global mutable registries *)
let trait_registry : (string, trait_def) Hashtbl.t = Hashtbl.create 16
let impl_registry : (string * mono_type, impl_def) Hashtbl.t = Hashtbl.create 32 (* key: (trait_name, for_type) *)

let clear () =
  Hashtbl.clear trait_registry;
  Hashtbl.clear impl_registry

(* Register a trait definition *)
let register_trait (def : trait_def) : unit = Hashtbl.replace trait_registry def.trait_name def

(* Register an impl block *)
let register_impl (def : impl_def) : unit =
  let key = (def.impl_trait_name, def.impl_for_type) in
  Hashtbl.replace impl_registry key def

(* Lookup a trait by name *)
let lookup_trait (name : string) : trait_def option = Hashtbl.find_opt trait_registry name

(* Lookup an impl for a specific trait and type *)
let lookup_impl (trait_name : string) (for_type : mono_type) : impl_def option =
  Hashtbl.find_opt impl_registry (trait_name, for_type)

(* Check if a type implements a trait (directly or through derivation) *)
let implements_trait (trait_name : string) (for_type : mono_type) : bool =
  match lookup_impl trait_name for_type with
  | Some _ -> true
  | None -> false

(* Validate that a trait definition is well-formed *)
let validate_trait_def (def : trait_def) : (unit, string) result =
  (* Check for duplicate method names *)
  let method_names = List.map (fun m -> m.method_name) def.trait_methods in
  let unique_names = List.sort_uniq String.compare method_names in
  if List.length unique_names <> List.length method_names then
    Error (Printf.sprintf "Trait '%s' has duplicate method names" def.trait_name)
  else
    (* Check that supertraits exist *)
    let missing_supertraits =
      List.filter (fun super_name -> lookup_trait super_name = None) def.trait_supertraits
    in
    if missing_supertraits <> [] then
      Error
        (Printf.sprintf "Trait '%s' references undefined supertraits: %s" def.trait_name
           (String.concat ", " missing_supertraits))
    else
      Ok ()

(* Validate that an impl matches its trait signature *)
let validate_impl (def : impl_def) : (unit, string) result =
  match lookup_trait def.impl_trait_name with
  | None -> Error (Printf.sprintf "Cannot implement undefined trait: %s" def.impl_trait_name)
  | Some trait_def ->
      (* Check that all trait methods are implemented *)
      let trait_method_names =
        List.map (fun m -> m.method_name) trait_def.trait_methods |> List.sort String.compare
      in
      let impl_method_names = List.map (fun m -> m.method_name) def.impl_methods |> List.sort String.compare in

      if trait_method_names <> impl_method_names then
        Error
          (Printf.sprintf "Impl for trait '%s' does not match trait signature (expected methods: %s, got: %s)"
             def.impl_trait_name
             (String.concat ", " trait_method_names)
             (String.concat ", " impl_method_names))
      else
        (* TODO: Check method signatures match (param types and return types) *)
        Ok ()

(* Initialize with built-in traits (if any) *)
let init_builtins () = clear ()
(* No built-in traits yet *)

(* Tests *)

let%test "register and lookup trait" =
  clear ();
  let show_trait =
    {
      trait_name = "show";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [ { method_name = "show"; method_params = [ ("x", TVar "a") ]; method_return_type = TString } ];
    }
  in
  register_trait show_trait;
  match lookup_trait "show" with
  | None -> false
  | Some def -> def.trait_name = "show" && List.length def.trait_methods = 1

let%test "validate_trait_def - duplicate methods" =
  clear ();
  let bad_trait =
    {
      trait_name = "bad";
      trait_type_param = None;
      trait_supertraits = [];
      trait_methods =
        [
          { method_name = "foo"; method_params = []; method_return_type = TInt };
          { method_name = "foo"; method_params = []; method_return_type = TString };
        ];
    }
  in
  match validate_trait_def bad_trait with
  | Ok () -> false
  | Error _ -> true

let%test "validate_trait_def - missing supertrait" =
  clear ();
  let bad_trait =
    {
      trait_name = "ord";
      trait_type_param = Some "a";
      trait_supertraits = [ "eq" ];
      (* eq not registered *)
      trait_methods = [ { method_name = "compare"; method_params = []; method_return_type = TInt } ];
    }
  in
  match validate_trait_def bad_trait with
  | Ok () -> false
  | Error _ -> true

let%test "register and lookup impl" =
  clear ();
  let show_trait =
    {
      trait_name = "show";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [ { method_name = "show"; method_params = [ ("x", TVar "a") ]; method_return_type = TString } ];
    }
  in
  register_trait show_trait;

  let show_for_int =
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ { method_name = "show"; method_params = [ ("x", TInt) ]; method_return_type = TString } ];
    }
  in
  register_impl show_for_int;

  match lookup_impl "show" TInt with
  | None -> false
  | Some def -> def.impl_trait_name = "show" && def.impl_for_type = TInt

let%test "implements_trait checks impl registry" =
  clear ();
  let show_trait =
    {
      trait_name = "show";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [ { method_name = "show"; method_params = [ ("x", TVar "a") ]; method_return_type = TString } ];
    }
  in
  register_trait show_trait;

  let show_for_int =
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ { method_name = "show"; method_params = [ ("x", TInt) ]; method_return_type = TString } ];
    }
  in
  register_impl show_for_int;

  implements_trait "show" TInt && not (implements_trait "show" TString)
