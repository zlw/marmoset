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

let canonical_type (t : mono_type) : mono_type = canonicalize_mono_type t

let clear () =
  Hashtbl.clear trait_registry;
  Hashtbl.clear impl_registry

(* Register a trait definition *)
let register_trait (def : trait_def) : unit = Hashtbl.replace trait_registry def.trait_name def

(* Register an impl block *)
let register_impl (def : impl_def) : unit =
  let canonical_for_type = canonical_type def.impl_for_type in
  let def' = { def with impl_for_type = canonical_for_type } in
  let key = (def'.impl_trait_name, def'.impl_for_type) in
  Hashtbl.replace impl_registry key def'

(* Lookup a trait by name *)
let lookup_trait (name : string) : trait_def option = Hashtbl.find_opt trait_registry name

(* Lookup an impl for a specific trait and type *)
let lookup_impl (trait_name : string) (for_type : mono_type) : impl_def option =
  Hashtbl.find_opt impl_registry (trait_name, canonical_type for_type)

(* Return all registered impls (manual and derived). *)
let all_impls () : impl_def list = Hashtbl.fold (fun _ impl acc -> impl :: acc) impl_registry []

(* Check if a type implements a trait (directly or through derivation) *)
let implements_trait (trait_name : string) (for_type : mono_type) : bool =
  match lookup_impl trait_name for_type with
  | Some _ -> true
  | None -> false

(* Lookup a method implementation for a type by method name *)
(* Returns: (trait_name, method_sig) option *)
let lookup_method (for_type : mono_type) (method_name : string) : (string * method_sig) option =
  let for_type' = canonical_type for_type in
  (* Search through all registered impls for this type *)
  let matching_impls =
    Hashtbl.fold
      (fun (_trait_name, impl_type) impl_def acc ->
        if impl_type = for_type' then
          impl_def :: acc
        else
          acc)
      impl_registry []
  in
  (* Search for the method in matching impls *)
  let rec find_method impls =
    match impls with
    | [] -> None
    | impl_def :: rest -> (
        match List.find_opt (fun m -> m.method_name = method_name) impl_def.impl_methods with
        | Some method_sig -> Some (impl_def.impl_trait_name, method_sig)
        | None -> find_method rest)
  in
  find_method matching_impls

(* Check if a trait can be auto-derived *)
let is_derivable (trait_name : string) : bool =
  (* For now, only specific traits are derivable *)
  match trait_name with
  | "eq" | "show" | "debug" | "ord" | "hash" -> true
  | _ -> false

(* Validate that a type can derive a trait *)
let can_derive (trait_name : string) (for_type : mono_type) : (unit, string) result =
  (* Check if trait exists *)
  match lookup_trait trait_name with
  | None -> Error (Printf.sprintf "Cannot derive undefined trait: %s" trait_name)
  | Some _ -> (
      if
        (* Check if trait is derivable *)
        not (is_derivable trait_name)
      then
        Error (Printf.sprintf "Trait '%s' cannot be auto-derived" trait_name)
      else
        (* Check if type is a valid target for derivation *)
        match for_type with
        | TInt | TBool | TString | TNull -> Ok () (* Primitives can always derive *)
        | TEnum _ -> Ok () (* Enums can derive if variants are derivable - we'll check this later *)
        | TArray _ | THash _ ->
            (* Arrays/hashes can derive eq/show if element types can derive *)
            Ok () (* TODO: Check element types recursively *)
        | TFun _ -> Error "Cannot derive traits for function types"
        | TVar _ -> Ok () (* Type vars can derive - will be checked at instantiation *)
        | TUnion _ -> Ok () (* Unions can derive if all members can derive *)
        | _ -> Ok ())

(* Auto-generate an impl for a derived trait *)
let generate_derived_impl (trait_name : string) (for_type : mono_type) : impl_def option =
  (* Look up the trait definition *)
  match lookup_trait trait_name with
  | None -> None
  | Some trait_def ->
      (* Generate method implementations based on trait and type *)
      (* For now, we'll just create stub implementations *)
      (* TODO: Generate actual implementations based on type structure *)
      let generated_methods =
        List.map
          (fun (m : method_sig) ->
            (* Substitute type parameter with concrete type *)
            let substitute_type (t : mono_type) : mono_type =
              match trait_def.trait_type_param with
              | None -> t
              | Some type_param -> apply_substitution [ (type_param, for_type) ] t
            in
            let params = List.map (fun (name, t) -> (name, substitute_type t)) m.method_params in
            let return_type = substitute_type m.method_return_type in
            { method_name = m.method_name; method_params = params; method_return_type = return_type })
          trait_def.trait_methods
      in
      Some
        {
          impl_trait_name = trait_name;
          impl_type_params = [];
          impl_for_type = for_type;
          impl_methods = generated_methods;
        }

(* Validate and register a derived implementation *)
let derive_impl (trait_name : string) (for_type : mono_type) : (unit, string) result =
  (* Validate that trait can be derived for this type *)
  match can_derive trait_name for_type with
  | Error msg -> Error msg
  | Ok () -> (
      (* Generate the impl *)
      match generate_derived_impl trait_name for_type with
      | None -> Error (Printf.sprintf "Failed to generate impl for trait '%s'" trait_name)
      | Some impl_def ->
          (* Register it *)
          register_impl impl_def;
          Ok ())

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
        (* Check each method signature matches *)
        let check_method_sig (trait_method : method_sig) (impl_method : method_sig) : (unit, string) result =
          (* Substitute trait type parameter with impl_for_type *)
          let substitute_type (t : mono_type) : mono_type =
            match trait_def.trait_type_param with
            | None -> t (* No type param, use as-is *)
            | Some type_param -> apply_substitution [ (type_param, def.impl_for_type) ] t
          in

          (* Check param count *)
          if List.length trait_method.method_params <> List.length impl_method.method_params then
            Error
              (Printf.sprintf "Method '%s': expected %d parameters, got %d" trait_method.method_name
                 (List.length trait_method.method_params)
                 (List.length impl_method.method_params))
          else
            (* Check param types *)
          let param_errors =
              List.map2
                (fun (_tname, ttype) (_iname, itype) ->
                  let expected_type = canonical_type (substitute_type ttype) in
                  let impl_type = canonical_type itype in
                  if expected_type = impl_type then
                    None
                  else
                    Some
                      (Printf.sprintf "parameter type mismatch: expected %s, got %s" (to_string expected_type)
                         (to_string impl_type)))
                trait_method.method_params impl_method.method_params
              |> List.filter_map (fun x -> x)
            in

            if param_errors <> [] then
              Error (Printf.sprintf "Method '%s': %s" trait_method.method_name (List.hd param_errors))
            else
              (* Check return type *)
              let expected_return = canonical_type (substitute_type trait_method.method_return_type) in
              let impl_return = canonical_type impl_method.method_return_type in
              if expected_return = impl_return then
                Ok ()
              else
                Error
                  (Printf.sprintf "Method '%s': return type mismatch: expected %s, got %s"
                     trait_method.method_name (to_string expected_return) (to_string impl_return))
        in

        (* Find each trait method in impl methods and validate *)
        let errors =
          List.filter_map
            (fun trait_method ->
              match List.find_opt (fun im -> im.method_name = trait_method.method_name) def.impl_methods with
              | None -> Some (Printf.sprintf "Method '%s' not implemented" trait_method.method_name)
              | Some impl_method -> (
                  match check_method_sig trait_method impl_method with
                  | Ok () -> None
                  | Error msg -> Some msg))
            trait_def.trait_methods
        in

        if errors <> [] then
          Error (String.concat "; " errors)
        else
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

let%test "lookup_method canonicalizes reordered record receiver" =
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
  register_impl
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TRecord ([ { name = "x"; typ = TInt }; { name = "y"; typ = TInt } ], None);
      impl_methods =
        [
          {
            method_name = "show";
            method_params = [ ("x", TRecord ([ { name = "x"; typ = TInt }; { name = "y"; typ = TInt } ], None)) ];
            method_return_type = TString;
          };
        ];
    };
  match lookup_method (TRecord ([ { name = "y"; typ = TInt }; { name = "x"; typ = TInt } ], None)) "show" with
  | Some ("show", _) -> true
  | _ -> false

(* Comprehensive impl validation tests *)

let%test "validate_impl - undefined trait" =
  clear ();
  let bad_impl =
    {
      impl_trait_name = "undefined";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ { method_name = "foo"; method_params = []; method_return_type = TInt } ];
    }
  in
  match validate_impl bad_impl with
  | Ok () -> false
  | Error msg ->
      (* Check the error message contains "Cannot implement undefined" *)
      String.length msg >= 18 && String.sub msg 0 18 = "Cannot implement u"

let%test "validate_impl - missing method" =
  clear ();
  let eq_trait =
    {
      trait_name = "eq";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [
          { method_name = "eq"; method_params = [ ("x", TVar "a"); ("y", TVar "a") ]; method_return_type = TBool };
        ];
    }
  in
  register_trait eq_trait;

  let bad_impl =
    { impl_trait_name = "eq"; impl_type_params = []; impl_for_type = TInt; impl_methods = [] (* No methods! *) }
  in
  match validate_impl bad_impl with
  | Ok () -> false
  | Error msg -> String.length msg > 0

let%test "validate_impl - extra method" =
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

  let bad_impl =
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods =
        [
          { method_name = "show"; method_params = [ ("x", TInt) ]; method_return_type = TString };
          { method_name = "extra"; method_params = []; method_return_type = TInt };
          (* Extra method *)
        ];
    }
  in
  match validate_impl bad_impl with
  | Ok () -> false
  | Error msg -> String.length msg > 0

let%test "validate_impl - wrong param count" =
  clear ();
  let eq_trait =
    {
      trait_name = "eq";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [
          {
            method_name = "eq";
            method_params = [ ("x", TVar "a"); ("y", TVar "a") ];
            (* 2 params *)
            method_return_type = TBool;
          };
        ];
    }
  in
  register_trait eq_trait;

  let bad_impl =
    {
      impl_trait_name = "eq";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ { method_name = "eq"; method_params = [ ("x", TInt) ]; method_return_type = TBool } ];
      (* Only 1 param! *)
    }
  in
  match validate_impl bad_impl with
  | Ok () -> false
  | Error msg -> String.length msg > 0

let%test "validate_impl - wrong param type" =
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

  let bad_impl =
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods =
        [ { method_name = "show"; method_params = [ ("x", TString) ]; method_return_type = TString } ];
      (* x should be TInt! *)
    }
  in
  match validate_impl bad_impl with
  | Ok () -> false
  | Error msg -> String.length msg > 0

let%test "validate_impl - wrong return type" =
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

  let bad_impl =
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ { method_name = "show"; method_params = [ ("x", TInt) ]; method_return_type = TInt } ];
      (* Should return TString! *)
    }
  in
  match validate_impl bad_impl with
  | Ok () -> false
  | Error msg -> String.length msg > 0

let%test "validate_impl - correct substitution" =
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

  let good_impl =
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ { method_name = "show"; method_params = [ ("x", TInt) ]; method_return_type = TString } ];
      (* TVar "a" -> TInt *)
    }
  in
  match validate_impl good_impl with
  | Ok () -> true
  | Error _ -> false

let%test "validate_impl - multiple methods" =
  clear ();
  let ord_trait =
    {
      trait_name = "ord";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [
          {
            method_name = "compare";
            method_params = [ ("x", TVar "a"); ("y", TVar "a") ];
            method_return_type = TInt;
          };
          {
            method_name = "less_than";
            method_params = [ ("x", TVar "a"); ("y", TVar "a") ];
            method_return_type = TBool;
          };
        ];
    }
  in
  register_trait ord_trait;

  let good_impl =
    {
      impl_trait_name = "ord";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods =
        [
          { method_name = "compare"; method_params = [ ("x", TInt); ("y", TInt) ]; method_return_type = TInt };
          { method_name = "less_than"; method_params = [ ("x", TInt); ("y", TInt) ]; method_return_type = TBool };
        ];
    }
  in
  match validate_impl good_impl with
  | Ok () -> true
  | Error _ -> false

(* Derive validation tests *)

let%test "is_derivable - eq is derivable" = is_derivable "eq"
let%test "is_derivable - show is derivable" = is_derivable "show"
let%test "is_derivable - custom trait not derivable" = not (is_derivable "my_custom_trait")

let%test "can_derive - undefined trait" =
  clear ();
  match can_derive "undefined" TInt with
  | Ok () -> false
  | Error msg -> String.length msg > 0

let%test "can_derive - non-derivable trait" =
  clear ();
  let custom_trait =
    {
      trait_name = "custom";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [ { method_name = "custom"; method_params = [ ("x", TVar "a") ]; method_return_type = TInt } ];
    }
  in
  register_trait custom_trait;
  match can_derive "custom" TInt with
  | Ok () -> false
  | Error msg -> String.sub msg 0 (min 6 (String.length msg)) = "Trait "

let%test "can_derive - primitive types" =
  clear ();
  let eq_trait =
    {
      trait_name = "eq";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [
          { method_name = "eq"; method_params = [ ("x", TVar "a"); ("y", TVar "a") ]; method_return_type = TBool };
        ];
    }
  in
  register_trait eq_trait;
  (* All primitives can derive eq *)
  match can_derive "eq" TInt with
  | Ok () -> (
      match can_derive "eq" TBool with
      | Ok () -> (
          match can_derive "eq" TString with
          | Ok () -> true
          | Error _ -> false)
      | Error _ -> false)
  | Error _ -> false

let%test "can_derive - function types fail" =
  clear ();
  let eq_trait =
    {
      trait_name = "eq";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [
          { method_name = "eq"; method_params = [ ("x", TVar "a"); ("y", TVar "a") ]; method_return_type = TBool };
        ];
    }
  in
  register_trait eq_trait;
  match can_derive "eq" (TFun (TInt, TInt)) with
  | Ok () -> false
  | Error msg -> String.length msg > 0

let%test "generate_derived_impl - eq for int" =
  clear ();
  let eq_trait =
    {
      trait_name = "eq";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [
          { method_name = "eq"; method_params = [ ("x", TVar "a"); ("y", TVar "a") ]; method_return_type = TBool };
        ];
    }
  in
  register_trait eq_trait;
  match generate_derived_impl "eq" TInt with
  | None -> false
  | Some impl_def ->
      impl_def.impl_trait_name = "eq" && impl_def.impl_for_type = TInt && List.length impl_def.impl_methods = 1

let%test "generate_derived_impl - show for string" =
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
  match generate_derived_impl "show" TString with
  | None -> false
  | Some impl_def ->
      impl_def.impl_trait_name = "show"
      && impl_def.impl_for_type = TString
      && List.length impl_def.impl_methods = 1
      && (List.hd impl_def.impl_methods).method_return_type = TString

let%test "derive_impl - registers impl" =
  clear ();
  let eq_trait =
    {
      trait_name = "eq";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [
          { method_name = "eq"; method_params = [ ("x", TVar "a"); ("y", TVar "a") ]; method_return_type = TBool };
        ];
    }
  in
  register_trait eq_trait;
  match derive_impl "eq" TInt with
  | Error _ -> false
  | Ok () -> (
      (* Check that impl was registered *)
      match lookup_impl "eq" TInt with
      | None -> false
      | Some _ -> implements_trait "eq" TInt)

let%test "derive_impl - fails for non-derivable" =
  clear ();
  let custom_trait =
    {
      trait_name = "custom";
      trait_type_param = None;
      trait_supertraits = [];
      trait_methods = [ { method_name = "foo"; method_params = []; method_return_type = TInt } ];
    }
  in
  register_trait custom_trait;
  match derive_impl "custom" TInt with
  | Ok () -> false
  | Error _ -> true

let%test "derive_impl - type parameter substitution" =
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
  match derive_impl "show" TBool with
  | Error _ -> false
  | Ok () -> (
      match lookup_impl "show" TBool with
      | None -> false
      | Some impl_def ->
          (* Check that TVar "a" was substituted with TBool *)
          let method_impl = List.hd impl_def.impl_methods in
          let _param_name, param_type = List.hd method_impl.method_params in
          param_type = TBool && method_impl.method_return_type = TString)
