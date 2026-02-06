(* Trait Solver - Determines if a type implements a trait *)

module Types = Types
module Trait_registry = Trait_registry

(* Check if a concrete type implements a trait *)
let implements_trait (typ : Types.mono_type) (trait_name : string) : bool =
  (* First check direct implementations *)
  let direct_impl = Trait_registry.implements_trait trait_name typ in

  if direct_impl then
    true
  else
    (* Check if there's a supertrait chain *)
    (* If trait Tr has supertraits, we don't need those for this check *)
    (* We only care: does this TYPE have an impl for THIS TRAIT? *)
    false

(* Check if a type satisfies a list of trait constraints *)
let satisfies_constraints (typ : Types.mono_type) (constraints : string list) : bool =
  List.for_all (implements_trait typ) constraints

(* Check if a type satisfies constraints, returning error if not *)
let check_constraints (typ : Types.mono_type) (constraints : string list) : (unit, string) result =
  let rec check traits =
    match traits with
    | [] -> Ok ()
    | trait :: rest ->
        if implements_trait typ trait then
          check rest
        else
          let type_str = Types.to_string typ in
          Error (Printf.sprintf "Type %s does not implement trait %s" type_str trait)
  in
  check constraints

(* Given a type variable's constraints, find what methods are available *)
let available_methods (constraints : string list) : (string * Trait_registry.trait_def) list =
  (* For each constraint (trait name), get the trait definition *)
  List.filter_map
    (fun trait_name ->
      match Trait_registry.lookup_trait trait_name with
      | Some trait_def -> Some (trait_name, trait_def)
      | None -> None)
    constraints

(* Check if a method is available for a type given constraints *)
let method_available (method_name : string) (constraints : string list) : string option =
  (* Returns the trait name that provides this method, if any *)
  let rec find_in_traits traits =
    match traits with
    | [] -> None
    | trait_name :: rest -> (
        match Trait_registry.lookup_trait trait_name with
        | Some trait_def ->
            (* Check if this trait defines the method *)
            let has_method =
              List.exists
                (fun (method_def : Trait_registry.method_sig) -> method_def.method_name = method_name)
                trait_def.trait_methods
            in
            if has_method then
              Some trait_name
            else
              find_in_traits rest
        | None -> find_in_traits rest)
  in
  find_in_traits constraints

(* Inline tests *)
(* Helper to setup builtin traits and impls for tests.
   We can't call Builtins module directly due to circular dependency,
   so we inline the setup here. *)
let setup_builtins () =
  (* Register show trait *)
  Trait_registry.register_trait
    {
      trait_name = "show";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [
          { method_name = "show"; method_params = [ ("x", Types.TVar "a") ]; method_return_type = Types.TString };
        ];
    };
  (* Register eq trait *)
  Trait_registry.register_trait
    {
      trait_name = "eq";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [
          {
            method_name = "eq";
            method_params = [ ("x", Types.TVar "a"); ("y", Types.TVar "a") ];
            method_return_type = Types.TBool;
          };
        ];
    };
  (* Register impls for int *)
  Trait_registry.register_impl
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = Types.TInt;
      impl_methods =
        [ { method_name = "show"; method_params = [ ("x", Types.TInt) ]; method_return_type = Types.TString } ];
    };
  Trait_registry.register_impl
    {
      impl_trait_name = "eq";
      impl_type_params = [];
      impl_for_type = Types.TInt;
      impl_methods =
        [
          {
            method_name = "eq";
            method_params = [ ("x", Types.TInt); ("y", Types.TInt) ];
            method_return_type = Types.TBool;
          };
        ];
    };
  (* Register impl for string *)
  Trait_registry.register_impl
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = Types.TString;
      impl_methods =
        [ { method_name = "show"; method_params = [ ("x", Types.TString) ]; method_return_type = Types.TString } ];
    }

let%test "int implements show (builtin)" =
  setup_builtins ();
  implements_trait Types.TInt "show"

let%test "int implements eq (builtin)" =
  setup_builtins ();
  implements_trait Types.TInt "eq"

let%test "string implements show (builtin)" =
  setup_builtins ();
  implements_trait Types.TString "show"

let%test "array does not implement show" =
  setup_builtins ();
  not (implements_trait (Types.TArray Types.TInt) "show")

let%test "satisfies_constraints - int with show+eq" =
  setup_builtins ();
  satisfies_constraints Types.TInt [ "show"; "eq" ]

let%test "satisfies_constraints - array fails show" =
  setup_builtins ();
  not (satisfies_constraints (Types.TArray Types.TInt) [ "show" ])

let%test "check_constraints success" =
  setup_builtins ();
  match check_constraints Types.TInt [ "show"; "eq" ] with
  | Ok () -> true
  | Error _ -> false

let%test "check_constraints failure" =
  setup_builtins ();
  match check_constraints (Types.TArray Types.TInt) [ "show" ] with
  | Ok () -> false
  | Error msg -> String.length msg > 0

let%test "method_available - show method in show trait" =
  setup_builtins ();
  method_available "show" [ "show" ] = Some "show"

let%test "method_available - eq method in eq trait" =
  setup_builtins ();
  method_available "eq" [ "eq" ] = Some "eq"

let%test "method_available - not found" =
  setup_builtins ();
  method_available "nonexistent" [ "show" ] = None
