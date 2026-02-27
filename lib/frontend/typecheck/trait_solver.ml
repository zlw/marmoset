(* Trait Solver - Determines if a type implements a trait *)

module Types = Types
module Trait_registry = Trait_registry
module StringSet = Set.Make (String)

let dedupe_preserve_order (traits : string list) : string list =
  let rec go seen acc = function
    | [] -> List.rev acc
    | trait_name :: rest ->
        if StringSet.mem trait_name seen then
          go seen acc rest
        else
          go (StringSet.add trait_name seen) (trait_name :: acc) rest
  in
  go StringSet.empty [] traits

let expand_constraints_with_supertraits (constraints : string list) : string list =
  constraints
  |> List.fold_left
       (fun acc trait_name -> acc @ Trait_registry.trait_with_supertraits trait_name)
       []
  |> dedupe_preserve_order

let rec check_trait_with_supertraits (visited : StringSet.t) (typ : Types.mono_type) (trait_name : string) :
    (unit, string) result =
  if StringSet.mem trait_name visited then
    Ok ()
  else
    let visited' = StringSet.add trait_name visited in
    if not (Trait_registry.implements_trait trait_name typ) then
      let type_str = Types.to_string typ in
      Error (Printf.sprintf "Type %s does not implement trait %s" type_str trait_name)
    else
      match Trait_registry.lookup_trait trait_name with
      | None -> Ok ()
      | Some trait_def -> check_supertraits visited' typ trait_def.trait_supertraits

and check_supertraits (visited : StringSet.t) (typ : Types.mono_type) (traits : string list) : (unit, string) result =
  match traits with
  | [] -> Ok ()
  | trait_name :: rest -> (
      match check_trait_with_supertraits visited typ trait_name with
      | Ok () -> check_supertraits visited typ rest
      | Error _ as err -> err)

(* Check if a concrete type implements a trait *)
let implements_trait (typ : Types.mono_type) (trait_name : string) : bool =
  match check_trait_with_supertraits StringSet.empty typ trait_name with
  | Ok () -> true
  | Error _ -> false

(* Check if a type satisfies constraints, returning error if not *)
let check_constraints (typ : Types.mono_type) (constraints : string list) : (unit, string) result =
  let rec check traits =
    match traits with
    | [] -> Ok ()
    | trait :: rest ->
        (match check_trait_with_supertraits StringSet.empty typ trait with
        | Ok () -> check rest
        | Error _ as err -> err)
  in
  check constraints

(* Check if a type satisfies a list of trait constraints *)
let satisfies_constraints (typ : Types.mono_type) (constraints : string list) : bool =
  match check_constraints typ constraints with
  | Ok () -> true
  | Error _ -> false

(* Given a type variable's constraints, find what methods are available *)
let available_methods (constraints : string list) : (string * Trait_registry.trait_def) list =
  (* For each constraint (trait name), get the trait definition *)
  let expanded_constraints = expand_constraints_with_supertraits constraints in
  List.filter_map
    (fun trait_name ->
      match Trait_registry.lookup_trait trait_name with
      | Some trait_def -> Some (trait_name, trait_def)
      | None -> None)
    expanded_constraints

(* Check if a method is available for a type given constraints *)
let method_available (method_name : string) (constraints : string list) : string option =
  let expanded_constraints = expand_constraints_with_supertraits constraints in
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
  find_in_traits expanded_constraints

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
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = Types.TInt;
      impl_methods =
        [ { method_name = "show"; method_params = [ ("x", Types.TInt) ]; method_return_type = Types.TString } ];
    };
  Trait_registry.register_impl ~builtin:true
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
  Trait_registry.register_impl ~builtin:true
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

let%test "check_constraints enforces supertrait obligations transitively" =
  Trait_registry.clear ();
  Trait_registry.register_trait
    {
      trait_name = "eq";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [
          { method_name = "eq"; method_params = [ ("x", Types.TVar "a"); ("y", Types.TVar "a") ]; method_return_type = Types.TBool };
        ];
    };
  Trait_registry.register_trait
    {
      trait_name = "ord";
      trait_type_param = Some "a";
      trait_supertraits = [ "eq" ];
      trait_methods =
        [
          {
            method_name = "compare";
            method_params = [ ("x", Types.TVar "a"); ("y", Types.TVar "a") ];
            method_return_type = Types.TInt;
          };
        ];
    };
  Trait_registry.register_impl
    {
      impl_trait_name = "ord";
      impl_type_params = [];
      impl_for_type = Types.TString;
      impl_methods =
        [
          {
            method_name = "compare";
            method_params = [ ("x", Types.TString); ("y", Types.TString) ];
            method_return_type = Types.TInt;
          };
        ];
    };
  match check_constraints Types.TString [ "ord" ] with
  | Ok () -> false
  | Error msg -> String.length msg > 0
