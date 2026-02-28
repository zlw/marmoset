(* Trait Solver - Determines if a type implements a trait *)

module Types = Types
module Trait_registry = Trait_registry
module Unify = Unify
module AST = Syntax.Ast.AST
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
  |> List.fold_left (fun acc trait_name -> acc @ Trait_registry.trait_with_supertraits trait_name) []
  |> dedupe_preserve_order

let check_trait_fields (typ : Types.mono_type) (trait_name : string) : (unit, string) result =
  match Trait_registry.lookup_trait_fields trait_name with
  | None | Some [] -> Ok ()
  | Some required_fields -> (
      let type_str = Types.to_string typ in
      match typ with
      | Types.TRecord (actual_fields, _row) ->
          let rec check_required = function
            | [] -> Ok ()
            | (required : Types.record_field_type) :: rest -> (
                match
                  List.find_opt (fun (f : Types.record_field_type) -> f.name = required.name) actual_fields
                with
                | None ->
                    Error
                      (Printf.sprintf
                         "Trait satisfaction failed [missing-field]: type %s does not satisfy trait %s because field '%s' is required"
                         type_str trait_name required.name)
                | Some actual -> (
                    let actual_type = Types.canonicalize_mono_type actual.typ in
                    let required_type = Types.canonicalize_mono_type required.typ in
                    match Unify.unify actual_type required_type with
                    | Ok _ -> check_required rest
                    | Error _ ->
                        Error
                          (Printf.sprintf
                             "Trait satisfaction failed [field-type-mismatch]: type %s does not satisfy trait %s because field '%s' has type %s but expected %s"
                             type_str trait_name required.name (Types.to_string actual_type)
                             (Types.to_string required_type))))
          in
          check_required required_fields
      | _ ->
          Error
            (Printf.sprintf
               "Trait satisfaction failed [non-record-for-field-trait]: type %s does not satisfy trait %s because field traits require a record receiver"
               type_str trait_name))

let rec check_trait_methods (typ : Types.mono_type) (trait_name : string) : (unit, string) result =
  let typ' = Types.canonicalize_mono_type typ in
  match Trait_registry.resolve_impl trait_name typ' with
  | Error msg -> Error (Printf.sprintf "Trait satisfaction failed [impl-resolution]: %s" msg)
  | Ok None ->
      Error
        (Printf.sprintf "Trait satisfaction failed [missing-impl]: type %s does not implement trait %s"
           (Types.to_string typ') trait_name)
  | Ok (Some resolved_impl) ->
      let rec check_generic_constraints = function
        | [] -> Ok ()
        | (p : AST.generic_param) :: rest -> (
            match List.assoc_opt p.name resolved_impl.specialization_subst with
            | None ->
                Error
                  (Printf.sprintf
                     "Trait satisfaction failed [impl-specialization]: generic impl for trait %s could not resolve parameter '%s' for type %s"
                     trait_name p.name (Types.to_string typ'))
            | Some concrete_param_type ->
                let concrete_param_type' = Types.canonicalize_mono_type concrete_param_type in
                let rec check_param_constraints = function
                  | [] -> Ok ()
                  | constraint_trait :: constraints_rest -> (
                      match check_trait_with_supertraits StringSet.empty concrete_param_type' constraint_trait with
                      | Ok () -> check_param_constraints constraints_rest
                      | Error msg ->
                          Error
                            (Printf.sprintf
                               "Trait satisfaction failed [generic-impl-constraint]: parameter '%s' resolved to type %s does not satisfy required constraint %s (%s)"
                               p.name (Types.to_string concrete_param_type') constraint_trait msg))
                in
                match check_param_constraints p.constraints with
                | Ok () -> check_generic_constraints rest
                | Error _ as err -> err)
      in
      check_generic_constraints resolved_impl.impl.impl_type_params

and check_trait_self_requirements (typ : Types.mono_type) (trait_name : string) : (unit, string) result =
  match Trait_registry.lookup_trait trait_name with
  | None -> Error (Printf.sprintf "Trait satisfaction failed [unknown-trait]: %s" trait_name)
  | Some _ -> (
      let kind = Trait_registry.trait_kind trait_name in
      let field_result = check_trait_fields typ trait_name in
      let method_result =
        match kind with
        | Some Trait_registry.FieldOnly -> Ok ()
        | Some Trait_registry.MethodOnly | Some Trait_registry.Mixed | None -> check_trait_methods typ trait_name
      in
      match field_result with
      | Error _ as err -> err
      | Ok () -> method_result)

and check_trait_with_supertraits (visited : StringSet.t) (typ : Types.mono_type) (trait_name : string) :
    (unit, string) result =
  if StringSet.mem trait_name visited then
    Ok ()
  else
    let visited' = StringSet.add trait_name visited in
    match check_trait_self_requirements typ trait_name with
    | Error _ as err -> err
    | Ok () -> (
        match Trait_registry.lookup_trait trait_name with
        | None -> Ok ()
        | Some trait_def -> check_supertraits visited' typ trait_def.trait_supertraits)

and check_supertraits (visited : StringSet.t) (typ : Types.mono_type) (traits : string list) :
    (unit, string) result =
  match traits with
  | [] -> Ok ()
  | trait_name :: rest -> (
      match check_trait_with_supertraits visited typ trait_name with
      | Ok () -> check_supertraits visited typ rest
      | Error _ as err -> err)

and satisfies_trait (typ : Types.mono_type) (trait_name : string) : (unit, string) result =
  check_trait_with_supertraits StringSet.empty (Types.canonicalize_mono_type typ) trait_name

let satisfies_trait_bool (typ : Types.mono_type) (trait_name : string) : bool =
  match satisfies_trait typ trait_name with
  | Ok () -> true
  | Error _ -> false

(* Check if a concrete type implements a trait *)
let implements_trait (typ : Types.mono_type) (trait_name : string) : bool = satisfies_trait_bool typ trait_name

(* Check if a type satisfies constraints, returning error if not *)
let check_constraints (typ : Types.mono_type) (constraints : string list) : (unit, string) result =
  let rec check traits =
    match traits with
    | [] -> Ok ()
    | trait :: rest -> (
        match satisfies_trait typ trait with
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

let contains_substring s sub =
  let len_s = String.length s in
  let len_sub = String.length sub in
  let rec loop i =
    if i + len_sub > len_s then
      false
    else if String.sub s i len_sub = sub then
      true
    else
      loop (i + 1)
  in
  loop 0

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
  | Error msg -> String.length msg > 0 && contains_substring msg "[missing-impl]"

let%test "satisfies_trait succeeds for builtin eq[int]" =
  setup_builtins ();
  match satisfies_trait Types.TInt "eq" with
  | Ok () -> true
  | Error _ -> false

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
          {
            method_name = "eq";
            method_params = [ ("x", Types.TVar "a"); ("y", Types.TVar "a") ];
            method_return_type = Types.TBool;
          };
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

let%test "satisfies_trait enforces supertrait obligations transitively" =
  Trait_registry.clear ();
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
  match satisfies_trait Types.TString "ord" with
  | Ok () -> false
  | Error msg -> String.length msg > 0

let%test "field-only trait is satisfied structurally by matching record" =
  Trait_registry.clear ();
  Trait_registry.register_trait
    { trait_name = "named"; trait_type_param = None; trait_supertraits = []; trait_methods = [] };
  Trait_registry.set_trait_fields "named" [ { Types.name = "name"; typ = Types.TString } ];
  match satisfies_trait (Types.TRecord ([ { Types.name = "name"; typ = Types.TString } ], None)) "named" with
  | Ok () -> true
  | Error _ -> false

let%test "field-only trait rejects non-record types" =
  Trait_registry.clear ();
  Trait_registry.register_trait
    { trait_name = "named"; trait_type_param = None; trait_supertraits = []; trait_methods = [] };
  Trait_registry.set_trait_fields "named" [ { Types.name = "name"; typ = Types.TString } ];
  match satisfies_trait Types.TInt "named" with
  | Ok () -> false
  | Error msg -> contains_substring msg "record" && contains_substring msg "[non-record-for-field-trait]"

let%test "field-only trait missing-field error includes category" =
  Trait_registry.clear ();
  Trait_registry.register_trait
    { trait_name = "named"; trait_type_param = None; trait_supertraits = []; trait_methods = [] };
  Trait_registry.set_trait_fields "named" [ { Types.name = "name"; typ = Types.TString } ];
  match satisfies_trait (Types.TRecord ([ { Types.name = "age"; typ = Types.TInt } ], None)) "named" with
  | Ok () -> false
  | Error msg -> contains_substring msg "missing-field" && contains_substring msg "field 'name'"

let%test "mixed trait requires both structural fields and nominal impl" =
  Trait_registry.clear ();
  let person_type = Types.TRecord ([ { Types.name = "name"; typ = Types.TString } ], None) in
  Trait_registry.register_trait
    {
      trait_name = "named_show";
      trait_type_param = None;
      trait_supertraits = [];
      trait_methods =
        [ { method_name = "show"; method_params = [ ("x", person_type) ]; method_return_type = Types.TString } ];
    };
  Trait_registry.set_trait_fields "named_show" [ { Types.name = "name"; typ = Types.TString } ];
  let fails_without_impl =
    match satisfies_trait person_type "named_show" with
    | Ok () -> false
    | Error _ -> true
  in
  Trait_registry.register_impl
    {
      impl_trait_name = "named_show";
      impl_type_params = [];
      impl_for_type = person_type;
      impl_methods =
        [ { method_name = "show"; method_params = [ ("x", person_type) ]; method_return_type = Types.TString } ];
    };
  let passes_with_impl =
    match satisfies_trait person_type "named_show" with
    | Ok () -> true
    | Error _ -> false
  in
  fails_without_impl && passes_with_impl
