(* Trait Solver - Determines if a type implements a trait *)

module Types = Types
module Constraints = Constraints
module Trait_registry = Trait_registry
module Diagnostic = Diagnostics.Diagnostic
module AST = Syntax.Ast.AST
module String_utils = Diagnostics.String_utils
module StringSet = Set.Make (String)

let dedupe_preserve_order (constraints : Constraints.t list) : Constraints.t list =
  let rec go seen acc = function
    | [] -> List.rev acc
    | constraint_ref :: rest ->
        let constraint_name = Constraints.name constraint_ref in
        if StringSet.mem constraint_name seen then
          go seen acc rest
        else
          go (StringSet.add constraint_name seen) (constraint_ref :: acc) rest
  in
  go StringSet.empty [] constraints

let expand_constraint_refs (constraints : Constraints.t list) : Constraints.t list =
  let rec expand_one (visited : StringSet.t) (constraint_ref : Constraints.t) :
      StringSet.t * Constraints.t list =
    let constraint_ref = Constraints.canonicalize constraint_ref in
    let constraint_name = Constraints.name constraint_ref in
    if StringSet.mem constraint_name visited then
      (visited, [])
    else
      match constraint_ref with
      | Constraints.ShapeConstraint _ -> (StringSet.add constraint_name visited, [ constraint_ref ])
      | Constraints.TraitConstraint trait_name -> (
          match Trait_registry.lookup_trait trait_name with
          | None -> (StringSet.add constraint_name visited, [ constraint_ref ])
          | Some trait_def ->
              let visited' = StringSet.add constraint_name visited in
              let visited'', expanded_supertraits =
                expand_many visited' (Constraints.of_names trait_def.trait_supertraits)
              in
              (visited'', constraint_ref :: expanded_supertraits))
  and expand_many (visited : StringSet.t) (constraints : Constraints.t list) :
      StringSet.t * Constraints.t list =
    match constraints with
    | [] -> (visited, [])
    | constraint_ref :: rest ->
        let visited', expanded_constraint = expand_one visited constraint_ref in
        let visited'', expanded_rest = expand_many visited' rest in
        (visited'', expanded_constraint @ expanded_rest)
  in
  constraints |> expand_many StringSet.empty |> snd |> dedupe_preserve_order

let expand_constraint_names (constraints : string list) : string list =
  expand_constraint_refs (Constraints.of_names constraints) |> Constraints.names

let expand_constraints_with_supertraits_refs (constraints : Constraints.t list) : Constraints.t list =
  expand_constraint_refs constraints
  |> List.filter (function
         | Constraints.TraitConstraint _ -> true
         | Constraints.ShapeConstraint _ -> false)
  |> dedupe_preserve_order

let expand_constraints_with_supertraits (constraints : string list) : string list =
  expand_constraints_with_supertraits_refs (Constraints.of_names constraints) |> Constraints.names

let trait_error ~(code : string) (message : string) : (unit, Diagnostic.t) result =
  Error (Diagnostic.error_no_span ~code ~message)

let rec check_shape_fields (typ : Types.mono_type) (shape_name : string) : (unit, Diagnostic.t) result =
  match Structural.shape_fields shape_name with
  | None -> trait_error ~code:"type-shape-unknown" (Printf.sprintf "Unknown shape: %s" shape_name)
  | Some required_fields -> (
      let type_str = Types.to_string typ in
      match Types.canonicalize_mono_type typ with
      | Types.TIntersection members ->
          let rec check_all = function
            | [] -> Ok ()
            | member :: rest -> (
                match check_shape_fields member shape_name with
                | Ok () -> check_all rest
                | Error _ as err -> err)
          in
          check_all members
      | concrete -> (
          match Structural.fields_of_type concrete with
          | Some actual_fields ->
              let rec check_required = function
                | [] -> Ok ()
                | (required : Types.record_field_type) :: rest -> (
                    match
                      List.find_opt (fun (f : Types.record_field_type) -> f.name = required.name) actual_fields
                    with
                    | None ->
                        trait_error ~code:"type-shape-missing-field"
                          (Printf.sprintf "Type %s does not satisfy shape %s because field '%s' is required"
                             type_str shape_name required.name)
                    | Some actual -> (
                        let actual_type = Types.canonicalize_mono_type actual.typ in
                        let required_type = Types.canonicalize_mono_type required.typ in
                        if Structural.field_types_compatible actual_type required_type then
                          check_required rest
                        else
                          trait_error ~code:"type-shape-type-mismatch"
                            (Printf.sprintf
                               "Type %s does not satisfy shape %s because field '%s' has type %s but expected %s"
                               type_str shape_name required.name (Types.to_string actual_type)
                               (Types.to_string required_type))))
              in
              check_required required_fields
          | None ->
              trait_error ~code:"type-shape-non-record"
                (Printf.sprintf
                   "Type %s does not satisfy shape %s because structural shape constraints require record fields"
                   type_str shape_name)))

let rec check_trait_methods (typ : Types.mono_type) (trait_name : string) : (unit, Diagnostic.t) result =
  let typ' = Types.canonicalize_mono_type typ in
  match Trait_registry.resolve_impl trait_name typ' with
  | Error msg ->
      trait_error ~code:"type-trait-impl-resolution" (Printf.sprintf "Trait impl resolution failed: %s" msg)
  | Ok None ->
      trait_error ~code:"type-trait-missing-impl"
        (Printf.sprintf "Type %s does not implement trait %s" (Types.to_string typ') trait_name)
  | Ok (Some resolved_impl) ->
      let rec check_generic_constraints = function
        | [] -> Ok ()
        | (p : AST.generic_param) :: rest -> (
            match Types.SubstMap.find_opt p.name resolved_impl.specialization_subst with
            | None ->
                trait_error ~code:"type-trait-specialization"
                  (Printf.sprintf "Generic impl for trait %s could not resolve parameter '%s' for type %s"
                     trait_name p.name (Types.to_string typ'))
            | Some concrete_param_type -> (
                let concrete_param_type' = Types.canonicalize_mono_type concrete_param_type in
                let rec check_param_constraints = function
                  | [] -> Ok ()
                  | constraint_name :: constraints_rest -> (
                      match
                        check_constraint_ref_with_superconstraints StringSet.empty concrete_param_type'
                          (Constraints.of_name constraint_name)
                      with
                      | Ok () -> check_param_constraints constraints_rest
                      | Error diag ->
                          trait_error ~code:"type-trait-generic-constraint"
                            (Printf.sprintf
                               "Parameter '%s' resolved to type %s does not satisfy required constraint %s (%s)"
                               p.name
                               (Types.to_string concrete_param_type')
                               constraint_name diag.message))
                in
                match check_param_constraints p.constraints with
                | Ok () -> check_generic_constraints rest
                | Error _ as err -> err))
      in
      check_generic_constraints resolved_impl.impl.impl_type_params

and check_trait_self_requirements (typ : Types.mono_type) (trait_name : string) : (unit, Diagnostic.t) result =
  match Trait_registry.lookup_trait trait_name with
  | None -> trait_error ~code:"type-trait-unknown" (Printf.sprintf "Unknown trait: %s" trait_name)
  | Some _ -> check_trait_methods typ trait_name

and check_constraint_ref_with_superconstraints
    (visited : StringSet.t)
    (typ : Types.mono_type)
    (constraint_ref : Constraints.t) : (unit, Diagnostic.t) result =
  let constraint_ref = Constraints.canonicalize constraint_ref in
  let constraint_name = Constraints.name constraint_ref in
  if StringSet.mem constraint_name visited then
    Ok ()
  else
    match constraint_ref with
    | Constraints.ShapeConstraint shape_name -> check_shape_fields typ shape_name
    | Constraints.TraitConstraint trait_name ->
        let visited' = StringSet.add constraint_name visited in
        match check_trait_self_requirements typ trait_name with
        | Error _ as err -> err
        | Ok () -> (
            match Trait_registry.lookup_trait trait_name with
            | None -> Ok ()
            | Some trait_def ->
                check_superconstraints visited' typ (Constraints.of_names trait_def.trait_supertraits))

and check_superconstraints (visited : StringSet.t) (typ : Types.mono_type) (constraints : Constraints.t list) :
    (unit, Diagnostic.t) result =
  match constraints with
  | [] -> Ok ()
  | constraint_ref :: rest -> (
      match check_constraint_ref_with_superconstraints visited typ constraint_ref with
      | Ok () -> check_superconstraints visited typ rest
      | Error _ as err -> err)

and check_constraint_ref (typ : Types.mono_type) (constraint_ref : Constraints.t) : (unit, Diagnostic.t) result =
  check_constraint_ref_with_superconstraints StringSet.empty (Types.canonicalize_mono_type typ) constraint_ref

and satisfies_constraint (typ : Types.mono_type) (constraint_name : string) : (unit, Diagnostic.t) result =
  check_constraint_ref typ (Constraints.of_name constraint_name)

and satisfies_trait (typ : Types.mono_type) (trait_name : string) : (unit, Diagnostic.t) result =
  check_constraint_ref typ (Constraints.TraitConstraint (Trait_registry.canonical_trait_name trait_name))

let satisfies_trait_bool (typ : Types.mono_type) (trait_name : string) : bool =
  match satisfies_trait typ trait_name with
  | Ok () -> true
  | Error _ -> false

let implements_trait (typ : Types.mono_type) (trait_name : string) : bool = satisfies_trait_bool typ trait_name

let check_constraint_refs (typ : Types.mono_type) (constraints : Constraints.t list) : (unit, Diagnostic.t) result =
  let rec check = function
    | [] -> Ok ()
    | constraint_ref :: rest -> (
        match check_constraint_ref typ constraint_ref with
        | Ok () -> check rest
        | Error _ as err -> err)
  in
  check constraints

let check_constraints (typ : Types.mono_type) (constraints : string list) : (unit, Diagnostic.t) result =
  check_constraint_refs typ (Constraints.of_names constraints)

let satisfies_constraints (typ : Types.mono_type) (constraints : string list) : bool =
  match check_constraints typ constraints with
  | Ok () -> true
  | Error _ -> false

let available_methods_refs (constraints : Constraints.t list) : (string * Trait_registry.trait_def) list =
  expand_constraints_with_supertraits_refs constraints
  |> List.filter_map (function
         | Constraints.ShapeConstraint _ -> None
         | Constraints.TraitConstraint trait_name -> (
             match Trait_registry.lookup_trait trait_name with
             | Some trait_def -> Some (trait_name, trait_def)
             | None -> None))

let available_methods (constraints : string list) : (string * Trait_registry.trait_def) list =
  available_methods_refs (Constraints.of_names constraints)

let method_available_refs (method_name : string) (constraints : Constraints.t list) : string option =
  let expanded_constraints =
    expand_constraints_with_supertraits_refs constraints |> List.filter_map Constraints.trait_name_opt
  in
  let rec find_in_traits traits =
    match traits with
    | [] -> None
    | trait_name :: rest -> (
        match Trait_registry.lookup_trait trait_name with
        | Some trait_def ->
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

let method_available (method_name : string) (constraints : string list) : string option =
  method_available_refs method_name (Constraints.of_names constraints)

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
          Trait_registry.mk_method_sig ~name:"show"
            ~params:[ ("x", Types.TVar "a") ]
            ~return_type:Types.TString ();
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
          Trait_registry.mk_method_sig ~name:"eq"
            ~params:[ ("x", Types.TVar "a"); ("y", Types.TVar "a") ]
            ~return_type:Types.TBool ();
        ];
    };
  (* Register impls for int *)
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = Types.TInt;
      impl_methods =
        [ Trait_registry.mk_method_sig ~name:"show" ~params:[ ("x", Types.TInt) ] ~return_type:Types.TString () ];
    };
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "eq";
      impl_type_params = [];
      impl_for_type = Types.TInt;
      impl_methods =
        [
          Trait_registry.mk_method_sig ~name:"eq"
            ~params:[ ("x", Types.TInt); ("y", Types.TInt) ]
            ~return_type:Types.TBool ();
        ];
    };
  (* Register impl for string *)
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = Types.TString;
      impl_methods =
        [
          Trait_registry.mk_method_sig ~name:"show" ~params:[ ("x", Types.TString) ] ~return_type:Types.TString ();
        ];
    }

let contains_substring s sub = String_utils.contains_substring ~needle:sub s

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
  | Error diag -> String.length diag.message > 0 && diag.code = "type-trait-missing-impl"

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
          Trait_registry.mk_method_sig ~name:"eq"
            ~params:[ ("x", Types.TVar "a"); ("y", Types.TVar "a") ]
            ~return_type:Types.TBool ();
        ];
    };
  Trait_registry.register_trait
    {
      trait_name = "ord";
      trait_type_param = Some "a";
      trait_supertraits = [ "eq" ];
      trait_methods =
        [
          Trait_registry.mk_method_sig ~name:"compare"
            ~params:[ ("x", Types.TVar "a"); ("y", Types.TVar "a") ]
            ~return_type:Types.TInt ();
        ];
    };
  Trait_registry.register_impl
    {
      impl_trait_name = "ord";
      impl_type_params = [];
      impl_for_type = Types.TString;
      impl_methods =
        [
          Trait_registry.mk_method_sig ~name:"compare"
            ~params:[ ("x", Types.TString); ("y", Types.TString) ]
            ~return_type:Types.TInt ();
        ];
    };
  match check_constraints Types.TString [ "ord" ] with
  | Ok () -> false
  | Error diag -> String.length diag.message > 0

let%test "satisfies_trait enforces supertrait obligations transitively" =
  Trait_registry.clear ();
  Trait_registry.register_trait
    {
      trait_name = "eq";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [
          Trait_registry.mk_method_sig ~name:"eq"
            ~params:[ ("x", Types.TVar "a"); ("y", Types.TVar "a") ]
            ~return_type:Types.TBool ();
        ];
    };
  Trait_registry.register_trait
    {
      trait_name = "ord";
      trait_type_param = Some "a";
      trait_supertraits = [ "eq" ];
      trait_methods =
        [
          Trait_registry.mk_method_sig ~name:"compare"
            ~params:[ ("x", Types.TVar "a"); ("y", Types.TVar "a") ]
            ~return_type:Types.TInt ();
        ];
    };
  Trait_registry.register_impl
    {
      impl_trait_name = "ord";
      impl_type_params = [];
      impl_for_type = Types.TString;
      impl_methods =
        [
          Trait_registry.mk_method_sig ~name:"compare"
            ~params:[ ("x", Types.TString); ("y", Types.TString) ]
            ~return_type:Types.TInt ();
        ];
    };
  match satisfies_trait Types.TString "ord" with
  | Ok () -> false
  | Error diag -> String.length diag.message > 0

let%test "shape constraint is satisfied structurally by matching record" =
  Trait_registry.clear ();
  Type_registry.clear ();
  Type_registry.register_shape
    {
      Type_registry.shape_name = "named";
      shape_type_params = [];
      shape_fields = [ { Types.name = "name"; typ = Types.TString } ];
    };
  match check_constraints (Types.TRecord ([ { Types.name = "name"; typ = Types.TString } ], None)) [ "named" ] with
  | Ok () -> true
  | Error _ -> false

let%test "shape constraint rejects non-record types" =
  Trait_registry.clear ();
  Type_registry.clear ();
  Type_registry.register_shape
    {
      Type_registry.shape_name = "named";
      shape_type_params = [];
      shape_fields = [ { Types.name = "name"; typ = Types.TString } ];
    };
  match check_constraints Types.TInt [ "named" ] with
  | Ok () -> false
  | Error diag -> contains_substring diag.message "record" && diag.code = "type-shape-non-record"

let%test "shape constraint missing-field error includes category" =
  Trait_registry.clear ();
  Type_registry.clear ();
  Type_registry.register_shape
    {
      Type_registry.shape_name = "named";
      shape_type_params = [];
      shape_fields = [ { Types.name = "name"; typ = Types.TString } ];
    };
  match check_constraints (Types.TRecord ([ { Types.name = "age"; typ = Types.TInt } ], None)) [ "named" ] with
  | Ok () -> false
  | Error diag -> diag.code = "type-shape-missing-field" && contains_substring diag.message "field 'name'"

let%test "trait with shape superconstraint requires both structural fields and nominal impl" =
  Trait_registry.clear ();
  Type_registry.clear ();
  let person_type = Types.TRecord ([ { Types.name = "name"; typ = Types.TString } ], None) in
  Type_registry.register_shape
    {
      Type_registry.shape_name = "named";
      shape_type_params = [];
      shape_fields = [ { Types.name = "name"; typ = Types.TString } ];
    };
  Trait_registry.register_trait
    {
      trait_name = "named_show";
      trait_type_param = Some "a";
      trait_supertraits = [ "named" ];
      trait_methods =
        [ Trait_registry.mk_method_sig ~name:"show" ~params:[ ("x", Types.TVar "a") ] ~return_type:Types.TString () ];
    };
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
        [ Trait_registry.mk_method_sig ~name:"show" ~params:[ ("x", person_type) ] ~return_type:Types.TString () ];
    };
  let passes_with_impl =
    match satisfies_trait person_type "named_show" with
    | Ok () -> true
    | Error _ -> false
  in
  fails_without_impl && passes_with_impl

let%test "shape constraint accepts compatible record intersections" =
  Trait_registry.clear ();
  Type_registry.clear ();
  Type_registry.register_shape
    {
      Type_registry.shape_name = "named";
      shape_type_params = [];
      shape_fields = [ { Types.name = "name"; typ = Types.TString } ];
    };
  let intersection =
    Types.TIntersection
      [
        Types.TRecord ([ { Types.name = "name"; typ = Types.TString } ], None);
        Types.TRecord
          ([ { Types.name = "age"; typ = Types.TInt }; { Types.name = "name"; typ = Types.TString } ], None);
      ]
  in
  match check_constraints intersection [ "named" ] with
  | Ok () -> true
  | Error _ -> false
