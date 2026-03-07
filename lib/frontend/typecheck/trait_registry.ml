(* Trait Registry - tracks defined traits, implementations, and derivations *)

open Types
module AST = Syntax.Ast.AST
module String_utils = Diagnostics.String_utils

(* A method signature in a trait *)
type method_sig = {
  method_key : Resolution_artifacts.callable_key;
  method_name : string;
  method_generics : string list; (* method-level type parameter names *)
  method_params : (string * mono_type) list; (* param name and type *)
  method_return_type : mono_type;
  method_effect : [ `Pure | `Effectful ]; (* -> vs => *)
}

(* Construct a method_sig with synthetic key and default pure/no-generics.
   Used for builtins, derives, and tests. *)
let mk_method_sig ?(generics = []) ?(effect = `Pure) ~name ~params ~return_type () =
  {
    method_key = Resolution_artifacts.SyntheticCallable name;
    method_name = name;
    method_generics = generics;
    method_params = params;
    method_return_type = return_type;
    method_effect = effect;
  }

(* A trait definition *)
type trait_def = {
  trait_name : string;
  trait_type_param : string option; (* trait show[a] has Some "a" *)
  trait_supertraits : string list; (* trait ord: eq has ["eq"] *)
  trait_methods : method_sig list;
}

type trait_kind =
  | FieldOnly
  | MethodOnly
  | Mixed

type impl_source = {
  file_id : string option;
  start_pos : int;
  end_pos : int;
}

(* A trait implementation *)
type impl_def = {
  impl_trait_name : string;
  impl_type_params : AST.generic_param list; (* impl eq[a: eq] has [{ name = "a"; constraints = ["eq"] }] *)
  impl_for_type : mono_type; (* The type being implemented for *)
  impl_methods : method_sig list; (* Actual method implementations *)
}

type resolved_impl = {
  impl : impl_def;
  specialization_subst : Types.substitution;
  source_site : string;
}

(* Global mutable registries *)
let trait_registry : (string, trait_def) Hashtbl.t = Hashtbl.create 16
let impl_registry : (string * mono_type, impl_def) Hashtbl.t = Hashtbl.create 32 (* key: (trait_name, for_type) *)
let generic_impl_registry : (string * mono_type, impl_def) Hashtbl.t = Hashtbl.create 32
let impl_source_registry : (string * mono_type, impl_source) Hashtbl.t = Hashtbl.create 32
let builtin_impl_keys : (string * mono_type, unit) Hashtbl.t = Hashtbl.create 32
let generic_impl_source_registry : (string * mono_type, impl_source) Hashtbl.t = Hashtbl.create 32
let builtin_generic_impl_keys : (string * mono_type, unit) Hashtbl.t = Hashtbl.create 32
let trait_field_registry : (string, record_field_type list) Hashtbl.t = Hashtbl.create 16
let canonical_type (t : mono_type) : mono_type = canonicalize_mono_type t

let is_builtin_impl_key (trait_name : string) (for_type : mono_type) : bool =
  Hashtbl.mem builtin_impl_keys (trait_name, canonical_type for_type)

let canonical_generic_impl_for_type (def : impl_def) : mono_type =
  let rename_subst =
    List.fold_left
      (fun acc (idx, (p : AST.generic_param)) ->
        SubstMap.add p.name (TVar (Printf.sprintf "__impl_param_%d" idx)) acc)
      SubstMap.empty
      (List.mapi (fun i p -> (i, p)) def.impl_type_params)
  in
  canonical_type (apply_substitution rename_subst def.impl_for_type)

let generic_impl_key (def : impl_def) : string * mono_type =
  (def.impl_trait_name, canonical_generic_impl_for_type def)

let is_builtin_generic_impl_key (trait_name : string) (generic_for_type : mono_type) : bool =
  Hashtbl.mem builtin_generic_impl_keys (trait_name, canonical_type generic_for_type)

let clear () =
  Hashtbl.clear trait_registry;
  Hashtbl.clear impl_registry;
  Hashtbl.clear generic_impl_registry;
  Hashtbl.clear impl_source_registry;
  Hashtbl.clear generic_impl_source_registry;
  Hashtbl.clear builtin_impl_keys;
  Hashtbl.clear builtin_generic_impl_keys;
  Hashtbl.clear trait_field_registry

(* Register a trait definition *)
let register_trait (def : trait_def) : unit = Hashtbl.replace trait_registry def.trait_name def

let set_trait_fields (trait_name : string) (fields : record_field_type list) : unit =
  let canonical_fields =
    fields
    |> List.map (fun (f : record_field_type) -> { f with typ = canonical_type f.typ })
    |> normalize_record_fields
  in
  if canonical_fields = [] then
    Hashtbl.remove trait_field_registry trait_name
  else
    Hashtbl.replace trait_field_registry trait_name canonical_fields

let lookup_trait_fields (trait_name : string) : record_field_type list option =
  Hashtbl.find_opt trait_field_registry trait_name

let trait_kind (trait_name : string) : trait_kind option =
  match Hashtbl.find_opt trait_registry trait_name with
  | None -> None
  | Some _ ->
      let visited : (string, unit) Hashtbl.t = Hashtbl.create 16 in
      let rec accumulate (name : string) ((has_fields, has_methods) : bool * bool) : bool * bool =
        if Hashtbl.mem visited name then
          (has_fields, has_methods)
        else (
          Hashtbl.replace visited name ();
          match Hashtbl.find_opt trait_registry name with
          | None -> (has_fields, has_methods)
          | Some trait_def ->
              let has_methods' = has_methods || trait_def.trait_methods <> [] in
              let has_fields' =
                has_fields
                ||
                match lookup_trait_fields name with
                | Some fields when fields <> [] -> true
                | _ -> false
              in
              List.fold_left
                (fun acc super_name -> accumulate super_name acc)
                (has_fields', has_methods') trait_def.trait_supertraits)
      in
      let has_fields, has_methods = accumulate trait_name (false, false) in
      if has_fields && has_methods then
        Some Mixed
      else if has_fields then
        Some FieldOnly
      else
        Some MethodOnly

let validate_trait_fields (trait_name : string) (fields : record_field_type list) : (unit, string) result =
  let field_names = List.map (fun (f : record_field_type) -> f.name) fields in
  let unique_names = List.sort_uniq String.compare field_names in
  if List.length unique_names <> List.length field_names then
    Error (Printf.sprintf "Trait '%s' has duplicate field names" trait_name)
  else
    Ok ()

(* Register an impl block *)
let register_impl ?(builtin = false) ?source (def : impl_def) : unit =
  let canonical_for_type = canonical_type def.impl_for_type in
  let def' = { def with impl_for_type = canonical_for_type } in
  if def'.impl_type_params = [] then (
    let key = (def'.impl_trait_name, def'.impl_for_type) in
    let existing = Hashtbl.find_opt impl_registry key in
    if builtin then (
      match existing with
      | Some _ when not (Hashtbl.mem builtin_impl_keys key) ->
          failwith
            (Printf.sprintf
               "Duplicate impl registration for trait '%s' and type %s (existing user impl cannot be replaced by builtin)"
               def'.impl_trait_name (to_string def'.impl_for_type))
      | _ ->
          Hashtbl.replace builtin_impl_keys key ();
          Hashtbl.remove impl_source_registry key;
          Hashtbl.replace impl_registry key def')
    else
      match existing with
      | Some _ when not (Hashtbl.mem builtin_impl_keys key) ->
          failwith
            (Printf.sprintf "Duplicate impl registration for trait '%s' and type %s" def'.impl_trait_name
               (to_string def'.impl_for_type))
      | _ ->
          (* User impl replaces builtin marker for this key (allowed exactly once). *)
          Hashtbl.remove builtin_impl_keys key;
          (match source with
          | Some src -> Hashtbl.replace impl_source_registry key src
          | None -> Hashtbl.remove impl_source_registry key);
          Hashtbl.replace impl_registry key def')
  else
    let key = generic_impl_key def' in
    let existing = Hashtbl.find_opt generic_impl_registry key in
    if builtin then (
      match existing with
      | Some _ when not (Hashtbl.mem builtin_generic_impl_keys key) ->
          failwith
            (Printf.sprintf
               "Duplicate generic impl registration for trait '%s' and pattern %s (existing user impl cannot be replaced by builtin)"
               def'.impl_trait_name
               (to_string (snd key)))
      | _ ->
          Hashtbl.replace builtin_generic_impl_keys key ();
          Hashtbl.remove generic_impl_source_registry key;
          Hashtbl.replace generic_impl_registry key def')
    else
      match existing with
      | Some _ when not (Hashtbl.mem builtin_generic_impl_keys key) ->
          failwith
            (Printf.sprintf "Duplicate generic impl registration for trait '%s' and pattern %s"
               def'.impl_trait_name
               (to_string (snd key)))
      | _ ->
          Hashtbl.remove builtin_generic_impl_keys key;
          (match source with
          | Some src -> Hashtbl.replace generic_impl_source_registry key src
          | None -> Hashtbl.remove generic_impl_source_registry key);
          Hashtbl.replace generic_impl_registry key def'

(* Lookup a trait by name *)
let lookup_trait (name : string) : trait_def option = Hashtbl.find_opt trait_registry name

(* Expand a trait into itself plus all transitive supertraits (deterministic order). *)
let trait_with_supertraits (trait_name : string) : string list =
  let visited : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  let rec visit acc name =
    if Hashtbl.mem visited name then
      acc
    else (
      Hashtbl.replace visited name ();
      let acc' = name :: acc in
      match lookup_trait name with
      | None -> acc'
      | Some def -> List.fold_left visit acc' def.trait_supertraits)
  in
  List.rev (visit [] trait_name)

let supertraits_of_trait (trait_name : string) : string list =
  match trait_with_supertraits trait_name with
  | [] -> []
  | _self :: rest -> rest

let format_impl_site_from_source (trait_name : string) (source_opt : impl_source option) : string =
  match source_opt with
  | Some src ->
      let file =
        match src.file_id with
        | Some f -> f
        | None -> "<unknown-file>"
      in
      Printf.sprintf "%s@%s:%d-%d" trait_name file src.start_pos src.end_pos
  | None -> Printf.sprintf "%s@<builtin-or-unknown>" trait_name

let format_concrete_impl_site (trait_name : string) (for_type : mono_type) : string =
  format_impl_site_from_source trait_name
    (Hashtbl.find_opt impl_source_registry (trait_name, canonical_type for_type))

let format_generic_impl_site (trait_name : string) (generic_for_type : mono_type) : string =
  format_impl_site_from_source trait_name
    (Hashtbl.find_opt generic_impl_source_registry (trait_name, canonical_type generic_for_type))

let specialized_impl (def : impl_def) (for_type : mono_type) (subst : Types.substitution) : impl_def =
  let for_type' = canonical_type for_type in
  let methods' =
    List.map
      (fun (m : method_sig) ->
        {
          m with
          method_params =
            List.map (fun (name, ty) -> (name, canonical_type (apply_substitution subst ty))) m.method_params;
          method_return_type = canonical_type (apply_substitution subst m.method_return_type);
        })
      def.impl_methods
  in
  {
    impl_trait_name = def.impl_trait_name;
    impl_type_params = def.impl_type_params;
    impl_for_type = for_type';
    impl_methods = methods';
  }

let resolve_generic_impl (trait_name : string) (for_type : mono_type) : (resolved_impl option, string) result =
  let for_type' = canonical_type for_type in
  let matches =
    Hashtbl.fold
      (fun (candidate_trait_name, generic_for_type) candidate_def acc ->
        if candidate_trait_name <> trait_name then
          acc
        else
          let pattern_type = canonical_type candidate_def.impl_for_type in
          match Unify.unify pattern_type for_type' with
          | Error _ -> acc
          | Ok subst ->
              let specialization_subst =
                List.fold_left
                  (fun acc (p : AST.generic_param) ->
                    SubstMap.add p.name (canonical_type (apply_substitution subst (TVar p.name))) acc)
                  SubstMap.empty candidate_def.impl_type_params
              in
              let impl = specialized_impl candidate_def for_type' specialization_subst in
              let source_site = format_generic_impl_site trait_name generic_for_type in
              { impl; specialization_subst; source_site } :: acc)
      generic_impl_registry []
  in
  match matches with
  | [] -> Ok None
  | [ resolved ] -> Ok (Some resolved)
  | many ->
      let sites =
        many
        |> List.map (fun (r : resolved_impl) -> r.source_site)
        |> List.sort_uniq String.compare
        |> String.concat ", "
      in
      Error
        (Printf.sprintf "Ambiguous impl selection for trait '%s' and type %s (matching generic impl sites: %s)"
           trait_name (to_string for_type') sites)

(* Resolve an impl for a specific trait and concrete type.
   Concrete impls have precedence over generic impls. *)
let resolve_impl (trait_name : string) (for_type : mono_type) : (resolved_impl option, string) result =
  let for_type' = canonical_type for_type in
  match Hashtbl.find_opt impl_registry (trait_name, for_type') with
  | Some concrete_impl ->
      Ok
        (Some
           {
             impl = concrete_impl;
             specialization_subst = empty_substitution;
             source_site = format_concrete_impl_site trait_name for_type';
           })
  | None -> resolve_generic_impl trait_name for_type'

(* Lookup an impl for a specific trait and type *)
let lookup_impl (trait_name : string) (for_type : mono_type) : impl_def option =
  match resolve_impl trait_name for_type with
  | Ok (Some resolved) -> Some resolved.impl
  | Ok None | Error _ -> None

(* Return all registered impls (manual and derived). *)
let all_impls () : impl_def list =
  let concrete = Hashtbl.fold (fun _ impl acc -> impl :: acc) impl_registry [] in
  Hashtbl.fold (fun _ impl acc -> impl :: acc) generic_impl_registry concrete

(* Check if a type implements a trait (directly or through derivation) *)
let implements_trait (trait_name : string) (for_type : mono_type) : bool =
  match resolve_impl trait_name for_type with
  | Ok (Some _) -> true
  | Ok None | Error _ -> false

let structurally_satisfies_field_trait (trait_name : string) (for_type : mono_type) : bool =
  match lookup_trait_fields trait_name with
  | None | Some [] -> true
  | Some required_fields -> (
      match canonical_type for_type with
      | TRecord (actual_fields, _row) ->
          List.for_all
            (fun (required : record_field_type) ->
              match List.find_opt (fun (f : record_field_type) -> f.name = required.name) actual_fields with
              | None -> false
              | Some actual -> (
                  let actual_type = canonical_type actual.typ in
                  let required_type = canonical_type required.typ in
                  match Unify.unify actual_type required_type with
                  | Ok _ -> true
                  | Error _ -> false))
            required_fields
      | _ -> false)

let resolve_method (for_type : mono_type) (method_name : string) : (string * method_sig, string) result =
  let for_type' = canonical_type for_type in
  let trait_names = Hashtbl.to_seq_keys trait_registry |> List.of_seq |> List.sort String.compare in
  let candidates, resolution_errors =
    List.fold_left
      (fun (cands_acc, errs_acc) trait_name ->
        match resolve_impl trait_name for_type' with
        | Error msg -> (cands_acc, msg :: errs_acc)
        | Ok None -> (cands_acc, errs_acc)
        | Ok (Some resolved) -> (
            match List.find_opt (fun m -> m.method_name = method_name) resolved.impl.impl_methods with
            | None -> (cands_acc, errs_acc)
            | Some method_sig -> ((trait_name, method_sig, resolved.source_site) :: cands_acc, errs_acc)))
      ([], []) trait_names
  in
  if resolution_errors <> [] then
    let errs = String.concat "; " (List.rev resolution_errors) in
    Error (Printf.sprintf "Method resolution failed for type %s: %s" (to_string for_type') errs)
  else
    let candidates =
      List.sort (fun (a, _, _) (b, _, _) -> String.compare a b) candidates
      |> List.map (fun (trait_name, method_sig, site) -> (trait_name, method_sig, site))
    in
    match candidates with
    | [] -> Error (Printf.sprintf "No method '%s' found for type %s" method_name (to_string for_type'))
    | [ (trait_name, method_sig, _site) ] -> Ok (trait_name, method_sig)
    | many ->
        let trait_names = many |> List.map (fun (name, _, _) -> name) |> List.sort_uniq String.compare in
        let impl_sites =
          many
          |> List.map (fun (_name, _method_sig, site) -> site)
          |> List.sort_uniq String.compare
          |> String.concat ", "
        in
        Error
          (Printf.sprintf "Ambiguous method '%s' for type %s (provided by traits: %s; impl sites: %s)" method_name
             (to_string for_type') (String.concat ", " trait_names) impl_sites)

let lookup_method (for_type : mono_type) (method_name : string) : (string * method_sig) option =
  match resolve_method for_type method_name with
  | Ok resolved -> Some resolved
  | Error _ -> None

type derive_kind =
  | DeriveEq
  | DeriveShow
  | DeriveDebug
  | DeriveOrd
  | DeriveHash

let derive_kind_of_trait_name (trait_name : string) : derive_kind option =
  match trait_name with
  | "eq" -> Some DeriveEq
  | "show" -> Some DeriveShow
  | "debug" -> Some DeriveDebug
  | "ord" -> Some DeriveOrd
  | "hash" -> Some DeriveHash
  | _ -> None

let derive_method_names (kind : derive_kind) : string list =
  match kind with
  | DeriveEq -> [ "eq" ]
  | DeriveShow -> [ "show" ]
  | DeriveDebug -> [ "debug" ]
  | DeriveOrd -> [ "compare" ]
  | DeriveHash -> [ "hash" ]

let derive_kind_for_impl (impl : impl_def) : derive_kind option =
  match derive_kind_of_trait_name impl.impl_trait_name with
  | None -> None
  | Some kind ->
      let expected = List.sort String.compare (derive_method_names kind) in
      let actual =
        List.map (fun (m : method_sig) -> m.method_name) impl.impl_methods |> List.sort String.compare
      in
      if expected = actual then
        Some kind
      else
        None

(* Check if a trait can be auto-derived *)
let is_derivable (trait_name : string) : bool = derive_kind_of_trait_name trait_name <> None

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
        | TFun (_, _, _) -> Error "Cannot derive traits for function types"
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
              | Some type_param -> apply_substitution (substitution_singleton type_param for_type) t
            in
            let params = List.map (fun (name, t) -> (name, substitute_type t)) m.method_params in
            let return_type = substitute_type m.method_return_type in
            { m with method_params = params; method_return_type = return_type })
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
  let for_type' = canonical_type for_type in
  let generate () =
    match can_derive trait_name for_type' with
    | Error msg -> Error msg
    | Ok () -> (
        match generate_derived_impl trait_name for_type' with
        | None -> Error (Printf.sprintf "Failed to generate impl for trait '%s'" trait_name)
        | Some impl_def ->
            register_impl impl_def;
            Ok ())
  in
  match Hashtbl.find_opt impl_registry (trait_name, for_type') with
  | Some _ when not (is_builtin_impl_key trait_name for_type') ->
      Error (Printf.sprintf "Duplicate impl for trait '%s' and type %s" trait_name (to_string for_type'))
  | _ -> generate ()

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

let validate_impl_signature (trait_def : trait_def) (def : impl_def) : (unit, string) result =
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
        | Some type_param -> apply_substitution (substitution_singleton type_param def.impl_for_type) t
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
          if expected_return <> impl_return then
            Error
              (Printf.sprintf "Method '%s': return type mismatch: expected %s, got %s" trait_method.method_name
                 (to_string expected_return) (to_string impl_return))
          else if trait_method.method_effect <> impl_method.method_effect then
            let effect_str = function
              | `Pure -> "->"
              | `Effectful -> "=>"
            in
            Error
              (Printf.sprintf "Method '%s': effect mismatch: trait uses %s, impl uses %s" trait_method.method_name
                 (effect_str trait_method.method_effect)
                 (effect_str impl_method.method_effect))
          else if List.length trait_method.method_generics <> List.length impl_method.method_generics then
            Error
              (Printf.sprintf "Method '%s': generic arity mismatch: trait has %d type parameters, impl has %d"
                 trait_method.method_name
                 (List.length trait_method.method_generics)
                 (List.length impl_method.method_generics))
          else
            Ok ()
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

(* Validate that an impl matches its trait signature *)
let validate_impl (def : impl_def) : (unit, string) result =
  let for_type' = canonical_type def.impl_for_type in
  let def' = { def with impl_for_type = for_type' } in
  let duplicate_error =
    if def'.impl_type_params = [] then
      match Hashtbl.find_opt impl_registry (def'.impl_trait_name, for_type') with
      | Some _ when not (is_builtin_impl_key def'.impl_trait_name for_type') ->
          Some
            (Printf.sprintf "Duplicate impl for trait '%s' and type %s" def'.impl_trait_name (to_string for_type'))
      | _ -> None
    else
      let generic_key = generic_impl_key def' in
      match Hashtbl.find_opt generic_impl_registry generic_key with
      | Some _ when not (is_builtin_generic_impl_key (fst generic_key) (snd generic_key)) ->
          Some
            (Printf.sprintf "Duplicate generic impl for trait '%s' and pattern %s" def'.impl_trait_name
               (to_string (snd generic_key)))
      | _ -> None
  in
  match duplicate_error with
  | Some msg -> Error msg
  | None -> (
      match lookup_trait def'.impl_trait_name with
      | None -> Error (Printf.sprintf "Cannot implement undefined trait: %s" def'.impl_trait_name)
      | Some trait_def -> (
          match trait_kind def'.impl_trait_name with
          | Some FieldOnly ->
              Error
                (Printf.sprintf
                   "Trait '%s' is field-only and cannot have impl blocks (field traits are structural)"
                   def'.impl_trait_name)
          | _ -> (
              let unused_impl_params =
                List.filter
                  (fun (p : AST.generic_param) -> not (occurs_in p.name def'.impl_for_type))
                  def'.impl_type_params
              in
              if unused_impl_params <> [] then
                let names =
                  unused_impl_params |> List.map (fun (p : AST.generic_param) -> p.name) |> String.concat ", "
                in
                Error
                  (Printf.sprintf
                     "Generic impl for trait '%s' has type parameter(s) not used in impl target type: %s"
                     def'.impl_trait_name names)
              else
                match validate_impl_signature trait_def def' with
                | Error _ as err -> err
                | Ok () ->
                    if def'.impl_type_params <> [] then
                      Ok ()
                    else
                      let rec check_supertraits = function
                        | [] -> Ok ()
                        | supertrait :: rest ->
                            let supertrait_satisfied =
                              match trait_kind supertrait with
                              | Some FieldOnly -> structurally_satisfies_field_trait supertrait for_type'
                              | _ -> implements_trait supertrait for_type'
                            in
                            if supertrait_satisfied then
                              check_supertraits rest
                            else
                              Error
                                (Printf.sprintf
                                   "Impl for trait '%s' on type %s is missing required supertrait '%s' implementation"
                                   def'.impl_trait_name (to_string for_type') supertrait)
                      in
                      check_supertraits (supertraits_of_trait def'.impl_trait_name))))

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
      trait_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
    }
  in
  register_trait show_trait;
  match lookup_trait "show" with
  | None -> false
  | Some def -> def.trait_name = "show" && List.length def.trait_methods = 1

let%test "trait_kind includes field-only supertraits" =
  clear ();
  register_trait { trait_name = "named"; trait_type_param = None; trait_supertraits = []; trait_methods = [] };
  set_trait_fields "named" [ { name = "name"; typ = TString } ];
  register_trait
    { trait_name = "alias"; trait_type_param = None; trait_supertraits = [ "named" ]; trait_methods = [] };
  trait_kind "alias" = Some FieldOnly

let%test "trait_kind includes method supertraits" =
  clear ();
  register_trait
    {
      trait_name = "show";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
    };
  register_trait
    { trait_name = "display"; trait_type_param = None; trait_supertraits = [ "show" ]; trait_methods = [] };
  trait_kind "display" = Some MethodOnly

let%test "trait_kind becomes mixed with field and method supertraits" =
  clear ();
  register_trait { trait_name = "named"; trait_type_param = None; trait_supertraits = []; trait_methods = [] };
  set_trait_fields "named" [ { name = "name"; typ = TString } ];
  register_trait
    {
      trait_name = "show";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
    };
  register_trait
    {
      trait_name = "named_show";
      trait_type_param = None;
      trait_supertraits = [ "named"; "show" ];
      trait_methods = [];
    };
  trait_kind "named_show" = Some Mixed

let%test "validate_trait_def - duplicate methods" =
  clear ();
  let bad_trait =
    {
      trait_name = "bad";
      trait_type_param = None;
      trait_supertraits = [];
      trait_methods =
        [
          mk_method_sig ~name:"foo" ~params:[] ~return_type:TInt ();
          mk_method_sig ~name:"foo" ~params:[] ~return_type:TString ();
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
      trait_methods = [ mk_method_sig ~name:"compare" ~params:[] ~return_type:TInt () ];
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
      trait_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
    }
  in
  register_trait show_trait;

  let show_for_int =
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TInt) ] ~return_type:TString () ];
    }
  in
  register_impl show_for_int;

  match lookup_impl "show" TInt with
  | None -> false
  | Some def -> def.impl_trait_name = "show" && def.impl_for_type = TInt

let%test "register_impl rejects duplicate user impl at registration" =
  clear ();
  register_trait
    {
      trait_name = "show";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
    };
  let show_for_int =
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TInt) ] ~return_type:TString () ];
    }
  in
  register_impl show_for_int;
  let contains_substring s sub = String_utils.contains_substring ~needle:sub s in
  match
    try
      register_impl show_for_int;
      None
    with Failure msg -> Some msg
  with
  | None -> false
  | Some msg -> contains_substring msg "Duplicate impl registration"

let%test "register_impl allows user override of builtin impl key" =
  clear ();
  register_trait
    {
      trait_name = "show";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
    };
  let builtin_show_for_int =
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TInt) ] ~return_type:TString () ];
    }
  in
  let user_show_for_int =
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TInt) ] ~return_type:TString () ];
    }
  in
  register_impl ~builtin:true builtin_show_for_int;
  try
    register_impl user_show_for_int;
    true
  with Failure _ -> false

let%test "implements_trait checks impl registry" =
  clear ();
  let show_trait =
    {
      trait_name = "show";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
    }
  in
  register_trait show_trait;

  let show_for_int =
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TInt) ] ~return_type:TString () ];
    }
  in
  register_impl show_for_int;

  implements_trait "show" TInt && not (implements_trait "show" TString)

let%test "implements_trait supports generic impl matching for concrete types" =
  clear ();
  register_trait
    {
      trait_name = "show";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
    };
  register_impl
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TInt) ] ~return_type:TString () ];
    };
  register_impl
    {
      impl_trait_name = "show";
      impl_type_params = [ { AST.name = "b"; constraints = [ "show" ] } ];
      impl_for_type = TArray (TVar "b");
      impl_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TArray (TVar "b")) ] ~return_type:TString () ];
    };
  implements_trait "show" (TArray TInt)

let%test "lookup_method specializes generic impl method signature for receiver type" =
  clear ();
  register_trait
    {
      trait_name = "show";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
    };
  register_impl
    {
      impl_trait_name = "show";
      impl_type_params = [ { AST.name = "b"; constraints = [] } ];
      impl_for_type = TArray (TVar "b");
      impl_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TArray (TVar "b")) ] ~return_type:TString () ];
    };
  match lookup_method (TArray TInt) "show" with
  | Some ("show", method_sig) -> (
      match method_sig.method_params with
      | [ ("x", TArray TInt) ] -> method_sig.method_return_type = TString
      | _ -> false)
  | _ -> false

let%test "lookup_method canonicalizes reordered record receiver" =
  clear ();
  let show_trait =
    {
      trait_name = "show";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
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
          mk_method_sig ~name:"show"
            ~params:[ ("x", TRecord ([ { name = "x"; typ = TInt }; { name = "y"; typ = TInt } ], None)) ]
            ~return_type:TString ();
        ];
    };
  match lookup_method (TRecord ([ { name = "y"; typ = TInt }; { name = "x"; typ = TInt } ], None)) "show" with
  | Some ("show", _) -> true
  | _ -> false

let%test "resolve_method reports ambiguity for multiple matching traits" =
  clear ();
  register_trait
    {
      trait_name = "render_a";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods = [ mk_method_sig ~name:"render" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
    };
  register_trait
    {
      trait_name = "render_b";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods = [ mk_method_sig ~name:"render" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
    };
  register_impl
    ~source:{ file_id = Some "test.mr"; start_pos = 10; end_pos = 20 }
    {
      impl_trait_name = "render_a";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ mk_method_sig ~name:"render" ~params:[ ("x", TInt) ] ~return_type:TString () ];
    };
  register_impl
    ~source:{ file_id = Some "test.mr"; start_pos = 30; end_pos = 40 }
    {
      impl_trait_name = "render_b";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ mk_method_sig ~name:"render" ~params:[ ("x", TInt) ] ~return_type:TString () ];
    };
  let contains_substring s sub = String_utils.contains_substring ~needle:sub s in
  match resolve_method TInt "render" with
  | Ok _ -> false
  | Error msg ->
      String.length msg > 0
      && String.sub msg 0 (min 16 (String.length msg)) = "Ambiguous method"
      && contains_substring msg "impl sites:"
      && contains_substring msg "render_a@test.mr:10-20"
      && contains_substring msg "render_b@test.mr:30-40"

let%test "lookup_method no longer picks first candidate on ambiguity" =
  clear ();
  register_trait
    {
      trait_name = "render_a";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods = [ mk_method_sig ~name:"render" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
    };
  register_trait
    {
      trait_name = "render_b";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods = [ mk_method_sig ~name:"render" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
    };
  register_impl
    {
      impl_trait_name = "render_a";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ mk_method_sig ~name:"render" ~params:[ ("x", TInt) ] ~return_type:TString () ];
    };
  register_impl
    {
      impl_trait_name = "render_b";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ mk_method_sig ~name:"render" ~params:[ ("x", TInt) ] ~return_type:TString () ];
    };
  match lookup_method TInt "render" with
  | None -> true
  | Some _ -> false

let%test "validate_impl rejects duplicate trait/type pair" =
  clear ();
  let show_trait =
    {
      trait_name = "show";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
    }
  in
  register_trait show_trait;
  register_impl
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TInt) ] ~return_type:TString () ];
    };
  match
    validate_impl
      {
        impl_trait_name = "show";
        impl_type_params = [];
        impl_for_type = TInt;
        impl_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TInt) ] ~return_type:TString () ];
      }
  with
  | Error msg ->
      String.length msg > 0 && String.sub msg 0 (min 24 (String.length msg)) = "Duplicate impl for trait"
  | Ok () -> false

let%test "validate_impl allows overriding builtin impl once" =
  clear ();
  let show_trait =
    {
      trait_name = "show";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
    }
  in
  register_trait show_trait;
  let builtin_show_for_int =
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TInt) ] ~return_type:TString () ];
    }
  in
  let user_show_for_int =
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TInt) ] ~return_type:TString () ];
    }
  in
  register_impl ~builtin:true builtin_show_for_int;
  match validate_impl user_show_for_int with
  | Error _ -> false
  | Ok () -> (
      register_impl user_show_for_int;
      match validate_impl user_show_for_int with
      | Ok () -> false
      | Error msg ->
          String.length msg > 0 && String.sub msg 0 (min 24 (String.length msg)) = "Duplicate impl for trait")

(* Comprehensive impl validation tests *)

let%test "validate_impl - undefined trait" =
  clear ();
  let bad_impl =
    {
      impl_trait_name = "undefined";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ mk_method_sig ~name:"foo" ~params:[] ~return_type:TInt () ];
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
        [ mk_method_sig ~name:"eq" ~params:[ ("x", TVar "a"); ("y", TVar "a") ] ~return_type:TBool () ];
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
      trait_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
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
          mk_method_sig ~name:"show" ~params:[ ("x", TInt) ] ~return_type:TString ();
          mk_method_sig ~name:"extra" ~params:[] ~return_type:TInt ();
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
          mk_method_sig ~name:"eq" ~params:[ ("x", TVar "a"); ("y", TVar "a") ] ~return_type:TBool ();
          (* 2 params *)
        ];
    }
  in
  register_trait eq_trait;

  let bad_impl =
    {
      impl_trait_name = "eq";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ mk_method_sig ~name:"eq" ~params:[ ("x", TInt) ] ~return_type:TBool () ];
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
      trait_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
    }
  in
  register_trait show_trait;

  let bad_impl =
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TString) ] ~return_type:TString () ];
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
      trait_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
    }
  in
  register_trait show_trait;

  let bad_impl =
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TInt) ] ~return_type:TInt () ];
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
      trait_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
    }
  in
  register_trait show_trait;

  let good_impl =
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TInt) ] ~return_type:TString () ];
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
          mk_method_sig ~name:"compare" ~params:[ ("x", TVar "a"); ("y", TVar "a") ] ~return_type:TInt ();
          mk_method_sig ~name:"less_than" ~params:[ ("x", TVar "a"); ("y", TVar "a") ] ~return_type:TBool ();
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
          mk_method_sig ~name:"compare" ~params:[ ("x", TInt); ("y", TInt) ] ~return_type:TInt ();
          mk_method_sig ~name:"less_than" ~params:[ ("x", TInt); ("y", TInt) ] ~return_type:TBool ();
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

let%test "derive_kind_for_impl recognizes eq contract" =
  let impl =
    {
      impl_trait_name = "eq";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ mk_method_sig ~name:"eq" ~params:[ ("x", TInt); ("y", TInt) ] ~return_type:TBool () ];
    }
  in
  match derive_kind_for_impl impl with
  | Some DeriveEq -> true
  | _ -> false

let%test "derive_kind_for_impl rejects mismatched method set" =
  let impl =
    {
      impl_trait_name = "eq";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ mk_method_sig ~name:"compare" ~params:[ ("x", TInt); ("y", TInt) ] ~return_type:TBool () ];
    }
  in
  derive_kind_for_impl impl = None

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
      trait_methods = [ mk_method_sig ~name:"custom" ~params:[ ("x", TVar "a") ] ~return_type:TInt () ];
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
        [ mk_method_sig ~name:"eq" ~params:[ ("x", TVar "a"); ("y", TVar "a") ] ~return_type:TBool () ];
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
        [ mk_method_sig ~name:"eq" ~params:[ ("x", TVar "a"); ("y", TVar "a") ] ~return_type:TBool () ];
    }
  in
  register_trait eq_trait;
  match can_derive "eq" (tfun TInt TInt) with
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
        [ mk_method_sig ~name:"eq" ~params:[ ("x", TVar "a"); ("y", TVar "a") ] ~return_type:TBool () ];
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
      trait_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
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
        [ mk_method_sig ~name:"eq" ~params:[ ("x", TVar "a"); ("y", TVar "a") ] ~return_type:TBool () ];
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

let%test "derive_impl - overriding builtin impl is allowed once" =
  clear ();
  let show_trait =
    {
      trait_name = "show";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
    }
  in
  register_trait show_trait;
  register_impl ~builtin:true
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TInt) ] ~return_type:TString () ];
    };
  match derive_impl "show" TInt with
  | Error _ -> false
  | Ok () -> (
      match derive_impl "show" TInt with
      | Ok () -> false
      | Error msg ->
          String.length msg > 0 && String.sub msg 0 (min 24 (String.length msg)) = "Duplicate impl for trait")

let%test "derive_impl - fails for non-derivable" =
  clear ();
  let custom_trait =
    {
      trait_name = "custom";
      trait_type_param = None;
      trait_supertraits = [];
      trait_methods = [ mk_method_sig ~name:"foo" ~params:[] ~return_type:TInt () ];
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
      trait_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
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

let%test "validate_impl rejects missing required supertrait implementation" =
  clear ();
  register_trait
    {
      trait_name = "eq";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [ mk_method_sig ~name:"eq" ~params:[ ("x", TVar "a"); ("y", TVar "a") ] ~return_type:TBool () ];
    };
  register_trait
    {
      trait_name = "ord";
      trait_type_param = Some "a";
      trait_supertraits = [ "eq" ];
      trait_methods =
        [ mk_method_sig ~name:"compare" ~params:[ ("x", TVar "a"); ("y", TVar "a") ] ~return_type:TInt () ];
    };
  match
    validate_impl
      {
        impl_trait_name = "ord";
        impl_type_params = [];
        impl_for_type = TString;
        impl_methods =
          [ mk_method_sig ~name:"compare" ~params:[ ("x", TString); ("y", TString) ] ~return_type:TInt () ];
      }
  with
  | Ok () -> false
  | Error msg -> String.length msg > 0

let%test "validate_impl accepts structural field-only supertrait satisfaction" =
  clear ();
  register_trait { trait_name = "named"; trait_type_param = None; trait_supertraits = []; trait_methods = [] };
  set_trait_fields "named" [ { name = "name"; typ = TString } ];
  register_trait
    {
      trait_name = "shown";
      trait_type_param = Some "a";
      trait_supertraits = [ "named" ];
      trait_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
    };
  match
    validate_impl
      {
        impl_trait_name = "shown";
        impl_type_params = [];
        impl_for_type = TRecord ([ { name = "name"; typ = TString } ], None);
        impl_methods =
          [
            mk_method_sig ~name:"show"
              ~params:[ ("x", TRecord ([ { name = "name"; typ = TString } ], None)) ]
              ~return_type:TString ();
          ];
      }
  with
  | Ok () -> true
  | Error _ -> false

let%test "validate_impl rejects field-only supertrait when structural fields are missing" =
  clear ();
  register_trait { trait_name = "named"; trait_type_param = None; trait_supertraits = []; trait_methods = [] };
  set_trait_fields "named" [ { name = "name"; typ = TString } ];
  register_trait
    {
      trait_name = "shown";
      trait_type_param = Some "a";
      trait_supertraits = [ "named" ];
      trait_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
    };
  match
    validate_impl
      {
        impl_trait_name = "shown";
        impl_type_params = [];
        impl_for_type = TInt;
        impl_methods = [ mk_method_sig ~name:"show" ~params:[ ("x", TInt) ] ~return_type:TString () ];
      }
  with
  | Ok () -> false
  | Error msg -> String.length msg > 0

let%test "validate_impl rejects impl blocks for field-only traits" =
  clear ();
  register_trait { trait_name = "named"; trait_type_param = None; trait_supertraits = []; trait_methods = [] };
  set_trait_fields "named" [ { name = "name"; typ = TString } ];
  match
    validate_impl
      {
        impl_trait_name = "named";
        impl_type_params = [];
        impl_for_type = TRecord ([ { name = "name"; typ = TString } ], None);
        impl_methods = [];
      }
  with
  | Ok () -> false
  | Error msg -> String.length msg > 0 && String.sub msg 0 (min (String.length msg) 5) = "Trait"
