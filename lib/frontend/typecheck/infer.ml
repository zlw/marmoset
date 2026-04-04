(* Type inference using Algorithm W (Hindley-Milner) *)

open Types
open Unify
module AST = Syntax.Ast.AST
module Constraints = Constraints
module Diagnostic = Diagnostics.Diagnostic
module String_utils = Diagnostics.String_utils

let ( let* ) = Result.bind

let map_result f xs =
  let rec loop rev_acc = function
    | [] -> Ok (List.rev rev_acc)
    | x :: rest ->
        let* y = f x in
        loop (y :: rev_acc) rest
  in
  loop [] xs

(* ============================================================
   Type Environment
   ============================================================
   
   Maps variable names to their type schemes (poly_types).
   
   We use poly_types (not mono_types) because let-bound variables
   can be polymorphic. For example:
   
     fn id(x) = x
     id(5);      // id used at Int -> Int
     id(true);   // id used at Bool -> Bool
   
   Here 'id' has type ∀a. a -> a, which gets instantiated differently
   at each use site.
*)

module TypeEnv = Map.Make (String)
module StringSet = Set.Make (String)
module NameMap = Map.Make (String)

type type_env = poly_type TypeEnv.t

let empty_env : type_env = TypeEnv.empty

(* Free type variables in an environment (union of all free vars in all types) *)
let free_type_vars_env (env : type_env) : TypeVarSet.t =
  TypeEnv.fold (fun _ poly acc -> TypeVarSet.union (free_type_vars_poly poly) acc) env TypeVarSet.empty

(* Apply substitution to all types in environment *)
let apply_substitution_env (subst : substitution) (env : type_env) : type_env =
  TypeEnv.map (apply_substitution_poly subst) env

(* ============================================================
   Type Map
   ============================================================
   
   Maps expression IDs to their inferred types.
   This allows codegen to lookup types without re-inferring.
*)

type type_map = (int, mono_type) Hashtbl.t

let create_type_map () : type_map = Hashtbl.create 256

let record_type (type_map : type_map) (expr : AST.expression) (t : mono_type) : unit =
  Hashtbl.add type_map expr.id t

let apply_substitution_type_map (subst : substitution) (type_map : type_map) : unit =
  Hashtbl.iter
    (fun id ty ->
      let ty' = apply_substitution subst ty in
      Hashtbl.replace type_map id ty')
    type_map

type symbol_id = int

type symbol_kind =
  | BuiltinValue
  | TopLevelLet
  | LocalLet
  | Param
  | PatternVar
  | TypeSym
  | TypeAliasSym
  | ShapeSym
  | TraitSym
  | EnumSym
  | EnumVariantSym
  | ImplMethodParam

type symbol = {
  id : symbol_id;
  name : string;
  kind : symbol_kind;
  definition_pos : int;
  definition_end_pos : int;
  file_id : string option;
}

type prebound_symbol_info = {
  kind : symbol_kind;
  definition_pos : int;
  definition_end_pos : int;
  file_id : string option;
}

let symbol_kind_tag = function
  | BuiltinValue -> "builtin-value"
  | TopLevelLet -> "toplevel-let"
  | LocalLet -> "local-let"
  | Param -> "param"
  | PatternVar -> "pattern-var"
  | TypeSym -> "type"
  | TypeAliasSym -> "type"
  | ShapeSym -> "shape"
  | TraitSym -> "trait"
  | EnumSym -> "enum"
  | EnumVariantSym -> "enum-variant"
  | ImplMethodParam -> "impl-method-param"

(* ============================================================
   Inference Session State
   ============================================================
   
   Tracks mutable per-inference-run state:
   - fresh type-variable counter
   - trait constraints attached to type variables
   
   We keep this state explicit so callers can choose:
   - isolated runs (fresh state per check), or
   - session runs (shared state across multiple checks, e.g. REPL).
*)

type inference_state = {
  fresh_var_counter : int ref;
  constraint_store : (string, Constraints.t list) Hashtbl.t;
  constrained_field_store : (string, (Constraints.t * record_field_type) list) Hashtbl.t;
  type_var_user_names : (string, string) Hashtbl.t;
  top_level_placeholder_store : (string, mono_type) Hashtbl.t;
  symbol_table : (symbol_id, symbol) Hashtbl.t;
  symbol_key_store : (symbol_id, string) Hashtbl.t;
  identifier_symbol_store : (int, symbol_id) Hashtbl.t;
  method_resolution_store : (int, method_resolution) Hashtbl.t;
}

and method_resolution =
  | TraitMethod of string
  | DynamicTraitMethod of string
  | InherentMethod
  | QualifiedTraitMethod of string (* Trait.method(receiver, args...) *)
  | QualifiedInherentMethod (* Type.method(receiver, args...) *)
  | FieldFunctionCall

and method_call_attempt = {
  mca_subst : substitution;
  mca_return_type : mono_type;
  mca_resolution : method_resolution;
  mca_effectful : bool;
  mca_method_type_args : mono_type list option;
}

let create_inference_state () : inference_state =
  {
    fresh_var_counter = ref 0;
    constraint_store = Hashtbl.create 64;
    constrained_field_store = Hashtbl.create 64;
    type_var_user_names = Hashtbl.create 32;
    top_level_placeholder_store = Hashtbl.create 64;
    symbol_table = Hashtbl.create 256;
    symbol_key_store = Hashtbl.create 256;
    identifier_symbol_store = Hashtbl.create 512;
    method_resolution_store = Hashtbl.create 128;
  }

let active_inference_state : inference_state ref = ref (create_inference_state ())
let global_method_resolution_store : (int, method_resolution) Hashtbl.t = Hashtbl.create 256
let global_method_type_args_store : (int, mono_type list) Hashtbl.t = Hashtbl.create 64
let global_effectful_method_call_store : (int, bool) Hashtbl.t = Hashtbl.create 128

let global_trait_object_coercion_store : (int, Resolution_artifacts.trait_object_coercion) Hashtbl.t =
  Hashtbl.create 64

type placeholder_rewrite_map = (int, AST.expression) Hashtbl.t

type placeholder_callback_expectation =
  | PlaceholderCallbackNotCallable
  | PlaceholderCallbackPureAllowed
  | PlaceholderCallbackEffectfulOnly

let global_placeholder_rewrite_store : placeholder_rewrite_map = Hashtbl.create 64
let global_synthetic_expr_id_counter : int ref = ref (-1)
let global_diagnostics : Diagnostic.t list ref = ref []

(* Track method names being defined in the current inherent impl, so recursive calls
   can find them while body inference is in progress. Pairs: (for_type, method_name). *)
let current_inherent_impl_methods : (mono_type * string) list ref = ref []

let with_inference_state (state : inference_state) (f : unit -> 'a) : 'a =
  let previous = !active_inference_state in
  active_inference_state := state;
  try
    let result = f () in
    active_inference_state := previous;
    result
  with exn ->
    active_inference_state := previous;
    raise exn

let current_constraint_store () : (string, Constraints.t list) Hashtbl.t =
  !active_inference_state.constraint_store

let current_constrained_field_store () : (string, (Constraints.t * record_field_type) list) Hashtbl.t =
  !active_inference_state.constrained_field_store

let current_type_var_user_names_store () : (string, string) Hashtbl.t =
  !active_inference_state.type_var_user_names

let current_top_level_placeholder_store () : (string, mono_type) Hashtbl.t =
  !active_inference_state.top_level_placeholder_store

let current_symbol_table () : (symbol_id, symbol) Hashtbl.t = !active_inference_state.symbol_table
let current_symbol_key_store () : (symbol_id, string) Hashtbl.t = !active_inference_state.symbol_key_store

let current_identifier_symbol_store () : (int, symbol_id) Hashtbl.t =
  !active_inference_state.identifier_symbol_store

let stable_symbol_id_from_key (key : string) (salt : int) : symbol_id =
  let salted =
    if salt = 0 then
      key
    else
      key ^ "#" ^ string_of_int salt
  in
  let hex = Digest.to_hex (Digest.string salted) in
  let raw = Int64.of_string ("0x" ^ String.sub hex 0 15) |> Int64.logand 0x3fffffffffffffffL |> Int64.to_int in
  if raw = 0 then
    1
  else
    raw

let rec allocate_stable_symbol_id (key : string) (salt : int) : symbol_id =
  let id = stable_symbol_id_from_key key salt in
  let key_store = current_symbol_key_store () in
  match Hashtbl.find_opt key_store id with
  | None ->
      Hashtbl.replace key_store id key;
      id
  | Some existing when existing = key -> id
  | Some _ -> allocate_stable_symbol_id key (salt + 1)

let register_symbol
    ~(name : string) ~(kind : symbol_kind) ~(pos : int) ~(end_pos : int) ~(file_id : string option) : symbol_id =
  let key =
    Printf.sprintf "%s|%s|%s|%d|%d" (symbol_kind_tag kind) name
      (match file_id with
      | Some f -> f
      | None -> "<none>")
      pos end_pos
  in
  let id = allocate_stable_symbol_id key 0 in
  let table = current_symbol_table () in
  if Hashtbl.mem table id then
    id
  else (
    Hashtbl.replace table id { id; name; kind; definition_pos = pos; definition_end_pos = end_pos; file_id };
    id)

let record_identifier_symbol (expr_id : int) (symbol_id : symbol_id) : unit =
  Hashtbl.replace (current_identifier_symbol_store ()) expr_id symbol_id

let lookup_identifier_symbol (expr_id : int) : symbol_id option =
  Hashtbl.find_opt (current_identifier_symbol_store ()) expr_id

let lookup_symbol (id : symbol_id) : symbol option = Hashtbl.find_opt (current_symbol_table ()) id

let lookup_identifier_symbol_in_state (state : inference_state) (expr_id : int) : symbol_id option =
  Hashtbl.find_opt state.identifier_symbol_store expr_id

let lookup_symbol_in_state (state : inference_state) (id : symbol_id) : symbol option =
  Hashtbl.find_opt state.symbol_table id

let symbol_table_bindings () : (symbol_id * symbol) list = Hashtbl.to_seq (current_symbol_table ()) |> List.of_seq

let identifier_symbol_bindings () : (int * symbol_id) list =
  Hashtbl.to_seq (current_identifier_symbol_store ()) |> List.of_seq

let symbol_table_bindings_in_state (state : inference_state) : (symbol_id * symbol) list =
  Hashtbl.to_seq state.symbol_table |> List.of_seq

let identifier_symbol_bindings_in_state (state : inference_state) : (int * symbol_id) list =
  Hashtbl.to_seq state.identifier_symbol_store |> List.of_seq

let clear_symbol_stores () : unit =
  Hashtbl.clear (current_symbol_table ());
  Hashtbl.clear (current_symbol_key_store ());
  Hashtbl.clear (current_identifier_symbol_store ())

let clear_top_level_placeholders () : unit = Hashtbl.clear (current_top_level_placeholder_store ())

let set_top_level_placeholder (name : string) (typ : mono_type) : unit =
  Hashtbl.replace (current_top_level_placeholder_store ()) name typ

let lookup_top_level_placeholder (name : string) : mono_type option =
  Hashtbl.find_opt (current_top_level_placeholder_store ()) name

let lowered_shape_fields_for_constraints (constraints : Constraints.t list) :
    (Constraints.t * record_field_type) list =
  Trait_solver.expand_constraint_refs constraints
  |> List.concat_map (fun constraint_ref ->
         match constraint_ref with
         | Constraints.ShapeConstraint shape_name -> (
             match Type_registry.instantiate_shape_fields shape_name [] with
             | Some (Ok fields) ->
                 List.map
                   (fun (field : record_field_type) ->
                     (constraint_ref, { name = field.name; typ = canonicalize_mono_type field.typ }))
                   fields
             | Some (Error _) | None -> [])
         | Constraints.TraitConstraint _ -> [])

let update_type_var_constrained_fields (type_var_name : string) (constraints : Constraints.t list) : unit =
  let store = current_constrained_field_store () in
  let lowered = lowered_shape_fields_for_constraints constraints in
  if lowered = [] then
    Hashtbl.remove store type_var_name
  else
    Hashtbl.replace store type_var_name lowered

let add_type_var_constraint_refs (type_var_name : string) (constraints : Constraints.t list) : unit =
  let store = current_constraint_store () in
  let constraints = List.map Constraints.canonicalize constraints in
  let merged =
    match Hashtbl.find_opt store type_var_name with
    | None -> Constraints.dedupe_preserve_order constraints
    | Some existing ->
        let seen = Hashtbl.create (List.length existing + List.length constraints) in
        List.iter (fun constraint_ref -> Hashtbl.replace seen (Constraints.name constraint_ref) ()) existing;
        let rec collect_new_unique rev_new = function
          | [] -> existing @ List.rev rev_new
          | constraint_ref :: rest ->
              let constraint_name = Constraints.name constraint_ref in
              if Hashtbl.mem seen constraint_name then
                collect_new_unique rev_new rest
              else (
                Hashtbl.replace seen constraint_name ();
                collect_new_unique (constraint_ref :: rev_new) rest)
        in
        collect_new_unique [] constraints
  in
  Hashtbl.replace store type_var_name merged;
  update_type_var_constrained_fields type_var_name merged

let add_type_var_constraints (type_var_name : string) (constraints : string list) : unit =
  add_type_var_constraint_refs type_var_name (Constraints.of_names constraints)

let lookup_type_var_constraints (type_var_name : string) : Constraints.t list =
  match Hashtbl.find_opt (current_constraint_store ()) type_var_name with
  | Some constraints -> constraints
  | None -> []

let lookup_type_var_constrained_fields (type_var_name : string) : (Constraints.t * record_field_type) list =
  match Hashtbl.find_opt (current_constrained_field_store ()) type_var_name with
  | Some fields -> fields
  | None -> []

let clear_constraint_store () : unit =
  Hashtbl.clear (current_constraint_store ());
  Hashtbl.clear (current_constrained_field_store ())

let global_method_def_store : (int, Resolution_artifacts.typed_method_def) Hashtbl.t = Hashtbl.create 64

let clear_method_resolution_store () : unit =
  Hashtbl.clear global_method_resolution_store;
  Hashtbl.clear global_method_type_args_store;
  Hashtbl.clear global_effectful_method_call_store;
  Hashtbl.clear global_trait_object_coercion_store;
  Hashtbl.clear global_placeholder_rewrite_store;
  Hashtbl.clear global_method_def_store;
  global_synthetic_expr_id_counter := -1;
  global_diagnostics := []

let emit_diagnostic (diag : Diagnostic.t) : unit = global_diagnostics := diag :: !global_diagnostics
let snapshot_diagnostics () : Diagnostic.t list = List.rev !global_diagnostics

let record_method_def (method_id : int) (def : Resolution_artifacts.typed_method_def) : unit =
  Hashtbl.replace global_method_def_store method_id def

let snapshot_method_def_store () : (int, Resolution_artifacts.typed_method_def) Hashtbl.t =
  Hashtbl.copy global_method_def_store

let record_placeholder_rewrite (original_expr : AST.expression) (rewritten_expr : AST.expression) : unit =
  Hashtbl.replace global_placeholder_rewrite_store original_expr.id rewritten_expr

let snapshot_placeholder_rewrite_store () : placeholder_rewrite_map =
  Hashtbl.copy global_placeholder_rewrite_store

let placeholder_section_rewrite_disable_depth = ref 0
let placeholder_section_fallback_disable_depth = ref 0
let placeholder_rewrite_enabled () : bool = !placeholder_section_rewrite_disable_depth = 0

let placeholder_section_fallback_enabled () : bool =
  placeholder_rewrite_enabled () && !placeholder_section_fallback_disable_depth = 0

let with_placeholder_section_rewrite_disabled (f : unit -> 'a) : 'a =
  incr placeholder_section_rewrite_disable_depth;
  match f () with
  | result ->
      decr placeholder_section_rewrite_disable_depth;
      result
  | exception exn ->
      decr placeholder_section_rewrite_disable_depth;
      raise exn

let with_placeholder_section_fallback_disabled (f : unit -> 'a) : 'a =
  incr placeholder_section_fallback_disable_depth;
  match f () with
  | result ->
      decr placeholder_section_fallback_disable_depth;
      result
  | exception exn ->
      decr placeholder_section_fallback_disable_depth;
      raise exn

let fresh_synthetic_expr_id () : int =
  let id = !global_synthetic_expr_id_counter in
  decr global_synthetic_expr_id_counter;
  id

let clear_type_var_user_names () : unit = Hashtbl.clear (current_type_var_user_names_store ())

let record_type_var_user_name ~(fresh_name : string) ~(user_name : string) : unit =
  Hashtbl.replace (current_type_var_user_names_store ()) fresh_name user_name

let lookup_type_var_user_name (fresh_name : string) : string option =
  Hashtbl.find_opt (current_type_var_user_names_store ()) fresh_name

let lookup_type_var_user_name_in_state (state : inference_state) (fresh_name : string) : string option =
  Hashtbl.find_opt state.type_var_user_names fresh_name

let type_var_user_name_bindings_in_state (state : inference_state) : (string * string) list =
  Hashtbl.to_seq state.type_var_user_names |> List.of_seq

let user_named_type_bindings_in_env (env : type_env) : (string * mono_type) list =
  let add_binding acc fresh_name =
    match lookup_type_var_user_name fresh_name with
    | Some user_name when not (List.mem_assoc user_name acc) -> (user_name, TVar fresh_name) :: acc
    | _ -> acc
  in
  let rec collect_mono acc = function
    | TInt | TFloat | TBool | TString | TNull -> acc
    | TVar fresh_name -> add_binding acc fresh_name
    | TFun (arg, ret, _) -> collect_mono (collect_mono acc arg) ret
    | TArray element -> collect_mono acc element
    | THash (key, value) -> collect_mono (collect_mono acc key) value
    | TRecord (fields, row) ->
        let acc =
          List.fold_left (fun inner (field : record_field_type) -> collect_mono inner field.typ) acc fields
        in
        let acc =
          match row with
          | Some row_type -> collect_mono acc row_type
          | None -> acc
        in
        acc
    | TRowVar _ | TTraitObject _ -> acc
    | TUnion members | TIntersection members | TEnum (_, members) | TNamed (_, members) ->
        List.fold_left collect_mono acc members
  in
  let collect_poly acc = function
    | Forall ([], mono) -> collect_mono acc mono
    | Forall (_ :: _, _) -> acc
  in
  TypeEnv.fold (fun _ poly acc -> collect_poly acc poly) env [] |> List.rev

let record_method_resolution (expr : AST.expression) (resolution : method_resolution) : unit =
  Hashtbl.replace global_method_resolution_store expr.id resolution

let lookup_method_resolution (expr_id : int) : method_resolution option =
  Hashtbl.find_opt global_method_resolution_store expr_id

let record_effectful_method_call (expr : AST.expression) (is_effectful : bool) : unit =
  Hashtbl.replace global_effectful_method_call_store expr.id is_effectful

let lookup_effectful_method_call (expr_id : int) : bool =
  match Hashtbl.find_opt global_effectful_method_call_store expr_id with
  | Some is_effectful -> is_effectful
  | None -> false

let snapshot_method_resolution_store () : (int, method_resolution) Hashtbl.t =
  Hashtbl.copy global_method_resolution_store

let record_trait_object_coercion (expr : AST.expression) (coercion : Resolution_artifacts.trait_object_coercion) :
    unit =
  Hashtbl.replace global_trait_object_coercion_store expr.id
    { coercion with target_traits = Types.normalize_trait_object_traits coercion.target_traits }

let snapshot_trait_object_coercion_store () : (int, Resolution_artifacts.trait_object_coercion) Hashtbl.t =
  Hashtbl.copy global_trait_object_coercion_store

let record_method_type_args (expr : AST.expression) (type_args : mono_type list) : unit =
  if type_args <> [] then
    Hashtbl.replace global_method_type_args_store expr.id type_args

let snapshot_method_type_args_store () : (int, mono_type list) Hashtbl.t =
  Hashtbl.copy global_method_type_args_store

let apply_substitution_method_type_args_store (subst : substitution) : unit =
  Hashtbl.iter
    (fun id args ->
      let args' = List.map (fun t -> apply_substitution subst t) args in
      Hashtbl.replace global_method_type_args_store id args')
    global_method_type_args_store

let%test "trait object coercion store snapshots and clears" =
  clear_method_resolution_store ();
  let expr = AST.mk_expr ~id:77 (AST.Integer 1L) in
  record_trait_object_coercion expr Resolution_artifacts.{ target_traits = [ "Show"; "Eq" ]; source_type = TInt };
  let snapshot = snapshot_trait_object_coercion_store () in
  let before_clear =
    Hashtbl.find_opt snapshot expr.id
    = Some Resolution_artifacts.{ target_traits = [ "Eq"; "Show" ]; source_type = TInt }
  in
  clear_method_resolution_store ();
  before_clear && Hashtbl.length (snapshot_trait_object_coercion_store ()) = 0

let%test "dynamic trait method resolution snapshots and clears" =
  clear_method_resolution_store ();
  let expr = AST.mk_expr ~id:91 (AST.Integer 1L) in
  record_method_resolution expr (DynamicTraitMethod "Show");
  let snapshot = snapshot_method_resolution_store () in
  let before_clear = Hashtbl.find_opt snapshot expr.id = Some (DynamicTraitMethod "Show") in
  clear_method_resolution_store ();
  before_clear && Hashtbl.length (snapshot_method_resolution_store ()) = 0

type obligation_reason = GenericConstraint of string

type obligation = {
  constraint_ref : Constraints.t;
  typ : mono_type;
  reason : obligation_reason;
}

let obligation_reason_to_string = function
  | GenericConstraint type_var_name -> Printf.sprintf "constraint on type variable '%s'" type_var_name

type canonical_inference_var =
  | CanonicalTypeVar of int
  | CanonicalRowVar of int
  | NonCanonicalVar of string

let parse_canonical_inference_var (var_name : string) : canonical_inference_var =
  let len = String.length var_name in
  if len < 2 then
    NonCanonicalVar var_name
  else
    let kind = var_name.[0] in
    let numeric_suffix = String.sub var_name 1 (len - 1) in
    match int_of_string_opt numeric_suffix with
    | Some n when kind = 't' -> CanonicalTypeVar n
    | Some n when kind = 'r' -> CanonicalRowVar n
    | _ -> NonCanonicalVar var_name

let compare_inference_var_names (left : string) (right : string) : int =
  match (parse_canonical_inference_var left, parse_canonical_inference_var right) with
  | CanonicalTypeVar l, CanonicalTypeVar r ->
      let order = Int.compare l r in
      if order = 0 then
        String.compare left right
      else
        order
  | CanonicalRowVar l, CanonicalRowVar r ->
      let order = Int.compare l r in
      if order = 0 then
        String.compare left right
      else
        order
  | CanonicalTypeVar _, CanonicalRowVar _ -> -1
  | CanonicalRowVar _, CanonicalTypeVar _ -> 1
  | (CanonicalTypeVar _ | CanonicalRowVar _), NonCanonicalVar _ -> -1
  | NonCanonicalVar _, (CanonicalTypeVar _ | CanonicalRowVar _) -> 1
  | NonCanonicalVar l, NonCanonicalVar r -> String.compare l r

let obligations_from_substitution (subst : substitution) : obligation list =
  let build_for_var (var_name : string) (resolved_type : mono_type) : obligation list =
    let constraints = lookup_type_var_constraints var_name in
    if constraints = [] then
      []
    else
      let concrete_type =
        (* Follow the substitution chain to get the fully resolved type *)
        apply_substitution subst resolved_type
      in
      match concrete_type with
      | TVar target_var ->
          (* Still unresolved - propagate constraints to the target variable
             so they survive subsequent lookups (e.g., method dispatch on TVars). *)
          add_type_var_constraint_refs target_var constraints;
          []
      | _ ->
          List.map
            (fun constraint_ref -> { constraint_ref; typ = concrete_type; reason = GenericConstraint var_name })
            constraints
  in
  let ordered_bindings =
    SubstMap.bindings subst
    |> List.sort (fun (left_name, _) (right_name, _) -> compare_inference_var_names left_name right_name)
  in
  List.fold_left
    (fun acc (var_name, resolved_type) -> List.rev_append (build_for_var var_name resolved_type) acc)
    [] ordered_bindings
  |> List.rev

let verify_obligation (o : obligation) : (unit, Diagnostic.t) result =
  match Trait_solver.check_constraint_ref o.typ o.constraint_ref with
  | Ok () -> Ok ()
  | Error diag ->
      let reason = obligation_reason_to_string o.reason in
      Error { diag with message = Printf.sprintf "Constraint obligation failed (%s): %s" reason diag.message }

let verify_obligations (obligations : obligation list) : (unit, Diagnostic.t) result =
  let rec go = function
    | [] -> Ok ()
    | o :: rest -> (
        match verify_obligation o with
        | Ok () -> go rest
        | Error _ as err -> err)
  in
  go obligations

module ConstraintCtx = Map.Make (String)

type constraint_ctx = Constraints.t list ConstraintCtx.t

let empty_constraints : constraint_ctx = ConstraintCtx.empty

let add_constraint (ctx : constraint_ctx) (type_var : string) (constraints : string list) : constraint_ctx =
  ConstraintCtx.add type_var (Constraints.of_names constraints) ctx

let lookup_constraints (ctx : constraint_ctx) (type_var : string) : Constraints.t list =
  match ConstraintCtx.find_opt type_var ctx with
  | Some constraints -> constraints
  | None -> []

(* Apply substitution to constraint context - when t0 -> Int, update constraints *)
let apply_substitution_constraints (subst : substitution) (ctx : constraint_ctx) :
    (constraint_ctx, Diagnostic.t) result =
  ConstraintCtx.fold
    (fun var traits acc ->
      let* acc = acc in
      match SubstMap.find_opt var subst with
      | Some (TVar new_var) -> Ok (ConstraintCtx.add new_var traits acc)
      | Some concrete_type -> (
          match Trait_solver.check_constraint_refs concrete_type traits with
          | Ok () -> Ok acc
          | Error diag -> Error diag)
      | None -> Ok (ConstraintCtx.add var traits acc))
    ctx (Ok empty_constraints)

(* Verify that all type variable constraints in the global constraint_store
   are satisfied after applying a substitution.
   Returns Error if any constraint is violated. *)
let verify_constraints_in_substitution (subst : substitution) : (unit, Diagnostic.t) result =
  let obligations = obligations_from_substitution subst in
  verify_obligations obligations

let propagate_type_var_constraints_through_substitution (subst : substitution) : unit =
  SubstMap.iter
    (fun type_var_name resolved_type ->
      let constraints = lookup_type_var_constraints type_var_name in
      if constraints <> [] then
        match apply_substitution subst resolved_type with
        | TVar target_var_name -> add_type_var_constraint_refs target_var_name constraints
        | _ -> ())
    subst

let enforce_trait_requirement_on_type (typ : mono_type) (trait_name : string) : (unit, Diagnostic.t) result =
  if Trait_registry.lookup_trait trait_name = None then
    Ok ()
  else
    let typ' = canonicalize_mono_type typ in
    match typ' with
    | TVar type_var_name ->
        add_type_var_constraints type_var_name [ trait_name ];
        Ok ()
    | _ -> (
        match Trait_solver.satisfies_trait typ' trait_name with
        | Ok () -> Ok ()
        | Error diag -> Error diag)

(* ============================================================
   Fresh Type Variables
   ============================================================
   
   We need to generate unique type variable names during inference.
   Using a simple counter: t0, t1, t2, ...
*)

let fresh_type_var () : mono_type =
  let counter = !active_inference_state.fresh_var_counter in
  let n = !counter in
  counter := n + 1;
  TVar ("t" ^ string_of_int n)

let fresh_row_var () : mono_type =
  let counter = !active_inference_state.fresh_var_counter in
  let n = !counter in
  counter := n + 1;
  TRowVar ("r" ^ string_of_int n)

let has_prefix (s : string) (prefix : string) : bool =
  let len_s = String.length s in
  let len_prefix = String.length prefix in
  len_s >= len_prefix && String.sub s 0 len_prefix = prefix

let rec row_vars_in_type (mono : mono_type) : TypeVarSet.t =
  match mono with
  | TInt | TFloat | TBool | TString | TNull | TVar _ -> TypeVarSet.empty
  | TFun (arg, ret, _) -> TypeVarSet.union (row_vars_in_type arg) (row_vars_in_type ret)
  | TArray element -> row_vars_in_type element
  | THash (k, v) -> TypeVarSet.union (row_vars_in_type k) (row_vars_in_type v)
  | TRecord (fields, row) ->
      let field_rows =
        List.fold_left
          (fun acc (f : Types.record_field_type) -> TypeVarSet.union acc (row_vars_in_type f.typ))
          TypeVarSet.empty fields
      in
      let row_rows =
        match row with
        | None -> TypeVarSet.empty
        | Some r -> row_vars_in_type r
      in
      TypeVarSet.union field_rows row_rows
  | TRowVar name -> TypeVarSet.singleton name
  | TTraitObject _ -> TypeVarSet.empty
  | TUnion members ->
      List.fold_left (fun acc t -> TypeVarSet.union acc (row_vars_in_type t)) TypeVarSet.empty members
  | TIntersection members ->
      List.fold_left (fun acc t -> TypeVarSet.union acc (row_vars_in_type t)) TypeVarSet.empty members
  | TEnum (_, args) | TNamed (_, args) ->
      List.fold_left (fun acc t -> TypeVarSet.union acc (row_vars_in_type t)) TypeVarSet.empty args

let is_trait_object_row = function
  | TRowVar row_name -> has_prefix row_name "trait_obj_row_"
  | _ -> false

(* Reset counter (useful for testing to get predictable names) *)
let reset_fresh_counter () =
  let state = !active_inference_state in
  state.fresh_var_counter := 0;
  clear_constraint_store ();
  clear_type_var_user_names ();
  clear_top_level_placeholders ();
  clear_symbol_stores ()

(* ============================================================
   Instantiation
   ============================================================
   
   When we use a polymorphic variable, we "instantiate" it by
   replacing all quantified type variables with fresh ones.
   
   Example: ∀a. a -> a  becomes  t0 -> t0 (with fresh t0)
*)

let instantiate (Forall (quantified_vars, mono)) : mono_type =
  let row_vars = row_vars_in_type mono in
  (* Preserve kinding: quantified row vars must instantiate to fresh row vars, not plain type vars. *)
  let subst =
    List.fold_left
      (fun acc var ->
        let fresh =
          if TypeVarSet.mem var row_vars then
            fresh_row_var ()
          else
            fresh_type_var ()
        in
        (* Copy constraints from old type vars to new ones *)
        (match fresh with
        | TVar new_var ->
            let constraints = lookup_type_var_constraints var in
            if constraints <> [] then
              add_type_var_constraint_refs new_var constraints
        | _ -> ());
        SubstMap.add var fresh acc)
      SubstMap.empty quantified_vars
  in
  apply_substitution subst mono

let instantiate_with_explicit_type_args
    ~(mk_error : code:string -> message:string -> Diagnostic.t)
    (Forall (quantified_vars, mono) as poly_type)
    (type_args : AST.type_expr list) : (mono_type, Diagnostic.t) result =
  let rec convert_all acc = function
    | [] -> Ok (List.rev acc)
    | te :: rest -> (
        match Annotation.type_expr_to_mono_type te with
        | Error diag -> Error (mk_error ~code:diag.code ~message:diag.message)
        | Ok mono_arg -> convert_all (mono_arg :: acc) rest)
  in
  if quantified_vars = [] then
    match type_args with
    | [] -> Ok mono
    | _ ->
        Error
          (mk_error ~code:"type-constructor"
             ~message:
               (Printf.sprintf "Value of type %s takes no type parameters, but %d were provided"
                  (poly_type_to_string poly_type) (List.length type_args)))
  else if List.length type_args <> List.length quantified_vars then
    Error
      (mk_error ~code:"type-constructor"
         ~message:
           (Printf.sprintf "Expected %d type argument(s), but %d were provided" (List.length quantified_vars)
              (List.length type_args)))
  else
    let open Result in
    let ( let* ) = bind in
    let* mono_args = convert_all [] type_args in
    let* () =
      List.fold_left2
        (fun acc quantified_var mono_arg ->
          match acc with
          | Error _ -> acc
          | Ok () ->
              let constraints = lookup_type_var_constraints quantified_var in
              (match mono_arg with
              | TVar type_var_name when constraints <> [] ->
                  add_type_var_constraint_refs type_var_name constraints
              | _ -> ());
              List.fold_left
                (fun inner_acc constraint_ref ->
                  match inner_acc with
                  | Error _ -> inner_acc
                  | Ok () -> (
                      match canonicalize_mono_type mono_arg with
                      | TVar _ | TRowVar _ -> Ok ()
                      | concrete_arg -> (
                          match Trait_solver.check_constraint_ref concrete_arg constraint_ref with
                          | Ok () -> Ok ()
                          | Error diag -> Error (mk_error ~code:diag.code ~message:diag.message))))
                (Ok ()) constraints)
        (Ok ()) quantified_vars mono_args
    in
    let subst = substitution_of_list (List.combine quantified_vars mono_args) in
    Ok (apply_substitution subst mono)

(* ============================================================
   Generalization
   ============================================================
   
   When we bind a let, we "generalize" the type by quantifying over
   all type variables that are free in the type but NOT free in the
   environment.
   
   Example: In empty env, Int -> t0  generalizes to  ∀t0. Int -> t0
   But if t0 is used elsewhere in env, we can't generalize over it.
*)

let generalize (env : type_env) (mono : mono_type) : poly_type =
  let free_in_mono = free_type_vars mono in
  let free_in_env = free_type_vars_env env in
  let can_generalize =
    TypeVarSet.diff free_in_mono free_in_env
    |> TypeVarSet.filter (fun var_name -> not (has_prefix var_name "trait_obj_row_"))
  in
  Forall (TypeVarSet.elements can_generalize, mono)

(* ============================================================
   Inference Errors
   ============================================================ *)

(* ============================================================
   Direct Diagnostic Construction
   ============================================================ *)

(* Create an error without position *)
let error ~code ~message = Diagnostic.error_no_span ~code ~message

(* Create an error with position from an expression *)
let error_at ~code ~message (expr : AST.expression) =
  let file_id = Option.value expr.file_id ~default:"<unknown>" in
  Diagnostic.error_with_span ~code ~message ~file_id ~start_pos:expr.pos ~end_pos:expr.end_pos ()

(* Create an error with position from a statement *)
let error_at_stmt ~code ~message (stmt : AST.statement) =
  let file_id = Option.value stmt.file_id ~default:"<unknown>" in
  Diagnostic.error_with_span ~code ~message ~file_id ~start_pos:stmt.pos ~end_pos:stmt.end_pos ()

let warning_at_stmt ~code ~message (stmt : AST.statement) =
  { (error_at_stmt ~code ~message stmt) with severity = Diagnostic.Warning }

let unescape_internal_component (name : string) : string =
  let buffer = Buffer.create (String.length name) in
  let rec go idx =
    if idx >= String.length name then
      ()
    else if
      idx + 5 < String.length name
      && name.[idx] = '_'
      && name.[idx + 1] = 'u'
      &&
      let is_hex c =
        match c with
        | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
        | _ -> false
      in
      is_hex name.[idx + 2] && is_hex name.[idx + 3] && is_hex name.[idx + 4] && is_hex name.[idx + 5]
    then
      let hex = String.sub name (idx + 2) 4 in
      (match int_of_string_opt ("0x" ^ hex) with
      | Some code when code >= 0 && code <= 255 ->
          Buffer.add_char buffer (Char.chr code);
          go (idx + 6)
      | _ ->
          Buffer.add_string buffer (String.sub name idx 6);
          go (idx + 6))
    else (
      Buffer.add_char buffer name.[idx];
      go (idx + 1))
  in
  go 0;
  Buffer.contents buffer

let display_binding_name (name : string) : string =
  let len = String.length name in
  let rec find_suffix_start idx =
    if idx <= 0 then
      0
    else if name.[idx - 1] = '_' && name.[idx] = '_' then
      idx + 1
    else
      find_suffix_start (idx - 1)
  in
  let suffix_start = find_suffix_start (len - 1) in
  String.sub name suffix_start (len - suffix_start) |> unescape_internal_component

let unknown_constructor_message (type_name : string) (constructor_name : string) : string =
  Printf.sprintf "Unknown constructor: %s.%s" (display_binding_name type_name) constructor_name

let missing_constructor_or_inherent_method_message (type_name : string) (member_name : string) : string =
  Printf.sprintf "Type '%s' has no constructor or inherent method '%s'" (display_binding_name type_name) member_name

let unknown_type_message (type_name : string) : string = Printf.sprintf "Unknown type: %s" type_name

let canonical_enum_name_of_source_name (enum_name : string) : string =
  match Annotation.lookup_enum_by_source_name enum_name with
  | Some enum_def -> enum_def.name
  | None -> enum_name

let attach_constraint_refs_if_tvar (typ : mono_type) (constraints : Constraints.t list) : unit =
  match typ with
  | TVar type_var_name when constraints <> [] -> add_type_var_constraint_refs type_var_name constraints
  | _ -> ()

let instantiate_method_generics_for_value (method_sig : Trait_registry.method_sig) :
    Trait_registry.method_sig * mono_type list =
  let method_subst =
    List.fold_left
      (fun acc (param_name, constraints) ->
        let fresh = fresh_type_var () in
        attach_constraint_refs_if_tvar fresh constraints;
        SubstMap.add param_name fresh acc)
      SubstMap.empty method_sig.method_generics
  in
  let instantiated_sig =
    {
      method_sig with
      method_params =
        List.map (fun (name, ty) -> (name, apply_substitution method_subst ty)) method_sig.method_params;
      method_return_type = apply_substitution method_subst method_sig.method_return_type;
    }
  in
  let resolved_method_type_args =
    List.map
      (fun (param_name, _constraints) ->
        apply_substitution method_subst (TVar param_name) |> canonicalize_mono_type)
      method_sig.method_generics
  in
  (instantiated_sig, resolved_method_type_args)

let callable_type_of_method_sig (method_sig : Trait_registry.method_sig) : mono_type =
  List.fold_right
    (fun (_param_name, param_type) acc -> TFun (param_type, acc, method_sig.method_effect = `Effectful))
    method_sig.method_params method_sig.method_return_type

let resolve_dotted_type_name (name : string) : mono_type option =
  match Annotation.builtin_primitive_type name with
  | Some primitive -> Some primitive
  | None when Type_registry.is_named_type_name name -> (
      match Type_registry.named_type_arity name with
      | Some arity -> Some (TNamed (name, List.init arity (fun _ -> fresh_type_var ())))
      | None -> None)
  | None -> (
      match Annotation.lookup_type_alias name with
      | Some alias_info when alias_info.alias_type_params = [] -> (
          match Annotation.type_expr_to_mono_type alias_info.alias_body with
          | Ok mono -> Some mono
          | Error _ -> None)
      | _ -> (
          match Annotation.lookup_enum_by_source_name name with
          | Some enum_def ->
              let fresh_args = List.map (fun _ -> fresh_type_var ()) enum_def.type_params in
              Some (TEnum (enum_def.name, fresh_args))
          | None -> None))

(* Result type for inference *)
type 'a infer_result = ('a, Diagnostic.t) result

let rec type_expr_contains_intersection (te : AST.type_expr) : bool =
  match te with
  | AST.TIntersection _ -> true
  | AST.TApp (_, args) | AST.TUnion args -> List.exists type_expr_contains_intersection args
  | AST.TArrow (params, ret, _) ->
      List.exists type_expr_contains_intersection params || type_expr_contains_intersection ret
  | AST.TRecord (fields, _row_var) ->
      List.exists (fun (field : AST.record_type_field) -> type_expr_contains_intersection field.field_type) fields
  | AST.TCon _ | AST.TVar _ | AST.TTraitObject _ -> false

let should_preserve_annotation_error (te : AST.type_expr) (diag : Diagnostic.t) : bool =
  diag.code = "type-open-row-rejected"
  || diag.code = "type-intersection-invalid"
  || (diag.code = "type-annotation-invalid" && type_expr_contains_intersection te)

(* Annotation helper: convert type expression, preserving hard annotation errors
   while still falling back to a fresh variable for non-fatal future cases. *)
let annotation_or_fresh te =
  match Annotation.type_expr_to_mono_type te with
  | Error d when should_preserve_annotation_error te d -> Error d
  | Error _ -> Ok (fresh_type_var ())
  | Ok t -> Ok t

let annotation_with_bindings_or_fresh (type_bindings : (string * mono_type) list) te =
  match Annotation.type_expr_to_mono_type_with type_bindings te with
  | Error d when should_preserve_annotation_error te d -> Error d
  | Error _ -> Ok (fresh_type_var ())
  | Ok t -> Ok t

let rec has_unresolved_var (t : mono_type) : bool =
  match t with
  | TVar _ | TRowVar _ -> true
  | TFun (arg, ret, _) -> has_unresolved_var arg || has_unresolved_var ret
  | TArray elem -> has_unresolved_var elem
  | THash (k, v) -> has_unresolved_var k || has_unresolved_var v
  | TRecord (fields, row) -> (
      List.exists (fun (f : record_field_type) -> has_unresolved_var f.typ) fields
      ||
      match row with
      | None -> false
      | Some r -> has_unresolved_var r)
  | TTraitObject _ -> false
  | TEnum (_, args) | TNamed (_, args) | TUnion args | TIntersection args -> List.exists has_unresolved_var args
  | TInt | TFloat | TBool | TString | TNull -> false

let rec mono_type_contains_intersection (t : mono_type) : bool =
  match t with
  | TIntersection _ -> true
  | TFun (arg, ret, _) -> mono_type_contains_intersection arg || mono_type_contains_intersection ret
  | TArray elem -> mono_type_contains_intersection elem
  | THash (k, v) -> mono_type_contains_intersection k || mono_type_contains_intersection v
  | TRecord (fields, row) -> (
      List.exists (fun (f : record_field_type) -> mono_type_contains_intersection f.typ) fields
      ||
      match row with
      | None -> false
      | Some r -> mono_type_contains_intersection r)
  | TEnum (_, args) | TNamed (_, args) | TUnion args -> List.exists mono_type_contains_intersection args
  | TVar _ | TRowVar _ | TTraitObject _ | TInt | TFloat | TBool | TString | TNull -> false

let intersection_annotation_compatible (actual : mono_type) (expected : mono_type) : bool =
  (not (has_unresolved_var actual || has_unresolved_var expected))
  && (mono_type_contains_intersection actual || mono_type_contains_intersection expected)
  &&
  match unify actual expected with
  | Ok _ -> true
  | Error _ -> false

let compatible_with_expected_type (actual : mono_type) (expected : mono_type) :
    (substitution, Diagnostic.t) result =
  if has_unresolved_var actual || has_unresolved_var expected then
    match unify actual expected with
    | Ok subst -> Ok subst
    | Error _ when Annotation.is_subtype_of actual expected || intersection_annotation_compatible actual expected
      ->
        Ok empty_substitution
    | Error e -> Error e
  else if Annotation.is_subtype_of actual expected || intersection_annotation_compatible actual expected then
    Ok empty_substitution
  else
    Error (type_mismatch actual expected)

let binding_type_for_env
    ~(value_expr : AST.expression) ~(type_annotation : AST.type_expr option) (stmt_type : mono_type) : mono_type =
  match value_expr.expr with
  | AST.Function _ -> stmt_type
  | _ -> (
      match type_annotation with
      | None -> stmt_type
      | Some type_expr -> (
          match Annotation.type_expr_to_mono_type type_expr with
          | Ok annotated_type -> annotated_type
          | Error _ -> stmt_type))

let should_monomorphize_let_binding_value (value_expr : AST.expression) : bool =
  match value_expr.expr with
  | AST.RecordLit _ -> true
  | AST.FieldAccess _ -> (
      match lookup_method_resolution value_expr.id with
      | Some (QualifiedTraitMethod _) | Some QualifiedInherentMethod -> false
      | Some (TraitMethod _) | Some (DynamicTraitMethod _) | Some InherentMethod | Some FieldFunctionCall | None
        ->
          true)
  | _ -> false

let%test "annotation_or_fresh preserves invalid intersection diagnostics" =
  match
    annotation_or_fresh (AST.TIntersection [ AST.TCon "Int"; AST.TUnion [ AST.TCon "Int"; AST.TCon "Str" ] ])
  with
  | Error d -> d.Diagnostic.code = "type-annotation-invalid"
  | Ok _ -> false

let%test "annotation_with_bindings_or_fresh preserves invalid intersection diagnostics" =
  match
    annotation_with_bindings_or_fresh []
      (AST.TIntersection [ AST.TCon "Int"; AST.TUnion [ AST.TCon "Int"; AST.TCon "Str" ] ])
  with
  | Error d -> d.Diagnostic.code = "type-annotation-invalid"
  | Ok _ -> false

type trait_object_compatibility =
  | TraitObjectAlreadyCompatible
  | TraitObjectNeedsPackaging of mono_type

let normalized_trait_object_membership (traits : string list) : string list =
  traits
  |> List.concat_map Trait_registry.trait_with_supertraits
  |> List.map Trait_registry.canonical_trait_name
  |> List.sort_uniq String.compare

let validate_trait_object_traits
    ~(mk_error : code:string -> message:string -> Diagnostic.t) (traits : string list) :
    (string list, Diagnostic.t) result =
  let normalized = Types.normalize_trait_object_traits traits in
  let dyn_type = Types.to_string (TTraitObject normalized) in
  let rec loop = function
    | [] -> Ok normalized
    | trait_name :: rest -> (
        match Constraints.of_name trait_name with
        | Constraints.ShapeConstraint shape_name ->
            Error
              (mk_error ~code:"type-trait-object-shape"
                 ~message:
                   (Printf.sprintf
                      "Trait object type %s cannot include shape '%s'; Dyn[...] accepts trait names only" dyn_type
                      shape_name))
        | Constraints.TraitConstraint canonical_trait_name -> (
            match Trait_registry.lookup_trait canonical_trait_name with
            | None ->
                Error
                  (mk_error ~code:"type-trait-object-unknown"
                     ~message:(Printf.sprintf "Unknown trait '%s' in %s" canonical_trait_name dyn_type))
            | Some _ -> loop rest))
  in
  loop normalized

let classify_trait_object_compatibility
    ~(mk_error : code:string -> message:string -> Diagnostic.t)
    (actual_type : mono_type)
    (target_traits : string list) : (trait_object_compatibility, Diagnostic.t) result =
  let* normalized_traits = validate_trait_object_traits ~mk_error target_traits in
  let target_members = normalized_trait_object_membership normalized_traits in
  let target_dyn = TTraitObject normalized_traits in
  let actual_type' = canonicalize_mono_type actual_type in
  match actual_type' with
  | TTraitObject source_traits ->
      let source_members = normalized_trait_object_membership source_traits in
      if
        List.for_all
          (fun trait_name -> List.mem (Trait_registry.canonical_trait_name trait_name) source_members)
          target_members
      then
        if Types.normalize_trait_object_traits source_traits = normalized_traits then
          Ok TraitObjectAlreadyCompatible
        else
          Ok (TraitObjectNeedsPackaging actual_type')
      else
        Error
          (mk_error ~code:"type-trait-object-mismatch"
             ~message:
               (Printf.sprintf "Cannot use %s where %s is expected" (Types.to_string actual_type')
                  (Types.to_string target_dyn)))
  | TVar type_var_name ->
      let available_constraints =
        lookup_type_var_constraints type_var_name
        |> Trait_solver.expand_constraints_with_supertraits_refs
        |> List.filter_map Constraints.trait_name_opt
        |> List.map Trait_registry.canonical_trait_name
        |> List.sort_uniq String.compare
      in
      if
        List.for_all
          (fun trait_name -> List.mem (Trait_registry.canonical_trait_name trait_name) available_constraints)
          target_members
      then
        Ok (TraitObjectNeedsPackaging actual_type')
      else
        let rendered_name =
          Option.value
            (Hashtbl.find_opt (current_type_var_user_names_store ()) type_var_name)
            ~default:type_var_name
        in
        Error
          (mk_error ~code:"type-trait-object-missing-constraint"
             ~message:
               (Printf.sprintf "Type variable '%s' is not known to satisfy %s" rendered_name
                  (Types.to_string target_dyn)))
  | concrete_type ->
      let rec check_traits = function
        | [] -> Ok (TraitObjectNeedsPackaging concrete_type)
        | trait_name :: rest -> (
            match Trait_solver.satisfies_trait concrete_type trait_name with
            | Ok () -> check_traits rest
            | Error diag ->
                Error
                  (mk_error ~code:diag.code
                     ~message:
                       (Printf.sprintf "Cannot coerce %s to %s because %s" (Types.to_string concrete_type)
                          (Types.to_string target_dyn) diag.message)))
      in
      check_traits normalized_traits

let trait_object_method_candidates (trait_names : string list) (method_name : string) :
    (string * Trait_registry.trait_def * Trait_registry.method_sig) list =
  Types.normalize_trait_object_traits trait_names
  |> List.map Trait_registry.canonical_trait_name
  |> List.filter_map (fun trait_name ->
         match Trait_registry.lookup_trait trait_name with
         | None -> None
         | Some trait_def -> (
             match Trait_registry.lookup_trait_method_with_supertraits trait_name method_name with
             | None -> None
             | Some (_origin_trait_def, method_sig) -> Some (trait_name, trait_def, method_sig)))

let instantiate_dynamic_trait_method_sig
    ~(mk_error : code:string -> message:string -> Diagnostic.t)
    ~(receiver_type : mono_type)
    ~(method_name : string)
    ~(trait_name : string)
    (trait_def : Trait_registry.trait_def)
    (method_sig : Trait_registry.method_sig) : (Trait_registry.method_sig, Diagnostic.t) result =
  let receiver_type = canonicalize_mono_type receiver_type in
  let dyn_type = Types.to_string receiver_type in
  if method_sig.Trait_registry.method_generics <> [] then
    Error
      (mk_error ~code:"type-trait-object-object-unsafe"
         ~message:
           (Printf.sprintf
              "Method '%s' from trait '%s' is not callable through %s because Dyn[...] does not support method-generic dispatch"
              method_name trait_name dyn_type))
  else
    match trait_def.Trait_registry.trait_type_param with
    | None -> Ok method_sig
    | Some type_param ->
        let mentions_self ty = TypeVarSet.mem type_param (free_type_vars ty) in
        let additional_params =
          match method_sig.Trait_registry.method_params with
          | [] -> []
          | _receiver :: rest -> rest
        in
        let self_in_additional_params = List.exists (fun (_, ty) -> mentions_self ty) additional_params in
        let self_in_return = mentions_self method_sig.Trait_registry.method_return_type in
        if self_in_additional_params || self_in_return then
          Error
            (mk_error ~code:"type-trait-object-object-unsafe"
               ~message:
                 (Printf.sprintf
                    "Method '%s' from trait '%s' is not callable through %s because it mentions the hidden concrete type outside the receiver"
                    method_name trait_name dyn_type))
        else
          let subst = substitution_singleton type_param receiver_type in
          Ok
            {
              method_sig with
              method_params =
                List.map (fun (name, ty) -> (name, apply_substitution subst ty)) method_sig.method_params;
              method_return_type = apply_substitution subst method_sig.method_return_type;
            }

let instantiate_dynamic_trait_method
    ~(mk_error : code:string -> message:string -> Diagnostic.t) (receiver_type : mono_type) (method_name : string)
    : (string * Trait_registry.method_sig, Diagnostic.t) result =
  match canonicalize_mono_type receiver_type with
  | TTraitObject trait_names -> (
      let dyn_type = Types.to_string (TTraitObject trait_names) in
      let candidates = trait_object_method_candidates trait_names method_name in
      let ambiguous_message matches =
        let trait_names =
          matches |> List.map (fun (trait_name, _, _) -> trait_name) |> List.sort_uniq String.compare
        in
        Printf.sprintf "Ambiguous dynamic method '%s' for %s (provided by traits: %s)" method_name dyn_type
          (String.concat ", " trait_names)
      in
      let instantiate_for_trait (trait_name, trait_def, method_sig) =
        match
          instantiate_dynamic_trait_method_sig ~mk_error ~receiver_type ~method_name ~trait_name trait_def
            method_sig
        with
        | Error _ as err -> err
        | Ok instantiated_sig -> Ok (trait_name, instantiated_sig)
      in
      match candidates with
      | [] ->
          Error
            (mk_error ~code:"type-constructor"
               ~message:(Printf.sprintf "No method '%s' found for type %s" method_name dyn_type))
      | [ candidate ] -> instantiate_for_trait candidate
      | many -> Error (mk_error ~code:"type-constructor" ~message:(ambiguous_message many)))
  | _ ->
      Error
        (mk_error ~code:"type-internal"
           ~message:
             (Printf.sprintf "Expected Dyn receiver while resolving dynamic method '%s', got %s" method_name
                (Types.to_string receiver_type)))

let instantiate_dynamic_trait_method_for_trait
    ~(mk_error : code:string -> message:string -> Diagnostic.t)
    (receiver_type : mono_type)
    (trait_name : string)
    (method_name : string) : (Trait_registry.method_sig, Diagnostic.t) result =
  match canonicalize_mono_type receiver_type with
  | TTraitObject source_traits -> (
      let target_trait = Trait_registry.canonical_trait_name trait_name in
      let available_traits =
        source_traits
        |> List.concat_map Trait_registry.trait_with_supertraits
        |> List.map Trait_registry.canonical_trait_name
        |> List.sort_uniq String.compare
      in
      if not (List.mem target_trait available_traits) then
        Error
          (mk_error ~code:"type-trait-missing-impl"
             ~message:
               (Printf.sprintf "Type %s does not implement trait %s"
                  (Types.to_string (canonicalize_mono_type receiver_type))
                  target_trait))
      else
        match Trait_registry.lookup_trait_method_with_supertraits target_trait method_name with
        | None ->
            Error
              (mk_error ~code:"type-constructor"
                 ~message:(Printf.sprintf "Trait '%s' has no method '%s'" target_trait method_name))
        | Some (source_trait_def, method_sig) ->
            let source_trait_name =
              Trait_registry.canonical_trait_name source_trait_def.Trait_registry.trait_name
            in
            instantiate_dynamic_trait_method_sig ~mk_error ~receiver_type ~method_name
              ~trait_name:source_trait_name source_trait_def method_sig)
  | _ ->
      Error
        (mk_error ~code:"type-internal"
           ~message:
             (Printf.sprintf "Expected Dyn receiver while resolving qualified dynamic method '%s', got %s"
                method_name (Types.to_string receiver_type)))

let maybe_record_trait_object_coercion
    ~(mk_error : code:string -> message:string -> Diagnostic.t)
    (expr : AST.expression)
    (actual_type : mono_type)
    (expected_type : mono_type) : (unit, Diagnostic.t) result =
  match canonicalize_mono_type expected_type with
  | TTraitObject target_traits -> (
      match classify_trait_object_compatibility ~mk_error actual_type target_traits with
      | Error _ as err -> err
      | Ok TraitObjectAlreadyCompatible -> Ok ()
      | Ok (TraitObjectNeedsPackaging source_type) ->
          record_trait_object_coercion expr
            Resolution_artifacts.{ target_traits; source_type = canonicalize_mono_type source_type };
          Ok ())
  | _ -> Ok ()

let expression_type_or_lookup (type_map : type_map) (expr : AST.expression) : (mono_type, Diagnostic.t) result =
  match Hashtbl.find_opt type_map expr.id with
  | Some ty -> Ok ty
  | None -> (
      match Hashtbl.find_opt global_placeholder_rewrite_store expr.id with
      | Some rewritten_expr -> (
          match Hashtbl.find_opt type_map rewritten_expr.id with
          | Some ty -> Ok ty
          | None ->
              Error
                (Diagnostic.error_no_span ~code:"type-internal"
                   ~message:
                     (Printf.sprintf
                        "Missing inferred type for expression id %d (or placeholder rewrite id %d) at Dyn coercion site"
                        expr.id rewritten_expr.id)))
      | None ->
          Error
            (Diagnostic.error_no_span ~code:"type-internal"
               ~message:(Printf.sprintf "Missing inferred type for expression id %d at Dyn coercion site" expr.id))
      )

let rec record_expected_trait_object_coercions
    (type_map : type_map) (expr : AST.expression) (expected_type : mono_type) : (unit, Diagnostic.t) result =
  let record_here () =
    match canonicalize_mono_type expected_type with
    | TTraitObject _ ->
        let* actual_type = expression_type_or_lookup type_map expr in
        maybe_record_trait_object_coercion
          ~mk_error:(fun ~code ~message -> error_at ~code ~message expr)
          expr actual_type expected_type
    | _ -> Ok ()
  in
  let record_record_fields expected_fields fields =
    let rec loop = function
      | [] -> Ok ()
      | (field : AST.record_field) :: rest ->
          let* () =
            match field.field_value with
            | None -> Ok ()
            | Some field_expr -> (
                match
                  List.find_opt
                    (fun (expected_field : Types.record_field_type) -> expected_field.name = field.field_name)
                    expected_fields
                with
                | Some expected_field ->
                    record_expected_trait_object_coercions type_map field_expr expected_field.typ
                | None -> Ok ())
          in
          loop rest
    in
    loop fields
  in
  let record_enum_constructor_args enum_name variant_name args =
    match canonicalize_mono_type expected_type with
    | TEnum (expected_enum_name, type_args) when expected_enum_name = enum_name -> (
        match (Enum_registry.lookup enum_name, Enum_registry.lookup_variant enum_name variant_name) with
        | Some enum_def, Some variant ->
            let subst = substitution_of_list (List.combine enum_def.type_params type_args) in
            let expected_fields = List.map (apply_substitution subst) variant.fields in
            let rec loop remaining_args remaining_expected =
              match (remaining_args, remaining_expected) with
              | [], _ | _, [] -> Ok ()
              | arg :: rest_args, expected_field :: rest_expected ->
                  let* () = record_expected_trait_object_coercions type_map arg expected_field in
                  loop rest_args rest_expected
            in
            loop args expected_fields
        | _ -> Ok ())
    | _ -> Ok ()
  in
  match expr.expr with
  | AST.If (_cond, cons, alt_opt) -> (
      let* () = record_stmt_tail_expected_trait_object_coercions type_map cons expected_type in
      match alt_opt with
      | None -> Ok ()
      | Some alt_stmt -> record_stmt_tail_expected_trait_object_coercions type_map alt_stmt expected_type)
  | AST.Match (_scrutinee, arms) ->
      let rec loop = function
        | [] -> Ok ()
        | (arm : AST.match_arm) :: rest ->
            let* () = record_expected_trait_object_coercions type_map arm.body expected_type in
            loop rest
      in
      loop arms
  | AST.BlockExpr stmts -> (
      match List.rev stmts with
      | [] -> Ok ()
      | last :: _ -> record_stmt_tail_expected_trait_object_coercions type_map last expected_type)
  | _ -> (
      let* () = record_here () in
      match expr.expr with
      | AST.RecordLit (fields, _spread) -> (
          match Structural.fields_of_type (canonicalize_mono_type expected_type) with
          | Some expected_fields -> record_record_fields expected_fields fields
          | None -> Ok ())
      | AST.EnumConstructor (enum_name, variant_name, args) ->
          record_enum_constructor_args enum_name variant_name args
      | AST.MethodCall
          { mc_receiver = { expr = AST.Identifier enum_name; _ }; mc_method = variant_name; mc_args = args; _ } ->
          record_enum_constructor_args enum_name variant_name args
      | AST.Call ({ expr = AST.Identifier type_name; _ }, [ arg ]) when Type_registry.is_named_type_name type_name
        -> (
          match canonicalize_mono_type expected_type with
          | TNamed (expected_name, type_args) when expected_name = type_name -> (
              match Type_registry.instantiate_named_wrapper_representation type_name type_args with
              | Some (Ok representation_type) ->
                  record_expected_trait_object_coercions type_map arg representation_type
              | Some (Error msg) -> Error (error_at ~code:"type-constructor" ~message:msg expr)
              | None -> Ok ())
          | _ -> Ok ())
      | _ -> Ok ())

and record_stmt_tail_expected_trait_object_coercions
    (type_map : type_map) (stmt : AST.statement) (expected_type : mono_type) : (unit, Diagnostic.t) result =
  match stmt.stmt with
  | AST.ExportDecl _ | AST.ImportDecl _ -> Ok ()
  | AST.ExpressionStmt expr | AST.Return expr ->
      record_expected_trait_object_coercions type_map expr expected_type
  | AST.Block [] -> Ok ()
  | AST.Block stmts ->
      record_stmt_tail_expected_trait_object_coercions type_map (List.hd (List.rev stmts)) expected_type
  | AST.Let _ | AST.EnumDef _ | AST.TypeDef _ | AST.ShapeDef _ | AST.TraitDef _ | AST.ImplDef _
  | AST.InherentImplDef _ | AST.DeriveDef _ | AST.TypeAlias _ ->
      Ok ()

let provisional_function_type
    ?(outer_type_bindings = [])
    (generics_opt : AST.generic_param list option)
    (params : (string * AST.type_expr option) list)
    (return_annot : AST.type_expr option)
    (is_effectful : bool) : (mono_type, Diagnostic.t) result =
  let local_type_var_map =
    match generics_opt with
    | None -> []
    | Some generics ->
        List.map
          (fun (generic : AST.generic_param) ->
            let fresh_var = fresh_type_var () in
            (match fresh_var with
            | TVar n ->
                add_type_var_constraints n generic.constraints;
                record_type_var_user_name ~fresh_name:n ~user_name:generic.name
            | _ -> ());
            (generic.name, fresh_var))
          generics
  in
  let type_var_map = local_type_var_map @ outer_type_bindings in
  let convert_annot_or_fresh = annotation_with_bindings_or_fresh type_var_map in
  let return_type_result =
    match return_annot with
    | Some type_expr -> convert_annot_or_fresh type_expr
    | None -> Ok (fresh_type_var ())
  in
  match return_type_result with
  | Error _ as e -> e
  | Ok return_type -> (
      let param_types_result =
        List.fold_left
          (fun acc (_name, annot_opt) ->
            match acc with
            | Error _ -> acc
            | Ok rev_types -> (
                match annot_opt with
                | None -> Ok (fresh_type_var () :: rev_types)
                | Some type_expr -> (
                    match convert_annot_or_fresh type_expr with
                    | Error _ as e -> e
                    | Ok t -> Ok (t :: rev_types))))
          (Ok []) params
      in
      match param_types_result with
      | Error _ as e -> e
      | Ok rev_param_types ->
          let param_types = List.rev rev_param_types in
          Ok
            (List.fold_right (fun param_type acc -> TFun (param_type, acc, is_effectful)) param_types return_type)
      )

let provisional_placeholder_section_type ?(outer_type_bindings = []) () : (mono_type, Diagnostic.t) result =
  provisional_function_type ~outer_type_bindings None [ ("__section_param", None) ] None false

type symbol_scope = symbol_id NameMap.t
type symbol_scope_stack = symbol_scope list

let empty_scope_stack (root_scope : symbol_scope) : symbol_scope_stack = [ root_scope ]
let push_scope (stack : symbol_scope_stack) : symbol_scope_stack = NameMap.empty :: stack

let add_scope_binding (stack : symbol_scope_stack) (name : string) (sid : symbol_id) : symbol_scope_stack =
  match stack with
  | [] -> [ NameMap.singleton name sid ]
  | scope :: rest -> NameMap.add name sid scope :: rest

let rec lookup_scope_binding (stack : symbol_scope_stack) (name : string) : symbol_id option =
  match stack with
  | [] -> None
  | scope :: rest -> (
      match NameMap.find_opt name scope with
      | Some sid -> Some sid
      | None -> lookup_scope_binding rest name)

let rec placeholder_identifier_count_expr (expr : AST.expression) : int =
  match expr.expr with
  | AST.Identifier "_" -> 1
  | AST.Identifier _ | AST.Integer _ | AST.Float _ | AST.Boolean _ | AST.String _ -> 0
  | AST.Prefix (_, inner) | AST.TypeCheck (inner, _) | AST.FieldAccess (inner, _) | AST.TypeApply (inner, _) ->
      placeholder_identifier_count_expr inner
  | AST.Infix (left, _, right) -> placeholder_identifier_count_expr left + placeholder_identifier_count_expr right
  | AST.If (cond, cons, alt) -> (
      placeholder_identifier_count_expr cond
      + placeholder_identifier_count_stmt cons
      +
      match alt with
      | Some stmt -> placeholder_identifier_count_stmt stmt
      | None -> 0)
  | AST.Function _ -> 0
  | AST.Call (fn_expr, args) ->
      placeholder_identifier_count_expr fn_expr
      + List.fold_left (fun acc arg -> acc + placeholder_identifier_count_expr arg) 0 args
  | AST.Array elements -> List.fold_left (fun acc arg -> acc + placeholder_identifier_count_expr arg) 0 elements
  | AST.Hash pairs ->
      List.fold_left
        (fun acc (key, value) ->
          acc + placeholder_identifier_count_expr key + placeholder_identifier_count_expr value)
        0 pairs
  | AST.Index (container, index) ->
      placeholder_identifier_count_expr container + placeholder_identifier_count_expr index
  | AST.EnumConstructor (_, _, args) ->
      List.fold_left (fun acc arg -> acc + placeholder_identifier_count_expr arg) 0 args
  | AST.Match (scrutinee, arms) ->
      placeholder_identifier_count_expr scrutinee
      + List.fold_left (fun acc arm -> acc + placeholder_identifier_count_expr arm.AST.body) 0 arms
  | AST.RecordLit (fields, spread) -> (
      let fields_count =
        List.fold_left
          (fun acc (field : AST.record_field) ->
            acc
            +
            match field.field_value with
            | Some value -> placeholder_identifier_count_expr value
            | None -> 0)
          0 fields
      in
      fields_count
      +
      match spread with
      | Some spread_expr -> placeholder_identifier_count_expr spread_expr
      | None -> 0)
  | AST.MethodCall { mc_receiver; mc_args; _ } ->
      placeholder_identifier_count_expr mc_receiver
      + List.fold_left (fun acc arg -> acc + placeholder_identifier_count_expr arg) 0 mc_args
  | AST.BlockExpr stmts -> placeholder_identifier_count_stmts stmts

and placeholder_identifier_count_stmt (stmt : AST.statement) : int =
  match stmt.stmt with
  | AST.ExportDecl _ | AST.ImportDecl _ -> 0
  | AST.Let { value; _ } -> placeholder_identifier_count_expr value
  | AST.Return expr | AST.ExpressionStmt expr -> placeholder_identifier_count_expr expr
  | AST.Block stmts -> placeholder_identifier_count_stmts stmts
  | AST.EnumDef _ | AST.TypeDef _ | AST.ShapeDef _ | AST.TraitDef _ | AST.ImplDef _ | AST.InherentImplDef _
  | AST.DeriveDef _ | AST.TypeAlias _ ->
      0

and placeholder_identifier_count_stmts (stmts : AST.statement list) : int =
  List.fold_left (fun acc stmt -> acc + placeholder_identifier_count_stmt stmt) 0 stmts

let is_placeholder_identifier_expr (expr : AST.expression) : bool =
  match expr.expr with
  | AST.Identifier "_" -> true
  | _ -> false

let top_level_placeholder_section_candidate (expr : AST.expression) : bool =
  if placeholder_identifier_count_expr expr <> 1 then
    false
  else
    match expr.expr with
    | AST.Call (callee, args) ->
        is_placeholder_identifier_expr callee || List.exists is_placeholder_identifier_expr args
    | AST.MethodCall { mc_receiver; mc_args; _ } ->
        is_placeholder_identifier_expr mc_receiver || List.exists is_placeholder_identifier_expr mc_args
    | _ -> true

let top_level_forward_reference_candidate (expr : AST.expression) : bool =
  match expr.expr with
  | AST.Function _ -> true
  | _ -> top_level_placeholder_section_candidate expr

let current_top_level_placeholder_type (env : type_env) (name : string) : mono_type option =
  match lookup_top_level_placeholder name with
  | None -> None
  | Some placeholder -> (
      match TypeEnv.find_opt name env with
      | Some (Forall ([], mono)) -> Some mono
      | _ -> Some placeholder)

let register_prebound_symbols ?(prebound_symbols = []) (env : type_env) : symbol_scope =
  let prebound_symbols =
    List.fold_left (fun acc (name, info) -> NameMap.add name info acc) NameMap.empty prebound_symbols
  in
  TypeEnv.fold
    (fun name _poly scope ->
      let sid =
        match NameMap.find_opt name prebound_symbols with
        | Some info ->
            register_symbol ~name ~kind:info.kind ~pos:info.definition_pos ~end_pos:info.definition_end_pos
              ~file_id:info.file_id
        | None -> register_symbol ~name ~kind:BuiltinValue ~pos:(-1) ~end_pos:(-1) ~file_id:None
      in
      NameMap.add name sid scope)
    env NameMap.empty

let register_top_level_symbol_definitions (root_scope : symbol_scope) (program : AST.program) :
    (symbol_scope, Diagnostic.t) result =
  let rec go (scope : symbol_scope) (seen_lets : StringSet.t) (stmts : AST.statement list) :
      (symbol_scope, Diagnostic.t) result =
    match stmts with
    | [] -> Ok scope
    | stmt :: rest -> (
        match stmt.stmt with
        | AST.Let let_binding ->
            if let_binding.name = "_" then
              go scope seen_lets rest
            else if StringSet.mem let_binding.name seen_lets then
              Error
                (error_at_stmt ~code:"type-constructor"
                   ~message:(Printf.sprintf "Duplicate top-level let definition: %s" let_binding.name)
                   stmt)
            else
              let sid =
                register_symbol ~name:let_binding.name ~kind:TopLevelLet ~pos:stmt.pos ~end_pos:stmt.end_pos
                  ~file_id:stmt.file_id
              in
              let scope' =
                if top_level_forward_reference_candidate let_binding.value then
                  NameMap.add let_binding.name sid scope
                else
                  (* Top-level value bindings remain declaration-ordered; only function-like
                     declarations participate in forward reference resolution. *)
                  scope
              in
              go scope' (StringSet.add let_binding.name seen_lets) rest
        | AST.TypeAlias alias_def ->
            let _ =
              register_symbol ~name:alias_def.alias_name ~kind:TypeAliasSym ~pos:stmt.pos ~end_pos:stmt.end_pos
                ~file_id:stmt.file_id
            in
            go scope seen_lets rest
        | AST.TypeDef type_def ->
            let _ =
              register_symbol ~name:type_def.type_name ~kind:TypeSym ~pos:stmt.pos ~end_pos:stmt.end_pos
                ~file_id:stmt.file_id
            in
            go scope seen_lets rest
        | AST.ShapeDef shape_def ->
            let _ =
              register_symbol ~name:shape_def.shape_name ~kind:ShapeSym ~pos:stmt.pos ~end_pos:stmt.end_pos
                ~file_id:stmt.file_id
            in
            go scope seen_lets rest
        | AST.TraitDef trait_def ->
            let _ =
              register_symbol ~name:trait_def.name ~kind:TraitSym ~pos:stmt.pos ~end_pos:stmt.end_pos
                ~file_id:stmt.file_id
            in
            go scope seen_lets rest
        | AST.EnumDef enum_def ->
            let _ =
              register_symbol ~name:enum_def.name ~kind:EnumSym ~pos:stmt.pos ~end_pos:stmt.end_pos
                ~file_id:stmt.file_id
            in
            let _ =
              List.iter
                (fun (v : AST.variant_def) ->
                  let _ =
                    register_symbol
                      ~name:(enum_def.name ^ "." ^ v.variant_name)
                      ~kind:EnumVariantSym ~pos:stmt.pos ~end_pos:stmt.end_pos ~file_id:stmt.file_id
                  in
                  ())
                enum_def.variants
            in
            go scope seen_lets rest
        | _ -> go scope seen_lets rest)
  in
  go root_scope StringSet.empty program

let resolve_let_binding_symbol
    ~(is_top_level : bool) (stack : symbol_scope_stack) (stmt : AST.statement) (name : string) :
    symbol_scope_stack =
  if name = "_" then
    stack
  else if is_top_level then
    match lookup_scope_binding stack name with
    | Some _ -> stack
    | None ->
        let sid =
          register_symbol ~name ~kind:TopLevelLet ~pos:stmt.pos ~end_pos:stmt.end_pos ~file_id:stmt.file_id
        in
        add_scope_binding stack name sid
  else
    let sid = register_symbol ~name ~kind:LocalLet ~pos:stmt.pos ~end_pos:stmt.end_pos ~file_id:stmt.file_id in
    add_scope_binding stack name sid

let resolve_param_symbols (stack : symbol_scope_stack) (params : (string * 'a) list) (expr : AST.expression) :
    symbol_scope_stack =
  List.fold_left
    (fun acc (name, _annot) ->
      let sid = register_symbol ~name ~kind:Param ~pos:expr.pos ~end_pos:expr.end_pos ~file_id:expr.file_id in
      add_scope_binding acc name sid)
    stack params

let resolve_impl_method_params (stack : symbol_scope_stack) (m : AST.method_impl) : symbol_scope_stack =
  List.fold_left
    (fun acc (name, _annot) ->
      let sid =
        register_symbol ~name ~kind:ImplMethodParam ~pos:m.impl_method_body.pos
          ~end_pos:m.impl_method_body.end_pos ~file_id:m.impl_method_body.file_id
      in
      add_scope_binding acc name sid)
    stack m.impl_method_params

let rec resolve_pattern_bindings (stack : symbol_scope_stack) (pat : AST.pattern) : symbol_scope_stack =
  match pat.pat with
  | AST.PWildcard | AST.PLiteral _ -> stack
  | AST.PVariable name ->
      let sid = register_symbol ~name ~kind:PatternVar ~pos:pat.pos ~end_pos:pat.end_pos ~file_id:pat.file_id in
      add_scope_binding stack name sid
  | AST.PConstructor (_enum_name, _variant_name, fields) -> List.fold_left resolve_pattern_bindings stack fields
  | AST.PRecord (fields, rest) -> (
      let with_fields =
        List.fold_left
          (fun acc (field : AST.record_pattern_field) ->
            match field.pat_field_pattern with
            | Some nested -> resolve_pattern_bindings acc nested
            | None ->
                let sid =
                  register_symbol ~name:field.pat_field_name ~kind:PatternVar ~pos:pat.pos ~end_pos:pat.end_pos
                    ~file_id:pat.file_id
                in
                add_scope_binding acc field.pat_field_name sid)
          stack fields
      in
      match rest with
      | Some rest_name ->
          let sid =
            register_symbol ~name:rest_name ~kind:PatternVar ~pos:pat.pos ~end_pos:pat.end_pos
              ~file_id:pat.file_id
          in
          add_scope_binding with_fields rest_name sid
      | None -> with_fields)

let rec resolve_expr_symbols (stack : symbol_scope_stack) (expr : AST.expression) : unit =
  match expr.expr with
  | AST.Identifier name -> (
      match lookup_scope_binding stack name with
      | Some sid -> record_identifier_symbol expr.id sid
      | None -> ())
  | AST.Integer _ | AST.Float _ | AST.Boolean _ | AST.String _ -> ()
  | AST.Array xs -> List.iter (resolve_expr_symbols stack) xs
  | AST.Index (a, b) ->
      resolve_expr_symbols stack a;
      resolve_expr_symbols stack b
  | AST.TypeApply (callee, _type_args) -> resolve_expr_symbols stack callee
  | AST.Hash pairs ->
      List.iter
        (fun (k, v) ->
          resolve_expr_symbols stack k;
          resolve_expr_symbols stack v)
        pairs
  | AST.Prefix (_, e) -> resolve_expr_symbols stack e
  | AST.Infix (l, _, r) ->
      resolve_expr_symbols stack l;
      resolve_expr_symbols stack r
  | AST.TypeCheck (e, _t) -> resolve_expr_symbols stack e
  | AST.If (cond, cons, alt) -> (
      resolve_expr_symbols stack cond;
      ignore (resolve_stmt_symbols stack ~is_top_level:false cons);
      match alt with
      | Some b -> ignore (resolve_stmt_symbols stack ~is_top_level:false b)
      | None -> ())
  | AST.Function fn_expr ->
      let scoped = resolve_param_symbols (push_scope stack) fn_expr.params expr in
      ignore (resolve_stmt_symbols scoped ~is_top_level:false fn_expr.body)
  | AST.Call (f, args) ->
      resolve_expr_symbols stack f;
      List.iter (resolve_expr_symbols stack) args
  | AST.EnumConstructor (_enum_name, _variant_name, args) -> List.iter (resolve_expr_symbols stack) args
  | AST.Match (scrutinee, arms) ->
      resolve_expr_symbols stack scrutinee;
      List.iter (resolve_match_arm_symbols stack) arms
  | AST.RecordLit (fields, spread) -> (
      List.iter
        (fun (f : AST.record_field) ->
          match f.field_value with
          | Some v -> resolve_expr_symbols stack v
          | None -> ())
        fields;
      match spread with
      | Some s -> resolve_expr_symbols stack s
      | None -> ())
  | AST.FieldAccess (receiver, _field_name) -> resolve_expr_symbols stack receiver
  | AST.MethodCall { mc_receiver; mc_args; _ } ->
      resolve_expr_symbols stack mc_receiver;
      List.iter (resolve_expr_symbols stack) mc_args
  | AST.BlockExpr stmts ->
      ignore (List.fold_left (fun acc stmt -> resolve_stmt_symbols acc ~is_top_level:false stmt) stack stmts)

and resolve_match_arm_symbols (stack : symbol_scope_stack) (arm : AST.match_arm) : unit =
  let arm_scope = List.fold_left resolve_pattern_bindings (push_scope stack) arm.patterns in
  resolve_expr_symbols arm_scope arm.body

and resolve_impl_method_symbols (stack : symbol_scope_stack) (m : AST.method_impl) : unit =
  let method_scope = resolve_impl_method_params (push_scope stack) m in
  ignore (resolve_stmt_symbols method_scope ~is_top_level:false m.impl_method_body)

and resolve_stmt_symbols (stack : symbol_scope_stack) ~(is_top_level : bool) (stmt : AST.statement) :
    symbol_scope_stack =
  match stmt.stmt with
  | AST.ExportDecl _ | AST.ImportDecl _ -> stack
  | AST.ExpressionStmt expr ->
      resolve_expr_symbols stack expr;
      stack
  | AST.Return expr ->
      resolve_expr_symbols stack expr;
      stack
  | AST.Block stmts ->
      ignore (resolve_stmt_list_symbols (push_scope stack) ~is_top_level:false stmts);
      stack
  | AST.Let let_binding ->
      let scope_with_binding = resolve_let_binding_symbol ~is_top_level stack stmt let_binding.name in
      resolve_expr_symbols scope_with_binding let_binding.value;
      scope_with_binding
  | AST.EnumDef enum_def ->
      let sid =
        register_symbol ~name:enum_def.name ~kind:EnumSym ~pos:stmt.pos ~end_pos:stmt.end_pos
          ~file_id:stmt.file_id
      in
      let with_enum = add_scope_binding stack enum_def.name sid in
      List.iter
        (fun (v : AST.variant_def) ->
          ignore
            (register_symbol
               ~name:(enum_def.name ^ "." ^ v.variant_name)
               ~kind:EnumVariantSym ~pos:stmt.pos ~end_pos:stmt.end_pos ~file_id:stmt.file_id))
        enum_def.variants;
      with_enum
  | AST.TypeDef type_def ->
      ignore
        (register_symbol ~name:type_def.type_name ~kind:TypeSym ~pos:stmt.pos ~end_pos:stmt.end_pos
           ~file_id:stmt.file_id);
      stack
  | AST.ShapeDef shape_def ->
      ignore
        (register_symbol ~name:shape_def.shape_name ~kind:ShapeSym ~pos:stmt.pos ~end_pos:stmt.end_pos
           ~file_id:stmt.file_id);
      stack
  | AST.TraitDef trait_def ->
      ignore
        (register_symbol ~name:trait_def.name ~kind:TraitSym ~pos:stmt.pos ~end_pos:stmt.end_pos
           ~file_id:stmt.file_id);
      stack
  | AST.TypeAlias alias_def ->
      ignore
        (register_symbol ~name:alias_def.alias_name ~kind:TypeAliasSym ~pos:stmt.pos ~end_pos:stmt.end_pos
           ~file_id:stmt.file_id);
      stack
  | AST.ImplDef impl_def ->
      List.iter (resolve_impl_method_symbols stack) impl_def.impl_methods;
      stack
  | AST.InherentImplDef impl_def ->
      List.iter (resolve_impl_method_symbols stack) impl_def.inherent_methods;
      stack
  | AST.DeriveDef _ -> stack

and resolve_stmt_list_symbols (stack : symbol_scope_stack) ~(is_top_level : bool) (stmts : AST.statement list) :
    symbol_scope_stack =
  List.fold_left (fun acc stmt -> resolve_stmt_symbols acc ~is_top_level stmt) stack stmts

let resolve_program_symbols ?(prebound_symbols = []) (env : type_env) (program : AST.program) :
    (unit, Diagnostic.t) result =
  clear_symbol_stores ();
  let root_scope = register_prebound_symbols ~prebound_symbols env in
  match register_top_level_symbol_definitions root_scope program with
  | Error e -> Error e
  | Ok root_scope' ->
      ignore (resolve_stmt_list_symbols (empty_scope_stack root_scope') ~is_top_level:true program);
      Ok ()

(* ============================================================
   Main Inference Function
   ============================================================
   
   infer_expression takes:
   - env: the current type environment
   - expr: the expression to infer
   
   Returns:
   - Ok (substitution, mono_type): the substitution built up and inferred type
   - Error: if type error found
   
   The substitution accumulates all the constraints we've discovered.
   After inference, apply the final substitution to get concrete types.
*)

let rec infer_expression (type_map : type_map) (env : type_env) (expr : AST.expression) :
    (substitution * mono_type) infer_result =
  let standalone_placeholder_count =
    if placeholder_section_fallback_enabled () then
      placeholder_identifier_count_expr expr
    else
      0
  in
  let infer_without_section_fallback () =
    with_placeholder_section_fallback_disabled (fun () ->
        match expr.expr with
        (* Literals have known types *)
        | AST.Integer _ -> Ok (empty_substitution, TInt)
        | AST.Float _ -> Ok (empty_substitution, TFloat)
        | AST.Boolean _ -> Ok (empty_substitution, TBool)
        | AST.String _ -> Ok (empty_substitution, TString)
        (* Variable lookup - instantiate its poly_type *)
        | AST.Identifier name -> (
            match TypeEnv.find_opt name env with
            | None -> (
                match lookup_top_level_placeholder name with
                | Some placeholder -> Ok (empty_substitution, instantiate (mono_to_poly placeholder))
                | None -> Error (error_at ~code:"type-unbound-var" ~message:("Unbound variable: " ^ name) expr))
            | Some poly_type ->
                let instantiated = instantiate poly_type in
                Ok (empty_substitution, instantiated))
        (* Prefix operators *)
        | AST.Prefix (op, operand) -> infer_prefix type_map env op operand
        (* Infix operators *)
        | AST.Infix (left, op, right) -> infer_infix type_map env left op right
        (* Type checking operator: x is int *)
        | AST.TypeCheck (expr, type_ann) -> (
            (* Infer type of expression *)
            match infer_expression type_map env expr with
            | Error e -> Error e
            | Ok (subst1, _expr_type) -> (
                (* Convert type annotation to mono_type (for validation, not currently used) *)
                match Annotation.type_expr_to_mono_type type_ann with
                | Error e -> Error e
                | Ok _check_type ->
                    (* Result is always bool *)
                    Ok (subst1, TBool)))
        (* If expressions *)
        | AST.If (condition, consequence, alternative) -> infer_if type_map env condition consequence alternative
        (* Function literals *)
        | AST.Function { origin; generics; params; return_type; is_effectful; body } ->
            infer_function_literal type_map env expr ~origin ~generics ~params ~return_type ~is_effectful ~body
        (* Function calls *)
        | AST.Call (func, args) -> infer_call type_map env expr func args
        | AST.TypeApply (func, type_args) -> infer_type_apply type_map env expr func type_args
        (* Arrays *)
        | AST.Array elements -> infer_array type_map env elements
        (* Hashes *)
        | AST.Hash pairs -> infer_hash type_map env pairs
        (* Index expressions *)
        | AST.Index (container, index) -> infer_index type_map env container index
        (* Phase 4.2: Enums and pattern matching - to be implemented *)
        | AST.EnumConstructor (enum_name, variant_name, args) -> (
            (* Look up the variant in the registry *)
            match Enum_registry.lookup_variant enum_name variant_name with
            | None ->
                Error
                  (error_at ~code:"type-constructor"
                     ~message:(unknown_constructor_message enum_name variant_name)
                     expr)
            | Some variant -> (
                (* Infer types of constructor arguments *)
                match infer_args type_map env empty_substitution args with
                | Error e -> Error e
                | Ok (subst, arg_types) -> (
                    if
                      (* Check argument count matches *)
                      List.length args <> List.length variant.fields
                    then
                      Error
                        (error_at ~code:"type-constructor"
                           ~message:
                             (Printf.sprintf "%s.%s expects %d arguments, got %d" enum_name variant_name
                                (List.length variant.fields) (List.length args))
                           expr)
                    else
                      (* Get the enum definition to know type parameters *)
                      match Enum_registry.lookup enum_name with
                      | None ->
                          Error (error_at ~code:"type-constructor" ~message:(unknown_type_message enum_name) expr)
                      | Some enum_def -> (
                          (* Create fresh type variables for each type parameter *)
                          let fresh_vars = List.map (fun _ -> fresh_type_var ()) enum_def.type_params in
                          let param_subst = substitution_of_list (List.combine enum_def.type_params fresh_vars) in

                          (* Substitute type parameters in variant field types *)
                          let expected_types = List.map (apply_substitution param_subst) variant.fields in

                          (* Unify argument types with expected field types *)
                          let arg_types' = List.map (apply_substitution subst) arg_types in
                          let rec unify_all subst_acc types1 types2 =
                            match (types1, types2) with
                            | [], [] -> Ok subst_acc
                            | t1 :: rest1, t2 :: rest2 -> (
                                let t1' = apply_substitution subst_acc t1 in
                                let t2' = apply_substitution subst_acc t2 in
                                match unify t1' t2' with
                                | Error e -> Error (error_at ~code:e.code ~message:e.message expr)
                                | Ok subst2 ->
                                    let new_subst = compose_substitution subst_acc subst2 in
                                    unify_all new_subst rest1 rest2)
                            | _ ->
                                Error (error_at ~code:"type-constructor" ~message:"Argument count mismatch" expr)
                          in
                          match unify_all empty_substitution arg_types' expected_types with
                          | Error e -> Error e
                          | Ok subst2 ->
                              let final_subst = compose_substitution subst subst2 in

                              (* Build the result enum type with substituted type arguments *)
                              let result_type_args = List.map (apply_substitution final_subst) fresh_vars in
                              let result_type = TEnum (enum_name, result_type_args) in

                              Ok (final_subst, result_type)))))
        | AST.Match (scrutinee, arms) -> (
            (* Infer scrutinee type *)
            match infer_expression type_map env scrutinee with
            | Error e -> Error e
            | Ok (subst, scrutinee_type) -> (
                let env' = apply_substitution_env subst env in
                (* Check exhaustiveness *)
                match Exhaustiveness.check_exhaustive scrutinee_type arms with
                | Error msg -> Error (error_at ~code:"type-match" ~message:msg expr)
                | Ok () ->
                    (* Check each arm and collect body types *)
                    infer_match_arms type_map env' scrutinee scrutinee_type arms subst expr))
        | AST.RecordLit (fields, spread) -> infer_record_literal type_map env fields spread expr
        | AST.FieldAccess (receiver, variant_name) -> (
            let infer_enum_constructor_value (enum_source_name : string) :
                (substitution * mono_type) infer_result =
              let enum_name = canonical_enum_name_of_source_name enum_source_name in
              match Enum_registry.lookup_variant enum_name variant_name with
              | None ->
                  Error
                    (error_at ~code:"type-constructor"
                       ~message:(unknown_constructor_message enum_source_name variant_name)
                       expr)
              | Some variant -> (
                  match Enum_registry.lookup enum_name with
                  | None ->
                      Error
                        (error_at ~code:"type-constructor" ~message:(unknown_type_message enum_source_name) expr)
                  | Some enum_def ->
                      let fresh_vars = List.map (fun _ -> fresh_type_var ()) enum_def.type_params in
                      let param_subst = substitution_of_list (List.combine enum_def.type_params fresh_vars) in
                      let field_types = List.map (apply_substitution param_subst) variant.fields in
                      let result_type = TEnum (enum_name, fresh_vars) in
                      let callable_type =
                        List.fold_right
                          (fun field_type acc -> TFun (field_type, acc, false))
                          field_types result_type
                      in
                      Ok (empty_substitution, callable_type))
            in
            let infer_qualified_trait_value (trait_name : string) : (substitution * mono_type) infer_result =
              match Trait_registry.lookup_trait trait_name with
              | None ->
                  Error
                    (error_at ~code:"type-constructor"
                       ~message:(Printf.sprintf "Unknown trait '%s'" trait_name)
                       expr)
              | Some _trait_def -> (
                  match Trait_registry.lookup_trait_method_with_supertraits trait_name variant_name with
                  | None ->
                      Error
                        (error_at ~code:"type-constructor"
                           ~message:(Printf.sprintf "Trait '%s' has no method '%s'" trait_name variant_name)
                           expr)
                  | Some (source_trait_def, method_sig) ->
                      let trait_instantiated_sig =
                        match source_trait_def.Trait_registry.trait_type_param with
                        | None -> method_sig
                        | Some type_param ->
                            let fresh = fresh_type_var () in
                            attach_constraint_refs_if_tvar fresh (Constraints.of_names [ trait_name ]);
                            let subst = substitution_singleton type_param fresh in
                            {
                              method_sig with
                              method_params =
                                List.map
                                  (fun (name, ty) -> (name, apply_substitution subst ty))
                                  method_sig.method_params;
                              method_return_type = apply_substitution subst method_sig.method_return_type;
                            }
                      in
                      let instantiated_sig, resolved_method_type_args =
                        instantiate_method_generics_for_value trait_instantiated_sig
                      in
                      record_method_resolution expr (QualifiedTraitMethod trait_name);
                      record_method_type_args expr resolved_method_type_args;
                      Ok (empty_substitution, callable_type_of_method_sig instantiated_sig))
            in
            let infer_qualified_type_value
                ?(on_missing : (unit -> (substitution * mono_type) infer_result) option = None)
                (for_type : mono_type) : (substitution * mono_type) infer_result =
              match Inherent_registry.resolve_method for_type variant_name with
              | Error msg -> Error (error_at ~code:"type-constructor" ~message:msg expr)
              | Ok None -> (
                  match on_missing with
                  | Some missing -> missing ()
                  | None ->
                      Error
                        (error_at ~code:"type-constructor"
                           ~message:
                             (Printf.sprintf "No inherent method '%s' found for type %s" variant_name
                                (Types.to_string for_type))
                           expr))
              | Ok (Some method_sig) ->
                  let instantiated_sig, resolved_method_type_args =
                    instantiate_method_generics_for_value method_sig
                  in
                  record_method_resolution expr QualifiedInherentMethod;
                  record_method_type_args expr resolved_method_type_args;
                  Ok (empty_substitution, callable_type_of_method_sig instantiated_sig)
            in
            let infer_real_field_access () =
              (* Real field access on records *)
              match infer_expression type_map env receiver with
              | Error e -> Error e
              | Ok (subst1, receiver_type) ->
                  let receiver_type' = canonicalize_mono_type (apply_substitution subst1 receiver_type) in
                  (* Exact record access *)
                  let lookup_field fields =
                    List.find_opt (fun (f : Types.record_field_type) -> f.name = variant_name) fields
                  in
                  let resolve_consistent_candidate_type
                      ~(merge_closed_records : bool)
                      (candidates : Types.mono_type list)
                      ~(on_empty : unit -> ('a, string) result)
                      ~(on_conflict : Types.mono_type -> ('a, string) result)
                      ~(wrap_success : Types.mono_type -> ('a, string) result) : ('a, string) result =
                    let all_closed_records =
                      merge_closed_records
                      && List.for_all
                           (function
                             | Types.TRecord (_, None) -> true
                             | _ -> false)
                           candidates
                    in
                    match candidates with
                    | [] -> on_empty ()
                    | first_type :: rest ->
                        let rec unify_candidates current_type subst_acc = function
                          | [] ->
                              apply_substitution subst_acc current_type |> canonicalize_mono_type |> wrap_success
                          | candidate_type :: tail -> (
                              let lhs = apply_substitution subst_acc current_type in
                              let rhs = apply_substitution subst_acc candidate_type in
                              match unify lhs rhs with
                              | Ok subst' ->
                                  let composed = compose_substitution subst_acc subst' in
                                  let next = apply_substitution composed current_type in
                                  unify_candidates next composed tail
                              | Error _ when Annotation.is_subtype_of lhs rhs ->
                                  unify_candidates lhs subst_acc tail
                              | Error _ when Annotation.is_subtype_of rhs lhs ->
                                  unify_candidates rhs subst_acc tail
                              | Error _ when all_closed_records ->
                                  Annotation.merged_record_intersection_type candidates
                                  |> Result.map Types.canonicalize_mono_type
                                  |> Result.map_error (fun (diag : Diagnostic.t) -> diag.message)
                                  |> fun result -> Result.bind result wrap_success
                              | Error _ -> on_conflict (canonicalize_mono_type lhs))
                        in
                        unify_candidates first_type empty_substitution rest
                  in
                  let resolve_consistent_field_type
                      ~(diagnostic_code : string) ~(conflict_message : string) (candidates : Types.mono_type list)
                      : (Types.mono_type, Diagnostic.t) result =
                    resolve_consistent_candidate_type ~merge_closed_records:true candidates
                      ~on_empty:(fun () ->
                        Error
                          (Printf.sprintf "Field '%s' is not guaranteed by every member of intersection %s"
                             variant_name (Types.to_string receiver_type')))
                      ~on_conflict:(fun _ -> Error conflict_message)
                      ~wrap_success:(fun field_type -> Ok field_type)
                    |> Result.map_error (fun message -> error_at ~code:diagnostic_code ~message expr)
                  in
                  let field_result =
                    match receiver_type' with
                    | TTraitObject _ ->
                        Error
                          (error_at ~code:"type-trait-object-field-access"
                             ~message:
                               (Printf.sprintf
                                  "Field access is not supported on %s; Dyn[...] only exposes trait methods"
                                  (Types.to_string receiver_type'))
                             expr)
                    | TRecord (fields, row) -> (
                        match lookup_field fields with
                        | Some field -> Ok (subst1, field.typ)
                        | None -> (
                            match row with
                            | Some row_var when is_trait_object_row row_var ->
                                Error
                                  (error_at ~code:"type-constructor"
                                     ~message:
                                       (Printf.sprintf "Record field '%s' not found in type %s" variant_name
                                          (Types.to_string receiver_type'))
                                     expr)
                            | Some _ -> (
                                (* Open row: the field may live in the tail; constrain it.
                                 Use a fresh tail variable to avoid recursive same-row constraints. *)
                                let field_type = fresh_type_var () in
                                let expected =
                                  TRecord ([ { name = variant_name; typ = field_type } ], Some (fresh_row_var ()))
                                in
                                match unify receiver_type' expected with
                                | Error e -> Error (error_at ~code:e.code ~message:e.message expr)
                                | Ok subst2 ->
                                    let final_subst = compose_substitution subst1 subst2 in
                                    Ok (final_subst, apply_substitution subst2 field_type))
                            | None ->
                                Error
                                  (error_at ~code:"type-constructor"
                                     ~message:
                                       (Printf.sprintf "Record field '%s' not found in type %s" variant_name
                                          (Types.to_string receiver_type'))
                                     expr)))
                    | TNamed (type_name, type_args) -> (
                        match Type_registry.instantiate_named_product_fields type_name type_args with
                        | Some (Ok fields) -> (
                            match lookup_field fields with
                            | Some field -> Ok (subst1, field.typ)
                            | None ->
                                Error
                                  (error_at ~code:"type-constructor"
                                     ~message:
                                       (Printf.sprintf "Field '%s' not found in named type %s" variant_name
                                          (Types.to_string receiver_type'))
                                     expr))
                        | Some (Error _) ->
                            Error
                              (error_at ~code:"type-constructor"
                                 ~message:
                                   (Printf.sprintf "Field '%s' is not available on wrapper type %s" variant_name
                                      (Types.to_string receiver_type'))
                                 expr)
                        | None ->
                            Error
                              (error_at ~code:"type-constructor"
                                 ~message:
                                   (Printf.sprintf "Unknown named type %s" (Types.to_string receiver_type'))
                                 expr))
                    | TIntersection members -> (
                        let member_field_types =
                          List.fold_left
                            (fun acc member ->
                              match (acc, member) with
                              | (Error _ as err), _ -> err
                              | Ok collected, TRecord (fields, _row) -> (
                                  match lookup_field fields with
                                  | Some field -> Ok (field.typ :: collected)
                                  | None -> Ok [])
                              | Ok _, _ -> Ok [])
                            (Ok []) members
                        in
                        match member_field_types with
                        | Error diag -> Error diag
                        | Ok field_types ->
                            let field_types = List.rev field_types in
                            if List.length field_types <> List.length members then
                              Error
                                (error_at ~code:"type-intersection-field-access"
                                   ~message:
                                     (Printf.sprintf
                                        "Field '%s' is not guaranteed by every member of intersection %s"
                                        variant_name (Types.to_string receiver_type'))
                                   expr)
                            else
                              resolve_consistent_field_type ~diagnostic_code:"type-intersection-field-access"
                                ~conflict_message:
                                  (Printf.sprintf
                                     "Field '%s' has conflicting types across members of intersection %s"
                                     variant_name (Types.to_string receiver_type'))
                                field_types
                              |> Result.map (fun field_type -> (subst1, field_type)))
                    | TVar _ -> (
                        match receiver_type' with
                        | TVar type_var_name -> (
                            let constraints = lookup_type_var_constraints type_var_name in
                            if constraints = [] then
                              (* Unconstrained TVar: row-polymorphic field access. *)
                              let field_type = fresh_type_var () in
                              let row_var = fresh_row_var () in
                              let expected =
                                TRecord ([ { name = variant_name; typ = field_type } ], Some row_var)
                              in
                              match unify receiver_type' expected with
                              | Error e -> Error (error_at ~code:e.code ~message:e.message expr)
                              | Ok subst2 ->
                                  let final_subst = compose_substitution subst1 subst2 in
                                  Ok (final_subst, apply_substitution subst2 field_type)
                            else
                              (* Constrained TVar: only fields guaranteed by constraints are accessible.
                               Do not unify away the type variable here, otherwise we can lose attached
                               trait obligations before call-site verification. *)
                              let expanded_constraints =
                                Trait_solver.expand_constraint_refs constraints |> Constraints.render_list
                              in
                              let constrained_field_types =
                                lookup_type_var_constrained_fields type_var_name
                                |> List.filter_map (fun (constraint_ref, (field : Types.record_field_type)) ->
                                       if field.name = variant_name then
                                         Some (constraint_ref, field.typ)
                                       else
                                         None)
                              in
                              let resolve_constrained_field_type candidates =
                                match candidates with
                                | [] ->
                                    Error
                                      (Printf.sprintf
                                         "Field '%s' is not guaranteed by constraints on type variable '%s' (%s)"
                                         variant_name type_var_name expanded_constraints)
                                | (_constraint_ref, first_type) :: rest ->
                                    let rec unify_candidates current_type subst_acc = function
                                      | [] -> Ok (apply_substitution subst_acc current_type)
                                      | (candidate_constraint, candidate_type) :: tail -> (
                                          let lhs = apply_substitution subst_acc current_type in
                                          let rhs = apply_substitution subst_acc candidate_type in
                                          match unify lhs rhs with
                                          | Ok subst' ->
                                              let composed = compose_substitution subst_acc subst' in
                                              let next = apply_substitution composed current_type in
                                              unify_candidates next composed tail
                                          | Error _ when Annotation.is_subtype_of lhs rhs ->
                                              unify_candidates lhs subst_acc tail
                                          | Error _ when Annotation.is_subtype_of rhs lhs ->
                                              unify_candidates rhs subst_acc tail
                                          | Error _ ->
                                              Error
                                                (Printf.sprintf
                                                   "Conflicting field type requirements for field '%s' on constrained type variable '%s' (constraint '%s')"
                                                   variant_name type_var_name
                                                   (Constraints.name candidate_constraint)))
                                    in
                                    unify_candidates first_type empty_substitution rest
                              in
                              match resolve_constrained_field_type constrained_field_types with
                              | Error msg -> Error (error_at ~code:"type-constructor" ~message:msg expr)
                              | Ok field_type -> Ok (subst1, field_type))
                        | _ ->
                            Error
                              (error_at ~code:"type-constructor" ~message:"internal error: expected type variable"
                                 expr))
                    | _ ->
                        Error
                          (error_at ~code:"type-constructor"
                             ~message:
                               (Printf.sprintf "Field access requires record type, got %s"
                                  (Types.to_string receiver_type'))
                             expr)
                  in
                  field_result
            in
            match receiver.expr with
            | AST.Identifier name -> (
                match classify_dotted_receiver env name variant_name with
                | `BoundVar | `Unknown -> infer_real_field_access ()
                | `EnumVariant -> infer_enum_constructor_value name
                | `EnumType -> (
                    match resolve_dotted_type_name name with
	                    | Some for_type ->
	                        infer_qualified_type_value for_type
	                          ~on_missing:
	                            (Some
	                               (fun () ->
	                                 Error
	                                   (error_at ~code:"type-constructor"
	                                      ~message:(missing_constructor_or_inherent_method_message name variant_name)
	                                      expr)))
                    | None -> infer_enum_constructor_value name)
                | `TypeName for_type -> infer_qualified_type_value for_type
                | `TraitName -> infer_qualified_trait_value name)
            | _ -> infer_real_field_access ())
        | AST.MethodCall { mc_receiver = receiver; mc_method = method_name; mc_type_args; mc_args = args } -> (
            let infer_enum_constructor (enum_source_name : string) : (substitution * mono_type) infer_result =
              let enum_name = canonical_enum_name_of_source_name enum_source_name in
              match Enum_registry.lookup_variant enum_name method_name with
              | None ->
                  Error
                    (error_at ~code:"type-constructor"
                       ~message:(unknown_constructor_message enum_source_name method_name)
                       expr)
              | Some variant -> (
                  match infer_args type_map env empty_substitution args with
                  | Error e -> Error e
                  | Ok (subst, arg_types) -> (
                      if List.length args <> List.length variant.fields then
                        Error
                          (error_at ~code:"type-constructor"
                             ~message:
                               (Printf.sprintf "%s.%s expects %d arguments, got %d" enum_source_name method_name
                                  (List.length variant.fields) (List.length args))
                             expr)
                      else
                        match Enum_registry.lookup enum_name with
                        | None ->
                            Error
                              (error_at ~code:"type-constructor" ~message:(unknown_type_message enum_source_name) expr)
                        | Some enum_def -> (
                            let fresh_vars = List.map (fun _ -> fresh_type_var ()) enum_def.type_params in
                            let param_subst =
                              substitution_of_list (List.combine enum_def.type_params fresh_vars)
                            in
                            let expected_types = List.map (apply_substitution param_subst) variant.fields in
                            let arg_types' = List.map (apply_substitution subst) arg_types in
                            let rec unify_all subst_acc types1 types2 =
                              match (types1, types2) with
                              | [], [] -> Ok subst_acc
                              | t1 :: rest1, t2 :: rest2 -> (
                                  let t1' = apply_substitution subst_acc t1 in
                                  let t2' = apply_substitution subst_acc t2 in
                                  match unify t1' t2' with
                                  | Ok new_subst ->
                                      unify_all (compose_substitution new_subst subst_acc) rest1 rest2
                                  | Error e -> Error (error_at ~code:e.code ~message:e.message expr))
                              | _ ->
                                  Error
                                    (error_at ~code:"type-constructor" ~message:"Argument count mismatch" expr)
                            in
                            match unify_all subst arg_types' expected_types with
                            | Error e -> Error e
                            | Ok final_subst ->
                                let result_type_args = List.map (apply_substitution final_subst) fresh_vars in
                                let result_type = TEnum (enum_name, result_type_args) in
                                Ok (final_subst, result_type))))
            in
            let infer_real_method_call () : (substitution * mono_type) infer_result =
              match infer_expression type_map env receiver with
              | Error e -> Error e
              | Ok (subst1, receiver_type) -> (
                  let receiver_type' = apply_substitution subst1 receiver_type in
                  let receiver_type_canon = canonicalize_mono_type receiver_type' in
                  let commit_method_call_attempt (attempt : method_call_attempt) :
                      (substitution * mono_type) infer_result =
                    record_method_resolution expr attempt.mca_resolution;
                    record_effectful_method_call expr attempt.mca_effectful;
                    (match attempt.mca_method_type_args with
                    | Some resolved_type_args -> record_method_type_args expr resolved_type_args
                    | None -> ());
                    Ok (attempt.mca_subst, attempt.mca_return_type)
                  in
                  let builtin_trait_display_name (trait_name : string) : string =
                    match Trait_registry.canonical_trait_name trait_name with
                    | "eq" -> "Eq"
                    | "show" -> "Show"
                    | "debug" -> "Debug"
                    | "ord" -> "Ord"
                    | "hash" -> "Hash"
                    | "num" -> "Num"
                    | "rem" -> "Rem"
                    | "neg" -> "Neg"
                    | other -> other
                  in
                  let named_product_type_name_of (typ : mono_type) : string option =
                    match canonicalize_mono_type typ with
                    | TRecord _ as record_type ->
                        Type_registry.all_named_types ()
                        |> List.find_map (fun (def : Type_registry.named_type_def) ->
                               match def.named_type_body with
                               | Type_registry.NamedWrapper _ -> None
                               | Type_registry.NamedProduct _ -> (
                                   let fresh_args = List.map (fun _ -> fresh_type_var ()) def.named_type_params in
                                   match
                                     Type_registry.instantiate_named_product_fields def.named_type_name fresh_args
                                   with
                                   | Some (Ok fields) -> (
                                       match
                                         unify (canonicalize_mono_type (TRecord (fields, None))) record_type
                                       with
                                       | Ok _ -> Some def.named_type_name
                                       | Error _ -> None)
                                   | Some (Error _) | None -> None))
                    | _ -> None
                  in
                  let explicit_type_name_of (typ : mono_type) : string option =
                    match canonicalize_mono_type typ with
                    | TInt | TFloat | TBool | TString | TNull | TNamed _ | TEnum _ ->
                        Some (Types.to_string (canonicalize_mono_type typ))
                    | TRecord _ -> named_product_type_name_of typ
                    | TArray _ | THash _ | TTraitObject _ | TVar _ | TRowVar _ | TFun _ | TUnion _
                    | TIntersection _ ->
                        None
                  in
                  let explicit_type_name_opt = explicit_type_name_of receiver_type_canon in
                  let trait_qualification_suggestions () : string list =
                    let format_trait_call trait_name =
                      Printf.sprintf "%s.%s(...)" (builtin_trait_display_name trait_name) method_name
                    in
                    match receiver_type_canon with
                    | TTraitObject trait_names ->
                        Types.normalize_trait_object_traits trait_names
                        |> List.filter_map (fun trait_name ->
                               match
                                 Trait_registry.lookup_trait_method_with_supertraits trait_name method_name
                               with
                               | None -> None
                               | Some (source_trait_def, _method_sig) ->
                                   Some
                                     (Trait_registry.canonical_trait_name
                                        source_trait_def.Trait_registry.trait_name))
                        |> List.sort_uniq String.compare
                        |> List.map format_trait_call
                    | TVar type_var_name ->
                        let seen_source_traits : (string, unit) Hashtbl.t = Hashtbl.create 8 in
                        lookup_type_var_constraints type_var_name
                        |> Trait_solver.available_methods_refs
                        |> List.filter_map (fun (trait_name, _trait_def) ->
                               match
                                 Trait_registry.lookup_trait_method_with_supertraits trait_name method_name
                               with
                               | None -> None
                               | Some (source_trait_def, _method_sig) ->
                                   let source_trait_name =
                                     Trait_registry.canonical_trait_name
                                       source_trait_def.Trait_registry.trait_name
                                   in
                                   if Hashtbl.mem seen_source_traits source_trait_name then
                                     None
                                   else (
                                     Hashtbl.replace seen_source_traits source_trait_name ();
                                     Some source_trait_name))
                        |> List.sort_uniq String.compare
                        |> List.map format_trait_call
                    | _ ->
                        Trait_registry.all_impls ()
                        |> List.map (fun (impl : Trait_registry.impl_def) -> impl.impl_trait_name)
                        |> List.sort_uniq String.compare
                        |> List.filter_map (fun trait_name ->
                               match Trait_registry.resolve_impl trait_name receiver_type' with
                               | Error _ | Ok None -> None
                               | Ok (Some _) -> (
                                   match
                                     Trait_registry.lookup_trait_method_with_supertraits trait_name method_name
                                   with
                                   | None -> None
                                   | Some _ -> Some trait_name))
                        |> List.map format_trait_call
                  in
                  let inherent_qualification_suggestions () : string list =
                    Inherent_registry.all_methods ()
                    |> List.filter_map (fun (for_type, (method_sig : Inherent_registry.method_sig)) ->
                           if method_sig.method_name <> method_name then
                             None
                           else
                             match explicit_type_name_of for_type with
                             | None -> None
                             | Some type_name -> (
                                 match Unify.unify (canonicalize_mono_type for_type) receiver_type_canon with
                                 | Ok _ -> Some (Printf.sprintf "%s.%s(...)" type_name method_name)
                                 | Error _ -> None))
                    |> List.sort_uniq String.compare
                  in
                  let inherent_qualification_suggestion () : string option =
                    match explicit_type_name_opt with
                    | None -> None
                    | Some type_name -> (
                        match Inherent_registry.resolve_method receiver_type' method_name with
                        | Ok (Some _) -> Some (Printf.sprintf "%s.%s(...)" type_name method_name)
                        | Ok None | Error _ -> None)
                  in
                  let no_direct_surface_error () : Diagnostic.t =
                    let suggestions =
                      let inherent_suggestions =
                        let direct_receiver_suggestion =
                          match inherent_qualification_suggestion () with
                          | Some suggestion -> [ suggestion ]
                          | None -> []
                        in
                        direct_receiver_suggestion @ inherent_qualification_suggestions ()
                      in
                      inherent_suggestions @ trait_qualification_suggestions () |> List.sort_uniq String.compare
                    in
                    let receiver_type_name = Types.to_string receiver_type_canon in
                    let message =
                      match suggestions with
                      | [] ->
                          Printf.sprintf
                            "Dot call syntax only works with callable fields. '%s' is not a callable field on type %s."
                            method_name receiver_type_name
                      | _ ->
                          Printf.sprintf
                            "Dot call syntax only works with callable fields. '%s' is not a callable field on type %s. Use an explicit call target: %s"
                            method_name receiver_type_name (String.concat ", " suggestions)
                    in
                    error_at ~code:"type-constructor" ~message expr
                  in
                  let field_expr =
                    AST.mk_expr ~id:(fresh_synthetic_expr_id ()) ~pos:expr.pos ~end_pos:expr.end_pos
                      ~file_id:expr.file_id
                      (AST.FieldAccess (receiver, method_name))
                  in
                  let infer_callable_field_attempt () : method_call_attempt infer_result option =
                    match infer_expression type_map env field_expr with
                    | Error _ -> None
                    | Ok (subst_field, field_callee_type) -> (
                        let field_type_may_be_callable =
                          match
                            ( canonicalize_mono_type field_callee_type,
                              contextual_callable_signature field_callee_type )
                          with
                          | TVar _, _ -> true
                          | _, Some _ -> true
                          | _ -> false
                        in
                        if not field_type_may_be_callable then
                          None
                        else
                          let env_field = apply_substitution_env subst_field env in
                          let args_expr =
                            AST.mk_expr ~id:(fresh_synthetic_expr_id ()) ~pos:expr.pos ~end_pos:expr.end_pos
                              ~file_id:expr.file_id
                              (AST.Call (field_expr, args))
                          in
                          match infer_expression type_map env_field args_expr with
                          | Error e -> Some (Error e)
                          | Ok (subst_field_call, field_call_type) ->
                              let field_callee_type' = apply_substitution subst_field_call field_callee_type in
                              Some
                                (Ok
                                   {
                                     mca_subst = compose_substitution subst_field_call subst_field;
                                     mca_return_type = field_call_type;
                                     mca_resolution = FieldFunctionCall;
                                     mca_effectful = type_may_be_effectful_callable field_callee_type';
                                     mca_method_type_args = None;
                                   }))
                  in
                  match infer_callable_field_attempt () with
                  | Some (Ok field_attempt) -> commit_method_call_attempt field_attempt
                  | Some (Error field_error) -> Error field_error
                  | None -> Error (no_direct_surface_error ()))
            in
            (* Phase 4.4: Qualified call resolution *)
            let instantiate_method_generics_for_call
                ~(mk_error : code:string -> message:string -> Diagnostic.t)
                (method_sig : Trait_registry.method_sig) :
                (Trait_registry.method_sig * substitution, Diagnostic.t) result =
              let method_generics = method_sig.method_generics in
              if method_generics = [] then
                match mc_type_args with
                | Some type_args when type_args <> [] ->
                    Error
                      (mk_error ~code:"type-constructor"
                         ~message:
                           (Printf.sprintf "Method '%s' takes no type parameters, but %d were provided"
                              method_name (List.length type_args)))
                | _ -> Ok (method_sig, SubstMap.empty)
              else
                match mc_type_args with
                | Some type_args ->
                    if List.length type_args <> List.length method_generics then
                      Error
                        (mk_error ~code:"type-constructor"
                           ~message:
                             (Printf.sprintf "Method '%s' expects %d type argument(s), but %d were provided"
                                method_name (List.length method_generics) (List.length type_args)))
                    else
                      let rec convert_all acc = function
                        | [] -> Ok (List.rev acc)
                        | te :: rest -> (
                            match Annotation.type_expr_to_mono_type te with
                            | Error diag -> Error (mk_error ~code:diag.code ~message:diag.message)
                            | Ok mono -> convert_all (mono :: acc) rest)
                      in
                      let open Result in
                      let ( let* ) = bind in
                      let* mono_args = convert_all [] type_args in
                      let method_subst =
                        List.fold_left2
                          (fun acc (param_name, _) mono_arg -> SubstMap.add param_name mono_arg acc)
                          SubstMap.empty method_generics mono_args
                      in
                      Ok
                        ( {
                            method_sig with
                            method_params =
                              List.map
                                (fun (name, ty) -> (name, apply_substitution method_subst ty))
                                method_sig.method_params;
                            method_return_type = apply_substitution method_subst method_sig.method_return_type;
                          },
                          method_subst )
                | None ->
                    let method_subst =
                      List.fold_left
                        (fun acc (param_name, constraints) ->
                          let fresh = fresh_type_var () in
                          attach_constraint_refs_if_tvar fresh constraints;
                          SubstMap.add param_name fresh acc)
                        SubstMap.empty method_generics
                    in
                    Ok
                      ( {
                          method_sig with
                          method_params =
                            List.map
                              (fun (name, ty) -> (name, apply_substitution method_subst ty))
                              method_sig.method_params;
                          method_return_type = apply_substitution method_subst method_sig.method_return_type;
                        },
                        method_subst )
            in
            let infer_qualified_method_args
                ~(mk_error : code:string -> message:string -> Diagnostic.t)
                ~(call_target : string)
                (param_types : mono_type list) : (substitution * mono_type list) infer_result =
              if List.length args <> List.length param_types then
                Error
                  (mk_error ~code:"type-constructor"
                     ~message:
                       (Printf.sprintf "Qualified call '%s' expects %d argument(s), got %d" call_target
                          (List.length param_types) (List.length args)))
              else
                infer_args_against_expected type_map env empty_substitution args param_types
            in
            let rec unify_qualified_method_params
                ~(mk_error : code:string -> message:string -> Diagnostic.t)
                (subst_acc : substitution)
                (actual_types : mono_type list)
                (expected_types : mono_type list) : (substitution, Diagnostic.t) result =
              match (actual_types, expected_types) with
              | [], [] -> Ok subst_acc
              | actual :: rest_a, expected :: rest_e -> (
                  let actual' = apply_substitution subst_acc actual in
                  let expected' = apply_substitution subst_acc expected in
                  match unify actual' expected' with
                  | Error e -> Error (mk_error ~code:e.code ~message:e.message)
                  | Ok new_subst ->
                      unify_qualified_method_params ~mk_error
                        (compose_substitution new_subst subst_acc)
                        rest_a rest_e)
              | _ -> Error (mk_error ~code:"type-constructor" ~message:"Argument count mismatch")
            in
            let check_method_generic_constraints
                ~(mk_error : code:string -> message:string -> Diagnostic.t)
                ~(method_generics : (string * Constraints.t list) list)
                ~(method_subst : substitution)
                ~(final_subst : substitution) : (unit, Diagnostic.t) result =
              let check_constraint param_name resolved_type constraint_ref =
                match resolved_type with
                | TVar _ -> Ok ()
                | _ -> (
                    match Trait_solver.check_constraint_ref resolved_type constraint_ref with
                    | Ok () -> Ok ()
                    | Error diag ->
                        Error
                          (mk_error ~code:diag.code
                             ~message:
                               (Printf.sprintf
                                  "Method '%s' type parameter '%s' requires constraint '%s', but %s does not satisfy it"
                                  method_name param_name (Constraints.name constraint_ref)
                                  (Types.to_string resolved_type))))
              in
              List.fold_left
                (fun acc (param_name, constraints) ->
                  match acc with
                  | Error _ -> acc
                  | Ok () ->
                      let resolved_type =
                        apply_substitution method_subst (TVar param_name)
                        |> apply_substitution final_subst
                        |> canonicalize_mono_type
                      in
                      List.fold_left
                        (fun inner_acc constraint_ref ->
                          match inner_acc with
                          | Error _ -> inner_acc
                          | Ok () -> check_constraint param_name resolved_type constraint_ref)
                        (Ok ()) constraints)
                (Ok ()) method_generics
            in
            let resolved_method_type_args
                (method_generics : (string * Constraints.t list) list)
                (method_subst : substitution)
                (final_subst : substitution) : mono_type list =
              List.map
                (fun (param_name, _) ->
                  apply_substitution method_subst (TVar param_name)
                  |> apply_substitution final_subst
                  |> canonicalize_mono_type)
                method_generics
            in
            let infer_static_qualified_method_call
                ~(mk_error : code:string -> message:string -> Diagnostic.t)
                ~(call_target : string)
                ~(receiver_check : (mono_type -> (unit, Diagnostic.t) result) option)
                (method_sig : Trait_registry.method_sig) :
                (substitution * mono_type * mono_type list) infer_result =
              match instantiate_method_generics_for_call ~mk_error method_sig with
              | Error e -> Error e
              | Ok (instantiated_sig, method_subst) -> (
                  let param_types = List.map snd instantiated_sig.method_params in
                  match infer_qualified_method_args ~mk_error ~call_target param_types with
                  | Error e -> Error e
                  | Ok (subst1, arg_types) -> (
                      match unify_qualified_method_params ~mk_error subst1 arg_types param_types with
                      | Error e -> Error e
                      | Ok final_subst -> (
                          match
                            check_method_generic_constraints ~mk_error
                              ~method_generics:instantiated_sig.method_generics ~method_subst ~final_subst
                          with
                          | Error e -> Error e
                          | Ok () -> (
                              let receiver_check_result =
                                match (receiver_check, param_types) with
                                | None, _ | Some _, [] -> Ok ()
                                | Some check_receiver, first_param :: _ ->
                                    let receiver_type =
                                      apply_substitution final_subst first_param |> canonicalize_mono_type
                                    in
                                    check_receiver receiver_type
                              in
                              match receiver_check_result with
                              | Error e -> Error e
                              | Ok () ->
                                  let return_type =
                                    apply_substitution final_subst instantiated_sig.method_return_type
                                  in
                                  Ok
                                    ( final_subst,
                                      return_type,
                                      resolved_method_type_args instantiated_sig.method_generics method_subst
                                        final_subst )))))
            in
            let infer_qualified_trait_call (trait_name : string) : (substitution * mono_type) infer_result =
              let mk_error ~code ~message = error_at ~code ~message expr in
              match Trait_registry.lookup_trait trait_name with
              | None ->
                  Error
                    (mk_error ~code:"type-constructor" ~message:(Printf.sprintf "Unknown trait '%s'" trait_name))
              | Some _trait_def -> (
                  match Trait_registry.lookup_trait_method_with_supertraits trait_name method_name with
                  | None ->
                      Error
                        (mk_error ~code:"type-constructor"
                           ~message:(Printf.sprintf "Trait '%s' has no method '%s'" trait_name method_name))
                  | Some (source_trait_def, method_sig) -> (
                      let infer_static_qualified_trait_call () : (substitution * mono_type) infer_result =
                        let trait_instantiated_sig =
                          match source_trait_def.Trait_registry.trait_type_param with
                          | None -> method_sig
                          | Some type_param ->
                              let fresh = fresh_type_var () in
                              let s = substitution_singleton type_param fresh in
                              {
                                method_sig with
                                method_params =
                                  List.map
                                    (fun (name, ty) -> (name, apply_substitution s ty))
                                    method_sig.method_params;
                                method_return_type = apply_substitution s method_sig.method_return_type;
                              }
                        in
                        let check_trait_receiver receiver_type =
                          match receiver_type with
                          | TVar _ -> Ok ()
                          | _ -> (
                              match Trait_solver.satisfies_trait receiver_type trait_name with
                              | Ok () -> Ok ()
                              | Error diag -> Error (mk_error ~code:diag.code ~message:diag.message))
                        in
                        match
                          infer_static_qualified_method_call ~mk_error
                            ~call_target:(Printf.sprintf "%s.%s" trait_name method_name)
                            ~receiver_check:(Some check_trait_receiver) trait_instantiated_sig
                        with
                        | Error e -> Error e
                        | Ok (final_subst, return_type, resolved_method_type_args) ->
                            record_method_resolution expr (QualifiedTraitMethod trait_name);
                            record_method_type_args expr resolved_method_type_args;
                            Ok (final_subst, return_type)
                      in
                      match args with
                      | receiver_arg :: rest -> (
                          match infer_expression type_map env receiver_arg with
                          | Error e -> Error e
                          | Ok (subst_receiver, receiver_arg_type) -> (
                              let receiver_arg_type =
                                apply_substitution subst_receiver receiver_arg_type |> canonicalize_mono_type
                              in
                              match receiver_arg_type with
                              | TTraitObject _ -> (
                                  match
                                    instantiate_dynamic_trait_method_for_trait ~mk_error receiver_arg_type
                                      trait_name method_name
                                  with
                                  | Error e -> Error e
                                  | Ok dynamic_sig -> (
                                      match mc_type_args with
                                      | Some type_args when type_args <> [] ->
                                          Error
                                            (mk_error ~code:"type-constructor"
                                               ~message:
                                                 (Printf.sprintf
                                                    "Method '%s' takes no type parameters, but %d were provided"
                                                    method_name (List.length type_args)))
                                      | _ -> (
                                          let expected_rest_types =
                                            match dynamic_sig.method_params with
                                            | [] -> []
                                            | _receiver :: rest_params -> List.map snd rest_params
                                          in
                                          let env_after_receiver = apply_substitution_env subst_receiver env in
                                          match
                                            infer_args_against_expected type_map env_after_receiver subst_receiver
                                              rest expected_rest_types
                                          with
                                          | Error e -> Error e
                                          | Ok (final_subst, rest_arg_types) ->
                                              if List.length rest_arg_types <> List.length expected_rest_types
                                              then
                                                Error
                                                  (mk_error ~code:"type-constructor"
                                                     ~message:
                                                       (Printf.sprintf
                                                          "Qualified call '%s.%s' expects %d argument(s), got %d"
                                                          trait_name method_name
                                                          (List.length dynamic_sig.method_params)
                                                          (List.length args)))
                                              else (
                                                record_method_resolution expr (QualifiedTraitMethod trait_name);
                                                Ok
                                                  ( final_subst,
                                                    apply_substitution final_subst dynamic_sig.method_return_type
                                                  )))))
                              | _ -> infer_static_qualified_trait_call ()))
                      | [] -> infer_static_qualified_trait_call ()))
            in
            let infer_qualified_type_call
                ?(on_missing : (unit -> (substitution * mono_type) infer_result) option = None)
                (for_type : mono_type) : (substitution * mono_type) infer_result =
              match Inherent_registry.resolve_method for_type method_name with
              | Error msg -> Error (error_at ~code:"type-constructor" ~message:msg expr)
              | Ok None -> (
                  match on_missing with
                  | Some missing -> missing ()
                  | None ->
                      Error
                        (error_at ~code:"type-constructor"
                           ~message:
                             (Printf.sprintf "No inherent method '%s' found for type %s" method_name
                                (Types.to_string for_type))
                           expr))
              | Ok (Some method_sig) -> (
                  let mk_error ~code ~message = error_at ~code ~message expr in
                  match
                    infer_static_qualified_method_call ~mk_error
                      ~call_target:(Printf.sprintf "%s.%s" (Types.to_string for_type) method_name)
                      ~receiver_check:None method_sig
                  with
                  | Error e -> Error e
                  | Ok (final_subst, return_type, resolved_method_type_args) ->
                      record_method_resolution expr QualifiedInherentMethod;
                      record_method_type_args expr resolved_method_type_args;
                      record_effectful_method_call expr (method_sig.method_effect = `Effectful);
                      Ok (final_subst, return_type))
            in
            match receiver.expr with
            | AST.Identifier name -> (
                (* Use shared classifier for consistent priority across FieldAccess and MethodCall *)
                match classify_dotted_receiver env name method_name with
                | `BoundVar -> infer_real_method_call ()
                | `EnumVariant -> infer_enum_constructor name
                | `EnumType -> (
                    match resolve_dotted_type_name name with
	                    | Some for_type ->
	                        infer_qualified_type_call for_type
	                          ~on_missing:
	                            (Some
	                               (fun () ->
	                                 Error
	                                   (error_at ~code:"type-constructor"
	                                      ~message:(missing_constructor_or_inherent_method_message name method_name)
	                                      expr)))
                    | None -> infer_enum_constructor name)
                | `TypeName for_type -> infer_qualified_type_call for_type
                | `TraitName -> infer_qualified_trait_call name
                | `Unknown -> infer_real_method_call ())
            | _ -> infer_real_method_call ())
        | AST.BlockExpr stmts -> infer_block type_map env stmts)
  in
  let result =
    if standalone_placeholder_count > 1 then
      Error (placeholder_count_error expr standalone_placeholder_count)
    else if standalone_placeholder_count = 1 then
      match infer_without_section_fallback () with
      | Ok _ as ok -> ok
      | Error diag when placeholder_unbound_identifier_error diag ->
          infer_placeholder_section_expr type_map env expr
      | Error _ as err -> err
    else
      infer_without_section_fallback ()
  in
  (* Record the type in the type map before returning *)
  match result with
  | Ok (subst, t) ->
      let t' = apply_substitution subst t in
      record_type type_map expr t';
      Ok (subst, t)
  | Error e -> Error e
(* ============================================================
   Prefix Operators: !, -
   ============================================================ *)

and infer_prefix type_map env op operand =
  match infer_expression type_map env operand with
  | Error e -> Error e
  | Ok (subst, operand_type) -> (
      match op with
      | "!" -> (
          (* ! requires Bool, returns Bool *)
          match unify operand_type TBool with
          | Error e -> Error (error_at ~code:e.code ~message:e.message operand)
          | Ok subst2 -> Ok (compose_substitution subst subst2, TBool))
      | "-" -> (
          (* Unary negation requires neg trait. *)
          let operand_type' = apply_substitution subst operand_type in
          match enforce_trait_requirement_on_type operand_type' "neg" with
          | Ok () -> Ok (subst, operand_type')
          | Error diag -> Error (error_at ~code:diag.code ~message:diag.message operand))
      | _ ->
          Error
            (error_at ~code:"type-invalid-operator"
               ~message:("Invalid operator " ^ op ^ " for type " ^ to_string operand_type)
               operand))

(* ============================================================
   Infix Operators
   ============================================================ *)

and infer_infix type_map env left op right =
  match infer_expression type_map env left with
  | Error e -> Error e
  | Ok (subst1, left_type) -> (
      let env' = apply_substitution_env subst1 env in
      match infer_expression type_map env' right with
      | Error e -> Error e
      | Ok (subst2, right_type) -> (
          let subst = compose_substitution subst1 subst2 in
          let left_type' = apply_substitution subst2 left_type in
          match op with
          (* Arithmetic operators: both operands same numeric type, result same type *)
          | "+" | "-" | "*" | "/" -> (
              (* First unify left and right *)
              match unify left_type' right_type with
              | Error e -> Error (error_at ~code:e.code ~message:e.message right)
              | Ok subst3 -> (
                  let subst' = compose_substitution subst subst3 in
                  let result_type = apply_substitution subst3 left_type' in
                  (* For +, also allow String concatenation *)
                  if op = "+" && result_type = TString then
                    Ok (subst', result_type)
                  else
                    (* Arithmetic requires num trait. *)
                    match enforce_trait_requirement_on_type result_type "num" with
                    | Ok () -> Ok (subst', result_type)
                    | Error diag -> Error (error_at ~code:diag.code ~message:diag.message right)))
          | "%" -> (
              match unify left_type' right_type with
              | Error e -> Error (error_at ~code:e.code ~message:e.message right)
              | Ok subst3 -> (
                  let subst' = compose_substitution subst subst3 in
                  let result_type = apply_substitution subst3 left_type' in
                  match enforce_trait_requirement_on_type result_type "rem" with
                  | Ok () -> Ok (subst', result_type)
                  | Error diag -> Error (error_at ~code:diag.code ~message:diag.message right)))
          (* Comparison operators: both same type, result Bool *)
          | "<" | ">" | "<=" | ">=" -> (
              match unify left_type' right_type with
              | Error e -> Error (error_at ~code:e.code ~message:e.message right)
              | Ok subst3 -> (
                  let subst' = compose_substitution subst subst3 in
                  let operand_type = apply_substitution subst3 left_type' in
                  match enforce_trait_requirement_on_type operand_type "ord" with
                  | Ok () -> Ok (subst', TBool)
                  | Error diag -> Error (error_at ~code:diag.code ~message:diag.message right)))
          (* Equality operators: both same type, result Bool *)
          | "==" | "!=" -> (
              match unify left_type' right_type with
              | Error e -> Error (error_at ~code:e.code ~message:e.message right)
              | Ok subst3 -> (
                  let subst' = compose_substitution subst subst3 in
                  let operand_type = apply_substitution subst3 left_type' in
                  match enforce_trait_requirement_on_type operand_type "eq" with
                  | Ok () -> Ok (subst', TBool)
                  | Error diag -> Error (error_at ~code:diag.code ~message:diag.message right)))
          | "&&" | "||" -> (
              match unify left_type' TBool with
              | Error e -> Error (error_at ~code:e.code ~message:e.message left)
              | Ok subst3 -> (
                  let subst' = compose_substitution subst subst3 in
                  let right_type' = apply_substitution subst3 right_type in
                  match unify right_type' TBool with
                  | Error e -> Error (error_at ~code:e.code ~message:e.message right)
                  | Ok subst4 -> Ok (compose_substitution subst' subst4, TBool)))
          | _ ->
              Error
                (error_at ~code:"type-invalid-operator"
                   ~message:("Invalid operator " ^ op ^ " for type " ^ to_string left_type')
                   left)))

(* ============================================================
   If Expressions
   ============================================================ *)

(* Helper: Detect type narrowing pattern on identifiers and field paths. *)
and detect_is_narrowing (type_map : type_map) (expr : AST.expression) :
    (Type_narrowing.path * mono_type * mono_type) option =
  match expr.expr with
  | AST.TypeCheck (target_expr, type_ann) -> (
      match Type_narrowing.path_of_expr target_expr with
      | None -> None
      | Some path -> (
          match (expression_type_or_lookup type_map target_expr, Annotation.type_expr_to_mono_type type_ann) with
          | Ok current_type, Ok narrow_type -> Some (path, current_type, narrow_type)
          | _ -> None))
  | _ -> None

and substitute_narrowed_path_in_stmt
    (path : Type_narrowing.path) (replacement_name : string) (stmt : AST.statement) : AST.statement =
  Type_narrowing.substitute_path_in_stmt path
    ~replace:(Type_narrowing.replacement_identifier replacement_name)
    stmt

and narrow_identifier_in_env
    (env : type_env) (var_name : string) (current_type : mono_type) (narrow_type : mono_type) : type_env =
  match Type_narrowing.compute_narrowed_type current_type narrow_type with
  | Some narrowed -> TypeEnv.add var_name (Forall ([], narrowed)) env
  | None -> env

and narrow_identifier_to_complement
    (env : type_env) (var_name : string) (current_type : mono_type) (narrow_type : mono_type) : type_env =
  match Type_narrowing.compute_complement_type current_type narrow_type with
  | Some complement -> TypeEnv.add var_name (Forall ([], complement)) env
  | None -> env

and infer_if type_map env condition consequence alternative =
  (* Infer condition type *)
  match infer_expression type_map env condition with
  | Error e -> Error e
  | Ok (subst1, cond_type) -> (
      (* Condition must be Bool *)
      match unify cond_type TBool with
      | Error _ ->
          Error
            (error_at ~code:"type-if-condition"
               ~message:("If condition must be Bool, got: " ^ to_string cond_type)
               condition)
      | Ok subst2 -> (
          let subst = compose_substitution subst1 subst2 in
          let env' = apply_substitution_env subst env in

          (* Check if condition is an 'is' narrowing pattern *)
          let narrowing =
            detect_is_narrowing type_map condition
            |> Option.map (fun (path, current_type, narrow_type) ->
                   (path, apply_substitution subst current_type, apply_substitution subst narrow_type))
          in

          (* Create narrowed environment for consequence branch *)
          let env_cons, consequence_stmt =
            match narrowing with
            | Some (path, current_type, narrow_type) when Type_narrowing.is_identifier_path path ->
                (narrow_identifier_in_env env' path.root current_type narrow_type, consequence)
            | Some (path, current_type, narrow_type) ->
                let narrowed_type =
                  match Type_narrowing.compute_narrowed_type current_type narrow_type with
                  | Some narrowed -> narrowed
                  | None -> narrow_type
                in
                let narrowed_name = Type_narrowing.temp_name path "typed" in
                ( TypeEnv.add narrowed_name (Forall ([], narrowed_type)) env',
                  substitute_narrowed_path_in_stmt path narrowed_name consequence )
            | None -> (env', consequence)
          in

          (* Infer consequence type with narrowed environment *)
          match infer_statement type_map env_cons consequence_stmt with
          | Error e -> Error e
          | Ok (subst3, cons_type) -> (
              let subst' = compose_substitution subst subst3 in
              match alternative with
              | None ->
                  (* No else branch - if-expression has type Null *)
                  Ok (subst', TNull)
              | Some alt -> (
                  let env'' = apply_substitution_env subst' env' in
                  (* For alternative branch, narrow to complement type *)
                  let env_alt, alternative_stmt =
                    match narrowing with
                    | Some (path, current_type, narrow_type) when Type_narrowing.is_identifier_path path ->
                        ( narrow_identifier_to_complement env'' path.root
                            (apply_substitution subst' current_type)
                            (apply_substitution subst' narrow_type),
                          alt )
                    | Some (path, current_type, narrow_type) -> (
                        match
                          Type_narrowing.compute_complement_type
                            (apply_substitution subst' current_type)
                            (apply_substitution subst' narrow_type)
                        with
                        | Some complement_type ->
                            let complement_name = Type_narrowing.temp_name path "complement" in
                            ( TypeEnv.add complement_name (Forall ([], complement_type)) env'',
                              substitute_narrowed_path_in_stmt path complement_name alt )
                        | None -> (env'', alt))
                    | None -> (env'', alt)
                  in
                  match infer_statement type_map env_alt alternative_stmt with
                  | Error e -> Error e
                  | Ok (subst4, alt_type) -> (
                      let subst'' = compose_substitution subst' subst4 in
                      let cons_type' = apply_substitution subst4 cons_type in

                      (* If we have type narrowing, preserve the wider (union) type *)
                      match narrowing with
                      | Some _ ->
                          (* With narrowing, create union of both branch types *)
                          let union_type = Types.normalize_union [ cons_type'; alt_type ] in
                          Ok (subst'', union_type)
                      | None -> (
                          (* No narrowing - try to unify both branches *)
                          match unify cons_type' alt_type with
                          | Ok subst5 ->
                              (* Types unified successfully *)
                              let final_subst = compose_substitution subst'' subst5 in
                              let result_type = apply_substitution subst5 cons_type' in
                              Ok (final_subst, result_type)
                          | Error _ ->
                              (* Types don't unify - create union (Phase 4.1) *)
                              let union_type = Types.normalize_union [ cons_type'; alt_type ] in
                              Ok (subst'', union_type)))))))

(* ============================================================
   Functions
   ============================================================ *)

(* Collect all return types from a statement, unifying them with an expected type.
   Returns the updated substitution and the unified return type. *)
and collect_and_unify_returns type_map env expected_ret_type (stmt : AST.statement) subst :
    (substitution * mono_type, Diagnostic.t) result =
  match stmt.AST.stmt with
  | AST.ExportDecl _ | AST.ImportDecl _ -> Ok (subst, expected_ret_type)
  | AST.Return expr -> (
      match infer_expression type_map env expr with
      | Error e -> Error e
      | Ok (subst1, ret_type) -> (
          let subst' = compose_substitution subst subst1 in
          let expected' = apply_substitution subst' expected_ret_type in
          let ret_type' = apply_substitution subst' ret_type in
          match Unify.unify expected' ret_type' with
          | Error e -> Error (error_at ~code:e.code ~message:e.message expr)
          | Ok subst2 ->
              let final_subst = compose_substitution subst' subst2 in
              let final_ret = apply_substitution subst2 ret_type' in
              Ok (final_subst, final_ret)))
  | AST.Block stmts ->
      let rec go subst ret_type = function
        | [] -> Ok (subst, ret_type)
        | s :: rest -> (
            match collect_and_unify_returns type_map env ret_type s subst with
            | Error e -> Error e
            | Ok (subst', ret_type') -> go subst' ret_type' rest)
      in
      go subst expected_ret_type stmts
  | AST.ExpressionStmt expr -> (
      match expr.AST.expr with
      | AST.If (_cond, cons, alt_opt) -> (
          match collect_and_unify_returns type_map env expected_ret_type cons subst with
          | Error e -> Error e
          | Ok (subst', ret_type') -> (
              match alt_opt with
              | None -> Ok (subst', ret_type')
              | Some alt -> collect_and_unify_returns type_map env ret_type' alt subst'))
      | _ -> Ok (subst, expected_ret_type))
  | AST.Let _ -> Ok (subst, expected_ret_type)
  | AST.EnumDef _ | AST.TypeDef _ | AST.ShapeDef _ -> Ok (subst, expected_ret_type)
  | AST.TraitDef _ -> Ok (subst, expected_ret_type) (* TODO: Phase 4.3 - handle trait defs *)
  | AST.ImplDef _ -> Ok (subst, expected_ret_type) (* TODO: Phase 4.3 - handle impl defs *)
  | AST.InherentImplDef _ -> Ok (subst, expected_ret_type) (* TODO: Phase 4.5 - handle inherent impl defs *)
  | AST.DeriveDef _ -> Ok (subst, expected_ret_type) (* TODO: Phase 4.3 - handle derive defs *)
  | AST.TypeAlias _ -> Ok (subst, expected_ret_type)
(* TODO: Phase 4.4 - handle type aliases *)

(* ============================================================
   Purity Enforcement
   ============================================================

   Walks a function body to detect calls to effectful functions.
   Used for:
   - Case 1: Error when a pure (->) function calls effectful operations
   - Case 3: Infer effectfulness for unannotated functions
*)

and body_has_effectful_call (type_map : type_map) (stmt : AST.statement) : bool =
  match stmt.stmt with
  | AST.ExportDecl _ | AST.ImportDecl _ -> false
  | AST.Let { value; _ } -> expr_has_effectful_call type_map value
  | AST.Return expr -> expr_has_effectful_call type_map expr
  | AST.ExpressionStmt expr -> expr_has_effectful_call type_map expr
  | AST.Block stmts -> List.exists (body_has_effectful_call type_map) stmts
  | AST.EnumDef _ | AST.TypeDef _ | AST.ShapeDef _ | AST.TraitDef _ | AST.ImplDef _ | AST.InherentImplDef _
  | AST.DeriveDef _ | AST.TypeAlias _ ->
      false

and type_may_be_effectful_callable (typ : mono_type) : bool =
  match typ with
  | TFun (_, _, true) -> true
  | TUnion members -> List.exists type_may_be_effectful_callable members
  | _ -> false

and expr_has_effectful_call (type_map : type_map) (expr : AST.expression) : bool =
  match expr.expr with
  | AST.TypeApply (callee, _type_args) -> expr_has_effectful_call type_map callee
  | AST.Call (func, args) ->
      let func_is_effectful =
        match Hashtbl.find_opt type_map func.id with
        | Some typ -> type_may_be_effectful_callable typ
        | _ -> false
      in
      func_is_effectful
      || expr_has_effectful_call type_map func
      || List.exists (expr_has_effectful_call type_map) args
  | AST.MethodCall { mc_receiver; mc_args; _ } ->
      lookup_effectful_method_call expr.id
      || expr_has_effectful_call type_map mc_receiver
      || List.exists (expr_has_effectful_call type_map) mc_args
  | AST.If (cond, then_branch, else_branch) -> (
      expr_has_effectful_call type_map cond
      || body_has_effectful_call type_map then_branch
      ||
      match else_branch with
      | None -> false
      | Some b -> body_has_effectful_call type_map b)
  | AST.Function _ -> false (* defining a function is not calling one *)
  | AST.Prefix (_, e) -> expr_has_effectful_call type_map e
  | AST.Infix (l, _, r) -> expr_has_effectful_call type_map l || expr_has_effectful_call type_map r
  | AST.Array elements -> List.exists (expr_has_effectful_call type_map) elements
  | AST.Index (arr, idx) -> expr_has_effectful_call type_map arr || expr_has_effectful_call type_map idx
  | AST.Hash pairs ->
      List.exists (fun (k, v) -> expr_has_effectful_call type_map k || expr_has_effectful_call type_map v) pairs
  | AST.Match (scrutinee, arms) ->
      expr_has_effectful_call type_map scrutinee
      || List.exists (fun (arm : AST.match_arm) -> expr_has_effectful_call type_map arm.body) arms
  | AST.RecordLit (fields, spread) -> (
      List.exists
        (fun (f : AST.record_field) ->
          match f.field_value with
          | None -> false
          | Some v -> expr_has_effectful_call type_map v)
        fields
      ||
      match spread with
      | None -> false
      | Some e -> expr_has_effectful_call type_map e)
  | AST.FieldAccess (e, _) -> expr_has_effectful_call type_map e
  | AST.TypeCheck (e, _) -> expr_has_effectful_call type_map e
  | AST.EnumConstructor (_, _, args) -> List.exists (expr_has_effectful_call type_map) args
  | AST.Identifier _ | AST.Integer _ | AST.Float _ | AST.Boolean _ | AST.String _ -> false
  | AST.BlockExpr stmts -> List.exists (body_has_effectful_call type_map) stmts

(* Phase 7: Wrap an expression body in a synthetic statement for type_callable. *)
and wrap_expr_as_stmt (expr : AST.expression) : AST.statement =
  { stmt = AST.ExpressionStmt expr; pos = expr.pos; end_pos = expr.end_pos; file_id = expr.file_id }

(* Phase 7: Unified callable typing engine.
   All callable forms — top-level functions, trait impl methods, and inherent impl
   methods — are typed through this single function. Adapters provide context-specific
   information via labeled arguments.

   Parameter type priority: explicit annotation > known positional type > fresh TVar
   Return type priority: explicit annotation > known return type > inferred from body
   Effect priority: explicit annotation > inferred from body *)
and type_callable
    (type_map : type_map)
    (env : type_env)
    ~(type_bindings : (string * mono_type) list)
    ~(known_param_types : (int * mono_type) list)
    ~(params : (string * AST.type_expr option) list)
    ~(return_annot : AST.type_expr option)
    ~(known_return : mono_type option)
    ~(effect_annot : [ `Pure | `Effectful | `Unspecified ])
    ~(strict_return_check : bool)
    ~(body : AST.statement) :
    (string list * mono_type list * mono_type * bool * substitution, Diagnostic.t) result =
  (* 1. Resolve parameter types: annotation > known positional > fresh TVar *)
  let convert_annot te =
    match Annotation.type_expr_to_mono_type_with type_bindings te with
    | Error d when d.Diagnostic.code = "type-open-row-rejected" -> Error d
    | Error d -> Error d
    | Ok mono -> Ok mono
  in
  let param_types_result =
    let rec loop idx acc = function
      | [] -> Ok (List.rev acc)
      | (_name, annot_opt) :: rest -> (
          let ty_result =
            match annot_opt with
            | Some te -> convert_annot te
            | None -> (
                match List.assoc_opt idx known_param_types with
                | Some known -> Ok known
                | None -> Ok (fresh_type_var ()))
          in
          match ty_result with
          | Error e -> Error e
          | Ok t -> loop (idx + 1) (t :: acc) rest)
    in
    loop 0 [] params
  in
  match param_types_result with
  | Error e -> Error e
  | Ok param_types -> (
      let param_names = List.map fst params in
      (* 2. Build environment with parameter bindings *)
      let env' =
        List.fold_left2
          (fun acc name mono -> TypeEnv.add name (mono_to_poly mono) acc)
          env param_names param_types
      in
      (* 3. Resolve expected return type: annotation > known return > None (infer) *)
      let expected_return_result =
        match return_annot with
        | Some te -> (
            match Annotation.type_expr_to_mono_type_with type_bindings te with
            | Error d when should_preserve_annotation_error te d -> Error d
            | Error _ -> Ok None
            | Ok t -> Ok (Some t))
        | None -> Ok known_return
      in
      match expected_return_result with
      | Error e -> Error e
      | Ok expected_return_opt -> (
          (* 4. Infer body type *)
          match infer_statement type_map env' body with
          | Error e -> Error e
          | Ok (subst, body_type) -> (
              let body_type' = apply_substitution subst body_type in
              apply_substitution_type_map subst type_map;
              (* 5. Determine effectfulness *)
              let has_effects = body_has_effectful_call type_map body in
              if effect_annot = `Pure && has_effects then
                Error
                  (error_at_stmt ~code:"type-purity"
                     ~message:
                       "Pure function (declared with ->) cannot call effectful operations. Use => to declare an effectful function."
                     body)
              else
                let actual_effectful =
                  match effect_annot with
                  | `Effectful -> true
                  | `Pure -> false
                  | `Unspecified -> has_effects
                in
                (* 6. Return type validation *)
                match expected_return_opt with
                | None -> (
                    (* No expected return type — infer from body and explicit returns *)
                    match collect_and_unify_returns type_map env' body_type' body subst with
                    | Error e -> Error e
                    | Ok (subst', unified_ret_type) ->
                        Ok (param_names, param_types, unified_ret_type, actual_effectful, subst'))
                | Some expected_ret ->
                    let* () = validate_return_statements type_map env' expected_ret body in
                    let* () = record_explicit_return_trait_object_coercions type_map expected_ret body in
                    let* () = record_tail_trait_object_coercions type_map expected_ret body in
                    if strict_return_check then
                      (* Strict mode (methods): subtype check only, no unification fallback.
                         Method generic type vars use original names and must remain polymorphic. *)
                      let expected_ret' = apply_substitution subst expected_ret in
                      let strict_return_ok =
                        if Annotation.check_annotation expected_ret' body_type' then
                          true
                        else if intersection_annotation_compatible body_type' expected_ret' then
                          true
                        else
                          match canonicalize_mono_type expected_ret' with
                          | TTraitObject target_traits -> (
                              match
                                classify_trait_object_compatibility
                                  ~mk_error:(fun ~code:_ ~message:_ ->
                                    Diagnostic.error_no_span ~code:"type-return-mismatch" ~message:"")
                                  body_type' target_traits
                              with
                              | Ok _ -> true
                              | Error _ -> false)
                          | _ -> false
                      in
                      if strict_return_ok then
                        Ok (param_names, param_types, expected_ret, actual_effectful, subst)
                      else
                        Error
                          (error_at_stmt ~code:"type-return-mismatch"
                             ~message:
                               ("Function return type annotation mismatch: expected "
                               ^ to_string expected_ret'
                               ^ " but inferred "
                               ^ to_string body_type')
                             body)
                    else
                      (* Permissive mode (top-level functions): subtype + unification fallback.
                         Top-level generic type vars use fresh names that can be specialized. *)
                      let rec has_unresolved_var (t : mono_type) : bool =
                        match t with
                        | TVar _ | TRowVar _ -> true
                        | TFun (arg, ret, _) -> has_unresolved_var arg || has_unresolved_var ret
                        | TArray elem -> has_unresolved_var elem
                        | THash (k, v) -> has_unresolved_var k || has_unresolved_var v
                        | TRecord (fields, row) -> (
                            List.exists (fun (f : record_field_type) -> has_unresolved_var f.typ) fields
                            ||
                            match row with
                            | None -> false
                            | Some r -> has_unresolved_var r)
                        | TTraitObject _ -> false
                        | TEnum (_, args) | TNamed (_, args) | TUnion args | TIntersection args ->
                            List.exists has_unresolved_var args
                        | TInt | TFloat | TBool | TString | TNull -> false
                      in
                      let subtype_ok =
                        Annotation.is_subtype_of body_type' expected_ret
                        || intersection_annotation_compatible body_type' expected_ret
                      in
                      let unify_compatible =
                        (has_unresolved_var body_type' || has_unresolved_var expected_ret)
                        &&
                        match unify body_type' expected_ret with
                        | Ok _ -> true
                        | Error _ -> false
                      in
                      if subtype_ok then
                        Ok (param_names, param_types, expected_ret, actual_effectful, subst)
                      else if unify_compatible then
                        match unify body_type' expected_ret with
                        | Error _e ->
                            Error
                              (error_at_stmt ~code:"type-return-mismatch"
                                 ~message:
                                   ("Function return type annotation mismatch: expected "
                                   ^ to_string expected_ret
                                   ^ " but inferred "
                                   ^ to_string body_type')
                                 body)
                        | Ok subst2 ->
                            let final_subst = compose_substitution subst subst2 in
                            let final_return_type = expected_ret in
                            Ok (param_names, param_types, final_return_type, actual_effectful, final_subst)
                      else
                        Error
                          (error_at_stmt ~code:"type-return-mismatch"
                             ~message:
                               ("Function return type annotation mismatch: expected "
                               ^ to_string expected_ret
                               ^ " but inferred "
                               ^ to_string body_type')
                             body))))

(* Top-level function adapter: processes generics, maps effect annotation, builds TFun type *)
and contextual_callable_signature (typ : mono_type) : (mono_type list * mono_type) option =
  let rec flatten rev_params = function
    | TFun (arg, ret, _) -> flatten (arg :: rev_params) ret
    | ret -> (List.rev rev_params, ret)
  in
  let signatures =
    match canonicalize_mono_type typ with
    | TFun _ as fn -> [ flatten [] fn ]
    | TUnion members ->
        List.filter_map
          (fun member ->
            match canonicalize_mono_type member with
            | TFun _ as fn -> Some (flatten [] fn)
            | _ -> None)
          members
    | _ -> []
  in
  match signatures with
  | [] -> None
  | signature :: rest when List.for_all (( = ) signature) rest -> Some signature
  | _ -> None

and contextual_callable_signature_for_arity (arity : int) (typ : mono_type) : (mono_type list * mono_type) option
    =
  let rec take_params rev_params remaining typ =
    if remaining = 0 then
      Some (List.rev rev_params, canonicalize_mono_type typ)
    else
      match canonicalize_mono_type typ with
      | TFun (arg, ret, _) -> take_params (arg :: rev_params) (remaining - 1) ret
      | _ -> None
  in
  let signatures =
    match canonicalize_mono_type typ with
    | TFun (TNull, ret, _) when arity = 0 -> [ ([], canonicalize_mono_type ret) ]
    | TFun _ as fn when arity > 0 -> Option.to_list (take_params [] arity fn)
    | TUnion members ->
        List.filter_map
          (fun member ->
            match canonicalize_mono_type member with
            | TFun (TNull, ret, _) when arity = 0 -> Some ([], canonicalize_mono_type ret)
            | TFun _ as fn when arity > 0 -> take_params [] arity fn
            | _ -> None)
          members
    | _ -> []
  in
  match signatures with
  | [] -> None
  | signature :: rest when List.for_all (( = ) signature) rest -> Some signature
  | _ -> None

and infer_function_with_annotations
    ?(known_param_types = []) ?known_return type_map env generics_opt params return_annot is_effectful body =
  (* Process generic parameters and their constraints *)
  let outer_type_bindings = user_named_type_bindings_in_env env in
  let local_type_var_map =
    match generics_opt with
    | None -> []
    | Some generics ->
        List.map
          (fun (generic : AST.generic_param) ->
            let fresh_var = fresh_type_var () in
            (match fresh_var with
            | TVar n ->
                add_type_var_constraints n generic.constraints;
                record_type_var_user_name ~fresh_name:n ~user_name:generic.name
            | _ -> ());
            (generic.name, fresh_var))
          generics
  in
  let type_var_map = local_type_var_map @ outer_type_bindings in
  (* Map effect annotation: => is Effectful, -> with return annot is Pure, otherwise Unspecified *)
  let effect_annot =
    if is_effectful then
      `Effectful
    else if Option.is_some return_annot then
      `Pure
    else
      `Unspecified
  in
  match
    type_callable type_map env ~type_bindings:type_var_map ~known_param_types ~params ~return_annot ~known_return
      ~effect_annot ~strict_return_check:false ~body
  with
  | Error e -> Error e
  | Ok (_param_names, param_types, ret_type, actual_effectful, subst) ->
      let mk_fun a b = TFun (a, b, actual_effectful) in
      let param_types' = List.map (apply_substitution subst) param_types in
      let func_type = List.fold_right mk_fun param_types' ret_type in
      Ok (subst, func_type)

and collect_used_names_expr (used : StringSet.t) (expr : AST.expression) : StringSet.t =
  match expr.expr with
  | AST.Identifier name -> StringSet.add name used
  | AST.Integer _ | AST.Float _ | AST.Boolean _ | AST.String _ -> used
  | AST.Prefix (_, inner) | AST.TypeCheck (inner, _) | AST.FieldAccess (inner, _) | AST.TypeApply (inner, _) ->
      collect_used_names_expr used inner
  | AST.Infix (left, _, right) -> collect_used_names_expr (collect_used_names_expr used left) right
  | AST.If (cond, cons, alt) -> (
      let used = collect_used_names_expr used cond in
      let used = collect_used_names_stmt used cons in
      match alt with
      | Some stmt -> collect_used_names_stmt used stmt
      | None -> used)
  | AST.Function { params; _ } -> List.fold_left (fun acc (name, _) -> StringSet.add name acc) used params
  | AST.Call (fn_expr, args) -> List.fold_left collect_used_names_expr (collect_used_names_expr used fn_expr) args
  | AST.Array elements -> List.fold_left collect_used_names_expr used elements
  | AST.Hash pairs ->
      List.fold_left
        (fun acc (key, value) -> collect_used_names_expr (collect_used_names_expr acc key) value)
        used pairs
  | AST.Index (container, index) -> collect_used_names_expr (collect_used_names_expr used container) index
  | AST.EnumConstructor (_, _, args) -> List.fold_left collect_used_names_expr used args
  | AST.Match (scrutinee, arms) ->
      List.fold_left
        (fun acc arm -> collect_used_names_expr acc arm.AST.body)
        (collect_used_names_expr used scrutinee)
        arms
  | AST.RecordLit (fields, spread) -> (
      let used =
        List.fold_left
          (fun acc (field : AST.record_field) ->
            match field.field_value with
            | Some value -> collect_used_names_expr acc value
            | None -> acc)
          used fields
      in
      match spread with
      | Some spread_expr -> collect_used_names_expr used spread_expr
      | None -> used)
  | AST.MethodCall { mc_receiver; mc_args; _ } ->
      List.fold_left collect_used_names_expr (collect_used_names_expr used mc_receiver) mc_args
  | AST.BlockExpr stmts -> collect_used_names_stmts used stmts

and collect_used_names_stmt (used : StringSet.t) (stmt : AST.statement) : StringSet.t =
  match stmt.stmt with
  | AST.ExportDecl _ | AST.ImportDecl _ -> used
  | AST.Let { name; value; _ } -> collect_used_names_expr (StringSet.add name used) value
  | AST.Return expr | AST.ExpressionStmt expr -> collect_used_names_expr used expr
  | AST.Block stmts -> collect_used_names_stmts used stmts
  | AST.EnumDef _ | AST.TypeDef _ | AST.ShapeDef _ | AST.TraitDef _ | AST.ImplDef _ | AST.InherentImplDef _
  | AST.DeriveDef _ | AST.TypeAlias _ ->
      used

and collect_used_names_stmts (used : StringSet.t) (stmts : AST.statement list) : StringSet.t =
  List.fold_left collect_used_names_stmt used stmts

and fresh_placeholder_param_name (expr : AST.expression) : string =
  let used = collect_used_names_expr StringSet.empty expr in
  let rec loop n =
    let candidate =
      if n = 0 then
        "it"
      else
        Printf.sprintf "it%d" n
    in
    if StringSet.mem candidate used then
      loop (n + 1)
    else
      candidate
  in
  loop 0

and placeholder_shadow_expr_id (expr_id : int) : int = -((expr_id * 2) + 1)
and placeholder_wrapper_expr_id (expr_id : int) : int = -((expr_id * 2) + 2)

and replace_placeholder_identifier_expr (param_name : string) (expr : AST.expression) : AST.expression =
  let rewritten =
    match expr.expr with
    | AST.Identifier "_" -> AST.Identifier param_name
    | AST.Identifier _ | AST.Integer _ | AST.Float _ | AST.Boolean _ | AST.String _ -> expr.expr
    | AST.Prefix (op, inner) -> AST.Prefix (op, replace_placeholder_identifier_expr param_name inner)
    | AST.Infix (left, op, right) ->
        AST.Infix
          ( replace_placeholder_identifier_expr param_name left,
            op,
            replace_placeholder_identifier_expr param_name right )
    | AST.TypeCheck (inner, typ) -> AST.TypeCheck (replace_placeholder_identifier_expr param_name inner, typ)
    | AST.TypeApply (callee, type_args) ->
        AST.TypeApply (replace_placeholder_identifier_expr param_name callee, type_args)
    | AST.If (cond, cons, alt) ->
        AST.If
          ( replace_placeholder_identifier_expr param_name cond,
            replace_placeholder_identifier_stmt param_name cons,
            Option.map (replace_placeholder_identifier_stmt param_name) alt )
    | AST.Function _ -> expr.expr
    | AST.Call (fn_expr, args) ->
        AST.Call
          ( replace_placeholder_identifier_expr param_name fn_expr,
            List.map (replace_placeholder_identifier_expr param_name) args )
    | AST.Array elements -> AST.Array (List.map (replace_placeholder_identifier_expr param_name) elements)
    | AST.Hash pairs ->
        AST.Hash
          (List.map
             (fun (key, value) ->
               ( replace_placeholder_identifier_expr param_name key,
                 replace_placeholder_identifier_expr param_name value ))
             pairs)
    | AST.Index (container, index) ->
        AST.Index
          ( replace_placeholder_identifier_expr param_name container,
            replace_placeholder_identifier_expr param_name index )
    | AST.EnumConstructor (enum_name, variant_name, args) ->
        AST.EnumConstructor
          (enum_name, variant_name, List.map (replace_placeholder_identifier_expr param_name) args)
    | AST.Match (scrutinee, arms) ->
        AST.Match
          ( replace_placeholder_identifier_expr param_name scrutinee,
            List.map
              (fun (arm : AST.match_arm) ->
                { arm with body = replace_placeholder_identifier_expr param_name arm.body })
              arms )
    | AST.RecordLit (fields, spread) ->
        AST.RecordLit
          ( List.map
              (fun (field : AST.record_field) ->
                {
                  field with
                  field_value = Option.map (replace_placeholder_identifier_expr param_name) field.field_value;
                })
              fields,
            Option.map (replace_placeholder_identifier_expr param_name) spread )
    | AST.FieldAccess (receiver, field_name) ->
        AST.FieldAccess (replace_placeholder_identifier_expr param_name receiver, field_name)
    | AST.MethodCall { mc_receiver; mc_method; mc_type_args; mc_args } ->
        AST.MethodCall
          {
            mc_receiver = replace_placeholder_identifier_expr param_name mc_receiver;
            mc_method;
            mc_type_args;
            mc_args = List.map (replace_placeholder_identifier_expr param_name) mc_args;
          }
    | AST.BlockExpr stmts -> AST.BlockExpr (List.map (replace_placeholder_identifier_stmt param_name) stmts)
  in
  { expr with id = placeholder_shadow_expr_id expr.id; expr = rewritten }

and replace_placeholder_identifier_stmt (param_name : string) (stmt : AST.statement) : AST.statement =
  let rewritten =
    match stmt.stmt with
    | AST.ExportDecl _ | AST.ImportDecl _ -> stmt.stmt
    | AST.Let ({ value; _ } as let_binding) ->
        AST.Let { let_binding with value = replace_placeholder_identifier_expr param_name value }
    | AST.Return expr -> AST.Return (replace_placeholder_identifier_expr param_name expr)
    | AST.ExpressionStmt expr -> AST.ExpressionStmt (replace_placeholder_identifier_expr param_name expr)
    | AST.Block stmts -> AST.Block (List.map (replace_placeholder_identifier_stmt param_name) stmts)
    | AST.EnumDef _ | AST.TypeDef _ | AST.ShapeDef _ | AST.TraitDef _ | AST.ImplDef _ | AST.InherentImplDef _
    | AST.DeriveDef _ | AST.TypeAlias _ ->
        stmt.stmt
  in
  { stmt with stmt = rewritten }

and placeholder_callback_expectation (typ : mono_type) : placeholder_callback_expectation =
  match canonicalize_mono_type typ with
  | TFun (_, _, is_effectful) ->
      if is_effectful then
        PlaceholderCallbackEffectfulOnly
      else
        PlaceholderCallbackPureAllowed
  | TUnion members ->
      List.fold_left
        (fun acc member ->
          match (acc, placeholder_callback_expectation member) with
          | PlaceholderCallbackPureAllowed, _ | _, PlaceholderCallbackPureAllowed ->
              PlaceholderCallbackPureAllowed
          | PlaceholderCallbackEffectfulOnly, PlaceholderCallbackEffectfulOnly -> PlaceholderCallbackEffectfulOnly
          | PlaceholderCallbackNotCallable, PlaceholderCallbackEffectfulOnly
          | PlaceholderCallbackEffectfulOnly, PlaceholderCallbackNotCallable ->
              PlaceholderCallbackEffectfulOnly
          | PlaceholderCallbackNotCallable, PlaceholderCallbackNotCallable -> PlaceholderCallbackNotCallable)
        PlaceholderCallbackNotCallable members
  | _ -> PlaceholderCallbackNotCallable

and placeholder_lambda_expr (expr : AST.expression) : AST.expression =
  let param_name = fresh_placeholder_param_name expr in
  let body_expr = replace_placeholder_identifier_expr param_name expr in
  let wrapper_id = placeholder_wrapper_expr_id expr.id in
  let body_stmt =
    AST.mk_stmt ~pos:body_expr.pos ~end_pos:body_expr.end_pos ~file_id:body_expr.file_id
      (AST.Block
         [
           AST.mk_stmt ~pos:body_expr.pos ~end_pos:body_expr.end_pos ~file_id:body_expr.file_id
             (AST.ExpressionStmt body_expr);
         ])
  in
  AST.mk_expr ~id:wrapper_id ~pos:expr.pos ~end_pos:expr.end_pos ~file_id:expr.file_id
    (AST.Function
       {
         origin = AST.PlaceholderSection;
         generics = None;
         params = [ (param_name, None) ];
         return_type = None;
         is_effectful = false;
         body = body_stmt;
       })

and placeholder_count_in_current_context (expr : AST.expression) : int =
  if placeholder_rewrite_enabled () then
    placeholder_identifier_count_expr expr
  else
    0

and placeholder_count_error (expr : AST.expression) (count : int) : Diagnostic.t =
  error_at ~code:"type-invalid-placeholder"
    ~message:(Printf.sprintf "Placeholder shorthand requires exactly one '_' placeholder, found %d" count)
    expr

and placeholder_effectful_error (expr : AST.expression) : Diagnostic.t =
  error_at ~code:"type-invalid-placeholder"
    ~message:"Placeholder shorthand is pure-only; use an explicit '=>' lambda for effectful functions" expr

and placeholder_unbound_identifier_error (diag : Diagnostic.t) : bool =
  diag.code = "type-unbound-var" && String_utils.contains_substring ~needle:"Unbound variable: _" diag.message

and placeholder_callable_is_effectful (typ : mono_type) : bool =
  match canonicalize_mono_type typ with
  | TFun (_, _, is_effectful) -> is_effectful
  | TUnion members -> List.exists placeholder_callable_is_effectful members
  | _ -> false

and infer_function_literal
    ?known_signature
    (type_map : type_map)
    (env : type_env)
    (fn_expr : AST.expression)
    ~(origin : AST.function_origin)
    ~(generics : AST.generic_param list option)
    ~(params : (string * AST.type_expr option) list)
    ~(return_type : AST.type_expr option)
    ~(is_effectful : bool)
    ~(body : AST.statement) : (substitution * mono_type) infer_result =
  let infer_literal () =
    match known_signature with
    | Some (param_types, known_return_type) ->
        let known_param_types = List.mapi (fun i ty -> (i, ty)) param_types in
        infer_function_with_annotations ~known_param_types ~known_return:known_return_type type_map env generics
          params return_type is_effectful body
    | None -> infer_function_with_annotations type_map env generics params return_type is_effectful body
  in
  let infer_result =
    match origin with
    | AST.ExplicitLambda -> with_placeholder_section_rewrite_disabled infer_literal
    | AST.DeclaredFunction | AST.PlaceholderSection -> infer_literal ()
  in
  match infer_result with
  | Ok (subst_fn, func_type) ->
      record_type type_map fn_expr (apply_substitution subst_fn func_type);
      Ok (subst_fn, func_type)
  | Error _ as err -> err

and callable_annotation_context ?(type_bindings = []) ?arity (type_annotation : AST.type_expr option) :
    ((mono_type * (mono_type list * mono_type)) option, Diagnostic.t) result =
  match type_annotation with
  | None -> Ok None
  | Some type_expr -> (
      match Annotation.type_expr_to_mono_type_with type_bindings type_expr with
      | Error d when should_preserve_annotation_error type_expr d -> Error d
      | Error _ -> Ok None
      | Ok annotated_type -> (
          let signature =
            match arity with
            | Some lambda_arity -> contextual_callable_signature_for_arity lambda_arity annotated_type
            | None -> contextual_callable_signature annotated_type
          in
          match signature with
          | Some signature -> Ok (Some (annotated_type, signature))
          | None -> Ok None))

and infer_placeholder_section_expr (type_map : type_map) (env : type_env) (expr : AST.expression) :
    (substitution * mono_type) infer_result =
  let rewritten_expr = placeholder_lambda_expr expr in
  record_placeholder_rewrite expr rewritten_expr;
  match rewritten_expr.expr with
  | AST.Function { origin; generics; params; return_type; is_effectful; body } -> (
      match
        infer_function_literal type_map env rewritten_expr ~origin ~generics ~params ~return_type ~is_effectful
          ~body
      with
      | Error _ as err -> err
      | Ok (subst, func_type) ->
          let inferred_type = apply_substitution subst func_type in
          if placeholder_callable_is_effectful inferred_type then
            Error (placeholder_effectful_error expr)
          else
            Ok (subst, func_type))
  | _ -> failwith "placeholder section rewrite must produce a function literal"

and infer_block_against_expected type_map env stmts expected_type =
  match stmts with
  | [] -> (
      match compatible_with_expected_type TNull expected_type with
      | Error e -> Error e
      | Ok subst -> Ok (subst, apply_substitution subst expected_type))
  | [ stmt ] -> infer_statement_against_expected type_map env stmt expected_type
  | stmt :: rest -> (
      match infer_statement type_map env stmt with
      | Error e -> Error e
      | Ok (subst1, stmt_type) -> (
          let env' =
            match stmt.stmt with
            | AST.Let let_binding ->
                let env_subst = apply_substitution_env subst1 env in
                if let_binding.name = "_" then
                  env_subst
                else
                  let binding_type =
                    binding_type_for_env ~value_expr:let_binding.value
                      ~type_annotation:let_binding.type_annotation stmt_type
                  in
                  let poly =
                    if should_monomorphize_let_binding_value let_binding.value then
                      mono_to_poly binding_type
                    else
                      generalize env_subst binding_type
                  in
                  TypeEnv.add let_binding.name poly env_subst
            | _ -> apply_substitution_env subst1 env
          in
          match infer_block_against_expected type_map env' rest (apply_substitution subst1 expected_type) with
          | Error e -> Error e
          | Ok (subst2, result_type) -> Ok (compose_substitution subst1 subst2, result_type)))

and infer_statement_against_expected type_map env stmt expected_type =
  match stmt.stmt with
  | AST.ExpressionStmt expr | AST.Return expr ->
      infer_arg_against_expected type_map env empty_substitution expr expected_type
  | AST.Block stmts -> infer_block_against_expected type_map env stmts expected_type
  | _ -> (
      match infer_statement type_map env stmt with
      | Error e -> Error e
      | Ok (subst, inferred_type) -> (
          let inferred_type' = apply_substitution subst inferred_type in
          let expected_type' = apply_substitution subst expected_type in
          match compatible_with_expected_type inferred_type' expected_type' with
          | Error e -> Error e
          | Ok subst2 ->
              let final_subst = compose_substitution subst subst2 in
              Ok (final_subst, apply_substitution final_subst expected_type)))

and infer_if_against_expected type_map env condition consequence alternative expected_type =
  match alternative with
  | None -> (
      match infer_if type_map env condition consequence alternative with
      | Error e -> Error e
      | Ok (subst, inferred_type) -> (
          let inferred_type' = apply_substitution subst inferred_type in
          let expected_type' = apply_substitution subst expected_type in
          match compatible_with_expected_type inferred_type' expected_type' with
          | Error e -> Error e
          | Ok subst2 ->
              let final_subst = compose_substitution subst subst2 in
              Ok (final_subst, apply_substitution final_subst expected_type)))
  | Some alt -> (
      match infer_expression type_map env condition with
      | Error e -> Error e
      | Ok (subst1, cond_type) -> (
          match unify cond_type TBool with
          | Error _ ->
              Error
                (error_at ~code:"type-if-condition"
                   ~message:("If condition must be Bool, got: " ^ to_string cond_type)
                   condition)
          | Ok subst2 -> (
              let subst = compose_substitution subst1 subst2 in
              let env' = apply_substitution_env subst env in
              let narrowing =
                detect_is_narrowing type_map condition
                |> Option.map (fun (path, current_type, narrow_type) ->
                       (path, apply_substitution subst current_type, apply_substitution subst narrow_type))
              in
              let env_cons, consequence_stmt =
                match narrowing with
                | Some (path, current_type, narrow_type) when Type_narrowing.is_identifier_path path ->
                    (narrow_identifier_in_env env' path.root current_type narrow_type, consequence)
                | Some (path, current_type, narrow_type) ->
                    let narrowed_type =
                      match Type_narrowing.compute_narrowed_type current_type narrow_type with
                      | Some narrowed -> narrowed
                      | None -> narrow_type
                    in
                    let narrowed_name = Type_narrowing.temp_name path "typed" in
                    ( TypeEnv.add narrowed_name (Forall ([], narrowed_type)) env',
                      substitute_narrowed_path_in_stmt path narrowed_name consequence )
                | None -> (env', consequence)
              in
              match
                infer_statement_against_expected type_map env_cons consequence_stmt
                  (apply_substitution subst expected_type)
              with
              | Error e -> Error e
              | Ok (subst3, _cons_type) -> (
                  let subst' = compose_substitution subst subst3 in
                  let env'' = apply_substitution_env subst' env' in
                  let env_alt, alternative_stmt =
                    match narrowing with
                    | Some (path, current_type, narrow_type) when Type_narrowing.is_identifier_path path ->
                        ( narrow_identifier_to_complement env'' path.root
                            (apply_substitution subst' current_type)
                            (apply_substitution subst' narrow_type),
                          alt )
                    | Some (path, current_type, narrow_type) -> (
                        match
                          Type_narrowing.compute_complement_type
                            (apply_substitution subst' current_type)
                            (apply_substitution subst' narrow_type)
                        with
                        | Some complement_type ->
                            let complement_name = Type_narrowing.temp_name path "complement" in
                            ( TypeEnv.add complement_name (Forall ([], complement_type)) env'',
                              substitute_narrowed_path_in_stmt path complement_name alt )
                        | None -> (env'', alt))
                    | None -> (env'', alt)
                  in
                  match
                    infer_statement_against_expected type_map env_alt alternative_stmt
                      (apply_substitution subst' expected_type)
                  with
                  | Error e -> Error e
                  | Ok (subst4, _alt_type) ->
                      let final_subst = compose_substitution subst' subst4 in
                      Ok (final_subst, apply_substitution final_subst expected_type)))))

and infer_match_arm_against_expected type_map env scrutinee scrutinee_type arm expected_type =
  let scrutinee_path_opt = Type_narrowing.path_of_expr scrutinee in
  match check_patterns arm.AST.patterns scrutinee_type with
  | Error e -> Error e
  | Ok (bindings, narrowed_type) ->
      let narrowed_scrutinee = canonicalize_mono_type narrowed_type <> canonicalize_mono_type scrutinee_type in
      let env', body =
        match (scrutinee_path_opt, narrowed_scrutinee) with
        | Some path, true when Type_narrowing.is_identifier_path path ->
            (TypeEnv.add path.root (mono_to_poly narrowed_type) env, arm.AST.body)
        | Some path, true ->
            let narrowed_name = Type_narrowing.temp_name path "typed" in
            ( TypeEnv.add narrowed_name (mono_to_poly narrowed_type) env,
              Type_narrowing.substitute_path_in_expr path
                ~replace:(Type_narrowing.replacement_identifier narrowed_name)
                arm.AST.body )
        | _ -> (env, arm.AST.body)
      in
      let env' = List.fold_left (fun e (name, ty) -> TypeEnv.add name (mono_to_poly ty) e) env' bindings in
      infer_arg_against_expected type_map env' empty_substitution body expected_type

and infer_match_against_expected type_map env match_expr scrutinee arms expected_type =
  match infer_expression type_map env scrutinee with
  | Error e -> Error e
  | Ok (subst, scrutinee_type) -> (
      let env' = apply_substitution_env subst env in
      match Exhaustiveness.check_exhaustive scrutinee_type arms with
      | Error msg -> Error (error_at ~code:"type-match" ~message:msg match_expr)
      | Ok () ->
          let rec loop subst_acc = function
            | [] -> Ok (subst_acc, apply_substitution subst_acc expected_type)
            | arm :: rest -> (
                match
                  infer_match_arm_against_expected type_map env' scrutinee scrutinee_type arm
                    (apply_substitution subst_acc expected_type)
                with
                | Error e -> Error e
                | Ok (arm_subst, _body_type) -> loop (compose_substitution subst_acc arm_subst) rest)
          in
          loop subst arms)

and infer_enum_constructor_expr
    ?(expected_result_type : mono_type option = None) type_map env expr enum_name variant_name args =
  match Enum_registry.lookup_variant enum_name variant_name with
  | None ->
      Error (error_at ~code:"type-constructor" ~message:(unknown_constructor_message enum_name variant_name) expr)
  | Some variant -> (
      if List.length args <> List.length variant.fields then
        Error
          (error_at ~code:"type-constructor"
             ~message:
               (Printf.sprintf "%s.%s expects %d arguments, got %d" enum_name variant_name
                  (List.length variant.fields) (List.length args))
             expr)
      else
        match Enum_registry.lookup enum_name with
        | None -> Error (error_at ~code:"type-constructor" ~message:(unknown_type_message enum_name) expr)
        | Some enum_def -> (
            match expected_result_type |> Option.map canonicalize_mono_type with
            | Some (TEnum (expected_enum_name, type_args)) when expected_enum_name = enum_name -> (
                let param_subst = substitution_of_list (List.combine enum_def.type_params type_args) in
                let expected_types = List.map (apply_substitution param_subst) variant.fields in
                match infer_args_against_expected type_map env empty_substitution args expected_types with
                | Error e -> Error e
                | Ok (subst, _arg_types) -> Ok (subst, apply_substitution subst (TEnum (enum_name, type_args))))
            | _ -> (
                match infer_args type_map env empty_substitution args with
                | Error e -> Error e
                | Ok (subst, arg_types) -> (
                    let fresh_vars = List.map (fun _ -> fresh_type_var ()) enum_def.type_params in
                    let param_subst = substitution_of_list (List.combine enum_def.type_params fresh_vars) in
                    let expected_types = List.map (apply_substitution param_subst) variant.fields in
                    let arg_types' = List.map (apply_substitution subst) arg_types in
                    let rec unify_all subst_acc types1 types2 =
                      match (types1, types2) with
                      | [], [] -> Ok subst_acc
                      | t1 :: rest1, t2 :: rest2 -> (
                          let t1' = apply_substitution subst_acc t1 in
                          let t2' = apply_substitution subst_acc t2 in
                          match unify t1' t2' with
                          | Error e -> Error (error_at ~code:e.code ~message:e.message expr)
                          | Ok subst2 ->
                              let new_subst = compose_substitution subst_acc subst2 in
                              unify_all new_subst rest1 rest2)
                      | _ -> Error (error_at ~code:"type-constructor" ~message:"Argument count mismatch" expr)
                    in
                    match unify_all empty_substitution arg_types' expected_types with
                    | Error e -> Error e
                    | Ok subst2 ->
                        let final_subst = compose_substitution subst subst2 in
                        let result_type_args = List.map (apply_substitution final_subst) fresh_vars in
                        Ok (final_subst, TEnum (enum_name, result_type_args))))))

and infer_named_type_constructor_call_against_expected
    (type_map : type_map)
    (env : type_env)
    (call_expr : AST.expression)
    (type_name : string)
    (args : AST.expression list)
    (expected_type : mono_type) : (substitution * mono_type) infer_result =
  match canonicalize_mono_type expected_type with
  | TNamed (expected_name, type_args) when expected_name = type_name -> (
      match Type_registry.lookup_named_type type_name with
      | None ->
          Error
            (error_at ~code:"type-constructor"
               ~message:(Printf.sprintf "Unknown named type constructor: %s" type_name)
               call_expr)
      | Some named_type_def -> (
          let expect_single_argument (expected_arg_type : mono_type) =
            if List.length args <> 1 then
              Error
                (error_at ~code:"type-constructor"
                   ~message:
                     (Printf.sprintf "Named type constructor %s expects 1 argument, got %d" type_name
                        (List.length args))
                   call_expr)
            else
              match infer_args_against_expected type_map env empty_substitution args [ expected_arg_type ] with
              | Error e -> Error e
              | Ok (subst, _arg_types) -> Ok (subst, apply_substitution subst expected_type)
          in
          match named_type_def.named_type_body with
          | Type_registry.NamedProduct _ -> (
              match Type_registry.instantiate_named_product_fields type_name type_args with
              | Some (Ok fields) -> expect_single_argument (TRecord (fields, None))
              | Some (Error msg) -> Error (error_at ~code:"type-constructor" ~message:msg call_expr)
              | None ->
                  Error
                    (error_at ~code:"type-constructor"
                       ~message:(Printf.sprintf "Unknown named product constructor: %s" type_name)
                       call_expr))
          | Type_registry.NamedWrapper _ -> (
              match Type_registry.instantiate_named_wrapper_representation type_name type_args with
              | Some (Ok representation_type) -> expect_single_argument representation_type
              | Some (Error msg) -> Error (error_at ~code:"type-constructor" ~message:msg call_expr)
              | None ->
                  Error
                    (error_at ~code:"type-constructor"
                       ~message:(Printf.sprintf "Unknown named wrapper constructor: %s" type_name)
                       call_expr))))
  | _ -> infer_named_type_constructor_call type_map env call_expr type_name args

and infer_expression_against_expected type_map env (expr : AST.expression) expected_type =
  let result =
    match expr.expr with
    | AST.Function { origin; generics; params; return_type; is_effectful; body } -> (
        match contextual_callable_signature_for_arity (List.length params) expected_type with
        | Some signature ->
            infer_function_literal ~known_signature:signature type_map env expr ~origin ~generics ~params
              ~return_type ~is_effectful ~body
        | None ->
            infer_function_literal type_map env expr ~origin ~generics ~params ~return_type ~is_effectful ~body)
    | AST.If (condition, consequence, alternative) ->
        infer_if_against_expected type_map env condition consequence alternative expected_type
    | AST.Match (scrutinee, arms) -> infer_match_against_expected type_map env expr scrutinee arms expected_type
    | AST.BlockExpr stmts -> infer_block_against_expected type_map env stmts expected_type
    | AST.EnumConstructor (enum_name, variant_name, args) ->
        infer_enum_constructor_expr ~expected_result_type:(Some expected_type) type_map env expr enum_name
          variant_name args
    | AST.MethodCall
        { mc_receiver = { expr = AST.Identifier name; _ }; mc_method = method_name; mc_args = args; _ } -> (
        match classify_dotted_receiver env name method_name with
        | `EnumVariant ->
            infer_enum_constructor_expr ~expected_result_type:(Some expected_type) type_map env expr name
              method_name args
        | _ -> infer_expression type_map env expr)
    | AST.Call ({ expr = AST.Identifier type_name; _ }, args)
      when (not (TypeEnv.mem type_name env)) && Type_registry.is_named_type_name type_name ->
        infer_named_type_constructor_call_against_expected type_map env expr type_name args expected_type
    | _ -> infer_expression type_map env expr
  in
  match result with
  | Ok (subst, t) ->
      let t' = apply_substitution subst t in
      record_type type_map expr t';
      Ok (subst, t)
  | Error e -> Error e

and infer_arg_against_expected type_map env subst (arg : AST.expression) (expected_type : mono_type) :
    (substitution * mono_type) infer_result =
  let expected_type' = apply_substitution subst expected_type in
  let placeholder_count = placeholder_count_in_current_context arg in
  let placeholder_expectation = placeholder_callback_expectation expected_type' in
  if placeholder_count > 1 then
    Error (placeholder_count_error arg placeholder_count)
  else
    match (placeholder_count, placeholder_expectation) with
    | 1, PlaceholderCallbackEffectfulOnly -> Error (placeholder_effectful_error arg)
    | _ -> (
        let inferred_arg =
          match (placeholder_count, placeholder_expectation) with
          | 1, PlaceholderCallbackPureAllowed ->
              let rewritten_arg = placeholder_lambda_expr arg in
              record_placeholder_rewrite arg rewritten_arg;
              rewritten_arg
          | _ -> arg
        in
        let env' = apply_substitution_env subst env in
        let infer_arg_expr () = infer_expression_against_expected type_map env' inferred_arg expected_type' in
        match infer_arg_expr () with
        | Error e -> Error e
        | Ok (subst1, arg_type) -> (
            let subst' = compose_substitution subst subst1 in
            let expected_type'' = apply_substitution subst' expected_type in
            let arg_type' = apply_substitution subst' arg_type in
            if
              placeholder_count = 1
              && placeholder_expectation = PlaceholderCallbackPureAllowed
              && placeholder_callable_is_effectful arg_type'
            then
              Error (placeholder_effectful_error arg)
            else
              match canonicalize_mono_type expected_type'' with
              | TTraitObject _ -> (
                  match record_expected_trait_object_coercions type_map arg expected_type'' with
                  | Error e -> Error e
                  | Ok () ->
                      propagate_type_var_constraints_through_substitution subst';
                      Ok (subst' (* coercion is metadata-only *), expected_type''))
              | _ -> (
                  match compatible_with_expected_type arg_type' expected_type'' with
                  | Error e -> Error (error_at ~code:e.code ~message:e.message arg)
                  | Ok subst2 -> (
                      let final_subst = compose_substitution subst' subst2 in
                      let final_type = apply_substitution final_subst arg_type in
                      match record_expected_trait_object_coercions type_map arg expected_type'' with
                      | Error e -> Error e
                      | Ok () ->
                          propagate_type_var_constraints_through_substitution final_subst;
                          Ok (final_subst, final_type)))))

and infer_args_against_expected type_map env subst args expected_types =
  match (args, expected_types) with
  | [], [] -> Ok (subst, [])
  | arg :: rest_args, expected :: rest_expected -> (
      match infer_arg_against_expected type_map env subst arg expected with
      | Error e -> Error e
      | Ok (subst', arg_type) -> (
          match infer_args_against_expected type_map env subst' rest_args rest_expected with
          | Error e -> Error e
          | Ok (subst'', rest_types) -> Ok (subst'', arg_type :: rest_types)))
  | _ -> Error (error ~code:"type-constructor" ~message:"Argument count mismatch")

(* ============================================================
   Function Calls
   ============================================================ *)

and infer_named_type_constructor_call
    (type_map : type_map)
    (env : type_env)
    (call_expr : AST.expression)
    (type_name : string)
    (args : AST.expression list) : (substitution * mono_type) infer_result =
  match Type_registry.lookup_named_type type_name with
  | None ->
      Error
        (error_at ~code:"type-constructor"
           ~message:(Printf.sprintf "Unknown named type constructor: %s" type_name)
           call_expr)
  | Some named_type_def -> (
      let type_args = List.map (fun _ -> fresh_type_var ()) named_type_def.named_type_params in
      let result_type = TNamed (type_name, type_args) in
      let expect_single_argument (expected_arg_type : mono_type) =
        if List.length args <> 1 then
          Error
            (error_at ~code:"type-constructor"
               ~message:
                 (Printf.sprintf "Named type constructor %s expects 1 argument, got %d" type_name
                    (List.length args))
               call_expr)
        else
          match infer_args_against_expected type_map env empty_substitution args [ expected_arg_type ] with
          | Error e -> Error e
          | Ok (subst, _arg_types) -> Ok (subst, apply_substitution subst result_type)
      in
      match named_type_def.named_type_body with
      | Type_registry.NamedProduct _ -> (
          match Type_registry.instantiate_named_product_fields type_name type_args with
          | Some (Ok fields) -> expect_single_argument (TRecord (fields, None))
          | Some (Error msg) -> Error (error_at ~code:"type-constructor" ~message:msg call_expr)
          | None ->
              Error
                (error_at ~code:"type-constructor"
                   ~message:(Printf.sprintf "Unknown named product constructor: %s" type_name)
                   call_expr))
      | Type_registry.NamedWrapper _ -> (
          match Type_registry.instantiate_named_wrapper_representation type_name type_args with
          | Some (Ok representation_type) -> expect_single_argument representation_type
          | Some (Error msg) -> Error (error_at ~code:"type-constructor" ~message:msg call_expr)
          | None ->
              Error
                (error_at ~code:"type-constructor"
                   ~message:(Printf.sprintf "Unknown named wrapper constructor: %s" type_name)
                   call_expr)))

and infer_call type_map env (call_expr : AST.expression) func args =
  match func.expr with
  | AST.Identifier type_name when (not (TypeEnv.mem type_name env)) && Type_registry.is_named_type_name type_name
    ->
      infer_named_type_constructor_call type_map env call_expr type_name args
  | _ -> (
      (* Infer function type *)
      match infer_expression type_map env func with
      | Error e -> Error e
      | Ok (subst1, func_type) -> (
          let func_type' = apply_substitution subst1 func_type in
          let expected_param_types = List.map (fun _ -> fresh_type_var ()) args in
          (* Create expected function type: arg1 -> arg2 -> ... -> result.
         For unknown/union callees, first try an effect-polymorphic callable
         (pure | effectful) so higher-order callbacks remain flexible.
         Fall back to a directional pure-then-effectful probe. *)
          let fresh_result_type () = fresh_type_var () in
          let expected_func_type_for is_effectful result_type =
            List.fold_right (fun arg_t acc -> TFun (arg_t, acc, is_effectful)) expected_param_types result_type
          in
          let try_effect_polymorphic_callable () =
            let result_type = fresh_result_type () in
            let expected_pure = expected_func_type_for false result_type in
            let expected_effectful = expected_func_type_for true result_type in
            let expected_union = Types.normalize_union [ expected_pure; expected_effectful ] in
            match unify func_type' expected_union with
            | Ok subst2 -> Ok (subst2, result_type)
            | Error e -> Error e
          in
          let try_directional_purity_probe () =
            let result_type_pure = fresh_result_type () in
            let expected_pure = expected_func_type_for false result_type_pure in
            match unify func_type' expected_pure with
            | Ok subst2 -> Ok (subst2, result_type_pure)
            | Error pure_err -> (
                let result_type_eff = fresh_result_type () in
                let expected_eff = expected_func_type_for true result_type_eff in
                match unify func_type' expected_eff with
                | Ok subst2 -> Ok (subst2, result_type_eff)
                | Error _ -> Error pure_err)
          in
          let call_result =
            match func_type' with
            | TVar _ -> (
                match try_effect_polymorphic_callable () with
                | Ok _ as ok -> ok
                | Error _ -> try_directional_purity_probe ())
            | TUnion members -> (
                let result_type = fresh_result_type () in
                let expected_pure = expected_func_type_for false result_type in
                let expected_effectful = expected_func_type_for true result_type in
                let expected_union = Types.normalize_union [ expected_pure; expected_effectful ] in
                match Unify.unify_union_all_with_concrete members expected_union with
                | Ok subst2 -> Ok (subst2, result_type)
                | Error e -> Error e)
            | _ -> try_directional_purity_probe ()
          in
          match call_result with
          | Error e -> Error (error_at ~code:e.code ~message:e.message call_expr)
          | Ok (subst2, result_type) -> (
              let subst' = compose_substitution subst1 subst2 in
              match infer_args_against_expected type_map env subst' args expected_param_types with
              | Error e -> Error e
              | Ok (final_subst, _arg_types) -> (
                  (* Phase 4.3+: Verify trait constraints are satisfied.
                 When calling a constrained generic function like fn[a: show](x: a),
                 we need to check that the actual argument type implements the required traits. *)
                  match verify_constraints_in_substitution final_subst with
                  | Error diag -> Error (error_at ~code:diag.code ~message:diag.message call_expr)
                  | Ok () ->
                      let final_result = apply_substitution final_subst result_type in
                      Ok (final_subst, final_result)))))

and infer_type_apply _type_map env (type_apply_expr : AST.expression) func type_args =
  let mk_error ~code ~message = error_at ~code ~message type_apply_expr in
  match func.expr with
  | AST.Identifier name -> (
      match TypeEnv.find_opt name env with
      | None -> Error (mk_error ~code:"type-unbound-var" ~message:("Unbound variable: " ^ name))
      | Some poly_type -> (
          match instantiate_with_explicit_type_args ~mk_error poly_type type_args with
          | Error e -> Error e
          | Ok specialized_type -> Ok (empty_substitution, specialized_type)))
  | _ ->
      Error
        (mk_error ~code:"type-constructor"
           ~message:"Explicit type arguments are only supported on named polymorphic values")

(* Helper to infer types of a list of arguments *)
and infer_args type_map env subst args =
  match args with
  | [] -> Ok (subst, [])
  | arg :: rest -> (
      match infer_expression type_map env arg with
      | Error e -> Error e
      | Ok (subst1, arg_type) -> (
          let subst' = compose_substitution subst subst1 in
          let env' = apply_substitution_env subst1 env in
          match infer_args type_map env' subst' rest with
          | Error e -> Error e
          | Ok (subst'', rest_types) ->
              let arg_type' = apply_substitution subst'' arg_type in
              Ok (subst'', arg_type' :: rest_types)))

(* ============================================================
   Arrays
   ============================================================ *)

and infer_array type_map env elements =
  match elements with
  | [] ->
      (* Empty array - create fresh type variable for element type *)
      let elem_type = fresh_type_var () in
      Ok (empty_substitution, TArray elem_type)
  | first :: rest -> (
      (* Infer first element type *)
      match infer_expression type_map env first with
      | Error e -> Error e
      | Ok (subst1, first_type) ->
          (* All other elements must have same type *)
          infer_array_elements type_map (apply_substitution_env subst1 env) subst1 first_type rest)

and infer_array_elements type_map env subst elem_type elements =
  match elements with
  | [] -> Ok (subst, TArray elem_type)
  | elem :: rest -> (
      match infer_expression type_map env elem with
      | Error e -> Error e
      | Ok (subst1, this_type) -> (
          let subst' = compose_substitution subst subst1 in
          let elem_type' = apply_substitution subst1 elem_type in
          match unify elem_type' this_type with
          | Error _ ->
              Error
                (error_at ~code:"type-array-element"
                   ~message:
                     ("Array element type mismatch: expected "
                     ^ to_string elem_type'
                     ^ ", got "
                     ^ to_string this_type)
                   elem)
          | Ok subst2 ->
              let subst'' = compose_substitution subst' subst2 in
              let elem_type'' = apply_substitution subst2 elem_type' in
              infer_array_elements type_map (apply_substitution_env subst2 env) subst'' elem_type'' rest))

(* ============================================================
   Hashes
   ============================================================ *)

and infer_hash type_map env pairs =
  match pairs with
  | [] ->
      (* Empty hash - create fresh type variables *)
      let key_type = fresh_type_var () in
      let val_type = fresh_type_var () in
      Ok (empty_substitution, THash (key_type, val_type))
  | (first_key, first_val) :: rest -> (
      match infer_expression type_map env first_key with
      | Error e -> Error e
      | Ok (subst1, key_type) -> (
          let env' = apply_substitution_env subst1 env in
          match infer_expression type_map env' first_val with
          | Error e -> Error e
          | Ok (subst2, val_type) ->
              let subst = compose_substitution subst1 subst2 in
              let key_type' = apply_substitution subst2 key_type in
              infer_hash_pairs type_map (apply_substitution_env subst2 env') subst key_type' val_type rest))

and infer_hash_pairs type_map env subst key_type val_type pairs =
  match pairs with
  | [] -> Ok (subst, THash (key_type, val_type))
  | (k, v) :: rest -> (
      match infer_expression type_map env k with
      | Error e -> Error e
      | Ok (subst1, this_key_type) -> (
          let subst' = compose_substitution subst subst1 in
          let key_type' = apply_substitution subst1 key_type in
          match unify key_type' this_key_type with
          | Error _ ->
              Error
                (error_at ~code:"type-hash-key"
                   ~message:
                     ("Hash key type mismatch: expected "
                     ^ to_string key_type'
                     ^ ", got "
                     ^ to_string this_key_type)
                   k)
          | Ok subst2 -> (
              let subst'' = compose_substitution subst' subst2 in
              let env' = apply_substitution_env subst2 env in
              match infer_expression type_map env' v with
              | Error e -> Error e
              | Ok (subst3, this_val_type) -> (
                  let subst''' = compose_substitution subst'' subst3 in
                  let val_type' = apply_substitution (compose_substitution subst2 subst3) val_type in
                  match unify val_type' this_val_type with
                  | Error _ ->
                      Error
                        (error_at ~code:"type-hash-value"
                           ~message:
                             ("Hash value type mismatch: expected "
                             ^ to_string val_type'
                             ^ ", got "
                             ^ to_string this_val_type)
                           v)
                  | Ok subst4 ->
                      let final_subst = compose_substitution subst''' subst4 in
                      let key_type'' = apply_substitution (compose_substitution subst3 subst4) key_type' in
                      let val_type'' = apply_substitution subst4 val_type' in
                      infer_hash_pairs type_map (apply_substitution_env subst4 env') final_subst key_type''
                        val_type'' rest))))

(* ============================================================
   Records
   ============================================================ *)

(* Shared receiver classifier for FieldAccess and MethodCall handlers.
   Determines what kind of entity a dotted-access receiver identifier refers to,
   using a unified priority: bound variable > enum > transparent type > trait > unknown. *)
and classify_dotted_receiver (env : type_env) (name : string) (member_name : string) :
    [ `BoundVar | `EnumVariant | `EnumType | `TypeName of mono_type | `TraitName | `Unknown ] =
  if TypeEnv.mem name env then
    `BoundVar
  else
    let enum_def_opt = Annotation.lookup_enum_by_source_name name in
    let is_enum = Option.is_some enum_def_opt in
    let is_enum_variant =
      match enum_def_opt with
      | Some enum_def -> Enum_registry.lookup_variant enum_def.name member_name <> None
      | None -> false
    in
    if is_enum_variant then
      `EnumVariant
    else if is_enum then
      `EnumType
    else
      match Annotation.builtin_primitive_type name with
      | Some primitive -> `TypeName primitive
      | None when Type_registry.is_named_type_name name -> (
          match Type_registry.named_type_arity name with
          | Some arity -> `TypeName (TNamed (name, List.init arity (fun _ -> fresh_type_var ())))
          | None -> `Unknown)
      | None -> (
          match Annotation.lookup_type_alias name with
          | Some alias_info when alias_info.alias_type_params = [] -> (
              match Annotation.type_expr_to_mono_type alias_info.alias_body with
              | Ok mono -> `TypeName mono
              | Error _ -> `Unknown)
          | _ -> (
              match Annotation.lookup_enum_by_source_name name with
              | Some enum_def ->
                  let fresh_args = List.map (fun _ -> fresh_type_var ()) enum_def.type_params in
                  `TypeName (TEnum (enum_def.name, fresh_args))
              | None -> (
                  match Trait_registry.lookup_trait name with
                  | Some _ -> `TraitName
                  | None -> `Unknown)))

and infer_record_literal type_map env fields spread expr =
  let rec infer_record_fields env_acc subst_acc typed_fields = function
    | [] -> Ok (env_acc, subst_acc, List.rev typed_fields)
    | (idx, field : int * AST.record_field) :: rest -> (
        let field_expr =
          match field.field_value with
          | Some e -> e
          | None ->
              let synthetic_id = -((expr.id lsl 8) + idx + 1) in
              AST.mk_expr ~id:synthetic_id ~pos:expr.pos ~end_pos:expr.end_pos ~file_id:expr.file_id
                (AST.Identifier field.field_name)
        in
        match infer_expression type_map env_acc field_expr with
        | Error e -> Error e
        | Ok (subst1, field_type) ->
            let env' = apply_substitution_env subst1 env_acc in
            let subst' = compose_substitution subst_acc subst1 in
            let field_type' = apply_substitution subst' field_type in
            let typed_field = { Types.name = field.field_name; typ = field_type' } in
            infer_record_fields env' subst' (typed_field :: typed_fields) rest)
  in
  let dedupe_fields_last_wins (field_types : Types.record_field_type list) : Types.record_field_type list =
    let tbl = Hashtbl.create 16 in
    List.iter (fun (f : Types.record_field_type) -> Hashtbl.replace tbl f.name f.typ) field_types;
    field_types
    |> List.filter (fun (f : Types.record_field_type) -> Hashtbl.mem tbl f.name)
    |> List.filter_map (fun (f : Types.record_field_type) ->
           match Hashtbl.find_opt tbl f.name with
           | None -> None
           | Some typ ->
               Hashtbl.remove tbl f.name;
               Some { Types.name = f.name; typ })
  in
  let remove_override_names_from_row (override_names : string list) (row : Types.mono_type option) :
      Types.mono_type option =
    let rec go = function
      | None -> None
      | Some (TRecord (row_fields, row_tail)) ->
          let kept_fields =
            List.filter (fun (f : Types.record_field_type) -> not (List.mem f.name override_names)) row_fields
          in
          let kept_tail = go row_tail in
          if kept_fields = [] then
            kept_tail
          else
            Some (Types.canonicalize_mono_type (TRecord (kept_fields, kept_tail)))
      | other -> other
    in
    go row
  in
  match infer_record_fields env empty_substitution [] (List.mapi (fun idx field -> (idx, field)) fields) with
  | Error e -> Error e
  | Ok (env1, subst1, field_types) -> (
      let field_types = dedupe_fields_last_wins field_types in
      let merge_fields (base_fields : Types.record_field_type list) (new_fields : Types.record_field_type list) =
        let tbl = Hashtbl.create 16 in
        List.iter (fun (f : Types.record_field_type) -> Hashtbl.replace tbl f.name f.typ) base_fields;
        List.iter (fun (f : Types.record_field_type) -> Hashtbl.replace tbl f.name f.typ) new_fields;
        let names_in_order =
          List.map (fun (f : Types.record_field_type) -> f.name) base_fields
          @ List.map (fun (f : Types.record_field_type) -> f.name) new_fields
        in
        let seen = Hashtbl.create 16 in
        List.filter_map
          (fun name ->
            if Hashtbl.mem seen name then
              None
            else (
              Hashtbl.add seen name ();
              match Hashtbl.find_opt tbl name with
              | None -> None
              | Some typ -> Some { Types.name; typ }))
          names_in_order
      in
      match spread with
      | None -> Ok (subst1, Types.canonicalize_mono_type (TRecord (field_types, None)))
      | Some spread_expr -> (
          match infer_expression type_map env1 spread_expr with
          | Error e -> Error e
          | Ok (subst2, spread_type) -> (
              let subst = compose_substitution subst1 subst2 in
              let spread_type' = apply_substitution subst spread_type in
              match spread_type' with
              | TRecord (base_fields, base_row) ->
                  let merged = merge_fields base_fields field_types in
                  let override_names = List.map (fun (f : Types.record_field_type) -> f.name) field_types in
                  let pruned_row = remove_override_names_from_row override_names base_row in
                  Ok (subst, Types.canonicalize_mono_type (TRecord (merged, pruned_row)))
              | TIntersection members
                when List.for_all
                       (function
                         | TRecord (_, None) -> true
                         | _ -> false)
                       members -> (
                  match Annotation.merged_record_intersection_type members with
                  | Error diag -> Error (error_at ~code:diag.code ~message:diag.message spread_expr)
                  | Ok (TRecord (base_fields, base_row)) ->
                      let merged = merge_fields base_fields field_types in
                      let override_names = List.map (fun (f : Types.record_field_type) -> f.name) field_types in
                      let pruned_row = remove_override_names_from_row override_names base_row in
                      Ok (subst, Types.canonicalize_mono_type (TRecord (merged, pruned_row)))
                  | Ok other ->
                      Error
                        (error_at ~code:"type-constructor"
                           ~message:(Printf.sprintf "Record spread expects a record, got %s" (to_string other))
                           spread_expr))
              | TVar _ -> (
                  let row_var = fresh_row_var () in
                  let expected_base = TRecord ([], Some row_var) in
                  match unify spread_type' expected_base with
                  | Error e -> Error (error_at ~code:e.code ~message:e.message spread_expr)
                  | Ok subst3 ->
                      let final_subst = compose_substitution subst subst3 in
                      let result_row = Some (apply_substitution subst3 row_var) in
                      Ok (final_subst, Types.canonicalize_mono_type (TRecord (field_types, result_row))))
              | TNamed (name, type_args) -> (
                  match Type_registry.instantiate_named_product_fields name type_args with
                  | Some (Ok base_fields) ->
                      let merged = merge_fields base_fields field_types in
                      Ok (subst, Types.canonicalize_mono_type (TRecord (merged, None)))
                  | Some (Error msg) -> Error (error_at ~code:"type-constructor" ~message:msg spread_expr)
                  | None ->
                      Error
                        (error_at ~code:"type-constructor"
                           ~message:
                             (Printf.sprintf "Record spread expects a record, got %s" (to_string spread_type'))
                           spread_expr))
              | _ ->
                  Error
                    (error_at ~code:"type-constructor"
                       ~message:(Printf.sprintf "Record spread expects a record, got %s" (to_string spread_type'))
                       spread_expr))))

(* ============================================================
   Index Expressions
   ============================================================ *)

and infer_index type_map env container index_expr =
  match infer_expression type_map env container with
  | Error e -> Error e
  | Ok (subst1, container_type) -> (
      let env' = apply_substitution_env subst1 env in
      match infer_expression type_map env' index_expr with
      | Error e -> Error e
      | Ok (subst2, index_type) -> (
          let subst = compose_substitution subst1 subst2 in
          let container_type' = apply_substitution subst2 container_type in
          (* Determine what we're indexing into *)
          match container_type' with
          | TArray elem_type -> (
              (* Array index must be Int *)
              match unify index_type TInt with
              | Error _ ->
                  Error
                    (error_at ~code:"type-index-mismatch"
                       ~message:
                         ("Index type mismatch: expected " ^ to_string TInt ^ ", got " ^ to_string index_type)
                       index_expr)
              | Ok subst3 ->
                  let final_subst = compose_substitution subst subst3 in
                  let elem_type' = apply_substitution subst3 elem_type in
                  Ok (final_subst, elem_type'))
          | THash (key_type, val_type) -> (
              (* Hash index must match key type *)
              match unify index_type key_type with
              | Error _ ->
                  Error
                    (error_at ~code:"type-index-mismatch"
                       ~message:
                         ("Index type mismatch: expected " ^ to_string key_type ^ ", got " ^ to_string index_type)
                       index_expr)
              | Ok subst3 ->
                  let final_subst = compose_substitution subst subst3 in
                  let val_type' = apply_substitution subst3 val_type in
                  Ok (final_subst, val_type'))
          | TString -> (
              (* String index must be Int, returns String *)
              match unify index_type TInt with
              | Error _ ->
                  Error
                    (error_at ~code:"type-index-mismatch"
                       ~message:
                         ("Index type mismatch: expected " ^ to_string TInt ^ ", got " ^ to_string index_type)
                       index_expr)
              | Ok subst3 -> Ok (compose_substitution subst subst3, TString))
          | TVar _ -> (
              (* Unknown container type - could be array or hash *)
              (* Infer the container type based on the index type *)
              match index_type with
              | TInt -> (
                  (* Int index -> assume array *)
                  let elem_type = fresh_type_var () in
                  match unify container_type' (TArray elem_type) with
                  | Error e -> Error (error_at ~code:e.code ~message:e.message container)
                  | Ok subst3 ->
                      let final_subst = compose_substitution subst subst3 in
                      let elem_type' = apply_substitution subst3 elem_type in
                      Ok (final_subst, elem_type'))
              | TString | TFloat | TBool | TNull | TArray _ | THash _ | TRecord _ | TRowVar _ | TTraitObject _
              | TFun _ | TUnion _ | TIntersection _ | TEnum _ | TNamed _ -> (
                  (* Non-int index -> assume hash *)
                  let val_type = fresh_type_var () in
                  match unify container_type' (THash (index_type, val_type)) with
                  | Error e -> Error (error_at ~code:e.code ~message:e.message container)
                  | Ok subst3 ->
                      let final_subst = compose_substitution subst subst3 in
                      let val_type' = apply_substitution subst3 val_type in
                      Ok (final_subst, val_type'))
              | TVar _ -> (
                  (* Index is also unknown - assume array with Int index *)
                  let elem_type = fresh_type_var () in
                  match unify container_type' (TArray elem_type) with
                  | Error e -> Error (error_at ~code:e.code ~message:e.message container)
                  | Ok subst3 -> (
                      match unify index_type TInt with
                      | Error _ ->
                          Error
                            (error_at ~code:"type-index-mismatch"
                               ~message:
                                 ("Index type mismatch: expected "
                                 ^ to_string TInt
                                 ^ ", got "
                                 ^ to_string index_type)
                               index_expr)
                      | Ok subst4 ->
                          let final_subst = compose_substitution subst (compose_substitution subst3 subst4) in
                          let elem_type' = apply_substitution (compose_substitution subst3 subst4) elem_type in
                          Ok (final_subst, elem_type'))))
          | _ ->
              Error
                (error_at ~code:"type-not-indexable"
                   ~message:("Cannot index into type: " ^ to_string container_type')
                   container)))

(* ============================================================
   Statements
   ============================================================ *)

and infer_statement type_map env stmt =
  match stmt.stmt with
  | AST.ExportDecl _ | AST.ImportDecl _ -> Ok (empty_substitution, TNull)
  | AST.ExpressionStmt expr -> infer_expression type_map env expr
  | AST.Return expr -> infer_expression type_map env expr
  | AST.Block stmts -> infer_block type_map env stmts
  | AST.Let let_binding -> infer_let type_map env let_binding.name let_binding.value let_binding.type_annotation
  | AST.TypeDef { type_name; type_type_params; type_body } -> (
      let type_bindings = List.map (fun name -> (name, TVar name)) type_type_params in
      let convert_type_expr (te : AST.type_expr) : (mono_type, Diagnostic.t) result =
        Annotation.type_expr_to_mono_type_with type_bindings te
      in
      match type_body with
      | AST.NamedTypeProduct fields ->
          let* field_types =
            map_result
              (fun (f : AST.record_type_field) ->
                let* typ = convert_type_expr f.field_type in
                Ok { Types.name = f.field_name; typ })
              fields
          in
          Type_registry.register_named_type
            {
              Type_registry.named_type_name = type_name;
              named_type_params = type_type_params;
              named_type_body = Type_registry.NamedProduct field_types;
            };
          Ok (empty_substitution, TNull)
      | AST.NamedTypeWrapper wrapper_body -> (
          let* wrapper_type = convert_type_expr wrapper_body in
          match canonicalize_mono_type wrapper_type with
          | TUnion _ ->
              Error
                (error_at_stmt ~code:"type-invalid-wrapper"
                   ~message:
                     (Printf.sprintf
                        "Named type '%s' cannot wrap a raw structural union in this milestone; use a transparent type for the union or a named sum type instead"
                        type_name)
                   stmt)
          | wrapper_type' ->
              Type_registry.register_named_type
                {
                  Type_registry.named_type_name = type_name;
                  named_type_params = type_type_params;
                  named_type_body = Type_registry.NamedWrapper wrapper_type';
                };
              Ok (empty_substitution, TNull)))
  | AST.ShapeDef { shape_name; shape_type_params; shape_fields } ->
      let type_bindings = List.map (fun name -> (name, TVar name)) shape_type_params in
      let* field_types =
        map_result
          (fun (f : AST.record_type_field) ->
            let* typ = Annotation.type_expr_to_mono_type_with type_bindings f.field_type in
            Ok { Types.name = f.field_name; typ })
          shape_fields
      in
      Type_registry.register_shape { Type_registry.shape_name; shape_type_params; shape_fields = field_types };
      Ok (empty_substitution, TNull)
  | AST.EnumDef { name; type_params; variants } -> (
      (* Predeclare the type name so recursive constructor payloads can resolve. *)
      Enum_registry.register { Enum_registry.name; type_params; variants = [] };
      let type_bindings = List.map (fun type_param -> (type_param, TVar type_param)) type_params in
      let convert_type_expr (te : AST.type_expr) : (mono_type, Diagnostic.t) result =
        match Annotation.type_expr_to_mono_type_with type_bindings te with
        | Ok typ -> Ok typ
        | Error diag -> Error (error ~code:diag.code ~message:diag.message)
      in

      let variant_defs_result =
        map_result
          (fun (v : AST.variant_def) ->
            let* field_types = map_result convert_type_expr v.variant_fields in
            Ok { Enum_registry.name = v.variant_name; fields = field_types })
          variants
      in
      match variant_defs_result with
      | Error e -> Error e
      | Ok variant_defs ->
          Enum_registry.register { Enum_registry.name; type_params; variants = variant_defs };
          Ok (empty_substitution, TNull))
  | AST.TraitDef { name; type_param; supertraits; methods } -> (
      (* Convert AST method signatures to trait registry method signatures *)
      (* We need to treat type_param as a type variable, not a type constructor *)
      let convert_type_expr (te : AST.type_expr) : (mono_type, Diagnostic.t) result =
        let is_trait_type_param (type_name : string) : bool =
          match type_param with
          | Some tp -> type_name = tp
          | None -> false
        in
        let trait_err msg = Error (error ~code:"type-constructor" ~message:msg) in
        let rec convert = function
          | AST.TVar v -> Ok (TVar v)
          | AST.TCon c ->
              if is_trait_type_param c then
                Ok (TVar c)
              else
                Annotation.type_expr_to_mono_type (AST.TCon c)
          | AST.TApp (con_name, args) -> (
              if is_trait_type_param con_name then
                trait_err (Printf.sprintf "Type parameter '%s' cannot be used as type constructor" con_name)
              else
                match con_name with
                | "list" -> (
                    match args with
                    | [ elem ] ->
                        let* t = convert elem in
                        Ok (TArray t)
                    | _ -> trait_err "list expects 1 argument")
                | "map" -> (
                    match args with
                    | [ k; v ] ->
                        let* kt = convert k in
                        let* vt = convert v in
                        Ok (THash (kt, vt))
                    | _ -> trait_err "map expects 2 arguments")
                | _ -> Annotation.type_expr_to_mono_type (AST.TApp (con_name, args)))
          | AST.TArrow (params, ret, is_effectful) ->
              let* param_types = map_result convert params in
              let* ret_type = convert ret in
              let mk_fun arg ret = TFun (arg, ret, is_effectful) in
              Ok (List.fold_right mk_fun param_types ret_type)
          | AST.TTraitObject traits -> Ok (canonicalize_mono_type (TTraitObject traits))
          | AST.TUnion types ->
              let* converted = map_result convert types in
              Ok (normalize_union converted)
          | AST.TIntersection types ->
              let* converted = map_result convert types in
              Annotation.validate_intersection_type converted
          | AST.TRecord (fields, row) ->
              let* field_types =
                map_result
                  (fun (f : AST.record_type_field) ->
                    let* typ = convert f.field_type in
                    Ok { Types.name = f.field_name; typ })
                  fields
              in
              let* row_type =
                match row with
                | None -> Ok None
                | Some r ->
                    let* t = convert r in
                    Ok (Some t)
              in
              Ok (TRecord (field_types, row_type))
        in
        convert te
      in
      let convert_method_header (m : AST.method_sig) :
          ( string list
            * (string * mono_type) list
            * mono_type
            * (string * Constraints.t list) list
            * [ `Pure | `Effectful ],
            Diagnostic.t )
          result =
        (* Build method-level type variable names for recognition during conversion *)
        let method_generic_names =
          match m.method_generics with
          | None -> []
          | Some gps -> List.map (fun (gp : AST.generic_param) -> gp.name) gps
        in
        let convert_method_type_expr (te : AST.type_expr) : (mono_type, Diagnostic.t) result =
          if method_generic_names = [] then
            convert_type_expr te
          else
            let method_bindings = List.map (fun n -> (n, TVar n)) method_generic_names in
            let trait_bindings =
              match type_param with
              | Some tp -> [ (tp, TVar tp) ]
              | None -> []
            in
            Annotation.type_expr_to_mono_type_with (method_bindings @ trait_bindings) te
        in
        let* param_types =
          map_result
            (fun (pname, ptype) ->
              let* t = convert_method_type_expr ptype in
              Ok (pname, t))
            m.method_params
        in
        let* return_type = convert_method_type_expr m.method_return_type in
        let method_generics =
          match m.method_generics with
          | None -> []
          | Some gps ->
              List.map (fun (gp : AST.generic_param) -> (gp.name, Constraints.of_names gp.constraints)) gps
        in
        let method_effect =
          match m.method_effect with
          | AST.Pure -> `Pure
          | AST.Effectful -> `Effectful
        in
        Ok (method_generic_names, param_types, return_type, method_generics, method_effect)
      in
      let convert_method_sig (m : AST.method_sig) : (Trait_registry.method_sig, Diagnostic.t) result =
        let* method_generic_names, param_types, return_type, method_generics, method_effect =
          convert_method_header m
        in
        let* method_generic_internal_vars =
          match m.method_default_impl with
          | None -> Ok []
          | Some default_expr -> (
              let trait_bindings =
                match type_param with
                | Some tp -> [ (tp, TVar tp) ]
                | None -> []
              in
              let method_bindings = List.map (fun n -> (n, TVar n)) method_generic_names in
              let known_param_types = List.mapi (fun i (_name, ty) -> (i, ty)) param_types in
              let body_stmt =
                AST.mk_stmt ~pos:default_expr.pos ~end_pos:default_expr.end_pos ~file_id:default_expr.file_id
                  (AST.Block
                     [
                       AST.mk_stmt ~pos:default_expr.pos ~end_pos:default_expr.end_pos
                         ~file_id:default_expr.file_id (AST.ExpressionStmt default_expr);
                     ])
              in
              let constraint_store = current_constraint_store () in
              let constrained_field_store = current_constrained_field_store () in
              let trait_constraint_snapshot =
                match type_param with
                | Some tp ->
                    let old_constraints = Hashtbl.find_opt constraint_store tp in
                    let old_fields = Hashtbl.find_opt constrained_field_store tp in
                    add_type_var_constraints tp (name :: supertraits);
                    Some (tp, old_constraints, old_fields)
                | None -> None
              in
              let method_constraint_snapshots =
                match m.method_generics with
                | None -> []
                | Some gps ->
                    List.map
                      (fun (gp : AST.generic_param) ->
                        let old_constraints = Hashtbl.find_opt constraint_store gp.name in
                        let old_fields = Hashtbl.find_opt constrained_field_store gp.name in
                        add_type_var_constraints gp.name gp.constraints;
                        (gp.name, old_constraints, old_fields))
                      gps
              in
              let restore_method_constraints () =
                (match trait_constraint_snapshot with
                | None -> ()
                | Some (name, old_constraints_opt, old_fields_opt) -> (
                    (match old_constraints_opt with
                    | Some old_constraints -> Hashtbl.replace constraint_store name old_constraints
                    | None -> Hashtbl.remove constraint_store name);
                    match old_fields_opt with
                    | Some old_fields -> Hashtbl.replace constrained_field_store name old_fields
                    | None -> Hashtbl.remove constrained_field_store name));
                List.iter
                  (fun (name, old_constraints_opt, old_fields_opt) ->
                    (match old_constraints_opt with
                    | Some old_constraints -> Hashtbl.replace constraint_store name old_constraints
                    | None -> Hashtbl.remove constraint_store name);
                    match old_fields_opt with
                    | Some old_fields -> Hashtbl.replace constrained_field_store name old_fields
                    | None -> Hashtbl.remove constrained_field_store name)
                  method_constraint_snapshots
              in
              let infer_default_body =
                Fun.protect ~finally:restore_method_constraints (fun () ->
                    type_callable type_map env ~type_bindings:(method_bindings @ trait_bindings)
                      ~known_param_types
                      ~params:(List.map (fun (name, ty) -> (name, Some ty)) m.method_params)
                      ~return_annot:(Some m.method_return_type) ~known_return:(Some return_type)
                      ~effect_annot:
                        (match m.method_effect with
                        | AST.Pure -> `Pure
                        | AST.Effectful -> `Effectful)
                      ~strict_return_check:true ~body:body_stmt)
              in
              match infer_default_body with
              | Error e -> Error e
              | Ok (param_names, inferred_param_types, inferred_return_type, is_effectful, subst) ->
                  record_method_def m.method_sig_id
                    {
                      Resolution_artifacts.md_param_names = param_names;
                      md_param_types = inferred_param_types;
                      md_return_type = inferred_return_type;
                      md_is_effectful = is_effectful;
                      md_body_id = m.method_sig_id;
                    };
                  Ok
                    (List.filter_map
                       (fun generic_name ->
                         match apply_substitution subst (TVar generic_name) with
                         | TVar internal_name when internal_name <> generic_name ->
                             Some (generic_name, internal_name)
                         | _ -> None)
                       method_generic_names))
        in
        Ok
          {
            Trait_registry.method_key =
              Resolution_artifacts.UserCallable { file_id = None; callable_id = m.method_sig_id };
            method_name = m.method_name;
            method_generics;
            method_params = param_types;
            method_return_type = return_type;
            method_effect;
            method_generic_internal_vars;
            method_default_impl = m.method_default_impl;
          }
      in
      let* provisional_method_sigs =
        map_result
          (fun (m : AST.method_sig) ->
            let* _generic_names, param_types, return_type, method_generics, method_effect =
              convert_method_header m
            in
            Ok
              {
                Trait_registry.method_key =
                  Resolution_artifacts.UserCallable { file_id = None; callable_id = m.method_sig_id };
                method_name = m.method_name;
                method_generics;
                method_params = param_types;
                method_return_type = return_type;
                method_effect;
                method_generic_internal_vars = [];
                method_default_impl = m.method_default_impl;
              })
          methods
      in
      Trait_registry.register_trait
        {
          Trait_registry.trait_name = name;
          trait_type_param = type_param;
          trait_supertraits = supertraits;
          trait_methods = provisional_method_sigs;
        };
      let* method_sigs = map_result convert_method_sig methods in
      let trait_def =
        {
          Trait_registry.trait_name = name;
          trait_type_param = type_param;
          trait_supertraits = supertraits;
          trait_methods = method_sigs;
        }
      in
      (* Validate and register trait *)
      match Trait_registry.validate_trait_def trait_def with
      | Error msg -> Error (error ~code:"type-constructor" ~message:msg)
      | Ok () ->
          Trait_registry.register_trait trait_def;
          Ok (empty_substitution, TNull))
  | AST.ImplDef { impl_trait_name; impl_type_params; impl_for_type; impl_methods } ->
      let type_param_names = List.map (fun (p : AST.generic_param) -> p.name) impl_type_params in
      let unique_param_names = List.sort_uniq String.compare type_param_names in
      let impl_origin =
        if Derive_expand.is_default_derived_impl ~trait_name:impl_trait_name ~for_type:impl_for_type then
          Trait_registry.DefaultDerivedImpl
        else
          Trait_registry.ExplicitImpl
      in
      if List.length unique_param_names <> List.length type_param_names then
        Error
          (error ~code:"type-constructor"
             ~message:(Printf.sprintf "Generic impl '%s' has duplicate type parameter names" impl_trait_name))
      else
        let impl_type_bindings =
          List.map (fun (p : AST.generic_param) -> (p.name, TVar p.name)) impl_type_params
        in
        let convert_impl_type_expr (te : AST.type_expr) : (mono_type, Diagnostic.t) result =
          Annotation.type_expr_to_mono_type_with impl_type_bindings te
        in
        let with_impl_type_param_constraints (f : unit -> (substitution * mono_type, Diagnostic.t) result) :
            (substitution * mono_type, Diagnostic.t) result =
          let constraint_store = current_constraint_store () in
          let constrained_field_store = current_constrained_field_store () in
          let snapshots =
            List.map
              (fun (p : AST.generic_param) ->
                (p.name, Hashtbl.find_opt constraint_store p.name, Hashtbl.find_opt constrained_field_store p.name))
              impl_type_params
          in
          let restore () =
            List.iter
              (fun (name, old_constraints_opt, old_fields_opt) ->
                (match old_constraints_opt with
                | Some constraints -> Hashtbl.replace constraint_store name constraints
                | None -> Hashtbl.remove constraint_store name);
                match old_fields_opt with
                | Some fields -> Hashtbl.replace constrained_field_store name fields
                | None -> Hashtbl.remove constrained_field_store name)
              snapshots
          in
          List.iter
            (fun (p : AST.generic_param) -> add_type_var_constraints p.name p.constraints)
            impl_type_params;
          Fun.protect ~finally:restore f
        in
        with_impl_type_param_constraints (fun () ->
            (* Convert impl_for_type from AST.type_expr to mono_type *)
            match convert_impl_type_expr impl_for_type with
            | Error e -> Error e
            | Ok for_type_mono -> (
                (* Phase 7: Trait impl method adapter — routes through type_callable.
                   Looks up trait signature for known types so annotations are optional. *)
                let find_trait_method_sig method_name =
                  Trait_registry.lookup_trait_method_with_supertraits impl_trait_name method_name
                in

                let infer_impl_method_body (m : AST.method_impl) :
                    ((Trait_registry.method_sig * substitution) option, Diagnostic.t) result =
                  (* Known types from trait signature *)
                  let trait_method_opt = find_trait_method_sig m.impl_method_name in
                  let effective_method_generics =
                    match (m.impl_method_generics, trait_method_opt) with
                    | Some gps, _ -> gps
                    | None, Some (_source_trait_def, tm) ->
                        List.map
                          (fun (name, constraints) -> AST.{ name; constraints = Constraints.names constraints })
                          tm.Trait_registry.method_generics
                    | None, None -> []
                  in
                  (* Build type bindings: method generics @ impl type params *)
                  let method_type_bindings =
                    List.map (fun (gp : AST.generic_param) -> (gp.name, TVar gp.name)) effective_method_generics
                  in
                  let all_type_bindings = method_type_bindings @ impl_type_bindings in

                  let known_param_types =
                    match trait_method_opt with
                    | None -> []
                    | Some (source_trait_def, tm) ->
                        let source_trait_subst =
                          match source_trait_def.Trait_registry.trait_type_param with
                          | None -> SubstMap.empty
                          | Some tp -> SubstMap.singleton tp for_type_mono
                        in
                        List.mapi
                          (fun i (_name, ty) -> (i, apply_substitution source_trait_subst ty))
                          tm.method_params
                  in
                  let known_return =
                    match trait_method_opt with
                    | None -> None
                    | Some (source_trait_def, tm) ->
                        let source_trait_subst =
                          match source_trait_def.Trait_registry.trait_type_param with
                          | None -> SubstMap.empty
                          | Some tp -> SubstMap.singleton tp for_type_mono
                        in
                        Some (apply_substitution source_trait_subst tm.method_return_type)
                  in

                  (* Map effect annotation — None means Unspecified (infer from body) *)
                  let effect_annot =
                    match m.impl_method_effect with
                    | Some AST.Effectful -> `Effectful
                    | Some AST.Pure -> `Pure
                    | None -> `Unspecified
                  in

                  (* Set up method-level generic constraints *)
                  let constraint_store = current_constraint_store () in
                  let constrained_field_store = current_constrained_field_store () in
                  let method_constraint_snapshots =
                    List.map
                      (fun (gp : AST.generic_param) ->
                        let old_constraints = Hashtbl.find_opt constraint_store gp.name in
                        let old_fields = Hashtbl.find_opt constrained_field_store gp.name in
                        add_type_var_constraints gp.name gp.constraints;
                        (gp.name, old_constraints, old_fields))
                      effective_method_generics
                  in
                  let restore_method_constraints () =
                    List.iter
                      (fun (name, old_constraints_opt, old_fields_opt) ->
                        (match old_constraints_opt with
                        | Some old_constraints -> Hashtbl.replace constraint_store name old_constraints
                        | None -> Hashtbl.remove constraint_store name);
                        match old_fields_opt with
                        | Some old_fields -> Hashtbl.replace constrained_field_store name old_fields
                        | None -> Hashtbl.remove constrained_field_store name)
                      method_constraint_snapshots
                  in

                  (* Call unified callable engine with constraint protection *)
                  let body_result =
                    Fun.protect ~finally:restore_method_constraints (fun () ->
                        type_callable type_map env ~type_bindings:all_type_bindings ~known_param_types
                          ~params:m.impl_method_params ~return_annot:m.impl_method_return_type ~known_return
                          ~effect_annot ~strict_return_check:true ~body:m.impl_method_body)
                  in
                  match body_result with
                  | Error e -> Error e
                  | Ok (param_names, param_types, return_type, is_effectful, subst) ->
                      let method_generics =
                        List.map
                          (fun (gp : AST.generic_param) -> (gp.name, Constraints.of_names gp.constraints))
                          effective_method_generics
                      in
                      let method_effect =
                        if is_effectful then
                          `Effectful
                        else
                          `Pure
                      in
                      let method_generic_internal_vars =
                        List.filter_map
                          (fun (gp_name, _) ->
                            match apply_substitution subst (TVar gp_name) with
                            | TVar internal_name when internal_name <> gp_name -> Some (gp_name, internal_name)
                            | _ -> None)
                          method_generics
                      in
                      record_method_def m.impl_method_id
                        {
                          Resolution_artifacts.md_param_names = param_names;
                          md_param_types = param_types;
                          md_return_type = return_type;
                          md_is_effectful = is_effectful;
                          md_body_id = m.impl_method_id;
                        };
                      Ok
                        (Some
                           ( {
                               Trait_registry.method_key =
                                 Resolution_artifacts.UserCallable
                                   { file_id = None; callable_id = m.impl_method_id };
                               method_name = m.impl_method_name;
                               method_generics;
                               method_params = List.combine param_names param_types;
                               method_return_type = return_type;
                               method_effect;
                               method_generic_internal_vars;
                               method_default_impl = None;
                             },
                             subst ))
                in

                let rec collect_methods_and_subst subst_acc methods_acc = function
                  | [] -> Ok (List.rev methods_acc, subst_acc)
                  | m :: rest -> (
                      match infer_impl_method_body m with
                      | Error e -> Error e
                      | Ok None -> collect_methods_and_subst subst_acc methods_acc rest
                      | Ok (Some (method_sig, subst_method)) ->
                          let subst' = compose_substitution subst_acc subst_method in
                          collect_methods_and_subst subst' (method_sig :: methods_acc) rest)
                in

                match collect_methods_and_subst empty_substitution [] impl_methods with
                | Error e -> Error e
                | Ok (methods, method_subst) -> (
                    let for_type_mono' = apply_substitution method_subst for_type_mono in
                    let methods' =
                      List.map
                        (fun (m : Trait_registry.method_sig) ->
                          (* Scope isolation: remove method-level generic param names from subst
                             so they stay polymorphic in the stored signature (Phase 3 fix). *)
                          let isolated_subst =
                            List.fold_left
                              (fun s (gname, _) -> SubstMap.remove gname s)
                              method_subst m.method_generics
                          in
                          {
                            m with
                            method_params =
                              List.map
                                (fun (name, ty) -> (name, apply_substitution isolated_subst ty))
                                m.method_params;
                            method_return_type = apply_substitution isolated_subst m.method_return_type;
                          })
                        methods
                    in

                    let impl_def =
                      {
                        Trait_registry.impl_trait_name;
                        impl_type_params;
                        impl_for_type = for_type_mono';
                        impl_methods = methods';
                      }
                    in

                    (* Validate override annotations against trait defaults *)
                    let override_check =
                      match impl_origin with
                      | Trait_registry.DefaultDerivedImpl -> Ok ()
                      | _ -> (
                          match Trait_registry.lookup_trait impl_trait_name with
                          | None -> Ok ()
                          | Some _trait_def ->
                              let has_default method_name =
                                match
                                  Trait_registry.lookup_trait_method_with_supertraits impl_trait_name method_name
                                with
                                | Some (_source_trait_def, m) -> m.method_default_impl <> None
                                | None -> false
                              in
                              let rec check = function
                                | [] -> Ok ()
                                | (m : AST.method_impl) :: rest ->
                                    let is_override = m.impl_method_override in
                                    let replaces_default = has_default m.impl_method_name in
                                    if replaces_default && not is_override then
                                      Error
                                        (error ~code:"override-required"
                                           ~message:
                                             (Printf.sprintf
                                                "Method '%s' in impl '%s' replaces a trait default; add 'override' keyword"
                                                m.impl_method_name impl_trait_name))
                                    else (
                                      if is_override && not replaces_default then
                                        emit_diagnostic
                                          (warning_at_stmt ~code:"override-unnecessary"
                                             ~message:
                                               (Printf.sprintf
                                                  "Method '%s' in impl '%s' is marked 'override' but trait has no default for it"
                                                  m.impl_method_name impl_trait_name)
                                             m.impl_method_body);
                                      check rest)
                              in
                              check impl_methods)
                    in
                    match override_check with
                    | Error e -> Error e
                    | Ok () -> (
                        (* Validate and register impl *)
                        match Trait_registry.validate_impl impl_def with
                        | Error msg -> Error (error ~code:"type-constructor" ~message:msg)
                        | Ok () ->
                            let impl_source : Trait_registry.impl_source =
                              { file_id = stmt.file_id; start_pos = stmt.pos; end_pos = stmt.end_pos }
                            in
                            Trait_registry.register_impl ~source:impl_source ~origin:impl_origin impl_def;
                            Ok (method_subst, TNull)))))
  | AST.InherentImplDef { inherent_for_type; inherent_methods } -> (
      let reject_invalid_override =
        let rec loop = function
          | [] -> Ok ()
          | (m : AST.method_impl) :: rest ->
              if m.impl_method_override then
                Error
                  (error_at_stmt ~code:"override-invalid"
                     ~message:
                       (Printf.sprintf
                          "Method '%s' uses 'override' in an inherent impl; 'override' is only valid in trait impls"
                          m.impl_method_name)
                     m.impl_method_body)
              else
                loop rest
        in
        loop inherent_methods
      in
      match reject_invalid_override with
      | Error e -> Error e
      | Ok () -> (
          let is_known_type_name (name : string) : bool =
            Option.is_some (Annotation.builtin_primitive_type name)
            || Option.is_some (Annotation.builtin_type_constructor_name name)
            || Annotation.lookup_enum_by_source_name name <> None
            || Trait_registry.lookup_trait name <> None
          in
          let rec collect_target_generic_names ~(in_head : bool) (te : AST.type_expr) (acc : StringSet.t) :
              StringSet.t =
            match te with
            | AST.TCon name ->
                if in_head || is_known_type_name name then
                  acc
                else
                  StringSet.add name acc
            | AST.TVar name ->
                if in_head then
                  acc
                else
                  StringSet.add name acc
            | AST.TTraitObject _ -> acc
            | AST.TApp (_con_name, args) ->
                List.fold_left (fun acc' arg -> collect_target_generic_names ~in_head:false arg acc') acc args
            | AST.TArrow (params, ret, _) ->
                let acc' =
                  List.fold_left
                    (fun acc' param -> collect_target_generic_names ~in_head:false param acc')
                    acc params
                in
                collect_target_generic_names ~in_head:false ret acc'
            | AST.TUnion members ->
                List.fold_left
                  (fun acc' member -> collect_target_generic_names ~in_head:false member acc')
                  acc members
            | AST.TIntersection members ->
                List.fold_left
                  (fun acc' member -> collect_target_generic_names ~in_head:false member acc')
                  acc members
            | AST.TRecord (fields, _row_var) ->
                List.fold_left
                  (fun acc' (field : AST.record_type_field) ->
                    collect_target_generic_names ~in_head:false field.field_type acc')
                  acc fields
          in
          let generic_type_bindings : (string * mono_type) list =
            collect_target_generic_names ~in_head:true inherent_for_type StringSet.empty
            |> StringSet.elements
            |> List.map (fun name -> (name, TVar name))
          in
          let convert_inherent_type_expr (te : AST.type_expr) : (mono_type, Diagnostic.t) result =
            Annotation.type_expr_to_mono_type_with generic_type_bindings te
          in
          match convert_inherent_type_expr inherent_for_type with
          | Error e -> Error e
          | Ok inherent_for_type_mono -> (
              let inherent_for_type_mono = canonicalize_mono_type inherent_for_type_mono in
              (* Phase 7: Inherent impl method adapter — routes through type_callable.
             Provides receiver type as known param, computes method_generic_internal_vars. *)
              let infer_inherent_method_body (m : AST.method_impl) :
                  (Trait_registry.method_sig * substitution, Diagnostic.t) result =
                (* Validate receiver exists before inference *)
                if m.impl_method_params = [] then
                  Error
                    (error ~code:"type-constructor"
                       ~message:
                         (Printf.sprintf "Inherent method '%s' must declare a receiver parameter"
                            m.impl_method_name))
                else
                  (* Build type bindings: method generics @ inherent type bindings *)
                  let method_type_bindings =
                    match m.impl_method_generics with
                    | None -> []
                    | Some gps -> List.map (fun (gp : AST.generic_param) -> (gp.name, TVar gp.name)) gps
                  in
                  let all_type_bindings = method_type_bindings @ generic_type_bindings in

                  (* Known receiver type from impl target *)
                  let known_param_types = [ (0, inherent_for_type_mono) ] in

                  (* Map effect annotation — None means Unspecified (infer from body) *)
                  let effect_annot =
                    match m.impl_method_effect with
                    | Some AST.Effectful -> `Effectful
                    | Some AST.Pure -> `Pure
                    | None -> `Unspecified
                  in

                  (* Set up method-level generic constraints *)
                  let constraint_store = current_constraint_store () in
                  let constrained_field_store = current_constrained_field_store () in
                  let method_constraint_snapshots =
                    match m.impl_method_generics with
                    | None -> []
                    | Some gps ->
                        List.map
                          (fun (gp : AST.generic_param) ->
                            let old_constraints = Hashtbl.find_opt constraint_store gp.name in
                            let old_fields = Hashtbl.find_opt constrained_field_store gp.name in
                            add_type_var_constraints gp.name gp.constraints;
                            (gp.name, old_constraints, old_fields))
                          gps
                  in
                  let restore_method_constraints () =
                    List.iter
                      (fun (name, old_constraints_opt, old_fields_opt) ->
                        (match old_constraints_opt with
                        | Some old_constraints -> Hashtbl.replace constraint_store name old_constraints
                        | None -> Hashtbl.remove constraint_store name);
                        match old_fields_opt with
                        | Some old_fields -> Hashtbl.replace constrained_field_store name old_fields
                        | None -> Hashtbl.remove constrained_field_store name)
                      method_constraint_snapshots
                  in

                  (* Call unified callable engine with constraint protection *)
                  let body_result =
                    Fun.protect ~finally:restore_method_constraints (fun () ->
                        type_callable type_map env ~type_bindings:all_type_bindings ~known_param_types
                          ~params:m.impl_method_params ~return_annot:m.impl_method_return_type ~known_return:None
                          ~effect_annot ~strict_return_check:true ~body:m.impl_method_body)
                  in
                  match body_result with
                  | Error e -> Error e
                  | Ok (param_names, param_types, return_type, is_effectful, subst) ->
                      let method_generics =
                        match m.impl_method_generics with
                        | None -> []
                        | Some gps ->
                            List.map
                              (fun (gp : AST.generic_param) -> (gp.name, Constraints.of_names gp.constraints))
                              gps
                      in
                      let method_effect =
                        if is_effectful then
                          `Effectful
                        else
                          `Pure
                      in
                      (* Track which internal TVars correspond to method-level generics.
                     Body inference may map e.g. b -> t0; the emitter needs this
                     to substitute t0 -> String when specializing for b = String. *)
                      let method_generic_internal_vars =
                        List.filter_map
                          (fun (gp_name, _) ->
                            match apply_substitution subst (TVar gp_name) with
                            | TVar internal_name when internal_name <> gp_name -> Some (gp_name, internal_name)
                            | _ -> None)
                          method_generics
                      in
                      record_method_def m.impl_method_id
                        {
                          Resolution_artifacts.md_param_names = param_names;
                          md_param_types = param_types;
                          md_return_type = return_type;
                          md_is_effectful = is_effectful;
                          md_body_id = m.impl_method_id;
                        };
                      Ok
                        ( {
                            Trait_registry.method_key =
                              Resolution_artifacts.UserCallable { file_id = None; callable_id = m.impl_method_id };
                            method_name = m.impl_method_name;
                            method_generics;
                            method_params = List.combine param_names param_types;
                            method_return_type = return_type;
                            method_effect;
                            method_generic_internal_vars;
                            method_default_impl = None;
                          },
                          subst )
              in
              (* Pre-register inherent methods with annotation-based stubs so recursive
             calls within method bodies can resolve them. The stubs are replaced with
             final signatures after body inference completes. *)
              let target_type_for_preregister = canonicalize_mono_type inherent_for_type_mono in
              let preregister_result =
                List.fold_left
                  (fun acc (m : AST.method_impl) ->
                    match acc with
                    | Error _ -> acc
                    | Ok () -> (
                        let all_type_bindings =
                          let method_type_bindings =
                            match m.impl_method_generics with
                            | None -> []
                            | Some gps -> List.map (fun (gp : AST.generic_param) -> (gp.name, TVar gp.name)) gps
                          in
                          method_type_bindings @ generic_type_bindings
                        in
                        let convert te = Annotation.type_expr_to_mono_type_with all_type_bindings te in
                        let param_types =
                          List.mapi
                            (fun i ((_name, annot) : string * AST.type_expr option) ->
                              if i = 0 then
                                target_type_for_preregister
                              else
                                match annot with
                                | Some te -> (
                                    match convert te with
                                    | Ok t -> t
                                    | Error _ -> fresh_type_var ())
                                | None -> fresh_type_var ())
                            m.impl_method_params
                        in
                        let return_type =
                          match m.impl_method_return_type with
                          | Some te -> (
                              match convert te with
                              | Ok t -> t
                              | Error _ -> fresh_type_var ())
                          | None -> fresh_type_var ()
                        in
                        let param_names = List.map fst m.impl_method_params in
                        let method_generics =
                          match m.impl_method_generics with
                          | None -> []
                          | Some gps ->
                              List.map
                                (fun (gp : AST.generic_param) -> (gp.name, Constraints.of_names gp.constraints))
                                gps
                        in
                        let stub_sig : Trait_registry.method_sig =
                          {
                            method_key =
                              Resolution_artifacts.UserCallable { file_id = None; callable_id = m.impl_method_id };
                            method_name = m.impl_method_name;
                            method_generics;
                            method_params = List.combine param_names param_types;
                            method_return_type = return_type;
                            method_effect = `Pure;
                            method_generic_internal_vars = [];
                            method_default_impl = None;
                          }
                        in
                        match
                          Inherent_registry.register_method ~for_type:target_type_for_preregister stub_sig
                        with
                        | Ok () -> Ok ()
                        | Error msg -> Error (error ~code:"type-constructor" ~message:msg)))
                  (Ok ()) inherent_methods
              in
              match preregister_result with
              | Error e -> Error e
              | Ok () ->
                  (* Track which methods are being defined so resolution can avoid
             self-shadowing: when both inherent and trait provide the same name,
             prefer the trait method inside the body (avoids infinite recursion). *)
                  let impl_method_names =
                    List.map
                      (fun (m : AST.method_impl) -> (target_type_for_preregister, m.impl_method_name))
                      inherent_methods
                  in
                  let prev_inherent_impl_methods = !current_inherent_impl_methods in
                  current_inherent_impl_methods := impl_method_names @ prev_inherent_impl_methods;
                  let rec register_methods subst_acc = function
                    | [] -> Ok (subst_acc, TNull)
                    | m :: rest -> (
                        match infer_inherent_method_body m with
                        | Error e -> Error e
                        | Ok (method_sig, method_subst) -> (
                            let method_sig =
                              {
                                method_sig with
                                method_params =
                                  List.map
                                    (fun (name, ty) -> (name, apply_substitution method_subst ty))
                                    method_sig.method_params;
                                method_return_type = apply_substitution method_subst method_sig.method_return_type;
                              }
                            in
                            (* Restore original generic names in stored signature.
                       Body inference may replace generic params (e.g. b) with fresh
                       TVars (e.g. t0) via method_subst. Build a reverse mapping so
                       the stored signature uses the original names, allowing
                       instantiate_method_generics to substitute them correctly. *)
                            let method_sig =
                              let generic_names_to_restore =
                                List.map fst generic_type_bindings @ List.map fst method_sig.method_generics
                              in
                              match generic_names_to_restore with
                              | [] -> method_sig
                              | _ ->
                                  let reverse_subst =
                                    List.fold_left
                                      (fun acc param_name ->
                                        let resolved = apply_substitution method_subst (TVar param_name) in
                                        match resolved with
                                        | TVar fresh_name when fresh_name <> param_name ->
                                            SubstMap.add fresh_name (TVar param_name) acc
                                        | _ -> acc)
                                      SubstMap.empty generic_names_to_restore
                                  in
                                  if SubstMap.is_empty reverse_subst then
                                    method_sig
                                  else
                                    {
                                      method_sig with
                                      method_params =
                                        List.map
                                          (fun (name, ty) -> (name, apply_substitution reverse_subst ty))
                                          method_sig.method_params;
                                      method_return_type =
                                        apply_substitution reverse_subst method_sig.method_return_type;
                                    }
                            in
                            record_method_def m.impl_method_id
                              {
                                Resolution_artifacts.md_param_names = List.map fst method_sig.method_params;
                                md_param_types = List.map snd method_sig.method_params;
                                md_return_type = method_sig.method_return_type;
                                md_is_effectful =
                                  (match method_sig.method_effect with
                                  | `Effectful -> true
                                  | `Pure -> false);
                                md_body_id = m.impl_method_id;
                              };
                            let receiver_type =
                              match method_sig.method_params with
                              | [] ->
                                  failwith
                                    "impossible: inherent method parameter list unexpectedly empty after validation"
                              | (_, first_param_type) :: _ -> canonicalize_mono_type first_param_type
                            in
                            let target_type = canonicalize_mono_type inherent_for_type_mono in
                            let register_current_method () =
                              (* Remove pre-registered stub, then register final inferred signature.
                         Using remove+register preserves cross-impl duplicate detection. *)
                              Inherent_registry.remove_method ~for_type:target_type method_sig.method_name;
                              match Inherent_registry.register_method ~for_type:target_type method_sig with
                              | Error msg -> Error (error ~code:"type-constructor" ~message:msg)
                              | Ok () ->
                                  let subst' = compose_substitution subst_acc method_subst in
                                  register_methods subst' rest
                            in
                            match Unify.unify receiver_type target_type with
                            | Error _ ->
                                Error
                                  (error ~code:"type-constructor"
                                     ~message:
                                       (Printf.sprintf
                                          "Inherent method '%s' receiver type %s does not match impl target type %s"
                                          method_sig.method_name (Types.to_string receiver_type)
                                          (Types.to_string target_type)))
                            | Ok _ ->
                                (* Phase 4.6: Allow inherent methods even when a trait provides the same name.
                           Collision is resolved at call sites: dot calls may be ambiguous,
                           and qualified calls can explicitly select the desired target. *)
                                register_current_method ()))
                  in
                  let result = register_methods empty_substitution inherent_methods in
                  current_inherent_impl_methods := prev_inherent_impl_methods;
                  result)))
  | AST.DeriveDef { derive_traits; derive_for_type } -> (
      (* Auto-generate implementations for derived traits *)
      match Annotation.type_expr_to_mono_type derive_for_type with
      | Error e -> Error e
      | Ok for_type_mono ->
          (* Process each trait to derive *)
          let derive_errors =
            List.filter_map
              (fun (t : AST.derive_trait) ->
                match Trait_registry.derive_impl t.derive_trait_name for_type_mono with
                | Ok () -> None
                | Error msg -> Some msg)
              derive_traits
          in

          if derive_errors <> [] then
            Error (error ~code:"type-constructor" ~message:(String.concat "; " derive_errors))
          else
            Ok (empty_substitution, TNull))
  | AST.TypeAlias _ ->
      let alias_def =
        match stmt.stmt with
        | AST.TypeAlias alias_def -> alias_def
        | _ -> failwith "impossible"
      in
      let type_bindings = List.map (fun name -> (name, TVar name)) alias_def.alias_type_params in
      let* _validated = Annotation.type_expr_to_mono_type_with type_bindings alias_def.alias_body in
      Ok (empty_substitution, TNull)

(* Record metadata-only trait-object packaging sites for explicit returns and
   implicit tail expressions in function bodies. *)
and record_tail_trait_object_coercions (type_map : type_map) (expected_type : mono_type) (stmt : AST.statement) :
    (unit, Diagnostic.t) result =
  match stmt.stmt with
  | AST.ExportDecl _ | AST.ImportDecl _ -> Ok ()
  | AST.Return _ -> Ok ()
  | AST.ExpressionStmt expr -> record_expected_trait_object_coercions type_map expr expected_type
  | AST.Block [] -> Ok ()
  | AST.Block stmts -> record_tail_trait_object_coercions type_map expected_type (List.hd (List.rev stmts))
  | AST.Let _ | AST.EnumDef _ | AST.TypeDef _ | AST.ShapeDef _ | AST.TraitDef _ | AST.ImplDef _
  | AST.InherentImplDef _ | AST.DeriveDef _ | AST.TypeAlias _ ->
      Ok ()

and record_explicit_return_trait_object_coercions
    (type_map : type_map) (expected_type : mono_type) (stmt : AST.statement) : (unit, Diagnostic.t) result =
  match stmt.stmt with
  | AST.ExportDecl _ | AST.ImportDecl _ -> Ok ()
  | AST.Return expr -> record_expected_trait_object_coercions type_map expr expected_type
  | AST.Block stmts ->
      let rec loop = function
        | [] -> Ok ()
        | s :: rest -> (
            match record_explicit_return_trait_object_coercions type_map expected_type s with
            | Error _ as err -> err
            | Ok () -> loop rest)
      in
      loop stmts
  | AST.ExpressionStmt _ -> Ok ()
  | AST.Let _ | AST.EnumDef _ | AST.TypeDef _ | AST.ShapeDef _ | AST.TraitDef _ | AST.ImplDef _
  | AST.InherentImplDef _ | AST.DeriveDef _ | AST.TypeAlias _ ->
      Ok ()

(* Simple validation: check that all explicit return statements match expected type *)
and validate_return_statements
    (type_map : type_map) (env : type_env) (expected_type : mono_type) (stmt : AST.statement) :
    (unit, Diagnostic.t) result =
  match stmt.stmt with
  | AST.ExportDecl _ | AST.ImportDecl _ -> Ok ()
  | AST.Return expr -> (
      match infer_expression type_map env expr with
      | Error e -> Error e
      | Ok (_subst, inferred_type) ->
          (* Use subtyping check: inferred must be subtype of expected *)
          if
            Annotation.is_subtype_of inferred_type expected_type
            || intersection_annotation_compatible inferred_type expected_type
          then
            Ok ()
          else
            Error
              (error_at_stmt ~code:"type-return-mismatch"
                 ~message:
                   ("Function return type annotation mismatch: expected "
                   ^ to_string expected_type
                   ^ " but inferred "
                   ^ to_string inferred_type)
                 stmt))
  | AST.Block stmts ->
      let rec check_all stmts =
        match stmts with
        | [] -> Ok ()
        | s :: rest -> (
            match validate_return_statements type_map env expected_type s with
            | Error e -> Error e
            | Ok () -> check_all rest)
      in
      check_all stmts
  | AST.ExpressionStmt expr -> (
      match expr.expr with
      | AST.If (_cond, cons, alt) -> (
          match validate_return_statements type_map env expected_type cons with
          | Error e -> Error e
          | Ok () -> (
              match alt with
              | None -> Ok ()
              | Some alt_stmt -> validate_return_statements type_map env expected_type alt_stmt))
      | _ -> Ok ())
  | AST.Let _ -> Ok ()
  | AST.EnumDef _ | AST.TypeDef _ | AST.ShapeDef _ -> Ok ()
  | AST.TraitDef _ -> Ok () (* TODO: Phase 4.3 - validate trait defs *)
  | AST.ImplDef _ -> Ok () (* TODO: Phase 4.3 - validate impl defs *)
  | AST.InherentImplDef _ -> Ok () (* TODO: Phase 4.5 - validate inherent impl defs *)
  | AST.DeriveDef _ -> Ok () (* TODO: Phase 4.3 - validate derive defs *)
  | AST.TypeAlias _ -> Ok () (* TODO: Phase 4.4 - validate type aliases *)

(* ============================================================
   Pattern Matching Helpers
   ============================================================ *)

and normalize_pattern_scrutinee (pattern : AST.pattern) (scrutinee_type : mono_type) : mono_type =
  let canonical = canonicalize_mono_type scrutinee_type in
  let canonical =
    match pattern.AST.pat with
    | AST.PRecord (_, _) -> (
        match canonical with
        | TIntersection members -> (
            match Annotation.merged_record_intersection_type members with
            | Ok merged -> merged
            | Error _ -> canonical)
        | _ -> canonical)
    | _ -> canonical
  in
  match (pattern.AST.pat, canonical) with
  | AST.PRecord (_, _), TNamed (name, type_args) -> (
      match Type_registry.instantiate_named_product_fields name type_args with
      | Some (Ok fields) -> TRecord (fields, None)
      | Some (Error _) | None -> canonical)
  | _ -> canonical

and merge_binding_type (left : mono_type) (right : mono_type) : mono_type =
  Types.normalize_union [ canonicalize_mono_type left; canonicalize_mono_type right ]

and merge_pattern_bindings (binding_sets : (string * mono_type) list list) : (string * mono_type) list =
  let rec upsert acc (name, typ) =
    match acc with
    | [] -> [ (name, typ) ]
    | (existing_name, existing_typ) :: rest when existing_name = name ->
        (existing_name, merge_binding_type existing_typ typ) :: rest
    | binding :: rest -> binding :: upsert rest (name, typ)
  in
  List.fold_left (fun acc bindings -> List.fold_left upsert acc bindings) [] binding_sets

and pattern_binding_names (bindings : (string * mono_type) list) : StringSet.t =
  List.fold_left (fun acc (name, _typ) -> StringSet.add name acc) StringSet.empty bindings

and pattern_binding_names_to_string (bindings : StringSet.t) : string =
  match StringSet.elements bindings with
  | [] -> "<none>"
  | names -> String.concat ", " names

and validate_or_pattern_bindings (results : ((string * mono_type) list * mono_type) list) :
    (unit, Diagnostic.t) result =
  match results with
  | [] | [ _ ] -> Ok ()
  | (first_bindings, _) :: rest ->
      let expected = pattern_binding_names first_bindings in
      let rec loop = function
        | [] -> Ok ()
        | (bindings, _) :: remaining ->
            let actual = pattern_binding_names bindings in
            if StringSet.equal expected actual then
              loop remaining
            else
              Error
                (error ~code:"type-pattern"
                   ~message:
                     (Printf.sprintf "Or-pattern alternatives must bind the same names (expected %s, found %s)"
                        (pattern_binding_names_to_string expected)
                        (pattern_binding_names_to_string actual)))
      in
      loop rest

and combine_pattern_results (results : ((string * mono_type) list * mono_type) list) :
    (string * mono_type) list * mono_type =
  let narrowed_members =
    List.concat_map
      (fun (_bindings, narrowed_type) ->
        match canonicalize_mono_type narrowed_type with
        | TUnion members -> members
        | member -> [ member ])
      results
  in
  let narrowed_type =
    match Types.normalize_union narrowed_members with
    | TUnion [ single ] -> single
    | normalized -> normalized
  in
  (merge_pattern_bindings (List.map fst results), narrowed_type)

(* Check all arms of a match expression and collect their body types *)
and infer_match_arms type_map env scrutinee scrutinee_type arms subst match_expr =
  let rec loop acc_subst arm_types = function
    | [] -> (
        (* All arms processed, unify all arm body types or create union *)
        match arm_types with
        | [] ->
            Error (error_at ~code:"type-match" ~message:"Match expression must have at least one arm" match_expr)
        | [ single_type ] -> Ok (acc_subst, single_type) (* Single arm, return its type *)
        | first :: rest -> (
            (* Try to unify all types together *)
            let rec try_unify_all s unified_types remaining =
              match remaining with
              | [] -> Ok (s, unified_types) (* All unified successfully *)
              | t :: rest_types -> (
                  let first' = apply_substitution s (List.hd unified_types) in
                  let t' = apply_substitution s t in
                  match Unify.unify first' t' with
                  | Ok s2 ->
                      (* Unified successfully, continue *)
                      let new_s = compose_substitution s s2 in
                      try_unify_all new_s unified_types rest_types
                  | Error _ ->
                      (* Couldn't unify, add this type to the list *)
                      try_unify_all s (t :: unified_types) rest_types)
            in
            match try_unify_all empty_substitution [ first ] rest with
            | Ok (unify_subst, unified) -> (
                (* Apply final substitution *)
                let final_subst = compose_substitution acc_subst unify_subst in
                let final_types = List.map (apply_substitution final_subst) unified in
                (* If all unified to one type, return it; otherwise create union *)
                match final_types with
                | [ single ] -> Ok (final_subst, single)
                | multiple -> Ok (final_subst, Types.normalize_union (List.rev multiple)))
            | Error e -> Error e))
    | arm :: rest_arms -> (
        match infer_match_arm type_map env scrutinee scrutinee_type arm with
        | Error e -> Error e
        | Ok (arm_subst, body_type) ->
            let new_subst = compose_substitution acc_subst arm_subst in
            loop new_subst (body_type :: arm_types) rest_arms)
  in
  loop subst [] arms

(* Infer the type of a single match arm *)
and infer_match_arm type_map env scrutinee scrutinee_type arm =
  let scrutinee_path_opt = Type_narrowing.path_of_expr scrutinee in
  match check_patterns arm.AST.patterns scrutinee_type with
  | Error e -> Error e
  | Ok (bindings, narrowed_type) ->
      let narrowed_scrutinee = canonicalize_mono_type narrowed_type <> canonicalize_mono_type scrutinee_type in
      let env', body =
        match (scrutinee_path_opt, narrowed_scrutinee) with
        | Some path, true when Type_narrowing.is_identifier_path path ->
            (TypeEnv.add path.root (mono_to_poly narrowed_type) env, arm.AST.body)
        | Some path, true ->
            let narrowed_name = Type_narrowing.temp_name path "typed" in
            ( TypeEnv.add narrowed_name (mono_to_poly narrowed_type) env,
              Type_narrowing.substitute_path_in_expr path
                ~replace:(Type_narrowing.replacement_identifier narrowed_name)
                arm.AST.body )
        | _ -> (env, arm.AST.body)
      in
      let env' = List.fold_left (fun e (name, ty) -> TypeEnv.add name (mono_to_poly ty) e) env' bindings in
      infer_expression type_map env' body

(* Check a list of patterns (for | syntax) against the scrutinee type *)
and check_patterns patterns scrutinee_type =
  match patterns with
  | [] -> Error (error ~code:"type-pattern" ~message:"Match arm must have at least one pattern")
  | first :: rest -> (
      match check_pattern first scrutinee_type with
      | Error e -> Error e
      | Ok first_result -> (
          let rec check_rest = function
            | [] -> Ok [ first_result ]
            | pat :: rest_pats -> (
                match check_pattern pat scrutinee_type with
                | Error e -> Error e
                | Ok result -> (
                    match check_rest rest_pats with
                    | Error e -> Error e
                    | Ok rest_results -> Ok (result :: rest_results)))
          in
          match check_rest rest with
          | Error e -> Error e
          | Ok results -> (
              match validate_or_pattern_bindings results with
              | Error e -> Error e
              | Ok () -> Ok (combine_pattern_results results))))

(* Check a single pattern against the scrutinee type, return variable bindings *)
and check_pattern pattern scrutinee_type =
  let scrutinee_type = normalize_pattern_scrutinee pattern scrutinee_type in
  match (pattern.AST.pat, scrutinee_type) with
  | AST.PRecord (_, _), TUnion members -> (
      let matching_members =
        List.filter_map
          (fun member ->
            match check_pattern pattern member with
            | Ok result -> Some result
            | Error _ -> None)
          members
      in
      match matching_members with
      | [] ->
          Error
            (error ~code:"type-pattern"
               ~message:
                 (Printf.sprintf "Record pattern doesn't match scrutinee type %s" (to_string scrutinee_type)))
      | results -> Ok (combine_pattern_results results))
  | _ -> (
      match pattern.AST.pat with
      | AST.PWildcard -> Ok ([], scrutinee_type)
      | AST.PVariable name -> Ok ([ (name, scrutinee_type) ], scrutinee_type)
      | AST.PLiteral lit -> (
          let lit_type =
            match lit with
            | AST.LInt _ -> TInt
            | AST.LString _ -> TString
            | AST.LBool _ -> TBool
          in
          match Unify.unify lit_type scrutinee_type with
          | Error e -> Error (error ~code:e.code ~message:e.message)
          | Ok _ -> Ok ([], lit_type))
      | AST.PConstructor (enum_name, variant_name, field_patterns) -> (
          (* Check scrutinee is the right enum type *)
          match scrutinee_type with
          | TEnum (sname, type_args) when sname = enum_name -> (
              (* Look up variant *)
              match Enum_registry.lookup_variant enum_name variant_name with
              | None ->
                  Error
                    (error ~code:"type-pattern"
                       ~message:(Printf.sprintf "Unknown variant: %s.%s" enum_name variant_name))
              | Some variant -> (
                  if List.length field_patterns <> List.length variant.fields then
                    Error
                      (error ~code:"type-pattern"
                         ~message:
                           (Printf.sprintf "Pattern %s.%s expects %d fields, got %d" enum_name variant_name
                              (List.length variant.fields) (List.length field_patterns)))
                  else
                    (* Get field types with type args substituted *)
                    let enum_def = Option.get (Enum_registry.lookup enum_name) in
                    let subst = substitution_of_list (List.combine enum_def.type_params type_args) in
                    let field_types = List.map (apply_substitution subst) variant.fields in
                    (* Check each field pattern and collect bindings *)
                    let rec check_fields bindings_acc pats types =
                      match (pats, types) with
                      | [], [] -> Ok bindings_acc
                      | pat :: rest_pats, ty :: rest_types -> (
                          match check_pattern pat ty with
                          | Error e -> Error e
                          | Ok (field_bindings, _field_type) ->
                              check_fields (bindings_acc @ field_bindings) rest_pats rest_types)
                      | _ -> Error (error ~code:"type-pattern" ~message:"Field pattern count mismatch")
                    in
                    match check_fields [] field_patterns field_types with
                    | Error e -> Error e
                    | Ok bindings -> Ok (bindings, scrutinee_type)))
          | _ ->
              Error
                (error ~code:"type-pattern"
                   ~message:
                     (Printf.sprintf "Pattern %s.%s doesn't match scrutinee type %s" enum_name variant_name
                        (to_string scrutinee_type))))
      | AST.PRecord (fields, rest) -> (
          match scrutinee_type with
          | TRecord (scrutinee_fields, scrutinee_row) ->
              let rec check_fields seen_names bindings = function
                | [] ->
                    let rest_bindings =
                      match rest with
                      | None -> []
                      | Some rest_name ->
                          let remaining_fields =
                            List.filter
                              (fun (f : Types.record_field_type) -> not (List.mem f.name seen_names))
                              scrutinee_fields
                          in
                          [ (rest_name, TRecord (remaining_fields, scrutinee_row)) ]
                    in
                    Ok (bindings @ rest_bindings, scrutinee_type)
                | (field : AST.record_pattern_field) :: rest_fields -> (
                    let field_name = field.pat_field_name in
                    match
                      List.find_opt (fun (f : Types.record_field_type) -> f.name = field_name) scrutinee_fields
                    with
                    | None ->
                        Error
                          (error ~code:"type-pattern"
                             ~message:
                               (Printf.sprintf "Record pattern field '%s' not found in scrutinee type %s"
                                  field_name (to_string scrutinee_type)))
                    | Some scrutinee_field -> (
                        let effective_pat =
                          match field.pat_field_pattern with
                          | Some p -> p
                          | None ->
                              AST.
                                {
                                  pat = PVariable field_name;
                                  pos = pattern.pos;
                                  end_pos = pattern.end_pos;
                                  file_id = pattern.file_id;
                                }
                        in
                        match check_pattern effective_pat scrutinee_field.typ with
                        | Error e -> Error e
                        | Ok (field_bindings, _field_type) ->
                            check_fields (field_name :: seen_names) (bindings @ field_bindings) rest_fields))
              in
              check_fields [] [] fields
          | _ ->
              Error
                (error ~code:"type-pattern"
                   ~message:
                     (Printf.sprintf "Record pattern doesn't match scrutinee type %s" (to_string scrutinee_type)))
          ))

(* Unify function shapes while ignoring only the effect flag on arrows.
   Used to reconcile recursive placeholders for unannotated functions that
   may later infer as either pure or effectful. *)
and unify_function_shape_ignoring_effect (left : mono_type) (right : mono_type) :
    (substitution, Diagnostic.t) result =
  match (left, right) with
  | TFun (arg_l, ret_l, _), TFun (arg_r, ret_r, _) -> (
      match unify arg_l arg_r with
      | Error e -> Error e
      | Ok subst1 -> (
          let ret_l' = apply_substitution subst1 ret_l in
          let ret_r' = apply_substitution subst1 ret_r in
          match unify_function_shape_ignoring_effect ret_l' ret_r' with
          | Error e -> Error e
          | Ok subst2 -> Ok (compose_substitution subst1 subst2)))
  | _ -> unify left right

(* ============================================================
   Let Binding Inference
   ============================================================ *)

(* Infer a let binding.
   
    For recursive functions, we use the following approach:
    1. Create a fresh type variable for the binding
    2. Add the binding to the environment with that type variable
    3. Infer the expression type (now the name is in scope for recursion)
    4. Unify the inferred type with the type variable
    5. Generalize as usual
    
    This allows the function to reference itself in its body.
    We treat ALL let bindings this way for simplicity - it's harmless
    for non-recursive bindings and enables recursion for functions.
*)
and infer_let ?(prefer_existing_self = false) type_map env name expr type_annotation =
  let outer_type_bindings = user_named_type_bindings_in_env env in
  let callable_context_result =
    match expr.expr with
    | AST.Function { params; _ } ->
        callable_annotation_context ~type_bindings:outer_type_bindings ~arity:(List.length params) type_annotation
    | _ -> Ok None
  in
  match callable_context_result with
  | Error e -> Error e
  | Ok callable_context -> (
      (* Check if the expression is a function with a return type annotation *)
      (* If so, create a partially constrained type for recursion *)
      let inferred_self_type_result =
        match (expr.expr, callable_context, type_annotation) with
        | AST.Function _, Some (annotated_type, _), _ -> Ok annotated_type
        | AST.Function f, None, _ ->
            provisional_function_type ~outer_type_bindings f.generics f.params f.return_type f.is_effectful
        | _, _, Some type_expr -> annotation_or_fresh type_expr
        | _, _, None -> Ok (fresh_type_var ())
      in
      match inferred_self_type_result with
      | Error e -> Error e
      | Ok inferred_self_type -> (
          let self_type =
            if prefer_existing_self && name <> "_" then
              match current_top_level_placeholder_type env name with
              | Some existing -> existing
              | None -> inferred_self_type
            else
              inferred_self_type
          in
          (* Add to environment as monomorphic (not generalized yet) *)
          let env_with_self =
            if name = "_" then
              env
            else
              TypeEnv.add name (mono_to_poly self_type) env
          in
          (* Infer expression type with self in scope *)
          let infer_expr_result =
            match (expr.expr, callable_context, type_annotation) with
            | AST.Function { origin; generics; params; return_type; is_effectful; body }, Some (_, signature), _
              ->
                infer_function_literal ~known_signature:signature type_map env_with_self expr ~origin ~generics
                  ~params ~return_type ~is_effectful ~body
            | AST.Function { origin; generics; params; return_type; is_effectful; body }, None, _ ->
                infer_function_literal type_map env_with_self expr ~origin ~generics ~params ~return_type
                  ~is_effectful ~body
            | _, _, Some type_expr -> (
                match Annotation.type_expr_to_mono_type type_expr with
                | Error d -> Error d
                | Ok annotated_type ->
                    infer_expression_against_expected type_map env_with_self expr annotated_type)
            | _, _, None -> infer_expression type_map env_with_self expr
          in
          match infer_expr_result with
          | Error e -> Error e
          | Ok (subst1, expr_type) -> (
              (* Unify the inferred type with our placeholder *)
              let self_type' = apply_substitution subst1 self_type in
              let unify_result =
                match canonicalize_mono_type self_type' with
                | TTraitObject _ -> (
                    match record_expected_trait_object_coercions type_map expr self_type' with
                    | Ok () -> Ok empty_substitution
                    | Error e -> Error e)
                | _ -> (
                    match compatible_with_expected_type expr_type self_type' with
                    | Ok subst2 -> Ok subst2
                    | Error e -> (
                        match expr.expr with
                        | AST.Function _ -> (
                            match unify_function_shape_ignoring_effect self_type' expr_type with
                            | Ok subst2 -> Ok subst2
                            | Error _ -> Error e)
                        | _ -> Error e))
              in
              match unify_result with
              | Error e -> Error (error_at ~code:e.code ~message:e.message expr)
              | Ok subst2 -> (
                  let final_subst = compose_substitution subst1 subst2 in
                  propagate_type_var_constraints_through_substitution final_subst;
                  let inferred_final_type = apply_substitution subst2 expr_type in
                  let actual_expr_type = apply_substitution final_subst expr_type in
                  let final_type_result =
                    match type_annotation with
                    | None -> Ok inferred_final_type
                    | Some type_expr -> (
                        match Annotation.type_expr_to_mono_type type_expr with
                        | Error d -> Error d
                        | Ok annotated_type ->
                            if
                              Annotation.check_annotation annotated_type actual_expr_type
                              || intersection_annotation_compatible actual_expr_type annotated_type
                            then
                              match expr.expr with
                              | AST.Function _ -> Ok inferred_final_type
                              | _ -> Ok annotated_type
                            else
                              Error
                                (error_at ~code:"type-annotation-mismatch"
                                   ~message:
                                     (Printf.sprintf
                                        "Type annotation mismatch for '%s': expected %s but inferred %s" name
                                        (Annotation.format_mono_type annotated_type)
                                        (Annotation.format_mono_type actual_expr_type))
                                   expr))
                  in
                  match final_type_result with
                  | Error e -> Error e
                  | Ok final_type -> (
                      match record_expected_trait_object_coercions type_map expr final_type with
                      | Error e -> Error e
                      | Ok () ->
                          (* For top-level forward-reference placeholders, avoid self-leak in Γ:
             otherwise the placeholder binding can block intended generalization. *)
                          let env_for_generalize =
                            if prefer_existing_self && name <> "_" then
                              TypeEnv.remove name env
                            else
                              env
                          in
                          let env' = apply_substitution_env final_subst env_for_generalize in
                          let poly_type = generalize env' final_type in
                          let _ = poly_type in
                          Ok (final_subst, final_type))))))

and infer_block type_map env stmts =
  match stmts with
  | [] -> Ok (empty_substitution, TNull)
  | [ stmt ] -> infer_statement type_map env stmt
  | stmt :: rest -> (
      match infer_statement type_map env stmt with
      | Error e -> Error e
      | Ok (subst1, stmt_type) -> (
          (* For let statements, add the binding to the environment *)
          let env' =
            match stmt.stmt with
            | AST.Let let_binding ->
                let env_subst = apply_substitution_env subst1 env in
                if let_binding.name = "_" then
                  env_subst
                else
                  let binding_type =
                    binding_type_for_env ~value_expr:let_binding.value
                      ~type_annotation:let_binding.type_annotation stmt_type
                  in
                  let poly =
                    if should_monomorphize_let_binding_value let_binding.value then
                      mono_to_poly binding_type
                    else
                      generalize env_subst binding_type
                  in
                  TypeEnv.add let_binding.name poly env_subst
            | _ -> apply_substitution_env subst1 env
          in
          match infer_block type_map env' rest with
          | Error e -> Error e
          | Ok (subst2, result_type) -> Ok (compose_substitution subst1 subst2, result_type)))

(* ============================================================
   Phase 2: Bidirectional Type Checking
   ============================================================ *)

(* Check an expression against an expected type.
   For Phase 2, this is simple: just infer the type and verify it matches.
   In Phase 2.5+, this could do more sophisticated bidirectional flow.
*)
let check_expression (type_map : type_map) (env : type_env) (expr : AST.expression) (expected : mono_type) :
    (substitution * mono_type) infer_result =
  match infer_expression type_map env expr with
  | Error e -> Error e
  | Ok (subst, inferred) ->
      (* For Phase 2, we do simple equality checking after applying substitution *)
      let inferred_applied = apply_substitution subst inferred in
      let expected_applied = apply_substitution subst expected in
      if Annotation.check_annotation expected_applied inferred_applied then
        Ok (subst, inferred)
      else
        (* Use IfBranchMismatch error as a proxy for type mismatch *)
        Error
          (error_at ~code:"type-if-branch-mismatch"
             ~message:
               ("If branches have different types: "
               ^ to_string expected_applied
               ^ " vs "
               ^ to_string inferred_applied)
             expr)

(* ============================================================
   Program Inference
   ============================================================ *)

let predeclare_top_level_lets (env : type_env) (program : AST.program) : (type_env, Diagnostic.t) result =
  let rec go (seen : StringSet.t) (env_acc : type_env) (stmts : AST.statement list) :
      (type_env, Diagnostic.t) result =
    match stmts with
    | [] -> Ok env_acc
    | stmt :: rest -> (
        match stmt.stmt with
        | AST.Let let_binding when let_binding.name <> "_" -> (
            if StringSet.mem let_binding.name seen then
              Error
                (error_at_stmt ~code:"type-constructor"
                   ~message:(Printf.sprintf "Duplicate top-level let definition: %s" let_binding.name)
                   stmt)
            else
              let seen' = StringSet.add let_binding.name seen in
              if TypeEnv.mem let_binding.name env_acc then
                go seen' env_acc rest
              else
                let outer_type_bindings = user_named_type_bindings_in_env env_acc in
                let placeholder_result =
                  if top_level_forward_reference_candidate let_binding.value then
                    match let_binding.value.expr with
                    | AST.Function f ->
                        Some
                          (provisional_function_type ~outer_type_bindings f.generics f.params f.return_type
                             f.is_effectful)
                    | _ -> Some (provisional_placeholder_section_type ~outer_type_bindings ())
                  else
                    None
                in
                match placeholder_result with
                | Some (Error e) -> Error e
                | Some (Ok placeholder) ->
                    set_top_level_placeholder let_binding.name placeholder;
                    go seen' (TypeEnv.add let_binding.name (mono_to_poly placeholder) env_acc) rest
                | None ->
                    (* Keep value bindings strict-order for now; only function
                       declarations and placeholder sections participate in top-level
                       forward references. *)
                    go seen' env_acc rest)
        | _ -> go seen env_acc rest)
  in
  go StringSet.empty env program

let predeclare_top_level_type_aliases (program : AST.program) : (unit, Diagnostic.t) result =
  let rec go (seen : StringSet.t) (stmts : AST.statement list) =
    match stmts with
    | [] -> Ok ()
    | stmt :: rest -> (
        match stmt.stmt with
        | AST.TypeAlias alias_def ->
            if StringSet.mem alias_def.alias_name seen then
              Error
                (error_at_stmt ~code:"type-constructor"
                   ~message:(Printf.sprintf "Duplicate type definition: %s" alias_def.alias_name)
                   stmt)
            else (
              Annotation.register_type_alias alias_def;
              go (StringSet.add alias_def.alias_name seen) rest)
        | _ -> go seen rest)
  in
  go StringSet.empty program

let predeclare_top_level_named_declarations (program : AST.program) : (unit, Diagnostic.t) result =
  let rec go (seen_types : StringSet.t) (seen_shapes : StringSet.t) (stmts : AST.statement list) =
    match stmts with
    | [] -> Ok ()
    | stmt :: rest -> (
        match stmt.stmt with
        | AST.TypeDef type_def ->
            if StringSet.mem type_def.type_name seen_types then
              Error
                (error_at_stmt ~code:"type-constructor"
                   ~message:(Printf.sprintf "Duplicate type definition: %s" type_def.type_name)
                   stmt)
            else (
              Type_registry.predeclare_named_type type_def;
              go (StringSet.add type_def.type_name seen_types) seen_shapes rest)
        | AST.ShapeDef shape_def ->
            if StringSet.mem shape_def.shape_name seen_shapes then
              Error
                (error_at_stmt ~code:"type-constructor"
                   ~message:(Printf.sprintf "Duplicate shape definition: %s" shape_def.shape_name)
                   stmt)
            else (
              Type_registry.predeclare_shape shape_def;
              go seen_types (StringSet.add shape_def.shape_name seen_shapes) rest)
        | _ -> go seen_types seen_shapes rest)
  in
  go StringSet.empty StringSet.empty program

let register_top_level_named_declarations (program : AST.program) : (unit, Diagnostic.t) result =
  let predecl_type_map = create_type_map () in
  let rec register_enums = function
    | [] -> Ok ()
    | (stmt : AST.statement) :: rest -> (
        match stmt.stmt with
        | AST.EnumDef _ -> (
            match infer_statement predecl_type_map empty_env stmt with
            | Error e -> Error e
            | Ok _ -> register_enums rest)
        | _ -> register_enums rest)
  in
  let rec register_named = function
    | [] -> Ok ()
    | (stmt : AST.statement) :: rest -> (
        match stmt.stmt with
        | AST.TypeDef _ | AST.ShapeDef _ -> (
            match infer_statement predecl_type_map empty_env stmt with
            | Error e -> Error e
            | Ok _ -> register_named rest)
        | _ -> register_named rest)
  in
  let* () = register_enums program in
  register_named program

let infer_program
    ?(env = empty_env)
    ?(prebound_symbols = [])
    ?state
    ?(prepare_state = true)
    ?(expand_derives = true)
    (program : AST.program) :
    (type_env * type_map * mono_type) infer_result =
  let state = Option.value state ~default:(create_inference_state ()) in
  with_inference_state state (fun () ->
      if prepare_state then (
        Annotation.clear_type_aliases ();
        Type_registry.clear ();
        Inherent_registry.clear ();
        clear_method_resolution_store ();
        clear_type_var_user_names ();
        clear_top_level_placeholders ());
      let expanded_program_result =
        if expand_derives then
          Derive_expand.expand_user_derives program
        else
          Ok program
      in
      match expanded_program_result with
      | Error e -> Error e
      | Ok expanded_program -> (
          match predeclare_top_level_named_declarations expanded_program with
          | Error e -> Error e
          | Ok () -> (
              match predeclare_top_level_type_aliases expanded_program with
              | Error e -> Error e
              | Ok () -> (
                  match register_top_level_named_declarations expanded_program with
                  | Error e -> Error e
                  | Ok () -> (
                      match resolve_program_symbols ~prebound_symbols env expanded_program with
                      | Error e -> Error e
                      | Ok () -> (
                          let predeclare_top_level_impl_headers (program : AST.program) : unit =
                            List.iter
                              (fun (stmt : AST.statement) ->
                                match stmt.stmt with
                                | AST.ImplDef impl_def when impl_def.impl_type_params = [] -> (
                                    match Annotation.type_expr_to_mono_type impl_def.impl_for_type with
                                    | Ok for_type ->
                                        Trait_registry.predeclare_impl_header
                                          {
                                            Trait_registry.impl_trait_name = impl_def.impl_trait_name;
                                            impl_type_params = [];
                                            impl_for_type = for_type;
                                            impl_methods = [];
                                          }
                                    | Error _ -> ())
                                | _ -> ())
                              program
                          in
                          predeclare_top_level_impl_headers expanded_program;
                          let type_map = create_type_map () in
                          let register_top_level_declaration
                              seen_traits seen_enums seen_aliases seen_types seen_shapes (stmt : AST.statement) =
                            match stmt.stmt with
                            | AST.TypeAlias alias_def ->
                                if StringSet.mem alias_def.alias_name seen_aliases then
                                  Error
                                    (error ~code:"type-constructor"
                                       ~message:
                                         (Printf.sprintf "Duplicate type definition: %s" alias_def.alias_name))
                                else
                                  Ok
                                    ( seen_traits,
                                      seen_enums,
                                      StringSet.add alias_def.alias_name seen_aliases,
                                      seen_types,
                                      seen_shapes )
                            | AST.TypeDef type_def ->
                                if StringSet.mem type_def.type_name seen_types then
                                  Error
                                    (error ~code:"type-constructor"
                                       ~message:
                                         (Printf.sprintf "Duplicate type definition: %s" type_def.type_name))
                                else
                                  Ok
                                    ( seen_traits,
                                      seen_enums,
                                      seen_aliases,
                                      StringSet.add type_def.type_name seen_types,
                                      seen_shapes )
                            | AST.ShapeDef shape_def ->
                                if StringSet.mem shape_def.shape_name seen_shapes then
                                  Error
                                    (error ~code:"type-constructor"
                                       ~message:
                                         (Printf.sprintf "Duplicate shape definition: %s" shape_def.shape_name))
                                else
                                  Ok
                                    ( seen_traits,
                                      seen_enums,
                                      seen_aliases,
                                      seen_types,
                                      StringSet.add shape_def.shape_name seen_shapes )
                            | AST.TraitDef trait_def ->
                                if StringSet.mem trait_def.name seen_traits then
                                  Error
                                    (error ~code:"type-constructor"
                                       ~message:(Printf.sprintf "Duplicate trait definition: %s" trait_def.name))
                                else
                                  Ok
                                    ( StringSet.add trait_def.name seen_traits,
                                      seen_enums,
                                      seen_aliases,
                                      seen_types,
                                      seen_shapes )
                            | AST.EnumDef enum_def ->
                                if StringSet.mem enum_def.name seen_enums then
                                  Error
                                    (error ~code:"type-constructor"
                                       ~message:(Printf.sprintf "Duplicate type definition: %s" enum_def.name))
                                else
                                  Ok
                                    ( seen_traits,
                                      StringSet.add enum_def.name seen_enums,
                                      seen_aliases,
                                      seen_types,
                                      seen_shapes )
                            | _ -> Ok (seen_traits, seen_enums, seen_aliases, seen_types, seen_shapes)
                          in
                          let infer_top_level_stmt env (stmt : AST.statement) =
                            match stmt.stmt with
                            | AST.Let let_binding ->
                                infer_let ~prefer_existing_self:true type_map env let_binding.name
                                  let_binding.value let_binding.type_annotation
                            | _ -> infer_statement type_map env stmt
                          in
                          let add_let_binding env (stmt : AST.statement) stmt_type =
                            match stmt.stmt with
                            | AST.Let let_binding ->
                                if let_binding.name = "_" then
                                  env
                                else
                                  let binding_type =
                                    binding_type_for_env ~value_expr:let_binding.value
                                      ~type_annotation:let_binding.type_annotation stmt_type
                                  in
                                  (match current_top_level_placeholder_type env let_binding.name with
                                  | Some _ -> set_top_level_placeholder let_binding.name binding_type
                                  | None -> ());
                                  let env_for_generalize = TypeEnv.remove let_binding.name env in
                                  let poly =
                                    if should_monomorphize_let_binding_value let_binding.value then
                                      mono_to_poly binding_type
                                    else
                                      generalize env_for_generalize binding_type
                                  in
                                  TypeEnv.add let_binding.name poly env
                            | _ -> env
                          in
                          let rec go
                              env
                              subst
                              seen_traits
                              seen_enums
                              seen_aliases
                              seen_types
                              seen_shapes
                              (stmts : AST.statement list) =
                            match stmts with
                            | [] -> Ok (env, subst, TNull)
                            | [ stmt ] -> (
                                match
                                  register_top_level_declaration seen_traits seen_enums seen_aliases seen_types
                                    seen_shapes stmt
                                with
                                | Error e -> Error e
                                | Ok (_seen_traits', _seen_enums', _seen_aliases', _seen_types', _seen_shapes')
                                  -> (
                                    match infer_top_level_stmt env stmt with
                                    | Error e -> Error e
                                    | Ok (stmt_subst, stmt_type) ->
                                        let final_subst = compose_substitution subst stmt_subst in
                                        let env' = apply_substitution_env final_subst env in
                                        let stmt_type' = apply_substitution final_subst stmt_type in
                                        let env'' = add_let_binding env' stmt stmt_type' in
                                        Ok (env'', final_subst, stmt_type')))
                            | stmt :: rest -> (
                                match
                                  register_top_level_declaration seen_traits seen_enums seen_aliases seen_types
                                    seen_shapes stmt
                                with
                                | Error e -> Error e
                                | Ok (seen_traits', seen_enums', seen_aliases', seen_types', seen_shapes') -> (
                                    match infer_top_level_stmt env stmt with
                                    | Error e -> Error e
                                    | Ok (stmt_subst, stmt_type) ->
                                        let subst' = compose_substitution subst stmt_subst in
                                        let env' = apply_substitution_env stmt_subst env in
                                        let env'' = add_let_binding env' stmt stmt_type in
                                        go env'' subst' seen_traits' seen_enums' seen_aliases' seen_types'
                                          seen_shapes' rest))
                          in
                          match predeclare_top_level_lets env expanded_program with
                          | Error e -> Error e
                          | Ok env_with_placeholders -> (
                              match
                                go env_with_placeholders empty_substitution StringSet.empty StringSet.empty
                                  StringSet.empty StringSet.empty StringSet.empty expanded_program
                              with
                              | Error e -> Error e
                              | Ok (env', final_subst, result_type) ->
                                  apply_substitution_type_map final_subst type_map;
                                  apply_substitution_method_type_args_store final_subst;
                                  Ok (env', type_map, result_type))))))))

module Test = struct
  (* Helper to parse and infer *)
  let infer_string code =
    match Syntax.Parser.parse ~file_id:"<test>" code with
    | Error errs ->
        let msg = String.concat "; " (List.map (fun (d : Diagnostics.Diagnostic.t) -> d.message) errs) in
        Printf.printf "Parse errors: %s\n" msg;
        Error (error ~code:"type-unbound-var" ~message:("Unbound variable: " ^ "parse error: " ^ msg))
    | Ok program -> infer_program program

  (* Helper to check inferred type *)
  let infers_to code expected_type =
    match infer_string code with
    | Error e ->
        Printf.printf "Error: %s\n" e.message;
        false
    | Ok (_, _type_map, t) ->
        if t = expected_type then
          true
        else (
          Printf.printf "Expected %s but got %s\n" (to_string expected_type) (to_string t);
          false)

  let contains_substring s sub = String_utils.contains_substring ~needle:sub s
  let is_code (diag : Diagnostic.t) (code : string) : bool = String.equal diag.code code

  let diagnostic_has_file_id (diag : Diagnostic.t) (target : string) : bool =
    let rec first_primary = function
      | [] -> None
      | { Diagnostic.primary = true; span = Diagnostic.NoSpan; _ } :: rest -> first_primary rest
      | { Diagnostic.primary = true; span = Diagnostic.Span span; _ } :: _ -> Some span.file_id
      | _ :: rest -> first_primary rest
    in
    let rec first_any = function
      | [] -> None
      | { Diagnostic.span = Diagnostic.NoSpan; _ } :: rest -> first_any rest
      | { Diagnostic.span = Diagnostic.Span span; _ } :: _ -> Some span.file_id
    in
    let file_id =
      Option.value (first_primary diag.labels) ~default:(Option.value (first_any diag.labels) ~default:"")
    in
    String.equal file_id target

  let rec identifier_occurrences_in_expr (name : string) (expr : AST.expression) : (int * int) list =
    match expr.expr with
    | AST.Identifier n when n = name -> [ (expr.id, expr.pos) ]
    | AST.Identifier _ | AST.Integer _ | AST.Float _ | AST.Boolean _ | AST.String _ -> []
    | AST.Array xs -> List.concat_map (identifier_occurrences_in_expr name) xs
    | AST.Index (a, b) -> identifier_occurrences_in_expr name a @ identifier_occurrences_in_expr name b
    | AST.TypeApply (callee, _) -> identifier_occurrences_in_expr name callee
    | AST.Hash pairs ->
        List.concat_map
          (fun (k, v) -> identifier_occurrences_in_expr name k @ identifier_occurrences_in_expr name v)
          pairs
    | AST.Prefix (_, e) -> identifier_occurrences_in_expr name e
    | AST.Infix (l, _, r) -> identifier_occurrences_in_expr name l @ identifier_occurrences_in_expr name r
    | AST.TypeCheck (e, _) -> identifier_occurrences_in_expr name e
    | AST.If (cond, cons, alt) -> (
        identifier_occurrences_in_expr name cond
        @ identifier_occurrences_in_stmt name cons
        @
        match alt with
        | None -> []
        | Some s -> identifier_occurrences_in_stmt name s)
    | AST.Function fn_expr -> identifier_occurrences_in_stmt name fn_expr.body
    | AST.Call (f, args) ->
        identifier_occurrences_in_expr name f @ List.concat_map (identifier_occurrences_in_expr name) args
    | AST.EnumConstructor (_, _, args) -> List.concat_map (identifier_occurrences_in_expr name) args
    | AST.Match (scrutinee, arms) ->
        identifier_occurrences_in_expr name scrutinee
        @ List.concat_map (fun (arm : AST.match_arm) -> identifier_occurrences_in_expr name arm.body) arms
    | AST.RecordLit (fields, spread) -> (
        List.concat_map
          (fun (f : AST.record_field) ->
            match f.field_value with
            | None -> []
            | Some v -> identifier_occurrences_in_expr name v)
          fields
        @
        match spread with
        | None -> []
        | Some e -> identifier_occurrences_in_expr name e)
    | AST.FieldAccess (receiver, _) -> identifier_occurrences_in_expr name receiver
    | AST.MethodCall { mc_receiver; mc_args; _ } ->
        identifier_occurrences_in_expr name mc_receiver
        @ List.concat_map (identifier_occurrences_in_expr name) mc_args
    | AST.BlockExpr stmts -> List.concat_map (identifier_occurrences_in_stmt name) stmts

  and identifier_occurrences_in_stmt (name : string) (stmt : AST.statement) : (int * int) list =
    match stmt.stmt with
    | AST.ExportDecl _ | AST.ImportDecl _ -> []
    | AST.ExpressionStmt e | AST.Return e -> identifier_occurrences_in_expr name e
    | AST.Block stmts -> List.concat_map (identifier_occurrences_in_stmt name) stmts
    | AST.Let let_binding -> identifier_occurrences_in_expr name let_binding.value
    | AST.ImplDef impl_def ->
        List.concat_map
          (fun (m : AST.method_impl) -> identifier_occurrences_in_stmt name m.impl_method_body)
          impl_def.impl_methods
    | AST.InherentImplDef impl_def ->
        List.concat_map
          (fun (m : AST.method_impl) -> identifier_occurrences_in_stmt name m.impl_method_body)
          impl_def.inherent_methods
    | AST.EnumDef _ | AST.TypeDef _ | AST.ShapeDef _ | AST.TraitDef _ | AST.DeriveDef _ | AST.TypeAlias _ -> []

  let identifier_occurrences_in_program (name : string) (program : AST.program) : (int * int) list =
    List.concat_map (identifier_occurrences_in_stmt name) program

  let find_symbol_id_by_name_kind (state : inference_state) (name : string) (kind : symbol_kind) :
      symbol_id option =
    symbol_table_bindings_in_state state
    |> List.find_map (fun (sid, (sym : symbol)) ->
           if sym.name = name && sym.kind = kind then
             Some sid
           else
             None)

  let%test "infer integer literal" = infers_to "42" TInt
  let%test "infer float literal" = infers_to "3.14" TFloat
  let%test "infer boolean literal" = infers_to "true" TBool && infers_to "false" TBool
  let%test "infer string literal" = infers_to "\"hello\"" TString

  let%test "infer arithmetic" =
    infers_to "1 + 2" TInt && infers_to "1 - 2" TInt && infers_to "2 * 3" TInt && infers_to "4 / 2" TInt

  let%test "infer modulo arithmetic" = infers_to "10 % 3" TInt
  let%test "infer float arithmetic" = infers_to "1.0 + 2.0" TFloat && infers_to "3.14 * 2.0" TFloat

  let%test "infer comparison" =
    infers_to "1 < 2" TBool && infers_to "1 > 2" TBool && infers_to "1 == 2" TBool && infers_to "1 != 2" TBool

  let%test "infer logical operators" = infers_to "true && false" TBool && infers_to "true || false" TBool
  let%test "infer prefix operators" = infers_to "!true" TBool && infers_to "-5" TInt && infers_to "-3.14" TFloat
  let%test "infer if expression" = infers_to "if (true) { 1 } else { 2 }" TInt

  let%test "infer simple function" =
    (* (x) -> x + 1 should be Int -> Int *)
    infers_to "(x) -> x + 1" (tfun TInt TInt)

  let%test "infer identity function" =
    (* (x) -> x should be t0 -> t0 (polymorphic) *)
    match infer_string "(x) -> x" with
    | Error _ -> false
    | Ok (_, _type_map, TFun (TVar a, TVar b, _)) -> a = b (* same type variable *)
    | Ok _ -> false

  let%test "infer two-arg function" =
    (* (x, y) -> x + y should have both args same type and return same type *)
    (* Without type classes, we can't constrain to just numeric types *)
    match infer_string "(x, y) -> x + y" with
    | Error _ -> false
    | Ok (_, _type_map, TFun (TVar a, TFun (TVar b, TVar c, _), _)) ->
        (* All three type vars should be the same *)
        a = b && b = c
    | Ok _ -> false

  let%test "infer two-arg function with literal" =
    (* (x, y) -> x + y + 1 should be (Int, Int) -> Int because of the literal *)
    infers_to "(x, y) -> x + y + 1" (tfun TInt (tfun TInt TInt))

  let%test "infer function call" =
    (* ((x) -> x + 1)(5) should be Int *)
    infers_to "((x) -> x + 1)(5)" TInt

  let%test "infer let binding" =
    (* let x = 5; x should be Int *)
    infers_to "let x = 5; x" TInt

  let%test "infer underscore let binding acts as discard" =
    (* let _ = 5; 1 should still infer as Int and not bind '_' *)
    infers_to "let _ = 5; 1" TInt

  let%test "infer discard binding does not shadow placeholder identity section" =
    infers_to "let _ = 5\nlet id = _\nid(1)" TInt

  let%test "infer let with function" =
    (* fn f(x) = x + 1; f(5) should be Int *)
    infers_to "fn f(x) = x + 1\nf(5)" TInt

  let%test "mixed explicit generic and shorthand params stay independent" =
    infers_to
      "shape Named = { name: Str }\nfn pair[a: Named](left: a, right: Named) -> Str = left.name + \":\" + right.name\nlet left = { name: \"alpha\", id: 1 }\nlet right = { name: \"beta\", bananas: 3 }\npair(left, right)"
      TString

  let%test "explicit generic name t0 does not alias shorthand-generated binders" =
    infers_to
      "shape Named = { name: Str }\nfn pair[t0: Named](left: t0, right: Named) -> Str = left.name + \":\" + right.name\nlet left = { name: \"alpha\", id: 1 }\nlet right = { name: \"beta\", bananas: 3 }\npair(left, right)"
      TString

  let%test "infer array literal" =
    (* [1, 2, 3] should be [Int] *)
    infers_to "[1, 2, 3]" (TArray TInt)

  let%test "infer array index" =
    (* [1, 2, 3][0] should be Int *)
    infers_to "[1, 2, 3][0]" TInt

  let%test "infer hash literal" =
    (* {"a": 1} should be {String: Int} *)
    infers_to "{\"a\": 1}" (THash (TString, TInt))

  let%test "infer hash index" =
    (* {"a": 1}["a"] should be Int *)
    infers_to "{\"a\": 1}[\"a\"]" TInt

  let%test "infer record literal" =
    match infer_string "let p = { x: 1, y: \"s\" }; p" with
    | Ok (_, _, TRecord (fields, None)) ->
        List.length fields = 2
        && List.exists (fun (f : Types.record_field_type) -> f.name = "x" && f.typ = TInt) fields
        && List.exists (fun (f : Types.record_field_type) -> f.name = "y" && f.typ = TString) fields
    | _ -> false

  let%test "infer record field access" = infers_to "let p = { x: 1, y: 2 }; p.x + p.y" TInt

  (* Regression: return type annotation with unresolved record field type variable.
     fn f(value) -> Str = value.name should succeed — the field type var unifies with string. *)
  let%test "return annotation unifies with record field type var" =
    infers_to "fn f(r) -> Str = r.name\nf({ name: \"hi\" })" TString

  let%test "infer transparent type for record annotation" =
    infers_to "type Point = { x: Int, y: Int }\nlet p: Point = { x: 1, y: 2 }\np.x" TInt

  let%test "top-level let annotation can forward-reference a later type" =
    infers_to "let p: Point = { x: 1, y: 2 }\ntype Point = { x: Int, y: Int }\np.x" TInt

  let%test "top-level fn signature can forward-reference a later type" =
    infers_to "fn get_x(p: Point) -> Int = p.x\ntype Point = { x: Int, y: Int }\nget_x({ x: 1, y: 2 })" TInt

  let%test "infer generic transparent type annotation" =
    infers_to "type Box[a] = { value: a }\nlet p: Box[Str] = { value: \"ok\" }\np.value" TString

  let%test "duplicate trait definition in one program is rejected" =
    match
      infer_string "trait Ping[a] = { fn ping(x: a) -> Int }\ntrait Ping[a] = { fn pong(x: a) -> Int }\n1"
    with
    | Error diag when is_code diag "type-constructor" ->
        let msg = diag.message in
        let needle = "Duplicate trait definition: Ping" in
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
        has_needle 0
    | _ -> false

  let%test "duplicate enum definition in one program is rejected" =
    match infer_string "enum Dup = { A }\nenum Dup = { B }\n1" with
    | Error diag when is_code diag "type-constructor" ->
        let msg = diag.message in
        let needle = "Duplicate type definition: Dup" in
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
        has_needle 0
    | _ -> false

  let%test "duplicate type definition in one program is rejected" =
    match infer_string "type Point = { x: Int }\ntype Point = { y: Int }\n1" with
    | Error diag when is_code diag "type-constructor" ->
        let msg = diag.message in
        let needle = "Duplicate type definition: Point" in
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
        has_needle 0
    | _ -> false

  let%test "top-level forward reference to later function infers" =
    infers_to "let y = add1(41)\nfn add1(x: Int) -> Int = x + 1\ny" TInt

  let%test "predeclare_top_level_lets includes standalone placeholder sections" =
    match Syntax.Parser.parse ~file_id:"<test>" "let y = add1(41)\nlet add1 = _ + 1\ny" with
    | Error _ -> false
    | Ok program -> (
        match predeclare_top_level_lets empty_env program with
        | Error _ -> false
        | Ok env -> TypeEnv.mem "add1" env)

  let%test "top-level forward reference to later placeholder section infers" =
    infers_to "let y = add1(41)\nlet add1 = _ + 1\ny" TInt

  let%test "top-level forward reference to later non-function value is rejected" =
    match infer_string "let a = b; let b = 1; a" with
    | Error diag -> is_code diag "type-unbound-var" && contains_substring diag.message "Unbound variable: b"
    | _ -> false

  let%test "top-level mutual recursion with forward references infers" =
    infers_to
      "fn even(n: Int) -> Bool = if (n == 0) { true } else { odd(n - 1) }\nfn odd(n: Int) -> Bool = if (n == 0) { false } else { even(n - 1) }\neven(4)"
      TBool

  let%test "symbol resolution maps forward top-level function references to declaration symbols" =
    match Syntax.Parser.parse ~file_id:"<test>" "let y = add1(41)\nfn add1(x: Int) -> Int = x + 1\ny" with
    | Error _ -> false
    | Ok program -> (
        let state = create_inference_state () in
        match infer_program ~state program with
        | Error _ -> false
        | Ok _ -> (
            let add1_refs = identifier_occurrences_in_program "add1" program in
            let add1_symbol = find_symbol_id_by_name_kind state "add1" TopLevelLet in
            match (add1_refs, add1_symbol) with
            | [ (add1_ref_id, _) ], Some add1_sid ->
                lookup_identifier_symbol_in_state state add1_ref_id = Some add1_sid
            | _ -> false))

  let%test "symbol resolution maps forward top-level placeholder section references to declaration symbols" =
    match Syntax.Parser.parse ~file_id:"<test>" "let y = add1(41)\nlet add1 = _ + 1\ny" with
    | Error _ -> false
    | Ok program -> (
        let state = create_inference_state () in
        match infer_program ~state program with
        | Error _ -> false
        | Ok _ -> (
            let add1_refs = identifier_occurrences_in_program "add1" program in
            let add1_symbol = find_symbol_id_by_name_kind state "add1" TopLevelLet in
            match (add1_refs, add1_symbol) with
            | [ (add1_ref_id, _) ], Some add1_sid ->
                lookup_identifier_symbol_in_state state add1_ref_id = Some add1_sid
            | _ -> false))

  let%test "symbol resolution leaves forward top-level value refs unresolved" =
    match Syntax.Parser.parse ~file_id:"<test>" "let a = b; let b = 1; a" with
    | Error _ -> false
    | Ok program -> (
        let state = create_inference_state () in
        match infer_program ~state program with
        | Error diag when is_code diag "type-unbound-var" && contains_substring diag.message "Unbound variable: b"
          -> (
            let b_refs = identifier_occurrences_in_program "b" program in
            let b_symbol = find_symbol_id_by_name_kind state "b" TopLevelLet in
            match (b_refs, b_symbol) with
            | [ (b_ref_id, _) ], Some _ -> lookup_identifier_symbol_in_state state b_ref_id = None
            | _ -> false)
        | _ -> false)

  let%test "symbol resolution maps enum constructor receiver after enum definition" =
    match Syntax.Parser.parse ~file_id:"<test>" "enum Direction = { North }\nlet x = Direction.North\nx" with
    | Error _ -> false
    | Ok program -> (
        let state = create_inference_state () in
        match infer_program ~state program with
        | Error _ -> false
        | Ok _ -> (
            let direction_refs = identifier_occurrences_in_program "Direction" program in
            let enum_symbol = find_symbol_id_by_name_kind state "Direction" EnumSym in
            match (direction_refs, enum_symbol) with
            | [ (direction_ref_id, _) ], Some enum_sid ->
                lookup_identifier_symbol_in_state state direction_ref_id = Some enum_sid
            | _ -> false))

  let%test "symbol resolution leaves forward enum receiver refs unresolved" =
    match Syntax.Parser.parse ~file_id:"<test>" "let x = Direction.North\nenum Direction = { North }\nx" with
    | Error _ -> false
    | Ok program -> (
        let state = create_inference_state () in
        let _ = infer_program ~state program in
        let direction_refs = identifier_occurrences_in_program "Direction" program in
        match direction_refs with
        | [ (direction_ref_id, _) ] -> lookup_identifier_symbol_in_state state direction_ref_id = None
        | _ -> false)

  let%test "duplicate top-level let definition is rejected" =
    match infer_string "let x = 1; let x = 2; x" with
    | Error diag ->
        is_code diag "type-constructor" && contains_substring diag.message "Duplicate top-level let definition: x"
    | _ -> false

  let%test "symbol resolution maps top-level identifiers to top-level symbols" =
    match Syntax.Parser.parse ~file_id:"<test>" "let x = 1; let y = x; y" with
    | Error _ -> false
    | Ok program -> (
        let state = create_inference_state () in
        match infer_program ~state program with
        | Error _ -> false
        | Ok _ -> (
            let x_refs = identifier_occurrences_in_program "x" program in
            let y_refs = identifier_occurrences_in_program "y" program in
            let x_symbol = find_symbol_id_by_name_kind state "x" TopLevelLet in
            let y_symbol = find_symbol_id_by_name_kind state "y" TopLevelLet in
            match (x_refs, y_refs, x_symbol, y_symbol) with
            | [ (x_ref_id, _) ], [ (y_ref_id, _) ], Some x_sid, Some y_sid -> (
                lookup_identifier_symbol_in_state state x_ref_id = Some x_sid
                && lookup_identifier_symbol_in_state state y_ref_id = Some y_sid
                &&
                match lookup_symbol_in_state state x_sid with
                | Some (sym : symbol) -> sym.kind = TopLevelLet && sym.name = "x"
                | None -> false)
            | _ -> false))

  let%test "symbol resolution prefers inner parameter over top-level binding when shadowed" =
    match Syntax.Parser.parse ~file_id:"<test>" "let x = 1\nfn f(x: Int) -> Int = x\nf(2)\nx" with
    | Error _ -> false
    | Ok program -> (
        let state = create_inference_state () in
        match infer_program ~state program with
        | Error _ -> false
        | Ok _ -> (
            let x_refs =
              identifier_occurrences_in_program "x" program
              |> List.sort (fun (_, p1) (_, p2) -> Int.compare p1 p2)
            in
            let top_level_symbol = find_symbol_id_by_name_kind state "x" TopLevelLet in
            let param_symbol = find_symbol_id_by_name_kind state "x" Param in
            match (x_refs, top_level_symbol, param_symbol) with
            | [ (inner_x_id, _); (outer_x_id, _) ], Some top_sid, Some param_sid ->
                lookup_identifier_symbol_in_state state inner_x_id = Some param_sid
                && lookup_identifier_symbol_in_state state outer_x_id = Some top_sid
            | _ -> false))

  let%test "builtin identifiers are tracked as builtin symbols when resolved from prelude env" =
    match Syntax.Parser.parse ~file_id:"<test>" "puts(1)" with
    | Error _ -> false
    | Ok program -> (
        let state = create_inference_state () in
        let env = TypeEnv.add "puts" (mono_to_poly (TFun (TInt, TInt, true))) empty_env in
        match infer_program ~state ~env program with
        | Error _ -> false
        | Ok _ -> (
            let puts_refs = identifier_occurrences_in_program "puts" program in
            let puts_builtin = find_symbol_id_by_name_kind state "puts" BuiltinValue in
            match (puts_refs, puts_builtin) with
            | [ (puts_ref_id, _) ], Some sid -> (
                lookup_identifier_symbol_in_state state puts_ref_id = Some sid
                &&
                match lookup_symbol_in_state state sid with
                | Some (sym : symbol) -> sym.kind = BuiltinValue
                | None -> false)
            | _ -> false))

  let%test "top-level let can shadow prelude builtin in symbol resolution" =
    match Syntax.Parser.parse ~file_id:"<test>" "fn puts(x: Int) -> Int = x\nputs(1)" with
    | Error _ -> false
    | Ok program -> (
        let state = create_inference_state () in
        let env = TypeEnv.add "puts" (mono_to_poly (TFun (TInt, TInt, true))) empty_env in
        match infer_program ~state ~env program with
        | Error _ -> false
        | Ok _ -> (
            let puts_refs = identifier_occurrences_in_program "puts" program in
            let puts_builtin = find_symbol_id_by_name_kind state "puts" BuiltinValue in
            let puts_top_level = find_symbol_id_by_name_kind state "puts" TopLevelLet in
            match (puts_refs, puts_builtin, puts_top_level) with
            | [ (puts_ref_id, _) ], Some builtin_sid, Some top_sid ->
                lookup_identifier_symbol_in_state state puts_ref_id = Some top_sid && top_sid <> builtin_sid
            | _ -> false))

  let%test "symbol ids are stable across independent inference runs for identical source spans" =
    match Syntax.Parser.parse ~file_id:"main.mr" "let x = 1; let y = x; y" with
    | Error _ -> false
    | Ok program -> (
        let state1 = create_inference_state () in
        let state2 = create_inference_state () in
        match (infer_program ~state:state1 program, infer_program ~state:state2 program) with
        | Ok _, Ok _ ->
            let top_level_signature state =
              symbol_table_bindings_in_state state
              |> List.filter_map (fun (_sid, (sym : symbol)) ->
                     if sym.kind = TopLevelLet then
                       Some (sym.name, sym.id, sym.definition_pos, sym.definition_end_pos, sym.file_id)
                     else
                       None)
              |> List.sort compare
            in
            top_level_signature state1 = top_level_signature state2
        | _ -> false)

  let%test "constrained field access preserves non-field trait obligations at call sites" =
    match
      infer_string
        "shape Named = { name: Str }\ntrait Shown[a] = { fn show(x: a) -> Str }\nfn get_name[t: Named & Shown](x: t) -> Str = x.name\nlet p = { name: \"alice\" }\nget_name(p)"
    with
    | Error diag when String_utils.contains_substring ~needle:"type-trait" diag.code ->
        String_utils.contains_substring ~needle:"Constraint obligation failed" diag.message
        && String_utils.contains_substring ~needle:"does not implement trait" diag.message
    | _ -> false

  let%test "constrained field access rejects fields not guaranteed by constraints" =
    match infer_string "shape Named = { name: Str }\nfn get_age[t: Named](x: t) = x.age\n1" with
    | Error diag when is_code diag "type-constructor" ->
        let msg = diag.message in
        let needle = "Field 'age' is not guaranteed by constraints on type variable" in
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
        has_needle 0
    | _ -> false

  let%test "constrained field access works when all constraints are satisfied" =
    infers_to
      "type Person = { name: Str, age: Int }\nshape Named = { name: Str }\ntrait Shown[a] = { fn show(x: a) -> Str }\nimpl Shown[Person] = {\n  fn show(x: Person) -> Str = x.name\n}\nfn get_name[t: Named & Shown](x: t) -> Str = x.name\nlet p: Person = { name: \"alice\", age: 42 }\nget_name(p)"
      TString

  let%test "explicit generics and shorthand params stay independent at call sites" =
    infers_to
      "shape Named = { name: Str }\nfn pair[a: Named](left: a, right: Named) -> Str = left.name + \":\" + right.name\nlet left = { name: \"alpha\", id: 1 }\nlet right = { name: \"beta\", bananas: 3 }\npair(left, right)"
      TString

  let%test "typed arrow-lambda params accept constrained-param shorthand" =
    infers_to
      "shape Named = { name: Str }\nfn apply[a, b](x: a, f: (a) -> b) -> b = f(x)\nlet user = { name: \"mia\", id: 7 }\napply(user, (x: Named) -> x.name)"
      TString

  let%test "nested lambda annotations reuse outer generic binders" =
    infers_to
      "fn keep[a](x: a) -> a = {\n  let loop = (y: a) -> {\n    if (true) { y } else { loop(y) }\n  }\n  loop(x)\n}\nkeep(1)"
      TInt

  let%test "generic impl cannot specialize generic return type from body literals" =
    match
      infer_string
        "trait Id[a] = { fn id(x: a) -> a }\nimpl[b] Id[List[b]] = {\n  fn id(x: List[b]) -> List[b] = [1]\n}\n1"
    with
    | Error diag -> is_code diag "type-return-mismatch"
    | _ -> false

  let%test "explicit row-polymorphic annotation is rejected in v1" =
    reset_fresh_counter ();
    match infer_string "fn get_x(r: { x: Int, ...row }) -> Int = r.x" with
    | Error _ -> true
    | Ok _ -> false

  let%test "field access on unannotated record param works" =
    infers_to "let p = { x: 5, y: 10, z: 20 }\nfn get_x(r) = r.x\nget_x(p)" TInt

  let%test "row-polymorphic field accessor can be reused across distinct record tails" =
    infers_to "fn get_x(r) = r.x\nlet a = get_x({ x: 1, y: true })\nlet b = get_x({ x: 2, z: \"s\" })\na + b" TInt

  let%test "multiple field access with closed record annotation works" =
    infers_to "let p = { x: 5, y: 10 }\nfn sum_xy(r: { x: Int, y: Int }) = r.x + r.y\nsum_xy(p)" TInt

  let%test "infer record match pattern with punning" =
    infers_to "let p = { x: 10, y: 20 }\nmatch p { case { x:, y: }: x + y case _: 0 }" TInt

  let%test "infer record match pattern with rest binding" =
    infers_to "let p = { x: 10, y: 20, z: 30 }\nmatch p { case { x:, ...rest }: x + rest.y case _: 0 }" TInt

  let%test "record spread accepts transparent record base" =
    infers_to "type Point = { x: Int, y: Int }\nlet p: Point = { x: 1, y: 2 }\nlet q = { ...p, x: 10 }\nq.x + q.y"
      TInt

  let%test "transparent record annotation accepts spread update" =
    infers_to
      "type Point = { x: Int, y: Int }\nlet p: Point = { x: 1, y: 2 }\nlet q: Point = { ...p, x: 10 }\nq.x + q.y"
      TInt

  let%test "record pattern matches transparent record scrutinee" =
    infers_to
      "type Point = { x: Int, y: Int }\nlet p: Point = { x: 10, y: 20 }\nmatch p { case { x:, y: }: x + y case _: 0 }"
      TInt

  let%test "record pattern rest binding on transparent record is structural remainder" =
    infers_to
      "type Point = { x: Int, y: Int, z: Int }\nfn sum_rest(rest: { y: Int, z: Int }) -> Int = rest.y + rest.z\nlet p: Point = { x: 1, y: 2, z: 3 }\nmatch p { case { x:, ...rest }: x + sum_rest(rest) case _: 0 }"
      TInt

  let%test "single-arm record match is exhaustive" =
    infers_to "let p = { name: \"George\", age: 7 }\nmatch p { case { name:, ...rest }: name }" TString

  let%test "infer polymorphic let" =
    (* fn id(x) = x; id(5) should work and be Int *)
    infers_to "fn id(x) = x\nid(5)" TInt

  let%test "infer polymorphic let used twice" =
    (* fn id(x) = x; id(5); id(true) should work *)
    (* The result type is the type of the last expression: Bool *)
    infers_to "fn id(x) = x\nid(5)\nid(true)" TBool

  let%test "infer higher order function" =
    (* fn apply(f, x) = f(x); apply((n) -> n + 1, 5) should be Int *)
    infers_to "fn apply(f, x) = f(x)\napply((n) -> n + 1, 5)" TInt

  let%test "error on unbound variable" =
    match infer_string "x" with
    | Error diag -> is_code diag "type-unbound-var" && contains_substring diag.message "Unbound variable: x"
    | _ -> false

  let%test "union type from if with different branch types" =
    (* Phase 4.1: Different branch types create unions instead of errors *)
    match infer_string "if (true) { 1 } else { \"hello\" }" with
    | Ok (_, _, TUnion _) -> true
    | _ -> false

  let%test "error on non-bool condition" =
    match infer_string "if (5) { 1 }" with
    | Error diag -> is_code diag "type-if-condition"
    | _ -> false

  let%test "error on array element mismatch" =
    match infer_string "[1, \"hello\"]" with
    | Error diag -> is_code diag "type-array-element"
    | _ -> false

  let%test "infer simple recursive function" =
    (* Factorial: fn fact(n) = if (n == 0) { 1 } else { n * fact(n - 1) } *)
    let code = "fn fact(n) = if (n == 0) { 1 } else { n * fact(n - 1) }\nfact(5)" in
    infers_to code TInt

  let%test "infer recursive function type" =
    (* The factorial function should have type Int -> Int *)
    let code = "fn fact(n) = if (n == 0) { 1 } else { n * fact(n - 1) }\nfact" in
    infers_to code (tfun TInt TInt)

  let%test "infer fibonacci" =
    let code = "fn fib(n) = if (n < 2) { n } else { fib(n - 1) + fib(n - 2) }\nfib(10)" in
    infers_to code TInt

  let%test "infer mutually referencing let" =
    (* A let that references itself but isn't a function - should still work *)
    (* let x = x would be infinite, but let x = 5 should be fine *)
    infers_to "let x = 5; x" TInt

  let%test "infer countdown" =
    (* Recursive function that returns unit/last value *)
    let code = "fn countdown(n) = if (n == 0) { 0 } else { countdown(n - 1) }\ncountdown(10)" in
    infers_to code TInt

  let%test "infer recursive with array" =
    (* Recursive function that works with arrays *)
    let code = "fn sum(arr, i) = if (i == 0) { arr[0] } else { arr[i] + sum(arr, i - 1) }\nsum([1, 2, 3], 2)" in
    infers_to code TInt

  (* Enum constructor tests *)
  let%test "infer simple enum constructor" =
    let code = "enum Direction = { North, South, East, West }\nDirection.North" in
    infers_to code (TEnum ("Direction", []))

  let%test "infer option.some with int" =
    let code = "enum Option[a] = { Some(a), None }\nOption.Some(42)" in
    infers_to code (TEnum ("Option", [ TInt ]))

  let%test "infer option.none" =
    let code = "enum Option[a] = { Some(a), None }\nOption.None" in
    match infer_string code with
    | Error _ -> false
    | Ok (_, _type_map, t) -> (
        match t with
        | TEnum ("Option", [ TVar _ ]) -> true
        | _ ->
            Printf.printf "Expected Option[_] but got %s\n" (to_string t);
            false)

  let%test "infer result.success with int" =
    let code = "enum Result[a, e] = { Success(a), Failure(e) }\nResult.Success(42)" in
    match infer_string code with
    | Error _ -> false
    | Ok (_, _type_map, t) -> (
        match t with
        | TEnum ("Result", [ TInt; TVar _ ]) -> true
        | _ ->
            Printf.printf "Expected Result[Int, _] but got %s\n" (to_string t);
            false)

  let%test "infer result.failure with string" =
    let code = "enum Result[a, e] = { Success(a), Failure(e) }\nResult.Failure(\"error\")" in
    match infer_string code with
    | Error _ -> false
    | Ok (_, _type_map, t) -> (
        match t with
        | TEnum ("Result", [ TVar _; TString ]) -> true
        | _ ->
            Printf.printf "Expected Result[_, String] but got %s\n" (to_string t);
            false)

  let%test "infer constructor with wrong arg count" =
    let code = "enum Option[a] = { Some(a), None }\nOption.Some(1, 2)" in
    match infer_string code with
    | Error _ -> true
    | Ok _ -> false

  let%test "infer canonical sum type with recursive payloads" =
    let code = "type Expr = { Num(Int), Add(Expr, Expr) }\nlet expr = Expr.Add(Expr.Num(1), Expr.Num(2))\nexpr" in
    infers_to code (TEnum ("Expr", []))

  let%test "infer canonical sum type with record payloads" =
    let code = "type Event = { Click({ x: Int, y: Int }), Quit }\nEvent.Click({ x: 1, y: 2 })" in
    infers_to code (TEnum ("Event", []))

  (* Match expression tests *)
  let%test "infer simple match with option" =
    let code =
      "enum Option[a] = { Some(a), None }
let x = Option.Some(42)
match x {
  case Option.Some(v): v + 1
  case Option.None: 0
}"
    in
    infers_to code TInt

  let%test "infer match with wildcard" =
    let code = "match 5 {
  case 0: \"zero\"
  case _: \"other\"
}" in
    infers_to code TString

  let%test "infer match with variable pattern" =
    let code = "match 42 {
  case n: n + 1
}" in
    infers_to code TInt

  let%test "infer match extracts constructor value" =
    let code =
      "enum Result[a, e] = { Success(a), Failure(e) }
let r = Result.Success(100)
match r {
  case Result.Success(val): val * 2
  case Result.Failure(err): 0
}"
    in
    infers_to code TInt

  let%test "infer match with literal patterns" =
    let code = "let x = 5
match x {
  case 0: \"zero\"
  case 1: \"one\"
  case _: \"many\"
}" in
    infers_to code TString

  let%test "match expression union type from different arm types" =
    (* Phase 4.2: Different arm types create unions instead of errors *)
    let code = "match 5 {
  case 0: 42
  case _: \"string\"
}" in
    match infer_string code with
    | Ok (_, _, TUnion _) -> true
    | _ -> false

  (* Exhaustiveness checking tests *)
  let%test "non-exhaustive match on option is error" =
    let code =
      "enum Option[a] = { Some(a), None }
let x = Option.Some(42)
match x {
  case Option.Some(v): v
}"
    in
    match infer_string code with
    | Error e ->
        let msg = e.message in
        String.length msg > 0 && String.sub msg 0 (min 17 (String.length msg)) = "Non-exhaustive ma"
    | Ok _ -> false

  let%test "exhaustive match on option passes" =
    let code =
      "enum Option[a] = { Some(a), None }
let x = Option.Some(42)
match x {
  case Option.Some(v): v
  case Option.None: 0
}"
    in
    infers_to code TInt

  let%test "match with wildcard is exhaustive" =
    let code =
      "enum Result[a, e] = { Success(a), Failure(e) }
let r = Result.Success(100)
match r {
  case Result.Success(v): v
  case _: 0
}"
    in
    infers_to code TInt

  let%test "match with variable pattern is exhaustive" =
    let code = "enum Option[a] = { Some(a), None }
match Option.Some(5) {
  case x: 42
}" in
    infers_to code TInt

  let%test "non-exhaustive match on bool is error" =
    let code = "match true {
  case true: 1
}" in
    match infer_string code with
    | Error _ -> true
    | Ok _ -> false

  let%test "exhaustive match on bool passes" =
    let code = "match true {
  case true: 1
  case false: 0
}" in
    infers_to code TInt

  let%test "non-exhaustive match on result is error" =
    let code =
      "enum Result[a, e] = { Success(a), Failure(e) }
match Result.Success(42) {
  case Result.Success(v): v
}"
    in
    match infer_string code with
    | Error _ -> true
    | Ok _ -> false

  let%test "constraint store does not leak across independent runs" =
    let constrained_code =
      "trait Show[a] = {
  fn show(x: a) -> Str
}
fn f[a: Show](x: a) -> Str = Show.show(x)
f"
    in
    let unconstrained_code = "fn id(x) = x\nid([1, 2, 3])" in
    match infer_string constrained_code with
    | Error _ -> false
    | Ok _ -> (
        match infer_string unconstrained_code with
        | Error _ -> false
        | Ok (_, _, inferred_type) -> inferred_type = TArray TInt)

  let%test "string interpolation requires show for embedded values" =
    let code = "type Stone = { weight: Int }\nlet stone: Stone = { weight: 3 }\n\"stone #{stone}\"" in
    match infer_string code with
    | Error diag when is_code diag "type-trait-missing-impl" ->
        contains_substring diag.message "does not implement trait show"
    | _ -> false

  let%test "obligations_from_substitution creates one obligation per trait" =
    clear_constraint_store ();
    add_type_var_constraints "t0" [ "show"; "eq" ];
    let obligations = obligations_from_substitution (substitution_of_list [ ("t0", TInt) ]) in
    List.length obligations = 2
    && List.exists
         (fun (o : obligation) -> o.constraint_ref = Constraints.TraitConstraint "show" && o.typ = TInt)
         obligations
    && List.exists
         (fun (o : obligation) -> o.constraint_ref = Constraints.TraitConstraint "eq" && o.typ = TInt)
         obligations

  let%test "obligations_from_substitution orders canonical numeric variables naturally" =
    clear_constraint_store ();
    add_type_var_constraints "t2" [ "show" ];
    add_type_var_constraints "t10" [ "show" ];
    let obligations = obligations_from_substitution (substitution_of_list [ ("t10", TInt); ("t2", TString) ]) in
    let vars =
      List.map
        (fun (o : obligation) ->
          match o.reason with
          | GenericConstraint type_var_name -> type_var_name)
        obligations
    in
    vars = [ "t2"; "t10" ]

  let%test "obligations_from_substitution emits canonical variables before non-canonical names" =
    clear_constraint_store ();
    add_type_var_constraints "x" [ "show" ];
    add_type_var_constraints "r2" [ "show" ];
    add_type_var_constraints "t1" [ "show" ];
    let obligations =
      obligations_from_substitution (substitution_of_list [ ("x", TInt); ("r2", TString); ("t1", TBool) ])
    in
    let vars =
      List.map
        (fun (o : obligation) ->
          match o.reason with
          | GenericConstraint type_var_name -> type_var_name)
        obligations
    in
    vars = [ "t1"; "r2"; "x" ]

  let%test "obligations_from_substitution orders non-canonical names lexicographically" =
    clear_constraint_store ();
    add_type_var_constraints "beta" [ "show" ];
    add_type_var_constraints "alpha" [ "show" ];
    add_type_var_constraints "zeta" [ "show" ];
    let obligations =
      obligations_from_substitution (substitution_of_list [ ("zeta", TInt); ("alpha", TString); ("beta", TBool) ])
    in
    let vars =
      List.map
        (fun (o : obligation) ->
          match o.reason with
          | GenericConstraint type_var_name -> type_var_name)
        obligations
    in
    vars = [ "alpha"; "beta"; "zeta" ]

  let%test "verify_constraints_in_substitution includes obligation context in errors" =
    let contains_substring s sub = String_utils.contains_substring ~needle:sub s in
    clear_constraint_store ();
    add_type_var_constraints "t0" [ "show" ];
    match verify_constraints_in_substitution (substitution_of_list [ ("t0", tfun TInt TInt) ]) with
    | Ok () -> false
    | Error diag ->
        contains_substring diag.message "Constraint obligation failed"
        && contains_substring diag.message "type variable 't0'"

  let%test "add_type_var_constraints lowers field requirements from supertraits" =
    Trait_registry.clear ();
    Type_registry.clear ();
    Type_registry.register_shape
      {
        Type_registry.shape_name = "named";
        shape_type_params = [];
        shape_fields = [ { name = "name"; typ = TString } ];
      };
    Trait_registry.register_trait
      {
        trait_name = "labeled";
        trait_type_param = Some "a";
        trait_supertraits = [ "named" ];
        trait_methods =
          [ Trait_registry.mk_method_sig ~name:"label" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
      };
    clear_constraint_store ();
    add_type_var_constraints "t0" [ "labeled" ];
    let lowered = lookup_type_var_constrained_fields "t0" in
    List.exists
      (fun (_trait_name, (field : Types.record_field_type)) -> field.name = "name" && field.typ = TString)
      lowered

  let%test "add_type_var_constraints refreshes lowered fields after merged constraints" =
    Trait_registry.clear ();
    Type_registry.clear ();
    Type_registry.register_shape
      {
        Type_registry.shape_name = "named";
        shape_type_params = [];
        shape_fields = [ { name = "name"; typ = TString } ];
      };
    Type_registry.register_shape
      {
        Type_registry.shape_name = "aged";
        shape_type_params = [];
        shape_fields = [ { name = "age"; typ = TInt } ];
      };
    clear_constraint_store ();
    add_type_var_constraints "t0" [ "named" ];
    add_type_var_constraints "t0" [ "aged" ];
    let lowered = lookup_type_var_constrained_fields "t0" in
    List.exists
      (fun (_trait_name, (field : Types.record_field_type)) -> field.name = "name" && field.typ = TString)
      lowered
    && List.exists
         (fun (_trait_name, (field : Types.record_field_type)) -> field.name = "age" && field.typ = TInt)
         lowered

  let%test "clear_constraint_store clears lowered constrained field requirements" =
    Trait_registry.clear ();
    Type_registry.clear ();
    Type_registry.register_shape
      {
        Type_registry.shape_name = "named";
        shape_type_params = [];
        shape_fields = [ { name = "name"; typ = TString } ];
      };
    clear_constraint_store ();
    add_type_var_constraints "t0" [ "named" ];
    let had_fields = lookup_type_var_constrained_fields "t0" <> [] in
    clear_constraint_store ();
    had_fields && lookup_type_var_constrained_fields "t0" = []

  let%test "constrained type-variable dot call is rejected and suggests qualification" =
    let contains_substring s sub = String_utils.contains_substring ~needle:sub s in
    Trait_registry.clear ();
    Trait_registry.register_trait
      {
        trait_name = "T1";
        trait_type_param = Some "a";
        trait_supertraits = [];
        trait_methods =
          [ Trait_registry.mk_method_sig ~name:"render" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
      };
    Trait_registry.register_trait
      {
        trait_name = "T2";
        trait_type_param = Some "a";
        trait_supertraits = [];
        trait_methods =
          [ Trait_registry.mk_method_sig ~name:"render" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
      };
    match infer_string "fn f[a: T1 & T2](x: a) -> Str = x.render()\nf" with
    | Ok _ -> false
    | Error e ->
        let msg = e.message in
        contains_substring msg "'render' is not a callable field"
        && contains_substring msg "T1.render(...)"
        && contains_substring msg "T2.render(...)"

  let%test "dyn trait-object dot call is rejected" =
    let code =
      "trait Show[a] = { fn show(x: a) -> Str }\nimpl Show[Int] = { fn show(x: Int) -> Str = \"42\" }\nlet x: Dyn[Show] = 42\nx.show()"
    in
    match infer_string code with
    | Ok _ -> false
    | Error e -> String_utils.contains_substring ~needle:"'show' is not a callable field" e.message

  let%test "qualified dyn trait call ignores shape-only supertraits during witness dispatch" =
    let code =
      "shape Named = { name: Str }\ntrait NamedShow[a]: Named = { fn show(x: a) -> Str }\ntype Person = { name: Str }\nimpl NamedShow[Person] = { fn show(x: Person) -> Str = x.name }\nlet x: Dyn[NamedShow] = { name: \"alice\" }\nNamedShow.show(x)"
    in
    infers_to code TString

  let%test "type-qualified inherent call resolves for concrete receiver" =
    let code =
      "type Point = { x: Int, y: Int }\nimpl Point = { fn sum(p: Point) -> Int = p.x + p.y }\nlet p: Point = { x: 1, y: 2 }\nPoint.sum(p)"
    in
    infers_to code TInt

  let%test "concrete dot call does not fall back to top-level function" =
    let contains_substring s sub = String_utils.contains_substring ~needle:sub s in
    let code = "fn apply_twice[a](x: a, f: (a) -> a) -> a = f(f(x))\n1.apply_twice((x: Int) -> x * 2)" in
    match infer_string code with
    | Ok _ -> false
    | Error e ->
        let msg = e.message in
        contains_substring msg "'apply_twice' is not a callable field" && contains_substring msg "Int"

  let%test "concrete dot call does not fall back to inherent impl" =
    let contains_substring s sub = String_utils.contains_substring ~needle:sub s in
    let code = "impl Int = { fn bump(x: Int) -> Int = x + 1 }\n1.bump()" in
    match infer_string code with
    | Ok _ -> false
    | Error e ->
        let msg = e.message in
        contains_substring msg "'bump' is not a callable field" && contains_substring msg "Int.bump"

  let%test "concrete dot call does not fall back to trait impl" =
    let contains_substring s sub = String_utils.contains_substring ~needle:sub s in
    let code =
      "trait Label[a] = { fn label(x: a) -> Str }\nimpl Label[Int] = { fn label(x: Int) -> Str = \"int\" }\n1.label()"
    in
    match infer_string code with
    | Ok _ -> false
    | Error e ->
        let msg = e.message in
        contains_substring msg "'label' is not a callable field" && contains_substring msg "Label.label"

  let%test "callable field invocation remains available on concrete receiver" =
    let code = "type Runner = { run: (Int) -> Int }\nlet r: Runner = { run: (x: Int) -> x * 2 }\nr.run(10)" in
    infers_to code TInt

  let%test "type-qualified inherent call resolves for type-application receiver" =
    let code =
      "type Opt[a] = { Some(a), None }\nimpl[a] Opt[a] = { fn is_some(o: Opt[a]) -> Bool = match o { case Opt.Some(_): true case Opt.None: false } }\nlet opt = Opt.Some(1)\nOpt.is_some(opt)"
    in
    infers_to code TBool

  let%test "inherent method receiver must match impl target type" =
    let contains_substring s sub = String_utils.contains_substring ~needle:sub s in
    let code = "type Point = { x: Int }\nimpl Point = { fn bad(x: Int) -> Int = x }\n1" in
    match infer_string code with
    | Ok _ -> false
    | Error e ->
        let msg = e.message in
        contains_substring msg "receiver type" && contains_substring msg "does not match impl target type"

  let%test "duplicate inherent method for same type is rejected" =
    let contains_substring s sub = String_utils.contains_substring ~needle:sub s in
    let code = "impl Int = { fn ping(x: Int) -> Int = x }\nimpl Int = { fn ping(x: Int) -> Int = x }\n1" in
    match infer_string code with
    | Ok _ -> false
    | Error e ->
        let msg = e.message in
        contains_substring msg "Duplicate inherent method 'ping'"

  let%test "inherent method coexists with trait method on same type (Phase 4.6)" =
    Trait_registry.clear ();
    let code =
      "trait Show[a] = { fn show(x: a) -> Str }\nimpl Show[Int] = { fn show(x: Int) -> Str = \"trait\" }\nimpl Int = { fn show(x: Int) -> Str = \"inherent\" }\n1"
    in
    match infer_string code with
    | Ok _ -> true
    | Error _ -> false

  let%test "inherent methods do not satisfy trait constraints" =
    let contains_substring s sub = String_utils.contains_substring ~needle:sub s in
    Trait_registry.clear ();
    let code =
      "trait Show[a] = { fn show(x: a) -> Str }\ntype Point = { x: Int }\nimpl Point = { fn show(p: Point) -> Str = \"p\" }\nfn f[t: Show](x: t) -> Str = Show.show(x)\nlet p: Point = { x: 1 }\nf(p)"
    in
    match infer_string code with
    | Ok _ -> false
    | Error e ->
        let msg = e.message in
        contains_substring msg "does not implement trait"

  let%test "infer_program isolates itself from stale global constraint state" =
    (* Simulate stale process-global state from an earlier session. *)
    clear_constraint_store ();
    reset_fresh_counter ();
    add_type_var_constraints "t0" [ "show" ];
    let code = "((x) -> x)((y) -> y)" in
    match Syntax.Parser.parse ~file_id:"<test>" code with
    | Error _ -> false
    | Ok program -> (
        match infer_program program with
        | Error _ -> false
        | Ok (_, _, _) -> true)

  let%test "infer_program captures user generic names in inference state" =
    match
      Syntax.Parser.parse ~file_id:"<test>"
        "shape Named = { name: Str }\nfn get[t: Named](x: t) -> Str = x.name\nget"
    with
    | Error _ -> false
    | Ok program -> (
        let state = create_inference_state () in
        match infer_program ~state program with
        | Error _ -> false
        | Ok _ ->
            type_var_user_name_bindings_in_state state
            |> List.exists (fun (_fresh_name, user_name) -> user_name = "t"))

  let%test "infer_program clears stale user generic-name mappings for shared state" =
    let shared_state = create_inference_state () in
    match
      Syntax.Parser.parse ~file_id:"<test>"
        "shape Named = { name: Str }\nfn get[t: Named](x: t) -> Str = x.name\nget"
    with
    | Error _ -> false
    | Ok first_program -> (
        match infer_program ~state:shared_state first_program with
        | Error _ -> false
        | Ok _ -> (
            match Syntax.Parser.parse ~file_id:"<test>" "let x = 1; x" with
            | Error _ -> false
            | Ok second_program -> (
                match infer_program ~state:shared_state second_program with
                | Error _ -> false
                | Ok _ -> type_var_user_name_bindings_in_state shared_state = [])))

  let%test "reused env preserves constrained generic obligations across infer_program runs" =
    let contains_substring s sub = String_utils.contains_substring ~needle:sub s in
    Trait_registry.clear ();
    Trait_registry.register_trait
      {
        Trait_registry.trait_name = "Show";
        trait_type_param = Some "a";
        trait_supertraits = [];
        trait_methods =
          [ Trait_registry.mk_method_sig ~name:"show" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
      };
    Trait_registry.register_impl ~builtin:true
      {
        impl_trait_name = "Show";
        impl_type_params = [];
        impl_for_type = TInt;
        impl_methods =
          [ Trait_registry.mk_method_sig ~name:"show" ~params:[ ("x", TInt) ] ~return_type:TString () ];
      };
    let shared_state = create_inference_state () in
    match Syntax.Parser.parse ~file_id:"<test>" "fn check[a: Show](x: a) -> Str = Show.show(x)" with
    | Error _ -> false
    | Ok first_program -> (
        match infer_program ~state:shared_state first_program with
        | Error _ -> false
        | Ok (env_with_check, _, _) -> (
            match Syntax.Parser.parse ~file_id:"<test>" "check((y) -> y)" with
            | Error _ -> false
            | Ok second_program -> (
                match infer_program ~state:shared_state ~env:env_with_check second_program with
                | Ok _ -> false
                | Error e ->
                    let msg = e.message in
                    contains_substring msg "does not implement trait")))

  let%test "infer errors preserve parser file_id metadata" =
    match Syntax.Parser.parse ~file_id:"main.mr" "1 + true" with
    | Error _ -> false
    | Ok program -> (
        match infer_program program with
        | Error diag -> diagnostic_has_file_id diag "main.mr"
        | _ -> false)

  let%test "infer effectful function type uses fat arrow" =
    (* fn add1(x: Int) => Int = x + 1 should infer as Int => Int *)
    match infer_string "fn add1(x: Int) => Int = x + 1\nadd1" with
    | Error _ -> false
    | Ok (_, _, TFun (TInt, TInt, true)) -> true
    | Ok _ -> false

  let%test "infer pure function type uses thin arrow" =
    (* fn add1(x: Int) -> Int = x + 1 should infer as Int -> Int (not effectful) *)
    match infer_string "fn add1(x: Int) -> Int = x + 1\nadd1" with
    | Error _ -> false
    | Ok (_, _, TFun (TInt, TInt, false)) -> true
    | Ok _ -> false

  let%test "infer unannotated function is pure by default" =
    match infer_string "fn add1(x) = x + 1\nadd1" with
    | Error _ -> false
    | Ok (_, _, TFun (_, _, false)) -> true
    | Ok _ -> false

  (* Purity enforcement tests *)
  (* Note: infer_string uses empty_env (no builtins), so we define effectful
     functions inline using => to test purity enforcement *)

  let%test "pure function calling effectful operation is error" =
    (* Define an effectful function, then a pure function that calls it *)
    let code = "fn eff(x: Int) => Int = x\nfn pure(y: Int) -> Int = eff(y)\npure" in
    match infer_string code with
    | Error diag -> is_code diag "type-purity"
    | _ -> false

  let%test "pure function with pure body is ok" =
    match infer_string "fn add1(x: Int) -> Int = x + 1\nadd1" with
    | Ok (_, _, TFun (TInt, TInt, false)) -> true
    | _ -> false

  let%test "effectful annotation with pure body is ok (no enforcement yet)" =
    (* Case 2 is not enforced — => with pure body is allowed *)
    match infer_string "fn add1(x: Int) => Int = x + 1\nadd1" with
    | Ok (_, _, TFun (TInt, TInt, true)) -> true
    | _ -> false

  let%test "unannotated function calling effectful infers as effectful" =
    let code = "fn eff(x: Int) => Int = x\nfn caller(y: Int) = eff(y)\ncaller" in
    match infer_string code with
    | Ok (_, _, TFun (TInt, TInt, true)) -> true
    | _ -> false

  let%test "unannotated function with pure body infers as pure" =
    match infer_string "fn add1(x) = x + 1\nadd1" with
    | Ok (_, _, TFun (_, _, false)) -> true
    | _ -> false

  let%test "pure function calling effectful in let binding is error" =
    let code = "fn eff(x: Int) => Int = x\nfn pure(y: Int) -> Int = { let z = eff(y); z }\npure" in
    match infer_string code with
    | Error diag -> is_code diag "type-purity"
    | _ -> false

  let%test "pure function calling effectful in if branch is error" =
    let code = "fn eff(x: Int) => Int = x\nfn pure(y: Int) -> Int = if (true) { eff(y) } else { 0 }\npure" in
    match infer_string code with
    | Error diag -> is_code diag "type-purity"
    | _ -> false

  let%test "pure function calling effectful method is error" =
    let code =
      "type Logger = { prefix: Str }\nimpl Logger = {\n  fn log(l: Logger, msg: Str) => Str = { msg }\n}\nfn bad(l: Logger) -> Str = Logger.log(l, \"x\")\nbad"
    in
    match infer_string code with
    | Error diag -> is_code diag "type-purity"
    | _ -> false

  let%test "pure function calling maybe-effectful function from union is error" =
    let code =
      "fn choose(flag: Bool) = if (flag) { (x: Int) -> x + 1 } else { (x: Int) => x }\nfn pure(flag: Bool) -> Int = choose(flag)(1)\npure"
    in
    match infer_string code with
    | Error diag -> is_code diag "type-purity"
    | _ -> false

  let%test "unannotated function calling maybe-effectful union infers effectful" =
    let code =
      "fn choose(flag: Bool) = if (flag) { (x: Int) -> x + 1 } else { (x: Int) => x }\nfn caller(flag: Bool) = choose(flag)(1)\ncaller"
    in
    match infer_string code with
    | Ok (_, _, TFun (TBool, TInt, true)) -> true
    | _ -> false

  let%test "unannotated higher-order caller accepts pure and effectful callbacks" =
    let code =
      "fn hof(f) = f(1)\nlet pure = (x: Int) -> x + 1\nlet eff = (x: Int) => x + 2\nhof(pure) + hof(eff)"
    in
    infers_to code TInt

  let%test "pure annotated higher-order caller with unknown callback is rejected conservatively" =
    let code = "fn hof(f) -> Int = f(1)\nlet eff = (x: Int) => x\nhof(eff)" in
    match infer_string code with
    | Error diag -> is_code diag "type-purity"
    | _ -> false

  let%test "union of pure/effectful callables normalizes to callable and can be called" =
    let code =
      "fn choose(flag: Bool) = if (flag) { (x: Int) -> x + 1 } else { (x: Int) => x + 2 }\nlet f = choose(true)\nf(1)"
    in
    infers_to code TInt

  let%test "higher-order callback via local alias still infers call result" =
    let code = "fn hof(f) = { let g = f; g(1) }\nlet eff = (x: Int) => x + 2\nhof(eff)" in
    infers_to code TInt

  let%test "union of callable and non-callable cannot be called" =
    let code = "let f = if (true) { (x: Int) -> x + 1 } else { 0 }\nf(1)" in
    match infer_string code with
    | Error diag -> is_code diag "type-mismatch" || is_code diag "type-occurs-check"
    | _ -> false

  let%test "union of callables with mismatched arity cannot be called" =
    let code = "let f = if (true) { (x: Int) -> x } else { (x: Int, y: Int) -> x + y }\nf(1)" in
    match infer_string code with
    | Error diag -> is_code diag "type-mismatch" || is_code diag "type-occurs-check"
    | _ -> false

  let%test "pure function defining effectful function is ok" =
    (* Defining (not calling) an effectful function inside a pure function is fine *)
    let code = "(x: Int) -> ((y: Int) => y)" in
    match infer_string code with
    | Ok _ -> true
    | Error _ -> false

  let%test "unannotated recursive function with effectful call infers as effectful" =
    let code =
      "fn eff(x: Int) => Int = x\nfn loop(n: Int) = { if (n == 0) { 0 } else { eff(n); loop(n - 1) } }\nloop"
    in
    match infer_string code with
    | Ok (_, _, TFun (TInt, TInt, true)) -> true
    | _ -> false

  let%test "pure annotated recursive function with effectful call is rejected" =
    let code =
      "fn eff(x: Int) => Int = x\nfn loop(n: Int) -> Int = if (n == 0) { 0 } else { eff(n); loop(n - 1) }\nloop(2)"
    in
    match infer_string code with
    | Error diag -> is_code diag "type-purity"
    | _ -> false
end
