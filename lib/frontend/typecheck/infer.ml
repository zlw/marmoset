(* Type inference using Algorithm W (Hindley-Milner) *)

open Types
open Unify
module AST = Syntax.Ast.AST
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
   
     let id = fn(x) { x };
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
  | TypeAliasSym
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

let symbol_kind_tag = function
  | BuiltinValue -> "builtin-value"
  | TopLevelLet -> "toplevel-let"
  | LocalLet -> "local-let"
  | Param -> "param"
  | PatternVar -> "pattern-var"
  | TypeAliasSym -> "type-alias"
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
  constraint_store : (string, string list) Hashtbl.t;
  constrained_field_store : (string, (string * record_field_type) list) Hashtbl.t;
  type_var_user_names : (string, string) Hashtbl.t;
  top_level_placeholder_store : (string, mono_type) Hashtbl.t;
  symbol_table : (symbol_id, symbol) Hashtbl.t;
  symbol_key_store : (symbol_id, string) Hashtbl.t;
  identifier_symbol_store : (int, symbol_id) Hashtbl.t;
  method_resolution_store : (int, method_resolution) Hashtbl.t;
}

and method_resolution =
  | TraitMethod of string
  | InherentMethod
  | QualifiedTraitMethod of string (* Trait.method(receiver, args...) *)
  | QualifiedInherentMethod (* Type.method(receiver, args...) *)

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

let current_constraint_store () : (string, string list) Hashtbl.t = !active_inference_state.constraint_store

let current_constrained_field_store () : (string, (string * record_field_type) list) Hashtbl.t =
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

let lowered_trait_fields_for_constraints (traits : string list) : (string * record_field_type) list =
  Trait_solver.expand_constraints_with_supertraits traits
  |> List.concat_map (fun trait_name ->
         match Trait_registry.lookup_trait_fields trait_name with
         | None -> []
         | Some fields ->
             List.map
               (fun (field : record_field_type) ->
                 (trait_name, { name = field.name; typ = canonicalize_mono_type field.typ }))
               fields)

let update_type_var_constrained_fields (type_var_name : string) (traits : string list) : unit =
  let store = current_constrained_field_store () in
  let lowered = lowered_trait_fields_for_constraints traits in
  if lowered = [] then
    Hashtbl.remove store type_var_name
  else
    Hashtbl.replace store type_var_name lowered

let add_type_var_constraints (type_var_name : string) (traits : string list) : unit =
  let store = current_constraint_store () in
  let traits = List.map Trait_registry.canonical_trait_name traits in
  let merged =
    match Hashtbl.find_opt store type_var_name with
    | None -> traits
    | Some existing ->
        let seen = Hashtbl.create (List.length existing + List.length traits) in
        List.iter (fun trait_name -> Hashtbl.replace seen trait_name ()) existing;
        let rec collect_new_unique rev_new = function
          | [] -> existing @ List.rev rev_new
          | trait_name :: rest ->
              if Hashtbl.mem seen trait_name then
                collect_new_unique rev_new rest
              else (
                Hashtbl.replace seen trait_name ();
                collect_new_unique (trait_name :: rev_new) rest)
        in
        collect_new_unique [] traits
  in
  Hashtbl.replace store type_var_name merged;
  update_type_var_constrained_fields type_var_name merged

let lookup_type_var_constraints (type_var_name : string) : string list =
  match Hashtbl.find_opt (current_constraint_store ()) type_var_name with
  | Some traits -> traits
  | None -> []

let lookup_type_var_constrained_fields (type_var_name : string) : (string * record_field_type) list =
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

let snapshot_placeholder_rewrite_store () : placeholder_rewrite_map = Hashtbl.copy global_placeholder_rewrite_store

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

let record_method_resolution (expr : AST.expression) (resolution : method_resolution) : unit =
  Hashtbl.replace global_method_resolution_store expr.id resolution

let lookup_method_resolution (expr_id : int) : method_resolution option =
  Hashtbl.find_opt global_method_resolution_store expr_id

let snapshot_method_resolution_store () : (int, method_resolution) Hashtbl.t =
  Hashtbl.copy global_method_resolution_store

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

type obligation_reason = GenericConstraint of string

type obligation = {
  trait_name : string;
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
          add_type_var_constraints target_var constraints;
          []
      | _ ->
          List.map
            (fun trait_name -> { trait_name; typ = concrete_type; reason = GenericConstraint var_name })
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
  match Trait_solver.satisfies_trait o.typ o.trait_name with
  | Ok () -> Ok ()
  | Error diag ->
      let reason = obligation_reason_to_string o.reason in
      Error { diag with message = Printf.sprintf "Trait obligation failed (%s): %s" reason diag.message }

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

type constraint_ctx = string list ConstraintCtx.t

let empty_constraints : constraint_ctx = ConstraintCtx.empty

let add_constraint (ctx : constraint_ctx) (type_var : string) (traits : string list) : constraint_ctx =
  ConstraintCtx.add type_var (List.map Trait_registry.canonical_trait_name traits) ctx

let lookup_constraints (ctx : constraint_ctx) (type_var : string) : string list =
  match ConstraintCtx.find_opt type_var ctx with
  | Some traits -> traits
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
          match Trait_solver.check_constraints concrete_type traits with
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
  | TUnion members ->
      List.fold_left (fun acc t -> TypeVarSet.union acc (row_vars_in_type t)) TypeVarSet.empty members
  | TEnum (_, args) ->
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
              add_type_var_constraints new_var constraints
        | _ -> ());
        SubstMap.add var fresh acc)
      SubstMap.empty quantified_vars
  in
  apply_substitution subst mono

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

(* Result type for inference *)
type 'a infer_result = ('a, Diagnostic.t) result

(* Annotation helper: convert type expression, propagating open-row-rejected errors
   but falling back to fresh_type_var for other annotation errors *)
let annotation_or_fresh te =
  match Annotation.type_expr_to_mono_type te with
  | Error d when d.Diagnostic.code = "type-open-row-rejected" -> Error d
  | Error _ -> Ok (fresh_type_var ())
  | Ok t -> Ok t

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

let register_prebound_symbols (env : type_env) : symbol_scope =
  TypeEnv.fold
    (fun name _poly scope ->
      let sid = register_symbol ~name ~kind:BuiltinValue ~pos:(-1) ~end_pos:(-1) ~file_id:None in
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
                match let_binding.value.expr with
                | AST.Function _ -> NameMap.add let_binding.name sid scope
                | _ ->
                    (* Top-level value bindings remain declaration-ordered; only function
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
  let arm_scope =
    match arm.patterns with
    | [] -> push_scope stack
    | first :: _ -> resolve_pattern_bindings (push_scope stack) first
  in
  resolve_expr_symbols arm_scope arm.body

and resolve_impl_method_symbols (stack : symbol_scope_stack) (m : AST.method_impl) : unit =
  let method_scope = resolve_impl_method_params (push_scope stack) m in
  ignore (resolve_stmt_symbols method_scope ~is_top_level:false m.impl_method_body)

and resolve_stmt_symbols (stack : symbol_scope_stack) ~(is_top_level : bool) (stmt : AST.statement) :
    symbol_scope_stack =
  match stmt.stmt with
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

let resolve_program_symbols (env : type_env) (program : AST.program) : (unit, Diagnostic.t) result =
  clear_symbol_stores ();
  let root_scope = register_prebound_symbols env in
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
  let result =
    match expr.expr with
    (* Literals have known types *)
    | AST.Integer _ -> Ok (empty_substitution, TInt)
    | AST.Float _ -> Ok (empty_substitution, TFloat)
    | AST.Boolean _ -> Ok (empty_substitution, TBool)
    | AST.String _ -> Ok (empty_substitution, TString)
    (* Variable lookup - instantiate its poly_type *)
    | AST.Identifier name -> (
        match TypeEnv.find_opt name env with
        | None -> Error (error_at ~code:"type-unbound-var" ~message:("Unbound variable: " ^ name) expr)
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
    | AST.Function f ->
        (* Phase 2: Use parameter and return type annotations to guide inference *)
        (* Phase 4.3+: Handle generic parameters with constraints *)
        infer_function_with_annotations type_map env f.generics f.params f.return_type f.is_effectful f.body
    (* Function calls *)
    | AST.Call (func, args) -> infer_call type_map env expr func args
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
                 ~message:(Printf.sprintf "Unknown enum constructor: %s.%s" enum_name variant_name)
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
                      Error
                        (error_at ~code:"type-constructor"
                           ~message:(Printf.sprintf "Unknown enum: %s" enum_name)
                           expr)
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
                        | _ -> Error (error_at ~code:"type-constructor" ~message:"Argument count mismatch" expr)
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
                infer_match_arms type_map env' scrutinee_type arms subst expr))
    | AST.RecordLit (fields, spread) -> infer_record_literal type_map env fields spread expr
    | AST.FieldAccess (receiver, variant_name) -> (
        (* Phase 4.3/4.4: Could be field access or nullary enum constructor *)
        match receiver.expr with
        | AST.Identifier enum_name
          when let c = classify_dotted_receiver env enum_name variant_name in
               c = `EnumVariant || c = `EnumType -> (
            (* Nullary enum constructor like option.none or direction.north,
               or unknown variant on an enum type (produces error) *)
            match Enum_registry.lookup_variant enum_name variant_name with
            | None ->
                Error
                  (error_at ~code:"type-constructor"
                     ~message:(Printf.sprintf "Unknown enum constructor: %s.%s" enum_name variant_name)
                     expr)
            | Some variant -> (
                if List.length variant.fields <> 0 then
                  Error
                    (error_at ~code:"type-constructor"
                       ~message:
                         (Printf.sprintf "%s.%s expects %d arguments, got 0" enum_name variant_name
                            (List.length variant.fields))
                       expr)
                else
                  (* Get the enum definition to know type parameters *)
                  match Enum_registry.lookup enum_name with
                  | None ->
                      Error
                        (error_at ~code:"type-constructor"
                           ~message:(Printf.sprintf "Unknown enum: %s" enum_name)
                           expr)
                  | Some enum_def ->
                      (* Create fresh type variables for each type parameter *)
                      let fresh_vars = List.map (fun _ -> fresh_type_var ()) enum_def.type_params in
                      (* Build the result enum type with fresh type variables *)
                      let result_type = TEnum (enum_name, fresh_vars) in
                      Ok (empty_substitution, result_type)))
        | _ -> (
            (* Real field access on records *)
            match infer_expression type_map env receiver with
            | Error e -> Error e
            | Ok (subst1, receiver_type) ->
                let receiver_type' = apply_substitution subst1 receiver_type in
                (* Exact record access *)
                let lookup_field fields =
                  List.find_opt (fun (f : Types.record_field_type) -> f.name = variant_name) fields
                in
                let field_result =
                  match receiver_type' with
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
                              Trait_solver.expand_constraints_with_supertraits constraints
                            in
                            let constrained_field_types =
                              lookup_type_var_constrained_fields type_var_name
                              |> List.filter_map (fun (trait_name, (field : Types.record_field_type)) ->
                                     if field.name = variant_name then
                                       Some (trait_name, field.typ)
                                     else
                                       None)
                            in
                            let resolve_constrained_field_type candidates =
                              match candidates with
                              | [] ->
                                  Error
                                    (Printf.sprintf
                                       "Field '%s' is not guaranteed by constraints on type variable '%s' (%s)"
                                       variant_name type_var_name
                                       (String.concat ", " expanded_constraints))
                              | (_trait_name, first_type) :: rest ->
                                  let rec unify_candidates current_type subst_acc = function
                                    | [] -> Ok (apply_substitution subst_acc current_type)
                                    | (candidate_trait, candidate_type) :: tail -> (
                                        let lhs = apply_substitution subst_acc current_type in
                                        let rhs = apply_substitution subst_acc candidate_type in
                                        match unify lhs rhs with
                                        | Ok subst' ->
                                            unify_candidates lhs (compose_substitution subst' subst_acc) tail
                                        | Error _ ->
                                            Error
                                              (Printf.sprintf
                                                 "Conflicting field type requirements for field '%s' on constrained type variable '%s' (trait '%s')"
                                                 variant_name type_var_name candidate_trait))
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
                field_result))
    | AST.MethodCall { mc_receiver = receiver; mc_method = method_name; mc_type_args; mc_args = args } -> (
        let infer_enum_constructor (enum_name : string) : (substitution * mono_type) infer_result =
          match Enum_registry.lookup_variant enum_name method_name with
          | None ->
              Error
                (error_at ~code:"type-constructor"
                   ~message:(Printf.sprintf "Unknown enum constructor: %s.%s" enum_name method_name)
                   expr)
          | Some variant -> (
              match infer_args type_map env empty_substitution args with
              | Error e -> Error e
              | Ok (subst, arg_types) -> (
                  if List.length args <> List.length variant.fields then
                    Error
                      (error_at ~code:"type-constructor"
                         ~message:
                           (Printf.sprintf "%s.%s expects %d arguments, got %d" enum_name method_name
                              (List.length variant.fields) (List.length args))
                         expr)
                  else
                    match Enum_registry.lookup enum_name with
                    | None ->
                        Error
                          (error_at ~code:"type-constructor"
                             ~message:(Printf.sprintf "Unknown enum: %s" enum_name)
                             expr)
                    | Some enum_def -> (
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
                              | Ok new_subst -> unify_all (compose_substitution new_subst subst_acc) rest1 rest2
                              | Error e -> Error (error_at ~code:e.code ~message:e.message expr))
                          | _ -> Error (error_at ~code:"type-constructor" ~message:"Argument count mismatch" expr)
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
              let env1 = apply_substitution_env subst1 env in
              let receiver_type' = apply_substitution subst1 receiver_type in
              let infer_with_method_signature
                  (resolved_method : Trait_registry.method_sig) (resolution : method_resolution) :
                  (substitution * mono_type) infer_result =
                (* Phase 3.4: Method generic instantiation at call site *)
                let method_generics = resolved_method.method_generics in
                let instantiate_method_generics () :
                    (Trait_registry.method_sig * substitution, Diagnostics.Diagnostic.t) result =
                  if method_generics = [] then
                    (* No method-level generics *)
                    match mc_type_args with
                    | Some type_args when type_args <> [] ->
                        Error
                          (error_at ~code:"type-constructor"
                             ~message:
                               (Printf.sprintf "Method '%s' takes no type parameters, but %d were provided"
                                  method_name (List.length type_args))
                             expr)
                    | _ -> Ok (resolved_method, SubstMap.empty)
                  else
                    match mc_type_args with
                    | Some type_args ->
                        (* Explicit type args: validate count and convert *)
                        if List.length type_args <> List.length method_generics then
                          Error
                            (error_at ~code:"type-constructor"
                               ~message:
                                 (Printf.sprintf "Method '%s' expects %d type argument(s), but %d were provided"
                                    method_name (List.length method_generics) (List.length type_args))
                               expr)
                        else
                          let rec convert_all acc = function
                            | [] -> Ok (List.rev acc)
                            | te :: rest -> (
                                match Annotation.type_expr_to_mono_type te with
                                | Error diag -> Error (error_at ~code:diag.code ~message:diag.message expr)
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
                                resolved_method with
                                method_params =
                                  List.map
                                    (fun (name, ty) -> (name, apply_substitution method_subst ty))
                                    resolved_method.method_params;
                                method_return_type =
                                  apply_substitution method_subst resolved_method.method_return_type;
                              },
                              method_subst )
                    | None ->
                        (* No explicit type args: create fresh TVars with constraint propagation *)
                        let method_subst =
                          List.fold_left
                            (fun acc (param_name, constraints) ->
                              let fresh = fresh_type_var () in
                              (match fresh with
                              | TVar fresh_name ->
                                  if constraints <> [] then
                                    add_type_var_constraints fresh_name constraints
                              | _ -> ());
                              SubstMap.add param_name fresh acc)
                            SubstMap.empty method_generics
                        in
                        Ok
                          ( {
                              resolved_method with
                              method_params =
                                List.map
                                  (fun (name, ty) -> (name, apply_substitution method_subst ty))
                                  resolved_method.method_params;
                              method_return_type =
                                apply_substitution method_subst resolved_method.method_return_type;
                            },
                            method_subst )
                in
                match instantiate_method_generics () with
                | Error e -> Error e
                | Ok (instantiated_method, method_subst) -> (
                    let expected_arg_types =
                      instantiated_method.method_params |> List.tl |> List.map snd
                    in
                    match infer_args_against_expected type_map env1 subst1 args expected_arg_types with
                    | Error e -> Error e
                    | Ok (subst2, arg_types) -> (
                        let all_arg_types = receiver_type' :: arg_types in
                        let expected_param_types = List.map snd instantiated_method.method_params in
                        if List.length all_arg_types <> List.length expected_param_types then
                          Error
                            (error_at ~code:"type-constructor"
                               ~message:
                                 (Printf.sprintf "Method '%s' expects %d arguments, got %d" method_name
                                    (List.length expected_param_types - 1)
                                    (List.length args))
                               expr)
                        else
                          let rec unify_params subst_acc actual_types expected_types =
                            match (actual_types, expected_types) with
                            | [], [] -> Ok subst_acc
                            | actual :: rest_actual, expected :: rest_expected -> (
                                let actual' = apply_substitution subst_acc actual in
                                let expected' = apply_substitution subst_acc expected in
                                match unify actual' expected' with
                                | Error e -> Error (error_at ~code:e.code ~message:e.message expr)
                                | Ok new_subst ->
                                    unify_params
                                      (compose_substitution new_subst subst_acc)
                                      rest_actual rest_expected)
                            | _ ->
                                Error (error_at ~code:"type-constructor" ~message:"Argument count mismatch" expr)
                          in
                          match unify_params subst2 all_arg_types expected_param_types with
                          | Error e -> Error e
                          | Ok final_subst -> (
                              (* Phase 3.4.6: Enforce method-level generic constraints *)
                              let constraint_check =
                                List.fold_left
                                  (fun acc (param_name, constraints) ->
                                    match acc with
                                    | Error _ -> acc
                                    | Ok () ->
                                        (* Resolve through method_subst (b -> t0) then final_subst (t0 -> int) *)
                                        let resolved_type =
                                          apply_substitution method_subst (TVar param_name)
                                          |> apply_substitution final_subst
                                          |> canonicalize_mono_type
                                        in
                                        List.fold_left
                                          (fun inner_acc trait_name ->
                                            match inner_acc with
                                            | Error _ -> inner_acc
                                            | Ok () -> (
                                                match resolved_type with
                                                | TVar _ ->
                                                    (* Still a TVar after unification: add constraint for later *)
                                                    Ok ()
                                                | _ -> (
                                                    match
                                                      Trait_solver.satisfies_trait resolved_type trait_name
                                                    with
                                                    | Ok () -> Ok ()
                                                    | Error diag ->
                                                        Error
                                                          (error_at ~code:diag.code
                                                             ~message:
                                                               (Printf.sprintf
                                                                  "Method '%s' type parameter '%s' requires trait '%s', but %s does not satisfy it"
                                                                  method_name param_name trait_name
                                                                  (Types.to_string resolved_type))
                                                             expr))))
                                          acc constraints)
                                  (Ok ()) method_generics
                              in
                              match constraint_check with
                              | Error e -> Error e
                              | Ok () ->
                                  let return_type =
                                    apply_substitution final_subst instantiated_method.method_return_type
                                  in
                                  record_method_resolution expr resolution;
                                  (* Phase 6.4: Record resolved method-level type args for emitter *)
                                  let resolved_method_type_args =
                                    List.map
                                      (fun (param_name, _) ->
                                        apply_substitution method_subst (TVar param_name)
                                        |> apply_substitution final_subst
                                        |> canonicalize_mono_type)
                                      method_generics
                                  in
                                  record_method_type_args expr resolved_method_type_args;
                                  Ok (final_subst, return_type))))
              in
              let method_lookup_result =
                match receiver_type' with
                | TVar type_var_name -> (
                    let constraints = lookup_type_var_constraints type_var_name in
                    if constraints = [] then
                      `Not_found
                    else
                      let candidates =
                        Trait_solver.available_methods constraints
                        |> List.filter_map (fun (trait_name, trait_def) ->
                               match
                                 List.find_opt
                                   (fun (m : Trait_registry.method_sig) -> m.method_name = method_name)
                                   trait_def.Trait_registry.trait_methods
                               with
                               | None -> None
                               | Some method_sig -> Some (trait_name, method_sig))
                        |> List.sort (fun (a, _) (b, _) -> String.compare a b)
                      in
                      match candidates with
                      | [] -> `Not_found
                      | [ candidate ] -> `Found candidate
                      | many ->
                          let trait_names = many |> List.map fst |> List.sort_uniq String.compare in
                          `Error
                            (Printf.sprintf
                               "Ambiguous method '%s' for constrained type variable '%s' (provided by traits: %s)"
                               method_name type_var_name (String.concat ", " trait_names)))
                | _ -> (
                    match Trait_registry.resolve_method receiver_type' method_name with
                    | Ok method_info -> `Found method_info
                    | Error msg ->
                        let no_method_prefix = "No method '" in
                        if
                          String.length msg >= String.length no_method_prefix
                          && String.sub msg 0 (String.length no_method_prefix) = no_method_prefix
                        then
                          `Not_found
                        else
                          `Error msg)
              in
              let inherent_method_result =
                match receiver_type' with
                | TVar _ -> Ok None
                | _ -> Inherent_registry.resolve_method receiver_type' method_name
              in
              match method_lookup_result with
              | `Error msg -> (
                  (* Phase 4.3: Even on ambiguity, inherent method wins if present *)
                  match inherent_method_result with
                  | Ok (Some inherent_sig) -> infer_with_method_signature inherent_sig InherentMethod
                  | _ -> Error (error_at ~code:"type-constructor" ~message:msg expr))
              | `Not_found -> (
                  match inherent_method_result with
                  | Error msg -> Error (error_at ~code:"type-constructor" ~message:msg expr)
                  | Ok (Some inherent_sig) -> infer_with_method_signature inherent_sig InherentMethod
                  | Ok None -> (
                      let field_expr =
                        AST.mk_expr ~pos:expr.pos ~end_pos:expr.end_pos ~file_id:expr.file_id
                          (AST.FieldAccess (receiver, method_name))
                      in
                      let call_expr =
                        AST.mk_expr ~pos:expr.pos ~end_pos:expr.end_pos ~file_id:expr.file_id
                          (AST.Call (field_expr, args))
                      in
                      match infer_expression type_map env call_expr with
                      | Ok (subst_field_call, field_call_type) -> Ok (subst_field_call, field_call_type)
                      | Error _ ->
                          Error
                            (error_at ~code:"type-constructor"
                               ~message:
                                 (Printf.sprintf "No method '%s' found for type %s" method_name
                                    (Types.to_string receiver_type'))
                               expr)))
              | `Found (trait_name, method_sig) -> (
                  let is_self_defining =
                    let recv_canon = canonicalize_mono_type receiver_type' in
                    List.exists
                      (fun (for_type, mname) ->
                        mname = method_name && canonicalize_mono_type for_type = recv_canon)
                      !current_inherent_impl_methods
                  in
                  match inherent_method_result with
                  | Error msg -> Error (error_at ~code:"type-constructor" ~message:msg expr)
                  | Ok (Some inherent_sig) when not is_self_defining ->
                      (* Phase 4.3: Inherent method takes precedence over trait on dot calls,
                         unless the inherent method is currently being defined (avoid infinite recursion) *)
                      infer_with_method_signature inherent_sig InherentMethod
                  | Ok (Some _) | Ok None -> (
                      let instantiated_method_sig =
                        match Trait_registry.lookup_trait trait_name with
                        | None -> method_sig
                        | Some trait_def -> (
                            match trait_def.trait_type_param with
                            | None -> method_sig
                            | Some type_param ->
                                let subst_type_param = substitution_singleton type_param receiver_type' in
                                {
                                  method_sig with
                                  method_params =
                                    List.map
                                      (fun (name, ty) -> (name, apply_substitution subst_type_param ty))
                                      method_sig.method_params;
                                  method_return_type =
                                    apply_substitution subst_type_param method_sig.method_return_type;
                                })
                      in
                      let trait_requirement_result =
                        match receiver_type' with
                        | TVar _ -> Ok ()
                        | _ -> (
                            match Trait_solver.satisfies_trait receiver_type' trait_name with
                            | Ok () -> Ok ()
                            | Error diag -> Error diag)
                      in
                      match trait_requirement_result with
                      | Error diag -> Error (error_at ~code:diag.code ~message:diag.message expr)
                      | Ok () -> infer_with_method_signature instantiated_method_sig (TraitMethod trait_name))))
        in
        (* Phase 4.4: Qualified call resolution *)
        let infer_qualified_trait_call (trait_name : string) : (substitution * mono_type) infer_result =
          match Trait_registry.lookup_trait trait_name with
          | None ->
              Error
                (error_at ~code:"type-constructor" ~message:(Printf.sprintf "Unknown trait '%s'" trait_name) expr)
          | Some trait_def -> (
              let method_opt =
                List.find_opt
                  (fun (m : Trait_registry.method_sig) -> m.method_name = method_name)
                  trait_def.trait_methods
              in
              match method_opt with
              | None ->
                  Error
                    (error_at ~code:"type-constructor"
                       ~message:(Printf.sprintf "Trait '%s' has no method '%s'" trait_name method_name)
                       expr)
              | Some method_sig -> (
                  (* Instantiate trait type param with fresh TVar *)
                  let instantiated_sig =
                    match trait_def.trait_type_param with
                    | None -> method_sig
                    | Some type_param ->
                        let fresh = fresh_type_var () in
                        let s = substitution_singleton type_param fresh in
                        {
                          method_sig with
                          method_params =
                            List.map (fun (name, ty) -> (name, apply_substitution s ty)) method_sig.method_params;
                          method_return_type = apply_substitution s method_sig.method_return_type;
                        }
                  in
                  (* All args are explicit (first arg is receiver) *)
                  let param_types = List.map snd instantiated_sig.method_params in
                  match infer_args_against_expected type_map env empty_substitution args param_types with
                  | Error e -> Error e
                  | Ok (subst1, arg_types) -> (
                      if List.length arg_types <> List.length param_types then
                        Error
                          (error_at ~code:"type-constructor"
                             ~message:
                               (Printf.sprintf "Qualified call '%s.%s' expects %d argument(s), got %d" trait_name
                                  method_name (List.length param_types) (List.length args))
                             expr)
                      else
                        let rec unify_params subst_acc actual_types expected_types =
                          match (actual_types, expected_types) with
                          | [], [] -> Ok subst_acc
                          | actual :: rest_a, expected :: rest_e -> (
                              let actual' = apply_substitution subst_acc actual in
                              let expected' = apply_substitution subst_acc expected in
                              match unify actual' expected' with
                              | Error e -> Error (error_at ~code:e.code ~message:e.message expr)
                              | Ok new_subst ->
                                  unify_params (compose_substitution new_subst subst_acc) rest_a rest_e)
                          | _ -> Error (error_at ~code:"type-constructor" ~message:"Argument count mismatch" expr)
                        in
                        match unify_params subst1 arg_types param_types with
                        | Error e -> Error e
                        | Ok final_subst -> (
                            (* Verify trait constraint on receiver type *)
                            let receiver_type =
                              match param_types with
                              | first :: _ -> apply_substitution final_subst first |> canonicalize_mono_type
                              | [] -> TNull
                            in
                            let trait_check =
                              match receiver_type with
                              | TVar _ -> Ok ()
                              | _ -> (
                                  match Trait_solver.satisfies_trait receiver_type trait_name with
                                  | Ok () -> Ok ()
                                  | Error diag -> Error (error_at ~code:diag.code ~message:diag.message expr))
                            in
                            match trait_check with
                            | Error e -> Error e
                            | Ok () ->
                                let return_type =
                                  apply_substitution final_subst instantiated_sig.method_return_type
                                in
                                record_method_resolution expr (QualifiedTraitMethod trait_name);
                                Ok (final_subst, return_type)))))
        in
        let resolve_type_name (name : string) : mono_type option =
          match Annotation.builtin_primitive_type name with
          | Some primitive -> Some primitive
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
        in
        let infer_qualified_type_call (for_type : mono_type) : (substitution * mono_type) infer_result =
          match Inherent_registry.resolve_method for_type method_name with
          | Error msg -> Error (error_at ~code:"type-constructor" ~message:msg expr)
          | Ok None ->
              Error
                (error_at ~code:"type-constructor"
                   ~message:
                     (Printf.sprintf "No inherent method '%s' found for type %s" method_name
                        (Types.to_string for_type))
                   expr)
          | Ok (Some method_sig) -> (
              (* All args are explicit (first arg is receiver) *)
              let param_types = List.map snd method_sig.method_params in
              match infer_args_against_expected type_map env empty_substitution args param_types with
              | Error e -> Error e
              | Ok (subst1, arg_types) -> (
                  if List.length arg_types <> List.length param_types then
                    Error
                      (error_at ~code:"type-constructor"
                         ~message:
                           (Printf.sprintf "Qualified call '%s.%s' expects %d argument(s), got %d"
                              (Types.to_string for_type) method_name (List.length param_types) (List.length args))
                         expr)
                  else
                    let rec unify_params subst_acc actual_types expected_types =
                      match (actual_types, expected_types) with
                      | [], [] -> Ok subst_acc
                      | actual :: rest_a, expected :: rest_e -> (
                          let actual' = apply_substitution subst_acc actual in
                          let expected' = apply_substitution subst_acc expected in
                          match unify actual' expected' with
                          | Error e -> Error (error_at ~code:e.code ~message:e.message expr)
                          | Ok new_subst -> unify_params (compose_substitution new_subst subst_acc) rest_a rest_e)
                      | _ -> Error (error_at ~code:"type-constructor" ~message:"Argument count mismatch" expr)
                    in
                    match unify_params subst1 arg_types param_types with
                    | Error e -> Error e
                    | Ok final_subst ->
                        let return_type = apply_substitution final_subst method_sig.method_return_type in
                        record_method_resolution expr QualifiedInherentMethod;
                        Ok (final_subst, return_type)))
        in
        match receiver.expr with
        | AST.Identifier name -> (
            (* Use shared classifier for consistent priority across FieldAccess and MethodCall *)
            match classify_dotted_receiver env name method_name with
            | `BoundVar -> infer_real_method_call ()
            | `EnumVariant -> infer_enum_constructor name
            | `EnumType -> (
                match resolve_type_name name with
                | Some for_type -> infer_qualified_type_call for_type
                | None -> infer_enum_constructor name)
            | `TypeName for_type -> infer_qualified_type_call for_type
            | `TraitName -> infer_qualified_trait_call name
            | `Unknown -> infer_real_method_call ())
        | _ -> infer_real_method_call ())
    | AST.BlockExpr stmts -> infer_block type_map env stmts
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
          | "+" | "-" | "*" | "/" | "%" -> (
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

(* Helper: Detect type narrowing pattern: x is int *)
and detect_is_narrowing (expr : AST.expression) : (string * mono_type) option =
  match expr.expr with
  | AST.TypeCheck (var_expr, type_ann) -> (
      match var_expr.expr with
      | AST.Identifier var_name -> (
          match Annotation.type_expr_to_mono_type type_ann with
          | Ok narrow_type -> Some (var_name, narrow_type)
          | Error _ -> None)
      | _ -> None)
  | _ -> None

(* Helper: Narrow a variable's type in the environment *)
and narrow_type_in_env (env : type_env) (var_name : string) (narrow_type : mono_type) : type_env =
  match TypeEnv.find_opt var_name env with
  | Some (Forall ([], current_type)) -> (
      (* Check if current type is a union containing the narrow type *)
      match current_type with
      | TUnion members when List.mem narrow_type members ->
          (* Replace var's type with narrowed type *)
          TypeEnv.add var_name (Forall ([], narrow_type)) env
      | _ when current_type = narrow_type ->
          (* Already the right type, no change needed *)
          env
      | _ ->
          (* Not a union or doesn't contain the narrow type - don't narrow *)
          env)
  | _ -> env

(* Helper: Compute complement type (all members except narrow_type) *)
and compute_complement_type (current_type : mono_type) (narrow_type : mono_type) : mono_type option =
  match current_type with
  | TUnion members -> (
      let remaining = List.filter (fun t -> t <> narrow_type) members in
      match remaining with
      | [] -> None (* No members left *)
      | [ single ] -> Some single (* Single type remains *)
      | multiple -> Some (TUnion multiple)
      (* Multiple types remain *))
  | _ when current_type = narrow_type -> None (* Narrowing to same type, no complement *)
  | _ -> Some current_type (* Not a union, complement is the whole type *)

(* Helper: Narrow to complement type in else branch *)
and narrow_to_complement (env : type_env) (var_name : string) (narrow_type : mono_type) : type_env =
  match TypeEnv.find_opt var_name env with
  | Some (Forall ([], current_type)) -> (
      match compute_complement_type current_type narrow_type with
      | Some complement -> TypeEnv.add var_name (Forall ([], complement)) env
      | None ->
          (* No complement (all cases exhausted), keep current env *)
          env)
  | _ -> env

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
          let narrowing = detect_is_narrowing condition in

          (* Create narrowed environment for consequence branch *)
          let env_cons =
            match narrowing with
            | Some (var_name, narrow_type) -> narrow_type_in_env env' var_name narrow_type
            | None -> env'
          in

          (* Infer consequence type with narrowed environment *)
          match infer_statement type_map env_cons consequence with
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
                  let env_alt =
                    match narrowing with
                    | Some (var_name, narrow_type) -> narrow_to_complement env'' var_name narrow_type
                    | None -> env''
                  in
                  match infer_statement type_map env_alt alt with
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
  | AST.EnumDef _ -> Ok (subst, expected_ret_type)
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
  | AST.Let { value; _ } -> expr_has_effectful_call type_map value
  | AST.Return expr -> expr_has_effectful_call type_map expr
  | AST.ExpressionStmt expr -> expr_has_effectful_call type_map expr
  | AST.Block stmts -> List.exists (body_has_effectful_call type_map) stmts
  | AST.EnumDef _ | AST.TraitDef _ | AST.ImplDef _ | AST.InherentImplDef _ | AST.DeriveDef _ | AST.TypeAlias _ ->
      false

and type_may_be_effectful_callable (typ : mono_type) : bool =
  match typ with
  | TFun (_, _, true) -> true
  | TUnion members -> List.exists type_may_be_effectful_callable members
  | _ -> false

and expr_has_effectful_call (type_map : type_map) (expr : AST.expression) : bool =
  match expr.expr with
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
      expr_has_effectful_call type_map mc_receiver || List.exists (expr_has_effectful_call type_map) mc_args
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
            | Error d when d.Diagnostic.code = "type-open-row-rejected" -> Error d
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
                | Some expected_ret -> (
                    (* Validate explicit return statements match expected type *)
                    match validate_return_statements type_map env' expected_ret body with
                    | Error e -> Error e
                    | Ok () ->
                        if strict_return_check then
                          (* Strict mode (methods): subtype check only, no unification fallback.
                             Method generic type vars use original names and must remain polymorphic. *)
                          let expected_ret' = apply_substitution subst expected_ret in
                          if Annotation.check_annotation expected_ret' body_type' then
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
                            | TEnum (_, args) | TUnion args -> List.exists has_unresolved_var args
                            | TInt | TFloat | TBool | TString | TNull -> false
                          in
                          let subtype_ok = Annotation.is_subtype_of body_type' expected_ret in
                          let unify_compatible =
                            (has_unresolved_var body_type' || has_unresolved_var expected_ret)
                            &&
                            match unify body_type' expected_ret with
                            | Ok _ -> true
                            | Error _ -> false
                          in
                          if subtype_ok || unify_compatible then
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
                                 body)))))

(* Top-level function adapter: processes generics, maps effect annotation, builds TFun type *)
and infer_function_with_annotations type_map env generics_opt params return_annot is_effectful body =
  (* Process generic parameters and their constraints *)
  let type_var_map =
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
    type_callable type_map env ~type_bindings:type_var_map ~known_param_types:[] ~params ~return_annot
      ~known_return:None ~effect_annot ~strict_return_check:false ~body
  with
  | Error e -> Error e
  | Ok (_param_names, param_types, ret_type, actual_effectful, subst) ->
      let mk_fun a b = TFun (a, b, actual_effectful) in
      let param_types' = List.map (apply_substitution subst) param_types in
      let func_type = List.fold_right mk_fun param_types' ret_type in
      Ok (subst, func_type)

and placeholder_identifier_count_expr (expr : AST.expression) : int =
  match expr.expr with
  | AST.Identifier "_" -> 1
  | AST.Identifier _ | AST.Integer _ | AST.Float _ | AST.Boolean _ | AST.String _ -> 0
  | AST.Prefix (_, inner) | AST.TypeCheck (inner, _) | AST.FieldAccess (inner, _) ->
      placeholder_identifier_count_expr inner
  | AST.Infix (left, _, right) -> placeholder_identifier_count_expr left + placeholder_identifier_count_expr right
  | AST.If (cond, cons, alt) ->
      placeholder_identifier_count_expr cond
      + placeholder_identifier_count_stmt cons
      +
      (match alt with
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
  | AST.RecordLit (fields, spread) ->
      let fields_count =
        List.fold_left
          (fun acc (field : AST.record_field) ->
            acc
            +
            (match field.field_value with
            | Some value -> placeholder_identifier_count_expr value
            | None -> 0))
          0 fields
      in
      fields_count
      +
      (match spread with
      | Some spread_expr -> placeholder_identifier_count_expr spread_expr
      | None -> 0)
  | AST.MethodCall { mc_receiver; mc_args; _ } ->
      placeholder_identifier_count_expr mc_receiver
      + List.fold_left (fun acc arg -> acc + placeholder_identifier_count_expr arg) 0 mc_args
  | AST.BlockExpr stmts -> placeholder_identifier_count_stmts stmts

and placeholder_identifier_count_stmt (stmt : AST.statement) : int =
  match stmt.stmt with
  | AST.Let { value; _ } -> placeholder_identifier_count_expr value
  | AST.Return expr | AST.ExpressionStmt expr -> placeholder_identifier_count_expr expr
  | AST.Block stmts -> placeholder_identifier_count_stmts stmts
  | AST.EnumDef _ | AST.TraitDef _ | AST.ImplDef _ | AST.InherentImplDef _ | AST.DeriveDef _ | AST.TypeAlias _ ->
      0

and placeholder_identifier_count_stmts (stmts : AST.statement list) : int =
  List.fold_left (fun acc stmt -> acc + placeholder_identifier_count_stmt stmt) 0 stmts

and collect_used_names_expr (used : StringSet.t) (expr : AST.expression) : StringSet.t =
  match expr.expr with
  | AST.Identifier name -> StringSet.add name used
  | AST.Integer _ | AST.Float _ | AST.Boolean _ | AST.String _ -> used
  | AST.Prefix (_, inner) | AST.TypeCheck (inner, _) | AST.FieldAccess (inner, _) ->
      collect_used_names_expr used inner
  | AST.Infix (left, _, right) -> collect_used_names_expr (collect_used_names_expr used left) right
  | AST.If (cond, cons, alt) ->
      let used = collect_used_names_expr used cond in
      let used = collect_used_names_stmt used cons in
      (match alt with
      | Some stmt -> collect_used_names_stmt used stmt
      | None -> used)
  | AST.Function { params; _ } ->
      List.fold_left (fun acc (name, _) -> StringSet.add name acc) used params
  | AST.Call (fn_expr, args) ->
      List.fold_left collect_used_names_expr (collect_used_names_expr used fn_expr) args
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
        (collect_used_names_expr used scrutinee) arms
  | AST.RecordLit (fields, spread) ->
      let used =
        List.fold_left
          (fun acc (field : AST.record_field) ->
            match field.field_value with
            | Some value -> collect_used_names_expr acc value
            | None -> acc)
          used fields
      in
      (match spread with
      | Some spread_expr -> collect_used_names_expr used spread_expr
      | None -> used)
  | AST.MethodCall { mc_receiver; mc_args; _ } ->
      List.fold_left collect_used_names_expr (collect_used_names_expr used mc_receiver) mc_args
  | AST.BlockExpr stmts -> collect_used_names_stmts used stmts

and collect_used_names_stmt (used : StringSet.t) (stmt : AST.statement) : StringSet.t =
  match stmt.stmt with
  | AST.Let { name; value; _ } -> collect_used_names_expr (StringSet.add name used) value
  | AST.Return expr | AST.ExpressionStmt expr -> collect_used_names_expr used expr
  | AST.Block stmts -> collect_used_names_stmts used stmts
  | AST.EnumDef _ | AST.TraitDef _ | AST.ImplDef _ | AST.InherentImplDef _ | AST.DeriveDef _ | AST.TypeAlias _ ->
      used

and collect_used_names_stmts (used : StringSet.t) (stmts : AST.statement list) : StringSet.t =
  List.fold_left collect_used_names_stmt used stmts

and fresh_placeholder_param_name (expr : AST.expression) : string =
  let used = collect_used_names_expr StringSet.empty expr in
  let rec loop n =
    let candidate = if n = 0 then "it" else Printf.sprintf "it%d" n in
    if StringSet.mem candidate used then
      loop (n + 1)
    else
      candidate
  in
  loop 0

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
               (replace_placeholder_identifier_expr param_name key, replace_placeholder_identifier_expr param_name value))
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
  { expr with id = fresh_synthetic_expr_id (); expr = rewritten }

and replace_placeholder_identifier_stmt (param_name : string) (stmt : AST.statement) : AST.statement =
  let rewritten =
    match stmt.stmt with
    | AST.Let ({ value; _ } as let_binding) ->
        AST.Let { let_binding with value = replace_placeholder_identifier_expr param_name value }
    | AST.Return expr -> AST.Return (replace_placeholder_identifier_expr param_name expr)
    | AST.ExpressionStmt expr -> AST.ExpressionStmt (replace_placeholder_identifier_expr param_name expr)
    | AST.Block stmts -> AST.Block (List.map (replace_placeholder_identifier_stmt param_name) stmts)
    | AST.EnumDef _ | AST.TraitDef _ | AST.ImplDef _ | AST.InherentImplDef _ | AST.DeriveDef _ | AST.TypeAlias _ ->
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
          | PlaceholderCallbackEffectfulOnly, PlaceholderCallbackEffectfulOnly ->
              PlaceholderCallbackEffectfulOnly
          | PlaceholderCallbackNotCallable, PlaceholderCallbackEffectfulOnly
          | PlaceholderCallbackEffectfulOnly, PlaceholderCallbackNotCallable ->
              PlaceholderCallbackEffectfulOnly
          | PlaceholderCallbackNotCallable, PlaceholderCallbackNotCallable -> PlaceholderCallbackNotCallable)
        PlaceholderCallbackNotCallable members
  | _ -> PlaceholderCallbackNotCallable

and placeholder_lambda_expr (expr : AST.expression) : AST.expression =
  let param_name = fresh_placeholder_param_name expr in
  let body_expr = replace_placeholder_identifier_expr param_name expr in
  let body_stmt =
    AST.mk_stmt ~pos:body_expr.pos ~end_pos:body_expr.end_pos ~file_id:body_expr.file_id
      (AST.Block
         [
           AST.mk_stmt ~pos:body_expr.pos ~end_pos:body_expr.end_pos ~file_id:body_expr.file_id
             (AST.ExpressionStmt body_expr);
         ])
  in
  AST.mk_expr ~id:(fresh_synthetic_expr_id ()) ~pos:expr.pos ~end_pos:expr.end_pos ~file_id:expr.file_id
    (AST.Function
       {
         generics = None;
         params = [ (param_name, None) ];
         return_type = None;
         is_effectful = false;
         body = body_stmt;
       })

and infer_arg_against_expected type_map env subst (arg : AST.expression) (expected_type : mono_type) :
    (substitution * mono_type) infer_result =
  let expected_type' = apply_substitution subst expected_type in
  let placeholder_count = placeholder_identifier_count_expr arg in
  let placeholder_expectation = placeholder_callback_expectation expected_type' in
  if placeholder_count > 1 then
    Error
      (error_at ~code:"type-invalid-placeholder"
         ~message:
           (Printf.sprintf "Placeholder shorthand requires exactly one '_' placeholder, found %d"
              placeholder_count)
         arg)
  else
    match (placeholder_count, placeholder_expectation) with
    | 1, PlaceholderCallbackEffectfulOnly ->
        Error
          (error_at ~code:"type-invalid-placeholder"
             ~message:"Placeholder shorthand is pure-only; effectful callbacks require explicit '=>' lambda"
             arg)
    | _ ->
        let inferred_arg =
          match (placeholder_count, placeholder_expectation) with
          | 1, PlaceholderCallbackPureAllowed ->
              let rewritten_arg = placeholder_lambda_expr arg in
              record_placeholder_rewrite arg rewritten_arg;
              rewritten_arg
          | _ -> arg
        in
        let env' = apply_substitution_env subst env in
        match infer_expression type_map env' inferred_arg with
        | Error e -> Error e
        | Ok (subst1, arg_type) -> (
            let subst' = compose_substitution subst subst1 in
            let expected_type'' = apply_substitution subst' expected_type in
            let arg_type' = apply_substitution subst' arg_type in
            match unify arg_type' expected_type'' with
            | Error e -> Error (error_at ~code:e.code ~message:e.message arg)
            | Ok subst2 ->
                let final_subst = compose_substitution subst' subst2 in
                let final_type = apply_substitution final_subst arg_type in
                Ok (final_subst, final_type))

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

and infer_call type_map env (call_expr : AST.expression) func args =
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
      | Ok (subst2, result_type) ->
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
                  Ok (final_subst, final_result)))

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
   using a unified priority: bound variable > enum > type alias > trait > unknown. *)
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
    | (field : AST.record_field) :: rest -> (
        let field_expr =
          match field.field_value with
          | Some e -> e
          | None -> AST.mk_expr ~pos:expr.pos (AST.Identifier field.field_name)
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
  match infer_record_fields env empty_substitution [] fields with
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
              | TVar _ -> (
                  let row_var = fresh_row_var () in
                  let expected_base = TRecord ([], Some row_var) in
                  match unify spread_type' expected_base with
                  | Error e -> Error (error_at ~code:e.code ~message:e.message spread_expr)
                  | Ok subst3 ->
                      let final_subst = compose_substitution subst subst3 in
                      let result_row = Some (apply_substitution subst3 row_var) in
                      Ok (final_subst, Types.canonicalize_mono_type (TRecord (field_types, result_row))))
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
              | TString | TFloat | TBool | TNull | TArray _ | THash _ | TRecord _ | TRowVar _ | TFun _ | TUnion _
              | TEnum _ -> (
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
  | AST.ExpressionStmt expr -> infer_expression type_map env expr
  | AST.Return expr -> infer_expression type_map env expr
  | AST.Block stmts -> infer_block type_map env stmts
  | AST.Let let_binding -> infer_let type_map env let_binding.name let_binding.value let_binding.type_annotation
  | AST.EnumDef { name; type_params; variants } -> (
      (* Register the enum in the registry *)
      (* Convert type expressions to mono_types, treating type_params as TVar *)
      let convert_type_expr (te : AST.type_expr) : (mono_type, Diagnostic.t) result =
        let enum_err msg = Error (error ~code:"type-constructor" ~message:msg) in
        let rec convert = function
          | AST.TVar v -> Ok (TVar v)
          | AST.TCon c ->
              if List.mem c type_params then
                Ok (TVar c)
              else
                Annotation.type_expr_to_mono_type (AST.TCon c)
          | AST.TApp (con_name, args) -> (
              if List.mem con_name type_params then
                enum_err (Printf.sprintf "Type parameter '%s' cannot be used as type constructor" con_name)
              else
                match con_name with
                | "list" -> (
                    match args with
                    | [ elem ] ->
                        let* t = convert elem in
                        Ok (TArray t)
                    | _ -> enum_err "list expects 1 argument")
                | "map" -> (
                    match args with
                    | [ k; v ] ->
                        let* kt = convert k in
                        let* vt = convert v in
                        Ok (THash (kt, vt))
                    | _ -> enum_err "map expects 2 arguments")
                | _ -> enum_err (Printf.sprintf "Unknown type constructor in enum: %s" con_name))
          | AST.TArrow (params, ret, is_effectful) ->
              let* param_types = map_result convert params in
              let* ret_type = convert ret in
              let mk_fun arg ret = TFun (arg, ret, is_effectful) in
              Ok (List.fold_right mk_fun param_types ret_type)
          | AST.TUnion types ->
              let* converted = map_result convert types in
              Ok (normalize_union converted)
          | AST.TRecord (_fields, _row) -> enum_err "Record types not yet implemented in enum definition"
        in
        convert te
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
  | AST.TraitDef { name; type_param; supertraits; fields; methods } -> (
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
          | AST.TUnion types ->
              let* converted = map_result convert types in
              Ok (normalize_union converted)
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
      let convert_method_sig (m : AST.method_sig) : (Trait_registry.method_sig, Diagnostic.t) result =
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
          | Some gps -> List.map (fun (gp : AST.generic_param) -> (gp.name, gp.constraints)) gps
        in
        let method_effect =
          match m.method_effect with
          | AST.Pure -> `Pure
          | AST.Effectful -> `Effectful
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
          }
      in
      let convert_trait_field (f : AST.record_type_field) : (Types.record_field_type, Diagnostic.t) result =
        let* typ = convert_type_expr f.field_type in
        Ok { Types.name = f.field_name; typ }
      in
      let* method_sigs = map_result convert_method_sig methods in
      let* trait_fields = map_result convert_trait_field fields in
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
      | Ok () -> (
          match Trait_registry.validate_trait_fields name trait_fields with
          | Error msg -> Error (error ~code:"type-constructor" ~message:msg)
          | Ok () ->
              Trait_registry.register_trait trait_def;
              Trait_registry.set_trait_fields name trait_fields;
              Ok (empty_substitution, TNull)))
  | AST.ImplDef { impl_trait_name; impl_type_params; impl_for_type; impl_methods } ->
      let type_param_names = List.map (fun (p : AST.generic_param) -> p.name) impl_type_params in
      let unique_param_names = List.sort_uniq String.compare type_param_names in
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
                let for_type_subst =
                  match Trait_registry.lookup_trait impl_trait_name with
                  | None -> SubstMap.empty
                  | Some td -> (
                      match td.trait_type_param with
                      | None -> SubstMap.empty
                      | Some tp -> SubstMap.singleton tp for_type_mono)
                in
                let find_trait_method_sig method_name =
                  match Trait_registry.lookup_trait impl_trait_name with
                  | None -> None
                  | Some td ->
                      List.find_opt
                        (fun (ms : Trait_registry.method_sig) -> ms.method_name = method_name)
                        td.trait_methods
                in

                let infer_impl_method_body (m : AST.method_impl) :
                    ((Trait_registry.method_sig * substitution) option, Diagnostic.t) result =
                  (* Build type bindings: method generics @ impl type params *)
                  let method_type_bindings =
                    match m.impl_method_generics with
                    | None -> []
                    | Some gps -> List.map (fun (gp : AST.generic_param) -> (gp.name, TVar gp.name)) gps
                  in
                  let all_type_bindings = method_type_bindings @ impl_type_bindings in

                  (* Known types from trait signature *)
                  let trait_method_opt = find_trait_method_sig m.impl_method_name in
                  let known_param_types =
                    match trait_method_opt with
                    | None -> []
                    | Some tm ->
                        List.mapi
                          (fun i (_name, ty) -> (i, apply_substitution for_type_subst ty))
                          tm.method_params
                  in
                  let known_return =
                    match trait_method_opt with
                    | None -> None
                    | Some tm -> Some (apply_substitution for_type_subst tm.method_return_type)
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
                  let method_constraint_snapshots =
                    match m.impl_method_generics with
                    | None -> []
                    | Some gps ->
                        List.map
                          (fun (gp : AST.generic_param) ->
                            let old = Hashtbl.find_opt constraint_store gp.name in
                            add_type_var_constraints gp.name gp.constraints;
                            (gp.name, old))
                          gps
                  in
                  let restore_method_constraints () =
                    List.iter
                      (fun (name, old_opt) ->
                        match old_opt with
                        | Some old -> Hashtbl.replace constraint_store name old
                        | None -> Hashtbl.remove constraint_store name)
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
                        match m.impl_method_generics with
                        | None -> []
                        | Some gps -> List.map (fun (gp : AST.generic_param) -> (gp.name, gp.constraints)) gps
                      in
                      let method_effect =
                        if is_effectful then
                          `Effectful
                        else
                          `Pure
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
                               method_generic_internal_vars = [];
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
                      match Trait_registry.lookup_trait impl_trait_name with
                      | None -> Ok ()
                      | Some trait_def ->
                          let has_default method_name =
                            List.exists
                              (fun (m : Trait_registry.method_sig) ->
                                m.method_name = method_name && m.method_default_impl <> None)
                              trait_def.trait_methods
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
                          check impl_methods
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
                            Trait_registry.register_impl ~source:impl_source impl_def;
                            Ok (method_subst, TNull)))))
  | AST.InherentImplDef { inherent_for_type; inherent_methods } -> (
      let is_known_type_name (name : string) : bool =
        Option.is_some (Annotation.builtin_primitive_type name)
        || Option.is_some (Annotation.builtin_type_constructor_name name)
        || Annotation.lookup_enum_by_source_name name <> None
        || Annotation.lookup_type_alias name <> None
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
        | AST.TApp (_con_name, args) ->
            List.fold_left (fun acc' arg -> collect_target_generic_names ~in_head:false arg acc') acc args
        | AST.TArrow (params, ret, _) ->
            let acc' =
              List.fold_left (fun acc' param -> collect_target_generic_names ~in_head:false param acc') acc params
            in
            collect_target_generic_names ~in_head:false ret acc'
        | AST.TUnion members ->
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
                     (Printf.sprintf "Inherent method '%s' must declare a receiver parameter" m.impl_method_name))
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
              let method_constraint_snapshots =
                match m.impl_method_generics with
                | None -> []
                | Some gps ->
                    List.map
                      (fun (gp : AST.generic_param) ->
                        let old = Hashtbl.find_opt constraint_store gp.name in
                        add_type_var_constraints gp.name gp.constraints;
                        (gp.name, old))
                      gps
              in
              let restore_method_constraints () =
                List.iter
                  (fun (name, old_opt) ->
                    match old_opt with
                    | Some old -> Hashtbl.replace constraint_store name old
                    | None -> Hashtbl.remove constraint_store name)
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
                    | Some gps -> List.map (fun (gp : AST.generic_param) -> (gp.name, gp.constraints)) gps
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
                      | Some gps -> List.map (fun (gp : AST.generic_param) -> (gp.name, gp.constraints)) gps
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
                    match Inherent_registry.register_method ~for_type:target_type_for_preregister stub_sig with
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
                          match method_sig.method_generics with
                          | [] -> method_sig
                          | generics ->
                              let reverse_subst =
                                List.fold_left
                                  (fun acc (param_name, _) ->
                                    let resolved = apply_substitution method_subst (TVar param_name) in
                                    match resolved with
                                    | TVar fresh_name when fresh_name <> param_name ->
                                        SubstMap.add fresh_name (TVar param_name) acc
                                    | _ -> acc)
                                  SubstMap.empty generics
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
                           Collision is resolved at call sites: inherent wins on dot calls,
                           qualified calls can explicitly select the trait. *)
                            register_current_method ()))
              in
              let result = register_methods empty_substitution inherent_methods in
              current_inherent_impl_methods := prev_inherent_impl_methods;
              result))
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
      (* Type aliases are registered for annotation/type conversion *)
      (* Runtime value is unit-like *)
      Ok (empty_substitution, TNull)

(* Simple validation: check that all explicit return statements match expected type *)
and validate_return_statements
    (type_map : type_map) (env : type_env) (expected_type : mono_type) (stmt : AST.statement) :
    (unit, Diagnostic.t) result =
  match stmt.stmt with
  | AST.Return expr -> (
      match infer_expression type_map env expr with
      | Error e -> Error e
      | Ok (_subst, inferred_type) ->
          (* Use subtyping check: inferred must be subtype of expected *)
          if Annotation.is_subtype_of inferred_type expected_type then
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
  | AST.EnumDef _ -> Ok ()
  | AST.TraitDef _ -> Ok () (* TODO: Phase 4.3 - validate trait defs *)
  | AST.ImplDef _ -> Ok () (* TODO: Phase 4.3 - validate impl defs *)
  | AST.InherentImplDef _ -> Ok () (* TODO: Phase 4.5 - validate inherent impl defs *)
  | AST.DeriveDef _ -> Ok () (* TODO: Phase 4.3 - validate derive defs *)
  | AST.TypeAlias _ -> Ok () (* TODO: Phase 4.4 - validate type aliases *)

(* ============================================================
   Pattern Matching Helpers
   ============================================================ *)

(* Check all arms of a match expression and collect their body types *)
and infer_match_arms type_map env scrutinee_type arms subst match_expr =
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
        match infer_match_arm type_map env scrutinee_type arm with
        | Error e -> Error e
        | Ok (arm_subst, body_type) ->
            let new_subst = compose_substitution acc_subst arm_subst in
            loop new_subst (body_type :: arm_types) rest_arms)
  in
  loop subst [] arms

(* Infer the type of a single match arm *)
and infer_match_arm type_map env scrutinee_type arm =
  (* Check patterns and get bindings *)
  match check_patterns arm.AST.patterns scrutinee_type with
  | Error e -> Error e
  | Ok bindings ->
      (* Extend environment with pattern bindings *)
      let env' = List.fold_left (fun e (name, ty) -> TypeEnv.add name (mono_to_poly ty) e) env bindings in
      (* Infer body type *)
      infer_expression type_map env' arm.AST.body

(* Check a list of patterns (for | syntax) against the scrutinee type *)
and check_patterns patterns scrutinee_type =
  match patterns with
  | [] -> Error (error ~code:"type-pattern" ~message:"Match arm must have at least one pattern")
  | first :: rest -> (
      match check_pattern first scrutinee_type with
      | Error e -> Error e
      | Ok bindings ->
          (* Verify rest match same type - TODO: they might bind different vars *)
          let rec check_rest = function
            | [] -> Ok bindings
            | pat :: rest_pats -> (
                match check_pattern pat scrutinee_type with
                | Error e -> Error e
                | Ok _ -> check_rest rest_pats)
          in
          check_rest rest)

(* Check a single pattern against the scrutinee type, return variable bindings *)
and check_pattern pattern scrutinee_type =
  match pattern.AST.pat with
  | AST.PWildcard -> Ok []
  | AST.PVariable name -> Ok [ (name, scrutinee_type) ]
  | AST.PLiteral lit -> (
      let lit_type =
        match lit with
        | AST.LInt _ -> TInt
        | AST.LString _ -> TString
        | AST.LBool _ -> TBool
      in
      match Unify.unify lit_type scrutinee_type with
      | Error e -> Error (error ~code:e.code ~message:e.message)
      | Ok _ -> Ok [])
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
          | Some variant ->
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
                      | Ok new_bindings -> check_fields (bindings_acc @ new_bindings) rest_pats rest_types)
                  | _ -> Error (error ~code:"type-pattern" ~message:"Field pattern count mismatch")
                in
                check_fields [] field_patterns field_types)
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
                Ok (bindings @ rest_bindings)
            | (field : AST.record_pattern_field) :: rest_fields -> (
                let field_name = field.pat_field_name in
                match
                  List.find_opt (fun (f : Types.record_field_type) -> f.name = field_name) scrutinee_fields
                with
                | None ->
                    Error
                      (error ~code:"type-pattern"
                         ~message:
                           (Printf.sprintf "Record pattern field '%s' not found in scrutinee type %s" field_name
                              (to_string scrutinee_type)))
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
                    | Ok field_bindings ->
                        check_fields (field_name :: seen_names) (bindings @ field_bindings) rest_fields))
          in
          check_fields [] [] fields
      | _ ->
          Error
            (error ~code:"type-pattern"
               ~message:
                 (Printf.sprintf "Record pattern doesn't match scrutinee type %s" (to_string scrutinee_type))))

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
  (* Check if the expression is a function with a return type annotation *)
  (* If so, create a partially constrained type for recursion *)
  let inferred_self_type_result =
    match (expr.expr, type_annotation) with
    | AST.Function f, _ -> (
        let return_type_result =
          match f.return_type with
          | Some type_expr -> annotation_or_fresh type_expr
          | None -> Ok (fresh_type_var ())
        in
        match return_type_result with
        | Error _ as e -> e
        | Ok return_type -> (
            let mk_f a b = TFun (a, b, f.is_effectful) in
            let param_types_result =
              List.fold_left
                (fun acc (_name, annot_opt) ->
                  match acc with
                  | Error _ -> acc
                  | Ok rev_types -> (
                      match annot_opt with
                      | None -> Ok (fresh_type_var () :: rev_types)
                      | Some annot -> (
                          match annotation_or_fresh annot with
                          | Error _ as e -> e
                          | Ok t -> Ok (t :: rev_types))))
                (Ok []) f.params
            in
            match param_types_result with
            | Error _ as e -> e
            | Ok rev_param_types ->
                let param_types = List.rev rev_param_types in
                Ok (List.fold_right (fun param_t acc -> mk_f param_t acc) param_types return_type)))
    | _, Some type_expr -> annotation_or_fresh type_expr
    | _, None -> Ok (fresh_type_var ())
  in
  match inferred_self_type_result with
  | Error e -> Error e
  | Ok inferred_self_type -> (
      let self_type =
        if prefer_existing_self && name <> "_" then
          match lookup_top_level_placeholder name with
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
      match infer_expression type_map env_with_self expr with
      | Error e -> Error e
      | Ok (subst1, expr_type) -> (
          (* Unify the inferred type with our placeholder *)
          let self_type' = apply_substitution subst1 self_type in
          let unify_result =
            match unify self_type' expr_type with
            | Ok subst2 -> Ok subst2
            | Error e -> (
                match expr.expr with
                | AST.Function _ -> (
                    match unify_function_shape_ignoring_effect self_type' expr_type with
                    | Ok subst2 -> Ok subst2
                    | Error _ -> Error e)
                | _ -> Error e)
          in
          match unify_result with
          | Error e -> Error (error_at ~code:e.code ~message:e.message expr)
          | Ok subst2 -> (
              let final_subst = compose_substitution subst1 subst2 in
              let inferred_final_type = apply_substitution subst2 expr_type in
              let final_type_result =
                match type_annotation with
                | None -> Ok inferred_final_type
                | Some type_expr -> (
                    match Annotation.type_expr_to_mono_type type_expr with
                    | Error d when d.Diagnostic.code = "type-open-row-rejected" -> Error d
                    | Error d -> Error d
                    | Ok t -> Ok t)
              in
              match final_type_result with
              | Error e -> Error e
              | Ok final_type ->
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
                  Ok (final_subst, final_type))))

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
                  let poly =
                    match let_binding.value.expr with
                    | AST.RecordLit _ | AST.FieldAccess _ -> mono_to_poly stmt_type
                    | _ -> generalize env_subst stmt_type
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
                match let_binding.value.expr with
                | AST.Function f -> (
                    let return_type_result =
                      match f.return_type with
                      | Some type_expr -> annotation_or_fresh type_expr
                      | None -> Ok (fresh_type_var ())
                    in
                    match return_type_result with
                    | Error e -> Error e
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
                                      match annotation_or_fresh type_expr with
                                      | Error _ as e -> e
                                      | Ok t -> Ok (t :: rev_types))))
                            (Ok []) f.params
                        in
                        match param_types_result with
                        | Error e -> Error e
                        | Ok rev_param_types ->
                            let param_types = List.rev rev_param_types in
                            let placeholder =
                              List.fold_right
                                (fun param_type acc -> TFun (param_type, acc, f.is_effectful))
                                param_types return_type
                            in
                            set_top_level_placeholder let_binding.name placeholder;
                            go seen' (TypeEnv.add let_binding.name (mono_to_poly placeholder) env_acc) rest))
                | _ ->
                    (* Keep value bindings strict-order for now; only function
                       declarations participate in top-level forward references. *)
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
                   ~message:(Printf.sprintf "Duplicate type alias definition: %s" alias_def.alias_name)
                   stmt)
            else (
              Annotation.register_type_alias alias_def;
              go (StringSet.add alias_def.alias_name seen) rest)
        | _ -> go seen rest)
  in
  go StringSet.empty program

let infer_program ?(env = empty_env) ?state (program : AST.program) :
    (type_env * type_map * mono_type) infer_result =
  let state = Option.value state ~default:(create_inference_state ()) in
  with_inference_state state (fun () ->
      Annotation.clear_type_aliases ();
      Inherent_registry.clear ();
      clear_method_resolution_store ();
      clear_type_var_user_names ();
      clear_top_level_placeholders ();
      match resolve_program_symbols env program with
      | Error e -> Error e
      | Ok () -> (
          match predeclare_top_level_type_aliases program with
          | Error e -> Error e
          | Ok () -> (
          let type_map = create_type_map () in
          let register_top_level_declaration seen_traits seen_enums seen_aliases (stmt : AST.statement) =
            match stmt.stmt with
            | AST.TypeAlias alias_def ->
                if StringSet.mem alias_def.alias_name seen_aliases then
                  Error
                    (error ~code:"type-constructor"
                       ~message:(Printf.sprintf "Duplicate type alias definition: %s" alias_def.alias_name))
                else
                  Ok (seen_traits, seen_enums, StringSet.add alias_def.alias_name seen_aliases)
            | AST.TraitDef trait_def ->
                if StringSet.mem trait_def.name seen_traits then
                  Error
                    (error ~code:"type-constructor"
                       ~message:(Printf.sprintf "Duplicate trait definition: %s" trait_def.name))
                else
                  Ok (StringSet.add trait_def.name seen_traits, seen_enums, seen_aliases)
            | AST.EnumDef enum_def ->
                if StringSet.mem enum_def.name seen_enums then
                  Error
                    (error ~code:"type-constructor"
                       ~message:(Printf.sprintf "Duplicate enum definition: %s" enum_def.name))
                else
                  Ok (seen_traits, StringSet.add enum_def.name seen_enums, seen_aliases)
            | _ -> Ok (seen_traits, seen_enums, seen_aliases)
          in
          let infer_top_level_stmt env (stmt : AST.statement) =
            match stmt.stmt with
            | AST.Let let_binding ->
                infer_let ~prefer_existing_self:true type_map env let_binding.name let_binding.value
                  let_binding.type_annotation
            | _ -> infer_statement type_map env stmt
          in
          let add_let_binding env (stmt : AST.statement) stmt_type =
            match stmt.stmt with
            | AST.Let let_binding ->
                if let_binding.name = "_" then
                  env
                else
                  let env_for_generalize = TypeEnv.remove let_binding.name env in
                  let poly =
                    match let_binding.value.expr with
                    | AST.RecordLit _ | AST.FieldAccess _ -> mono_to_poly stmt_type
                    | _ -> generalize env_for_generalize stmt_type
                  in
                  TypeEnv.add let_binding.name poly env
            | _ -> env
          in
          let rec go env subst seen_traits seen_enums seen_aliases (stmts : AST.statement list) =
            match stmts with
            | [] -> Ok (env, subst, TNull)
            | [ stmt ] -> (
                match register_top_level_declaration seen_traits seen_enums seen_aliases stmt with
                | Error e -> Error e
                | Ok (_seen_traits', _seen_enums', _seen_aliases') -> (
                    match infer_top_level_stmt env stmt with
                    | Error e -> Error e
                    | Ok (stmt_subst, stmt_type) ->
                        let final_subst = compose_substitution subst stmt_subst in
                        let env' = apply_substitution_env final_subst env in
                        let stmt_type' = apply_substitution final_subst stmt_type in
                        let env'' = add_let_binding env' stmt stmt_type' in
                        Ok (env'', final_subst, stmt_type')))
            | stmt :: rest -> (
                match register_top_level_declaration seen_traits seen_enums seen_aliases stmt with
                | Error e -> Error e
                | Ok (seen_traits', seen_enums', seen_aliases') -> (
                    match infer_top_level_stmt env stmt with
                    | Error e -> Error e
                    | Ok (stmt_subst, stmt_type) ->
                        let subst' = compose_substitution subst stmt_subst in
                        let env' = apply_substitution_env stmt_subst env in
                        let env'' = add_let_binding env' stmt stmt_type in
                        go env'' subst' seen_traits' seen_enums' seen_aliases' rest))
          in
          match predeclare_top_level_lets env program with
          | Error e -> Error e
          | Ok env_with_placeholders -> (
              match
                go env_with_placeholders empty_substitution StringSet.empty StringSet.empty StringSet.empty
                  program
              with
              | Error e -> Error e
              | Ok (env', final_subst, result_type) ->
                  apply_substitution_type_map final_subst type_map;
                  apply_substitution_method_type_args_store final_subst;
                  Ok (env', type_map, result_type)))))

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
    | AST.EnumDef _ | AST.TraitDef _ | AST.DeriveDef _ | AST.TypeAlias _ -> []

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

  let%test "infer underscore is not available as value after let discard" =
    match infer_string "let _ = 5; _" with
    | Error diag -> is_code diag "type-unbound-var" && contains_substring diag.message "Unbound variable: _"
    | _ -> false

  let%test "infer let with function" =
    (* fn f(x) = x + 1; f(5) should be Int *)
    infers_to "fn f(x) = x + 1\nf(5)" TInt

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

  let%test "infer type alias for record annotation" =
    infers_to "type Point = { x: Int, y: Int }\nlet p: Point = { x: 1, y: 2 }\np.x" TInt

  let%test "top-level let annotation can forward-reference a later type alias" =
    infers_to "let p: Point = { x: 1, y: 2 }\ntype Point = { x: Int, y: Int }\np.x" TInt

  let%test "top-level fn signature can forward-reference a later type alias" =
    infers_to
      "fn get_x(p: Point) -> Int = p.x\ntype Point = { x: Int, y: Int }\nget_x({ x: 1, y: 2 })"
      TInt

  let%test "infer generic type alias annotation" =
    infers_to "type Box[a] = { value: a }\nlet p: Box[Str] = { value: \"ok\" }\np.value" TString

  let%test "duplicate trait definition in one program is rejected" =
    match infer_string "trait Ping[a] = { fn ping(x: a) -> Int }\ntrait Ping[a] = { fn pong(x: a) -> Int }\n1" with
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
        let needle = "Duplicate enum definition: Dup" in
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

  let%test "duplicate type alias definition in one program is rejected" =
    match infer_string "type Point = { x: Int }\ntype Point = { y: Int }\n1" with
    | Error diag when is_code diag "type-constructor" ->
        let msg = diag.message in
        let needle = "Duplicate type alias definition: Point" in
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
        "trait Named = { name: Str }\ntrait Shown[a] = { fn show(x: a) -> Str }\nfn get_name[t: Named & Shown](x: t) -> Str = x.name\nlet p = { name: \"alice\" }\nget_name(p)"
    with
    | Error diag when String_utils.contains_substring ~needle:"type-trait" diag.code ->
        String_utils.contains_substring ~needle:"Trait obligation failed" diag.message
        && String_utils.contains_substring ~needle:"does not implement trait" diag.message
    | _ -> false

  let%test "constrained field access rejects fields not guaranteed by constraints" =
    match infer_string "trait Named = { name: Str }\nfn get_age[t: Named](x: t) = x.age\n1" with
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
      "type Person = { name: Str, age: Int }\ntrait Named = { name: Str }\ntrait Shown[a] = { fn show(x: a) -> Str }\nimpl Shown[Person] = {\n  fn show(x: Person) -> Str = x.name\n}\nfn get_name[t: Named & Shown](x: t) -> Str = x.name\nlet p: Person = { name: \"alice\", age: 42 }\nget_name(p)"
      TString

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
    infers_to
      "fn get_x(r) = r.x\nlet a = get_x({ x: 1, y: true })\nlet b = get_x({ x: 2, z: \"s\" })\na + b" TInt

  let%test "multiple field access with closed record annotation works" =
    infers_to "let p = { x: 5, y: 10 }\nfn sum_xy(r: { x: Int, y: Int }) = r.x + r.y\nsum_xy(p)" TInt

  let%test "infer record match pattern with punning" =
    infers_to "let p = { x: 10, y: 20 }\nmatch p { case { x:, y: }: x + y case _: 0 }" TInt

  let%test "infer record match pattern with rest binding" =
    infers_to "let p = { x: 10, y: 20, z: 30 }\nmatch p { case { x:, ...rest }: x + rest.y case _: 0 }" TInt

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
    let code =
      "fn sum(arr, i) = if (i == 0) { arr[0] } else { arr[i] + sum(arr, i - 1) }\nsum([1, 2, 3], 2)"
    in
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
    let code = "enum Option[a] = { Some(a), None }
let x = Option.Some(42)
match x {
  case Option.Some(v): v
}" in
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
fn f[a: Show](x: a) -> Str = x.show()
f"
    in
    let unconstrained_code = "fn id(x) = x\nid([1, 2, 3])" in
    match infer_string constrained_code with
    | Error _ -> false
    | Ok _ -> (
        match infer_string unconstrained_code with
        | Error _ -> false
        | Ok (_, _, inferred_type) -> inferred_type = TArray TInt)

  let%test "obligations_from_substitution creates one obligation per trait" =
    clear_constraint_store ();
    add_type_var_constraints "t0" [ "show"; "eq" ];
    let obligations = obligations_from_substitution (substitution_of_list [ ("t0", TInt) ]) in
    List.length obligations = 2
    && List.exists (fun (o : obligation) -> o.trait_name = "show" && o.typ = TInt) obligations
    && List.exists (fun (o : obligation) -> o.trait_name = "eq" && o.typ = TInt) obligations

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
        contains_substring diag.message "Trait obligation failed"
        && contains_substring diag.message "type variable 't0'"

  let%test "add_type_var_constraints lowers field requirements from supertraits" =
    Trait_registry.clear ();
    Trait_registry.register_trait
      { trait_name = "named"; trait_type_param = None; trait_supertraits = []; trait_methods = [] };
    Trait_registry.set_trait_fields "named" [ { name = "name"; typ = TString } ];
    Trait_registry.register_trait
      { trait_name = "labeled"; trait_type_param = None; trait_supertraits = [ "named" ]; trait_methods = [] };
    Trait_registry.set_trait_fields "labeled" [];
    clear_constraint_store ();
    add_type_var_constraints "t0" [ "labeled" ];
    let lowered = lookup_type_var_constrained_fields "t0" in
    List.exists
      (fun (_trait_name, (field : Types.record_field_type)) -> field.name = "name" && field.typ = TString)
      lowered

  let%test "add_type_var_constraints refreshes lowered fields after merged constraints" =
    Trait_registry.clear ();
    Trait_registry.register_trait
      { trait_name = "named"; trait_type_param = None; trait_supertraits = []; trait_methods = [] };
    Trait_registry.register_trait
      { trait_name = "aged"; trait_type_param = None; trait_supertraits = []; trait_methods = [] };
    Trait_registry.set_trait_fields "named" [ { name = "name"; typ = TString } ];
    Trait_registry.set_trait_fields "aged" [ { name = "age"; typ = TInt } ];
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
    Trait_registry.register_trait
      { trait_name = "named"; trait_type_param = None; trait_supertraits = []; trait_methods = [] };
    Trait_registry.set_trait_fields "named" [ { name = "name"; typ = TString } ];
    clear_constraint_store ();
    add_type_var_constraints "t0" [ "named" ];
    let had_fields = lookup_type_var_constrained_fields "t0" <> [] in
    clear_constraint_store ();
    had_fields && lookup_type_var_constrained_fields "t0" = []

  let%test "constrained type-variable method lookup reports ambiguity" =
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
        contains_substring msg "Ambiguous method 'render'"

  let%test "inherent method call resolves for concrete receiver" =
    let code =
      "type Point = { x: Int, y: Int }\nimpl Point = { fn sum(p: Point) -> Int = p.x + p.y }\nlet p: Point = { x: 1, y: 2 }\np.sum()"
    in
    infers_to code TInt

  let%test "inherent method call resolves for type-application receiver" =
    let code = "impl List[Int] = { fn size(xs: List[Int]) -> Int = 1 }\n[1, 2, 3].size()" in
    infers_to code TInt

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
      "trait Show[a] = { fn show(x: a) -> Str }\ntype Point = { x: Int }\nimpl Point = { fn show(p: Point) -> Str = \"p\" }\nfn f[t: Show](x: t) -> Str = x.show()\nlet p: Point = { x: 1 }\nf(p)"
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
        "trait Named = { name: Str }\nfn get[t: Named](x: t) -> Str = x.name\nget"
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
        "trait Named = { name: Str }\nfn get[t: Named](x: t) -> Str = x.name\nget"
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
    match Syntax.Parser.parse ~file_id:"<test>" "fn check[a: Show](x: a) -> Str = x.show()" with
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
    let code =
      "let f = if (true) { (x: Int) -> x } else { (x: Int, y: Int) -> x + y }\nf(1)"
    in
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
