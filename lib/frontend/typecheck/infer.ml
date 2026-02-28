(* Type inference using Algorithm W (Hindley-Milner) *)

open Types
open Unify
module AST = Syntax.Ast.AST

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
  top_level_placeholder_store : (string, mono_type) Hashtbl.t;
  symbol_table : (symbol_id, symbol) Hashtbl.t;
  symbol_key_store : (symbol_id, string) Hashtbl.t;
  identifier_symbol_store : (int, symbol_id) Hashtbl.t;
  method_resolution_store : (int, method_resolution) Hashtbl.t;
}

and method_resolution =
  | TraitMethod of string
  | InherentMethod

let create_inference_state () : inference_state =
  {
    fresh_var_counter = ref 0;
    constraint_store = Hashtbl.create 64;
    constrained_field_store = Hashtbl.create 64;
    top_level_placeholder_store = Hashtbl.create 64;
    symbol_table = Hashtbl.create 256;
    symbol_key_store = Hashtbl.create 256;
    identifier_symbol_store = Hashtbl.create 512;
    method_resolution_store = Hashtbl.create 128;
  }

let active_inference_state : inference_state ref = ref (create_inference_state ())
let global_method_resolution_store : (int, method_resolution) Hashtbl.t = Hashtbl.create 256

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
  let raw =
    Int64.of_string ("0x" ^ String.sub hex 0 15)
    |> Int64.logand 0x3fffffffffffffffL
    |> Int64.to_int
  in
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

let register_symbol ~(name : string) ~(kind : symbol_kind) ~(pos : int) ~(end_pos : int) ~(file_id : string option) :
    symbol_id =
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

let symbol_table_bindings () : (symbol_id * symbol) list =
  Hashtbl.to_seq (current_symbol_table ()) |> List.of_seq

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
  let merged =
    match Hashtbl.find_opt store type_var_name with
    | None -> traits
    | Some existing ->
        List.fold_left
          (fun acc trait_name ->
            if List.mem trait_name acc then
              acc
            else
              acc @ [ trait_name ])
          existing traits
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
let clear_method_resolution_store () : unit = Hashtbl.clear global_method_resolution_store

let record_method_resolution (expr : AST.expression) (resolution : method_resolution) : unit =
  Hashtbl.replace global_method_resolution_store expr.id resolution

let lookup_method_resolution (expr_id : int) : method_resolution option =
  Hashtbl.find_opt global_method_resolution_store expr_id

type obligation_reason = GenericConstraint of string

type obligation = {
  trait_name : string;
  typ : mono_type;
  reason : obligation_reason;
}

let obligation_reason_to_string = function
  | GenericConstraint type_var_name -> Printf.sprintf "constraint on type variable '%s'" type_var_name

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
      | TVar _ ->
          (* Still unresolved - keep waiting until a concrete type is known. *)
          []
      | _ ->
          List.map
            (fun trait_name -> { trait_name; typ = concrete_type; reason = GenericConstraint var_name })
            constraints
  in
  List.fold_left
    (fun acc (var_name, resolved_type) -> List.rev_append (build_for_var var_name resolved_type) acc)
    [] subst
  |> List.rev

let verify_obligation (o : obligation) : (unit, string) result =
  match Trait_solver.satisfies_trait o.typ o.trait_name with
  | Ok () -> Ok ()
  | Error msg ->
      let reason = obligation_reason_to_string o.reason in
      Error (Printf.sprintf "Trait obligation failed (%s): %s" reason msg)

let verify_obligations (obligations : obligation list) : (unit, string) result =
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
  ConstraintCtx.add type_var traits ctx

let lookup_constraints (ctx : constraint_ctx) (type_var : string) : string list =
  match ConstraintCtx.find_opt type_var ctx with
  | Some traits -> traits
  | None -> []

(* Apply substitution to constraint context - when t0 -> Int, update constraints *)
let apply_substitution_constraints (subst : substitution) (ctx : constraint_ctx) : constraint_ctx =
  ConstraintCtx.fold
    (fun var traits acc ->
      match List.assoc_opt var subst with
      | Some (TVar new_var) ->
          (* Type var substituted to another type var - move constraints *)
          ConstraintCtx.add new_var traits acc
      | Some concrete_type -> (
          (* Type var substituted to concrete type - check constraints *)
          match Trait_solver.check_constraints concrete_type traits with
          | Ok () -> acc (* Constraints satisfied, remove from context *)
          | Error msg -> failwith msg (* Constraint violation *))
      | None ->
          (* No substitution for this var, keep constraint *)
          ConstraintCtx.add var traits acc)
    ctx empty_constraints

(* Verify that all type variable constraints in the global constraint_store
   are satisfied after applying a substitution.
   Returns Error if any constraint is violated. *)
let verify_constraints_in_substitution (subst : substitution) : (unit, string) result =
  let obligations = obligations_from_substitution subst in
  verify_obligations obligations

let enforce_trait_requirement_on_type (typ : mono_type) (trait_name : string) : (unit, string) result =
  if Trait_registry.lookup_trait trait_name = None then
    Ok ()
  else
    let typ' = canonicalize_mono_type typ in
    match typ' with
    | TVar type_var_name ->
        add_type_var_constraints type_var_name [ trait_name ];
        Ok ()
    | _ -> Trait_solver.satisfies_trait typ' trait_name

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
  | TUnion members -> List.fold_left (fun acc t -> TypeVarSet.union acc (row_vars_in_type t)) TypeVarSet.empty members
  | TEnum (_, args) -> List.fold_left (fun acc t -> TypeVarSet.union acc (row_vars_in_type t)) TypeVarSet.empty args

let is_trait_object_row = function
  | TRowVar row_name -> has_prefix row_name "trait_obj_row_"
  | _ -> false

(* Reset counter (useful for testing to get predictable names) *)
let reset_fresh_counter () =
  let state = !active_inference_state in
  state.fresh_var_counter := 0;
  clear_constraint_store ();
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
    List.map
      (fun var ->
        if TypeVarSet.mem var row_vars then
          (var, fresh_row_var ())
        else
          (var, fresh_type_var ()))
      quantified_vars
  in
  (* Copy constraints from old type vars to new ones *)
  List.iter
    (fun (old_var, new_type) ->
      match new_type with
      | TVar new_var ->
          let constraints = lookup_type_var_constraints old_var in
          if constraints <> [] then
            add_type_var_constraints new_var constraints
      | _ -> ())
    subst;
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

(* The kind of type error (without position info) *)
type error_kind =
  | UnboundVariable of string
  | UnificationError of unify_error
  | InvalidOperator of string * mono_type
  | NonFunctionCall of mono_type
  | IfBranchMismatch of mono_type * mono_type
  | IfConditionNotBool of mono_type
  | ArrayElementMismatch of mono_type * mono_type
  | HashKeyMismatch of mono_type * mono_type
  | HashValueMismatch of mono_type * mono_type
  | NotIndexable of mono_type
  | IndexTypeMismatch of mono_type * mono_type (* expected, got *)
  | EmptyArrayUnknownType
  | EmptyHashUnknownType
  | ReturnTypeMismatch of mono_type * mono_type (* expected, got *)
  | IfExpressionWithoutElse (* if-value used but no else branch *)
  | ConstructorError of string (* generic constructor error *)
  | PatternError of string (* pattern matching error *)
  | MatchError of string (* match expression error *)
  | PurityViolation of string (* pure function calls effectful operation *)

(* An error with optional position info *)
type infer_error = {
  kind : error_kind;
  pos : int option; (* byte offset in source, if available *)
  end_pos : int option; (* end byte offset in source, if available *)
  file_id : string option;
}

(* Create an error without position *)
let error kind = { kind; pos = None; end_pos = None; file_id = None }

(* Create an error with position from an expression *)
let error_at kind (expr : AST.expression) =
  { kind; pos = Some expr.pos; end_pos = Some expr.end_pos; file_id = expr.file_id }

(* Create an error with position from a statement *)
let error_at_stmt kind (stmt : AST.statement) =
  { kind; pos = Some stmt.pos; end_pos = Some stmt.end_pos; file_id = stmt.file_id }

let error_kind_to_string = function
  | UnboundVariable name -> "Unbound variable: " ^ name
  | UnificationError err -> Unify.error_to_string err
  | InvalidOperator (op, t) -> "Invalid operator " ^ op ^ " for type " ^ to_string t
  | NonFunctionCall t -> "Cannot call non-function type: " ^ to_string t
  | IfBranchMismatch (t1, t2) -> "If branches have different types: " ^ to_string t1 ^ " vs " ^ to_string t2
  | IfConditionNotBool t -> "If condition must be Bool, got: " ^ to_string t
  | ArrayElementMismatch (expected, got) ->
      "Array element type mismatch: expected " ^ to_string expected ^ ", got " ^ to_string got
  | HashKeyMismatch (expected, got) ->
      "Hash key type mismatch: expected " ^ to_string expected ^ ", got " ^ to_string got
  | HashValueMismatch (expected, got) ->
      "Hash value type mismatch: expected " ^ to_string expected ^ ", got " ^ to_string got
  | NotIndexable t -> "Cannot index into type: " ^ to_string t
  | IndexTypeMismatch (expected, got) ->
      "Index type mismatch: expected " ^ to_string expected ^ ", got " ^ to_string got
  | EmptyArrayUnknownType -> "Cannot infer type of empty array"
  | EmptyHashUnknownType -> "Cannot infer type of empty hash"
  | ReturnTypeMismatch (expected, got) ->
      "Function return type annotation mismatch: expected "
      ^ to_string expected
      ^ " but inferred "
      ^ to_string got
  | IfExpressionWithoutElse -> "If-expression requires else branch when value is used"
  | ConstructorError msg -> msg
  | PatternError msg -> msg
  | MatchError msg -> msg
  | PurityViolation msg -> msg

let error_to_string (err : infer_error) : string = error_kind_to_string err.kind

(* Result type for inference *)
type 'a infer_result = ('a, infer_error) result

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
    (symbol_scope, infer_error) result =
  let rec go (scope : symbol_scope) (seen_lets : StringSet.t) (stmts : AST.statement list) :
      (symbol_scope, infer_error) result =
    match stmts with
    | [] -> Ok scope
    | stmt :: rest -> (
        match stmt.stmt with
        | AST.Let let_binding ->
            if let_binding.name = "_" then
              go scope seen_lets rest
            else if StringSet.mem let_binding.name seen_lets then
              Error
                {
                  kind = ConstructorError (Printf.sprintf "Duplicate top-level let definition: %s" let_binding.name);
                  pos = Some stmt.pos;
                  end_pos = Some stmt.end_pos;
                  file_id = stmt.file_id;
                }
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
                    register_symbol ~name:(enum_def.name ^ "." ^ v.variant_name) ~kind:EnumVariantSym ~pos:stmt.pos
                      ~end_pos:stmt.end_pos ~file_id:stmt.file_id
                  in
                  ())
                enum_def.variants
            in
            go scope seen_lets rest
        | _ -> go scope seen_lets rest)
  in
  go root_scope StringSet.empty program

let resolve_let_binding_symbol
    ~(is_top_level : bool)
    (stack : symbol_scope_stack)
    (stmt : AST.statement)
    (name : string) : symbol_scope_stack =
  if name = "_" then
    stack
  else if is_top_level then (
    match lookup_scope_binding stack name with
    | Some _ -> stack
    | None ->
        let sid = register_symbol ~name ~kind:TopLevelLet ~pos:stmt.pos ~end_pos:stmt.end_pos ~file_id:stmt.file_id in
        add_scope_binding stack name sid)
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
        register_symbol ~name ~kind:ImplMethodParam ~pos:m.impl_method_body.pos ~end_pos:m.impl_method_body.end_pos
          ~file_id:m.impl_method_body.file_id
      in
      add_scope_binding acc name sid)
    stack m.impl_method_params

let rec resolve_pattern_bindings (stack : symbol_scope_stack) (pat : AST.pattern) : symbol_scope_stack =
  match pat.pat with
  | AST.PWildcard | AST.PLiteral _ -> stack
  | AST.PVariable name ->
      let sid = register_symbol ~name ~kind:PatternVar ~pos:pat.pos ~end_pos:pat.end_pos ~file_id:pat.file_id in
      add_scope_binding stack name sid
  | AST.PConstructor (_enum_name, _variant_name, fields) ->
      List.fold_left resolve_pattern_bindings stack fields
  | AST.PRecord (fields, rest) ->
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
      (match rest with
      | Some rest_name ->
          let sid =
            register_symbol ~name:rest_name ~kind:PatternVar ~pos:pat.pos ~end_pos:pat.end_pos ~file_id:pat.file_id
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
  | AST.If (cond, cons, alt) ->
      resolve_expr_symbols stack cond;
      ignore (resolve_stmt_symbols stack ~is_top_level:false cons);
      (match alt with
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
  | AST.RecordLit (fields, spread) ->
      List.iter
        (fun (f : AST.record_field) ->
          match f.field_value with
          | Some v -> resolve_expr_symbols stack v
          | None -> ())
        fields;
      (match spread with
      | Some s -> resolve_expr_symbols stack s
      | None -> ())
  | AST.FieldAccess (receiver, _field_name) -> resolve_expr_symbols stack receiver
  | AST.MethodCall (receiver, _method_name, args) ->
      resolve_expr_symbols stack receiver;
      List.iter (resolve_expr_symbols stack) args

and resolve_match_arm_symbols (stack : symbol_scope_stack) (arm : AST.match_arm) : unit =
  let arm_scope =
    match arm.patterns with
    | [] -> push_scope stack
    | first :: _ -> resolve_pattern_bindings (push_scope stack) first
  in
  resolve_expr_symbols arm_scope arm.body

and resolve_impl_method_symbols (stack : symbol_scope_stack) (m : AST.method_impl) : unit =
  let method_scope = resolve_impl_method_params (push_scope stack) m in
  resolve_expr_symbols method_scope m.impl_method_body

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
        register_symbol ~name:enum_def.name ~kind:EnumSym ~pos:stmt.pos ~end_pos:stmt.end_pos ~file_id:stmt.file_id
      in
      let with_enum = add_scope_binding stack enum_def.name sid in
      List.iter
        (fun (v : AST.variant_def) ->
          ignore
            (register_symbol ~name:(enum_def.name ^ "." ^ v.variant_name) ~kind:EnumVariantSym ~pos:stmt.pos
               ~end_pos:stmt.end_pos ~file_id:stmt.file_id))
        enum_def.variants;
      with_enum
  | AST.TraitDef trait_def ->
      ignore
        (register_symbol ~name:trait_def.name ~kind:TraitSym ~pos:stmt.pos ~end_pos:stmt.end_pos ~file_id:stmt.file_id);
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

let resolve_program_symbols (env : type_env) (program : AST.program) : (unit, infer_error) result =
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
        | None -> Error (error_at (UnboundVariable name) expr)
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
        | Ok (subst1, _expr_type) ->
            (* Convert type annotation to mono_type (for validation, not currently used) *)
            let _check_type = Annotation.type_expr_to_mono_type type_ann in
            (* Result is always bool *)
            Ok (subst1, TBool))
    (* If expressions *)
    | AST.If (condition, consequence, alternative) -> infer_if type_map env condition consequence alternative
    (* Function literals *)
    | AST.Function f ->
        (* Phase 2: Use parameter and return type annotations to guide inference *)
        (* Phase 4.3+: Handle generic parameters with constraints *)
        infer_function_with_annotations type_map env f.generics f.params f.return_type f.is_effectful f.body
    (* Function calls *)
    | AST.Call (func, args) -> infer_call type_map env func args
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
              {
                kind = ConstructorError (Printf.sprintf "Unknown enum constructor: %s.%s" enum_name variant_name);
                pos = Some expr.pos;
                end_pos = Some expr.end_pos;
                file_id = expr.file_id;
              }
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
                    {
                      kind =
                        ConstructorError
                          (Printf.sprintf "%s.%s expects %d arguments, got %d" enum_name variant_name
                             (List.length variant.fields) (List.length args));
                      pos = Some expr.pos;
                      end_pos = Some expr.end_pos;
                      file_id = expr.file_id;
                    }
                else
                  (* Get the enum definition to know type parameters *)
                  match Enum_registry.lookup enum_name with
                  | None ->
                      Error
                        {
                          kind = ConstructorError (Printf.sprintf "Unknown enum: %s" enum_name);
                          pos = Some expr.pos;
                          end_pos = Some expr.end_pos;
                          file_id = expr.file_id;
                        }
                  | Some enum_def -> (
                      (* Create fresh type variables for each type parameter *)
                      let fresh_vars = List.map (fun _ -> fresh_type_var ()) enum_def.type_params in
                      let param_subst = List.combine enum_def.type_params fresh_vars in

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
                            | Error e -> Error (error_at (UnificationError e) expr)
                            | Ok subst2 ->
                                let new_subst = compose_substitution subst_acc subst2 in
                                unify_all new_subst rest1 rest2)
                        | _ -> Error (error_at (ConstructorError "Argument count mismatch") expr)
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
            | Error msg -> Error (error_at (MatchError msg) expr)
            | Ok () ->
                (* Check each arm and collect body types *)
                infer_match_arms type_map env' scrutinee_type arms subst expr))
    | AST.RecordLit (fields, spread) -> infer_record_literal type_map env fields spread expr
    | AST.FieldAccess (receiver, variant_name) -> (
        (* Phase 4.3/4.4: Could be field access or nullary enum constructor *)
        (* Check if this is actually a nullary enum constructor (receiver is enum type identifier) *)
        match receiver.expr with
        | AST.Identifier enum_name when Enum_registry.lookup enum_name <> None -> (
            (* This is a nullary enum constructor like option.none or direction.north *)
            (* Redirect to enum constructor logic with empty args *)
            match Enum_registry.lookup_variant enum_name variant_name with
            | None ->
                Error
                  (error_at
                     (ConstructorError (Printf.sprintf "Unknown enum constructor: %s.%s" enum_name variant_name))
                     expr)
            | Some variant -> (
                if List.length variant.fields <> 0 then
                  Error
                    (error_at
                       (ConstructorError
                          (Printf.sprintf "%s.%s expects %d arguments, got 0" enum_name variant_name
                             (List.length variant.fields)))
                       expr)
                else
                  (* Get the enum definition to know type parameters *)
                  match Enum_registry.lookup enum_name with
                  | None -> Error (error_at (ConstructorError (Printf.sprintf "Unknown enum: %s" enum_name)) expr)
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
                                (error_at
                                   (ConstructorError
                                      (Printf.sprintf "Record field '%s' not found in type %s" variant_name
                                         (Types.to_string receiver_type')))
                                   expr)
                          | Some _ -> (
                              (* Open row: the field may live in the tail; constrain it.
                                 Use a fresh tail variable to avoid recursive same-row constraints. *)
                              let field_type = fresh_type_var () in
                              let expected =
                                TRecord ([ { name = variant_name; typ = field_type } ], Some (fresh_row_var ()))
                              in
                              match unify receiver_type' expected with
                              | Error e -> Error (error_at (UnificationError e) expr)
                              | Ok subst2 ->
                                  let final_subst = compose_substitution subst1 subst2 in
                                  Ok (final_subst, apply_substitution subst2 field_type))
                          | None ->
                              Error
                                (error_at
                                   (ConstructorError
                                      (Printf.sprintf "Record field '%s' not found in type %s" variant_name
                                         (Types.to_string receiver_type')))
                                   expr)))
                  | TVar _ -> (
                      match receiver_type' with
                      | TVar type_var_name ->
                          let constraints = lookup_type_var_constraints type_var_name in
                          if constraints = [] then
                            (* Unconstrained TVar: row-polymorphic field access. *)
                            let field_type = fresh_type_var () in
                            let row_var = fresh_row_var () in
                            let expected =
                              TRecord ([ { name = variant_name; typ = field_type } ], Some row_var)
                            in
                            (match unify receiver_type' expected with
                            | Error e -> Error (error_at (UnificationError e) expr)
                            | Ok subst2 ->
                                let final_subst = compose_substitution subst1 subst2 in
                                Ok (final_subst, apply_substitution subst2 field_type))
                          else
                            (* Constrained TVar: only fields guaranteed by constraints are accessible.
                               Do not unify away the type variable here, otherwise we can lose attached
                               trait obligations before call-site verification. *)
                            let expanded_constraints = Trait_solver.expand_constraints_with_supertraits constraints in
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
                                       variant_name type_var_name (String.concat ", " expanded_constraints))
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
                            (match resolve_constrained_field_type constrained_field_types with
                            | Error msg -> Error (error_at (ConstructorError msg) expr)
                            | Ok field_type -> Ok (subst1, field_type))
                      | _ -> Error (error_at (ConstructorError "internal error: expected type variable") expr))
                  | _ ->
                      Error
                        (error_at
                           (ConstructorError
                              (Printf.sprintf "Field access requires record type, got %s"
                                 (Types.to_string receiver_type')))
                           expr)
                in
                field_result))
    | AST.MethodCall (receiver, method_name, args) ->
        let infer_enum_constructor (enum_name : string) : (substitution * mono_type) infer_result =
          match Enum_registry.lookup_variant enum_name method_name with
          | None ->
              Error
                (error_at
                   (ConstructorError (Printf.sprintf "Unknown enum constructor: %s.%s" enum_name method_name))
                   expr)
          | Some variant -> (
              match infer_args type_map env empty_substitution args with
              | Error e -> Error e
              | Ok (subst, arg_types) ->
                  if List.length args <> List.length variant.fields then
                    Error
                      (error_at
                         (ConstructorError
                            (Printf.sprintf "%s.%s expects %d arguments, got %d" enum_name method_name
                               (List.length variant.fields) (List.length args)))
                         expr)
                  else
                    match Enum_registry.lookup enum_name with
                    | None ->
                        Error (error_at (ConstructorError (Printf.sprintf "Unknown enum: %s" enum_name)) expr)
                    | Some enum_def ->
                        let fresh_vars = List.map (fun _ -> fresh_type_var ()) enum_def.type_params in
                        let param_subst = List.combine enum_def.type_params fresh_vars in
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
                              | Error e -> Error (error_at (UnificationError e) expr))
                          | _ -> Error (error_at (ConstructorError "Argument count mismatch") expr)
                        in
                        match unify_all subst arg_types' expected_types with
                        | Error e -> Error e
                        | Ok final_subst ->
                            let result_type_args = List.map (apply_substitution final_subst) fresh_vars in
                            let result_type = TEnum (enum_name, result_type_args) in
                            Ok (final_subst, result_type))
        in
        let infer_real_method_call () : (substitution * mono_type) infer_result =
          match infer_expression type_map env receiver with
          | Error e -> Error e
          | Ok (subst1, receiver_type) ->
              let env1 = apply_substitution_env subst1 env in
              let receiver_type' = apply_substitution subst1 receiver_type in
              let infer_with_method_signature
                  (resolved_method : Trait_registry.method_sig)
                  (resolution : method_resolution) : (substitution * mono_type) infer_result =
                match infer_args type_map env1 subst1 args with
                | Error e -> Error e
                | Ok (subst2, arg_types) ->
                    let all_arg_types = receiver_type' :: arg_types in
                    let expected_param_types = List.map snd resolved_method.method_params in
                    if List.length all_arg_types <> List.length expected_param_types then
                      Error
                        (error_at
                           (ConstructorError
                              (Printf.sprintf "Method '%s' expects %d arguments, got %d" method_name
                                 (List.length expected_param_types - 1)
                                 (List.length args)))
                           expr)
                    else
                      let rec unify_params subst_acc actual_types expected_types =
                        match (actual_types, expected_types) with
                        | [], [] -> Ok subst_acc
                        | actual :: rest_actual, expected :: rest_expected -> (
                            let actual' = apply_substitution subst_acc actual in
                            let expected' = apply_substitution subst_acc expected in
                            match unify actual' expected' with
                            | Error e -> Error (error_at (UnificationError e) expr)
                            | Ok new_subst ->
                                unify_params (compose_substitution new_subst subst_acc) rest_actual rest_expected)
                        | _ -> Error (error_at (ConstructorError "Argument count mismatch") expr)
                      in
                      (match unify_params subst2 all_arg_types expected_param_types with
                      | Error e -> Error e
                      | Ok final_subst ->
                          let return_type = apply_substitution final_subst resolved_method.method_return_type in
                          record_method_resolution expr resolution;
                          Ok (final_subst, return_type))
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
                      (match candidates with
                      | [] -> `Not_found
                      | [ candidate ] -> `Found candidate
                      | many ->
                          let trait_names = many |> List.map fst |> List.sort_uniq String.compare in
                          `Error
                            (Printf.sprintf
                               "Ambiguous method '%s' for constrained type variable '%s' (provided by traits: %s)"
                               method_name type_var_name (String.concat ", " trait_names))))
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
              let inherent_method =
                match receiver_type' with
                | TVar _ -> None
                | _ -> Inherent_registry.lookup_method receiver_type' method_name
              in
              match method_lookup_result with
              | `Error msg -> Error (error_at (ConstructorError msg) expr)
              | `Not_found -> (
                  match inherent_method with
                  | Some inherent_sig -> infer_with_method_signature inherent_sig InherentMethod
                  | None ->
                      Error
                        (error_at
                           (ConstructorError
                              (Printf.sprintf "No method '%s' found for type %s" method_name
                                 (Types.to_string receiver_type')))
                           expr))
              | `Found (trait_name, method_sig) -> (
                  match inherent_method with
                  | Some _ ->
                      Error
                        (error_at
                           (ConstructorError
                              (Printf.sprintf
                                 "Ambiguous method '%s' for type %s (provided by inherent method and trait '%s')"
                                 method_name (Types.to_string receiver_type') trait_name))
                           expr)
                  | None ->
                  let instantiated_method_sig =
                    match Trait_registry.lookup_trait trait_name with
                    | None -> method_sig
                    | Some trait_def -> (
                        match trait_def.trait_type_param with
                        | None -> method_sig
                        | Some type_param ->
                            let subst_type_param = [ (type_param, receiver_type') ] in
                            {
                              Trait_registry.method_name = method_sig.method_name;
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
                    | _ -> Trait_solver.satisfies_trait receiver_type' trait_name
                  in
                  match trait_requirement_result with
                  | Error msg -> Error (error_at (ConstructorError msg) expr)
                  | Ok () -> infer_with_method_signature instantiated_method_sig (TraitMethod trait_name))
        in
        (match receiver.expr with
        | AST.Identifier enum_name ->
            if Enum_registry.lookup enum_name <> None then
              infer_enum_constructor enum_name
            else
              infer_real_method_call ()
        | _ -> infer_real_method_call ())
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
          | Error e -> Error (error_at (UnificationError e) operand)
          | Ok subst2 -> Ok (compose_substitution subst subst2, TBool))
      | "-" -> (
          (* Unary negation requires neg trait. *)
          let operand_type' = apply_substitution subst operand_type in
          match enforce_trait_requirement_on_type operand_type' "neg" with
          | Ok () -> Ok (subst, operand_type')
          | Error msg -> Error (error_at (ConstructorError msg) operand))
      | _ -> Error (error_at (InvalidOperator (op, operand_type)) operand))

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
              | Error e -> Error (error_at (UnificationError e) right)
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
                    | Error msg -> Error (error_at (ConstructorError msg) right)))
          (* Comparison operators: both same type, result Bool *)
          | "<" | ">" | "<=" | ">=" -> (
              match unify left_type' right_type with
              | Error e -> Error (error_at (UnificationError e) right)
              | Ok subst3 -> (
                  let subst' = compose_substitution subst subst3 in
                  let operand_type = apply_substitution subst3 left_type' in
                  match enforce_trait_requirement_on_type operand_type "ord" with
                  | Ok () -> Ok (subst', TBool)
                  | Error msg -> Error (error_at (ConstructorError msg) right)))
          (* Equality operators: both same type, result Bool *)
          | "==" | "!=" -> (
              match unify left_type' right_type with
              | Error e -> Error (error_at (UnificationError e) right)
              | Ok subst3 -> (
                  let subst' = compose_substitution subst subst3 in
                  let operand_type = apply_substitution subst3 left_type' in
                  match enforce_trait_requirement_on_type operand_type "eq" with
                  | Ok () -> Ok (subst', TBool)
                  | Error msg -> Error (error_at (ConstructorError msg) right)))
          | _ -> Error (error_at (InvalidOperator (op, left_type')) left)))

(* ============================================================
   If Expressions
   ============================================================ *)

(* Helper: Detect type narrowing pattern: x is int *)
and detect_is_narrowing (expr : AST.expression) : (string * mono_type) option =
  match expr.expr with
  | AST.TypeCheck (var_expr, type_ann) -> (
      match var_expr.expr with
      | AST.Identifier var_name ->
          let narrow_type = Annotation.type_expr_to_mono_type type_ann in
          Some (var_name, narrow_type)
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
      | Error _ -> Error (error_at (IfConditionNotBool cond_type) condition)
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
    (substitution * mono_type, infer_error) result =
  match stmt.AST.stmt with
  | AST.Return expr -> (
      match infer_expression type_map env expr with
      | Error e -> Error e
      | Ok (subst1, ret_type) -> (
          let subst' = compose_substitution subst subst1 in
          let expected' = apply_substitution subst' expected_ret_type in
          let ret_type' = apply_substitution subst' ret_type in
          match Unify.unify expected' ret_type' with
          | Error e -> Error (error_at (UnificationError e) expr)
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

and infer_function type_map env params body =
  (* Create fresh type variables for each parameter *)
  let param_names =
    List.map
      (fun (p : AST.expression) ->
        match p.expr with
        | AST.Identifier name -> name
        | _ -> failwith "Function parameter must be identifier")
      params
  in
  let param_types = List.map (fun _ -> fresh_type_var ()) param_names in
  (* Extend environment with parameters (monomorphic - no generalization) *)
  let env' =
    List.fold_left2 (fun acc name mono -> TypeEnv.add name (mono_to_poly mono) acc) env param_names param_types
  in
  (* Infer body type *)
  match infer_statement type_map env' body with
  | Error e -> Error e
  | Ok (subst, body_type) -> (
      (* Collect and unify all return types with the body type *)
      let body_type' = apply_substitution subst body_type in
      match collect_and_unify_returns type_map env' body_type' body subst with
      | Error e -> Error e
      | Ok (subst', unified_ret_type) ->
          (* Apply substitution to parameter types *)
          let param_types' = List.map (apply_substitution subst') param_types in
          (* Build function type: p1 -> p2 -> ... -> unified_ret_type *)
          let func_type = List.fold_right (fun param_t acc -> tfun param_t acc) param_types' unified_ret_type in
          Ok (subst', func_type))

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
  | AST.MethodCall (receiver, _, args) ->
      expr_has_effectful_call type_map receiver || List.exists (expr_has_effectful_call type_map) args
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

(* Infer function with parameter type annotations *)
and infer_function_with_annotations type_map env generics_opt params return_annot is_effectful body =
  (* Phase 4.3+: Process generic parameters and their constraints *)
  let type_var_map =
    match generics_opt with
    | None -> []
    | Some generics ->
        (* For each generic parameter like {name="a"; constraints=["show"; "eq"]},
           create a fresh type variable and track its constraints *)
        List.map
          (fun (generic : AST.generic_param) ->
            let fresh_var = fresh_type_var () in
            (* Store constraints in global store *)
            (match fresh_var with
            | TVar n -> add_type_var_constraints n generic.constraints
            | _ -> ());
            (generic.name, fresh_var))
          generics
  in

  (* Extract parameter names and type annotations *)
  let param_info = params in
  (* For each parameter, either use its annotation or create a fresh type variable *)
  let param_types =
    List.map
      (fun (_name, annot_opt) ->
        match annot_opt with
        | None -> fresh_type_var ()
        | Some type_expr -> (
            try
              (* Convert annotation to mono_type *)
              let mono = Annotation.type_expr_to_mono_type type_expr in
              (* Replace generic parameter names (like "a") with their fresh type vars (like "t0") *)
              (* Handle both TVar "a" (if parsed as type variable) and TCon "a" (if parsed as type constructor) *)
              let mono_with_subst =
                List.fold_left
                  (fun ty_acc (gen_name, gen_var) ->
                    (* Substitute both TVar gen_name and references in the type *)
                    let rec subst_generic ty =
                      match ty with
                      | TVar v when v = gen_name -> gen_var
                      | TFun (arg, ret, eff) -> TFun (subst_generic arg, subst_generic ret, eff)
                      | TArray elem -> TArray (subst_generic elem)
                      | THash (k, v) -> THash (subst_generic k, subst_generic v)
                      | TRecord (fields, row) ->
                          let fields' =
                            List.map
                              (fun (f : Types.record_field_type) -> { f with typ = subst_generic f.typ })
                              fields
                          in
                          let row' = Option.map subst_generic row in
                          TRecord (fields', row')
                      | TRowVar r when r = gen_name -> gen_var
                      | TEnum (name, args) -> TEnum (name, List.map subst_generic args)
                      | TUnion types -> TUnion (List.map subst_generic types)
                      | other -> other
                    in
                    subst_generic ty_acc)
                  mono type_var_map
              in
              mono_with_subst
            with
            | Annotation.Open_row_rejected msg -> raise (Annotation.Open_row_rejected msg)
            | Failure msg -> (
                (* If annotation parsing fails (e.g., "Unknown type constructor: a"),
                 check if it's a generic parameter name *)
                let is_generic_ref =
                  match type_expr with
                  | Syntax.Ast.AST.TCon name -> List.assoc_opt name type_var_map
                  | _ -> None
                in
                match is_generic_ref with
                | Some gen_var -> gen_var
                | None ->
                    (* Not a direct generic reference. Preserve annotation errors unless
                       this is the "unknown constructor" form for a declared generic. *)
                    if
                      String.length msg > 0
                      && String.sub msg 0 (min 23 (String.length msg)) = "Unknown type constructor"
                    then
                      (* Extract the name and check if it's in our generics *)
                      let parts = String.split_on_char ':' msg in
                      if List.length parts > 1 then
                        let name = String.trim (List.nth parts 1) in
                        match List.assoc_opt name type_var_map with
                        | Some gen_var -> gen_var
                        | None -> raise (Failure msg)
                      else
                        raise (Failure msg)
                    else
                      raise (Failure msg))))
      param_info
  in
  let param_names = List.map fst param_info in
  (* Extend environment with parameters *)
  let env' =
    List.fold_left2 (fun acc name mono -> TypeEnv.add name (mono_to_poly mono) acc) env param_names param_types
  in
  (* Extract expected return type from annotation if present *)
  let expected_return_type_opt =
    match return_annot with
    | None -> None
    | Some type_expr -> (
        try Some (Annotation.type_expr_to_mono_type type_expr) with
        | Annotation.Open_row_rejected _ as e -> raise e
        | Failure _ -> None)
  in
  (* Infer body type *)
  match infer_statement type_map env' body with
  | Error e -> Error e
  | Ok (subst, body_type) -> (
      let body_type' = apply_substitution subst body_type in
      apply_substitution_type_map subst type_map;
      (* Purity enforcement: check if the body calls effectful operations *)
      let has_effects = body_has_effectful_call type_map body in
      (* Case 1: Pure function (->) calling effectful operation is an error *)
      if (not is_effectful) && Option.is_some return_annot && has_effects then
        Error
          (error_at_stmt
             (PurityViolation
                "Pure function (declared with ->) cannot call effectful operations. Use => to declare an effectful function.")
             body)
      else
        (* Determine actual effectfulness:
           - User wrote =>: keep effectful
           - User wrote -> (pure body verified above): keep pure
           - No annotation: infer from body *)
        let actual_effectful =
          if is_effectful then
            true
          else
            has_effects
        in
        let mk_fun a b = TFun (a, b, actual_effectful) in
        (* If we have a return type annotation, unify with it to ensure type consistency *)
        match expected_return_type_opt with
        | None -> (
            (* No return type annotation - collect all return types and unify with body type *)
            match collect_and_unify_returns type_map env' body_type' body subst with
            | Error e -> Error e
            | Ok (subst', unified_ret_type) ->
                let param_types' = List.map (apply_substitution subst') param_types in
                let func_type =
                  List.fold_right (fun param_t acc -> mk_fun param_t acc) param_types' unified_ret_type
                in
                Ok (subst', func_type))
        | Some expected_ret_type -> (
            (* First, validate all explicit return statements match expected type *)
            match validate_return_statements type_map env' expected_ret_type body with
            | Error e -> Error e
            | Ok () ->
                (* Then check that inferred body type is a subtype of expected return type *)
                (* This catches cases like `fn() -> string { if (...) { "a" } else { 42 } }`
                   where body type is string|int which is NOT a subtype of string *)
                if Annotation.is_subtype_of body_type' expected_ret_type then
                  (* Body type is compatible, now unify to propagate constraints *)
                  match unify body_type' expected_ret_type with
                  | Error _e -> Error (error_at_stmt (ReturnTypeMismatch (expected_ret_type, body_type')) body)
                  | Ok subst2 ->
                      let final_subst = compose_substitution subst subst2 in
                      (* Expose the annotation surface type, not the wider inferred body type.
                         In particular, keep trait-object projection rows opaque so callers
                         cannot recover extra fields by unification side effects. *)
                      let final_return_type = expected_ret_type in
                      (* Type checking is automatic via unification above *)
                      let param_types' = List.map (apply_substitution final_subst) param_types in
                      let func_type =
                        List.fold_right (fun param_t acc -> mk_fun param_t acc) param_types' final_return_type
                      in
                      Ok (final_subst, func_type)
                else
                  Error (error_at_stmt (ReturnTypeMismatch (expected_ret_type, body_type')) body)))

(* ============================================================
   Function Calls
   ============================================================ *)

and infer_call type_map env func args =
  (* Infer function type *)
  match infer_expression type_map env func with
  | Error e -> Error e
  | Ok (subst1, func_type) -> (
      (* Infer argument types *)
      match infer_args type_map (apply_substitution_env subst1 env) subst1 args with
      | Error e -> Error e
      | Ok (subst2, arg_types) -> (
          let func_type' = apply_substitution subst2 func_type in
          (* Create expected function type: arg1 -> arg2 -> ... -> result.
             For unknown/union callees, first try an effect-polymorphic callable
             (pure | effectful) so higher-order callbacks remain flexible.
             Fall back to legacy pure-then-effectful probing. *)
          let fresh_result_type () = fresh_type_var () in
          let expected_func_type_for is_effectful result_type =
            List.fold_right (fun arg_t acc -> TFun (arg_t, acc, is_effectful)) arg_types result_type
          in
          let try_effect_polymorphic_callable () =
            let result_type = fresh_result_type () in
            let expected_pure = expected_func_type_for false result_type in
            let expected_effectful = expected_func_type_for true result_type in
            let expected_union = Types.normalize_union [ expected_pure; expected_effectful ] in
            match unify func_type' expected_union with
            | Ok subst3 -> Ok (subst3, result_type)
            | Error e -> Error e
          in
          let try_legacy_pure_then_effectful () =
            let result_type_pure = fresh_result_type () in
            let expected_pure = expected_func_type_for false result_type_pure in
            match unify func_type' expected_pure with
            | Ok subst3 -> Ok (subst3, result_type_pure)
            | Error pure_err ->
                let result_type_eff = fresh_result_type () in
                let expected_eff = expected_func_type_for true result_type_eff in
                (match unify func_type' expected_eff with
                | Ok subst3 -> Ok (subst3, result_type_eff)
                | Error _ -> Error pure_err)
          in
          let call_result =
            match func_type' with
            | TVar _ -> (
                match try_effect_polymorphic_callable () with
                | Ok _ as ok -> ok
                | Error _ -> try_legacy_pure_then_effectful ())
            | TUnion members ->
                let result_type = fresh_result_type () in
                let expected_pure = expected_func_type_for false result_type in
                let expected_effectful = expected_func_type_for true result_type in
                let expected_union = Types.normalize_union [ expected_pure; expected_effectful ] in
                (match Unify.unify_union_all_with_concrete members expected_union with
                | Ok subst3 -> Ok (subst3, result_type)
                | Error e -> Error e)
            | _ -> try_legacy_pure_then_effectful ()
          in
          match call_result with
          | Error e -> Error (error_at (UnificationError e) func)
          | Ok (subst3, result_type) -> (
              let final_subst = compose_substitution subst2 subst3 in
              (* Phase 4.3+: Verify trait constraints are satisfied.
                 When calling a constrained generic function like fn[a: show](x: a),
                 we need to check that the actual argument type implements the required traits. *)
              match verify_constraints_in_substitution final_subst with
              | Error msg -> Error (error_at (ConstructorError msg) func)
              | Ok () ->
                  let final_result = apply_substitution subst3 result_type in
                  Ok (final_subst, final_result))))

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
          | Error _ -> Error (error_at (ArrayElementMismatch (elem_type', this_type)) elem)
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
          | Error _ -> Error (error_at (HashKeyMismatch (key_type', this_key_type)) k)
          | Ok subst2 -> (
              let subst'' = compose_substitution subst' subst2 in
              let env' = apply_substitution_env subst2 env in
              match infer_expression type_map env' v with
              | Error e -> Error e
              | Ok (subst3, this_val_type) -> (
                  let subst''' = compose_substitution subst'' subst3 in
                  let val_type' = apply_substitution (compose_substitution subst2 subst3) val_type in
                  match unify val_type' this_val_type with
                  | Error _ -> Error (error_at (HashValueMismatch (val_type', this_val_type)) v)
                  | Ok subst4 ->
                      let final_subst = compose_substitution subst''' subst4 in
                      let key_type'' = apply_substitution (compose_substitution subst3 subst4) key_type' in
                      let val_type'' = apply_substitution subst4 val_type' in
                      infer_hash_pairs type_map (apply_substitution_env subst4 env') final_subst key_type''
                        val_type'' rest))))

(* ============================================================
   Records
   ============================================================ *)

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
                  Ok (subst, Types.canonicalize_mono_type (TRecord (merged, base_row)))
              | TVar _ -> (
                  let row_var = fresh_row_var () in
                  let expected_base = TRecord ([], Some row_var) in
                  match unify spread_type' expected_base with
                  | Error e -> Error (error_at (UnificationError e) spread_expr)
                  | Ok subst3 ->
                      let final_subst = compose_substitution subst subst3 in
                      let result_row = Some (apply_substitution subst3 row_var) in
                      Ok (final_subst, Types.canonicalize_mono_type (TRecord (field_types, result_row))))
              | _ ->
                  Error
                    (error_at
                       (ConstructorError
                          (Printf.sprintf "Record spread expects a record, got %s" (to_string spread_type')))
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
              | Error _ -> Error (error_at (IndexTypeMismatch (TInt, index_type)) index_expr)
              | Ok subst3 ->
                  let final_subst = compose_substitution subst subst3 in
                  let elem_type' = apply_substitution subst3 elem_type in
                  Ok (final_subst, elem_type'))
          | THash (key_type, val_type) -> (
              (* Hash index must match key type *)
              match unify index_type key_type with
              | Error _ -> Error (error_at (IndexTypeMismatch (key_type, index_type)) index_expr)
              | Ok subst3 ->
                  let final_subst = compose_substitution subst subst3 in
                  let val_type' = apply_substitution subst3 val_type in
                  Ok (final_subst, val_type'))
          | TString -> (
              (* String index must be Int, returns String *)
              match unify index_type TInt with
              | Error _ -> Error (error_at (IndexTypeMismatch (TInt, index_type)) index_expr)
              | Ok subst3 -> Ok (compose_substitution subst subst3, TString))
          | TVar _ -> (
              (* Unknown container type - could be array or hash *)
              (* Infer the container type based on the index type *)
              match index_type with
              | TInt -> (
                  (* Int index -> assume array *)
                  let elem_type = fresh_type_var () in
                  match unify container_type' (TArray elem_type) with
                  | Error e -> Error (error_at (UnificationError e) container)
                  | Ok subst3 ->
                      let final_subst = compose_substitution subst subst3 in
                      let elem_type' = apply_substitution subst3 elem_type in
                      Ok (final_subst, elem_type'))
              | TString | TFloat | TBool | TNull | TArray _ | THash _ | TRecord _ | TRowVar _ | TFun _ | TUnion _
              | TEnum _ -> (
                  (* Non-int index -> assume hash *)
                  let val_type = fresh_type_var () in
                  match unify container_type' (THash (index_type, val_type)) with
                  | Error e -> Error (error_at (UnificationError e) container)
                  | Ok subst3 ->
                      let final_subst = compose_substitution subst subst3 in
                      let val_type' = apply_substitution subst3 val_type in
                      Ok (final_subst, val_type'))
              | TVar _ -> (
                  (* Index is also unknown - assume array with Int index *)
                  let elem_type = fresh_type_var () in
                  match unify container_type' (TArray elem_type) with
                  | Error e -> Error (error_at (UnificationError e) container)
                  | Ok subst3 -> (
                      match unify index_type TInt with
                      | Error _ -> Error (error_at (IndexTypeMismatch (TInt, index_type)) index_expr)
                      | Ok subst4 ->
                          let final_subst = compose_substitution subst (compose_substitution subst3 subst4) in
                          let elem_type' = apply_substitution (compose_substitution subst3 subst4) elem_type in
                          Ok (final_subst, elem_type'))))
          | _ -> Error (error_at (NotIndexable container_type') container)))

(* ============================================================
   Statements
   ============================================================ *)

and infer_statement type_map env stmt =
  match stmt.stmt with
  | AST.ExpressionStmt expr -> infer_expression type_map env expr
  | AST.Return expr -> infer_expression type_map env expr
  | AST.Block stmts -> infer_block type_map env stmts
  | AST.Let let_binding -> infer_let type_map env let_binding.name let_binding.value let_binding.type_annotation
  | AST.EnumDef { name; type_params; variants } ->
      (* Register the enum in the registry *)
      (* Convert type expressions to mono_types, treating type_params as TVar *)
      let convert_type_expr (te : AST.type_expr) : mono_type =
        let rec convert = function
          | AST.TVar v -> TVar v
          | AST.TCon c ->
              (* Check if it's a type parameter *)
              if List.mem c type_params then
                TVar c
              else
                Annotation.type_expr_to_mono_type (AST.TCon c)
          | AST.TApp (con_name, args) -> (
              if
                (* Check if constructor is a type param (shouldn't happen but handle it) *)
                List.mem con_name type_params
              then
                failwith (Printf.sprintf "Type parameter '%s' cannot be used as type constructor" con_name)
              else
                Annotation.type_expr_to_mono_type (AST.TApp (con_name, List.map (fun _ -> AST.TCon "dummy") args))
                |> fun _ ->
                (* Actually convert properly *)
                match con_name with
                | "list" -> (
                    match args with
                    | [ elem ] -> TArray (convert elem)
                    | _ -> failwith "list expects 1 argument")
                | "map" -> (
                    match args with
                    | [ k; v ] -> THash (convert k, convert v)
                    | _ -> failwith "map expects 2 arguments")
                | _ -> failwith (Printf.sprintf "Unknown type constructor in enum: %s" con_name))
          | AST.TArrow (params, ret) ->
              let param_types = List.map convert params in
              let ret_type = convert ret in
              List.fold_right (fun p r -> tfun p r) param_types ret_type
          | AST.TUnion types -> normalize_union (List.map convert types)
          | AST.TRecord (_fields, _row) -> failwith "Record types not yet implemented in enum definition"
        in
        convert te
      in

      let variant_defs =
        List.map
          (fun (v : AST.variant_def) ->
            let field_types = List.map convert_type_expr v.variant_fields in
            { Enum_registry.name = v.variant_name; fields = field_types })
          variants
      in

      Enum_registry.register { Enum_registry.name; type_params; variants = variant_defs };

      (* Enum def doesn't have a value *)
      Ok (empty_substitution, TNull)
  | AST.TraitDef { name; type_param; supertraits; fields; methods } ->
      (* Convert AST method signatures to trait registry method signatures *)
      (* We need to treat type_param as a type variable, not a type constructor *)
      let convert_type_expr (te : AST.type_expr) : mono_type =
        let is_trait_type_param (type_name : string) : bool =
          match type_param with
          | Some tp -> type_name = tp
          | None -> false
        in
        let rec convert = function
          | AST.TVar v -> TVar v
          | AST.TCon c ->
              (* Check if it's the trait's type parameter *)
              if is_trait_type_param c then
                TVar c
              else
                Annotation.type_expr_to_mono_type (AST.TCon c)
          | AST.TApp (con_name, args) -> (
              if
                (* For generic types, convert recursively *)
                is_trait_type_param con_name
              then
                failwith (Printf.sprintf "Type parameter '%s' cannot be used as type constructor" con_name)
              else
                match con_name with
                | "list" -> (
                    match args with
                    | [ elem ] -> TArray (convert elem)
                    | _ -> failwith "list expects 1 argument")
                | "map" -> (
                    match args with
                    | [ k; v ] -> THash (convert k, convert v)
                    | _ -> failwith "map expects 2 arguments")
                | _ -> Annotation.type_expr_to_mono_type (AST.TApp (con_name, args)))
          | AST.TArrow (params, ret) ->
              let param_types = List.map convert params in
              let ret_type = convert ret in
              List.fold_right (fun p r -> tfun p r) param_types ret_type
          | AST.TUnion types -> normalize_union (List.map convert types)
          | AST.TRecord (fields, row) ->
              let field_types =
                List.map
                  (fun (f : AST.record_type_field) -> { Types.name = f.field_name; typ = convert f.field_type })
                  fields
              in
              let row_type = Option.map convert row in
              TRecord (field_types, row_type)
        in
        convert te
      in
      let convert_method_sig (m : AST.method_sig) : Trait_registry.method_sig =
        let param_types = List.map (fun (pname, ptype) -> (pname, convert_type_expr ptype)) m.method_params in
        let return_type = convert_type_expr m.method_return_type in
        {
          Trait_registry.method_name = m.method_name;
          method_params = param_types;
          method_return_type = return_type;
        }
      in
      let convert_trait_field (f : AST.record_type_field) : Types.record_field_type =
        { Types.name = f.field_name; typ = convert_type_expr f.field_type }
      in
      let result =
        try
          let method_sigs = List.map convert_method_sig methods in
          let trait_fields = List.map convert_trait_field fields in
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
          | Error msg -> Error (error (ConstructorError msg))
          | Ok () -> (
              match Trait_registry.validate_trait_fields name trait_fields with
              | Error msg -> Error (error (ConstructorError msg))
              | Ok () ->
                  Trait_registry.register_trait trait_def;
                  Trait_registry.set_trait_fields name trait_fields;
                  Ok (empty_substitution, TNull))
        with Failure msg -> Error (error (ConstructorError msg))
      in
      result
  | AST.ImplDef { impl_trait_name; impl_type_params; impl_for_type; impl_methods } -> (
      let type_param_names = List.map (fun (p : AST.generic_param) -> p.name) impl_type_params in
      let unique_param_names = List.sort_uniq String.compare type_param_names in
      if List.length unique_param_names <> List.length type_param_names then
        Error
          (error
             (ConstructorError
                (Printf.sprintf "Generic impl '%s' has duplicate type parameter names" impl_trait_name)))
      else
        let impl_type_bindings =
          List.map (fun (p : AST.generic_param) -> (p.name, TVar p.name)) impl_type_params
        in
        let convert_impl_type_expr (te : AST.type_expr) : (mono_type, infer_error) result =
          try Ok (Annotation.type_expr_to_mono_type_with impl_type_bindings te) with
          | Annotation.Open_row_rejected msg -> Error (error (ConstructorError msg))
          | Failure msg -> Error (error (ConstructorError msg))
        in
        let with_impl_type_param_constraints (f : unit -> (substitution * mono_type, infer_error) result) :
            (substitution * mono_type, infer_error) result =
          let constraint_store = current_constraint_store () in
          let constrained_field_store = current_constrained_field_store () in
          let snapshots =
            List.map
              (fun (p : AST.generic_param) ->
                ( p.name,
                  Hashtbl.find_opt constraint_store p.name,
                  Hashtbl.find_opt constrained_field_store p.name ))
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
          List.iter (fun (p : AST.generic_param) -> add_type_var_constraints p.name p.constraints) impl_type_params;
          Fun.protect ~finally:restore f
        in
        with_impl_type_param_constraints (fun () ->
            (* Convert impl_for_type from AST.type_expr to mono_type *)
            match convert_impl_type_expr impl_for_type with
            | Error e -> Error e
            | Ok for_type_mono -> (
                (* Infer and validate method bodies so codegen can rely on a complete type_map. *)
                let infer_impl_method_body (m : AST.method_impl) :
                    ((Trait_registry.method_sig * substitution) option, infer_error) result =
                  let param_types_result =
                    List.fold_left
                      (fun acc (pname, ptype_opt) ->
                        match acc with
                        | Error _ as err -> err
                        | Ok params -> (
                            match ptype_opt with
                            | Some ptype -> (
                                match convert_impl_type_expr ptype with
                                | Ok mono -> Ok (params @ [ (pname, mono) ])
                                | Error _ as err -> err)
                            | None ->
                                Error
                                  (error
                                     (ConstructorError
                                        (Printf.sprintf "Impl method '%s' parameter '%s' is missing a type annotation"
                                           m.impl_method_name pname)))))
                      (Ok []) m.impl_method_params
                  in
                  match param_types_result with
                  | Error e -> Error e
                  | Ok param_types -> (
                      match m.impl_method_return_type with
                      | None ->
                          Error
                            (error
                               (ConstructorError
                                  (Printf.sprintf "Impl method '%s' is missing a return type annotation"
                                     m.impl_method_name)))
                      | Some rt -> (
                          match convert_impl_type_expr rt with
                          | Error e -> Error e
                          | Ok return_type -> (
                              let method_env =
                                List.fold_left
                                  (fun env_acc (pname, ptype) -> TypeEnv.add pname (mono_to_poly ptype) env_acc)
                                  env param_types
                              in
                              match infer_expression type_map method_env m.impl_method_body with
                              | Error e -> Error e
                              | Ok (subst, inferred_body_type) ->
                                  let inferred_body_type' = apply_substitution subst inferred_body_type in
                                  let return_type' = apply_substitution subst return_type in
                                  if Annotation.check_annotation return_type' inferred_body_type' then
                                    Ok
                                      (Some
                                         ( {
                                             Trait_registry.method_name = m.impl_method_name;
                                             method_params = param_types;
                                             method_return_type = return_type;
                                           },
                                           subst ))
                                  else
                                    Error
                                      (error_at
                                         (ReturnTypeMismatch (return_type', inferred_body_type'))
                                         m.impl_method_body))))
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
                          {
                            m with
                            method_params =
                              List.map (fun (name, ty) -> (name, apply_substitution method_subst ty))
                                m.method_params;
                            method_return_type = apply_substitution method_subst m.method_return_type;
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

                    (* Validate and register impl *)
                    match Trait_registry.validate_impl impl_def with
                    | Error msg -> Error (error (ConstructorError msg))
                    | Ok () ->
                        let impl_source : Trait_registry.impl_source =
                          { file_id = stmt.file_id; start_pos = stmt.pos; end_pos = stmt.end_pos }
                        in
                        Trait_registry.register_impl ~source:impl_source impl_def;
                        Ok (method_subst, TNull)))))
  | AST.InherentImplDef { inherent_for_type; inherent_methods } -> (
      let convert_inherent_type_expr (te : AST.type_expr) : (mono_type, infer_error) result =
        try Ok (Annotation.type_expr_to_mono_type te) with
        | Annotation.Open_row_rejected msg -> Error (error (ConstructorError msg))
        | Failure msg -> Error (error (ConstructorError msg))
      in
      let starts_with (prefix : string) (s : string) : bool =
        let len_prefix = String.length prefix in
        String.length s >= len_prefix && String.sub s 0 len_prefix = prefix
      in
      match convert_inherent_type_expr inherent_for_type with
      | Error e -> Error e
      | Ok inherent_for_type_mono ->
          let inherent_for_type_mono = canonicalize_mono_type inherent_for_type_mono in
          let infer_inherent_method_body (m : AST.method_impl) :
              (Trait_registry.method_sig * substitution, infer_error) result =
            let param_types_result =
              List.fold_left
                (fun acc (pname, ptype_opt) ->
                  match acc with
                  | Error _ as err -> err
                  | Ok params -> (
                      match ptype_opt with
                      | Some ptype -> (
                          match convert_inherent_type_expr ptype with
                          | Ok mono -> Ok (params @ [ (pname, mono) ])
                          | Error _ as err -> err)
                      | None ->
                          Error
                            (error
                               (ConstructorError
                                  (Printf.sprintf
                                     "Inherent method '%s' parameter '%s' is missing a type annotation"
                                     m.impl_method_name pname)))))
                (Ok []) m.impl_method_params
            in
            match param_types_result with
            | Error e -> Error e
            | Ok param_types -> (
                if param_types = [] then
                  Error
                    (error
                       (ConstructorError
                          (Printf.sprintf "Inherent method '%s' must declare a receiver parameter"
                             m.impl_method_name)))
                else
                  match m.impl_method_return_type with
                  | None ->
                      Error
                        (error
                           (ConstructorError
                              (Printf.sprintf "Inherent method '%s' is missing a return type annotation"
                                 m.impl_method_name)))
                  | Some rt -> (
                      match convert_inherent_type_expr rt with
                      | Error e -> Error e
                      | Ok return_type -> (
                          let method_env =
                            List.fold_left
                              (fun env_acc (pname, ptype) -> TypeEnv.add pname (mono_to_poly ptype) env_acc)
                              env param_types
                          in
                          match infer_expression type_map method_env m.impl_method_body with
                          | Error e -> Error e
                          | Ok (subst, inferred_body_type) ->
                              let inferred_body_type' = apply_substitution subst inferred_body_type in
                              let return_type' = apply_substitution subst return_type in
                              if Annotation.check_annotation return_type' inferred_body_type' then
                                Ok
                                  ( {
                                      Trait_registry.method_name = m.impl_method_name;
                                      method_params = param_types;
                                      method_return_type = return_type;
                                    },
                                    subst )
                              else
                                Error
                                  (error_at
                                     (ReturnTypeMismatch (return_type', inferred_body_type'))
                                     m.impl_method_body))))
          in
          let rec register_methods subst_acc = function
            | [] -> Ok (subst_acc, TNull)
            | m :: rest -> (
                match infer_inherent_method_body m with
                | Error e -> Error e
                | Ok (method_sig, method_subst) ->
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
                    let receiver_type =
                      match method_sig.method_params with
                      | [] ->
                          failwith "impossible: inherent method parameter list unexpectedly empty after validation"
                      | (_, first_param_type) :: _ -> canonicalize_mono_type first_param_type
                    in
                    let target_type = canonicalize_mono_type inherent_for_type_mono in
                    (match Unify.unify receiver_type target_type with
                    | Error _ ->
                        Error
                          (error
                             (ConstructorError
                                (Printf.sprintf
                                   "Inherent method '%s' receiver type %s does not match impl target type %s"
                                   method_sig.method_name (Types.to_string receiver_type)
                                   (Types.to_string target_type))))
                    | Ok _ ->
                        (match Trait_registry.resolve_method target_type method_sig.method_name with
                        | Ok (trait_name, _) ->
                            Error
                              (error
                                 (ConstructorError
                                    (Printf.sprintf
                                       "Inherent method '%s' for type %s collides with trait method from trait '%s'"
                                       method_sig.method_name (Types.to_string target_type) trait_name)))
                        | Error msg ->
                            if starts_with "No method '" msg then
                              (match Inherent_registry.register_method ~for_type:target_type method_sig with
                              | Error msg -> Error (error (ConstructorError msg))
                              | Ok () ->
                                  let subst' = compose_substitution subst_acc method_subst in
                                  register_methods subst' rest)
                            else
                              Error
                                (error
                                   (ConstructorError
                                      (Printf.sprintf
                                         "Cannot define inherent method '%s' for type %s due to trait method \
                                          ambiguity: %s"
                                         method_sig.method_name (Types.to_string target_type) msg))))))
          in
          register_methods empty_substitution inherent_methods)
  | AST.DeriveDef { derive_traits; derive_for_type } ->
      (* Auto-generate implementations for derived traits *)
      let for_type_mono = Annotation.type_expr_to_mono_type derive_for_type in

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
        Error (error (ConstructorError (String.concat "; " derive_errors)))
      else
        Ok (empty_substitution, TNull)
  | AST.TypeAlias _ ->
      (* Type aliases are registered for annotation/type conversion *)
      (* Runtime value is unit-like *)
      Ok (empty_substitution, TNull)

(* Simple validation: check that all explicit return statements match expected type *)
and validate_return_statements
    (type_map : type_map) (env : type_env) (expected_type : mono_type) (stmt : AST.statement) :
    (unit, infer_error) result =
  match stmt.stmt with
  | AST.Return expr -> (
      match infer_expression type_map env expr with
      | Error e -> Error e
      | Ok (_subst, inferred_type) ->
          (* Use subtyping check: inferred must be subtype of expected *)
          if Annotation.is_subtype_of inferred_type expected_type then
            Ok ()
          else
            Error (error_at_stmt (ReturnTypeMismatch (expected_type, inferred_type)) stmt))
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
        | [] -> Error (error_at (MatchError "Match expression must have at least one arm") match_expr)
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
  | [] -> Error (error (PatternError "Match arm must have at least one pattern"))
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
      | Error e -> Error (error (UnificationError e))
      | Ok _ -> Ok [])
  | AST.PConstructor (enum_name, variant_name, field_patterns) -> (
      (* Check scrutinee is the right enum type *)
      match scrutinee_type with
      | TEnum (sname, type_args) when sname = enum_name -> (
          (* Look up variant *)
          match Enum_registry.lookup_variant enum_name variant_name with
          | None -> Error (error (PatternError (Printf.sprintf "Unknown variant: %s.%s" enum_name variant_name)))
          | Some variant ->
              if List.length field_patterns <> List.length variant.fields then
                Error
                  (error
                     (PatternError
                        (Printf.sprintf "Pattern %s.%s expects %d fields, got %d" enum_name variant_name
                           (List.length variant.fields) (List.length field_patterns))))
              else
                (* Get field types with type args substituted *)
                let enum_def = Option.get (Enum_registry.lookup enum_name) in
                let subst = List.combine enum_def.type_params type_args in
                let field_types = List.map (apply_substitution subst) variant.fields in
                (* Check each field pattern and collect bindings *)
                let rec check_fields bindings_acc pats types =
                  match (pats, types) with
                  | [], [] -> Ok bindings_acc
                  | pat :: rest_pats, ty :: rest_types -> (
                      match check_pattern pat ty with
                      | Error e -> Error e
                      | Ok new_bindings -> check_fields (bindings_acc @ new_bindings) rest_pats rest_types)
                  | _ -> Error (error (PatternError "Field pattern count mismatch"))
                in
                check_fields [] field_patterns field_types)
      | _ ->
          Error
            (error
               (PatternError
                  (Printf.sprintf "Pattern %s.%s doesn't match scrutinee type %s" enum_name variant_name
                     (to_string scrutinee_type)))))
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
                      (error
                         (PatternError
                            (Printf.sprintf "Record pattern field '%s' not found in scrutinee type %s" field_name
                               (to_string scrutinee_type))))
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
            (error
               (PatternError
                  (Printf.sprintf "Record pattern doesn't match scrutinee type %s" (to_string scrutinee_type)))))

(* Unify function shapes while ignoring only the effect flag on arrows.
   Used to reconcile recursive placeholders for unannotated functions that
   may later infer as either pure or effectful. *)
and unify_function_shape_ignoring_effect (left : mono_type) (right : mono_type) :
    (substitution, unify_error) result =
  match (left, right) with
  | TFun (arg_l, ret_l, _), TFun (arg_r, ret_r, _) -> (
      match unify arg_l arg_r with
      | Error e -> Error e
      | Ok subst1 ->
          let ret_l' = apply_substitution subst1 ret_l in
          let ret_r' = apply_substitution subst1 ret_r in
          (match unify_function_shape_ignoring_effect ret_l' ret_r' with
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
  let inferred_self_type =
    match (expr.expr, type_annotation) with
    | AST.Function f, _ -> (
        try
          (* Create a partially constrained function type for recursive calls. *)
          let return_type =
            match f.return_type with
            | Some type_expr -> Annotation.type_expr_to_mono_type type_expr
            | None -> fresh_type_var ()
          in
          let mk_f a b = TFun (a, b, f.is_effectful) in
          let param_types =
            List.map
              (fun (_name, annot_opt) ->
                match annot_opt with
                | None -> fresh_type_var ()
                | Some annot -> Annotation.type_expr_to_mono_type annot)
              f.params
          in
          List.fold_right (fun param_t acc -> mk_f param_t acc) param_types return_type
        with
        | Annotation.Open_row_rejected _ as e -> raise e
        | Failure _ -> fresh_type_var ())
    | _, Some type_expr -> (
        (* Phase 4.4: Use type annotation from let binding *)
        try Annotation.type_expr_to_mono_type type_expr with
        | Annotation.Open_row_rejected _ as e -> raise e
        | Failure _ -> fresh_type_var ())
    | _, None -> fresh_type_var ()
  in
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
      | Error e -> Error (error_at (UnificationError e) expr)
      | Ok subst2 ->
          let final_subst = compose_substitution subst1 subst2 in
          let inferred_final_type = apply_substitution subst2 expr_type in
          let final_type =
            match type_annotation with
            | None -> inferred_final_type
            | Some type_expr -> (
                try Annotation.type_expr_to_mono_type type_expr with
                | Annotation.Open_row_rejected _ as e -> raise e
                | Failure _ -> inferred_final_type)
          in
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
          Ok (final_subst, final_type))

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
                  let poly = generalize env_subst stmt_type in
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
        Error (error_at (IfBranchMismatch (expected_applied, inferred_applied)) expr)

(* ============================================================
   Program Inference
   ============================================================ *)

let predeclare_top_level_lets (env : type_env) (program : AST.program) : (type_env, infer_error) result =
  let rec go (seen : StringSet.t) (env_acc : type_env) (stmts : AST.statement list) :
      (type_env, infer_error) result =
    match stmts with
    | [] -> Ok env_acc
    | stmt :: rest -> (
        match stmt.stmt with
        | AST.Let let_binding when let_binding.name <> "_" ->
            if StringSet.mem let_binding.name seen then
              Error
                {
                  kind = ConstructorError (Printf.sprintf "Duplicate top-level let definition: %s" let_binding.name);
                  pos = Some stmt.pos;
                  end_pos = Some stmt.end_pos;
                  file_id = stmt.file_id;
                }
            else
              let seen' = StringSet.add let_binding.name seen in
              if TypeEnv.mem let_binding.name env_acc then
                go seen' env_acc rest
              else
                (match let_binding.value.expr with
                | AST.Function f ->
                    let return_type =
                      match f.return_type with
                      | Some type_expr -> (
                          try Annotation.type_expr_to_mono_type type_expr with
                          | Annotation.Open_row_rejected _ as e -> raise e
                          | Failure _ -> fresh_type_var ())
                      | None -> fresh_type_var ()
                    in
                    let param_types =
                      List.map
                        (fun (_name, annot_opt) ->
                          match annot_opt with
                          | Some type_expr -> (
                              try Annotation.type_expr_to_mono_type type_expr with
                              | Annotation.Open_row_rejected _ as e -> raise e
                              | Failure _ -> fresh_type_var ())
                          | None -> fresh_type_var ())
                        f.params
                    in
                    let placeholder =
                      List.fold_right (fun param_type acc -> TFun (param_type, acc, f.is_effectful)) param_types
                        return_type
                    in
                    set_top_level_placeholder let_binding.name placeholder;
                    go seen' (TypeEnv.add let_binding.name (mono_to_poly placeholder) env_acc) rest
                | _ ->
                    (* Keep value bindings strict-order for now; only function
                       declarations participate in top-level forward references. *)
                    go seen' env_acc rest)
        | _ -> go seen env_acc rest)
  in
  go StringSet.empty env program

let infer_program ?(env = empty_env) ?state (program : AST.program) :
    (type_env * type_map * mono_type) infer_result =
  let state = Option.value state ~default:(create_inference_state ()) in
  try
    with_inference_state state (fun () ->
      Annotation.clear_type_aliases ();
      Inherent_registry.clear ();
      clear_method_resolution_store ();
      clear_top_level_placeholders ();
      match resolve_program_symbols env program with
      | Error e -> Error e
      | Ok () ->
          let type_map = create_type_map () in
          let register_top_level_declaration seen_traits seen_enums seen_aliases (stmt : AST.statement) =
            match stmt.stmt with
            | AST.TypeAlias alias_def ->
                if StringSet.mem alias_def.alias_name seen_aliases then
                  Error (error (ConstructorError (Printf.sprintf "Duplicate type alias definition: %s" alias_def.alias_name)))
                else (
                  Annotation.register_type_alias alias_def;
                  Ok (seen_traits, seen_enums, StringSet.add alias_def.alias_name seen_aliases))
            | AST.TraitDef trait_def ->
                if StringSet.mem trait_def.name seen_traits then
                  Error (error (ConstructorError (Printf.sprintf "Duplicate trait definition: %s" trait_def.name)))
                else
                  Ok (StringSet.add trait_def.name seen_traits, seen_enums, seen_aliases)
            | AST.EnumDef enum_def ->
                if StringSet.mem enum_def.name seen_enums then
                  Error (error (ConstructorError (Printf.sprintf "Duplicate enum definition: %s" enum_def.name)))
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
                  let poly = generalize env_for_generalize stmt_type in
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
                go env_with_placeholders empty_substitution StringSet.empty StringSet.empty StringSet.empty program
              with
              | Error e -> Error e
              | Ok (env', final_subst, result_type) ->
                  apply_substitution_type_map final_subst type_map;
                  Ok (env', type_map, result_type)))
  with
  | Annotation.Open_row_rejected msg -> Error (error (ConstructorError msg))

module Test = struct
  (* Helper to parse and infer *)
  let infer_string code =
    match Syntax.Parser.parse code with
    | Error errs ->
        let msg = String.concat "; " errs in
        Printf.printf "Parse errors: %s\n" msg;
        Error (error (UnboundVariable ("parse error: " ^ msg)))
    | Ok program -> infer_program program

  (* Helper to check inferred type *)
  let infers_to code expected_type =
    match infer_string code with
    | Error e ->
        Printf.printf "Error: %s\n" (error_to_string e);
        false
    | Ok (_, _type_map, t) ->
        if t = expected_type then
          true
        else (
          Printf.printf "Expected %s but got %s\n" (to_string expected_type) (to_string t);
          false)

  let contains_substring (s : string) (sub : string) : bool =
    let len_s = String.length s in
    let len_sub = String.length sub in
    let rec go i =
      if i + len_sub > len_s then
        false
      else if String.sub s i len_sub = sub then
        true
      else
        go (i + 1)
    in
    go 0

  let rec identifier_occurrences_in_expr (name : string) (expr : AST.expression) : (int * int) list =
    match expr.expr with
    | AST.Identifier n when n = name -> [ (expr.id, expr.pos) ]
    | AST.Identifier _ | AST.Integer _ | AST.Float _ | AST.Boolean _ | AST.String _ -> []
    | AST.Array xs -> List.concat_map (identifier_occurrences_in_expr name) xs
    | AST.Index (a, b) ->
        identifier_occurrences_in_expr name a @ identifier_occurrences_in_expr name b
    | AST.Hash pairs ->
        List.concat_map
          (fun (k, v) -> identifier_occurrences_in_expr name k @ identifier_occurrences_in_expr name v)
          pairs
    | AST.Prefix (_, e) -> identifier_occurrences_in_expr name e
    | AST.Infix (l, _, r) ->
        identifier_occurrences_in_expr name l @ identifier_occurrences_in_expr name r
    | AST.TypeCheck (e, _) -> identifier_occurrences_in_expr name e
    | AST.If (cond, cons, alt) ->
        identifier_occurrences_in_expr name cond
        @ identifier_occurrences_in_stmt name cons
        @
        (match alt with
        | None -> []
        | Some s -> identifier_occurrences_in_stmt name s)
    | AST.Function fn_expr -> identifier_occurrences_in_stmt name fn_expr.body
    | AST.Call (f, args) ->
        identifier_occurrences_in_expr name f @ List.concat_map (identifier_occurrences_in_expr name) args
    | AST.EnumConstructor (_, _, args) -> List.concat_map (identifier_occurrences_in_expr name) args
    | AST.Match (scrutinee, arms) ->
        identifier_occurrences_in_expr name scrutinee
        @ List.concat_map (fun (arm : AST.match_arm) -> identifier_occurrences_in_expr name arm.body) arms
    | AST.RecordLit (fields, spread) ->
        List.concat_map
          (fun (f : AST.record_field) ->
            match f.field_value with
            | None -> []
            | Some v -> identifier_occurrences_in_expr name v)
          fields
        @
        (match spread with
        | None -> []
        | Some e -> identifier_occurrences_in_expr name e)
    | AST.FieldAccess (receiver, _) -> identifier_occurrences_in_expr name receiver
    | AST.MethodCall (receiver, _, args) ->
        identifier_occurrences_in_expr name receiver @ List.concat_map (identifier_occurrences_in_expr name) args

  and identifier_occurrences_in_stmt (name : string) (stmt : AST.statement) : (int * int) list =
    match stmt.stmt with
    | AST.ExpressionStmt e | AST.Return e -> identifier_occurrences_in_expr name e
    | AST.Block stmts -> List.concat_map (identifier_occurrences_in_stmt name) stmts
    | AST.Let let_binding -> identifier_occurrences_in_expr name let_binding.value
    | AST.ImplDef impl_def ->
        List.concat_map (fun (m : AST.method_impl) -> identifier_occurrences_in_expr name m.impl_method_body)
          impl_def.impl_methods
    | AST.InherentImplDef impl_def ->
        List.concat_map (fun (m : AST.method_impl) -> identifier_occurrences_in_expr name m.impl_method_body)
          impl_def.inherent_methods
    | AST.EnumDef _ | AST.TraitDef _ | AST.DeriveDef _ | AST.TypeAlias _ -> []

  let identifier_occurrences_in_program (name : string) (program : AST.program) : (int * int) list =
    List.concat_map (identifier_occurrences_in_stmt name) program

  let find_symbol_id_by_name_kind (state : inference_state) (name : string) (kind : symbol_kind) : symbol_id option =
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

  let%test "infer float arithmetic" = infers_to "1.0 + 2.0" TFloat && infers_to "3.14 * 2.0" TFloat

  let%test "infer comparison" =
    infers_to "1 < 2" TBool && infers_to "1 > 2" TBool && infers_to "1 == 2" TBool && infers_to "1 != 2" TBool

  let%test "infer prefix operators" = infers_to "!true" TBool && infers_to "-5" TInt && infers_to "-3.14" TFloat
  let%test "infer if expression" = infers_to "if (true) { 1 } else { 2 }" TInt

  let%test "infer simple function" =
    (* fn(x) { x + 1 } should be Int -> Int *)
    infers_to "fn(x) { x + 1 }" (tfun TInt TInt)

  let%test "infer identity function" =
    (* fn(x) { x } should be t0 -> t0 (polymorphic) *)
    match infer_string "fn(x) { x }" with
    | Error _ -> false
    | Ok (_, _type_map, TFun (TVar a, TVar b, _)) -> a = b (* same type variable *)
    | Ok _ -> false

  let%test "infer two-arg function" =
    (* fn(x, y) { x + y } should have both args same type and return same type *)
    (* Without type classes, we can't constrain to just numeric types *)
    match infer_string "fn(x, y) { x + y }" with
    | Error _ -> false
    | Ok (_, _type_map, TFun (TVar a, TFun (TVar b, TVar c, _), _)) ->
        (* All three type vars should be the same *)
        a = b && b = c
    | Ok _ -> false

  let%test "infer two-arg function with literal" =
    (* fn(x, y) { x + y + 1 } should be (Int, Int) -> Int because of the literal *)
    infers_to "fn(x, y) { x + y + 1 }" (tfun TInt (tfun TInt TInt))

  let%test "infer function call" =
    (* fn(x) { x + 1 }(5) should be Int *)
    infers_to "fn(x) { x + 1 }(5)" TInt

  let%test "infer let binding" =
    (* let x = 5; x should be Int *)
    infers_to "let x = 5; x" TInt

  let%test "infer underscore let binding acts as discard" =
    (* let _ = 5; 1 should still infer as Int and not bind '_' *)
    infers_to "let _ = 5; 1" TInt

  let%test "infer underscore is not available as value after let discard" =
    match infer_string "let _ = 5; _" with
    | Error { kind = UnboundVariable "_"; _ } -> true
    | _ -> false

  let%test "infer let with function" =
    (* let f = fn(x) { x + 1 }; f(5) should be Int *)
    infers_to "let f = fn(x) { x + 1 }; f(5)" TInt

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

  let%test "infer type alias for record annotation" =
    infers_to "type point = { x: int, y: int }; let p: point = { x: 1, y: 2 }; p.x" TInt

  let%test "infer generic type alias annotation" =
    infers_to "type box[a] = { value: a }; let p: box[string] = { value: \"ok\" }; p.value" TString

  let%test "duplicate trait definition in one program is rejected" =
    match infer_string "trait ping[a] { fn ping(x: a) -> int }\ntrait ping[a] { fn pong(x: a) -> int }\n1" with
    | Error { kind = ConstructorError msg; _ } ->
        let needle = "Duplicate trait definition: ping" in
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
    match infer_string "enum dup { a }\nenum dup { b }\n1" with
    | Error { kind = ConstructorError msg; _ } ->
        let needle = "Duplicate enum definition: dup" in
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
    match infer_string "type point = { x: int }\ntype point = { y: int }\n1" with
    | Error { kind = ConstructorError msg; _ } ->
        let needle = "Duplicate type alias definition: point" in
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
    infers_to "let y = add1(41); let add1 = fn(x: int) -> int { x + 1 }; y" TInt

  let%test "top-level forward reference to later non-function value is rejected" =
    match infer_string "let a = b; let b = 1; a" with
    | Error { kind = UnboundVariable "b"; _ } -> true
    | _ -> false

  let%test "top-level mutual recursion with forward references infers" =
    infers_to
      "let even = fn(n: int) -> bool { if (n == 0) { true } else { odd(n - 1) } }; let odd = fn(n: int) -> bool { if (n == 0) { false } else { even(n - 1) } }; even(4)"
      TBool

  let%test "symbol resolution maps forward top-level function references to declaration symbols" =
    match Syntax.Parser.parse "let y = add1(41); let add1 = fn(x: int) -> int { x + 1 }; y" with
    | Error _ -> false
    | Ok program ->
        let state = create_inference_state () in
        (match infer_program ~state program with
        | Error _ -> false
        | Ok _ ->
            let add1_refs = identifier_occurrences_in_program "add1" program in
            let add1_symbol = find_symbol_id_by_name_kind state "add1" TopLevelLet in
            match (add1_refs, add1_symbol) with
            | [ (add1_ref_id, _) ], Some add1_sid ->
                lookup_identifier_symbol_in_state state add1_ref_id = Some add1_sid
            | _ -> false)

  let%test "symbol resolution leaves forward top-level value refs unresolved" =
    match Syntax.Parser.parse "let a = b; let b = 1; a" with
    | Error _ -> false
    | Ok program ->
        let state = create_inference_state () in
        (match infer_program ~state program with
        | Error { kind = UnboundVariable "b"; _ } ->
            let b_refs = identifier_occurrences_in_program "b" program in
            let b_symbol = find_symbol_id_by_name_kind state "b" TopLevelLet in
            (match (b_refs, b_symbol) with
            | [ (b_ref_id, _) ], Some _ -> lookup_identifier_symbol_in_state state b_ref_id = None
            | _ -> false)
        | _ -> false)

  let%test "symbol resolution maps enum constructor receiver after enum definition" =
    match Syntax.Parser.parse "enum direction { north }\nlet x = direction.north\nx" with
    | Error _ -> false
    | Ok program ->
        let state = create_inference_state () in
        (match infer_program ~state program with
        | Error _ -> false
        | Ok _ ->
            let direction_refs = identifier_occurrences_in_program "direction" program in
            let enum_symbol = find_symbol_id_by_name_kind state "direction" EnumSym in
            match (direction_refs, enum_symbol) with
            | [ (direction_ref_id, _) ], Some enum_sid ->
                lookup_identifier_symbol_in_state state direction_ref_id = Some enum_sid
            | _ -> false)

  let%test "symbol resolution leaves forward enum receiver refs unresolved" =
    match Syntax.Parser.parse "let x = direction.north\nenum direction { north }\nx" with
    | Error _ -> false
    | Ok program ->
        let state = create_inference_state () in
        let _ = infer_program ~state program in
        let direction_refs = identifier_occurrences_in_program "direction" program in
        match direction_refs with
        | [ (direction_ref_id, _) ] -> lookup_identifier_symbol_in_state state direction_ref_id = None
        | _ -> false

  let%test "duplicate top-level let definition is rejected" =
    match infer_string "let x = 1; let x = 2; x" with
    | Error { kind = ConstructorError msg; _ } -> contains_substring msg "Duplicate top-level let definition: x"
    | _ -> false

  let%test "symbol resolution maps top-level identifiers to top-level symbols" =
    match Syntax.Parser.parse "let x = 1; let y = x; y" with
    | Error _ -> false
    | Ok program ->
        let state = create_inference_state () in
        (match infer_program ~state program with
        | Error _ -> false
        | Ok _ ->
            let x_refs = identifier_occurrences_in_program "x" program in
            let y_refs = identifier_occurrences_in_program "y" program in
            let x_symbol = find_symbol_id_by_name_kind state "x" TopLevelLet in
            let y_symbol = find_symbol_id_by_name_kind state "y" TopLevelLet in
            match (x_refs, y_refs, x_symbol, y_symbol) with
            | [ (x_ref_id, _) ], [ (y_ref_id, _) ], Some x_sid, Some y_sid ->
                lookup_identifier_symbol_in_state state x_ref_id = Some x_sid
                && lookup_identifier_symbol_in_state state y_ref_id = Some y_sid
                &&
                (match lookup_symbol_in_state state x_sid with
                | Some (sym : symbol) -> sym.kind = TopLevelLet && sym.name = "x"
                | None -> false)
            | _ -> false)

  let%test "symbol resolution prefers inner parameter over top-level binding when shadowed" =
    match Syntax.Parser.parse "let x = 1; let f = fn(x: int) -> int { x }; f(2); x" with
    | Error _ -> false
    | Ok program ->
        let state = create_inference_state () in
        (match infer_program ~state program with
        | Error _ -> false
        | Ok _ ->
            let x_refs =
              identifier_occurrences_in_program "x" program |> List.sort (fun (_, p1) (_, p2) -> Int.compare p1 p2)
            in
            let top_level_symbol = find_symbol_id_by_name_kind state "x" TopLevelLet in
            let param_symbol = find_symbol_id_by_name_kind state "x" Param in
            match (x_refs, top_level_symbol, param_symbol) with
            | [ (inner_x_id, _); (outer_x_id, _) ], Some top_sid, Some param_sid ->
                lookup_identifier_symbol_in_state state inner_x_id = Some param_sid
                && lookup_identifier_symbol_in_state state outer_x_id = Some top_sid
            | _ -> false)

  let%test "builtin identifiers are tracked as builtin symbols when resolved from prelude env" =
    match Syntax.Parser.parse "puts(1)" with
    | Error _ -> false
    | Ok program ->
        let state = create_inference_state () in
        let env = TypeEnv.add "puts" (mono_to_poly (TFun (TInt, TInt, true))) empty_env in
        (match infer_program ~state ~env program with
        | Error _ -> false
        | Ok _ ->
            let puts_refs = identifier_occurrences_in_program "puts" program in
            let puts_builtin = find_symbol_id_by_name_kind state "puts" BuiltinValue in
            match (puts_refs, puts_builtin) with
            | [ (puts_ref_id, _) ], Some sid ->
                lookup_identifier_symbol_in_state state puts_ref_id = Some sid
                &&
                (match lookup_symbol_in_state state sid with
                | Some (sym : symbol) -> sym.kind = BuiltinValue
                | None -> false)
            | _ -> false)

  let%test "top-level let can shadow prelude builtin in symbol resolution" =
    match Syntax.Parser.parse "let puts = fn(x: int) -> int { x }; puts(1)" with
    | Error _ -> false
    | Ok program ->
        let state = create_inference_state () in
        let env = TypeEnv.add "puts" (mono_to_poly (TFun (TInt, TInt, true))) empty_env in
        (match infer_program ~state ~env program with
        | Error _ -> false
        | Ok _ ->
            let puts_refs = identifier_occurrences_in_program "puts" program in
            let puts_builtin = find_symbol_id_by_name_kind state "puts" BuiltinValue in
            let puts_top_level = find_symbol_id_by_name_kind state "puts" TopLevelLet in
            match (puts_refs, puts_builtin, puts_top_level) with
            | [ (puts_ref_id, _) ], Some builtin_sid, Some top_sid ->
                lookup_identifier_symbol_in_state state puts_ref_id = Some top_sid && top_sid <> builtin_sid
            | _ -> false)

  let%test "symbol ids are stable across independent inference runs for identical source spans" =
    match Syntax.Parser.parse ~file_id:"main.mr" "let x = 1; let y = x; y" with
    | Error _ -> false
    | Ok program ->
        let state1 = create_inference_state () in
        let state2 = create_inference_state () in
        (match (infer_program ~state:state1 program, infer_program ~state:state2 program) with
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

  let%test "field-only trait object rejects access to fields outside trait projection" =
    match
      infer_string
        "trait named { name: string }\nlet x: named = { name: \"alice\", age: 42 }\nx.age"
    with
    | Error { kind = ConstructorError msg; _ } ->
        let needle = "Record field 'age' not found in type" in
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

  let%test "function return trait-object annotation projects away extra fields" =
    match
      infer_string
        "trait named { name: string }\nlet mk = fn(n: int) -> named { { name: \"alice\", age: n } }\nlet x = mk(42)\nx.age"
    with
    | Error { kind = ConstructorError msg; _ } ->
        let needle = "Record field 'age' not found in type" in
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

  let%test "function return trait-object annotation keeps projected fields accessible" =
    infers_to
      "trait named { name: string }\nlet mk = fn(n: int) -> named { { name: \"alice\", age: n } }\nlet x = mk(42)\nx.name"
      TString

  let%test "constrained field access preserves non-field trait obligations at call sites" =
    match
      infer_string
        "trait named { name: string }\n\
         trait shown[a] { fn show(x: a) -> string }\n\
         let get_name = fn[t: named + shown](x: t) { x.name }\n\
         let p = { name: \"alice\" }\n\
         get_name(p)"
    with
    | Error { kind = ConstructorError msg; _ } ->
        let has needle =
          let len_msg = String.length msg in
          let len_needle = String.length needle in
          let rec go i =
            if i + len_needle > len_msg then
              false
            else if String.sub msg i len_needle = needle then
              true
            else
              go (i + 1)
          in
          go 0
        in
        has "Trait obligation failed" && has "does not implement trait shown"
    | _ -> false

  let%test "constrained field access rejects fields not guaranteed by constraints" =
    match infer_string "trait named { name: string }\nlet get_age = fn[t: named](x: t) { x.age }\n1" with
    | Error { kind = ConstructorError msg; _ } ->
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
      "type person = { name: string, age: int }\n\
       trait named { name: string }\n\
       trait shown[a] { fn show(x: a) -> string }\n\
       impl shown for person {\n\
      \  fn show(x: person) -> string {\n\
      \    x.name\n\
      \  }\n\
       }\n\
       let get_name = fn[t: named + shown](x: t) { x.name }\n\
       let p: person = { name: \"alice\", age: 42 }\n\
       get_name(p)"
      TString

  let%test "explicit row-polymorphic annotation is rejected in v1" =
    reset_fresh_counter ();
    match infer_string "let get_x = fn(r: { x: int, ...row }) -> int { r.x }" with
    | Error _ -> true
    | Ok _ -> false

  let%test "field access on unannotated record param works" =
    infers_to "let p = { x: 5, y: 10, z: 20 }; let get_x = fn(r) { r.x }; get_x(p)" TInt

  let%test "row-polymorphic field accessor can be reused across distinct record tails" =
    infers_to
      "let get_x = fn(r) { r.x }; let a = get_x({ x: 1, y: true }); let b = get_x({ x: 2, z: \"s\" }); a + b"
      TInt

  let%test "multiple field access with closed record annotation works" =
    infers_to "let p = { x: 5, y: 10 }; let sum_xy = fn(r: { x: int, y: int }) { r.x + r.y }; sum_xy(p)" TInt

  let%test "infer record match pattern with punning" =
    infers_to "let p = { x: 10, y: 20 }; match p { { x:, y: }: x + y _: 0 }" TInt

  let%test "infer record match pattern with rest binding" =
    infers_to "let p = { x: 10, y: 20, z: 30 }; match p { { x:, ...rest }: x + rest.y _: 0 }" TInt

  let%test "infer polymorphic let" =
    (* let id = fn(x) { x }; id(5) should work and be Int *)
    infers_to "let id = fn(x) { x }; id(5)" TInt

  let%test "infer polymorphic let used twice" =
    (* let id = fn(x) { x }; id(5); id(true) should work *)
    (* The result type is the type of the last expression: Bool *)
    infers_to "let id = fn(x) { x }; id(5); id(true)" TBool

  let%test "infer higher order function" =
    (* let apply = fn(f, x) { f(x) }; apply(fn(n) { n + 1 }, 5) should be Int *)
    infers_to "let apply = fn(f, x) { f(x) }; apply(fn(n) { n + 1 }, 5)" TInt

  let%test "error on unbound variable" =
    match infer_string "x" with
    | Error { kind = UnboundVariable "x"; _ } -> true
    | _ -> false

  let%test "union type from if with different branch types" =
    (* Phase 4.1: Different branch types create unions instead of errors *)
    match infer_string "if (true) { 1 } else { \"hello\" }" with
    | Ok (_, _, TUnion _) -> true
    | _ -> false

  let%test "error on non-bool condition" =
    match infer_string "if (5) { 1 }" with
    | Error { kind = IfConditionNotBool _; _ } -> true
    | _ -> false

  let%test "error on array element mismatch" =
    match infer_string "[1, \"hello\"]" with
    | Error { kind = ArrayElementMismatch _; _ } -> true
    | _ -> false

  let%test "infer simple recursive function" =
    (* Factorial: fn(n) { if (n == 0) { 1 } else { n * fact(n - 1) } } *)
    let code = "let fact = fn(n) { if (n == 0) { 1 } else { n * fact(n - 1) } }; fact(5)" in
    infers_to code TInt

  let%test "infer recursive function type" =
    (* The factorial function should have type Int -> Int *)
    let code = "let fact = fn(n) { if (n == 0) { 1 } else { n * fact(n - 1) } }; fact" in
    infers_to code (tfun TInt TInt)

  let%test "infer fibonacci" =
    let code = "let fib = fn(n) { if (n < 2) { n } else { fib(n - 1) + fib(n - 2) } }; fib(10)" in
    infers_to code TInt

  let%test "infer mutually referencing let" =
    (* A let that references itself but isn't a function - should still work *)
    (* let x = x would be infinite, but let x = 5 should be fine *)
    infers_to "let x = 5; x" TInt

  let%test "infer countdown" =
    (* Recursive function that returns unit/last value *)
    let code = "let countdown = fn(n) { if (n == 0) { 0 } else { countdown(n - 1) } }; countdown(10)" in
    infers_to code TInt

  let%test "infer recursive with array" =
    (* Recursive function that works with arrays *)
    let code =
      "let sum = fn(arr, i) { if (i == 0) { arr[0] } else { arr[i] + sum(arr, i - 1) } }; sum([1,2,3], 2)"
    in
    infers_to code TInt

  (* Enum constructor tests *)
  let%test "infer simple enum constructor" =
    let code = "enum direction { north south east west }
direction.north" in
    infers_to code (TEnum ("direction", []))

  let%test "infer option.some with int" =
    let code = "enum option[a] { some(a) none }
option.some(42)" in
    infers_to code (TEnum ("option", [ TInt ]))

  let%test "infer option.none" =
    let code = "enum option[a] { some(a) none }
option.none" in
    match infer_string code with
    | Error _ -> false
    | Ok (_, _type_map, t) -> (
        match t with
        | TEnum ("option", [ TVar _ ]) -> true
        | _ ->
            Printf.printf "Expected option[_] but got %s\n" (to_string t);
            false)

  let%test "infer result.success with int" =
    let code = "enum result[a, e] { success(a) failure(e) }
result.success(42)" in
    match infer_string code with
    | Error _ -> false
    | Ok (_, _type_map, t) -> (
        match t with
        | TEnum ("result", [ TInt; TVar _ ]) -> true
        | _ ->
            Printf.printf "Expected result[Int, _] but got %s\n" (to_string t);
            false)

  let%test "infer result.failure with string" =
    let code = "enum result[a, e] { success(a) failure(e) }
result.failure(\"error\")" in
    match infer_string code with
    | Error _ -> false
    | Ok (_, _type_map, t) -> (
        match t with
        | TEnum ("result", [ TVar _; TString ]) -> true
        | _ ->
            Printf.printf "Expected result[_, String] but got %s\n" (to_string t);
            false)

  let%test "infer constructor with wrong arg count" =
    let code = "enum option[a] { some(a) none }
option.some(1, 2)" in
    match infer_string code with
    | Error _ -> true
    | Ok _ -> false

  (* Match expression tests *)
  let%test "infer simple match with option" =
    let code =
      "enum option[a] { some(a) none }
let x = option.some(42)
match x {
  option.some(v): v + 1
  option.none: 0
}"
    in
    infers_to code TInt

  let%test "infer match with wildcard" =
    let code = "match 5 {
  0: \"zero\"
  _: \"other\"
}" in
    infers_to code TString

  let%test "infer match with variable pattern" =
    let code = "match 42 {
  n: n + 1
}" in
    infers_to code TInt

  let%test "infer match extracts constructor value" =
    let code =
      "enum result[a, e] { success(a) failure(e) }
let r = result.success(100)
match r {
  result.success(val): val * 2
  result.failure(err): 0
}"
    in
    infers_to code TInt

  let%test "infer match with literal patterns" =
    let code = "let x = 5
match x {
  0: \"zero\"
  1: \"one\"
  _: \"many\"
}" in
    infers_to code TString

  let%test "match expression union type from different arm types" =
    (* Phase 4.2: Different arm types create unions instead of errors *)
    let code = "match 5 {
  0: 42
  _: \"string\"
}" in
    match infer_string code with
    | Ok (_, _, TUnion _) -> true
    | _ -> false

  (* Exhaustiveness checking tests *)
  let%test "non-exhaustive match on option is error" =
    let code = "enum option[a] { some(a) none }
let x = option.some(42)
match x {
  option.some(v): v
}" in
    match infer_string code with
    | Error e ->
        let msg = error_to_string e in
        String.length msg > 0 && String.sub msg 0 (min 17 (String.length msg)) = "Non-exhaustive ma"
    | Ok _ -> false

  let%test "exhaustive match on option passes" =
    let code =
      "enum option[a] { some(a) none }
let x = option.some(42)
match x {
  option.some(v): v
  option.none: 0
}"
    in
    infers_to code TInt

  let%test "match with wildcard is exhaustive" =
    let code =
      "enum result[a, e] { success(a) failure(e) }
let r = result.success(100)
match r {
  result.success(v): v
  _: 0
}"
    in
    infers_to code TInt

  let%test "match with variable pattern is exhaustive" =
    let code = "enum option[a] { some(a) none }
match option.some(5) {
  x: 42
}" in
    infers_to code TInt

  let%test "non-exhaustive match on bool is error" =
    let code = "match true {
  true: 1
}" in
    match infer_string code with
    | Error _ -> true
    | Ok _ -> false

  let%test "exhaustive match on bool passes" =
    let code = "match true {
  true: 1
  false: 0
}" in
    infers_to code TInt

  let%test "non-exhaustive match on result is error" =
    let code =
      "enum result[a, e] { success(a) failure(e) }
match result.success(42) {
  result.success(v): v
}"
    in
    match infer_string code with
    | Error _ -> true
    | Ok _ -> false

  let%test "constraint store does not leak across independent runs" =
    let constrained_code =
      "trait show[a] {
  fn show(x: a) -> string
}
let f = fn[a: show](x: a) -> string { x.show() }
f"
    in
    let unconstrained_code = "let id = fn(x) { x }; id([1, 2, 3])" in
    match infer_string constrained_code with
    | Error _ -> false
    | Ok _ -> (
        match infer_string unconstrained_code with
        | Error _ -> false
        | Ok (_, _, inferred_type) -> inferred_type = TArray TInt)

  let%test "obligations_from_substitution creates one obligation per trait" =
    clear_constraint_store ();
    add_type_var_constraints "t0" [ "show"; "eq" ];
    let obligations = obligations_from_substitution [ ("t0", TInt) ] in
    List.length obligations = 2
    && List.exists (fun (o : obligation) -> o.trait_name = "show" && o.typ = TInt) obligations
    && List.exists (fun (o : obligation) -> o.trait_name = "eq" && o.typ = TInt) obligations

  let%test "verify_constraints_in_substitution includes obligation context in errors" =
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
    in
    clear_constraint_store ();
    add_type_var_constraints "t0" [ "show" ];
    match verify_constraints_in_substitution [ ("t0", tfun TInt TInt) ] with
    | Ok () -> false
    | Error msg -> contains_substring msg "Trait obligation failed" && contains_substring msg "type variable 't0'"

  let%test "add_type_var_constraints lowers field requirements from supertraits" =
    Trait_registry.clear ();
    Trait_registry.register_trait
      {
        trait_name = "named";
        trait_type_param = None;
        trait_supertraits = [];
        trait_methods = [];
      };
    Trait_registry.set_trait_fields "named" [ { name = "name"; typ = TString } ];
    Trait_registry.register_trait
      {
        trait_name = "labeled";
        trait_type_param = None;
        trait_supertraits = [ "named" ];
        trait_methods = [];
      };
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
      {
        trait_name = "named";
        trait_type_param = None;
        trait_supertraits = [];
        trait_methods = [];
      };
    Trait_registry.register_trait
      {
        trait_name = "aged";
        trait_type_param = None;
        trait_supertraits = [];
        trait_methods = [];
      };
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
      {
        trait_name = "named";
        trait_type_param = None;
        trait_supertraits = [];
        trait_methods = [];
      };
    Trait_registry.set_trait_fields "named" [ { name = "name"; typ = TString } ];
    clear_constraint_store ();
    add_type_var_constraints "t0" [ "named" ];
    let had_fields = lookup_type_var_constrained_fields "t0" <> [] in
    clear_constraint_store ();
    had_fields && lookup_type_var_constrained_fields "t0" = []

  let%test "constrained type-variable method lookup reports ambiguity" =
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
    in
    Trait_registry.clear ();
    Trait_registry.register_trait
      {
        trait_name = "t1";
        trait_type_param = Some "a";
        trait_supertraits = [];
        trait_methods =
          [ { method_name = "render"; method_params = [ ("x", TVar "a") ]; method_return_type = TString } ];
      };
    Trait_registry.register_trait
      {
        trait_name = "t2";
        trait_type_param = Some "a";
        trait_supertraits = [];
        trait_methods =
          [ { method_name = "render"; method_params = [ ("x", TVar "a") ]; method_return_type = TString } ];
      };
    match infer_string "let f = fn[a: t1 + t2](x: a) -> string { x.render() }; f" with
    | Ok _ -> false
    | Error e ->
        let msg = error_to_string e in
        contains_substring msg "Ambiguous method 'render'"

  let%test "inherent method call resolves for concrete receiver" =
    let code =
      "type point = { x: int, y: int }\n\
       impl point { fn sum(p: point) -> int { p.x + p.y } }\n\
       let p: point = { x: 1, y: 2 }\n\
       p.sum()"
    in
    infers_to code TInt

  let%test "inherent method call resolves for type-application receiver" =
    let code =
      "impl list[int] { fn size(xs: list[int]) -> int { 1 } }\n\
       [1, 2, 3].size()"
    in
    infers_to code TInt

  let%test "inherent method receiver must match impl target type" =
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
    in
    let code = "type point = { x: int }\nimpl point { fn bad(x: int) -> int { x } }\n1" in
    match infer_string code with
    | Ok _ -> false
    | Error e ->
        let msg = error_to_string e in
        contains_substring msg "receiver type" && contains_substring msg "does not match impl target type"

  let%test "duplicate inherent method for same type is rejected" =
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
    in
    let code =
      "impl int { fn ping(x: int) -> int { x } }\n\
       impl int { fn ping(x: int) -> int { x } }\n\
       1"
    in
    match infer_string code with
    | Ok _ -> false
    | Error e ->
        let msg = error_to_string e in
        contains_substring msg "Duplicate inherent method 'ping'"

  let%test "inherent method registration rejects trait collision on same type and method name" =
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
    in
    Trait_registry.clear ();
    let code =
      "trait show[a] { fn show(x: a) -> string }\n\
       impl show for int { fn show(x: int) -> string { \"trait\" } }\n\
       impl int { fn show(x: int) -> string { \"inherent\" } }\n\
       1"
    in
    match infer_string code with
    | Ok _ -> false
    | Error e ->
        let msg = error_to_string e in
        contains_substring msg "collides with trait method"

  let%test "inherent methods do not satisfy trait constraints" =
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
    in
    Trait_registry.clear ();
    let code =
      "trait show[a] { fn show(x: a) -> string }\n\
       type point = { x: int }\n\
       impl point { fn show(p: point) -> string { \"p\" } }\n\
       let f = fn[t: show](x: t) -> string { x.show() }\n\
       let p: point = { x: 1 }\n\
       f(p)"
    in
    match infer_string code with
    | Ok _ -> false
    | Error e ->
        let msg = error_to_string e in
        contains_substring msg "does not implement trait show"

  let%test "infer_program isolates itself from stale global constraint state" =
    (* Simulate stale process-global state from an earlier session. *)
    clear_constraint_store ();
    reset_fresh_counter ();
    add_type_var_constraints "t0" [ "show" ];
    let code = "(fn(x) { x })(fn(y) { y })" in
    match Syntax.Parser.parse code with
    | Error _ -> false
    | Ok program -> (
        match infer_program program with
        | Error _ -> false
        | Ok (_, _, _) -> true)

  let%test "reused env preserves constrained generic obligations across infer_program runs" =
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
    in
    Trait_registry.clear ();
    Trait_registry.register_trait
      {
        Trait_registry.trait_name = "show";
        trait_type_param = Some "a";
        trait_supertraits = [];
        trait_methods =
          [ { method_name = "show"; method_params = [ ("x", TVar "a") ]; method_return_type = TString } ];
      };
    Trait_registry.register_impl ~builtin:true
      {
        impl_trait_name = "show";
        impl_type_params = [];
        impl_for_type = TInt;
        impl_methods = [ { method_name = "show"; method_params = [ ("x", TInt) ]; method_return_type = TString } ];
      };
    let shared_state = create_inference_state () in
    match Syntax.Parser.parse "let check = fn[a: show](x: a) -> string { x.show() }" with
    | Error _ -> false
    | Ok first_program -> (
        match infer_program ~state:shared_state first_program with
        | Error _ -> false
        | Ok (env_with_check, _, _) -> (
            match Syntax.Parser.parse "check(fn(y) { y })" with
            | Error _ -> false
            | Ok second_program -> (
                match infer_program ~state:shared_state ~env:env_with_check second_program with
                | Ok _ -> false
                | Error e ->
                    let msg = error_to_string e in
                    contains_substring msg "does not implement trait show")))

  let%test "infer errors preserve parser file_id metadata" =
    match Syntax.Parser.parse ~file_id:"main.mr" "1 + true" with
    | Error _ -> false
    | Ok program -> (
        match infer_program program with
        | Error { file_id = Some "main.mr"; _ } -> true
        | _ -> false)

  let%test "infer effectful function type uses fat arrow" =
    (* fn(x: int) => int { x + 1 } should infer as Int => Int *)
    match infer_string "fn(x: int) => int { x + 1 }" with
    | Error _ -> false
    | Ok (_, _, TFun (TInt, TInt, true)) -> true
    | Ok _ -> false

  let%test "infer pure function type uses thin arrow" =
    (* fn(x: int) -> int { x + 1 } should infer as Int -> Int (not effectful) *)
    match infer_string "fn(x: int) -> int { x + 1 }" with
    | Error _ -> false
    | Ok (_, _, TFun (TInt, TInt, false)) -> true
    | Ok _ -> false

  let%test "infer unannotated function is pure by default" =
    match infer_string "fn(x) { x + 1 }" with
    | Error _ -> false
    | Ok (_, _, TFun (_, _, false)) -> true
    | Ok _ -> false

  (* Purity enforcement tests *)
  (* Note: infer_string uses empty_env (no builtins), so we define effectful
     functions inline using => to test purity enforcement *)

  let%test "pure function calling effectful operation is error" =
    (* Define an effectful function, then a pure function that calls it *)
    let code = "let eff = fn(x: int) => int { x }; fn(y: int) -> int { eff(y) }" in
    match infer_string code with
    | Error { kind = PurityViolation _; _ } -> true
    | _ -> false

  let%test "pure function with pure body is ok" =
    match infer_string "fn(x: int) -> int { x + 1 }" with
    | Ok (_, _, TFun (TInt, TInt, false)) -> true
    | _ -> false

  let%test "effectful annotation with pure body is ok (no enforcement yet)" =
    (* Case 2 is not enforced — => with pure body is allowed *)
    match infer_string "fn(x: int) => int { x + 1 }" with
    | Ok (_, _, TFun (TInt, TInt, true)) -> true
    | _ -> false

  let%test "unannotated function calling effectful infers as effectful" =
    let code = "let eff = fn(x: int) => int { x }; fn(y: int) { eff(y) }" in
    match infer_string code with
    | Ok (_, _, TFun (TInt, TInt, true)) -> true
    | _ -> false

  let%test "unannotated function with pure body infers as pure" =
    match infer_string "fn(x) { x + 1 }" with
    | Ok (_, _, TFun (_, _, false)) -> true
    | _ -> false

  let%test "pure function calling effectful in let binding is error" =
    let code = "let eff = fn(x: int) => int { x }; fn(y: int) -> int { let z = eff(y); z }" in
    match infer_string code with
    | Error { kind = PurityViolation _; _ } -> true
    | _ -> false

  let%test "pure function calling effectful in if branch is error" =
    let code = "let eff = fn(x: int) => int { x }; fn(y: int) -> int { if (true) { eff(y) } else { 0 } }" in
    match infer_string code with
    | Error { kind = PurityViolation _; _ } -> true
    | _ -> false

  let%test "pure function calling maybe-effectful function from union is error" =
    let code =
      "fn(flag: bool) -> int { let f = if (flag) { fn(x: int) -> int { x + 1 } } else { fn(x: int) => int { x } }; f(1) }"
    in
    match infer_string code with
    | Error { kind = PurityViolation _; _ } -> true
    | _ -> false

  let%test "unannotated function calling maybe-effectful union infers effectful" =
    let code =
      "fn(flag: bool) { let f = if (flag) { fn(x: int) -> int { x + 1 } } else { fn(x: int) => int { x } }; f(1) }"
    in
    match infer_string code with
    | Ok (_, _, TFun (TBool, TInt, true)) -> true
    | _ -> false

  let%test "unannotated higher-order caller accepts pure and effectful callbacks" =
    let code =
      "let hof = fn(f) { f(1) }; let pure = fn(x: int) -> int { x + 1 }; let eff = fn(x: int) => int { x + 2 }; hof(pure) + hof(eff)"
    in
    infers_to code TInt

  let%test "pure annotated higher-order caller with unknown callback is rejected conservatively" =
    let code = "let hof = fn(f) -> int { f(1) }; let eff = fn(x: int) => int { x }; hof(eff)" in
    match infer_string code with
    | Error { kind = PurityViolation _; _ } -> true
    | _ -> false

  let%test "union of pure/effectful callables normalizes to callable and can be called" =
    let code =
      "let choose = fn(flag: bool) { if (flag) { fn(x: int) -> int { x + 1 } } else { fn(x: int) => int { x + 2 } } }; let f = choose(true); f(1)"
    in
    infers_to code TInt

  let%test "higher-order callback via local alias still infers call result" =
    let code =
      "let hof = fn(f) { let g = f; g(1) }; let eff = fn(x: int) => int { x + 2 }; hof(eff)"
    in
    infers_to code TInt

  let%test "union of callable and non-callable cannot be called" =
    let code = "let f = if (true) { fn(x: int) -> int { x + 1 } } else { 0 }; f(1)" in
    match infer_string code with
    | Error { kind = UnificationError _; _ } -> true
    | _ -> false

  let%test "union of callables with mismatched arity cannot be called" =
    let code =
      "let f = if (true) { fn(x: int) -> int { x } } else { fn(x: int, y: int) -> int { x + y } }; f(1)"
    in
    match infer_string code with
    | Error { kind = UnificationError _; _ } -> true
    | _ -> false

  let%test "pure function defining effectful function is ok" =
    (* Defining (not calling) an effectful function inside a pure function is fine *)
    let code = "fn(x: int) { fn(y: int) => int { y } }" in
    match infer_string code with
    | Ok _ -> true
    | Error _ -> false

  let%test "unannotated recursive function with effectful call infers as effectful" =
    let code =
      "let eff = fn(x: int) => int { x }; let loop = fn(n: int) { if (n == 0) { 0 } else { eff(n); loop(n - 1) } }; loop"
    in
    match infer_string code with
    | Ok (_, _, TFun (TInt, TInt, true)) -> true
    | _ -> false

  let%test "pure annotated recursive function with effectful call is rejected" =
    let code =
      "let eff = fn(x: int) => int { x }; let loop = fn(n: int) -> int { if (n == 0) { 0 } else { eff(n); loop(n - 1) } }; loop(2)"
    in
    match infer_string code with
    | Error { kind = PurityViolation _; _ } -> true
    | _ -> false
end
