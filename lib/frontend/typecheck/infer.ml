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

(* ============================================================
   Constraint Context
   ============================================================
   
   Tracks trait constraints for type variables.
   Global store (like trait_registry) for simplicity.
*)

let constraint_store : (string, string list) Hashtbl.t = Hashtbl.create 64

let add_type_var_constraints (type_var_name : string) (traits : string list) : unit =
  Hashtbl.add constraint_store type_var_name traits

let lookup_type_var_constraints (type_var_name : string) : string list =
  match Hashtbl.find_opt constraint_store type_var_name with
  | Some traits -> traits
  | None -> []

let clear_constraint_store () : unit = Hashtbl.clear constraint_store

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
  (* For each substitution (var -> type), check if var has constraints *)
  let rec check_all = function
    | [] -> Ok ()
    | (var_name, resolved_type) :: rest -> (
        let constraints = lookup_type_var_constraints var_name in
        if constraints = [] then
          (* No constraints on this var *)
          check_all rest
        else
          (* Has constraints - check if resolved type satisfies them *)
          let concrete_type =
            (* Follow the substitution chain to get the fully resolved type *)
            apply_substitution subst resolved_type
          in
          match concrete_type with
          | TVar _ ->
              (* Still a type variable, constraints will be checked when it's resolved *)
              check_all rest
          | _ -> (
              (* Concrete type - check constraints *)
              match Trait_solver.check_constraints concrete_type constraints with
              | Ok () -> check_all rest
              | Error msg -> Error msg))
  in
  check_all subst

(* ============================================================
   Fresh Type Variables
   ============================================================
   
   We need to generate unique type variable names during inference.
   Using a simple counter: t0, t1, t2, ...
*)

let fresh_var_counter = ref 0

let fresh_type_var () : mono_type =
  let n = !fresh_var_counter in
  fresh_var_counter := n + 1;
  TVar ("t" ^ string_of_int n)

(* Reset counter (useful for testing to get predictable names) *)
let reset_fresh_counter () = fresh_var_counter := 0

(* ============================================================
   Instantiation
   ============================================================
   
   When we use a polymorphic variable, we "instantiate" it by
   replacing all quantified type variables with fresh ones.
   
   Example: ∀a. a -> a  becomes  t0 -> t0 (with fresh t0)
*)

let instantiate (Forall (quantified_vars, mono)) : mono_type =
  (* Create a substitution mapping each quantified var to a fresh var *)
  let subst = List.map (fun var -> (var, fresh_type_var ())) quantified_vars in
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
  let can_generalize = TypeVarSet.diff free_in_mono free_in_env in
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

(* An error with optional position info *)
type infer_error = {
  kind : error_kind;
  pos : int option; (* byte offset in source, if available *)
}

(* Create an error without position *)
let error kind = { kind; pos = None }

(* Create an error with position from an expression *)
let error_at kind (expr : AST.expression) = { kind; pos = Some expr.pos }

(* Create an error with position from a statement *)
let error_at_stmt kind (stmt : AST.statement) = { kind; pos = Some stmt.pos }

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

let error_to_string (err : infer_error) : string = error_kind_to_string err.kind

(* Result type for inference *)
type 'a infer_result = ('a, infer_error) result

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
        infer_function_with_annotations type_map env f.generics f.params f.return_type f.body
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
                    }
                else
                  (* Get the enum definition to know type parameters *)
                  match Enum_registry.lookup enum_name with
                  | None ->
                      Error
                        {
                          kind = ConstructorError (Printf.sprintf "Unknown enum: %s" enum_name);
                          pos = Some expr.pos;
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
        | _ ->
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
                          | Some row_var ->
                              (* Open row: the field may live in the tail; constrain it. *)
                              let field_type = fresh_type_var () in
                              let expected = TRecord ([ { name = variant_name; typ = field_type } ], Some row_var) in
                              (match unify receiver_type' expected with
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
                  | TVar _ ->
                      (* Row-polymorphic field access: receiver must have this field plus unknown tail *)
                      let field_type = fresh_type_var () in
                      let row_var = TRowVar ("r" ^ string_of_int !fresh_var_counter) in
                      fresh_var_counter := !fresh_var_counter + 1;
                      let expected = TRecord ([ { name = variant_name; typ = field_type } ], Some row_var) in
                      (match unify receiver_type' expected with
                      | Error e -> Error (error_at (UnificationError e) expr)
                      | Ok subst2 ->
                          let final_subst = compose_substitution subst1 subst2 in
                          Ok (final_subst, apply_substitution subst2 field_type))
                  | _ ->
                      Error
                        (error_at
                           (ConstructorError
                              (Printf.sprintf "Field access requires record type, got %s"
                                 (Types.to_string receiver_type')))
                           expr)
                in
                field_result)
    | AST.MethodCall (receiver, method_name, args) -> (
        (* Phase 4.3: Method calls and enum constructors *)
        (* Check if this is actually an enum constructor (receiver is enum type identifier) *)
        match receiver.expr with
        | AST.Identifier enum_name when Enum_registry.lookup enum_name <> None -> (
            (* This is actually an enum constructor like option.some(42) *)
            (* Redirect to enum constructor logic - same as EnumConstructor handling *)
            match Enum_registry.lookup_variant enum_name method_name with
            | None ->
                Error
                  (error_at
                     (ConstructorError (Printf.sprintf "Unknown enum constructor: %s.%s" enum_name method_name))
                     expr)
            | Some variant -> (
                (* Infer types of constructor arguments *)
                match infer_args type_map env empty_substitution args with
                | Error e -> Error e
                | Ok (subst, arg_types) -> (
                    if List.length args <> List.length variant.fields then
                      Error
                        (error_at
                           (ConstructorError
                              (Printf.sprintf "%s.%s expects %d arguments, got %d" enum_name method_name
                                 (List.length variant.fields) (List.length args)))
                           expr)
                    else
                      (* Get the enum definition to know type parameters *)
                      match Enum_registry.lookup enum_name with
                      | None ->
                          Error (error_at (ConstructorError (Printf.sprintf "Unknown enum: %s" enum_name)) expr)
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
                                | Ok new_subst -> unify_all (compose_substitution new_subst subst_acc) rest1 rest2
                                | Error e -> Error (error_at (UnificationError e) expr))
                            | _ -> Error (error_at (ConstructorError "Argument count mismatch") expr)
                          in
                          match unify_all subst arg_types' expected_types with
                          | Error e -> Error e
                          | Ok final_subst ->
                              (* Build the result enum type with substituted type arguments *)
                              let result_type_args = List.map (apply_substitution final_subst) fresh_vars in
                              let result_type = TEnum (enum_name, result_type_args) in
                              Ok (final_subst, result_type)))))
        | _ -> (
            (* Not an enum constructor - this is a real method call *)
            (* Phase 4.3: Trait method resolution *)
            match infer_expression type_map env receiver with
            | Error e -> Error e
            | Ok (subst1, receiver_type) -> (
                let env1 = apply_substitution_env subst1 env in
                let receiver_type' = apply_substitution subst1 receiver_type in

                (* Look up the method for this type *)
                (* Phase 4.3+: For type variables, check constraints *)
                let method_lookup_result =
                  match receiver_type' with
                  | TVar type_var_name -> (
                      (* Check if this type variable has constraints *)
                      let constraints = lookup_type_var_constraints type_var_name in
                      if constraints = [] then
                        (* No constraints, can't find method *)
                        None
                      else
                        (* Check if any constraint provides this method *)
                        match Trait_solver.method_available method_name constraints with
                        | None -> None
                        | Some trait_name -> (
                            (* Found the trait that provides this method *)
                            match Trait_registry.lookup_trait trait_name with
                            | None -> None
                            | Some trait_def -> (
                                (* Find the method signature in the trait *)
                                match
                                  List.find_opt
                                    (fun (m : Trait_registry.method_sig) -> m.method_name = method_name)
                                    trait_def.trait_methods
                                with
                                | None -> None
                                | Some method_sig -> Some (trait_name, method_sig))))
                  | _ ->
                      (* Concrete type, use normal lookup *)
                      Trait_registry.lookup_method receiver_type' method_name
                in

                match method_lookup_result with
                | None ->
                    Error
                      (error_at
                         (ConstructorError
                            (Printf.sprintf "No method '%s' found for type %s" method_name
                               (Types.to_string receiver_type')))
                         expr)
                | Some (trait_name, method_sig) -> (
                    (* Type check the method call *)
                    (* Method signature has receiver as first parameter *)
                    (* receiver.method(arg1, arg2) becomes method(receiver, arg1, arg2) *)

                    (* IMPORTANT: Instantiate the method signature by replacing the trait's
                       type parameter with the receiver type. This prevents corruption of
                       the caller's type variables when unifying.
                       
                       For example, if we have `fn[a: show](x: a) { x.show() }`:
                       - receiver_type' = TVar "t0" (the fresh var for generic param "a")
                       - trait show has type param "a" with method show(x: a) -> string
                       - We substitute "a" -> TVar "t0" in the method signature
                       - This gives us show(x: TVar "t0") -> string
                       - Now unifying TVar "t0" with TVar "t0" produces empty subst (no corruption) *)
                    let instantiated_method_sig =
                      match Trait_registry.lookup_trait trait_name with
                      | None -> method_sig (* Shouldn't happen, but fallback *)
                      | Some trait_def -> (
                          match trait_def.trait_type_param with
                          | None -> method_sig (* No type param to substitute *)
                          | Some type_param ->
                              (* Substitute the trait's type param with the receiver type *)
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

                    (* Infer types of call arguments *)
                    match infer_args type_map env1 subst1 args with
                    | Error e -> Error e
                    | Ok (subst2, arg_types) -> (
                        (* Combine receiver type with argument types *)
                        let all_arg_types = receiver_type' :: arg_types in

                        (* Get expected parameter types from instantiated method signature *)
                        let expected_param_types = List.map snd instantiated_method_sig.method_params in

                        (* Check argument count *)
                        if List.length all_arg_types <> List.length expected_param_types then
                          Error
                            (error_at
                               (ConstructorError
                                  (Printf.sprintf "Method '%s' expects %d arguments, got %d" method_name
                                     (List.length expected_param_types - 1)
                                     (List.length args)))
                               expr)
                        else
                          (* Unify each argument type with expected parameter type *)
                          let rec unify_params subst_acc actual_types expected_types =
                            match (actual_types, expected_types) with
                            | [], [] -> Ok subst_acc
                            | actual :: rest_actual, expected :: rest_expected -> (
                                let actual' = apply_substitution subst_acc actual in
                                let expected' = apply_substitution subst_acc expected in
                                match unify actual' expected' with
                                | Error e -> Error (error_at (UnificationError e) expr)
                                | Ok new_subst ->
                                    unify_params
                                      (compose_substitution new_subst subst_acc)
                                      rest_actual rest_expected)
                            | _ -> Error (error_at (ConstructorError "Argument count mismatch") expr)
                          in
                          match unify_params subst2 all_arg_types expected_param_types with
                          | Error e -> Error e
                          | Ok final_subst ->
                              (* Return the method's return type with substitutions applied *)
                              let return_type =
                                apply_substitution final_subst instantiated_method_sig.method_return_type
                              in
                              Ok (final_subst, return_type))))))
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
          (* - requires numeric type (Int or Float), returns same type *)
          let result_type = fresh_type_var () in
          (* Try to unify with Int first, then Float *)
          match unify operand_type TInt with
          | Ok subst2 -> Ok (compose_substitution subst subst2, TInt)
          | Error _ -> (
              match unify operand_type TFloat with
              | Ok subst2 -> Ok (compose_substitution subst subst2, TFloat)
              | Error _ -> (
                  (* If operand is a type variable, default to Int for now *)
                  (* TODO: This needs proper numeric type class support *)
                  match unify operand_type result_type with
                  | Error e -> Error (error_at (UnificationError e) operand)
                  | Ok subst2 ->
                      let subst3 = compose_substitution subst subst2 in
                      Ok (subst3, apply_substitution subst3 result_type))))
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
              | Ok subst3 ->
                  let subst' = compose_substitution subst subst3 in
                  let result_type = apply_substitution subst3 left_type' in
                  (* For +, also allow String concatenation *)
                  if op = "+" then
                    Ok (subst', result_type)
                  else
                    (* For -, *, /, require numeric *)
                    (* TODO: proper numeric type class *)
                    Ok (subst', result_type))
          (* Comparison operators: both same type, result Bool *)
          | "<" | ">" | "<=" | ">=" -> (
              match unify left_type' right_type with
              | Error e -> Error (error_at (UnificationError e) right)
              | Ok subst3 -> Ok (compose_substitution subst subst3, TBool))
          (* Equality operators: both same type, result Bool *)
          | "==" | "!=" -> (
              match unify left_type' right_type with
              | Error e -> Error (error_at (UnificationError e) right)
              | Ok subst3 -> Ok (compose_substitution subst subst3, TBool))
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
          let func_type =
            List.fold_right (fun param_t acc -> TFun (param_t, acc)) param_types' unified_ret_type
          in
          Ok (subst', func_type))

(* Infer function with parameter type annotations *)
and infer_function_with_annotations type_map env generics_opt params return_annot body =
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
                      | TFun (arg, ret) -> TFun (subst_generic arg, subst_generic ret)
                      | TArray elem -> TArray (subst_generic elem)
                      | THash (k, v) -> THash (subst_generic k, subst_generic v)
                      | TRecord (fields, row) ->
                          let fields' =
                            List.map (fun (f : Types.record_field_type) -> { f with typ = subst_generic f.typ }) fields
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
            with Failure msg -> (
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
                  (* Not a generic, re-raise or create fresh *)
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
                      | None -> fresh_type_var ()
                    else
                      fresh_type_var ()
                  else
                    fresh_type_var ())))
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
    | Some type_expr -> ( try Some (Annotation.type_expr_to_mono_type type_expr) with Failure _ -> None)
  in
  (* Infer body type *)
  match infer_statement type_map env' body with
  | Error e -> Error e
  | Ok (subst, body_type) -> (
      let body_type' = apply_substitution subst body_type in
      (* If we have a return type annotation, unify with it to ensure type consistency *)
      match expected_return_type_opt with
      | None -> (
          (* No return type annotation - collect all return types and unify with body type *)
          match collect_and_unify_returns type_map env' body_type' body subst with
          | Error e -> Error e
          | Ok (subst', unified_ret_type) ->
              let param_types' = List.map (apply_substitution subst') param_types in
              let func_type =
                List.fold_right (fun param_t acc -> TFun (param_t, acc)) param_types' unified_ret_type
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
                    let final_body_type = apply_substitution subst2 body_type' in
                    (* Type checking is automatic via unification above *)
                    let param_types' = List.map (apply_substitution final_subst) param_types in
                    let func_type =
                      List.fold_right (fun param_t acc -> TFun (param_t, acc)) param_types' final_body_type
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
          (* Create expected function type: arg1 -> arg2 -> ... -> result *)
          let result_type = fresh_type_var () in
          let expected_func_type = List.fold_right (fun arg_t acc -> TFun (arg_t, acc)) arg_types result_type in
          (* Unify actual function type with expected *)
          match unify func_type' expected_func_type with
          | Error e -> Error (error_at (UnificationError e) func)
          | Ok subst3 -> (
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
  | Ok (env1, subst1, field_types) ->
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
      (match spread with
      | None -> Ok (subst1, TRecord (field_types, None))
      | Some spread_expr -> (
          match infer_expression type_map env1 spread_expr with
          | Error e -> Error e
          | Ok (subst2, spread_type) ->
              let subst = compose_substitution subst1 subst2 in
              let spread_type' = apply_substitution subst spread_type in
              match spread_type' with
              | TRecord (base_fields, base_row) ->
                  let merged = merge_fields base_fields field_types in
                  Ok (subst, TRecord (merged, base_row))
              | TVar _ ->
                  let row_var = TRowVar ("r" ^ string_of_int !fresh_var_counter) in
                  fresh_var_counter := !fresh_var_counter + 1;
                  let expected_base = TRecord ([], Some row_var) in
                  (match unify spread_type' expected_base with
                  | Error e -> Error (error_at (UnificationError e) spread_expr)
                  | Ok subst3 ->
                      let final_subst = compose_substitution subst subst3 in
                      let result_row = Some (apply_substitution subst3 row_var) in
                      Ok (final_subst, TRecord (field_types, result_row)))
              | _ ->
                  Error
                    (error_at
                       (ConstructorError
                          (Printf.sprintf "Record spread expects a record, got %s" (to_string spread_type')))
                       spread_expr)))

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
              List.fold_right (fun p r -> TFun (p, r)) param_types ret_type
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
  | AST.TraitDef { name; type_param; supertraits; methods } -> (
      (* Convert AST method signatures to trait registry method signatures *)
      (* We need to treat type_param as a type variable, not a type constructor *)
      let convert_type_expr (te : AST.type_expr) : mono_type =
        let rec convert = function
          | AST.TVar v -> TVar v
          | AST.TCon c ->
              (* Check if it's the trait's type parameter *)
              if c = type_param then
                TVar c
              else
                Annotation.type_expr_to_mono_type (AST.TCon c)
          | AST.TApp (con_name, args) -> (
              if
                (* For generic types, convert recursively *)
                con_name = type_param
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
              List.fold_right (fun p r -> TFun (p, r)) param_types ret_type
          | AST.TUnion types -> normalize_union (List.map convert types)
          | AST.TRecord (_fields, _row) -> failwith "Record types not yet implemented in trait definition"
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

      let method_sigs = List.map convert_method_sig methods in

      let trait_def =
        {
          Trait_registry.trait_name = name;
          trait_type_param = Some type_param;
          trait_supertraits = supertraits;
          trait_methods = method_sigs;
        }
      in

      (* Validate and register trait *)
      match Trait_registry.validate_trait_def trait_def with
      | Error msg -> Error (error (ConstructorError msg))
      | Ok () ->
          Trait_registry.register_trait trait_def;
          Ok (empty_substitution, TNull))
  | AST.ImplDef { impl_trait_name; impl_type_params; impl_for_type; impl_methods } -> (
      (* Convert impl_for_type from AST.type_expr to mono_type *)
      let for_type_mono = Annotation.type_expr_to_mono_type impl_for_type in

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
                  | Some ptype -> Ok (params @ [ (pname, Annotation.type_expr_to_mono_type ptype) ])
                  | None ->
                      Error
                        (error
                           (ConstructorError
                              (Printf.sprintf
                                 "Impl method '%s' parameter '%s' is missing a type annotation" m.impl_method_name
                                 pname)))))
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
                        (Printf.sprintf "Impl method '%s' is missing a return type annotation" m.impl_method_name)))
            | Some rt ->
                let return_type = Annotation.type_expr_to_mono_type rt in
                let method_env =
                  List.fold_left
                    (fun env_acc (pname, ptype) -> TypeEnv.add pname (mono_to_poly ptype) env_acc)
                    env param_types
                in
                (match infer_expression type_map method_env m.impl_method_body with
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
                      Error (error_at (ReturnTypeMismatch (return_type', inferred_body_type')) m.impl_method_body)))
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
      | Ok (methods, method_subst) ->
          let for_type_mono' = apply_substitution method_subst for_type_mono in
          let methods' =
            List.map
              (fun (m : Trait_registry.method_sig) ->
                {
                  m with
                  method_params =
                    List.map (fun (name, ty) -> (name, apply_substitution method_subst ty)) m.method_params;
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
              Trait_registry.register_impl impl_def;
              Ok (method_subst, TNull))
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
                match List.find_opt (fun (f : Types.record_field_type) -> f.name = field_name) scrutinee_fields with
                | None ->
                    Error
                      (error
                         (PatternError
                            (Printf.sprintf "Record pattern field '%s' not found in scrutinee type %s" field_name
                               (to_string scrutinee_type))))
                | Some scrutinee_field ->
                    let effective_pat =
                      match field.pat_field_pattern with
                      | Some p -> p
                      | None -> AST.{ pat = PVariable field_name; pos = pattern.pos }
                    in
                    (match check_pattern effective_pat scrutinee_field.typ with
                    | Error e -> Error e
                    | Ok field_bindings -> check_fields (field_name :: seen_names) (bindings @ field_bindings) rest_fields))
          in
          check_fields [] [] fields
      | _ ->
          Error
            (error
               (PatternError (Printf.sprintf "Record pattern doesn't match scrutinee type %s" (to_string scrutinee_type)))))

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
and infer_let type_map env name expr type_annotation =
  (* Check if the expression is a function with a return type annotation *)
  (* If so, create a partially constrained type for recursion *)
  let self_type =
    match (expr.expr, type_annotation) with
    | AST.Function f, _ -> (
        match f.return_type with
        | None -> fresh_type_var ()
        | Some type_expr -> (
            try
              (* Create a partially constrained function type: param1 -> param2 -> ... -> return_type *)
              let return_type = Annotation.type_expr_to_mono_type type_expr in
              let param_types =
                List.map
                  (fun (_name, annot_opt) ->
                    match annot_opt with
                    | None -> fresh_type_var ()
                    | Some annot -> Annotation.type_expr_to_mono_type annot)
                  f.params
              in
              (* Build function type from parameters and return type *)
              List.fold_right (fun param_t acc -> TFun (param_t, acc)) param_types return_type
            with Failure _ -> fresh_type_var ()))
    | _, Some type_expr -> (
        (* Phase 4.4: Use type annotation from let binding *)
        try Annotation.type_expr_to_mono_type type_expr with Failure _ -> fresh_type_var ())
    | _, None -> fresh_type_var ()
  in
  (* Add to environment as monomorphic (not generalized yet) *)
  let env_with_self = TypeEnv.add name (mono_to_poly self_type) env in
  (* Infer expression type with self in scope *)
  match infer_expression type_map env_with_self expr with
  | Error e -> Error e
  | Ok (subst1, expr_type) -> (
      (* Unify the inferred type with our placeholder *)
      let self_type' = apply_substitution subst1 self_type in
      match unify self_type' expr_type with
      | Error e -> Error (error_at (UnificationError e) expr)
      | Ok subst2 ->
          let final_subst = compose_substitution subst1 subst2 in
          let final_type = apply_substitution subst2 expr_type in
          (* Generalize the type *)
          let env' = apply_substitution_env final_subst env in
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

let infer_program ?(env = empty_env) (program : AST.program) : (type_env * type_map * mono_type) infer_result =
  reset_fresh_counter ();
  Annotation.clear_type_aliases ();
  let type_map = create_type_map () in
  let rec go env subst stmts =
    match stmts with
    | [] -> Ok (env, subst, TNull)
    | [ stmt_node ] -> (
        let stmt : AST.statement = stmt_node in
        (match stmt.stmt with
        | AST.TypeAlias alias_def -> Annotation.register_type_alias alias_def
        | _ -> ());
        match infer_statement type_map env stmt with
        | Error e -> Error e
        | Ok (subst', result_type) ->
            let final_subst = compose_substitution subst subst' in
            let env' = apply_substitution_env final_subst env in
            let result_type' = apply_substitution final_subst result_type in
            (* Add let bindings to final environment *)
            let env'' =
              match stmt.stmt with
              | AST.Let let_binding ->
                  let poly = generalize env' result_type' in
                  TypeEnv.add let_binding.name poly env'
              | _ -> env'
            in
            Ok (env'', final_subst, result_type'))
    | stmt_node :: rest -> (
        let stmt : AST.statement = stmt_node in
        (match stmt.stmt with
        | AST.TypeAlias alias_def -> Annotation.register_type_alias alias_def
        | _ -> ());
        match infer_statement type_map env stmt with
        | Error e -> Error e
        | Ok (subst', stmt_type) ->
            let subst'' = compose_substitution subst subst' in
            let env' = apply_substitution_env subst' env in
            (* Add let bindings to environment for subsequent statements *)
            let env'' =
              match stmt.stmt with
              | AST.Let let_binding ->
                  let poly = generalize env' stmt_type in
                  TypeEnv.add let_binding.name poly env'
              | _ -> env'
            in
            go env'' subst'' rest)
  in
  match go env empty_substitution program with
  | Error e -> Error e
  | Ok (env', final_subst, result_type) ->
      (* Apply final substitution to all types in type_map *)
      let apply_subst_to_type_map () =
        Hashtbl.iter
          (fun id ty ->
            let ty' = apply_substitution final_subst ty in
            Hashtbl.replace type_map id ty')
          type_map
      in
      apply_subst_to_type_map ();
      Ok (env', type_map, result_type)

module Test = struct
  (* Helper to parse and infer *)
  let infer_string code =
    reset_fresh_counter ();
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
    infers_to "fn(x) { x + 1 }" (TFun (TInt, TInt))

  let%test "infer identity function" =
    (* fn(x) { x } should be t0 -> t0 (polymorphic) *)
    match infer_string "fn(x) { x }" with
    | Error _ -> false
    | Ok (_, _type_map, TFun (TVar a, TVar b)) -> a = b (* same type variable *)
    | Ok _ -> false

  let%test "infer two-arg function" =
    (* fn(x, y) { x + y } should have both args same type and return same type *)
    (* Without type classes, we can't constrain to just numeric types *)
    match infer_string "fn(x, y) { x + y }" with
    | Error _ -> false
    | Ok (_, _type_map, TFun (TVar a, TFun (TVar b, TVar c))) ->
        (* All three type vars should be the same *)
        a = b && b = c
    | Ok _ -> false

  let%test "infer two-arg function with literal" =
    (* fn(x, y) { x + y + 1 } should be (Int, Int) -> Int because of the literal *)
    infers_to "fn(x, y) { x + y + 1 }" (TFun (TInt, TFun (TInt, TInt)))

  let%test "infer function call" =
    (* fn(x) { x + 1 }(5) should be Int *)
    infers_to "fn(x) { x + 1 }(5)" TInt

  let%test "infer let binding" =
    (* let x = 5; x should be Int *)
    infers_to "let x = 5; x" TInt

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

  let%test "infer explicit row-polymorphic parameter annotation" =
    infers_to "let p = { x: 5, y: 10, z: 20 }; let get_x = fn(r: { x: int, ...row }) -> int { r.x }; get_x(p)" TInt

  let%test "infer explicit row-polymorphic parameter with multiple field accesses" =
    infers_to
      "let p = { x: 5, y: 10, z: 20 }; let sum_xy = fn(r: { x: int, y: int, ...row }) -> int { r.x + r.y }; sum_xy(p)"
      TInt

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
    infers_to code (TFun (TInt, TInt))

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
end
