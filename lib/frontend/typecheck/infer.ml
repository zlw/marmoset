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

let rec infer_expression (env : type_env) (expr : AST.expression) : (substitution * mono_type) infer_result =
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
  | AST.Prefix (op, operand) -> infer_prefix env op operand
  (* Infix operators *)
  | AST.Infix (left, op, right) -> infer_infix env left op right
  (* Type checking operator: x is int *)
  | AST.TypeCheck (expr, type_ann) -> (
      (* Infer type of expression *)
      match infer_expression env expr with
      | Error e -> Error e
      | Ok (subst1, _expr_type) ->
          (* Convert type annotation to mono_type (for validation, not currently used) *)
          let _check_type = Annotation.type_expr_to_mono_type type_ann in
          (* Result is always bool *)
          Ok (subst1, TBool))
  (* If expressions *)
  | AST.If (condition, consequence, alternative) -> infer_if env condition consequence alternative
  (* Function literals *)
  | AST.Function f ->
      (* Phase 2: Use parameter and return type annotations to guide inference *)
      infer_function_with_annotations env f.params f.return_type f.body
  (* Function calls *)
  | AST.Call (func, args) -> infer_call env func args
  (* Arrays *)
  | AST.Array elements -> infer_array env elements
  (* Hashes *)
  | AST.Hash pairs -> infer_hash env pairs
  (* Index expressions *)
  | AST.Index (container, index) -> infer_index env container index
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
          match infer_args env empty_substitution args with
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
      match infer_expression env scrutinee with
      | Error e -> Error e
      | Ok (subst, scrutinee_type) -> (
          let env' = apply_substitution_env subst env in
          (* Check exhaustiveness *)
          match Exhaustiveness.check_exhaustive scrutinee_type arms with
          | Error msg -> Error (error_at (MatchError msg) expr)
          | Ok () ->
              (* Check each arm and collect body types *)
              infer_match_arms env' scrutinee_type arms subst expr))
(* ============================================================
   Prefix Operators: !, -
   ============================================================ *)

and infer_prefix env op operand =
  match infer_expression env operand with
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

and infer_infix env left op right =
  match infer_expression env left with
  | Error e -> Error e
  | Ok (subst1, left_type) -> (
      let env' = apply_substitution_env subst1 env in
      match infer_expression env' right with
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

and infer_if env condition consequence alternative =
  (* Infer condition type *)
  match infer_expression env condition with
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
          match infer_statement env_cons consequence with
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
                  match infer_statement env_alt alt with
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

and infer_function env params body =
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
  match infer_statement env' body with
  | Error e -> Error e
  | Ok (subst, body_type) ->
      (* Apply substitution to parameter types *)
      let param_types' = List.map (apply_substitution subst) param_types in
      (* Build function type: p1 -> p2 -> ... -> body_type *)
      let func_type = List.fold_right (fun param_t acc -> TFun (param_t, acc)) param_types' body_type in
      Ok (subst, func_type)

(* Infer function with parameter type annotations *)
and infer_function_with_annotations env params return_annot body =
  (* Extract parameter names and type annotations *)
  let param_info = params in
  (* For each parameter, either use its annotation or create a fresh type variable *)
  let param_types =
    List.map
      (fun (_name, annot_opt) ->
        match annot_opt with
        | None -> fresh_type_var ()
        | Some type_expr -> (
            try Annotation.type_expr_to_mono_type type_expr
            with Failure _ -> fresh_type_var () (* Fallback to fresh if annotation fails *)))
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
  match infer_statement env' body with
  | Error e -> Error e
  | Ok (subst, body_type) -> (
      let body_type' = apply_substitution subst body_type in
      (* If we have a return type annotation, unify with it to ensure type consistency *)
      match expected_return_type_opt with
      | None ->
          (* No return type annotation *)
          let param_types' = List.map (apply_substitution subst) param_types in
          let func_type = List.fold_right (fun param_t acc -> TFun (param_t, acc)) param_types' body_type' in
          Ok (subst, func_type)
      | Some expected_ret_type -> (
          (* First, validate all explicit return statements match expected type *)
          match validate_return_statements env' expected_ret_type body with
          | Error e -> Error e
          | Ok () -> (
              (* Then check that inferred return type matches annotation *)
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
                  Ok (final_subst, func_type))))

(* ============================================================
   Function Calls
   ============================================================ *)

and infer_call env func args =
  (* Infer function type *)
  match infer_expression env func with
  | Error e -> Error e
  | Ok (subst1, func_type) -> (
      (* Infer argument types *)
      match infer_args (apply_substitution_env subst1 env) subst1 args with
      | Error e -> Error e
      | Ok (subst2, arg_types) -> (
          let func_type' = apply_substitution subst2 func_type in
          (* Create expected function type: arg1 -> arg2 -> ... -> result *)
          let result_type = fresh_type_var () in
          let expected_func_type = List.fold_right (fun arg_t acc -> TFun (arg_t, acc)) arg_types result_type in
          (* Unify actual function type with expected *)
          match unify func_type' expected_func_type with
          | Error e -> Error (error_at (UnificationError e) func)
          | Ok subst3 ->
              let final_subst = compose_substitution subst2 subst3 in
              let final_result = apply_substitution subst3 result_type in
              Ok (final_subst, final_result)))

(* Helper to infer types of a list of arguments *)
and infer_args env subst args =
  match args with
  | [] -> Ok (subst, [])
  | arg :: rest -> (
      match infer_expression env arg with
      | Error e -> Error e
      | Ok (subst1, arg_type) -> (
          let subst' = compose_substitution subst subst1 in
          let env' = apply_substitution_env subst1 env in
          match infer_args env' subst' rest with
          | Error e -> Error e
          | Ok (subst'', rest_types) ->
              let arg_type' = apply_substitution subst'' arg_type in
              Ok (subst'', arg_type' :: rest_types)))

(* ============================================================
   Arrays
   ============================================================ *)

and infer_array env elements =
  match elements with
  | [] ->
      (* Empty array - create fresh type variable for element type *)
      let elem_type = fresh_type_var () in
      Ok (empty_substitution, TArray elem_type)
  | first :: rest -> (
      (* Infer first element type *)
      match infer_expression env first with
      | Error e -> Error e
      | Ok (subst1, first_type) ->
          (* All other elements must have same type *)
          infer_array_elements (apply_substitution_env subst1 env) subst1 first_type rest)

and infer_array_elements env subst elem_type elements =
  match elements with
  | [] -> Ok (subst, TArray elem_type)
  | elem :: rest -> (
      match infer_expression env elem with
      | Error e -> Error e
      | Ok (subst1, this_type) -> (
          let subst' = compose_substitution subst subst1 in
          let elem_type' = apply_substitution subst1 elem_type in
          match unify elem_type' this_type with
          | Error _ -> Error (error_at (ArrayElementMismatch (elem_type', this_type)) elem)
          | Ok subst2 ->
              let subst'' = compose_substitution subst' subst2 in
              let elem_type'' = apply_substitution subst2 elem_type' in
              infer_array_elements (apply_substitution_env subst2 env) subst'' elem_type'' rest))

(* ============================================================
   Hashes
   ============================================================ *)

and infer_hash env pairs =
  match pairs with
  | [] ->
      (* Empty hash - create fresh type variables *)
      let key_type = fresh_type_var () in
      let val_type = fresh_type_var () in
      Ok (empty_substitution, THash (key_type, val_type))
  | (first_key, first_val) :: rest -> (
      match infer_expression env first_key with
      | Error e -> Error e
      | Ok (subst1, key_type) -> (
          let env' = apply_substitution_env subst1 env in
          match infer_expression env' first_val with
          | Error e -> Error e
          | Ok (subst2, val_type) ->
              let subst = compose_substitution subst1 subst2 in
              let key_type' = apply_substitution subst2 key_type in
              infer_hash_pairs (apply_substitution_env subst2 env') subst key_type' val_type rest))

and infer_hash_pairs env subst key_type val_type pairs =
  match pairs with
  | [] -> Ok (subst, THash (key_type, val_type))
  | (k, v) :: rest -> (
      match infer_expression env k with
      | Error e -> Error e
      | Ok (subst1, this_key_type) -> (
          let subst' = compose_substitution subst subst1 in
          let key_type' = apply_substitution subst1 key_type in
          match unify key_type' this_key_type with
          | Error _ -> Error (error_at (HashKeyMismatch (key_type', this_key_type)) k)
          | Ok subst2 -> (
              let subst'' = compose_substitution subst' subst2 in
              let env' = apply_substitution_env subst2 env in
              match infer_expression env' v with
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
                      infer_hash_pairs (apply_substitution_env subst4 env') final_subst key_type'' val_type'' rest
                  ))))

(* ============================================================
   Index Expressions
   ============================================================ *)

and infer_index env container index_expr =
  match infer_expression env container with
  | Error e -> Error e
  | Ok (subst1, container_type) -> (
      let env' = apply_substitution_env subst1 env in
      match infer_expression env' index_expr with
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
              | TString | TFloat | TBool | TNull | TArray _ | THash _ | TFun _ | TUnion _ | TEnum _ -> (
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

and infer_statement env stmt =
  match stmt.stmt with
  | AST.ExpressionStmt expr -> infer_expression env expr
  | AST.Return expr -> infer_expression env expr
  | AST.Block stmts -> infer_block env stmts
  | AST.Let let_binding -> infer_let env let_binding.name let_binding.value let_binding.type_annotation
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

(* Simple validation: check that all explicit return statements match expected type *)
and validate_return_statements (env : type_env) (expected_type : mono_type) (stmt : AST.statement) :
    (unit, infer_error) result =
  match stmt.stmt with
  | AST.Return expr -> (
      match infer_expression env expr with
      | Error e -> Error e
      | Ok (_subst, inferred_type) -> (
          match Unify.unify inferred_type expected_type with
          | Ok _ -> Ok ()
          | Error _ -> Error (error_at_stmt (ReturnTypeMismatch (expected_type, inferred_type)) stmt)))
  | AST.Block stmts ->
      let rec check_all stmts =
        match stmts with
        | [] -> Ok ()
        | s :: rest -> (
            match validate_return_statements env expected_type s with
            | Error e -> Error e
            | Ok () -> check_all rest)
      in
      check_all stmts
  | AST.ExpressionStmt expr -> (
      match expr.expr with
      | AST.If (_cond, cons, alt) -> (
          match validate_return_statements env expected_type cons with
          | Error e -> Error e
          | Ok () -> (
              match alt with
              | None -> Ok ()
              | Some alt_stmt -> validate_return_statements env expected_type alt_stmt))
      | _ -> Ok ())
  | AST.Let _ -> Ok ()
  | AST.EnumDef _ -> Ok ()

(* ============================================================
   Pattern Matching Helpers
   ============================================================ *)

(* Check all arms of a match expression and collect their body types *)
and infer_match_arms env scrutinee_type arms subst match_expr =
  let rec loop acc_subst arm_types = function
    | [] -> (
        (* All arms processed, unify all arm body types *)
        match arm_types with
        | [] -> Error (error_at (MatchError "Match expression must have at least one arm") match_expr)
        | first :: rest -> (
            let rec unify_all s types =
              match types with
              | [] -> Ok s
              | t :: rest_types -> (
                  let first' = apply_substitution s first in
                  let t' = apply_substitution s t in
                  match Unify.unify first' t' with
                  | Error e -> Error (error_at (UnificationError e) match_expr)
                  | Ok s2 ->
                      let new_s = compose_substitution s s2 in
                      unify_all new_s rest_types)
            in
            match unify_all empty_substitution rest with
            | Error e -> Error e
            | Ok unify_subst ->
                let final_subst = compose_substitution acc_subst unify_subst in
                let result_type = apply_substitution unify_subst first in
                Ok (final_subst, result_type)))
    | arm :: rest_arms -> (
        match infer_match_arm env scrutinee_type arm with
        | Error e -> Error e
        | Ok (arm_subst, body_type) ->
            let new_subst = compose_substitution acc_subst arm_subst in
            loop new_subst (body_type :: arm_types) rest_arms)
  in
  loop subst [] arms

(* Infer the type of a single match arm *)
and infer_match_arm env scrutinee_type arm =
  (* Check patterns and get bindings *)
  match check_patterns arm.AST.patterns scrutinee_type with
  | Error e -> Error e
  | Ok bindings ->
      (* Extend environment with pattern bindings *)
      let env' = List.fold_left (fun e (name, ty) -> TypeEnv.add name (mono_to_poly ty) e) env bindings in
      (* Infer body type *)
      infer_expression env' arm.AST.body

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
and infer_let env name expr type_annotation =
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
  match infer_expression env_with_self expr with
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

and infer_block env stmts =
  match stmts with
  | [] -> Ok (empty_substitution, TNull)
  | [ stmt ] -> infer_statement env stmt
  | stmt :: rest -> (
      match infer_statement env stmt with
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
          match infer_block env' rest with
          | Error e -> Error e
          | Ok (subst2, result_type) -> Ok (compose_substitution subst1 subst2, result_type)))

(* ============================================================
   Phase 2: Bidirectional Type Checking
   ============================================================ *)

(* Check an expression against an expected type.
   For Phase 2, this is simple: just infer the type and verify it matches.
   In Phase 2.5+, this could do more sophisticated bidirectional flow.
*)
let check_expression (env : type_env) (expr : AST.expression) (expected : mono_type) :
    (substitution * mono_type) infer_result =
  match infer_expression env expr with
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

let infer_program ?(env = empty_env) (program : AST.program) : (type_env * mono_type) infer_result =
  reset_fresh_counter ();
  let rec go env subst stmts =
    match stmts with
    | [] -> Ok (env, TNull)
    | [ stmt ] -> (
        match infer_statement env stmt with
        | Error e -> Error e
        | Ok (subst', result_type) ->
            let env' = apply_substitution_env (compose_substitution subst subst') env in
            let result_type' = apply_substitution subst' result_type in
            (* Add let bindings to final environment *)
            let env'' =
              match stmt.stmt with
              | AST.Let let_binding ->
                  let poly = generalize env' result_type' in
                  TypeEnv.add let_binding.name poly env'
              | _ -> env'
            in
            Ok (env'', result_type'))
    | stmt :: rest -> (
        match infer_statement env stmt with
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
  go env empty_substitution program

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
    | Ok (_, t) ->
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
    | Ok (_, TFun (TVar a, TVar b)) -> a = b (* same type variable *)
    | Ok _ -> false

  let%test "infer two-arg function" =
    (* fn(x, y) { x + y } should have both args same type and return same type *)
    (* Without type classes, we can't constrain to just numeric types *)
    match infer_string "fn(x, y) { x + y }" with
    | Error _ -> false
    | Ok (_, TFun (TVar a, TFun (TVar b, TVar c))) ->
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

  let%test "error on type mismatch in if" =
    match infer_string "if (true) { 1 } else { \"hello\" }" with
    | Error { kind = IfBranchMismatch _; _ } -> true
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
    | Ok (_, t) -> (
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
    | Ok (_, t) -> (
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
    | Ok (_, t) -> (
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

  let%test "match expression type error: mismatched arm types" =
    let code = "match 5 {
  0: 42
  _: \"string\"
}" in
    match infer_string code with
    | Error _ -> true
    | Ok _ -> false

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
