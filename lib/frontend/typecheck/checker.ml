(* Typecheck module - main entry point for type checking *)

open Types

(* Re-export commonly used types *)
type mono = mono_type
type poly = poly_type
type env = Infer.type_env

(* Result of type checking *)
type typecheck_result = {
  result_type : mono_type; (* Type of the final expression *)
  environment : Infer.type_env; (* Final type environment with all bindings *)
}

(* Error with source location info *)
type error = {
  message : string;
  loc : Source_loc.loc option; (* line/column if available *)
}

(* Convert infer error to typecheck error, optionally with source for location *)
let error_of_infer_error ?(source : string option) (e : Infer.infer_error) : error =
  let loc =
    match (source, e.pos) with
    | Some src, Some pos -> Some (Source_loc.offset_to_loc src pos)
    | _ -> None
  in
  { message = Infer.error_to_string e; loc }

(* Format error with location prefix *)
let format_error (err : error) : string =
  match err.loc with
  | None -> err.message
  | Some loc -> Printf.sprintf "%s: %s" (Source_loc.to_string loc) err.message

(* Format error with source context showing the offending line *)
let format_error_with_context (source : string) (err : error) : string =
  match err.loc with
  | None -> err.message
  | Some loc ->
      let context = Source_loc.format_with_context source loc in
      Printf.sprintf "%s: %s\n%s" (Source_loc.to_string loc) err.message context

(* ============================================================
   Main API
   ============================================================ *)

(* Type check a program (list of statements).
   Returns the type of the last expression and the final environment.
   Note: Without source, we can't provide location info. Use check_string for that. *)
let check_program ?(env = Infer.empty_env) (program : Syntax.Ast.AST.program) : (typecheck_result, error) result =
  match Infer.infer_program ~env program with
  | Error e -> Error (error_of_infer_error e)
  | Ok (final_env, result_type) -> Ok { result_type; environment = final_env }

(* Type check source code string.
    Parses and type checks in one step.
    Errors include source location information. *)
let check_string ?(env = Infer.empty_env) (source : string) : (typecheck_result, error) result =
  match Syntax.Parser.parse source with
  | Error errors -> Error { message = "Parse error: " ^ String.concat ", " errors; loc = None }
  | Ok program -> (
      match Infer.infer_program ~env program with
      | Error e -> Error (error_of_infer_error ~source e)
      | Ok (final_env, result_type) -> Ok { result_type; environment = final_env })

(* ============================================================
   Phase 2: Type check with annotations
   ============================================================ *)

(* Check if a let binding's annotation matches its inferred type *)
let check_let_annotation
    (name : string) (annotation : Syntax.Ast.AST.type_expr option) (inferred_type : mono_type) :
    (unit, error) result =
  match annotation with
  | None ->
      (* No annotation, nothing to check *)
      Ok ()
  | Some type_annot -> (
      (* Convert annotation to mono_type and check match *)
      try
        let annotated_type = Annotation.type_expr_to_mono_type type_annot in
        if Annotation.check_annotation annotated_type inferred_type then
          Ok ()
        else
          Error
            {
              message =
                Printf.sprintf "Type annotation mismatch for '%s': expected %s but inferred %s" name
                  (Annotation.format_mono_type annotated_type)
                  (Annotation.format_mono_type inferred_type);
              loc = None;
            }
      with Failure msg ->
        Error { message = Printf.sprintf "Invalid type annotation for '%s': %s" name msg; loc = None })

(* Check if a function expression's return type annotation matches its inferred type *)
let check_function_annotation (return_annotation : Syntax.Ast.AST.type_expr option) (inferred_type : mono_type) :
    (unit, error) result =
  match return_annotation with
  | None ->
      (* No annotation, nothing to check *)
      Ok ()
  | Some type_annot -> (
      (* Extract the return type from the function type *)
      (* inferred_type should be TFun(...) or possibly a polymorphic function *)
      (* We need to extract just the return type *)
      try
        let annotated_return_type = Annotation.type_expr_to_mono_type type_annot in
        (* Extract the return type from the function type by recursively unwrapping TFun *)
        let rec extract_return_type (t : mono_type) : mono_type =
          match t with
          | TFun (_, ret) -> extract_return_type ret
          | other -> other
        in
        let actual_return = extract_return_type inferred_type in
        if Annotation.check_annotation annotated_return_type actual_return then
          Ok ()
        else
          Error
            {
              message =
                Printf.sprintf "Function return type annotation mismatch: expected %s but inferred %s"
                  (Annotation.format_mono_type annotated_return_type)
                  (Annotation.format_mono_type actual_return);
              loc = None;
            }
      with Failure msg -> Error { message = Printf.sprintf "Invalid function annotation: %s" msg; loc = None })

(* Type check a program with annotation support.
    This checks that all annotations match the inferred types.
    For Phase 2, constraint validation is skipped (Phase 3 work). *)
let check_program_with_annotations ?(env = Infer.empty_env) (program : Syntax.Ast.AST.program) :
    (typecheck_result, error) result =
  (* First, do standard inference *)
  match Infer.infer_program ~env program with
  | Error e -> Error (error_of_infer_error e)
  | Ok (final_env, result_type) -> (
      (* Phase 2: Validate annotations against inferred types *)
      let rec check_stmts_with_infer (stmts : Syntax.Ast.AST.statement list) (env_check : env) :
          (unit, error) result =
        match stmts with
        | [] -> Ok ()
        | stmt :: rest -> (
            match stmt.stmt with
            | Syntax.Ast.AST.Let let_binding -> (
                (* Check if annotation matches inferred type of the let binding *)
                match Infer.TypeEnv.find_opt let_binding.name env_check with
                | None ->
                    (* Variable not in environment (shouldn't happen) *)
                    Error
                      {
                        message =
                          Printf.sprintf "Internal error: variable '%s' not in environment" let_binding.name;
                        loc = None;
                      }
                | Some (Forall (_, mono_type)) -> (
                    (* Check let binding annotation *)
                    match check_let_annotation let_binding.name let_binding.type_annotation mono_type with
                    | Error e -> Error e
                    | Ok () -> (
                        (* Also check function expression annotation if present *)
                        match check_expr_annotations let_binding.value mono_type env_check with
                        | Error e -> Error e
                        | Ok () -> check_stmts_with_infer rest env_check)))
            | Syntax.Ast.AST.Block nested_stmts -> (
                (* Recursively check block statements *)
                match check_stmts_with_infer nested_stmts env_check with
                | Error e -> Error e
                | Ok () -> check_stmts_with_infer rest env_check)
            | _ ->
                (* Other statements don't have annotations to check *)
                check_stmts_with_infer rest env_check)
      and check_expr_annotations (expr : Syntax.Ast.AST.expression) (inferred : mono_type) (env_check : env) :
          (unit, error) result =
        match expr.expr with
        | Syntax.Ast.AST.Function { return_type; params = _; body; generics = _ } -> (
            (* Check function return type annotation *)
            match check_function_annotation return_type inferred with
            | Error e -> Error e
            | Ok () ->
                (* Also recursively check body statements *)
                check_stmts_with_infer [ body ] env_check)
        | _ -> Ok () (* Other expressions don't have annotations to check *)
      in
      match check_stmts_with_infer program final_env with
      | Error e -> Error e
      | Ok () -> Ok { result_type; environment = final_env })

(* Type check source code with annotations.
   Parses and type checks in one step, with annotation support. *)
let check_string_with_annotations ?(env = Infer.empty_env) (source : string) : (typecheck_result, error) result =
  match Syntax.Parser.parse source with
  | Error errors -> Error { message = "Parse error: " ^ String.concat ", " errors; loc = None }
  | Ok program -> check_program_with_annotations ~env program

(* Get the type of an expression as a string *)
let type_string (source : string) : string =
  match check_string source with
  | Error e -> "Error: " ^ e.message
  | Ok result -> to_string result.result_type

(* ============================================================
   Environment utilities
   ============================================================ *)

(* Look up a variable's type in the environment *)
let lookup (name : string) (env : env) : poly_type option = Infer.TypeEnv.find_opt name env

(* Look up and format as string *)
let lookup_string (name : string) (env : env) : string =
  match lookup name env with
  | None -> "undefined"
  | Some poly -> poly_type_to_string poly

(* Add a type binding to the environment *)
let bind (name : string) (poly : poly_type) (env : env) : env = Infer.TypeEnv.add name poly env

(* Create environment with builtin types from prelude *)
let default_env () : env = Builtins.prelude_env ()

(* ============================================================
   Pretty printing
   ============================================================ *)

(* Format an environment as a string (useful for debugging/REPL) *)
let env_to_string (env : env) : string =
  let bindings = Infer.TypeEnv.bindings env in
  match bindings with
  | [] -> "(empty environment)"
  | _ -> bindings |> List.map (fun (name, poly) -> name ^ " : " ^ poly_type_to_string poly) |> String.concat "\n"

(* ============================================================
   Tests
   ============================================================ *)

let%test "check_string literal" =
  match check_string "42" with
  | Ok { result_type = TInt; _ } -> true
  | _ -> false

let%test "check_string function" =
  match check_string "fn(x) { x + 1 }" with
  | Ok { result_type = TFun (TInt, TInt); _ } -> true
  | _ -> false

let%test "check_string let binding adds to env" =
  match check_string "let x = 5; x" with
  | Ok { result_type = TInt; environment } -> (
      match lookup "x" environment with
      | Some (Forall ([], TInt)) -> true
      | _ -> false)
  | _ -> false

let%test "check_string polymorphic function in env" =
  match check_string "let id = fn(x) { x }; id" with
  | Ok { environment; _ } -> (
      match lookup "id" environment with
      | Some (Forall (vars, TFun (TVar a, TVar b))) -> List.length vars = 1 && a = b
      | _ -> false)
  | _ -> false

let%test "type_string helper" = type_string "1 + 2" = "Int" && type_string "true" = "Bool"

let%test "error on type mismatch" =
  match check_string "1 + true" with
  | Error _ -> true
  | Ok _ -> false

let%test "env_to_string" =
  let env = bind "x" (Forall ([], TInt)) Infer.empty_env in
  env_to_string env = "x : Int"

(* ============================================================
   Builtin/Prelude Tests
   ============================================================ *)

let%test "prelude has len" =
  match lookup "len" (default_env ()) with
  | Some _ -> true
  | None -> false

let%test "prelude has all builtins" =
  let env = default_env () in
  List.for_all (fun name -> Option.is_some (lookup name env)) [ "len"; "first"; "last"; "rest"; "push"; "puts" ]

(* Helper that uses default_env *)
let check code =
  Infer.reset_fresh_counter ();
  check_string ~env:(default_env ()) code

let%test "len returns Int" =
  match check "len([1, 2, 3])" with
  | Ok { result_type = TInt; _ } -> true
  | _ -> false

let%test "len on string returns Int" =
  match check "len(\"hello\")" with
  | Ok { result_type = TInt; _ } -> true
  | _ -> false

let%test "first returns element type" =
  match check "first([1, 2, 3])" with
  | Ok { result_type = TInt; _ } -> true
  | _ -> false

let%test "last returns element type" =
  match check "last([true, false])" with
  | Ok { result_type = TBool; _ } -> true
  | _ -> false

let%test "rest returns array type" =
  match check "rest([1, 2, 3])" with
  | Ok { result_type = TArray TInt; _ } -> true
  | _ -> false

let%test "push returns array type" =
  match check "push([1, 2], 3)" with
  | Ok { result_type = TArray TInt; _ } -> true
  | _ -> false

let%test "puts returns Null" =
  match check "puts(42)" with
  | Ok { result_type = TNull; _ } -> true
  | _ -> false

let%test "puts accepts any type" =
  match check "puts(\"hello\")" with
  | Ok { result_type = TNull; _ } -> true
  | _ -> false

let%test "chained builtins" =
  (* first(rest([1, 2, 3])) should be Int *)
  match check "first(rest([1, 2, 3]))" with
  | Ok { result_type = TInt; _ } -> true
  | _ -> false

let%test "push then len" =
  (* len(push([1, 2], 3)) should be Int *)
  match check "len(push([1, 2], 3))" with
  | Ok { result_type = TInt; _ } -> true
  | _ -> false

(* ============================================================
   Source Location Tests
   ============================================================ *)

let%test "error includes source location" =
  match check_string "1 + true" with
  | Error { loc = Some loc; _ } -> loc.line = 1 && loc.column > 0
  | _ -> false

let%test "error location points to problematic expression" =
  (* "true" starts at column 5 (1-indexed) in "1 + true" *)
  match check_string "1 + true" with
  | Error { loc = Some loc; _ } -> loc.column = 5
  | _ -> false

let%test "multiline error location" =
  let code = "let x = 5;\nlet y = x + true;" in
  match check_string code with
  | Error { loc = Some loc; _ } -> loc.line = 2
  | _ -> false

let%test "format_error includes line:col" =
  match check_string "1 + true" with
  | Error err -> String.sub (format_error err) 0 4 = "1:5:"
  | Ok _ -> false

let%test "format_error_with_context shows source line" =
  let source = "1 + true" in
  match check_string source with
  | Error err ->
      let formatted = format_error_with_context source err in
      String.length formatted > 0 && String.sub formatted 0 4 = "1:5:"
  | Ok _ -> false
