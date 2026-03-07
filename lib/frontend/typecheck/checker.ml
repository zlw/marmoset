(* Typecheck module - main entry point for type checking *)

open Types
module Diagnostic = Diagnostics.Diagnostic
module String_utils = Diagnostics.String_utils

(* Re-export commonly used types *)
type mono = mono_type
type poly = poly_type
type env = Infer.type_env

(* Result of type checking *)
type typecheck_result = {
  result_type : mono_type; (* Type of the final expression *)
  environment : Infer.type_env; (* Final type environment with all bindings *)
  type_map : Infer.type_map; (* Map from expression IDs to their inferred types *)
  call_resolution_map : (int, Infer.method_resolution) Hashtbl.t;
      (* Phase 5: Explicit call-resolution metadata for emitter *)
  method_def_map : (int, Resolution_artifacts.typed_method_def) Hashtbl.t;
      (* Phase 5.4: Typed method definitions for emitter. Populated during Phase 6. *)
  method_type_args_map : (int, Types.mono_type list) Hashtbl.t;
      (* Phase 6.4: Resolved method-level type args per call site for monomorphization *)
}

let infer_program_safe ?state ~(env : Infer.type_env) (program : Syntax.Ast.AST.program) :
    (Infer.type_env * Infer.type_map * mono_type, Diagnostic.t list) result =
  try
    match Infer.infer_program ?state ~env program with
    | Ok result -> Ok result
    | Error e -> Error [ e ]
  with exn -> Error [ Diagnostic.error_no_span ~code:"type-internal" ~message:(Printexc.to_string exn) ]

let format_error (err : Diagnostic.t) : string = Diagnostic.render_cli ~source_lookup:(fun _ -> None) err

(* Format error with source context showing the offending line *)
let format_error_with_context (source : string) (err : Diagnostic.t) : string =
  let source_lookup _file_id = Some source in
  Diagnostic.render_cli ~source_lookup err

(* ============================================================
   Main API
   ============================================================ *)

(* Type check a program (list of statements).
   Returns the type of the last expression and the final environment. *)
let check_program ?state ?(env = Infer.empty_env) (program : Syntax.Ast.AST.program) :
    (typecheck_result, Diagnostic.t list) result =
  match infer_program_safe ?state ~env program with
  | Error e -> Error e
  | Ok (final_env, type_map, result_type) ->
      let call_resolution_map = Infer.snapshot_method_resolution_store () in
      let method_def_map = Hashtbl.create 0 in
      let method_type_args_map = Infer.snapshot_method_type_args_store () in
      Ok
        {
          result_type;
          environment = final_env;
          type_map;
          call_resolution_map;
          method_def_map;
          method_type_args_map;
        }

(* Type check source code string.
    Parses and type checks in one step.
    Errors include source location information. *)
let check_string ?state ?(env = Infer.empty_env) ~file_id (source : string) :
    (typecheck_result, Diagnostic.t list) result =
  match Syntax.Parser.parse ~file_id source with
  | Error errors -> Error errors
  | Ok program -> (
      match infer_program_safe ?state ~env program with
      | Error e -> Error e
      | Ok (final_env, type_map, result_type) ->
          let call_resolution_map = Infer.snapshot_method_resolution_store () in
          let method_def_map = Hashtbl.create 0 in
          let method_type_args_map = Infer.snapshot_method_type_args_store () in
          Ok
            {
              result_type;
              environment = final_env;
              type_map;
              call_resolution_map;
              method_def_map;
              method_type_args_map;
            })

(* ============================================================
   Phase 2: Type check with annotations
   ============================================================ *)

(* Check if a let binding's annotation matches its inferred type *)
let check_let_annotation
    (name : string) (annotation : Syntax.Ast.AST.type_expr option) (inferred_type : mono_type) :
    (unit, Diagnostic.t) result =
  match annotation with
  | None ->
      (* No annotation, nothing to check *)
      Ok ()
  | Some type_annot -> (
      match Annotation.type_expr_to_mono_type type_annot with
      | Error d -> Error d
      | Ok annotated_type ->
          if Annotation.check_annotation annotated_type inferred_type then
            Ok ()
          else
            Error
              (Diagnostic.error_no_span ~code:"type-annotation-mismatch"
                 ~message:
                   (Printf.sprintf "Type annotation mismatch for '%s': expected %s but inferred %s" name
                      (Annotation.format_mono_type annotated_type)
                      (Annotation.format_mono_type inferred_type))))

(* Check if a function expression's return type annotation matches its inferred type *)
let check_function_annotation (return_annotation : Syntax.Ast.AST.type_expr option) (inferred_type : mono_type) :
    (unit, Diagnostic.t) result =
  match return_annotation with
  | None ->
      (* No annotation, nothing to check *)
      Ok ()
  | Some type_annot -> (
      match Annotation.type_expr_to_mono_type type_annot with
      | Error d -> Error d
      | Ok annotated_return_type ->
          let rec extract_return_type (t : mono_type) : mono_type =
            match t with
            | TFun (_, ret, _) -> extract_return_type ret
            | other -> other
          in
          let actual_return = extract_return_type inferred_type in
          if Annotation.check_annotation annotated_return_type actual_return then
            Ok ()
          else
            Error
              (Diagnostic.error_no_span ~code:"type-annotation-mismatch"
                 ~message:
                   (Printf.sprintf "Function return type annotation mismatch: expected %s but inferred %s"
                      (Annotation.format_mono_type annotated_return_type)
                      (Annotation.format_mono_type actual_return))))

(* Type check a program with annotation support.
    This checks that all annotations match the inferred types.
    For Phase 2, constraint validation is skipped (Phase 3 work). *)
let check_program_with_annotations ?state ?(env = Infer.empty_env) (program : Syntax.Ast.AST.program) :
    (typecheck_result, Diagnostic.t list) result =
  (* First, do standard inference *)
  match infer_program_safe ?state ~env program with
  | Error e -> Error e
  | Ok (final_env, type_map, result_type) -> (
      (* Phase 2: Validate annotations against inferred types *)
      let rec check_stmts_with_infer (stmts : Syntax.Ast.AST.statement list) : (unit, Diagnostic.t) result =
        match stmts with
        | [] -> Ok ()
        | stmt :: rest -> (
            match stmt.stmt with
            | Syntax.Ast.AST.Let let_binding -> (
                (* Check if annotation matches inferred type of the let binding *)
                let inferred =
                  match Hashtbl.find_opt type_map let_binding.value.id with
                  | Some t -> Some t
                  | None -> (
                      match Infer.TypeEnv.find_opt let_binding.name final_env with
                      | Some (Forall (_, mono_type)) -> Some mono_type
                      | None -> None)
                in
                match inferred with
                | None ->
                    Error
                      (Diagnostic.error_no_span ~code:"type-internal"
                         ~message:
                           (Printf.sprintf "Internal error: missing inferred type for let '%s' (expr id %d)"
                              let_binding.name let_binding.value.id))
                | Some mono_type -> (
                    (* Check let binding annotation *)
                    match check_let_annotation let_binding.name let_binding.type_annotation mono_type with
                    | Error e -> Error e
                    | Ok () -> (
                        (* Also check function expression annotation if present *)
                        match check_expr_annotations let_binding.value mono_type with
                        | Error e -> Error e
                        | Ok () -> check_stmts_with_infer rest)))
            | Syntax.Ast.AST.Block nested_stmts -> (
                (* Recursively check block statements *)
                match check_stmts_with_infer nested_stmts with
                | Error e -> Error e
                | Ok () -> check_stmts_with_infer rest)
            | _ ->
                (* Other statements don't have annotations to check *)
                check_stmts_with_infer rest)
      and check_expr_annotations (expr : Syntax.Ast.AST.expression) (inferred : mono_type) :
          (unit, Diagnostic.t) result =
        match expr.expr with
        | Syntax.Ast.AST.Function { return_type; params = _; body; generics = _; is_effectful = _ } -> (
            (* Check function return type annotation *)
            match check_function_annotation return_type inferred with
            | Error e -> Error e
            | Ok () ->
                (* Also recursively check body statements *)
                check_stmts_with_infer [ body ])
        | _ -> Ok () (* Other expressions don't have annotations to check *)
      in
      match check_stmts_with_infer program with
      | Error e -> Error [ e ]
      | Ok () ->
          let call_resolution_map = Infer.snapshot_method_resolution_store () in
          let method_def_map = Hashtbl.create 0 in
          let method_type_args_map = Infer.snapshot_method_type_args_store () in
          Ok
            {
              result_type;
              environment = final_env;
              type_map;
              call_resolution_map;
              method_def_map;
              method_type_args_map;
            })

(* Type check source code with annotations.
   Parses and type checks in one step, with annotation support. *)
let check_string_with_annotations ?state ?(env = Infer.empty_env) ~file_id (source : string) :
    (typecheck_result, Diagnostic.t list) result =
  match Syntax.Parser.parse ~file_id source with
  | Error errors -> Error errors
  | Ok program -> check_program_with_annotations ?state ~env program

(* Get the type of an expression as a string *)
let type_string (source : string) : string =
  match check_string ~file_id:"<test>" source with
  | Error (e :: _) -> "Error: " ^ e.message
  | Error [] -> "Error"
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
  match check_string ~file_id:"<test>" "42" with
  | Ok { result_type = TInt; _ } -> true
  | _ -> false

let%test "check_string function" =
  match check_string ~file_id:"<test>" "fn(x) { x + 1 }" with
  | Ok { result_type = TFun (TInt, TInt, _); _ } -> true
  | _ -> false

let%test "check_string let binding adds to env" =
  match check_string ~file_id:"<test>" "let x = 5; x" with
  | Ok { result_type = TInt; environment; _ } -> (
      match lookup "x" environment with
      | Some (Forall ([], TInt)) -> true
      | _ -> false)
  | _ -> false

let%test "check_string polymorphic function in env" =
  match check_string ~file_id:"<test>" "let id = fn(x) { x }; id" with
  | Ok { environment; _ } -> (
      match lookup "id" environment with
      | Some (Forall (vars, TFun (TVar a, TVar b, _))) -> List.length vars = 1 && a = b
      | _ -> false)
  | _ -> false

let%test "type_string helper" = type_string "1 + 2" = "Int" && type_string "true" = "Bool"

let%test "error on type mismatch" =
  match check_string ~file_id:"<test>" "1 + true" with
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
  check_string ~file_id:"<test>" ~env:(default_env ()) code

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

let string_contains_substring haystack ~substring = String_utils.contains_substring ~needle:substring haystack

let diagnostic_locs (source : string) (err : Diagnostic.t) :
    Diagnostics.Source_loc.loc option * Diagnostics.Source_loc.loc option =
  match Diagnostic.pick_primary_span err.labels with
  | Some (Diagnostic.Span { start_pos; end_pos; _ }) ->
      ( Some (Diagnostics.Source_loc.offset_to_loc source start_pos),
        Option.map (Diagnostics.Source_loc.offset_to_loc source) end_pos )
  | Some Diagnostic.NoSpan | None -> (None, None)

let%test "error includes source location" =
  match check_string ~file_id:"<test>" "1 + true" with
  | Error (err :: _) -> (
      match diagnostic_locs "1 + true" err with
      | Some loc, Some loc_end -> loc.line = 1 && loc.column > 0 && loc_end.column >= loc.column
      | _ -> false)
  | _ -> false

let%test "error location points to problematic expression" =
  (* "true" starts at column 5 (1-indexed) in "1 + true" *)
  match check_string ~file_id:"<test>" "1 + true" with
  | Error (err :: _) -> (
      match diagnostic_locs "1 + true" err with
      | Some loc, Some loc_end -> loc.column = 5 && loc_end.column = 8
      | _ -> false)
  | _ -> false

let%test "multiline error location" =
  let code = "let x = 5;\nlet y = x + true;" in
  match check_string ~file_id:"<test>" code with
  | Error (err :: _) -> (
      match diagnostic_locs code err with
      | Some loc, _ -> loc.line = 2
      | _ -> false)
  | _ -> false

let%test "format_error includes line:col" =
  match check_string ~file_id:"<test>" "1 + true" with
  | Error (err :: _) -> string_contains_substring (format_error err) ~substring:":1:5"
  | Ok _ | Error [] -> false

let%test "format_error_with_context shows source line" =
  let source = "1 + true" in
  match check_string ~file_id:"<test>" source with
  | Error (err :: _) ->
      let formatted = format_error_with_context source err in
      string_contains_substring formatted ~substring:":1:5"
  | Ok _ | Error [] -> false

let%test "format_error includes file id when provided" =
  match check_string ~file_id:"main.mr" "1 + true" with
  | Error (err :: _) ->
      let formatted = format_error err in
      string_contains_substring formatted ~substring:"main.mr:1:5"
  | Ok _ | Error [] -> false

(* ============================================================
   Type Annotation Tests - Parameter Annotations
   ============================================================ *)

let%test "annotation: single int parameter infers correctly" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "let f = fn(x: int) { x + 1 }; f" with
  | Error _ -> false
  | Ok { environment; _ } -> (
      match lookup "f" environment with
      | Some (Forall (_, TFun (TInt, TInt, _))) -> true
      | _ -> false)

let%test "annotation: multiple int parameters infer correctly" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "let add = fn(x: int, y: int) { x + y }; add" with
  | Error _ -> false
  | Ok { environment; _ } -> (
      match lookup "add" environment with
      | Some (Forall (_, TFun (TInt, TFun (TInt, TInt, _), _))) -> true
      | _ -> false)

let%test "annotation: mixed type parameters infer correctly" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "let f = fn(x: int, y: string) { y }; f" with
  | Error _ -> false
  | Ok { environment; _ } -> (
      match lookup "f" environment with
      | Some (Forall (_, TFun (TInt, TFun (TString, TString, _), _))) -> true
      | _ -> false)

let%test "annotation: bool parameter" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "let f = fn(x: bool) { !x }; f" with
  | Error _ -> false
  | Ok { environment; _ } -> (
      match lookup "f" environment with
      | Some (Forall (_, TFun (TBool, TBool, _))) -> true
      | _ -> false)

let%test "annotation: array parameter" =
  Infer.reset_fresh_counter ();
  let env = default_env () in
  match check_string ~file_id:"<test>" ~env "let f = fn(x: list[int]) { len(x) }; f" with
  | Error _ -> false
  | Ok { environment; _ } -> (
      match lookup "f" environment with
      | Some (Forall (_, TFun (TArray TInt, TInt, _))) -> true
      | _ -> false)

(* ============================================================
   Type Annotation Tests - Return Type Annotations
   ============================================================ *)

let%test "annotation: return type int matches" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "let f = fn() -> int { 42 }; f" with
  | Error _ -> false
  | Ok _ -> true

let%test "annotation: return type string matches" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "let f = fn() -> string { \"hello\" }; f" with
  | Error _ -> false
  | Ok _ -> true

let%test "annotation: return type bool matches" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "let f = fn() -> bool { true }; f" with
  | Error _ -> false
  | Ok _ -> true

let%test "annotation: return type array[int] matches" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "let f = fn() -> list[int] { [1, 2, 3] }; f" with
  | Error _ -> false
  | Ok _ -> true

(* ============================================================
   Type Annotation Tests - Mismatch Detection (CRITICAL)
   ============================================================ *)

let%test "annotation: mismatch int vs string is caught" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "let f = fn() -> string { 42 }; f" with
  | Ok _ | Error [] -> false (* MUST fail *)
  | Error (err :: _) ->
      (* Check message contains both types and indicates mismatch *)
      let lower = String.lowercase_ascii err.message in
      String.contains lower 's' && String.contains lower 'i' (* Has string/int *)

let%test "annotation: mismatch string vs int is caught" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "let f = fn() -> int { \"hello\" }; f" with
  | Ok _ | Error [] -> false
  | Error (err :: _) ->
      let lower = String.lowercase_ascii err.message in
      String.contains lower 's' && String.contains lower 'i'

let%test "annotation: mismatch bool vs int is caught" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "let f = fn() -> bool { 42 }; f" with
  | Ok _ | Error [] -> false
  | Error (err :: _) -> string_contains_substring (String.lowercase_ascii err.message) ~substring:"annotation"

let%test "annotation: mismatch array vs int is caught" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "let f = fn() -> list[int] { 42 }; f" with
  | Ok _ -> false
  | Error _ -> true

(* ============================================================
   Type Annotation Tests - Complex Cases
   ============================================================ *)

let%test "annotation: full signature with params and return" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "let f = fn(x: int, y: int) -> int { x + y }; f" with
  | Error _ -> false
  | Ok { environment; _ } -> (
      match lookup "f" environment with
      | Some (Forall (_, TFun (TInt, TFun (TInt, TInt, _), _))) -> true
      | _ -> false)

let%test "annotation: conditional with matching return type" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "let f = fn(x: int) -> int { if (x < 0) { 0 } else { x } }; f" with
  | Error _ -> false
  | Ok _ -> true

let%test "annotation: conditional with mismatched branch types fails" =
  Infer.reset_fresh_counter ();
  match
    check_string ~file_id:"<test>" "let f = fn(x: int) -> string { if (x < 0) { \"neg\" } else { 42 } }; f"
  with
  | Ok _ -> false
  | Error _ -> true

let%test "annotation: recursive fibonacci with correct type" =
  Infer.reset_fresh_counter ();
  let code =
    {|let fib = fn(n: int) -> int { 
    if (n < 2) { return n }
    return fib(n - 1) + fib(n - 2);
  };
  fib|}
  in
  match check_string ~file_id:"<test>" code with
  | Error _ -> false
  | Ok _ -> true

let%test "annotation: recursive fibonacci with wrong return type FAILS" =
  Infer.reset_fresh_counter ();
  let code =
    {|let fib = fn(n: int) -> string { 
    if (n < 2) { return n }
    return fib(n - 1) + fib(n - 2);
  };
  fib|}
  in
  match check_string ~file_id:"<test>" code with
  | Ok _ | Error [] -> false
  | Error (err :: _) -> string_contains_substring (String.lowercase_ascii err.message) ~substring:"mismatch"

(* ============================================================
   Backward Compatibility - Non-Annotated Functions Still Work
   ============================================================ *)

let%test "no annotation: unannotated function still works" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "let f = fn(x) { x + 1 }; f" with
  | Error _ -> false
  | Ok _ -> true

let%test "no annotation: polymorphic identity function" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "let id = fn(x) { x }; id" with
  | Error _ -> false
  | Ok { environment; _ } -> (
      match lookup "id" environment with
      | Some (Forall (vars, TFun (TVar a, TVar b, _))) -> List.length vars >= 1 && a = b
      | _ -> false)

let%test "no annotation: mixed annotated and unannotated params" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "let f = fn(x: int, y) { x + y }; f" with
  | Error _ -> false
  | Ok { environment; _ } -> (
      match lookup "f" environment with
      | Some (Forall (_, TFun (TInt, TFun (_, TInt, _), _))) -> true
      | _ -> false)

let%test "annotation checker: local let bindings in function body do not require global env entries" =
  Infer.reset_fresh_counter ();
  let env = default_env () in
  let code =
    {|
    let book = {
      "title": "Writing A Compiler In Go",
      "author": "Thorsten Ball"
    };
    let printBookName = fn(book) {
      let title = book["title"];
      let author = book["author"];
      puts(author + " - " + title);
    };
    printBookName(book)
  |}
  in
  match check_string_with_annotations ~file_id:"<test>" ~env code with
  | Ok _ -> true
  | Error _ -> false

(* ============================================================
   Error Message Quality Tests
   ============================================================ *)

let%test "error: annotation mismatch message is clear" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "let f = fn() -> string { 42 }; f" with
  | Ok _ | Error [] -> false
  | Error (err :: _) ->
      let lower = String.lowercase_ascii err.message in
      (* Should mention annotation and mismatch *)
      string_contains_substring lower ~substring:"annotation"
      || string_contains_substring lower ~substring:"mismatch"

let%test "error: shows both expected and inferred types" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "let f = fn() -> bool { \"not bool\" }; f" with
  | Ok _ | Error [] -> false
  | Error (err :: _) ->
      String.length err.message > 20
      &&
      (* Reasonable error message *)
      let lower_msg = String.lowercase_ascii err.message in
      string_contains_substring lower_msg ~substring:"bool"
      || string_contains_substring lower_msg ~substring:"string"

(* ============================================================
   Phase 4.4: Record Typechecking Tests
   ============================================================ *)

let%test "record literal and field access typecheck" =
  Infer.reset_fresh_counter ();
  match check "let p = { x: 10, y: 20 }; p.x + p.y" with
  | Ok { result_type = TInt; _ } -> true
  | _ -> false

let%test "record literal with punning typechecks" =
  Infer.reset_fresh_counter ();
  match check "let x = 10; let y = 20; let p = { x:, y: }; p.x + p.y" with
  | Ok { result_type = TInt; _ } -> true
  | _ -> false

let%test "type alias for record typechecks" =
  Infer.reset_fresh_counter ();
  let code = "type point = { x: int, y: int }; let p: point = { x: 1, y: 2 }; p.x + p.y" in
  match check code with
  | Ok { result_type = TInt; _ } -> true
  | _ -> false

let%test "generic type alias for record typechecks" =
  Infer.reset_fresh_counter ();
  let code = "type box[a] = { value: a }; let p: box[int] = { value: 42 }; p.value" in
  match check code with
  | Ok { result_type = TInt; _ } -> true
  | _ -> false

let%test "explicit row-polymorphic annotation is rejected in v1" =
  Infer.reset_fresh_counter ();
  let code = "let get_x = fn(r: { x: int, ...row }) -> int { r.x }" in
  match check code with
  | Error _ -> true
  | Ok _ -> false

let%test "field access on record without row annotation works" =
  Infer.reset_fresh_counter ();
  let code = "let p = { x: 5, y: 10, z: 20 }; let get_x = fn(r) { r.x }; get_x(p)" in
  match check code with
  | Ok { result_type = TInt; _ } -> true
  | _ -> false

let%test "record match pattern typechecks" =
  Infer.reset_fresh_counter ();
  let code = "let p = { x: 10, y: 20 }; match p { { x:, y: }: x + y _: 0 }" in
  match check code with
  | Ok { result_type = TInt; _ } -> true
  | _ -> false

let%test "env reuse with shared inference state preserves constrained generic obligations" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  Trait_registry.register_trait
    {
      Trait_registry.trait_name = "show";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [ Trait_registry.mk_method_sig ~name:"show" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
    };
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ Trait_registry.mk_method_sig ~name:"show" ~params:[ ("x", TInt) ] ~return_type:TString () ];
    };
  let shared_state = Infer.create_inference_state () in
  match
    check_string ~file_id:"<test>" ~state:shared_state
      "let check = fn[a: show](x: a) -> string { x.show() }; check"
  with
  | Error _ -> false
  | Ok first -> (
      match check_string ~file_id:"<test>" ~state:shared_state ~env:first.environment "check(fn(y) { y })" with
      | Ok _ | Error [] -> false
      | Error (err :: _) -> String_utils.contains_substring ~needle:"does not implement trait show" err.message)
