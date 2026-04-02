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
  trait_object_coercion_map : (int, Resolution_artifacts.trait_object_coercion) Hashtbl.t;
      (* Track B B1: recorded trait-object packaging sites keyed by source expression id *)
  placeholder_rewrite_map : Infer.placeholder_rewrite_map;
      (* Placeholder shorthand rewrites keyed by original expression id *)
  diagnostics : Diagnostic.t list; (* Diagnostics emitted during a successful check *)
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

let snapshot_diagnostics () : Diagnostic.t list = Infer.snapshot_diagnostics ()

let merge_diagnostics (fatal_diags : Diagnostic.t list) : Diagnostic.t list =
  match snapshot_diagnostics () with
  | [] -> fatal_diags
  | diagnostics -> diagnostics @ fatal_diags

let make_typecheck_result ~(result_type : mono_type) ~(environment : Infer.type_env) ~(type_map : Infer.type_map)
    : typecheck_result =
  let call_resolution_map = Infer.snapshot_method_resolution_store () in
  let method_def_map = Infer.snapshot_method_def_store () in
  let method_type_args_map = Infer.snapshot_method_type_args_store () in
  let trait_object_coercion_map = Infer.snapshot_trait_object_coercion_store () in
  let placeholder_rewrite_map = Infer.snapshot_placeholder_rewrite_store () in
  let diagnostics = snapshot_diagnostics () in
  {
    result_type;
    environment;
    type_map;
    call_resolution_map;
    method_def_map;
    method_type_args_map;
    trait_object_coercion_map;
    placeholder_rewrite_map;
    diagnostics;
  }

(* ============================================================
   Main API
   ============================================================ *)

(* Type check a program (list of statements).
   Returns the type of the last expression and the final environment. *)
let check_program ?state ?(env = Infer.empty_env) (program : Syntax.Ast.AST.program) :
    (typecheck_result, Diagnostic.t list) result =
  match infer_program_safe ?state ~env program with
  | Error e -> Error (merge_diagnostics e)
  | Ok (final_env, type_map, result_type) ->
      Ok (make_typecheck_result ~result_type ~environment:final_env ~type_map)

(* Type check source code string.
    Parses and type checks in one step.
    Errors include source location information. *)
let check_string ?state ?(env = Infer.empty_env) ~file_id (source : string) :
    (typecheck_result, Diagnostic.t list) result =
  match Syntax.Parser.parse ~file_id source with
  | Error errors -> Error errors
  | Ok program -> (
      match infer_program_safe ?state ~env program with
      | Error e -> Error (merge_diagnostics e)
      | Ok (final_env, type_map, result_type) ->
          Ok (make_typecheck_result ~result_type ~environment:final_env ~type_map))

(* ============================================================
   Phase 2: Type check with annotations
   ============================================================ *)

let annotation_matches_inferred_type (annotated_type : mono_type) (inferred_type : mono_type) : bool =
  if Annotation.check_annotation annotated_type inferred_type then
    true
  else if
    Infer.mono_type_contains_intersection annotated_type || Infer.mono_type_contains_intersection inferred_type
  then
    match Unify.unify inferred_type annotated_type with
    | Ok _ -> true
    | Error _ -> false
  else
    false

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
          if annotation_matches_inferred_type annotated_type inferred_type then
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
          if annotation_matches_inferred_type annotated_return_type actual_return then
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
  | Error e -> Error (merge_diagnostics e)
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
        | Syntax.Ast.AST.Function { return_type; params = _; body; generics; is_effectful = _; _ } -> (
            (* For generic functions, skip the return annotation check: type_callable already
               validated it during inference with proper type variable bindings.
               The second-pass check here can't reproduce the fresh-var mapping. *)
            let has_generics =
              match generics with
              | Some (_ :: _) -> true
              | _ -> false
            in
            let annot_check =
              if has_generics then
                Ok ()
              else
                check_function_annotation return_type inferred
            in
            match annot_check with
            | Error e -> Error e
            | Ok () ->
                (* Also recursively check body statements *)
                check_stmts_with_infer [ body ])
        | _ -> Ok () (* Other expressions don't have annotations to check *)
      in
      match check_stmts_with_infer program with
      | Error e -> Error (merge_diagnostics [ e ])
      | Ok () -> Ok (make_typecheck_result ~result_type ~environment:final_env ~type_map))

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
  match check_string ~file_id:"<test>" "(x) -> x + 1" with
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
  match check_string ~file_id:"<test>" "fn id(x) = x\nid" with
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
  match check_string ~file_id:"<test>" "fn f(x: Int) = x + 1\nf" with
  | Error _ -> false
  | Ok { environment; _ } -> (
      match lookup "f" environment with
      | Some (Forall (_, TFun (TInt, TInt, _))) -> true
      | _ -> false)

let%test "annotation: multiple int parameters infer correctly" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "fn add(x: Int, y: Int) = x + y\nadd" with
  | Error _ -> false
  | Ok { environment; _ } -> (
      match lookup "add" environment with
      | Some (Forall (_, TFun (TInt, TFun (TInt, TInt, _), _))) -> true
      | _ -> false)

let%test "annotation: mixed type parameters infer correctly" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "fn f(x: Int, y: Str) = y\nf" with
  | Error _ -> false
  | Ok { environment; _ } -> (
      match lookup "f" environment with
      | Some (Forall (_, TFun (TInt, TFun (TString, TString, _), _))) -> true
      | _ -> false)

let%test "annotation: bool parameter" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "fn f(x: Bool) = !x\nf" with
  | Error _ -> false
  | Ok { environment; _ } -> (
      match lookup "f" environment with
      | Some (Forall (_, TFun (TBool, TBool, _))) -> true
      | _ -> false)

let%test "annotation: array parameter" =
  Infer.reset_fresh_counter ();
  let env = default_env () in
  match check_string ~file_id:"<test>" ~env "fn f(x: List[Int]) = len(x)\nf" with
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
  match check_string ~file_id:"<test>" "fn f() -> Int = 42\nf" with
  | Error _ -> false
  | Ok _ -> true

let%test "annotation: return type string matches" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "fn f() -> Str = \"hello\"\nf" with
  | Error _ -> false
  | Ok _ -> true

let%test "annotation: return type bool matches" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "fn f() -> Bool = true\nf" with
  | Error _ -> false
  | Ok _ -> true

let%test "annotation: return type array[int] matches" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "fn f() -> List[Int] = [1, 2, 3]\nf" with
  | Error _ -> false
  | Ok _ -> true

(* ============================================================
   Type Annotation Tests - Mismatch Detection (CRITICAL)
   ============================================================ *)

let%test "annotation: mismatch int vs string is caught" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "fn f() -> Str = 42\nf" with
  | Ok _ | Error [] -> false (* MUST fail *)
  | Error (err :: _) ->
      (* Check message contains both types and indicates mismatch *)
      let lower = String.lowercase_ascii err.message in
      String.contains lower 's' && String.contains lower 'i' (* Has string/int *)

let%test "annotation: mismatch string vs int is caught" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "fn f() -> Int = \"hello\"\nf" with
  | Ok _ | Error [] -> false
  | Error (err :: _) ->
      let lower = String.lowercase_ascii err.message in
      String.contains lower 's' && String.contains lower 'i'

let%test "annotation: mismatch bool vs int is caught" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "fn f() -> Bool = 42\nf" with
  | Ok _ | Error [] -> false
  | Error (err :: _) -> string_contains_substring (String.lowercase_ascii err.message) ~substring:"annotation"

let%test "annotation: mismatch array vs int is caught" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "fn f() -> List[Int] = 42\nf" with
  | Ok _ -> false
  | Error _ -> true

(* ============================================================
   Type Annotation Tests - Complex Cases
   ============================================================ *)

let%test "annotation: full signature with params and return" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "fn f(x: Int, y: Int) -> Int = x + y\nf" with
  | Error _ -> false
  | Ok { environment; _ } -> (
      match lookup "f" environment with
      | Some (Forall (_, TFun (TInt, TFun (TInt, TInt, _), _))) -> true
      | _ -> false)

let%test "annotation: conditional with matching return type" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "fn f(x: Int) -> Int = if (x < 0) { 0 } else { x }\nf" with
  | Error _ -> false
  | Ok _ -> true

let%test "annotation: conditional with mismatched branch types fails" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "fn f(x: Int) -> Str = if (x < 0) { \"neg\" } else { 42 }\nf" with
  | Ok _ -> false
  | Error _ -> true

let%test "annotation: recursive fibonacci with correct type" =
  Infer.reset_fresh_counter ();
  let code =
    {|fn fib(n: Int) -> Int = {
    if (n < 2) { return n }
    return fib(n - 1) + fib(n - 2);
  }
  fib|}
  in
  match check_string ~file_id:"<test>" code with
  | Error _ -> false
  | Ok _ -> true

let%test "annotation: recursive fibonacci with wrong return type FAILS" =
  Infer.reset_fresh_counter ();
  let code =
    {|fn fib(n: Int) -> Str = {
    if (n < 2) { return n }
    return fib(n - 1) + fib(n - 2);
  }
  fib|}
  in
  match check_string ~file_id:"<test>" code with
  | Ok _ | Error [] -> false
  | Error (err :: _) -> string_contains_substring (String.lowercase_ascii err.message) ~substring:"mismatch"

(* ============================================================
   Unannotated Functions Still Work
   ============================================================ *)

let%test "no annotation: unannotated function still works" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "fn f(x) = x + 1\nf" with
  | Error _ -> false
  | Ok _ -> true

let%test "no annotation: polymorphic identity function" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "fn id(x) = x\nid" with
  | Error _ -> false
  | Ok { environment; _ } -> (
      match lookup "id" environment with
      | Some (Forall (vars, TFun (TVar a, TVar b, _))) -> List.length vars >= 1 && a = b
      | _ -> false)

let%test "no annotation: mixed annotated and unannotated params" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "fn f(x: Int, y) = x + y\nf" with
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
    }
    fn printBookName(book) = {
      let title = book["title"];
      let author = book["author"];
      puts(author + " - " + title);
    }
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
  match check_string ~file_id:"<test>" "fn f() -> Str = 42\nf" with
  | Ok _ | Error [] -> false
  | Error (err :: _) ->
      let lower = String.lowercase_ascii err.message in
      (* Should mention annotation and mismatch *)
      string_contains_substring lower ~substring:"annotation"
      || string_contains_substring lower ~substring:"mismatch"

let%test "error: shows both expected and inferred types" =
  Infer.reset_fresh_counter ();
  match check_string ~file_id:"<test>" "fn f() -> Bool = \"not bool\"\nf" with
  | Ok _ | Error [] -> false
  | Error (err :: _) ->
      String.length err.message > 20
      &&
      (* Reasonable error message *)
      let lower_msg = String.lowercase_ascii err.message in
      string_contains_substring lower_msg ~substring:"bool"
      || string_contains_substring lower_msg ~substring:"string"

let%test "override warning is surfaced on successful check as a diagnostic with span" =
  Infer.reset_fresh_counter ();
  let code =
    {|
      trait override_warn_success[a] = {
        fn greet(x: a) -> Str
      }
      impl override_warn_success[Int] = {
        override fn greet(x: Int) -> Str = "hi"
      }
    |}
  in
  match check_string ~file_id:"main.mr" code with
  | Error _ -> false
  | Ok result -> (
      match
        List.find_opt (fun (diag : Diagnostic.t) -> diag.code = "override-unnecessary") result.diagnostics
      with
      | None -> false
      | Some diag -> diag.severity = Diagnostic.Warning && Diagnostic.pick_primary_span diag.labels <> None)

let%test "override warning survives alongside later hard errors in the same diagnostics stream" =
  Infer.reset_fresh_counter ();
  let code =
    {|
      trait override_warn_error[a] = {
        fn greet(x: a) -> Str
      }
      impl override_warn_error[Int] = {
        override fn greet(x: Int) -> Str = "hi"
      }
      1 + true
    |}
  in
  match check_string ~file_id:"main.mr" code with
  | Ok _ -> false
  | Error diags ->
      List.exists
        (fun (diag : Diagnostic.t) -> diag.code = "override-unnecessary" && diag.severity = Diagnostic.Warning)
        diags
      && List.exists
           (fun (diag : Diagnostic.t) -> diag.code = "type-mismatch" && diag.severity = Diagnostic.Error)
           diags

let%test "invalid bare trait let annotation is not masked by later body errors" =
  Infer.reset_fresh_counter ();
  let state = Infer.create_inference_state () in
  let code =
    {|
      trait Named[a] = {
        fn show(x: a) -> Str
      }
      let x: Named = 1
      Named.show(x)
    |}
  in
  match check_string ~state ~file_id:"main.mr" code with
  | Ok _ -> false
  | Error diags -> List.exists (fun (diag : Diagnostic.t) -> diag.code = "type-annotation-invalid") diags

let%test "undefined trait impl reports undefined trait instead of unknown type constructor" =
  Infer.reset_fresh_counter ();
  let state = Infer.create_inference_state () in
  let code =
    {|
      impl NonexistentTrait[Int] = {
        fn do_thing(x: Int) -> Int = x
      }
      1
    |}
  in
  match check_string ~state ~file_id:"main.mr" code with
  | Ok _ -> false
  | Error diags ->
      List.exists
        (fun (diag : Diagnostic.t) ->
          String_utils.contains_substring ~needle:"Cannot implement undefined trait" diag.message)
        diags

let%test "effectful function types in enum payloads preserve their effect bit" =
  Infer.reset_fresh_counter ();
  let state = Infer.create_inference_state () in
  let code =
    {|
      enum Runner = {
        Run((Int) => Int)
      }
      fn invoke(r: Runner) => Int = match r {
        case Runner.Run(f): f(1)
      }
      let runner = Runner.Run((n: Int) => n)
      invoke(runner)
    |}
  in
  match check_string ~state ~file_id:"main.mr" code with
  | Ok _ -> true
  | Error _ -> false

let%test "effectful function types in shape fields preserve their effect bit" =
  Infer.reset_fresh_counter ();
  let state = Infer.create_inference_state () in
  let code =
    {|
      shape Processor = {
        run: (Int) => Int
      }
      fn invoke(x: Processor) => Int = x.run(1)
      let runner = { run: (n: Int) => n }
      invoke(runner)
    |}
  in
  match check_string ~state ~file_id:"main.mr" code with
  | Ok _ -> true
  | Error _ -> false

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

let%test "transparent type for record typechecks" =
  Infer.reset_fresh_counter ();
  let code = "type Point = { x: Int, y: Int }\nlet p: Point = { x: 1, y: 2 }\np.x + p.y" in
  match check code with
  | Ok { result_type = TInt; _ } -> true
  | _ -> false

let%test "generic transparent type for record typechecks" =
  Infer.reset_fresh_counter ();
  let code = "type Box[a] = { value: a }\nlet p: Box[Int] = { value: 42 }\np.value" in
  match check code with
  | Ok { result_type = TInt; _ } -> true
  | _ -> false

let%test "transparent type may participate in explicit trait-qualified calls" =
  Infer.reset_fresh_counter ();
  let code =
    {|
      type Point = { x: Int, y: Int }
      trait Show[a] = {
        fn show(x: a) -> Str
      }
      impl Show[Point] = {
        fn show(p: Point) -> Str = "point"
      }
      let p: Point = { x: 1, y: 2 }
      Show.show(p)
    |}
  in
  match check code with
  | Ok { result_type = TString; _ } -> true
  | Ok _ -> false
  | Error _ -> false

let%test "transparent type may participate in explicit exact-type calls" =
  Infer.reset_fresh_counter ();
  let code =
    {|
      type Point = { x: Int, y: Int }
      impl Point = {
        fn sum(self: Point) -> Int = self.x + self.y
      }
      let p: Point = { x: 1, y: 2 }
      Point.sum(p)
    |}
  in
  match check code with
  | Ok { result_type = TInt; _ } -> true
  | Ok _ -> false
  | Error _ -> false

let%test "field-path union narrowing supports direct projection checks" =
  Infer.reset_fresh_counter ();
  let code =
    {|
      type Box = { value: Int | Str }
      fn render(box: Box) -> Str = {
        if (box.value is Int) {
          Show.show(box.value)
        } else {
          box.value
        }
      }
      render({ value: 1 })
    |}
  in
  match check code with
  | Ok { result_type = Types.TString; _ } -> true
  | _ -> false

let%test "match record patterns narrow union scrutinees in arm bodies" =
  Infer.reset_fresh_counter ();
  let code =
    {|
      type Left = { x: Int }
      type Right = { y: Int }
      fn read(v: Left | Right) -> Int = {
        match v {
          case { x: }: v.x
          case { y: }: v.y
          case _: 0
        }
      }
      read({ x: 1 })
    |}
  in
  match check code with
  | Ok { result_type = Types.TInt; _ } -> true
  | _ -> false

let%test "explicit row-polymorphic annotation is rejected in v1" =
  Infer.reset_fresh_counter ();
  let code = "fn get_x(r: { x: Int, ...row }) -> Int = r.x" in
  match check code with
  | Error _ -> true
  | Ok _ -> false

let%test "field access on record without row annotation works" =
  Infer.reset_fresh_counter ();
  let code = "let p = { x: 5, y: 10, z: 20 }\nfn get_x(r) = r.x\nget_x(p)" in
  match check code with
  | Ok { result_type = TInt; _ } -> true
  | _ -> false

let%test "record match pattern typechecks" =
  Infer.reset_fresh_counter ();
  let code = "let p = { x: 10, y: 20 }\nmatch p { case { x:, y: }: x + y case _: 0 }" in
  match check code with
  | Ok { result_type = TInt; _ } -> true
  | _ -> false

let%test "env reuse with shared inference state preserves constrained generic obligations" =
  Infer.reset_fresh_counter ();
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
      impl_methods = [ Trait_registry.mk_method_sig ~name:"show" ~params:[ ("x", TInt) ] ~return_type:TString () ];
    };
  let shared_state = Infer.create_inference_state () in
  match check_string ~file_id:"<test>" ~state:shared_state "fn check[a: Show](x: a) -> Str = Show.show(x)\ncheck" with
  | Error _ -> false
  | Ok first -> (
      match check_string ~file_id:"<test>" ~state:shared_state ~env:first.environment "check((y) -> y)" with
      | Ok _ | Error [] -> false
      | Error (err :: _) -> String_utils.contains_substring ~needle:"does not implement trait show" err.message)

(* Phase 3: vNext canonical function declarations work end-to-end *)
let%test "Phase3: vNext fn decl compiles and typechecks" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match check_string ~file_id:"<test>" "fn add(x: Int, y: Int) -> Int = x + y\nadd(1, 2)" with
  | Ok result -> result.result_type = Types.TInt
  | Error _ -> false

let%test "Phase3: vNext fn decl with block body typechecks" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match check_string ~file_id:"<test>" "fn double(x: Int) -> Int = { x + x }\ndouble(5)" with
  | Ok result -> result.result_type = Types.TInt
  | Error _ -> false

let%test "Phase6 prep: canonical builtin type names typecheck end-to-end" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match
    check_string ~file_id:"<test>"
      "type UserId = Int\nfn greet(name: Str) -> Str = name\nlet xs: List[Int] = [1, 2]\nlet counts: Map[Str, Int] = { \"hi\": 2 }\ngreet(\"ok\"); xs[0]"
  with
  | Ok result -> result.result_type = Types.TInt
  | Error _ -> false

let%test "Phase3: trait default method - impl without method succeeds if default exists" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  (* Register a trait with a default impl (simulating what the parser+lowerer would produce) *)
  let default_body = Syntax.Ast.AST.mk_expr (Syntax.Ast.AST.String "default") in
  Trait_registry.register_trait
    {
      Trait_registry.trait_name = "greetable";
      trait_type_param = None;
      trait_supertraits = [];
      trait_methods =
        [
          {
            Trait_registry.method_key = Resolution_artifacts.SyntheticCallable "greet";
            method_name = "greet";
            method_generics = [];
            method_params = [ ("self", Types.TInt) ];
            method_return_type = Types.TString;
            method_effect = `Pure;
            method_generic_internal_vars = [];
            method_default_impl = Some default_body;
          };
        ];
    };
  (* Register an impl that does NOT provide the method (uses default) *)
  Trait_registry.register_impl ~builtin:true
    { impl_trait_name = "greetable"; impl_type_params = []; impl_for_type = Types.TInt; impl_methods = [] };
  (* Should be able to call greet on int via explicit qualification *)
  match check_string ~file_id:"<test>" "greetable.greet(1)" with
  | Ok result -> result.result_type = Types.TString
  | Error _ -> false

let%test "followup A2: user default-backed derive expands into a callable impl" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match
    check_string ~file_id:"<test>"
      "trait Greeter[a] = { fn greet(self: a) -> Str = \"hello\" }\ntype Score = Int derive Greeter\nlet s: Score = 1\nGreeter.greet(s)"
  with
  | Ok result -> result.result_type = Types.TString
  | Error _ -> false

let%test "followup A2: user default-backed derive registers default-derived provenance" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match
    check_string ~file_id:"<test>"
      "trait Greeter[a] = { fn greet(self: a) -> Str = \"hello\" }\ntype Score = Int derive Greeter\nlet s: Score = 1\nGreeter.greet(s)"
  with
  | Error _ -> false
  | Ok _ -> Trait_registry.lookup_impl_origin "Greeter" Types.TInt = Some Trait_registry.DefaultDerivedImpl

let%test "Phase6 prep: canonical builtin trait names work in derives" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  let env = default_env () in
  match
    check_string ~env ~file_id:"<test>" "type Point = { x: Int } derive Eq\nlet p: Point = { x: 1 }\nEq.eq(p, p)"
  with
  | Ok result -> result.result_type = Types.TBool
  | Error _ -> false

let%test "Phase6 prep: canonical builtin trait names work in generic constraints" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  let env = default_env () in
  match check_string ~env ~file_id:"<test>" "fn same[a: Eq](x: a, y: a) -> Bool = Eq.eq(x, y)\nsame(1, 2)" with
  | Ok result -> result.result_type = Types.TBool
  | Error _ -> false

let%test "Phase6 prep: canonical builtin trait names work in impl headers" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  let env = default_env () in
  match
    check_string ~env ~file_id:"<test>" "impl Show[Int] = { fn show(self: Int) -> Str = \"int\" }\nShow.show(1)"
  with
  | Ok result -> result.result_type = Types.TString
  | Error _ -> false

let%test "Phase6 prep: transparent record fields may reference enum types" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  let env = default_env () in
  match
    check_string ~env ~file_id:"<test>"
      "enum Fruit = { Banana, Mango }\ntype Monkey = { favorite_fruit: Fruit }\nlet m: Monkey = { favorite_fruit: Fruit.Banana }\nm.favorite_fruit"
  with
  | Ok result -> result.result_type = Types.TEnum ("Fruit", [])
  | Error _ -> false

let%test "Phase6 prep: shape fields may reference enum types" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  let env = default_env () in
  match
    check_string ~env ~file_id:"<test>"
      "enum Fruit = { Banana, Mango }\nshape HasFruit = { favorite_fruit: Fruit }\nfn get[t: HasFruit](x: t) -> Fruit = x.favorite_fruit\nget({ favorite_fruit: Fruit.Banana })"
  with
  | Ok result -> result.result_type = Types.TEnum ("Fruit", [])
  | Error _ -> false

let%test "Phase6 prep: omitted impl binders are inferred from impl targets" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  let env = default_env () in
  match
    check_string ~env ~file_id:"<test>"
      "trait Show[a] = { fn show(a) -> Str }\nimpl Show[List[a]] = { fn show(self: List[a]) -> Str = \"\" }"
  with
  | Ok _ -> true
  | Error _ -> false

let%test "Phase6 prep: constrained-param shorthand works end-to-end for top-level fn decls" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match
    check_string ~file_id:"<test>"
      "trait JungleDweller[a] = { fn introduce(a) -> Str }\ntype Monkey = { name: Str }\nimpl JungleDweller[Monkey] = { fn introduce(self: Monkey) -> Str = self.name }\nfn who_dis(x: JungleDweller) -> Str = JungleDweller.introduce(x)\nlet george: Monkey = { name: \"George\" }\nwho_dis(george)"
  with
  | Ok result -> result.result_type = Types.TString
  | Error _ -> false

let%test "Phase6 prep: constrained-param shorthand works in typed arrow-lambda params" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match
    check_string ~file_id:"<test>"
      "shape Named = { name: Str }\nlet get_name = (x: Named) -> x.name\nget_name({ name: \"Ada\" })"
  with
  | Ok result -> result.result_type = Types.TString
  | Error _ -> false

let%test "Phase6 prep: constrained-param shorthand works in inherent method params" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match
    check_string ~file_id:"<test>"
      "shape Named = { name: Str }\ntype User = { name: Str }\nimpl User = {\n\  fn label(self: User, other: Named) -> Str = other.name\n}\nlet user: User = { name: \"Ada\" }\nUser.label(user, { name: \"Ada\" })"
  with
  | Ok result -> result.result_type = Types.TString
  | Error _ -> false

let%test "Phase6 prep: override is rejected in inherent impls" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match check_string ~file_id:"<test>" "impl Int = {\n\  override fn twice(x: Int) -> Int = x * 2\n}" with
  | Error diags -> List.exists (fun (d : Diagnostic.t) -> d.code = "override-invalid") diags
  | Ok _ -> false

let%test "Phase6 prep: placeholder shorthand works in call arguments" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match
    check_string ~file_id:"<test>"
      "fn map_list[a, b](items: List[a], f: (a) -> b) -> List[b] = []\nlet doubled = map_list([1, 2], _ * 2)\ndoubled"
  with
  | Ok result -> result.result_type = Types.TArray Types.TInt
  | Error _ -> false

let%test "Phase6 prep: placeholder shorthand works as a standalone section value" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match check_string ~file_id:"<test>" "let times_two = _ * 2\ntimes_two(21)" with
  | Ok result -> result.result_type = Types.TInt
  | Error _ -> false

let%test "Phase6 prep: standalone sections support top-level forward references" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match check_string ~file_id:"<test>" "let y = add1(41)\nlet add1 = _ + 1\ny" with
  | Ok result -> result.result_type = Types.TInt
  | Error _ -> false

let%test "Phase6 prep: placeholder shorthand supports qualified partial application" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match
    check_string ~env:(Builtins.prelude_env ()) ~file_id:"<test>"
      "type Post = { owner: Str }\ntrait Visible[a] = {\n  fn visible_to(x: a, viewer: Str) -> Bool\n}\nimpl Visible[Post] = {\n  fn visible_to(x: Post, viewer: Str) -> Bool = x.owner == viewer\n}\nlet visible_to_ada = Visible.visible_to(_, \"ada\")\nvisible_to_ada({ owner: \"ada\" })"
  with
  | Ok result -> result.result_type = Types.TBool
  | Error _ -> false

let%test "Phase6 prep: placeholder shorthand supports projection section values" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match check_string ~file_id:"<test>" "type Post = { updated_at: Int }\nlet updated_at = _.updated_at\nupdated_at({ updated_at: 7 })" with
  | Ok result -> result.result_type = Types.TInt
  | Error _ -> false

let%test "Phase6 prep: placeholder shorthand works when callback is invoked in the callee" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match
    check_string ~file_id:"<test>"
      "fn apply[a, b](x: a, f: (a) -> b) -> b = f(x)\nlet result = apply(21, _ * 2)\nresult"
  with
  | Ok result -> result.result_type = Types.TInt
  | Error _ -> false

let%test "Phase6 prep: placeholder shorthand works through annotation checking" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match
    Syntax.Parser.parse ~file_id:"<test>"
      "fn apply[a, b](x: a, f: (a) -> b) -> b = f(x)\nlet result = apply(21, _ * 2)\nresult"
  with
  | Error _ -> false
  | Ok program -> (
      match check_program_with_annotations ~env:(Builtins.prelude_env ()) program with
      | Ok result -> result.result_type = Types.TInt
      | Error _ -> false)

let%test "Phase6 prep: outer non-callback calls do not steal placeholder rewrites" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match
    Syntax.Parser.parse ~file_id:"<test>" "fn apply[a, b](x: a, f: (a) -> b) -> b = f(x)\nputs(apply(21, _ * 2))"
  with
  | Error _ -> false
  | Ok program -> (
      match check_program_with_annotations ~env:(Builtins.prelude_env ()) program with
      | Ok result -> result.result_type = Types.TNull
      | Error _ -> false)

let%test "Phase6 prep: nested placeholder shorthand works in call arguments" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match
    check_string ~file_id:"<test>"
      "fn map_list[a, b](items: List[a], f: (a) -> b) -> List[b] = []\nfn trim(s: Str) -> Str = s\nfn strip(s: Str) -> Str = s\nlet cleaned = map_list([\" a \", \" b \"], trim(strip(_)))\ncleaned"
  with
  | Ok result -> result.result_type = Types.TArray Types.TString
  | Error _ -> false

let%test "Phase6 prep: placeholder shorthand rejects multiple placeholders in one callback argument" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match
    check_string ~file_id:"<test>"
      "fn map_list[a, b](items: List[a], f: (a) -> b) -> List[b] = []\nmap_list([1, 2], _ + _)"
  with
  | Error diags ->
      List.exists
        (fun (d : Diagnostic.t) ->
          d.code = "type-invalid-placeholder"
          && String_utils.contains_substring ~needle:"exactly one '_'" d.message)
        diags
  | Ok _ -> false

let%test "Phase6 prep: placeholder shorthand rejects effectful callback slots" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match
    check_string ~env:(Builtins.prelude_env ()) ~file_id:"<test>"
      "fn foreach_list[a](items: List[a], f: (a) => Unit) => Unit = puts(\"x\")\nforeach_list([1, 2], puts(_))"
  with
  | Error diags ->
      List.exists
        (fun (d : Diagnostic.t) ->
          d.code = "type-invalid-placeholder"
          && String_utils.contains_substring ~needle:"explicit '=>'" d.message)
        diags
  | Ok _ -> false

let%test "Phase6 prep: placeholder shorthand rejects effectful standalone sections" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match check_string ~env:(Builtins.prelude_env ()) ~file_id:"<test>" "let printer = puts(_)\nprinter" with
  | Error diags ->
      List.exists
        (fun (d : Diagnostic.t) ->
          d.code = "type-invalid-placeholder"
          && String_utils.contains_substring ~needle:"explicit '=>'" d.message)
        diags
  | Ok _ -> false

let%test "Phase6 prep: placeholder shorthand rejects multiple placeholders across block bodies" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match
    check_string ~file_id:"<test>"
      "fn apply[a, b](x: a, f: (a) -> b) -> b = f(x)\napply(21, {\n\  let left = _\n\  left + _\n})"
  with
  | Error diags ->
      List.exists
        (fun (d : Diagnostic.t) ->
          d.code = "type-invalid-placeholder"
          && String_utils.contains_substring ~needle:"exactly one '_' placeholder" d.message)
        diags
  | Ok _ -> false

let%test "Phase6 prep: placeholder shorthand survives nested conditional callback bodies" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match
    check_string ~file_id:"<test>"
      "fn apply[a, b](x: a, f: (a) -> b) -> b = f(x)\nlet result = apply(21, if (_ > 20) { \"big\" } else { \"small\" })\nresult"
  with
  | Ok result -> result.result_type = Types.TString
  | Error _ -> false

let%test "Phase6 prep: placeholder shorthand survives field access inside nested calls" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match
    check_string ~env:(Builtins.prelude_env ()) ~file_id:"<test>"
      "type User = { id: Int }\nfn apply[a, b](x: a, f: (a) -> b) -> b = f(x)\nfn plus_one(n: Int) -> Int = n + 1\nfn render(n: Int) -> Str = \"#\" + Show.show(n)\nlet user: User = { id: 7 }\nlet result = apply(user, render(plus_one(_.id)))\nresult"
  with
  | Ok result -> result.result_type = Types.TString
  | Error _ -> false

let%test "Phase6 prep: pipe fills placeholder inside call-shaped sections" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match
    check_string ~file_id:"<test>"
      "fn add(x: Int, y: Int) -> Int = x + y\nlet result = 1 |> add(_, 2)\nresult"
  with
  | Ok result -> result.result_type = Types.TInt
  | Error _ -> false

let%test "Phase6 prep: pipe fills placeholder inside qualified call sections" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match
    check_string ~env:(Builtins.prelude_env ()) ~file_id:"<test>"
      "type Post = { owner: Str }\ntrait Visible[a] = {\n  fn visible_to(x: a, viewer: Str) -> Bool\n}\nimpl Visible[Post] = {\n  fn visible_to(x: Post, viewer: Str) -> Bool = x.owner == viewer\n}\nlet post: Post = { owner: \"ada\" }\nlet result = post |> Visible.visible_to(_, \"ada\")\nresult"
  with
  | Ok result -> result.result_type = Types.TBool
  | Error _ -> false

let%test "followup B1: successful checks expose an empty trait object coercion map by default" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match check_string ~file_id:"<test>" "let x = 1\nx" with
  | Ok result -> Hashtbl.length result.trait_object_coercion_map = 0
  | Error _ -> false

let%test "followup B3: let annotation to Dyn records one coercion site" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match check_string ~env:(Builtins.prelude_env ()) ~file_id:"<test>" "let x: Dyn[Show] = 42\nx" with
  | Ok result ->
      result.result_type = Types.TTraitObject [ "Show" ]
      && Hashtbl.fold
           (fun _id (coercion : Resolution_artifacts.trait_object_coercion) acc ->
             acc || (coercion.target_traits = [ "Show" ] && coercion.source_type = Types.TInt))
           result.trait_object_coercion_map false
  | Error _ -> false

let%test "followup B3: Dyn argument position records a coercion site" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match
    check_string ~env:(Builtins.prelude_env ()) ~file_id:"<test>"
      "fn keep(x: Dyn[Show]) -> Dyn[Show] = x\nlet y = keep(42)\ny"
  with
  | Ok result ->
      result.result_type = Types.TTraitObject [ "Show" ]
      && Hashtbl.fold
           (fun _id (coercion : Resolution_artifacts.trait_object_coercion) acc ->
             acc || (coercion.target_traits = [ "Show" ] && coercion.source_type = Types.TInt))
           result.trait_object_coercion_map false
  | Error _ -> false

let%test "followup B3: Dyn return position records a coercion site" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match
    check_string ~env:(Builtins.prelude_env ()) ~file_id:"<test>" "fn box() -> Dyn[Show] = 42\nlet y = box()\ny"
  with
  | Ok result ->
      result.result_type = Types.TTraitObject [ "Show" ]
      && Hashtbl.fold
           (fun _id (coercion : Resolution_artifacts.trait_object_coercion) acc ->
             acc || (coercion.target_traits = [ "Show" ] && coercion.source_type = Types.TInt))
           result.trait_object_coercion_map false
  | Error _ -> false

let%test "followup B3: Dyn rejects shapes in annotations" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  let code =
    {|
      shape Named = {
        name: Str
      }
      let x: Dyn[Named] = { name: "alice" }
      x
    |}
  in
  match check_string ~env:(Builtins.prelude_env ()) ~file_id:"main.mr" code with
  | Ok _ -> false
  | Error diags -> List.exists (fun (diag : Diagnostic.t) -> diag.code = "type-trait-object-shape") diags

let%test "followup B3: Dyn rejects mixed trait sets containing a shape" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  let code =
    {|
      shape Named = {
        name: Str
      }
      let x: Dyn[Show & Named] = 42
      x
    |}
  in
  match check_string ~env:(Builtins.prelude_env ()) ~file_id:"main.mr" code with
  | Ok _ -> false
  | Error diags -> List.exists (fun (diag : Diagnostic.t) -> diag.code = "type-trait-object-shape") diags

let%test "followup B3: qualified Dyn trait calls typecheck" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match check_string ~env:(Builtins.prelude_env ()) ~file_id:"<test>" "let x: Dyn[Show] = 42\nShow.show(x)" with
  | Ok result -> result.result_type = Types.TString
  | Error _ -> false

let%test "followup B3: Dyn field access is rejected explicitly" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  let code =
    {|
      type Person = { name: Str }
      trait NamedShow[a] = {
        fn show(x: a) -> Str
      }
      impl NamedShow[Person] = {
        fn show(x: Person) -> Str = x.name
      }
      let person: Person = { name: "alice" }
      let x: Dyn[NamedShow] = person
      x.name
    |}
  in
  match check_string ~env:(Builtins.prelude_env ()) ~file_id:"main.mr" code with
  | Ok _ -> false
  | Error diags -> List.exists (fun (diag : Diagnostic.t) -> diag.code = "type-trait-object-field-access") diags

let%test "followup B3: Dyn coercion reports missing impl diagnostics" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  let code =
    {|
      trait Printable[a] = {
        fn print(x: a) -> Str
      }
      let x: Dyn[Printable] = 42
      x
    |}
  in
  match check_string ~env:(Builtins.prelude_env ()) ~file_id:"main.mr" code with
  | Ok _ -> false
  | Error diags -> List.exists (fun (diag : Diagnostic.t) -> diag.code = "type-trait-missing-impl") diags

let%test "followup B3: Dyn rejects non-object-safe method calls" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  match check_string ~env:(Builtins.prelude_env ()) ~file_id:"<test>" "let x: Dyn[Eq] = 1\nEq.eq(x, x)" with
  | Ok _ -> false
  | Error diags -> List.exists (fun (diag : Diagnostic.t) -> diag.code = "type-trait-object-object-unsafe") diags

let%test "followup B3: Dyn rejects method-generic dispatch" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  let code =
    {|
      trait Caster[a] = {
        fn cast[b](x: a, f: (a) -> b) -> b
      }
      impl Caster[Int] = {
        fn cast[b](x: Int, f: (Int) -> b) -> b = f(x)
      }
      let x: Dyn[Caster] = 1
      Caster.cast[Str](x, (n: Int) -> Show.show(n))
    |}
  in
  match check_string ~env:(Builtins.prelude_env ()) ~file_id:"main.mr" code with
  | Ok _ -> false
  | Error diags -> List.exists (fun (diag : Diagnostic.t) -> diag.code = "type-trait-object-object-unsafe") diags

let%test "followup C2: record intersections allow field access guaranteed by every member" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  let code = {|
      let value: ({ x: Int, y: Str } & { x: Int }) = { x: 1, y: "ok" }
      value.x
    |} in
  match check_string ~file_id:"main.mr" code with
  | Ok result -> result.result_type = Types.TInt
  | Error _ -> false

let%test "followup C2: record intersections reject field access not guaranteed by every member" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  let code = {|
      let value: ({ x: Int, y: Str } & { x: Int }) = { x: 1, y: "ok" }
      value.y
    |} in
  match check_string ~file_id:"main.mr" code with
  | Ok _ -> false
  | Error diags ->
      List.exists
        (fun (diag : Diagnostic.t) ->
          diag.code = "type-intersection-field-access"
          && String_utils.contains_substring ~needle:"every member" diag.message)
        diags

let%test "followup C2: intersections reject mixed Dyn and non-Dyn members" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  let code = {|
      let value: Dyn[Show] & Int = 42
      value
    |} in
  match check_string ~env:(Builtins.prelude_env ()) ~file_id:"main.mr" code with
  | Ok _ -> false
  | Error diags ->
      List.exists
        (fun (diag : Diagnostic.t) ->
          diag.code = "type-annotation-invalid"
          && String_utils.contains_substring ~needle:"Dyn[...]" diag.message
          && String_utils.contains_substring ~needle:"non-Dyn" diag.message)
        diags

let%test "followup C2: intersections reject multi-member callable types" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  let code = {|
      let f: ((Int) -> Int) & Bool = (x: Int) -> x
      f
    |} in
  match check_string ~file_id:"main.mr" code with
  | Ok _ -> false
  | Error diags ->
      List.exists
        (fun (diag : Diagnostic.t) ->
          diag.code = "type-annotation-invalid"
          && String_utils.contains_substring ~needle:"callable intersections" diag.message)
        diags

let%test "followup C2: invalid let intersection annotation survives initializer failures" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  let code = {|
      let value: Dyn[Show] & Int = 1 + true
      value
    |} in
  match check_string ~env:(Builtins.prelude_env ()) ~file_id:"main.mr" code with
  | Ok _ -> false
  | Error diags -> List.exists (fun (diag : Diagnostic.t) -> diag.code = "type-annotation-invalid") diags

let%test "followup C2: intersection values flow to compatible record annotations" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  let code =
    {|
      let value: ({ x: Int, y: Str } & { x: Int }) = { x: 1, y: "ok" }
      let projected: { x: Int } = value
      projected.x
    |}
  in
  match check_string ~file_id:"main.mr" code with
  | Ok result -> result.result_type = Types.TInt
  | Error _ -> false

let%test "followup C2: shape constraints accept compatible record intersections" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  let code =
    {|
      shape Named = { name: Str }
      fn get_name[t: Named](x: t) -> Str = x.name
      let value: ({ name: Str, age: Int } & { name: Str }) = { name: "alice", age: 1 }
      get_name(value)
    |}
  in
  match check_string ~file_id:"main.mr" code with
  | Ok result -> result.result_type = Types.TString
  | Error _ -> false

let%test "followup C2: intersection return annotations accept identical meets" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  let code =
    {|
      fn keep(x: ({ x: Int } & { y: Int })) -> ({ x: Int } & { y: Int }) = x
      keep({ x: 1, y: 2 })
    |}
  in
  match check_string ~file_id:"main.mr" code with
  | Ok result ->
      result.result_type
      = Types.TIntersection
          [
            Types.TRecord ([ { Types.name = "x"; typ = Types.TInt } ], None);
            Types.TRecord ([ { Types.name = "y"; typ = Types.TInt } ], None);
          ]
  | Error _ -> false

let%test "followup C2: invalid function return intersection annotations are rejected" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  let code = {|
      fn bad() -> Dyn[Show] & Int = 42
      bad()
    |} in
  match check_string ~env:(Builtins.prelude_env ()) ~file_id:"main.mr" code with
  | Ok _ -> false
  | Error diags -> List.exists (fun (diag : Diagnostic.t) -> diag.code = "type-annotation-invalid") diags

let%test "generic equality operator rejects function specialization without Eq" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  let code =
    {|
      fn same[a](x: a, y: a) -> Bool = x == y
      fn id1(n: Int) -> Int = n
      fn id2(n: Int) -> Int = n + 1
      same(id1, id2)
    |}
  in
  match check_string ~env:(Builtins.prelude_env ()) ~file_id:"main.mr" code with
  | Ok _ -> false
  | Error diags -> List.exists (fun (diag : Diagnostic.t) -> diag.code = "type-trait-missing-impl") diags

let%test "generic ordering operator rejects array specialization without Ord" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  let code = {|
      fn less[a](x: a, y: a) -> Bool = x < y
      less([1], [2])
    |} in
  match check_string ~env:(Builtins.prelude_env ()) ~file_id:"main.mr" code with
  | Ok _ -> false
  | Error diags -> List.exists (fun (diag : Diagnostic.t) -> diag.code = "type-trait-missing-impl") diags

let%test "generic arithmetic operator rejects bool specialization without Num" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  let code = {|
      fn add[a](x: a, y: a) = x + y
      add(true, false)
    |} in
  match check_string ~env:(Builtins.prelude_env ()) ~file_id:"main.mr" code with
  | Ok _ -> false
  | Error diags -> List.exists (fun (diag : Diagnostic.t) -> diag.code = "type-trait-missing-impl") diags

let%test "spread-returning polymorphic function preserves wide record fields" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  let code =
    {|
      fn update_x(r, new_x) = { ...r, x: new_x }
      let base = { x: 1, y: 20 }
      let result = update_x(base, 5)
      result.y
    |}
  in
  match check_string ~file_id:"main.mr" code with
  | Ok result -> result.result_type = Types.TInt
  | Error _ -> false

let%test "followup C2: record intersections satisfy shape constraints" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  let code =
    {|
      shape Named = {
        name: Str
      }
      fn get_name[t: Named](x: t) -> Str = x.name
      let value: ({ name: Str, age: Int } & { name: Str }) = { name: "alice", age: 1 }
      get_name(value)
    |}
  in
  match check_string ~file_id:"main.mr" code with
  | Ok result -> result.result_type = Types.TString
  | Error _ -> false

let%test "followup C2: intersections can flow to compatible member record annotations" =
  Infer.reset_fresh_counter ();
  Trait_registry.clear ();
  let code =
    {|
      let value: ({ x: Int, y: Str } & { x: Int }) = { x: 1, y: "ok" }
      let projected: { x: Int } = value
      projected.x
    |}
  in
  match check_string ~file_id:"main.mr" code with
  | Ok result -> result.result_type = Types.TInt
  | Error _ -> false
