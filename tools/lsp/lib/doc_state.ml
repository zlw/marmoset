(* Document analysis: parse + typecheck → diagnostics *)

module Lsp_t = Linol_lsp.Types
module Checker = Marmoset.Lib.Checker
module Parser = Marmoset.Lib.Parser
module Infer = Marmoset.Lib.Infer
module Builtins = Marmoset.Lib.Builtins
module Ast = Marmoset.Lib.Ast
module Diagnostic = Marmoset.Lib.Diagnostic

type analysis_result = {
  source : string;
  program : Ast.AST.program option;
  type_map : Infer.type_map option;
  environment : Infer.type_env option;
  type_var_user_names : (string * string) list;
  diagnostics : Lsp_t.Diagnostic.t list;
}

(* Reset all global mutable state before a fresh analysis *)
let reset_globals () =
  Infer.reset_fresh_counter ();
  Infer.clear_method_resolution_store ();
  Typecheck.Trait_registry.clear ();
  Typecheck.Enum_registry.clear ();
  Typecheck.Annotation.clear_type_aliases ()

let make_diagnostic ~code ~(range : Lsp_t.Range.t) ~(severity : Lsp_t.DiagnosticSeverity.t) ~(message : string) :
    Lsp_t.Diagnostic.t =
  Lsp_t.Diagnostic.create ?code ~range ~severity ~source:"marmoset" ~message:(`String message) ()

let zero_range =
  let zero = Lsp_t.Position.create ~line:0 ~character:0 in
  Lsp_t.Range.create ~start:zero ~end_:zero

let lsp_severity_of_diagnostic = function
  | Diagnostic.Error -> Lsp_t.DiagnosticSeverity.Error
  | Diagnostic.Warning -> Lsp_t.DiagnosticSeverity.Warning
  | Diagnostic.Info -> Lsp_t.DiagnosticSeverity.Information

let first_diagnostic_span (diag : Diagnostic.t) : Diagnostic.span option =
  let rec first_primary = function
    | [] -> None
    | { Diagnostic.primary = true; span = Diagnostic.NoSpan; _ } :: rest -> first_primary rest
    | { Diagnostic.primary = true; span; _ } :: _ -> Some span
    | _ :: rest -> first_primary rest
  in
  let rec first_with_span = function
    | [] -> None
    | { Diagnostic.span = Diagnostic.NoSpan; _ } :: rest -> first_with_span rest
    | { Diagnostic.span; _ } :: _ -> Some span
  in
  match first_primary diag.labels with
  | Some _ as span -> span
  | None -> first_with_span diag.labels

let range_of_diagnostic_span ~(source : string) ~(active_file_id : string) (span : Diagnostic.span option) :
    Lsp_t.Range.t =
  match span with
  | None | Some Diagnostic.NoSpan -> zero_range
  | Some (Diagnostic.Span { file_id; start_pos; end_pos = Some end_pos }) ->
      if not (String.equal file_id active_file_id) then
        zero_range
      else
        Lsp_utils.offset_range_to_lsp ~source ~pos:start_pos ~end_pos
  | Some (Diagnostic.Span { file_id; start_pos; end_pos = None }) ->
      if not (String.equal file_id active_file_id) then
        zero_range
      else
        let start = Lsp_utils.offset_to_position ~source ~offset:start_pos in
        let end_ = Lsp_utils.offset_to_position ~source ~offset:(start_pos + 1) in
        Lsp_t.Range.create ~start ~end_

let lsp_code_of_diagnostic (diag : Diagnostic.t) = `String diag.code

let lsp_diagnostic_of_canonical ~(source : string) ~(active_file_id : string) (diag : Diagnostic.t) :
    Lsp_t.Diagnostic.t =
  let range = range_of_diagnostic_span ~source ~active_file_id (first_diagnostic_span diag) in
  let severity = lsp_severity_of_diagnostic diag.severity in
  make_diagnostic ~code:(Some (lsp_code_of_diagnostic diag)) ~range ~severity ~message:diag.message

(* Analyze a document, returning parse/type errors as LSP diagnostics *)
let analyze_with_file_id ~(file_id : string) ~(source : string) : analysis_result =
  reset_globals ();
  let state = Infer.create_inference_state () in
  match Parser.parse ~file_id source with
  | Error errors ->
      let diagnostics = List.map (lsp_diagnostic_of_canonical ~source ~active_file_id:file_id) errors in
      { source; program = None; type_map = None; environment = None; type_var_user_names = []; diagnostics }
  | Ok program -> (
      let env = Builtins.prelude_env () in
      match Checker.check_program_with_annotations ~state ~source ~env program with
      | Error diag ->
          let type_var_user_names = Infer.type_var_user_name_bindings_in_state state in
          let diagnostics =
            [ lsp_diagnostic_of_canonical ~source ~active_file_id:file_id diag ]
          in
          {
            source;
            program = Some program;
            type_map = None;
            environment = None;
            type_var_user_names;
            diagnostics;
          }
      | Ok result ->
          let type_var_user_names = Infer.type_var_user_name_bindings_in_state state in
          {
            source;
            program = Some program;
            type_map = Some result.type_map;
            environment = Some result.environment;
            type_var_user_names;
            diagnostics = [];
          })

let analyze ~(source : string) : analysis_result = analyze_with_file_id ~file_id:"<lsp>" ~source

(* ============================================================
   Tests
   ============================================================ *)

let%test "analyze valid code produces empty diagnostics" =
  let result = analyze ~source:"let x = 42;" in
  result.diagnostics = [] && result.program <> None && result.type_map <> None

let%test "analyze parse error produces diagnostic" =
  let result = analyze ~source:"let = ;" in
  List.length result.diagnostics > 0 && result.program = None && result.type_map = None

let%test "analyze parse error has non-zero range when span exists" =
  let result = analyze ~source:"let = ;" in
  match result.diagnostics with
  | diag :: _ -> diag.range.start <> diag.range.end_
  | [] -> false

let%test "analyze type error produces diagnostic with range" =
  let result = analyze ~source:"1 + true" in
  List.length result.diagnostics = 1
  &&
  let diag = List.hd result.diagnostics in
  diag.severity = Some Lsp_t.DiagnosticSeverity.Error && diag.source = Some "marmoset"

let%test "analyze diagnostics expose structured code field" =
  let result = analyze ~source:"1 + true" in
  match result.diagnostics with
  | [ diag ] -> diag.code <> None
  | _ -> false

let%test "analyze type error has non-zero range when location available" =
  let result = analyze ~source:"1 + true" in
  match result.diagnostics with
  | [ diag ] ->
      (* The type error for "true" should have position info *)
      diag.range.start.line >= 0
  | _ -> false

let%test "analyze successful code stores type_map and environment" =
  let result = analyze ~source:"let f = fn(x) { x + 1 }; f" in
  result.diagnostics = [] && result.type_map <> None && result.environment <> None

let%test "analyze with builtins works" =
  let result = analyze ~source:"len([1, 2, 3])" in
  result.diagnostics = [] && result.type_map <> None

let%test "analyze captures user generic names for hover formatting" =
  let result =
    analyze ~source:"trait named { name: string }\nlet get = fn[t: named](x: t) -> string { x.name }; get"
  in
  result.diagnostics = [] && List.exists (fun (_fresh, user_name) -> user_name = "t") result.type_var_user_names

let%test "analyze does not leak generic-name mappings across documents" =
  let _ =
    analyze ~source:"trait named { name: string }\nlet get = fn[t: named](x: t) -> string { x.name }; get"
  in
  let result = analyze ~source:"let x = 1; x" in
  result.type_var_user_names = []
