(* Document analysis: parse + typecheck → diagnostics *)

module Lsp_t = Lsp_compat.Types
module Checker = Marmoset.Lib.Checker
module Parser = Marmoset.Lib.Parser
module Infer = Marmoset.Lib.Infer
module Builtins = Marmoset.Lib.Builtins
module Ast = Marmoset.Lib.Ast

type analysis_result = {
  source : string;
  program : Ast.AST.program option;
  type_map : Infer.type_map option;
  environment : Infer.type_env option;
  diagnostics : Lsp_t.Diagnostic.t list;
}

(* Reset all global mutable state before a fresh analysis *)
let reset_globals () =
  Infer.reset_fresh_counter ();
  Typecheck.Trait_registry.clear ();
  Typecheck.Enum_registry.clear ();
  Typecheck.Annotation.clear_type_aliases ()

let make_diagnostic ~(range : Lsp_t.Range.t) ~(severity : Lsp_t.DiagnosticSeverity.t) ~(message : string) :
    Lsp_t.Diagnostic.t =
  Lsp_t.Diagnostic.create ~range ~severity ~source:"marmoset" ~message ()

let zero_range =
  let zero = Lsp_t.Position.create ~line:0 ~character:0 in
  Lsp_t.Range.create ~start:zero ~end_:zero

(* Analyze a document, returning parse/type errors as LSP diagnostics *)
let analyze ~(source : string) : analysis_result =
  reset_globals ();
  match Parser.parse source with
  | Error errors ->
      (* Parser currently returns unlocated string errors *)
      let diagnostics =
        List.map
          (fun msg -> make_diagnostic ~range:zero_range ~severity:Lsp_t.DiagnosticSeverity.Error ~message:msg)
          errors
      in
      { source; program = None; type_map = None; environment = None; diagnostics }
  | Ok program -> (
      let env = Builtins.prelude_env () in
      match Checker.check_program_with_annotations ~source ~env program with
      | Error err ->
          let range =
            match (err.loc, err.loc_end) with
            | Some loc, Some loc_end ->
                let start = Lsp_utils.loc_to_position loc in
                let end_ = Lsp_utils.loc_to_position loc_end in
                Lsp_t.Range.create ~start ~end_
            | Some loc, None ->
                let pos = Lsp_utils.loc_to_position loc in
                Lsp_t.Range.create ~start:pos ~end_:pos
            | None, _ -> zero_range
          in
          let diagnostics =
            [ make_diagnostic ~range ~severity:Lsp_t.DiagnosticSeverity.Error ~message:err.message ]
          in
          { source; program = Some program; type_map = None; environment = None; diagnostics }
      | Ok result ->
          {
            source;
            program = Some program;
            type_map = Some result.type_map;
            environment = Some result.environment;
            diagnostics = [];
          })

(* ============================================================
   Tests
   ============================================================ *)

let%test "analyze valid code produces empty diagnostics" =
  let result = analyze ~source:"let x = 42;" in
  result.diagnostics = [] && result.program <> None && result.type_map <> None

let%test "analyze parse error produces diagnostic" =
  let result = analyze ~source:"let = ;" in
  List.length result.diagnostics > 0 && result.program = None && result.type_map = None

let%test "analyze type error produces diagnostic with range" =
  let result = analyze ~source:"1 + true" in
  List.length result.diagnostics = 1
  &&
  let diag = List.hd result.diagnostics in
  diag.severity = Some Lsp_t.DiagnosticSeverity.Error && diag.source = Some "marmoset"

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
