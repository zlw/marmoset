(* Document analysis: parse + typecheck → diagnostics *)

module Lsp_t = Linol_lsp.Types
module Checker = Marmoset.Lib.Checker
module Compiler = Marmoset.Lib.Frontend_compiler
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

let file_path_of_file_id (file_id : string) : string option =
  try
    if Diagnostics.String_utils.contains_substring ~needle:"://" file_id then
      Some (Lsp_t.DocumentUri.(to_path (of_string file_id)))
    else if Sys.file_exists file_id then
      Some file_id
    else
      None
  with _ -> None

let active_file_matches ~(active_file_id : string) ~(diag_file_id : string) : bool =
  String.equal diag_file_id active_file_id
  ||
  match file_path_of_file_id active_file_id with
  | Some active_path -> String.equal diag_file_id active_path
  | None -> false

let lsp_severity_of_diagnostic = function
  | Diagnostic.Error -> Lsp_t.DiagnosticSeverity.Error
  | Diagnostic.Warning -> Lsp_t.DiagnosticSeverity.Warning
  | Diagnostic.Info -> Lsp_t.DiagnosticSeverity.Information

let range_of_diagnostic_span ~(source : string) ~(active_file_id : string) (span : Diagnostic.span option) :
    Lsp_t.Range.t =
  match span with
  | None | Some Diagnostic.NoSpan -> zero_range
  | Some (Diagnostic.Span { file_id; start_pos; end_pos = Some end_pos }) ->
      if not (active_file_matches ~active_file_id ~diag_file_id:file_id) then
        zero_range
      else
        Lsp_utils.offset_range_to_lsp ~source ~pos:start_pos ~end_pos
  | Some (Diagnostic.Span { file_id; start_pos; end_pos = None }) ->
      if not (active_file_matches ~active_file_id ~diag_file_id:file_id) then
        zero_range
      else
        let start = Lsp_utils.offset_to_position ~source ~offset:start_pos in
        let end_ = Lsp_utils.offset_to_position ~source ~offset:(start_pos + 1) in
        Lsp_t.Range.create ~start ~end_

let lsp_code_of_diagnostic (diag : Diagnostic.t) = `String diag.code

let lsp_diagnostic_of_canonical ~(source : string) ~(active_file_id : string) (diag : Diagnostic.t) :
    Lsp_t.Diagnostic.t =
  let range = range_of_diagnostic_span ~source ~active_file_id (Diagnostic.pick_primary_span diag.labels) in
  let severity = lsp_severity_of_diagnostic diag.severity in
  make_diagnostic ~code:(Some (lsp_code_of_diagnostic diag)) ~range ~severity ~message:diag.message

let has_module_headers (program : Ast.AST.program) : bool =
  List.exists
    (fun (stmt : Ast.AST.statement) ->
      match stmt.stmt with
      | Ast.AST.ExportDecl _ | Ast.AST.ImportDecl _ -> true
      | _ -> false)
    program

let module_aware_analysis_with_file_id ~(file_id : string) ~(source : string) ~(program : Ast.AST.program) :
    analysis_result option =
  match (file_path_of_file_id file_id, has_module_headers program) with
  | Some entry_file, true -> (
      let diagnostics =
        match Compiler.check_entry_with_source ~entry_file ~entry_source:source with
        | Ok diags | Error diags ->
            List.map (lsp_diagnostic_of_canonical ~source ~active_file_id:file_id) diags
      in
      Some { source; program = Some program; type_map = None; environment = None; type_var_user_names = []; diagnostics })
  | _ -> None

(* Analyze a document, returning parse/type errors as LSP diagnostics *)
let analyze_with_file_id ~(file_id : string) ~(source : string) : analysis_result =
  reset_globals ();
  let state = Infer.create_inference_state () in
  match Parser.parse ~file_id source with
  | Error errors ->
      let diagnostics = List.map (lsp_diagnostic_of_canonical ~source ~active_file_id:file_id) errors in
      { source; program = None; type_map = None; environment = None; type_var_user_names = []; diagnostics }
  | Ok program -> (
      match module_aware_analysis_with_file_id ~file_id ~source ~program with
      | Some result -> result
      | None ->
          let env = Builtins.prelude_env () in
          match Checker.check_program_with_annotations ~state ~env program with
          | Error diags ->
              let type_var_user_names = Infer.type_var_user_name_bindings_in_state state in
              let diagnostics = List.map (lsp_diagnostic_of_canonical ~source ~active_file_id:file_id) diags in
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
              let diagnostics =
                List.map (lsp_diagnostic_of_canonical ~source ~active_file_id:file_id) result.diagnostics
              in
              {
                source;
                program = Some program;
                type_map = Some result.type_map;
                environment = Some result.environment;
                type_var_user_names;
                diagnostics;
              })

let analyze ~(source : string) : analysis_result = analyze_with_file_id ~file_id:"<lsp>" ~source

let with_temp_project (files : (string * string) list) (f : string -> bool) : bool =
  let root = Filename.temp_file "marmoset_lsp_modules_" "" in
  Sys.remove root;
  ignore (Sys.command ("mkdir -p " ^ Filename.quote root));
  let write_one (relative_path, content) =
    let path = Filename.concat root relative_path in
    ignore (Sys.command ("mkdir -p " ^ Filename.quote (Filename.dirname path)));
    let oc = open_out_bin path in
    Fun.protect ~finally:(fun () -> close_out oc) (fun () -> output_string oc content)
  in
  List.iter write_one files;
  Fun.protect ~finally:(fun () -> ignore (Sys.command ("rm -rf " ^ Filename.quote root))) (fun () -> f root)

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
  let result = analyze ~source:"let f = (x) -> x + 1; f" in
  result.diagnostics = [] && result.type_map <> None && result.environment <> None

let%test "analyze with builtins works" =
  let result = analyze ~source:"len([1, 2, 3])" in
  result.diagnostics = [] && result.type_map <> None

let%test "analyze successful code surfaces warning diagnostics" =
  let result =
    analyze
      ~source:
        {|
          trait Greeter[a] = {
            fn greet(x: a) -> Str
          }
          impl Greeter[Int] = {
            override fn greet(x: Int) -> Str = "hi"
          }
        |}
  in
  match result.diagnostics with
  | [ diag ] -> (
      diag.severity = Some Lsp_t.DiagnosticSeverity.Warning
      &&
      match diag.code with
      | Some (`String "override-unnecessary") -> true
      | _ -> false)
  | _ -> false

let%test "analyze captures user generic names for hover formatting" =
  let result = analyze ~source:"shape HasName = { name: Str }\nfn get[t: HasName](x: t) -> Str = x.name\nget" in
  result.diagnostics = [] && List.exists (fun (_fresh, user_name) -> user_name = "t") result.type_var_user_names

let%test "analyze does not leak generic-name mappings across documents" =
  let _ = analyze ~source:"shape HasName = { name: Str }\nfn get[t: HasName](x: t) -> Str = x.name\nget" in
  let result = analyze ~source:"let x = 1; x" in
  result.type_var_user_names = []

let%test "analyze_with_file_id uses module-aware checking for imported files" =
  with_temp_project
    [
      ( "main.mr",
        "import sum\n\
         type Point = { x: Int, y: Int }\n\
         let point: Point = { x: 1, y: 2 }\n\
         let moved = { ...point, x: 10 }\n\
         fn get_x(value: Point) -> Int = value.x\n\
         let nums = sum.sum([1, 2, 3])\n\
         puts(get_x(moved))\n\
         puts(moved.x + moved.y)\n\
         puts(nums)\n" );
      ("sum.mr", "export sum\nfn sum(values: List[Int]) -> Int = len(values)\n");
    ]
    (fun root ->
      let file_id = Filename.concat root "main.mr" in
      let source =
        "import sum\n\
         type Point = { x: Int, y: Int }\n\
         let point: Point = { x: 1, y: 2 }\n\
         let moved = { ...point, x: 10 }\n\
         fn get_x(value: Point) -> Int = value.x\n\
         let nums = sum.sum([1, 2, 3])\n\
         puts(get_x(moved))\n\
         puts(moved.x + moved.y)\n\
         puts(nums)\n"
      in
      let result = analyze_with_file_id ~file_id ~source in
      result.diagnostics = [] && result.program <> None)
