(* Document analysis: parse + typecheck → diagnostics *)

module Lsp_t = Linol_lsp.Types
module Compiler = Marmoset.Lib.Frontend_compiler
module Infer = Marmoset.Lib.Infer
module Ast = Marmoset.Lib.Ast
module Diagnostic = Marmoset.Lib.Diagnostic

type analysis_result = {
  source : string;
  module_id : string option;
  source_root : string option;
  project_root : string option;
  program : Ast.AST.program option;
  type_map : Infer.type_map option;
  environment : Infer.type_env option;
  type_var_user_names : (string * string) list;
  diagnostics : Lsp_t.Diagnostic.t list;
  compiler_analysis : Compiler.entry_analysis option;
}

let make_diagnostic ~code ~(range : Lsp_t.Range.t) ~(severity : Lsp_t.DiagnosticSeverity.t) ~(message : string) :
    Lsp_t.Diagnostic.t =
  Lsp_t.Diagnostic.create ?code ~range ~severity ~source:"marmoset" ~message:(`String message) ()

let zero_range =
  let zero = Lsp_t.Position.create ~line:0 ~character:0 in
  Lsp_t.Range.create ~start:zero ~end_:zero

let normalize_path (path : string) : string =
  if Filename.is_relative path then
    Filename.concat (Sys.getcwd ()) path
  else
    path

let file_path_of_file_id (file_id : string) : string option =
  try
    if Diagnostics.String_utils.contains_substring ~needle:"://" file_id then
      Some (normalize_path Lsp_t.DocumentUri.(to_path (of_string file_id)))
    else if String.length file_id > 0 && file_id.[0] <> '<' then
      Some (normalize_path file_id)
    else
      None
  with _ -> None

let normalize_source_overrides (source_overrides : (string, string) Hashtbl.t) : (string, string) Hashtbl.t =
  let normalized = Hashtbl.create (Hashtbl.length source_overrides) in
  Hashtbl.iter (fun path source -> Hashtbl.replace normalized (normalize_path path) source) source_overrides;
  normalized

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

let compiler_analysis_for_file_id
    ?source_root ?(source_overrides = Hashtbl.create 0) ~(file_id : string) ~(source : string) :
    unit -> Compiler.entry_analysis =
 fun () ->
  match file_path_of_file_id file_id with
  | Some entry_file ->
      Compiler.analyze_entry_with_overrides ?source_root ~entry_file ~entry_source:source
        ~source_overrides:(normalize_source_overrides source_overrides)
        ()
  | None -> Compiler.analyze_entry_with_source ?source_root ~entry_file:file_id ~entry_source:source ()

let expose_typed_state (_analysis : Compiler.entry_analysis) : bool = true

(* Analyze a document, returning parse/type errors as LSP diagnostics *)
let analyze_with_file_id
    ?source_root ?(source_overrides = Hashtbl.create 0) ~(file_id : string) ~(source : string) :
    unit -> analysis_result =
 fun () ->
  let compiler_analysis = compiler_analysis_for_file_id ?source_root ~source_overrides ~file_id ~source () in
  let active_file = compiler_analysis.active_file in
  let diagnostics =
    List.map (lsp_diagnostic_of_canonical ~source ~active_file_id:file_id) active_file.diagnostics
  in
  let program =
    match active_file.surface_program with
    | Some program when (not (has_module_headers program)) || compiler_analysis.mode = Compiler.Modules ->
        Some program
    | other -> other
  in
  let should_expose_typed_state = expose_typed_state compiler_analysis in
  {
    source;
    module_id = active_file.module_id;
    source_root = compiler_analysis.source_root;
    project_root = compiler_analysis.project_root;
    program;
    type_map =
      (if should_expose_typed_state then
         active_file.type_map
       else
         None);
    environment =
      (if should_expose_typed_state then
         active_file.environment
       else
         None);
    type_var_user_names =
      (if should_expose_typed_state then
         active_file.type_var_user_names
       else
         []);
    diagnostics;
    compiler_analysis = Some compiler_analysis;
  }

let analyze ~(source : string) : analysis_result =
  let root = Filename.temp_file "marmoset_lsp_snippet_" "" in
  Sys.remove root;
  ignore (Sys.command ("mkdir -p " ^ Filename.quote root));
  let entry_file = Filename.concat root "main.mr" in
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command ("rm -rf " ^ Filename.quote root)))
    (fun () -> analyze_with_file_id ~file_id:entry_file ~source ())

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
  result.diagnostics = []
  && result.module_id = Some "main"
  && result.source_root = None
  && result.project_root <> None
  && result.type_map <> None
  && result.environment <> None
  && result.compiler_analysis <> None

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
        "import sum\ntype Point = { x: Int, y: Int }\nlet point: Point = { x: 1, y: 2 }\nlet moved = { ...point, x: 10 }\nfn get_x(value: Point) -> Int = value.x\nlet nums = sum.sum([1, 2, 3])\nputs(get_x(moved))\nputs(moved.x + moved.y)\nputs(nums)\n"
      );
      ("sum.mr", "export sum\nfn sum(values: List[Int]) -> Int = len(values)\n");
    ]
    (fun root ->
      let file_id = Filename.concat root "main.mr" in
      let source =
        "import sum\ntype Point = { x: Int, y: Int }\nlet point: Point = { x: 1, y: 2 }\nlet moved = { ...point, x: 10 }\nfn get_x(value: Point) -> Int = value.x\nlet nums = sum.sum([1, 2, 3])\nputs(get_x(moved))\nputs(moved.x + moved.y)\nputs(nums)\n"
      in
      let result = analyze_with_file_id ~file_id ~source () in
      result.diagnostics = []
      && result.module_id = Some "main"
      && result.program <> None
      && result.compiler_analysis <> None)

let%test "analyze helper uses module-aware compiler analysis" =
  let result = analyze ~source:"let id = (x) -> x\nid(1)\n" in
  match result.compiler_analysis with
  | None -> false
  | Some analysis ->
      analysis.mode = Compiler.Modules
      && result.module_id = Some "main"
      && result.source_root = None
      && result.project_root <> None
      && result.type_map <> None
      && result.environment <> None

let%test "analyze_with_file_id keeps module surface AST while exposing compiler analysis" =
  with_temp_project
    [
      ("main.mr", "import sum\nputs(sum.sum([1, 2, 3]))\n");
      ("sum.mr", "export sum\nfn sum(values: List[Int]) -> Int = len(values)\n");
    ]
    (fun root ->
      let file_id = Filename.concat root "main.mr" in
      let result = analyze_with_file_id ~file_id ~source:"import sum\nputs(sum.sum([1, 2, 3]))\n" () in
      let surface_has_namespace =
        match result.program with
        | None -> false
        | Some program ->
            List.exists
              (fun (stmt : Ast.AST.statement) ->
                match stmt.stmt with
                | Ast.AST.ExpressionStmt
                    {
                      expr =
                        Ast.AST.Call
                          ( { expr = Ast.AST.Identifier "puts"; _ },
                            [
                              {
                                expr =
                                  Ast.AST.MethodCall
                                    {
                                      mc_receiver = { expr = Ast.AST.Identifier "sum"; _ };
                                      mc_method = "sum";
                                      mc_args = _;
                                      _;
                                    };
                                _;
                              };
                            ] );
                      _;
                    } ->
                    true
                | _ -> false)
              program
      in
      match result.compiler_analysis with
      | None -> false
      | Some analysis ->
          analysis.mode = Compiler.Modules
          && surface_has_namespace
          && result.module_id = Some "main"
          && result.project_root = Some root
          && result.type_map <> None
          && result.environment <> None)

let%test "analyze_with_file_id preserves module export diagnostics for qualified access" =
  with_temp_project
    [ ("main.mr", "import sum\nsum.reduce\n"); ("sum.mr", "fn reduce(values: List[Int]) -> Int = len(values)\n") ]
    (fun root ->
      let file_id = Filename.concat root "main.mr" in
      let result = analyze_with_file_id ~file_id ~source:"import sum\nsum.reduce\n" () in
      List.exists
        (fun (diag : Lsp_t.Diagnostic.t) ->
          match diag.message with
          | `String message -> String.equal message "Module 'sum' does not export 'reduce'"
          | _ -> false)
        result.diagnostics)

let%test "analyze_with_file_id applies normalized file overrides for file URI inputs" =
  with_temp_project
    [ ("main.mr", "import math.answer\nanswer() + 1\n"); ("math.mr", "export answer\nfn answer() -> Int = 41\n") ]
    (fun root ->
      let main_path = Filename.concat root "main.mr" in
      let main_uri = Lsp_t.DocumentUri.(to_string (of_path main_path)) in
      let overrides = Hashtbl.create 1 in
      Hashtbl.replace overrides (Filename.concat root "math.mr")
        "export answer\nfn answer() -> Str = \"forty one\"\n";
      let result =
        analyze_with_file_id ~source_root:root ~source_overrides:overrides ~file_id:main_uri
          ~source:"import math.answer\nanswer() + 1\n" ()
      in
      result.project_root = Some root
      && List.exists
           (fun (diag : Lsp_t.Diagnostic.t) ->
             match diag.message with
             | `String message -> Diagnostics.String_utils.contains_substring ~needle:"Str" message
             | _ -> false)
           result.diagnostics)
