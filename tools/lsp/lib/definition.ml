module Lsp_t = Linol_lsp.Types
module Ast = Marmoset.Lib.Ast
module Compiler = Marmoset.Lib.Frontend_compiler
module Lexer = Marmoset.Lib.Lexer
module Token = Marmoset.Lib.Token
module Import_resolver = Frontend.Import_resolver
module Module_sig = Typecheck.Module_sig

type definition_target =
  | File_start of string
  | Span of {
      file_path : string;
      start_pos : int;
      end_pos : int;
    }

type chain_segment = {
  name : string;
  start_pos : int;
  end_pos : int;
}

type namespace_ref = {
  root_expr : Ast.AST.expression;
  segments : chain_segment list;
  cursor_segment_index : int;
}

let token_end_pos (tok : Token.token) : int = max tok.pos (tok.pos + String.length tok.literal - 1)

let first_some a b =
  match a with
  | Some _ -> a
  | None -> b

let take n xs =
  let rec go acc remaining = function
    | _ when remaining <= 0 -> List.rev acc
    | [] -> List.rev acc
    | x :: rest -> go (x :: acc) (remaining - 1) rest
  in
  go [] n xs

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '?' | '!' -> true
  | _ -> false

let skip_ascii_spaces ~(source : string) ~(pos : int) : int =
  let i = ref pos in
  while !i < String.length source && Char.code source.[!i] <= 32 do
    incr i
  done;
  !i

let definition_target_of_site (site : Module_sig.definition_site) : definition_target =
  Span { file_path = site.file_path; start_pos = site.start_pos; end_pos = site.end_pos }

let definition_target_of_member_binding (binding : Module_sig.member_binding) : definition_target option =
  let definition_site =
    [
      binding.value_definition;
      binding.enum_definition;
      binding.named_type_definition;
      binding.transparent_type_definition;
      binding.shape_definition;
      binding.trait_definition;
    ]
    |> List.find_opt Option.is_some
  in
  match definition_site with
  | Some (Some site) -> Some (definition_target_of_site site)
  | _ -> None

let definition_target_of_presence (presence : Import_resolver.member_presence) : definition_target option =
  let definition_site =
    [
      presence.value_definition;
      presence.enum_definition;
      presence.named_type_definition;
      presence.transparent_type_definition;
      presence.shape_definition;
      presence.trait_definition;
    ]
    |> List.find_opt Option.is_some
  in
  match definition_site with
  | Some (Some site) -> Some (definition_target_of_site site)
  | _ -> None

let source_for_file ~(analysis : Doc_state.analysis_result) ~(file_path : string) : string option =
  match analysis.compiler_analysis with
  | Some compiler_analysis when String.equal file_path compiler_analysis.active_file.file_path ->
      Some analysis.source
  | Some compiler_analysis -> (
      match Compiler.find_parsed_module_by_file compiler_analysis ~file_path with
      | Some (module_ : Compiler.Module_context.parsed_module) -> Some module_.source
      | None -> None)
  | None -> None

let read_file_source (file_path : string) : string option =
  try Some (Compiler.read_source_file file_path) with _ -> None

let location_of_target ~(analysis : Doc_state.analysis_result) (target : definition_target) :
    Lsp_t.Location.t option =
  let mk_location ~file_path ~range =
    Some (Lsp_t.Location.create ~uri:(Lsp_t.DocumentUri.of_path file_path) ~range)
  in
  match target with
  | File_start file_path ->
      let zero = Lsp_t.Position.create ~line:0 ~character:0 in
      mk_location ~file_path ~range:(Lsp_t.Range.create ~start:zero ~end_:zero)
  | Span { file_path; start_pos; end_pos } ->
      let source =
        match source_for_file ~analysis ~file_path with
        | Some source -> Some source
        | None -> read_file_source file_path
      in
      Option.map
        (fun source ->
          let range = Lsp_utils.offset_range_to_lsp ~source ~pos:start_pos ~end_pos in
          Lsp_t.Location.create ~uri:(Lsp_t.DocumentUri.of_path file_path) ~range)
        source

let locations ~(analysis : Doc_state.analysis_result) (target : definition_target option) :
    Lsp_t.Locations.t option =
  Option.bind target (fun target ->
      Option.map (fun location -> `Location [ location ]) (location_of_target ~analysis target))

let module_file_path_of_id (analysis : Compiler.entry_analysis) ~(module_id : string) : string option =
  match Compiler.find_checked_module_by_id analysis ~module_id with
  | Some checked_module -> Some checked_module.file_path
  | None -> (
      match analysis.graph with
      | Some graph ->
          Option.map
            (fun (module_ : Compiler.Module_context.parsed_module) -> module_.file_path)
            (Hashtbl.find_opt graph.modules module_id)
      | None -> None)

let module_target_of_id (analysis : Compiler.entry_analysis) ~(module_id : string) : definition_target option =
  Option.map (fun file_path -> File_start file_path) (module_file_path_of_id analysis ~module_id)

let symbol_target (symbol : Marmoset.Lib.Infer.symbol) : definition_target option =
  match Option.bind symbol.file_id Doc_state.file_path_of_file_id with
  | Some file_path ->
      Some (Span { file_path; start_pos = symbol.definition_pos; end_pos = symbol.definition_end_pos })
  | None -> None

let find_member_range_after_receiver
    ~(source : string) ~(receiver_end_pos : int) ~(search_end : int) ~(member_name : string) : (int * int) option
    =
  let len = String.length source in
  let limit = min search_end (len - 1) in
  let rec find_dot i =
    if i > limit then
      None
    else if source.[i] = '.' then
      Some (i + 1)
    else
      find_dot (i + 1)
  in
  Option.bind
    (find_dot (receiver_end_pos + 1))
    (fun after_dot ->
      let start_pos = skip_ascii_spaces ~source ~pos:after_dot in
      let end_pos = start_pos + String.length member_name - 1 in
      if
        end_pos <= limit
        && String.sub source start_pos (String.length member_name) = member_name
        && (end_pos + 1 >= len || not (is_ident_char source.[end_pos + 1]))
      then
        Some (start_pos, end_pos)
      else
        None)

let rec chain_of_expr ~(source : string) (expr : Ast.AST.expression) :
    (Ast.AST.expression * chain_segment list) option =
  match expr.expr with
  | Ast.AST.Identifier name -> Some (expr, [ { name; start_pos = expr.pos; end_pos = expr.end_pos } ])
  | Ast.AST.FieldAccess (receiver, field_name) ->
      Option.bind (chain_of_expr ~source receiver) (fun (root_expr, segments) ->
          Option.map
            (fun (start_pos, end_pos) -> (root_expr, segments @ [ { name = field_name; start_pos; end_pos } ]))
            (find_member_range_after_receiver ~source ~receiver_end_pos:receiver.end_pos ~search_end:expr.end_pos
               ~member_name:field_name))
  | Ast.AST.MethodCall { mc_receiver; mc_method; _ } ->
      Option.bind (chain_of_expr ~source mc_receiver) (fun (root_expr, segments) ->
          Option.map
            (fun (start_pos, end_pos) -> (root_expr, segments @ [ { name = mc_method; start_pos; end_pos } ]))
            (find_member_range_after_receiver ~source ~receiver_end_pos:mc_receiver.end_pos
               ~search_end:expr.end_pos ~member_name:mc_method))
  | Ast.AST.TypeApply (inner, _) -> chain_of_expr ~source inner
  | _ -> None

let chain_segment_index_at_offset (segments : chain_segment list) ~(offset : int) : int option =
  let rec go idx = function
    | [] -> None
    | segment :: rest ->
        if offset >= segment.start_pos && offset <= segment.end_pos then
          Some idx
        else
          go (idx + 1) rest
  in
  go 0 segments

let rec find_namespace_ref_in_expr ~(source : string) ~(offset : int) (expr : Ast.AST.expression) :
    namespace_ref option =
  if offset < expr.pos || offset > expr.end_pos then
    None
  else
    match chain_of_expr ~source expr with
    | Some (root_expr, segments) when List.length segments > 1 -> (
        match chain_segment_index_at_offset segments ~offset with
        | Some cursor_segment_index -> Some { root_expr; segments; cursor_segment_index }
        | None -> find_namespace_ref_in_children ~source ~offset expr)
    | _ -> find_namespace_ref_in_children ~source ~offset expr

and find_namespace_ref_in_children ~(source : string) ~(offset : int) (expr : Ast.AST.expression) :
    namespace_ref option =
  match expr.expr with
  | Ast.AST.Infix (left, _, right) ->
      first_some
        (find_namespace_ref_in_expr ~source ~offset left)
        (find_namespace_ref_in_expr ~source ~offset right)
  | Ast.AST.Prefix (_, e) | Ast.AST.TypeApply (e, _) | Ast.AST.TypeCheck (e, _) ->
      find_namespace_ref_in_expr ~source ~offset e
  | Ast.AST.Call (fn_expr, args) ->
      first_some
        (find_namespace_ref_in_expr ~source ~offset fn_expr)
        (List.find_map (find_namespace_ref_in_expr ~source ~offset) args)
  | Ast.AST.If (cond, then_, else_) ->
      first_some
        (find_namespace_ref_in_expr ~source ~offset cond)
        (first_some
           (find_namespace_ref_in_stmt ~source ~offset then_)
           (Option.bind else_ (find_namespace_ref_in_stmt ~source ~offset)))
  | Ast.AST.Function { body; _ } -> find_namespace_ref_in_stmt ~source ~offset body
  | Ast.AST.Index (arr, idx) ->
      first_some (find_namespace_ref_in_expr ~source ~offset arr) (find_namespace_ref_in_expr ~source ~offset idx)
  | Ast.AST.Array exprs -> List.find_map (find_namespace_ref_in_expr ~source ~offset) exprs
  | Ast.AST.Hash pairs ->
      List.find_map
        (fun (left, right) ->
          first_some
            (find_namespace_ref_in_expr ~source ~offset left)
            (find_namespace_ref_in_expr ~source ~offset right))
        pairs
  | Ast.AST.FieldAccess (receiver, _) -> find_namespace_ref_in_expr ~source ~offset receiver
  | Ast.AST.MethodCall { mc_receiver; mc_args; _ } ->
      first_some
        (find_namespace_ref_in_expr ~source ~offset mc_receiver)
        (List.find_map (find_namespace_ref_in_expr ~source ~offset) mc_args)
  | Ast.AST.Match (scrutinee, arms) ->
      first_some
        (find_namespace_ref_in_expr ~source ~offset scrutinee)
        (List.find_map
           (fun (arm : Ast.AST.match_arm) -> find_namespace_ref_in_expr ~source ~offset arm.body)
           arms)
  | Ast.AST.RecordLit (fields, spread) ->
      first_some
        (List.find_map
           (fun (field : Ast.AST.record_field) ->
             Option.bind field.field_value (find_namespace_ref_in_expr ~source ~offset))
           fields)
        (Option.bind spread (find_namespace_ref_in_expr ~source ~offset))
  | Ast.AST.EnumConstructor (_, _, args) -> List.find_map (find_namespace_ref_in_expr ~source ~offset) args
  | Ast.AST.BlockExpr stmts -> List.find_map (find_namespace_ref_in_stmt ~source ~offset) stmts
  | Ast.AST.Identifier _ | Ast.AST.Integer _ | Ast.AST.Float _ | Ast.AST.Boolean _ | Ast.AST.String _ -> None

and find_namespace_ref_in_stmt ~(source : string) ~(offset : int) (stmt : Ast.AST.statement) :
    namespace_ref option =
  match stmt.stmt with
  | Ast.AST.Let { value; _ } -> find_namespace_ref_in_expr ~source ~offset value
  | Ast.AST.ExpressionStmt expr | Ast.AST.Return expr -> find_namespace_ref_in_expr ~source ~offset expr
  | Ast.AST.Block stmts -> List.find_map (find_namespace_ref_in_stmt ~source ~offset) stmts
  | Ast.AST.ImplDef { impl_methods; _ } ->
      List.find_map
        (fun (method_ : Ast.AST.method_impl) ->
          find_namespace_ref_in_stmt ~source ~offset method_.impl_method_body)
        impl_methods
  | Ast.AST.InherentImplDef { inherent_methods; _ } ->
      List.find_map
        (fun (method_ : Ast.AST.method_impl) ->
          find_namespace_ref_in_stmt ~source ~offset method_.impl_method_body)
        inherent_methods
  | Ast.AST.TraitDef { methods; _ } ->
      List.find_map
        (fun (method_ : Ast.AST.method_sig) ->
          Option.bind method_.method_default_impl (find_namespace_ref_in_expr ~source ~offset))
        methods
  | Ast.AST.ExportDecl _ | Ast.AST.ImportDecl _ -> None
  | Ast.AST.EnumDef _ | Ast.AST.TypeDef _ | Ast.AST.ShapeDef _ | Ast.AST.DeriveDef _ | Ast.AST.TypeAlias _ -> None

let find_namespace_ref_in_program ~(source : string) ~(offset : int) (program : Ast.AST.program) :
    namespace_ref option =
  List.find_map (find_namespace_ref_in_stmt ~source ~offset) program

let lexical_namespace_ref ~(source : string) ~(program : Ast.AST.program) ~(offset : int) : namespace_ref option =
  let tokens = Array.of_list (Lexer.lex source) in
  let len = Array.length tokens in
  let token_idx =
    let rec find idx =
      if idx >= len then
        None
      else
        let tok = tokens.(idx) in
        if tok.token_type = Token.Ident && offset >= tok.pos && offset <= token_end_pos tok then
          Some idx
        else
          find (idx + 1)
    in
    find 0
  in
  Option.bind token_idx (fun current_idx ->
      let rec leftmost idx =
        if idx >= 2 && tokens.(idx - 1).token_type = Token.Dot && tokens.(idx - 2).token_type = Token.Ident then
          leftmost (idx - 2)
        else
          idx
      in
      let start_idx = leftmost current_idx in
      let rec collect idx rev_segments cursor_segment_index next_segment_index =
        if idx >= len || tokens.(idx).token_type <> Token.Ident then
          None
        else
          let tok = tokens.(idx) in
          let segment = { name = tok.literal; start_pos = tok.pos; end_pos = token_end_pos tok } in
          let cursor_segment_index =
            if idx = current_idx then
              Some next_segment_index
            else
              cursor_segment_index
          in
          if idx + 2 < len && tokens.(idx + 1).token_type = Token.Dot && tokens.(idx + 2).token_type = Token.Ident
          then
            collect (idx + 2) (segment :: rev_segments) cursor_segment_index (next_segment_index + 1)
          else
            Some (List.rev (segment :: rev_segments), cursor_segment_index)
      in
      Option.bind (collect start_idx [] None 0) (fun (segments, cursor_segment_index) ->
          match (segments, cursor_segment_index) with
          | _ :: _ :: _, Some cursor_segment_index -> (
              match Hover.find_in_program (List.hd segments).start_pos program with
              | Some ({ expr = Ast.AST.Identifier _; _ } as root_expr) ->
                  Some { root_expr; segments; cursor_segment_index }
              | _ -> None)
          | _ -> None))

let resolve_import_path_target (analysis : Compiler.entry_analysis) ~(path_segments : string list) :
    definition_target option =
  let module_id = String.concat "." path_segments in
  match module_target_of_id analysis ~module_id with
  | Some _ as target -> target
  | None -> (
      match List.rev path_segments with
      | member_name :: rev_module_segments -> (
          match List.rev rev_module_segments with
          | [] -> None
          | module_segments ->
              Option.bind
                (Compiler.find_export_binding analysis ~module_id:(String.concat "." module_segments)
                   ~surface_name:member_name)
                definition_target_of_member_binding)
      | [] -> None)

let resolve_visible_module_target
    (analysis : Compiler.entry_analysis)
    ~(namespace_roots : Import_resolver.namespace_node Import_resolver.StringMap.t)
    ~(segments : string list) : definition_target option =
  match Import_resolver.resolve_namespace_member ~namespace_roots segments with
  | Some `ModulePath -> module_target_of_id analysis ~module_id:(String.concat "." segments)
  | _ -> None

let symbol_path (symbol : Marmoset.Lib.Infer.symbol) : string option =
  Option.bind symbol.file_id Doc_state.file_path_of_file_id

let resolve_namespace_ref
    (analysis : Compiler.entry_analysis)
    ~(active_file_path : string)
    ~(namespace_roots : Import_resolver.namespace_node Import_resolver.StringMap.t)
    (reference : namespace_ref) : definition_target option =
  let segment_names = List.map (fun segment -> segment.name) reference.segments in
  let root_symbol = Compiler.find_active_file_symbol analysis ~expr_id:reference.root_expr.id in
  match root_symbol with
  | Some symbol when Option.fold ~none:true ~some:(String.equal active_file_path) (symbol_path symbol) ->
      if reference.cursor_segment_index = 0 then
        symbol_target symbol
      else
        None
  | Some _ | None -> (
      let visible_prefix count =
        resolve_visible_module_target analysis ~namespace_roots ~segments:(take count segment_names)
      in
      if reference.cursor_segment_index < List.length segment_names - 1 then
        visible_prefix (reference.cursor_segment_index + 1)
      else
        match Import_resolver.resolve_namespace_member ~namespace_roots segment_names with
        | Some `ModulePath -> module_target_of_id analysis ~module_id:(String.concat "." segment_names)
        | Some (`Exported presence) -> definition_target_of_presence presence
        | Some (`NotExported _) | Some (`MissingMember _) | None -> None)

let import_header_target (analysis : Compiler.entry_analysis) ~(source : string) ~(offset : int) :
    definition_target option =
  match Import_header.find_header_target_at_offset ~source ~offset with
  | Some (header, Import_header.Alias) ->
      resolve_import_path_target analysis ~path_segments:(Import_header.path_segment_names header)
  | Some (header, Import_header.Path_segment idx) ->
      let path_segments = Import_header.path_segment_names header in
      if idx < List.length path_segments - 1 then
        module_target_of_id analysis ~module_id:(String.concat "." (take (idx + 1) path_segments))
      else
        resolve_import_path_target analysis ~path_segments
  | None -> None

let expression_target
    (analysis : Compiler.entry_analysis) ~(source : string) ~(program : Ast.AST.program) ~(offset : int) :
    definition_target option =
  let active_module = Compiler.find_checked_module_by_file analysis ~file_path:analysis.active_file.file_path in
  let namespace_ref =
    match find_namespace_ref_in_program ~source ~offset program with
    | Some _ as reference -> reference
    | None -> lexical_namespace_ref ~source ~program ~offset
  in
  match (namespace_ref, active_module) with
  | Some namespace_ref, Some checked_module ->
      resolve_namespace_ref analysis ~active_file_path:analysis.active_file.file_path
        ~namespace_roots:checked_module.navigation.resolved_imports.namespace_roots namespace_ref
  | _ -> (
      match Hover.find_in_program offset program with
      | Some { expr = Ast.AST.Identifier _; id; _ } ->
          Option.bind (Compiler.find_active_file_symbol analysis ~expr_id:id) symbol_target
      | _ -> None)

let find_definition ~(analysis : Doc_state.analysis_result) ~(line : int) ~(character : int) :
    definition_target option =
  let offset = Lsp_utils.position_to_offset ~source:analysis.source ~line ~character in
  match analysis.compiler_analysis with
  | None -> None
  | Some compiler_analysis -> (
      match import_header_target compiler_analysis ~source:analysis.source ~offset with
      | Some _ as target -> target
      | None -> (
          match analysis.program with
          | Some program -> expression_target compiler_analysis ~source:analysis.source ~program ~offset
          | None -> None))

let nth_substring_offset ~(source : string) ~(needle : string) ~(occurrence : int) : int option =
  let rec scan start remaining =
    if remaining <= 0 then
      None
    else if start + String.length needle > String.length source then
      None
    else if String.sub source start (String.length needle) = needle then
      if remaining = 1 then
        Some start
      else
        scan (start + 1) (remaining - 1)
    else
      scan (start + 1) remaining
  in
  scan 0 occurrence

let target_span_of_substring ~(file_path : string) ~(source : string) ~(needle : string) ?(occurrence = 1) () :
    definition_target option =
  Option.map
    (fun start_pos -> Span { file_path; start_pos; end_pos = start_pos + String.length needle - 1 })
    (nth_substring_offset ~source ~needle ~occurrence)

let definition_at
    ?(occurrence = 1) ?(offset_in_needle = 0) ~(file_id : string) ~(source : string) ~(needle : string) () :
    definition_target option =
  let analysis = Doc_state.analyze_with_file_id ~source_root:(Filename.dirname file_id) ~file_id ~source () in
  match nth_substring_offset ~source ~needle ~occurrence with
  | None -> None
  | Some base ->
      let pos = Lsp_utils.offset_to_position ~source ~offset:(base + offset_in_needle) in
      find_definition ~analysis ~line:pos.line ~character:pos.character

let string_of_target = function
  | None -> "None"
  | Some (File_start file_path) -> Printf.sprintf "File_start(%s)" file_path
  | Some (Span { file_path; start_pos; end_pos }) -> Printf.sprintf "Span(%s:%d-%d)" file_path start_pos end_pos

let expect_target ~(label : string) ~(actual : definition_target option) ~(expected : definition_target option) :
    bool =
  if actual = expected then
    true
  else (
    Printf.eprintf "[definition-test] %s\n  actual: %s\n  expected: %s\n%!" label (string_of_target actual)
      (string_of_target expected);
    false)

let%test "direct import usage resolves to exported value span" =
  Doc_state.with_temp_project
    [
      ("main.mr", "import math.add\nadd(1, 2)\n");
      ("math.mr", "export add\nfn add(x: Int, y: Int) -> Int = x + y\n");
    ]
    (fun root ->
      let main_path = Filename.concat root "main.mr" in
      let math_path = Filename.concat root "math.mr" in
      let main_source = "import math.add\nadd(1, 2)\n" in
      let math_source = "export add\nfn add(x: Int, y: Int) -> Int = x + y\n" in
      expect_target ~label:"direct import usage"
        ~actual:(definition_at ~file_id:main_path ~source:main_source ~needle:"add(" ())
        ~expected:
          (target_span_of_substring ~file_path:math_path ~source:math_source
             ~needle:"fn add(x: Int, y: Int) -> Int = x + y" ()))

let%test "direct import alias usage resolves to underlying export span" =
  Doc_state.with_temp_project
    [
      ("main.mr", "import math.add as plus\nplus(1, 2)\n");
      ("math.mr", "export add\nfn add(x: Int, y: Int) -> Int = x + y\n");
    ]
    (fun root ->
      let main_path = Filename.concat root "main.mr" in
      let math_path = Filename.concat root "math.mr" in
      let main_source = "import math.add as plus\nplus(1, 2)\n" in
      let math_source = "export add\nfn add(x: Int, y: Int) -> Int = x + y\n" in
      expect_target ~label:"direct import alias usage"
        ~actual:(definition_at ~file_id:main_path ~source:main_source ~needle:"plus(" ())
        ~expected:
          (target_span_of_substring ~file_path:math_path ~source:math_source
             ~needle:"fn add(x: Int, y: Int) -> Int = x + y" ()))

let%test "import header alias resolves to imported target" =
  Doc_state.with_temp_project
    [
      ("main.mr", "import math.add as plus\nplus(1, 2)\n");
      ("math.mr", "export add\nfn add(x: Int, y: Int) -> Int = x + y\n");
    ]
    (fun root ->
      let main_path = Filename.concat root "main.mr" in
      let math_path = Filename.concat root "math.mr" in
      let main_source = "import math.add as plus\nplus(1, 2)\n" in
      let math_source = "export add\nfn add(x: Int, y: Int) -> Int = x + y\n" in
      expect_target ~label:"import header alias"
        ~actual:(definition_at ~file_id:main_path ~source:main_source ~needle:"plus" ())
        ~expected:
          (target_span_of_substring ~file_path:math_path ~source:math_source
             ~needle:"fn add(x: Int, y: Int) -> Int = x + y" ()))

let%test "namespace receiver resolves to module file start" =
  Doc_state.with_temp_project
    [
      ("main.mr", "import math\nmath.add(1, 2)\n");
      ("math.mr", "export add\nfn add(x: Int, y: Int) -> Int = x + y\n");
    ]
    (fun root ->
      let main_path = Filename.concat root "main.mr" in
      let main_source = "import math\nmath.add(1, 2)\n" in
      expect_target ~label:"namespace receiver"
        ~actual:(definition_at ~file_id:main_path ~source:main_source ~needle:"math.add" ())
        ~expected:(Some (File_start (Filename.concat root "math.mr"))))

let%test "namespace member resolves to exported span" =
  Doc_state.with_temp_project
    [
      ("main.mr", "import math\nmath.add(1, 2)\n");
      ("math.mr", "export add\nfn add(x: Int, y: Int) -> Int = x + y\n");
    ]
    (fun root ->
      let main_path = Filename.concat root "main.mr" in
      let math_path = Filename.concat root "math.mr" in
      let main_source = "import math\nmath.add(1, 2)\n" in
      let math_source = "export add\nfn add(x: Int, y: Int) -> Int = x + y\n" in
      expect_target ~label:"namespace member"
        ~actual:(definition_at ~file_id:main_path ~source:main_source ~needle:"math.add" ~offset_in_needle:5 ())
        ~expected:
          (target_span_of_substring ~file_path:math_path ~source:math_source
             ~needle:"fn add(x: Int, y: Int) -> Int = x + y" ()))

let%test "shadowed namespace resolves to the local binding" =
  Doc_state.with_temp_project
    [
      ("main.mr", "import math\nlet math = { add: (x: Int) -> x }\nmath.add(1)\n");
      ("math.mr", "export add\nfn add(x: Int) -> Int = x\n");
    ]
    (fun root ->
      let main_path = Filename.concat root "main.mr" in
      let main_source = "import math\nlet math = { add: (x: Int) -> x }\nmath.add(1)\n" in
      expect_target ~label:"shadowed namespace"
        ~actual:(definition_at ~file_id:main_path ~source:main_source ~needle:"math.add" ())
        ~expected:
          (target_span_of_substring ~file_path:main_path ~source:main_source
             ~needle:"let math = { add: (x: Int) -> x }" ()))

let%test "private import headers return none" =
  Doc_state.with_temp_project
    [ ("main.mr", "import math.secret\nsecret()\n"); ("math.mr", "fn secret() -> Int = 1\n") ]
    (fun root ->
      let main_path = Filename.concat root "main.mr" in
      let main_source = "import math.secret\nsecret()\n" in
      definition_at ~file_id:main_path ~source:main_source ~needle:"secret" () = None)
