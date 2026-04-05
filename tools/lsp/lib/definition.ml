module Lsp_t = Linol_lsp.Types
module Ast = Marmoset.Lib.Ast
module Compiler = Marmoset.Lib.Frontend_compiler
module Cursor_context = Cursor_context
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

let exact_type_target_of_presence
    (analysis : Compiler.entry_analysis)
    ~(surface_name : string)
    (presence : Import_resolver.member_presence) : definition_target option =
  let fallback = definition_target_of_presence presence in
  match Compiler.parsed_module_of_presence analysis presence with
  | None -> fallback
  | Some parsed_module ->
      first_some
        (Option.map definition_target_of_site
           (Compiler.find_type_head_site_in_surface_program parsed_module.surface_program ~surface_name))
        fallback

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
  let module_ref_of_segments roots = function
    | [] -> None
    | root :: rest -> (
        match Import_resolver.StringMap.find_opt root roots with
        | None -> None
        | Some node ->
            let rec walk current = function
              | [] -> current.Import_resolver.module_ref
              | segment :: tail -> (
                  match Import_resolver.StringMap.find_opt segment current.children with
                  | None -> None
                  | Some next -> walk next tail)
            in
            walk node rest)
  in
  Option.bind
    (module_ref_of_segments namespace_roots segments)
    (fun (module_surface : Import_resolver.module_surface) ->
      module_target_of_id analysis ~module_id:module_surface.module_id)

let symbol_path (symbol : Marmoset.Lib.Infer.symbol) : string option =
  Option.bind symbol.file_id Doc_state.file_path_of_file_id

let definition_target_of_exact_site (site : Module_sig.definition_site option) : definition_target option =
  Option.map definition_target_of_site site

let definition_target_of_name_ref (name_ref : Marmoset.Lib.Surface_ast.Surface.name_ref) : definition_target option =
  definition_target_of_exact_site (Compiler.definition_site_of_name_ref name_ref)

let local_variant_target
    ~(surface_program : Marmoset.Lib.Surface_ast.Surface.surface_program option)
    ~(type_name : string)
    ~(variant_name : string) : definition_target option =
  Option.bind surface_program (fun program ->
      List.find_map
        (fun (stmt : Marmoset.Lib.Surface_ast.Surface.surface_top_stmt) ->
          match stmt.std_decl with
          | Marmoset.Lib.Surface_ast.Surface.STypeDef
              {
                type_name = candidate_type;
                type_body = Marmoset.Lib.Surface_ast.Surface.STNamedSum variants;
                _;
              }
            when String.equal candidate_type type_name ->
              List.find_map
                (fun (variant : Marmoset.Lib.Surface_ast.Surface.surface_variant_def) ->
                  if String.equal variant.sv_name variant_name then
                    definition_target_of_name_ref variant.sv_name_ref
                  else
                    None)
                variants
          | _ -> None)
        program)

let cursor_input_of_analysis (analysis : Doc_state.analysis_result) : Cursor_context.cursor_context_input option =
  match (analysis.surface_program, analysis.program) with
  | Some surface_program, Some lowered_program ->
      Some
        {
          Cursor_context.surface_program;
          lowered_program;
          scope_index = Cursor_context.build_scope_index surface_program;
        }
  | _ -> None

let namespace_roots_of_analysis (analysis : Compiler.entry_analysis) :
    Import_resolver.namespace_node Import_resolver.StringMap.t option =
  Option.map
    (fun (module_ : Compiler.checked_module) -> module_.navigation.resolved_imports.namespace_roots)
    (Compiler.find_checked_module_by_file analysis ~file_path:analysis.active_file.file_path)

let same_file_symbol_target (analysis : Compiler.entry_analysis) ~(expr_id : int) : definition_target option =
  match Compiler.find_active_file_symbol analysis ~expr_id with
  | Some symbol when Option.fold ~none:true ~some:(String.equal analysis.active_file.file_path) (symbol_path symbol) ->
      symbol_target symbol
  | Some _ | None -> None

let same_file_symbol (analysis : Compiler.entry_analysis) ~(expr_id : int) : Marmoset.Lib.Infer.symbol option =
  match Compiler.find_active_file_symbol analysis ~expr_id with
  | Some symbol when Option.fold ~none:true ~some:(String.equal analysis.active_file.file_path) (symbol_path symbol) ->
      Some symbol
  | Some _ | None -> None

let value_namespace_symbol_target (symbol : Marmoset.Lib.Infer.symbol) : definition_target option =
  match symbol.kind with
  | Marmoset.Lib.Infer.BuiltinValue
  | Marmoset.Lib.Infer.TopLevelLet
  | Marmoset.Lib.Infer.LocalLet
  | Marmoset.Lib.Infer.Param
  | Marmoset.Lib.Infer.PatternVar
  | Marmoset.Lib.Infer.ImplMethodParam ->
      symbol_target symbol
  | Marmoset.Lib.Infer.TypeSym
  | Marmoset.Lib.Infer.TypeAliasSym
  | Marmoset.Lib.Infer.ShapeSym
  | Marmoset.Lib.Infer.TraitSym
  | Marmoset.Lib.Infer.EnumSym
  | Marmoset.Lib.Infer.EnumVariantSym ->
      None

let cursor_reference_target
    (analysis : Compiler.entry_analysis)
    ~(source : string)
    ~(surface_program : Marmoset.Lib.Surface_ast.Surface.surface_program option)
    ~(reference : Cursor_context.reference) : definition_target option =
  let _ = source in
  let active_file_path = analysis.active_file.file_path in
  match reference with
  | Cursor_context.Import_alias { import_path; _ } -> resolve_import_path_target analysis ~path_segments:import_path
  | Cursor_context.Import_path_segment { import_path; segment_index; _ } ->
      if segment_index < List.length import_path - 1 then
        module_target_of_id analysis ~module_id:(String.concat "." (take (segment_index + 1) import_path))
      else
        resolve_import_path_target analysis ~path_segments:import_path
  | Cursor_context.Declaration_head { name_ref; _ } -> definition_target_of_name_ref name_ref
  | Cursor_context.Type_identifier { binding = Some binding; _ } -> definition_target_of_name_ref binding.binding_ref
  | Cursor_context.Type_path_segment { path_segments; segment_index; _ } -> (
      match namespace_roots_of_analysis analysis with
      | None -> None
      | Some namespace_roots ->
          if segment_index < List.length path_segments - 1 then
            resolve_visible_module_target analysis ~namespace_roots ~segments:(take (segment_index + 1) path_segments)
          else
            match Import_resolver.resolve_namespace_member ~namespace_roots path_segments with
            | Some `ModulePath -> resolve_visible_module_target analysis ~namespace_roots ~segments:path_segments
            | Some (`Exported presence)
              when Compiler.presence_has_type_namespace presence
                   || Compiler.presence_has_constraint_namespace presence ->
                exact_type_target_of_presence analysis
                  ~surface_name:(List.nth path_segments (List.length path_segments - 1))
                  presence
            | Some (`Exported _) | Some (`NotExported _) | Some (`MissingMember _) | None -> None)
  | Cursor_context.Type_identifier { name_ref; binding = None } ->
      definition_target_of_exact_site
        (Compiler.find_visible_type_declaration_site analysis ~file_path:active_file_path ~surface_name:name_ref.text)
  | Cursor_context.Constraint_identifier { name_ref } ->
      definition_target_of_exact_site
        (Compiler.find_visible_constraint_declaration_site analysis ~file_path:active_file_path
           ~surface_name:name_ref.text)
  | Cursor_context.Value_identifier { expr_id; name_ref; _ } ->
      first_some
        (Option.bind (Compiler.find_active_file_symbol analysis ~expr_id) symbol_target)
        (definition_target_of_exact_site
           (Compiler.find_visible_type_declaration_site analysis ~file_path:active_file_path
              ~surface_name:name_ref.text))
  | Cursor_context.Qualified_root { root_ref; root_expr_id; access_expr_id = _; _ } ->
      let module_root_target =
        Option.bind
          (namespace_roots_of_analysis analysis)
          (fun namespace_roots -> resolve_visible_module_target analysis ~namespace_roots ~segments:[ root_ref.text ])
      in
      first_some
        (Option.bind root_expr_id (fun expr_id -> same_file_symbol_target analysis ~expr_id))
        (match module_root_target with
        | Some _ as target -> target
        | None ->
            first_some
              (definition_target_of_exact_site
                 (Compiler.find_visible_type_declaration_site analysis ~file_path:active_file_path
                    ~surface_name:root_ref.text))
              (definition_target_of_exact_site
                 (Compiler.find_visible_constraint_declaration_site analysis ~file_path:active_file_path
                    ~surface_name:root_ref.text)))
  | Cursor_context.Qualified_member { root_ref; root_expr_id; member_ref; access_expr_id } ->
      let same_file_root_target =
        Option.bind root_expr_id (fun expr_id ->
            Option.bind (same_file_symbol analysis ~expr_id) value_namespace_symbol_target)
      in
      let method_target =
        match Compiler.find_active_file_method_resolution analysis ~expr_id:access_expr_id with
        | Some
            ( Marmoset.Lib.Infer.TraitMethod trait_name
            | Marmoset.Lib.Infer.DynamicTraitMethod trait_name
            | Marmoset.Lib.Infer.QualifiedTraitMethod trait_name ) ->
            definition_target_of_exact_site
              (Compiler.find_trait_method_declaration_site analysis ~trait_name ~method_name:member_ref.text)
        | Some (Marmoset.Lib.Infer.InherentMethod | Marmoset.Lib.Infer.QualifiedInherentMethod) ->
            Option.bind
              (Compiler.resolve_visible_type_name_to_mono analysis ~file_path:active_file_path
                 ~surface_name:root_ref.text)
              (fun receiver_type ->
                definition_target_of_exact_site
                  (Compiler.find_inherent_method_declaration_site analysis ~receiver_type
                     ~method_name:member_ref.text))
        | Some Marmoset.Lib.Infer.FieldFunctionCall | None -> None
      in
      let module_root_target =
        Option.bind (namespace_roots_of_analysis analysis) (fun namespace_roots ->
            match Import_resolver.resolve_namespace_member ~namespace_roots [ root_ref.text ] with
            | Some `ModulePath -> Some ()
            | _ -> None)
      in
      let module_member_target =
        Option.bind (namespace_roots_of_analysis analysis) (fun namespace_roots ->
            match Import_resolver.resolve_namespace_member ~namespace_roots [ root_ref.text; member_ref.text ] with
            | Some `ModulePath ->
                resolve_visible_module_target analysis ~namespace_roots ~segments:[ root_ref.text; member_ref.text ]
            | Some (`Exported presence) -> definition_target_of_presence presence
            | Some (`NotExported _) | Some (`MissingMember _) | None -> None)
      in
      let type_member_target =
        first_some
          (local_variant_target ~surface_program ~type_name:root_ref.text ~variant_name:member_ref.text)
          (definition_target_of_exact_site
             (Compiler.find_visible_variant_declaration_site analysis ~file_path:active_file_path
                ~type_name:root_ref.text ~variant_name:member_ref.text))
      in
      first_some
        method_target
        (first_some same_file_root_target
           (match module_root_target with
           | Some () -> module_member_target
           | None -> first_some type_member_target module_member_target))

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
      match cursor_input_of_analysis analysis with
      | Some input -> (
          let reference =
            match Cursor_context.reference_at ~source:analysis.source ~input ~offset with
            | Some _ as reference -> reference
            | None when offset > 0 -> Cursor_context.reference_at ~source:analysis.source ~input ~offset:(offset - 1)
            | None -> None
          in
          match reference with
          | Some reference ->
              first_some
                (cursor_reference_target compiler_analysis ~source:analysis.source
                   ~surface_program:analysis.surface_program ~reference)
                None
          | None -> (
              match import_header_target compiler_analysis ~source:analysis.source ~offset with
              | Some _ as target -> target
              | None -> (
                  match analysis.program with
                  | Some program -> expression_target compiler_analysis ~source:analysis.source ~program ~offset
                  | None -> None)))
      | None -> (
      match import_header_target compiler_analysis ~source:analysis.source ~offset with
      | Some _ as target -> target
      | None -> (
          match analysis.program with
          | Some program -> expression_target compiler_analysis ~source:analysis.source ~program ~offset
          | None -> None)))

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

let%test "type annotation resolves to named type declaration head" =
  Doc_state.with_temp_project
    [ ("main.mr", "type Point = { x: Int }\nlet p: Point = { x: 1 }\np\n") ]
    (fun root ->
      let main_path = Filename.concat root "main.mr" in
      let main_source = "type Point = { x: Int }\nlet p: Point = { x: 1 }\np\n" in
      expect_target ~label:"type annotation"
        ~actual:(definition_at ~file_id:main_path ~source:main_source ~needle:"Point = { x: 1 }" ())
        ~expected:(target_span_of_substring ~file_path:main_path ~source:main_source ~needle:"Point" ()))

let%test "type annotation resolves when the cursor is at the end of the type name" =
  Doc_state.with_temp_project
    [ ("main.mr", "type Point = { x: Int }\nlet p: Point = { x: 1 }\np\n") ]
    (fun root ->
      let main_path = Filename.concat root "main.mr" in
      let main_source = "type Point = { x: Int }\nlet p: Point = { x: 1 }\np\n" in
      expect_target ~label:"type annotation at end"
        ~actual:(definition_at ~file_id:main_path ~source:main_source ~needle:"Point = { x: 1 }"
                   ~offset_in_needle:5 ())
        ~expected:(target_span_of_substring ~file_path:main_path ~source:main_source ~needle:"Point" ()))

let%test "generic annotation resolves to nearest type parameter declaration" =
  let source = "fn id[t](x: t) -> t = x\nid\n" in
  let file_id = Filename.concat (Filename.get_temp_dir_name ()) "cursor_generic.mr" in
  expect_target ~label:"generic annotation"
    ~actual:(definition_at ~file_id ~source ~needle:"t) ->" ())
    ~expected:(target_span_of_substring ~file_path:file_id ~source ~needle:"t" ())

let%test "wrapper constructor call head resolves to type declaration head" =
  Doc_state.with_temp_project
    [ ("main.mr", "type UserId = UserId(Int)\nlet id = UserId(42)\nid\n") ]
    (fun root ->
      let main_path = Filename.concat root "main.mr" in
      let main_source = "type UserId = UserId(Int)\nlet id = UserId(42)\nid\n" in
      expect_target ~label:"wrapper constructor"
        ~actual:(definition_at ~file_id:main_path ~source:main_source ~needle:"UserId(42)" ())
        ~expected:(target_span_of_substring ~file_path:main_path ~source:main_source ~needle:"UserId" ()))

let%test "enum constructor member resolves to the variant declaration head" =
  Doc_state.with_temp_project
    [ ("main.mr", "type Option[a] = { Some(a), None }\nlet x = Option.Some(42)\nx\n") ]
    (fun root ->
      let main_path = Filename.concat root "main.mr" in
      let main_source = "type Option[a] = { Some(a), None }\nlet x = Option.Some(42)\nx\n" in
      expect_target ~label:"enum constructor member"
        ~actual:(definition_at ~file_id:main_path ~source:main_source ~needle:"Option.Some" ~offset_in_needle:7 ())
        ~expected:(target_span_of_substring ~file_path:main_path ~source:main_source ~needle:"Some" ()))

let%test "constraint annotation resolves to trait declaration head" =
  Doc_state.with_temp_project
    [ ("main.mr", "trait Named[a] = { fn label(self: a) -> Str }\nfn show[t: Named](x: t) -> t = x\nshow\n") ]
    (fun root ->
      let main_path = Filename.concat root "main.mr" in
      let main_source =
        "trait Named[a] = { fn label(self: a) -> Str }\nfn show[t: Named](x: t) -> t = x\nshow\n"
      in
      expect_target ~label:"constraint annotation"
        ~actual:(definition_at ~file_id:main_path ~source:main_source ~needle:"Named](x" ())
        ~expected:(target_span_of_substring ~file_path:main_path ~source:main_source ~needle:"Named" ()))

let%test "qualified trait method resolves to the trait method declaration head" =
  Doc_state.with_temp_project
    [
      ( "main.mr",
        "trait Greeter[a] = { fn greet(self: a, prefix: Str) -> Str }\n\
         type Monkey = { name: Str }\n\
         impl Greeter[Monkey] = { fn greet(self: Monkey, prefix: Str) -> Str = prefix + self.name }\n\
         let monkey = { name: \"George\" }\n\
         let x = Greeter.greet(monkey, \"hi\")\n\
         x\n" );
    ]
    (fun root ->
      let main_path = Filename.concat root "main.mr" in
      let main_source =
        "trait Greeter[a] = { fn greet(self: a, prefix: Str) -> Str }\n\
         type Monkey = { name: Str }\n\
         impl Greeter[Monkey] = { fn greet(self: Monkey, prefix: Str) -> Str = prefix + self.name }\n\
         let monkey = { name: \"George\" }\n\
         let x = Greeter.greet(monkey, \"hi\")\n\
         x\n"
      in
      expect_target ~label:"qualified trait method"
        ~actual:(definition_at ~file_id:main_path ~source:main_source ~needle:"Greeter.greet" ~offset_in_needle:8 ())
        ~expected:(target_span_of_substring ~file_path:main_path ~source:main_source ~needle:"greet" ()))

let%test "qualified inherent method resolves to the impl method declaration head" =
  Doc_state.with_temp_project
    [
      ( "main.mr",
        "type Monkey = { name: Str }\n\
         impl Monkey = { fn rename(self: Monkey, next_name: Str) -> Monkey = self }\n\
         let monkey = { name: \"George\" }\n\
         let x = Monkey.rename(monkey, \"Bob\")\n\
         x\n" );
    ]
    (fun root ->
      let main_path = Filename.concat root "main.mr" in
      let main_source =
        "type Monkey = { name: Str }\n\
         impl Monkey = { fn rename(self: Monkey, next_name: Str) -> Monkey = self }\n\
         let monkey = { name: \"George\" }\n\
         let x = Monkey.rename(monkey, \"Bob\")\n\
         x\n"
      in
      expect_target ~label:"qualified inherent method"
        ~actual:(definition_at ~file_id:main_path ~source:main_source ~needle:"Monkey.rename" ~offset_in_needle:7 ())
        ~expected:(target_span_of_substring ~file_path:main_path ~source:main_source ~needle:"rename" ()))

let%test "namespace alias receiver resolves to module file start outside import headers" =
  Doc_state.with_temp_project
    [
      ("main.mr", "import types.geo\nlet p = { x: 1, y: 2 }\nputs(geo.render_point(p))\n");
      ( "types/geo.mr",
        "export Point, HasXY, Drawable, render_point\n\
         type Point = { x: Int, y: Int }\n\
         shape HasXY = { x: Int, y: Int }\n\
         trait Drawable[a] = { fn draw(x: a) -> Str }\n\
         impl Drawable[Point] = { fn draw(p: Point) -> Str = \"point\" }\n\
         fn render_point(p: Point) -> Str = Drawable.draw(p)\n" );
    ]
    (fun root ->
      let main_path = Filename.concat root "main.mr" in
      let main_source = "import types.geo\nlet p = { x: 1, y: 2 }\nputs(geo.render_point(p))\n" in
      expect_target ~label:"namespace alias receiver in expression"
        ~actual:(definition_at ~file_id:main_path ~source:main_source ~needle:"geo.render_point" ())
        ~expected:(Some (File_start (Filename.concat root "types/geo.mr"))))

let%test "qualified type namespace root resolves to imported module file start" =
  Doc_state.with_temp_project
    [
      ("main.mr", "import types.geo\nlet p: geo.Point = { x: 1, y: 2 }\n");
      ("types/geo.mr", "export Point\ntype Point = { x: Int, y: Int }\n");
    ]
    (fun root ->
      let main_path = Filename.concat root "main.mr" in
      let main_source = "import types.geo\nlet p: geo.Point = { x: 1, y: 2 }\n" in
      expect_target ~label:"qualified type namespace root"
        ~actual:(definition_at ~file_id:main_path ~source:main_source ~needle:"geo.Point" ())
        ~expected:(Some (File_start (Filename.concat root "types/geo.mr"))))

let%test "qualified named type member resolves to exported type declaration head" =
  Doc_state.with_temp_project
    [
      ("main.mr", "import types.geo\nlet p: geo.Point = { x: 1, y: 2 }\n");
      ("types/geo.mr", "export Point\ntype Point = { x: Int, y: Int }\n");
    ]
    (fun root ->
      let main_path = Filename.concat root "main.mr" in
      let geo_path = Filename.concat root "types/geo.mr" in
      let main_source = "import types.geo\nlet p: geo.Point = { x: 1, y: 2 }\n" in
      let geo_source = "export Point\ntype Point = { x: Int, y: Int }\n" in
      expect_target ~label:"qualified named type member"
        ~actual:(definition_at ~file_id:main_path ~source:main_source ~needle:"geo.Point" ~offset_in_needle:4 ())
        ~expected:(target_span_of_substring ~file_path:geo_path ~source:geo_source ~needle:"Point" ~occurrence:2 ()))

let%test "qualified shape member resolves to exported shape declaration head" =
  Doc_state.with_temp_project
    [
      ("main.mr", "import types.geo\nlet p = { x: 1, y: 2 }\nlet q: geo.HasXY = p\n");
      ("types/geo.mr", "export HasXY\nshape HasXY = { x: Int, y: Int }\n");
    ]
    (fun root ->
      let main_path = Filename.concat root "main.mr" in
      let geo_path = Filename.concat root "types/geo.mr" in
      let main_source = "import types.geo\nlet p = { x: 1, y: 2 }\nlet q: geo.HasXY = p\n" in
      let geo_source = "export HasXY\nshape HasXY = { x: Int, y: Int }\n" in
      expect_target ~label:"qualified shape member"
        ~actual:(definition_at ~file_id:main_path ~source:main_source ~needle:"geo.HasXY" ~offset_in_needle:4 ())
        ~expected:(target_span_of_substring ~file_path:geo_path ~source:geo_source ~needle:"HasXY" ~occurrence:2 ()))

let%test "qualified transparent type member resolves to exported alias declaration head" =
  Doc_state.with_temp_project
    [
      ("main.mr", "import types.wrappers\nlet xs: wrappers.Users = wrappers.empty_users()\n");
      ("types/wrappers.mr", "export Users, empty_users\ntype Users = List[Int]\nfn empty_users() -> Users = []\n");
    ]
    (fun root ->
      let main_path = Filename.concat root "main.mr" in
      let wrappers_path = Filename.concat root "types/wrappers.mr" in
      let main_source = "import types.wrappers\nlet xs: wrappers.Users = wrappers.empty_users()\n" in
      let wrappers_source = "export Users, empty_users\ntype Users = List[Int]\nfn empty_users() -> Users = []\n" in
      expect_target ~label:"qualified transparent type member"
        ~actual:(definition_at ~file_id:main_path ~source:main_source ~needle:"wrappers.Users" ~offset_in_needle:9 ())
        ~expected:
          (target_span_of_substring ~file_path:wrappers_path ~source:wrappers_source ~needle:"Users"
             ~occurrence:2 ()))

let%test "definition on a declaration head returns that declaration span" =
  Doc_state.with_temp_project
    [ ("main.mr", "fn add(x: Int) -> Int = x\nadd(1)\n") ]
    (fun root ->
      let main_path = Filename.concat root "main.mr" in
      let main_source = "fn add(x: Int) -> Int = x\nadd(1)\n" in
      expect_target ~label:"declaration head"
        ~actual:(definition_at ~file_id:main_path ~source:main_source ~needle:"add(x" ())
        ~expected:(target_span_of_substring ~file_path:main_path ~source:main_source ~needle:"add" ()))

let%test "builtin primitive type returns no definition target" =
  Doc_state.with_temp_project
    [ ("main.mr", "let x: Int = 1\nx\n") ]
    (fun root ->
      let main_path = Filename.concat root "main.mr" in
      let main_source = "let x: Int = 1\nx\n" in
      definition_at ~file_id:main_path ~source:main_source ~needle:"Int" () = None)
