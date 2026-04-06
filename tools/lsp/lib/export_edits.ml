module Lsp_t = Linol_lsp.Types
module Parser = Marmoset.Lib.Parser
module Surface = Marmoset.Lib.Surface_ast.Surface

type declaration_kind =
  | Let_decl
  | Fn_decl
  | Type_decl
  | Shape_decl
  | Trait_decl

type exportable_decl = {
  surface_name : string;
  declaration_kind : declaration_kind;
  name_ref : Surface.name_ref;
}

type visibility =
  | Public
  | Private

let exportable_declarations (program : Surface.surface_program) : exportable_decl list =
  List.filter_map
    (fun (stmt : Surface.surface_top_stmt) ->
      match stmt.std_decl with
      | Surface.SLet { name; name_ref; _ } -> Some { surface_name = name; declaration_kind = Let_decl; name_ref }
      | Surface.SFnDecl { name; name_ref; _ } -> Some { surface_name = name; declaration_kind = Fn_decl; name_ref }
      | Surface.STypeDef { type_name; type_name_ref; _ } ->
          Some { surface_name = type_name; declaration_kind = Type_decl; name_ref = type_name_ref }
      | Surface.SShapeDef { shape_name; shape_name_ref; _ } ->
          Some { surface_name = shape_name; declaration_kind = Shape_decl; name_ref = shape_name_ref }
      | Surface.STraitDef { name; name_ref; _ } -> Some { surface_name = name; declaration_kind = Trait_decl; name_ref }
      | Surface.SExportDecl _ | Surface.SImportDecl _ | Surface.SAmbiguousImplDef _ | Surface.SInherentImplDef _
      | Surface.SExpressionStmt _ | Surface.SReturn _ | Surface.SBlock _ ->
          None)
    program

let declaration_range ~(source : string) (decl : exportable_decl) : Lsp_t.Range.t =
  Lsp_utils.offset_range_to_lsp ~source ~pos:decl.name_ref.pos ~end_pos:decl.name_ref.end_pos

let current_exports (program : Surface.surface_program) : string list =
  List.find_map
    (fun (stmt : Surface.surface_top_stmt) ->
      match stmt.std_decl with
      | Surface.SExportDecl names -> Some names
      | _ -> None)
    program
  |> Option.value ~default:[]

let is_exported ~(exports : string list) (decl : exportable_decl) : bool =
  List.exists (String.equal decl.surface_name) exports

let export_header_stmt (program : Surface.surface_program) : (Surface.surface_top_stmt * string list) option =
  List.find_map
    (fun (stmt : Surface.surface_top_stmt) ->
      match stmt.std_decl with
      | Surface.SExportDecl names -> Some (stmt, names)
      | _ -> None)
    program

let replace_edit ~(source : string) ~(start_pos : int) ~(end_pos : int) ~(new_text : string) : Lsp_t.TextEdit.t =
  let range = Lsp_utils.offset_range_to_lsp ~source ~pos:start_pos ~end_pos in
  Lsp_t.TextEdit.create ~newText:new_text ~range

let insert_edit ~(source : string) ~(offset : int) ~(new_text : string) : Lsp_t.TextEdit.t =
  let pos = Lsp_utils.offset_to_position ~source ~offset in
  let range = Lsp_t.Range.create ~start:pos ~end_:pos in
  Lsp_t.TextEdit.create ~newText:new_text ~range

let skip_inline_space ~(source : string) ~(offset : int) : int =
  let len = String.length source in
  let rec go i =
    if i < len && (source.[i] = ' ' || source.[i] = '\t') then
      go (i + 1)
    else
      i
  in
  go offset

let export_stmt_end ~(source : string) (stmt : Surface.surface_top_stmt) : int * bool =
  let candidate = skip_inline_space ~source ~offset:(stmt.std_end_pos + 1) in
  if candidate < String.length source && source.[candidate] = ';' then
    (candidate, true)
  else
    (stmt.std_end_pos, false)

let delete_end_after_newline ~(source : string) ~(end_pos : int) : int =
  let next = end_pos + 1 in
  if next + 1 < String.length source && source.[next] = '\r' && source.[next + 1] = '\n' then
    next + 1
  else if next < String.length source && (source.[next] = '\n' || source.[next] = '\r') then
    next
  else
    end_pos

let render_export_header ~(names : string list) ~(has_semicolon : bool) : string =
  let suffix =
    if has_semicolon then
      ";"
    else
      ""
  in
  "export " ^ String.concat ", " names ^ suffix

let edit_for_visibility
    ~(source : string)
    ~(program : Surface.surface_program)
    ~(decl : exportable_decl)
    ~(visibility : visibility) : Lsp_t.TextEdit.t list option =
  let exports = current_exports program in
  let already_exported = is_exported ~exports decl in
  match (visibility, already_exported, export_header_stmt program) with
  | Public, true, _ | Private, false, _ -> None
  | Public, false, Some (header_stmt, header_names) ->
      let end_pos, has_semicolon = export_stmt_end ~source header_stmt in
      let updated = render_export_header ~names:(header_names @ [ decl.surface_name ]) ~has_semicolon in
      Some [ replace_edit ~source ~start_pos:header_stmt.std_pos ~end_pos ~new_text:updated ]
  | Public, false, None ->
      let insert_at =
        match program with
        | first_stmt :: _ -> first_stmt.std_pos
        | [] -> String.length source
      in
      Some [ insert_edit ~source ~offset:insert_at ~new_text:("export " ^ decl.surface_name ^ "\n") ]
  | Private, true, Some (header_stmt, header_names) ->
      let remaining = List.filter (fun name -> not (String.equal name decl.surface_name)) header_names in
      let end_pos, has_semicolon = export_stmt_end ~source header_stmt in
      if remaining = [] then
        let delete_end = delete_end_after_newline ~source ~end_pos in
        Some [ replace_edit ~source ~start_pos:header_stmt.std_pos ~end_pos:delete_end ~new_text:"" ]
      else
        let updated = render_export_header ~names:remaining ~has_semicolon in
        Some [ replace_edit ~source ~start_pos:header_stmt.std_pos ~end_pos ~new_text:updated ]
  | Private, true, None -> None

let parse_surface_program ~(source : string) : Surface.surface_program =
  match Parser.parse_with_surface ~file_id:"/tmp/export_edits_test.mr" source with
  | Ok result -> result.surface_program
  | Error diagnostics ->
      let message =
        diagnostics
        |> List.map (fun (diag : Marmoset.Lib.Diagnostic.t) -> diag.message)
        |> String.concat "; "
      in
      failwith ("parse failed: " ^ message)

let declaration_named ~(source : string) ~(name : string) : exportable_decl =
  let program = parse_surface_program ~source in
  match List.find_opt (fun (decl : exportable_decl) -> String.equal decl.surface_name name) (exportable_declarations program) with
  | Some decl -> decl
  | None -> failwith ("missing exportable declaration: " ^ name)

let apply_text_edits ~(source : string) (edits : Lsp_t.TextEdit.t list) : string =
  let edits_with_offsets =
    List.map
      (fun (edit : Lsp_t.TextEdit.t) ->
        let start_offset =
          Lsp_utils.position_to_offset ~source ~line:edit.range.start.line ~character:edit.range.start.character
        in
        let end_offset =
          Lsp_utils.position_to_offset ~source ~line:edit.range.end_.line ~character:edit.range.end_.character
        in
        (start_offset, end_offset, edit.newText))
      edits
    |> List.sort (fun (left_start, _, _) (right_start, _, _) -> compare right_start left_start)
  in
  List.fold_left
    (fun acc (start_offset, end_offset, new_text) ->
      String.sub acc 0 start_offset ^ new_text
      ^ String.sub acc end_offset (String.length acc - end_offset))
    source edits_with_offsets

let updated_source ~(source : string) ~(name : string) ~(visibility : visibility) : string option =
  let program = parse_surface_program ~source in
  let decl = declaration_named ~source ~name in
  Option.map (apply_text_edits ~source) (edit_for_visibility ~source ~program ~decl ~visibility)

let declaration_names decls = List.map (fun (decl : exportable_decl) -> decl.surface_name) decls

let%test "exportable_declarations includes only top-level exportable declarations" =
  let source =
    "let top = 1\nfn greet(name: Str) -> Str = { let local = name\nlocal }\ntype Point = { x: Int }\nshape Named = { name: Str }\ntrait Show[a] = { fn show(self: a) -> Str }\nimpl Point = { fn helper(self: Point) -> Int = self.x }\n"
  in
  let decls = exportable_declarations (parse_surface_program ~source) in
  declaration_names decls = [ "top"; "greet"; "Point"; "Named"; "Show" ]

let%test "edit_for_visibility appends a name to an existing export header" =
  let source = "export foo\nfn foo() -> Int = 1\nfn bar() -> Int = 2\n" in
  updated_source ~source ~name:"bar" ~visibility:Public = Some "export foo, bar\nfn foo() -> Int = 1\nfn bar() -> Int = 2\n"

let%test "edit_for_visibility creates an export header before imports" =
  let source = "import math\nfn foo() -> Int = math.add(1, 2)\n" in
  updated_source ~source ~name:"foo" ~visibility:Public
  = Some "export foo\nimport math\nfn foo() -> Int = math.add(1, 2)\n"

let%test "edit_for_visibility creates an export header after leading comments and blanks" =
  let source = "# heading\n\nfn foo() -> Int = 1\n" in
  updated_source ~source ~name:"foo" ~visibility:Public = Some "# heading\n\nexport foo\nfn foo() -> Int = 1\n"

let%test "edit_for_visibility removes a middle export name" =
  let source = "export foo, bar, baz\nfn foo() -> Int = 1\nfn bar() -> Int = 2\nfn baz() -> Int = 3\n" in
  updated_source ~source ~name:"bar" ~visibility:Private
  = Some "export foo, baz\nfn foo() -> Int = 1\nfn bar() -> Int = 2\nfn baz() -> Int = 3\n"

let%test "edit_for_visibility removes the final export header and one trailing newline" =
  let source = "export foo\nfn foo() -> Int = 1\n" in
  updated_source ~source ~name:"foo" ~visibility:Private = Some "fn foo() -> Int = 1\n"

let%test "edit_for_visibility returns no edit when visibility already matches reality" =
  let source = "export foo\nfn foo() -> Int = 1\nfn bar() -> Int = 2\n" in
  updated_source ~source ~name:"foo" ~visibility:Public = None
  && updated_source ~source ~name:"bar" ~visibility:Private = None
