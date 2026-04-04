module Diagnostic = Diagnostics.Diagnostic
module AST = Syntax.Ast.AST
module Module_context = Module_context

let id_stride = 1_000_000

type discovery_state = {
  root_dir : string;
  modules : (string, Module_context.parsed_module) Hashtbl.t;
  dependencies : (string, string list) Hashtbl.t;
  source_overrides : (string, string) Hashtbl.t;
  next_file_index : int ref;
}

let ( let* ) = Result.bind

let starts_with ~(prefix : string) (s : string) : bool =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len && String.sub s 0 prefix_len = prefix

let option_map_default opt ~default ~f =
  match opt with
  | None -> default
  | Some value -> f value

let normalize_path (path : string) : string =
  if Filename.is_relative path then
    Filename.concat (Sys.getcwd ()) path
  else
    path

let read_file (path : string) : string =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let source_for_path (state : discovery_state) (path : string) : string =
  let path = normalize_path path in
  match Hashtbl.find_opt state.source_overrides path with
  | Some source -> source
  | None -> read_file path

let relative_to_root ~(root_dir : string) (path : string) : string =
  let prefix = root_dir ^ Filename.dir_sep in
  if starts_with ~prefix path then
    String.sub path (String.length prefix) (String.length path - String.length prefix)
  else if String.equal root_dir path then
    ""
  else
    failwith (Printf.sprintf "Path %s is not under root %s" path root_dir)

let module_id_of_file ~(root_dir : string) (file_path : string) : string =
  let relative = relative_to_root ~root_dir file_path in
  let without_ext =
    if Filename.check_suffix relative ".mr" then
      Filename.chop_suffix relative ".mr"
    else
      relative
  in
  String.split_on_char Filename.dir_sep.[0] without_ext |> String.concat "."

let module_file_path ~(root_dir : string) (parts : string list) : string =
  Filename.concat root_dir (String.concat Filename.dir_sep parts ^ ".mr")

let import_path_string (parts : string list) : string = String.concat "." parts

let export_list_of_program (program : AST.program) : string list =
  let rec first_export = function
    | [] -> []
    | { AST.stmt = AST.ExportDecl names; _ } :: _ -> names
    | _ :: rest -> first_export rest
  in
  first_export program

let imports_of_program (program : AST.program) : Module_context.import_info list =
  List.filter_map
    (fun (stmt : AST.statement) ->
      match stmt.stmt with
      | AST.ImportDecl { import_path; import_alias } ->
          Some
            {
              Module_context.import_path;
              import_alias;
              file_id = stmt.file_id;
              start_pos = stmt.pos;
              end_pos = stmt.end_pos;
            }
      | _ -> None)
    program

let import_error (imp : Module_context.import_info) ~(code : string) ~(message : string) : Diagnostic.t =
  match imp.file_id with
  | Some file_id ->
      Diagnostic.error_with_span ~code ~message ~file_id ~start_pos:imp.start_pos ~end_pos:imp.end_pos ()
  | None -> Diagnostic.error_no_span ~code ~message

let exported_name_exists (module_info : Module_context.parsed_module) (name : string) : bool =
  List.mem name module_info.exports

let rec discover_module (state : discovery_state) ~(module_id : string) ~(file_path : string) :
    (unit, Diagnostic.t) result =
  if Hashtbl.mem state.modules module_id then
    Ok ()
  else
    let source = source_for_path state file_path in
    let file_index = !(state.next_file_index) in
    state.next_file_index := file_index + 1;
    let id_offset = file_index * id_stride in
    let* program =
      match Syntax.Parser.parse ~id_offset ~file_id:file_path source with
      | Ok program -> Ok program
      | Error diagnostics -> Error (List.hd diagnostics)
    in
    let parsed_module =
      {
        Module_context.module_id;
        file_path;
        source;
        program;
        exports = export_list_of_program program;
        imports = imports_of_program program;
      }
    in
    Hashtbl.replace state.modules module_id parsed_module;
    let* deps =
      List.fold_left
        (fun acc import_info ->
          let* acc = acc in
          let* dep = resolve_import state import_info in
          Ok (dep :: acc))
        (Ok []) parsed_module.imports
    in
    Hashtbl.replace state.dependencies module_id (List.rev deps);
    Ok ()

and resolve_import (state : discovery_state) (imp : Module_context.import_info) : (string, Diagnostic.t) result =
  let full_module_path = module_file_path ~root_dir:state.root_dir imp.import_path in
  let module_candidate_exists = Sys.file_exists full_module_path in
  let parent_candidate =
    match List.rev imp.import_path with
    | _member :: rev_parent when rev_parent <> [] ->
        let parent_parts = List.rev rev_parent in
        let member_name = List.hd (List.rev imp.import_path) in
        Some (parent_parts, member_name, module_file_path ~root_dir:state.root_dir parent_parts)
    | _ -> None
  in
  let* member_candidate =
    match parent_candidate with
    | None -> Ok None
    | Some (parent_parts, member_name, parent_file_path) ->
        if Sys.file_exists parent_file_path then
          let parent_module_id = import_path_string parent_parts in
          let* () = discover_module state ~module_id:parent_module_id ~file_path:parent_file_path in
          let parent_module = Hashtbl.find state.modules parent_module_id in
          Ok (Some (parent_module_id, parent_module, member_name))
        else
          Ok None
  in
  let member_candidate_exists =
    match member_candidate with
    | Some (_module_id, module_info, member_name) -> exported_name_exists module_info member_name
    | None -> false
  in
  if module_candidate_exists && member_candidate_exists then
    Error
      (import_error imp ~code:"module-import-ambiguous"
         ~message:
           (Printf.sprintf "Ambiguous import '%s': it matches both module '%s' and exported member '%s'"
              (import_path_string imp.import_path) (import_path_string imp.import_path)
              (List.hd (List.rev imp.import_path))))
  else if module_candidate_exists then
    let module_id = import_path_string imp.import_path in
    let* () = discover_module state ~module_id ~file_path:full_module_path in
    Ok module_id
  else
    match member_candidate with
    | Some (module_id, _module_info, _member_name) when member_candidate_exists -> Ok module_id
    | Some (module_id, _module_info, member_name) ->
        Error
          (import_error imp ~code:"module-import-not-exported"
             ~message:(Printf.sprintf "Module '%s' does not export '%s'" module_id member_name))
    | None ->
        Error
          (import_error imp ~code:"module-import-not-found"
             ~message:
               (Printf.sprintf "Import '%s' does not resolve to a module or exported member"
                  (import_path_string imp.import_path)))

let discover_project ?source_root ~(entry_file : string) () : (Module_context.module_graph, Diagnostic.t) result =
  let entry_file = normalize_path entry_file in
  let root_dir =
    match source_root with
    | Some root -> normalize_path root
    | None -> Filename.dirname entry_file
  in
  let entry_module = module_id_of_file ~root_dir entry_file in
  let state =
    {
      root_dir;
      modules = Hashtbl.create 16;
      dependencies = Hashtbl.create 16;
      source_overrides = Hashtbl.create 0;
      next_file_index = ref 0;
    }
  in
  let* () = discover_module state ~module_id:entry_module ~file_path:entry_file in
  Module_context.build_graph ~modules:state.modules ~dependencies:state.dependencies ~entry_module

let discover_project_with_entry_source ?source_root ~(entry_file : string) ~(entry_source : string) () :
    (Module_context.module_graph, Diagnostic.t) result =
  let entry_file = normalize_path entry_file in
  let root_dir =
    match source_root with
    | Some root -> normalize_path root
    | None -> Filename.dirname entry_file
  in
  let entry_module = module_id_of_file ~root_dir entry_file in
  let state =
    {
      root_dir;
      modules = Hashtbl.create 16;
      dependencies = Hashtbl.create 16;
      source_overrides = Hashtbl.create 1;
      next_file_index = ref 0;
    }
  in
  Hashtbl.replace state.source_overrides entry_file entry_source;
  let* () = discover_module state ~module_id:entry_module ~file_path:entry_file in
  Module_context.build_graph ~modules:state.modules ~dependencies:state.dependencies ~entry_module

let make_temp_dir (prefix : string) : string =
  let path = Filename.temp_file prefix "" in
  Sys.remove path;
  Unix.mkdir path 0o755;
  path

let write_file (path : string) (content : string) : unit =
  let oc = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> output_string oc content)

let mkdir_p (path : string) : unit =
  let rec go current =
    if current = "" || current = Filename.dir_sep then
      ()
    else if Sys.file_exists current then
      ()
    else (
      go (Filename.dirname current);
      Unix.mkdir current 0o755)
  in
  go path

let with_temp_project (files : (string * string) list) (f : string -> bool) : bool =
  let root = make_temp_dir "marmoset_modules_" in
  let write_one (relative_path, content) =
    let path = Filename.concat root relative_path in
    mkdir_p (Filename.dirname path);
    write_file path content
  in
  List.iter write_one files;
  Fun.protect ~finally:(fun () -> ignore (Sys.command ("rm -rf " ^ Filename.quote root))) (fun () -> f root)

let collect_expr_ids (program : AST.program) : int list =
  let rec expr_ids (expr : AST.expression) =
    let child_ids =
      match expr.expr with
      | AST.Identifier _ | AST.Integer _ | AST.Float _ | AST.Boolean _ | AST.String _ -> []
      | AST.Array xs -> List.concat_map expr_ids xs
      | AST.Index (a, b) -> expr_ids a @ expr_ids b
      | AST.TypeApply (callee, _) -> expr_ids callee
      | AST.Hash pairs -> List.concat_map (fun (k, v) -> expr_ids k @ expr_ids v) pairs
      | AST.Prefix (_, e) | AST.TypeCheck (e, _) | AST.FieldAccess (e, _) -> expr_ids e
      | AST.Infix (l, _, r) -> expr_ids l @ expr_ids r
      | AST.If (cond, cons, alt) -> expr_ids cond @ stmt_ids cons @ option_map_default alt ~default:[] ~f:stmt_ids
      | AST.Function { body; _ } -> stmt_ids body
      | AST.Call (callee, args) -> expr_ids callee @ List.concat_map expr_ids args
      | AST.EnumConstructor (_, _, args) -> List.concat_map expr_ids args
      | AST.Match (scrutinee, arms) ->
          expr_ids scrutinee @ List.concat_map (fun (arm : AST.match_arm) -> expr_ids arm.body) arms
      | AST.RecordLit (fields, spread) ->
          List.concat_map
            (fun (field : AST.record_field) -> option_map_default field.field_value ~default:[] ~f:expr_ids)
            fields
          @ option_map_default spread ~default:[] ~f:expr_ids
      | AST.MethodCall { mc_receiver; mc_args; _ } -> expr_ids mc_receiver @ List.concat_map expr_ids mc_args
      | AST.BlockExpr stmts -> List.concat_map stmt_ids stmts
    in
    expr.id :: child_ids
  and stmt_ids (stmt : AST.statement) =
    match stmt.stmt with
    | AST.ExportDecl _ | AST.ImportDecl _ -> []
    | AST.Let { value; _ } -> expr_ids value
    | AST.Return expr | AST.ExpressionStmt expr -> expr_ids expr
    | AST.Block stmts -> List.concat_map stmt_ids stmts
    | AST.EnumDef _ | AST.TypeDef _ | AST.ShapeDef _ | AST.TraitDef _ | AST.ImplDef _ | AST.InherentImplDef _
    | AST.DeriveDef _ | AST.TypeAlias _ ->
        []
  in
  List.concat_map stmt_ids program

let%test "discover_project finds transitive module dependencies" =
  with_temp_project
    [
      ("main.mr", "import math\nputs(math.add(1, 2))\n");
      ("math.mr", "export add\nimport util.helper\nfn add(x: Int, y: Int) -> Int = helper(x + y)\n");
      ("util.mr", "export helper\nfn helper(x: Int) -> Int = x\n");
    ]
    (fun root ->
      match discover_project ~entry_file:(Filename.concat root "main.mr") () with
      | Error _ -> false
      | Ok graph -> (
          Hashtbl.length graph.modules = 3
          && graph.topo_order = [ "util"; "math"; "main" ]
          &&
          match Hashtbl.find_opt graph.dependencies "math" with
          | Some [ "util" ] -> true
          | _ -> false))

let%test "discover_project reports import ambiguity" =
  with_temp_project
    [
      ("main.mr", "import a.b.c\nputs(1)\n"); ("a/b.mr", "export c\nlet c = 1\n"); ("a/b/c.mr", "let value = 1\n");
    ]
    (fun root ->
      match discover_project ~entry_file:(Filename.concat root "main.mr") () with
      | Ok _ -> false
      | Error diag -> diag.code = "module-import-ambiguous")

let%test "discover_project assigns non-overlapping expression id ranges per file" =
  with_temp_project
    [ ("main.mr", "import math\nputs(math.value)\n"); ("math.mr", "export value\nlet value = 1\n") ]
    (fun root ->
      match discover_project ~entry_file:(Filename.concat root "main.mr") () with
      | Error _ -> false
      | Ok graph -> (
          match (Hashtbl.find_opt graph.modules "main", Hashtbl.find_opt graph.modules "math") with
          | Some main_module, Some math_module ->
              let main_ids = collect_expr_ids main_module.program in
              let math_ids = collect_expr_ids math_module.program in
              List.for_all (fun id -> not (List.mem id math_ids)) main_ids
          | _ -> false))
