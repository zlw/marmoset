module Compiler = Compiler
module Discovery = Discovery
module Import_resolver = Import_resolver
module Module_context = Module_context
module Module_sig = Typecheck.Module_sig
module Parser = Syntax.Parser
module StringSet = Set.Make (String)

type entry = {
  module_id : string;
  file_path : string;
  surface : Import_resolver.module_surface;
  typed_signature : Module_sig.module_signature option;
}

type t = {
  root_dir : string;
  modules : (string, entry) Hashtbl.t;
}

let take n xs =
  let rec go acc remaining = function
    | _ when remaining <= 0 -> List.rev acc
    | [] -> List.rev acc
    | x :: rest -> go (x :: acc) (remaining - 1) rest
  in
  go [] n xs

let export_list_of_program (program : Syntax.Ast.AST.program) : string list =
  let rec first_export = function
    | [] -> []
    | { Syntax.Ast.AST.stmt = Syntax.Ast.AST.ExportDecl names; _ } :: _ -> names
    | _ :: rest -> first_export rest
  in
  first_export program

let source_for_path ~(source_overrides : (string, string) Hashtbl.t) (path : string) : string =
  let path = Discovery.normalize_path path in
  match Hashtbl.find_opt source_overrides path with
  | Some source -> source
  | None -> Compiler.read_source_file path

let parsed_surface_of_file ~(module_id : string) ~(file_path : string) ~(source : string) :
    Import_resolver.module_surface option =
  match Parser.parse_with_surface ~id_offset:0 ~file_id:file_path source with
  | Error _ -> None
  | Ok parse_result ->
      let parsed_module =
        {
          Module_context.module_id;
          file_path;
          source;
          surface_program = parse_result.surface_program;
          program = parse_result.program;
          exports = export_list_of_program parse_result.program;
          imports = [];
        }
      in
      Result.to_option (Import_resolver.build_module_surface ~preserve_top_level_names:false parsed_module)

let hidden_dir (name : string) : bool =
  (String.length name > 0 && name.[0] = '.') || String.equal name "_build" || String.equal name "_opam"

let rec collect_module_files (root_dir : string) (dir : string) (acc : string list ref) : unit =
  Array.iter
    (fun entry ->
      let path = Filename.concat dir entry in
      if Sys.is_directory path then (
        if not (hidden_dir entry) then
          collect_module_files root_dir path acc)
      else if Filename.check_suffix entry ".mr" then
        acc := Discovery.normalize_path path :: !acc)
    (Sys.readdir dir)

let build ?(source_overrides = Hashtbl.create 0) ~(root_dir : string) ?analysis () : t =
  let root_dir = Discovery.normalize_path root_dir in
  let modules = Hashtbl.create 32 in
  Option.iter
    (fun (analysis : Compiler.entry_analysis) ->
      match analysis.project with
      | None -> ()
      | Some project ->
          List.iter
            (fun (checked_module : Compiler.checked_module) ->
              Hashtbl.replace modules checked_module.module_id
                {
                  module_id = checked_module.module_id;
                  file_path = checked_module.file_path;
                  surface = checked_module.navigation.surface;
                  typed_signature = Some checked_module.signature;
                })
            project.modules)
    analysis;
  let files = ref [] in
  collect_module_files root_dir root_dir files;
  List.iter
    (fun file_path ->
      let module_id = Discovery.module_id_of_file ~root_dir file_path in
      if not (Hashtbl.mem modules module_id) then
        let source = source_for_path ~source_overrides file_path in
        match parsed_surface_of_file ~module_id ~file_path ~source with
        | Some surface ->
            Hashtbl.replace modules module_id { module_id; file_path; surface; typed_signature = None }
        | None -> ())
    !files;
  { root_dir; modules }

let find_module (catalog : t) ~(module_id : string) : entry option = Hashtbl.find_opt catalog.modules module_id

let child_segments (catalog : t) ~(prefix_segments : string list) : string list =
  let prefix_len = List.length prefix_segments in
  let children =
    Hashtbl.fold
      (fun _ (entry : entry) acc ->
        let parts = String.split_on_char '.' entry.module_id in
        if List.length parts > prefix_len && take prefix_len parts = prefix_segments then
          StringSet.add (List.nth parts prefix_len) acc
        else
          acc)
      catalog.modules StringSet.empty
  in
  StringSet.elements children

let%test "module_catalog scans child segments from project root" =
  Discovery.with_temp_project
    [
      ("main.mr", "import collections.list\n");
      ("collections/list.mr", "export map\nfn map(x: Int) -> Int = x\n");
      ("math.mr", "export add\nfn add(x: Int, y: Int) -> Int = x + y\n");
    ]
    (fun root ->
      let catalog = build ~root_dir:root () in
      let root_children = child_segments catalog ~prefix_segments:[] in
      List.mem "collections" root_children
      && List.mem "math" root_children
      && child_segments catalog ~prefix_segments:[ "collections" ] = [ "list" ])

let%test "module_catalog respects source overrides for off-graph exports" =
  Discovery.with_temp_project
    [ ("main.mr", "import math.mul\n"); ("math.mr", "export add\nfn add(x: Int, y: Int) -> Int = x + y\n") ]
    (fun root ->
      let overrides = Hashtbl.create 1 in
      Hashtbl.replace overrides (Filename.concat root "math.mr")
        "export mul, Point\ntype Point = { x: Int, y: Int }\nfn mul(x: Int, y: Int) -> Int = x * y\n";
      let catalog = build ~root_dir:root ~source_overrides:overrides () in
      match find_module catalog ~module_id:"math" with
      | Some entry ->
          Import_resolver.StringMap.mem "mul" entry.surface.exports
          && Import_resolver.StringMap.mem "Point" entry.surface.exports
      | None -> false)
