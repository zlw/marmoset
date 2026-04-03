module Diagnostic = Diagnostics.Diagnostic
module AST = Syntax.Ast.AST

type import_info = {
  import_path : string list;
  import_alias : string option;
  file_id : string option;
  start_pos : int;
  end_pos : int;
}

type parsed_module = {
  module_id : string;
  file_path : string;
  source : string;
  program : AST.program;
  exports : string list;
  imports : import_info list;
}

type module_graph = {
  modules : (string, parsed_module) Hashtbl.t;
  dependencies : (string, string list) Hashtbl.t;
  topo_order : string list;
  entry_module : string;
}

let cycle_error (cycle : string list) : Diagnostic.t =
  Diagnostic.error_no_span ~code:"module-cycle"
    ~message:(Printf.sprintf "Circular module dependency: %s" (String.concat " -> " cycle))

let build_graph
    ~(modules : (string, parsed_module) Hashtbl.t)
    ~(dependencies : (string, string list) Hashtbl.t)
    ~(entry_module : string) : (module_graph, Diagnostic.t) result =
  let visiting : (string, int) Hashtbl.t = Hashtbl.create 16 in
  let visited : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  let order = ref [] in
  let rec drop n xs =
    if n <= 0 then
      xs
    else
      match xs with
      | [] -> []
      | _ :: rest -> drop (n - 1) rest
  in
  let rec visit (stack : string list) (module_id : string) =
    if Hashtbl.mem visited module_id then
      Ok ()
    else
      match Hashtbl.find_opt visiting module_id with
      | Some idx ->
          let cycle_tail = drop idx (List.rev stack) in
          Error (cycle_error (cycle_tail @ [ module_id ]))
      | None -> (
          Hashtbl.replace visiting module_id (List.length stack);
          let deps = Option.value (Hashtbl.find_opt dependencies module_id) ~default:[] in
          let rec visit_deps = function
            | [] -> Ok ()
            | dep :: rest -> (
                if not (Hashtbl.mem modules dep) then
                  Error
                    (Diagnostic.error_no_span ~code:"module-missing"
                       ~message:(Printf.sprintf "Missing parsed module for dependency '%s'" dep))
                else
                  match visit (module_id :: stack) dep with
                  | Error _ as err -> err
                  | Ok () -> visit_deps rest)
          in
          match visit_deps deps with
          | Error _ as err -> err
          | Ok () ->
              Hashtbl.remove visiting module_id;
              Hashtbl.replace visited module_id ();
              order := module_id :: !order;
              Ok ())
  in
  match visit [] entry_module with
  | Error _ as err -> err
  | Ok () -> Ok { modules; dependencies; topo_order = List.rev !order; entry_module }

let%test "build_graph topo orders dependencies before dependents" =
  let mk module_id =
    { module_id; file_path = module_id ^ ".mr"; source = ""; program = []; exports = []; imports = [] }
  in
  let modules = Hashtbl.create 3 in
  Hashtbl.replace modules "main" (mk "main");
  Hashtbl.replace modules "math" (mk "math");
  Hashtbl.replace modules "util" (mk "util");
  let deps = Hashtbl.create 3 in
  Hashtbl.replace deps "main" [ "math" ];
  Hashtbl.replace deps "math" [ "util" ];
  Hashtbl.replace deps "util" [];
  match build_graph ~modules ~dependencies:deps ~entry_module:"main" with
  | Error _ -> false
  | Ok graph -> graph.topo_order = [ "util"; "math"; "main" ]

let%test "build_graph reports cycle path" =
  let mk module_id =
    { module_id; file_path = module_id ^ ".mr"; source = ""; program = []; exports = []; imports = [] }
  in
  let modules = Hashtbl.create 3 in
  Hashtbl.replace modules "main" (mk "main");
  Hashtbl.replace modules "a" (mk "a");
  Hashtbl.replace modules "b" (mk "b");
  let deps = Hashtbl.create 3 in
  Hashtbl.replace deps "main" [ "a" ];
  Hashtbl.replace deps "a" [ "b" ];
  Hashtbl.replace deps "b" [ "a" ];
  match build_graph ~modules ~dependencies:deps ~entry_module:"main" with
  | Ok _ -> false
  | Error diag ->
      diag.code = "module-cycle" && String.equal diag.message "Circular module dependency: a -> b -> a"
