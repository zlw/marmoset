module AST = Syntax.Ast.AST
module Diagnostic = Diagnostics.Diagnostic
module Artifacts = Resolution_artifacts

let ( let* ) = Result.bind

let map_result f xs =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | x :: rest ->
        let* y = f x in
        go (y :: acc) rest
  in
  go [] xs

let declaration_by_source : (string * string, Artifacts.extern_func) Hashtbl.t = Hashtbl.create 64
let declaration_by_key : (string, Artifacts.extern_func) Hashtbl.t = Hashtbl.create 64
let shim_id_by_qualifier : (string, string) Hashtbl.t = Hashtbl.create 32
let registered_block_shim_ids : (string, Diagnostic.span) Hashtbl.t = Hashtbl.create 16
let extern_calls : (int, Artifacts.extern_call) Hashtbl.t = Hashtbl.create 128
let current_module_id : string option ref = ref None

let clear () : unit =
  Hashtbl.clear declaration_by_source;
  Hashtbl.clear declaration_by_key;
  Hashtbl.clear shim_id_by_qualifier;
  Hashtbl.clear registered_block_shim_ids;
  Hashtbl.clear extern_calls;
  current_module_id := None

let set_current_module_id module_id = current_module_id := Some module_id
let declaring_module_or fallback = Option.value !current_module_id ~default:fallback

let shim_key ~(shim_id : string) ~(func_name : string) : string = shim_id ^ "\x00" ^ func_name

let source_span ~(file_id : string option) ~(start_pos : int) ~(end_pos : int) : Diagnostic.span =
  match file_id with
  | Some file_id -> Diagnostic.Span { file_id; start_pos; end_pos = Some end_pos }
  | None -> Diagnostic.NoSpan

let error_at_span ~code ~message = function
  | Diagnostic.Span { file_id; start_pos; end_pos } ->
      Diagnostic.error_with_span ~code ~message ~file_id ~start_pos ?end_pos ()
  | Diagnostic.NoSpan -> Diagnostic.error_no_span ~code ~message

let registry_error span ~code ~message = Error (error_at_span ~code ~message span)

let capitalize_words words =
  words |> List.filter (fun word -> not (String.equal word "")) |> List.map String.capitalize_ascii
  |> String.concat ""

let words_of_identifier_base (base : string) : string list =
  let flush current words =
    match Buffer.contents current with
    | "" -> words
    | word ->
        Buffer.clear current;
        String.lowercase_ascii word :: words
  in
  let current = Buffer.create (String.length base) in
  let rec go words prev_lower_or_digit idx =
    if idx >= String.length base then
      List.rev (flush current words)
    else
      match base.[idx] with
      | '_' -> go (flush current words) false (idx + 1)
      | ('A' .. 'Z' as ch) when prev_lower_or_digit ->
          let words = flush current words in
          Buffer.add_char current (Char.lowercase_ascii ch);
          go words false (idx + 1)
      | ('A' .. 'Z' as ch) ->
          Buffer.add_char current (Char.lowercase_ascii ch);
          go words false (idx + 1)
      | ('a' .. 'z' | '0' .. '9') as ch ->
          Buffer.add_char current ch;
          go words true (idx + 1)
      | ch ->
          Buffer.add_string current (Printf.sprintf "x%02x" (Char.code ch));
          go words false (idx + 1)
  in
  go [] false 0

let go_symbol_name (name : string) : string =
  let base, suffix =
    if String.ends_with ~suffix:"?" name then
      (String.sub name 0 (String.length name - 1), [ "q" ])
    else if String.ends_with ~suffix:"!" name then
      (String.sub name 0 (String.length name - 1), [ "bang" ])
    else
      (name, [])
  in
  match capitalize_words (words_of_identifier_base base @ suffix) with
  | "" -> "Shim"
  | symbol -> symbol

let signature_equal (a : Artifacts.extern_func) (b : Artifacts.extern_func) : bool =
  List.length a.param_boundary_types = List.length b.param_boundary_types
  && List.for_all2 Shim_boundary.equal_boundary_type a.param_boundary_types b.param_boundary_types
  && Shim_boundary.equal_boundary_type a.return_boundary_type b.return_boundary_type
  && Bool.equal a.is_effectful b.is_effectful

let validate_unique_param_names span ~(fn_name : string) (params : AST.extern_param list) :
    (unit, Diagnostic.t) result =
  let rec go seen = function
    | [] -> Ok ()
    | (param : AST.extern_param) :: rest ->
        if List.mem param.extern_param_name seen then
          registry_error span ~code:"shim-function-duplicate"
            ~message:
              (Printf.sprintf "shim function %s has duplicate parameter name %s" fn_name
                 param.extern_param_name)
        else
          go (param.extern_param_name :: seen) rest
  in
  go [] params

let build_func ~(owner_module_id : string) ~(file_id : string option) (block : AST.extern_block_def)
    (fn_sig : AST.extern_fn_sig) : (Artifacts.extern_func, Diagnostic.t) result =
  let span = source_span ~file_id ~start_pos:fn_sig.extern_fn_pos ~end_pos:fn_sig.extern_fn_end_pos in
  let* param_types =
    map_result
      (fun (param : AST.extern_param) ->
        Annotation.type_expr_to_mono_type param.extern_param_type
        |> Result.map Types.canonicalize_mono_type
        |> Result.map_error (fun (diag : Diagnostic.t) ->
               error_at_span ~code:"type-shim-boundary" ~message:diag.message span))
      fn_sig.extern_fn_params
  in
  let* return_type =
    Annotation.type_expr_to_mono_type fn_sig.extern_fn_return_type
    |> Result.map Types.canonicalize_mono_type
    |> Result.map_error (fun (diag : Diagnostic.t) ->
           error_at_span ~code:"type-shim-boundary" ~message:diag.message span)
  in
  let* () = validate_unique_param_names span ~fn_name:fn_sig.extern_fn_name fn_sig.extern_fn_params in
  let* param_boundary_types =
    map_result
      (fun typ -> Shim_boundary.classify ~source_span:span ~owner_module_id typ)
      param_types
  in
  let* return_boundary_type = Shim_boundary.classify ~source_span:span ~owner_module_id return_type in
  Ok
    {
      Artifacts.shim_key = shim_key ~shim_id:block.extern_shim_id ~func_name:fn_sig.extern_fn_name;
      shim_id = block.extern_shim_id;
      owner_module_id;
      source_qualifier = block.extern_qualifier;
      marmoset_func_name = fn_sig.extern_fn_name;
      go_symbol_name = go_symbol_name fn_sig.extern_fn_name;
      param_names = List.map (fun (param : AST.extern_param) -> param.extern_param_name) fn_sig.extern_fn_params;
      param_boundary_types;
      return_boundary_type;
      is_effectful = fn_sig.extern_fn_effectful;
      source_span = span;
      boundary_spans = List.init (List.length fn_sig.extern_fn_params + 1) (fun _ -> span);
    }

let register_func (func : Artifacts.extern_func) : (unit, Diagnostic.t) result =
  let source_key = (func.source_qualifier, func.marmoset_func_name) in
  match Hashtbl.find_opt declaration_by_source source_key with
  | Some _ ->
      registry_error func.source_span ~code:"shim-function-duplicate"
        ~message:(Printf.sprintf "duplicate shim function %s.%s" func.source_qualifier func.marmoset_func_name)
  | None -> (
      match Hashtbl.find_opt shim_id_by_qualifier func.source_qualifier with
      | Some existing_id when existing_id <> func.shim_id ->
          registry_error func.source_span ~code:"module-extern-qualifier-collision"
            ~message:
              (Printf.sprintf "extern qualifier %s already refers to shim %S, not %S" func.source_qualifier
                 existing_id func.shim_id)
      | _ -> (
          match Hashtbl.find_opt declaration_by_key func.shim_key with
          | Some existing when not (signature_equal existing func) ->
              registry_error func.source_span ~code:"shim-function-duplicate"
                ~message:
                  (Printf.sprintf "conflicting shim signatures for %S.%s" func.shim_id
                     func.marmoset_func_name)
          | Some _ | None ->
              Hashtbl.replace shim_id_by_qualifier func.source_qualifier func.shim_id;
              Hashtbl.replace declaration_by_source source_key func;
              Hashtbl.replace declaration_by_key func.shim_key func;
              Ok ()))

let validate_go_symbol_collisions (funcs : Artifacts.extern_func list) : (unit, Diagnostic.t) result =
  let by_symbol : (string, Artifacts.extern_func) Hashtbl.t = Hashtbl.create (List.length funcs) in
  let rec go = function
    | [] -> Ok ()
    | (func : Artifacts.extern_func) :: rest -> (
        match Hashtbl.find_opt by_symbol func.go_symbol_name with
        | Some existing ->
            registry_error func.source_span ~code:"shim-symbol-collision"
              ~message:
                (Printf.sprintf "shim functions %s and %s both map to Go symbol %s"
                   existing.marmoset_func_name func.marmoset_func_name func.go_symbol_name)
        | None ->
            Hashtbl.add by_symbol func.go_symbol_name func;
            go rest)
  in
  go funcs

let register_block ~(declaring_module : string) ~(file_id : string option) (block : AST.extern_block_def) :
    (unit, Diagnostic.t) result =
  let block_span =
    match block.extern_fns with
    | first :: _ -> source_span ~file_id ~start_pos:first.extern_fn_pos ~end_pos:first.extern_fn_end_pos
    | [] -> Diagnostic.NoSpan
  in
  let* _segments = Shim_catalog.validate_known ~source_span:block_span block.extern_shim_id in
  let* expected_owner =
    Shim_catalog.owner_module_id block.extern_shim_id
    |> Result.map_error (fun _ -> Shim_catalog.invalid_diagnostic ~source_span:block_span block.extern_shim_id)
  in
  if not (String.equal declaring_module expected_owner) then
    registry_error block_span ~code:"shim-owner-mismatch"
      ~message:
        (Printf.sprintf "shim %S must be declared by owner module '%s', not '%s'" block.extern_shim_id
           expected_owner declaring_module)
  else if Hashtbl.mem registered_block_shim_ids block.extern_shim_id then
    registry_error block_span ~code:"shim-block-duplicate"
      ~message:(Printf.sprintf "shim %S is declared by multiple extern blocks in module '%s'" block.extern_shim_id declaring_module)
  else (
    Hashtbl.add registered_block_shim_ids block.extern_shim_id block_span;
    let* funcs = map_result (build_func ~owner_module_id:declaring_module ~file_id block) block.extern_fns in
    let* () = validate_go_symbol_collisions funcs in
    let rec go = function
      | [] -> Ok ()
      | func :: rest ->
          let* () = register_func func in
          go rest
    in
    go funcs)

let lookup ~(source_qualifier : string) ~(func_name : string) : Artifacts.extern_func option =
  Hashtbl.find_opt declaration_by_source (source_qualifier, func_name)

let is_qualifier (source_qualifier : string) : bool = Hashtbl.mem shim_id_by_qualifier source_qualifier

let record_call (expr_id : int) (call : Artifacts.extern_call) : unit =
  Hashtbl.replace extern_calls expr_id call

let snapshot_declarations () : (string, Artifacts.extern_func) Hashtbl.t = Hashtbl.copy declaration_by_key
let snapshot_calls () : (int, Artifacts.extern_call) Hashtbl.t = Hashtbl.copy extern_calls

let%test "shim symbol mangling handles suffixes and collisions" =
  go_symbol_name "read_bytes" = "ReadBytes"
  && go_symbol_name "exists?" = "ExistsQ"
  && go_symbol_name "exists_q" = "ExistsQ"
  && go_symbol_name "existsQ" = "ExistsQ"
  && go_symbol_name "write!" = "WriteBang"

let parse_one_extern source =
  match Syntax.Parser.parse ~file_id:"<test>" source with
  | Ok [ { AST.stmt = AST.ExternBlock block; file_id; _ } ] -> Ok (block, file_id)
  | Ok _ -> Error "expected one extern block"
  | Error diags -> Error (String.concat "; " (List.map (fun (diag : Diagnostic.t) -> diag.message) diags))

let register_source ?(declaring_module = "test.scalar") source =
  match parse_one_extern source with
  | Error msg -> Error (Diagnostic.error_no_span ~code:"test" ~message:msg)
  | Ok (block, file_id) -> register_block ~declaring_module ~file_id block

let%test "shim registry snapshots scalar declarations" =
  clear ();
  match register_source "extern \"test/scalar\" = { fn upcase(s: Str) -> Str }" with
  | Error _ -> false
  | Ok () -> (
      match lookup ~source_qualifier:"scalar" ~func_name:"upcase" with
      | Some func ->
          func.shim_id = "test/scalar" && func.go_symbol_name = "Upcase"
          && func.param_boundary_types = [ Shim_boundary.BStr ]
          && func.return_boundary_type = Shim_boundary.BStr
          && Hashtbl.length (snapshot_declarations ()) = 1
      | None -> false)

let%test "shim registry rejects direct Go import paths" =
  clear ();
  match register_source ~declaring_module:"strings" "extern \"strings\" = { fn ToUpper(s: Str) -> Str }" with
  | Error diag -> diag.code = "shim-id-invalid"
  | Ok () -> false

let%test "shim registry rejects owner mismatch" =
  clear ();
  match register_source ~declaring_module:"main" "extern \"test/scalar\" = { fn upcase(s: Str) -> Str }" with
  | Error diag -> diag.code = "shim-owner-mismatch"
  | Ok () -> false

let%test "shim registry rejects unsupported boundary types" =
  clear ();
  match register_source "extern \"test/scalar\" = { fn bad(xs: List[Str]) -> Str }" with
  | Error diag -> diag.code = "type-shim-boundary"
  | Ok () -> false

let%test "shim registry rejects post-mangle symbol collisions" =
  clear ();
  match register_source "extern \"test/scalar\" = { fn exists?(s: Str) -> Bool fn exists_q(s: Str) -> Bool }" with
  | Error diag -> diag.code = "shim-symbol-collision"
  | Ok () -> false
