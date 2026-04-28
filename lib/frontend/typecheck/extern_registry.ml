open Types

module AST = Syntax.Ast.AST
module Diagnostic = Diagnostics.Diagnostic
module Artifacts = Resolution_artifacts

let ( let* ) = Result.bind

let map_result f xs =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | x :: rest -> (
        match f x with
        | Error e -> Error e
        | Ok y -> go (y :: acc) rest)
  in
  go [] xs

let declaration_by_source : (string * string, Artifacts.extern_func) Hashtbl.t = Hashtbl.create 64
let declaration_by_key : (string, Artifacts.extern_func) Hashtbl.t = Hashtbl.create 64
let path_by_qualifier : (string, string) Hashtbl.t = Hashtbl.create 32
let extern_calls : (int, Artifacts.extern_call) Hashtbl.t = Hashtbl.create 128

let clear () : unit =
  Hashtbl.clear declaration_by_source;
  Hashtbl.clear declaration_by_key;
  Hashtbl.clear path_by_qualifier;
  Hashtbl.clear extern_calls

let extern_key ~(go_path : string) ~(go_func_name : string) : string = go_path ^ "\x00" ^ go_func_name

let go_import_alias (go_path : string) : string =
  let buf = Buffer.create (String.length go_path + 5) in
  Buffer.add_string buf "mext_";
  let last_was_sep = ref false in
  String.iter
    (fun ch ->
      let add_sep () =
        if (not !last_was_sep) && Buffer.length buf > 5 then (
          Buffer.add_char buf '_';
          last_was_sep := true)
      in
      match ch with
      | 'a' .. 'z' | '0' .. '9' ->
          Buffer.add_char buf ch;
          last_was_sep := false
      | 'A' .. 'Z' ->
          Buffer.add_char buf (Char.lowercase_ascii ch);
          last_was_sep := false
      | _ -> add_sep ())
    go_path;
  let alias = Buffer.contents buf in
  if alias = "mext_" then
    "mext_pkg"
  else
    alias

let source_span ~(file_id : string option) ~(start_pos : int) ~(end_pos : int) : Diagnostic.span =
  match file_id with
  | Some file_id -> Diagnostic.Span { file_id; start_pos; end_pos = Some end_pos }
  | None -> Diagnostic.NoSpan

let error_at_span ~code ~message = function
  | Diagnostic.Span { file_id; start_pos; end_pos } ->
      Diagnostic.error_with_span ~code ~message ~file_id ~start_pos ?end_pos ()
  | Diagnostic.NoSpan -> Diagnostic.error_no_span ~code ~message

let type_error span message = Error (error_at_span ~code:"type-extern" ~message span)

let allowed_param_type = function
  | TInt | TFloat | TBool | TString -> true
  | TNull | TVar _ | TFun _ | TArray _ | THash _ | TRecord _ | TRowVar _ | TTraitObject _ | TUnion _
  | TIntersection _ | TEnum _ | TNamed _ ->
      false

let allowed_return_type = function
  | TInt | TFloat | TBool | TString | TNull -> true
  | TVar _ | TFun _ | TArray _ | THash _ | TRecord _ | TRowVar _ | TTraitObject _ | TUnion _ | TIntersection _
  | TEnum _ | TNamed _ ->
      false

let signature_equal (a : Artifacts.extern_func) (b : Artifacts.extern_func) : bool =
  List.length a.param_types = List.length b.param_types
  && List.for_all2 (=) a.param_types b.param_types
  && a.return_type = b.return_type
  && Bool.equal a.is_effectful b.is_effectful

let validate_param_type span ~(fn_name : string) ~(param_name : string) (typ : mono_type) : (unit, Diagnostic.t) result
    =
  let typ = canonicalize_mono_type typ in
  if allowed_param_type typ then
    Ok ()
  else
    type_error span
      (Printf.sprintf "extern function %s parameter %s uses unsupported type %s" fn_name param_name
         (to_string typ))

let validate_return_type span ~(fn_name : string) (typ : mono_type) : (unit, Diagnostic.t) result =
  let typ = canonicalize_mono_type typ in
  if allowed_return_type typ then
    Ok ()
  else
    type_error span
      (Printf.sprintf "extern function %s return uses unsupported type %s" fn_name (to_string typ))

let build_func ~(declaring_module : string) ~(file_id : string option) (block : AST.extern_block_def)
    (fn_sig : AST.extern_fn_sig) : (Artifacts.extern_func, Diagnostic.t) result =
  let span = source_span ~file_id ~start_pos:fn_sig.extern_fn_pos ~end_pos:fn_sig.extern_fn_end_pos in
  let* param_types =
    map_result
      (fun (param : AST.extern_param) ->
        Annotation.type_expr_to_mono_type param.extern_param_type
        |> Result.map canonicalize_mono_type
        |> Result.map_error (fun (diag : Diagnostic.t) -> error_at_span ~code:"type-extern" ~message:diag.message span))
      fn_sig.extern_fn_params
  in
  let* return_type =
    Annotation.type_expr_to_mono_type fn_sig.extern_fn_return_type
    |> Result.map canonicalize_mono_type
    |> Result.map_error (fun (diag : Diagnostic.t) -> error_at_span ~code:"type-extern" ~message:diag.message span)
  in
  let* () =
    map_result
      (fun ((param : AST.extern_param), typ) ->
        validate_param_type span ~fn_name:fn_sig.extern_fn_name ~param_name:param.extern_param_name typ)
      (List.combine fn_sig.extern_fn_params param_types)
    |> Result.map (fun _ -> ())
  in
  let* () = validate_return_type span ~fn_name:fn_sig.extern_fn_name return_type in
  Ok
    {
      Artifacts.extern_key =
        extern_key ~go_path:block.extern_go_path ~go_func_name:fn_sig.extern_fn_name;
      declaring_module;
      go_path = block.extern_go_path;
      source_qualifier = block.extern_qualifier;
      go_import_alias = go_import_alias block.extern_go_path;
      go_func_name = fn_sig.extern_fn_name;
      param_names = List.map (fun (param : AST.extern_param) -> param.extern_param_name) fn_sig.extern_fn_params;
      param_types;
      return_type;
      is_effectful = fn_sig.extern_fn_effectful;
      source_span = span;
    }

let register_func (func : Artifacts.extern_func) : (unit, Diagnostic.t) result =
  let source_key = (func.source_qualifier, func.go_func_name) in
  match Hashtbl.find_opt declaration_by_source source_key with
  | Some _ ->
      type_error func.source_span
        (Printf.sprintf "duplicate extern function %s.%s" func.source_qualifier func.go_func_name)
  | None -> (
      match Hashtbl.find_opt path_by_qualifier func.source_qualifier with
      | Some existing_path when existing_path <> func.go_path ->
          type_error func.source_span
            (Printf.sprintf "extern qualifier %s already refers to %S, not %S" func.source_qualifier
               existing_path func.go_path)
      | _ -> (
          match Hashtbl.find_opt declaration_by_key func.extern_key with
          | Some existing when not (signature_equal existing func) ->
              type_error func.source_span
                (Printf.sprintf "conflicting extern signatures for %S.%s" func.go_path func.go_func_name)
          | Some _ | None ->
              Hashtbl.replace path_by_qualifier func.source_qualifier func.go_path;
              Hashtbl.replace declaration_by_source source_key func;
              Hashtbl.replace declaration_by_key func.extern_key func;
              Ok ()))

let register_block ~(declaring_module : string) ~(file_id : string option) (block : AST.extern_block_def) :
    (unit, Diagnostic.t) result =
  let* funcs = map_result (build_func ~declaring_module ~file_id block) block.extern_fns in
  let rec go = function
    | [] -> Ok ()
    | func :: rest ->
        let* () = register_func func in
        go rest
  in
  go funcs

let lookup ~(source_qualifier : string) ~(go_func_name : string) : Artifacts.extern_func option =
  Hashtbl.find_opt declaration_by_source (source_qualifier, go_func_name)

let is_qualifier (source_qualifier : string) : bool = Hashtbl.mem path_by_qualifier source_qualifier

let record_call (expr_id : int) (call : Artifacts.extern_call) : unit =
  Hashtbl.replace extern_calls expr_id call

let snapshot_declarations () : (string, Artifacts.extern_func) Hashtbl.t = Hashtbl.copy declaration_by_key
let snapshot_calls () : (int, Artifacts.extern_call) Hashtbl.t = Hashtbl.copy extern_calls

let%test "go import alias canonicalizes paths" =
  go_import_alias "path/filepath" = "mext_path_filepath"
  && go_import_alias "github.com/acme/text-case" = "mext_github_com_acme_text_case"

let parse_one_extern source =
  match Syntax.Parser.parse ~file_id:"<test>" source with
  | Ok [ { AST.stmt = AST.ExternBlock block; file_id; _ } ] -> Ok (block, file_id)
  | Ok _ -> Error "expected one extern block"
  | Error diags -> Error (String.concat "; " (List.map (fun (diag : Diagnostic.t) -> diag.message) diags))

let register_source source =
  match parse_one_extern source with
  | Error msg -> Error (Diagnostic.error_no_span ~code:"test" ~message:msg)
  | Ok (block, file_id) -> register_block ~declaring_module:"<test>" ~file_id block

let%test "extern registry snapshots declarations" =
  clear ();
  match register_source "extern \"strings\" = { fn ToUpper(s: Str) -> Str }" with
  | Error _ -> false
  | Ok () -> (
      match lookup ~source_qualifier:"strings" ~go_func_name:"ToUpper" with
      | Some func ->
          func.go_path = "strings" && func.go_import_alias = "mext_strings"
          && func.param_types = [ TString ] && func.return_type = TString
          && Hashtbl.length (snapshot_declarations ()) = 1
      | None -> false)

let%test "extern registry rejects Unit parameters" =
  clear ();
  match register_source "extern \"fmt\" = { fn Println(x: Unit) -> Unit }" with
  | Error diag -> diag.code = "type-extern" && String.contains diag.message 'x'
  | Ok () -> false

let%test "extern registry accepts Unit return" =
  clear ();
  match register_source "extern \"fmt\" = { fn Println(s: Str) => Unit }" with
  | Ok () -> true
  | Error _ -> false

let%test "extern registry rejects duplicate source functions" =
  clear ();
  match
    register_source
      "extern \"strings\" = {\n\
      \  fn ToUpper(s: Str) -> Str\n\
      \  fn ToUpper(s: Str) -> Str\n\
       }"
  with
  | Error diag -> diag.code = "type-extern"
  | Ok () -> false

let%test "extern registry rejects conflicting path function signatures" =
  clear ();
  match register_source "extern \"strings\" as s1 = { fn ToUpper(s: Str) -> Str }" with
  | Error _ -> false
  | Ok () -> (
      match register_source "extern \"strings\" as s2 = { fn ToUpper(s: Int) -> Str }" with
      | Error diag -> diag.code = "type-extern"
      | Ok () -> false)

let%test "extern registry normalizes transparent aliases" =
  clear ();
  match
    Syntax.Parser.parse ~file_id:"<test>"
      "type Stringy = Str\n\
       extern \"strings\" = { fn ToUpper(s: Stringy) -> Stringy }"
  with
  | Error _ -> false
  | Ok program ->
      Annotation.clear_type_aliases ();
      List.iter
        (fun (stmt : AST.statement) ->
          match stmt.stmt with
          | AST.TypeAlias alias_def -> Annotation.register_type_alias alias_def
          | _ -> ())
        program;
      let result =
        List.fold_left
          (fun acc (stmt : AST.statement) ->
            match (acc, stmt.stmt) with
            | Error _ as err, _ -> err
            | Ok (), AST.ExternBlock block -> register_block ~declaring_module:"<test>" ~file_id:stmt.file_id block
            | Ok (), _ -> Ok ())
          (Ok ()) program
      in
      let ok =
        match lookup ~source_qualifier:"strings" ~go_func_name:"ToUpper" with
        | Some func -> func.param_types = [ TString ] && func.return_type = TString
        | None -> false
      in
      clear ();
      Annotation.clear_type_aliases ();
      Result.is_ok result && ok
