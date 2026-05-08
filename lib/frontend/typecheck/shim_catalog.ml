module Diagnostic = Diagnostics.Diagnostic

let go_keywords =
  [
    "break";
    "default";
    "func";
    "interface";
    "select";
    "case";
    "defer";
    "go";
    "map";
    "struct";
    "chan";
    "else";
    "goto";
    "package";
    "switch";
    "const";
    "fallthrough";
    "if";
    "range";
    "type";
    "continue";
    "for";
    "import";
    "return";
    "var";
  ]

let allowed_roots = [ "std"; "test" ]

let is_lower_snake_segment (segment : string) : bool =
  let len = String.length segment in
  len > 0
  &&
  match segment.[0] with
  | 'a' .. 'z' ->
      String.for_all
        (function
          | 'a' .. 'z' | '0' .. '9' | '_' -> true
          | _ -> false)
        segment
  | _ -> false

let split_id (shim_id : string) = String.split_on_char '/' shim_id

let validate_id (shim_id : string) : (string list, string) result =
  let segments = split_id shim_id in
  match segments with
  | root :: _ when List.length segments >= 2 ->
      if not (List.mem root allowed_roots) then
        Error (Printf.sprintf "invalid shim id %S: allowed roots are std and test" shim_id)
      else if List.exists (fun segment -> String.equal segment "" || String.equal segment "." || String.equal segment "..") segments then
        Error (Printf.sprintf "invalid shim id %S: segments must be non-empty and cannot be . or .." shim_id)
      else if List.exists (fun segment -> not (is_lower_snake_segment segment)) segments then
        Error (Printf.sprintf "invalid shim id %S: segments must be lowercase snake-case identifiers" shim_id)
      else if List.exists (fun segment -> List.mem segment go_keywords) segments then
        Error (Printf.sprintf "invalid shim id %S: segments cannot be Go keywords" shim_id)
      else
        Ok segments
  | _ -> Error (Printf.sprintf "invalid shim id %S: expected at least two slash-separated segments" shim_id)

let default_qualifier (shim_id : string) : string option =
  match validate_id shim_id with
  | Error _ -> None
  | Ok segments -> Some (List.hd (List.rev segments))

let owner_module_id (shim_id : string) : (string, string) result =
  Result.map (String.concat ".") (validate_id shim_id)

let runtime_shim_dir_from_cwd (segments : string list) : string =
  List.fold_left Filename.concat "runtime/go/shims" segments

let exists (shim_id : string) : bool =
  match validate_id shim_id with
  | Error _ -> false
  | Ok ("test" :: _) -> true
  | Ok segments -> Sys.file_exists (runtime_shim_dir_from_cwd segments)

let invalid_diagnostic ?source_span (shim_id : string) : Diagnostic.t =
  let message =
    match validate_id shim_id with
    | Ok _ -> Printf.sprintf "invalid shim id %S" shim_id
    | Error msg -> msg
  in
  match source_span with
  | Some (Diagnostic.Span { file_id; start_pos; end_pos }) ->
      Diagnostic.error_with_span ~code:"shim-id-invalid" ~message ~file_id ~start_pos ?end_pos ()
  | Some Diagnostic.NoSpan | None -> Diagnostic.error_no_span ~code:"shim-id-invalid" ~message

let not_found_diagnostic ?source_span (shim_id : string) : Diagnostic.t =
  let message = Printf.sprintf "shim id %S was not found in the shim catalog" shim_id in
  match source_span with
  | Some (Diagnostic.Span { file_id; start_pos; end_pos }) ->
      Diagnostic.error_with_span ~code:"shim-id-not-found" ~message ~file_id ~start_pos ?end_pos ()
  | Some Diagnostic.NoSpan | None -> Diagnostic.error_no_span ~code:"shim-id-not-found" ~message

let validate_known ?source_span (shim_id : string) : (string list, Diagnostic.t) result =
  match validate_id shim_id with
  | Error _ -> Error (invalid_diagnostic ?source_span shim_id)
  | Ok segments ->
      if exists shim_id then
        Ok segments
      else
        Error (not_found_diagnostic ?source_span shim_id)

let%test "validates synthetic test shim ids" =
  validate_known "test/scalar" = Ok [ "test"; "scalar" ]
  && default_qualifier "test/scalar" = Some "scalar"
  && owner_module_id "test/scalar" = Ok "test.scalar"

let%test "rejects direct Go import looking shim ids before catalog lookup" =
  match validate_known "strings" with
  | Error diag -> diag.code = "shim-id-invalid"
  | Ok _ -> false

let%test "rejects missing std shim ids after shape validation" =
  match validate_known "std/missing" with
  | Error diag -> diag.code = "shim-id-not-found"
  | Ok _ -> false

let%test "rejects unsafe shim id segments" =
  List.for_all
    (fun shim_id ->
      match validate_known shim_id with
      | Error diag -> diag.code = "shim-id-invalid"
      | Ok _ -> false)
    [ "test/"; "test/../x"; "test/Camel"; "test/has-hyphen"; "test/type" ]
