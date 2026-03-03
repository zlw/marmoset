type severity = Error | Warning | Info

type span =
  | NoSpan
  | Span of {
      file_id : string;
      start_pos : int;
      end_pos : int option;
    }

type label = {
  span : span;
  message : string option;
  primary : bool;
}

type t = {
  code : string;
  severity : severity;
  message : string;
  labels : label list;
  notes : string list;
}

let severity_to_string = function
  | Error -> "error"
  | Warning -> "warning"
  | Info -> "info"

let make ~code ~severity ~message ?(labels = []) ?(notes = []) () : t = { code; severity; message; labels; notes }

let primary_label ?message (span : span) : label = { span; message; primary = true }
let secondary_label ?message (span : span) : label = { span; message; primary = false }

let error_no_span ~code ~message : t = make ~code ~severity:Error ~message ()

let error_with_span ~code ~message ~file_id ~start_pos ?end_pos () : t =
  let span = Span { file_id; start_pos; end_pos } in
  make ~code ~severity:Error ~message ~labels:[ primary_label span ] ()

let with_note (diag : t) (note : string) : t = { diag with notes = diag.notes @ [ note ] }

let with_secondary_label (diag : t) (label : label) : t =
  { diag with labels = diag.labels @ [ { label with primary = false } ] }

let pick_primary_span (labels : label list) : span option =
  let rec first_primary = function
    | [] -> None
    | { span = NoSpan; primary = true; _ } :: rest -> first_primary rest
    | { span; primary = true; _ } :: _ -> Some span
    | _ :: rest -> first_primary rest
  in
  let rec first_with_span = function
    | [] -> None
    | { span = NoSpan; _ } :: rest -> first_with_span rest
    | { span; _ } :: _ -> Some span
  in
  match first_primary labels with
  | Some _ as span -> span
  | None -> first_with_span labels

let offset_to_loc_fallback (source : string option) (offset : int) : Source_loc.loc =
  match source with
  | Some src -> Source_loc.offset_to_loc src offset
  | None ->
      let normalized = max 0 offset in
      { Source_loc.line = 1; column = normalized + 1; offset = normalized }

let header_of_diagnostic ~(source_lookup : string -> string option) (diag : t) : string =
  let severity = severity_to_string diag.severity in
  match pick_primary_span diag.labels with
  | None -> Printf.sprintf "%s %s: %s" severity diag.code diag.message
  | Some NoSpan -> Printf.sprintf "%s %s: %s" severity diag.code diag.message
  | Some (Span { file_id; start_pos; end_pos = None }) ->
      let start_loc = offset_to_loc_fallback (source_lookup file_id) start_pos in
      Printf.sprintf "%s:%d:%d: %s %s: %s" file_id start_loc.line start_loc.column severity diag.code diag.message
  | Some (Span { file_id; start_pos; end_pos = Some end_pos }) ->
      let source = source_lookup file_id in
      let start_loc = offset_to_loc_fallback source start_pos in
      let end_loc = offset_to_loc_fallback source end_pos in
      Printf.sprintf "%s:%d:%d-%d:%d: %s %s: %s" file_id start_loc.line start_loc.column end_loc.line end_loc.column
        severity diag.code diag.message

let render_label_line ~(source_lookup : string -> string option) (label : label) : string option =
  match label.message with
  | None -> None
  | Some msg -> (
      match label.span with
      | NoSpan -> Some (Printf.sprintf "  = %s" msg)
      | Span { file_id; start_pos; end_pos = _ } ->
          let loc = offset_to_loc_fallback (source_lookup file_id) start_pos in
          Some (Printf.sprintf "  = %s:%d:%d: %s" file_id loc.line loc.column msg))

let render_cli ~(source_lookup : string -> string option) (diag : t) : string =
  let lines = ref [ header_of_diagnostic ~source_lookup diag ] in
  List.iter
    (fun label ->
      match render_label_line ~source_lookup label with
      | Some line -> lines := !lines @ [ line ]
      | None -> ())
    (List.filter (fun l -> not l.primary) diag.labels);
  List.iter (fun note -> lines := !lines @ [ Printf.sprintf "  note: %s" note ]) diag.notes;
  String.concat "\n" !lines

let render_many_cli ~(source_lookup : string -> string option) (diags : t list) : string =
  diags |> List.map (render_cli ~source_lookup) |> String.concat "\n\n"

module Test = struct
  let source_lookup (file_id : string) : string option =
    match file_id with
    | "main.mr" -> Some "let x = 1\nlet y = true\n"
    | _ -> None

  let%test "error_no_span constructor" =
    let diag = error_no_span ~code:"type-mismatch" ~message:"expected int" in
    diag.code = "type-mismatch"
    && diag.severity = Error
    && diag.message = "expected int"
    && diag.labels = []
    && diag.notes = []

  let%test "error_with_span constructor creates primary label" =
    let diag = error_with_span ~code:"type-unbound-var" ~message:"x not found" ~file_id:"main.mr" ~start_pos:4 () in
    match diag.labels with
    | [ { span = Span { file_id; start_pos; end_pos = None }; primary = true; _ } ] ->
        file_id = "main.mr" && start_pos = 4
    | _ -> false

  let%test "render no span header" =
    let diag = error_no_span ~code:"build-go-missing" ~message:"go not found" in
    render_cli ~source_lookup diag = "error build-go-missing: go not found"

  let%test "render point span header" =
    let diag = error_with_span ~code:"type-mismatch" ~message:"cannot unify" ~file_id:"main.mr" ~start_pos:4 () in
    render_cli ~source_lookup diag = "main.mr:1:5: error type-mismatch: cannot unify"

  let%test "render range span header" =
    let diag =
      error_with_span ~code:"type-mismatch" ~message:"cannot unify" ~file_id:"main.mr" ~start_pos:12 ~end_pos:15 ()
    in
    render_cli ~source_lookup diag = "main.mr:2:3-2:6: error type-mismatch: cannot unify"

  let%test "render_many_cli is deterministic" =
    let a = error_no_span ~code:"code-a" ~message:"first" in
    let b = error_no_span ~code:"code-b" ~message:"second" in
    let once = render_many_cli ~source_lookup [ a; b ] in
    let twice = render_many_cli ~source_lookup [ a; b ] in
    once = twice

  let%test "render fallback without source lookup" =
    let diag = error_with_span ~code:"parse-expected-token" ~message:"missing token" ~file_id:"unknown.mr" ~start_pos:9 () in
    render_cli ~source_lookup diag = "unknown.mr:1:10: error parse-expected-token: missing token"

  let%test "warning severity renders correctly" =
    let diag = { (error_no_span ~code:"lint-unused" ~message:"unused variable x") with severity = Warning } in
    render_cli ~source_lookup diag = "warning lint-unused: unused variable x"

  let%test "info severity renders correctly" =
    let diag = { (error_no_span ~code:"hint-refactor" ~message:"consider extracting") with severity = Info } in
    render_cli ~source_lookup diag = "info hint-refactor: consider extracting"

  let%test "warning with span renders correctly" =
    let diag =
      { (error_with_span ~code:"lint-unused" ~message:"unused variable x" ~file_id:"main.mr" ~start_pos:4 ())
        with severity = Warning }
    in
    render_cli ~source_lookup diag = "main.mr:1:5: warning lint-unused: unused variable x"

  let%test "notes and secondary label render in stable order" =
    let diag0 = error_with_span ~code:"type-mismatch" ~message:"headline" ~file_id:"main.mr" ~start_pos:4 () in
    let diag1 =
      with_secondary_label diag0
        (secondary_label ~message:"secondary hint" (Span { file_id = "main.mr"; start_pos = 12; end_pos = None }))
    in
    let diag2 = with_note diag1 "first note" in
    let diag = with_note diag2 "second note" in
    render_cli ~source_lookup diag
    = String.concat "\n"
        [ "main.mr:1:5: error type-mismatch: headline";
          "  = main.mr:2:3: secondary hint";
          "  note: first note";
          "  note: second note" ]
end
