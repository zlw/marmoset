module Lexer = Marmoset.Lib.Lexer
module Token = Marmoset.Lib.Token

type segment = {
  name : string;
  start_pos : int;
  end_pos : int;
}

type header = {
  path_segments : segment list;
  alias_segment : segment option;
  trailing_dot : int option;
  start_pos : int;
  end_pos : int;
}

type cursor_target =
  | Path_segment of int
  | Alias

type completion_path_context = {
  typed_segments : string list;
  prefix : string;
  in_alias : bool;
}

let token_end_pos (tok : Token.token) : int = max tok.pos (tok.pos + String.length tok.literal - 1)

let parse_path_segment (tok : Token.token) : segment =
  { name = tok.literal; start_pos = tok.pos; end_pos = token_end_pos tok }

let parse_import_header (tokens : Token.token array) (start_idx : int) : (header * int) option =
  let len = Array.length tokens in
  if start_idx >= len || tokens.(start_idx).token_type <> Token.Import then
    None
  else
    let rec collect_path idx acc trailing_dot =
      if idx >= len then
        (List.rev acc, None, trailing_dot, idx)
      else
        match tokens.(idx).token_type with
        | Token.Ident ->
            let trailing_dot =
              match trailing_dot with
              | Some _ -> None
              | None -> None
            in
            collect_after_ident (idx + 1) (parse_path_segment tokens.(idx) :: acc) trailing_dot
        | _ -> (List.rev acc, None, trailing_dot, idx)
    and collect_after_ident idx acc trailing_dot =
      if idx >= len then
        (List.rev acc, None, trailing_dot, idx)
      else
        match tokens.(idx).token_type with
        | Token.Dot -> (
            if idx + 1 < len && tokens.(idx + 1).token_type = Token.Ident then
              collect_path (idx + 1) acc None
            else
              (List.rev acc, None, Some tokens.(idx).pos, idx + 1))
        | Token.As ->
            if idx + 1 < len && tokens.(idx + 1).token_type = Token.Ident then
              (List.rev acc, Some (parse_path_segment tokens.(idx + 1)), trailing_dot, idx + 2)
            else
              (List.rev acc, None, trailing_dot, idx + 1)
        | _ -> (List.rev acc, None, trailing_dot, idx)
    in
    let path_segments, alias_segment, trailing_dot, next_idx = collect_path (start_idx + 1) [] None in
    match path_segments with
    | [] -> None
    | _ ->
        let end_pos =
          match alias_segment with
          | Some alias -> alias.end_pos
          | None -> (
              match trailing_dot with
              | Some dot_pos -> dot_pos
              | None ->
                  match List.rev path_segments with
                  | [] -> tokens.(start_idx).pos
                  | segment :: _ -> segment.end_pos)
        in
        Some ({ path_segments; alias_segment; trailing_dot; start_pos = tokens.(start_idx).pos; end_pos }, next_idx)

let all_headers ~(source : string) : header list =
  let tokens = Array.of_list (Lexer.lex source) in
  let len = Array.length tokens in
  let rec walk idx acc =
    if idx >= len then
      List.rev acc
    else
      match parse_import_header tokens idx with
      | Some (header, next_idx) -> walk next_idx (header :: acc)
      | None -> walk (idx + 1) acc
  in
  walk 0 []

let find_header_target_at_offset ~(source : string) ~(offset : int) : (header * cursor_target) option =
  let target_in_header (header : header) =
    match header.alias_segment with
    | Some alias when offset >= alias.start_pos && offset <= alias.end_pos -> Some (header, Alias)
    | _ ->
        let rec find_path_segment (idx : int) (segments : segment list) =
          match segments with
          | [] -> None
          | (segment : segment) :: rest ->
              if offset >= segment.start_pos && offset <= segment.end_pos then
                Some (header, Path_segment idx)
              else
                find_path_segment (idx + 1) rest
        in
        find_path_segment 0 header.path_segments
  in
  List.find_map target_in_header (all_headers ~source)

let path_segment_names (header : header) : string list = List.map (fun segment -> segment.name) header.path_segments

let prefix_until ~(source : string) ~(start_pos : int) ~(offset : int) : string =
  let len = max 0 (offset - start_pos) in
  String.sub source start_pos len

let completion_context_at_offset ~(source : string) ~(offset : int) : completion_path_context option =
  let header_matches (header : header) =
    let after_trailing_dot =
      match header.trailing_dot with
      | Some dot_pos -> offset = dot_pos + 1
      | None -> false
    in
    (offset >= header.start_pos && offset <= header.end_pos + 1) || after_trailing_dot
  in
  let classify (header : header) =
    match header.alias_segment with
    | Some alias when offset >= alias.start_pos && offset <= alias.end_pos + 1 ->
        Some { typed_segments = path_segment_names header; prefix = prefix_until ~source ~start_pos:alias.start_pos ~offset; in_alias = true }
    | _ -> (
        match header.trailing_dot with
        | Some dot_pos when offset = dot_pos + 1 ->
            Some { typed_segments = path_segment_names header; prefix = ""; in_alias = false }
        | _ ->
            let rec find_segment idx typed = function
              | [] -> None
              | (segment : segment) :: rest ->
                  if offset >= segment.start_pos && offset <= segment.end_pos + 1 then
                    Some
                      {
                        typed_segments = List.rev typed;
                        prefix = prefix_until ~source ~start_pos:segment.start_pos ~offset;
                        in_alias = false;
                      }
                  else
                    find_segment (idx + 1) (segment.name :: typed) rest
            in
            find_segment 0 [] header.path_segments)
  in
  List.find_map
    (fun header ->
      if header_matches header then
        classify header
      else
        None)
    (all_headers ~source)

let%test "all_headers keeps path segments and alias positions" =
  match all_headers ~source:"import math.add as plus\nimport collections.list\n" with
  | [ first; second ] ->
      path_segment_names first = [ "math"; "add" ]
      && Option.map (fun alias -> alias.name) first.alias_segment = Some "plus"
      && path_segment_names second = [ "collections"; "list" ]
  | _ -> false

let%test "find_header_target_at_offset finds alias tokens" =
  match find_header_target_at_offset ~source:"import math.add as plus\n" ~offset:19 with
  | Some (_, Alias) -> true
  | _ -> false

let%test "all_headers preserves trailing dot for parse-incomplete imports" =
  match all_headers ~source:"import math.\n" with
  | [ header ] -> path_segment_names header = [ "math" ] && header.trailing_dot = Some 11
  | _ -> false

let%test "completion_context_at_offset captures trailing-dot import paths" =
  match completion_context_at_offset ~source:"import math.\n" ~offset:12 with
  | Some { typed_segments; prefix; in_alias } -> typed_segments = [ "math" ] && prefix = "" && not in_alias
  | _ -> false

let%test "completion_context_at_offset keeps alias edits out of import-path context" =
  match completion_context_at_offset ~source:"import math.add as pl\n" ~offset:21 with
  | Some { in_alias; _ } -> in_alias
  | _ -> false
