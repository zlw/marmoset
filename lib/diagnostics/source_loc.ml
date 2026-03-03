(* Source location utilities for error reporting *)

(* A location in source code *)
type loc = {
  line : int; (* 1-indexed line number *)
  column : int; (* 1-indexed column number *)
  offset : int; (* 0-indexed byte offset *)
}

(* Convert a byte offset to line:column using the source string.
   Lines and columns are 1-indexed for human readability. *)
let offset_to_loc (source : string) (offset : int) : loc =
  let len = String.length source in
  let offset = min offset len |> max 0 in
  let rec scan pos line col =
    if pos >= offset then
      { line; column = col; offset }
    else if pos >= len then
      { line; column = col; offset }
    else
      match source.[pos] with
      | '\n' -> scan (pos + 1) (line + 1) 1
      | _ -> scan (pos + 1) line (col + 1)
  in
  scan 0 1 1

(* Format a location as "line:column" *)
let to_string (loc : loc) : string = Printf.sprintf "%d:%d" loc.line loc.column

let to_string_range (start_loc : loc) (end_loc : loc) : string =
  if start_loc.line = end_loc.line && start_loc.column = end_loc.column then
    to_string start_loc
  else
    Printf.sprintf "%s-%s" (to_string start_loc) (to_string end_loc)

(* Format a location with context, showing the relevant line and a caret *)
let format_with_context (source : string) (loc : loc) : string =
  (* Find the start of the line *)
  let line_start =
    let rec find_start pos =
      if pos <= 0 then
        0
      else if source.[pos - 1] = '\n' then
        pos
      else
        find_start (pos - 1)
    in
    find_start loc.offset
  in
  (* Find the end of the line *)
  let line_end =
    let len = String.length source in
    let rec find_end pos =
      if pos >= len then
        pos
      else if source.[pos] = '\n' then
        pos
      else
        find_end (pos + 1)
    in
    find_end loc.offset
  in
  (* Extract the line *)
  let line_content = String.sub source line_start (line_end - line_start) in
  (* Create the caret line *)
  let caret_pos = loc.column - 1 in
  let caret_line = String.make caret_pos ' ' ^ "^" in
  Printf.sprintf "%s\n%s" line_content caret_line

let format_with_context_range (source : string) (start_loc : loc) (end_loc : loc) : string =
  if start_loc.line <> end_loc.line then
    format_with_context source start_loc
  else
    (* Find the start of the line *)
    let line_start =
      let rec find_start pos =
        if pos <= 0 then
          0
        else if source.[pos - 1] = '\n' then
          pos
        else
          find_start (pos - 1)
      in
      find_start start_loc.offset
    in
    (* Find the end of the line *)
    let line_end =
      let len = String.length source in
      let rec find_end pos =
        if pos >= len then
          pos
        else if source.[pos] = '\n' then
          pos
        else
          find_end (pos + 1)
      in
      find_end start_loc.offset
    in
    let line_content = String.sub source line_start (line_end - line_start) in
    let caret_start = max 0 (start_loc.column - 1) in
    let caret_end = max caret_start (end_loc.column - 1) in
    let caret_len = max 1 (caret_end - caret_start + 1) in
    let caret_line = String.make caret_start ' ' ^ String.make caret_len '^' in
    Printf.sprintf "%s\n%s" line_content caret_line

module Test = struct
  let%test "offset_to_loc single line" =
    let source = "hello world" in
    let loc = offset_to_loc source 6 in
    loc.line = 1 && loc.column = 7

  let%test "offset_to_loc multi line" =
    let source = "hello\nworld" in
    let loc = offset_to_loc source 6 in
    loc.line = 2 && loc.column = 1

  let%test "offset_to_loc second char of second line" =
    let source = "hello\nworld" in
    let loc = offset_to_loc source 7 in
    loc.line = 2 && loc.column = 2

  let%test "offset_to_loc beginning" =
    let source = "hello" in
    let loc = offset_to_loc source 0 in
    loc.line = 1 && loc.column = 1

  let%test "offset_to_loc handles negative offset" =
    let source = "hello" in
    let loc = offset_to_loc source (-5) in
    loc.line = 1 && loc.column = 1

  let%test "offset_to_loc handles offset past end" =
    let source = "hello" in
    let loc = offset_to_loc source 100 in
    loc.line = 1 && loc.column = 6 (* one past last char *)

  let%test "to_string formats correctly" =
    let loc = { line = 5; column = 10; offset = 42 } in
    to_string loc = "5:10"

  let%test "to_string_range formats ranges" =
    let start_loc = { line = 3; column = 4; offset = 20 } in
    let end_loc = { line = 3; column = 8; offset = 24 } in
    to_string_range start_loc end_loc = "3:4-3:8"

  let%test "format_with_context shows line and caret" =
    let source = "let x = 5;\nlet y = true + 1;\nlet z = 3;" in
    let loc = offset_to_loc source 15 in
    (* 'y' in second line *)
    let formatted = format_with_context source loc in
    String.sub formatted 0 17 = "let y = true + 1;"

  let%test "format_with_context_range shows multi-caret span" =
    let source = "let value = true + 1;" in
    let start_loc = offset_to_loc source 4 in
    let end_loc = offset_to_loc source 8 in
    let formatted = format_with_context_range source start_loc end_loc in
    String.contains formatted '^'
    && String.length formatted > String.length source
    && String.sub formatted 0 (String.length source) = source
end
