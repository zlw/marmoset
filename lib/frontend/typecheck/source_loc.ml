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
  let offset = min offset (len - 1) |> max 0 in
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
    loc.line = 1 && loc.column = 5 (* at last char *)

  let%test "to_string formats correctly" =
    let loc = { line = 5; column = 10; offset = 42 } in
    to_string loc = "5:10"

  let%test "format_with_context shows line and caret" =
    let source = "let x = 5;\nlet y = true + 1;\nlet z = 3;" in
    let loc = offset_to_loc source 15 in
    (* 'y' in second line *)
    let formatted = format_with_context source loc in
    String.sub formatted 0 17 = "let y = true + 1;"
end
