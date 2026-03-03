(* LSP ↔ Marmoset position/range conversion utilities *)

module Lsp_t = Linol_lsp.Types

type line_index = {
  source : string;
  len : int;
  line_starts : int array;
}

let build_line_index ~(source : string) : line_index =
  let len = String.length source in
  let starts = ref [ 0 ] in
  for i = 0 to len - 1 do
    if source.[i] = '\n' then
      starts := (i + 1) :: !starts
  done;
  { source; len; line_starts = Array.of_list (List.rev !starts) }

(* Convert Marmoset Source_loc.loc (1-indexed) to LSP Position (0-indexed) *)
let loc_to_position (loc : Marmoset.Lib.Source_loc.loc) : Lsp_t.Position.t =
  Lsp_t.Position.create ~line:(loc.line - 1) ~character:(loc.column - 1)

let offset_to_position_with_index ~(index : line_index) ~(offset : int) : Lsp_t.Position.t =
  let offset = max 0 (min offset index.len) in
  let starts = index.line_starts in
  let rec binary_search low high best =
    if low > high then
      best
    else
      let mid = low + ((high - low) / 2) in
      let start = starts.(mid) in
      if start <= offset then
        binary_search (mid + 1) high mid
      else
        binary_search low (mid - 1) best
  in
  let line = binary_search 0 (Array.length starts - 1) 0 in
  let line_start = starts.(line) in
  Lsp_t.Position.create ~line ~character:(offset - line_start)

let position_to_offset_with_index ~(index : line_index) ~(line : int) ~(character : int) : int =
  let character = max 0 character in
  if line <= 0 then
    min character index.len
  else if line >= Array.length index.line_starts then
    index.len
  else
    min (index.line_starts.(line) + character) index.len

let offset_range_to_lsp_with_index ~(index : line_index) ~(pos : int) ~(end_pos : int) : Lsp_t.Range.t =
  let start = offset_to_position_with_index ~index ~offset:pos in
  let end_inclusive = offset_to_position_with_index ~index ~offset:end_pos in
  (* end_pos is inclusive in Marmoset but LSP Range.end is exclusive,
     so add 1 to the character *)
  let end_ = Lsp_t.Position.create ~line:end_inclusive.line ~character:(end_inclusive.character + 1) in
  Lsp_t.Range.create ~start ~end_

(* Convert a pos/end_pos byte offset pair to an LSP Range *)
let offset_range_to_lsp ~(source : string) ~(pos : int) ~(end_pos : int) : Lsp_t.Range.t =
  let index = build_line_index ~source in
  offset_range_to_lsp_with_index ~index ~pos ~end_pos

(* Convert LSP Position (0-indexed line/character) to byte offset in source.
   Clamps to [0, len] — offset=len represents end-of-file (one past last char). *)
let position_to_offset ~(source : string) ~(line : int) ~(character : int) : int =
  let index = build_line_index ~source in
  position_to_offset_with_index ~index ~line ~character

(* Convert a byte offset to an LSP Position (0-indexed) *)
let offset_to_position ~(source : string) ~(offset : int) : Lsp_t.Position.t =
  let index = build_line_index ~source in
  offset_to_position_with_index ~index ~offset

(* ============================================================
   Tests
   ============================================================ *)

let%test "loc_to_position converts 1-indexed to 0-indexed" =
  let loc : Marmoset.Lib.Source_loc.loc = { line = 3; column = 5; offset = 20 } in
  let pos = loc_to_position loc in
  pos.line = 2 && pos.character = 4

let%test "loc_to_position line 1 col 1 becomes 0,0" =
  let loc : Marmoset.Lib.Source_loc.loc = { line = 1; column = 1; offset = 0 } in
  let pos = loc_to_position loc in
  pos.line = 0 && pos.character = 0

let%test "offset_range_to_lsp single line" =
  let source = "let x = 42;" in
  let range = offset_range_to_lsp ~source ~pos:4 ~end_pos:4 in
  range.start.line = 0 && range.start.character = 4 && range.end_.line = 0 && range.end_.character = 5

let%test "offset_range_to_lsp multi-line" =
  let source = "let x = 5;\nlet y = true;" in
  (* "y" is at offset 15, line 2 col 5 (1-indexed) → 0-indexed line=1, char=4 *)
  let range = offset_range_to_lsp ~source ~pos:15 ~end_pos:15 in
  range.start.line = 1 && range.start.character = 4 && range.end_.line = 1 && range.end_.character = 5

let%test "position_to_offset first line" =
  let source = "let x = 42;" in
  let offset = position_to_offset ~source ~line:0 ~character:4 in
  offset = 4

let%test "position_to_offset second line" =
  let source = "let x = 5;\nlet y = true;" in
  let offset = position_to_offset ~source ~line:1 ~character:4 in
  offset = 15

let%test "position_to_offset beginning of file" =
  let source = "hello" in
  let offset = position_to_offset ~source ~line:0 ~character:0 in
  offset = 0

let%test "position_to_offset past end clamps to len" =
  let source = "hello" in
  let offset = position_to_offset ~source ~line:0 ~character:100 in
  offset = 5 (* one past last char = end of file *)

let%test "round trip: offset -> loc -> position -> offset" =
  let source = "let x = 5;\nlet y = 10;\nlet z = 15;" in
  let original_offset = 15 in
  let loc = Marmoset.Lib.Source_loc.offset_to_loc source original_offset in
  let pos = loc_to_position loc in
  let recovered = position_to_offset ~source ~line:pos.line ~character:pos.character in
  recovered = original_offset

let%test "indexed conversion matches non-indexed conversion" =
  let source = "line0\nline1\nline2\n" in
  let index = build_line_index ~source in
  let rec check offset =
    if offset > String.length source then
      true
    else
      let via_index = offset_to_position_with_index ~index ~offset in
      let via_plain = offset_to_position ~source ~offset in
      if via_index.line <> via_plain.line || via_index.character <> via_plain.character then
        false
      else
        check (offset + 1)
  in
  check 0

let%test "indexed position_to_offset matches non-indexed conversion" =
  let source = "a\nbb\nccc" in
  let index = build_line_index ~source in
  let coords = [ (0, 0); (0, 1); (1, 0); (1, 10); (2, 2); (10, 0) ] in
  List.for_all
    (fun (line, character) ->
      position_to_offset_with_index ~index ~line ~character = position_to_offset ~source ~line ~character)
    coords
