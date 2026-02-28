(* LSP ↔ Marmoset position/range conversion utilities *)

module Lsp_t = Linol_lsp.Types

(* Convert Marmoset Source_loc.loc (1-indexed) to LSP Position (0-indexed) *)
let loc_to_position (loc : Marmoset.Lib.Source_loc.loc) : Lsp_t.Position.t =
  Lsp_t.Position.create ~line:(loc.line - 1) ~character:(loc.column - 1)

(* Convert a pos/end_pos byte offset pair to an LSP Range *)
let offset_range_to_lsp ~(source : string) ~(pos : int) ~(end_pos : int) : Lsp_t.Range.t =
  let start_loc = Marmoset.Lib.Source_loc.offset_to_loc source pos in
  let end_loc = Marmoset.Lib.Source_loc.offset_to_loc source end_pos in
  let start = loc_to_position start_loc in
  (* end_pos is inclusive in Marmoset but LSP Range.end is exclusive,
     so add 1 to the character *)
  let end_ = Lsp_t.Position.create ~line:(end_loc.line - 1) ~character:end_loc.column in
  Lsp_t.Range.create ~start ~end_

(* Convert LSP Position (0-indexed line/character) to byte offset in source *)
let position_to_offset ~(source : string) ~(line : int) ~(character : int) : int =
  let len = String.length source in
  let rec scan pos current_line =
    if current_line >= line then
      (* We're on the target line, advance by character count *)
      min (pos + character) (len - 1) |> max 0
    else if pos >= len then
      len
    else
      match source.[pos] with
      | '\n' -> scan (pos + 1) (current_line + 1)
      | _ -> scan (pos + 1) current_line
  in
  scan 0 0

(* Convert a byte offset to an LSP Position (0-indexed) *)
let offset_to_position ~(source : string) ~(offset : int) : Lsp_t.Position.t =
  loc_to_position (Marmoset.Lib.Source_loc.offset_to_loc source offset)

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

let%test "position_to_offset past end clamps" =
  let source = "hello" in
  let offset = position_to_offset ~source ~line:0 ~character:100 in
  offset = 4 (* last valid index *)

let%test "round trip: offset -> loc -> position -> offset" =
  let source = "let x = 5;\nlet y = 10;\nlet z = 15;" in
  let original_offset = 15 in
  let loc = Marmoset.Lib.Source_loc.offset_to_loc source original_offset in
  let pos = loc_to_position loc in
  let recovered = position_to_offset ~source ~line:pos.line ~character:pos.character in
  recovered = original_offset
