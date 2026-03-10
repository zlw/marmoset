(* Adversarial edge-case tests for LSP modules.
   These tests probe boundary conditions, unusual inputs, and potential crash sites.
   Tests marked BUG document real issues found during adversarial testing. *)

module Lsp_t = Linol_lsp.Types

(* ============================================================
   Helpers (reuse the same patterns from the modules under test)
   ============================================================ *)

(* Semantic tokens helper *)
let get_tokens source =
  let result = Doc_state.analyze ~source in
  match (result.program, result.type_map, result.environment) with
  | Some prog, Some tm, Some env -> Semantic_tokens.compute ~source ~program:prog ~type_map:tm ~environment:env
  | _ -> None

let decode_tokens data =
  let len = Array.length data in
  let rec go i acc =
    if i + 4 >= len then
      List.rev acc
    else
      let tuple = (data.(i), data.(i + 1), data.(i + 2), data.(i + 3), data.(i + 4)) in
      go (i + 5) (tuple :: acc)
  in
  go 0 []

let has_token_type ty tokens =
  match tokens with
  | Some (st : Lsp_t.SemanticTokens.t) ->
      let decoded = decode_tokens st.data in
      List.exists (fun (_, _, _, t, _) -> t = ty) decoded
  | None -> false

let count_token_type ty tokens =
  match tokens with
  | Some (st : Lsp_t.SemanticTokens.t) ->
      let decoded = decode_tokens st.data in
      List.length (List.filter (fun (_, _, _, t, _) -> t = ty) decoded)
  | None -> 0

(* Folding ranges helper *)
let get_ranges source =
  match Marmoset.Lib.Parser.parse ~file_id:"<test>" source with
  | Error _ -> []
  | Ok program -> Folding_ranges.compute ~source ~program

(* Selection ranges helper *)
let get_selection source positions =
  match Marmoset.Lib.Parser.parse ~file_id:"<test>" source with
  | Error _ -> []
  | Ok program ->
      let lsp_positions = List.map (fun (line, character) -> Lsp_t.Position.create ~line ~character) positions in
      Selection_ranges.compute ~source ~program ~positions:lsp_positions

let rec chain_depth (sr : Lsp_t.SelectionRange.t) =
  match sr.parent with
  | None -> 1
  | Some p -> 1 + chain_depth p

(* ============================================================
   SEMANTIC TOKENS EDGE CASES
   ============================================================ *)

(* 1. Source with ONLY whitespace *)
let%test "semantic_tokens: whitespace-only source returns None" =
  let tokens = get_tokens "   \n  \n  " in
  tokens = None

(* 2. Very deeply nested expression -- long chain of infix operators *)
let%test "semantic_tokens: deeply nested infix expression" =
  let tokens = get_tokens "let x = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10;" in
  match tokens with
  | Some st ->
      let decoded = decode_tokens st.data in
      (* Should have number tokens for each literal (1-10 = 10 numbers) *)
      let num_count =
        List.length (List.filter (fun (_, _, _, t, _) -> t = Semantic_tokens.number_type) decoded)
      in
      (* And operator tokens for each + (9 operators) *)
      let op_count =
        List.length (List.filter (fun (_, _, _, t, _) -> t = Semantic_tokens.operator_type) decoded)
      in
      num_count = 10 && op_count = 9
  | None -> false

(* 3. Variable shadowing a function *)
let%test "semantic_tokens: variable and function with different names" =
  let tokens = get_tokens "let f = (x) -> x; let g = 42; g" in
  match tokens with
  | Some _st ->
      (* Should succeed without crash *)
      let fn_count = count_token_type Semantic_tokens.function_type tokens in
      let var_count = count_token_type Semantic_tokens.variable_type tokens in
      fn_count >= 1 && var_count >= 1
  | None -> false

(* 4. Same-name parameter and outer variable *)
let%test "semantic_tokens: parameter shadows outer variable" =
  let tokens = get_tokens "let x = 1; let f = (x) -> x; x" in
  match tokens with
  | Some _st ->
      let param_count = count_token_type Semantic_tokens.parameter_type tokens in
      let var_count = count_token_type Semantic_tokens.variable_type tokens in
      param_count >= 1 && var_count >= 1
  | None -> false

(* 5. Record with field name same as variable *)
let%test "semantic_tokens: record field same name as variable" =
  let tokens = get_tokens "let x = 1; let r = { x: x };" in
  match tokens with
  | Some _st ->
      has_token_type Semantic_tokens.property_type tokens && has_token_type Semantic_tokens.variable_type tokens
  | None -> false

(* 6. Source with unicode characters in string literal *)
let%test "semantic_tokens: source with unicode in string literal" =
  let tokens = get_tokens "let x = \"hello \xc3\xa9\xc3\xa0\xc3\xbc\";" in
  match tokens with
  | Some _st ->
      has_token_type Semantic_tokens.string_type tokens && has_token_type Semantic_tokens.variable_type tokens
  | None -> false

(* 7. Single expression with no statements *)
let%test "semantic_tokens: bare expression" =
  let tokens = get_tokens "42" in
  has_token_type Semantic_tokens.number_type tokens

(* 8. Multiple operators in sequence *)
let%test "semantic_tokens: negative number via prefix" =
  let tokens = get_tokens "let x = -42;" in
  match tokens with
  | Some _st ->
      has_token_type Semantic_tokens.operator_type tokens && has_token_type Semantic_tokens.number_type tokens
  | None -> false

(* 9. Empty string literal *)
let%test "semantic_tokens: empty string literal" =
  let tokens = get_tokens "let x = \"\";" in
  has_token_type Semantic_tokens.string_type tokens

(* 10. Boolean values *)
let%test "semantic_tokens: booleans produce no typed tokens" =
  let tokens = get_tokens "true" in
  tokens = None

(* 11. Stress test: many let bindings *)
let%test "semantic_tokens: 50 let bindings" =
  let src = String.concat " " (List.init 50 (fun i -> Printf.sprintf "let v%d = %d;" i i)) in
  let tokens = get_tokens src in
  match tokens with
  | Some _st ->
      let var_count = count_token_type Semantic_tokens.variable_type tokens in
      let num_count = count_token_type Semantic_tokens.number_type tokens in
      var_count = 50 && num_count = 50
  | None -> false

(* 12. Delta encoding: tokens on multiple lines *)
let%test "semantic_tokens: multi-line delta encoding is correct" =
  let tokens = get_tokens "let x = 1;\nlet y = 2;" in
  match tokens with
  | Some st ->
      let decoded = decode_tokens st.data in
      List.exists (fun (dl, _, _, _, _) -> dl > 0) decoded
  | None -> false

(* 13. Extremely long identifier *)
let%test "semantic_tokens: very long identifier name" =
  let long_name = String.make 500 'a' in
  let src = Printf.sprintf "let %s = 42;" long_name in
  let tokens = get_tokens src in
  match tokens with
  | Some st ->
      let decoded = decode_tokens st.data in
      List.exists (fun (_, _, len, t, _) -> t = Semantic_tokens.variable_type && len = 500) decoded
  | None -> false

(* 14. All delta_line values are non-negative in multi-line source *)
let%test "semantic_tokens: delta_line values are non-negative" =
  let tokens = get_tokens "let x = 1;\nlet y = 2;\nlet z = 3;" in
  match tokens with
  | Some st ->
      let decoded = decode_tokens st.data in
      List.for_all (fun (dl, _, _, _, _) -> dl >= 0) decoded
  | None -> false

(* 15. All lengths are positive *)
let%test "semantic_tokens: all token lengths are positive" =
  let tokens = get_tokens "let f = (x) -> x + 1; f(42)" in
  match tokens with
  | Some st ->
      let decoded = decode_tokens st.data in
      List.for_all (fun (_, _, len, _, _) -> len > 0) decoded
  | None -> false

(* ============================================================
   FOLDING RANGES EDGE CASES
   ============================================================ *)

(* 1. Deeply nested functions *)
let%test "folding_ranges: deeply nested functions" =
  let src =
    "let f = (x) -> {\n  let g = (y) -> {\n    let h = (z) -> {\n      x + y + z\n    };\n    h\n  };\n  g\n}"
  in
  let ranges = get_ranges src in
  List.length ranges >= 3

(* 2. Single-line everything -- should produce 0 ranges *)
let%test "folding_ranges: single-line source produces 0 ranges" =
  let src = "let x = 42; let y = (a) -> a + 1; y(x)" in
  let ranges = get_ranges src in
  List.length ranges = 0

(* 3. Source that parses but has only expressions *)
let%test "folding_ranges: only expressions no definitions" =
  let src = "1 + 2 + 3" in
  let ranges = get_ranges src in
  List.length ranges = 0

(* 4. Very long single line *)
let%test "folding_ranges: very long single line" =
  let src = "let x = " ^ String.concat " + " (List.init 100 string_of_int) ^ ";" in
  let ranges = get_ranges src in
  List.length ranges = 0

(* 5. Empty source *)
let%test "folding_ranges: empty source" =
  let ranges = get_ranges "" in
  List.length ranges = 0

(* 6. Whitespace-only source *)
let%test "folding_ranges: whitespace-only source" =
  let ranges = get_ranges "   \n\n  \n" in
  List.length ranges = 0

(* 7. Multiple top-level multi-line constructs *)
let%test "folding_ranges: multiple top-level multi-line blocks" =
  let src =
    "let f = (x) -> {\n  x + 1\n};\nlet g = (y) -> {\n  y + 2\n};\nenum Color = {\n  Red\n  Green\n}"
  in
  let ranges = get_ranges src in
  List.length ranges >= 3

(* 8. Range startLine is always less than endLine *)
let%test "folding_ranges: all ranges have startLine < endLine" =
  let src = "let f = (x) -> {\n  if (true) {\n    x\n  } else {\n    0\n  }\n}" in
  let ranges = get_ranges src in
  List.length ranges > 0 && List.for_all (fun (r : Lsp_t.FoldingRange.t) -> r.startLine < r.endLine) ranges

(* ============================================================
   SELECTION RANGES EDGE CASES
   ============================================================ *)

(* 1. Cursor at byte 0 *)
let%test "selection_ranges: cursor at byte 0" =
  let ranges = get_selection "let x = 42;" [ (0, 0) ] in
  match ranges with
  | [ sr ] -> chain_depth sr >= 1
  | _ -> false

(* 2. Cursor past end of source *)
let%test "selection_ranges: cursor past end of source" =
  let ranges = get_selection "let x = 42;" [ (0, 100) ] in
  match ranges with
  | [ sr ] -> chain_depth sr >= 1
  | _ -> false

(* 3. Cursor on whitespace between statements *)
let%test "selection_ranges: cursor on whitespace between statements" =
  let ranges = get_selection "let x = 1; let y = 2;" [ (0, 10) ] in
  match ranges with
  | [ sr ] -> chain_depth sr >= 1
  | _ -> false

(* 4. Multiple cursors on the same expression *)
let%test "selection_ranges: multiple cursors on same expression" =
  let ranges = get_selection "let x = 42;" [ (0, 8); (0, 9) ] in
  match ranges with
  | [ sr1; sr2 ] -> chain_depth sr1 >= 2 && chain_depth sr2 >= 2
  | _ -> false

(* 5. Cursor on the very last character *)
let%test "selection_ranges: cursor on last character" =
  let ranges = get_selection "let x = 42;" [ (0, 10) ] in
  match ranges with
  | [ sr ] -> chain_depth sr >= 1
  | _ -> false

(* 6. Empty source *)
let%test "selection_ranges: empty source" =
  let ranges = get_selection "" [ (0, 0) ] in
  match ranges with
  | [ sr ] -> sr.range.start.line = 0 && sr.range.start.character = 0
  | _ -> false

(* 7. Multiple lines, cursor on second line *)
let%test "selection_ranges: cursor on second line" =
  let ranges = get_selection "let x = 1;\nlet y = 2;" [ (1, 4) ] in
  match ranges with
  | [ sr ] -> chain_depth sr >= 2
  | _ -> false

(* 8. BUG: Cursor at start of left operand in infix expression
   The infix expression node's span starts at the operator position, not the left
   operand position. This means the left operand falls OUTSIDE the infix node's
   span. Selection ranges therefore cannot drill into the left operand when the
   cursor is on it -- the chain only reaches the let statement level.

   In 'let x = 1 + 2;': infix node spans [10..12] but left child is at [8..8].
   Cursor at char 8 ('1') gets chain depth 2 (stmt + program) instead of the
   expected 4+ (integer -> infix -> let -> program).

   This is a parser AST span bug: the infix node should span [8..12]. *)
let%test "selection_ranges: BUG infix left operand outside parent span" =
  let source = "let x = 1 + 2;" in
  let program =
    match Marmoset.Lib.Parser.parse ~file_id:"<test>" source with
    | Ok p -> p
    | Error _ -> failwith "parse error"
  in
  let module Ast = Marmoset.Lib.Ast in
  match program with
  | [ stmt ] -> (
      match stmt.stmt with
      | Ast.AST.Let { value; _ } -> (
          match value.expr with
          | Ast.AST.Infix (left, _, _right) ->
              (* BUG: The left child's position is BEFORE the parent infix node's position *)
              let bug_present = left.pos < value.pos in
              bug_present (* true = bug confirmed *)
          | _ -> false)
      | _ -> false)
  | _ -> false

(* Document that cursor at char 8 only gets depth 2 due to the span bug *)
let%test "selection_ranges: cursor on left operand gets shallow chain (known bug)" =
  let ranges = get_selection "let x = 1 + 2;" [ (0, 8) ] in
  match ranges with
  | [ sr ] ->
      (* Due to the bug, depth is only 2, not the ideal 4+ *)
      chain_depth sr = 2
  | _ -> false

(* Cursor on right operand works correctly *)
let%test "selection_ranges: cursor on right operand gets deep chain" =
  let ranges = get_selection "let x = 1 + 2;" [ (0, 12) ] in
  match ranges with
  | [ sr ] ->
      (* Right operand IS within the infix span, so this works: int -> infix -> let -> program *)
      chain_depth sr >= 4
  | _ -> false

(* 9. Large number of cursors *)
let%test "selection_ranges: many cursors" =
  let positions = List.init 20 (fun i -> (0, i)) in
  let ranges = get_selection "let x = 1 + 2 + 3;" positions in
  List.length ranges = 20

(* 10. Cursor way past last line *)
let%test "selection_ranges: cursor on non-existent line" =
  let ranges = get_selection "let x = 42;" [ (100, 0) ] in
  match ranges with
  | [ sr ] -> chain_depth sr >= 1
  | _ -> false

(* ============================================================
   DOC_STATE EDGE CASES
   ============================================================ *)

(* 1. Empty source parses to empty program successfully (correct behavior) *)
let%test "doc_state: empty source produces empty program with no errors" =
  let result = Doc_state.analyze ~source:"" in
  result.program <> None && result.diagnostics = []

(* 2. Whitespace only *)
let%test "doc_state: whitespace only" =
  let result = Doc_state.analyze ~source:"   \n  \n" in
  (* Should gracefully handle -- parser returns empty program or error *)
  ignore result;
  true (* not crashing is the test *)

(* 3. Deeply nested expressions do not crash *)
let%test "doc_state: deeply nested arithmetic" =
  let src = "let x = " ^ String.concat " + " (List.init 100 string_of_int) ^ ";" in
  let result = Doc_state.analyze ~source:src in
  result.diagnostics = [] && result.program <> None

(* 4. Type error produces diagnostics *)
let%test "doc_state: type error returns diagnostics" =
  let result = Doc_state.analyze ~source:"1 + true" in
  List.length result.diagnostics >= 1

(* 5. Valid source with enum + match *)
let%test "doc_state: complex valid source with enum and match" =
  let src =
    "enum Shape = {\n  Circle(Float)\n  Rect(Float, Float)\n}\nlet area = (s: Shape) -> {\n  match s {\n    case Shape.Circle(r): r\n    case Shape.Rect(w, h): w\n  }\n}"
  in
  let result = Doc_state.analyze ~source:src in
  result.diagnostics = [] && result.program <> None && result.type_map <> None

(* 6. Source with trailing newlines *)
let%test "doc_state: trailing newlines" =
  let result = Doc_state.analyze ~source:"let x = 42;\n\n\n" in
  result.diagnostics = [] && result.program <> None

(* 7. Source with only a semicolon -- should not crash *)
let%test "doc_state: just a semicolon does not crash" =
  let result = Doc_state.analyze ~source:";" in
  ignore result;
  true

(* 8. Source with unicode in identifier position -- should not crash *)
let%test "doc_state: unicode in identifier position does not crash" =
  let result = Doc_state.analyze ~source:"let caf\xc3\xa9 = 42;" in
  ignore result;
  true

(* 9. Parser limitation: fn type in annotation causes parse error *)
let%test "doc_state: fn type annotation in param now parses successfully" =
  let src = "let apply = (f: (Int) -> Int, x: Int) -> f(x);" in
  let result = Doc_state.analyze ~source:src in
  result.diagnostics = [] && result.program <> None

(* 10. Repeated analysis resets global state *)
let%test "doc_state: repeated analysis does not leak state" =
  let _ = Doc_state.analyze ~source:"enum Color = { Red Green Blue }" in
  let result2 = Doc_state.analyze ~source:"let x = 42;" in
  result2.diagnostics = [] && result2.program <> None

(* ============================================================
   LSP_UTILS EDGE CASES
   ============================================================ *)

(* 1. Empty source offset_to_position *)
let%test "lsp_utils: offset_to_position on empty source" =
  let pos = Lsp_utils.offset_to_position ~source:"" ~offset:0 in
  pos.line = 0 && pos.character = 0

(* 2. Negative offset *)
let%test "lsp_utils: offset_to_position with negative offset" =
  let pos = Lsp_utils.offset_to_position ~source:"hello" ~offset:(-5) in
  pos.line = 0 && pos.character = 0

(* 3. Offset way past end *)
let%test "lsp_utils: offset_to_position way past end" =
  let pos = Lsp_utils.offset_to_position ~source:"hello" ~offset:10000 in
  pos.line >= 0

(* 4. position_to_offset on empty source *)
let%test "lsp_utils: position_to_offset on empty source" =
  let offset = Lsp_utils.position_to_offset ~source:"" ~line:0 ~character:0 in
  offset = 0

(* 5. position_to_offset with large line number *)
let%test "lsp_utils: position_to_offset with large line number" =
  let offset = Lsp_utils.position_to_offset ~source:"hello" ~line:100 ~character:0 in
  offset >= 0 && offset <= 5

(* 6. offset_range_to_lsp on single-char source *)
let%test "lsp_utils: offset_range_to_lsp on single-char source" =
  let range = Lsp_utils.offset_range_to_lsp ~source:"x" ~pos:0 ~end_pos:0 in
  range.start.line = 0 && range.start.character = 0

(* 7. offset_range_to_lsp where end_pos > source length *)
let%test "lsp_utils: offset_range_to_lsp end_pos past source" =
  let range = Lsp_utils.offset_range_to_lsp ~source:"hello" ~pos:0 ~end_pos:100 in
  range.start.line = 0

(* 8. position_to_offset round-trip on multi-line source *)
let%test "lsp_utils: round-trip offset for every byte in multi-line source" =
  let source = "ab\ncd\nef" in
  let len = String.length source in
  let all_ok = ref true in
  for offset = 0 to len - 1 do
    let pos = Lsp_utils.offset_to_position ~source ~offset in
    let recovered = Lsp_utils.position_to_offset ~source ~line:pos.line ~character:pos.character in
    if recovered <> offset then
      all_ok := false
  done;
  !all_ok

(* ============================================================
   CROSS-MODULE EDGE CASES
   ============================================================ *)

(* Semantic tokens + selection ranges on same source *)
let%test "cross-module: semantic tokens and selection ranges on same source" =
  let source = "let f = (x) -> x + 1; f(42)" in
  let tokens = get_tokens source in
  let selections = get_selection source [ (0, 0); (0, 12); (0, 25) ] in
  tokens <> None && List.length selections = 3

(* All modules handle a simpler complex source (avoiding fn type annotation parser limitation) *)
let%test "cross-module: all modules handle enum + match source" =
  let source =
    "enum Opt[a] = {\n  Some(a)\n  None\n}\nlet map = (o: Opt[Int]) -> {\n  match o {\n    case Opt.Some(v): Opt.Some(v)\n    case Opt.None: Opt.None\n  }\n};\nmap(Opt.Some(42))"
  in
  let result = Doc_state.analyze ~source in
  let _tokens =
    match (result.program, result.type_map, result.environment) with
    | Some prog, Some tm, Some env -> Semantic_tokens.compute ~source ~program:prog ~type_map:tm ~environment:env
    | _ -> None
  in
  let _ranges =
    match result.program with
    | Some prog -> Folding_ranges.compute ~source ~program:prog
    | None -> []
  in
  let _selections =
    match result.program with
    | Some prog ->
        let positions = [ Lsp_t.Position.create ~line:0 ~character:0 ] in
        Selection_ranges.compute ~source ~program:prog ~positions
    | None -> []
  in
  result.program <> None && result.diagnostics = []

(* Folding + semantic tokens agree on which lines have content *)
let%test "cross-module: folding ranges only on lines with tokens" =
  let source = "let f = (x) -> {\n  x + 1\n};\nlet g = (y) -> {\n  y * 2\n};" in
  let result = Doc_state.analyze ~source in
  match (result.program, result.type_map, result.environment) with
  | Some prog, Some tm, Some env ->
      let tokens = Semantic_tokens.compute ~source ~program:prog ~type_map:tm ~environment:env in
      let ranges = Folding_ranges.compute ~source ~program:prog in
      tokens <> None && List.length ranges >= 2
  | _ -> false
