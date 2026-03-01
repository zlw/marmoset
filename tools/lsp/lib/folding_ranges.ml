(* Folding ranges: walk AST and emit collapsible regions for multi-line constructs *)

module Lsp_t = Linol_lsp.Types
module Ast = Marmoset.Lib.Ast

(* Emit a folding range only if it spans 2+ lines.
   Uses offset_range_to_lsp to correctly handle Marmoset's inclusive end_pos
   vs LSP's exclusive end position. *)
let maybe_range ~source ~pos ~end_pos ~kind ranges =
  let range = Lsp_utils.offset_range_to_lsp ~source ~pos ~end_pos in
  if range.end_.line > range.start.line then
    ranges :=
      Lsp_t.FoldingRange.create ~startLine:range.start.line ~startCharacter:range.start.character
        ~endLine:range.end_.line ~endCharacter:range.end_.character ~kind ()
      :: !ranges

(* Walk an expression collecting folding ranges *)
let rec walk_expr ~source ~ranges (expr : Ast.AST.expression) =
  match expr.expr with
  | Ast.AST.Function { body; _ } ->
      (* Emit range for the whole function (fn(...) { ... }), not the inner block *)
      maybe_range ~source ~pos:expr.pos ~end_pos:expr.end_pos ~kind:Lsp_t.FoldingRangeKind.Region ranges;
      walk_stmt_skip_block ~source ~ranges body
  | Ast.AST.If (cond, then_, else_) -> (
      walk_expr ~source ~ranges cond;
      walk_stmt ~source ~ranges then_;
      match else_ with
      | Some else_stmt -> walk_stmt ~source ~ranges else_stmt
      | None -> ())
  | Ast.AST.Match (scrutinee, arms) ->
      maybe_range ~source ~pos:expr.pos ~end_pos:expr.end_pos ~kind:Lsp_t.FoldingRangeKind.Region ranges;
      walk_expr ~source ~ranges scrutinee;
      List.iter (fun (arm : Ast.AST.match_arm) -> walk_expr ~source ~ranges arm.body) arms
  | Ast.AST.RecordLit (fields, spread) ->
      maybe_range ~source ~pos:expr.pos ~end_pos:expr.end_pos ~kind:Lsp_t.FoldingRangeKind.Region ranges;
      List.iter (fun (f : Ast.AST.record_field) -> Option.iter (walk_expr ~source ~ranges) f.field_value) fields;
      Option.iter (walk_expr ~source ~ranges) spread
  | Ast.AST.Array elts ->
      maybe_range ~source ~pos:expr.pos ~end_pos:expr.end_pos ~kind:Lsp_t.FoldingRangeKind.Region ranges;
      List.iter (walk_expr ~source ~ranges) elts
  | Ast.AST.Call (fn_expr, args) ->
      maybe_range ~source ~pos:expr.pos ~end_pos:expr.end_pos ~kind:Lsp_t.FoldingRangeKind.Region ranges;
      walk_expr ~source ~ranges fn_expr;
      List.iter (walk_expr ~source ~ranges) args
  | Ast.AST.Infix (l, _, r) ->
      walk_expr ~source ~ranges l;
      walk_expr ~source ~ranges r
  | Ast.AST.Prefix (_, e) -> walk_expr ~source ~ranges e
  | Ast.AST.Index (arr, idx) ->
      walk_expr ~source ~ranges arr;
      walk_expr ~source ~ranges idx
  | Ast.AST.Hash pairs ->
      maybe_range ~source ~pos:expr.pos ~end_pos:expr.end_pos ~kind:Lsp_t.FoldingRangeKind.Region ranges;
      List.iter
        (fun (k, v) ->
          walk_expr ~source ~ranges k;
          walk_expr ~source ~ranges v)
        pairs
  | Ast.AST.FieldAccess (e, _) -> walk_expr ~source ~ranges e
  | Ast.AST.MethodCall (recv, _, args) ->
      maybe_range ~source ~pos:expr.pos ~end_pos:expr.end_pos ~kind:Lsp_t.FoldingRangeKind.Region ranges;
      walk_expr ~source ~ranges recv;
      List.iter (walk_expr ~source ~ranges) args
  | Ast.AST.EnumConstructor (_, _, args) -> List.iter (walk_expr ~source ~ranges) args
  | Ast.AST.TypeCheck (e, _) -> walk_expr ~source ~ranges e
  | Ast.AST.Identifier _ | Ast.AST.Integer _ | Ast.AST.Float _ | Ast.AST.Boolean _ | Ast.AST.String _ -> ()

(* Walk a statement but skip Block-level range emission to avoid duplicates
   with the parent construct that already emitted a range *)
and walk_stmt_skip_block ~source ~ranges (stmt : Ast.AST.statement) =
  match stmt.stmt with
  | Ast.AST.Block stmts -> List.iter (walk_stmt ~source ~ranges) stmts
  | _ -> walk_stmt ~source ~ranges stmt

(* Walk a statement collecting folding ranges *)
and walk_stmt ~source ~ranges (stmt : Ast.AST.statement) =
  match stmt.stmt with
  | Ast.AST.Let { value; _ } -> walk_expr ~source ~ranges value
  | Ast.AST.ExpressionStmt e -> walk_expr ~source ~ranges e
  | Ast.AST.Return e -> walk_expr ~source ~ranges e
  | Ast.AST.Block stmts ->
      maybe_range ~source ~pos:stmt.pos ~end_pos:stmt.end_pos ~kind:Lsp_t.FoldingRangeKind.Region ranges;
      List.iter (walk_stmt ~source ~ranges) stmts
  | Ast.AST.EnumDef _ ->
      maybe_range ~source ~pos:stmt.pos ~end_pos:stmt.end_pos ~kind:Lsp_t.FoldingRangeKind.Region ranges
  | Ast.AST.TraitDef { methods; _ } ->
      maybe_range ~source ~pos:stmt.pos ~end_pos:stmt.end_pos ~kind:Lsp_t.FoldingRangeKind.Region ranges;
      List.iter
        (fun (m : Ast.AST.method_sig) -> Option.iter (walk_expr ~source ~ranges) m.method_default_impl)
        methods
  | Ast.AST.ImplDef { impl_methods; _ } ->
      maybe_range ~source ~pos:stmt.pos ~end_pos:stmt.end_pos ~kind:Lsp_t.FoldingRangeKind.Region ranges;
      List.iter (fun (m : Ast.AST.method_impl) -> walk_expr ~source ~ranges m.impl_method_body) impl_methods
  | Ast.AST.InherentImplDef { inherent_methods; _ } ->
      maybe_range ~source ~pos:stmt.pos ~end_pos:stmt.end_pos ~kind:Lsp_t.FoldingRangeKind.Region ranges;
      List.iter (fun (m : Ast.AST.method_impl) -> walk_expr ~source ~ranges m.impl_method_body) inherent_methods
  | Ast.AST.DeriveDef _ | Ast.AST.TypeAlias _ -> ()

(* Public entry point *)
let compute ~(source : string) ~(program : Ast.AST.program) : Lsp_t.FoldingRange.t list =
  let ranges = ref [] in
  List.iter (walk_stmt ~source ~ranges) program;
  List.rev !ranges

(* ============================================================
   Tests
   ============================================================ *)

(* Helper: parse and compute folding ranges *)
let get_ranges source =
  match Marmoset.Lib.Parser.parse source with
  | Error _ -> []
  | Ok program -> compute ~source ~program

let%test "multi-line function body produces folding range" =
  let ranges = get_ranges "let f = fn(x) {\n  x + 1\n}" in
  (* Exactly 1 range for the function, no duplicate from the inner block *)
  List.length ranges = 1

let%test "single-line function produces no folding range" =
  let ranges = get_ranges "let f = fn(x) { x }" in
  List.length ranges = 0

let%test "multi-line enum produces folding range" =
  let ranges = get_ranges "enum color {\n  red\n  green\n  blue\n}" in
  List.length ranges >= 1

let%test "multi-line if/else produces ranges for each branch" =
  let src = "let f = fn(x) {\n  if (x == 0) {\n    x\n  } else {\n    0\n  }\n}" in
  let ranges = get_ranges src in
  (* Function (1) + then block (1) + else block (1) = 3, no duplicates *)
  List.length ranges = 3

let%test "single-line constructs produce no ranges" =
  let ranges = get_ranges "let x = 42;" in
  List.length ranges = 0

let%test "folding range startLine < endLine" =
  let ranges = get_ranges "let f = fn(x) {\n  x + 1\n}" in
  List.for_all (fun (r : Lsp_t.FoldingRange.t) -> r.startLine < r.endLine) ranges

let%test "match expression produces folding range" =
  let src =
    "enum opt[a] {\n  some(a)\n  none\n}\nlet f = fn(x: opt[int]) {\n  match x {\n    opt.some(v): v\n    opt.none: 0\n  }\n}"
  in
  let ranges = get_ranges src in
  (* enum (1) + function (1) + match (1) = 3, no duplicates *)
  List.length ranges = 3
