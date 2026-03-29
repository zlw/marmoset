(* Selection ranges: for each cursor position, build a nested chain from innermost to outermost AST node *)

module Lsp_t = Linol_lsp.Types
module Ast = Marmoset.Lib.Ast

(* Convert byte offset range to LSP Range *)
let make_range ~source ~pos ~end_pos = Lsp_utils.offset_range_to_lsp ~source ~pos ~end_pos

(* Build a SelectionRange with optional parent *)
let sel_range ~source ~pos ~end_pos ~parent =
  let range = make_range ~source ~pos ~end_pos in
  Lsp_t.SelectionRange.create ~range ?parent ()

(* Find the deepest expression containing the offset, returning a chain of ranges.
   Note: MethodCall/FieldAccess pos is the DOT position, not the receiver start. *)
let rec find_in_expr ~source ~offset ~parent (expr : Ast.AST.expression) : Lsp_t.SelectionRange.t option =
  let start_pos =
    match expr.expr with
    | Ast.AST.MethodCall { mc_receiver = recv; _ } | Ast.AST.FieldAccess (recv, _) -> min recv.pos expr.pos
    | _ -> expr.pos
  in
  if offset < start_pos || offset > expr.end_pos then
    None
  else
    let current = Some (sel_range ~source ~pos:expr.pos ~end_pos:expr.end_pos ~parent) in
    let child =
      match expr.expr with
      | Ast.AST.Infix (left, _, right) ->
          first_some (find_in_expr ~source ~offset ~parent:current left) (fun () ->
              find_in_expr ~source ~offset ~parent:current right)
      | Ast.AST.Prefix (_, e) -> find_in_expr ~source ~offset ~parent:current e
      | Ast.AST.Call (fn_expr, args) ->
          first_some (find_in_expr ~source ~offset ~parent:current fn_expr) (fun () ->
              find_first_in_exprs ~source ~offset ~parent:current args)
      | Ast.AST.If (cond, then_, else_) ->
          first_some (find_in_expr ~source ~offset ~parent:current cond) (fun () ->
              first_some (find_in_stmt ~source ~offset ~parent:current then_) (fun () ->
                  match else_ with
                  | Some e -> find_in_stmt ~source ~offset ~parent:current e
                  | None -> None))
      | Ast.AST.Function { body; _ } -> find_in_stmt ~source ~offset ~parent:current body
      | Ast.AST.Index (arr, idx) ->
          first_some (find_in_expr ~source ~offset ~parent:current arr) (fun () ->
              find_in_expr ~source ~offset ~parent:current idx)
      | Ast.AST.Array elts -> find_first_in_exprs ~source ~offset ~parent:current elts
      | Ast.AST.Hash pairs ->
          List.find_map
            (fun (k, v) ->
              first_some (find_in_expr ~source ~offset ~parent:current k) (fun () ->
                  find_in_expr ~source ~offset ~parent:current v))
            pairs
      | Ast.AST.FieldAccess (e, _) -> find_in_expr ~source ~offset ~parent:current e
      | Ast.AST.MethodCall { mc_receiver; mc_args; _ } ->
          first_some (find_in_expr ~source ~offset ~parent:current mc_receiver) (fun () ->
              find_first_in_exprs ~source ~offset ~parent:current mc_args)
      | Ast.AST.Match (scrutinee, arms) ->
          first_some (find_in_expr ~source ~offset ~parent:current scrutinee) (fun () ->
              List.find_map
                (fun (arm : Ast.AST.match_arm) ->
                  first_some (find_first_in_patterns ~source ~offset ~parent:current arm.patterns) (fun () ->
                      find_in_expr ~source ~offset ~parent:current arm.body))
                arms)
      | Ast.AST.RecordLit (fields, spread) ->
          first_some
            (List.find_map
               (fun (f : Ast.AST.record_field) ->
                 Option.bind f.field_value (find_in_expr ~source ~offset ~parent:current))
               fields)
            (fun () -> Option.bind spread (find_in_expr ~source ~offset ~parent:current))
      | Ast.AST.EnumConstructor (_, _, args) -> find_first_in_exprs ~source ~offset ~parent:current args
      | Ast.AST.TypeCheck (e, _) -> find_in_expr ~source ~offset ~parent:current e
      | Ast.AST.BlockExpr stmts -> find_first_in_stmts ~source ~offset ~parent:current stmts
      | Ast.AST.Identifier _ | Ast.AST.Integer _ | Ast.AST.Float _ | Ast.AST.Boolean _ | Ast.AST.String _ -> None
    in
    match child with
    | Some _ -> child
    | None -> current

and find_in_stmt ~source ~offset ~parent (stmt : Ast.AST.statement) : Lsp_t.SelectionRange.t option =
  if offset < stmt.pos || offset > stmt.end_pos then
    None
  else
    let current = Some (sel_range ~source ~pos:stmt.pos ~end_pos:stmt.end_pos ~parent) in
    let child =
      match stmt.stmt with
      | Ast.AST.Let { value; _ } -> find_in_expr ~source ~offset ~parent:current value
      | Ast.AST.ExpressionStmt e -> find_in_expr ~source ~offset ~parent:current e
      | Ast.AST.Return e -> find_in_expr ~source ~offset ~parent:current e
      | Ast.AST.Block stmts -> find_first_in_stmts ~source ~offset ~parent:current stmts
      | Ast.AST.ImplDef { impl_methods; _ } ->
          List.find_map
            (fun (m : Ast.AST.method_impl) -> find_in_stmt ~source ~offset ~parent:current m.impl_method_body)
            impl_methods
      | Ast.AST.TraitDef { methods; _ } ->
          List.find_map
            (fun (m : Ast.AST.method_sig) ->
              Option.bind m.method_default_impl (find_in_expr ~source ~offset ~parent:current))
            methods
      | Ast.AST.InherentImplDef { inherent_methods; _ } ->
          List.find_map
            (fun (m : Ast.AST.method_impl) -> find_in_stmt ~source ~offset ~parent:current m.impl_method_body)
            inherent_methods
      | Ast.AST.EnumDef _ | Ast.AST.TypeDef _ | Ast.AST.ShapeDef _ | Ast.AST.DeriveDef _ | Ast.AST.TypeAlias _ ->
          None
    in
    match child with
    | Some _ -> child
    | None -> current

and find_in_pattern ~source ~offset ~parent (pat : Ast.AST.pattern) : Lsp_t.SelectionRange.t option =
  if offset < pat.pos || offset > pat.end_pos then
    None
  else
    let current = Some (sel_range ~source ~pos:pat.pos ~end_pos:pat.end_pos ~parent) in
    let child =
      match pat.pat with
      | Ast.AST.PConstructor (_, _, sub_pats) -> find_first_in_patterns ~source ~offset ~parent:current sub_pats
      | Ast.AST.PRecord (fields, _) ->
          List.find_map
            (fun (f : Ast.AST.record_pattern_field) ->
              Option.bind f.pat_field_pattern (find_in_pattern ~source ~offset ~parent:current))
            fields
      | Ast.AST.PWildcard | Ast.AST.PVariable _ | Ast.AST.PLiteral _ -> None
    in
    match child with
    | Some _ -> child
    | None -> current

and find_first_in_patterns ~source ~offset ~parent pats =
  List.find_map (find_in_pattern ~source ~offset ~parent) pats

and find_first_in_exprs ~source ~offset ~parent exprs = List.find_map (find_in_expr ~source ~offset ~parent) exprs
and find_first_in_stmts ~source ~offset ~parent stmts = List.find_map (find_in_stmt ~source ~offset ~parent) stmts

(* Lazy first_some: only evaluates second thunk if first returns None *)
and first_some a b_thunk =
  match a with
  | Some _ -> a
  | None -> b_thunk ()

(* Find selection range for a single position across the whole program *)
let find_at_position ~source ~program ~offset : Lsp_t.SelectionRange.t =
  (* The outermost parent covers the entire source *)
  let total_len = String.length source in
  let program_parent =
    if total_len > 0 then
      Some (sel_range ~source ~pos:0 ~end_pos:(total_len - 1) ~parent:None)
    else
      None
  in
  let result = List.find_map (find_in_stmt ~source ~offset ~parent:program_parent) program in
  match result with
  | Some sr -> sr
  | None -> (
      (* Fallback: return the program-level range *)
      match program_parent with
      | Some sr -> sr
      | None ->
          let range =
            Lsp_t.Range.create
              ~start:(Lsp_t.Position.create ~line:0 ~character:0)
              ~end_:(Lsp_t.Position.create ~line:0 ~character:0)
          in
          Lsp_t.SelectionRange.create ~range ())

(* Public entry point *)
let compute ~(source : string) ~(program : Ast.AST.program) ~(positions : Lsp_t.Position.t list) :
    Lsp_t.SelectionRange.t list =
  List.map
    (fun (pos : Lsp_t.Position.t) ->
      let offset = Lsp_utils.position_to_offset ~source ~line:pos.line ~character:pos.character in
      find_at_position ~source ~program ~offset)
    positions

(* ============================================================
   Tests
   ============================================================ *)

(* Helper: parse and compute selection ranges *)
let get_selection source positions =
  match Marmoset.Lib.Parser.parse ~file_id:"<test>" source with
  | Error _ -> []
  | Ok program ->
      let lsp_positions = List.map (fun (line, character) -> Lsp_t.Position.create ~line ~character) positions in
      compute ~source ~program ~positions:lsp_positions

(* Count how many nested parents a SelectionRange has *)
let rec chain_depth (sr : Lsp_t.SelectionRange.t) =
  match sr.parent with
  | None -> 1
  | Some p -> 1 + chain_depth p

let%test "cursor on identifier has nested chain" =
  let ranges = get_selection "let x = 42;" [ (0, 4) ] in
  match ranges with
  | [ sr ] ->
      (* Should have at least: identifier → let stmt → program *)
      chain_depth sr >= 2
  | _ -> false

let%test "cursor on nested expression has deeper chain" =
  let ranges = get_selection "let x = 1 + 2;" [ (0, 10) ] in
  match ranges with
  | [ sr ] ->
      (* Should have: number → infix → let stmt → program *)
      chain_depth sr >= 3
  | _ -> false

let%test "multiple positions produce multiple results" =
  let ranges = get_selection "let x = 1; let y = 2;" [ (0, 4); (0, 15) ] in
  List.length ranges = 2

let%test "cursor outside any expression returns program-level range" =
  let ranges = get_selection "let x = 42;" [ (0, 11) ] in
  match ranges with
  | [ sr ] -> chain_depth sr >= 1
  | _ -> false

let%test "cursor in function body has deep chain" =
  let ranges = get_selection "let f = (x) -> { x + 1 }" [ (0, 17) ] in
  match ranges with
  | [ sr ] ->
      (* x → x+1 → body → fn → let → program — deep chain *)
      chain_depth sr >= 4
  | _ -> false

let%test "innermost range is the narrowest" =
  let ranges = get_selection "let x = 1 + 2;" [ (0, 8) ] in
  match ranges with
  | [ sr ] -> (
      (* The innermost range should be smaller than its parent *)
      match sr.parent with
      | Some parent ->
          let inner_size = sr.range.end_.character - sr.range.start.character in
          let outer_size = parent.range.end_.character - parent.range.start.character in
          inner_size <= outer_size
      | None -> true)
  | _ -> false
