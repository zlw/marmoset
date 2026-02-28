(* Hover: find expression at cursor and show its type *)

module Lsp_t = Linol_lsp.Types
module Ast = Marmoset.Lib.Ast
module Infer = Marmoset.Lib.Infer
module Types = Marmoset.Lib.Types

let first_some a b =
  match a with
  | Some _ -> a
  | None -> b

(* Find the deepest expression in the AST that contains the given byte offset.
   Note: MethodCall/FieldAccess pos is the DOT position, not the receiver start,
   so we expand the containment check for those to include the receiver range. *)
let rec find_expr_at (offset : int) (expr : Ast.AST.expression) : Ast.AST.expression option =
  let start_pos =
    match expr.expr with
    | Ast.AST.MethodCall (recv, _, _) | Ast.AST.FieldAccess (recv, _) -> min recv.pos expr.pos
    | _ -> expr.pos
  in
  if offset < start_pos || offset > expr.end_pos then
    None
  else
    let child =
      match expr.expr with
      | Ast.AST.Infix (left, _, right) -> first_some (find_expr_at offset left) (find_expr_at offset right)
      | Ast.AST.Prefix (_, e) -> find_expr_at offset e
      | Ast.AST.Call (fn_expr, args) ->
          first_some (find_expr_at offset fn_expr) (List.find_map (find_expr_at offset) args)
      | Ast.AST.If (cond, then_, else_) ->
          first_some (find_expr_at offset cond)
            (first_some (find_expr_in_stmt offset then_) (Option.bind else_ (find_expr_in_stmt offset)))
      | Ast.AST.Function { body; _ } -> find_expr_in_stmt offset body
      | Ast.AST.Index (arr, idx) -> first_some (find_expr_at offset arr) (find_expr_at offset idx)
      | Ast.AST.Array elts -> List.find_map (find_expr_at offset) elts
      | Ast.AST.Hash pairs ->
          List.find_map (fun (k, v) -> first_some (find_expr_at offset k) (find_expr_at offset v)) pairs
      | Ast.AST.FieldAccess (e, _) -> find_expr_at offset e
      | Ast.AST.MethodCall (recv, _, args) ->
          first_some (find_expr_at offset recv) (List.find_map (find_expr_at offset) args)
      | Ast.AST.Match (scrutinee, arms) ->
          first_some (find_expr_at offset scrutinee)
            (List.find_map (fun (arm : Ast.AST.match_arm) -> find_expr_at offset arm.body) arms)
      | Ast.AST.RecordLit (fields, spread) ->
          first_some
            (List.find_map
               (fun (f : Ast.AST.record_field) -> Option.bind f.field_value (find_expr_at offset))
               fields)
            (Option.bind spread (find_expr_at offset))
      | Ast.AST.EnumConstructor (_, _, args) -> List.find_map (find_expr_at offset) args
      | Ast.AST.TypeCheck (e, _) -> find_expr_at offset e
      | Ast.AST.Identifier _ | Ast.AST.Integer _ | Ast.AST.Float _ | Ast.AST.Boolean _ | Ast.AST.String _ -> None
    in
    match child with
    | Some _ -> child
    | None -> Some expr

and find_expr_in_stmt (offset : int) (stmt : Ast.AST.statement) : Ast.AST.expression option =
  match stmt.stmt with
  | Ast.AST.ExpressionStmt e -> find_expr_at offset e
  | Ast.AST.Let { value; _ } -> find_expr_at offset value
  | Ast.AST.Return e -> find_expr_at offset e
  | Ast.AST.Block stmts -> List.find_map (find_expr_in_stmt offset) stmts
  | Ast.AST.ImplDef { impl_methods; _ } ->
      List.find_map (fun (m : Ast.AST.method_impl) -> find_expr_at offset m.impl_method_body) impl_methods
  | Ast.AST.TraitDef { methods; _ } ->
      List.find_map
        (fun (m : Ast.AST.method_sig) ->
          Option.bind m.method_default_impl (find_expr_at offset))
        methods
  | Ast.AST.EnumDef _ | Ast.AST.DeriveDef _ | Ast.AST.TypeAlias _ -> None

(* Find an expression at a given offset across the entire program *)
let find_in_program (offset : int) (program : Ast.AST.program) : Ast.AST.expression option =
  List.find_map (find_expr_in_stmt offset) program

(* Format a poly_type in Marmoset syntax: name[a, b]: type *)
let format_poly ~(name : string) (Types.Forall (vars, mono)) : string =
  let type_str = Types.to_string_pretty mono in
  match vars with
  | [] -> Printf.sprintf "%s: %s" name type_str
  | _ -> Printf.sprintf "%s[%s]: %s" name (String.concat ", " vars) type_str

(* Format a type for hover display *)
let format_hover_type ~(type_map : Infer.type_map) ~(environment : Infer.type_env) (expr : Ast.AST.expression) :
    string option =
  match expr.expr with
  | Ast.AST.Identifier name -> (
      (* For identifiers, prefer poly_type from env for better display *)
      match Infer.TypeEnv.find_opt name environment with
      | Some poly -> Some (format_poly ~name poly)
      | None -> (
          match Hashtbl.find_opt type_map expr.id with
          | Some mono -> Some (Printf.sprintf "%s: %s" name (Types.to_string_pretty mono))
          | None -> None))
  | _ -> (
      match Hashtbl.find_opt type_map expr.id with
      | Some mono -> Some (Types.to_string_pretty mono)
      | None -> None)

(* Provide hover info at a given cursor position *)
let hover_at
    ~(source : string)
    ~(program : Ast.AST.program)
    ~(type_map : Infer.type_map)
    ~(environment : Infer.type_env)
    ~(line : int)
    ~(character : int) : Lsp_t.Hover.t option =
  let offset = Lsp_utils.position_to_offset ~source ~line ~character in
  match find_in_program offset program with
  | None -> None
  | Some expr -> (
      match format_hover_type ~type_map ~environment expr with
      | None -> None
      | Some type_str ->
          let contents =
            Lsp_t.MarkupContent.create ~kind:Lsp_t.MarkupKind.Markdown
              ~value:(Printf.sprintf "```marmoset\n%s\n```" type_str)
          in
          let range = Lsp_utils.offset_range_to_lsp ~source ~pos:expr.pos ~end_pos:expr.end_pos in
          Some (Lsp_t.Hover.create ~contents:(`MarkupContent contents) ~range ()))

(* ============================================================
   Tests
   ============================================================ *)

let string_contains haystack needle =
  let len_h = String.length haystack in
  let len_n = String.length needle in
  if len_n > len_h then
    false
  else
    let rec check i =
      if i + len_n > len_h then
        false
      else if String.sub haystack i len_n = needle then
        true
      else
        check (i + 1)
    in
    check 0

(* Helper: parse and typecheck source, then test hover *)
let check_hover source line character =
  let result = Doc_state.analyze ~source in
  match (result.program, result.type_map, result.environment) with
  | Some prog, Some tm, Some env -> hover_at ~source ~program:prog ~type_map:tm ~environment:env ~line ~character
  | _ -> None

let%test "hover on integer literal shows Int" =
  match check_hover "42" 0 0 with
  | Some hover -> (
      match hover.contents with
      | `MarkupContent mc ->
          let lower = String.lowercase_ascii mc.value in
          string_contains lower "int"
      | _ -> false)
  | None -> false

let%test "hover on identifier shows name and type" =
  match check_hover "let x = 42; x" 0 12 with
  | Some hover -> (
      match hover.contents with
      | `MarkupContent mc -> string_contains mc.value "x" && string_contains mc.value "Int"
      | _ -> false)
  | None -> false

let%test "hover on whitespace/out of range returns None" =
  let result = check_hover "42" 5 0 in
  result = None

let%test "hover on function shows function type" =
  match check_hover "let f = fn(x) { x + 1 }; f" 0 25 with
  | Some hover -> (
      match hover.contents with
      | `MarkupContent mc -> string_contains mc.value "->"
      | _ -> false)
  | None -> false

let%test "find_expr_at returns None for offset outside expression" =
  let expr = Ast.AST.mk_expr ~id:1 ~pos:5 ~end_pos:10 (Ast.AST.Integer 42L) in
  find_expr_at 0 expr = None && find_expr_at 15 expr = None

let%test "find_expr_at returns expression when offset is inside" =
  let expr = Ast.AST.mk_expr ~id:1 ~pos:0 ~end_pos:5 (Ast.AST.Identifier "hello") in
  match find_expr_at 3 expr with
  | Some e -> e.id = 1
  | None -> false

let%test "hover on polymorphic function uses bracket syntax" =
  (* "let id = fn(x) { x }; id" — id should show as "id[a]: a -> a" *)
  match check_hover "let id = fn(x) { x }; id" 0 23 with
  | Some hover -> (
      match hover.contents with
      | `MarkupContent mc -> string_contains mc.value "id[" && string_contains mc.value "]: "
      | _ -> false)
  | None -> false
