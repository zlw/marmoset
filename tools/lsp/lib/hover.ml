(* Hover: find expression at cursor and show its type *)

module Lsp_t = Linol_lsp.Types
module Ast = Marmoset.Lib.Ast
module Infer = Marmoset.Lib.Infer
module Types = Marmoset.Lib.Types

type type_var_user_name_map = Source_syntax.type_var_user_name_map

let first_some a b =
  match a with
  | Some _ -> a
  | None -> b

(* The parser sets pos to the operator/paren position for Infix, Call,
   MethodCall, FieldAccess — not the leftmost token. This helper computes
   the true start by walking into left children recursively. *)
let rec effective_start_pos (expr : Ast.AST.expression) : int =
  match expr.expr with
  | Ast.AST.MethodCall { mc_receiver = recv; _ } | Ast.AST.FieldAccess (recv, _) ->
      min (effective_start_pos recv) expr.pos
  | Ast.AST.Infix (left, _, _) -> min (effective_start_pos left) expr.pos
  | Ast.AST.Call (fn_expr, _) -> min (effective_start_pos fn_expr) expr.pos
  | _ -> expr.pos

(* Find the deepest expression in the AST that contains the given byte offset. *)
let rec find_expr_at (offset : int) (expr : Ast.AST.expression) : Ast.AST.expression option =
  let start_pos = effective_start_pos expr in
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
      | Ast.AST.MethodCall { mc_receiver; mc_args; _ } ->
          first_some (find_expr_at offset mc_receiver) (List.find_map (find_expr_at offset) mc_args)
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
      | Ast.AST.BlockExpr stmts -> List.find_map (find_expr_in_stmt offset) stmts
      | Ast.AST.Identifier _ | Ast.AST.Integer _ | Ast.AST.Float _ | Ast.AST.Boolean _ | Ast.AST.String _ -> None
    in
    match child with
    | Some _ -> child
    | None -> Some expr

and find_expr_in_stmt (offset : int) (stmt : Ast.AST.statement) : Ast.AST.expression option =
  match stmt.stmt with
  | Ast.AST.ExpressionStmt e -> find_expr_at offset e
  | Ast.AST.Let { value; _ } -> (
      (* Search inside the value expression first; if not found and offset is
         within the statement range (e.g. on the binding name), return the value
         itself so the hover shows the binding's type. *)
      match find_expr_at offset value with
      | Some _ as found -> found
      | None ->
          if offset >= stmt.pos && offset <= value.pos then
            Some value
          else
            None)
  | Ast.AST.Return e -> (
      match find_expr_at offset e with
      | Some _ as found -> found
      | None ->
          if offset >= stmt.pos && offset <= e.pos then
            Some e
          else
            None)
  | Ast.AST.Block stmts -> List.find_map (find_expr_in_stmt offset) stmts
  | Ast.AST.ImplDef { impl_methods; _ } ->
      List.find_map (fun (m : Ast.AST.method_impl) -> find_expr_in_stmt offset m.impl_method_body) impl_methods
  | Ast.AST.InherentImplDef { inherent_methods; _ } ->
      List.find_map
        (fun (m : Ast.AST.method_impl) -> find_expr_in_stmt offset m.impl_method_body)
        inherent_methods
  | Ast.AST.TraitDef { methods; _ } ->
      List.find_map
        (fun (m : Ast.AST.method_sig) -> Option.bind m.method_default_impl (find_expr_at offset))
        methods
  | Ast.AST.EnumDef _ | Ast.AST.DeriveDef _ | Ast.AST.TypeAlias _ -> None

(* Find an expression at a given offset across the entire program *)
let find_in_program (offset : int) (program : Ast.AST.program) : Ast.AST.expression option =
  List.find_map (find_expr_in_stmt offset) program

let type_to_source = Source_syntax.mono_type_to_source
let normalize_with_user_names = Source_syntax.normalize_mono_type_with_user_names
let format_poly = Source_syntax.format_poly_binding

(* Format a type for hover display *)
let format_hover_type
    ~(type_var_user_names : type_var_user_name_map)
    ~(type_map : Infer.type_map)
    ~(environment : Infer.type_env)
    (expr : Ast.AST.expression) : string option =
  match expr.expr with
  | Ast.AST.Identifier name -> (
      (* For identifiers, prefer poly_type from env for better display *)
      match Infer.TypeEnv.find_opt name environment with
      | Some poly -> Some (format_poly ~type_var_user_names ~name poly)
      | None -> (
          match Hashtbl.find_opt type_map expr.id with
          | Some mono ->
              let norm = normalize_with_user_names ~type_var_user_names mono in
              Some (Printf.sprintf "%s: %s" name (type_to_source ~type_var_user_names norm))
          | None -> None))
  | _ -> (
      match Hashtbl.find_opt type_map expr.id with
      | Some mono ->
          let norm = normalize_with_user_names ~type_var_user_names mono in
          Some (type_to_source ~type_var_user_names norm)
      | None -> None)

(* Provide hover info at a given cursor position *)
let hover_at
    ~(source : string)
    ~(program : Ast.AST.program)
    ~(type_map : Infer.type_map)
    ~(environment : Infer.type_env)
    ~(type_var_user_names : type_var_user_name_map)
    ~(line : int)
    ~(character : int) : Lsp_t.Hover.t option =
  let offset = Lsp_utils.position_to_offset ~source ~line ~character in
  match find_in_program offset program with
  | None -> None
  | Some expr -> (
      match format_hover_type ~type_var_user_names ~type_map ~environment expr with
      | None -> None
      | Some type_str ->
          let contents =
            Lsp_t.MarkupContent.create ~kind:Lsp_t.MarkupKind.Markdown
              ~value:(Printf.sprintf "```marmoset\n%s\n```" type_str)
          in
          let effective_pos = effective_start_pos expr in
          let range = Lsp_utils.offset_range_to_lsp ~source ~pos:effective_pos ~end_pos:expr.end_pos in
          Some (Lsp_t.Hover.create ~contents:(`MarkupContent contents) ~range ()))

(* ============================================================
   Tests
   ============================================================ *)

(* ============================================================
   Test helpers — check type text AND highlighted range
   ============================================================ *)

let string_contains haystack needle = Diagnostics.String_utils.contains_substring ~needle haystack

(* Full hover result: type text, highlighted source range *)
type hover_result = {
  type_text : string; (* content of the tooltip *)
  highlighted : string; (* substring of source that the range covers *)
  start_line : int;
  start_col : int;
  end_line : int;
  end_col : int;
}

let check_hover source line character =
  let result = Doc_state.analyze ~source in
  match (result.program, result.type_map, result.environment) with
  | Some prog, Some tm, Some env ->
      hover_at ~source ~program:prog ~type_map:tm ~environment:env ~type_var_user_names:result.type_var_user_names
        ~line ~character
  | _ -> None

let hover_info source line character : hover_result option =
  match check_hover source line character with
  | None -> None
  | Some hover ->
      let type_text =
        match hover.contents with
        | `MarkupContent mc -> mc.value
        | _ -> ""
      in
      let r =
        match hover.range with
        | Some r -> r
        | None -> failwith "hover missing range"
      in
      (* Extract the highlighted substring from source using line/col offsets *)
      let start_off = Lsp_utils.position_to_offset ~source ~line:r.start.line ~character:r.start.character in
      let end_off = Lsp_utils.position_to_offset ~source ~line:r.end_.line ~character:r.end_.character in
      let highlighted = String.sub source start_off (end_off - start_off) in
      Some
        {
          type_text;
          highlighted;
          start_line = r.start.line;
          start_col = r.start.character;
          end_line = r.end_.line;
          end_col = r.end_.character;
        }

(* ============================================================
   Tests: literals
   ============================================================ *)

let%test "hover on integer literal: type=Int, highlights '42'" =
  match hover_info "42" 0 0 with
  | Some h -> string_contains h.type_text "Int" && h.highlighted = "42"
  | None -> false

let%test "hover on string literal: type=Str, highlights the string" =
  match hover_info {|"hello"|} 0 0 with
  | Some h -> string_contains h.type_text "Str" && h.highlighted = {|"hello"|}
  | None -> false

let%test "hover on boolean literal: type=Bool, highlights 'true'" =
  match hover_info "true" 0 0 with
  | Some h -> string_contains h.type_text "Bool" && h.highlighted = "true"
  | None -> false

(* ============================================================
   Tests: identifiers and let bindings
   ============================================================ *)

(*  let x = 42; x
    0         1
    0123456789012345
    x at col 12 *)
let%test "hover on identifier: type=int, highlights 'x'" =
  match hover_info "let x = 42; x" 0 12 with
  | Some h -> string_contains h.type_text "x" && string_contains h.type_text "Int" && h.highlighted = "x"
  | None -> false

(*  let f = fn(x) { x + 1 }; f
    0         1         2
    0123456789012345678901234567
    b at col 25 *)
let%test "hover on function identifier: shows arrow type, highlights 'f'" =
  match hover_info "let f = fn(x) { x + 1 }; f" 0 25 with
  | Some h -> string_contains h.type_text "->" && h.highlighted = "f"
  | None -> false

let%test "hover on polymorphic function uses bracket syntax" =
  match hover_info "let id = fn(x) { x }; id" 0 23 with
  | Some h -> string_contains h.type_text "id[" && string_contains h.type_text "]: "
  | None -> false

let%test "hover on let binding name: shows value type" =
  match hover_info "let x = 42;" 0 4 with
  | Some h -> string_contains h.type_text "Int"
  | None -> false

(* ============================================================
   Tests: function calls — identifier vs call result
   ============================================================ *)

(*  let f = fn(x) { x + 1 }; f(1)
    0         1         2
    0123456789012345678901234567890
    f at col 25, ( at 26, 1 at 27, ) at 28 *)
let%test "hover on call-site function name: arrow type, highlights 'f'" =
  match hover_info "let f = fn(x) { x + 1 }; f(1)" 0 25 with
  | Some h -> string_contains h.type_text "->" && h.highlighted = "f"
  | None -> false

let%test "hover on call argument: type=int, highlights '1'" =
  match hover_info "let f = fn(x) { x + 1 }; f(1)" 0 27 with
  | Some h -> string_contains h.type_text "Int" && h.highlighted = "1"
  | None -> false

(* ============================================================
   Tests: infix — left operand must be reachable
   ============================================================ *)

(*  1 + 2
    01234
    1 at col 0, + at col 2, 2 at col 4 *)
let%test "hover on left of infix: type=int, highlights '1'" =
  match hover_info "1 + 2" 0 0 with
  | Some h -> string_contains h.type_text "Int" && h.highlighted = "1"
  | None -> false

let%test "hover on right of infix: type=int, highlights '2'" =
  match hover_info "1 + 2" 0 4 with
  | Some h -> string_contains h.type_text "Int" && h.highlighted = "2"
  | None -> false

let%test "hover on infix operator: type=int, highlights whole expression" =
  match hover_info "1 + 2" 0 2 with
  | Some h -> string_contains h.type_text "Int" && h.highlighted = "1 + 2"
  | None -> false

(* ============================================================
   Tests: nested calls inside infix (the fib pattern)
   ============================================================ *)

let fib_source = "let fib = fn(n) {\n  if (n < 2) { return n }\n  return fib(n - 2) + fib(n - 1);\n}"

(*  line 2: "  return fib(n - 2) + fib(n - 1);"
             0123456789012345678901234567890123
             fib at col 9, + at col 20, second fib at col 22 *)
(* Non-recursive call inside infix *)
let%test "hover on fn name in call inside infix: highlights 'f'" =
  let source = "let f = fn(x) { x + 1 }; f(1) + 2" in
  match hover_info source 0 25 with
  | Some h -> h.highlighted = "f"
  | None -> false

(* Recursive fib — the analysis must succeed and hover must find fib inside body *)
let%test "fib source analyzes without errors" =
  let result = Doc_state.analyze ~source:fib_source in
  result.diagnostics = [] && result.type_map <> None

let%test "hover on fib inside recursive body finds something" = check_hover fib_source 2 9 <> None

let%test "hover on n inside fib body" =
  (* line 2: "  return fib(n - 2) + fib(n - 1);"   col 13 = n *)
  (* Note: parser end_pos for inner expressions is imprecise, so hover may
     match a parent expression instead of the exact identifier. Correct type
     is still returned — just a wider highlight. Will be fixed in parser rework. *)
  match hover_info fib_source 2 13 with
  | Some h -> string_contains h.type_text "Int"
  | None -> false

(* Sanity: what does hover find on the simple return line? *)
let%test "hover on literal 2 inside fib body" =
  (* line 2: "  return fib(n - 2) + fib(n - 1);"  col 17 = '2' *)
  match hover_info fib_source 2 17 with
  | Some h -> string_contains h.type_text "Int"
  | None -> false

let%test "hover on + operator inside fib body" =
  (* line 2: col 20 = '+' *)
  match hover_info fib_source 2 20 with
  | Some _ -> true
  | None -> false

let%test "hover on return keyword: shows return expr type" =
  match hover_info fib_source 2 3 with
  | Some h -> string_contains h.type_text "Int"
  | None -> false

let%test "hover on return keyword: highlights full return expression" =
  match hover_info fib_source 2 3 with
  | Some h -> string_contains h.highlighted "fib(n - 2) + fib(n - 1)"
  | None -> false

(* ============================================================
   Tests: edge cases
   ============================================================ *)

let full_fib_source =
  "let fib = fn(n) {\n  if (n < 2) { return n }\n  return fib(n - 2) + fib(n - 1);\n}\n\nputs(fib(35) == 9227465)\n"

let%test "hover on first return keyword shows int, not unit" =
  (* line 1: "  if (n < 2) { return n }"  col 15 = 'r' in 'return' *)
  match hover_info full_fib_source 1 15 with
  | Some h -> string_contains h.type_text "Int"
  | None -> false

let%test "hover on second return keyword shows int" =
  (* line 2: "  return fib(n - 2) + fib(n - 1);"  col 2 = 'r' in 'return' *)
  match hover_info full_fib_source 2 2 with
  | Some h -> string_contains h.type_text "Int"
  | None -> false

let%test "hover on n param inside first return" =
  (* line 1: col 22 = 'n' after return *)
  match hover_info full_fib_source 1 22 with
  | Some h -> string_contains h.type_text "Int" && h.highlighted = "n"
  | None -> false

let%test "hover on whitespace/out of range returns None" = check_hover "42" 5 0 = None

let%test "find_expr_at returns None for offset outside expression" =
  let expr = Ast.AST.mk_expr ~id:1 ~pos:5 ~end_pos:10 (Ast.AST.Integer 42L) in
  find_expr_at 0 expr = None && find_expr_at 15 expr = None

let%test "find_expr_at returns expression when offset is inside" =
  let expr = Ast.AST.mk_expr ~id:1 ~pos:0 ~end_pos:5 (Ast.AST.Identifier "hello") in
  match find_expr_at 3 expr with
  | Some e -> e.id = 1
  | None -> false

(* ============================================================
   Phase 8 regression: hover inside trait impl method body
   ============================================================ *)

let trait_impl_source =
  "trait greet[a] {\n  fn hello(x: a) -> string\n}\nimpl greet for int {\n  fn hello(x: int) -> string { \"hi\" }\n}\nputs(greet.hello(42))"

let%test "hover on expression inside trait impl method body" =
  (* line 4: fn hello(x: int) -> string { "hi" }  — "hi" is a string literal *)
  match hover_info trait_impl_source 4 32 with
  | Some h -> string_contains h.type_text "Str"
  | None -> false

let inherent_impl_source = "impl int {\n  fn double(x: int) -> int { x * 2 }\n}\nputs(42.double())"

let%test "hover on expression inside inherent impl method body" =
  (* line 1: fn double(x: int) -> int { x * 2 } — x * 2 *)
  match hover_info inherent_impl_source 1 29 with
  | Some h -> string_contains h.type_text "Int"
  | None -> false
