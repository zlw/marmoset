module AST = Syntax.Ast.AST
module Diagnostic = Diagnostics.Diagnostic

let expand_user_derives (program : AST.program) : (AST.program, Diagnostic.t) result = Ok program

let%test "expand_user_derives preserves statement count in scaffold phase" =
  let program = [ AST.mk_stmt (AST.ExpressionStmt (AST.mk_expr ~id:7 (AST.Integer 42L))) ] in
  match expand_user_derives program with
  | Ok expanded -> List.length expanded = 1
  | Error _ -> false
