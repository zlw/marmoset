open Ast

let ( let* ) res f = Result.bind res f

(* Helpers to create AST nodes with position from current token *)
let mk_expr pos kind = AST.{ expr = kind; pos }
let mk_stmt pos kind = AST.{ stmt = kind; pos }

type parser = {
  lexer : Lexer.lexer;
  curr_token : Token.token;
  peek_token : Token.token;
  errors : errors;
}

and errors = string list

type precedence = int

let prec_lowest = 1
let prec_equals = 2
let prec_less_greater = 3
let prec_sum = 4
let prec_product = 5
let prec_prefix = 6
let prec_call = 7
let prec_index = 8

let precedences = function
  | Token.Eq | Token.NotEq | Token.Is -> prec_equals
  | Token.Lt | Token.Gt -> prec_less_greater
  | Token.Plus | Token.Minus -> prec_sum
  | Token.Asterisk | Token.Slash -> prec_product
  | Token.LParen -> prec_call
  | Token.LBracket -> prec_index
  | _ -> prec_lowest

let peek_precedence (p : parser) : precedence = precedences p.peek_token.token_type
let curr_precedence (p : parser) : precedence = precedences p.curr_token.token_type

let next_token (p : parser) : parser =
  let curr_token = p.peek_token in
  let lexer, peek_token = Lexer.next_token p.lexer in
  { p with lexer; curr_token; peek_token }

let init (l : Lexer.lexer) : parser =
  { lexer = l; curr_token = Token.init Illegal ""; peek_token = Token.init Illegal ""; errors = [] }
  |> next_token
  |> next_token

let curr_token_is (p : parser) (t : Token.token_type) : bool = p.curr_token.token_type = t
let peek_token_is (p : parser) (t : Token.token_type) : bool = p.peek_token.token_type = t
let add_error (p : parser) (msg : string) : parser = { p with errors = [ msg ] @ p.errors }

let skip (p : parser) (t : Token.token_type) : parser =
  if peek_token_is p t then
    next_token p
  else
    p

let peek_error (p : parser) (tt : Token.token_type) : parser =
  let msg =
    Printf.sprintf "expected next token to be %s, got %s instead" (Token.show_token_type tt)
      (Token.show_token_type p.peek_token.token_type)
  in
  add_error p msg

let expect_peek (p : parser) (tt : Token.token_type) : (parser, parser) result =
  if peek_token_is p tt then
    Ok (next_token p)
  else
    Error (peek_error p tt)

let no_prefix_parse_fn_error (p : parser) (t : Token.token_type) : parser =
  let msg = Printf.sprintf "syntax error, unexpected %s found" (Token.show_token_type t) in
  add_error p msg

let rec parse_program (p : parser) : (parser * AST.program, parser) result =
  let rec loop (lp : parser) (prog : AST.program) =
    if curr_token_is lp Token.EOF then
      Ok (lp, List.rev prog)
    else
      let* lp2, prog2 = parse_statement lp in
      loop (next_token lp2) ([ prog2 ] @ prog)
  in

  loop p []

and parse_statement (p : parser) : (parser * AST.statement, parser) result =
  match p.curr_token.token_type with
  | Token.Let -> parse_let_statement p
  | Token.Return -> parse_return_statement p
  | _ -> parse_expression_statement p

(* Phase 2: Type expression parsing *)

(* Parse a single type atom (not union) *)
and parse_type_atom (p : parser) : (parser * AST.type_expr, parser) result =
  if curr_token_is p Token.Ident then
    let ident = p.curr_token.literal in
    let p2 = next_token p in
    (* Check for generic application: list[int], map[string, int], etc. *)
    if curr_token_is p2 Token.LBracket then
      let* p3, type_args = parse_type_expr_list (next_token p2) in
      if curr_token_is p3 Token.RBracket then
        Ok (next_token p3, AST.TApp (ident, type_args))
      else
        Error (peek_error p3 Token.RBracket)
    else
      Ok (p2, AST.TCon ident)
  else if curr_token_is p Token.Function then
    (* Function type: fn(int, string) -> bool *)
    let* p2 = expect_peek p Token.LParen in
    let* p3, param_types = parse_type_expr_list (next_token p2) in
    let* p4 = expect_peek p3 Token.RParen in
    let* p5 = expect_peek p4 Token.Arrow in
    let* p6, return_type = parse_type_expr (next_token p5) in
    Ok (p6, AST.TArrow (param_types, return_type))
  else if curr_token_is p Token.LParen then
    (* Parenthesized type or function type: (int | string) or (int, string) -> bool *)
    let* p2, first = parse_type_expr (next_token p) in
    if curr_token_is p2 Token.Comma then
      (* Multiple params: (int, string) -> bool *)
      let rec collect_params lp params =
        let* lp2, param_type = parse_type_expr (next_token lp) in
        let new_params = params @ [ param_type ] in
        if curr_token_is lp2 Token.Comma then
          collect_params lp2 new_params
        else if curr_token_is lp2 Token.RParen then
          Ok (next_token lp2, new_params)
        else
          Error (peek_error lp2 Token.RParen)
      in
      let* p3, params = collect_params p2 [ first ] in
      let* p4 = expect_peek p3 Token.Arrow in
      let* p5, return_type = parse_type_expr (next_token p4) in
      Ok (p5, AST.TArrow (params, return_type))
    else if curr_token_is p2 Token.RParen then
      (* Single type in parens: (int) or (int | string) *)
      let p3 = next_token p2 in
      if curr_token_is p3 Token.Arrow then
        (* Single-param function: (int) -> bool *)
        let* p4 = expect_peek p3 Token.Arrow in
        let* p5, return_type = parse_type_expr (next_token p4) in
        Ok (p5, AST.TArrow ([ first ], return_type))
      else
        (* Just grouping: (int) or (int | string) *)
        Ok (p3, first)
    else
      Error (peek_error p2 Token.RParen)
  else
    Error (no_prefix_parse_fn_error p p.curr_token.token_type)

(* Parse a type expression, including unions: int | string | bool *)
and parse_type_expr (p : parser) : (parser * AST.type_expr, parser) result =
  let* p2, first_type = parse_type_atom p in
  (* Check for union: type | type | type ... *)
  if curr_token_is p2 Token.Pipe then
    let rec collect_union_members lp members =
      let* lp2, next_type = parse_type_atom (next_token lp) in
      let new_members = members @ [ next_type ] in
      if curr_token_is lp2 Token.Pipe then
        collect_union_members lp2 new_members
      else
        Ok (lp2, AST.TUnion new_members)
    in
    collect_union_members p2 [ first_type ]
  else
    Ok (p2, first_type)

and parse_type_expr_list (p : parser) : (parser * AST.type_expr list, parser) result =
  if curr_token_is p Token.RParen || curr_token_is p Token.RBracket then
    Ok (p, [])
  else
    let rec loop (lp : parser) (types : AST.type_expr list) =
      let* lp2, te = parse_type_expr lp in
      if curr_token_is lp2 Token.Comma then
        loop (next_token lp2) ([ te ] @ types)
      else
        Ok (lp2, List.rev ([ te ] @ types))
    in
    loop p []

and parse_trait_constraint (p : parser) : (parser * string list, parser) result =
  (* Parse trait constraints: show, eq, show + eq, etc. *)
  let rec loop (lp : parser) (traits : string list) =
    if curr_token_is lp Token.Ident then
      let trait = lp.curr_token.literal in
      let lp2 = next_token lp in
      if curr_token_is lp2 Token.Plus then
        loop (next_token lp2) ([ trait ] @ traits)
      else
        Ok (lp2, List.rev ([ trait ] @ traits))
    else
      Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
  in
  loop p []

and parse_generic_params (p : parser) : (parser * AST.generic_param list option, parser) result =
  (* Parse generic parameters: [a], [a: show], [a: show + eq, b: eq], etc. *)
  (* Check if the NEXT token is [ (generics start after fn keyword) *)
  if peek_token_is p Token.LBracket then
    let p_bracket = next_token p in
    (* Move to [ *)
    let rec loop (lp : parser) (params : AST.generic_param list) =
      if not (curr_token_is lp Token.Ident) then
        Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
      else
        let name = lp.curr_token.literal in
        let lp3 = next_token lp in
        let* lp4, constraints =
          if curr_token_is lp3 Token.Colon then
            parse_trait_constraint (next_token lp3)
          else
            Ok (lp3, [])
        in
        let param = AST.{ name; constraints } in
        if curr_token_is lp4 Token.Comma then
          loop (next_token lp4) ([ param ] @ params)
        else if curr_token_is lp4 Token.RBracket then
          Ok (lp4, List.rev ([ param ] @ params))
        else
          Error (peek_error lp4 Token.RBracket)
    in
    let* p2, params = loop (next_token p_bracket) [] in
    Ok (p2, Some params)
  else
    Ok (p, None)

and parse_let_statement (p : parser) : (parser * AST.statement, parser) result =
  let pos = p.curr_token.pos in
  let* p2 = expect_peek p Token.Ident in
  let name = p2.curr_token.literal in
  (* Phase 2: Parse optional type annotation *)
  let* p3, type_annotation =
    if peek_token_is p2 Token.Colon then
      let p_colon = next_token p2 in
      (* Move to : *)
      let p_type_start = next_token p_colon in
      (* Move to type identifier *)
      let* p_type_end, te = parse_type_expr p_type_start in
      (* parse_type_expr returns parser positioned one past the type expression *)
      (* For `int`, it's positioned at = *)
      Ok (p_type_end, Some te)
    else
      (* No colon, so p2 is still at the identifier *)
      (* We want to leave p2 as-is so that expect_peek can find = *)
      Ok (p2, None)
  in
  (* p3 is positioned appropriately for the next step *)
  (* For annotated: p3 is at the token after the type (should be =) *)
  (* For non-annotated: p3 is at the identifier, so expect_peek will find = *)
  let* p4 =
    match type_annotation with
    | Some _ ->
        (* After type parsing, we're positioned at/past the type *)
        if curr_token_is p3 Token.Assign then
          Ok p3 (* Already at = *)
        else if peek_token_is p3 Token.Assign then
          Ok (next_token p3)
          (* Move to = *)
        else
          Error (peek_error p3 Token.Assign)
    | None ->
        (* No annotation, so expect_peek to find = *)
        expect_peek p3 Token.Assign
  in
  let* p5, expr = parse_expression (next_token p4) prec_lowest in
  let p6 = skip p5 Token.Semicolon in
  Ok (p6, mk_stmt pos (AST.Let { name; value = expr; type_annotation }))

and parse_return_statement (p : parser) : (parser * AST.statement, parser) result =
  let pos = p.curr_token.pos in
  let* p2, expr = parse_expression (next_token p) prec_lowest in
  let p3 = skip p2 Token.Semicolon in
  Ok (p3, mk_stmt pos (AST.Return expr))

and parse_expression_statement (p : parser) : (parser * AST.statement, parser) result =
  let pos = p.curr_token.pos in
  let* p2, expr = parse_expression p prec_lowest in
  let p3 = skip p2 Token.Semicolon in
  Ok (p3, mk_stmt pos (AST.ExpressionStmt expr))

and parse_expression (p : parser) (prec : precedence) : (parser * AST.expression, parser) result =
  let* p2, left_expr = prefixFn p in
  let* p3, left = infixFn p2 left_expr prec in
  Ok (p3, left)

and prefixFn (p : parser) : (parser * AST.expression, parser) result =
  let tt = p.curr_token.token_type in
  match tt with
  | Token.Ident -> parse_identifier p
  | Token.Int -> parse_integer_literal p
  | Token.Float -> parse_float_literal p
  | Token.String -> parse_string_literal p
  | Token.Bang | Token.Minus -> parse_prefix_expression p
  | Token.True | Token.False -> parse_boolean p
  | Token.LParen -> parse_grouped_expression p
  | Token.If -> parse_if_expression p
  | Token.Function -> parse_function_literal p
  | Token.LBracket -> parse_array_literal p
  | Token.LBrace -> parse_hash_literal p
  | _ -> Error (no_prefix_parse_fn_error p tt)

and infixFn (p : parser) (left_expr : AST.expression) (prec : precedence) :
    (parser * AST.expression, parser) result =
  let rec loop (lp : parser) (left : AST.expression) : (parser * AST.expression, parser) result =
    let peek_is_semicolon = peek_token_is lp Token.Semicolon in
    let lower_precedence = prec < peek_precedence lp in
    if (not peek_is_semicolon) && lower_precedence then
      let* lp2, left2 =
        match lp.peek_token.token_type with
        | Token.Plus | Token.Minus | Token.Slash | Token.Asterisk | Token.Eq | Token.NotEq | Token.Lt | Token.Gt
        | Token.Is ->
            parse_infix_expression (next_token lp) left
        | LParen -> parse_call_expression (next_token lp) left
        | LBracket -> parse_index_expression (next_token lp) left
        | _ -> Ok (lp, left)
      in
      loop lp2 left2
    else
      Ok (lp, left)
  in
  loop p left_expr

and parse_identifier (p : parser) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  Ok (p, mk_expr pos (AST.Identifier p.curr_token.literal))

and parse_integer_literal (p : parser) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  match Int64.of_string_opt p.curr_token.literal with
  | Some int -> Ok (p, mk_expr pos (AST.Integer int))
  | None ->
      let msg = Printf.sprintf "can't parse number from %s" p.curr_token.literal in
      Error (add_error p msg)

and parse_float_literal (p : parser) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  match float_of_string_opt p.curr_token.literal with
  | Some float -> Ok (p, mk_expr pos (AST.Float float))
  | None ->
      let msg = Printf.sprintf "can't parse number from %s" p.curr_token.literal in
      Error (add_error p msg)

and parse_string_literal (p : parser) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  Ok (p, mk_expr pos (AST.String p.curr_token.literal))

and parse_prefix_expression (p : parser) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  let op = p.curr_token.literal in
  let p2 = next_token p in
  let* p3, right = parse_expression p2 prec_prefix in
  Ok (p3, mk_expr pos (AST.Prefix (op, right)))

and parse_infix_expression (p : parser) (left : AST.expression) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  match p.curr_token.token_type with
  | Token.Is ->
      (* x is int - for now, only support simple type identifiers *)
      (* Advance to the type name *)
      let p2 = next_token p in
      if not (curr_token_is p2 Token.Ident) then
        Error (add_error p2 "Expected type name after 'is'")
      else
        let type_name = p2.curr_token.literal in
        let type_expr = AST.TCon type_name in
        (* Don't advance further - stay at the type identifier *)
        (* This matches the convention of other infix operators *)
        Ok (p2, mk_expr pos (AST.TypeCheck (left, type_expr)))
  | _ ->
      let op = p.curr_token.literal in
      let prec = curr_precedence p in
      let p2 = next_token p in
      let* p3, right = parse_expression p2 prec in
      Ok (p3, mk_expr pos (AST.Infix (left, op, right)))

and parse_boolean (p : parser) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  Ok (p, mk_expr pos (AST.Boolean (p.curr_token.token_type = Token.True)))

and parse_grouped_expression (p : parser) : (parser * AST.expression, parser) result =
  let* p2, expr = parse_expression (next_token p) prec_lowest in
  let* p3 = expect_peek p2 Token.RParen in
  Ok (p3, expr)

and parse_if_expression (p : parser) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  let* p2 = expect_peek p Token.LParen in
  let* p3, cond = parse_expression (next_token p2) prec_lowest in
  let* p4 = expect_peek p3 Token.RParen in

  (* After ), check for { or return (with early return support) *)
  let* p5, cons =
    if peek_token_is p4 Token.LBrace then
      (* Normal: { ... } *)
      let* p5 = expect_peek p4 Token.LBrace in
      let* p6, cons = parse_block_statement p5 in
      Ok (p6, cons)
    else if peek_token_is p4 Token.Return then
      (* New: return expr without braces *)
      let p5 = next_token p4 in
      (* Now at 'return' token *)
      let pos_ret = p5.curr_token.pos in
      let* p6, expr = parse_expression (next_token p5) prec_lowest in
      let p7 = skip p6 Token.Semicolon in
      let ret_stmt = mk_stmt pos_ret (AST.Return expr) in
      Ok (p7, mk_stmt pos_ret (AST.Block [ ret_stmt ]))
    else
      Error (add_error p4 "Expected '{' or 'return' after if condition")
  in

  if not (peek_token_is p5 Token.Else) then
    Ok (p5, mk_expr pos (AST.If (cond, cons, None)))
  else
    let p6 = next_token p5 in
    (* Now at 'else' token *)
    let* p7, alt =
      if peek_token_is p6 Token.LBrace then
        (* Normal: else { ... } *)
        let* p7 = expect_peek p6 Token.LBrace in
        let* p8, alt = parse_block_statement p7 in
        Ok (p8, alt)
      else if peek_token_is p6 Token.Return then
        (* New: else return expr without braces *)
        let p7 = next_token p6 in
        (* Now at 'return' token *)
        let pos_ret = p7.curr_token.pos in
        let* p8, expr = parse_expression (next_token p7) prec_lowest in
        let p9 = skip p8 Token.Semicolon in
        let ret_stmt = mk_stmt pos_ret (AST.Return expr) in
        Ok (p9, mk_stmt pos_ret (AST.Block [ ret_stmt ]))
      else
        (* Error: else without block or return *)
        Error (add_error p6 "Expected '{' or 'return' after 'else'")
    in
    Ok (p7, mk_expr pos (AST.If (cond, cons, Some alt)))

and parse_block_statement (p : parser) : (parser * AST.statement, parser) result =
  let pos = p.curr_token.pos in
  let rec loop (lp : parser) (stmts : AST.statement list) : (parser * AST.statement, parser) result =
    if curr_token_is lp Token.RBrace || curr_token_is lp Token.EOF then
      Ok (lp, mk_stmt pos (AST.Block (List.rev stmts)))
    else
      let* lp2, new_block = parse_statement lp in
      loop (next_token lp2) ([ new_block ] @ stmts)
  in

  loop (next_token p) []

and parse_function_literal (p : parser) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  (* Phase 2: Parse generic parameters if present *)
  let* p2, generics = parse_generic_params p in
  let* p3 = expect_peek p2 Token.LParen in
  let* p4, params = parse_function_parameters p3 in
  (* Phase 2: Parse return type annotation if present *)
  let* p5, return_type =
    if peek_token_is p4 Token.Arrow then
      let p5 = next_token p4 in
      let* p6, te = parse_type_expr (next_token p5) in
      Ok (p6, Some te)
    else if peek_token_is p4 Token.FatArrow then
      (* Effect annotation - parse but ignore for Phase 2 *)
      let p5 = next_token p4 in
      let* p6, _te = parse_type_expr (next_token p5) in
      Ok (p6, None)
    else
      Ok (p4, None)
  in
  (* After parsing return type or parameters, check for { *)
  let* p6 =
    if curr_token_is p5 Token.LBrace then
      Ok p5
    else if peek_token_is p5 Token.LBrace then
      Ok (next_token p5)
    else
      Error (peek_error p5 Token.LBrace)
  in
  let* p7, body = parse_block_statement p6 in
  Ok (p7, mk_expr pos (AST.Function { generics; params; return_type; body }))

and parse_function_parameters (p : parser) : (parser * (string * AST.type_expr option) list, parser) result =
  if peek_token_is p Token.RParen then
    Ok (next_token p, [])
  else
    let rec loop (lp : parser) (params_acc : (string * AST.type_expr option) list) =
      (* Get the parameter name *)
      if not (curr_token_is lp Token.Ident) then
        Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
      else
        let param_name = lp.curr_token.literal in
        let lp_after_name = next_token lp in
        (* Phase 2: Parse type annotation if present *)
        let* lp_after_annot, type_annot =
          if curr_token_is lp_after_name Token.Colon then
            (* Parse the type expression *)
            let* lp_parse, te = parse_type_expr (next_token lp_after_name) in
            Ok (lp_parse, Some te)
          else
            Ok (lp_after_name, None)
        in
        let new_param = (param_name, type_annot) in
        if curr_token_is lp_after_annot Token.Comma then
          loop (next_token lp_after_annot) (params_acc @ [ new_param ])
        else if curr_token_is lp_after_annot Token.RParen then
          Ok (lp_after_annot, params_acc @ [ new_param ])
        else
          Error (peek_error lp_after_annot Token.RParen)
    in
    let p2 = next_token p in
    loop p2 []

and parse_call_expression (p : parser) (c : AST.expression) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  let* p2, arguments = parse_expression_list p Token.RParen in
  Ok (p2, mk_expr pos (AST.Call (c, arguments)))

and parse_expression_list (p : parser) (end_tt : Token.token_type) : (parser * AST.expression list, parser) result
    =
  if peek_token_is p end_tt then
    Ok (next_token p, [])
  else
    let* p2, arg = parse_expression (next_token p) prec_lowest in

    let rec loop (lp : parser) (args : AST.expression list) =
      if peek_token_is lp Token.Comma then
        let* lp2, arg = parse_expression (next_token (next_token lp)) prec_lowest in
        loop lp2 ([ arg ] @ args)
      else
        let* lp2 = expect_peek lp end_tt in
        Ok (lp2, List.rev args)
    in

    loop p2 [ arg ]

and parse_array_literal (p : parser) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  let* p2, exprs = parse_expression_list p Token.RBracket in
  Ok (p2, mk_expr pos (AST.Array exprs))

and parse_index_expression (p : parser) (left : AST.expression) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  let p2 = next_token p in
  let* p3, index = parse_expression p2 prec_lowest in
  let* p4 = expect_peek p3 Token.RBracket in
  Ok (p4, mk_expr pos (AST.Index (left, index)))

and parse_hash_literal (p : parser) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  let rec loop lp (pairs : (AST.expression * AST.expression) list) =
    if peek_token_is lp Token.RBrace then
      Ok (next_token lp, mk_expr pos (AST.Hash (List.rev pairs)))
    else
      let* lp2, key = parse_expression (next_token lp) prec_lowest in
      let* lp3 = expect_peek lp2 Token.Colon in
      let* lp4, value = parse_expression (next_token lp3) prec_lowest in

      if peek_token_is lp4 Token.Comma then
        loop (next_token lp4) ([ (key, value) ] @ pairs)
      else if not (peek_token_is lp4 Token.RBrace) then
        Error lp4
      else
        loop lp4 ([ (key, value) ] @ pairs)
  in

  loop p []

let parse (s : string) : (AST.program, errors) result =
  match s |> Lexer.init |> init |> parse_program with
  | Ok (_, program) -> Ok program
  | Error parser -> Error (List.rev parser.errors)

module Test = struct
  type test = {
    input : string;
    output : AST.program;
  }

  (* Shorthand constructors for tests (ignoring positions) *)
  let e kind = AST.mk_expr kind
  let s kind = AST.mk_stmt kind

  (* Helper for Let statements with the new record structure *)
  let let_stmt name value = AST.Let { name; value; type_annotation = None }

  (* Helper for Function expressions with the new record structure *)
  let fn_expr params body = AST.Function { generics = None; params; return_type = None; body }

  let run (tests : test list) : bool =
    tests
    |> List.for_all (fun test ->
           match test.input |> parse with
           | Ok program -> AST.program_equal program test.output
           | Error _ -> false)

  let run_print (tests : test list) : unit =
    tests
    |> List.iter (fun test ->
           match test.input |> parse with
           | Ok program ->
               Printf.printf "input:\n%s\n" test.input;
               Printf.printf "expected:\n%s\n" (AST.show_program test.output);
               Printf.printf "output:\n%s\n" (AST.show_program program);
               flush stdout
           | Error errors ->
               Printf.printf "input:\n%s\n" test.input;
               Printf.printf "errors:\n%s\n" (String.concat "\n" errors);
               flush stdout)

  let%test "test_let_statements" =
    [
      { input = "let x = 5;"; output = [ s (let_stmt "x" (e (AST.Integer 5L))) ] };
      { input = "let y = 10;"; output = [ s (let_stmt "y" (e (AST.Integer 10L))) ] };
      { input = "let foobar = 838383;"; output = [ s (let_stmt "foobar" (e (AST.Integer 838383L))) ] };
    ]
    |> run

  let%test "test_return_statements" =
    [
      { input = "return 5;"; output = [ s (AST.Return (e (AST.Integer 5L))) ] };
      { input = "return 10;"; output = [ s (AST.Return (e (AST.Integer 10L))) ] };
      { input = "return foobar;"; output = [ s (AST.Return (e (AST.Identifier "foobar"))) ] };
    ]
    |> run

  let%test "test_identifier_expressions" =
    [ { input = "foobar;"; output = [ s (AST.ExpressionStmt (e (AST.Identifier "foobar"))) ] } ] |> run

  let%test "test_integer_literal_expressions" =
    [ { input = "5;"; output = [ s (AST.ExpressionStmt (e (AST.Integer 5L))) ] } ] |> run

  let%test "test_float_literal_expressions" =
    [ { input = "5.5;"; output = [ s (AST.ExpressionStmt (e (AST.Float 5.5))) ] } ] |> run

  let%test "test_string_literal_expressions" =
    [ { input = "\"hello world\";"; output = [ s (AST.ExpressionStmt (e (AST.String "hello world"))) ] } ] |> run

  let%test "test_array_literals" =
    [
      { input = "[]"; output = [ s (AST.ExpressionStmt (e (AST.Array []))) ] };
      {
        input = "[1, 2, 3];";
        output =
          [
            s (AST.ExpressionStmt (e (AST.Array [ e (AST.Integer 1L); e (AST.Integer 2L); e (AST.Integer 3L) ])));
          ];
      };
      {
        input = "[1, 2 * 2, 3 + 3];";
        output =
          [
            s
              (AST.ExpressionStmt
                 (e
                    (AST.Array
                       [
                         e (AST.Integer 1L);
                         e (AST.Infix (e (AST.Integer 2L), "*", e (AST.Integer 2L)));
                         e (AST.Infix (e (AST.Integer 3L), "+", e (AST.Integer 3L)));
                       ])));
          ];
      };
    ]
    |> run

  let%test "test_index_expressions" =
    [
      {
        input = "myArray[1 + 1];";
        output =
          [
            s
              (AST.ExpressionStmt
                 (e
                    (AST.Index
                       (e (AST.Identifier "myArray"), e (AST.Infix (e (AST.Integer 1L), "+", e (AST.Integer 1L)))))));
          ];
      };
      {
        input = "myArray[1];";
        output = [ s (AST.ExpressionStmt (e (AST.Index (e (AST.Identifier "myArray"), e (AST.Integer 1L))))) ];
      };
    ]
    |> run

  let%test "test_prefix_expressions" =
    [
      { input = "!5;"; output = [ s (AST.ExpressionStmt (e (AST.Prefix ("!", e (AST.Integer 5L))))) ] };
      { input = "-15;"; output = [ s (AST.ExpressionStmt (e (AST.Prefix ("-", e (AST.Integer 15L))))) ] };
      {
        input = "!foobar;";
        output = [ s (AST.ExpressionStmt (e (AST.Prefix ("!", e (AST.Identifier "foobar"))))) ];
      };
      {
        input = "-foobar;";
        output = [ s (AST.ExpressionStmt (e (AST.Prefix ("-", e (AST.Identifier "foobar"))))) ];
      };
      { input = "!true;"; output = [ s (AST.ExpressionStmt (e (AST.Prefix ("!", e (AST.Boolean true))))) ] };
      { input = "!false;"; output = [ s (AST.ExpressionStmt (e (AST.Prefix ("!", e (AST.Boolean false))))) ] };
    ]
    |> run

  let%test "test_infix_expressions" =
    [
      {
        input = "5 + 5;";
        output = [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Integer 5L), "+", e (AST.Integer 5L))))) ];
      };
      {
        input = "5 - 5;";
        output = [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Integer 5L), "-", e (AST.Integer 5L))))) ];
      };
      {
        input = "5 * 5;";
        output = [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Integer 5L), "*", e (AST.Integer 5L))))) ];
      };
      {
        input = "5 / 5;";
        output = [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Integer 5L), "/", e (AST.Integer 5L))))) ];
      };
      {
        input = "5 > 5;";
        output = [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Integer 5L), ">", e (AST.Integer 5L))))) ];
      };
      {
        input = "5 < 5;";
        output = [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Integer 5L), "<", e (AST.Integer 5L))))) ];
      };
      {
        input = "5 == 5;";
        output = [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Integer 5L), "==", e (AST.Integer 5L))))) ];
      };
      {
        input = "5 != 5;";
        output = [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Integer 5L), "!=", e (AST.Integer 5L))))) ];
      };
      {
        input = "foo + bar;";
        output =
          [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Identifier "foo"), "+", e (AST.Identifier "bar"))))) ];
      };
      {
        input = "foo - bar;";
        output =
          [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Identifier "foo"), "-", e (AST.Identifier "bar"))))) ];
      };
      {
        input = "foo * bar;";
        output =
          [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Identifier "foo"), "*", e (AST.Identifier "bar"))))) ];
      };
      {
        input = "foo / bar;";
        output =
          [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Identifier "foo"), "/", e (AST.Identifier "bar"))))) ];
      };
      {
        input = "foo > bar;";
        output =
          [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Identifier "foo"), ">", e (AST.Identifier "bar"))))) ];
      };
      {
        input = "foo < bar;";
        output =
          [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Identifier "foo"), "<", e (AST.Identifier "bar"))))) ];
      };
      {
        input = "foo == bar;";
        output =
          [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Identifier "foo"), "==", e (AST.Identifier "bar"))))) ];
      };
      {
        input = "foo != bar;";
        output =
          [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Identifier "foo"), "!=", e (AST.Identifier "bar"))))) ];
      };
      {
        input = "true == true;";
        output = [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Boolean true), "==", e (AST.Boolean true))))) ];
      };
      {
        input = "true != false;";
        output = [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Boolean true), "!=", e (AST.Boolean false))))) ];
      };
      {
        input = "false == false;";
        output = [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Boolean false), "==", e (AST.Boolean false))))) ];
      };
    ]
    |> run

  let%test "test_operator_precedence" =
    [
      ("-a * b", "((-a) * b)");
      ("!-a", "(!(-a))");
      ("a + b + c", "((a + b) + c)");
      ("a + b - c", "((a + b) - c)");
      ("a * b * c", "((a * b) * c)");
      ("a * b / c", "((a * b) / c)");
      ("a + b / c", "(a + (b / c))");
      ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)");
      ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)");
      ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))");
      ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))");
      ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))");
      ("true", "true");
      ("false", "false");
      ("3 > 5 == false", "((3 > 5) == false)");
      ("3 < 5 == true", "((3 < 5) == true)");
      ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)");
      ("(5 + 5) * 2", "((5 + 5) * 2)");
      ("2 / (5 + 5)", "(2 / (5 + 5))");
      ("(5 + 5) * 2 * (5 + 5)", "(((5 + 5) * 2) * (5 + 5))");
      ("-(5 + 5)", "(-(5 + 5))");
      ("!(true == true)", "(!(true == true))");
      ("a + add(b * c) + d", "((a + add((b * c))) + d)");
      ("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))");
      ("add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))");
      ("a * [1, 2, 3, 4][b * c] * d", "((a * ([1, 2, 3, 4][(b * c)])) * d)");
      ("add(a * b[2], b[1], 2 * [1, 2][1])", "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))");
    ]
    |> List.for_all (fun test ->
           let input, output = test in
           match input |> parse with
           | Ok program -> AST.to_string program = output
           | Error _ -> false)

  let%test "test_boolean_expression" =
    [
      { input = "true;"; output = [ s (AST.ExpressionStmt (e (AST.Boolean true))) ] };
      { input = "false;"; output = [ s (AST.ExpressionStmt (e (AST.Boolean false))) ] };
    ]
    |> run

  let%test "test_if_expression" =
    [
      {
        input = "if (x < y) { x }";
        output =
          [
            s
              (AST.ExpressionStmt
                 (e
                    (AST.If
                       ( e (AST.Infix (e (AST.Identifier "x"), "<", e (AST.Identifier "y"))),
                         s (AST.Block [ s (AST.ExpressionStmt (e (AST.Identifier "x"))) ]),
                         None ))));
          ];
      };
      {
        input = "if (x < y) { x } else { y }";
        output =
          [
            s
              (AST.ExpressionStmt
                 (e
                    (AST.If
                       ( e (AST.Infix (e (AST.Identifier "x"), "<", e (AST.Identifier "y"))),
                         s (AST.Block [ s (AST.ExpressionStmt (e (AST.Identifier "x"))) ]),
                         Some (s (AST.Block [ s (AST.ExpressionStmt (e (AST.Identifier "y"))) ])) ))));
          ];
      };
    ]
    |> run

  let%test "test_function_expression" =
    [
      {
        input = "fn(x, y) { x + y; }";
        output =
          [
            s
              (AST.ExpressionStmt
                 (e
                    (fn_expr
                       [ ("x", None); ("y", None) ]
                       (s
                          (AST.Block
                             [
                               s
                                 (AST.ExpressionStmt
                                    (e (AST.Infix (e (AST.Identifier "x"), "+", e (AST.Identifier "y")))));
                             ])))));
          ];
      };
      { input = "fn() {}"; output = [ s (AST.ExpressionStmt (e (fn_expr [] (s (AST.Block []))))) ] };
      { input = "fn(x) {}"; output = [ s (AST.ExpressionStmt (e (fn_expr [ ("x", None) ] (s (AST.Block []))))) ] };
      {
        input = "fn(foo, bar, baz) {};";
        output =
          [
            s
              (AST.ExpressionStmt (e (fn_expr [ ("foo", None); ("bar", None); ("baz", None) ] (s (AST.Block [])))));
          ];
      };
    ]
    |> run

  let%test "test_call_expressions" =
    [
      {
        input = "add(1, 2 * 3, 4 + 5);";
        output =
          [
            s
              (AST.ExpressionStmt
                 (e
                    (AST.Call
                       ( e (AST.Identifier "add"),
                         [
                           e (AST.Integer 1L);
                           e (AST.Infix (e (AST.Integer 2L), "*", e (AST.Integer 3L)));
                           e (AST.Infix (e (AST.Integer 4L), "+", e (AST.Integer 5L)));
                         ] ))));
          ];
      };
      {
        input = "fn(x, y) { x + y; }(2, 3)";
        output =
          [
            s
              (AST.ExpressionStmt
                 (e
                    (AST.Call
                       ( e
                           (fn_expr
                              [ ("x", None); ("y", None) ]
                              (s
                                 (AST.Block
                                    [
                                      s
                                        (AST.ExpressionStmt
                                           (e (AST.Infix (e (AST.Identifier "x"), "+", e (AST.Identifier "y")))));
                                    ]))),
                         [ e (AST.Integer 2L); e (AST.Integer 3L) ] ))));
          ];
      };
      {
        input = "callsFunction(2, 3, fn(x, y) { x + y; });";
        output =
          [
            s
              (AST.ExpressionStmt
                 (e
                    (AST.Call
                       ( e (AST.Identifier "callsFunction"),
                         [
                           e (AST.Integer 2L);
                           e (AST.Integer 3L);
                           e
                             (fn_expr
                                [ ("x", None); ("y", None) ]
                                (s
                                   (AST.Block
                                      [
                                        s
                                          (AST.ExpressionStmt
                                             (e (AST.Infix (e (AST.Identifier "x"), "+", e (AST.Identifier "y")))));
                                      ])));
                         ] ))));
          ];
      };
    ]
    |> run

  let%test "test_hash_literal" =
    [
      { input = "{}"; output = [ s (AST.ExpressionStmt (e (AST.Hash []))) ] };
      {
        input = "{\"one\": 1, \"two\": 2, \"three\": 3}";
        output =
          [
            s
              (AST.ExpressionStmt
                 (e
                    (AST.Hash
                       [
                         (e (AST.String "one"), e (AST.Integer 1L));
                         (e (AST.String "two"), e (AST.Integer 2L));
                         (e (AST.String "three"), e (AST.Integer 3L));
                       ])));
          ];
      };
      {
        input = "{ \"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15 / 5 }";
        output =
          [
            s
              (AST.ExpressionStmt
                 (e
                    (AST.Hash
                       [
                         (e (AST.String "one"), e (AST.Infix (e (AST.Integer 0L), "+", e (AST.Integer 1L))));
                         (e (AST.String "two"), e (AST.Infix (e (AST.Integer 10L), "-", e (AST.Integer 8L))));
                         (e (AST.String "three"), e (AST.Infix (e (AST.Integer 15L), "/", e (AST.Integer 5L))));
                       ])));
          ];
      };
    ]
    |> run
end
