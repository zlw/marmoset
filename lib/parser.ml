open Ast

type parser = {
  lexer : Lexer.lexer;
  curr_token : Token.token;
  peek_token : Token.token;
  errors : string list;
}

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
  | Token.Eq | Token.NotEq -> prec_equals
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
  let msg = Printf.sprintf "no prefix parse function for %s found" (Token.show_token_type t) in
  add_error p msg

let rec parse_program (p : parser) : parser * AST.program =
  let rec loop (lp : parser) (prog : AST.program) =
    if curr_token_is lp Token.EOF then
      (lp, List.rev prog)
    else
      let lp2, prog2 =
        match parse_statement lp with Ok (lp3, stmt) -> (lp3, [ stmt ] @ prog) | Error lp3 -> (lp3, prog)
      in

      loop (next_token lp2) prog2
  in

  loop p []

and parse_statement (p : parser) : (parser * AST.statement, parser) result =
  match p.curr_token.token_type with
  | Token.Let -> parse_let_statement p
  | Token.Return -> parse_return_statement p
  | _ -> parse_expression_statement p

and parse_let_statement (p : parser) : (parser * AST.statement, parser) result =
  match expect_peek p Token.Ident with
  | Error p2 -> Error p2
  | Ok p2 -> (
      match expect_peek p2 Token.Assign with
      | Error p3 -> Error p3
      | Ok p3 -> (
          match parse_expression (next_token p3) prec_lowest with
          | Error p4 -> Error p4
          | Ok (p4, expr) ->
              if peek_token_is p4 Token.Semicolon then
                Ok (next_token p4, AST.Let (p2.curr_token.literal, expr))
              else
                Ok (p4, AST.Let (p2.curr_token.literal, expr))))

and parse_return_statement (p : parser) : (parser * AST.statement, parser) result =
  match parse_expression (next_token p) prec_lowest with
  | Error p2 -> Error p2
  | Ok (p2, expr) ->
      if peek_token_is p2 Token.Semicolon then
        Ok (next_token p2, AST.Return expr)
      else
        Ok (p2, AST.Return expr)

and parse_expression_statement (p : parser) : (parser * AST.statement, parser) result =
  match parse_expression p prec_lowest with
  | Error p2 -> Error p2
  | Ok (p2, expr) ->
      if peek_token_is p2 Token.Semicolon then
        Ok (next_token p2, AST.Expression expr)
      else
        Ok (p2, AST.Expression expr)

and parse_expression (p : parser) (prec : precedence) : (parser * AST.expression, parser) result =
  let tt = p.curr_token.token_type in
  let result =
    match tt with
    | Token.Ident -> Ok (parse_identifier p)
    | Token.Int -> Ok (parse_integer_literal p)
    | Token.String -> Ok (parse_string_literal p)
    | Token.Bang | Token.Minus -> Ok (parse_prefix_expression p)
    | Token.True | Token.False -> Ok (parse_boolean p)
    | Token.LParen -> Ok (parse_grouped_expression p)
    | Token.If -> Ok (parse_if_expression p)
    | Token.Function -> Ok (parse_function_literal p)
    | Token.LBracket -> Ok (parse_array_literal p)
    | Token.LBrace -> Ok (parse_hash_literal p)
    | _ -> Error (no_prefix_parse_fn_error p tt)
  in

  match result with
  | Error p2 -> Error p2
  | Ok (p2, left_expr) ->
      let rec loop (lp : parser) (left : AST.expression) : parser * AST.expression =
        let peek_is_semicolon = peek_token_is lp Token.Semicolon in
        let lower_precedence = prec < peek_precedence lp in
        if (not peek_is_semicolon) && lower_precedence then
          let lp2, left2 =
            match lp.peek_token.token_type with
            | Token.Plus | Token.Minus | Token.Slash | Token.Asterisk | Token.Eq | Token.NotEq | Token.Lt
            | Token.Gt ->
                parse_infix_expression (next_token lp) left
            | LParen -> parse_call_expression (next_token lp) left
            | LBracket -> parse_index_expression (next_token lp) left
            | _ -> (lp, left)
          in
          loop lp2 left2
        else
          (lp, left)
      in
      Ok (loop p2 left_expr)

and parse_identifier (p : parser) : parser * AST.expression = (p, AST.Identifier p.curr_token.literal)

and parse_integer_literal (p : parser) : parser * AST.expression =
  match Int64.of_string_opt p.curr_token.literal with
  | Some int -> (p, AST.Integer int)
  | None -> failwith ("can't parse number from " ^ p.curr_token.literal)

and parse_string_literal (p : parser) : parser * AST.expression = (p, AST.String p.curr_token.literal)

and parse_prefix_expression (p : parser) : parser * AST.expression =
  let op = p.curr_token.literal in
  let p2 = next_token p in
  match parse_expression p2 prec_prefix with
  | Ok (p3, right) -> (p3, AST.Prefix (op, right))
  | Error _p3 -> failwith "parse_prefix_expression failed"

and parse_infix_expression (p : parser) (left : AST.expression) : parser * AST.expression =
  let op = p.curr_token.literal in
  let prec = curr_precedence p in
  let p2 = next_token p in
  match parse_expression p2 prec with
  | Ok (p3, right) -> (p3, AST.Infix (left, op, right))
  | Error _p3 -> failwith "parse_infix_expression failed"

and parse_boolean (p : parser) : parser * AST.expression = (p, AST.Boolean (p.curr_token.token_type = Token.True))

and parse_grouped_expression (p : parser) : parser * AST.expression =
  match parse_expression (next_token p) prec_lowest with
  | Error _p2 -> failwith "parse_grouped_expression failed"
  | Ok (p2, expr) -> (
      match expect_peek p2 Token.RParen with
      | Ok p3 -> (p3, expr)
      | Error _p3 -> failwith "parse_grouped_expression failed")

and parse_if_expression (p : parser) : parser * AST.expression =
  match expect_peek p Token.LParen with
  | Error _p2 -> failwith "parse_if_expression failed"
  | Ok p2 -> (
      match parse_expression (next_token p2) prec_lowest with
      | Error _p3 -> failwith "parse_if_expression failed"
      | Ok (p3, cond) -> (
          match expect_peek p3 Token.RParen with
          | Error _p4 -> failwith "parse_if_expression failed"
          | Ok p4 -> (
              match expect_peek p4 Token.LBrace with
              | Error _p5 -> failwith "parse_if_expression failed"
              | Ok p5 -> (
                  let p6, cons = parse_block_statement p5 in
                  if not (peek_token_is p6 Token.Else) then
                    (p6, AST.If (cond, cons, None))
                  else
                    match expect_peek (next_token p6) Token.LBrace with
                    | Error _p7 -> failwith "parse_if_expression failed"
                    | Ok p7 ->
                        let p8, alt = parse_block_statement p7 in
                        (p8, AST.If (cond, cons, Some alt))))))

and parse_block_statement (p : parser) : parser * AST.statement =
  let rec loop (lp : parser) (stmts : AST.statement list) =
    if curr_token_is lp Token.RBrace || curr_token_is lp Token.EOF then
      (lp, AST.Block (List.rev stmts))
    else
      let lp2, new_block =
        match parse_statement lp with
        | Ok (new_parser, stmt) -> (new_parser, [ stmt ] @ stmts)
        | Error new_parser -> (new_parser, stmts)
      in

      loop (next_token lp2) new_block
  in
  let p2 = next_token p in

  loop p2 []

and parse_function_literal (p : parser) : parser * AST.expression =
  match expect_peek p Token.LParen with
  | Error _p2 -> failwith "parse_function_literal failed"
  | Ok p2 -> (
      let p3, params = parse_function_parameters p2 in

      match expect_peek p3 Token.LBrace with
      | Error _p4 -> failwith "parse_function_literal failed"
      | Ok p4 ->
          let p5, body = parse_block_statement p4 in
          (p5, AST.Function (params, body)))

and parse_function_parameters (p : parser) : parser * AST.expression list =
  if peek_token_is p Token.RParen then
    (next_token p, [])
  else
    let rec loop (lp : parser) (idents : AST.expression list) =
      if peek_token_is lp Token.Comma then
        let lp2 = next_token (next_token lp) in
        let ident = [ AST.Identifier lp2.curr_token.literal ] @ idents in

        loop lp2 ident
      else
        match expect_peek lp Token.RParen with
        | Error _lp2 -> failwith "can't parse function parameters"
        | Ok lp2 -> (lp2, List.rev idents)
    in
    let p2 = next_token p in

    loop p2 [ AST.Identifier p2.curr_token.literal ]

and parse_call_expression (p : parser) (c : AST.expression) : parser * AST.expression =
  let p2, arguments = parse_expression_list p Token.RParen in
  (p2, AST.Call (c, arguments))

and parse_expression_list (p : parser) (end_tt : Token.token_type) : parser * AST.expression list =
  if peek_token_is p end_tt then
    (next_token p, [])
  else
    match parse_expression (next_token p) prec_lowest with
    | Error _p2 -> failwith "can't parse call arguments"
    | Ok (p2, arg) ->
        let rec loop (lp : parser) (args : AST.expression list) =
          if peek_token_is lp Token.Comma then
            match parse_expression (next_token (next_token lp)) prec_lowest with
            | Error _lp2 -> failwith "can't parse call arguments"
            | Ok (lp2, arg) -> loop lp2 ([ arg ] @ args)
          else
            match expect_peek lp end_tt with
            | Error _lp2 -> failwith "can't parse call arguments"
            | Ok lp2 -> (lp2, List.rev args)
        in

        loop p2 [ arg ]

and parse_array_literal (p : parser) : parser * AST.expression =
  let p2, exprs = parse_expression_list p Token.RBracket in
  (p2, AST.Array exprs)

and parse_index_expression (p : parser) (left : AST.expression) : parser * AST.expression =
  let p2 = next_token p in
  match parse_expression p2 prec_lowest with
  | Error _p3 -> failwith "parse_index_expression failed"
  | Ok (p3, index) -> (
      match expect_peek p3 Token.RBracket with
      | Error _p4 -> failwith "parse_index_expression failed"
      | Ok p4 -> (p4, AST.Index (left, index)))

and parse_hash_literal (p : parser) : parser * AST.expression =
  let rec loop (lp : parser) (pairs : (AST.expression * AST.expression) list) =
    if peek_token_is lp Token.RBrace then
      (next_token lp, AST.Hash (List.rev pairs))
    else
      let lp2, key =
        match parse_expression (next_token lp) prec_lowest with
        | Error _lp2 -> failwith "parse_hash_literal failed"
        | Ok (lp2, key) -> (lp2, key)
      in

      match expect_peek lp2 Token.Colon with
      | Error _lp3 -> failwith "parse_hash_literal failed#2"
      | Ok lp3 ->
          let lp4, value =
            match parse_expression (next_token lp3) prec_lowest with
            | Error _lp4 -> failwith "parse_hash_literal failed"
            | Ok (lp4, value) -> (lp4, value)
          in

          if peek_token_is lp4 Token.Comma then
            loop (next_token lp4) ([ (key, value) ] @ pairs)
          else if not (peek_token_is lp4 Token.RBrace) then
            failwith "parse_hash_literal failed"
          else
            loop lp4 ([ (key, value) ] @ pairs)
  in

  loop p []

let parse (s : string) : (AST.program, string list) result =
  let parser, program = s |> Lexer.init |> init |> parse_program in

  if parser.errors = [] then
    Ok program
  else
    Error (List.rev parser.errors)

module Test = struct
  type test = {
    input : string;
    output : AST.program;
  }

  let run (tests : test list) : bool =
    tests
    |> List.for_all (fun test ->
           match test.input |> parse with Ok program -> program = test.output | Error _ -> false)

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
      { input = "let x = 5;"; output = [ AST.Let ("x", AST.Integer 5L) ] };
      { input = "let y = 10;"; output = [ AST.Let ("y", AST.Integer 10L) ] };
      { input = "let foobar = 838383;"; output = [ AST.Let ("foobar", AST.Integer 838383L) ] };
    ]
    |> run

  let%test "test_return_statements" =
    [
      { input = "return 5;"; output = [ AST.Return (AST.Integer 5L) ] };
      { input = "return 10;"; output = [ AST.Return (AST.Integer 10L) ] };
      { input = "return foobar;"; output = [ AST.Return (AST.Identifier "foobar") ] };
    ]
    |> run

  let%test "test_identifier_expressions" =
    [ { input = "foobar;"; output = [ AST.Expression (AST.Identifier "foobar") ] } ] |> run

  let%test "test_integer_literal_expressions" =
    [ { input = "5;"; output = [ AST.Expression (AST.Integer 5L) ] } ] |> run

  let%test "test_string_literal_expressions" =
    [ { input = "\"hello world\";"; output = [ AST.Expression (AST.String "hello world") ] } ] |> run

  let%test "test_array_literals" =
    [
      { input = "[]"; output = [ AST.Expression (AST.Array []) ] };
      {
        input = "[1, 2, 3];";
        output = [ AST.Expression (AST.Array [ AST.Integer 1L; AST.Integer 2L; AST.Integer 3L ]) ];
      };
      {
        input = "[1, 2 * 2, 3 + 3];";
        output =
          [
            AST.Expression
              (AST.Array
                 [
                   AST.Integer 1L;
                   AST.Infix (AST.Integer 2L, "*", AST.Integer 2L);
                   AST.Infix (AST.Integer 3L, "+", AST.Integer 3L);
                 ]);
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
            AST.Expression (AST.Index (AST.Identifier "myArray", AST.Infix (AST.Integer 1L, "+", AST.Integer 1L)));
          ];
      };
      {
        input = "myArray[1];";
        output = [ AST.Expression (AST.Index (AST.Identifier "myArray", AST.Integer 1L)) ];
      };
    ]
    |> run

  let%test "test_prefix_expressions" =
    [
      { input = "!5;"; output = [ AST.Expression (AST.Prefix ("!", AST.Integer 5L)) ] };
      { input = "-15;"; output = [ AST.Expression (AST.Prefix ("-", AST.Integer 15L)) ] };
      { input = "!foobar;"; output = [ AST.Expression (AST.Prefix ("!", AST.Identifier "foobar")) ] };
      { input = "-foobar;"; output = [ AST.Expression (AST.Prefix ("-", AST.Identifier "foobar")) ] };
      { input = "!true;"; output = [ AST.Expression (AST.Prefix ("!", AST.Boolean true)) ] };
      { input = "!false;"; output = [ AST.Expression (AST.Prefix ("!", AST.Boolean false)) ] };
    ]
    |> run

  let%test "test_infix_expressions" =
    [
      { input = "5 + 5;"; output = [ AST.Expression (AST.Infix (AST.Integer 5L, "+", AST.Integer 5L)) ] };
      { input = "5 - 5;"; output = [ AST.Expression (AST.Infix (AST.Integer 5L, "-", AST.Integer 5L)) ] };
      { input = "5 * 5;"; output = [ AST.Expression (AST.Infix (AST.Integer 5L, "*", AST.Integer 5L)) ] };
      { input = "5 / 5;"; output = [ AST.Expression (AST.Infix (AST.Integer 5L, "/", AST.Integer 5L)) ] };
      { input = "5 > 5;"; output = [ AST.Expression (AST.Infix (AST.Integer 5L, ">", AST.Integer 5L)) ] };
      { input = "5 < 5;"; output = [ AST.Expression (AST.Infix (AST.Integer 5L, "<", AST.Integer 5L)) ] };
      { input = "5 == 5;"; output = [ AST.Expression (AST.Infix (AST.Integer 5L, "==", AST.Integer 5L)) ] };
      { input = "5 != 5;"; output = [ AST.Expression (AST.Infix (AST.Integer 5L, "!=", AST.Integer 5L)) ] };
      {
        input = "foo + bar;";
        output = [ AST.Expression (AST.Infix (AST.Identifier "foo", "+", AST.Identifier "bar")) ];
      };
      {
        input = "foo - bar;";
        output = [ AST.Expression (AST.Infix (AST.Identifier "foo", "-", AST.Identifier "bar")) ];
      };
      {
        input = "foo * bar;";
        output = [ AST.Expression (AST.Infix (AST.Identifier "foo", "*", AST.Identifier "bar")) ];
      };
      {
        input = "foo / bar;";
        output = [ AST.Expression (AST.Infix (AST.Identifier "foo", "/", AST.Identifier "bar")) ];
      };
      {
        input = "foo > bar;";
        output = [ AST.Expression (AST.Infix (AST.Identifier "foo", ">", AST.Identifier "bar")) ];
      };
      {
        input = "foo < bar;";
        output = [ AST.Expression (AST.Infix (AST.Identifier "foo", "<", AST.Identifier "bar")) ];
      };
      {
        input = "foo == bar;";
        output = [ AST.Expression (AST.Infix (AST.Identifier "foo", "==", AST.Identifier "bar")) ];
      };
      {
        input = "foo != bar;";
        output = [ AST.Expression (AST.Infix (AST.Identifier "foo", "!=", AST.Identifier "bar")) ];
      };
      {
        input = "true == true;";
        output = [ AST.Expression (AST.Infix (AST.Boolean true, "==", AST.Boolean true)) ];
      };
      {
        input = "true != false;";
        output = [ AST.Expression (AST.Infix (AST.Boolean true, "!=", AST.Boolean false)) ];
      };
      {
        input = "false == false;";
        output = [ AST.Expression (AST.Infix (AST.Boolean false, "==", AST.Boolean false)) ];
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
           match input |> parse with Ok program -> AST.to_string program = output | Error _ -> false)

  let%test "test_boolean_expression" =
    [
      { input = "true;"; output = [ AST.Expression (AST.Boolean true) ] };
      { input = "false;"; output = [ AST.Expression (AST.Boolean false) ] };
    ]
    |> run

  let%test "test_if_expression" =
    [
      {
        input = "if (x < y) { x }";
        output =
          [
            AST.Expression
              (AST.If
                 ( AST.Infix (AST.Identifier "x", "<", AST.Identifier "y"),
                   AST.Block [ AST.Expression (AST.Identifier "x") ],
                   None ));
          ];
      };
      {
        input = "if (x < y) { x } else { y }";
        output =
          [
            AST.Expression
              (AST.If
                 ( AST.Infix (AST.Identifier "x", "<", AST.Identifier "y"),
                   AST.Block [ AST.Expression (AST.Identifier "x") ],
                   Some (AST.Block [ AST.Expression (AST.Identifier "y") ]) ));
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
            AST.Expression
              (AST.Function
                 ( [ AST.Identifier "x"; AST.Identifier "y" ],
                   AST.Block [ AST.Expression (AST.Infix (AST.Identifier "x", "+", AST.Identifier "y")) ] ));
          ];
      };
      { input = "fn() {}"; output = [ AST.Expression (AST.Function ([], AST.Block [])) ] };
      { input = "fn(x) {}"; output = [ AST.Expression (AST.Function ([ AST.Identifier "x" ], AST.Block [])) ] };
      {
        input = "fn(foo, bar, baz) {};";
        output =
          [
            AST.Expression
              (AST.Function ([ AST.Identifier "foo"; AST.Identifier "bar"; AST.Identifier "baz" ], AST.Block []));
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
            AST.Expression
              (AST.Call
                 ( AST.Identifier "add",
                   [
                     AST.Integer 1L;
                     AST.Infix (AST.Integer 2L, "*", AST.Integer 3L);
                     AST.Infix (AST.Integer 4L, "+", AST.Integer 5L);
                   ] ));
          ];
      };
      {
        input = "fn(x, y) { x + y; }(2, 3)";
        output =
          [
            AST.Expression
              (AST.Call
                 ( AST.Function
                     ( [ AST.Identifier "x"; AST.Identifier "y" ],
                       AST.Block [ AST.Expression (AST.Infix (AST.Identifier "x", "+", AST.Identifier "y")) ] ),
                   [ AST.Integer 2L; AST.Integer 3L ] ));
          ];
      };
      {
        input = "callsFunction(2, 3, fn(x, y) { x + y; });";
        output =
          [
            AST.Expression
              (AST.Call
                 ( AST.Identifier "callsFunction",
                   [
                     AST.Integer 2L;
                     AST.Integer 3L;
                     AST.Function
                       ( [ AST.Identifier "x"; AST.Identifier "y" ],
                         AST.Block [ AST.Expression (AST.Infix (AST.Identifier "x", "+", AST.Identifier "y")) ] );
                   ] ));
          ];
      };
    ]
    |> run

  let%test "test_hash_literal" =
    [
      { input = "{}"; output = [ AST.Expression (AST.Hash []) ] };
      {
        input = "{\"one\": 1, \"two\": 2, \"three\": 3}";
        output =
          [
            AST.Expression
              (AST.Hash
                 [
                   (AST.String "one", AST.Integer 1L);
                   (AST.String "two", AST.Integer 2L);
                   (AST.String "three", AST.Integer 3L);
                 ]);
          ];
      };
      {
        input = "{ \"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15 / 5 }";
        output =
          [
            AST.Expression
              (AST.Hash
                 [
                   (AST.String "one", AST.Infix (AST.Integer 0L, "+", AST.Integer 1L));
                   (AST.String "two", AST.Infix (AST.Integer 10L, "-", AST.Integer 8L));
                   (AST.String "three", AST.Infix (AST.Integer 15L, "/", AST.Integer 5L));
                 ]);
          ];
      };
    ]
    |> run
end
