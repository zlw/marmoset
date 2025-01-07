open Ast

let rec eval (p : AST.program) (e : Env.env) : Value.value * Env.env =
  match eval_program p e with Value.Return v, e' -> (v, e') | v, e' -> (v, e')

and eval_program (stmts : AST.statement list) (e : Env.env) : Value.value * Env.env =
  let rec loop stmts (v, e) =
    match (stmts, v) with
    | [], _ -> (v, e)
    | _, Value.Return return -> (return, e)
    | _, Value.Error _ -> (v, e)
    | stmt :: rest, _ ->
        let v', e' = eval_statement stmt e in
        loop rest (v', e')
  in
  loop stmts (Value.null_value, e)

and eval_statement (stmt : AST.statement) (e : Env.env) : Value.value * Env.env =
  match stmt with
  | Expression expr -> eval_expression expr e
  | Block stmts -> eval_program stmts e
  | Let (ident, expr) -> (
      let v, e' = eval_expression expr e in
      match v with Value.Error _ -> (v, e') | _ -> (v, Env.set e' ident v))
  | Return expr -> (
      let v, e' = eval_expression expr e in
      match v with Value.Error _ -> (v, e') | _ -> (Value.Return v, e'))

and eval_expression (expr : AST.expression) (e : Env.env) : Value.value * Env.env =
  match expr with
  | Integer i -> (Value.Integer i, e)
  | Boolean b ->
      if b then
        (Value.true_value, e)
      else
        (Value.false_value, e)
  | Prefix ("!", right) -> (
      let v, e' = eval_expression right e in
      match v with Value.Boolean b -> (Value.Boolean (not b), e') | _ -> (Value.false_value, e'))
  | Prefix ("-", right) -> (
      let v, e' = eval_expression right e in
      match v with
      | Value.Integer i -> (Value.Integer (Int64.neg i), e')
      | _ ->
          let error = Value.unknown_prefix_operator_error "-" right in
          (error, e'))
  | Infix (left, op, right) -> (
      let left', e' = eval_expression left e in

      match left' with
      | Value.Error _ -> (left', e')
      | _ -> (
          let right', e'' = eval_expression right e' in
          match right' with
          | Value.Error _ -> (right', e'')
          | _ -> (
              match (left', right') with
              | Value.Integer left'', Value.Integer right'' -> (
                  match op with
                  | "+" -> (Value.Integer (Int64.add left'' right''), e'')
                  | "-" -> (Value.Integer (Int64.sub left'' right''), e'')
                  | "*" -> (Value.Integer (Int64.mul left'' right''), e'')
                  | "/" -> (Value.Integer (Int64.div left'' right''), e'')
                  | "<" -> (Value.Boolean (left'' < right''), e'')
                  | ">" -> (Value.Boolean (left'' > right''), e'')
                  | "==" -> (Value.Boolean (left'' = right''), e'')
                  | "!=" -> (Value.Boolean (left'' <> right''), e'')
                  | _ ->
                      let error = Value.unknown_infix_operator_error left op right in
                      (error, e''))
              | Value.Boolean left'', Value.Boolean right'' -> (
                  match op with
                  | "==" -> (Value.Boolean (left'' = right''), e'')
                  | "!=" -> (Value.Boolean (left'' <> right''), e'')
                  | _ ->
                      let error = Value.unknown_infix_operator_error left op right in
                      (error, e''))
              | _ ->
                  let error = Value.type_mismatch_error left op right in
                  (error, e''))))
  | If (condition, consequence, alternative) -> (
      let cond, e' = eval_expression condition e in
      match cond with
      | Value.Error _ -> (cond, e')
      | _ -> (
          if Value.is_truthy cond then
            eval_statement consequence e'
          else
            match alternative with
            | None -> (Value.null_value, e')
            | Some alt -> eval_statement alt e'))
  | Identifier ident -> (
      match Env.get e ident with Some v -> (v, e) | None -> (Value.Error ("identifier not found: " ^ ident), e))
  | Function (params, body) -> (Value.Function (params, body), e)
  | Call (func, args) -> (
      let func', e' = eval_expression func e in
      match func' with
      | Error _ -> (func', e')
      | Value.Function (_, _) -> (
          let args', e'' = eval_arguments args e' in
          match args' with [ Value.Error _ ] -> (List.hd args', e'') | _ -> apply_function func' args' e')
      | _ -> failwith "foo")
  | _ -> failwith "not implemented"

and eval_arguments (args : AST.expression list) (e : Env.env) : Value.value list * Env.env =
  let rec loop exps result env =
    match exps with
    | [] -> (List.rev result, env)
    | head :: rest -> (
        let v, e' = eval_expression head env in
        match v with Value.Error _ -> ([ v ], e') | _ -> loop rest (v :: result) e')
  in

  loop args [] e

and apply_function (func : Value.value) (args : Value.value list) (env : Env.env) : Value.value * Env.env =
  match func with
  | Value.Function (params, body) ->
      let e' = extend_function_env params args env in
      let evaluated, e'' = eval_statement body e' in
      (unwrap_return_value evaluated, e'')
  | _ -> (Value.Error "not a function", env)

and unwrap_return_value (foo : Value.value) : Value.value =
  match foo with Value.Return return_value -> return_value | _ -> foo

and extend_function_env params args env : Env.env =
  let env' = Env.wrap env in
  List.fold_left2
    (fun acc param arg ->
      match param with AST.Identifier str -> Env.set acc str arg | _ -> failwith "expect identifier")
    env' params args

module Test = struct
  type test = {
    input : string;
    output : Value.value;
  }

  let run (tests : test list) : bool =
    tests
    |> List.for_all (fun { input; output } ->
           let _, p = Lexer.init input |> Parser.init |> Parser.parse_program in
           let v, _ = eval p Env.init in
           v = output)

  let run_print (tests : test list) : unit =
    tests
    |> List.iter (fun test ->
           let parser, program = test.input |> Lexer.init |> Parser.init |> Parser.parse_program in
           let value, _ = eval program Env.init in
           Printf.printf "input:\n%s\n" test.input;
           Printf.printf "expected:\n%s\n" (Value.show_value test.output);
           Printf.printf "output:\n%s\n" (Value.show_value value);
           Printf.printf "errors:\n%s\n" (String.concat ", " parser.errors);
           flush stdout)

  let%test "test_eval_integer_expression" =
    [
      { input = "5;"; output = Value.Integer 5L };
      { input = "10;"; output = Value.Integer 10L };
      { input = "-5;"; output = Value.Integer (-5L) };
      { input = "-10;"; output = Value.Integer (-10L) };
      { input = "5 + 5 + 5 + 5 - 10;"; output = Value.Integer 10L };
      { input = "2 * 2 * 2 * 2 * 2;"; output = Value.Integer 32L };
      { input = "-50 + 100 + -50;"; output = Value.Integer 0L };
      { input = "5 * 2 + 10;"; output = Value.Integer 20L };
      { input = "5 + 2 * 10;"; output = Value.Integer 25L };
      { input = "20 + 2 * -10;"; output = Value.Integer 0L };
      { input = "50 / 2 * 2 + 10;"; output = Value.Integer 60L };
      { input = "2 * (5 + 10);"; output = Value.Integer 30L };
      { input = "3 * 3 * 3 + 10;"; output = Value.Integer 37L };
      { input = "3 * (3 * 3) + 10;"; output = Value.Integer 37L };
      { input = "(5 + 10 * 2 + 15 / 3) * 2 + -10;"; output = Value.Integer 50L };
    ]
    |> run

  let%test "test_eval_boolean_expression" =
    [
      { input = "true;"; output = Value.Boolean true };
      { input = "false;"; output = Value.Boolean false };
      { input = "1 < 2;"; output = Value.Boolean true };
      { input = "1 > 2;"; output = Value.Boolean false };
      { input = "1 < 1;"; output = Value.Boolean false };
      { input = "1 > 1;"; output = Value.Boolean false };
      { input = "1 == 1;"; output = Value.Boolean true };
      { input = "1 != 1;"; output = Value.Boolean false };
      { input = "1 == 2;"; output = Value.Boolean false };
      { input = "1 != 2;"; output = Value.Boolean true };
      { input = "true == true;"; output = Value.Boolean true };
      { input = "false == false;"; output = Value.Boolean true };
      { input = "true == false;"; output = Value.Boolean false };
      { input = "true != false;"; output = Value.Boolean true };
      { input = "false != true;"; output = Value.Boolean true };
      { input = "(1 < 2) == true;"; output = Value.Boolean true };
      { input = "(1 < 2) == false;"; output = Value.Boolean false };
      { input = "(1 > 2) == true;"; output = Value.Boolean false };
      { input = "(1 > 2) == false;"; output = Value.Boolean true };
    ]
    |> run

  let%test "test_bang_operator" =
    [
      { input = "!true;"; output = Value.Boolean false };
      { input = "!false;"; output = Value.Boolean true };
      { input = "!5;"; output = Value.Boolean false };
      { input = "!!true;"; output = Value.Boolean true };
      { input = "!!false;"; output = Value.Boolean false };
      { input = "!!5;"; output = Value.Boolean true };
    ]
    |> run

  let%test "test_if_else_expression" =
    [
      { input = "if (true) { 10 }"; output = Value.Integer 10L };
      { input = "if (false) { 10 }"; output = Value.null_value };
      { input = "if (1) { 10 }"; output = Value.Integer 10L };
      { input = "if (1 < 2) { 10 }"; output = Value.Integer 10L };
      { input = "if (1 > 2) { 10 }"; output = Value.null_value };
      { input = "if (1 > 2) { 10 } else { 20 }"; output = Value.Integer 20L };
      { input = "if (1 < 2) { 10 } else { 20 }"; output = Value.Integer 10L };
    ]
    |> run

  let%test "test_return_statement" =
    [
      { input = "return 10;"; output = Value.Integer 10L };
      { input = "return 10; 9;"; output = Value.Integer 10L };
      { input = "return 2 * 5; 9;"; output = Value.Integer 10L };
      { input = "9; return 2 * 5; 9;"; output = Value.Integer 10L };
      { input = "if (10 > 1) { if (10 > 1) { return 10; } return 1; }"; output = Value.Integer 10L };
    ]
    |> run

  let%test "test_error_handling" =
    [
      { input = "5 + true;"; output = Value.Error "type mismatch: Integer + Boolean" };
      { input = "5 + true; 5;"; output = Value.Error "type mismatch: Integer + Boolean" };
      { input = "-true;"; output = Value.Error "unknown operator: -Boolean" };
      { input = "true + false;"; output = Value.Error "unknown operator: Boolean + Boolean" };
      { input = "true + false + true + false;"; output = Value.Error "unknown operator: Boolean + Boolean" };
      { input = "5; true + false; 5"; output = Value.Error "unknown operator: Boolean + Boolean" };
      { input = "if (10 > 1) { true + false; }"; output = Value.Error "unknown operator: Boolean + Boolean" };
      {
        input = "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }";
        output = Value.Error "unknown operator: Boolean + Boolean";
      };
      { input = "foobar"; output = Value.Error "identifier not found: foobar" };
    ]
    |> run

  let%test "test_let_statements" =
    [
      { input = "let a = 5; a;"; output = Value.Integer 5L };
      { input = "let a = 5 * 5; a;"; output = Value.Integer 25L };
      { input = "let a = 5; let b = a; b;"; output = Value.Integer 5L };
      { input = "let a = 5; let b = a; let c = a + b + 5; c;"; output = Value.Integer 15L };
    ]
    |> run

  let%test "test_function_object" =
    [
      {
        input = "fn(x) { x + 2; };";
        output =
          Value.Function
            ( [ AST.Identifier "x" ],
              AST.Block [ AST.Expression (AST.Infix (AST.Identifier "x", "+", AST.Integer 2L)) ] );
      };
      {
        input = "fn(x, y) { if (x < y) { x } else { y } }";
        output =
          Value.Function
            ( [ AST.Identifier "x"; AST.Identifier "y" ],
              AST.Block
                [
                  AST.Expression
                    (AST.If
                       ( AST.Infix (AST.Identifier "x", "<", AST.Identifier "y"),
                         AST.Block [ AST.Expression (AST.Identifier "x") ],
                         Some (AST.Block [ AST.Expression (AST.Identifier "y") ]) ));
                ] );
      };
    ]
    |> run

  let%test "test_function_application" =
    [
      { input = "let identity = fn(x) { x; }; identity(5);"; output = Value.Integer 5L };
      { input = "let identity = fn(x) { return x; }; identity(5);"; output = Value.Integer 5L };
      { input = "let double = fn(x) { x * 2; }; double(5);"; output = Value.Integer 10L };
      { input = "let add = fn(x, y) { x + y; }; add(5, 5);"; output = Value.Integer 10L };
      { input = "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));"; output = Value.Integer 20L };
      { input = "fn(x) { x; }(5)"; output = Value.Integer 5L };
    ]
    |> run

  let%test "test_closures" =
    [
      {
        input = "let newAdder = fn(x) { fn(y) { x + y }; }; let addTwo = newAdder(2); addTwo(2);";
        output = Value.Integer 4L;
      };
    ]
    |> run
end
