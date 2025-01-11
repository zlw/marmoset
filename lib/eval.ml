open Ast

let ( let* ) res f =
  match res with
  | Value.Error _, _ -> res
  | _ -> f res

type env = Value.value Env.env

let rec eval (p : AST.program) (e : env) : Value.value * env =
  match eval_program p e with
  | Value.Return v, e' -> (v, e')
  | v, e' -> (v, e')

and eval_program (stmts : AST.statement list) (e : env) : Value.value * env =
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

and eval_statement (stmt : AST.statement) (e : env) : Value.value * env =
  match stmt with
  | Expression expr -> eval_expression expr e
  | Block stmts -> eval_program stmts e
  | Let (ident, expr) ->
      let* v, e' = eval_expression expr e in
      (v, Env.set e' ident v)
  | Return expr ->
      let* v, e' = eval_expression expr e in
      (Value.Return v, e')

and eval_expression (expr : AST.expression) (e : env) : Value.value * env =
  match expr with
  | Integer i -> (Value.Integer i, e)
  | Boolean b ->
      if b then
        (Value.true_value, e)
      else
        (Value.false_value, e)
  | String s -> (Value.String s, e)
  | Array exprs ->
      let vs, e' = eval_expressions exprs e in
      (Value.Array vs, e')
  | Index (arr, idx) -> (
      let* arr', e' = eval_expression arr e in
      let* idx', e'' = eval_expression idx e' in
      match (arr', idx') with
      | Value.Array vs, Value.Integer i ->
          if Int64.to_int i >= List.length vs then
            (Value.null_value, e'')
          else if i < 0L then
            if Int64.to_int i + List.length vs < 0 then
              (Value.null_value, e'')
            else
              (List.nth vs (List.length vs + Int64.to_int i), e'')
          else
            (List.nth vs (Int64.to_int i), e'')
      | Value.Hash _, _ -> (
          match Value.get arr' idx' with
          | Some v -> (v, e'')
          | None -> (Value.null_value, e''))
      | Value.String s, Value.Integer i ->
          if Int64.to_int i >= String.length s then
            (Value.null_value, e'')
          else if i < 0L then
            if Int64.to_int i + String.length s < 0 then
              (Value.null_value, e'')
            else
              (Value.String (String.make 1 s.[String.length s + Int64.to_int i]), e'')
          else
            (Value.String (String.sub s (Int64.to_int i) 1), e'')
      | _ ->
          let msg = Printf.sprintf "index operator not supported: %s" (Value.type_of arr') in
          (Value.Error msg, e'))
  | Hash pairs ->
      let rec loop pairs (acc : Value.value) env =
        match pairs with
        | [] -> (acc, env)
        | (k, v) :: rest ->
            let* k', env' = eval_expression k env in
            let* v', env'' = eval_expression v env' in
            loop rest (Value.set acc k' v') env''
      in
      loop pairs (Value.init ()) e
  | Prefix ("!", right) -> (
      let v, e' = eval_expression right e in
      match v with
      | Value.Boolean b -> (Value.Boolean (not b), e')
      | _ -> (Value.false_value, e'))
  | Prefix ("-", right) -> (
      let v, e' = eval_expression right e in
      match v with
      | Value.Integer i -> (Value.Integer (Int64.neg i), e')
      | _ ->
          let error = Value.unknown_prefix_operator_error "-" right in
          (error, e'))
  | Infix (left, op, right) -> (
      let* left', e' = eval_expression left e in
      let* right', e'' = eval_expression right e' in
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
      | Value.String left'', Value.String right'' -> (
          match op with
          | "+" -> (Value.String (left'' ^ right''), e'')
          | _ ->
              let error = Value.unknown_infix_operator_error left op right in
              (error, e''))
      | _ ->
          let error = Value.type_mismatch_error left op right in
          (error, e''))
  | If (condition, consequence, alternative) -> (
      let* cond, e' = eval_expression condition e in
      if Value.is_truthy cond then
        eval_statement consequence e'
      else
        match alternative with
        | None -> (Value.null_value, e')
        | Some alt -> eval_statement alt e')
  | Identifier ident -> (
      match Env.get e ident with
      | Some v -> (v, e)
      | None -> (
          match Builtins.builtin ident with
          | Some f -> (Value.BuiltinFunction f, e)
          | None -> (Value.Error ("identifier not found: " ^ ident), e)))
  | Function (params, body) -> (Value.Function (params, body, Some e), e)
  | Call (func, args) -> (
      let* func', e' = eval_expression func e in
      match func' with
      | Value.Function (_, _, _) | Value.BuiltinFunction _ -> (
          let args', e'' = eval_expressions args e' in
          match args' with
          | [ Value.Error _ ] -> (List.hd args', e'')
          | _ -> (apply_function func' args', e'))
      | _ -> failwith "not a function")
  | _ -> failwith "not implemented"

and eval_expressions (args : AST.expression list) (e : env) : Value.value list * env =
  let rec loop exps result env =
    match exps with
    | [] -> (List.rev result, env)
    | head :: rest -> (
        let v, e' = eval_expression head env in
        match v with
        | Value.Error _ -> ([ v ], e')
        | _ -> loop rest (v :: result) e')
  in

  loop args [] e

and apply_function (func : Value.value) (args : Value.value list) : Value.value =
  match func with
  | Value.Function (params, body, Some env) ->
      let params_len = List.length params in
      let args_len = List.length args in

      if params_len <> args_len then
        let msg = Printf.sprintf "wrong number of arguments. got=%d, want=%d" args_len params_len in
        Value.Error msg
      else
        let e' = extend_function_env params args env in
        let evaluated, _e'' = eval_statement body e' in
        unwrap_return_value evaluated
  | Value.BuiltinFunction f -> f args
  | _ -> Value.Error "not a function"

and unwrap_return_value (return : Value.value) : Value.value =
  match return with
  | Value.Return return_value -> return_value
  | _ -> return

and extend_function_env (params : AST.expression list) (args : Value.value list) (e : env) : env =
  let e' = Env.wrap e in
  List.fold_left2
    (fun acc param arg ->
      match param with
      | AST.Identifier str -> Env.set acc str arg
      | _ -> failwith "expect identifier")
    e' params args

module Test = struct
  type test = {
    input : string;
    output : Value.value;
  }

  let run (tests : test list) : bool =
    tests
    |> List.for_all (fun { input; output } ->
           match Parser.parse input with
           | Error _ -> false
           | Ok p ->
               let v, _ = eval p (Env.init ()) in
               v = output)

  let run_print (tests : test list) : unit =
    tests
    |> List.iter (fun { input; output } ->
           match Parser.parse input with
           | Error e ->
               Printf.printf "errors:\n%s\n" (String.concat ", " e);
               flush stdout
           | Ok p ->
               let value, _ = eval p (Env.init ()) in
               Printf.printf "input:\n%s\n" input;
               Printf.printf "expected:\n%s\n" (Value.show_value output);
               Printf.printf "output:\n%s\n" (Value.show_value value);
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
      { input = "5 + \"foo\";"; output = Value.Error "type mismatch: Integer + String" };
      { input = "-true;"; output = Value.Error "unknown operator: -Boolean" };
      { input = "true + false;"; output = Value.Error "unknown operator: Boolean + Boolean" };
      { input = "true + false + true + false;"; output = Value.Error "unknown operator: Boolean + Boolean" };
      { input = "5; true + false; 5"; output = Value.Error "unknown operator: Boolean + Boolean" };
      { input = "if (10 > 1) { true + false; }"; output = Value.Error "unknown operator: Boolean + Boolean" };
      { input = "\"foo\" - \"bar\""; output = Value.Error "unknown operator: String - String" };
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

  let%test "test_function_application" =
    [
      { input = "let identity = fn(x) { x; }; identity(5);"; output = Value.Integer 5L };
      { input = "let identity = fn(x) { return x; }; identity(5);"; output = Value.Integer 5L };
      { input = "let double = fn(x) { x * 2; }; double(5);"; output = Value.Integer 10L };
      { input = "let add = fn(x, y) { x + y; }; add(5, 5);"; output = Value.Integer 10L };
      { input = "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));"; output = Value.Integer 20L };
      { input = "fn(x) { x; }(5)"; output = Value.Integer 5L };
      { input = "fn(x) { x }(1,2)"; output = Value.Error "wrong number of arguments. got=2, want=1" };
    ]
    |> run

  let%test "test_closures" =
    [
      {
        input = "let newAdder = fn(x) { fn(y) { x + y }; }; let addTwo = newAdder(2); addTwo(2);";
        output = Value.Integer 4L;
      };
      {
        input = "let newAdder = fn(x) { fn(y) { x + y } }; let addTwo = newAdder(2); x;";
        output = Value.Error "identifier not found: x";
      };
      {
        input = "let addTen = fn(x) { let y = 10; fn() { x + y }; }; let f = addTen(5); f()";
        output = Value.Integer 15L;
      };
      {
        input = "let addTen = fn(x) { let y = 10; fn() { x + y }; }; let f = addTen(5); f(); y";
        output = Value.Error "identifier not found: y";
      };
    ]
    |> run

  let%test "test_higher_order_functions" =
    [
      {
        input = "let applyTwice = fn(f, x) { f(f(x)) }; let double = fn(x) { x + x }; applyTwice(double, 3);";
        output = Value.Integer 12L;
      };
      {
        input =
          "let add = fn(a, b) { a + b }; let applyFunc = fn(a, b, func) { func(a, b) }; applyFunc(2, 2, add);";
        output = Value.Integer 4L;
      };
      {
        input =
          "let sub = fn(a, b) { a - b }; let applyFunc = fn(a, b, func) { func(a, b) }; applyFunc(4, 2, sub);";
        output = Value.Integer 2L;
      };
    ]
    |> run

  let%test "test_recursive_functions" =
    [
      {
        input = "let countDown = fn(x) { if (x == 0) { return 0; } else { countDown(x - 1); } }; countDown(5);";
        output = Value.Integer 0L;
      };
      {
        input =
          "let countUp = fn(x, acc) { if (x == acc) { return acc } else { countUp(x, acc + 1) } }; countUp(5, 0);";
        output = Value.Integer 5L;
      };
    ]
    |> run

  let%test "test_eval_string_expression" =
    [
      { input = "\"foobar\";"; output = Value.String "foobar" };
      { input = "\"foo\" + \"bar\";"; output = Value.String "foobar" };
      { input = "\"foo\" + \"bar\" + \"baz\";"; output = Value.String "foobarbaz" };
    ]
    |> run

  let%test "test_string_indexing" =
    [
      { input = "\"hello\"[0]"; output = Value.String "h" };
      { input = "\"hello\"[1]"; output = Value.String "e" };
      { input = "\"hello\"[2]"; output = Value.String "l" };
      { input = "\"hello\"[3]"; output = Value.String "l" };
      { input = "\"hello\"[4]"; output = Value.String "o" };
      { input = "\"hello\"[5]"; output = Value.null_value };
      { input = "\"hello\"[-1]"; output = Value.String "o" };
      { input = "\"hello\"[-2]"; output = Value.String "l" };
      { input = "\"hello\"[-3]"; output = Value.String "l" };
      { input = "\"hello\"[-4]"; output = Value.String "e" };
      { input = "\"hello\"[-5]"; output = Value.String "h" };
      { input = "\"hello\"[-6]"; output = Value.null_value };
    ]
    |> run

  let%test "test_builtin_functions" =
    [
      (* len string *)
      { input = "len(\"\")"; output = Value.Integer 0L };
      { input = "len(\"four\")"; output = Value.Integer 4L };
      { input = "len(\"hello world\")"; output = Value.Integer 11L };
      { input = "len(1)"; output = Value.Error "argument to `len` not supported, got Integer" };
      { input = "len(\"one\", \"two\")"; output = Value.Error "wrong number of arguments. got=2, want=1" };
      (* len array *)
      { input = "len([]);"; output = Value.Integer 0L };
      { input = "len([1, 2, 3]);"; output = Value.Integer 3L };
      { input = "len([1, 2 * 3, 4, 5]);"; output = Value.Integer 4L };
      { input = "len(1);"; output = Value.Error "argument to `len` not supported, got Integer" };
      { input = "len([1, 2], [3, 4]);"; output = Value.Error "wrong number of arguments. got=2, want=1" };
      (* first *)
      { input = "first([1, 2, 3])"; output = Value.Integer 1L };
      { input = "first([])"; output = Value.null_value };
      { input = "first(1)"; output = Value.Error "argument to `first` not supported, got Integer" };
      { input = "first([1, 2], [3, 4])"; output = Value.Error "wrong number of arguments. got=2, want=1" };
      (* last *)
      { input = "last([1, 2, 3])"; output = Value.Integer 3L };
      { input = "last([])"; output = Value.null_value };
      { input = "last(1)"; output = Value.Error "argument to `last` not supported, got Integer" };
      { input = "last([1, 2], [3, 4])"; output = Value.Error "wrong number of arguments. got=2, want=1" };
      (* rest *)
      { input = "rest([1, 2, 3])"; output = Value.Array [ Value.Integer 2L; Value.Integer 3L ] };
      { input = "rest([])"; output = Value.null_value };
      { input = "rest(1)"; output = Value.Error "argument to `rest` not supported, got Integer" };
      { input = "rest([1, 2], [3, 4])"; output = Value.Error "wrong number of arguments. got=2, want=1" };
      (* push *)
      { input = "push([], 1)"; output = Value.Array [ Value.Integer 1L ] };
      { input = "push([1], 2)"; output = Value.Array [ Value.Integer 1L; Value.Integer 2L ] };
      { input = "push([1, 2], 3)"; output = Value.Array [ Value.Integer 1L; Value.Integer 2L; Value.Integer 3L ] };
      { input = "push(1, 2)"; output = Value.Error "argument to `push` not supported, got Integer" };
      { input = "push([1], 2, 3)"; output = Value.Error "wrong number of arguments. got=3, want=2" };
    ]
    |> run

  let%test "test_builtin_functions_integration" =
    let map =
      "
      let map = fn(arr, f) {
        let iter = fn(arr, accumulated) {
          if (len(arr) == 0) {
            accumulated
          } else {
            iter(rest(arr), push(accumulated, f(first(arr))));
          }
        };
        iter(arr, []);
      };
      "
    in

    let reduce =
      "
      let reduce = fn(arr, initial, f) {
        let iter = fn(arr, result) {
          if (len(arr) == 0) {
            result
          } else {
            iter(rest(arr), f(result, first(arr)));
          }
        };
        iter(arr, initial);
      };
      "
    in

    let sum =
      "
      let sum = fn(arr) {
        reduce(arr, 0, fn(initial, el) { initial + el });
      };
      "
    in
    [
      {
        input = "" ^ map ^ "let a = [1, 2, 3, 4]; let double = fn(x) { x * 2 }; map(a, double);";
        output = Value.Array [ Value.Integer 2L; Value.Integer 4L; Value.Integer 6L; Value.Integer 8L ];
      };
      { input = "" ^ reduce ^ "" ^ sum ^ "sum([1,2,3,4,5]);"; output = Value.Integer 15L };
    ]
    |> run

  let%test "test_array_literals" =
    [
      { input = "[]"; output = Value.Array [] };
      { input = "[1, 2, 3]"; output = Value.Array [ Value.Integer 1L; Value.Integer 2L; Value.Integer 3L ] };
      {
        input = "[1 + 2, 3 * 4, 5 + 6]";
        output = Value.Array [ Value.Integer 3L; Value.Integer 12L; Value.Integer 11L ];
      };
    ]
    |> run

  let%test "test_array_indexing" =
    [
      { input = "[1, 2, 3][0]"; output = Value.Integer 1L };
      { input = "[1, 2, 3][1]"; output = Value.Integer 2L };
      { input = "[1, 2, 3][2]"; output = Value.Integer 3L };
      { input = "let i = 0; [1][i]"; output = Value.Integer 1L };
      { input = "[1, 2, 3][1 + 1]"; output = Value.Integer 3L };
      { input = "let myArray = [1, 2, 3]; myArray[2]"; output = Value.Integer 3L };
      { input = "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2]"; output = Value.Integer 6L };
      { input = "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]"; output = Value.Integer 2L };
      { input = "[1, 2, 3][3]"; output = Value.null_value };
      { input = "[1, 2, 3][-1]"; output = Value.Integer 3L };
      { input = "[1, 2, 3][-2]"; output = Value.Integer 2L };
      { input = "[1, 2, 3][-3]"; output = Value.Integer 1L };
      { input = "[1, 2, 3][-4]"; output = Value.null_value };
    ]
    |> run

  let%test "test_hash_literals" =
    [
      { input = "{}"; output = Value.Hash (Hashtbl.create 0) };
      {
        input = "{1: 2, 2: 3}";
        output =
          Value.Hash
            (Hashtbl.of_seq
               (List.to_seq [ (Value.Integer 1L, Value.Integer 2L); (Value.Integer 2L, Value.Integer 3L) ]));
      };
      {
        input = "{1 + 1: 2 * 2, 3 + 3: 4 * 4}";
        output =
          Value.Hash
            (Hashtbl.of_seq
               (List.to_seq [ (Value.Integer 2L, Value.Integer 4L); (Value.Integer 6L, Value.Integer 16L) ]));
      };
      {
        input =
          "let two = \"two\"; { \"one\": 10 - 9, two: 1 + 1, \"thr\" + \"ee\": 6 / 2, 4: 4, true: 5, false: 6 }";
        output =
          Value.Hash
            (Hashtbl.of_seq
               (List.to_seq
                  [
                    (Value.String "one", Value.Integer 1L);
                    (Value.String "two", Value.Integer 2L);
                    (Value.String "three", Value.Integer 3L);
                    (Value.Integer 4L, Value.Integer 4L);
                    (Value.Boolean true, Value.Integer 5L);
                    (Value.Boolean false, Value.Integer 6L);
                  ]));
      };
    ]
    |> run

  let%test "test_hash_index_expressions" =
    [
      { input = "{\"foo\": 5}[\"foo\"]"; output = Value.Integer 5L };
      { input = "{\"foo\": 5}[\"bar\"]"; output = Value.null_value };
      { input = "let key = \"foo\"; {\"foo\": 5}[key]"; output = Value.Integer 5L };
      { input = "{}[\"foo\"]"; output = Value.null_value };
      { input = "{5: 5}[5]"; output = Value.Integer 5L };
      { input = "{true: 5}[true]"; output = Value.Integer 5L };
      { input = "{false: 5}[false]"; output = Value.Integer 5L };
      { input = "{\"name\": \"Monkey\"}[fn(x) { x }];"; output = Value.Error "unusable as hash key: Function" };
    ]
    |> run

  let%test "test_integration" =
    let input =
      "
        let people = [{\"name\": \"Alice\", \"age\": 24}, {\"name\": \"Anna\", \"age\": 28}];
        let getName = fn(person) { person[\"name\"] };
        let getAge = fn(person) { person[\"age\"] };

        [getName(people[0]), getName(people[1]), getAge(people[0]) + getAge(people[1])];
      "
    in
    [ { input; output = Value.Array [ Value.String "Alice"; Value.String "Anna"; Value.Integer 52L ] } ] |> run
end
