module [eval]

import Object exposing [Object, trueObject, falseObject, nullObject]
import AST exposing [Program, Expression]
import Parser
import Lexer
import Environment exposing [Environment]

eval : Program, Environment -> (Object, Environment)
eval = \program, env ->
    when evalExpressions program env is
        (ReturnValue returnValue, new_env) -> (returnValue, new_env)
        (returnValue, new_env) -> (returnValue, new_env)

evalExpressions : List Expression, Environment -> (Object, Environment)
evalExpressions = \expressions, env ->
    loop : List Expression, (Object, Environment) -> (Object, Environment)
    loop = \exprs, (result, looped_env) ->
        when (exprs, result) is
            ([], _) -> (result, looped_env)
            (_, ReturnValue _) -> (result, looped_env)
            (_, Error _) -> (result, looped_env)
            ([expr, .. as rest], _) ->
                (evalExpression expr looped_env)
                |> \(new_result, new_env) -> loop rest (new_result, new_env)

    loop expressions (nullObject, env)

evalExpression : Expression, Environment -> (Object, Environment)
evalExpression = \expression, env ->
    when expression is
        Integer i -> (Integer i, env)
        Boolean b -> (if b then trueObject else falseObject, env)
        Prefix "!" expr ->
            (evaluatedExpr, env2) = evalExpression expr env
            when evaluatedExpr is
                Boolean b -> (if b then falseObject else trueObject, env2)
                _ -> (falseObject, env2)

        Prefix "-" expr ->
            (evaluatedExpr, env2) = evalExpression expr env
            when evaluatedExpr is
                Error _ -> (evaluatedExpr, env2)
                Integer i -> (Integer (-i), env2)
                _ -> (Error "unknown operator: -$(AST.typeOf expr)", env2)

        Infix left operator right ->
            (leftExpr, env2) = evalExpression left env

            when leftExpr is
                Error _ -> (leftExpr, env2)
                _ ->
                    (rightExpr, env3) = evalExpression right env2

                    when rightExpr is
                        Error _ -> (rightExpr, env3)
                        _ ->
                            when (leftExpr, rightExpr) is
                                (Integer leftValue, Integer rightValue) ->
                                    when operator is
                                        "+" -> (Integer (leftValue + rightValue), env3)
                                        "-" -> (Integer (leftValue - rightValue), env3)
                                        "*" -> (Integer (leftValue * rightValue), env3)
                                        "/" -> (Integer (leftValue // rightValue), env3)
                                        "<" -> (Boolean (leftValue < rightValue), env3)
                                        ">" -> (Boolean (leftValue > rightValue), env3)
                                        "==" -> (Boolean (leftValue == rightValue), env3)
                                        "!=" -> (Boolean (leftValue != rightValue), env3)
                                        _ -> (nullObject, env3)

                                (Boolean leftValue, Boolean rightValue) ->
                                    when operator is
                                        "==" -> (Boolean (leftValue == rightValue), env3)
                                        "!=" -> (Boolean (leftValue != rightValue), env3)
                                        _ -> (Error "unknown operator: Boolean $(operator) Boolean", env3)

                                _ -> (Error "type mismatch: $(AST.typeOf left) $(operator) $(AST.typeOf right)", env3)

        If condition consequence alternative ->
            (conditionExpr, env2) = evalExpression condition env

            when conditionExpr is
                Error _ -> (conditionExpr, env2)
                _ ->
                    if isTruthy conditionExpr then
                        evalExpressions consequence env2
                    else
                        when alternative is
                            WithElse block -> evalExpressions block env2
                            NoElse -> (nullObject, env2)

        Let (Identifier ident) expr ->
            (evaluatedExpr, env2) = evalExpression expr env
            when evaluatedExpr is
                Error _ -> (evaluatedExpr, env2)
                _ -> (evaluatedExpr, Environment.set env2 ident evaluatedExpr)

        Identifier ident ->
            when Environment.get env ident is
                Ok value -> (value, env)
                Err _ -> (Error "identifier not found: $(ident)", env)

        Return expr ->
            (evaluatedExpr, env2) = evalExpression expr env
            when evaluatedExpr is
                Error _ -> (evaluatedExpr, env2)
                _ -> (ReturnValue evaluatedExpr, env2)

        _ -> (nullObject, env)

isTruthy : Object -> Bool
isTruthy = \object ->
    when object is
        Null -> Bool.false
        Boolean b -> b
        _ -> Bool.true

# Test Eval Integer Expression
expect
    [
        { input: "5;", expected: Integer 5 },
        { input: "10;", expected: Integer 10 },
        { input: "-5;", expected: Integer -5 },
        { input: "-10;", expected: Integer -10 },
        { input: "5 + 5 + 5 + 5 - 10;", expected: Integer 10 },
        { input: "2 * 2 * 2 * 2 * 2;", expected: Integer 32 },
        { input: "-50 + 100 + -50;", expected: Integer 0 },
        { input: "5 * 2 + 10;", expected: Integer 20 },
        { input: "5 + 2 * 10;", expected: Integer 25 },
        { input: "20 + 2 * -10;", expected: Integer 0 },
        { input: "50 / 2 * 2 + 10;", expected: Integer 60 },
        { input: "2 * (5 + 10);", expected: Integer 30 },
        { input: "3 * 3 * 3 + 10;", expected: Integer 37 },
        { input: "3 * (3 * 3) + 10;", expected: Integer 37 },
        { input: "(5 + 10 * 2 + 15 / 3) * 2 + -10;", expected: Integer 50 },
    ]
    |> List.all \test ->
        (_parser, program) =
            test.input
            |> Lexer.new
            |> Parser.new
            |> Parser.parseProgram

        (eval program Environment.new).0 == test.expected

# Test Eval Boolean Expression
expect
    [
        { input: "true;", expected: Boolean Bool.true },
        { input: "false;", expected: Boolean Bool.false },
        { input: "1 < 2;", expected: Boolean Bool.true },
        { input: "1 > 2;", expected: Boolean Bool.false },
        { input: "1 < 1;", expected: Boolean Bool.false },
        { input: "1 > 1;", expected: Boolean Bool.false },
        { input: "1 == 1;", expected: Boolean Bool.true },
        { input: "1 != 1;", expected: Boolean Bool.false },
        { input: "1 == 2;", expected: Boolean Bool.false },
        { input: "1 != 2;", expected: Boolean Bool.true },
        { input: "true == true;", expected: Boolean Bool.true },
        { input: "false == false;", expected: Boolean Bool.true },
        { input: "true == false;", expected: Boolean Bool.false },
        { input: "true != false;", expected: Boolean Bool.true },
        { input: "false != true;", expected: Boolean Bool.true },
        { input: "(1 < 2) == true;", expected: Boolean Bool.true },
        { input: "(1 < 2) == false;", expected: Boolean Bool.false },
        { input: "(1 > 2) == true;", expected: Boolean Bool.false },
        { input: "(1 > 2) == false;", expected: Boolean Bool.true },
    ]
    |> List.all \test ->
        (_parser, program) =
            test.input
            |> Lexer.new
            |> Parser.new
            |> Parser.parseProgram

        (eval program Environment.new).0 == test.expected

# Test Bang Operator
expect
    [
        { input: "!true;", expected: Boolean Bool.false },
        { input: "!false;", expected: Boolean Bool.true },
        { input: "!5;", expected: Boolean Bool.false },
        { input: "!!true;", expected: Boolean Bool.true },
        { input: "!!false;", expected: Boolean Bool.false },
        { input: "!!5;", expected: Boolean Bool.true },
    ]
    |> List.all \test ->
        (_parser, program) =
            test.input
            |> Lexer.new
            |> Parser.new
            |> Parser.parseProgram

        (eval program Environment.new).0 == test.expected

# Test If Else Expression
expect
    [
        { input: "if (true) { 10 }", expected: Integer 10 },
        { input: "if (false) { 10 }", expected: nullObject },
        { input: "if (1) { 10 }", expected: Integer 10 },
        { input: "if (1 < 2) { 10 }", expected: Integer 10 },
        { input: "if (1 > 2) { 10 }", expected: nullObject },
        { input: "if (1 > 2) { 10 } else { 20 }", expected: Integer 20 },
        { input: "if (1 < 2) { 10 } else { 20 }", expected: Integer 10 },
    ]
    |> List.all \test ->
        (_parser, program) =
            test.input
            |> Lexer.new
            |> Parser.new
            |> Parser.parseProgram

        (eval program Environment.new).0 == test.expected

# Test Return Statement
expect
    [
        { input: "return 10;", expected: Integer 10 },
        { input: "return 10; 9;", expected: Integer 10 },
        { input: "return 2 * 5; 9;", expected: Integer 10 },
        { input: "9; return 2 * 5; 9;", expected: Integer 10 },
        { input: "if (10 > 1) { if (10 > 1) { return 10; return 11; } return 1; }", expected: Integer 10 },
    ]
    |> List.all \test ->
        (_parser, program) =
            test.input
            |> Lexer.new
            |> Parser.new
            |> Parser.parseProgram

        (eval program Environment.new).0 == test.expected

# Test Error Handling
expect
    [
        { input: "5 + true;", expected: Error "type mismatch: Integer + Boolean" },
        { input: "5 + true; 5;", expected: Error "type mismatch: Integer + Boolean" },
        { input: "-true;", expected: Error "unknown operator: -Boolean" },
        { input: "true + false;", expected: Error "unknown operator: Boolean + Boolean" },
        { input: "5; true + false; 5", expected: Error "unknown operator: Boolean + Boolean" },
        { input: "if (10 > 1) { true + false; }", expected: Error "unknown operator: Boolean + Boolean" },
        { input: "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }", expected: Error "unknown operator: Boolean + Boolean" },
        { input: "foobar", expected: Error "identifier not found: foobar" },
    ]
    |> List.all \test ->
        (_parser, program) =
            test.input
            |> Lexer.new
            |> Parser.new
            |> Parser.parseProgram

        (eval program Environment.new).0 == test.expected

# Test Let Statements
expect
    [
        { input: "let a = 5; a;", expected: Integer 5 },
        { input: "let a = 5 * 5; a;", expected: Integer 25 },
        { input: "let a = 5; let b = a; b;", expected: Integer 5 },
        { input: "let a = 5; let b = a; let c = a + b + 5; c;", expected: Integer 15 },
    ]
    |> List.all \test ->
        (_parser, program) =
            test.input
            |> Lexer.new
            |> Parser.new
            |> Parser.parseProgram

        (eval program Environment.new).0 == test.expected
