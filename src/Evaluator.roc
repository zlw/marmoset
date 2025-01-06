module [eval]

import Object exposing [Object, trueObject, falseObject, nullObject]
import AST exposing [Program, Expression]
import Parser
import Lexer

eval : Program -> Object
eval = \program ->
    when evalExpressions program is
        ReturnValue returnValue -> returnValue
        returnValue -> returnValue

evalExpressions : List Expression -> Object
evalExpressions = \expressions ->
    loop : List Expression, Object -> Object
    loop = \exprs, result ->
        when (exprs, result) is
            ([], _) -> result
            (_, ReturnValue _) -> result
            ([expr, .. as rest], _) -> loop rest (evalExpression expr)

    loop expressions nullObject

evalExpression : Expression -> Object
evalExpression = \expression ->
    when expression is
        Integer i -> Integer i
        Boolean b -> if b then trueObject else falseObject
        Prefix "!" expr ->
            when evalExpression expr is
                Boolean b -> if b then falseObject else trueObject
                _ -> falseObject

        Prefix "-" expr ->
            when evalExpression expr is
                Integer i -> Integer (-i)
                _ -> nullObject

        Infix left operator right ->
            leftExpr = evalExpression left
            rightExpr = evalExpression right

            when (leftExpr, rightExpr) is
                (Integer leftValue, Integer rightValue) ->
                    when operator is
                        "+" -> Integer (leftValue + rightValue)
                        "-" -> Integer (leftValue - rightValue)
                        "*" -> Integer (leftValue * rightValue)
                        "/" -> Integer (leftValue // rightValue)
                        "<" -> Boolean (leftValue < rightValue)
                        ">" -> Boolean (leftValue > rightValue)
                        "==" -> Boolean (leftValue == rightValue)
                        "!=" -> Boolean (leftValue != rightValue)
                        _ -> nullObject

                (Boolean leftValue, Boolean rightValue) ->
                    when operator is
                        "==" -> Boolean (leftValue == rightValue)
                        "!=" -> Boolean (leftValue != rightValue)
                        _ -> nullObject

                _ -> nullObject

        If condition consequence alternative ->
            conditionExpr = evalExpression condition

            if isTruthy conditionExpr then
                evalExpressions consequence
            else
                when alternative is
                    WithElse block -> evalExpressions block
                    NoElse -> nullObject

        Return expr -> ReturnValue (evalExpression expr)
        _ -> nullObject

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

        eval program == test.expected

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

        eval program == test.expected

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

        eval program == test.expected

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

        eval program == test.expected

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

        eval program == test.expected
