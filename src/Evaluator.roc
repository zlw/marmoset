module [eval]

import Object exposing [Object, trueObject, falseObject, nullObject]
import AST exposing [Program, Expression]
import Parser
import Lexer

eval : Program -> Object
eval = \program ->
    evalExpressions program

evalExpressions : List Expression -> Object
evalExpressions = \expressions ->
    loop : List Expression, Object -> Object
    loop = \exprs, result ->
        when exprs is
            [] -> result
            [expr, .. as rest] -> loop rest (evalExpression expr)

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

        _ -> nullObject

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
