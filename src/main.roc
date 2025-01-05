app [main] { cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import Lexer
import REPL
import Parser
import AST

main = REPL.start

# Chapter 1.2: The Lexer
expect
    input = "=+(){},;"

    Lexer.lex input
    == [
        { type: Assign, literal: "=" },
        { type: Plus, literal: "+" },
        { type: LParen, literal: "(" },
        { type: RParen, literal: ")" },
        { type: LBrace, literal: "{" },
        { type: RBrace, literal: "}" },
        { type: Comma, literal: "," },
        { type: Semicolon, literal: ";" },
        { type: EOF, literal: "" },
    ]

# Chapter 1.2: The Lexer
expect
    input =
        """
        let five = 5;
        let ten = 10;

        let add = fn(x, y) {
            x + y;
        };

        let result = add(five, ten);
        """

    Lexer.lex input
    == [
        { type: Let, literal: "let" },
        { type: Ident, literal: "five" },
        { type: Assign, literal: "=" },
        { type: Int, literal: "5" },
        { type: Semicolon, literal: ";" },
        { type: Let, literal: "let" },
        { type: Ident, literal: "ten" },
        { type: Assign, literal: "=" },
        { type: Int, literal: "10" },
        { type: Semicolon, literal: ";" },
        { type: Let, literal: "let" },
        { type: Ident, literal: "add" },
        { type: Assign, literal: "=" },
        { type: Function, literal: "fn" },
        { type: LParen, literal: "(" },
        { type: Ident, literal: "x" },
        { type: Comma, literal: "," },
        { type: Ident, literal: "y" },
        { type: RParen, literal: ")" },
        { type: LBrace, literal: "{" },
        { type: Ident, literal: "x" },
        { type: Plus, literal: "+" },
        { type: Ident, literal: "y" },
        { type: Semicolon, literal: ";" },
        { type: RBrace, literal: "}" },
        { type: Semicolon, literal: ";" },
        { type: Let, literal: "let" },
        { type: Ident, literal: "result" },
        { type: Assign, literal: "=" },
        { type: Ident, literal: "add" },
        { type: LParen, literal: "(" },
        { type: Ident, literal: "five" },
        { type: Comma, literal: "," },
        { type: Ident, literal: "ten" },
        { type: RParen, literal: ")" },
        { type: Semicolon, literal: ";" },
        { type: EOF, literal: "" },
    ]

# Chapter 1.3: Extending our Token set and Lexer
expect
    input =
        """
        let five = 5;
        let ten = 10;

        let add = fn(x, y) {
            x + y;
        };

        let result = add(five, ten);
        !-/*5;
        5 < 10 > 5;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 10;
        10 != 9;
        """

    Lexer.lex input
    == [
        { type: Let, literal: "let" },
        { type: Ident, literal: "five" },
        { type: Assign, literal: "=" },
        { type: Int, literal: "5" },
        { type: Semicolon, literal: ";" },
        { type: Let, literal: "let" },
        { type: Ident, literal: "ten" },
        { type: Assign, literal: "=" },
        { type: Int, literal: "10" },
        { type: Semicolon, literal: ";" },
        { type: Let, literal: "let" },
        { type: Ident, literal: "add" },
        { type: Assign, literal: "=" },
        { type: Function, literal: "fn" },
        { type: LParen, literal: "(" },
        { type: Ident, literal: "x" },
        { type: Comma, literal: "," },
        { type: Ident, literal: "y" },
        { type: RParen, literal: ")" },
        { type: LBrace, literal: "{" },
        { type: Ident, literal: "x" },
        { type: Plus, literal: "+" },
        { type: Ident, literal: "y" },
        { type: Semicolon, literal: ";" },
        { type: RBrace, literal: "}" },
        { type: Semicolon, literal: ";" },
        { type: Let, literal: "let" },
        { type: Ident, literal: "result" },
        { type: Assign, literal: "=" },
        { type: Ident, literal: "add" },
        { type: LParen, literal: "(" },
        { type: Ident, literal: "five" },
        { type: Comma, literal: "," },
        { type: Ident, literal: "ten" },
        { type: RParen, literal: ")" },
        { type: Semicolon, literal: ";" },
        { type: Bang, literal: "!" },
        { type: Minus, literal: "-" },
        { type: Slash, literal: "/" },
        { type: Asterisk, literal: "*" },
        { type: Int, literal: "5" },
        { type: Semicolon, literal: ";" },
        { type: Int, literal: "5" },
        { type: Lt, literal: "<" },
        { type: Int, literal: "10" },
        { type: Gt, literal: ">" },
        { type: Int, literal: "5" },
        { type: Semicolon, literal: ";" },
        { type: If, literal: "if" },
        { type: LParen, literal: "(" },
        { type: Int, literal: "5" },
        { type: Lt, literal: "<" },
        { type: Int, literal: "10" },
        { type: RParen, literal: ")" },
        { type: LBrace, literal: "{" },
        { type: Return, literal: "return" },
        { type: True, literal: "true" },
        { type: Semicolon, literal: ";" },
        { type: RBrace, literal: "}" },
        { type: Else, literal: "else" },
        { type: LBrace, literal: "{" },
        { type: Return, literal: "return" },
        { type: False, literal: "false" },
        { type: Semicolon, literal: ";" },
        { type: RBrace, literal: "}" },
        { type: Int, literal: "10" },
        { type: Eq, literal: "==" },
        { type: Int, literal: "10" },
        { type: Semicolon, literal: ";" },
        { type: Int, literal: "10" },
        { type: NotEq, literal: "!=" },
        { type: Int, literal: "9" },
        { type: Semicolon, literal: ";" },
        { type: EOF, literal: "" },
    ]

# Chapter 2.4: Parser's first steps: parsing let statements
expect
    input =
        """
        let x = 5;
        let y = 10;
        let foobar = 838383;
        """
    (parser, program) =
        Lexer.new input
        |> Parser.new
        |> Parser.parseProgram

    match = List.map2 program ["x", "y", "foobar"] \statement, expected ->
        when statement is
            Let actual -> actual == expected
            _ -> Bool.false

    (expectNoErrors parser) && (expectStatementsCount program 3) && match == [Bool.true, Bool.true, Bool.true]

# Chapter 2.4: Parser's first steps: parsing let statements
# expect
#    input =
#        """
#        let x 5;
#        let = 10;
#        let 838383;
#        """

#    (parser, _) =
#        Lexer.new input
#        |> Parser.new
#        |> Parser.parseProgram

#    parser.errors
#    == [
#        "expected next token to be Assign, got Int",
#        "expected next token to be Ident, got Assign",
#        "expected next token to be Ident, got Int",
#    ]

# Chapter 2.5 - Parsing return statements
expect
    input =
        """
        return 5;
        return 10;
        return 993322;
        """

    (parser, program) =
        Lexer.new input
        |> Parser.new
        |> Parser.parseProgram

    match = List.all program \statement ->
        when statement is
            Return -> Bool.true
            _ -> Bool.false

    (expectNoErrors parser) && (expectStatementsCount program 3) && match == Bool.true

# Chapter 2.6 - Parsing expressions - Preparing AST
expect
    input = "let myVar = anotherVar;"

    (parser, program) =
        Lexer.new input
        |> Parser.new
        |> Parser.parseProgram

    (expectNoErrors parser) && AST.toStr program == "let myVar = ;"

# Chapter 2.6: Parsing expressions - Identifiers
expect
    input = "foobar;"

    (parser, program) =
        Lexer.new input
        |> Parser.new
        |> Parser.parseProgram

    (expectNoErrors parser)
    &&
    (expectStatementsCount program 1)
    &&
    (expectExpression program (Identifier "foobar"))

# Chapter 2.6 - Parsing expressions - Integer Literals
expect
    input = "5;"

    (parser, program) =
        Lexer.new input
        |> Parser.new
        |> Parser.parseProgram

    (expectNoErrors parser)
    &&
    (expectStatementsCount program 1)
    &&
    (expectExpression program (Integer 5))

# Chapter 2.6 - Parsing expressions - Prefix Operators (Integers)
expect
    [
        { input: "!5;", operator: "!", value: 5 },
        { input: "-15;", operator: "-", value: 15 },
    ]
    |> List.map \test ->
        (parser, program) =
            Lexer.new test.input
            |> Parser.new
            |> Parser.parseProgram

        (expectNoErrors parser)
        &&
        (expectStatementsCount program 1)
        &&
        (expectExpression program (Prefix test.operator (Integer test.value)))
    |> Bool.isEq [Bool.true, Bool.true]

# Chapter 2.8 - Extending the Parser - Prefix Operators (Booleans)
expect
    [
        { input: "!true;", operator: "!", value: Bool.true },
        { input: "!false;", operator: "!", value: Bool.false },
    ]
    |> List.map \test ->
        (parser, program) =
            Lexer.new test.input
            |> Parser.new
            |> Parser.parseProgram

        (expectNoErrors parser)
        &&
        (expectStatementsCount program 1)
        &&
        (expectExpression program (Prefix test.operator (Boolean test.value)))

    |> Bool.isEq [Bool.true, Bool.true]

# Chapter 2.6 - Parsing expressions - Infix Operators (Integers)
expect
    [
        { input: "5 + 5;", leftValue: 5, operator: "+", rightValue: 5 },
        { input: "5 - 5;", leftValue: 5, operator: "-", rightValue: 5 },
        { input: "5 * 5;", leftValue: 5, operator: "*", rightValue: 5 },
        { input: "5 / 5;", leftValue: 5, operator: "/", rightValue: 5 },
        { input: "5 > 5;", leftValue: 5, operator: ">", rightValue: 5 },
        { input: "5 < 5;", leftValue: 5, operator: "<", rightValue: 5 },
        { input: "5 == 5;", leftValue: 5, operator: "==", rightValue: 5 },
        { input: "5 != 5;", leftValue: 5, operator: "!=", rightValue: 5 },
    ]
    |> List.map \test ->
        (parser, program) =
            Lexer.new test.input
            |> Parser.new
            |> Parser.parseProgram

        (expectNoErrors parser)
        &&
        (expectStatementsCount program 1)
        &&
        (expectExpression program (Infix (Integer test.leftValue) test.operator (Integer test.rightValue)))

    |> Bool.isEq [Bool.true, Bool.true, Bool.true, Bool.true, Bool.true, Bool.true, Bool.true, Bool.true]

# Chapter 2.8 - Extendind the Parser - Infix Operators (Booleans)
expect
    [
        { input: "true == true;", leftValue: Bool.true, operator: "==", rightValue: Bool.true },
        { input: "true != false;", leftValue: Bool.true, operator: "!=", rightValue: Bool.false },
        { input: "false == false;", leftValue: Bool.false, operator: "==", rightValue: Bool.false },
    ]
    |> List.map \test ->
        (parser, program) =
            Lexer.new test.input
            |> Parser.new
            |> Parser.parseProgram

        (expectNoErrors parser)
        &&
        (expectStatementsCount program 1)
        &&
        (expectExpression program (Infix (Boolean test.leftValue) test.operator (Boolean test.rightValue)))

    |> Bool.isEq [Bool.true, Bool.true, Bool.true]

# Chapter 2.6 - Parsing expressions - Operator Precedence
expect
    [
        ("-a * b", "((-a) * b)"),
        ("!-a", "(!(-a))"),
        ("a + b + c", "((a + b) + c)"),
        ("a + b - c", "((a + b) - c)"),
        ("a * b * c", "((a * b) * c)"),
        ("a * b / c", "((a * b) / c)"),
        ("a + b / c", "(a + (b / c))"),
        ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
        ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
        ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
        ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
        ("true", "true"),
        ("false", "false"),
        ("3 > 5 == false", "((3 > 5) == false)"),
        ("3 < 5 == true", "((3 < 5) == true)"),
        ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
        ("(5 + 5) * 2", "((5 + 5) * 2)"),
        ("2 / (5 + 5)", "(2 / (5 + 5))"),
        ("(5 + 5) * 2 * (5 + 5)", "(((5 + 5) * 2) * (5 + 5))"),
        ("-(5 + 5)", "(-(5 + 5))"),
        ("!(true == true)", "(!(true == true))"),
        ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
        ("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))"),
        ("add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))"),
    ]
    |> List.all \(input, expected) ->
        (parser, program) =
            Lexer.new input
            |> Parser.new
            |> Parser.parseProgram

        output = AST.toStr program

        (expectNoErrors parser)
        &&
        expected
        == output

    |> Bool.isEq Bool.true

# Chapter 2.8 - Extending Parser - Boolean literals
expect
    [
        ("true;", Bool.true),
        ("false;", Bool.false),
    ]
    |> List.map \(input, expected) ->
        (parser, program) =
            Lexer.new input
            |> Parser.new
            |> Parser.parseProgram

        (expectNoErrors parser)
        &&
        (expectStatementsCount program 1)
        &&
        (expectExpression program (Boolean expected))

    |> Bool.isEq [Bool.true, Bool.true]

# Chapter 2.8 - Extending Parser - If Expressions
expect
    input = "if (x < y) { x }"

    (parser, program) =
        Lexer.new input
        |> Parser.new
        |> Parser.parseProgram

    (expectNoErrors parser)
    &&
    (expectStatementsCount program 1)
    &&
    (expectExpression program (If (Infix (Identifier "x") "<" (Identifier "y")) [Identifier "x"] NoElse))

# Chapter 2.8 - Extending Parser - If Expressions
expect
    input = "if (x < y) { x } else { y }"

    (parser, program) =
        Lexer.new input
        |> Parser.new
        |> Parser.parseProgram

    (expectNoErrors parser)
    &&
    (expectStatementsCount program 1)
    &&
    (expectExpression program (If (Infix (Identifier "x") "<" (Identifier "y")) [Identifier "x"] (WithElse [Identifier "y"])))

# Chapter 2.8 - Extending Parser - Function Literals
expect
    input = "fn(x, y) { x + y; }"

    (parser, program) =
        Lexer.new input
        |> Parser.new
        |> Parser.parseProgram

    (expectNoErrors parser)
    &&
    (expectStatementsCount program 1)
    &&
    (expectExpression program (Function [Identifier "x", Identifier "y"] [Infix (Identifier "x") "+" (Identifier "y")]))

# Chapter 2.8 - Extending Parser - Function Literals
expect
    [
        { input: "fn() {};", expectedParams: [] },
        { input: "fn(x) {};", expectedParams: [Identifier "x"] },
        { input: "fn(foo, bar, baz) {};", expectedParams: [Identifier "foo", Identifier "bar", Identifier "baz"] },
    ]
    |> List.all \test ->
        (parser, program) =
            Lexer.new test.input
            |> Parser.new
            |> Parser.parseProgram

        (expectNoErrors parser)
        &&
        (expectStatementsCount program 1)
        &&
        (expectExpression program (Function test.expectedParams []))

    |> Bool.isEq Bool.true

# Chapter 2.8 - Extending Parser - Function Literals
expect
    input = "add(1, 2 * 3, 4 + 5);"

    (parser, program) =
        Lexer.new input
        |> Parser.new
        |> Parser.parseProgram

    (expectNoErrors parser)
    &&
    (expectStatementsCount program 1)
    &&
    (expectExpression program (Call (Identifier "add") [Integer 1, Infix (Integer 2) "*" (Integer 3), Infix (Integer 4) "+" (Integer 5)]))

# Helpers
expectNoErrors = \parser ->
    (List.len parser.errors) == 0

expectStatementsCount = \program, count ->
    (List.len program) == count

expectExpression = \program, expectation ->
    List.first program == Ok expectation
