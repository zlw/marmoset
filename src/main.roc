app [main] { cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import Lexer
import REPL
import Parser

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
    (_, program) =
        Lexer.new input
        |> Parser.new
        |> Parser.parseProgram

    match = List.map2 program ["x", "y", "foobar"] \Let actual, expected ->
        actual == expected

    match == [Bool.true, Bool.true, Bool.true]

# Chapter 2.4: Parser's first steps: parsing let statements
expect
    input =
        """
        let x 5;
        let = 10;
        let 838383;
        """

    (parser, program) =
        Lexer.new input
        |> Parser.new
        |> Parser.parseProgram

    (List.len program)
    == 0
    && parser.errors
    == [
        "expected next token to be Assign, got Int",
        "expected next token to be Ident, got Assign",
        "expected next token to be Ident, got Int",
    ]
