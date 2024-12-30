app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import pf.Stdout
import Lexer

main =
    Stdout.line! "Hi there, from inside a Roc app. 🎉"

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
