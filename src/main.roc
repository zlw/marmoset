app [main] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.17.0/lZFLstMUCUvd5bjnnpYromZJXkQUrdhbva4xdBInicE.tar.br" }

import pf.Stdout
import Token exposing [Token]
import Lexer exposing [Lexer]

main =
    Stdout.line! "Hi there, from inside a Roc app. 🎉"

expect
    Lexer.new "=+(){},;"
    == { input: [61, 43, 40, 41, 123, 125, 44, 59], position: 0, readPosition: 0, ch: 0 }

expect
    Lexer.readChar (Lexer.new "=+(){},;")
    == { input: [61, 43, 40, 41, 123, 125, 44, 59], position: 0, readPosition: 1, ch: 61 }

expect
    str = "=+(){},;"

    expected_tokens : List Token
    expected_tokens = [
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

    loop : Lexer, List Token -> List Token
    loop = \lexer, tokens ->
        if List.last (tokens) == Ok ({ type: EOF, literal: "" }) then
            tokens
        else
            (new_lexer, new_tokens) = Lexer.nextToken (lexer, tokens)
            loop new_lexer new_tokens

    loop (Lexer.readChar (Lexer.new str)) [] == expected_tokens
