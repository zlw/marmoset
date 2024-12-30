module [Lexer, new, readChar, nextToken]

import Token exposing [Token]

Lexer : {
    input : List U8,
    position : U64, # current position in input (points to current char)
    readPosition : U64, # current reading position in input (after current char)
    ch : U8, # current char under examination
}

new : Str -> Lexer
new = \s ->
    { input: Str.toUtf8 s, position: 0, readPosition: 0, ch: 0 }

readChar : Lexer -> Lexer
readChar = \lexer ->
    if lexer.readPosition >= List.len lexer.input then
        { lexer & ch: 0, position: lexer.readPosition, readPosition: lexer.readPosition + 1 }
    else
        when List.get lexer.input lexer.readPosition is
            Ok c -> { lexer & ch: c, position: lexer.readPosition, readPosition: lexer.readPosition + 1 }
            Err _ -> crash "Lexer is unable to read character"

nextToken : (Lexer, List Token) -> (Lexer, List Token)
nextToken = \(lexer, tokens) ->
    token =
        when lexer.ch is
            '=' -> Token.new Assign "="
            ';' -> Token.new Semicolon ";"
            '(' -> Token.new LParen "("
            ')' -> Token.new RParen ")"
            ',' -> Token.new Comma ","
            '+' -> Token.new Plus "+"
            '{' -> Token.new LBrace "{"
            '}' -> Token.new RBrace "}"
            0 -> Token.new EOF ""
            _ -> crash "Lexer is unable to parse character"

    (readChar lexer, List.append tokens token)
