module [lex]

import Token exposing [Token, TokenType]

Lexer : {
    input : List U8,
    position : U64, # current position in input (points to current char)
    readPosition : U64, # current reading position in input (after current char)
    ch : U8, # current char under examination
}

lex : Str -> List Token
lex = \input ->
    loop : Lexer, List Token -> List Token
    loop = \lexer, tokens ->
        if List.last (tokens) == Ok ({ type: EOF, literal: "" }) then
            tokens
        else
            (new_lexer, new_tokens) = Lexer.nextToken (skipWhitespace lexer, tokens)
            loop new_lexer new_tokens

    loop (Lexer.readChar (Lexer.new input)) []

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

peekChar : Lexer -> U8
peekChar = \lexer ->
    if lexer.readPosition >= List.len lexer.input then
        0
    else
        when List.get lexer.input lexer.readPosition is
            Ok c -> c
            Err _ -> crash "Lexer is unable to read character"

nextToken : (Lexer, List Token) -> (Lexer, List Token)
nextToken = \(lexer, tokens) ->
    (new_lexer, token) =
        when lexer.ch is
            '=' ->
                when peekChar lexer is
                    '=' -> (readChar (readChar lexer), Token.new Eq "==")
                    _ -> (readChar lexer, Token.new Assign "=")

            '+' -> (readChar lexer, Token.new Plus "+")
            '-' -> (readChar lexer, Token.new Minus "-")
            '!' ->
                when peekChar lexer is
                    '=' -> (readChar (readChar lexer), Token.new NotEq "!=")
                    _ -> (readChar lexer, Token.new Bang "!")

            '*' -> (readChar lexer, Token.new Asterisk "*")
            '/' -> (readChar lexer, Token.new Slash "/")
            '<' -> (readChar lexer, Token.new Lt "<")
            '>' -> (readChar lexer, Token.new Gt ">")
            ';' -> (readChar lexer, Token.new Semicolon ";")
            '(' -> (readChar lexer, Token.new LParen "(")
            ')' -> (readChar lexer, Token.new RParen ")")
            ',' -> (readChar lexer, Token.new Comma ",")
            '{' -> (readChar lexer, Token.new LBrace "{")
            '}' -> (readChar lexer, Token.new RBrace "}")
            0 -> (readChar lexer, Token.new EOF "")
            _ ->
                if isLetter (lexer.ch) then
                    (lexer2, ident) = readIdentifier lexer
                    (lexer2, Token.new (lookupIdent ident) ident)
                else if isDigit (lexer.ch) then
                    (lexer2, num) = readNumber lexer
                    (lexer2, Token.new Int num)
                else
                    when Str.fromUtf8 [lexer.ch] is
                        Ok ch -> (lexer, Token.new Illegal ch)
                        Err _ -> crash "Lexer is unable to read character"

    (new_lexer, List.append tokens token)

skipWhitespace : Lexer -> Lexer
skipWhitespace = \lexer ->
    when lexer.ch is
        ' ' -> skipWhitespace (readChar lexer)
        '\t' -> skipWhitespace (readChar lexer)
        '\n' -> skipWhitespace (readChar lexer)
        '\r' -> skipWhitespace (readChar lexer)
        _ -> lexer

readIdentifier : Lexer -> (Lexer, Str)
readIdentifier = \lexer ->
    readUntil lexer isLetter

readNumber : Lexer -> (Lexer, Str)
readNumber = \lexer ->
    readUntil lexer isDigit

lookupIdent : Str -> TokenType
lookupIdent = \ident ->
    when ident is
        "fn" -> Function
        "let" -> Let
        "true" -> True
        "false" -> False
        "if" -> If
        "else" -> Else
        "return" -> Return
        _ -> Ident

readUntil : Lexer, (U8 -> Bool) -> (Lexer, Str)
readUntil = \lexer, func ->
    start = lexer.position

    loop : Lexer, U64 -> (Lexer, Str)
    loop = \looped_lexer, len ->
        if func (looped_lexer.ch) then
            loop (readChar looped_lexer) (len + 1)
        else
            when List.sublist looped_lexer.input { start: start, len: len } |> Str.fromUtf8 is
                Ok str -> (looped_lexer, str)
                Err _ -> crash "Lexer is unable to read string"

    loop lexer 0

isLetter : U8 -> Bool
isLetter = \ch ->
    ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_'

isDigit : U8 -> Bool
isDigit = \ch ->
    '0' <= ch && ch <= '9'
