module [new, nextToken, parseProgram]

import Lexer exposing [Lexer]
import Token exposing [Token]
import AST exposing [Program, Statement, Identifier, addStatement]

Parser : {
    lexer : Lexer,
    currToken : Token,
    peekToken : Token,
}

new : Lexer -> Parser
new = \lexer ->
    { lexer: lexer, currToken: Token.new Illegal "", peekToken: Token.new Illegal "" }
    |> nextToken
    |> nextToken

nextToken : Parser -> Parser
nextToken = \parser ->
    currToken = parser.peekToken
    (lexer, peekToken) = Lexer.nextToken parser.lexer
    { parser & lexer, currToken, peekToken }

currTokenIs : Parser, Token.TokenType -> Bool
currTokenIs = \parser, t -> parser.currToken.type == t

peekTokenIs : Parser, Token.TokenType -> Bool
peekTokenIs = \parser, t -> parser.peekToken.type == t

expectPeek : Parser, Token.TokenType -> (Parser, Bool)
expectPeek = \parser, t ->
    if peekTokenIs parser t then
        (nextToken parser, Bool.true)
    else
        (parser, Bool.false)

parseProgram : Parser -> Program
parseProgram = \parser ->
    loop : Parser, Program -> Program
    loop = \looped_parser, program ->
        when looped_parser.currToken.type is
            EOF -> program
            _ ->
                when parseStatement looped_parser is
                    Ok (new_parser, stmt) ->
                        loop new_parser (addStatement program stmt)

                    Err (UnknownStatement new_parser) -> loop new_parser program

    loop parser []

parseStatement : Parser -> Result (Parser, Statement) [UnknownStatement Parser]
parseStatement = \parser ->
    when parser.currToken.type is
        Let ->
            when parseLetStatement parser is
                Ok (new_parser, stmt) -> Ok ((new_parser, stmt))
                Err (NotLet new_parser) -> Err (UnknownStatement new_parser)

        _ -> Err (UnknownStatement parser)

parseLetStatement : Parser -> Result (Parser, [Let Identifier]) [NotLet Parser]
parseLetStatement = \parser ->
    (parser2, isIdent) = expectPeek parser Ident
    if !isIdent then
        Err (NotLet parser)
    else
        (parser3, isAssign) = expectPeek parser2 Assign
        if !isAssign then
            Err (NotLet parser2)
        else
            loop = \looped_parser ->
                if currTokenIs looped_parser Semicolon then
                    nextToken looped_parser
                else
                    loop (nextToken looped_parser)

            Ok (loop parser3, Let parser2.currToken.literal)
