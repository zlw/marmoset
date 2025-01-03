module [new, nextToken, parseProgram]

import Lexer exposing [Lexer]
import Token exposing [Token]
import AST exposing [Program, Statement, Expression, addStatement]

Parser : {
    lexer : Lexer,
    currToken : Token,
    peekToken : Token,
    errors : List Str,
}

prefixParseFns : Dict Token.TokenType (Parser -> Expression)
prefixParseFns = Dict.fromList [
    (Ident, parseIdentifier),
    (Int, parseIntegerLiteral),
]

new : Lexer -> Parser
new = \lexer ->
    { lexer: lexer, currToken: Token.new Illegal "", peekToken: Token.new Illegal "", errors: [] }
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

expectPeek : Parser, Token.TokenType -> Result Parser [PeekError Parser]
expectPeek = \parser, t ->
    if peekTokenIs parser t then
        Ok (nextToken parser)
    else
        Err (PeekError (peekError parser t))

peekError : Parser, Token.TokenType -> Parser
peekError = \parser, t ->
    error = "expected next token to be $(Inspect.toStr t), got $(Inspect.toStr parser.peekToken.type)"
    new_errors = List.append parser.errors error
    { parser & errors: new_errors }

parseProgram : Parser -> (Parser, Program)
parseProgram = \parser ->
    loop : Parser, Program -> (Parser, Program)
    loop = \looped_parser, program ->
        when looped_parser.currToken.type is
            EOF -> (looped_parser, program)
            _ ->
                when parseStatement looped_parser is
                    Ok (new_parser, stmt) -> loop (nextToken new_parser) (addStatement program stmt)
                    Err (UnknownStatement new_parser) -> loop (nextToken new_parser) program

    loop parser []

parseStatement : Parser -> Result (Parser, Statement) [UnknownStatement Parser]
parseStatement = \parser ->
    when parser.currToken.type is
        Let ->
            when parseLetStatement parser is
                Ok (new_parser, stmt) -> Ok ((new_parser, stmt))
                Err (NotLet new_parser) -> Err (UnknownStatement new_parser)

        Return ->
            when parseReturnStatement parser is
                Ok (new_parser, stmt) -> Ok ((new_parser, stmt))
                Err (NotReturn new_parser) -> Err (UnknownStatement new_parser)

        _ ->
            when parseExpressionStatement parser is
                Ok (new_parser, stmt) -> Ok ((new_parser, stmt))
                Err (NotExpressionStatement new_parser) -> Err (UnknownStatement new_parser)

parseLetStatement : Parser -> Result (Parser, [Let [Identifier Str]]) [NotLet Parser]
parseLetStatement = \parser ->
    when expectPeek parser Ident is
        Err (PeekError parser2) -> Err (NotLet parser2)
        Ok parser2 ->
            when expectPeek parser2 Assign is
                Err (PeekError parser3) -> Err (NotLet parser3)
                Ok parser3 ->
                    loop : Parser -> Parser
                    loop = \looped_parser ->
                        if peekTokenIs looped_parser Semicolon then
                            nextToken looped_parser
                        else
                            loop (nextToken looped_parser)

                    Ok (loop (nextToken parser3), Let (Identifier parser2.currToken.literal))

parseReturnStatement : Parser -> Result (Parser, [Return]) []
parseReturnStatement = \parser ->
    parser2 = nextToken parser

    loop : Parser -> Parser
    loop = \looped_parser ->
        if peekTokenIs looped_parser Semicolon then
            nextToken looped_parser
        else
            loop (nextToken looped_parser)

    Ok (loop parser2, Return)

parseExpressionStatement : Parser -> Result (Parser, [ExpressionStatement Expression]) [NotExpressionStatement Parser]
parseExpressionStatement = \parser ->
    when parseExpression parser precLowest is
        Ok expression ->
            stmt = ExpressionStatement expression

            if peekTokenIs parser Semicolon then
                Ok (nextToken parser, stmt)
            else
                Ok (parser, stmt)

        Err (NoPrecRule new_parser) -> Err (NotExpressionStatement new_parser)

Precedence := U8
precLowest = @Precedence 1
precEquals = @Precedence 2
precLessGreater = @Precedence 3
precSum = @Precedence 4
precProduct = @Precedence 5
precPrefix = @Precedence 6
precCall = @Precedence 7

parseExpression : Parser, Precedence -> Result Expression [NoPrecRule Parser]
parseExpression = \parser, _precedence ->
    when Dict.get prefixParseFns parser.currToken.type is
        Ok prefixFn -> Ok (prefixFn parser)
        _ -> Err (NoPrecRule parser)

parseIdentifier : Parser -> [Identifier Str]
parseIdentifier = \parser ->
    Identifier parser.currToken.literal

parseIntegerLiteral : Parser -> [Integer I64]
parseIntegerLiteral = \parser ->
    when Str.toI64 parser.currToken.literal is
        Ok int -> Integer int
        Err InvalidNumStr -> crash "can't parse number from $(parser.currToken.literal)"
