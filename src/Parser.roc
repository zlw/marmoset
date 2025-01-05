module [Parser, new, nextToken, parseProgram]

import Lexer exposing [Lexer]
import Token exposing [Token]
import AST exposing [Program, Expression, addStatement]

Parser : {
    lexer : Lexer,
    currToken : Token,
    peekToken : Token,
    errors : List Str,
}

Precedence := U8
precLowest = @Precedence 1
precEquals = @Precedence 2
precLessGreater = @Precedence 3
precSum = @Precedence 4
precProduct = @Precedence 5
precPrefix = @Precedence 6
precCall = @Precedence 7

precedences : Token.TokenType -> Precedence
precedences = \token ->
    when token is
        Eq -> precEquals
        NotEq -> precEquals
        Lt -> precLessGreater
        Gt -> precLessGreater
        Plus -> precSum
        Minus -> precSum
        Slash -> precProduct
        Asterisk -> precProduct
        LParen -> precCall
        _ -> precLowest

precLT : Precedence, Precedence -> Bool
precLT = \@Precedence p1, @Precedence p2 -> p1 < p2

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

noPrefixParseFnError : Parser, Token.TokenType -> Parser
noPrefixParseFnError = \parser, t ->
    error = "no prefix parse function for $(Inspect.toStr t) found"
    new_errors = List.append parser.errors error
    { parser & errors: new_errors }

parseProgram : Parser -> (Parser, Program)
parseProgram = \parser ->
    loop : Parser, Program -> (Parser, Program)
    loop = \looped_parser, program ->
        if currTokenIs looped_parser EOF then
            (looped_parser, program)
        else
            (new_looped_parser, new_program) =
                when parseStatement looped_parser is
                    Ok (new_parser, stmt) -> (new_parser, addStatement program stmt)
                    Err (UnknownStatement new_parser) -> (new_parser, program)

            loop (nextToken new_looped_parser) new_program

    loop parser []

parseStatement : Parser -> Result (Parser, Expression) [UnknownStatement Parser]
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

parseReturnStatement : Parser -> Result (Parser, [Return Expression]) [NotReturn Parser]
parseReturnStatement = \parser ->
    when parseExpression (nextToken parser) precLowest is
        Err (NoPrecRule parser3) -> Err (NotReturn parser3)
        Ok (parser3, expression) ->
            if peekTokenIs parser3 Semicolon then
                Ok (nextToken parser3, Return expression)
            else
                Ok (parser3, Return expression)

parseExpressionStatement : Parser -> Result (Parser, Expression) [NotExpressionStatement Parser]
parseExpressionStatement = \parser ->
    when parseExpression parser precLowest is
        Ok (new_parser, expression) ->
            if peekTokenIs new_parser Semicolon then
                Ok (nextToken new_parser, expression)
            else
                Ok (new_parser, expression)

        Err (NoPrecRule new_parser) -> Err (NotExpressionStatement new_parser)

parseExpression : Parser, Precedence -> Result (Parser, Expression) [NoPrecRule Parser]
parseExpression = \parser, precedence ->
    tokenType = parser.currToken.type

    result =
        # Mimic prefixParseFn
        when tokenType is
            Ident -> Ok (parseIdentifier parser)
            Int -> Ok (parseIntegerLiteral parser)
            Bang -> Ok (parsePrefixExpression parser)
            Minus -> Ok (parsePrefixExpression parser)
            True -> Ok (parseBoolean parser)
            False -> Ok (parseBoolean parser)
            LParen -> Ok (parseGroupedExpression parser)
            If -> Ok (parseIfExpression parser)
            Function -> Ok (parseFunctionLiteral parser)
            _ -> Err (NoPrecRule (noPrefixParseFnError parser tokenType))

    when result is
        Err (NoPrecRule new_parser) -> Err (NoPrecRule new_parser)
        Ok (parser2, leftExp) ->
            loop : Parser, Expression -> (Parser, Expression)
            loop = \looped_parser, left ->
                if !(peekTokenIs looped_parser Semicolon) && (precLT precedence (peekPrecedence looped_parser)) then
                    (new_looped_parser, new_left) =
                        # Mimic infixParseFn
                        when looped_parser.peekToken.type is
                            Plus | Minus | Slash | Asterisk | Eq | NotEq | Lt | Gt ->
                                parseInfixExpression (nextToken looped_parser) left

                            LParen ->
                                parseCallExpression (nextToken looped_parser) left

                            _ -> (looped_parser, left)

                    loop (new_looped_parser) new_left
                else
                    (looped_parser, left)

            Ok (loop parser2 leftExp)

peekPrecedence : Parser -> Precedence
peekPrecedence = \parser -> precedences parser.peekToken.type

currPrecedence : Parser -> Precedence
currPrecedence = \parser -> precedences parser.currToken.type

parseIdentifier : Parser -> (Parser, [Identifier Str])
parseIdentifier = \parser ->
    (parser, Identifier parser.currToken.literal)

parseIntegerLiteral : Parser -> (Parser, [Integer I64])
parseIntegerLiteral = \parser ->
    when Str.toI64 parser.currToken.literal is
        Ok int -> (parser, Integer int)
        Err InvalidNumStr -> crash "can't parse number from $(parser.currToken.literal)"

parsePrefixExpression : Parser -> (Parser, [Prefix Str Expression])
parsePrefixExpression = \parser ->
    operator = parser.currToken.literal
    parser2 = nextToken parser

    when parseExpression parser2 precPrefix is
        Ok (parser3, right) -> (parser3, Prefix operator right)
        Err (NoPrecRule _parser3) -> crash "can't parse prefix expression"

parseInfixExpression : Parser, Expression -> (Parser, [Infix Expression Str Expression])
parseInfixExpression = \parser, left ->
    operator = parser.currToken.literal
    precedence = currPrecedence parser
    parser2 = nextToken parser

    when parseExpression parser2 precedence is
        Ok (parser3, right) -> (parser3, Infix left operator right)
        Err (NoPrecRule _parser3) -> crash "can't parse infix expression"

parseBoolean : Parser -> (Parser, [Boolean Bool])
parseBoolean = \parser ->
    (parser, Boolean (parser.currToken.type == True))

parseGroupedExpression : Parser -> (Parser, Expression)
parseGroupedExpression = \parser ->
    when parseExpression (nextToken parser) precLowest is
        Ok (parser2, expression) ->
            when expectPeek parser2 RParen is
                Ok parser3 -> (parser3, expression)
                Err (PeekError _parser3) -> crash "can't parse grouped expression"

        Err (NoPrecRule _parser2) -> crash "can't parse grouped expression"

parseIfExpression : Parser -> (Parser, [If Expression (List Expression) [NoElse, WithElse (List Expression)]])
parseIfExpression = \parser ->
    when expectPeek parser LParen is
        Err (PeekError _parser2) -> crash "can't parse if expression"
        Ok parser2 ->
            when parseExpression (nextToken parser2) precLowest is
                Err (NoPrecRule _parser3) -> crash "can't parse if expression"
                Ok (parser3, condition) ->
                    when expectPeek parser3 RParen is
                        Err (PeekError _parser4) -> crash "can't parse if expression"
                        Ok parser4 ->
                            when expectPeek parser4 LBrace is
                                Err (PeekError _parser5) -> crash "can't parse if expression"
                                Ok parser5 ->
                                    (parser6, consequence) = parseBlockStatement parser5

                                    if !(peekTokenIs parser6 Else) then
                                        (parser6, If condition consequence NoElse)
                                    else
                                        when expectPeek (nextToken parser6) LBrace is
                                            Err (PeekError _parser7) -> crash "can't parse if expression"
                                            Ok parser7 ->
                                                (parser8, alternative) = parseBlockStatement parser7
                                                (parser8, If condition consequence (WithElse alternative))

parseBlockStatement : Parser -> (Parser, List Expression)
parseBlockStatement = \parser ->
    parser2 = nextToken parser

    loop : Parser, List Expression -> (Parser, List Expression)
    loop = \looped_parser, expressions ->
        if !(currTokenIs looped_parser RBrace) && !(currTokenIs looped_parser EOF) then
            when parseStatement looped_parser is
                Ok (new_looped_parser, expression) -> loop (nextToken new_looped_parser) (List.append expressions expression)
                Err (UnknownStatement new_looped_parser) -> loop (nextToken new_looped_parser) expressions
        else
            (looped_parser, expressions)

    loop parser2 []

parseFunctionLiteral : Parser -> (Parser, [Function (List [Identifier Str]) (List Expression)])
parseFunctionLiteral = \parser ->
    when expectPeek parser LParen is
        Err (PeekError _parser2) -> crash "can't parse function literal"
        Ok parser2 ->
            (parser3, parameters) = parseFunctionParameters parser2

            when expectPeek parser3 LBrace is
                Err (PeekError _parser4) -> crash "can't parse function literal"
                Ok parser4 ->
                    (parser5, body) = parseBlockStatement parser4

                    (parser5, Function parameters body)

parseFunctionParameters : Parser -> (Parser, List [Identifier Str])
parseFunctionParameters = \parser ->
    if peekTokenIs parser RParen then
        (nextToken parser, [])
    else
        parser2 = nextToken parser

        loop : Parser, List [Identifier Str] -> (Parser, List [Identifier Str])
        loop = \looped_parser, identifiers ->
            if peekTokenIs looped_parser Comma then
                new_parser = nextToken (nextToken looped_parser)
                new_identifiers = List.append identifiers (Identifier new_parser.currToken.literal)

                loop new_parser new_identifiers
            else
                when expectPeek looped_parser RParen is
                    Err (PeekError _new_looped_parser) -> crash "can't parse function parameters"
                    Ok new_looped_parser -> (new_looped_parser, identifiers)

        loop parser2 [Identifier parser2.currToken.literal]

parseCallExpression : Parser, Expression -> (Parser, [Call [WithFunction [Function (List [Identifier Str]) (List Expression)], WithIdentifier [Identifier Str]] (List Expression)])
parseCallExpression = \parser, callee ->
    (parser2, arguments) = parseCallArguments parser
    when callee is
        Identifier ident -> (parser2, Call (WithIdentifier (Identifier ident)) arguments)
        Function params body -> (parser2, Call (WithFunction (Function params body)) arguments)
        _ -> crash "can't parse call expression"

parseCallArguments : Parser -> (Parser, List Expression)
parseCallArguments = \parser ->
    if peekTokenIs parser RParen then
        (nextToken parser, [])
    else
        when parseExpression (nextToken parser) precLowest is
            Err (NoPrecRule _parser2) -> crash "can't parse call arguments"
            Ok (parser2, arg) ->
                loop : Parser, List Expression -> (Parser, List Expression)
                loop = \looped_parser, args ->
                    if peekTokenIs looped_parser Comma then
                        when parseExpression (nextToken (nextToken looped_parser)) precLowest is
                            Err (NoPrecRule _new_parser2) -> crash "can't parse call arguments"
                            Ok (new_parser2, new_arg) -> loop new_parser2 (List.append args new_arg)
                    else
                        when expectPeek looped_parser RParen is
                            Err (PeekError _new_parser2) -> crash "can't parse call arguments"
                            Ok new_parser2 -> (new_parser2, args)

                loop parser2 [arg]
