module [Program, Expression, addStatement, toStr]

Expression : [
    Let [Identifier Str], # statement
    Return, # statement
    Identifier Str,
    Integer I64,
    Prefix Operator Expression,
    Infix Expression Operator Expression,
    Boolean Bool,
    If Expression (List Expression) [NoElse, WithElse (List Expression)],
    Function (List [Identifier Str]) (List Expression),
    Call [WithFunction [Function (List [Identifier Str]) (List Expression)], WithIdentifier [Identifier Str]] (List Expression),
]

Operator : Str

Program : List Expression

addStatement : Program, Expression -> Program
addStatement = \program, expression ->
    List.append program expression

toStr : Program -> Str
toStr = \program ->
    program
    |> List.map expressionToStr
    |> Str.joinWith ""

expressionToStr : Expression -> Str
expressionToStr = \expression ->
    when expression is
        Let (Identifier identifier) -> "let $(identifier) = ;"
        Return -> "return ;"
        Identifier ident -> ident
        Integer i -> Num.toStr i
        Prefix op expr -> "($(op)$(expressionToStr expr))"
        Infix left op right -> "($(expressionToStr left) $(op) $(expressionToStr right))"
        Boolean b -> if b then "true" else "false"
        If cond consequence NoElse -> "if $(expressionToStr cond) $(blockToStr consequence)"
        If cond consequence (WithElse alternative) -> "if $(expressionToStr cond) $(blockToStr consequence) else $(blockToStr alternative)"
        Function params body -> functionToStr params body
        Call (WithFunction (Function params body)) args -> "$(functionToStr params body)$(argsToStr args)"
        Call (WithIdentifier (Identifier ident)) args -> "$(ident)$(argsToStr args)"

# This is exaclty the same as the toStr, but we can't reuse because of bug in the Roc compiler
blockToStr : List Expression -> Str
blockToStr = \block ->
    block
    |> List.map expressionToStr
    |> Str.joinWith ""

functionToStr : List [Identifier Str], List Expression -> Str
functionToStr = \params, body ->
    "fn ($((List.map params \Identifier p -> p) |> Str.joinWith ", ")) $(blockToStr body)"

argsToStr : List Expression -> Str
argsToStr = \args ->
    "($(args |> List.map expressionToStr |> Str.joinWith ", "))"
