module [Program, Expression, addStatement, toStr]

Expression : [
    Let Str,
    Return,
    Identifier Str,
    Integer I64,
    Prefix Operator Expression,
    Infix Expression Operator Expression,
    Boolean Bool,
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
        Let identifiers -> "let $(identifiers) = ;"
        Return -> "return ;"
        Identifier ident -> ident
        Integer i -> Num.toStr i
        Prefix op expr -> "($(op)$(expressionToStr expr))"
        Infix left op right -> "($(expressionToStr left) $(op) $(expressionToStr right))"
        Boolean b -> if b then "true" else "false"
