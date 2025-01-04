module [Program, Statement, Expression, addStatement, toStr]

Operator : Str
Ident : [Identifier Str]

Expression : [
    Identifier Str,
    Integer I64,
    Prefix Operator Expression,
    Infix Expression Operator Expression,
    Boolean Bool,
]

Statement : [
    Let Ident, # expression is identifier, but we can't do Let Identifier, cause Roc doesn't like it
    Return,
    ExpressionStatement Expression,
]

Program : List Statement

addStatement : Program, Statement -> Program
addStatement = \program, statement ->
    List.append program statement

toStr : Program -> Str
toStr = \program ->
    program
    |> List.map statementToStr
    |> Str.joinWith ""

statementToStr : Statement -> Str
statementToStr = \statement ->
    when statement is
        Let (Identifier ident) -> "let $(ident) = ;"
        Return -> "return ;"
        ExpressionStatement expr -> expressionToStr expr

expressionToStr : Expression -> Str
expressionToStr = \expression ->
    when expression is
        Identifier ident -> ident
        Integer i -> Num.toStr i
        Prefix op expr -> "($(op)$(expressionToStr expr))"
        Infix left op right -> "($(expressionToStr left) $(op) $(expressionToStr right))"
        Boolean b -> if b then "true" else "false"
