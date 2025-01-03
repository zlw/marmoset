module [Program, Statement, Expression, addStatement, toStr]

Expression : [
    Identifier Str,
    Integer I64,
    Prefix Str Expression,
]

Statement : [
    Let Expression, # expression is identifier, but we can't do Let Identifier, cause Roc doesn't like it
    Return,
    ExpressionStatement Expression,
]

Program : List Statement

addStatement : Program, Statement -> Program
addStatement = \program, statement ->
    List.append program statement

toStr : Program -> Str
toStr = \program ->
    List.map program \statement -> statementToStr statement
    |> Str.joinWith "\n"

statementToStr : Statement -> Str
statementToStr = \statement ->
    when statement is
        Let (Identifier ident) -> "let $(ident) = ;"
        Return -> "return ;"
        ExpressionStatement expr -> expressionToStr expr
        _ -> "unknown statement"

expressionToStr : Expression -> Str
expressionToStr = \expression ->
    when expression is
        Identifier ident -> ident
        Integer i -> Inspect.toStr i
        Prefix op expr -> "($(op)$(expressionToStr expr))"
