module [Program, Statement, Identifier, addStatement]

Statement : [
    Let Identifier,
    Return,
]

Program : List Statement

Identifier : Str

addStatement : Program, Statement -> Program
addStatement = \program, statement ->
    List.append program statement
