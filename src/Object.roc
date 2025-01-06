module [Object, toStr, nullObject, trueObject, falseObject]

import AST exposing [Expression]

Object : [
    Null,
    Integer I64,
    Boolean Bool,
    ReturnValue Object,
    Error Str,
    Function (List [Identifier Str]) (List Expression),
]

trueObject : Object
trueObject = Boolean Bool.true

falseObject : Object
falseObject = Boolean Bool.false

nullObject : Object
nullObject = Null

toStr : Object -> Str
toStr = \object ->
    when object is
        Null -> "null"
        Integer i -> Num.toStr i
        Boolean b -> if b then "true" else "false"
        ReturnValue value -> toStr value
        Error message -> "ERROR: $(message)"
        Function params _ -> "fn($((List.map params \Identifier p -> p) |> Str.joinWith ", ")) { ... }"
