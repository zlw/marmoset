module [Object, toStr, nullObject, trueObject, falseObject]

Object : [
    Null,
    Integer I64,
    Boolean Bool,
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
