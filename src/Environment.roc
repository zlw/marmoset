module [Environment, new, get, set]

import Object exposing [Object]

# This is pure madness, but all other ways failed
# - I can't use { store: Dict Str Object } because of the bug in type checker
# - I can't use { store: List (Str, Object) } because of the bug in the compiler
Environment : {
    key_store : List Str,
    val_store : List Object,
}

new : Environment
new = { key_store: [], val_store: [] }

get : Environment, Str -> Result Object [KeyNotFound]
get = \env, key ->
    when List.findFirstIndex env.key_store \stored_key -> stored_key == key is
        Ok index ->
            when List.get env.val_store index is
                Ok value -> Ok value
                Err _ -> Err KeyNotFound

        Err _ -> Err KeyNotFound

set : Environment, Str, Object -> Environment
set = \env, key, val ->
    when List.findFirstIndex env.key_store \stored_key -> stored_key == key is
        Ok index ->
            val_store2 = List.replace env.val_store index val
            { env & val_store: val_store2.list }

        Err _ ->
            key_store2 = List.append env.key_store key
            val_store2 = List.append env.val_store val
            { env & key_store: key_store2, val_store: val_store2 }

expect
    new |> get "foo" == Err KeyNotFound

expect
    env = new |> set "foo" (Integer 42) |> set "bar" (Integer 43) |> set "foo" (Integer 44)

    (env |> get "foo" == Ok (Integer 44)) && (env |> get "bar" == Ok (Integer 43))
