open Ast

type value =
  | Null
  | Integer of int64
  | Boolean of bool
  | String of string
  | Return of value
  | Error of string
  | Function of AST.expression list * AST.statement * value Env.env option
  | BuiltinFunction of (value list -> value)
[@@deriving show]

let true_value = Boolean true
let false_value = Boolean false
let null_value = Null
let is_truthy = function Null -> false | Boolean b -> b | _ -> true

let rec to_string = function
  | Null -> "null"
  | Integer i -> Int64.to_string i
  | Boolean b -> string_of_bool b
  | String s -> "\"" ^ s ^ "\""
  | Return v -> to_string v
  | Error s -> "ERROR: " ^ s
  | Function (params, body, _) ->
      Printf.sprintf "fn(%s) { %s }"
        (params |> List.map param_to_string |> String.concat ", ")
        (AST.to_string [ body ])
  | BuiltinFunction _ -> "<builtin function>"

and param_to_string = function AST.Identifier p -> p | _ -> failwith "invalid parameter"

let rec type_of = function
  | Null -> "Null"
  | Integer _ -> "Integer"
  | Boolean _ -> "Boolean"
  | String _ -> "String"
  | Return v -> type_of v
  | Error _ -> "Error"
  | Function _ -> "Function"
  | BuiltinFunction _ -> "BuiltinFunction"

let type_mismatch_error left op right =
  let msg = Printf.sprintf "%s %s %s" (AST.type_of left) op (AST.type_of right) in
  Error ("type mismatch: " ^ msg)

let unknown_infix_operator_error left op right =
  let msg = Printf.sprintf "%s %s %s" (AST.type_of left) op (AST.type_of right) in
  Error ("unknown operator: " ^ msg)

let unknown_prefix_operator_error op right =
  let msg = Printf.sprintf "%s%s" op (AST.type_of right) in
  Error ("unknown operator: " ^ msg)
