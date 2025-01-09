open Ast

module Hashtbl_printable = struct
  type ('k, 's) t = ('k, 's) Hashtbl.t

  let pp pp_key pp_value ppf values =
    values |> Hashtbl.iter (fun key data -> Stdlib.Format.fprintf ppf "@[<1>%a: %a@]@." pp_key key pp_value data)
end

type value =
  | Null
  | Integer of int64
  | Boolean of bool
  | String of string
  | Array of value list
  | Hash of (value, value) Hashtbl_printable.t
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
  | Array vs -> Printf.sprintf "[%s]" (vs |> List.map to_string |> String.concat ", ")
  | Hash h ->
      Printf.sprintf "{%s}"
        (Hashtbl.fold (fun k v acc -> Printf.sprintf "%s: %s, %s" (to_string k) (to_string v) acc) h "")
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
  | Array _ -> "Array"
  | Hash _ -> "Hash"
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

let init () : value = Hash (Hashtbl.create 2)

let get (h : value) (k : value) : value option =
  match h with
  | Hash h -> (
      match k with
      | Null | Integer _ | Boolean _ | String _ -> Hashtbl.find_opt h k
      | _ -> Some (Error ("unusable as hash key: " ^ type_of k)))
  | _ -> Some (Error ("unusable as hash: " ^ type_of h))

let set (h : value) (k : value) (v : value) =
  match h with
  | Hash h' -> (
      match k with
      | Null | Integer _ | Boolean _ | String _ ->
          Hashtbl.replace h' k v;
          h
      | _ -> Error ("unusable as hash key: " ^ type_of k))
  | _ -> Error ("unusable as hash: " ^ type_of h)

let%test "test_hashing" =
  let h1 = Hashtbl.create 2 in
  let str1 = String "Hello World" in
  let str2 = String "Hello World" in
  let str3 = String "Goodbye World" in

  Hashtbl.replace h1 str1 (Integer 1L);
  Hashtbl.find_opt h1 str2 = Some (Integer 1L) && Hashtbl.find_opt h1 str3 = None

let%test "test_hashing" =
  let h1 = Hashtbl.create 2 in
  let str1 = String "Hello World" in
  let str2 = String "Hello World" in

  Hashtbl.replace h1 str1 (Integer 1L);
  Hashtbl.remove h1 str1;
  Hashtbl.find_opt h1 str1 = None && Hashtbl.find_opt h1 str2 = None
