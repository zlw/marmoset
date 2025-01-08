let builtin_len args =
  match args with
  | [ Value.String s ] -> Value.Integer (Int64.of_int (String.length s))
  | [ _ ] -> Error ("argument to `len` not supported, got " ^ Value.type_of (List.hd args))
  | _ ->
      let msg = Printf.sprintf "wrong number of arguments. got=%d, want=1" (List.length args) in
      Error msg

let builtin = function "len" -> Some builtin_len | _ -> None
