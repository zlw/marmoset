let builtin_len args =
  match args with
  | [ Value.String s ] -> Value.Integer (Int64.of_int (String.length s))
  | [ Value.Array a ] -> Value.Integer (Int64.of_int (List.length a))
  | [ _ ] -> Error ("argument to `len` not supported, got " ^ Value.type_of (List.hd args))
  | _ ->
      let msg = Printf.sprintf "wrong number of arguments. got=%d, want=1" (List.length args) in
      Error msg

let builtin_first args =
  match args with
  | [ Value.Array a ] -> ( match a with [] -> Value.Null | _ -> List.hd a)
  | [ _ ] -> Error ("argument to `first` not supported, got " ^ Value.type_of (List.hd args))
  | _ ->
      let msg = Printf.sprintf "wrong number of arguments. got=%d, want=1" (List.length args) in
      Error msg

let builtin_last args =
  match args with
  | [ Value.Array a ] -> ( match a with [] -> Value.Null | _ -> List.hd (List.rev a))
  | [ _ ] -> Error ("argument to `last` not supported, got " ^ Value.type_of (List.hd args))
  | _ ->
      let msg = Printf.sprintf "wrong number of arguments. got=%d, want=1" (List.length args) in
      Error msg

let builtin_rest args =
  match args with
  | [ Value.Array a ] -> ( match a with [] -> Value.Null | _ -> Value.Array (List.tl a))
  | [ _ ] -> Error ("argument to `rest` not supported, got " ^ Value.type_of (List.hd args))
  | _ ->
      let msg = Printf.sprintf "wrong number of arguments. got=%d, want=1" (List.length args) in
      Error msg

let builtin_push args =
  match args with
  | [ Value.Array a; v ] -> Value.Array (a @ [ v ])
  | [ _; _ ] -> Error ("argument to `push` not supported, got " ^ Value.type_of (List.hd args))
  | _ ->
      let msg = Printf.sprintf "wrong number of arguments. got=%d, want=2" (List.length args) in
      Error msg

let builtin_puts args =
  List.iter (fun arg -> print_endline (Value.to_string arg)) args;
  Value.Null

let builtin = function
  | "len" -> Some builtin_len
  | "first" -> Some builtin_first
  | "last" -> Some builtin_last
  | "rest" -> Some builtin_rest
  | "push" -> Some builtin_push
  | "puts" -> Some builtin_puts
  | _ -> None
