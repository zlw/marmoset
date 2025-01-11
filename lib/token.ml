type token = {
  token_type : token_type;
  literal : string;
}
[@@deriving show]

and token_type =
  | Illegal
  | EOF
  (* Identifiers + literals *)
  | Ident
  | Int
  | Float
  | String
  (* Operators *)
  | Assign
  | Plus
  | Minus
  | Bang
  | Asterisk
  | Slash
  | Lt
  | Gt
  | Eq
  | NotEq
  (* Delimiters *)
  | Comma
  | Semicolon
  | Colon
  | LParen
  | RParen
  | LBrace
  | RBrace
  | LBracket
  | RBracket
  (* Keywords *)
  | Function
  | Let
  | True
  | False
  | If
  | Else
  | Return
[@@deriving show]

let init t l = { token_type = t; literal = l }

let lookup_ident s =
  match s with
  | "fn" -> Function
  | "let" -> Let
  | "true" -> True
  | "false" -> False
  | "if" -> If
  | "else" -> Else
  | "return" -> Return
  | _ -> Ident
