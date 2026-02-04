type token = {
  token_type : token_type;
  literal : string;
  pos : int; (* byte offset in source *)
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
  | Arrow (* -> for return type annotations *)
  | FatArrow (* => for effect markers *)
  | Pipe (* | for union types *)
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

let init ?(pos = 0) t l = { token_type = t; literal = l; pos }

(* Equality ignoring position (for testing) *)
let equal_ignoring_pos t1 t2 = t1.token_type = t2.token_type && t1.literal = t2.literal

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
