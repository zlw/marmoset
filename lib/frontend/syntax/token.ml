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
  | Dot
  | Spread (* ... for record spread/row variables *)
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
  | Is (* is keyword for type checking *)
  | Enum (* enum keyword for algebraic data types *)
  | Match (* match keyword for pattern matching *)
  | Trait (* trait keyword for trait definitions *)
  | Impl (* impl keyword for trait implementations *)
  | Derive (* derive keyword for automatic trait derivation *)
  | Type (* type keyword for type aliases *)
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
  | "is" -> Is
  | "enum" -> Enum
  | "match" -> Match
  | "trait" -> Trait
  | "impl" -> Impl
  | "derive" -> Derive
  | "type" -> Type
  | _ -> Ident
