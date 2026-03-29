open Token

type lexer = {
  input : string;
  position : int;
  read_position : int;
  ch : char;
}

let rec init (s : string) : lexer = read_char { input = s; position = 0; read_position = 0; ch = '\000' }

and read_char (l : lexer) : lexer =
  { l with ch = peek_char l; position = l.read_position; read_position = l.read_position + 1 }

and peek_char (l : lexer) : char =
  if l.read_position >= String.length l.input then
    '\000'
  else
    String.get l.input l.read_position

let rec next_token (l : lexer) : lexer * Token.token =
  let pos = l.position in
  match l.ch with
  | '=' ->
      if peek_char l = '=' then
        (read_char (read_char l), Token.init ~pos Eq "==")
      else if peek_char l = '>' then
        (read_char (read_char l), Token.init ~pos FatArrow "=>")
      else
        (read_char l, Token.init ~pos Assign "=")
  | '!' ->
      if peek_char l = '=' then
        (read_char (read_char l), Token.init ~pos NotEq "!=")
      else
        (read_char l, Token.init ~pos Bang "!")
  | '+' -> (read_char l, Token.init ~pos Plus "+")
  | '-' ->
      if peek_char l = '>' then
        (read_char (read_char l), Token.init ~pos Arrow "->")
      else
        (read_char l, Token.init ~pos Minus "-")
  | '*' -> (read_char l, Token.init ~pos Asterisk "*")
  | '/' -> (read_char l, Token.init ~pos Slash "/")
  | '<' ->
      if peek_char l = '=' then
        (read_char (read_char l), Token.init ~pos Le "<=")
      else
        (read_char l, Token.init ~pos Lt "<")
  | '>' ->
      if peek_char l = '=' then
        (read_char (read_char l), Token.init ~pos Ge ">=")
      else
        (read_char l, Token.init ~pos Gt ">")
  | ';' -> (read_char l, Token.init ~pos Semicolon ";")
  | ':' -> (read_char l, Token.init ~pos Colon ":")
  | '(' -> (read_char l, Token.init ~pos LParen "(")
  | ')' -> (read_char l, Token.init ~pos RParen ")")
  | '{' -> (read_char l, Token.init ~pos LBrace "{")
  | '}' -> (read_char l, Token.init ~pos RBrace "}")
  | '[' -> (read_char l, Token.init ~pos LBracket "[")
  | ']' -> (read_char l, Token.init ~pos RBracket "]")
  | ',' -> (read_char l, Token.init ~pos Comma ",")
  | '|' ->
      if peek_char l = '|' then
        (read_char (read_char l), Token.init ~pos PipePipe "||")
      else
        (read_char l, Token.init ~pos Pipe "|")
  | '&' ->
      if peek_char l = '&' then
        (read_char (read_char l), Token.init ~pos AmpAmp "&&")
      else
        (read_char l, Token.init ~pos Ampersand "&")
  | '%' -> (read_char l, Token.init ~pos Percent "%")
  | '.' ->
      (* Check for ... (spread operator) *)
      if peek_char l = '.' then
        let l2 = read_char l in
        if peek_char l2 = '.' then
          (* ... spread token *)
          (read_char (read_char l2), Token.init ~pos Spread "...")
        else
          (* .. not a valid token *)
          (read_char l2, Token.init ~pos Illegal "..")
        (* Only emit Dot if NOT followed by digit (which would be a float) *)
      else if is_digit (peek_char l) then
        (read_char l, Token.init ~pos Illegal ".")
      else
        (read_char l, Token.init ~pos Dot ".")
  | '#' -> next_token (fst (read_until (read_char l) (fun c -> c <> '\n' && c <> '\000')))
  | '"' ->
      let l2, lit = read_string (read_char l) in
      (read_char l2, Token.init ~pos String lit)
  | ' ' | '\t' | '\n' | '\r' -> next_token (read_char l)
  | '\000' -> (read_char l, Token.init ~pos EOF "")
  | _ ->
      if is_letter l.ch then
        let l2, lit = read_identifier l in
        (* allow trailing ? or ! as identifier suffix (e.g. hungry?, panic!) *)
        let l3, lit =
          if l2.ch = '?' then
            (read_char l2, lit ^ "?")
          else if l2.ch = '!' && peek_char l2 <> '=' then
            (read_char l2, lit ^ "!")
          else
            (l2, lit)
        in
        let tt = Token.lookup_ident lit in
        (l3, Token.init ~pos tt lit)
      else if is_digit l.ch then
        let l2, lit = read_number l in
        if is_float l2 then
          let l4, lit2 = read_number (read_char l2) in
          (l4, Token.init ~pos Float (lit ^ "." ^ lit2))
        else
          (l2, Token.init ~pos Int lit)
      else
        (read_char l, Token.init ~pos Illegal (String.make 1 l.ch))

and read_identifier (l : lexer) : lexer * string = read_until l is_ident_char
and read_number (l : lexer) : lexer * string = read_until l is_digit
and read_string (l : lexer) : lexer * string = read_until l (fun c -> c <> '"' && c <> '\000')

and read_until (l : lexer) (f : char -> bool) : lexer * string =
  let start = l.position in
  let rec loop (ll : lexer) =
    if f ll.ch then
      loop (read_char ll)
    else
      (ll, String.sub ll.input start (ll.position - start))
  in

  loop l

and is_letter (c : char) : bool = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c = '_'
and is_digit (c : char) : bool = '0' <= c && c <= '9'
and is_ident_char (c : char) : bool = is_letter c || is_digit c
and is_float (l : lexer) : bool = l.ch = '.' && is_digit (peek_char l)

let lex (i : string) : Token.token list =
  let rec loop (l : lexer) (ts : Token.token list) =
    match ts with
    | { token_type = EOF; literal = ""; _ } :: _ -> List.rev ts
    | _ ->
        let l2, t = next_token l in
        loop l2 (t :: ts)
  in
  loop (init i) []

let%test "test_lexer" =
  let input =
    "
    let five = 5;
    let ten = 10;

    fn add(x, y) = {
        x + y;
    }

    # this is a comment

    let result = add(five, ten);
    !-/*5;
    5 < 10 > 5;

    if (5 < 10) {
        return true;
    } else {
        return false;
    }

    # this is another a comment

    10 == 10;
    10 != 9;
    \"foobar\"
    \"Hello, World!\"
    [1, 2];
    {\"foo\": \"bar\", 1: 2};
    3.14;
  "
  in
  let expected =
    [
      Token.init Let "let";
      Token.init Ident "five";
      Token.init Assign "=";
      Token.init Int "5";
      Token.init Semicolon ";";
      Token.init Let "let";
      Token.init Ident "ten";
      Token.init Assign "=";
      Token.init Int "10";
      Token.init Semicolon ";";
      Token.init Function "fn";
      Token.init Ident "add";
      Token.init LParen "(";
      Token.init Ident "x";
      Token.init Comma ",";
      Token.init Ident "y";
      Token.init RParen ")";
      Token.init Assign "=";
      Token.init LBrace "{";
      Token.init Ident "x";
      Token.init Plus "+";
      Token.init Ident "y";
      Token.init Semicolon ";";
      Token.init RBrace "}";
      Token.init Let "let";
      Token.init Ident "result";
      Token.init Assign "=";
      Token.init Ident "add";
      Token.init LParen "(";
      Token.init Ident "five";
      Token.init Comma ",";
      Token.init Ident "ten";
      Token.init RParen ")";
      Token.init Semicolon ";";
      Token.init Bang "!";
      Token.init Minus "-";
      Token.init Slash "/";
      Token.init Asterisk "*";
      Token.init Int "5";
      Token.init Semicolon ";";
      Token.init Int "5";
      Token.init Lt "<";
      Token.init Int "10";
      Token.init Gt ">";
      Token.init Int "5";
      Token.init Semicolon ";";
      Token.init If "if";
      Token.init LParen "(";
      Token.init Int "5";
      Token.init Lt "<";
      Token.init Int "10";
      Token.init RParen ")";
      Token.init LBrace "{";
      Token.init Return "return";
      Token.init True "true";
      Token.init Semicolon ";";
      Token.init RBrace "}";
      Token.init Else "else";
      Token.init LBrace "{";
      Token.init Return "return";
      Token.init False "false";
      Token.init Semicolon ";";
      Token.init RBrace "}";
      Token.init Int "10";
      Token.init Eq "==";
      Token.init Int "10";
      Token.init Semicolon ";";
      Token.init Int "10";
      Token.init NotEq "!=";
      Token.init Int "9";
      Token.init Semicolon ";";
      Token.init String "foobar";
      Token.init String "Hello, World!";
      Token.init LBracket "[";
      Token.init Int "1";
      Token.init Comma ",";
      Token.init Int "2";
      Token.init RBracket "]";
      Token.init Semicolon ";";
      Token.init LBrace "{";
      Token.init String "foo";
      Token.init Colon ":";
      Token.init String "bar";
      Token.init Comma ",";
      Token.init Int "1";
      Token.init Colon ":";
      Token.init Int "2";
      Token.init RBrace "}";
      Token.init Semicolon ";";
      Token.init Float "3.14";
      Token.init Semicolon ";";
      Token.init EOF "";
    ]
  in
  let actual = lex input in
  List.length actual = List.length expected && List.for_all2 Token.equal_ignoring_pos actual expected

let%test "alias and shape lex as dedicated keywords" =
  match lex "alias Name = Int\nshape HasName = { name: Str }\n" with
  | { token_type = Alias; _ }
    :: { token_type = Ident; literal = "Name"; _ }
    :: { token_type = Assign; _ }
    :: { token_type = Ident; literal = "Int"; _ }
    :: { token_type = Shape; _ }
    :: { token_type = Ident; literal = "HasName"; _ }
    :: _ ->
      true
  | _ -> false

let%test "identifiers with digits" =
  let input = "let foo5 = 5; let x2y = foo5" in
  let tokens = lex input in
  let idents = List.filter (fun t -> t.Token.token_type = Token.Ident) tokens in
  (* foo5 appears twice (definition and use), x2y appears once *)
  List.length idents = 3
  && (List.nth idents 0).literal = "foo5"
  && (List.nth idents 1).literal = "x2y"
  && (List.nth idents 2).literal = "foo5"

let%test "arrow token" =
  let input = "(x: Int) -> Int" in
  let tokens = lex input in
  List.exists (fun t -> t.Token.token_type = Token.Arrow) tokens

let%test "fat arrow token" =
  let input = "() => value" in
  let tokens = lex input in
  List.exists (fun t -> t.Token.token_type = Token.FatArrow) tokens

let%test "less-equal token" =
  let input = "1 <= 2" in
  let tokens = lex input in
  List.exists (fun t -> t.Token.token_type = Token.Le && t.Token.literal = "<=") tokens

let%test "greater-equal token" =
  let input = "2 >= 1" in
  let tokens = lex input in
  List.exists (fun t -> t.Token.token_type = Token.Ge && t.Token.literal = ">=") tokens

let%test "pipe token for union types" =
  let input = "(x: Int | Str) -> Bool" in
  let tokens = lex input in
  List.exists (fun t -> t.Token.token_type = Token.Pipe) tokens

let%test "trait keyword" =
  let input = "trait Show[a] = { fn show(x: a) -> Str }" in
  let tokens = lex input in
  List.exists (fun t -> t.Token.token_type = Token.Trait && t.Token.literal = "trait") tokens

let%test "impl keyword" =
  let input = "impl Show[Int] = { }" in
  let tokens = lex input in
  List.exists (fun t -> t.Token.token_type = Token.Impl && t.Token.literal = "impl") tokens

let%test "derive keyword" =
  let input = "enum Color = { Red } derive Eq, Show" in
  let tokens = lex input in
  List.exists (fun t -> t.Token.token_type = Token.Derive && t.Token.literal = "derive") tokens

let%test "all trait keywords together" =
  let input = "trait Eq[a] = { fn eq(x: a, y: a) -> Bool } impl Eq[Int] = { } enum Color = { Red } derive Show" in
  let tokens = lex input in
  let has_trait = List.exists (fun t -> t.Token.token_type = Token.Trait) tokens in
  let has_impl = List.exists (fun t -> t.Token.token_type = Token.Impl) tokens in
  let has_derive = List.exists (fun t -> t.Token.token_type = Token.Derive) tokens in
  has_trait && has_impl && has_derive

(* Phase 4.4: Records - Lexer tests *)
let%test "spread token - three dots" =
  let input = "{ ...record, x: 5 }" in
  let tokens = lex input in
  List.exists (fun t -> t.Token.token_type = Token.Spread && t.Token.literal = "...") tokens

let%test "type keyword" =
  let input = "type Point = { x: Int, y: Int }" in
  let tokens = lex input in
  List.exists (fun t -> t.Token.token_type = Token.Type && t.Token.literal = "type") tokens

let%test "dot vs spread distinction" =
  let input = "record.x ... record2" in
  let tokens = lex input in
  let has_dot = List.exists (fun t -> t.Token.token_type = Token.Dot && t.Token.literal = ".") tokens in
  let has_spread = List.exists (fun t -> t.Token.token_type = Token.Spread && t.Token.literal = "...") tokens in
  has_dot && has_spread

let%test "illegal double dot advances lexer" =
  let l0 = init ".." in
  let l1, tok = next_token l0 in
  tok.Token.token_type = Token.Illegal && tok.Token.literal = ".." && l1.position > l0.position

let%test "dot before digit is illegal but lexer advances" =
  let l0 = init ".1" in
  let l1, tok1 = next_token l0 in
  let _l2, tok2 = next_token l1 in
  tok1.Token.token_type = Token.Illegal
  && tok1.Token.literal = "."
  && l1.position > l0.position
  && tok2.Token.token_type = Token.Int
  && tok2.Token.literal = "1"

let%test "generic illegal character advances lexer" =
  let l0 = init "@" in
  let l1, tok = next_token l0 in
  tok.Token.token_type = Token.Illegal && tok.Token.literal = "@" && l1.position > l0.position

(* Phase 1a: New vNext tokens *)
let%test "ampersand token" =
  let tokens = lex "Show & Eq" in
  List.exists (fun t -> t.Token.token_type = Token.Ampersand && t.Token.literal = "&") tokens

let%test "ampamp token" =
  let tokens = lex "x && y" in
  List.exists (fun t -> t.Token.token_type = Token.AmpAmp && t.Token.literal = "&&") tokens

let%test "pipepipe token" =
  let tokens = lex "x || y" in
  List.exists (fun t -> t.Token.token_type = Token.PipePipe && t.Token.literal = "||") tokens

let%test "percent token" =
  let tokens = lex "x % 3" in
  List.exists (fun t -> t.Token.token_type = Token.Percent && t.Token.literal = "%") tokens

let%test "override keyword" =
  let tokens = lex "override fn foo()" in
  List.exists (fun t -> t.Token.token_type = Token.Override && t.Token.literal = "override") tokens

let%test "case keyword" =
  let tokens = lex "case Foo.Bar:" in
  List.exists (fun t -> t.Token.token_type = Token.Case && t.Token.literal = "case") tokens

let%test "identifier with question mark suffix" =
  let tokens = lex "hungry?" in
  match tokens with
  | [ { token_type = Token.Ident; literal = "hungry?"; _ }; { token_type = Token.EOF; _ } ] -> true
  | _ -> false

let%test "identifier with bang suffix" =
  let tokens = lex "panic!" in
  match tokens with
  | [ { token_type = Token.Ident; literal = "panic!"; _ }; { token_type = Token.EOF; _ } ] -> true
  | _ -> false

let%test "bang suffix does not absorb not-equal" =
  (* foo!= should lex as Ident "foo", NotEq "!=" *)
  let tokens = lex "foo!=" in
  match tokens with
  | [
   { token_type = Token.Ident; literal = "foo"; _ };
   { token_type = Token.NotEq; literal = "!="; _ };
   { token_type = Token.EOF; _ };
  ] ->
      true
  | _ -> false

let%test "pipe vs pipepipe distinction" =
  let tokens_single = lex "a | b" in
  let tokens_double = lex "a || b" in
  let has_pipe = List.exists (fun t -> t.Token.token_type = Token.Pipe) tokens_single in
  let has_pipepipe = List.exists (fun t -> t.Token.token_type = Token.PipePipe) tokens_double in
  let no_pipepipe_in_single = not (List.exists (fun t -> t.Token.token_type = Token.PipePipe) tokens_single) in
  has_pipe && has_pipepipe && no_pipepipe_in_single

let%test "amp vs ampamp distinction" =
  let tokens_single = lex "a & b" in
  let tokens_double = lex "a && b" in
  let has_amp = List.exists (fun t -> t.Token.token_type = Token.Ampersand) tokens_single in
  let has_ampamp = List.exists (fun t -> t.Token.token_type = Token.AmpAmp) tokens_double in
  has_amp && has_ampamp
