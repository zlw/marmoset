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
  match l.ch with
  | '=' ->
      if peek_char l = '=' then
        (read_char (read_char l), Token.init Eq "==")
      else
        (read_char l, Token.init Assign "=")
  | '!' ->
      if peek_char l = '=' then
        (read_char (read_char l), Token.init NotEq "!=")
      else
        (read_char l, Token.init Bang "!")
  | '+' -> (read_char l, Token.init Plus "+")
  | '-' -> (read_char l, Token.init Minus "-")
  | '*' -> (read_char l, Token.init Asterisk "*")
  | '/' -> (read_char l, Token.init Slash "/")
  | '<' -> (read_char l, Token.init Lt "<")
  | '>' -> (read_char l, Token.init Gt ">")
  | ';' -> (read_char l, Token.init Semicolon ";")
  | ':' -> (read_char l, Token.init Colon ":")
  | '(' -> (read_char l, Token.init LParen "(")
  | ')' -> (read_char l, Token.init RParen ")")
  | '{' -> (read_char l, Token.init LBrace "{")
  | '}' -> (read_char l, Token.init RBrace "}")
  | '[' -> (read_char l, Token.init LBracket "[")
  | ']' -> (read_char l, Token.init RBracket "]")
  | ',' -> (read_char l, Token.init Comma ",")
  | '"' ->
      let l2, lit = read_string (read_char l) in
      (read_char l2, Token.init String lit)
  | ' ' | '\t' | '\n' | '\r' -> next_token (read_char l)
  | '\000' -> (read_char l, Token.init EOF "")
  | _ ->
      if is_letter l.ch then
        let l2, lit = read_identifier l in
        let tt = Token.lookup_ident lit in
        (l2, Token.init tt lit)
      else if is_digit l.ch then
        let l2, lit = read_number l in
        if is_float l2 then
          let l4, lit2 = read_number (read_char l2) in
          (l4, Token.init Float (lit ^ "." ^ lit2))
        else
          (l2, Token.init Int lit)
      else
        (l, Token.init Illegal (String.make 1 l.ch))

and read_identifier (l : lexer) : lexer * string = read_until l is_letter
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
and is_float (l : lexer) : bool = l.ch = '.' && is_digit (peek_char l)

let lex (i : string) : Token.token list =
  let rec loop (l : lexer) (ts : Token.token list) =
    match ts with
    | { token_type = EOF; literal = "" } :: _ -> List.rev ts
    | _ ->
        let l2, t = next_token l in
        loop l2 ([ t ] @ ts)
  in
  loop (init i) []

let%test "test_lexer" =
  let input =
    "
    let five = 5;
    let ten = 10;

    let add = fn(x, y) {
        x + y;
    };

    let result = add(five, ten);
    !-/*5;
    5 < 10 > 5;

    if (5 < 10) {
        return true;
    } else {
        return false;
    }

    10 == 10;
    10 != 9;
    \"foobar\"
    \"Hello, World!\"
    [1, 2];
    {\"foo\": \"bar\", 1: 2};
    3.14;
  "
  in
  lex input
  = [
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
      Token.init Let "let";
      Token.init Ident "add";
      Token.init Assign "=";
      Token.init Function "fn";
      Token.init LParen "(";
      Token.init Ident "x";
      Token.init Comma ",";
      Token.init Ident "y";
      Token.init RParen ")";
      Token.init LBrace "{";
      Token.init Ident "x";
      Token.init Plus "+";
      Token.init Ident "y";
      Token.init Semicolon ";";
      Token.init RBrace "}";
      Token.init Semicolon ";";
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
