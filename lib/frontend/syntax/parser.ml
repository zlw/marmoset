open Ast

let ( let* ) res f = Result.bind res f

(* Helpers to create AST nodes with position and ID from parser *)
let mk_expr id pos kind = AST.{ id; expr = kind; pos; end_pos = pos; file_id = None }
let mk_stmt pos kind = AST.{ stmt = kind; pos; end_pos = pos; file_id = None }

type parser = {
  lexer : Lexer.lexer;
  curr_token : Token.token;
  peek_token : Token.token;
  errors : errors;
  next_id : int;
}

and errors = string list

(* Helper to get fresh ID and increment counter *)
let fresh_id p = (p.next_id, { p with next_id = p.next_id + 1 })

type precedence = int

let prec_lowest = 1
let prec_equals = 2
let prec_less_greater = 3
let prec_sum = 4
let prec_product = 5
let prec_prefix = 6
let prec_call = 7
let prec_index = 8

let precedences = function
  | Token.Eq | Token.NotEq | Token.Is -> prec_equals
  | Token.Lt | Token.Gt -> prec_less_greater
  | Token.Plus | Token.Minus -> prec_sum
  | Token.Asterisk | Token.Slash -> prec_product
  | Token.LParen -> prec_call
  | Token.LBracket -> prec_index
  | Token.Dot -> prec_index (* Same precedence as indexing *)
  | _ -> prec_lowest

let peek_precedence (p : parser) : precedence = precedences p.peek_token.token_type
let curr_precedence (p : parser) : precedence = precedences p.curr_token.token_type

let next_token (p : parser) : parser =
  let curr_token = p.peek_token in
  let lexer, peek_token = Lexer.next_token p.lexer in
  { p with lexer; curr_token; peek_token }

let init (l : Lexer.lexer) : parser =
  { lexer = l; curr_token = Token.init Illegal ""; peek_token = Token.init Illegal ""; errors = []; next_id = 0 }
  |> next_token
  |> next_token

let curr_token_is (p : parser) (t : Token.token_type) : bool = p.curr_token.token_type = t
let peek_token_is (p : parser) (t : Token.token_type) : bool = p.peek_token.token_type = t
let add_error (p : parser) (msg : string) : parser = { p with errors = [ msg ] @ p.errors }

let skip (p : parser) (t : Token.token_type) : parser =
  if peek_token_is p t then
    next_token p
  else
    p

let peek_error (p : parser) (tt : Token.token_type) : parser =
  let msg =
    Printf.sprintf "expected next token to be %s, got %s instead" (Token.show_token_type tt)
      (Token.show_token_type p.peek_token.token_type)
  in
  add_error p msg

let expect_peek (p : parser) (tt : Token.token_type) : (parser, parser) result =
  if peek_token_is p tt then
    Ok (next_token p)
  else
    Error (peek_error p tt)

let no_prefix_parse_fn_error (p : parser) (t : Token.token_type) : parser =
  let msg = Printf.sprintf "syntax error, unexpected %s found" (Token.show_token_type t) in
  add_error p msg

let rec parse_program (p : parser) : (parser * AST.program, parser) result =
  let rec loop (lp : parser) (prog : AST.program) =
    if curr_token_is lp Token.EOF then
      Ok (lp, List.rev prog)
    else
      let* lp2, prog2 = parse_statement lp in
      let lp3 =
        match prog2.stmt with
        | AST.TypeAlias _ ->
            (* parse_type_expr leaves us at the token after the alias body *)
            if curr_token_is lp2 Token.Semicolon then
              next_token lp2
            else
              lp2
        | _ -> next_token lp2
      in
      loop lp3 ([ prog2 ] @ prog)
  in

  loop p []

and parse_statement (p : parser) : (parser * AST.statement, parser) result =
  match p.curr_token.token_type with
  | Token.Let -> parse_let_statement p
  | Token.Return -> parse_return_statement p
  | Token.Enum -> parse_enum_definition p
  | Token.Trait -> parse_trait_definition p
  | Token.Impl -> parse_impl_definition p
  | Token.Derive -> parse_derive_definition p
  | Token.Type -> parse_type_alias p
  | _ -> parse_expression_statement p

(* Phase 2: Type expression parsing *)

(* Parse a single type atom (not union) *)
and parse_type_atom (p : parser) : (parser * AST.type_expr, parser) result =
  if curr_token_is p Token.Ident then
    let ident = p.curr_token.literal in
    let p2 = next_token p in
    (* Check for generic application: list[int], map[string, int], etc. *)
    if curr_token_is p2 Token.LBracket then
      let* p3, type_args = parse_type_expr_list (next_token p2) in
      if curr_token_is p3 Token.RBracket then
        Ok (next_token p3, AST.TApp (ident, type_args))
      else
        Error (peek_error p3 Token.RBracket)
    else
      Ok (p2, AST.TCon ident)
  else if curr_token_is p Token.Function then
    (* Function type: fn(int, string) -> bool *)
    let* p2 = expect_peek p Token.LParen in
    let* p3, param_types = parse_type_expr_list (next_token p2) in
    let* p4 = expect_peek p3 Token.RParen in
    let* p5 = expect_peek p4 Token.Arrow in
    let* p6, return_type = parse_type_expr (next_token p5) in
    Ok (p6, AST.TArrow (param_types, return_type))
  else if curr_token_is p Token.LParen then
    (* Parenthesized type or function type: (int | string) or (int, string) -> bool *)
    let* p2, first = parse_type_expr (next_token p) in
    if curr_token_is p2 Token.Comma then
      (* Multiple params: (int, string) -> bool *)
      let rec collect_params lp params =
        let* lp2, param_type = parse_type_expr (next_token lp) in
        let new_params = params @ [ param_type ] in
        if curr_token_is lp2 Token.Comma then
          collect_params lp2 new_params
        else if curr_token_is lp2 Token.RParen then
          Ok (next_token lp2, new_params)
        else
          Error (peek_error lp2 Token.RParen)
      in
      let* p3, params = collect_params p2 [ first ] in
      let* p4 = expect_peek p3 Token.Arrow in
      let* p5, return_type = parse_type_expr (next_token p4) in
      Ok (p5, AST.TArrow (params, return_type))
    else if curr_token_is p2 Token.RParen then
      (* Single type in parens: (int) or (int | string) *)
      let p3 = next_token p2 in
      if curr_token_is p3 Token.Arrow then
        (* Single-param function: (int) -> bool *)
        let* p4 = expect_peek p3 Token.Arrow in
        let* p5, return_type = parse_type_expr (next_token p4) in
        Ok (p5, AST.TArrow ([ first ], return_type))
      else
        (* Just grouping: (int) or (int | string) *)
        Ok (p3, first)
    else
      Error (peek_error p2 Token.RParen)
  else if curr_token_is p Token.LBrace then
    (* Record type: { x: int, y: string } or { x: int, ...r } *)
    parse_record_type p
  else
    Error (no_prefix_parse_fn_error p p.curr_token.token_type)

(* Parse a type expression, including unions: int | string | bool *)
and parse_type_expr (p : parser) : (parser * AST.type_expr, parser) result =
  let* p2, first_type = parse_type_atom p in
  (* Check for union: type | type | type ... *)
  if curr_token_is p2 Token.Pipe then
    let rec collect_union_members lp members =
      let* lp2, next_type = parse_type_atom (next_token lp) in
      let new_members = members @ [ next_type ] in
      if curr_token_is lp2 Token.Pipe then
        collect_union_members lp2 new_members
      else
        Ok (lp2, AST.TUnion new_members)
    in
    collect_union_members p2 [ first_type ]
  else
    Ok (p2, first_type)

and parse_type_expr_list (p : parser) : (parser * AST.type_expr list, parser) result =
  if curr_token_is p Token.RParen || curr_token_is p Token.RBracket then
    Ok (p, [])
  else
    let rec loop (lp : parser) (types : AST.type_expr list) =
      let* lp2, te = parse_type_expr lp in
      if curr_token_is lp2 Token.Comma then
        loop (next_token lp2) ([ te ] @ types)
      else
        Ok (lp2, List.rev ([ te ] @ types))
    in
    loop p []

and parse_trait_constraint (p : parser) : (parser * string list, parser) result =
  (* Parse trait constraints: show, eq, show + eq, etc. *)
  let rec loop (lp : parser) (traits : string list) =
    if curr_token_is lp Token.Ident then
      let trait = lp.curr_token.literal in
      let lp2 = next_token lp in
      if curr_token_is lp2 Token.Plus then
        loop (next_token lp2) ([ trait ] @ traits)
      else
        Ok (lp2, List.rev ([ trait ] @ traits))
    else
      Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
  in
  loop p []

and parse_generic_params (p : parser) : (parser * AST.generic_param list option, parser) result =
  (* Parse generic parameters: [a], [a: show], [a: show + eq, b: eq], etc. *)
  (* Check if the NEXT token is [ (generics start after fn keyword) *)
  if peek_token_is p Token.LBracket then
    let p_bracket = next_token p in
    (* Move to [ *)
    let rec loop (lp : parser) (params : AST.generic_param list) =
      if not (curr_token_is lp Token.Ident) then
        Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
      else
        let name = lp.curr_token.literal in
        let lp3 = next_token lp in
        let* lp4, constraints =
          if curr_token_is lp3 Token.Colon then
            parse_trait_constraint (next_token lp3)
          else
            Ok (lp3, [])
        in
        let param = AST.{ name; constraints } in
        if curr_token_is lp4 Token.Comma then
          loop (next_token lp4) ([ param ] @ params)
        else if curr_token_is lp4 Token.RBracket then
          Ok (lp4, List.rev ([ param ] @ params))
        else
          Error (peek_error lp4 Token.RBracket)
    in
    let* p2, params = loop (next_token p_bracket) [] in
    Ok (p2, Some params)
  else
    Ok (p, None)

(* Phase 4.5: Parse record type: { x: int, y: string, ...r } *)
and parse_record_type (p : parser) : (parser * AST.type_expr, parser) result =
  (* Current token is { *)
  let p2 = next_token p in

  (* Parse fields and optional row variable *)
  let rec parse_fields lp fields =
    if curr_token_is lp Token.RBrace then
      (* Empty record or end of fields: { } *)
      Ok (next_token lp, AST.TRecord (List.rev fields, None))
    else if curr_token_is lp Token.Spread then
      (* Row variable: ...r *)
      let lp2 = next_token lp in
      let* lp3, row_var = parse_type_expr lp2 in
      let* lp4 =
        if curr_token_is lp3 Token.RBrace then
          Ok lp3
        else
          expect_peek lp3 Token.RBrace
      in
      Ok (next_token lp4, AST.TRecord (List.rev fields, Some row_var))
    else if curr_token_is lp Token.Ident then
      (* Field: field_name: type *)
      let field_name = lp.curr_token.literal in
      let* lp2 = expect_peek lp Token.Colon in
      let lp2 = next_token lp2 in
      let* lp3, field_type = parse_type_expr lp2 in
      let field = AST.{ field_name; field_type } in
      if curr_token_is lp3 Token.Comma then
        (* More fields *)
        parse_fields (next_token lp3) (field :: fields)
      else
        (* End of fields - continue to check for } or ...r *)
        parse_fields lp3 (field :: fields)
    else
      Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
  in
  parse_fields p2 []

and parse_let_statement (p : parser) : (parser * AST.statement, parser) result =
  let pos = p.curr_token.pos in
  let* p2 = expect_peek p Token.Ident in
  let name = p2.curr_token.literal in
  (* Phase 2: Parse optional type annotation *)
  let* p3, type_annotation =
    if peek_token_is p2 Token.Colon then
      let p_colon = next_token p2 in
      (* Move to : *)
      let p_type_start = next_token p_colon in
      (* Move to type identifier *)
      let* p_type_end, te = parse_type_expr p_type_start in
      (* parse_type_expr returns parser positioned one past the type expression *)
      (* For `int`, it's positioned at = *)
      Ok (p_type_end, Some te)
    else
      (* No colon, so p2 is still at the identifier *)
      (* We want to leave p2 as-is so that expect_peek can find = *)
      Ok (p2, None)
  in
  (* p3 is positioned appropriately for the next step *)
  (* For annotated: p3 is at the token after the type (should be =) *)
  (* For non-annotated: p3 is at the identifier, so expect_peek will find = *)
  let* p4 =
    match type_annotation with
    | Some _ ->
        (* After type parsing, we're positioned at/past the type *)
        if curr_token_is p3 Token.Assign then
          Ok p3 (* Already at = *)
        else if peek_token_is p3 Token.Assign then
          Ok (next_token p3)
          (* Move to = *)
        else
          Error (peek_error p3 Token.Assign)
    | None ->
        (* No annotation, so expect_peek to find = *)
        expect_peek p3 Token.Assign
  in
  let* p5, expr = parse_expression (next_token p4) prec_lowest in
  let p6 = skip p5 Token.Semicolon in
  Ok (p6, mk_stmt pos (AST.Let { name; value = expr; type_annotation }))

and parse_return_statement (p : parser) : (parser * AST.statement, parser) result =
  let pos = p.curr_token.pos in
  let* p2, expr = parse_expression (next_token p) prec_lowest in
  let p3 = skip p2 Token.Semicolon in
  Ok (p3, mk_stmt pos (AST.Return expr))

(* Phase 4.2: Parse enum definition *)
and parse_enum_definition (p : parser) : (parser * AST.statement, parser) result =
  (* Current token is 'enum' *)
  let pos = p.curr_token.pos in
  let* p2 = expect_peek p Token.Ident in
  let name = p2.curr_token.literal in

  (* Parse optional type parameters: [a, b] *)
  let* p3, type_params =
    if peek_token_is p2 Token.LBracket then
      parse_type_param_list (next_token (next_token p2))
    else
      Ok (next_token p2, [])
  in

  (* Expect opening brace *)
  let* p4 =
    if curr_token_is p3 Token.LBrace then
      Ok p3
    else
      expect_peek p3 Token.LBrace
  in

  (* Parse variants *)
  let* p5, variants = parse_variant_list (next_token p4) in

  (* Expect closing brace *)
  let* p6 =
    if curr_token_is p5 Token.RBrace then
      Ok p5
    else
      expect_peek p5 Token.RBrace
  in

  Ok (p6, mk_stmt pos (AST.EnumDef { name; type_params; variants }))

and parse_type_param_list (p : parser) : (parser * string list, parser) result =
  let rec loop lp params =
    if curr_token_is lp Token.Ident then
      let param = lp.curr_token.literal in
      let lp2 = next_token lp in
      if curr_token_is lp2 Token.Comma then
        loop (next_token lp2) (params @ [ param ])
      else if curr_token_is lp2 Token.RBracket then
        Ok (next_token lp2, params @ [ param ])
      else
        Error (peek_error lp2 Token.RBracket)
    else if curr_token_is lp Token.RBracket then
      Ok (next_token lp, params)
    else
      Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
  in
  loop p []

and parse_variant_list (p : parser) : (parser * AST.variant_def list, parser) result =
  let rec loop lp variants =
    if curr_token_is lp Token.RBrace then
      Ok (lp, List.rev variants)
    else if curr_token_is lp Token.Ident then
      let variant_name = lp.curr_token.literal in
      let lp2 = next_token lp in
      (* Check for variant data: some(a) *)
      let* lp3, variant_fields =
        if curr_token_is lp2 Token.LParen then
          let* lp3, fields = parse_type_expr_list (next_token lp2) in
          let* lp4 =
            if curr_token_is lp3 Token.RParen then
              Ok lp3
            else
              expect_peek lp3 Token.RParen
          in
          Ok (next_token lp4, fields)
        else
          Ok (lp2, [])
      in
      let variant = AST.{ variant_name; variant_fields } in
      loop lp3 (variant :: variants)
    else
      Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
  in
  loop p []

(* Phase 4.4: Type alias parsing *)
and parse_type_alias (p : parser) : (parser * AST.statement, parser) result =
  (* Current token is 'type' *)
  let pos = p.curr_token.pos in
  let* p2 = expect_peek p Token.Ident in
  let alias_name = p2.curr_token.literal in

  (* Parse optional type parameters: [a, b] *)
  let* p3, alias_type_params =
    if peek_token_is p2 Token.LBracket then
      parse_type_param_list (next_token (next_token p2))
    else
      Ok (next_token p2, [])
  in

  (* Expect: = *)
  let* p4 =
    if curr_token_is p3 Token.Assign then
      Ok p3
    else
      expect_peek p3 Token.Assign
  in

  (* Parse type expression *)
  let* p5, alias_body = parse_type_expr (next_token p4) in

  Ok (p5, mk_stmt pos (AST.TypeAlias { alias_name; alias_type_params; alias_body }))

(* Phase 4.3: Trait definition parsing *)
and parse_trait_definition (p : parser) : (parser * AST.statement, parser) result =
  (* Current token is 'trait' *)
  let pos = p.curr_token.pos in
  let* p2 = expect_peek p Token.Ident in
  let name = p2.curr_token.literal in

  (* Expect type parameter: [a] *)
  let* p3 = expect_peek p2 Token.LBracket in
  let* p4 = expect_peek p3 Token.Ident in
  let type_param = p4.curr_token.literal in
  let* p5 = expect_peek p4 Token.RBracket in

  (* Parse optional supertraits: : eq or : eq + show *)
  let* p6, supertraits =
    if peek_token_is p5 Token.Colon then
      parse_supertrait_list (next_token (next_token p5))
    else
      Ok (next_token p5, [])
  in

  (* Expect opening brace *)
  let* p7 =
    if curr_token_is p6 Token.LBrace then
      Ok p6
    else
      expect_peek p6 Token.LBrace
  in

  (* Parse method signatures *)
  let* p8, methods = parse_method_sig_list (next_token p7) in

  (* Expect closing brace *)
  let* p9 =
    if curr_token_is p8 Token.RBrace then
      Ok p8
    else
      expect_peek p8 Token.RBrace
  in

  Ok (p9, mk_stmt pos (AST.TraitDef { name; type_param; supertraits; methods }))

and parse_supertrait_list (p : parser) : (parser * string list, parser) result =
  let rec loop lp traits =
    if curr_token_is lp Token.Ident then
      let trait_name = lp.curr_token.literal in
      let lp2 = next_token lp in
      if curr_token_is lp2 Token.Plus then
        (* More supertraits: + trait *)
        loop (next_token lp2) (traits @ [ trait_name ])
      else
        (* End of supertrait list *)
        Ok (lp2, traits @ [ trait_name ])
    else
      Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
  in
  loop p []

and parse_method_sig_list (p : parser) : (parser * AST.method_sig list, parser) result =
  let rec loop lp methods =
    if curr_token_is lp Token.RBrace then
      Ok (lp, List.rev methods)
    else if curr_token_is lp Token.Function then
      let* lp2, method_sig = parse_method_sig lp in
      loop lp2 (method_sig :: methods)
    else
      Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
  in
  loop p []

and parse_method_sig (p : parser) : (parser * AST.method_sig, parser) result =
  (* Current token is 'fn' *)
  let* p2 = expect_peek p Token.Ident in
  let method_name = p2.curr_token.literal in

  (* Expect opening paren *)
  let* p3 = expect_peek p2 Token.LParen in

  (* Parse parameter list (name: type pairs) *)
  let* p4, params = parse_param_list_with_types (next_token p3) in

  (* Expect closing paren *)
  let* p5 =
    if curr_token_is p4 Token.RParen then
      Ok p4
    else
      expect_peek p4 Token.RParen
  in

  (* Expect arrow *)
  let* p6 = expect_peek p5 Token.Arrow in

  (* Parse return type *)
  let* p7, return_type = parse_type_expr (next_token p6) in

  (* For now, we don't support default implementations *)
  let method_default_impl = None in

  Ok (p7, AST.{ method_name; method_params = params; method_return_type = return_type; method_default_impl })

and parse_param_list_with_types (p : parser) : (parser * (string * AST.type_expr) list, parser) result =
  let rec loop lp params =
    if curr_token_is lp Token.Ident then
      let param_name = lp.curr_token.literal in
      let* lp2 = expect_peek lp Token.Colon in
      let* lp3, param_type = parse_type_expr (next_token lp2) in
      let lp4 = lp3 in
      if curr_token_is lp4 Token.Comma then
        loop (next_token lp4) (params @ [ (param_name, param_type) ])
      else
        Ok (lp4, params @ [ (param_name, param_type) ])
    else if curr_token_is lp Token.RParen then
      Ok (lp, params)
    else
      Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
  in
  loop p []

(* Phase 4.3: Impl definition parsing *)
and parse_impl_definition (p : parser) : (parser * AST.statement, parser) result =
  (* Current token is 'impl' *)
  let pos = p.curr_token.pos in

  (* Expect trait name *)
  let* p2 = expect_peek p Token.Ident in
  let impl_trait_name = p2.curr_token.literal in

  (* Parse optional type parameters with constraints: [a: eq, b: show] *)
  let* p3, impl_type_params =
    if peek_token_is p2 Token.LBracket then
      parse_generic_param_list (next_token (next_token p2))
    else
      Ok (next_token p2, [])
  in

  (* Expect 'for' keyword (treated as identifier, not reserved) *)
  let* p4 =
    if curr_token_is p3 Token.Ident && p3.curr_token.literal = "for" then
      Ok (next_token p3)
    else
      Error (add_error p3 "expected 'for' keyword in impl definition")
  in

  (* Parse the type being implemented for *)
  let* p5, impl_for_type = parse_type_expr p4 in

  (* Expect opening brace *)
  let* p6 =
    if curr_token_is p5 Token.LBrace then
      Ok p5
    else
      expect_peek p5 Token.LBrace
  in

  (* Parse method implementations *)
  let* p7, impl_methods = parse_method_impl_list (next_token p6) in

  (* Expect closing brace *)
  let* p8 =
    if curr_token_is p7 Token.RBrace then
      Ok p7
    else
      expect_peek p7 Token.RBrace
  in

  Ok (p8, mk_stmt pos (AST.ImplDef { impl_type_params; impl_trait_name; impl_for_type; impl_methods }))

and parse_generic_param_list (p : parser) : (parser * AST.generic_param list, parser) result =
  let rec loop lp params =
    if curr_token_is lp Token.Ident then
      let param_name = lp.curr_token.literal in
      let* lp2, constraints =
        if peek_token_is lp Token.Colon then
          (* Parse constraints: a: eq + ord *)
          parse_constraint_list (next_token (next_token lp))
        else
          Ok (next_token lp, [])
      in
      let generic_param = AST.{ name = param_name; constraints } in
      let lp3 = lp2 in
      if curr_token_is lp3 Token.Comma then
        loop (next_token lp3) (params @ [ generic_param ])
      else if curr_token_is lp3 Token.RBracket then
        Ok (next_token lp3, params @ [ generic_param ])
      else
        Error (peek_error lp3 Token.RBracket)
    else if curr_token_is lp Token.RBracket then
      Ok (next_token lp, params)
    else
      Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
  in
  loop p []

and parse_constraint_list (p : parser) : (parser * string list, parser) result =
  let rec loop lp constraints =
    if curr_token_is lp Token.Ident then
      let constraint_name = lp.curr_token.literal in
      let lp2 = next_token lp in
      if curr_token_is lp2 Token.Plus then
        (* More constraints: + trait *)
        loop (next_token lp2) (constraints @ [ constraint_name ])
      else
        (* End of constraint list *)
        Ok (lp2, constraints @ [ constraint_name ])
    else
      Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
  in
  loop p []

and parse_method_impl_list (p : parser) : (parser * AST.method_impl list, parser) result =
  let rec loop lp methods =
    if curr_token_is lp Token.RBrace then
      Ok (lp, List.rev methods)
    else if curr_token_is lp Token.Function then
      let* lp2, method_impl = parse_method_impl lp in
      loop lp2 (method_impl :: methods)
    else
      Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
  in
  loop p []

and parse_method_impl (p : parser) : (parser * AST.method_impl, parser) result =
  (* Current token is 'fn' *)
  let* p2 = expect_peek p Token.Ident in
  let impl_method_name = p2.curr_token.literal in

  (* Expect opening paren *)
  let* p3 = expect_peek p2 Token.LParen in

  (* Parse function parameters (can have optional types) *)
  let* p4, params = parse_function_parameters p3 in

  (* Expect closing paren *)
  let* p5 =
    if curr_token_is p4 Token.RParen then
      Ok p4
    else
      expect_peek p4 Token.RParen
  in

  (* Parse optional return type *)
  let* p6, impl_method_return_type =
    if peek_token_is p5 Token.Arrow then
      let* p6, ret_type = parse_type_expr (next_token (next_token p5)) in
      Ok (p6, Some ret_type)
    else
      Ok (next_token p5, None)
  in

  (* Expect opening brace for body *)
  let* p7 =
    if curr_token_is p6 Token.LBrace then
      Ok p6
    else
      expect_peek p6 Token.LBrace
  in

  (* Parse method body as an expression *)
  let* p8, body_expr = parse_expression (next_token p7) prec_lowest in

  (* Expect closing brace *)
  let* p9 =
    if curr_token_is p8 Token.RBrace then
      Ok p8
    else
      expect_peek p8 Token.RBrace
  in

  Ok
    ( next_token p9,
      AST.{ impl_method_name; impl_method_params = params; impl_method_return_type; impl_method_body = body_expr }
    )

(* Phase 4.3: Derive statement parsing *)
and parse_derive_definition (p : parser) : (parser * AST.statement, parser) result =
  (* Current token is 'derive' *)
  let pos = p.curr_token.pos in

  (* Parse trait list: eq, show, ord *)
  let* p2, derive_traits = parse_derive_trait_list (next_token p) in

  (* Expect 'for' keyword (treated as identifier, not reserved) *)
  let* p3 =
    if curr_token_is p2 Token.Ident && p2.curr_token.literal = "for" then
      Ok (next_token p2)
    else
      Error (add_error p2 "expected 'for' keyword in derive statement")
  in

  (* Parse the type being derived for *)
  let* p4, derive_for_type = parse_type_expr p3 in

  Ok (p4, mk_stmt pos (AST.DeriveDef { derive_traits; derive_for_type }))

and parse_derive_trait_list (p : parser) : (parser * AST.derive_trait list, parser) result =
  let rec loop lp traits =
    if curr_token_is lp Token.Ident then
      let trait_name = lp.curr_token.literal in
      let lp2 = next_token lp in

      (* Check for type parameters: eq[a: show] *)
      let* lp3, constraints =
        if curr_token_is lp2 Token.LBracket then
          parse_generic_param_list (next_token lp2)
        else
          Ok (lp2, [])
      in

      let derive_trait = AST.{ derive_trait_name = trait_name; derive_trait_constraints = constraints } in

      (* Check for comma (more traits) *)
      if curr_token_is lp3 Token.Comma then
        loop (next_token lp3) (traits @ [ derive_trait ])
      else
        (* End of trait list *)
        Ok (lp3, traits @ [ derive_trait ])
    else
      Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
  in
  loop p []

and parse_expression_statement (p : parser) : (parser * AST.statement, parser) result =
  let pos = p.curr_token.pos in
  let* p2, expr = parse_expression p prec_lowest in
  let p3 = skip p2 Token.Semicolon in
  Ok (p3, mk_stmt pos (AST.ExpressionStmt expr))

and parse_expression (p : parser) (prec : precedence) : (parser * AST.expression, parser) result =
  let* p2, left_expr = prefixFn p in
  let* p3, left = infixFn p2 left_expr prec in
  Ok (p3, left)

and prefixFn (p : parser) : (parser * AST.expression, parser) result =
  let tt = p.curr_token.token_type in
  match tt with
  | Token.Ident -> parse_identifier p
  | Token.Int -> parse_integer_literal p
  | Token.Float -> parse_float_literal p
  | Token.String -> parse_string_literal p
  | Token.Bang | Token.Minus -> parse_prefix_expression p
  | Token.True | Token.False -> parse_boolean p
  | Token.LParen -> parse_grouped_expression p
  | Token.If -> parse_if_expression p
  | Token.Match -> parse_match_expression p
  | Token.Function -> parse_function_literal p
  | Token.LBracket -> parse_array_literal p
  | Token.LBrace -> parse_record_or_hash_literal p
  | _ -> Error (no_prefix_parse_fn_error p tt)

and infixFn (p : parser) (left_expr : AST.expression) (prec : precedence) :
    (parser * AST.expression, parser) result =
  let rec loop (lp : parser) (left : AST.expression) : (parser * AST.expression, parser) result =
    let peek_is_semicolon = peek_token_is lp Token.Semicolon in
    let lower_precedence = prec < peek_precedence lp in
    if (not peek_is_semicolon) && lower_precedence then
      let* lp2, left2 =
        match lp.peek_token.token_type with
        | Token.Plus | Token.Minus | Token.Slash | Token.Asterisk | Token.Eq | Token.NotEq | Token.Lt | Token.Gt
        | Token.Is ->
            parse_infix_expression (next_token lp) left
        | LParen -> parse_call_expression (next_token lp) left
        | LBracket -> parse_index_expression (next_token lp) left
        | Dot -> parse_dot_expression (next_token lp) left
        | _ -> Ok (lp, left)
      in
      loop lp2 left2
    else
      Ok (lp, left)
  in
  loop p left_expr

and parse_identifier (p : parser) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  let name = p.curr_token.literal in
  (* Just parse as identifier - let postfix dot parser handle member access *)
  let id, p1 = fresh_id p in
  Ok (p1, mk_expr id pos (AST.Identifier name))

and parse_integer_literal (p : parser) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  match Int64.of_string_opt p.curr_token.literal with
  | Some int ->
      let id, p1 = fresh_id p in
      Ok (p1, mk_expr id pos (AST.Integer int))
  | None ->
      let msg = Printf.sprintf "can't parse number from %s" p.curr_token.literal in
      Error (add_error p msg)

and parse_float_literal (p : parser) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  match float_of_string_opt p.curr_token.literal with
  | Some float ->
      let id, p1 = fresh_id p in
      Ok (p1, mk_expr id pos (AST.Float float))
  | None ->
      let msg = Printf.sprintf "can't parse number from %s" p.curr_token.literal in
      Error (add_error p msg)

and parse_string_literal (p : parser) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  let id, p1 = fresh_id p in
  Ok (p1, mk_expr id pos (AST.String p.curr_token.literal))

and parse_prefix_expression (p : parser) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  let op = p.curr_token.literal in
  let p2 = next_token p in
  let* p3, right = parse_expression p2 prec_prefix in
  let id, p4 = fresh_id p3 in
  Ok (p4, mk_expr id pos (AST.Prefix (op, right)))

and parse_infix_expression (p : parser) (left : AST.expression) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  match p.curr_token.token_type with
  | Token.Is ->
      (* x is int - for now, only support simple type identifiers *)
      (* Advance to the type name *)
      let p2 = next_token p in
      if not (curr_token_is p2 Token.Ident) then
        Error (add_error p2 "Expected type name after 'is'")
      else
        let type_name = p2.curr_token.literal in
        let type_expr = AST.TCon type_name in
        (* Don't advance further - stay at the type identifier *)
        (* This matches the convention of other infix operators *)
        let id, p3 = fresh_id p2 in
        Ok (p3, mk_expr id pos (AST.TypeCheck (left, type_expr)))
  | _ ->
      let op = p.curr_token.literal in
      let prec = curr_precedence p in
      let p2 = next_token p in
      let* p3, right = parse_expression p2 prec in
      let id, p4 = fresh_id p3 in
      Ok (p4, mk_expr id pos (AST.Infix (left, op, right)))

and parse_boolean (p : parser) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  let id, p1 = fresh_id p in
  Ok (p1, mk_expr id pos (AST.Boolean (p.curr_token.token_type = Token.True)))

and parse_grouped_expression (p : parser) : (parser * AST.expression, parser) result =
  let* p2, expr = parse_expression (next_token p) prec_lowest in
  let* p3 = expect_peek p2 Token.RParen in
  Ok (p3, expr)

and parse_if_expression (p : parser) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  let* p2 = expect_peek p Token.LParen in
  let* p3, cond = parse_expression (next_token p2) prec_lowest in
  let* p4 = expect_peek p3 Token.RParen in

  (* After ), check for { or return (with early return support) *)
  let* p5, cons =
    if peek_token_is p4 Token.LBrace then
      (* Normal: { ... } *)
      let* p5 = expect_peek p4 Token.LBrace in
      let* p6, cons = parse_block_statement p5 in
      Ok (p6, cons)
    else if peek_token_is p4 Token.Return then
      (* New: return expr without braces *)
      let p5 = next_token p4 in
      (* Now at 'return' token *)
      let pos_ret = p5.curr_token.pos in
      let* p6, expr = parse_expression (next_token p5) prec_lowest in
      let p7 = skip p6 Token.Semicolon in
      let ret_stmt = mk_stmt pos_ret (AST.Return expr) in
      Ok (p7, mk_stmt pos_ret (AST.Block [ ret_stmt ]))
    else
      Error (add_error p4 "Expected '{' or 'return' after if condition")
  in

  if not (peek_token_is p5 Token.Else) then
    let id, p6 = fresh_id p5 in
    Ok (p6, mk_expr id pos (AST.If (cond, cons, None)))
  else
    let p6 = next_token p5 in
    (* Now at 'else' token *)
    let* p7, alt =
      if peek_token_is p6 Token.LBrace then
        (* Normal: else { ... } *)
        let* p7 = expect_peek p6 Token.LBrace in
        let* p8, alt = parse_block_statement p7 in
        Ok (p8, alt)
      else if peek_token_is p6 Token.Return then
        (* New: else return expr without braces *)
        let p7 = next_token p6 in
        (* Now at 'return' token *)
        let pos_ret = p7.curr_token.pos in
        let* p8, expr = parse_expression (next_token p7) prec_lowest in
        let p9 = skip p8 Token.Semicolon in
        let ret_stmt = mk_stmt pos_ret (AST.Return expr) in
        Ok (p9, mk_stmt pos_ret (AST.Block [ ret_stmt ]))
      else
        (* Error: else without block or return *)
        Error (add_error p6 "Expected '{' or 'return' after 'else'")
    in
    let id, p8 = fresh_id p7 in
    Ok (p8, mk_expr id pos (AST.If (cond, cons, Some alt)))

and parse_block_statement (p : parser) : (parser * AST.statement, parser) result =
  let pos = p.curr_token.pos in
  let rec loop (lp : parser) (stmts : AST.statement list) : (parser * AST.statement, parser) result =
    if curr_token_is lp Token.RBrace || curr_token_is lp Token.EOF then
      Ok (lp, mk_stmt pos (AST.Block (List.rev stmts)))
    else
      let* lp2, new_block = parse_statement lp in
      loop (next_token lp2) ([ new_block ] @ stmts)
  in

  loop (next_token p) []

and parse_function_literal (p : parser) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  (* Phase 2: Parse generic parameters if present *)
  let* p2, generics = parse_generic_params p in
  let* p3 = expect_peek p2 Token.LParen in
  let* p4, params = parse_function_parameters p3 in
  (* Phase 2: Parse return type annotation if present *)
  let* p5, return_type =
    if peek_token_is p4 Token.Arrow then
      let p5 = next_token p4 in
      let* p6, te = parse_type_expr (next_token p5) in
      Ok (p6, Some te)
    else if peek_token_is p4 Token.FatArrow then
      (* Effect annotation - parse but ignore for Phase 2 *)
      let p5 = next_token p4 in
      let* p6, _te = parse_type_expr (next_token p5) in
      Ok (p6, None)
    else
      Ok (p4, None)
  in
  (* After parsing return type or parameters, check for { *)
  let* p6 =
    if curr_token_is p5 Token.LBrace then
      Ok p5
    else if peek_token_is p5 Token.LBrace then
      Ok (next_token p5)
    else
      Error (peek_error p5 Token.LBrace)
  in
  let* p7, body = parse_block_statement p6 in
  let id, p8 = fresh_id p7 in
  Ok (p8, mk_expr id pos (AST.Function { generics; params; return_type; body }))

and parse_function_parameters (p : parser) : (parser * (string * AST.type_expr option) list, parser) result =
  if peek_token_is p Token.RParen then
    Ok (next_token p, [])
  else
    let rec loop (lp : parser) (params_acc : (string * AST.type_expr option) list) =
      (* Get the parameter name *)
      if not (curr_token_is lp Token.Ident) then
        Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
      else
        let param_name = lp.curr_token.literal in
        let lp_after_name = next_token lp in
        (* Phase 2: Parse type annotation if present *)
        let* lp_after_annot, type_annot =
          if curr_token_is lp_after_name Token.Colon then
            (* Parse the type expression *)
            let* lp_parse, te = parse_type_expr (next_token lp_after_name) in
            Ok (lp_parse, Some te)
          else
            Ok (lp_after_name, None)
        in
        let new_param = (param_name, type_annot) in
        if curr_token_is lp_after_annot Token.Comma then
          loop (next_token lp_after_annot) (params_acc @ [ new_param ])
        else if curr_token_is lp_after_annot Token.RParen then
          Ok (lp_after_annot, params_acc @ [ new_param ])
        else
          Error (peek_error lp_after_annot Token.RParen)
    in
    let p2 = next_token p in
    loop p2 []

and parse_call_expression (p : parser) (c : AST.expression) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  let* p2, arguments = parse_expression_list p Token.RParen in
  let id, p3 = fresh_id p2 in
  Ok (p3, mk_expr id pos (AST.Call (c, arguments)))

and parse_expression_list (p : parser) (end_tt : Token.token_type) : (parser * AST.expression list, parser) result
    =
  if peek_token_is p end_tt then
    Ok (next_token p, [])
  else
    let* p2, arg = parse_expression (next_token p) prec_lowest in

    let rec loop (lp : parser) (args : AST.expression list) =
      if peek_token_is lp Token.Comma then
        let* lp2, arg = parse_expression (next_token (next_token lp)) prec_lowest in
        loop lp2 ([ arg ] @ args)
      else
        let* lp2 = expect_peek lp end_tt in
        Ok (lp2, List.rev args)
    in

    loop p2 [ arg ]

and parse_array_literal (p : parser) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  let* p2, exprs = parse_expression_list p Token.RBracket in
  let id, p3 = fresh_id p2 in
  Ok (p3, mk_expr id pos (AST.Array exprs))

and parse_index_expression (p : parser) (left : AST.expression) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  let p2 = next_token p in
  let* p3, index = parse_expression p2 prec_lowest in
  let* p4 = expect_peek p3 Token.RBracket in
  let id, p5 = fresh_id p4 in
  Ok (p5, mk_expr id pos (AST.Index (left, index)))

(* Phase 4.3: Dot expression parsing - handles method calls and field access *)
(* Note: enum constructors are also parsed as MethodCall here, type checker will distinguish *)
and parse_dot_expression (p : parser) (left : AST.expression) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  (* p.curr_token is Dot, advance to the identifier *)
  let p2 = next_token p in
  if not (curr_token_is p2 Token.Ident) then
    Error (add_error p2 "expected identifier after '.'")
  else
    let member_name = p2.curr_token.literal in

    if peek_token_is p2 Token.LParen then
      (* expr.member(args) -> Method call or enum constructor *)
      let p3 = next_token p2 in
      let* p4, args = parse_expression_list p3 Token.RParen in
      let id, p5 = fresh_id p4 in
      Ok (p5, mk_expr id pos (AST.MethodCall (left, member_name, args)))
    else
      (* expr.member -> Field access or nullary enum constructor *)
      let id, p3 = fresh_id p2 in
      Ok (p3, mk_expr id pos (AST.FieldAccess (left, member_name)))

(* Phase 4.6: Distinguish record literal from hash literal *)
and parse_record_or_hash_literal (p : parser) : (parser * AST.expression, parser) result =
  (* Current token is { *)
  let pos = p.curr_token.pos in
  let p2 = next_token p in

  if curr_token_is p2 Token.RBrace then
    (* Empty braces: treat as empty hash for backwards compatibility *)
    let id, _p3 = fresh_id p2 in
    Ok (next_token p2, mk_expr id pos (AST.Hash []))
  else if curr_token_is p2 Token.Spread then
    (* Starts with spread: { ...base } or { ...base, x: 1 } *)
    parse_record_literal_with_spread p pos
  else if curr_token_is p2 Token.Ident then
    (* Check if it's a record (identifier key) or hash (expression key) *)
    (* Lookahead: is next token a colon? *)
    if peek_token_is p2 Token.Colon then
      (* Record literal: { x: ... } or { x:, ... } (punning) *)
      parse_record_literal p
    else
      (* Not a record field, treat as hash with expression key *)
      parse_hash_literal p
  else
    (* Starts with expression (string, int, etc.) - must be hash *)
    parse_hash_literal p

(* Parse record literal: { x: 1, y: 2 } or { x:, y: } *)
and parse_record_literal (p : parser) : (parser * AST.expression, parser) result =
  (* Current token is { *)
  let pos = p.curr_token.pos in
  let rec loop lp fields spread =
    if curr_token_is lp Token.RBrace then
      (* End of record *)
      let id, _lp1 = fresh_id lp in
      Ok (lp, mk_expr id pos (AST.RecordLit (List.rev fields, spread)))
    else if curr_token_is lp Token.Spread then
      (* Spread: { ...base, x: 1 } *)
      let lp2 = next_token lp in
      let* lp3, spread_expr = parse_expression lp2 prec_lowest in
      if peek_token_is lp3 Token.Comma then
        (* More fields after spread *)
        loop (next_token (next_token lp3)) fields (Some spread_expr)
      else
        (* End after spread *)
        loop (next_token lp3) fields (Some spread_expr)
    else if curr_token_is lp Token.Ident then
      (* Field: name: value or name: (punning) *)
      let field_name = lp.curr_token.literal in
      let* lp2 = expect_peek lp Token.Colon in

      (* Check for punning: { x:, y: } or { x: } *)
      (* After expect_peek, lp2 is at the colon, peek is the next token *)
      let* lp3, field_value =
        if peek_token_is lp2 Token.Comma || peek_token_is lp2 Token.RBrace then
          (* Punning: no value, use variable with same name *)
          (* Advance past colon to comma/brace *)
          Ok (next_token lp2, None)
        else
          (* Parse value *)
          let lp2 = next_token lp2 in
          let* lp3, value = parse_expression lp2 prec_lowest in
          Ok (lp3, Some value)
      in

      let field = AST.{ field_name; field_value } in
      match field_value with
      | None ->
          (* Punning: lp3 is already at , or } *)
          if curr_token_is lp3 Token.Comma then
            loop (next_token lp3) (field :: fields) spread
          else
            loop lp3 (field :: fields) spread
      | Some _ ->
          (* Non-punning: lp3 is at the value, need to advance *)
          if peek_token_is lp3 Token.Comma then
            loop (next_token (next_token lp3)) (field :: fields) spread
          else
            loop (next_token lp3) (field :: fields) spread
    else
      Error (add_error lp "cannot mix hash-style entries into record literal")
  in
  loop (next_token p) [] None

(* Parse record literal starting with spread: { ...base } or { ...base, x: 1 } *)
and parse_record_literal_with_spread (p : parser) (pos : int) : (parser * AST.expression, parser) result =
  (* Current token is { *)
  let p2 = next_token p in
  (* Now at ... *)
  let p3 = next_token p2 in
  (* Parse spread expression *)
  let* p4, spread_expr = parse_expression p3 prec_lowest in

  (* Check for more fields *)
  if peek_token_is p4 Token.Comma then
    (* More fields: { ...base, x: 1 } *)
    let rec loop lp fields =
      if curr_token_is lp Token.RBrace then
        let id, _lp1 = fresh_id lp in
        Ok (lp, mk_expr id pos (AST.RecordLit (List.rev fields, Some spread_expr)))
      else if curr_token_is lp Token.Ident then
        let field_name = lp.curr_token.literal in
        let* lp2 = expect_peek lp Token.Colon in

        let* lp3, field_value =
          if peek_token_is lp2 Token.Comma || peek_token_is lp2 Token.RBrace then
            Ok (next_token lp2, None)
          else
            let lp2 = next_token lp2 in
            let* lp3, value = parse_expression lp2 prec_lowest in
            Ok (lp3, Some value)
        in

        let field = AST.{ field_name; field_value } in
        match field_value with
        | None ->
            if curr_token_is lp3 Token.Comma then
              loop (next_token lp3) (field :: fields)
            else
              loop lp3 (field :: fields)
        | Some _ ->
            if peek_token_is lp3 Token.Comma then
              loop (next_token (next_token lp3)) (field :: fields)
            else
              loop (next_token lp3) (field :: fields)
      else
        Error (add_error lp "cannot mix hash-style entries into record literal")
    in
    loop (next_token (next_token p4)) []
  else if peek_token_is p4 Token.RBrace then
    (* Just spread: { ...base } *)
    let id, _p5 = fresh_id p4 in
    Ok (next_token p4, mk_expr id pos (AST.RecordLit ([], Some spread_expr)))
  else
    Error (peek_error p4 Token.RBrace)

and parse_hash_literal (p : parser) : (parser * AST.expression, parser) result =
  let pos = p.curr_token.pos in
  let rec loop lp (pairs : (AST.expression * AST.expression) list) =
    if peek_token_is lp Token.RBrace then
      let id, lp1 = fresh_id (next_token lp) in
      Ok (lp1, mk_expr id pos (AST.Hash (List.rev pairs)))
    else
      let key_start = next_token lp in
      if curr_token_is key_start Token.Spread then
        Error (add_error key_start "cannot mix record-style spread into hash literal")
      else if curr_token_is key_start Token.Ident && peek_token_is key_start Token.Colon then
        Error (add_error key_start "cannot mix record-style field into hash literal")
      else
        let* lp2, key = parse_expression key_start prec_lowest in
      let* lp3 = expect_peek lp2 Token.Colon in
      let* lp4, value = parse_expression (next_token lp3) prec_lowest in

      if peek_token_is lp4 Token.Comma then
        loop (next_token lp4) ([ (key, value) ] @ pairs)
      else if not (peek_token_is lp4 Token.RBrace) then
        Error lp4
      else
        loop lp4 ([ (key, value) ] @ pairs)
  in

  loop p []

(* Phase 4.2: Match expression parsing *)
and parse_match_expression (p : parser) : (parser * AST.expression, parser) result =
  (* Current token is 'match' *)
  let pos = p.curr_token.pos in

  (* Parse the scrutinee expression *)
  let* p2, scrutinee = parse_expression (next_token p) prec_lowest in

  (* Expect opening brace *)
  let* p3 = expect_peek p2 Token.LBrace in

  (* Parse match arms *)
  let* p4, arms = parse_match_arms (next_token p3) in

  (* Expect closing brace *)
  let* p5 =
    if curr_token_is p4 Token.RBrace then
      Ok p4
    else
      expect_peek p4 Token.RBrace
  in

  let id, p6 = fresh_id p5 in
  Ok (p6, mk_expr id pos (AST.Match (scrutinee, arms)))

and parse_match_arms (p : parser) : (parser * AST.match_arm list, parser) result =
  let rec loop lp arms =
    if curr_token_is lp Token.RBrace then
      Ok (lp, List.rev arms)
    else
      (* Parse patterns (may be multiple with | separator) *)
      let* lp2, patterns = parse_patterns lp in

      (* Expect colon *)
      let* lp3 = expect_peek lp2 Token.Colon in

      (* Parse arm body *)
      let* lp4, body = parse_expression (next_token lp3) prec_lowest in

      let arm = AST.{ patterns; body } in
      (* Advance past the body expression before continuing *)
      loop (next_token lp4) (arm :: arms)
  in
  loop p []

and parse_patterns (p : parser) : (parser * AST.pattern list, parser) result =
  let* p2, first_pattern = parse_pattern p in

  let rec loop lp patterns =
    if peek_token_is lp Token.Pipe then
      let* lp2, pat = parse_pattern (next_token (next_token lp)) in
      loop lp2 (pat :: patterns)
    else
      Ok (lp, List.rev patterns)
  in

  loop p2 [ first_pattern ]

and parse_pattern (p : parser) : (parser * AST.pattern, parser) result =
  match p.curr_token.token_type with
  | Token.Ident ->
      (* Check for wildcard first *)
      if p.curr_token.literal = "_" then
        Ok (p, AST.mk_pat ~pos:p.curr_token.pos AST.PWildcard)
        (* Could be: variable binding, or enum.variant(patterns) *)
      else if peek_token_is p Token.Dot then
        (* Enum constructor pattern: enum.variant or enum.variant(patterns) *)
        let enum_name = p.curr_token.literal in
        let p2 = next_token (next_token p) in
        (* skip enum and dot *)
        if curr_token_is p2 Token.Ident then
          let variant_name = p2.curr_token.literal in
          (* Check for nested patterns *)
          if peek_token_is p2 Token.LParen then
            let p3 = next_token p2 in
            (* move to LParen *)
            let* p4, nested_patterns = parse_pattern_list p3 in
            Ok (p4, AST.mk_pat ~pos:p.curr_token.pos (AST.PConstructor (enum_name, variant_name, nested_patterns)))
          else
            (* Nullary constructor *)
            Ok (p2, AST.mk_pat ~pos:p.curr_token.pos (AST.PConstructor (enum_name, variant_name, [])))
        else
          Error (add_error p2 "expected variant name after '.'")
      else
        (* Variable binding *)
        Ok (p, AST.mk_pat ~pos:p.curr_token.pos (AST.PVariable p.curr_token.literal))
  | Token.Int -> (
      match Int64.of_string_opt p.curr_token.literal with
      | Some i -> Ok (p, AST.mk_pat ~pos:p.curr_token.pos (AST.PLiteral (AST.LInt i)))
      | None -> Error (add_error p "invalid integer literal in pattern"))
  | Token.String -> Ok (p, AST.mk_pat ~pos:p.curr_token.pos (AST.PLiteral (AST.LString p.curr_token.literal)))
  | Token.True -> Ok (p, AST.mk_pat ~pos:p.curr_token.pos (AST.PLiteral (AST.LBool true)))
  | Token.False -> Ok (p, AST.mk_pat ~pos:p.curr_token.pos (AST.PLiteral (AST.LBool false)))
  | Token.LBrace -> parse_record_pattern p
  | Token.Minus ->
      (* Underscore is not a token, but we use it for wildcard *)
      (* Actually, wildcard should be an identifier "_" *)
      Ok (p, AST.mk_pat ~pos:p.curr_token.pos AST.PWildcard)
  | _ ->
      (* Check if it's underscore identifier *)
      if p.curr_token.token_type = Token.Ident && p.curr_token.literal = "_" then
        Ok (p, AST.mk_pat ~pos:p.curr_token.pos AST.PWildcard)
      else
        Error (add_error p "invalid pattern")

(* Phase 4.8: Parse record pattern: { x:, y: z, ...rest } *)
and parse_record_pattern (p : parser) : (parser * AST.pattern, parser) result =
  (* Current token is { *)
  let pos = p.curr_token.pos in
  let p2 = next_token p in

  let rec loop lp fields rest =
    if curr_token_is lp Token.RBrace then
      (* End of record pattern *)
      Ok (lp, AST.mk_pat ~pos (AST.PRecord (List.rev fields, rest)))
    else if curr_token_is lp Token.Spread then
      (* Rest pattern: { x:, ...rest } *)
      let lp2 = next_token lp in
      if curr_token_is lp2 Token.Ident then
        let rest_name = lp2.curr_token.literal in
        let* lp3 = expect_peek lp2 Token.RBrace in
        Ok (lp3, AST.mk_pat ~pos (AST.PRecord (List.rev fields, Some rest_name)))
      else
        Error (add_error lp2 "expected identifier after '...' in record pattern")
    else if curr_token_is lp Token.Ident then
      (* Field pattern: x: or x: pat *)
      let field_name = lp.curr_token.literal in
      let* lp2 = expect_peek lp Token.Colon in

      (* Check for punning: { x:, y: } *)
      let* lp3, field_pattern =
        if peek_token_is lp2 Token.Comma || peek_token_is lp2 Token.RBrace then
          (* Punning: bind to variable with same name *)
          Ok (next_token lp2, None)
        else
          (* Parse nested pattern *)
          let* lp3, pat = parse_pattern (next_token lp2) in
          Ok (lp3, Some pat)
      in

      let field = AST.{ pat_field_name = field_name; pat_field_pattern = field_pattern } in

      (* Handle comma or end *)
      (* For punning, lp3 is at comma or rbrace; for non-punning, lp3 is at pattern token *)
      if curr_token_is lp3 Token.Comma then
        (* Punning case: already at comma, advance to next field *)
        loop (next_token lp3) (field :: fields) rest
      else if peek_token_is lp3 Token.Comma then
        (* Non-punning case: at pattern token, peek is comma *)
        loop (next_token (next_token lp3)) (field :: fields) rest
      else if
        (* No comma: either at rbrace (punning) or pattern with no comma after (non-punning) *)
        (* Either way, continue loop to check for rbrace *)
        curr_token_is lp3 Token.RBrace || peek_token_is lp3 Token.RBrace
      then
        loop
          (if curr_token_is lp3 Token.RBrace then
             lp3
           else
             next_token lp3)
          (field :: fields) rest
      else
        Error (add_error lp3 "expected comma or closing brace after record pattern field")
    else
      Error (add_error lp "invalid record pattern field")
  in

  loop p2 [] None

and parse_pattern_list (p : parser) : (parser * AST.pattern list, parser) result =
  (* p is at LParen *)
  if peek_token_is p Token.RParen then
    Ok (next_token p, [])
  else
    let* p2, first_pat = parse_pattern (next_token p) in

    let rec loop lp pats =
      if peek_token_is lp Token.Comma then
        let* lp2, pat = parse_pattern (next_token (next_token lp)) in
        loop lp2 (pat :: pats)
      else
        let* lp2 = expect_peek lp Token.RParen in
        Ok (lp2, List.rev pats)
    in

    loop p2 [ first_pat ]

let parse (s : string) : (AST.program, errors) result =
  match s |> Lexer.init |> init |> parse_program with
  | Ok (_, program) -> Ok program
  | Error parser -> Error (List.rev parser.errors)

module Test = struct
  type test = {
    input : string;
    output : AST.program;
  }

  (* Shorthand constructors for tests (ignoring positions) *)
  let e kind = AST.mk_expr kind
  let s kind = AST.mk_stmt kind

  (* Helper for Let statements with the new record structure *)
  let let_stmt name value = AST.Let { name; value; type_annotation = None }

  (* Helper for Function expressions with the new record structure *)
  let fn_expr params body = AST.Function { generics = None; params; return_type = None; body }

  let run (tests : test list) : bool =
    tests
    |> List.for_all (fun test ->
           match test.input |> parse with
           | Ok program -> AST.program_equal program test.output
           | Error _ -> false)

  let run_print (tests : test list) : unit =
    tests
    |> List.iter (fun test ->
           match test.input |> parse with
           | Ok program ->
               Printf.printf "input:\n%s\n" test.input;
               Printf.printf "expected:\n%s\n" (AST.show_program test.output);
               Printf.printf "output:\n%s\n" (AST.show_program program);
               flush stdout
           | Error errors ->
               Printf.printf "input:\n%s\n" test.input;
               Printf.printf "errors:\n%s\n" (String.concat "\n" errors);
               flush stdout)

  let%test "test_let_statements" =
    [
      { input = "let x = 5;"; output = [ s (let_stmt "x" (e (AST.Integer 5L))) ] };
      { input = "let y = 10;"; output = [ s (let_stmt "y" (e (AST.Integer 10L))) ] };
      { input = "let foobar = 838383;"; output = [ s (let_stmt "foobar" (e (AST.Integer 838383L))) ] };
    ]
    |> run

  let%test "test_return_statements" =
    [
      { input = "return 5;"; output = [ s (AST.Return (e (AST.Integer 5L))) ] };
      { input = "return 10;"; output = [ s (AST.Return (e (AST.Integer 10L))) ] };
      { input = "return foobar;"; output = [ s (AST.Return (e (AST.Identifier "foobar"))) ] };
    ]
    |> run

  let%test "test_identifier_expressions" =
    [ { input = "foobar;"; output = [ s (AST.ExpressionStmt (e (AST.Identifier "foobar"))) ] } ] |> run

  let%test "test_integer_literal_expressions" =
    [ { input = "5;"; output = [ s (AST.ExpressionStmt (e (AST.Integer 5L))) ] } ] |> run

  let%test "test_float_literal_expressions" =
    [ { input = "5.5;"; output = [ s (AST.ExpressionStmt (e (AST.Float 5.5))) ] } ] |> run

  let%test "test_string_literal_expressions" =
    [ { input = "\"hello world\";"; output = [ s (AST.ExpressionStmt (e (AST.String "hello world"))) ] } ] |> run

  let%test "test_array_literals" =
    [
      { input = "[]"; output = [ s (AST.ExpressionStmt (e (AST.Array []))) ] };
      {
        input = "[1, 2, 3];";
        output =
          [
            s (AST.ExpressionStmt (e (AST.Array [ e (AST.Integer 1L); e (AST.Integer 2L); e (AST.Integer 3L) ])));
          ];
      };
      {
        input = "[1, 2 * 2, 3 + 3];";
        output =
          [
            s
              (AST.ExpressionStmt
                 (e
                    (AST.Array
                       [
                         e (AST.Integer 1L);
                         e (AST.Infix (e (AST.Integer 2L), "*", e (AST.Integer 2L)));
                         e (AST.Infix (e (AST.Integer 3L), "+", e (AST.Integer 3L)));
                       ])));
          ];
      };
    ]
    |> run

  let%test "test_index_expressions" =
    [
      {
        input = "myArray[1 + 1];";
        output =
          [
            s
              (AST.ExpressionStmt
                 (e
                    (AST.Index
                       (e (AST.Identifier "myArray"), e (AST.Infix (e (AST.Integer 1L), "+", e (AST.Integer 1L)))))));
          ];
      };
      {
        input = "myArray[1];";
        output = [ s (AST.ExpressionStmt (e (AST.Index (e (AST.Identifier "myArray"), e (AST.Integer 1L))))) ];
      };
    ]
    |> run

  let%test "test_prefix_expressions" =
    [
      { input = "!5;"; output = [ s (AST.ExpressionStmt (e (AST.Prefix ("!", e (AST.Integer 5L))))) ] };
      { input = "-15;"; output = [ s (AST.ExpressionStmt (e (AST.Prefix ("-", e (AST.Integer 15L))))) ] };
      {
        input = "!foobar;";
        output = [ s (AST.ExpressionStmt (e (AST.Prefix ("!", e (AST.Identifier "foobar"))))) ];
      };
      {
        input = "-foobar;";
        output = [ s (AST.ExpressionStmt (e (AST.Prefix ("-", e (AST.Identifier "foobar"))))) ];
      };
      { input = "!true;"; output = [ s (AST.ExpressionStmt (e (AST.Prefix ("!", e (AST.Boolean true))))) ] };
      { input = "!false;"; output = [ s (AST.ExpressionStmt (e (AST.Prefix ("!", e (AST.Boolean false))))) ] };
    ]
    |> run

  let%test "test_infix_expressions" =
    [
      {
        input = "5 + 5;";
        output = [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Integer 5L), "+", e (AST.Integer 5L))))) ];
      };
      {
        input = "5 - 5;";
        output = [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Integer 5L), "-", e (AST.Integer 5L))))) ];
      };
      {
        input = "5 * 5;";
        output = [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Integer 5L), "*", e (AST.Integer 5L))))) ];
      };
      {
        input = "5 / 5;";
        output = [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Integer 5L), "/", e (AST.Integer 5L))))) ];
      };
      {
        input = "5 > 5;";
        output = [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Integer 5L), ">", e (AST.Integer 5L))))) ];
      };
      {
        input = "5 < 5;";
        output = [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Integer 5L), "<", e (AST.Integer 5L))))) ];
      };
      {
        input = "5 == 5;";
        output = [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Integer 5L), "==", e (AST.Integer 5L))))) ];
      };
      {
        input = "5 != 5;";
        output = [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Integer 5L), "!=", e (AST.Integer 5L))))) ];
      };
      {
        input = "foo + bar;";
        output =
          [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Identifier "foo"), "+", e (AST.Identifier "bar"))))) ];
      };
      {
        input = "foo - bar;";
        output =
          [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Identifier "foo"), "-", e (AST.Identifier "bar"))))) ];
      };
      {
        input = "foo * bar;";
        output =
          [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Identifier "foo"), "*", e (AST.Identifier "bar"))))) ];
      };
      {
        input = "foo / bar;";
        output =
          [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Identifier "foo"), "/", e (AST.Identifier "bar"))))) ];
      };
      {
        input = "foo > bar;";
        output =
          [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Identifier "foo"), ">", e (AST.Identifier "bar"))))) ];
      };
      {
        input = "foo < bar;";
        output =
          [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Identifier "foo"), "<", e (AST.Identifier "bar"))))) ];
      };
      {
        input = "foo == bar;";
        output =
          [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Identifier "foo"), "==", e (AST.Identifier "bar"))))) ];
      };
      {
        input = "foo != bar;";
        output =
          [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Identifier "foo"), "!=", e (AST.Identifier "bar"))))) ];
      };
      {
        input = "true == true;";
        output = [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Boolean true), "==", e (AST.Boolean true))))) ];
      };
      {
        input = "true != false;";
        output = [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Boolean true), "!=", e (AST.Boolean false))))) ];
      };
      {
        input = "false == false;";
        output = [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Boolean false), "==", e (AST.Boolean false))))) ];
      };
    ]
    |> run

  let%test "test_operator_precedence" =
    [
      ("-a * b", "((-a) * b)");
      ("!-a", "(!(-a))");
      ("a + b + c", "((a + b) + c)");
      ("a + b - c", "((a + b) - c)");
      ("a * b * c", "((a * b) * c)");
      ("a * b / c", "((a * b) / c)");
      ("a + b / c", "(a + (b / c))");
      ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)");
      ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)");
      ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))");
      ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))");
      ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))");
      ("true", "true");
      ("false", "false");
      ("3 > 5 == false", "((3 > 5) == false)");
      ("3 < 5 == true", "((3 < 5) == true)");
      ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)");
      ("(5 + 5) * 2", "((5 + 5) * 2)");
      ("2 / (5 + 5)", "(2 / (5 + 5))");
      ("(5 + 5) * 2 * (5 + 5)", "(((5 + 5) * 2) * (5 + 5))");
      ("-(5 + 5)", "(-(5 + 5))");
      ("!(true == true)", "(!(true == true))");
      ("a + add(b * c) + d", "((a + add((b * c))) + d)");
      ("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))");
      ("add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))");
      ("a * [1, 2, 3, 4][b * c] * d", "((a * ([1, 2, 3, 4][(b * c)])) * d)");
      ("add(a * b[2], b[1], 2 * [1, 2][1])", "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))");
    ]
    |> List.for_all (fun test ->
           let input, output = test in
           match input |> parse with
           | Ok program -> AST.to_string program = output
           | Error _ -> false)

  let%test "test_boolean_expression" =
    [
      { input = "true;"; output = [ s (AST.ExpressionStmt (e (AST.Boolean true))) ] };
      { input = "false;"; output = [ s (AST.ExpressionStmt (e (AST.Boolean false))) ] };
    ]
    |> run

  let%test "test_if_expression" =
    [
      {
        input = "if (x < y) { x }";
        output =
          [
            s
              (AST.ExpressionStmt
                 (e
                    (AST.If
                       ( e (AST.Infix (e (AST.Identifier "x"), "<", e (AST.Identifier "y"))),
                         s (AST.Block [ s (AST.ExpressionStmt (e (AST.Identifier "x"))) ]),
                         None ))));
          ];
      };
      {
        input = "if (x < y) { x } else { y }";
        output =
          [
            s
              (AST.ExpressionStmt
                 (e
                    (AST.If
                       ( e (AST.Infix (e (AST.Identifier "x"), "<", e (AST.Identifier "y"))),
                         s (AST.Block [ s (AST.ExpressionStmt (e (AST.Identifier "x"))) ]),
                         Some (s (AST.Block [ s (AST.ExpressionStmt (e (AST.Identifier "y"))) ])) ))));
          ];
      };
    ]
    |> run

  let%test "test_function_expression" =
    [
      {
        input = "fn(x, y) { x + y; }";
        output =
          [
            s
              (AST.ExpressionStmt
                 (e
                    (fn_expr
                       [ ("x", None); ("y", None) ]
                       (s
                          (AST.Block
                             [
                               s
                                 (AST.ExpressionStmt
                                    (e (AST.Infix (e (AST.Identifier "x"), "+", e (AST.Identifier "y")))));
                             ])))));
          ];
      };
      { input = "fn() {}"; output = [ s (AST.ExpressionStmt (e (fn_expr [] (s (AST.Block []))))) ] };
      { input = "fn(x) {}"; output = [ s (AST.ExpressionStmt (e (fn_expr [ ("x", None) ] (s (AST.Block []))))) ] };
      {
        input = "fn(foo, bar, baz) {};";
        output =
          [
            s
              (AST.ExpressionStmt (e (fn_expr [ ("foo", None); ("bar", None); ("baz", None) ] (s (AST.Block [])))));
          ];
      };
    ]
    |> run

  let%test "test_call_expressions" =
    [
      {
        input = "add(1, 2 * 3, 4 + 5);";
        output =
          [
            s
              (AST.ExpressionStmt
                 (e
                    (AST.Call
                       ( e (AST.Identifier "add"),
                         [
                           e (AST.Integer 1L);
                           e (AST.Infix (e (AST.Integer 2L), "*", e (AST.Integer 3L)));
                           e (AST.Infix (e (AST.Integer 4L), "+", e (AST.Integer 5L)));
                         ] ))));
          ];
      };
      {
        input = "fn(x, y) { x + y; }(2, 3)";
        output =
          [
            s
              (AST.ExpressionStmt
                 (e
                    (AST.Call
                       ( e
                           (fn_expr
                              [ ("x", None); ("y", None) ]
                              (s
                                 (AST.Block
                                    [
                                      s
                                        (AST.ExpressionStmt
                                           (e (AST.Infix (e (AST.Identifier "x"), "+", e (AST.Identifier "y")))));
                                    ]))),
                         [ e (AST.Integer 2L); e (AST.Integer 3L) ] ))));
          ];
      };
      {
        input = "callsFunction(2, 3, fn(x, y) { x + y; });";
        output =
          [
            s
              (AST.ExpressionStmt
                 (e
                    (AST.Call
                       ( e (AST.Identifier "callsFunction"),
                         [
                           e (AST.Integer 2L);
                           e (AST.Integer 3L);
                           e
                             (fn_expr
                                [ ("x", None); ("y", None) ]
                                (s
                                   (AST.Block
                                      [
                                        s
                                          (AST.ExpressionStmt
                                             (e (AST.Infix (e (AST.Identifier "x"), "+", e (AST.Identifier "y")))));
                                      ])));
                         ] ))));
          ];
      };
    ]
    |> run

  let%test "test_hash_literal" =
    [
      { input = "{}"; output = [ s (AST.ExpressionStmt (e (AST.Hash []))) ] };
      {
        input = "{\"one\": 1, \"two\": 2, \"three\": 3}";
        output =
          [
            s
              (AST.ExpressionStmt
                 (e
                    (AST.Hash
                       [
                         (e (AST.String "one"), e (AST.Integer 1L));
                         (e (AST.String "two"), e (AST.Integer 2L));
                         (e (AST.String "three"), e (AST.Integer 3L));
                       ])));
          ];
      };
      {
        input = "{ \"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15 / 5 }";
        output =
          [
            s
              (AST.ExpressionStmt
                 (e
                    (AST.Hash
                       [
                         (e (AST.String "one"), e (AST.Infix (e (AST.Integer 0L), "+", e (AST.Integer 1L))));
                         (e (AST.String "two"), e (AST.Infix (e (AST.Integer 10L), "-", e (AST.Integer 8L))));
                         (e (AST.String "three"), e (AST.Infix (e (AST.Integer 15L), "/", e (AST.Integer 5L))));
                       ])));
          ];
      };
    ]
    |> run

  let%test "mixed record then hash entry errors deterministically" =
    let contains_substring s sub =
      let len_s = String.length s in
      let len_sub = String.length sub in
      let rec loop i =
        if i + len_sub > len_s then
          false
        else if String.sub s i len_sub = sub then
          true
        else
          loop (i + 1)
      in
      loop 0
    in
    let input = "let x = { a: 1, \"b\": 2 }" in
    match parse input with
    | Ok _ -> false
    | Error errs -> List.exists (fun msg -> contains_substring msg "cannot mix hash-style entries into record literal") errs

  let%test "mixed hash then spread entry errors deterministically" =
    let contains_substring s sub =
      let len_s = String.length s in
      let len_sub = String.length sub in
      let rec loop i =
        if i + len_sub > len_s then
          false
        else if String.sub s i len_sub = sub then
          true
        else
          loop (i + 1)
      in
      loop 0
    in
    let input = "let x = { \"a\": 1, ...rest }" in
    match parse input with
    | Ok _ -> false
    | Error errs -> List.exists (fun msg -> contains_substring msg "cannot mix record-style spread into hash literal") errs

  let%test "mixed hash then record field errors deterministically" =
    let contains_substring s sub =
      let len_s = String.length s in
      let len_sub = String.length sub in
      let rec loop i =
        if i + len_sub > len_s then
          false
        else if String.sub s i len_sub = sub then
          true
        else
          loop (i + 1)
      in
      loop 0
    in
    let input = "let x = { \"a\": 1, b: 2 }" in
    match parse input with
    | Ok _ -> false
    | Error errs -> List.exists (fun msg -> contains_substring msg "cannot mix record-style field into hash literal") errs
end

(* Phase 4.3: Trait definition tests *)
let%test "parse simple trait definition" =
  let input = "trait show[a] { fn show(x: a) -> string }" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TraitDef trait_def ->
              trait_def.name = "show"
              && trait_def.type_param = "a"
              && trait_def.supertraits = []
              && List.length trait_def.methods = 1
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse trait with multiple methods" =
  let input = "trait num[a] { fn add(x: a, y: a) -> a fn sub(x: a, y: a) -> a }" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TraitDef trait_def ->
              trait_def.name = "num" && trait_def.type_param = "a" && List.length trait_def.methods = 2
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse trait with supertraits" =
  let input = "trait ord[a]: eq { fn compare(x: a, y: a) -> int }" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TraitDef trait_def ->
              trait_def.name = "ord"
              && trait_def.type_param = "a"
              && trait_def.supertraits = [ "eq" ]
              && List.length trait_def.methods = 1
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse trait with multiple supertraits" =
  let input = "trait hashable[a]: eq + show { fn hash(x: a) -> int }" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TraitDef trait_def ->
              trait_def.name = "hashable"
              && trait_def.type_param = "a"
              && trait_def.supertraits = [ "eq"; "show" ]
              && List.length trait_def.methods = 1
          | _ -> false)
      | _ -> false)
  | Error _ -> false

(* Phase 4.3: Impl block tests *)
let%test "parse impl - just keyword" =
  let input = "impl" in
  let lexer = Lexer.init input in
  let p = init lexer in
  curr_token_is p Token.Impl

let%test "parse basic impl block - debug" =
  let input = "impl show for int { fn show(x: int) -> string { x } }" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [] ->
          Printf.printf "Empty program\n%!";
          false
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.ImplDef _ ->
              Printf.printf "Got ImplDef!\n%!";
              true
          | _ ->
              Printf.printf "Not ImplDef: %s\n%!" (AST.show_stmt_kind stmt.stmt);
              false)
      | _ ->
          Printf.printf "Multiple statements: %d\n%!" (List.length program);
          false)
  | Error p ->
      Printf.printf "Parse error: %s\n%!" (String.concat "; " p.errors);
      false

let%test "parse basic impl block" =
  let input = "impl show for int { fn show(x: int) -> string { x } }" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.ImplDef impl_def ->
              impl_def.impl_trait_name = "show"
              && impl_def.impl_type_params = []
              && List.length impl_def.impl_methods = 1
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse impl with type params" =
  let input = "impl eq[a: eq] for list[a] { fn eq(x: list[a], y: list[a]) -> bool { true } }" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.ImplDef impl_def ->
              let ok1 = impl_def.impl_trait_name = "eq" in
              let ok2 = List.length impl_def.impl_type_params = 1 in
              let ok3 = List.length impl_def.impl_methods = 1 in
              if not ok1 then
                Printf.printf "trait_name: %s\n%!" impl_def.impl_trait_name;
              if not ok2 then
                Printf.printf "type_params: %d\n%!" (List.length impl_def.impl_type_params);
              if not ok3 then
                Printf.printf "methods: %d\n%!" (List.length impl_def.impl_methods);
              ok1 && ok2 && ok3
          | _ ->
              Printf.printf "Not ImplDef\n%!";
              false)
      | _ ->
          Printf.printf "Wrong program length: %d\n%!" (List.length program);
          false)
  | Error p ->
      Printf.printf "Parse error: %s\n%!" (String.concat "; " p.errors);
      false

let%test "parse impl with multiple constraints" =
  let input = "impl show[a: eq + ord] for option[a] { fn show(x: option[a]) -> string { \"\" } }" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.ImplDef impl_def -> (
              impl_def.impl_trait_name = "show"
              && List.length impl_def.impl_type_params = 1
              &&
              match List.hd impl_def.impl_type_params with
              | { name = "a"; constraints = [ "eq"; "ord" ] } -> true
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse impl with multiple methods" =
  let input = "impl num for int { fn add(x: int, y: int) -> int { x } fn sub(x: int, y: int) -> int { y } }" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.ImplDef impl_def -> impl_def.impl_trait_name = "num" && List.length impl_def.impl_methods = 2
          | _ -> false)
      | _ -> false)
  | Error _ -> false

(* Phase 4.3: Derive statement tests *)
let%test "parse derive - just keyword" =
  let input = "derive" in
  let lexer = Lexer.init input in
  let p = init lexer in
  curr_token_is p Token.Derive

let%test "parse single derive" =
  let input = "derive eq for color" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.DeriveDef derive_def ->
              List.length derive_def.derive_traits = 1
              && (List.hd derive_def.derive_traits).derive_trait_name = "eq"
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse multiple derive traits" =
  let input = "derive eq, show, ord for person" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.DeriveDef derive_def ->
              let trait_names = List.map (fun t -> t.AST.derive_trait_name) derive_def.derive_traits in
              List.length trait_names = 3 && trait_names = [ "eq"; "show"; "ord" ]
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse derive with generic type" =
  let input = "derive eq for option[int]" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.DeriveDef derive_def -> (
              List.length derive_def.derive_traits = 1
              &&
              match derive_def.derive_for_type with
              | AST.TApp ("option", [ AST.TCon "int" ]) -> true
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

(* Phase 4.4: Type alias tests *)
let%test "parse type - just keyword" =
  let input = "type" in
  let lexer = Lexer.init input in
  let p = init lexer in
  curr_token_is p Token.Type

let%test "parse simple type alias" =
  let input = "type point = int" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TypeAlias alias_def -> (
              alias_def.alias_name = "point"
              && alias_def.alias_type_params = []
              &&
              match alias_def.alias_body with
              | AST.TCon "int" -> true
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse type alias followed by let without semicolon" =
  let input = "type myint = int\nlet x: myint = 1\nx" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> List.length program = 3
  | Error _ -> false

let%test "parse type alias with generic param" =
  let input = "type box[a] = a" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TypeAlias alias_def -> (
              alias_def.alias_name = "box"
              && alias_def.alias_type_params = [ "a" ]
              &&
              match alias_def.alias_body with
              | AST.TCon "a" -> true
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse type alias with multiple generic params" =
  let input = "type pair[a, b] = int" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TypeAlias alias_def -> alias_def.alias_name = "pair" && alias_def.alias_type_params = [ "a"; "b" ]
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse record type - empty" =
  let input = "type unit = { }" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TypeAlias alias_def -> (
              match alias_def.alias_body with
              | AST.TRecord (fields, None) -> List.length fields = 0
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse record type - single field" =
  let input = "type point = { x: int }" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TypeAlias alias_def -> (
              match alias_def.alias_body with
              | AST.TRecord (fields, None) -> (
                  List.length fields = 1
                  && (List.hd fields).field_name = "x"
                  &&
                  match (List.hd fields).field_type with
                  | AST.TCon "int" -> true
                  | _ -> false)
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse record type - multiple fields" =
  let input = "type point = { x: int, y: int }" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TypeAlias alias_def -> (
              match alias_def.alias_body with
              | AST.TRecord (fields, None) ->
                  List.length fields = 2
                  && (List.nth fields 0).field_name = "x"
                  && (List.nth fields 1).field_name = "y"
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse record type - with row variable" =
  let input = "type point[r] = { x: int, ...r }" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TypeAlias alias_def -> (
              match alias_def.alias_body with
              | AST.TRecord (fields, Some (AST.TCon "r")) ->
                  List.length fields = 1 && (List.hd fields).field_name = "x"
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse record type - only row variable" =
  let input = "type any[r] = { ...r }" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TypeAlias alias_def -> (
              match alias_def.alias_body with
              | AST.TRecord ([], Some (AST.TCon "r")) -> true
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

(* Phase 4.6: Record literal tests *)
let%test "parse empty record literal with spread" =
  let input = "let x = { ...base }" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.Let { value; _ } -> (
              match value.expr with
              | AST.RecordLit (fields, Some _) -> List.length fields = 0
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse record literal - single field" =
  let input = "let p = { x: 10 }" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.Let { value; _ } -> (
              match value.expr with
              | AST.RecordLit (fields, None) -> (
                  List.length fields = 1
                  && (List.hd fields).field_name = "x"
                  &&
                  match (List.hd fields).field_value with
                  | Some _ -> true
                  | None -> false)
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse record literal - multiple fields" =
  let input = "let p = { x: 10, y: 20 }" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.Let { value; _ } -> (
              match value.expr with
              | AST.RecordLit (fields, None) ->
                  List.length fields = 2
                  && (List.nth fields 0).field_name = "x"
                  && (List.nth fields 1).field_name = "y"
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse let with record literal followed by let" =
  let input = "let p = { x: 1, y: 2 }\nlet q = p.x\nq" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> List.length program = 3
  | Error _ -> false

let%test "parse record literal - punning single" =
  let input = "let p = { x: }" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.Let { value; _ } -> (
              match value.expr with
              | AST.RecordLit (fields, None) -> (
                  List.length fields = 1
                  && (List.hd fields).field_name = "x"
                  &&
                  match (List.hd fields).field_value with
                  | None -> true
                  | Some _ -> false)
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse record literal - punning multiple" =
  let input = "let p = { x:, y: }" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.Let { value; _ } -> (
              match value.expr with
              | AST.RecordLit (fields, None) -> (
                  List.length fields = 2
                  && (List.nth fields 0).field_name = "x"
                  && (List.nth fields 1).field_name = "y"
                  && (match (List.nth fields 0).field_value with
                     | None -> true
                     | _ -> false)
                  &&
                  match (List.nth fields 1).field_value with
                  | None -> true
                  | _ -> false)
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse record literal - spread only" =
  let input = "let p = { ...base }" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.Let { value; _ } -> (
              match value.expr with
              | AST.RecordLit (fields, Some _) -> List.length fields = 0
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse record literal - spread with fields after" =
  let input = "let p = { ...base, x: 10 }" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.Let { value; _ } -> (
              match value.expr with
              | AST.RecordLit (fields, Some _) -> List.length fields = 1 && (List.hd fields).field_name = "x"
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse record literal - fields with spread in middle" =
  let input = "let p = { x: 10, ...base, y: 20 }" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.Let { value; _ } -> (
              match value.expr with
              | AST.RecordLit (fields, Some _) ->
                  (* Should have x before spread, and y after spread *)
                  List.length fields = 2
                  && (List.nth fields 0).field_name = "x"
                  && (List.nth fields 1).field_name = "y"
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse hash literal still works" =
  let input = "let h = { \"x\": 10 }" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.Let { value; _ } -> (
              match value.expr with
              | AST.Hash _ -> true
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

(* Phase 4.7: Field access tests *)
let%test "parse field access" =
  let input = "let x = r.field" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.Let { value; _ } -> (
              match value.expr with
              | AST.FieldAccess (receiver, field) -> (
                  field = "field"
                  &&
                  match receiver.expr with
                  | AST.Identifier "r" -> true
                  | _ -> false)
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse field access - chained" =
  let input = "let x = r.a.b.c" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.Let { value; _ } -> (
              match value.expr with
              | AST.FieldAccess (inner, "c") -> (
                  match inner.expr with
                  | AST.FieldAccess (inner2, "b") -> (
                      match inner2.expr with
                      | AST.FieldAccess (receiver, "a") -> (
                          match receiver.expr with
                          | AST.Identifier "r" -> true
                          | _ -> false)
                      | _ -> false)
                  | _ -> false)
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse field access vs method call" =
  let input = "let x = r.field; let y = r.method()" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt1; stmt2 ] -> (
          match (stmt1.stmt, stmt2.stmt) with
          | AST.Let { name = "x"; value = v1; _ }, AST.Let { name = "y"; value = v2; _ } -> (
              match (v1.expr, v2.expr) with
              | AST.FieldAccess (_, "field"), AST.MethodCall (_, "method", []) -> true
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

(* Phase 4.8: Record pattern tests *)
let%test "parse record pattern - simple punning" =
  let input = "match p { { x:, y: }: x }" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.ExpressionStmt expr -> (
              match expr.expr with
              | AST.Match (_, arms) -> (
                  match arms with
                  | [ arm ] -> (
                      match arm.patterns with
                      | [ pattern ] -> (
                          match pattern.pat with
                          | AST.PRecord (fields, None) ->
                              List.length fields = 2
                              && (List.nth fields 0).pat_field_name = "x"
                              && (List.nth fields 0).pat_field_pattern = None
                              && (List.nth fields 1).pat_field_name = "y"
                              && (List.nth fields 1).pat_field_pattern = None
                          | _ -> false)
                      | _ -> false)
                  | _ -> false)
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse record pattern - with nested patterns" =
  let input = "match p { { x: a, y: b }: a }" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.ExpressionStmt expr -> (
              match expr.expr with
              | AST.Match (_, arms) -> (
                  match arms with
                  | [ arm ] -> (
                      match arm.patterns with
                      | [ pattern ] -> (
                          match pattern.pat with
                          | AST.PRecord (fields, None) -> (
                              List.length fields = 2
                              && (List.nth fields 0).pat_field_name = "x"
                              &&
                              match (List.nth fields 0).pat_field_pattern with
                              | Some p -> (
                                  match p.pat with
                                  | AST.PVariable "a" -> true
                                  | _ -> false)
                              | None -> false)
                          | _ -> false)
                      | _ -> false)
                  | _ -> false)
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse record pattern - with rest" =
  let input = "match p { { x:, ...rest }: x }" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.ExpressionStmt expr -> (
              match expr.expr with
              | AST.Match (_, arms) -> (
                  match arms with
                  | [ arm ] -> (
                      match arm.patterns with
                      | [ pattern ] -> (
                          match pattern.pat with
                          | AST.PRecord (fields, Some "rest") ->
                              List.length fields = 1 && (List.hd fields).pat_field_name = "x"
                          | _ -> false)
                      | _ -> false)
                  | _ -> false)
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse record pattern - empty" =
  let input = "match p { { }: 42 }" in
  let lexer = Lexer.init input in
  match parse_program (init lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.ExpressionStmt expr -> (
              match expr.expr with
              | AST.Match (_, arms) -> (
                  match arms with
                  | [ arm ] -> (
                      match arm.patterns with
                      | [ pattern ] -> (
                          match pattern.pat with
                          | AST.PRecord ([], None) -> true
                          | _ -> false)
                      | _ -> false)
                  | _ -> false)
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false
