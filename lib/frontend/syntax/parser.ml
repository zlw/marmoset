open Ast
open Surface_ast
module Diagnostic = Diagnostics.Diagnostic
module String_utils = Diagnostics.String_utils

let ( let* ) res f = Result.bind res f

(* Surface AST node constructors *)
let mk_surface_expr id pos kind : Surface.surface_expr =
  Surface.{ se_id = id; se_expr = kind; se_pos = pos; se_end_pos = pos; se_file_id = None }

let mk_surface_stmt pos kind : Surface.surface_stmt =
  Surface.{ ss_stmt = kind; ss_pos = pos; ss_end_pos = pos; ss_file_id = None }

let mk_surface_pat pos kind : Surface.surface_pattern =
  Surface.{ sp_pat = kind; sp_pos = pos; sp_end_pos = pos; sp_file_id = None }

let token_end (t : Token.token) : int =
  let len = String.length t.literal in
  match t.token_type with
  | Token.EOF -> t.pos
  | Token.String ->
      (* Token literal stores the inner string content; include both quotes. *)
      t.pos + len + 1
  | _ ->
      if len = 0 then
        t.pos
      else
        t.pos + len - 1

type parser = {
  lexer : Lexer.lexer;
  curr_token : Token.token;
  peek_token : Token.token;
  errors : errors;
  id_supply : Id_supply.Id_supply.t;
  file_id : string;
}

and errors = Diagnostic.t list

type brace_literal_mode =
  | RecordMode
  | HashMode

let first_some a b =
  match a with
  | Some _ -> a
  | None -> b

let with_surface_expr_end p (e : Surface.surface_expr) : Surface.surface_expr =
  Surface.
    {
      e with
      se_end_pos = max e.se_end_pos (token_end p.curr_token);
      se_file_id = first_some e.se_file_id (Some p.file_id);
    }

let with_surface_stmt_end p (s : Surface.surface_stmt) : Surface.surface_stmt =
  Surface.
    {
      s with
      ss_end_pos = max s.ss_end_pos (token_end p.curr_token);
      ss_file_id = first_some s.ss_file_id (Some p.file_id);
    }

let with_surface_pat_end p (pat : Surface.surface_pattern) : Surface.surface_pattern =
  Surface.
    {
      pat with
      sp_end_pos = max pat.sp_end_pos (token_end p.curr_token);
      sp_file_id = first_some pat.sp_file_id (Some p.file_id);
    }

let tokens_are_on_different_lines (p : parser) : bool =
  let start_pos = token_end p.curr_token + 1 in
  let end_pos = p.peek_token.pos - 1 in
  let rec loop i =
    if i > end_pos then
      false
    else
      match String.get p.lexer.input i with
      | '\n' | '\r' -> true
      | _ -> loop (i + 1)
  in
  start_pos <= end_pos && loop start_pos

let token_starts_line (p : parser) (t : Token.token) : bool =
  let rec loop i =
    if i < 0 then
      true
    else
      match String.get p.lexer.input i with
      | ' ' | '\t' -> loop (i - 1)
      | '\n' | '\r' -> true
      | _ -> false
  in
  loop (t.pos - 1)

let is_upper_ident (name : string) : bool =
  String.length name > 0
  &&
  match String.get name 0 with
  | 'A' .. 'Z' -> true
  | _ -> false

(* Helper to get fresh ID using the mutable id_supply *)
let fresh_id (p : parser) : int = Id_supply.Id_supply.fresh p.id_supply

type precedence = int

let prec_lowest = 1
let prec_or = 2 (* || *)
let prec_and = 3 (* && *)
let prec_equals = 4 (* == != is *)
let prec_less_greater = 5 (* < > <= >= *)
let prec_sum = 6 (* + - *)
let prec_product = 7 (* * / % *)
let prec_prefix = 8 (* ! - (prefix) *)
let prec_call = 9 (* f(args) *)
let prec_index = 10 (* a[i] a.b *)

let precedences = function
  | Token.PipePipe -> prec_or
  | Token.AmpAmp -> prec_and
  | Token.Eq | Token.NotEq | Token.Is -> prec_equals
  | Token.Lt | Token.Gt | Token.Le | Token.Ge -> prec_less_greater
  | Token.Plus | Token.Minus -> prec_sum
  | Token.Asterisk | Token.Slash | Token.Percent -> prec_product
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

let init ~file_id (l : Lexer.lexer) : parser =
  {
    lexer = l;
    curr_token = Token.init Illegal "";
    peek_token = Token.init Illegal "";
    errors = [];
    id_supply = Id_supply.Id_supply.create 0;
    file_id;
  }
  |> next_token
  |> next_token

let curr_token_is (p : parser) (t : Token.token_type) : bool = p.curr_token.token_type = t
let peek_token_is (p : parser) (t : Token.token_type) : bool = p.peek_token.token_type = t

let span_for_token (p : parser) (tok : Token.token) : Diagnostic.span =
  Diagnostic.Span { file_id = p.file_id; start_pos = tok.pos; end_pos = Some (max tok.pos (token_end tok)) }

let add_error ?(code = "parse-unexpected-token") ?token (p : parser) (msg : string) : parser =
  let tok =
    match token with
    | Some tok -> tok
    | None -> p.curr_token
  in
  let diag =
    Diagnostic.make ~code ~severity:Diagnostic.Error ~message:msg
      ~labels:[ Diagnostic.primary_label (span_for_token p tok) ]
      ()
  in
  { p with errors = diag :: p.errors }

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
  add_error ~code:"parse-expected-token" ~token:p.peek_token p msg

let expect_peek (p : parser) (tt : Token.token_type) : (parser, parser) result =
  if peek_token_is p tt then
    Ok (next_token p)
  else
    Error (peek_error p tt)

let no_prefix_parse_fn_error (p : parser) (t : Token.token_type) : parser =
  let msg = Printf.sprintf "syntax error, unexpected %s found" (Token.show_token_type t) in
  add_error ~code:"parse-unexpected-token" p msg

let rec parse_surface_program (p : parser) : (parser * Surface.surface_program, parser) result =
  let rec loop (lp : parser) (prog : Surface.surface_program) =
    if curr_token_is lp Token.EOF then
      Ok (lp, List.rev prog)
    else
      let start_pos = lp.curr_token.pos in
      let start_file_id = Some lp.file_id in
      let* lp2, decl = parse_top_decl lp in
      let lp3 =
        match decl with
        | Surface.STypeDef _ | Surface.SShapeDef _ ->
            (* Declaration parsers leave us at the token after the body. *)
            if curr_token_is lp2 Token.Semicolon then
              next_token lp2
            else
              lp2
        | _ -> next_token lp2
      in
      let end_pos = max start_pos (token_end lp2.curr_token) in
      let ts =
        Surface.{ std_decl = decl; std_pos = start_pos; std_end_pos = end_pos; std_file_id = start_file_id }
      in
      loop lp3 (ts :: prog)
  in
  loop p []

and parse_top_decl (p : parser) : (parser * Surface.top_decl, parser) result =
  match p.curr_token.token_type with
  | Token.Let -> parse_let_top p
  | Token.Return -> parse_return_top p
  | Token.Enum -> parse_enum_definition p
  | Token.Trait -> parse_trait_definition p
  | Token.Impl -> parse_impl_definition p
  | Token.Type -> parse_type_definition p
  | Token.Shape -> parse_shape_definition p
  | Token.Function when peek_token_is p Token.Ident -> parse_fn_decl_top p
  | _ -> parse_expression_top p

and parse_let_top (p : parser) : (parser * Surface.top_decl, parser) result =
  let* p2, ss = parse_let_statement p in
  match ss.Surface.ss_stmt with
  | Surface.SSLet { ss_name = name; ss_value = value; ss_type_annotation = type_annotation } ->
      Ok (p2, Surface.SLet { name; value; type_annotation })
  | _ -> assert false

and parse_return_top (p : parser) : (parser * Surface.top_decl, parser) result =
  let* p2, ss = parse_return_statement p in
  match ss.Surface.ss_stmt with
  | Surface.SSReturn e -> Ok (p2, Surface.SReturn e)
  | _ -> assert false

and parse_expression_top (p : parser) : (parser * Surface.top_decl, parser) result =
  let* p2, expr = parse_expression p prec_lowest in
  let p3 = skip p2 Token.Semicolon in
  Ok (p3, Surface.SExpressionStmt expr)

(* Phase 1b: vNext top-level fn declaration: fn name[generics](params) -> T = expr_or_block *)
and parse_fn_decl_top (p : parser) : (parser * Surface.top_decl, parser) result =
  (* curr_token = 'fn', peek = Ident *)
  let* p2 = expect_peek p Token.Ident in
  let name = p2.curr_token.literal in

  (* Optional generics: [a: Show, b] *)
  let* p3, generics = parse_generic_params p2 in
  let* p4 = expect_peek p3 Token.LParen in

  (* Parameters with optional type annotations *)
  let* p5, params = parse_function_parameters p4 in

  (* Optional return type: -> T (pure) or => T (effectful) *)
  let* p6, return_type, is_effectful =
    if peek_token_is p5 Token.Arrow then
      let p6 = next_token p5 in
      let* p7, te = parse_type_expr (next_token p6) in
      Ok (p7, Some te, false)
    else if peek_token_is p5 Token.FatArrow then
      let p6 = next_token p5 in
      let* p7, te = parse_type_expr (next_token p6) in
      Ok (p7, Some te, true)
    else
      Ok (p5, None, false)
  in

  (* Expect = *)
  let* p7 =
    if curr_token_is p6 Token.Assign then
      Ok p6
    else if peek_token_is p6 Token.Assign then
      Ok (next_token p6)
    else
      expect_peek p6 Token.Assign
  in

  (* Parse body: { block } or expression *)
  let p_body = next_token p7 in
  let* p8, body =
    if curr_token_is p_body Token.LBrace && is_block_body_start p_body then
      let* p8, block = parse_block_statement p_body in
      let* p9 =
        if curr_token_is p8 Token.RBrace then
          Ok p8
        else
          expect_peek p8 Token.RBrace
      in
      Ok (p9, Surface.SEOBBlock block)
    else
      let* p8, expr = parse_expression p_body prec_lowest in
      let p9 = skip p8 Token.Semicolon in
      Ok (p9, Surface.SEOBExpr expr)
  in

  Ok (p8, Surface.SFnDecl { name; generics; params; return_type; is_effectful; body })

and parse_block_stmt (p : parser) : (parser * Surface.surface_stmt, parser) result =
  let finalize = function
    | Ok (lp, stmt) -> Ok (lp, with_surface_stmt_end lp stmt)
    | Error _ as err -> err
  in
  (match p.curr_token.token_type with
  | Token.Let -> parse_let_statement p
  | Token.Return -> parse_return_statement p
  | _ -> parse_expression_stmt p)
  |> finalize

(* Phase 2: Type expression parsing *)

(* Parse a single type atom (not union) *)
and parse_type_atom (p : parser) : (parser * Surface.surface_type_expr, parser) result =
  if curr_token_is p Token.Ident then
    let ident = p.curr_token.literal in
    let p2 = next_token p in
    if ident = "Dyn" && curr_token_is p2 Token.LBracket then
      let* p3, traits = parse_constraint_list (next_token p2) in
      if curr_token_is p3 Token.RBracket then
        Ok (next_token p3, Surface.STTraitObject traits)
      else
        Error (peek_error p3 Token.RBracket)
      (* Check for generic application: List[Int], Map[Str, Int], etc. *)
    else if curr_token_is p2 Token.LBracket then
      let* p3, type_args = parse_type_expr_list (next_token p2) in
      if curr_token_is p3 Token.RBracket then
        Ok (next_token p3, Surface.STApp (ident, type_args))
      else
        Error (peek_error p3 Token.RBracket)
    else
      Ok (p2, Surface.STCon ident)
  else if curr_token_is p Token.LParen then
    (* Parenthesized type or function type: (Int | Str) or (Int, Str) -> Bool *)
    let* p2, first = parse_type_expr (next_token p) in
    if curr_token_is p2 Token.Comma then
      (* Multiple params: (Int, Str) -> Bool *)
      let rec collect_params lp rev_params =
        let* lp2, param_type = parse_type_expr (next_token lp) in
        let rev_params = param_type :: rev_params in
        if curr_token_is lp2 Token.Comma then
          collect_params lp2 rev_params
        else if curr_token_is lp2 Token.RParen then
          Ok (lp2, List.rev rev_params)
        else
          Error (peek_error lp2 Token.RParen)
      in
      let* p3, params = collect_params p2 [ first ] in
      let p4 = next_token p3 in
      let is_effectful = curr_token_is p4 Token.FatArrow in
      if curr_token_is p4 Token.Arrow || is_effectful then
        let* p5, return_type = parse_type_expr (next_token p4) in
        Ok (p5, Surface.STArrow (params, return_type, is_effectful))
      else
        Error (peek_error p3 Token.Arrow)
    else if curr_token_is p2 Token.RParen then
      (* Single type in parens: (Int) or (Int | Str) *)
      let p3 = next_token p2 in
      if curr_token_is p3 Token.Arrow || curr_token_is p3 Token.FatArrow then
        let is_effectful = curr_token_is p3 Token.FatArrow in
        let* p5, return_type = parse_type_expr (next_token p3) in
        Ok (p5, Surface.STArrow ([ first ], return_type, is_effectful))
      else
        (* Just grouping: (Int) or (Int | Str) *)
        Ok (p3, first)
    else
      Error (peek_error p2 Token.RParen)
  else if curr_token_is p Token.LBrace then
    (* Record type: { x: Int, y: Str } or { x: Int, ...r } *)
    parse_record_type p
  else
    Error (no_prefix_parse_fn_error p p.curr_token.token_type)

(* Parse a type intersection, including A & B & C *)
and parse_type_intersection (p : parser) : (parser * Surface.surface_type_expr, parser) result =
  let* p2, first_type = parse_type_atom p in
  if curr_token_is p2 Token.Ampersand then
    let rec collect_intersection_members lp rev_members =
      let* lp2, next_type = parse_type_atom (next_token lp) in
      let rev_members = next_type :: rev_members in
      if curr_token_is lp2 Token.Ampersand then
        collect_intersection_members lp2 rev_members
      else
        Ok (lp2, Surface.STIntersection (List.rev rev_members))
    in
    collect_intersection_members p2 [ first_type ]
  else
    Ok (p2, first_type)

(* Parse a type expression, including unions: Int | Str | Bool *)
and parse_type_expr (p : parser) : (parser * Surface.surface_type_expr, parser) result =
  let* p2, first_type = parse_type_intersection p in
  (* Check for union: type | type | type ... *)
  if curr_token_is p2 Token.Pipe then
    let rec collect_union_members lp rev_members =
      let* lp2, next_type = parse_type_intersection (next_token lp) in
      let rev_members = next_type :: rev_members in
      if curr_token_is lp2 Token.Pipe then
        collect_union_members lp2 rev_members
      else
        Ok (lp2, Surface.STUnion (List.rev rev_members))
    in
    collect_union_members p2 [ first_type ]
  else
    Ok (p2, first_type)

and parse_type_expr_list (p : parser) : (parser * Surface.surface_type_expr list, parser) result =
  if curr_token_is p Token.RParen || curr_token_is p Token.RBracket then
    Ok (p, [])
  else
    let rec loop (lp : parser) (types : Surface.surface_type_expr list) =
      let* lp2, te = parse_type_expr lp in
      if curr_token_is lp2 Token.Comma then
        loop (next_token lp2) (te :: types)
      else
        Ok (lp2, List.rev (te :: types))
    in
    loop p []

and parse_param_type_expr (p : parser) : (parser * Surface.surface_type_expr, parser) result =
  let rec try_constraint_chain (lp : parser) (rev_traits : string list) :
      (parser * Surface.surface_type_expr, parser) result option =
    if curr_token_is lp Token.Ident then
      let trait_name = lp.curr_token.literal in
      let lp2 = next_token lp in
      if curr_token_is lp2 Token.Ampersand then
        match try_constraint_chain (next_token lp2) (trait_name :: rev_traits) with
        | Some result -> Some result
        | None -> None
      else if curr_token_is lp2 Token.Comma || curr_token_is lp2 Token.RParen then
        match rev_traits with
        | [] -> None
        | _ -> Some (Ok (lp2, Surface.STConstraintShorthand (List.rev (trait_name :: rev_traits))))
      else
        None
    else
      None
  in
  match try_constraint_chain p [] with
  | Some result -> result
  | None -> parse_type_expr p

and parse_trait_constraint (p : parser) : (parser * string list, parser) result =
  (* Parse trait constraints: Eq, Show, Eq & Show, etc. *)
  let rec loop (lp : parser) (traits : string list) =
    if curr_token_is lp Token.Ident then
      let trait = lp.curr_token.literal in
      let lp2 = next_token lp in
      if curr_token_is lp2 Token.Ampersand then
        loop (next_token lp2) (trait :: traits)
      else
        Ok (lp2, List.rev (trait :: traits))
    else
      Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
  in
  loop p []

and parse_generic_params (p : parser) : (parser * AST.generic_param list option, parser) result =
  (* Parse generic parameters: [a], [a: Show], [a: Show & Eq, b: Eq], etc. *)
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

(* Phase 4.5: Parse record type: { x: Int, y: Str, ...r } *)
and parse_record_type (p : parser) : (parser * Surface.surface_type_expr, parser) result =
  (* Current token is { *)
  let p2 = next_token p in

  (* Parse fields and optional row variable *)
  let rec parse_fields lp fields =
    if curr_token_is lp Token.RBrace then
      (* Empty record or end of fields: { } *)
      Ok (next_token lp, Surface.STRecord (List.rev fields, None))
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
      Ok (next_token lp4, Surface.STRecord (List.rev fields, Some row_var))
    else if curr_token_is lp Token.Ident then
      (* Field: field_name: type *)
      let sf_name = lp.curr_token.literal in
      let* lp2 = expect_peek lp Token.Colon in
      let lp2 = next_token lp2 in
      let* lp3, sf_type = parse_type_expr lp2 in
      let field = Surface.{ sf_name; sf_type } in
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

and parse_let_statement (p : parser) : (parser * Surface.surface_stmt, parser) result =
  let pos = p.curr_token.pos in
  let* p2 = expect_peek p Token.Ident in
  let ss_name = p2.curr_token.literal in
  (* Phase 2: Parse optional type annotation *)
  let* p3, ss_type_annotation =
    if peek_token_is p2 Token.Colon then
      let p_colon = next_token p2 in
      (* Move to : *)
      let p_type_start = next_token p_colon in
      (* Move to type identifier *)
      let* p_type_end, te = parse_type_expr p_type_start in
      Ok (p_type_end, Some te)
    else
      Ok (p2, None)
  in
  let* p4 =
    match ss_type_annotation with
    | Some _ ->
        if curr_token_is p3 Token.Assign then
          Ok p3
        else if peek_token_is p3 Token.Assign then
          Ok (next_token p3)
        else
          Error (peek_error p3 Token.Assign)
    | None -> expect_peek p3 Token.Assign
  in
  let* p5, ss_value = parse_expression (next_token p4) prec_lowest in
  let p6 = skip p5 Token.Semicolon in
  Ok (p6, mk_surface_stmt pos (Surface.SSLet { ss_name; ss_value; ss_type_annotation }))

and parse_return_statement (p : parser) : (parser * Surface.surface_stmt, parser) result =
  let pos = p.curr_token.pos in
  let* p2, expr = parse_expression (next_token p) prec_lowest in
  let p3 = skip p2 Token.Semicolon in
  Ok (p3, mk_surface_stmt pos (Surface.SSReturn expr))

(* Phase 4.2: Parse enum definition *)
and parse_enum_definition (p : parser) : (parser * Surface.top_decl, parser) result =
  (* Current token is 'enum' *)
  let* p2 = expect_peek p Token.Ident in
  let name = p2.curr_token.literal in

  (* Parse optional type parameters: [a, b] *)
  let* p3, type_params =
    if peek_token_is p2 Token.LBracket then
      parse_type_param_list (next_token (next_token p2))
    else
      Ok (next_token p2, [])
  in

  (* Expect: = { *)
  let* p4 =
    if curr_token_is p3 Token.Assign then
      Ok p3
    else
      expect_peek p3 Token.Assign
  in
  let p_body = next_token p4 in
  let* p5, variants = parse_sum_type_body p_body in
  let* p6, derive = parse_postfix_derive p5 in
  Ok
    ( p6,
      Surface.STypeDef
        { type_name = name; type_type_params = type_params; type_body = Surface.STNamedSum variants; derive } )

and parse_type_param_list (p : parser) : (parser * string list, parser) result =
  let rec loop lp rev_params =
    if curr_token_is lp Token.Ident then
      let param = lp.curr_token.literal in
      let lp2 = next_token lp in
      if curr_token_is lp2 Token.Comma then
        loop (next_token lp2) (param :: rev_params)
      else if curr_token_is lp2 Token.RBracket then
        Ok (next_token lp2, List.rev (param :: rev_params))
      else
        Error (peek_error lp2 Token.RBracket)
    else if curr_token_is lp Token.RBracket then
      Ok (next_token lp, List.rev rev_params)
    else
      Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
  in
  loop p []

and parse_variant_list (p : parser) : (parser * Surface.surface_variant_def list, parser) result =
  let rec loop lp variants =
    if curr_token_is lp Token.RBrace then
      Ok (lp, List.rev variants)
    else if curr_token_is lp Token.Ident then
      let sv_name = lp.curr_token.literal in
      let lp2 = next_token lp in
      (* Check for variant data: some(a) *)
      let* lp3, sv_fields =
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
      let variant = Surface.{ sv_name; sv_fields } in
      (* Skip optional comma between variants *)
      let lp4 =
        if curr_token_is lp3 Token.Comma then
          next_token lp3
        else
          lp3
      in
      loop lp4 (variant :: variants)
    else
      Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
  in
  loop p []

and parse_sum_type_body (p : parser) : (parser * Surface.surface_variant_def list, parser) result =
  if curr_token_is p Token.LBrace then
    let* p2, variants = parse_variant_list (next_token p) in
    if curr_token_is p2 Token.RBrace then
      Ok (next_token p2, variants)
    else
      Error (peek_error p2 Token.RBrace)
  else
    Error (peek_error p Token.LBrace)

and parse_postfix_derive (p : parser) : (parser * AST.derive_trait list, parser) result =
  if curr_token_is p Token.Derive && not (token_starts_line p p.curr_token) then
    let* p2, traits = parse_derive_trait_list (next_token p) in
    Ok (p2, traits)
  else if peek_token_is p Token.Derive && not (token_starts_line p p.peek_token) then
    let p2 = next_token p in
    let* p3, traits = parse_derive_trait_list (next_token p2) in
    Ok (p3, traits)
  else
    Ok (p, [])

and parse_named_product_field_list (p : parser) : (parser * Surface.surface_record_type_field list, parser) result
    =
  let rec loop lp rev_fields =
    if curr_token_is lp Token.RBrace then
      Ok (lp, List.rev rev_fields)
    else if curr_token_is lp Token.Ident then
      let sf_name = lp.curr_token.literal in
      let* lp2 = expect_peek lp Token.Colon in
      let* lp3, sf_type = parse_type_expr (next_token lp2) in
      let next_lp =
        if curr_token_is lp3 Token.Comma then
          next_token lp3
        else
          lp3
      in
      loop next_lp (Surface.{ sf_name; sf_type } :: rev_fields)
    else
      Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
  in
  loop p []

and parse_shape_definition (p : parser) : (parser * Surface.top_decl, parser) result =
  let* p2 = expect_peek p Token.Ident in
  let shape_name = p2.curr_token.literal in
  let* p3, shape_type_params =
    if peek_token_is p2 Token.LBracket then
      parse_type_param_list (next_token (next_token p2))
    else
      Ok (next_token p2, [])
  in
  let* p4 =
    if curr_token_is p3 Token.Assign then
      Ok p3
    else
      expect_peek p3 Token.Assign
  in
  let* p5 =
    if curr_token_is p4 Token.LBrace then
      Ok p4
    else if peek_token_is p4 Token.LBrace then
      Ok (next_token p4)
    else
      expect_peek p4 Token.LBrace
  in
  let* p6, shape_fields = parse_named_product_field_list (next_token p5) in
  let* p7 =
    if curr_token_is p6 Token.RBrace then
      Ok (next_token p6)
    else
      expect_peek p6 Token.RBrace
  in
  Ok (p7, Surface.SShapeDef { shape_name; shape_type_params; shape_fields })

and parse_type_definition (p : parser) : (parser * Surface.top_decl, parser) result =
  let* p2 = expect_peek p Token.Ident in
  let type_name = p2.curr_token.literal in
  let* p3, type_type_params =
    if peek_token_is p2 Token.LBracket then
      parse_type_param_list (next_token (next_token p2))
    else
      Ok (next_token p2, [])
  in
  let* p4 =
    if curr_token_is p3 Token.Assign then
      Ok p3
    else
      expect_peek p3 Token.Assign
  in
  let p_body = next_token p4 in
  if
    curr_token_is p_body Token.Ident && p_body.curr_token.literal = type_name && peek_token_is p_body Token.LParen
  then
    let p_open = next_token p_body in
    let* p_payload_end, payload_types = parse_type_expr_list (next_token p_open) in
    let* p_after_paren =
      if curr_token_is p_payload_end Token.RParen then
        Ok (next_token p_payload_end)
      else
        expect_peek p_payload_end Token.RParen
    in
    match payload_types with
    | [ wrapper_body ] ->
        let* p6, derive = parse_postfix_derive p_after_paren in
        Ok
          ( p6,
            Surface.STypeDef
              { type_name; type_type_params; type_body = Surface.STNamedWrapper wrapper_body; derive } )
    | _ ->
        Error
          (add_error ~code:"parse-invalid-type-definition" p_body
             (Printf.sprintf "Wrapper type '%s' expects exactly one payload type" type_name))
  else if curr_token_is p_body Token.LBrace then
    let p_members = next_token p_body in
    if
      curr_token_is p_members Token.RBrace
      || curr_token_is p_members Token.Spread
      || (curr_token_is p_members Token.Ident && peek_token_is p_members Token.Colon)
    then
      let* p5, record_body = parse_type_expr p_body in
      let* p6, derive = parse_postfix_derive p5 in
      Ok
        ( p6,
          Surface.STypeDef { type_name; type_type_params; type_body = Surface.STTransparent record_body; derive }
        )
    else
      let* p5, variants = parse_sum_type_body p_body in
      let* p6, derive = parse_postfix_derive p5 in
      Ok (p6, Surface.STypeDef { type_name; type_type_params; type_body = Surface.STNamedSum variants; derive })
  else
    let* p5, transparent_body = parse_type_expr p_body in
    let* p6, derive = parse_postfix_derive p5 in
    Ok
      ( p6,
        Surface.STypeDef
          { type_name; type_type_params; type_body = Surface.STTransparent transparent_body; derive } )

(* Phase 4.3: Trait definition parsing *)
and parse_trait_definition (p : parser) : (parser * Surface.top_decl, parser) result =
  (* Current token is 'trait' *)
  let* p2 = expect_peek p Token.Ident in
  let name = p2.curr_token.literal in

  (* Parse optional type parameter: [a] *)
  let* p3, type_param =
    if peek_token_is p2 Token.LBracket then
      let* p3 = expect_peek p2 Token.LBracket in
      let* p4 = expect_peek p3 Token.Ident in
      let type_param = Some p4.curr_token.literal in
      let* p5 = expect_peek p4 Token.RBracket in
      Ok (p5, type_param)
    else
      Ok (next_token p2, None)
  in

  (* Parse optional supertraits: : Eq or : Eq & Show *)
  let* p4, supertraits =
    if curr_token_is p3 Token.Colon then
      parse_supertrait_list (next_token p3)
    else if peek_token_is p3 Token.Colon then
      parse_supertrait_list (next_token (next_token p3))
    else
      Ok (p3, [])
  in

  (* Expect: = { *)
  let* p5 =
    if curr_token_is p4 Token.Assign then
      Ok p4
    else
      expect_peek p4 Token.Assign
  in
  let* p6 =
    if peek_token_is p5 Token.LBrace then
      Ok (next_token p5)
    else
      expect_peek p5 Token.LBrace
  in

  (* Parse trait members: method signatures only. *)
  let* p7, methods = parse_trait_member_list (next_token p6) in

  (* Expect closing brace *)
  let* p8 =
    if curr_token_is p7 Token.RBrace then
      Ok p7
    else
      expect_peek p7 Token.RBrace
  in

  Ok (p8, Surface.STraitDef { name; type_param; supertraits; methods })

and parse_supertrait_list (p : parser) : (parser * string list, parser) result =
  let rec loop lp rev_traits =
    if curr_token_is lp Token.Ident then
      let trait_name = lp.curr_token.literal in
      let lp2 = next_token lp in
      if curr_token_is lp2 Token.Ampersand then
        (* More supertraits: & trait *)
        loop (next_token lp2) (trait_name :: rev_traits)
      else
        (* End of supertrait list *)
        Ok (lp2, List.rev (trait_name :: rev_traits))
    else
      Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
  in
  loop p []

and parse_trait_member_list (p : parser) : (parser * Surface.surface_method_sig list, parser) result =
  let rec loop lp methods =
    if curr_token_is lp Token.RBrace then
      Ok (lp, List.rev methods)
    else if curr_token_is lp Token.Function then
      let* lp2, method_sig = parse_method_sig lp in
      loop lp2 (method_sig :: methods)
    else if curr_token_is lp Token.Ident && peek_token_is lp Token.Colon then
      Error
        (add_error ~code:"parse-invalid-trait-member" lp
           "Trait bodies are method-only; use 'shape' for structural field contracts")
    else if curr_token_is lp Token.EOF then
      Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
    else
      Error (add_error ~code:"parse-invalid-trait-member" lp "Trait members must start with 'fn'")
  in
  loop p []

and parse_method_sig (p : parser) : (parser * Surface.surface_method_sig, parser) result =
  (* Current token is 'fn' *)
  let sm_id = fresh_id p in
  let* p2 = expect_peek p Token.Ident in
  let sm_name = p2.curr_token.literal in

  (* Parse optional method-level generics [a, b: show] *)
  let* p3, sm_generics = parse_generic_params p2 in
  let p3 = next_token p3 in

  (* Expect opening paren *)
  let* p3 =
    if curr_token_is p3 Token.LParen then
      Ok p3
    else
      expect_peek p3 Token.LParen
  in

  (* Parse parameter list (name: type pairs) *)
  let* p4, params = parse_param_list_with_types (next_token p3) in

  (* Expect closing paren *)
  let* p5 =
    if curr_token_is p4 Token.RParen then
      Ok p4
    else
      expect_peek p4 Token.RParen
  in

  (* Parse effect marker: -> (pure) or => (effectful) *)
  let* p6, sm_effect =
    if peek_token_is p5 Token.Arrow then
      Ok (next_token p5, AST.Pure)
    else if peek_token_is p5 Token.FatArrow then
      Ok (next_token p5, AST.Effectful)
    else
      Result.map (fun p -> (p, AST.Pure)) (expect_peek p5 Token.Arrow)
  in

  (* Parse return type *)
  let* p7, sm_return_type = parse_type_expr (next_token p6) in

  (* Parse optional default body: = expr_or_block *)
  let* p8, sm_default_impl =
    if curr_token_is p7 Token.Assign then
      let p_body = next_token p7 in
      if curr_token_is p_body Token.LBrace && is_block_body_start p_body then
        let* p9, block = parse_block_statement p_body in
        let* p10 =
          if curr_token_is p9 Token.RBrace then
            Ok p9
          else
            expect_peek p9 Token.RBrace
        in
        Ok (next_token p10, Some (Surface.SEOBBlock block))
      else
        let* p9, expr = parse_expression p_body prec_lowest in
        let p10 = skip p9 Token.Semicolon in
        Ok (next_token p10, Some (Surface.SEOBExpr expr))
    else
      Ok (p7, None)
  in

  Ok (p8, Surface.{ sm_id; sm_name; sm_generics; sm_params = params; sm_return_type; sm_effect; sm_default_impl })

and parse_param_list_with_types (p : parser) : (parser * (string * Surface.surface_type_expr) list, parser) result
    =
  let rec loop lp pos_idx rev_params =
    if curr_token_is lp Token.RParen then
      Ok (lp, List.rev rev_params)
    else if curr_token_is lp Token.Ident && peek_token_is lp Token.Colon then
      (* Named param: name: type *)
      let param_name = lp.curr_token.literal in
      let lp2 = next_token lp in
      (* now at : *)
      let* lp3, param_type = parse_param_type_expr (next_token lp2) in
      if curr_token_is lp3 Token.Comma then
        loop (next_token lp3) (pos_idx + 1) ((param_name, param_type) :: rev_params)
      else
        Ok (lp3, List.rev ((param_name, param_type) :: rev_params))
    else if curr_token_is lp Token.Ident then
      (* Positional param: just a type expression (no colon) *)
      let* lp2, param_type = parse_param_type_expr lp in
      let param_name = Printf.sprintf "$%d" pos_idx in
      if curr_token_is lp2 Token.Comma then
        loop (next_token lp2) (pos_idx + 1) ((param_name, param_type) :: rev_params)
      else
        Ok (lp2, List.rev ((param_name, param_type) :: rev_params))
    else
      Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
  in
  loop p 0 []

(* Phase 4.3: Impl definition parsing *)
and parse_impl_definition (p : parser) : (parser * Surface.top_decl, parser) result =
  (* Current token is 'impl' *)
  let parse_methods_block (lp : parser) : (parser * Surface.surface_method_impl list, parser) result =
    let* lp2 =
      if curr_token_is lp Token.LBrace then
        Ok lp
      else
        expect_peek lp Token.LBrace
    in
    let* lp3, methods = parse_method_impl_list (next_token lp2) in
    let* lp4 =
      if curr_token_is lp3 Token.RBrace then
        Ok lp3
      else
        expect_peek lp3 Token.RBrace
    in
    Ok (lp4, methods)
  in

  (* Check for explicit type params at impl level: impl[a: Show] ... *)
  let* p_after_explicit, explicit_type_params =
    if peek_token_is p Token.LBracket then
      let p_bracket = next_token (next_token p) in
      (* at first token inside [ *)
      let* p_after, params = parse_generic_param_list p_bracket in
      Ok (p_after, params)
    else
      Ok (next_token p, [])
  in
  let* p_head_end, impl_head_type = parse_type_expr p_after_explicit in
  let* p_assign =
    if curr_token_is p_head_end Token.Assign then
      Ok p_head_end
    else
      expect_peek p_head_end Token.Assign
  in
  let* p_end, impl_methods = parse_methods_block p_assign in
  match (explicit_type_params, impl_head_type) with
  | [], Surface.STCon _ ->
      Ok (p_end, Surface.SInherentImplDef { inherent_for_type = impl_head_type; inherent_methods = impl_methods })
  | _ ->
      Ok
        ( p_end,
          Surface.SAmbiguousImplDef { impl_type_params = explicit_type_params; impl_head_type; impl_methods } )

and parse_generic_param_list (p : parser) : (parser * AST.generic_param list, parser) result =
  let rec loop lp rev_params =
    if curr_token_is lp Token.Ident then
      let param_name = lp.curr_token.literal in
      let* lp2, constraints =
        if peek_token_is lp Token.Colon then
          (* Parse constraints: a: Eq & Ord *)
          parse_constraint_list (next_token (next_token lp))
        else
          Ok (next_token lp, [])
      in
      let generic_param = AST.{ name = param_name; constraints } in
      let lp3 = lp2 in
      if curr_token_is lp3 Token.Comma then
        loop (next_token lp3) (generic_param :: rev_params)
      else if curr_token_is lp3 Token.RBracket then
        Ok (next_token lp3, List.rev (generic_param :: rev_params))
      else
        Error (peek_error lp3 Token.RBracket)
    else if curr_token_is lp Token.RBracket then
      Ok (next_token lp, List.rev rev_params)
    else
      Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
  in
  loop p []

and parse_constraint_list (p : parser) : (parser * string list, parser) result =
  let rec loop lp rev_constraints =
    if curr_token_is lp Token.Ident then
      let constraint_name = lp.curr_token.literal in
      let lp2 = next_token lp in
      if curr_token_is lp2 Token.Ampersand then
        (* More constraints: & trait *)
        loop (next_token lp2) (constraint_name :: rev_constraints)
      else
        (* End of constraint list *)
        Ok (lp2, List.rev (constraint_name :: rev_constraints))
    else
      Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
  in
  loop p []

and parse_method_impl_list (p : parser) : (parser * Surface.surface_method_impl list, parser) result =
  let rec loop lp methods =
    if curr_token_is lp Token.RBrace then
      Ok (lp, List.rev methods)
    else if curr_token_is lp Token.Function then
      let* lp2, method_impl = parse_method_impl lp in
      loop lp2 (method_impl :: methods)
    else if curr_token_is lp Token.Override then
      let* p_fn = expect_peek lp Token.Function in
      let* lp2, method_impl = parse_method_impl p_fn in
      loop lp2 ({ method_impl with Surface.smi_override = true } :: methods)
    else
      Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
  in
  loop p []

and is_block_body_start (p : parser) : bool =
  (* p.curr_token = { — heuristic to tell block from record/hash/block literal *)
  let _, peek2 = Lexer.next_token p.lexer in
  match p.peek_token.token_type with
  | Token.Let | Token.Return | Token.If | Token.Match | Token.Function -> true
  | Token.RBrace -> true (* {} is an empty block expression in vNext *)
  | Token.Spread -> false (* { ...spread } is record *)
  | Token.Ident -> peek2.token_type <> Token.Colon (* { x: ... } is record *)
  | Token.String | Token.Int | Token.Float | Token.True | Token.False ->
      (* { "key": val } or { 10: val } or { true: val } is hash literal *)
      peek2.token_type <> Token.Colon
  | _ -> true

and parse_method_impl (p : parser) : (parser * Surface.surface_method_impl, parser) result =
  (* Current token is 'fn' *)
  let smi_id = fresh_id p in
  let* p2 = expect_peek p Token.Ident in
  let smi_name = p2.curr_token.literal in

  (* Parse optional method-level generics [a, b: show] *)
  let* p3, smi_generics = parse_generic_params p2 in
  let p3 = next_token p3 in

  (* Expect opening paren *)
  let* p3 =
    if curr_token_is p3 Token.LParen then
      Ok p3
    else
      expect_peek p3 Token.LParen
  in

  (* Parse function parameters (can have optional types) *)
  let* p4, params = parse_function_parameters p3 in

  (* Expect closing paren *)
  let* p5 =
    if curr_token_is p4 Token.RParen then
      Ok p4
    else
      expect_peek p4 Token.RParen
  in

  (* Parse optional effect marker + return type: -> T, => T, or neither *)
  let* p6, smi_return_type, smi_effect =
    if peek_token_is p5 Token.Arrow then
      let* p6, ret_type = parse_type_expr (next_token (next_token p5)) in
      Ok (p6, Some ret_type, Some AST.Pure)
    else if peek_token_is p5 Token.FatArrow then
      let* p6, ret_type = parse_type_expr (next_token (next_token p5)) in
      Ok (p6, Some ret_type, Some AST.Effectful)
    else
      Ok (next_token p5, None, None)
  in

  (* Parse body: = expr_or_block *)
  let* p7 =
    if curr_token_is p6 Token.Assign then
      Ok p6
    else
      expect_peek p6 Token.Assign
  in
  let p_body = next_token p7 in
  let* p8, smi_body =
    if curr_token_is p_body Token.LBrace && is_block_body_start p_body then
      (* block body: = { stmts } *)
      let* p8, block = parse_block_statement p_body in
      let* p9 =
        if curr_token_is p8 Token.RBrace then
          Ok p8
        else
          expect_peek p8 Token.RBrace
      in
      Ok (next_token p9, Surface.SEOBBlock block)
    else
      (* expression body: = expr *)
      let* p8, expr = parse_expression p_body prec_lowest in
      let p9 = skip p8 Token.Semicolon in
      Ok (next_token p9, Surface.SEOBExpr expr)
  in

  Ok
    ( p8,
      Surface.
        {
          smi_id;
          smi_name;
          smi_generics;
          smi_params = params;
          smi_return_type;
          smi_effect;
          smi_override = false;
          smi_body;
        } )

and parse_derive_trait_list (p : parser) : (parser * AST.derive_trait list, parser) result =
  let rec loop lp rev_traits =
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
        loop (next_token lp3) (derive_trait :: rev_traits)
      else
        (* End of trait list *)
        Ok (lp3, List.rev (derive_trait :: rev_traits))
    else
      Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
  in
  loop p []

and parse_expression_stmt (p : parser) : (parser * Surface.surface_stmt, parser) result =
  let pos = p.curr_token.pos in
  let* p2, expr = parse_expression p prec_lowest in
  let p3 = skip p2 Token.Semicolon in
  Ok (p3, mk_surface_stmt pos (Surface.SSExpressionStmt expr))

and parse_expression (p : parser) (prec : precedence) : (parser * Surface.surface_expr, parser) result =
  let* p2, left_expr = prefixFn p in
  let* p3, left = infixFn p2 left_expr prec in
  Ok (p3, with_surface_expr_end p3 left)

and prefixFn (p : parser) : (parser * Surface.surface_expr, parser) result =
  let tt = p.curr_token.token_type in
  match tt with
  | Token.Ident -> parse_identifier p
  | Token.Int -> parse_integer_literal p
  | Token.Float -> parse_float_literal p
  | Token.String -> parse_string_literal p
  | Token.Bang | Token.Minus -> parse_prefix_expression p
  | Token.True | Token.False -> parse_boolean p
  | Token.LParen -> parse_lambda_or_grouped p
  | Token.If -> parse_if_expression p
  | Token.Match -> parse_match_expression p
  | Token.LBracket -> parse_array_literal p
  | Token.LBrace ->
      if is_block_body_start p then
        parse_block_expr p
      else
        parse_record_or_hash_literal p
  | _ -> Error (no_prefix_parse_fn_error p tt)

and infixFn (p : parser) (left_expr : Surface.surface_expr) (prec : precedence) :
    (parser * Surface.surface_expr, parser) result =
  let rec loop (lp : parser) (left : Surface.surface_expr) : (parser * Surface.surface_expr, parser) result =
    let peek_is_semicolon = peek_token_is lp Token.Semicolon in
    let lower_precedence = prec < peek_precedence lp in
    let blocked_by_line_break =
      tokens_are_on_different_lines lp
      &&
      match lp.peek_token.token_type with
      | Token.LParen | Token.LBracket -> true
      | Token.Dot -> false
      | _ -> false
    in
    if (not peek_is_semicolon) && lower_precedence && not blocked_by_line_break then
      let* lp2, left2 =
        match lp.peek_token.token_type with
        | Token.Plus | Token.Minus | Token.Slash | Token.Asterisk | Token.Eq | Token.NotEq | Token.Lt | Token.Gt
        | Token.Le | Token.Ge | Token.Is | Token.PipePipe | Token.AmpAmp | Token.Percent ->
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

and parse_identifier (p : parser) : (parser * Surface.surface_expr, parser) result =
  let pos = p.curr_token.pos in
  let name = p.curr_token.literal in
  let id = fresh_id p in
  Ok (p, mk_surface_expr id pos (Surface.SEIdentifier name))

and parse_integer_literal (p : parser) : (parser * Surface.surface_expr, parser) result =
  let pos = p.curr_token.pos in
  match Int64.of_string_opt p.curr_token.literal with
  | Some i ->
      let id = fresh_id p in
      Ok (p, mk_surface_expr id pos (Surface.SEInteger i))
  | None ->
      let msg = Printf.sprintf "can't parse number from %s" p.curr_token.literal in
      Error (add_error ~code:"parse-invalid-number" p msg)

and parse_float_literal (p : parser) : (parser * Surface.surface_expr, parser) result =
  let pos = p.curr_token.pos in
  match float_of_string_opt p.curr_token.literal with
  | Some f ->
      let id = fresh_id p in
      Ok (p, mk_surface_expr id pos (Surface.SEFloat f))
  | None ->
      let msg = Printf.sprintf "can't parse number from %s" p.curr_token.literal in
      Error (add_error ~code:"parse-invalid-number" p msg)

and parse_string_literal (p : parser) : (parser * Surface.surface_expr, parser) result =
  let pos = p.curr_token.pos in
  let id = fresh_id p in
  Ok (p, mk_surface_expr id pos (Surface.SEString p.curr_token.literal))

and parse_prefix_expression (p : parser) : (parser * Surface.surface_expr, parser) result =
  let pos = p.curr_token.pos in
  let op = p.curr_token.literal in
  let p2 = next_token p in
  let* p3, right = parse_expression p2 prec_prefix in
  let id = fresh_id p3 in
  Ok (p3, mk_surface_expr id pos (Surface.SEPrefix (op, right)))

and parse_infix_expression (p : parser) (left : Surface.surface_expr) :
    (parser * Surface.surface_expr, parser) result =
  let pos = p.curr_token.pos in
  match p.curr_token.token_type with
  | Token.Is ->
      let p2 = next_token p in
      if not (curr_token_is p2 Token.Ident) then
        Error (add_error ~code:"parse-unexpected-token" p2 "Expected type name after 'is'")
      else
        let type_name = p2.curr_token.literal in
        let type_expr = Surface.STCon type_name in
        let id = fresh_id p2 in
        Ok (p2, mk_surface_expr id pos (Surface.SETypeCheck (left, type_expr)))
  | _ ->
      let op = p.curr_token.literal in
      let prec = curr_precedence p in
      let p2 = next_token p in
      let* p3, right = parse_expression p2 prec in
      let id = fresh_id p3 in
      Ok (p3, mk_surface_expr id pos (Surface.SEInfix (left, op, right)))

and parse_boolean (p : parser) : (parser * Surface.surface_expr, parser) result =
  let pos = p.curr_token.pos in
  let id = fresh_id p in
  Ok (p, mk_surface_expr id pos (Surface.SEBoolean (p.curr_token.token_type = Token.True)))

and parse_grouped_expression (p : parser) : (parser * Surface.surface_expr, parser) result =
  let* p2, expr = parse_expression (next_token p) prec_lowest in
  let* p3 = expect_peek p2 Token.RParen in
  Ok (p3, expr)

and parse_lambda_or_grouped (p : parser) : (parser * Surface.surface_expr, parser) result =
  let pos = p.curr_token.pos in
  (* p.curr_token = LParen *)
  (* Use Lexer.next_token to peek at the token after p.peek_token without advancing *)
  let _, peek2_tok = Lexer.next_token p.lexer in
  (* Check for empty lambda: () -> expr or () => expr *)
  let is_empty_lambda =
    peek_token_is p Token.RParen && (peek2_tok.token_type = Token.Arrow || peek2_tok.token_type = Token.FatArrow)
  in
  (* Check for multi-param or typed-param lambda: (a, b) -> or (a: T) -> *)
  let is_multi_or_typed =
    peek_token_is p Token.Ident && (peek2_tok.token_type = Token.Comma || peek2_tok.token_type = Token.Colon)
  in
  if is_empty_lambda then
    (* () -> expr or () => expr *)
    let p2 = next_token p in
    (* at ) *)
    let p3 = next_token p2 in
    (* at -> or => *)
    let is_effectful = curr_token_is p3 Token.FatArrow in
    let* p4, body_expr = parse_expression (next_token p3) prec_lowest in
    let id = fresh_id p4 in
    Ok
      ( p4,
        mk_surface_expr id pos
          (Surface.SEArrowLambda
             {
               se_lambda_params = [];
               se_lambda_is_effectful = is_effectful;
               se_lambda_body = Surface.SEOBExpr body_expr;
             }) )
  else if is_multi_or_typed then
    (* Parse as explicit lambda params *)
    let* p2, lparams = parse_lambda_param_list (next_token p) in
    (* p2 should be at ) after params *)
    let p3 = next_token p2 in
    (* past ) -> at -> or => *)
    let is_effectful = curr_token_is p3 Token.FatArrow in
    if not (curr_token_is p3 Token.Arrow || curr_token_is p3 Token.FatArrow) then
      Error (add_error ~code:"parse-unexpected-token" p3 "expected '->' or '=>' after lambda params")
    else
      let* p4, body_expr = parse_expression (next_token p3) prec_lowest in
      let id = fresh_id p4 in
      Ok
        ( p4,
          mk_surface_expr id pos
            (Surface.SEArrowLambda
               {
                 se_lambda_params = lparams;
                 se_lambda_is_effectful = is_effectful;
                 se_lambda_body = Surface.SEOBExpr body_expr;
               }) )
  else
    (* Grouped expression or single-param lambda: (x) -> expr *)
    let* p2, expr = parse_expression (next_token p) prec_lowest in
    let* p3 = expect_peek p2 Token.RParen in
    (* Check for single-ident lambda: (x) -> expr *)
    if peek_token_is p3 Token.Arrow || peek_token_is p3 Token.FatArrow then
      match expr.Surface.se_expr with
      | Surface.SEIdentifier name ->
          let p4 = next_token p3 in
          (* at -> or => *)
          let is_effectful = curr_token_is p4 Token.FatArrow in
          let* p5, body_expr = parse_expression (next_token p4) prec_lowest in
          let id = fresh_id p5 in
          Ok
            ( p5,
              mk_surface_expr id pos
                (Surface.SEArrowLambda
                   {
                     se_lambda_params = [ (name, None) ];
                     se_lambda_is_effectful = is_effectful;
                     se_lambda_body = Surface.SEOBExpr body_expr;
                   }) )
      | _ -> Ok (p3, expr)
    else
      Ok (p3, expr)

and parse_lambda_param_list (p : parser) :
    (parser * (string * Surface.surface_type_expr option) list, parser) result =
  (* p.curr_token = first token of params, inside ( *)
  let rec loop lp rev_params =
    if curr_token_is lp Token.RParen then
      Ok (lp, List.rev rev_params)
    else if curr_token_is lp Token.Ident then
      let name = lp.curr_token.literal in
      let lp2 = next_token lp in
      let* lp3, type_annot =
        if curr_token_is lp2 Token.Colon then
          let* lp3, te = parse_param_type_expr (next_token lp2) in
          Ok (lp3, Some te)
        else
          Ok (lp2, None)
      in
      if curr_token_is lp3 Token.Comma then
        loop (next_token lp3) ((name, type_annot) :: rev_params)
      else
        Ok (lp3, List.rev ((name, type_annot) :: rev_params))
    else
      Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
  in
  loop p []

and parse_if_expression (p : parser) : (parser * Surface.surface_expr, parser) result =
  let pos = p.curr_token.pos in
  let* p2 = expect_peek p Token.LParen in
  let* p3, cond = parse_expression (next_token p2) prec_lowest in
  let* p4 = expect_peek p3 Token.RParen in

  (* After ), check for { or return (with early return support) *)
  let* p5, cons =
    if peek_token_is p4 Token.LBrace then
      (* Normal: { ... } *)
      let* p5 = expect_peek p4 Token.LBrace in
      let* p6, block = parse_block_statement p5 in
      let cons =
        Surface.
          {
            ss_stmt = SSBlock block;
            ss_pos = block.sb_pos;
            ss_end_pos = block.sb_end_pos;
            ss_file_id = block.sb_file_id;
          }
      in
      Ok (p6, cons)
    else if peek_token_is p4 Token.Return then
      (* New: return expr without braces *)
      let p5 = next_token p4 in
      let pos_ret = p5.curr_token.pos in
      let* p6, expr = parse_expression (next_token p5) prec_lowest in
      let p7 = skip p6 Token.Semicolon in
      let ret_stmt = with_surface_stmt_end p7 (mk_surface_stmt pos_ret (Surface.SSReturn expr)) in
      let block =
        Surface.{ sb_stmts = [ ret_stmt ]; sb_pos = pos_ret; sb_end_pos = pos_ret; sb_file_id = None }
      in
      let block_stmt = with_surface_stmt_end p7 (mk_surface_stmt pos_ret (Surface.SSBlock block)) in
      Ok (p7, block_stmt)
    else
      Error (add_error ~code:"parse-unexpected-token" p4 "Expected '{' or 'return' after if condition")
  in

  if not (peek_token_is p5 Token.Else) then
    let id = fresh_id p5 in
    Ok (p5, mk_surface_expr id pos (Surface.SEIf (cond, cons, None)))
  else
    let p6 = next_token p5 in
    (* Now at 'else' token *)
    let* p7, alt =
      if peek_token_is p6 Token.LBrace then
        (* Normal: else { ... } *)
        let* p7 = expect_peek p6 Token.LBrace in
        let* p8, block = parse_block_statement p7 in
        let alt =
          Surface.
            {
              ss_stmt = SSBlock block;
              ss_pos = block.sb_pos;
              ss_end_pos = block.sb_end_pos;
              ss_file_id = block.sb_file_id;
            }
        in
        Ok (p8, alt)
      else if peek_token_is p6 Token.Return then
        (* New: else return expr without braces *)
        let p7 = next_token p6 in
        let pos_ret = p7.curr_token.pos in
        let* p8, expr = parse_expression (next_token p7) prec_lowest in
        let p9 = skip p8 Token.Semicolon in
        let ret_stmt = with_surface_stmt_end p9 (mk_surface_stmt pos_ret (Surface.SSReturn expr)) in
        let block =
          Surface.{ sb_stmts = [ ret_stmt ]; sb_pos = pos_ret; sb_end_pos = pos_ret; sb_file_id = None }
        in
        let block_stmt = with_surface_stmt_end p9 (mk_surface_stmt pos_ret (Surface.SSBlock block)) in
        Ok (p9, block_stmt)
      else
        (* Error: else without block or return *)
        Error (add_error ~code:"parse-unexpected-token" p6 "Expected '{' or 'return' after 'else'")
    in
    let id = fresh_id p7 in
    Ok (p7, mk_surface_expr id pos (Surface.SEIf (cond, cons, Some alt)))

and parse_block_statement (p : parser) : (parser * Surface.surface_block, parser) result =
  let pos = p.curr_token.pos in
  let rec loop (lp : parser) (stmts : Surface.surface_stmt list) : (parser * Surface.surface_block, parser) result
      =
    if curr_token_is lp Token.RBrace || curr_token_is lp Token.EOF then
      let end_pos = max pos (token_end lp.curr_token) in
      let file_id = Some lp.file_id in
      Ok (lp, Surface.{ sb_stmts = List.rev stmts; sb_pos = pos; sb_end_pos = end_pos; sb_file_id = file_id })
    else
      let* lp2, new_stmt = parse_block_stmt lp in
      loop (next_token lp2) (new_stmt :: stmts)
  in

  loop (next_token p) []

and parse_function_parameters (p : parser) :
    (parser * (string * Surface.surface_type_expr option) list, parser) result =
  if peek_token_is p Token.RParen then
    Ok (next_token p, [])
  else
    let rec loop (lp : parser) (rev_params_acc : (string * Surface.surface_type_expr option) list) =
      (* Get the parameter name *)
      if not (curr_token_is lp Token.Ident) then
        Error (no_prefix_parse_fn_error lp lp.curr_token.token_type)
      else
        let param_name = lp.curr_token.literal in
        let lp_after_name = next_token lp in
        (* Phase 2: Parse type annotation if present *)
        let* lp_after_annot, type_annot =
          if curr_token_is lp_after_name Token.Colon then
            let* lp_parse, te = parse_param_type_expr (next_token lp_after_name) in
            Ok (lp_parse, Some te)
          else
            Ok (lp_after_name, None)
        in
        let new_param = (param_name, type_annot) in
        if curr_token_is lp_after_annot Token.Comma then
          loop (next_token lp_after_annot) (new_param :: rev_params_acc)
        else if curr_token_is lp_after_annot Token.RParen then
          Ok (lp_after_annot, List.rev (new_param :: rev_params_acc))
        else
          Error (peek_error lp_after_annot Token.RParen)
    in
    let p2 = next_token p in
    loop p2 []

and parse_call_expression (p : parser) (c : Surface.surface_expr) : (parser * Surface.surface_expr, parser) result
    =
  let pos = p.curr_token.pos in
  if peek_token_is p Token.RParen then
    let* p2, arguments = parse_expression_list p Token.RParen in
    let id = fresh_id p2 in
    Ok (p2, mk_surface_expr id pos (Surface.SECall (c, arguments)))
  else
    let p_args = next_token p in
    if curr_token_is p_args Token.Spread || (curr_token_is p_args Token.Ident && peek_token_is p_args Token.Colon)
    then
      let* p2, record_arg = parse_labeled_call_record p_args in
      let id = fresh_id p2 in
      Ok (p2, mk_surface_expr id pos (Surface.SECall (c, [ record_arg ])))
    else
      let* p2, arguments = parse_expression_list p Token.RParen in
      let id = fresh_id p2 in
      Ok (p2, mk_surface_expr id pos (Surface.SECall (c, arguments)))

and parse_labeled_call_record (p : parser) : (parser * Surface.surface_expr, parser) result =
  let pos = p.curr_token.pos in
  let rec loop lp rev_fields spread =
    if curr_token_is lp Token.RParen then
      let id = fresh_id lp in
      Ok (lp, mk_surface_expr id pos (Surface.SERecordLit (List.rev rev_fields, spread)))
    else if curr_token_is lp Token.Spread then
      let lp2 = next_token lp in
      let* lp3, spread_expr = parse_expression lp2 prec_lowest in
      if Option.is_some spread then
        Error
          (add_error ~code:"parse-invalid-record" lp
             "multiple spread entries in record literal are not supported yet")
      else if curr_token_is lp3 Token.Comma then
        loop (next_token lp3) rev_fields (Some spread_expr)
      else if curr_token_is lp3 Token.RParen then
        loop lp3 rev_fields (Some spread_expr)
      else if peek_token_is lp3 Token.Comma then
        loop (next_token (next_token lp3)) rev_fields (Some spread_expr)
      else if peek_token_is lp3 Token.RParen then
        loop (next_token lp3) rev_fields (Some spread_expr)
      else
        Error (peek_error lp3 Token.RParen)
    else if curr_token_is lp Token.Ident && peek_token_is lp Token.Colon then
      let se_field_name = lp.curr_token.literal in
      let* lp2 = expect_peek lp Token.Colon in
      let* lp3, se_field_value = parse_expression (next_token lp2) prec_lowest in
      let field = Surface.{ se_field_name; se_field_value = Some se_field_value } in
      if curr_token_is lp3 Token.Comma then
        loop (next_token lp3) (field :: rev_fields) spread
      else if curr_token_is lp3 Token.RParen then
        loop lp3 (field :: rev_fields) spread
      else if peek_token_is lp3 Token.Comma then
        loop (next_token (next_token lp3)) (field :: rev_fields) spread
      else if peek_token_is lp3 Token.RParen then
        loop (next_token lp3) (field :: rev_fields) spread
      else
        Error (peek_error lp3 Token.RParen)
    else
      Error (add_error ~code:"parse-unexpected-token" lp "expected labeled argument")
  in
  loop p [] None

and parse_expression_list (p : parser) (end_tt : Token.token_type) :
    (parser * Surface.surface_expr list, parser) result =
  if peek_token_is p end_tt then
    Ok (next_token p, [])
  else
    let* p2, arg = parse_expression (next_token p) prec_lowest in

    let rec loop (lp : parser) (args : Surface.surface_expr list) =
      if peek_token_is lp Token.Comma then
        let* lp2, arg = parse_expression (next_token (next_token lp)) prec_lowest in
        loop lp2 (arg :: args)
      else
        let* lp2 = expect_peek lp end_tt in
        Ok (lp2, List.rev args)
    in

    loop p2 [ arg ]

and parse_array_literal (p : parser) : (parser * Surface.surface_expr, parser) result =
  let pos = p.curr_token.pos in
  let* p2, exprs = parse_expression_list p Token.RBracket in
  let id = fresh_id p2 in
  Ok (p2, mk_surface_expr id pos (Surface.SEArray exprs))

and parse_index_expression (p : parser) (left : Surface.surface_expr) :
    (parser * Surface.surface_expr, parser) result =
  let pos = p.curr_token.pos in
  let p2 = next_token p in
  let* p3, index = parse_expression p2 prec_lowest in
  let* p4 = expect_peek p3 Token.RBracket in
  let id = fresh_id p4 in
  Ok (p4, mk_surface_expr id pos (Surface.SEIndex (left, index)))

(* Phase 4.3: Dot expression parsing - handles method calls and field access *)
(* Note: enum constructors are also parsed as MethodCall here, type checker will distinguish *)
and parse_dot_expression (p : parser) (left : Surface.surface_expr) :
    (parser * Surface.surface_expr, parser) result =
  let pos = p.curr_token.pos in
  (* p.curr_token is Dot, advance to the identifier *)
  let p2 = next_token p in
  if not (curr_token_is p2 Token.Ident) then
    Error (add_error ~code:"parse-unexpected-token" p2 "expected identifier after '.'")
  else
    let member_name = p2.curr_token.literal in

    if peek_token_is p2 Token.LParen then
      (* expr.member(args) -> Method call or enum constructor *)
      let p3 = next_token p2 in
      let* p4, args = parse_expression_list p3 Token.RParen in
      let id = fresh_id p4 in
      Ok
        ( p4,
          mk_surface_expr id pos
            (Surface.SEMethodCall
               { se_receiver = left; se_method = member_name; se_type_args = None; se_args = args }) )
    else if peek_token_is p2 Token.LBracket then
      (* expr.member[...] -> could be method type args or index; try type args first *)
      let p_bracket = next_token (next_token p2) in
      (* advance past member and [ *)
      match parse_type_expr_list p_bracket with
      | Ok (p_after_types, type_args)
        when curr_token_is p_after_types Token.RBracket
             && peek_token_is p_after_types Token.LParen
             && type_args <> [] ->
          (* [type_args](args) -> method call with explicit type args *)
          let p_after_bracket = next_token p_after_types in
          (* consume ], now at ( *)
          let* p_after_args, args = parse_expression_list p_after_bracket Token.RParen in
          let id = fresh_id p_after_args in
          Ok
            ( p_after_args,
              mk_surface_expr id pos
                (Surface.SEMethodCall
                   { se_receiver = left; se_method = member_name; se_type_args = Some type_args; se_args = args })
            )
      | _ ->
          (* Not type args — fall back to field access (infix [ will handle indexing) *)
          let id = fresh_id p2 in
          Ok (p2, mk_surface_expr id pos (Surface.SEFieldAccess (left, member_name)))
    else
      (* expr.member -> Field access or nullary enum constructor *)
      let id = fresh_id p2 in
      Ok (p2, mk_surface_expr id pos (Surface.SEFieldAccess (left, member_name)))

(* Parse { stmts } in expression position as a block expression (SEBlockExpr) *)
and parse_block_expr (p : parser) : (parser * Surface.surface_expr, parser) result =
  (* p.curr_token = LBrace, and is_block_body_start is true *)
  let pos = p.curr_token.pos in
  let* p2, block = parse_block_statement p in
  let* p3 =
    if curr_token_is p2 Token.RBrace then
      Ok p2
    else
      expect_peek p2 Token.RBrace
  in
  let id = fresh_id p3 in
  Ok (p3, mk_surface_expr id pos (Surface.SEBlockExpr block))

(* Phase 4.6: Distinguish record literal from hash literal using one mode-locked loop *)
and parse_record_or_hash_literal (p : parser) : (parser * Surface.surface_expr, parser) result =
  let pos = p.curr_token.pos in
  let finish
      mode
      (fields : Surface.surface_record_field list)
      spread
      (pairs : (Surface.surface_expr * Surface.surface_expr) list)
      lp =
    let id = fresh_id lp in
    match mode with
    | RecordMode -> Ok (lp, mk_surface_expr id pos (Surface.SERecordLit (List.rev fields, spread)))
    | HashMode -> Ok (lp, mk_surface_expr id pos (Surface.SEHash (List.rev pairs)))
  in
  let rec loop
      (lp : parser)
      (mode : brace_literal_mode option)
      (fields : Surface.surface_record_field list)
      (spread : Surface.surface_expr option)
      (pairs : (Surface.surface_expr * Surface.surface_expr) list) :
      (parser * Surface.surface_expr, parser) result =
    if curr_token_is lp Token.RBrace then
      match mode with
      | Some mode' -> finish mode' fields spread pairs lp
      | None ->
          Error
            (add_error ~code:"parse-invalid-record" lp
               "empty '{}' literal is not supported as a map or record literal")
    else
      match mode with
      | Some RecordMode -> (
          let* lp_entry, entry = parse_record_entry lp in
          let* lp_sep = consume_brace_entry_separator lp_entry "expected ',' or '}' after record literal entry" in
          match entry with
          | `Field field -> loop lp_sep mode (field :: fields) spread pairs
          | `Spread spread_expr ->
              if Option.is_some spread then
                Error
                  (add_error ~code:"parse-invalid-record" lp
                     "multiple spread entries in record literal are not supported yet")
              else
                loop lp_sep mode fields (Some spread_expr) pairs)
      | Some HashMode ->
          let* lp_entry, pair = parse_hash_entry lp in
          let* lp_sep = consume_brace_entry_separator lp_entry "expected ',' or '}' after hash literal entry" in
          loop lp_sep mode fields spread (pair :: pairs)
      | None ->
          if curr_token_is lp Token.Spread || (curr_token_is lp Token.Ident && peek_token_is lp Token.Colon) then
            let* lp_entry, entry = parse_record_entry lp in
            let* lp_sep =
              consume_brace_entry_separator lp_entry "expected ',' or '}' after record literal entry"
            in
            match entry with
            | `Field field -> loop lp_sep (Some RecordMode) (field :: fields) spread pairs
            | `Spread spread_expr -> loop lp_sep (Some RecordMode) fields (Some spread_expr) pairs
          else
            let* lp_entry, pair = parse_hash_entry lp in
            let* lp_sep = consume_brace_entry_separator lp_entry "expected ',' or '}' after hash literal entry" in
            loop lp_sep (Some HashMode) fields spread (pair :: pairs)
  in
  loop (next_token p) None [] None []

and parse_record_entry (p : parser) :
    (parser * [ `Field of Surface.surface_record_field | `Spread of Surface.surface_expr ], parser) result =
  if curr_token_is p Token.Spread then
    let p2 = next_token p in
    let* p3, spread_expr = parse_expression p2 prec_lowest in
    Ok (p3, `Spread spread_expr)
  else if curr_token_is p Token.Ident && peek_token_is p Token.Colon then
    let se_field_name = p.curr_token.literal in
    let* p2 = expect_peek p Token.Colon in
    let* p3, se_field_value =
      if peek_token_is p2 Token.Comma || peek_token_is p2 Token.RBrace then
        Ok (next_token p2, None)
      else
        let* p3, value = parse_expression (next_token p2) prec_lowest in
        Ok (p3, Some value)
    in
    Ok (p3, `Field Surface.{ se_field_name; se_field_value })
  else
    Error (add_error ~code:"parse-invalid-record" p "cannot mix hash-style entries into record literal")

and parse_hash_entry (p : parser) : (parser * (Surface.surface_expr * Surface.surface_expr), parser) result =
  if curr_token_is p Token.Spread then
    Error (add_error ~code:"parse-invalid-record" p "cannot mix record-style spread into hash literal")
  else if curr_token_is p Token.Ident && peek_token_is p Token.Colon then
    Error (add_error ~code:"parse-invalid-record" p "cannot mix record-style field into hash literal")
  else
    let* p2, key = parse_expression p prec_lowest in
    let* p3 = expect_peek p2 Token.Colon in
    let* p4, value = parse_expression (next_token p3) prec_lowest in
    Ok (p4, (key, value))

and consume_brace_entry_separator (p : parser) (err_msg : string) : (parser, parser) result =
  if curr_token_is p Token.Comma then
    Ok (next_token p)
  else if curr_token_is p Token.RBrace then
    if peek_token_is p Token.Comma then
      Ok (next_token (next_token p))
    else if peek_token_is p Token.RBrace then
      Ok (next_token p)
    else
      Ok p
  else if peek_token_is p Token.Comma then
    Ok (next_token (next_token p))
  else if peek_token_is p Token.RBrace then
    Ok (next_token p)
  else
    Error (add_error ~code:"parse-invalid-record" p err_msg)

(* Phase 4.2: Match expression parsing *)
and parse_match_expression (p : parser) : (parser * Surface.surface_expr, parser) result =
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

  let id = fresh_id p5 in
  Ok (p5, mk_surface_expr id pos (Surface.SEMatch (scrutinee, arms)))

and parse_match_arms (p : parser) : (parser * Surface.surface_match_arm list, parser) result =
  let rec loop lp arms =
    if curr_token_is lp Token.RBrace then
      Ok (lp, List.rev arms)
    else
      let* lp =
        if curr_token_is lp Token.Case then
          Ok (next_token lp)
        else
          Error (add_error ~code:"parse-unexpected-token" lp "expected 'case' at start of match arm")
      in
      (* Parse patterns (may be multiple with | separator) *)
      let* lp2, se_patterns = parse_patterns lp in

      (* Expect colon *)
      let* lp3 = expect_peek lp2 Token.Colon in

      let lp4 = next_token lp3 in

      (* Parse body: block or expression *)
      let* lp5, body =
        if curr_token_is lp4 Token.LBrace && is_block_body_start lp4 then
          let* lp5, block = parse_block_statement lp4 in
          let* lp6 =
            if curr_token_is lp5 Token.RBrace then
              Ok lp5
            else
              expect_peek lp5 Token.RBrace
          in
          Ok (lp6, Surface.SEOBBlock block)
        else
          let* lp5, expr = parse_expression lp4 prec_lowest in
          Ok (lp5, Surface.SEOBExpr expr)
      in

      let arm = Surface.{ se_patterns; se_arm_body = body } in
      (* Advance past the body expression before continuing *)
      loop (next_token lp5) (arm :: arms)
  in
  loop p []

and parse_patterns (p : parser) : (parser * Surface.surface_pattern list, parser) result =
  let* p2, first_pattern = parse_pattern p in

  let rec loop lp patterns =
    if peek_token_is lp Token.Pipe then
      let* lp2, pat = parse_pattern (next_token (next_token lp)) in
      loop lp2 (pat :: patterns)
    else
      Ok (lp, List.rev patterns)
  in

  loop p2 [ first_pattern ]

and parse_constructor_payload_patterns ~(constructor_pos : int) (p : parser) :
    (parser * Surface.surface_pattern list, parser) result =
  let parse_flattened_record_payload (lp : parser) : (parser * Surface.surface_pattern list, parser) result =
    let rec loop current fields rest =
      if curr_token_is current Token.RParen then
        Ok (current, [ mk_surface_pat constructor_pos (Surface.SPRecord (List.rev fields, rest)) ])
      else if curr_token_is current Token.Spread then
        let current2 = next_token current in
        if curr_token_is current2 Token.Ident then
          let rest_name = current2.curr_token.literal in
          let* current3 =
            if peek_token_is current2 Token.RParen then
              Ok (next_token current2)
            else
              Error
                (add_error ~code:"parse-invalid-pattern" current2
                   "expected ')' after flattened record rest pattern")
          in
          Ok (current3, [ mk_surface_pat constructor_pos (Surface.SPRecord (List.rev fields, Some rest_name)) ])
        else
          Error
            (add_error ~code:"parse-invalid-pattern" current2
               "expected identifier after '...' in constructor payload pattern")
      else if curr_token_is current Token.Ident && peek_token_is current Token.Colon then
        let field_name = current.curr_token.literal in
        let current2 = next_token current in
        let* current3, field_pattern =
          if peek_token_is current2 Token.Comma || peek_token_is current2 Token.RParen then
            Ok (next_token current2, None)
          else
            let* current3, pat = parse_pattern (next_token current2) in
            Ok (current3, Some pat)
        in
        let field = Surface.{ sp_field_name = field_name; sp_field_pattern = field_pattern } in
        if curr_token_is current3 Token.Comma then
          loop (next_token current3) (field :: fields) rest
        else
          loop current3 (field :: fields) rest
      else
        Error (add_error ~code:"parse-invalid-pattern" current "invalid constructor payload pattern")
    in
    loop lp [] None
  in
  let p_payload = next_token p in
  if
    curr_token_is p_payload Token.Spread
    || (curr_token_is p_payload Token.Ident && peek_token_is p_payload Token.Colon)
  then
    parse_flattened_record_payload p_payload
  else
    parse_pattern_list p

and parse_pattern (p : parser) : (parser * Surface.surface_pattern, parser) result =
  let finalize = function
    | Ok (lp, pat) -> Ok (lp, with_surface_pat_end lp pat)
    | Error _ as err -> err
  in
  (match p.curr_token.token_type with
  | Token.Ident ->
      if p.curr_token.literal = "_" then
        Ok (p, mk_surface_pat p.curr_token.pos Surface.SPWildcard)
      else if peek_token_is p Token.LParen then
        let constructor_name = p.curr_token.literal in
        let p2 = next_token p in
        let* p3, nested_patterns = parse_constructor_payload_patterns ~constructor_pos:p.curr_token.pos p2 in
        Ok
          ( p3,
            mk_surface_pat p.curr_token.pos
              (Surface.SPConstructor (constructor_name, constructor_name, nested_patterns)) )
      else if peek_token_is p Token.Dot then
        (* Enum constructor pattern: enum.variant or enum.variant(patterns) *)
        let enum_name = p.curr_token.literal in
        let p2 = next_token (next_token p) in
        if curr_token_is p2 Token.Ident then
          let variant_name = p2.curr_token.literal in
          if peek_token_is p2 Token.LParen then
            let p3 = next_token p2 in
            let* p4, nested_patterns = parse_constructor_payload_patterns ~constructor_pos:p.curr_token.pos p3 in
            Ok
              ( p4,
                mk_surface_pat p.curr_token.pos (Surface.SPConstructor (enum_name, variant_name, nested_patterns))
              )
          else
            Ok (p2, mk_surface_pat p.curr_token.pos (Surface.SPConstructor (enum_name, variant_name, [])))
        else
          Error (add_error ~code:"parse-invalid-pattern" p2 "expected variant name after '.'")
      else if is_upper_ident p.curr_token.literal then
        Ok
          ( p,
            mk_surface_pat p.curr_token.pos
              (Surface.SPConstructor (p.curr_token.literal, p.curr_token.literal, [])) )
      else
        Ok (p, mk_surface_pat p.curr_token.pos (Surface.SPVariable p.curr_token.literal))
  | Token.Int -> (
      match Int64.of_string_opt p.curr_token.literal with
      | Some i -> Ok (p, mk_surface_pat p.curr_token.pos (Surface.SPLiteral (AST.LInt i)))
      | None -> Error (add_error ~code:"parse-invalid-number" p "invalid integer literal in pattern"))
  | Token.String -> Ok (p, mk_surface_pat p.curr_token.pos (Surface.SPLiteral (AST.LString p.curr_token.literal)))
  | Token.True -> Ok (p, mk_surface_pat p.curr_token.pos (Surface.SPLiteral (AST.LBool true)))
  | Token.False -> Ok (p, mk_surface_pat p.curr_token.pos (Surface.SPLiteral (AST.LBool false)))
  | Token.LBrace -> parse_record_pattern p
  | Token.Minus -> Ok (p, mk_surface_pat p.curr_token.pos Surface.SPWildcard)
  | _ ->
      if p.curr_token.token_type = Token.Ident && p.curr_token.literal = "_" then
        Ok (p, mk_surface_pat p.curr_token.pos Surface.SPWildcard)
      else
        Error (add_error ~code:"parse-invalid-pattern" p "invalid pattern"))
  |> finalize

(* Phase 4.8: Parse record pattern: { x:, y: z, ...rest } *)
and parse_record_pattern (p : parser) : (parser * Surface.surface_pattern, parser) result =
  (* Current token is { *)
  let pos = p.curr_token.pos in
  let p2 = next_token p in

  let rec loop lp fields rest =
    if curr_token_is lp Token.RBrace then
      Ok (lp, mk_surface_pat pos (Surface.SPRecord (List.rev fields, rest)))
    else if curr_token_is lp Token.Spread then
      let lp2 = next_token lp in
      if curr_token_is lp2 Token.Ident then
        let rest_name = lp2.curr_token.literal in
        let* lp3 = expect_peek lp2 Token.RBrace in
        Ok (lp3, mk_surface_pat pos (Surface.SPRecord (List.rev fields, Some rest_name)))
      else
        Error (add_error ~code:"parse-invalid-pattern" lp2 "expected identifier after '...' in record pattern")
    else if curr_token_is lp Token.Ident then
      let sp_field_name = lp.curr_token.literal in
      let* lp2 = expect_peek lp Token.Colon in

      let* lp3, sp_field_pattern =
        if peek_token_is lp2 Token.Comma || peek_token_is lp2 Token.RBrace then
          Ok (next_token lp2, None)
        else
          let* lp3, pat = parse_pattern (next_token lp2) in
          Ok (lp3, Some pat)
      in

      let field = Surface.{ sp_field_name; sp_field_pattern } in

      if curr_token_is lp3 Token.Comma then
        loop (next_token lp3) (field :: fields) rest
      else if peek_token_is lp3 Token.Comma then
        loop (next_token (next_token lp3)) (field :: fields) rest
      else if curr_token_is lp3 Token.RBrace || peek_token_is lp3 Token.RBrace then
        loop
          (if curr_token_is lp3 Token.RBrace then
             lp3
           else
             next_token lp3)
          (field :: fields) rest
      else
        Error
          (add_error ~code:"parse-invalid-pattern" lp3
             "expected comma or closing brace after record pattern field")
    else
      Error (add_error ~code:"parse-invalid-pattern" lp "invalid record pattern field")
  in

  loop p2 [] None

and parse_pattern_list (p : parser) : (parser * Surface.surface_pattern list, parser) result =
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

let parse_program (p : parser) : (parser * AST.program, parser) result =
  let id_supply = p.id_supply in
  match parse_surface_program p with
  | Ok (p2, surface_prog) -> Ok (p2, Lower.lower_program id_supply surface_prog)
  | Error p_err -> Error p_err

let parse ~file_id (s : string) : (AST.program, errors) result =
  match s |> Lexer.init |> init ~file_id |> parse_program with
  | Ok (_, program) -> Ok program
  | Error parser -> Error (List.rev parser.errors)

type parse_surface_result = {
  program : Surface_ast.Surface.surface_program;
  id_supply : Id_supply.Id_supply.t;
}

let parse_surface ~file_id (s : string) : (parse_surface_result, errors) result =
  let p = s |> Lexer.init |> init ~file_id in
  let id_supply = p.id_supply in
  match parse_surface_program p with
  | Ok (_, surface_prog) -> Ok { program = surface_prog; id_supply }
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
  let fn_expr params body =
    AST.Function { generics = None; params; return_type = None; is_effectful = false; body }

  let run (tests : test list) : bool =
    tests
    |> List.for_all (fun test ->
           match parse ~file_id:"<test>" test.input with
           | Ok program -> AST.program_equal program test.output
           | Error _ -> false)

  let run_print (tests : test list) : unit =
    tests
    |> List.iter (fun test ->
           match parse ~file_id:"<test>" test.input with
           | Ok program ->
               Printf.printf "input:\n%s\n" test.input;
               Printf.printf "expected:\n%s\n" (AST.show_program test.output);
               Printf.printf "output:\n%s\n" (AST.show_program program);
               flush stdout
           | Error errors ->
               Printf.printf "input:\n%s\n" test.input;
               let msgs = List.map (fun (d : Diagnostic.t) -> d.message) errors in
               Printf.printf "errors:\n%s\n" (String.concat "\n" msgs);
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

  let%test "expression span tracks token width" =
    match parse ~file_id:"<test>" "123;" with
    | Ok [ { AST.stmt = AST.ExpressionStmt expr; _ } ] -> expr.pos = 0 && expr.end_pos = 2
    | _ -> false

  let%test "string literal span includes quotes" =
    match parse ~file_id:"<test>" "\"ab\";" with
    | Ok [ { AST.stmt = AST.ExpressionStmt expr; _ } ] -> expr.pos = 0 && expr.end_pos = 3
    | _ -> false

  let%test "statement span includes trailing semicolon when present" =
    match parse ~file_id:"<test>" "let x = 1;" with
    | Ok [ stmt ] -> stmt.pos = 0 && stmt.end_pos = 9
    | _ -> false

  let%test "parser threads file_id into nodes" =
    match parse ~file_id:"main.mr" "let x = 1;" with
    | Ok [ { AST.stmt = AST.Let { value; _ }; file_id = Some file_id; _ } ] ->
        file_id = "main.mr" && value.file_id = Some "main.mr"
    | _ -> false

  let%test "parser emits coded expected-token diagnostic with span" =
    match parse ~file_id:"main.mr" "let x 1;" with
    | Error
        [
          {
            Diagnostic.code = "parse-expected-token";
            labels =
              [ { span = Diagnostic.Span { file_id; start_pos; end_pos = Some end_pos }; primary = true; _ } ];
            _;
          };
        ] ->
        file_id = "main.mr" && start_pos >= 0 && end_pos >= start_pos
    | _ -> false

  let%test "parser emits parse-invalid-record for mixed record/hash literal" =
    match parse ~file_id:"<test>" "let x = { a: 1, \"b\": 2 }" with
    | Error [ { Diagnostic.code = "parse-invalid-record"; _ } ] -> true
    | _ -> false

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

  let%test "array literal with identifier elements parses" =
    match parse ~file_id:"<test>" "let george = 1\nlet promoted_george = 2\n[george, promoted_george]" with
    | Ok
        [
          _;
          _;
          {
            AST.stmt =
              AST.ExpressionStmt
                {
                  AST.expr =
                    AST.Array
                      [ { expr = AST.Identifier "george"; _ }; { expr = AST.Identifier "promoted_george"; _ } ];
                  _;
                };
            _;
          };
        ] ->
        true
    | _ -> false

  let%test "array literal receiver with lambda call parses" =
    match
      parse_surface ~file_id:"<test>"
        "let george = { name: \"g\" }\nlet promoted_george = { name: \"p\" }\n[george, promoted_george].foreach((m) => puts(m.name))"
    with
    | Ok result -> (
        match result.program with
        | [ _; _; { Surface.std_decl = Surface.SExpressionStmt expr; _ } ] -> (
            match expr.se_expr with
            | Surface.SEMethodCall
                { se_receiver = { se_expr = Surface.SEArray [ _; _ ]; _ }; se_method = "foreach"; _ } ->
                true
            | _ -> false)
        | _ -> false)
    | Error _ -> false

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
        input = "5 <= 5;";
        output = [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Integer 5L), "<=", e (AST.Integer 5L))))) ];
      };
      {
        input = "5 >= 5;";
        output = [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Integer 5L), ">=", e (AST.Integer 5L))))) ];
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
        input = "foo <= bar;";
        output =
          [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Identifier "foo"), "<=", e (AST.Identifier "bar"))))) ];
      };
      {
        input = "foo >= bar;";
        output =
          [ s (AST.ExpressionStmt (e (AST.Infix (e (AST.Identifier "foo"), ">=", e (AST.Identifier "bar"))))) ];
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
      ("5 <= 4 == 3 >= 4", "((5 <= 4) == (3 >= 4))");
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
           match parse ~file_id:"<test>" input with
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

  let%test "test_lambda_expression" =
    [
      {
        input = "(x, y) -> x + y";
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
      {
        input = "() -> 42";
        output =
          [
            s
              (AST.ExpressionStmt
                 (e (fn_expr [] (s (AST.Block [ s (AST.ExpressionStmt (e (AST.Integer 42L))) ])))));
          ];
      };
      {
        input = "(foo, bar, baz) -> foo";
        output =
          [
            s
              (AST.ExpressionStmt
                 (e
                    (fn_expr
                       [ ("foo", None); ("bar", None); ("baz", None) ]
                       (s (AST.Block [ s (AST.ExpressionStmt (e (AST.Identifier "foo"))) ])))));
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
        input = "((x, y) -> x + y)(2, 3)";
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
        input = "callsFunction(2, 3, (x, y) -> x + y);";
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

  let%test "empty braces in expression position parse as an empty block expression" =
    match parse ~file_id:"<test>" "{}" with
    | Ok [ { AST.stmt = AST.ExpressionStmt { AST.expr = AST.BlockExpr []; _ }; _ } ] -> true
    | _ -> false

  let contains_substring s sub = String_utils.contains_substring ~needle:sub s

  let diagnostics_contain_substring (errs : Diagnostic.t list) (sub : string) : bool =
    List.exists (fun (d : Diagnostic.t) -> contains_substring d.message sub) errs

  let%test "mixed record then hash entry errors deterministically" =
    let input = "let x = { a: 1, \"b\": 2 }" in
    match parse ~file_id:"<test>" input with
    | Ok _ -> false
    | Error errs -> diagnostics_contain_substring errs "cannot mix hash-style entries into record literal"

  let%test "mixed hash then spread entry errors deterministically" =
    let input = "let x = { \"a\": 1, ...rest }" in
    match parse ~file_id:"<test>" input with
    | Ok _ -> false
    | Error errs -> diagnostics_contain_substring errs "cannot mix record-style spread into hash literal"

  let%test "mixed hash then record field errors deterministically" =
    let input = "let x = { \"a\": 1, b: 2 }" in
    match parse ~file_id:"<test>" input with
    | Ok _ -> false
    | Error errs -> diagnostics_contain_substring errs "cannot mix record-style field into hash literal"

  let%test "hash literal missing comma reports deterministic error" =
    let input = "let x = { \"a\": 1 b: 2 }" in
    match parse ~file_id:"<test>" input with
    | Ok _ -> false
    | Error errs -> diagnostics_contain_substring errs "expected ',' or '}' after hash literal entry"

  let%test "record literal missing comma reports deterministic error" =
    let input = "let x = { a: 1 b: 2 }" in
    match parse ~file_id:"<test>" input with
    | Ok _ -> false
    | Error errs -> diagnostics_contain_substring errs "expected ',' or '}' after record literal entry"

  let%test "record literal duplicate spread reports deterministic error" =
    let input = "let x = { ...a, ...b }" in
    match parse ~file_id:"<test>" input with
    | Ok _ -> false
    | Error errs ->
        diagnostics_contain_substring errs "multiple spread entries in record literal are not supported yet"

  let rec collect_expr_ids (expr : AST.expression) : int list =
    let child_ids =
      match expr.expr with
      | AST.Integer _ | AST.Float _ | AST.Boolean _ | AST.String _ | AST.Identifier _ -> []
      | AST.Prefix (_, e) -> collect_expr_ids e
      | AST.Infix (l, _, r) -> collect_expr_ids l @ collect_expr_ids r
      | AST.TypeCheck (e, _) -> collect_expr_ids e
      | AST.If (cond, cons, alt) -> (
          collect_expr_ids cond
          @ collect_stmt_ids cons
          @
          match alt with
          | Some s -> collect_stmt_ids s
          | None -> [])
      | AST.Function f -> collect_stmt_ids f.body
      | AST.Call (f, args) -> collect_expr_ids f @ List.concat_map collect_expr_ids args
      | AST.Array elements -> List.concat_map collect_expr_ids elements
      | AST.Hash pairs -> List.concat_map (fun (k, v) -> collect_expr_ids k @ collect_expr_ids v) pairs
      | AST.Index (container, index) -> collect_expr_ids container @ collect_expr_ids index
      | AST.EnumConstructor (_, _, args) -> List.concat_map collect_expr_ids args
      | AST.Match (scrutinee, arms) ->
          collect_expr_ids scrutinee
          @ List.concat_map (fun (arm : AST.match_arm) -> collect_expr_ids arm.body) arms
      | AST.RecordLit (fields, spread) -> (
          List.concat_map
            (fun (field : AST.record_field) ->
              match field.field_value with
              | Some e -> collect_expr_ids e
              | None -> [])
            fields
          @
          match spread with
          | Some e -> collect_expr_ids e
          | None -> [])
      | AST.FieldAccess (receiver, _) -> collect_expr_ids receiver
      | AST.MethodCall { mc_receiver; mc_args; _ } ->
          collect_expr_ids mc_receiver @ List.concat_map collect_expr_ids mc_args
      | AST.BlockExpr stmts -> List.concat_map collect_stmt_ids stmts
    in
    expr.id :: child_ids

  and collect_stmt_ids (stmt : AST.statement) : int list =
    match stmt.stmt with
    | AST.Let { value; _ } -> collect_expr_ids value
    | AST.Return e | AST.ExpressionStmt e -> collect_expr_ids e
    | AST.Block stmts -> List.concat_map collect_stmt_ids stmts
    | AST.EnumDef _ | AST.TypeDef _ | AST.ShapeDef _ | AST.TraitDef _ | AST.ImplDef _ | AST.InherentImplDef _
    | AST.DeriveDef _ | AST.TypeAlias _ ->
        []

  let%test "parser assigns unique expression ids for nested function and record literals" =
    let input = "fn outer(x) = { let mk = (v) -> { inner: v }; mk(x) }\nlet o = outer(5)\nputs(o.inner)" in
    match parse ~file_id:"<test>" input with
    | Error _ -> false
    | Ok program ->
        let ids = List.concat_map collect_stmt_ids program in
        let unique_ids = List.sort_uniq Int.compare ids in
        List.length ids = List.length unique_ids

  let%test "parse-invalid-number on malformed integer" =
    match parse ~file_id:"<test>" "let x = 99999999999999999999" with
    | Error errs -> List.exists (fun (d : Diagnostic.t) -> d.code = "parse-invalid-number") errs
    | _ -> false

  let%test "parse-invalid-pattern on missing variant name after dot" =
    match parse ~file_id:"<test>" "match 1 { case option.: 3 }" with
    | Error errs -> List.exists (fun (d : Diagnostic.t) -> d.code = "parse-invalid-pattern") errs
    | _ -> false

  let%test "parse-unexpected-token on missing brace after if condition" =
    match parse ~file_id:"<test>" "let x = if (true) 42" with
    | Error errs -> List.exists (fun (d : Diagnostic.t) -> d.code = "parse-unexpected-token") errs
    | _ -> false

  (* Phase 1b: Top-level fn declarations *)
  let%test "fn decl simple expression body" =
    match parse ~file_id:"<test>" "fn identity(x) = x" with
    | Ok
        [
          {
            AST.stmt =
              AST.Let
                { name = "identity"; value = { AST.expr = AST.Function { params = [ ("x", None) ]; _ }; _ }; _ };
            _;
          };
        ] ->
        true
    | _ -> false

  let%test "fn decl with typed param and return type" =
    match parse ~file_id:"<test>" "fn double(x: Int) -> Int = x" with
    | Ok
        [
          {
            AST.stmt =
              AST.Let
                {
                  name = "double";
                  value =
                    {
                      AST.expr =
                        AST.Function
                          {
                            params = [ ("x", Some (AST.TCon "Int")) ];
                            return_type = Some (AST.TCon "Int");
                            is_effectful = false;
                            _;
                          };
                      _;
                    };
                  _;
                };
            _;
          };
        ] ->
        true
    | _ -> false

  let%test "fn decl with block body" =
    match parse ~file_id:"<test>" "fn add(x: Int, y: Int) -> Int = { x + y }" with
    | Ok
        [
          {
            AST.stmt =
              AST.Let
                {
                  name = "add";
                  value = { AST.expr = AST.Function { body = { AST.stmt = AST.Block _; _ }; _ }; _ };
                  _;
                };
            _;
          };
        ] ->
        true
    | _ -> false

  let%test "fn decl effectful with fat arrow" =
    match parse ~file_id:"<test>" "fn greet(name: Str) => Str = puts(name)" with
    | Ok
        [
          {
            AST.stmt =
              AST.Let { name = "greet"; value = { AST.expr = AST.Function { is_effectful = true; _ }; _ }; _ };
            _;
          };
        ] ->
        true
    | _ -> false

  let%test "fn decl with generics" =
    match parse ~file_id:"<test>" "fn identity[a](x: a) -> a = x" with
    | Ok
        [
          {
            AST.stmt =
              AST.Let
                {
                  name = "identity";
                  value =
                    {
                      AST.expr =
                        AST.Function
                          {
                            generics = Some [ { AST.name = "a"; _ } ];
                            params = [ ("x", Some (AST.TVar "a")) ];
                            return_type = Some (AST.TVar "a");
                            _;
                          };
                      _;
                    };
                  _;
                };
            _;
          };
        ] ->
        true
    | _ -> false

  let%test "fn decl with bang suffix name" =
    match parse ~file_id:"<test>" "fn panic!() = 42" with
    | Ok [ { AST.stmt = AST.Let { name = "panic!"; _ }; _ } ] -> true
    | _ -> false

  let%test "block-style function literal is rejected" =
    match parse ~file_id:"<test>" "let f = fn(x) { x }" with
    | Ok _ -> false
    | Error _ -> true

  (* Phase 1c: Operator precedences *)
  let%test "logical or has lower precedence than equality" =
    (* a || b == c should be a || (b == c) *)
    match parse ~file_id:"<test>" "a || b == c" with
    | Ok [ { AST.stmt = AST.ExpressionStmt { AST.expr = AST.Infix (_, "||", _); _ }; _ } ] -> true
    | _ -> false

  let%test "logical and has lower precedence than equality" =
    (* a && b == c should be a && (b == c) *)
    match parse ~file_id:"<test>" "a && b == c" with
    | Ok [ { AST.stmt = AST.ExpressionStmt { AST.expr = AST.Infix (_, "&&", _); _ }; _ } ] -> true
    | _ -> false

  let%test "percent operator parses as infix" =
    match parse ~file_id:"<test>" "10 % 3" with
    | Ok [ { AST.stmt = AST.ExpressionStmt { AST.expr = AST.Infix (_, "%", _); _ }; _ } ] -> true
    | _ -> false

  (* Phase 1c: Supertrait with & separator *)
  let%test "supertrait accepts ampersand separator" =
    let input = "trait NamedShow: Named & Show = { fn label() -> Str }" in
    let lexer = Lexer.init input in
    match parse_program (init ~file_id:"<test>" lexer) with
    | Ok (_p, [ { AST.stmt = AST.TraitDef { supertraits; _ }; _ } ]) -> supertraits = [ "Named"; "Show" ]
    | _ -> false

  let%test "supertrait rejects plus separator" =
    match parse ~file_id:"<test>" "trait NamedShow: Named + Show = { fn label() -> Str }" with
    | Ok _ -> false
    | Error _ -> true

  (* Phase 1c: Positional params in method sig *)
  let%test "parse method sig with positional type params" =
    let input = "trait Show[a] = { fn show(a) -> Str }" in
    let lexer = Lexer.init input in
    match parse_program (init ~file_id:"<test>" lexer) with
    | Ok (_p, [ { AST.stmt = AST.TraitDef { methods; _ }; _ } ]) -> (
        match methods with
        | [ m ] -> List.length m.method_params = 1
        | _ -> false)
    | _ -> false

  (* Phase 1c: Default method impl *)
  let%test "parse method sig with default impl expression" =
    let input = "trait Foo[a] = { fn hello(a) -> Str = \"hello\" }" in
    match parse_surface ~file_id:"<test>" input with
    | Error _ -> false
    | Ok result -> (
        match result.program with
        | [ { Surface.std_decl = Surface.STraitDef { methods; _ }; _ } ] -> (
            match methods with
            | [ m ] -> Option.is_some m.sm_default_impl
            | _ -> false)
        | _ -> false)

  (* Phase 1d: Variant list with commas *)
  let%test "parse enum with comma-separated variants" =
    let input = "enum Color = { Red, Green, Blue }" in
    let lexer = Lexer.init input in
    match parse_program (init ~file_id:"<test>" lexer) with
    | Ok (_p, [ { AST.stmt = AST.EnumDef { variants; _ }; _ } ]) -> List.length variants = 3
    | _ -> false

  (* Phase 1d: Enum postfix derive *)
  let%test "parse enum with postfix derive" =
    let input = "enum Color = { Red, Green } derive Eq, Show" in
    match parse_surface ~file_id:"<test>" input with
    | Error _ -> false
    | Ok result -> (
        match result.program with
        | [ { Surface.std_decl = Surface.STypeDef { type_body = Surface.STNamedSum _; derive; _ }; _ } ] ->
            derive
            = [
                AST.{ derive_trait_name = "Eq"; derive_trait_constraints = [] };
                AST.{ derive_trait_name = "Show"; derive_trait_constraints = [] };
              ]
        | _ -> false)

  let%test "parse enum rejects missing equals" =
    match parse ~file_id:"<test>" "enum Color { Red, Green }" with
    | Ok _ -> false
    | Error _ -> true

  (* Phase 1d: named type postfix derive *)
  let%test "parse type definition with postfix derive" =
    let input = "type MyInt = Int derive Eq" in
    match parse_surface ~file_id:"<test>" input with
    | Error _ -> false
    | Ok result -> (
        match result.program with
        | [ { Surface.std_decl = Surface.STypeDef { derive; type_name = "MyInt"; _ }; _ } ] ->
            derive = [ AST.{ derive_trait_name = "Eq"; derive_trait_constraints = [] } ]
        | _ -> false)

  let%test "parse structural type declaration lowers to transparent type alias" =
    let input = "type Point = { x: Int, y: Int }" in
    match parse ~file_id:"<test>" input with
    | Ok [ { AST.stmt = AST.TypeAlias alias_def; _ } ] -> (
        alias_def.alias_name = "Point"
        &&
        match alias_def.alias_body with
        | AST.TRecord (fields, None) ->
            List.length fields = 2
            && List.exists (fun (field : AST.record_type_field) -> field.field_name = "x") fields
            && List.exists (fun (field : AST.record_type_field) -> field.field_name = "y") fields
        | _ -> false)
    | _ -> false

  let%test "parse transparent type with postfix derive lowers to alias plus derive" =
    let input = "type Box[t] = { value: t } derive Show" in
    match parse ~file_id:"<test>" input with
    | Ok
        [
          { AST.stmt = AST.TypeAlias alias_def; _ };
          {
            AST.stmt =
              AST.DeriveDef
                { derive_traits = [ derive_trait ]; derive_for_type = AST.TApp ("Box", [ AST.TVar "t" ]) };
            _;
          };
        ] ->
        alias_def.alias_name = "Box"
        && alias_def.alias_type_params = [ "t" ]
        && derive_trait.derive_trait_name = "Show"
    | _ -> false

  let%test "parse explicit wrapper type syntax" =
    let input = "type UserId = UserId(Int)" in
    match parse ~file_id:"<test>" input with
    | Ok
        [
          {
            AST.stmt =
              AST.TypeDef
                { type_name = "UserId"; type_type_params = []; type_body = AST.NamedTypeWrapper (AST.TCon "Int") };
            _;
          };
        ] ->
        true
    | _ -> false

  let%test "parse constructor-bearing type lowers to enum definition" =
    let input = "type Event = { Click(Int), Quit }" in
    match parse ~file_id:"<test>" input with
    | Ok
        [
          {
            AST.stmt =
              AST.EnumDef
                {
                  name = "Event";
                  type_params = [];
                  variants = [ { AST.variant_name = "Click"; _ }; { AST.variant_name = "Quit"; _ } ];
                };
            _;
          };
        ] ->
        true
    | _ -> false

  let%test "enum sugar canonicalizes to sum type surface" =
    let input = "enum Event = { Click(Int), Quit }" in
    match parse_surface ~file_id:"<test>" input with
    | Error _ -> false
    | Ok result -> (
        match result.program with
        | [
         {
           Surface.std_decl =
             Surface.STypeDef
               {
                 type_name = "Event";
                 type_type_params = [];
                 type_body = Surface.STNamedSum [ { sv_name = "Click"; _ }; { sv_name = "Quit"; _ } ];
                 derive = [];
               };
           _;
         };
        ] ->
            true
        | _ -> false)

  let%test "parse type definition preserves postfix derive target params" =
    let input = "type Box[t] = { value: t } derive Forwarder[u]" in
    match parse_surface ~file_id:"<test>" input with
    | Error _ -> false
    | Ok result -> (
        match result.program with
        | [ { Surface.std_decl = Surface.STypeDef { derive = [ derive_trait ]; type_name = "Box"; _ }; _ } ] ->
            derive_trait.derive_trait_name = "Forwarder"
            && derive_trait.derive_trait_constraints = [ AST.{ name = "u"; constraints = [] } ]
        | _ -> false)

  (* Phase 1d: vNext impl syntax *)
  let%test "parse vNext impl with type application head" =
    let input = "impl[a] Show[Option[a]] = { fn show(x: Option[a]) -> Str = { \"\" } }" in
    match parse ~file_id:"<test>" input with
    | Ok
        [
          {
            AST.stmt =
              AST.ImplDef
                {
                  impl_trait_name = "Show";
                  impl_type_params = [ { AST.name = "a"; constraints = [] } ];
                  impl_for_type = AST.TApp ("Option", [ AST.TVar "a" ]);
                  impl_methods;
                };
            _;
          };
        ] ->
        List.length impl_methods = 1
    | _ -> false

  let%test "parse impl requires bracket application syntax" =
    match parse ~file_id:"<test>" "impl Show for Int { fn show(x: Int) -> Str = { x } }" with
    | Ok _ -> false
    | Error _ -> true

  let%test "parse vNext inherent impl with = syntax" =
    let input = "impl Monkey = { fn greet() -> Str = { \"hello\" } }" in
    match parse_surface ~file_id:"<test>" input with
    | Error _ -> false
    | Ok result -> (
        match result.program with
        | [ { Surface.std_decl = Surface.SInherentImplDef { inherent_for_type; inherent_methods }; _ } ] ->
            inherent_for_type = Surface.STCon "Monkey" && List.length inherent_methods = 1
        | _ -> false)

  let%test "parse vNext generic inherent impl with inferred binders" =
    let input = "impl Result[a, b] = { fn ok(r: Result[a, b]) -> Bool = { true } }" in
    match parse ~file_id:"<test>" input with
    | Ok
        [
          {
            AST.stmt =
              AST.InherentImplDef
                { inherent_for_type = AST.TApp ("Result", [ AST.TCon "a"; AST.TCon "b" ]); inherent_methods };
            _;
          };
        ] ->
        List.length inherent_methods = 1
    | _ -> false

  (* Phase 1d: override keyword *)
  let%test "parse impl method with override keyword" =
    let input = "impl Foo = { override fn bar() -> Int = { 1 } }" in
    match parse_surface ~file_id:"<test>" input with
    | Error _ -> false
    | Ok result -> (
        match result.program with
        | [ { Surface.std_decl = Surface.SInherentImplDef { inherent_methods; _ }; _ } ] -> (
            match inherent_methods with
            | [ m ] -> m.smi_override = true
            | _ -> false)
        | _ -> false)

  (* Phase 1d: method impl with = expr syntax *)
  let%test "parse method impl with expression body" =
    let input = "impl Foo = { fn bar() -> Int = 42 }" in
    match parse_surface ~file_id:"<test>" input with
    | Error _ -> false
    | Ok result -> (
        match result.program with
        | [ { Surface.std_decl = Surface.SInherentImplDef { inherent_methods; _ }; _ } ] -> (
            match inherent_methods with
            | [ m ] -> (
                match m.smi_body with
                | Surface.SEOBExpr _ -> true
                | _ -> false)
            | _ -> false)
        | _ -> false)

  let%test "parse impl method rejects brace-only body" =
    match parse ~file_id:"<test>" "impl Foo = { fn bar() -> Int { 1 } }" with
    | Ok _ -> false
    | Error _ -> true

  (* Phase 1e: Lambda parsing *)
  let%test "parse empty lambda" =
    let input = "() -> 42" in
    match parse_surface ~file_id:"<test>" input with
    | Error _ -> false
    | Ok result -> (
        match result.program with
        | [ { Surface.std_decl = Surface.SExpressionStmt e; _ } ] -> (
            match e.se_expr with
            | Surface.SEArrowLambda { se_lambda_params = []; _ } -> true
            | _ -> false)
        | _ -> false)

  let%test "parse single-param lambda" =
    let input = "(x) -> x" in
    match parse_surface ~file_id:"<test>" input with
    | Error _ -> false
    | Ok result -> (
        match result.program with
        | [ { Surface.std_decl = Surface.SExpressionStmt e; _ } ] -> (
            match e.se_expr with
            | Surface.SEArrowLambda { se_lambda_params = [ ("x", None) ]; _ } -> true
            | _ -> false)
        | _ -> false)

  let%test "parse multi-param lambda" =
    let input = "(x, y) -> x + y" in
    match parse_surface ~file_id:"<test>" input with
    | Error _ -> false
    | Ok result -> (
        match result.program with
        | [ { Surface.std_decl = Surface.SExpressionStmt e; _ } ] -> (
            match e.se_expr with
            | Surface.SEArrowLambda { se_lambda_params = [ ("x", None); ("y", None) ]; _ } -> true
            | _ -> false)
        | _ -> false)

  let%test "parse effectful lambda with fat arrow" =
    let input = "(x) => puts(x)" in
    match parse_surface ~file_id:"<test>" input with
    | Error _ -> false
    | Ok result -> (
        match result.program with
        | [ { Surface.std_decl = Surface.SExpressionStmt e; _ } ] -> (
            match e.se_expr with
            | Surface.SEArrowLambda { se_lambda_is_effectful = true; _ } -> true
            | _ -> false)
        | _ -> false)

  let%test "grouped expression still works" =
    match parse ~file_id:"<test>" "(1 + 2) * 3" with
    | Ok [ { AST.stmt = AST.ExpressionStmt { AST.expr = AST.Infix (_, "*", _); _ }; _ } ] -> true
    | _ -> false

  (* Phase 1e: case keyword is mandatory on match arms *)
  let%test "parse match arm with case keyword" =
    let input = "match x { case option.some(v): v case option.none: 0 }" in
    match parse_surface ~file_id:"<test>" input with
    | Error _ -> false
    | Ok result -> (
        match result.program with
        | [ { Surface.std_decl = Surface.SExpressionStmt e; _ } ] -> (
            match e.se_expr with
            | Surface.SEMatch (_, arms) -> List.length arms = 2
            | _ -> false)
        | _ -> false)

  let%test "parse match arm rejects missing case keyword" =
    match parse ~file_id:"<test>" "match x { option.some(v): v }" with
    | Ok _ -> false
    | Error _ -> true
end

(* Phase 4.3: Trait definition tests *)
let%test "parse simple trait definition" =
  let input = "trait Show[a] = { fn show(x: a) -> Str }" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TraitDef trait_def ->
              trait_def.name = "Show"
              && trait_def.type_param = Some "a"
              && trait_def.supertraits = []
              && List.length trait_def.methods = 1
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse trait with multiple methods" =
  let input = "trait Num[a] = { fn add(x: a, y: a) -> a fn sub(x: a, y: a) -> a }" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TraitDef trait_def ->
              trait_def.name = "Num" && trait_def.type_param = Some "a" && List.length trait_def.methods = 2
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse trait without type parameter" =
  let input = "trait Ping = { fn ping(x: Int) -> Int }" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TraitDef trait_def ->
              trait_def.name = "Ping"
              && trait_def.type_param = None
              && trait_def.supertraits = []
              && List.length trait_def.methods = 1
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse trait with supertraits" =
  let input = "trait Ord[a]: Eq = { fn compare(x: a, y: a) -> Int }" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TraitDef trait_def ->
              trait_def.name = "Ord"
              && trait_def.type_param = Some "a"
              && trait_def.supertraits = [ "Eq" ]
              && List.length trait_def.methods = 1
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse trait with multiple supertraits" =
  let input = "trait Hashable[a]: Eq & Show = { fn hash(x: a) -> Int }" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TraitDef trait_def ->
              trait_def.name = "Hashable"
              && trait_def.type_param = Some "a"
              && trait_def.supertraits = [ "Eq"; "Show" ]
              && List.length trait_def.methods = 1
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse non-generic trait with supertraits" =
  let input = "trait NamedShow: Named & Show = { fn label() -> Str }" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TraitDef trait_def ->
              trait_def.name = "NamedShow"
              && trait_def.type_param = None
              && trait_def.supertraits = [ "Named"; "Show" ]
              && List.length trait_def.methods = 1
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse field-only trait definition is rejected" =
  match parse ~file_id:"<test>" "trait Named = { name: Str }" with
  | Ok _ -> false
  | Error errs ->
      List.exists (fun (d : Diagnostic.t) -> d.code = "parse-invalid-trait-member") errs
      && List.exists
           (fun (d : Diagnostic.t) ->
             String_utils.contains_substring ~needle:"use 'shape' for structural field contracts" d.message)
           errs

let%test "parse trait member missing fn uses neutral member error" =
  match parse ~file_id:"<test>" "trait Printable[a] = { format(x: a) -> Str }" with
  | Ok _ -> false
  | Error errs ->
      List.exists (fun (d : Diagnostic.t) -> d.code = "parse-invalid-trait-member") errs
      && List.exists
           (fun (d : Diagnostic.t) -> String_utils.contains_substring ~needle:"must start with 'fn'" d.message)
           errs

let%test "parse let in trait body uses neutral member error" =
  match parse ~file_id:"<test>" "trait Printable[a] = { let x = 1 }" with
  | Ok _ -> false
  | Error errs ->
      List.exists (fun (d : Diagnostic.t) -> d.code = "parse-invalid-trait-member") errs
      && List.exists
           (fun (d : Diagnostic.t) -> String_utils.contains_substring ~needle:"must start with 'fn'" d.message)
           errs

let%test "parse parameter constraint shorthand with ampersand" =
  match parse_surface ~file_id:"<test>" "fn describe(x: Named & Aged) -> Str = x.name" with
  | Error _ -> false
  | Ok result -> (
      match result.program with
      | [
       {
         Surface.std_decl =
           Surface.SFnDecl { params = [ ("x", Some (Surface.STConstraintShorthand [ "Named"; "Aged" ])) ]; _ };
         _;
       };
      ] ->
          true
      | _ -> false)

let%test "parse parenthesized parameter intersection does not become shorthand" =
  match parse_surface ~file_id:"<test>" "fn keep(x: (Foo & Bar)) -> Foo = x" with
  | Error _ -> false
  | Ok result -> (
      match result.program with
      | [
       {
         Surface.std_decl =
           Surface.SFnDecl
             { params = [ ("x", Some (Surface.STIntersection [ Surface.STCon "Foo"; Surface.STCon "Bar" ])) ]; _ };
         _;
       };
      ] ->
          true
      | _ -> false)

let%test "parse mixed trait definition is rejected" =
  match parse ~file_id:"<test>" "trait Printable[a] = { name: Str fn format(x: a) -> Str }" with
  | Ok _ -> false
  | Error _ -> true

let%test "parse trait rejects missing equals" =
  match parse ~file_id:"<test>" "trait Show[a] { fn show(x: a) -> Str }" with
  | Ok _ -> false
  | Error _ -> true

(* Phase 4.3: Impl block tests *)
let%test "parse impl - just keyword" =
  let input = "impl" in
  let lexer = Lexer.init input in
  let p = init ~file_id:"<test>" lexer in
  curr_token_is p Token.Impl

let%test "parse basic impl block - debug" =
  let input = "impl Show[Int] = { fn show(x: Int) -> Str = { x } }" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
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
      let msgs = List.map (fun (d : Diagnostic.t) -> d.message) p.errors in
      Printf.printf "Parse error: %s\n%!" (String.concat "; " msgs);
      false

let%test "parse basic impl block" =
  let input = "impl Show[Int] = { fn show(x: Int) -> Str = { x } }" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.ImplDef impl_def ->
              impl_def.impl_trait_name = "Show"
              && impl_def.impl_type_params = []
              && List.length impl_def.impl_methods = 1
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse basic inherent impl block" =
  let input = "impl Point = { fn sum(p: Point) -> Int = { p.x + p.y } }" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.InherentImplDef inherent_def -> (
              match inherent_def.inherent_for_type with
              | AST.TCon "Point" -> List.length inherent_def.inherent_methods = 1
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse inherent impl with bracketed type target" =
  let input = "impl List[Int] = { fn size(xs: List[Int]) -> Int = { 0 } }" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.InherentImplDef inherent_def -> (
              match inherent_def.inherent_for_type with
              | AST.TApp ("List", [ AST.TCon "Int" ]) -> List.length inherent_def.inherent_methods = 1
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse impl with type params" =
  let input = "impl[a: Eq] Eq[List[a]] = { fn eq(x: List[a], y: List[a]) -> Bool = { true } }" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.ImplDef impl_def ->
              let ok1 = impl_def.impl_trait_name = "Eq" in
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
      let msgs = List.map (fun (d : Diagnostic.t) -> d.message) p.errors in
      Printf.printf "Parse error: %s\n%!" (String.concat "; " msgs);
      false

let%test "parse impl with multiple constraints" =
  let input = "impl[a: Eq & Ord] Show[Option[a]] = { fn show(x: Option[a]) -> Str = { \"\" } }" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.ImplDef impl_def -> (
              impl_def.impl_trait_name = "Show"
              && List.length impl_def.impl_type_params = 1
              &&
              match List.hd impl_def.impl_type_params with
              | { name = "a"; constraints = [ "Eq"; "Ord" ] } -> true
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse impl with multiple methods" =
  let input = "impl Num[Int] = { fn add(x: Int, y: Int) -> Int = { x } fn sub(x: Int, y: Int) -> Int = { y } }" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.ImplDef impl_def -> impl_def.impl_trait_name = "Num" && List.length impl_def.impl_methods = 2
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse impl method body with direct record literal" =
  let input =
    "type Vec2 = { x: Int }\ntrait Num[a] = { fn add(x: a, y: a) -> a }\nimpl Num[Vec2] = { fn add(x: Vec2, y: Vec2) -> Vec2 = { { x: x.x + y.x } } }"
  in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ _; _; stmt ] -> (
          match stmt.stmt with
          | AST.ImplDef impl_def -> (
              match impl_def.impl_methods with
              | [ method_impl ] -> (
                  match method_impl.impl_method_body.stmt with
                  | AST.Block [ { stmt = AST.ExpressionStmt { expr = AST.RecordLit _; _ }; _ } ] -> true
                  | _ -> false)
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse impl method body with if expression and continue parsing next statement" =
  let input =
    "trait Pick[a] = { fn pick(x: a, y: a) -> a }\nimpl Pick[Int] = { fn pick(x: Int, y: Int) -> Int = { if (true) { x } else { y } } }\n1"
  in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ _trait; impl_stmt; expr_stmt ] -> (
          match (impl_stmt.stmt, expr_stmt.stmt) with
          | AST.ImplDef impl_def, AST.ExpressionStmt expr -> (
              match impl_def.impl_methods with
              | [ method_impl ] -> (
                  match (method_impl.impl_method_body.stmt, expr.expr) with
                  | AST.Block [ { stmt = AST.ExpressionStmt { expr = AST.If _; _ }; _ } ], AST.Integer 1L -> true
                  | _ -> false)
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse impl method rejects missing equals before body" =
  match parse ~file_id:"<test>" "impl Int = { fn show(x: Int) -> Str { x } }" with
  | Ok _ -> false
  | Error _ -> true

let%test "standalone derive statement is a parse error" =
  let input = "derive Eq for Color" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok _ -> false
  | Error _ -> true

let%test "standalone derive after type is a parse error" =
  match parse ~file_id:"<test>" "type Point = Int\nderive Eq for Point" with
  | Ok _ -> false
  | Error _ -> true

(* Phase 4.4: Transparent type tests *)
let%test "parse type - just keyword" =
  let input = "type" in
  let lexer = Lexer.init input in
  let p = init ~file_id:"<test>" lexer in
  curr_token_is p Token.Type

let%test "parse alias declaration with alias keyword is rejected" =
  match parse ~file_id:"<test>" "alias Point = Int" with
  | Ok _ -> false
  | Error _ -> true

let%test "parse shape declaration with shape keyword" =
  match parse ~file_id:"<test>" "shape HasName = { name: Str }" with
  | Ok _ -> true
  | Error _ -> false

let%test "parse simple transparent type" =
  let input = "type Point = Int" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TypeAlias alias_def -> (
              alias_def.alias_name = "Point"
              && alias_def.alias_type_params = []
              &&
              match alias_def.alias_body with
              | AST.TCon "Int" -> true
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse transparent type with function type body" =
  let input = "type Endo = (Int) -> Int" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TypeAlias alias_def -> (
              match alias_def.alias_body with
              | AST.TArrow ([ AST.TCon "Int" ], AST.TCon "Int", false) -> true
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse Dyn trait object type" =
  let input = "type Printer = Dyn[Show]" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TypeAlias alias_def -> (
              match alias_def.alias_body with
              | AST.TTraitObject [ "Show" ] -> true
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse Dyn multi-trait function parameter annotation" =
  let input = "fn show(x: Dyn[Show & Eq]) -> Str = x.show()" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.Let { value = { expr = AST.Function fn; _ }; _ } -> (
              match fn.params with
              | [ ("x", Some (AST.TTraitObject [ "Show"; "Eq" ])) ] -> true
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse Dyn rejects empty trait set" =
  match parse ~file_id:"<test>" "type Empty = Dyn[]" with
  | Ok _ -> false
  | Error _ -> true

let%test "parse transparent type with intersection body" =
  let input = "type NamedAge = { name: Str } & { age: Int }" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TypeAlias alias_def -> (
              match alias_def.alias_body with
              | AST.TIntersection [ AST.TRecord _; AST.TRecord _ ] -> true
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse type intersection binds tighter than union" =
  let input = "type Example = Int | Str & Bool" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TypeAlias alias_def -> (
              match alias_def.alias_body with
              | AST.TUnion [ AST.TCon "Int"; AST.TIntersection [ AST.TCon "Str"; AST.TCon "Bool" ] ] -> true
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse type intersection parenthesizes function members" =
  let input = "type Example = ((Int) -> Str) & Bool" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TypeAlias alias_def -> (
              match alias_def.alias_body with
              | AST.TIntersection [ AST.TArrow ([ AST.TCon "Int" ], AST.TCon "Str", false); AST.TCon "Bool" ] ->
                  true
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse function parameter annotation with function type" =
  let input = "fn apply(f: (Int) -> Int, x: Int) -> Int = f(x)" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.Let { value = { expr = AST.Function fn; _ }; _ } -> (
              match fn.params with
              | [
               ("f", Some (AST.TArrow ([ AST.TCon "Int" ], AST.TCon "Int", false))); ("x", Some (AST.TCon "Int"));
              ] ->
                  true
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse function type requires parenthesized arrow syntax" =
  match parse ~file_id:"<test>" "type Endo = fn(Int) -> Int" with
  | Ok _ -> false
  | Error _ -> true

let%test "parse transparent type followed by let without semicolon" =
  let input = "type MyInt = Int\nlet x: MyInt = 1\nx" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> List.length program = 3
  | Error _ -> false

let%test "parse transparent type with generic param" =
  let input = "type Box[a] = a" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TypeAlias alias_def -> (
              alias_def.alias_name = "Box"
              && alias_def.alias_type_params = [ "a" ]
              &&
              match alias_def.alias_body with
              | AST.TVar "a" -> true
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse named product constructor-call syntax" =
  match parse ~file_id:"<test>" "let user = User(first_name: \"Ada\", last_name: \"Lovelace\")" with
  | Ok _ -> true
  | Error _ -> false

let%test "parse transparent type with multiple generic params" =
  let input = "type Pair[a, b] = Int" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TypeAlias alias_def -> alias_def.alias_name = "Pair" && alias_def.alias_type_params = [ "a"; "b" ]
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse record type - empty" =
  let input = "type UnitLike = { }" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
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
  let input = "type Point = { x: Int }" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
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
                  | AST.TCon "Int" -> true
                  | _ -> false)
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse record type - multiple fields" =
  let input = "type Point = { x: Int, y: Int }" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
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
  let input = "type PointRow[r] = { x: Int, ...r }" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TypeAlias alias_def -> (
              match alias_def.alias_body with
              | AST.TRecord (fields, Some (AST.TVar "r")) ->
                  List.length fields = 1 && (List.hd fields).field_name = "x"
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse record type - only row variable" =
  let input = "type Any[r] = { ...r }" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.TypeAlias alias_def -> (
              match alias_def.alias_body with
              | AST.TRecord ([], Some (AST.TVar "r")) -> true
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

(* Phase 4.6: Record literal tests *)
let%test "parse empty record literal with spread" =
  let input = "let x = { ...base }" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
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
  match parse_program (init ~file_id:"<test>" lexer) with
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
  match parse_program (init ~file_id:"<test>" lexer) with
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

let%test "parse record literal - nested record field value" =
  let input = "let p = { a: { x: 2, y: 3 } }" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.Let { value; _ } -> (
              match value.expr with
              | AST.RecordLit (fields, None) -> (
                  match fields with
                  | [ field ] when field.field_name = "a" -> (
                      match field.field_value with
                      | Some { expr = AST.RecordLit (inner_fields, None); _ } ->
                          List.length inner_fields = 2
                          && (List.nth inner_fields 0).field_name = "x"
                          && (List.nth inner_fields 1).field_name = "y"
                      | _ -> false)
                  | _ -> false)
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse let with record literal followed by let" =
  let input = "let p = { x: 1, y: 2 }\nlet q = p.x\nq" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> List.length program = 3
  | Error _ -> false

let%test "parse record literal - punning single" =
  let input = "let p = { x: }" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
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
  match parse_program (init ~file_id:"<test>" lexer) with
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
  match parse_program (init ~file_id:"<test>" lexer) with
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
  match parse_program (init ~file_id:"<test>" lexer) with
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
  match parse_program (init ~file_id:"<test>" lexer) with
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

let%test "parse named product constructor-call with spread update" =
  let input = "let p = Point(...base, x: 10)" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.Let { value; _ } -> (
              match value.expr with
              | AST.Call ({ expr = AST.Identifier "Point"; _ }, [ { expr = AST.RecordLit (fields, Some _); _ } ])
                ->
                  List.length fields = 1 && (List.hd fields).field_name = "x"
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse hash literal still works" =
  let input = "let h = { \"x\": 10 }" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
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
  match parse_program (init ~file_id:"<test>" lexer) with
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
  match parse_program (init ~file_id:"<test>" lexer) with
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

let%test "parse field access - chained across newline before dot" =
  let input = "let x = r.a\n  .b" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.Let { value; _ } -> (
              match value.expr with
              | AST.FieldAccess (inner, "b") -> (
                  match inner.expr with
                  | AST.FieldAccess (receiver, "a") -> (
                      match receiver.expr with
                      | AST.Identifier "r" -> true
                      | _ -> false)
                  | _ -> false)
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse field access vs method call" =
  let input = "let x = r.field; let y = r.method()" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt1; stmt2 ] -> (
          match (stmt1.stmt, stmt2.stmt) with
          | AST.Let { name = "x"; value = v1; _ }, AST.Let { name = "y"; value = v2; _ } -> (
              match (v1.expr, v2.expr) with
              | AST.FieldAccess (_, "field"), AST.MethodCall { mc_method = "method"; mc_args = []; _ } -> true
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

let%test "parse method chain across newline before dot" =
  let input = "let x = 42.show()\n  .show()" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
  | Ok (_p, program) -> (
      match program with
      | [ stmt ] -> (
          match stmt.stmt with
          | AST.Let { value; _ } -> (
              match value.expr with
              | AST.MethodCall { mc_receiver; mc_method = "show"; mc_args = []; _ } -> (
                  match mc_receiver.expr with
                  | AST.MethodCall { mc_receiver = inner_recv; mc_method = "show"; mc_args = []; _ } -> (
                      match inner_recv.expr with
                      | AST.Integer 42L -> true
                      | _ -> false)
                  | _ -> false)
              | _ -> false)
          | _ -> false)
      | _ -> false)
  | Error _ -> false

(* Phase 4.8: Record pattern tests *)
let%test "parse record pattern - simple punning" =
  let input = "match p { case { x:, y: }: x }" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
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
  let input = "match p { case { x: a, y: b }: a }" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
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
  let input = "match p { case { x:, ...rest }: x }" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
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
  let input = "match p { case { }: 42 }" in
  let lexer = Lexer.init input in
  match parse_program (init ~file_id:"<test>" lexer) with
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

let%test "parse unqualified wrapper constructor pattern with flattened record payload sugar" =
  let input = "match user { case User(name:, ...rest): name }" in
  match parse ~file_id:"<test>" input with
  | Ok
      [
        {
          AST.stmt =
            AST.ExpressionStmt
              {
                AST.expr =
                  AST.Match
                    ( _,
                      [
                        {
                          AST.patterns =
                            [
                              {
                                AST.pat =
                                  AST.PConstructor
                                    ( "User",
                                      "User",
                                      [
                                        {
                                          AST.pat =
                                            AST.PRecord
                                              ( [ { AST.pat_field_name = "name"; pat_field_pattern = None } ],
                                                Some "rest" );
                                          _;
                                        };
                                      ] );
                                _;
                              };
                            ];
                          _;
                        };
                      ] );
                _;
              };
          _;
        };
      ] ->
      true
  | _ -> false

let%test "parse unqualified nullary constructor pattern" =
  let input = "match evt { case Quit: 0 }" in
  match parse ~file_id:"<test>" input with
  | Ok
      [
        {
          AST.stmt =
            AST.ExpressionStmt
              {
                AST.expr =
                  AST.Match
                    (_, [ { AST.patterns = [ { AST.pat = AST.PConstructor ("Quit", "Quit", []); _ } ]; _ } ]);
                _;
              };
          _;
        };
      ] ->
      true
  | _ -> false
