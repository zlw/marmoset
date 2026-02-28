(* Semantic tokens: walk AST and emit typed token array for rich highlighting *)

module Lsp_t = Linol_lsp.Types
module Ast = Marmoset.Lib.Ast
module Infer = Marmoset.Lib.Infer
module Types = Marmoset.Lib.Types

(* Token type indices — must match the legend order in server.ml *)
let namespace_type = 0
let _type_type = 1
let enum_type = 2
let interface_type = 3
let parameter_type = 4
let variable_type = 5
let enum_member_type = 6
let function_type = 7
let method_type = 8
let string_type = 11
let number_type = 12
let operator_type = 13
let property_type = 14

(* Modifier bit flags *)
let declaration_mod = 1 (* bit 0 *)
let definition_mod = 2 (* bit 1 *)

(* Token types list for the legend *)
let token_types =
  [
    "namespace";
    "type";
    "enum";
    "interface";
    "parameter";
    "variable";
    "enumMember";
    "function";
    "method";
    "keyword";
    "comment";
    "string";
    "number";
    "operator";
    "property";
  ]

let token_modifiers = [ "declaration"; "definition" ]

(* A raw token before delta-encoding *)
type raw_token = {
  pos : int;
  end_pos : int;
  token_type : int;
  modifiers : int;
}

(* Find the byte offset of a name within a range of source, respecting word boundaries *)
let find_name ~source ~start ~limit name =
  let name_len = String.length name in
  let src_len = String.length source in
  let limit = min limit src_len in
  let is_ident_char c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' in
  let rec scan i =
    if i + name_len > limit then
      None
    else if String.sub source i name_len = name then
      let after = i + name_len in
      let before_ok = i = 0 || not (is_ident_char source.[i - 1]) in
      let after_ok = after >= src_len || not (is_ident_char source.[after]) in
      if before_ok && after_ok then
        Some (i, after)
      else
        scan (i + 1)
    else
      scan (i + 1)
  in
  scan start

(* Find a substring without word-boundary checks — for operators and punctuation *)
let find_substr ~source ~start ~limit name =
  let name_len = String.length name in
  let src_len = String.length source in
  let limit = min limit src_len in
  let rec scan i =
    if i + name_len > limit then
      None
    else if String.sub source i name_len = name then
      Some (i, i + name_len)
    else
      scan (i + 1)
  in
  scan start

(* Collect raw tokens from an expression *)
let rec collect_expr ~source ~type_map ~environment ~params ~tokens (expr : Ast.AST.expression) =
  match expr.expr with
  | Ast.AST.Integer _ | Ast.AST.Float _ ->
      tokens := { pos = expr.pos; end_pos = expr.end_pos; token_type = number_type; modifiers = 0 } :: !tokens
  | Ast.AST.String _ ->
      tokens := { pos = expr.pos; end_pos = expr.end_pos; token_type = string_type; modifiers = 0 } :: !tokens
  | Ast.AST.Boolean _ -> (* Skip — keyword territory *) ()
  | Ast.AST.Identifier name ->
      let token_type =
        if List.mem name params then
          parameter_type
        else
          match Infer.TypeEnv.find_opt name environment with
          | Some (Types.Forall (_, Types.TFun _)) -> function_type
          | _ -> (
              match Hashtbl.find_opt type_map expr.id with
              | Some (Types.TFun _) -> function_type
              | _ -> variable_type)
      in
      tokens := { pos = expr.pos; end_pos = expr.end_pos; token_type; modifiers = 0 } :: !tokens
  | Ast.AST.Prefix (op, e) ->
      (* Operator token: pos to start of sub-expression *)
      let op_len = String.length op in
      if op_len > 0 then
        tokens :=
          { pos = expr.pos; end_pos = expr.pos + op_len - 1; token_type = operator_type; modifiers = 0 }
          :: !tokens;
      collect_expr ~source ~type_map ~environment ~params ~tokens e
  | Ast.AST.Infix (left, op, right) ->
      collect_expr ~source ~type_map ~environment ~params ~tokens left;
      (* Find operator position between left and right — use find_substr for operators *)
      let op_len = String.length op in
      (match find_substr ~source ~start:left.end_pos ~limit:(right.pos + 1) op with
      | Some (op_start, _) ->
          tokens :=
            { pos = op_start; end_pos = op_start + op_len - 1; token_type = operator_type; modifiers = 0 }
            :: !tokens
      | None -> ());
      collect_expr ~source ~type_map ~environment ~params ~tokens right
  | Ast.AST.Function { params = fn_params; body; _ } ->
      let param_names = List.map fst fn_params in
      (* Emit parameter tokens *)
      let search_from = ref expr.pos in
      List.iter
        (fun (pname, _annotation) ->
          match find_name ~source ~start:!search_from ~limit:expr.end_pos pname with
          | Some (pstart, pend) ->
              search_from := pend;
              tokens :=
                { pos = pstart; end_pos = pend - 1; token_type = parameter_type; modifiers = declaration_mod }
                :: !tokens
          | None -> ())
        fn_params;
      (* Recurse into body with params in scope *)
      let new_params = param_names @ params in
      collect_stmt ~source ~type_map ~environment ~params:new_params ~tokens body
  | Ast.AST.Call (fn_expr, args) ->
      collect_expr ~source ~type_map ~environment ~params ~tokens fn_expr;
      List.iter (collect_expr ~source ~type_map ~environment ~params ~tokens) args
  | Ast.AST.If (cond, then_, else_) ->
      collect_expr ~source ~type_map ~environment ~params ~tokens cond;
      collect_stmt ~source ~type_map ~environment ~params ~tokens then_;
      Option.iter (collect_stmt ~source ~type_map ~environment ~params ~tokens) else_
  | Ast.AST.Index (arr, idx) ->
      collect_expr ~source ~type_map ~environment ~params ~tokens arr;
      collect_expr ~source ~type_map ~environment ~params ~tokens idx
  | Ast.AST.Array elts -> List.iter (collect_expr ~source ~type_map ~environment ~params ~tokens) elts
  | Ast.AST.Hash pairs ->
      List.iter
        (fun (k, v) ->
          collect_expr ~source ~type_map ~environment ~params ~tokens k;
          collect_expr ~source ~type_map ~environment ~params ~tokens v)
        pairs
  | Ast.AST.FieldAccess (e, field) -> (
      collect_expr ~source ~type_map ~environment ~params ~tokens e;
      (* Field name token — search from the dot (expr.pos) since inner chain
         expressions may have incorrect end_pos *)
      let field_len = String.length field in
      match find_name ~source ~start:expr.pos ~limit:(expr.end_pos + 1) field with
      | Some (fstart, _) ->
          tokens :=
            { pos = fstart; end_pos = fstart + field_len - 1; token_type = property_type; modifiers = 0 }
            :: !tokens
      | None -> ())
  | Ast.AST.MethodCall (recv, method_name, args) ->
      collect_expr ~source ~type_map ~environment ~params ~tokens recv;
      (* Method name token — search from the dot (expr.pos) since inner chain
         expressions may have incorrect end_pos *)
      let mlen = String.length method_name in
      (match find_name ~source ~start:expr.pos ~limit:(expr.end_pos + 1) method_name with
      | Some (mstart, _) ->
          tokens :=
            { pos = mstart; end_pos = mstart + mlen - 1; token_type = method_type; modifiers = 0 } :: !tokens
      | None -> ());
      List.iter (collect_expr ~source ~type_map ~environment ~params ~tokens) args
  | Ast.AST.Match (scrutinee, arms) ->
      collect_expr ~source ~type_map ~environment ~params ~tokens scrutinee;
      List.iter
        (fun (arm : Ast.AST.match_arm) ->
          List.iter (collect_pattern ~source ~tokens) arm.patterns;
          collect_expr ~source ~type_map ~environment ~params ~tokens arm.body)
        arms
  | Ast.AST.RecordLit (fields, spread) ->
      let search_from = ref expr.pos in
      List.iter
        (fun (f : Ast.AST.record_field) ->
          (* Field name as property *)
          let fname = f.field_name in
          let fname_len = String.length fname in
          (match find_name ~source ~start:!search_from ~limit:(expr.end_pos + 1) fname with
          | Some (fstart, fend) ->
              search_from := fend;
              tokens :=
                { pos = fstart; end_pos = fstart + fname_len - 1; token_type = property_type; modifiers = 0 }
                :: !tokens
          | None -> ());
          Option.iter (collect_expr ~source ~type_map ~environment ~params ~tokens) f.field_value)
        fields;
      Option.iter (collect_expr ~source ~type_map ~environment ~params ~tokens) spread
  | Ast.AST.EnumConstructor (enum_name, variant_name, args) ->
      (* Enum name as namespace *)
      let elen = String.length enum_name in
      (match find_name ~source ~start:expr.pos ~limit:(expr.end_pos + 1) enum_name with
      | Some (estart, _) ->
          tokens :=
            { pos = estart; end_pos = estart + elen - 1; token_type = namespace_type; modifiers = 0 } :: !tokens
      | None -> ());
      (* Variant name as enum member *)
      let vlen = String.length variant_name in
      (match find_name ~source ~start:(expr.pos + elen) ~limit:(expr.end_pos + 1) variant_name with
      | Some (vstart, _) ->
          tokens :=
            { pos = vstart; end_pos = vstart + vlen - 1; token_type = enum_member_type; modifiers = 0 } :: !tokens
      | None -> ());
      List.iter (collect_expr ~source ~type_map ~environment ~params ~tokens) args
  | Ast.AST.TypeCheck (e, _te) -> collect_expr ~source ~type_map ~environment ~params ~tokens e

(* Note: type annotations (type_expr) lack byte-position info in the AST,
   so we cannot emit semantic tokens for them. Tree-sitter handles type highlighting. *)

(* Collect tokens from match patterns *)
and collect_pattern ~source ~tokens (pat : Ast.AST.pattern) =
  match pat.pat with
  | Ast.AST.PWildcard -> ()
  | Ast.AST.PVariable name ->
      let nlen = String.length name in
      tokens :=
        { pos = pat.pos; end_pos = pat.pos + nlen - 1; token_type = variable_type; modifiers = declaration_mod }
        :: !tokens
  | Ast.AST.PLiteral _ -> ()
  | Ast.AST.PConstructor (enum_name, variant_name, sub_pats) ->
      (* Enum name as namespace *)
      let elen = String.length enum_name in
      let enum_end =
        match find_name ~source ~start:pat.pos ~limit:(pat.end_pos + 1) enum_name with
        | Some (estart, eend) ->
            tokens :=
              { pos = estart; end_pos = estart + elen - 1; token_type = namespace_type; modifiers = 0 } :: !tokens;
            eend
        | None -> pat.pos + elen
      in
      (* Variant name as enum member — search AFTER enum name *)
      let vlen = String.length variant_name in
      (match find_name ~source ~start:enum_end ~limit:(pat.end_pos + 1) variant_name with
      | Some (vstart, _) ->
          tokens :=
            { pos = vstart; end_pos = vstart + vlen - 1; token_type = enum_member_type; modifiers = 0 } :: !tokens
      | None -> ());
      List.iter (collect_pattern ~source ~tokens) sub_pats
  | Ast.AST.PRecord (fields, _rest) ->
      let search_from = ref pat.pos in
      List.iter
        (fun (f : Ast.AST.record_pattern_field) ->
          let fname = f.pat_field_name in
          let flen = String.length fname in
          (match find_name ~source ~start:!search_from ~limit:(pat.end_pos + 1) fname with
          | Some (fstart, fend) ->
              search_from := fend;
              tokens :=
                { pos = fstart; end_pos = fstart + flen - 1; token_type = property_type; modifiers = 0 }
                :: !tokens
          | None -> ());
          Option.iter (collect_pattern ~source ~tokens) f.pat_field_pattern)
        fields

(* Collect tokens from a statement *)
and collect_stmt ~source ~type_map ~environment ~params ~tokens (stmt : Ast.AST.statement) =
  match stmt.stmt with
  | Ast.AST.Let { name; value; _ } ->
      (* Determine if this is a function binding *)
      let is_fn =
        match value.expr with
        | Ast.AST.Function _ -> true
        | _ -> false
      in
      let token_type =
        if is_fn then
          function_type
        else
          variable_type
      in
      let nlen = String.length name in
      (match find_name ~source ~start:stmt.pos ~limit:value.pos name with
      | Some (nstart, _) ->
          tokens :=
            { pos = nstart; end_pos = nstart + nlen - 1; token_type; modifiers = declaration_mod } :: !tokens
      | None -> ());
      collect_expr ~source ~type_map ~environment ~params ~tokens value
  | Ast.AST.ExpressionStmt e -> collect_expr ~source ~type_map ~environment ~params ~tokens e
  | Ast.AST.Return e -> collect_expr ~source ~type_map ~environment ~params ~tokens e
  | Ast.AST.Block stmts -> List.iter (collect_stmt ~source ~type_map ~environment ~params ~tokens) stmts
  | Ast.AST.EnumDef { name; variants; _ } ->
      let nlen = String.length name in
      let search_from = ref stmt.pos in
      (match find_name ~source ~start:!search_from ~limit:(stmt.end_pos + 1) name with
      | Some (nstart, nend) ->
          search_from := nend;
          tokens :=
            { pos = nstart; end_pos = nstart + nlen - 1; token_type = enum_type; modifiers = declaration_mod }
            :: !tokens
      | None -> ());
      List.iter
        (fun (v : Ast.AST.variant_def) ->
          let vname = v.variant_name in
          let vlen = String.length vname in
          match find_name ~source ~start:!search_from ~limit:(stmt.end_pos + 1) vname with
          | Some (vstart, vend) ->
              search_from := vend;
              tokens :=
                {
                  pos = vstart;
                  end_pos = vstart + vlen - 1;
                  token_type = enum_member_type;
                  modifiers = declaration_mod;
                }
                :: !tokens
          | None -> ())
        variants
  | Ast.AST.TraitDef { name; methods; _ } ->
      let nlen = String.length name in
      let search_from = ref stmt.pos in
      (match find_name ~source ~start:!search_from ~limit:(stmt.end_pos + 1) name with
      | Some (nstart, nend) ->
          search_from := nend;
          tokens :=
            {
              pos = nstart;
              end_pos = nstart + nlen - 1;
              token_type = interface_type;
              modifiers = declaration_mod;
            }
            :: !tokens
      | None -> ());
      List.iter
        (fun (m : Ast.AST.method_sig) ->
          let mname = m.method_name in
          let mlen = String.length mname in
          match find_name ~source ~start:!search_from ~limit:(stmt.end_pos + 1) mname with
          | Some (mstart, mend) ->
              search_from := mend;
              tokens :=
                {
                  pos = mstart;
                  end_pos = mstart + mlen - 1;
                  token_type = method_type;
                  modifiers = declaration_mod;
                }
                :: !tokens
          | None -> ())
        methods
  | Ast.AST.ImplDef { impl_trait_name; impl_methods; _ } ->
      let tlen = String.length impl_trait_name in
      let search_from = ref stmt.pos in
      (match find_name ~source ~start:!search_from ~limit:(stmt.end_pos + 1) impl_trait_name with
      | Some (tstart, tend) ->
          search_from := tend;
          tokens :=
            { pos = tstart; end_pos = tstart + tlen - 1; token_type = interface_type; modifiers = 0 } :: !tokens
      | None -> ());
      List.iter
        (fun (m : Ast.AST.method_impl) ->
          let mname = m.impl_method_name in
          let mlen = String.length mname in
          (* Limit search to BEFORE the method body to avoid matching uses inside
             prior method bodies that share a name *)
          (match find_name ~source ~start:!search_from ~limit:m.impl_method_body.pos mname with
          | Some (mstart, mend) ->
              search_from := mend;
              tokens :=
                {
                  pos = mstart;
                  end_pos = mstart + mlen - 1;
                  token_type = method_type;
                  modifiers = definition_mod;
                }
                :: !tokens
          | None -> ());
          collect_expr ~source ~type_map ~environment ~params ~tokens m.impl_method_body;
          (* Advance search_from past the method body *)
          search_from := m.impl_method_body.end_pos + 1)
        impl_methods
  | Ast.AST.DeriveDef _ | Ast.AST.TypeAlias _ -> ()

(* Sort tokens by position, then delta-encode *)
let encode_tokens ~source (raw : raw_token list) : int array =
  let sorted = List.sort (fun a b -> compare a.pos b.pos) raw in
  let result = Buffer.create (List.length sorted * 5) in
  let prev_line = ref 0 in
  let prev_char = ref 0 in
  List.iter
    (fun tok ->
      let start_pos = Lsp_utils.offset_to_position ~source ~offset:tok.pos in
      let end_pos = Lsp_utils.offset_to_position ~source ~offset:tok.end_pos in
      let line = start_pos.line in
      let char = start_pos.character in
      let length =
        if start_pos.line = end_pos.line then
          end_pos.character - start_pos.character + 1
        else
          (* Multi-line token — use distance to end of first line as approximation *)
          end_pos.character + 1
      in
      if length > 0 then (
        let delta_line = line - !prev_line in
        let delta_char =
          if delta_line = 0 then
            char - !prev_char
          else
            char
        in
        Buffer.add_string result (string_of_int delta_line);
        Buffer.add_char result ',';
        Buffer.add_string result (string_of_int delta_char);
        Buffer.add_char result ',';
        Buffer.add_string result (string_of_int length);
        Buffer.add_char result ',';
        Buffer.add_string result (string_of_int tok.token_type);
        Buffer.add_char result ',';
        Buffer.add_string result (string_of_int tok.modifiers);
        Buffer.add_char result ';';
        prev_line := line;
        prev_char := char))
    sorted;
  (* Parse the buffer into an int array *)
  let s = Buffer.contents result in
  if s = "" then
    [||]
  else
    let entries = String.split_on_char ';' s in
    let entries = List.filter (fun s -> s <> "") entries in
    let ints = ref [] in
    List.iter
      (fun entry ->
        let parts = String.split_on_char ',' entry in
        List.iter (fun p -> ints := int_of_string p :: !ints) parts)
      entries;
    Array.of_list (List.rev !ints)

(* Public entry point *)
let compute
    ~(source : string) ~(program : Ast.AST.program) ~(type_map : Infer.type_map) ~(environment : Infer.type_env) :
    Lsp_t.SemanticTokens.t option =
  let tokens = ref [] in
  List.iter (collect_stmt ~source ~type_map ~environment ~params:[] ~tokens) program;
  let data = encode_tokens ~source !tokens in
  if Array.length data = 0 then
    None
  else
    Some (Lsp_t.SemanticTokens.create ~data ())

(* ============================================================
   Tests
   ============================================================ *)

(* Helper: parse, typecheck, compute semantic tokens *)
let get_tokens source =
  let result = Doc_state.analyze ~source in
  match (result.program, result.type_map, result.environment) with
  | Some prog, Some tm, Some env -> compute ~source ~program:prog ~type_map:tm ~environment:env
  | _ -> None

(* Decode flat int array back to (deltaLine, deltaChar, len, type, mods) tuples *)
let decode_tokens data =
  let len = Array.length data in
  let rec go i acc =
    if i + 4 >= len then
      List.rev acc
    else
      let tuple = (data.(i), data.(i + 1), data.(i + 2), data.(i + 3), data.(i + 4)) in
      go (i + 5) (tuple :: acc)
  in
  go 0 []

let has_token_type ty tokens =
  match tokens with
  | Some (st : Lsp_t.SemanticTokens.t) ->
      let decoded = decode_tokens st.data in
      List.exists (fun (_, _, _, t, _) -> t = ty) decoded
  | None -> false

let count_token_type ty tokens =
  match tokens with
  | Some (st : Lsp_t.SemanticTokens.t) ->
      let decoded = decode_tokens st.data in
      List.length (List.filter (fun (_, _, _, t, _) -> t = ty) decoded)
  | None -> 0

let%test "integer literal produces number token" =
  let tokens = get_tokens "42" in
  has_token_type number_type tokens

let%test "string literal produces string token" =
  let tokens = get_tokens "\"hello\"" in
  has_token_type string_type tokens

let%test "let binding produces variable token with declaration" =
  let tokens = get_tokens "let x = 42;" in
  match tokens with
  | Some st ->
      let decoded = decode_tokens st.data in
      List.exists (fun (_, _, _, t, m) -> t = variable_type && m land declaration_mod <> 0) decoded
  | None -> false

let%test "function binding produces function token" =
  let tokens = get_tokens "let f = fn(x) { x + 1 };" in
  match tokens with
  | Some st ->
      let decoded = decode_tokens st.data in
      List.exists (fun (_, _, _, t, m) -> t = function_type && m land declaration_mod <> 0) decoded
  | None -> false

let%test "function params produce parameter tokens" =
  let tokens = get_tokens "let f = fn(x) { x + 1 };" in
  has_token_type parameter_type tokens

let%test "identifier referring to function gets function type" =
  let tokens = get_tokens "let f = fn(x) { x + 1 }; f(5)" in
  (* f in f(5) should be function_type *)
  count_token_type function_type tokens >= 2

let%test "record field access produces property token" =
  let tokens = get_tokens "let r = { x: 1 }; r.x" in
  has_token_type property_type tokens

let%test "enum def produces enum and enumMember tokens" =
  let tokens = get_tokens "enum color { red green blue }" in
  has_token_type enum_type tokens && has_token_type enum_member_type tokens

let%test "enum def has 3 enum member tokens" =
  let tokens = get_tokens "enum color { red green blue }" in
  count_token_type enum_member_type tokens = 3

let%test "infix operator produces operator token" =
  let tokens = get_tokens "let x = 1 + 2;" in
  has_token_type operator_type tokens

let%test "no tokens for empty/failing source" =
  let tokens = get_tokens "" in
  tokens = None

let%test "method call produces method token" =
  (* Use a trait impl so method call typechecks *)
  let src =
    "trait greet[a] {\n  fn hello(self: a) -> string\n}\nimpl greet for int {\n  fn hello(self: int) -> string { \"hi\" }\n}\nlet x = 1; x.hello()"
  in
  let tokens = get_tokens src in
  has_token_type method_type tokens
