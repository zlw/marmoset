(* Hover: find expression at cursor and show its type *)

module Lsp_t = Linol_lsp.Types
module Ast = Marmoset.Lib.Ast
module Infer = Marmoset.Lib.Infer
module Types = Marmoset.Lib.Types

type type_var_user_name_map = Source_syntax.type_var_user_name_map

let first_some a b =
  match a with
  | Some _ -> a
  | None -> b

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '?' | '!' -> true
  | _ -> false

let identifier_range_at_offset ~(source : string) ~(offset : int) : (int * int) option =
  if offset < 0 || offset >= String.length source || not (is_ident_char source.[offset]) then
    None
  else
    let start = ref offset in
    let stop = ref offset in
    while !start > 0 && is_ident_char source.[!start - 1] do
      decr start
    done;
    while !stop + 1 < String.length source && is_ident_char source.[!stop + 1] do
      incr stop
    done;
    Some (!start, !stop)

let identifier_at_offset ~(source : string) ~(offset : int) : string option =
  match identifier_range_at_offset ~source ~offset with
  | Some (start, stop) -> Some (String.sub source start (stop - start + 1))
  | None -> None

(* The parser sets pos to the operator/paren position for Infix, Call,
   MethodCall, FieldAccess — not the leftmost token. This helper computes
   the true start by walking into left children recursively. *)
let rec effective_start_pos (expr : Ast.AST.expression) : int =
  match expr.expr with
  | Ast.AST.MethodCall { mc_receiver = recv; _ } | Ast.AST.FieldAccess (recv, _) ->
      min (effective_start_pos recv) expr.pos
  | Ast.AST.Infix (left, _, _) -> min (effective_start_pos left) expr.pos
  | Ast.AST.Call (fn_expr, _) -> min (effective_start_pos fn_expr) expr.pos
  | _ -> expr.pos

(* Find the deepest expression in the AST that contains the given byte offset. *)
let rec find_expr_at (offset : int) (expr : Ast.AST.expression) : Ast.AST.expression option =
  let start_pos = effective_start_pos expr in
  if offset < start_pos || offset > expr.end_pos then
    None
  else
    let child =
      match expr.expr with
      | Ast.AST.Infix (left, _, right) -> first_some (find_expr_at offset left) (find_expr_at offset right)
      | Ast.AST.Prefix (_, e) -> find_expr_at offset e
      | Ast.AST.Call (fn_expr, args) ->
          first_some (find_expr_at offset fn_expr) (List.find_map (find_expr_at offset) args)
      | Ast.AST.If (cond, then_, else_) ->
          first_some (find_expr_at offset cond)
            (first_some (find_expr_in_stmt offset then_) (Option.bind else_ (find_expr_in_stmt offset)))
      | Ast.AST.Function { body; _ } -> find_expr_in_stmt offset body
      | Ast.AST.Index (arr, idx) -> first_some (find_expr_at offset arr) (find_expr_at offset idx)
      | Ast.AST.Array elts -> List.find_map (find_expr_at offset) elts
      | Ast.AST.Hash pairs ->
          List.find_map (fun (k, v) -> first_some (find_expr_at offset k) (find_expr_at offset v)) pairs
      | Ast.AST.FieldAccess (e, _) -> find_expr_at offset e
      | Ast.AST.MethodCall { mc_receiver; mc_args; _ } ->
          first_some (find_expr_at offset mc_receiver) (List.find_map (find_expr_at offset) mc_args)
      | Ast.AST.Match (scrutinee, arms) ->
          first_some (find_expr_at offset scrutinee)
            (List.find_map (fun (arm : Ast.AST.match_arm) -> find_expr_at offset arm.body) arms)
      | Ast.AST.RecordLit (fields, spread) ->
          first_some
            (List.find_map
               (fun (f : Ast.AST.record_field) -> Option.bind f.field_value (find_expr_at offset))
               fields)
            (Option.bind spread (find_expr_at offset))
      | Ast.AST.EnumConstructor (_, _, args) -> List.find_map (find_expr_at offset) args
      | Ast.AST.TypeCheck (e, _) -> find_expr_at offset e
      | Ast.AST.BlockExpr stmts -> List.find_map (find_expr_in_stmt offset) stmts
      | Ast.AST.Identifier _ | Ast.AST.Integer _ | Ast.AST.Float _ | Ast.AST.Boolean _ | Ast.AST.String _ -> None
    in
    match child with
    | Some _ -> child
    | None -> Some expr

and find_pattern_at (offset : int) (pat : Ast.AST.pattern) : Ast.AST.pattern option =
  if offset < pat.pos || offset > pat.end_pos then
    None
  else
    let child =
      match pat.pat with
      | Ast.AST.PConstructor (_, _, fields) -> List.find_map (find_pattern_at offset) fields
      | Ast.AST.PRecord (fields, _) ->
          List.find_map
            (fun (field : Ast.AST.record_pattern_field) ->
              Option.bind field.pat_field_pattern (find_pattern_at offset))
            fields
      | Ast.AST.PWildcard | Ast.AST.PVariable _ | Ast.AST.PLiteral _ -> None
    in
    match child with
    | Some _ -> child
    | None -> Some pat

and find_expr_in_stmt (offset : int) (stmt : Ast.AST.statement) : Ast.AST.expression option =
  match stmt.stmt with
  | Ast.AST.ExpressionStmt e -> find_expr_at offset e
  | Ast.AST.Let { value; _ } -> (
      (* Search inside the value expression first; if not found and offset is
         within the statement range (e.g. on the binding name), return the value
         itself so the hover shows the binding's type. *)
      match find_expr_at offset value with
      | Some _ as found -> found
      | None ->
          if offset >= stmt.pos && offset <= value.pos then
            Some value
          else
            None)
  | Ast.AST.Return e -> (
      match find_expr_at offset e with
      | Some _ as found -> found
      | None ->
          if offset >= stmt.pos && offset <= e.pos then
            Some e
          else
            None)
  | Ast.AST.Block stmts -> List.find_map (find_expr_in_stmt offset) stmts
  | Ast.AST.ImplDef { impl_methods; _ } ->
      List.find_map (fun (m : Ast.AST.method_impl) -> find_expr_in_stmt offset m.impl_method_body) impl_methods
  | Ast.AST.InherentImplDef { inherent_methods; _ } ->
      List.find_map
        (fun (m : Ast.AST.method_impl) -> find_expr_in_stmt offset m.impl_method_body)
        inherent_methods
  | Ast.AST.TraitDef { methods; _ } ->
      List.find_map
        (fun (m : Ast.AST.method_sig) -> Option.bind m.method_default_impl (find_expr_at offset))
        methods
  | Ast.AST.EnumDef _ | Ast.AST.TypeDef _ | Ast.AST.ShapeDef _ | Ast.AST.DeriveDef _ | Ast.AST.TypeAlias _ -> None

(* Find an expression at a given offset across the entire program *)
let find_in_program (offset : int) (program : Ast.AST.program) : Ast.AST.expression option =
  List.find_map (find_expr_in_stmt offset) program

let rec find_pattern_in_expr ~(offset : int) ~(type_map : Infer.type_map) (expr : Ast.AST.expression) :
    (Ast.AST.pattern * Types.mono_type) option =
  match expr.expr with
  | Ast.AST.If (cond, then_, else_) ->
      first_some
        (find_pattern_in_expr ~offset ~type_map cond)
        (first_some
           (find_pattern_in_stmt ~offset ~type_map then_)
           (Option.bind else_ (find_pattern_in_stmt ~offset ~type_map)))
  | Ast.AST.Function { body; _ } -> find_pattern_in_stmt ~offset ~type_map body
  | Ast.AST.Infix (left, _, right) ->
      first_some (find_pattern_in_expr ~offset ~type_map left) (find_pattern_in_expr ~offset ~type_map right)
  | Ast.AST.Prefix (_, e) | Ast.AST.TypeCheck (e, _) | Ast.AST.FieldAccess (e, _) ->
      find_pattern_in_expr ~offset ~type_map e
  | Ast.AST.Call (fn_expr, args) ->
      first_some
        (find_pattern_in_expr ~offset ~type_map fn_expr)
        (List.find_map (find_pattern_in_expr ~offset ~type_map) args)
  | Ast.AST.Index (arr, idx) ->
      first_some (find_pattern_in_expr ~offset ~type_map arr) (find_pattern_in_expr ~offset ~type_map idx)
  | Ast.AST.Array elts -> List.find_map (find_pattern_in_expr ~offset ~type_map) elts
  | Ast.AST.Hash pairs ->
      List.find_map
        (fun (k, v) ->
          first_some (find_pattern_in_expr ~offset ~type_map k) (find_pattern_in_expr ~offset ~type_map v))
        pairs
  | Ast.AST.MethodCall { mc_receiver; mc_args; _ } ->
      first_some
        (find_pattern_in_expr ~offset ~type_map mc_receiver)
        (List.find_map (find_pattern_in_expr ~offset ~type_map) mc_args)
  | Ast.AST.Match (scrutinee, arms) ->
      first_some
        (find_pattern_in_expr ~offset ~type_map scrutinee)
        (List.find_map
           (fun (arm : Ast.AST.match_arm) ->
             let pattern_target =
               List.find_map
                 (fun (pat : Ast.AST.pattern) ->
                   if offset >= pat.pos && offset <= pat.end_pos then
                     match Hashtbl.find_opt type_map scrutinee.id with
                     | Some scrutinee_type -> Some (pat, Types.canonicalize_mono_type scrutinee_type)
                     | None -> None
                   else
                     None)
                 arm.patterns
             in
             first_some pattern_target (find_pattern_in_expr ~offset ~type_map arm.body))
           arms)
  | Ast.AST.RecordLit (fields, spread) ->
      first_some
        (List.find_map
           (fun (f : Ast.AST.record_field) -> Option.bind f.field_value (find_pattern_in_expr ~offset ~type_map))
           fields)
        (Option.bind spread (find_pattern_in_expr ~offset ~type_map))
  | Ast.AST.EnumConstructor (_, _, args) -> List.find_map (find_pattern_in_expr ~offset ~type_map) args
  | Ast.AST.BlockExpr stmts -> List.find_map (find_pattern_in_stmt ~offset ~type_map) stmts
  | Ast.AST.Identifier _ | Ast.AST.Integer _ | Ast.AST.Float _ | Ast.AST.Boolean _ | Ast.AST.String _ -> None

and find_pattern_in_stmt ~(offset : int) ~(type_map : Infer.type_map) (stmt : Ast.AST.statement) :
    (Ast.AST.pattern * Types.mono_type) option =
  match stmt.stmt with
  | Ast.AST.ExpressionStmt e -> find_pattern_in_expr ~offset ~type_map e
  | Ast.AST.Let { value; _ } -> find_pattern_in_expr ~offset ~type_map value
  | Ast.AST.Return e -> find_pattern_in_expr ~offset ~type_map e
  | Ast.AST.Block stmts -> List.find_map (find_pattern_in_stmt ~offset ~type_map) stmts
  | Ast.AST.ImplDef { impl_methods; _ } ->
      List.find_map
        (fun (m : Ast.AST.method_impl) -> find_pattern_in_stmt ~offset ~type_map m.impl_method_body)
        impl_methods
  | Ast.AST.InherentImplDef { inherent_methods; _ } ->
      List.find_map
        (fun (m : Ast.AST.method_impl) -> find_pattern_in_stmt ~offset ~type_map m.impl_method_body)
        inherent_methods
  | Ast.AST.TraitDef { methods; _ } ->
      List.find_map
        (fun (m : Ast.AST.method_sig) ->
          Option.bind m.method_default_impl (find_pattern_in_expr ~offset ~type_map))
        methods
  | Ast.AST.EnumDef _ | Ast.AST.TypeDef _ | Ast.AST.ShapeDef _ | Ast.AST.DeriveDef _ | Ast.AST.TypeAlias _ -> None

let find_pattern_in_program ~(offset : int) ~(type_map : Infer.type_map) (program : Ast.AST.program) :
    (Ast.AST.pattern * Types.mono_type) option =
  List.find_map (find_pattern_in_stmt ~offset ~type_map) program

let type_to_source = Source_syntax.mono_type_to_source
let normalize_with_user_names = Source_syntax.normalize_mono_type_with_user_names
let format_poly = Source_syntax.format_poly_binding

(* Format a type for hover display *)
let format_hover_type
    ~(type_var_user_names : type_var_user_name_map)
    ~(type_map : Infer.type_map)
    ~(environment : Infer.type_env)
    (expr : Ast.AST.expression) : string option =
  match expr.expr with
  | Ast.AST.Identifier name -> (
      (* For identifiers, prefer poly_type from env for better display *)
      match Infer.TypeEnv.find_opt name environment with
      | Some poly -> Some (format_poly ~type_var_user_names ~name poly)
      | None -> (
          match Hashtbl.find_opt type_map expr.id with
          | Some mono ->
              let norm = normalize_with_user_names ~type_var_user_names mono in
              Some (Printf.sprintf "%s: %s" name (type_to_source ~type_var_user_names norm))
          | None -> None))
  | _ -> (
      match Hashtbl.find_opt type_map expr.id with
      | Some mono ->
          let norm = normalize_with_user_names ~type_var_user_names mono in
          Some (type_to_source ~type_var_user_names norm)
      | None -> None)

let format_hover_pattern
    ~(source : string)
    ~(offset : int)
    ~(type_var_user_names : type_var_user_name_map)
    (pattern : Ast.AST.pattern)
    (scrutinee_type : Types.mono_type) : string option =
  match Infer.check_pattern pattern scrutinee_type with
  | Error _ -> None
  | Ok (bindings, pattern_type) -> (
      let format_type t =
        let norm = normalize_with_user_names ~type_var_user_names t in
        type_to_source ~type_var_user_names norm
      in
      let binding_hover =
        Option.bind (identifier_at_offset ~source ~offset) (fun hovered_ident ->
            Option.map
              (fun bound_type -> Printf.sprintf "%s: %s" hovered_ident (format_type bound_type))
              (List.assoc_opt hovered_ident bindings))
      in
      match binding_hover with
      | Some _ as info -> info
      | None -> (
          match pattern.pat with
          | Ast.AST.PVariable name ->
              Option.map
                (fun bound_type -> Printf.sprintf "%s: %s" name (format_type bound_type))
                (List.assoc_opt name bindings)
          | Ast.AST.PConstructor (enum_name, variant_name, _) ->
              Some (Printf.sprintf "%s.%s: %s" enum_name variant_name (format_type pattern_type))
          | Ast.AST.PRecord _ -> Some (format_type pattern_type)
          | Ast.AST.PWildcard -> Some (Printf.sprintf "_: %s" (format_type pattern_type))
          | Ast.AST.PLiteral lit ->
              let literal_text =
                match lit with
                | Ast.AST.LInt n -> Int64.to_string n
                | Ast.AST.LString s -> Printf.sprintf "%S" s
                | Ast.AST.LBool b ->
                    if b then
                      "true"
                    else
                      "false"
              in
              Some (Printf.sprintf "%s: %s" literal_text (format_type pattern_type))))

(* Provide hover info at a given cursor position *)
let hover_at
    ~(source : string)
    ~(program : Ast.AST.program)
    ~(type_map : Infer.type_map)
    ~(environment : Infer.type_env)
    ~(type_var_user_names : type_var_user_name_map)
    ~(line : int)
    ~(character : int) : Lsp_t.Hover.t option =
  let offset = Lsp_utils.position_to_offset ~source ~line ~character in
  match find_pattern_in_program ~offset ~type_map program with
  | Some (pattern, scrutinee_type) -> (
      match format_hover_pattern ~source ~offset ~type_var_user_names pattern scrutinee_type with
      | None -> None
      | Some type_str ->
          let contents =
            Lsp_t.MarkupContent.create ~kind:Lsp_t.MarkupKind.Markdown
              ~value:(Printf.sprintf "```marmoset\n%s\n```" type_str)
          in
          let pos, end_pos =
            match identifier_range_at_offset ~source ~offset with
            | Some (start, stop) -> (start, stop)
            | None -> (pattern.pos, pattern.end_pos)
          in
          let range = Lsp_utils.offset_range_to_lsp ~source ~pos ~end_pos in
          Some (Lsp_t.Hover.create ~contents:(`MarkupContent contents) ~range ()))
  | None -> (
      match find_in_program offset program with
      | None -> None
      | Some expr -> (
          match format_hover_type ~type_var_user_names ~type_map ~environment expr with
          | None -> None
          | Some type_str ->
              let contents =
                Lsp_t.MarkupContent.create ~kind:Lsp_t.MarkupKind.Markdown
                  ~value:(Printf.sprintf "```marmoset\n%s\n```" type_str)
              in
              let effective_pos = effective_start_pos expr in
              let range = Lsp_utils.offset_range_to_lsp ~source ~pos:effective_pos ~end_pos:expr.end_pos in
              Some (Lsp_t.Hover.create ~contents:(`MarkupContent contents) ~range ())))

(* ============================================================
   Tests
   ============================================================ *)

(* ============================================================
   Test helpers — check type text AND highlighted range
   ============================================================ *)

let string_contains haystack needle = Diagnostics.String_utils.contains_substring ~needle haystack

(* Full hover result: type text, highlighted source range *)
type hover_result = {
  type_text : string; (* content of the tooltip *)
  highlighted : string; (* substring of source that the range covers *)
  start_line : int;
  start_col : int;
  end_line : int;
  end_col : int;
}

let check_hover source line character =
  let result = Doc_state.analyze ~source in
  match (result.program, result.type_map, result.environment) with
  | Some prog, Some tm, Some env ->
      hover_at ~source ~program:prog ~type_map:tm ~environment:env ~type_var_user_names:result.type_var_user_names
        ~line ~character
  | _ -> None

let hover_info source line character : hover_result option =
  match check_hover source line character with
  | None -> None
  | Some hover ->
      let type_text =
        match hover.contents with
        | `MarkupContent mc -> mc.value
        | _ -> ""
      in
      let r =
        match hover.range with
        | Some r -> r
        | None -> failwith "hover missing range"
      in
      (* Extract the highlighted substring from source using line/col offsets *)
      let start_off = Lsp_utils.position_to_offset ~source ~line:r.start.line ~character:r.start.character in
      let end_off = Lsp_utils.position_to_offset ~source ~line:r.end_.line ~character:r.end_.character in
      let highlighted = String.sub source start_off (end_off - start_off) in
      Some
        {
          type_text;
          highlighted;
          start_line = r.start.line;
          start_col = r.start.character;
          end_line = r.end_.line;
          end_col = r.end_.character;
        }

let source_with_cursor annotated =
  let cursor = String.index annotated '|' in
  let source =
    String.sub annotated 0 cursor ^ String.sub annotated (cursor + 1) (String.length annotated - cursor - 1)
  in
  let pos = Lsp_utils.offset_to_position ~source ~offset:cursor in
  (source, pos.line, pos.character)

let hover_marked annotated =
  let source, line, character = source_with_cursor annotated in
  (source, hover_info source line character)

(* ============================================================
   Tests: literals
   ============================================================ *)

let%test "hover on integer literal: type=Int, highlights '42'" =
  match hover_info "42" 0 0 with
  | Some h -> string_contains h.type_text "Int" && h.highlighted = "42"
  | None -> false

let%test "hover on string literal: type=Str, highlights the string" =
  match hover_info {|"hello"|} 0 0 with
  | Some h -> string_contains h.type_text "Str" && h.highlighted = {|"hello"|}
  | None -> false

let%test "hover on boolean literal: type=Bool, highlights 'true'" =
  match hover_info "true" 0 0 with
  | Some h -> string_contains h.type_text "Bool" && h.highlighted = "true"
  | None -> false

(* ============================================================
   Tests: identifiers and let bindings
   ============================================================ *)

(*  let x = 42; x
    0         1
    0123456789012345
    x at col 12 *)
let%test "hover on identifier: type=Int, highlights 'x'" =
  match hover_info "let x = 42; x" 0 12 with
  | Some h -> string_contains h.type_text "x" && string_contains h.type_text "Int" && h.highlighted = "x"
  | None -> false

let%test "hover on function identifier: shows arrow type, highlights 'f'" =
  match hover_marked "let f = (x) -> x + 1; |f" with
  | _, Some h -> string_contains h.type_text "->" && h.highlighted = "f"
  | _, None -> false

let%test "hover on polymorphic function uses bracket syntax" =
  match hover_marked "fn id[a](x: a) -> a = x\n|id" with
  | _, Some h -> string_contains h.type_text "->" && h.highlighted = "id"
  | _, None -> false

let%test "hover on let binding name: shows value type" =
  match hover_info "let x = 42;" 0 4 with
  | Some h -> string_contains h.type_text "Int"
  | None -> false

(* ============================================================
   Tests: function calls — identifier vs call result
   ============================================================ *)

let%test "hover on call-site function name: arrow type, highlights 'f'" =
  match hover_marked "let f = (x) -> x + 1; |f(1)" with
  | _, Some h -> string_contains h.type_text "->" && h.highlighted = "f"
  | _, None -> false

let%test "hover on call argument: type=Int, highlights '1'" =
  match hover_marked "let f = (x) -> x + 1; f(|1)" with
  | _, Some h -> string_contains h.type_text "Int" && h.highlighted = "1"
  | _, None -> false

(* ============================================================
   Tests: infix — left operand must be reachable
   ============================================================ *)

(*  1 + 2
    01234
    1 at col 0, + at col 2, 2 at col 4 *)
let%test "hover on left of infix: type=Int, highlights '1'" =
  match hover_info "1 + 2" 0 0 with
  | Some h -> string_contains h.type_text "Int" && h.highlighted = "1"
  | None -> false

let%test "hover on right of infix: type=Int, highlights '2'" =
  match hover_info "1 + 2" 0 4 with
  | Some h -> string_contains h.type_text "Int" && h.highlighted = "2"
  | None -> false

let%test "hover on infix operator: type=Int, highlights whole expression" =
  match hover_info "1 + 2" 0 2 with
  | Some h -> string_contains h.type_text "Int" && h.highlighted = "1 + 2"
  | None -> false

(* ============================================================
   Tests: nested calls inside infix (the fib pattern)
   ============================================================ *)

let fib_source = "fn fib(n) = {\n  if (n < 2) { return n }\n  return fib(n - 2) + fib(n - 1)\n}"

(*  line 2: "  return fib(n - 2) + fib(n - 1);"
             0123456789012345678901234567890123
             fib at col 9, + at col 20, second fib at col 22 *)
(* Non-recursive call inside infix *)
let%test "hover on fn name in call inside infix: highlights 'f'" =
  match hover_marked "let f = (x) -> x + 1; |f(1) + 2" with
  | _, Some h -> h.highlighted = "f"
  | _, None -> false

(* Recursive fib — the analysis must succeed and hover must find fib inside body *)
let%test "fib source analyzes without errors" =
  let result = Doc_state.analyze ~source:fib_source in
  result.diagnostics = [] && result.type_map <> None

let%test "hover on fib inside recursive body finds something" =
  match hover_marked "fn fib(n) = {\n  if (n < 2) { return n }\n  return |fib(n - 2) + fib(n - 1)\n}" with
  | _, Some _ -> true
  | _ -> false

let%test "hover on n inside fib body" =
  match hover_marked "fn fib(n) = {\n  if (n < 2) { return n }\n  return fib(|n - 2) + fib(n - 1)\n}" with
  | _, Some h -> string_contains h.type_text "Int"
  | _ -> false

let%test "hover on literal 2 inside fib body" =
  match hover_marked "fn fib(n) = {\n  if (n < 2) { return n }\n  return fib(n - |2) + fib(n - 1)\n}" with
  | _, Some h -> string_contains h.type_text "Int"
  | _ -> false

let%test "hover on + operator inside fib body" =
  match hover_marked "fn fib(n) = {\n  if (n < 2) { return n }\n  return fib(n - 2) |+ fib(n - 1)\n}" with
  | _, Some _ -> true
  | _ -> false

let%test "hover on return keyword: shows return expr type" =
  match hover_marked "fn fib(n) = {\n  if (n < 2) { return n }\n  |return fib(n - 2) + fib(n - 1)\n}" with
  | _, Some h -> string_contains h.type_text "Int"
  | _ -> false

let%test "hover on return keyword: highlights full return expression" =
  match hover_marked "fn fib(n) = {\n  if (n < 2) { return n }\n  |return fib(n - 2) + fib(n - 1)\n}" with
  | _, Some h -> string_contains h.highlighted "fib(n - 2) + fib(n - 1)"
  | _ -> false

(* ============================================================
   Tests: edge cases
   ============================================================ *)

let full_fib_source =
  "fn fib(n) = {\n  if (n < 2) { return n }\n  return fib(n - 2) + fib(n - 1)\n}\n\nputs(fib(35) == 9227465)\n"

let%test "hover on first return keyword shows int, not unit" =
  match
    hover_marked
      "fn fib(n) = {\n  if (n < 2) { |return n }\n  return fib(n - 2) + fib(n - 1)\n}\n\nputs(fib(35) == 9227465)\n"
  with
  | _, Some h -> string_contains h.type_text "Int"
  | _ -> false

let%test "hover on second return keyword shows int" =
  match
    hover_marked
      "fn fib(n) = {\n  if (n < 2) { return n }\n  |return fib(n - 2) + fib(n - 1)\n}\n\nputs(fib(35) == 9227465)\n"
  with
  | _, Some h -> string_contains h.type_text "Int"
  | _ -> false

let%test "hover on n param inside first return" =
  match
    hover_marked
      "fn fib(n) = {\n  if (n < 2) { return |n }\n  return fib(n - 2) + fib(n - 1)\n}\n\nputs(fib(35) == 9227465)\n"
  with
  | _, Some h -> string_contains h.type_text "Int" && h.highlighted = "n"
  | _ -> false

let%test "hover on whitespace/out of range returns None" = check_hover "42" 5 0 = None

let%test "find_expr_at returns None for offset outside expression" =
  let expr = Ast.AST.mk_expr ~id:1 ~pos:5 ~end_pos:10 (Ast.AST.Integer 42L) in
  find_expr_at 0 expr = None && find_expr_at 15 expr = None

let%test "find_expr_at returns expression when offset is inside" =
  let expr = Ast.AST.mk_expr ~id:1 ~pos:0 ~end_pos:5 (Ast.AST.Identifier "hello") in
  match find_expr_at 3 expr with
  | Some e -> e.id = 1
  | None -> false

(* ============================================================
   Phase 8 regression: hover inside trait impl method body
   ============================================================ *)

let trait_impl_source =
  "trait Greet[a] = {\n  fn hello(x: a) -> Str\n}\nimpl Greet[Int] = {\n  fn hello(x: Int) -> Str = \"hi\"\n}\nputs(Greet.hello(42))"

let%test "hover on expression inside trait impl method body" =
  match
    hover_marked
      "trait Greet[a] = {\n  fn hello(x: a) -> Str\n}\nimpl Greet[Int] = {\n  fn hello(x: Int) -> Str = |\"hi\"\n}\nputs(Greet.hello(42))"
  with
  | _, Some h -> string_contains h.type_text "Str"
  | _ -> false

let inherent_impl_source = "impl Int = {\n  fn double(x: Int) -> Int = x * 2\n}\nputs(42.double())"

let%test "hover on expression inside inherent impl method body" =
  match hover_marked "impl Int = {\n  fn double(x: Int) -> Int = x |* 2\n}\nputs(42.double())" with
  | _, Some h -> string_contains h.type_text "Int"
  | _ -> false

let%test "hover on constructor pattern binding shows bound type" =
  match
    hover_marked
      "type Option[a] = {\n  Some(a),\n  None,\n}\nfn unwrap(v: Option[Int]) -> Int = match v {\n  case Option.Some(|x): x\n  case Option.None: 0\n}"
  with
  | _, Some h -> string_contains h.type_text "x: Int" && h.highlighted = "x"
  | _ -> false

let%test "hover on record pattern punning shows bound field type" =
  match
    hover_marked
      "fn greet(m: { name: Str, bananas_eaten: Int }) -> Str = match m {\n  case { |name:, ...rest }: name\n}"
  with
  | _, Some h -> string_contains h.type_text "name: Str" && h.highlighted = "name"
  | _ -> false

let%test "hover formats list types with vnext casing" =
  match hover_info "let xs = [1, 2, 3]; xs" 0 20 with
  | Some h -> string_contains h.type_text "List[Int]"
  | None -> false
