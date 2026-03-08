(* Inlay hints: show inferred types for unannotated bindings and parameters *)

module Lsp_t = Linol_lsp.Types
module Ast = Marmoset.Lib.Ast
module Infer = Marmoset.Lib.Infer
module Types = Marmoset.Lib.Types

(* Find the byte offset just past the end of `name` in source[start..limit),
   checking word boundaries to avoid matching substrings. *)
let find_name_end ~source ~start ~limit name =
  let name_len = String.length name in
  let src_len = String.length source in
  let limit = min limit src_len in
  let is_ident_char c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_' in
  let rec scan i =
    if i + name_len > limit then
      None
    else if String.sub source i name_len = name then
      let after = i + name_len in
      (* Check word boundary: char before should not be ident, char after should not be ident *)
      let before_ok = i = 0 || not (is_ident_char source.[i - 1]) in
      let after_ok = after >= src_len || not (is_ident_char source.[after]) in
      if before_ok && after_ok then
        Some after
      else
        scan (i + 1)
    else
      scan (i + 1)
  in
  scan start

(* Extract n parameter types from a curried function type *)
let rec take_param_types n mono =
  if n <= 0 then
    []
  else
    match mono with
    | Types.TFun (param, rest, _) -> param :: take_param_types (n - 1) rest
    | _ -> []

(* Extract the return type of a function with n parameters *)
let rec get_return_type n mono =
  if n <= 0 then
    Some mono
  else
    match mono with
    | Types.TFun (_, rest, _) -> get_return_type (n - 1) rest
    | _ -> None

(* Find the byte offset of the closing ) for function parameters.
   Search backward from `limit` for ')'. *)
let find_close_paren ~source ~start ~limit =
  let rec scan i =
    if i < start then
      None
    else if source.[i] = ')' then
      Some (i + 1)
    else
      scan (i - 1)
  in
  scan (limit - 1)

(* Check if an expression is a chain-able receiver (MethodCall or FieldAccess) *)
let is_chain_receiver (expr : Ast.AST.expression) =
  match expr.expr with
  | Ast.AST.MethodCall _ | Ast.AST.FieldAccess _ -> true
  | _ -> false

(* Find the byte offset right after the receiver's actual last character,
   by scanning backward from the outer expression's dot position past whitespace
   and # line comments.
   NOTE: inner MethodCalls in a chain have incorrect end_pos (set to the dot
   position instead of ')'), so we cannot rely on recv.end_pos. Instead we
   derive the receiver's visual end from the known dot position of the outer
   expression.
   KNOWN LIMITATION: a '#' inside a string literal in method arguments on the
   same line as the closing paren can fool the comment detection. This is rare
   in practice. *)
let find_recv_actual_end ~source ~dot_pos =
  let rec scan i =
    if i < 0 then
      0
    else
      match source.[i] with
      | ' ' | '\t' | '\r' | '\n' -> scan (i - 1)
      | _ -> (
          (* Check if this character is inside a # line comment.
             Scan backward on the same line to find a '#'. *)
          let rec find_hash j =
            if j < 0 || source.[j] = '\n' then
              None
            else if source.[j] = '#' then
              Some j
            else
              find_hash (j - 1)
          in
          match find_hash i with
          | Some hash_pos -> scan (hash_pos - 1)
          | None -> i + 1)
  in
  scan (dot_pos - 1)

(* Check if there's a newline between two byte offsets *)
let has_newline_between ~source ~from_offset ~to_offset =
  let hi = min to_offset (String.length source) in
  let rec scan i =
    if i >= hi then
      false
    else if source.[i] = '\n' then
      true
    else
      scan (i + 1)
  in
  scan from_offset

(* Generate inlay hints for a single function expression *)
let hints_for_function ~source ~type_map ~hints (fn_expr : Ast.AST.expression) =
  match fn_expr.expr with
  | Ast.AST.Function { params; return_type; body; _ } -> (
      match Hashtbl.find_opt type_map fn_expr.id with
      | Some fn_type -> (
          let n_params = List.length params in
          let param_types = take_param_types n_params fn_type in
          (* Parameter type hints *)
          let search_from = ref fn_expr.pos in
          if List.length param_types = n_params then
            List.iter2
              (fun (pname, annotation) ptype ->
                if annotation = None then
                  match find_name_end ~source ~start:!search_from ~limit:fn_expr.end_pos pname with
                  | Some pend ->
                      search_from := pend;
                      let position = Lsp_utils.offset_to_position ~source ~offset:pend in
                      let type_str = Types.to_string_pretty ptype in
                      hints :=
                        Lsp_t.InlayHint.create ~position
                          ~label:(`String (": " ^ type_str))
                          ~kind:Lsp_t.InlayHintKind.Type ()
                        :: !hints
                  | None -> ()
                else
                  (* Still advance search_from past annotated params *)
                  match find_name_end ~source ~start:!search_from ~limit:fn_expr.end_pos pname with
                  | Some pend -> search_from := pend
                  | None -> ())
              params param_types;
          (* Return type hint *)
          if return_type = None then
            match get_return_type n_params fn_type with
            | Some ret_type -> (
                let body_start = body.pos in
                match find_close_paren ~source ~start:fn_expr.pos ~limit:body_start with
                | Some paren_end ->
                    let position = Lsp_utils.offset_to_position ~source ~offset:paren_end in
                    let type_str = Types.to_string_pretty ret_type in
                    hints :=
                      Lsp_t.InlayHint.create ~position
                        ~label:(`String (" -> " ^ type_str))
                        ~kind:Lsp_t.InlayHintKind.Type ~paddingRight:true ()
                      :: !hints
                | None -> ())
            | None -> ())
      | None -> ())
  | _ -> ()

(* Walk statements recursively, collecting inlay hints *)
let rec walk_stmt ~source ~type_map ~range_start ~range_end ~hints (stmt : Ast.AST.statement) =
  if stmt.end_pos < range_start || stmt.pos > range_end then
    ()
  else
    match stmt.stmt with
    | Ast.AST.Let { name; value; type_annotation = None; _ } ->
        (* Type hint for the let binding name *)
        (match Hashtbl.find_opt type_map value.id with
        | Some mono -> (
            match find_name_end ~source ~start:stmt.pos ~limit:value.pos name with
            | Some name_end ->
                let position = Lsp_utils.offset_to_position ~source ~offset:name_end in
                let type_str = Types.to_string_pretty mono in
                hints :=
                  Lsp_t.InlayHint.create ~position
                    ~label:(`String (": " ^ type_str))
                    ~kind:Lsp_t.InlayHintKind.Type ()
                  :: !hints
            | None -> ())
        | None -> ());
        (* If the value is a function, also add param/return hints *)
        hints_for_function ~source ~type_map ~hints value;
        (* Recurse into the function body if applicable *)
        walk_expr ~source ~type_map ~range_start ~range_end ~hints value
    | Ast.AST.Let { value; _ } ->
        (* Annotated let — still check for function param hints inside *)
        hints_for_function ~source ~type_map ~hints value;
        walk_expr ~source ~type_map ~range_start ~range_end ~hints value
    | Ast.AST.Block stmts -> List.iter (walk_stmt ~source ~type_map ~range_start ~range_end ~hints) stmts
    | Ast.AST.ExpressionStmt e -> walk_expr ~source ~type_map ~range_start ~range_end ~hints e
    | Ast.AST.Return e -> walk_expr ~source ~type_map ~range_start ~range_end ~hints e
    | Ast.AST.ImplDef { impl_methods; _ } ->
        List.iter
          (fun (m : Ast.AST.method_impl) ->
            walk_stmt ~source ~type_map ~range_start ~range_end ~hints m.impl_method_body)
          impl_methods
    | Ast.AST.InherentImplDef { inherent_methods; _ } ->
        List.iter
          (fun (m : Ast.AST.method_impl) ->
            walk_stmt ~source ~type_map ~range_start ~range_end ~hints m.impl_method_body)
          inherent_methods
    | Ast.AST.TraitDef { methods; _ } ->
        List.iter
          (fun (m : Ast.AST.method_sig) ->
            Option.iter (walk_expr ~source ~type_map ~range_start ~range_end ~hints) m.method_default_impl)
          methods
    | Ast.AST.EnumDef _ | Ast.AST.DeriveDef _ | Ast.AST.TypeAlias _ -> ()

(* Walk expressions to find nested functions *)
and walk_expr ~source ~type_map ~range_start ~range_end ~hints (expr : Ast.AST.expression) =
  if expr.end_pos < range_start || expr.pos > range_end then
    ()
  else
    match expr.expr with
    | Ast.AST.Function { body; _ } ->
        hints_for_function ~source ~type_map ~hints expr;
        walk_stmt ~source ~type_map ~range_start ~range_end ~hints body
    | Ast.AST.If (cond, then_, else_) ->
        walk_expr ~source ~type_map ~range_start ~range_end ~hints cond;
        walk_stmt ~source ~type_map ~range_start ~range_end ~hints then_;
        Option.iter (walk_stmt ~source ~type_map ~range_start ~range_end ~hints) else_
    | Ast.AST.Call (fn_expr, args) ->
        walk_expr ~source ~type_map ~range_start ~range_end ~hints fn_expr;
        List.iter (walk_expr ~source ~type_map ~range_start ~range_end ~hints) args
    | Ast.AST.Infix (l, _, r) ->
        walk_expr ~source ~type_map ~range_start ~range_end ~hints l;
        walk_expr ~source ~type_map ~range_start ~range_end ~hints r
    | Ast.AST.Prefix (_, e) -> walk_expr ~source ~type_map ~range_start ~range_end ~hints e
    | Ast.AST.Index (arr, idx) ->
        walk_expr ~source ~type_map ~range_start ~range_end ~hints arr;
        walk_expr ~source ~type_map ~range_start ~range_end ~hints idx
    | Ast.AST.Array elts -> List.iter (walk_expr ~source ~type_map ~range_start ~range_end ~hints) elts
    | Ast.AST.Hash pairs ->
        List.iter
          (fun (k, v) ->
            walk_expr ~source ~type_map ~range_start ~range_end ~hints k;
            walk_expr ~source ~type_map ~range_start ~range_end ~hints v)
          pairs
    | Ast.AST.FieldAccess (recv, _) ->
        (if is_chain_receiver recv then
           let recv_end = find_recv_actual_end ~source ~dot_pos:expr.pos in
           if has_newline_between ~source ~from_offset:recv_end ~to_offset:expr.pos then
             match Hashtbl.find_opt type_map recv.id with
             | Some mono ->
                 let position = Lsp_utils.offset_to_position ~source ~offset:recv_end in
                 let type_str = Types.to_string_pretty mono in
                 hints :=
                   Lsp_t.InlayHint.create ~position
                     ~label:(`String (": " ^ type_str))
                     ~kind:Lsp_t.InlayHintKind.Type ~paddingLeft:true ()
                   :: !hints
             | None -> ());
        walk_expr ~source ~type_map ~range_start ~range_end ~hints recv
    | Ast.AST.MethodCall { mc_receiver = recv; mc_args = args; _ } ->
        (if is_chain_receiver recv then
           let recv_end = find_recv_actual_end ~source ~dot_pos:expr.pos in
           if has_newline_between ~source ~from_offset:recv_end ~to_offset:expr.pos then
             match Hashtbl.find_opt type_map recv.id with
             | Some mono ->
                 let position = Lsp_utils.offset_to_position ~source ~offset:recv_end in
                 let type_str = Types.to_string_pretty mono in
                 hints :=
                   Lsp_t.InlayHint.create ~position
                     ~label:(`String (": " ^ type_str))
                     ~kind:Lsp_t.InlayHintKind.Type ~paddingLeft:true ()
                   :: !hints
             | None -> ());
        walk_expr ~source ~type_map ~range_start ~range_end ~hints recv;
        List.iter (walk_expr ~source ~type_map ~range_start ~range_end ~hints) args
    | Ast.AST.Match (scrutinee, arms) ->
        walk_expr ~source ~type_map ~range_start ~range_end ~hints scrutinee;
        List.iter
          (fun (arm : Ast.AST.match_arm) -> walk_expr ~source ~type_map ~range_start ~range_end ~hints arm.body)
          arms
    | Ast.AST.RecordLit (fields, spread) ->
        List.iter
          (fun (f : Ast.AST.record_field) ->
            Option.iter (walk_expr ~source ~type_map ~range_start ~range_end ~hints) f.field_value)
          fields;
        Option.iter (walk_expr ~source ~type_map ~range_start ~range_end ~hints) spread
    | Ast.AST.EnumConstructor (_, _, args) ->
        List.iter (walk_expr ~source ~type_map ~range_start ~range_end ~hints) args
    | Ast.AST.TypeCheck (e, _) -> walk_expr ~source ~type_map ~range_start ~range_end ~hints e
    | Ast.AST.Identifier _ | Ast.AST.Integer _ | Ast.AST.Float _ | Ast.AST.Boolean _ | Ast.AST.String _ -> ()

(* Public entry point: generate inlay hints for a document *)
let inlay_hints
    ~(source : string) ~(program : Ast.AST.program) ~(type_map : Infer.type_map) ~(range : Lsp_t.Range.t) :
    Lsp_t.InlayHint.t list =
  let range_start =
    Lsp_utils.position_to_offset ~source ~line:range.start.line ~character:range.start.character
  in
  let range_end = Lsp_utils.position_to_offset ~source ~line:range.end_.line ~character:range.end_.character in
  let hints = ref [] in
  List.iter (walk_stmt ~source ~type_map ~range_start ~range_end ~hints) program;
  List.rev !hints

(* ============================================================
   Tests
   ============================================================ *)

(* Helper: parse, typecheck, and get inlay hints for full document *)
let get_hints source =
  let result = Doc_state.analyze ~source in
  match (result.program, result.type_map) with
  | Some prog, Some tm ->
      let range =
        Lsp_t.Range.create
          ~start:(Lsp_t.Position.create ~line:0 ~character:0)
          ~end_:(Lsp_t.Position.create ~line:999 ~character:0)
      in
      inlay_hints ~source ~program:prog ~type_map:tm ~range
  | _ -> []

let hint_labels hints =
  List.map
    (fun (h : Lsp_t.InlayHint.t) ->
      match h.label with
      | `String s -> s
      | _ -> "")
    hints

let%test "let binding without annotation gets type hint" =
  let hints = get_hints "let x = 42;" in
  let labels = hint_labels hints in
  List.exists (fun l -> l = ": Int") labels

let%test "let binding with annotation gets no extra type hint" =
  let hints = get_hints "let x: int = 42;" in
  (* Should have no ": Int" hint for the let name since it's annotated *)
  let labels = hint_labels hints in
  not (List.exists (fun l -> l = ": Int") labels)

let%test "function params without annotations get type hints" =
  let hints = get_hints "let f = fn(x) { x + 1 };" in
  let labels = hint_labels hints in
  List.exists (fun l -> l = ": Int") labels

let%test "function return type hint shown" =
  let hints = get_hints "let f = fn(x) { x + 1 };" in
  let labels = hint_labels hints in
  List.exists (fun l -> l = " -> Int") labels

let%test "function with annotated params gets no param hints" =
  let hints = get_hints "let f = fn(x: int) { x + 1 };" in
  (* Should have let binding hint and return type hint, but NOT a ": Int" param hint *)
  let param_hints =
    List.filter
      (fun (h : Lsp_t.InlayHint.t) ->
        match h.label with
        | `String s -> s = ": Int" && h.kind = Some Lsp_t.InlayHintKind.Type
        | _ -> false)
      hints
  in
  (* One for the let binding ": Int -> Int", but not one for param x *)
  List.length param_hints <= 1

let%test "nested let gets type hint" =
  let hints = get_hints "let f = fn(x) { let y = x + 1; y };" in
  let labels = hint_labels hints in
  (* Should have hints for f, x, y, and return type *)
  List.length labels >= 3

let%test "no hints when parse fails" =
  let hints = get_hints "let = ;" in
  hints = []

let%test "multiple let bindings each get hints" =
  let hints = get_hints "let x = 1; let y = true; let z = \"hi\";" in
  let labels = hint_labels hints in
  List.exists (fun l -> l = ": Int") labels
  && List.exists (fun l -> l = ": Bool") labels
  && List.exists (fun l -> l = ": String") labels

let%test "multiline method chain shows intermediate type hint" =
  let source = "let result = 42.show()\n  .show();" in
  let hints = get_hints source in
  let labels = hint_labels hints in
  (* Let binding hint ": String" + chain hint ": String" for 42.show() receiver *)
  let string_hints = List.filter (fun l -> l = ": String") labels in
  List.length string_hints >= 2

let%test "single-line method chain gets no chain hint" =
  let source = "let result = 42.show().show();" in
  let hints = get_hints source in
  let labels = hint_labels hints in
  (* Only the let binding hint ": String", no intermediate chain hint *)
  let string_hints = List.filter (fun l -> l = ": String") labels in
  List.length string_hints = 1

let%test "multiline chain with 3 steps shows intermediate type hints" =
  let source = "let result = 42.add(1)\n  .add(2)\n  .show();" in
  let hints = get_hints source in
  let labels = hint_labels hints in
  (* Two chain hints ": Int" for .add(1) and .add(2) intermediate results *)
  let int_hints = List.filter (fun l -> l = ": Int") labels in
  List.length int_hints >= 2

let%test "non-chain multiline expression gets no chain hint" =
  let source = "let x = len(\n  [1, 2, 3]\n);" in
  let hints = get_hints source in
  let labels = hint_labels hints in
  (* Only let binding hint, no chain hints *)
  List.length labels = 1 && List.hd labels = ": Int"

let%test "chain hint position is after receiver closing paren, not at dot" =
  (* "let result = 42.show()\n  .show();"
     offset: l=0..t=2, ' '=3, r=4..t=9, ' '=10, '='=11, ' '=12,
             '4'=13, '2'=14, '.'=15, s=16..w=19, '('=20, ')'=21,
             '\n'=22, ' '=23, ' '=24, '.'=25, s=26..w=29, '('=30, ')'=31, ';'=32
     The chain hint should be at offset 22 (right after ')' at 21), which is
     line 0, character 22 in LSP terms (end of first line). *)
  let source = "let result = 42.show()\n  .show();" in
  let hints = get_hints source in
  let chain_hints =
    List.filter
      (fun (h : Lsp_t.InlayHint.t) ->
        h.paddingLeft = Some true
        &&
        match h.label with
        | `String s -> s = ": String"
        | _ -> false)
      hints
  in
  match chain_hints with
  | [ h ] -> h.position.line = 0 && h.position.character = 22
  | _ -> false

let%test "multiline chain with tab indentation" =
  let source = "let result = 42.show()\n\t.show();" in
  let hints = get_hints source in
  let labels = hint_labels hints in
  let string_hints = List.filter (fun l -> l = ": String") labels in
  (* Let binding hint + chain hint *)
  List.length string_hints >= 2

let%test "expression-statement chain (no let binding) gets chain hint" =
  let source = "42.show()\n  .show();" in
  let hints = get_hints source in
  let chain_hints =
    List.filter
      (fun (h : Lsp_t.InlayHint.t) ->
        h.paddingLeft = Some true
        &&
        match h.label with
        | `String s -> s = ": String"
        | _ -> false)
      hints
  in
  List.length chain_hints = 1

let%test "find_recv_actual_end skips # line comments" =
  (* Test the helper directly: "42.show()  # comment\n  ."
     dot_pos=33 (the second dot). Backward from 32: '\n' skip, then 'tnemm' etc.
     Without comment skipping, we'd land inside the comment text.
     With comment skipping, we should land right after ')'. *)
  let source = "42.show()  # comment\n  .show();" in
  (* The second '.' is at position 23 (after \n and two spaces) *)
  let dot_pos =
    let rec find i =
      if source.[i] = '.' && i > 10 then
        i
      else
        find (i + 1)
    in
    find 10
  in
  let recv_end = find_recv_actual_end ~source ~dot_pos in
  (* ')' is at position 8, so recv_end should be 9 *)
  recv_end = 9

let%test "multiline chain with # comment between segments shows hint" =
  (* End-to-end: comment between chain segments should not break hint placement.
     "let r = 42.show()  # comment\n  .show();"
     ')' at offset 16, hint should be at offset 17 = line 0 char 17 *)
  let source = "let r = 42.show()  # comment\n  .show();" in
  let hints = get_hints source in
  let chain_hints =
    List.filter
      (fun (h : Lsp_t.InlayHint.t) ->
        h.paddingLeft = Some true
        &&
        match h.label with
        | `String s -> s = ": String"
        | _ -> false)
      hints
  in
  match chain_hints with
  | [ h ] -> h.position.line = 0 && h.position.character = 17
  | _ -> false

let%test "find_recv_actual_end with only whitespace between dot and recv" =
  (* Simple case: just newline + spaces *)
  let source = "42.show()\n  .show();" in
  let dot_pos = 12 in
  (* the second '.' *)
  let recv_end = find_recv_actual_end ~source ~dot_pos in
  recv_end = 9 (* right after ')' at pos 8 *)

let%test "find_recv_actual_end skips full-line # comment" =
  (* Full-line comment between chain segments:
     "42.show()\n  # full line comment\n  .show();"
     ')' at 8, '#' at 12, second '.' at 34 *)
  let source = "42.show()\n  # full line comment\n  .show();" in
  let recv_end = find_recv_actual_end ~source ~dot_pos:34 in
  recv_end = 9 (* right after ')' at pos 8 *)
