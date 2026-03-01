(* Code actions: "Add type annotation" for unannotated let bindings, params, and return types *)

module Lsp_t = Linol_lsp.Types
module Ast = Marmoset.Lib.Ast
module Infer = Marmoset.Lib.Infer
module Types = Marmoset.Lib.Types

(* ============================================================
   type_to_source: convert inferred mono_type to Marmoset source syntax
   ============================================================

   Internal Types.to_string produces "Int", "String", "[Int]", etc.
   Source syntax uses "int", "string", "list[int]", etc.
*)

let rec type_to_source (mono : Types.mono_type) : string =
  match mono with
  | Types.TInt -> "int"
  | Types.TFloat -> "float"
  | Types.TBool -> "bool"
  | Types.TString -> "string"
  | Types.TNull -> "null"
  | Types.TVar name -> name
  | Types.TRowVar name -> name
  | Types.TArray element -> "list[" ^ type_to_source element ^ "]"
  | Types.THash (key, value) -> "map[" ^ type_to_source key ^ ", " ^ type_to_source value ^ "]"
  | Types.TFun _ ->
      let rec collect_args = function
        | Types.TFun (arg, rest, _) ->
            let args, ret = collect_args rest in
            (arg :: args, ret)
        | t -> ([], t)
      in
      let args, ret = collect_args mono in
      let args_str =
        match args with
        | [ single ] -> "(" ^ type_to_source single ^ ")"
        | _ -> "(" ^ String.concat ", " (List.map type_to_source args) ^ ")"
      in
      args_str ^ " -> " ^ type_to_source ret
  | Types.TRecord (fields, row) ->
      let field_strs =
        List.map (fun (f : Types.record_field_type) -> f.name ^ ": " ^ type_to_source f.typ) fields
      in
      let row_str =
        match row with
        | None -> ""
        | Some r ->
            if field_strs = [] then
              "..." ^ type_to_source r
            else
              ", ..." ^ type_to_source r
      in
      "{ " ^ String.concat ", " field_strs ^ row_str ^ " }"
  | Types.TUnion types -> String.concat " | " (List.map type_to_source types)
  | Types.TEnum (name, []) -> name
  | Types.TEnum (name, args) -> name ^ "[" ^ String.concat ", " (List.map type_to_source args) ^ "]"

(* ============================================================
   Position helpers (duplicated from inlay_hints.ml for independence)
   ============================================================ *)

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

(* Find the byte offset just past ')' for function parameters.
   Search backward from `limit` for ')', skipping content inside # comments. *)
let find_close_paren ~source ~start ~limit =
  (* Check if offset i is inside a # comment by scanning backward on the same line *)
  let in_comment i =
    let rec scan j =
      if j < 0 || source.[j] = '\n' then
        false
      else if source.[j] = '#' then
        true
      else
        scan (j - 1)
    in
    scan (i - 1)
  in
  let rec scan i =
    if i < start then
      None
    else if source.[i] = ')' && not (in_comment i) then
      Some (i + 1)
    else
      scan (i - 1)
  in
  scan (limit - 1)

(* ============================================================
   Annotation site collection
   ============================================================ *)

type annotation_site = {
  insert_offset : int;
  insert_text : string;
  title : string;
}

(* Collect annotation sites for a single function expression *)
let sites_for_function ~source ~type_map ~sites (fn_expr : Ast.AST.expression) =
  match fn_expr.expr with
  | Ast.AST.Function { params; return_type; body; _ } -> (
      match Hashtbl.find_opt type_map fn_expr.id with
      | Some fn_type -> (
          let n_params = List.length params in
          (* Normalize the FULL function type once to preserve type variable identity *)
          let norm_fn_type = Types.normalize fn_type in
          let param_types = take_param_types n_params norm_fn_type in
          (* Parameter annotation sites *)
          let search_from = ref fn_expr.pos in
          List.iter2
            (fun (pname, annotation) ptype ->
              if annotation = None then
                match find_name_end ~source ~start:!search_from ~limit:fn_expr.end_pos pname with
                | Some pend ->
                    search_from := pend;
                    let type_str = type_to_source ptype in
                    sites :=
                      {
                        insert_offset = pend;
                        insert_text = ": " ^ type_str;
                        title = "Add type annotation: " ^ pname ^ ": " ^ type_str;
                      }
                      :: !sites
                | None -> ()
              else
                match find_name_end ~source ~start:!search_from ~limit:fn_expr.end_pos pname with
                | Some pend -> search_from := pend
                | None -> ())
            params
            (if List.length param_types = n_params then
               param_types
             else
               List.init n_params (fun _ -> Types.TNull));
          (* Return type annotation site *)
          if return_type = None && n_params > 0 then
            match get_return_type n_params norm_fn_type with
            | Some ret_type -> (
                let body_start = body.pos in
                match find_close_paren ~source ~start:fn_expr.pos ~limit:body_start with
                | Some paren_end ->
                    let type_str = type_to_source ret_type in
                    sites :=
                      {
                        insert_offset = paren_end;
                        insert_text = " -> " ^ type_str;
                        title = "Add return type annotation: -> " ^ type_str;
                      }
                      :: !sites
                | None -> ())
            | None -> ())
      | None -> ())
  | _ -> ()

(* Walk statements recursively, collecting annotation sites *)
let rec walk_stmt ~source ~type_map ~range_start ~range_end ~sites (stmt : Ast.AST.statement) =
  if stmt.end_pos < range_start || stmt.pos > range_end then
    ()
  else
    match stmt.stmt with
    | Ast.AST.Let { name; value; type_annotation = None; _ } ->
        (* Let binding annotation site *)
        (match Hashtbl.find_opt type_map value.id with
        | Some mono -> (
            match find_name_end ~source ~start:stmt.pos ~limit:value.pos name with
            | Some name_end ->
                let type_str = type_to_source (Types.normalize mono) in
                sites :=
                  {
                    insert_offset = name_end;
                    insert_text = ": " ^ type_str;
                    title = "Add type annotation: " ^ name ^ ": " ^ type_str;
                  }
                  :: !sites
            | None -> ())
        | None -> ());
        sites_for_function ~source ~type_map ~sites value;
        walk_expr ~source ~type_map ~range_start ~range_end ~sites value
    | Ast.AST.Let { value; _ } ->
        sites_for_function ~source ~type_map ~sites value;
        walk_expr ~source ~type_map ~range_start ~range_end ~sites value
    | Ast.AST.Block stmts -> List.iter (walk_stmt ~source ~type_map ~range_start ~range_end ~sites) stmts
    | Ast.AST.ExpressionStmt e -> walk_expr ~source ~type_map ~range_start ~range_end ~sites e
    | Ast.AST.Return e -> walk_expr ~source ~type_map ~range_start ~range_end ~sites e
    | Ast.AST.EnumDef _ | Ast.AST.TraitDef _ | Ast.AST.ImplDef _ | Ast.AST.InherentImplDef _ | Ast.AST.DeriveDef _
    | Ast.AST.TypeAlias _ ->
        ()

and walk_expr ~source ~type_map ~range_start ~range_end ~sites (expr : Ast.AST.expression) =
  if expr.end_pos < range_start || expr.pos > range_end then
    ()
  else
    match expr.expr with
    | Ast.AST.Function { body; _ } ->
        (* NOTE: sites_for_function is NOT called here — it's called by walk_stmt
           for let bindings. Calling it here too would produce duplicate sites. *)
        walk_stmt ~source ~type_map ~range_start ~range_end ~sites body
    | Ast.AST.If (cond, then_, else_) ->
        walk_expr ~source ~type_map ~range_start ~range_end ~sites cond;
        walk_stmt ~source ~type_map ~range_start ~range_end ~sites then_;
        Option.iter (walk_stmt ~source ~type_map ~range_start ~range_end ~sites) else_
    | Ast.AST.Call (fn_expr, args) ->
        walk_expr ~source ~type_map ~range_start ~range_end ~sites fn_expr;
        List.iter (walk_expr ~source ~type_map ~range_start ~range_end ~sites) args
    | Ast.AST.Infix (l, _, r) ->
        walk_expr ~source ~type_map ~range_start ~range_end ~sites l;
        walk_expr ~source ~type_map ~range_start ~range_end ~sites r
    | Ast.AST.Prefix (_, e) -> walk_expr ~source ~type_map ~range_start ~range_end ~sites e
    | Ast.AST.Index (arr, idx) ->
        walk_expr ~source ~type_map ~range_start ~range_end ~sites arr;
        walk_expr ~source ~type_map ~range_start ~range_end ~sites idx
    | Ast.AST.Array elts -> List.iter (walk_expr ~source ~type_map ~range_start ~range_end ~sites) elts
    | Ast.AST.Hash pairs ->
        List.iter
          (fun (k, v) ->
            walk_expr ~source ~type_map ~range_start ~range_end ~sites k;
            walk_expr ~source ~type_map ~range_start ~range_end ~sites v)
          pairs
    | Ast.AST.FieldAccess (e, _) -> walk_expr ~source ~type_map ~range_start ~range_end ~sites e
    | Ast.AST.MethodCall (recv, _, args) ->
        walk_expr ~source ~type_map ~range_start ~range_end ~sites recv;
        List.iter (walk_expr ~source ~type_map ~range_start ~range_end ~sites) args
    | Ast.AST.Match (scrutinee, arms) ->
        walk_expr ~source ~type_map ~range_start ~range_end ~sites scrutinee;
        List.iter
          (fun (arm : Ast.AST.match_arm) -> walk_expr ~source ~type_map ~range_start ~range_end ~sites arm.body)
          arms
    | Ast.AST.RecordLit (fields, spread) ->
        List.iter
          (fun (f : Ast.AST.record_field) ->
            Option.iter (walk_expr ~source ~type_map ~range_start ~range_end ~sites) f.field_value)
          fields;
        Option.iter (walk_expr ~source ~type_map ~range_start ~range_end ~sites) spread
    | Ast.AST.EnumConstructor (_, _, args) ->
        List.iter (walk_expr ~source ~type_map ~range_start ~range_end ~sites) args
    | Ast.AST.TypeCheck (e, _) -> walk_expr ~source ~type_map ~range_start ~range_end ~sites e
    | Ast.AST.Identifier _ | Ast.AST.Integer _ | Ast.AST.Float _ | Ast.AST.Boolean _ | Ast.AST.String _ -> ()

(* ============================================================
   Build LSP CodeAction values
   ============================================================ *)

let make_code_action ~source ~uri (site : annotation_site) : Lsp_t.CodeAction.t =
  let pos = Lsp_utils.offset_to_position ~source ~offset:site.insert_offset in
  let range = Lsp_t.Range.create ~start:pos ~end_:pos in
  let edit = Lsp_t.TextEdit.create ~newText:site.insert_text ~range in
  let ws_edit = Lsp_t.WorkspaceEdit.create ~changes:[ (uri, [ edit ]) ] () in
  Lsp_t.CodeAction.create ~title:site.title ~kind:Lsp_t.CodeActionKind.QuickFix ~edit:ws_edit ()

let make_annotate_all_action ~source ~uri (all_sites : annotation_site list) : Lsp_t.CodeAction.t =
  let edits =
    List.map
      (fun (site : annotation_site) ->
        let pos = Lsp_utils.offset_to_position ~source ~offset:site.insert_offset in
        let range = Lsp_t.Range.create ~start:pos ~end_:pos in
        Lsp_t.TextEdit.create ~newText:site.insert_text ~range)
      all_sites
  in
  let ws_edit = Lsp_t.WorkspaceEdit.create ~changes:[ (uri, edits) ] () in
  Lsp_t.CodeAction.create ~title:"Add all type annotations" ~kind:Lsp_t.CodeActionKind.QuickFix ~edit:ws_edit ()

(* ============================================================
   Public entry point
   ============================================================ *)

let compute
    ~(source : string)
    ~(uri : Lsp_t.DocumentUri.t)
    ~(program : Ast.AST.program)
    ~(type_map : Infer.type_map)
    ~(range : Lsp_t.Range.t) : Lsp_t.CodeAction.t list =
  let range_start =
    Lsp_utils.position_to_offset ~source ~line:range.start.line ~character:range.start.character
  in
  let range_end = Lsp_utils.position_to_offset ~source ~line:range.end_.line ~character:range.end_.character in
  let sites = ref [] in
  List.iter (walk_stmt ~source ~type_map ~range_start ~range_end ~sites) program;
  let all_sites = List.rev !sites in
  match all_sites with
  | [] -> []
  | _ ->
      let individual = List.map (make_code_action ~source ~uri) all_sites in
      if List.length all_sites >= 2 then
        individual @ [ make_annotate_all_action ~source ~uri all_sites ]
      else
        individual

(* ============================================================
   Tests
   ============================================================ *)

(* Helper: parse, typecheck, and get code actions for full document *)
let get_actions source =
  let result = Doc_state.analyze ~source in
  match (result.program, result.type_map) with
  | Some prog, Some tm ->
      let range =
        Lsp_t.Range.create
          ~start:(Lsp_t.Position.create ~line:0 ~character:0)
          ~end_:(Lsp_t.Position.create ~line:999 ~character:0)
      in
      compute ~source ~uri:(Lsp_t.DocumentUri.of_string "file:///test.mr") ~program:prog ~type_map:tm ~range
  | _ -> []

let action_titles actions = List.map (fun (a : Lsp_t.CodeAction.t) -> a.title) actions

let starts_with s prefix =
  let pl = String.length prefix in
  String.length s >= pl && String.sub s 0 pl = prefix

(* type_to_source unit tests *)

let%test "type_to_source int" = type_to_source Types.TInt = "int"
let%test "type_to_source float" = type_to_source Types.TFloat = "float"
let%test "type_to_source bool" = type_to_source Types.TBool = "bool"
let%test "type_to_source string" = type_to_source Types.TString = "string"
let%test "type_to_source null" = type_to_source Types.TNull = "null"
let%test "type_to_source var" = type_to_source (Types.TVar "a") = "a"
let%test "type_to_source array" = type_to_source (Types.TArray Types.TInt) = "list[int]"

let%test "type_to_source nested array" =
  type_to_source (Types.TArray (Types.TArray Types.TString)) = "list[list[string]]"

let%test "type_to_source hash" = type_to_source (Types.THash (Types.TString, Types.TInt)) = "map[string, int]"

let%test "type_to_source single-arg function" =
  type_to_source (Types.tfun Types.TInt Types.TBool) = "(int) -> bool"

let%test "type_to_source multi-arg function" =
  type_to_source (Types.tfun Types.TInt (Types.tfun Types.TString Types.TBool)) = "(int, string) -> bool"

let%test "type_to_source record" =
  type_to_source (Types.TRecord ([ { name = "x"; typ = Types.TInt }; { name = "y"; typ = Types.TString } ], None))
  = "{ x: int, y: string }"

let%test "type_to_source union" = type_to_source (Types.TUnion [ Types.TInt; Types.TString ]) = "int | string"
let%test "type_to_source enum no args" = type_to_source (Types.TEnum ("direction", [])) = "direction"
let%test "type_to_source enum with args" = type_to_source (Types.TEnum ("option", [ Types.TInt ])) = "option[int]"

let%test "type_to_source enum multi args" =
  type_to_source (Types.TEnum ("result", [ Types.TString; Types.TInt ])) = "result[string, int]"

(* Code action integration tests *)

let%test "let binding gets add type annotation action" =
  let actions = get_actions "let x = 42;" in
  let titles = action_titles actions in
  List.exists (fun t -> starts_with t "Add type annotation:") titles

let%test "function param gets annotation action" =
  let actions = get_actions "let f = fn(x) { x + 1 };" in
  let titles = action_titles actions in
  List.exists (fun t -> starts_with t "Add type annotation:") titles
  && List.exists (fun t -> starts_with t "Add return type annotation") titles

let%test "function with annotated param gets no param action but gets return type" =
  let actions = get_actions "let f = fn(x: int) { x + 1 };" in
  let titles = action_titles actions in
  (not (List.exists (fun t -> starts_with t "Add type annotation: x:") titles))
  && List.exists (fun t -> starts_with t "Add return type annotation") titles

let%test "fully annotated function gets no param or return actions" =
  let actions = get_actions "let f: int -> int = fn(x: int) -> int { x + 1 };" in
  let titles = action_titles actions in
  (not (List.exists (fun t -> starts_with t "Add type annotation:") titles))
  && not (List.exists (fun t -> starts_with t "Add return type annotation") titles)

let%test "multiple sites produces annotate-all action" =
  let actions = get_actions "let x = 1; let y = true;" in
  let titles = action_titles actions in
  List.exists (fun t -> t = "Add all type annotations") titles

let%test "single site does not produce annotate-all action" =
  let actions = get_actions "let x: int = 1; let y = true;" in
  let titles = action_titles actions in
  (* Only y needs annotation — single site, no "annotate all" *)
  not (List.exists (fun t -> t = "Add all type annotations") titles)

let%test "no actions when parse fails" =
  let actions = get_actions "let = ;" in
  actions = []

let%test "range filtering works" =
  let source = "let x = 1;\nlet y = true;" in
  let result = Doc_state.analyze ~source in
  match (result.program, result.type_map) with
  | Some prog, Some tm ->
      (* Range covering only line 0 (characters 0..10) *)
      let range =
        Lsp_t.Range.create
          ~start:(Lsp_t.Position.create ~line:0 ~character:0)
          ~end_:(Lsp_t.Position.create ~line:0 ~character:10)
      in
      let actions =
        compute ~source ~uri:(Lsp_t.DocumentUri.of_string "file:///test.mr") ~program:prog ~type_map:tm ~range
      in
      let titles = action_titles actions in
      let has_x = List.exists (fun t -> starts_with t "Add type annotation: x:") titles in
      let has_y = List.exists (fun t -> starts_with t "Add type annotation: y:") titles in
      has_x && not has_y
  | _ -> false
