(* Signature help: show function parameter info at call sites *)

module Lsp_t = Linol_lsp.Types
module Ast = Marmoset.Lib.Ast
module Infer = Marmoset.Lib.Infer
module Types = Marmoset.Lib.Types
module Trait_registry = Typecheck.Trait_registry

(* Decompose a curried TFun into a flat param list and return type.
   TFun(a, TFun(b, c)) → ([a; b], c) *)
let rec collect_params = function
  | Types.TFun (arg, rest, _) ->
      let params, ret = collect_params rest in
      (arg :: params, ret)
  | t -> ([], t)

(* Info about an enclosing call site found by AST walking *)
type call_info = {
  fn_id : int; (* fn_expr.id for type_map lookup *)
  fn_name : string option; (* identifier name if Call(Identifier name, _) *)
  method_name : string option; (* for MethodCall *)
  recv_id : int option; (* receiver expr.id for MethodCall type lookup *)
  args_start : int; (* byte offset of first arg (after open paren) *)
  call_end : int; (* byte offset of call end *)
  arg_count : int; (* number of args in AST *)
}

(* Find the opening '(' between two byte offsets in source *)
let find_open_paren ~(source : string) ~(from : int) ~(limit : int) : int option =
  let len = String.length source in
  let limit = min limit len in
  let rec scan i =
    if i >= limit then
      None
    else if source.[i] = '(' then
      Some (i + 1)
      (* return position after '(' *)
    else
      scan (i + 1)
  in
  scan from

(* Find the matching ')' starting from the position after '(', tracking nesting.
   Returns the position OF the ')'. *)
let find_matching_close_paren ~(source : string) ~(from : int) : int option =
  let len = String.length source in
  let rec scan i depth =
    if i >= len then
      None
    else
      match source.[i] with
      | '(' -> scan (i + 1) (depth + 1)
      | ')' ->
          if depth = 0 then
            Some i
          else
            scan (i + 1) (depth - 1)
      | '"' -> skip_string (i + 1) depth
      | _ -> scan (i + 1) depth
  and skip_string i depth =
    if i >= len then
      None
    else
      match source.[i] with
      | '"' -> scan (i + 1) depth
      | '\\' -> skip_string (i + 2) depth
      | _ -> skip_string (i + 1) depth
  in
  scan from 0

(* Find the innermost enclosing Call or MethodCall whose argument region
   contains the cursor offset. We need the *enclosing* call — cursor may be
   on whitespace between args, not on any expression. *)
let find_enclosing_call ~(source : string) (offset : int) (program : Ast.AST.program) : call_info option =
  let best = ref None in
  let update_best info =
    match !best with
    | None -> best := Some info
    | Some prev ->
        (* Prefer tighter (smaller) span *)
        let prev_span = prev.call_end - prev.args_start in
        let new_span = info.call_end - info.args_start in
        if new_span < prev_span then
          best := Some info
  in
  let rec visit_expr (expr : Ast.AST.expression) =
    match expr.expr with
    | Ast.AST.Call (fn_expr, args) ->
        (* The call spans expr.pos..expr.end_pos. Find the '(' after fn_expr
           to determine where args actually begin.
           Only trigger when cursor is INSIDE the parens, not on the function name.
           Use find_matching_close_paren instead of expr.end_pos because inner
           calls in chains have wrong end_pos (points to dot, not closing paren). *)
        let fn_name =
          match fn_expr.expr with
          | Ast.AST.Identifier name -> Some name
          | _ -> None
        in
        let open_paren = find_open_paren ~source ~from:(fn_expr.end_pos + 1) ~limit:(String.length source) in
        let args_start =
          match open_paren with
          | Some pos -> pos
          | None -> fn_expr.end_pos + 2
        in
        let effective_end =
          match find_matching_close_paren ~source ~from:args_start with
          | Some close -> close
          | None -> expr.end_pos
        in
        if offset >= args_start && offset <= effective_end then
          update_best
            {
              fn_id = fn_expr.id;
              fn_name;
              method_name = None;
              recv_id = None;
              args_start;
              call_end = effective_end;
              arg_count = List.length args;
            };
        visit_expr fn_expr;
        List.iter visit_expr args
    | Ast.AST.MethodCall { mc_receiver = recv; mc_method = mname; mc_args = args; _ } ->
        (* For method calls like recv.method(args), find '(' after the dot.
           Use expr.pos (the dot position) as search start since recv.end_pos
           may be wrong for inner chain expressions.
           Use find_matching_close_paren instead of expr.end_pos for the same
           reason as Call — inner chain expressions have wrong end_pos. *)
        let open_paren = find_open_paren ~source ~from:expr.pos ~limit:(String.length source) in
        let args_start =
          match open_paren with
          | Some pos -> pos
          | None -> expr.pos + String.length mname + 2
        in
        let effective_end =
          match find_matching_close_paren ~source ~from:args_start with
          | Some close -> close
          | None -> expr.end_pos
        in
        if offset >= args_start && offset <= effective_end then
          update_best
            {
              fn_id = expr.id;
              fn_name = None;
              method_name = Some mname;
              recv_id = Some recv.id;
              args_start;
              call_end = effective_end;
              arg_count = List.length args;
            };
        visit_expr recv;
        List.iter visit_expr args
    | Ast.AST.Infix (l, _, r) ->
        visit_expr l;
        visit_expr r
    | Ast.AST.Prefix (_, e) -> visit_expr e
    | Ast.AST.If (cond, then_, else_) ->
        visit_expr cond;
        visit_stmt then_;
        Option.iter visit_stmt else_
    | Ast.AST.Function { body; _ } -> visit_stmt body
    | Ast.AST.Index (a, b) ->
        visit_expr a;
        visit_expr b
    | Ast.AST.Array elts -> List.iter visit_expr elts
    | Ast.AST.Hash pairs ->
        List.iter
          (fun (k, v) ->
            visit_expr k;
            visit_expr v)
          pairs
    | Ast.AST.FieldAccess (e, _) -> visit_expr e
    | Ast.AST.Match (scrutinee, arms) ->
        visit_expr scrutinee;
        List.iter (fun (arm : Ast.AST.match_arm) -> visit_expr arm.body) arms
    | Ast.AST.RecordLit (fields, spread) ->
        List.iter (fun (f : Ast.AST.record_field) -> Option.iter visit_expr f.field_value) fields;
        Option.iter visit_expr spread
    | Ast.AST.EnumConstructor (_, _, args) -> List.iter visit_expr args
    | Ast.AST.TypeCheck (e, _) -> visit_expr e
    | Ast.AST.Identifier _ | Ast.AST.Integer _ | Ast.AST.Float _ | Ast.AST.Boolean _ | Ast.AST.String _ -> ()
  and visit_stmt (stmt : Ast.AST.statement) =
    match stmt.stmt with
    | Ast.AST.ExpressionStmt e -> visit_expr e
    | Ast.AST.Let { value; _ } -> visit_expr value
    | Ast.AST.Return e -> visit_expr e
    | Ast.AST.Block stmts -> List.iter visit_stmt stmts
    | Ast.AST.EnumDef _ | Ast.AST.TraitDef _ | Ast.AST.ImplDef _ | Ast.AST.InherentImplDef _ | Ast.AST.DeriveDef _
    | Ast.AST.TypeAlias _ ->
        ()
  in
  List.iter visit_stmt program;
  !best

(* Count commas at depth 0 between `from` and `cursor` in source to determine
   the 0-indexed active parameter. Tracks paren/bracket/brace nesting. *)
let count_active_param ~(source : string) ~(from : int) ~(cursor : int) : int =
  let len = String.length source in
  let cursor = min cursor len in
  let rec scan i depth count =
    if i >= cursor then
      count
    else
      let c = source.[i] in
      match c with
      | '(' | '[' | '{' -> scan (i + 1) (depth + 1) count
      | ')' | ']' | '}' -> scan (i + 1) (max 0 (depth - 1)) count
      | ',' when depth = 0 -> scan (i + 1) depth (count + 1)
      | '"' -> skip_string (i + 1) depth count
      | _ -> scan (i + 1) depth count
  and skip_string i depth count =
    if i >= cursor then
      count
    else
      match source.[i] with
      | '"' -> scan (i + 1) depth count
      | '\\' -> skip_string (i + 2) depth count (* skip escaped char *)
      | _ -> skip_string (i + 1) depth count
  in
  scan from 0 0

(* Look up param names from a let-bound Function in the program *)
let find_param_names ~(program : Ast.AST.program) ~(name : string) : string list option =
  let rec search_stmts stmts =
    List.find_map
      (fun (stmt : Ast.AST.statement) ->
        match stmt.stmt with
        | Ast.AST.Let { name = n; value; _ } when n = name -> (
            match value.expr with
            | Ast.AST.Function { params; _ } -> Some (List.map fst params)
            | _ -> None)
        | Ast.AST.Block stmts -> search_stmts stmts
        | _ -> None)
      stmts
  in
  search_stmts program

(* Build a signature label string like "fn(x: Int, y: String) -> Bool"
   and return the byte offset ranges for each parameter within the label. *)
let build_label
    ~(param_names : string list)
    ~(param_types : Types.mono_type list)
    ~(ret_type : Types.mono_type)
    ~(fn_display_name : string) : string * (int * int) list =
  let param_names_arr = Array.of_list param_names in
  let param_name_count = Array.length param_names_arr in
  let buf = Buffer.create 64 in
  Buffer.add_string buf fn_display_name;
  Buffer.add_char buf '(';
  let offsets = ref [] in
  List.iteri
    (fun i typ ->
      if i > 0 then
        Buffer.add_string buf ", ";
      let name =
        if i < param_name_count then
          param_names_arr.(i)
        else
          Printf.sprintf "arg%d" (i + 1)
      in
      let start = Buffer.length buf in
      Buffer.add_string buf name;
      Buffer.add_string buf ": ";
      Buffer.add_string buf (Types.to_string typ);
      let stop = Buffer.length buf in
      offsets := (start, stop) :: !offsets)
    param_types;
  Buffer.add_string buf ") -> ";
  Buffer.add_string buf (Types.to_string ret_type);
  (Buffer.contents buf, List.rev !offsets)

(* Provide signature help at a given cursor position *)
let signature_help
    ~(source : string)
    ~(program : Ast.AST.program)
    ~(type_map : Infer.type_map)
    ~(environment : Infer.type_env)
    ~(line : int)
    ~(character : int) : Lsp_t.SignatureHelp.t option =
  let offset = Lsp_utils.position_to_offset ~source ~line ~character in
  match find_enclosing_call ~source offset program with
  | None -> None
  | Some call -> (
      (* Determine the function type *)
      let fn_type_opt =
        match call.method_name with
        | Some mname -> (
            (* Method call: look up receiver type, then resolve method *)
            match call.recv_id with
            | None -> None
            | Some recv_id -> (
                match Hashtbl.find_opt type_map recv_id with
                | None -> None
                | Some recv_type -> (
                    match Trait_registry.lookup_method recv_type mname with
                    | None -> None
                    | Some (_trait_name, method_sig) ->
                        (* Reconstruct a function type from method_sig params → return *)
                        let param_types = List.map snd method_sig.method_params in
                        let param_names = List.map fst method_sig.method_params in
                        Some (`Method (param_types, method_sig.method_return_type, param_names)))))
        | None -> (
            (* Regular call: look up fn_expr type *)
            match Hashtbl.find_opt type_map call.fn_id with
            | Some fn_type -> Some (`FnType fn_type)
            | None -> (
                (* Try environment for polymorphic functions *)
                match call.fn_name with
                | Some name -> (
                    match Infer.TypeEnv.find_opt name environment with
                    | Some (Types.Forall (_, mono)) -> Some (`FnType mono)
                    | None -> None)
                | None -> None))
      in
      match fn_type_opt with
      | None -> None
      | Some info -> (
          let param_types, ret_type, method_param_names =
            match info with
            | `FnType fn_type ->
                (* Normalize the full function type once to preserve type variable identity *)
                let norm = Types.normalize fn_type in
                let params, ret = collect_params norm in
                (params, ret, None)
            | `Method (ptypes, ret, pnames) ->
                (* Method types come from trait registry — normalize together *)
                let dummy_fn = List.fold_right (fun p acc -> Types.tfun p acc) ptypes ret in
                let norm = Types.normalize dummy_fn in
                let params, ret_n = collect_params norm in
                (params, ret_n, Some pnames)
          in
          match param_types with
          | [] -> None (* Not a function type — e.g. 42() *)
          | _ ->
              let fn_display_name =
                match call.method_name with
                | Some mname -> mname
                | None -> (
                    match call.fn_name with
                    | Some name -> name
                    | None -> "fn")
              in
              let param_names =
                match method_param_names with
                | Some names -> names
                | None -> (
                    match call.fn_name with
                    | Some name -> (
                        match find_param_names ~program ~name with
                        | Some names -> names
                        | None -> [])
                    | None -> [])
              in
              let label, offsets = build_label ~param_names ~param_types ~ret_type ~fn_display_name in
              let parameters =
                List.map
                  (fun (start, stop) -> Lsp_t.ParameterInformation.create ~label:(`Offset (start, stop)) ())
                  offsets
              in
              let active_param =
                let raw = count_active_param ~source ~from:call.args_start ~cursor:offset in
                min raw (List.length param_types - 1)
              in
              let sig_info =
                Lsp_t.SignatureInformation.create ~label ~parameters ~activeParameter:(Some active_param) ()
              in
              Some
                (Lsp_t.SignatureHelp.create ~signatures:[ sig_info ] ~activeSignature:0
                   ~activeParameter:(Some active_param) ())))

(* ============================================================
   Tests
   ============================================================ *)

(* Helper: parse, typecheck, and get signature help *)
let check_sig source line character =
  let result = Doc_state.analyze ~source in
  match (result.program, result.type_map, result.environment) with
  | Some prog, Some tm, Some env ->
      signature_help ~source ~program:prog ~type_map:tm ~environment:env ~line ~character
  | _ -> None

let%test "single param function — cursor inside parens shows signature" =
  (* "let f = fn(x: int) { x }; f(1)" *)
  (* cursor at the '1' inside f(1) *)
  let source = "let f = fn(x: int) { x }; f(1)" in
  match check_sig source 0 29 with
  | Some sh -> (
      List.length sh.signatures = 1
      &&
      let sig0 = List.hd sh.signatures in
      sig0.label <> ""
      &&
      match sig0.parameters with
      | Some params -> List.length params = 1
      | None -> false)
  | None -> false

let%test "two param function — cursor after comma shows activeParam=1" =
  let source = "let f = fn(x, y) { x + y }; f(1, 2)" in
  (* cursor at '2' which is at index 33 *)
  match check_sig source 0 33 with
  | Some sh -> (
      match sh.activeParameter with
      | Some (Some n) -> n = 1
      | _ -> false)
  | None -> false

let%test "nested call — innermost wins" =
  let source = "let f = fn(x) { x }; let g = fn(y) { y }; f(g(1))" in
  (* cursor at '1' inside g(1), which is at index 46 *)
  match check_sig source 0 46 with
  | Some sh ->
      let sig0 = List.hd sh.signatures in
      (* Should show g's signature, not f's *)
      let has_g = try String.sub sig0.label 0 1 = "g" with _ -> false in
      has_g
  | None -> false

let%test "cursor outside parens returns None" =
  let source = "let f = fn(x) { x }; f(1)" in
  (* cursor at 'f' before the call *)
  let result = check_sig source 0 21 in
  result = None

let%test "non-function call returns None" =
  let source = "let x = 42" in
  let result = check_sig source 0 5 in
  result = None

let%test "polymorphic function shows type variables" =
  let source = "let id = fn(x) { x }; id(1)" in
  (* cursor at '1' inside id(1) *)
  match check_sig source 0 26 with
  | Some sh ->
      let sig0 = List.hd sh.signatures in
      (* Should have a signature label *)
      String.length sig0.label > 0
  | None -> false

let%test "activeParam=0 for first arg" =
  let source = "let f = fn(x, y) { x + y }; f(1, 2)" in
  (* cursor at '1', before the comma *)
  match check_sig source 0 31 with
  | Some sh -> (
      match sh.activeParameter with
      | Some (Some n) -> n = 0
      | _ -> false)
  | None -> false

let%test "empty args shows signature with activeParam=0" =
  let source = "let f = fn(x: int) { x }; f()" in
  (* cursor on ')' at index 28, which is inside the call *)
  match check_sig source 0 28 with
  | Some sh -> (
      match sh.activeParameter with
      | Some (Some n) -> n = 0
      | _ -> false)
  | None -> false

let string_contains haystack needle = Diagnostics.String_utils.contains_substring ~needle haystack

let%test "parameter names come from function definition" =
  let source = "let greet = fn(name: string) { name }; greet(\"hi\")" in
  (* cursor inside greet("hi") *)
  match check_sig source 0 47 with
  | Some sh ->
      let sig0 = List.hd sh.signatures in
      string_contains sig0.label "name"
  | None -> false

let%test "parameter offsets use Offset label" =
  let source = "let f = fn(x: int, y: string) { x }; f(1, \"hi\")" in
  match check_sig source 0 41 with
  | Some sh -> (
      let sig0 = List.hd sh.signatures in
      match sig0.parameters with
      | Some params ->
          List.for_all
            (fun (p : Lsp_t.ParameterInformation.t) ->
              match p.label with
              | `Offset (s, e) -> s >= 0 && e > s
              | `String _ -> false)
            params
      | None -> false)
  | None -> false

let%test "inner function in nested call gets signature help (BUG-27)" =
  (* f(g(1)) — cursor on '1' inside g(1) should show g's signature *)
  let source = "let g = fn(x: int) { x }; let f = fn(y: int) { y }; f(g(1))" in
  (* Positions: f(52) ((53) g(54) ((55) 1(56) )(57) )(58) *)
  match check_sig source 0 56 with
  | Some sh ->
      let sig0 = List.hd sh.signatures in
      (* Should show g's signature, not f's *)
      let has_g = try String.sub sig0.label 0 1 = "g" with _ -> false in
      has_g
  | None -> false

let%test "outer function in nested call still works (BUG-27 regression)" =
  (* f(g(1), 2) — cursor on '2' should show f's signature *)
  let source = "let g = fn(x: int) { x }; let f = fn(y: int, z: int) { y + z }; f(g(1), 2)" in
  (* f(...) args: g(1) then , then 2 — cursor on '2' should be in f but not g *)
  let pos = String.length source - 2 in
  (* position of '2' *)
  match check_sig source 0 pos with
  | Some sh ->
      let sig0 = List.hd sh.signatures in
      let has_f = try String.sub sig0.label 0 1 = "f" with _ -> false in
      has_f
  | None -> false

(* Phase 8 regression: signature help on dot-style method calls *)
let%test "signature help on dot method call shows method signature" =
  let source =
    "trait greet[a] {\n  fn hello(x: a) -> string\n}\nimpl greet for int {\n  fn hello(x: int) -> string { \"hi\" }\n}\n42.hello()"
  in
  (* Last line: 42.hello() — cursor inside parens *)
  let last_line = 6 in
  let col = 9 in
  match check_sig source last_line col with
  | Some sh -> List.length sh.signatures >= 1
  | None -> false
