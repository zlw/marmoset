(* Document symbols: extract top-level definitions for outline view *)

module Lsp_t = Lsp.Types
module Ast = Marmoset.Lib.Ast

(* Extract document symbols from a program's top-level statements *)
let document_symbols ~(source : string) ~(program : Ast.AST.program) : Lsp_t.DocumentSymbol.t list =
  let symbol
      ~(name : string)
      ~(kind : Lsp_t.SymbolKind.t)
      ~(range : Lsp_t.Range.t)
      ?(children : Lsp_t.DocumentSymbol.t list = [])
      () : Lsp_t.DocumentSymbol.t =
    let children =
      if children = [] then
        None
      else
        Some children
    in
    Lsp_t.DocumentSymbol.create ~name ~kind ~range ~selectionRange:range ?children ()
  in
  let range_of_stmt (stmt : Ast.AST.statement) =
    Lsp_utils.offset_range_to_lsp ~source ~pos:stmt.pos ~end_pos:stmt.end_pos
  in
  List.filter_map
    (fun (stmt : Ast.AST.statement) ->
      match stmt.stmt with
      | Ast.AST.Let { name; value; _ } ->
          let kind =
            match value.expr with
            | Ast.AST.Function _ -> Lsp_t.SymbolKind.Function
            | _ -> Lsp_t.SymbolKind.Variable
          in
          Some (symbol ~name ~kind ~range:(range_of_stmt stmt) ())
      | Ast.AST.EnumDef { name; variants; _ } ->
          let children =
            List.map
              (fun (v : Ast.AST.variant_def) ->
                (* Variants don't have their own positions in the AST,
                   so use the parent enum's range *)
                symbol ~name:v.variant_name ~kind:Lsp_t.SymbolKind.EnumMember ~range:(range_of_stmt stmt) ())
              variants
          in
          Some (symbol ~name ~kind:Lsp_t.SymbolKind.Enum ~range:(range_of_stmt stmt) ~children ())
      | Ast.AST.TraitDef { name; _ } ->
          Some (symbol ~name ~kind:Lsp_t.SymbolKind.Interface ~range:(range_of_stmt stmt) ())
      | Ast.AST.ImplDef { impl_trait_name; impl_for_type; _ } ->
          let name = Printf.sprintf "impl %s for %s" impl_trait_name (Ast.AST.show_type_expr impl_for_type) in
          Some (symbol ~name ~kind:Lsp_t.SymbolKind.Class ~range:(range_of_stmt stmt) ())
      | Ast.AST.TypeAlias { alias_name; _ } ->
          Some (symbol ~name:alias_name ~kind:Lsp_t.SymbolKind.TypeParameter ~range:(range_of_stmt stmt) ())
      | Ast.AST.DeriveDef _ | Ast.AST.ExpressionStmt _ | Ast.AST.Return _ | Ast.AST.Block _ -> None)
    program

(* ============================================================
   Tests
   ============================================================ *)

(* Helper: parse source and extract symbols *)
let get_symbols source =
  match Marmoset.Lib.Parser.parse source with
  | Error _ -> []
  | Ok program -> document_symbols ~source ~program

let%test "let binding produces Variable symbol" =
  let symbols = get_symbols "let x = 42;" in
  match symbols with
  | [ s ] -> s.name = "x" && s.kind = Lsp_t.SymbolKind.Variable
  | _ -> false

let%test "function binding produces Function symbol" =
  let symbols = get_symbols "let f = fn(x) { x + 1 };" in
  match symbols with
  | [ s ] -> s.name = "f" && s.kind = Lsp_t.SymbolKind.Function
  | _ -> false

let%test "enum produces Enum symbol with variant children" =
  let symbols = get_symbols "enum option[a] {\n  some(a)\n  none\n}" in
  match symbols with
  | [ s ] -> (
      s.name = "option"
      && s.kind = Lsp_t.SymbolKind.Enum
      &&
      match s.children with
      | Some children -> List.length children = 2
      | None -> false)
  | _ -> false

let%test "trait produces Interface symbol" =
  let symbols = get_symbols "trait show[a] { fn show(x: a) -> string }" in
  match symbols with
  | [ s ] -> s.name = "show" && s.kind = Lsp_t.SymbolKind.Interface
  | _ -> false

let%test "type alias produces TypeParameter symbol" =
  let symbols = get_symbols "type point = { x: int, y: int }" in
  match symbols with
  | [ s ] -> s.name = "point" && s.kind = Lsp_t.SymbolKind.TypeParameter
  | _ -> false

let%test "multiple definitions produce multiple symbols" =
  let symbols = get_symbols "let x = 1; let f = fn(y) { y }; let z = \"hello\";" in
  List.length symbols = 3

let%test "expression statements are filtered out" =
  let symbols = get_symbols "42; let x = 1;" in
  List.length symbols = 1 && (List.hd symbols).name = "x"
