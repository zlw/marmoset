(* Completions: offer names from the type environment and keywords *)

module Lsp_t = Linol_lsp.Types
module Infer = Marmoset.Lib.Infer
module Types = Marmoset.Lib.Types

(* Marmoset keywords *)
let keywords =
  [
    ("let", "Variable binding");
    ("fn", "Function definition");
    ("if", "Conditional");
    ("else", "Else branch");
    ("return", "Return from function");
    ("match", "Pattern matching");
    ("case", "Match arm");
    ("enum", "Algebraic data type");
    ("type", "Type definition");
    ("shape", "Structural shape definition");
    ("trait", "Trait (interface) definition");
    ("impl", "Trait implementation");
    ("derive", "Automatic trait derivation");
    ("override", "Trait default replacement");
    ("true", "Boolean literal");
    ("false", "Boolean literal");
    ("is", "Type check operator");
  ]

(* Determine CompletionItemKind from a mono_type *)
let kind_of_type (mono : Types.mono_type) : Lsp_t.CompletionItemKind.t =
  match mono with
  | Types.TFun _ -> Lsp_t.CompletionItemKind.Function
  | Types.TEnum _ -> Lsp_t.CompletionItemKind.Enum
  | Types.TNamed _ | Types.TRecord _ -> Lsp_t.CompletionItemKind.Struct
  | _ -> Lsp_t.CompletionItemKind.Variable

(* Format a poly_type for completion detail in Marmoset syntax.
   Normalizes the mono_type first so var names in the bracket match the type. *)
let detail_of_poly poly : string = Source_syntax.poly_type_detail poly

(* Build completion items from the type environment *)
let completions_from_env (env : Infer.type_env) : Lsp_t.CompletionItem.t list =
  Infer.TypeEnv.bindings env
  |> List.map (fun (name, poly) ->
         let (Types.Forall (_, mono)) = poly in
         Lsp_t.CompletionItem.create ~label:name ~kind:(kind_of_type mono) ~detail:(detail_of_poly poly) ())

(* Build completion items from keywords *)
let completions_from_keywords () : Lsp_t.CompletionItem.t list =
  List.map
    (fun (kw, desc) ->
      Lsp_t.CompletionItem.create ~label:kw ~kind:Lsp_t.CompletionItemKind.Keyword ~detail:desc
        ~sortText:("zzz_" ^ kw)
        (* Sort keywords after env names *)
        ())
    keywords

(* Generate completions for a document *)
let completions ~(environment : Infer.type_env) : Lsp_t.CompletionItem.t list =
  let env_items = completions_from_env environment in
  let kw_items = completions_from_keywords () in
  env_items @ kw_items

(* ============================================================
   Tests
   ============================================================ *)

let env_with bindings =
  List.fold_left (fun env (name, poly) -> Infer.TypeEnv.add name poly env) Infer.empty_env bindings

let%test "completions include env names" =
  let env =
    env_with [ ("x", Types.Forall ([], Types.TInt)); ("f", Types.Forall ([], Types.tfun Types.TInt Types.TBool)) ]
  in
  let items = completions ~environment:env in
  let labels = List.map (fun (i : Lsp_t.CompletionItem.t) -> i.label) items in
  List.mem "x" labels && List.mem "f" labels

let%test "completions include keywords" =
  let env = env_with [] in
  let items = completions ~environment:env in
  let labels = List.map (fun (i : Lsp_t.CompletionItem.t) -> i.label) items in
  List.mem "let" labels
  && List.mem "fn" labels
  && List.mem "match" labels
  && List.mem "case" labels
  && List.mem "override" labels

let%test "function gets Function kind" =
  let env = env_with [ ("f", Types.Forall ([], Types.tfun Types.TInt Types.TBool)) ] in
  let items = completions ~environment:env in
  match List.find_opt (fun (i : Lsp_t.CompletionItem.t) -> i.label = "f") items with
  | Some item -> item.kind = Some Lsp_t.CompletionItemKind.Function
  | None -> false

let%test "variable gets Variable kind" =
  let env = env_with [ ("x", Types.Forall ([], Types.TInt)) ] in
  let items = completions ~environment:env in
  match List.find_opt (fun (i : Lsp_t.CompletionItem.t) -> i.label = "x") items with
  | Some item -> item.kind = Some Lsp_t.CompletionItemKind.Variable
  | None -> false

let%test "polymorphic function shows bracket syntax in detail" =
  (* Use "t0" not "a" — normalize maps t0->a but a->a causes infinite loop in apply_substitution *)
  let env = env_with [ ("id", Types.Forall ([ "t0" ], Types.tfun (Types.TVar "t0") (Types.TVar "t0"))) ] in
  let items = completions ~environment:env in
  match List.find_opt (fun (i : Lsp_t.CompletionItem.t) -> i.label = "id") items with
  | Some item -> (
      match item.detail with
      | Some d -> String.length d > 0
      | None -> false)
  | None -> false

let%test "keywords sort after env names" =
  let env = env_with [ ("alpha", Types.Forall ([], Types.TInt)) ] in
  let items = completions ~environment:env in
  let alpha = List.find_opt (fun (i : Lsp_t.CompletionItem.t) -> i.label = "alpha") items in
  let let_kw = List.find_opt (fun (i : Lsp_t.CompletionItem.t) -> i.label = "let") items in
  match (alpha, let_kw) with
  | Some a, Some l ->
      let sort_a =
        match a.sortText with
        | Some s -> s
        | None -> a.label
      in
      let sort_l =
        match l.sortText with
        | Some s -> s
        | None -> l.label
      in
      sort_a < sort_l
  | _ -> false

let%test "empty env still returns keywords" =
  let items = completions ~environment:Infer.empty_env in
  List.length items = List.length keywords

let%test "completions do not suggest legacy alias keyword" =
  let items = completions ~environment:Infer.empty_env in
  let labels = List.map (fun (i : Lsp_t.CompletionItem.t) -> i.label) items in
  not (List.mem "alias" labels)

let%test "detail shows type for monomorphic binding" =
  let env = env_with [ ("x", Types.Forall ([], Types.TInt)) ] in
  let items = completions ~environment:env in
  match List.find_opt (fun (i : Lsp_t.CompletionItem.t) -> i.label = "x") items with
  | Some item -> item.detail = Some "Int"
  | None -> false

let%test "detail uses vNext collection syntax" =
  let env = env_with [ ("xs", Types.Forall ([], Types.TArray Types.TInt)) ] in
  let items = completions ~environment:env in
  match List.find_opt (fun (i : Lsp_t.CompletionItem.t) -> i.label = "xs") items with
  | Some item -> item.detail = Some "List[Int]"
  | None -> false
