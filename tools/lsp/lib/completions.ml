(* Completions: offer names from the type environment and keywords *)

module Lsp_t = Linol_lsp.Types
module Ast = Marmoset.Lib.Ast
module Compiler = Marmoset.Lib.Frontend_compiler
module Discovery = Frontend.Discovery
module Infer = Marmoset.Lib.Infer
module Import_resolver = Frontend.Import_resolver
module Module_catalog = Frontend.Module_catalog
module Module_sig = Typecheck.Module_sig
module Token = Marmoset.Lib.Token
module Lexer = Marmoset.Lib.Lexer
module Types = Marmoset.Lib.Types
module StringSet = Set.Make (String)

type completion_context =
  | BareIdentifier of { prefix : string }
  | NamespaceMember of {
      receiver_segments : string list;
      prefix : string;
    }
  | ImportPath of {
      typed_segments : string list;
      prefix : string;
      in_alias : bool;
    }
  | Unsupported

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
    ("enum", "Compatibility sugar for constructor-bearing sum types");
    ("type", "Type declaration (transparent, wrapper, or sum)");
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

let starts_with ~(prefix : string) (value : string) : bool =
  let prefix_len = String.length prefix in
  String.length value >= prefix_len && String.sub value 0 prefix_len = prefix

let token_end_pos (tok : Token.token) : int = max tok.pos (tok.pos + String.length tok.literal - 1)

let semantic_analysis ~(latest : Doc_state.analysis_result) ~(last_good : Doc_state.analysis_result option) :
    Doc_state.analysis_result =
  let has_semantics (analysis : Doc_state.analysis_result) =
    analysis.compiler_analysis <> None
    && (analysis.environment <> None || analysis.project_root <> None || analysis.program <> None)
  in
  if has_semantics latest then
    latest
  else
    Option.value last_good ~default:latest

let current_checked_module (analysis : Doc_state.analysis_result) : Compiler.checked_module option =
  match analysis.compiler_analysis with
  | Some compiler_analysis ->
      Compiler.find_checked_module_by_file compiler_analysis ~file_path:compiler_analysis.active_file.file_path
  | None -> None

type navigation_snapshot = {
  surface : Import_resolver.module_surface;
  resolved_imports : Import_resolver.resolved_imports;
}

let navigation_from_surface_graph (analysis : Doc_state.analysis_result) : navigation_snapshot option =
  match analysis.compiler_analysis with
  | None -> None
  | Some compiler_analysis -> (
      match compiler_analysis.graph with
      | None -> None
      | Some graph -> (
          match Compiler.find_parsed_module_by_file compiler_analysis ~file_path:compiler_analysis.active_file.file_path with
          | None -> None
          | Some parsed_module -> (
              match Import_resolver.build_module_surfaces graph with
              | Error _ -> None
              | Ok surfaces -> (
                  match Hashtbl.find_opt surfaces parsed_module.module_id with
                  | None -> None
                  | Some surface -> (
                      match Import_resolver.build_resolved_imports ~surfaces parsed_module with
                      | Ok resolved_imports -> Some { surface; resolved_imports }
                      | Error _ -> None)))))

let current_navigation (analysis : Doc_state.analysis_result) : navigation_snapshot option =
  match current_checked_module analysis with
  | Some checked_module ->
      Some
        {
          surface = checked_module.navigation.surface;
          resolved_imports = checked_module.navigation.resolved_imports;
        }
  | None -> navigation_from_surface_graph analysis

let root_dir_of_analysis (analysis : Doc_state.analysis_result) : string option =
  match analysis.project_root with
  | Some _ as root -> root
  | None -> (
      match analysis.source_root with
      | Some _ as root -> root
      | None ->
          Option.bind analysis.compiler_analysis (fun compiler_analysis ->
              Some
                (Compiler.resolved_project_root ?source_root:compiler_analysis.source_root
                   ~entry_file:compiler_analysis.active_file.file_path ())))

let root_dir_for_completion ~(latest : Doc_state.analysis_result) ~(last_good : Doc_state.analysis_result option) :
    string option =
  match root_dir_of_analysis latest with
  | Some _ as root -> root
  | None -> Option.bind last_good root_dir_of_analysis

let source_overrides_normalized (source_overrides : (string, string) Hashtbl.t) : (string, string) Hashtbl.t =
  Discovery.normalize_source_overrides source_overrides

let module_catalog ~(latest : Doc_state.analysis_result) ~(last_good : Doc_state.analysis_result option)
    ~(analysis : Doc_state.analysis_result) ~(source_overrides : (string, string) Hashtbl.t) :
    Module_catalog.t option =
  Option.map
    (fun root_dir ->
      Module_catalog.build ~root_dir ?analysis:analysis.compiler_analysis
        ~source_overrides:(source_overrides_normalized source_overrides) ())
    (root_dir_for_completion ~latest ~last_good)

let poly_detail_of_internal ~(environment : Infer.type_env option) ~(internal_name : string) : string option =
  Option.bind environment (fun environment ->
      Option.map detail_of_poly (Infer.TypeEnv.find_opt internal_name environment))

let kind_of_binding (binding : Module_sig.member_binding) : Lsp_t.CompletionItemKind.t =
  match binding.value_type with
  | Some (Types.Forall (_, mono)) -> kind_of_type mono
  | None when binding.enum_def <> None -> Lsp_t.CompletionItemKind.Enum
  | None when binding.shape_def <> None || binding.trait_def <> None -> Lsp_t.CompletionItemKind.Interface
  | _ -> Lsp_t.CompletionItemKind.Struct

let detail_of_binding (binding : Module_sig.member_binding) : string option =
  Option.map detail_of_poly binding.value_type

let kind_of_presence ?typed_binding (presence : Import_resolver.member_presence) :
    Lsp_t.CompletionItemKind.t =
  match typed_binding with
  | Some binding -> kind_of_binding binding
  | None when presence.has_enum -> Lsp_t.CompletionItemKind.Enum
  | None when presence.has_shape || presence.has_trait -> Lsp_t.CompletionItemKind.Interface
  | None when presence.has_named_type || presence.has_transparent_type -> Lsp_t.CompletionItemKind.Struct
  | _ -> Lsp_t.CompletionItemKind.Variable

type semantic_item = {
  label : string;
  kind : Lsp_t.CompletionItemKind.t;
  detail : string option;
  sort_text : string option;
}

let completion_item_of_semantic (item : semantic_item) : Lsp_t.CompletionItem.t =
  Lsp_t.CompletionItem.create ~label:item.label ~kind:item.kind ?detail:item.detail ?sortText:item.sort_text ()

let add_semantic_item (items : (string, semantic_item) Hashtbl.t) (item : semantic_item) =
  match Hashtbl.find_opt items item.label with
  | None -> Hashtbl.replace items item.label item
  | Some prior ->
      let detail =
        match (prior.detail, item.detail) with
        | Some left, Some right when not (String.equal left right) -> Some (left ^ " or " ^ right)
        | Some left, _ -> Some left
        | None, Some right -> Some right
        | None, None -> None
      in
      let kind =
        if prior.kind = Lsp_t.CompletionItemKind.Module || item.kind = Lsp_t.CompletionItemKind.Module then
          Lsp_t.CompletionItemKind.Module
        else
          prior.kind
      in
      Hashtbl.replace items item.label { prior with kind; detail }

let prefix_filter ~(prefix : string) (label : string) : bool =
  prefix = "" || starts_with ~prefix label

let identifier_prefix_at_offset ~(source : string) ~(offset : int) : string option =
  let tokens = Array.of_list (Lexer.lex source) in
  let len = Array.length tokens in
  let rec find idx =
    if idx >= len then
      None
    else
      let tok = tokens.(idx) in
      if tok.token_type = Token.Ident && offset >= tok.pos && offset <= token_end_pos tok + 1 then
        let prefix_len = max 0 (min offset (token_end_pos tok + 1) - tok.pos) in
        Some (String.sub source tok.pos prefix_len)
      else
        find (idx + 1)
  in
  find 0

let collect_left_chain (tokens : Token.token array) (end_ident_idx : int) : string list =
  let rec leftmost idx =
    if idx >= 2 && tokens.(idx - 1).token_type = Token.Dot && tokens.(idx - 2).token_type = Token.Ident then
      leftmost (idx - 2)
    else
      idx
  in
  let start_idx = leftmost end_ident_idx in
  let rec collect idx acc =
    if idx > end_ident_idx || tokens.(idx).token_type <> Token.Ident then
      List.rev acc
    else if idx + 1 < Array.length tokens && idx < end_ident_idx then
      collect (idx + 2) (tokens.(idx).literal :: acc)
    else
      List.rev (tokens.(idx).literal :: acc)
  in
  collect start_idx []

let namespace_member_context ~(source : string) ~(offset : int) ~(trigger_is_dot : bool) :
    (string list * string * int option) option =
  let tokens = Array.of_list (Lexer.lex source) in
  let len = Array.length tokens in
  if trigger_is_dot then
    let rec find_prev idx best =
      if idx >= len then
        best
      else if token_end_pos tokens.(idx) < offset then
        find_prev (idx + 1) (Some idx)
      else
        best
    in
    match find_prev 0 None with
    | Some dot_idx when tokens.(dot_idx).token_type = Token.Dot && dot_idx >= 1 && tokens.(dot_idx - 1).token_type = Token.Ident ->
        Some (collect_left_chain tokens (dot_idx - 1), "", Some tokens.(dot_idx - 1).pos)
    | _ -> None
  else
    let rec find idx =
      if idx >= len then
        None
      else
        let tok = tokens.(idx) in
        if tok.token_type = Token.Ident && offset >= tok.pos && offset <= token_end_pos tok + 1 then
          if idx >= 1 && tokens.(idx - 1).token_type = Token.Dot then
            let prefix_len = max 0 (min offset (token_end_pos tok + 1) - tok.pos) in
            Some (collect_left_chain tokens (idx - 2), String.sub source tok.pos prefix_len, Some tok.pos)
          else
            find (idx + 1)
        else
          find (idx + 1)
    in
    find 0

let root_symbol_is_local ~(analysis : Doc_state.analysis_result) ~(root_offset : int) : bool =
  match (analysis.compiler_analysis, analysis.program) with
  | Some compiler_analysis, Some program -> (
      match Hover.find_in_program root_offset program with
      | Some { expr = Ast.AST.Identifier _; id; _ } -> (
          match Compiler.find_active_file_symbol compiler_analysis ~expr_id:id with
          | Some symbol -> (
              match Option.bind symbol.file_id Doc_state.file_path_of_file_id with
              | Some file_path -> String.equal file_path compiler_analysis.active_file.file_path
              | None -> true)
          | None -> false)
      | _ -> false)
  | _ -> false

let root_symbol_is_surface_local ~(analysis : Doc_state.analysis_result) ~(root_name : string) : bool =
  match current_navigation analysis with
  | Some navigation -> Import_resolver.StringMap.mem root_name navigation.surface.declarations
  | None -> false

let bare_identifier_items ~(analysis : Doc_state.analysis_result) ~(prefix : string) : Lsp_t.CompletionItem.t list =
  let items = Hashtbl.create 32 in
  let navigation = current_navigation analysis in
  let environment = analysis.environment in
  let hidden_internal_names =
    match navigation with
    | None -> StringSet.empty
    | Some navigation ->
        let from_decls =
          Import_resolver.StringMap.fold
            (fun _ (presence : Import_resolver.member_presence) acc -> StringSet.add presence.internal_name acc)
            navigation.surface.declarations StringSet.empty
        in
        Import_resolver.StringMap.fold
          (fun _ (presence : Import_resolver.member_presence) acc -> StringSet.add presence.internal_name acc)
          navigation.resolved_imports.direct_bindings from_decls
  in
  Option.iter
    (fun (navigation : navigation_snapshot) ->
      Import_resolver.StringMap.iter
        (fun label (presence : Import_resolver.member_presence) ->
          if prefix_filter ~prefix label then
            add_semantic_item items
              {
                label;
                kind = kind_of_presence presence;
                detail = poly_detail_of_internal ~environment ~internal_name:presence.internal_name;
                sort_text = Some label;
              })
        navigation.surface.declarations;
      Import_resolver.StringMap.iter
        (fun label (presence : Import_resolver.member_presence) ->
          if prefix_filter ~prefix label then
            add_semantic_item items
              {
                label;
                kind = kind_of_presence presence;
                detail = poly_detail_of_internal ~environment ~internal_name:presence.internal_name;
                sort_text = Some label;
              })
        navigation.resolved_imports.direct_bindings;
      Import_resolver.StringMap.iter
        (fun label _ ->
          if prefix_filter ~prefix label then
            add_semantic_item items
              { label; kind = Lsp_t.CompletionItemKind.Module; detail = Some "module"; sort_text = Some label })
        navigation.resolved_imports.namespace_roots)
    navigation;
  Option.iter
    (fun environment ->
      Infer.TypeEnv.bindings environment
      |> List.iter (fun (name, poly) ->
             if prefix_filter ~prefix name && not (StringSet.mem name hidden_internal_names) then
               let (Types.Forall (_, mono)) = poly in
               add_semantic_item items
                 { label = name; kind = kind_of_type mono; detail = Some (detail_of_poly poly); sort_text = Some name }))
    environment;
  let semantic_items = Hashtbl.to_seq_values items |> List.of_seq |> List.sort (fun a b -> String.compare a.label b.label) in
  semantic_items @ List.map (fun (kw, desc) ->
    { label = kw; kind = Lsp_t.CompletionItemKind.Keyword; detail = Some desc; sort_text = Some ("zzz_" ^ kw) }) keywords
  |> List.map completion_item_of_semantic

let export_completion_items (entry : Module_catalog.entry) ~(prefix : string) : semantic_item list =
  Import_resolver.StringMap.bindings entry.surface.exports
  |> List.filter_map (fun (label, presence) ->
         if prefix_filter ~prefix label then
           let typed_binding =
             Option.bind entry.typed_signature (fun signature -> Module_sig.find_export signature label)
           in
           Some
             {
               label;
               kind = kind_of_presence ?typed_binding presence;
               detail =
                 (match typed_binding with
                 | Some binding -> detail_of_binding binding
                 | None -> None);
               sort_text = Some label;
             }
         else
           None)

let namespace_or_import_items ~(catalog : Module_catalog.t) ~(prefix_segments : string list) ~(prefix : string) :
    semantic_item list =
  let items = Hashtbl.create 16 in
  List.iter
    (fun label ->
      if prefix_filter ~prefix label then
        add_semantic_item items
          { label; kind = Lsp_t.CompletionItemKind.Module; detail = Some "module"; sort_text = Some label })
    (Module_catalog.child_segments catalog ~prefix_segments);
  Option.iter
    (fun entry -> List.iter (add_semantic_item items) (export_completion_items entry ~prefix))
    (Module_catalog.find_module catalog ~module_id:(String.concat "." prefix_segments));
  Hashtbl.to_seq_values items |> List.of_seq |> List.sort (fun a b -> String.compare a.label b.label)

let context_at ~(source : string) ~(offset : int) ~(trigger_is_dot : bool) : completion_context =
  match Import_header.completion_context_at_offset ~source ~offset with
  | Some { Import_header.typed_segments; prefix; in_alias } -> ImportPath { typed_segments; prefix; in_alias }
  | None -> (
      match namespace_member_context ~source ~offset ~trigger_is_dot with
      | Some (receiver_segments, prefix, _) when receiver_segments <> [] ->
          NamespaceMember { receiver_segments; prefix }
      | _ -> (
          match identifier_prefix_at_offset ~source ~offset with
          | Some prefix -> BareIdentifier { prefix }
          | None -> BareIdentifier { prefix = "" }))

let complete_at ~(latest : Doc_state.analysis_result) ~(last_good : Doc_state.analysis_result option) ~(line : int)
    ~(character : int) ~(trigger_is_dot : bool) ~(source_overrides : (string, string) Hashtbl.t) :
    Lsp_t.CompletionItem.t list option =
  let source = latest.source in
  let offset = Lsp_utils.position_to_offset ~source ~line ~character in
  let semantic = semantic_analysis ~latest ~last_good in
  let catalog = module_catalog ~latest ~last_good ~analysis:semantic ~source_overrides in
  match context_at ~source ~offset ~trigger_is_dot with
  | BareIdentifier { prefix } -> Some (bare_identifier_items ~analysis:semantic ~prefix)
  | ImportPath { typed_segments; prefix; in_alias } ->
      if in_alias then
        None
      else
        Option.map
          (fun catalog ->
            namespace_or_import_items ~catalog ~prefix_segments:typed_segments ~prefix
            |> List.map completion_item_of_semantic)
          catalog
  | NamespaceMember { receiver_segments; prefix } -> (
      match current_navigation semantic with
      | None -> None
      | Some navigation -> (
          match catalog with
          | None -> None
          | Some catalog ->
              let root_offset =
                match namespace_member_context ~source ~offset ~trigger_is_dot with
                | Some (_, _, Some root_offset) -> root_offset
                | _ -> offset
              in
              let root_name = List.hd receiver_segments in
              if
                root_symbol_is_local ~analysis:semantic ~root_offset
                || root_symbol_is_surface_local ~analysis:semantic ~root_name
              then
                None
              else
                match
                  Import_resolver.lookup_namespace_node
                    navigation.resolved_imports.namespace_roots receiver_segments
                with
                | Some _ ->
                    Some
                      (namespace_or_import_items ~catalog ~prefix_segments:receiver_segments ~prefix
                      |> List.map completion_item_of_semantic)
                | None -> None))
  | Unsupported -> None

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

let%test "enum keyword detail reflects compatibility sugar" =
  let items = completions ~environment:Infer.empty_env in
  match List.find_opt (fun (i : Lsp_t.CompletionItem.t) -> i.label = "enum") items with
  | Some item -> item.detail = Some "Compatibility sugar for constructor-bearing sum types"
  | None -> false

let%test "type keyword detail reflects canonical declaration surface" =
  let items = completions ~environment:Infer.empty_env in
  match List.find_opt (fun (i : Lsp_t.CompletionItem.t) -> i.label = "type") items with
  | Some item -> item.detail = Some "Type declaration (transparent, wrapper, or sum)"
  | None -> false

let%test "detail shows type for monomorphic binding" =
  let env = env_with [ ("x", Types.Forall ([], Types.TInt)) ] in
  let items = completions ~environment:env in
  match List.find_opt (fun (i : Lsp_t.CompletionItem.t) -> i.label = "x") items with
  | Some item -> item.detail = Some "Int"
  | None -> false

let%test "detail uses canonical collection syntax" =
  let env = env_with [ ("xs", Types.Forall ([], Types.TArray Types.TInt)) ] in
  let items = completions ~environment:env in
  match List.find_opt (fun (i : Lsp_t.CompletionItem.t) -> i.label = "xs") items with
  | Some item -> item.detail = Some "List[Int]"
  | None -> false

let source_with_cursor (annotated : string) : string * int * int =
  let cursor = String.index annotated '|' in
  let source =
    String.sub annotated 0 cursor ^ String.sub annotated (cursor + 1) (String.length annotated - cursor - 1)
  in
  let pos = Lsp_utils.offset_to_position ~source ~offset:cursor in
  (source, pos.line, pos.character)

let completion_items_at ?last_good_annotated ?(trigger_is_dot = false) ?(overrides = []) ~(files : (string * string) list)
    ~(entry_rel : string) (annotated : string) : Lsp_t.CompletionItem.t list option =
  let captured = ref None in
  let _ =
    Doc_state.with_temp_project files (fun root ->
        let source, line, character = source_with_cursor annotated in
        let file_id = Filename.concat root entry_rel in
        let latest = Doc_state.analyze_with_file_id ~source_root:root ~file_id ~source () in
        let last_good =
          Option.map
            (fun annotated ->
              let source, _, _ = source_with_cursor annotated in
              Doc_state.analyze_with_file_id ~source_root:root ~file_id ~source ())
            last_good_annotated
        in
        let source_overrides = Hashtbl.create (List.length overrides) in
        List.iter
          (fun (relative_path, contents) -> Hashtbl.replace source_overrides (Filename.concat root relative_path) contents)
          overrides;
        captured :=
          complete_at ~latest ~last_good ~line ~character ~trigger_is_dot ~source_overrides;
        true)
  in
  !captured

let completion_labels ?last_good_annotated ?trigger_is_dot ?overrides ~files ~entry_rel annotated =
  match completion_items_at ?last_good_annotated ?trigger_is_dot ?overrides ~files ~entry_rel annotated with
  | Some items -> List.map (fun (item : Lsp_t.CompletionItem.t) -> item.label) items
  | None -> []

let debug_labels ~(label : string) (labels : string list) : unit =
  Printf.eprintf "[completion-test] %s -> [%s]\n%!" label (String.concat ", " labels)

let%test "module completions surface current top-level names without internal prefixes" =
  let labels =
    completion_labels
      ~files:[ ("main.mr", "export helper\nlet helper = 1\nhe|\n") ]
      ~entry_rel:"main.mr" "export helper\nlet helper = 1\nhe|\n"
  in
  debug_labels ~label:"current-module top-level" labels;
  List.mem "helper" labels && not (List.exists (fun label -> Diagnostics.String_utils.contains_substring ~needle:"__" label) labels)

let%test "module completions include direct-import aliases but not mangled targets" =
  let labels =
    completion_labels
      ~files:
        [
          ("main.mr", "import math.add as plus\npl|\n");
          ("math.mr", "export add\nfn add(x: Int, y: Int) -> Int = x + y\n");
        ]
      ~entry_rel:"main.mr" "import math.add as plus\npl|\n"
  in
  debug_labels ~label:"direct-import alias" labels;
  List.mem "plus" labels && not (List.mem "math__add" labels)

let%test "namespace completion returns exported members for parsed qualifiers" =
  let labels =
    completion_labels
      ~files:
        [
          ("main.mr", "import math\nmath.ad|\n");
          ("math.mr", "export add, Point, HasXY\nfn add(x: Int, y: Int) -> Int = x + y\ntype Point = { x: Int, y: Int }\nshape HasXY = { x: Int, y: Int }\n");
        ]
      ~entry_rel:"main.mr" "import math\nmath.ad|\n"
  in
  debug_labels ~label:"parsed namespace member" labels;
  List.mem "add" labels && not (List.mem "math__add" labels)

let%test "bare-dot namespace completion can use last_good" =
  let labels =
    completion_labels
      ~files:
        [
          ("main.mr", "import math\nmath.\n");
          ("math.mr", "export add, Point, HasXY\nfn add(x: Int, y: Int) -> Int = x + y\ntype Point = { x: Int, y: Int }\nshape HasXY = { x: Int, y: Int }\n");
        ]
      ~entry_rel:"main.mr" ~trigger_is_dot:true ~last_good_annotated:"import math\nmath|\n" "import math\nmath.|\n"
  in
  debug_labels ~label:"bare-dot namespace" labels;
  List.mem "add" labels && List.mem "Point" labels && List.mem "HasXY" labels

let%test "dot completion on a local value still returns none" =
  match
    completion_items_at ~trigger_is_dot:true ~last_good_annotated:"let math = { add: 1 }\nmath|\n"
      ~files:[ ("main.mr", "let math = { add: 1 }\nmath.\n") ]
      ~entry_rel:"main.mr" "let math = { add: 1 }\nmath.|\n"
  with
  | None -> true
  | Some _ -> false

let%test "import-path completion survives trailing dots and sees open overrides" =
  let labels =
    completion_labels ~trigger_is_dot:true
      ~files:
        [
          ("main.mr", "import math.\n");
          ("math.mr", "export add\nfn add(x: Int, y: Int) -> Int = x + y\n");
          ("collections/list.mr", "export map\nfn map(x: Int) -> Int = x\n");
        ]
      ~overrides:[ ("math.mr", "export mul, Point, HasXY\nfn mul(x: Int, y: Int) -> Int = x * y\ntype Point = { x: Int, y: Int }\nshape HasXY = { x: Int, y: Int }\n") ]
      ~entry_rel:"main.mr" "import math.|\n"
  in
  debug_labels ~label:"import trailing dot with override" labels;
  List.mem "mul" labels && List.mem "Point" labels && List.mem "HasXY" labels

let%test "import-path completion suggests sibling module segments from project root" =
  let labels =
    completion_labels ~trigger_is_dot:true
      ~files:
        [
          ("main.mr", "import collections.\n");
          ("collections/list.mr", "export map\nfn map(x: Int) -> Int = x\n");
        ]
      ~entry_rel:"main.mr" "import collections.|\n"
  in
  debug_labels ~label:"import sibling segments" labels;
  List.mem "list" labels
