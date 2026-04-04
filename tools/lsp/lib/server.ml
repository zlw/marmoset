(* LSP server: class + entry point *)

module Lsp_t = Linol_lsp.Types
module Diagnostic = Marmoset.Lib.Diagnostic

(* Per-document cached analysis *)
type cached_doc = {
  latest : Doc_state.analysis_result;
  last_good : Doc_state.analysis_result option;
}

type open_doc = {
  text : string;
  file_path : string option;
}

(* Per-document pending debounce state *)
type pending = { mutable task : unit Lwt.t option }

(* Debounce delay in seconds *)
let debounce_delay = 0.3

let analysis_has_typed_state (analysis : Doc_state.analysis_result) : bool =
  analysis.program <> None && analysis.type_map <> None && analysis.environment <> None

let update_cached_doc (previous : cached_doc option) (latest : Doc_state.analysis_result) : cached_doc =
  {
    latest;
    last_good =
      (if analysis_has_typed_state latest then
         Some latest
       else
         Option.bind previous (fun cached -> cached.last_good));
  }

let starts_with ~(prefix : string) (s : string) : bool =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len && String.sub s 0 prefix_len = prefix

let path_is_within_root ~(root : string) (path : string) : bool =
  String.equal root path || starts_with ~prefix:(root ^ Filename.dir_sep) path

let cached_project_root (cached : cached_doc) : string option =
  match cached.latest.project_root with
  | Some _ as root -> root
  | None -> Option.bind cached.last_good (fun analysis -> analysis.project_root)

let choose_known_source_root
    ~(analysis_cache : (Lsp_t.DocumentUri.t, cached_doc) Hashtbl.t)
    ~(uri : Lsp_t.DocumentUri.t)
    ~(file_path : string option) : string option =
  match Option.bind (Hashtbl.find_opt analysis_cache uri) cached_project_root with
  | Some _ as root -> root
  | None -> (
      match file_path with
      | None -> None
      | Some file_path ->
          Hashtbl.fold
            (fun _ cached best ->
              match cached_project_root cached with
              | Some root when path_is_within_root ~root file_path -> (
                  match best with
                  | Some current when String.length current >= String.length root -> best
                  | _ -> Some root)
              | _ -> best)
            analysis_cache None)

let related_open_docs
    ~(open_docs : (Lsp_t.DocumentUri.t, open_doc) Hashtbl.t)
    ~(project_root : string)
    ~(exclude_uri : Lsp_t.DocumentUri.t option) : (Lsp_t.DocumentUri.t * string) list =
  Hashtbl.fold
    (fun uri (doc : open_doc) acc ->
      let should_skip =
        match exclude_uri with
        | Some exclude_uri -> Lsp_t.DocumentUri.equal uri exclude_uri
        | None -> false
      in
      if should_skip then
        acc
      else
        match doc.file_path with
        | Some file_path when path_is_within_root ~root:project_root file_path -> (uri, doc.text) :: acc
        | _ -> acc)
    open_docs []

class marmoset_server =
  object (self)
    inherit Linol_lwt.Jsonrpc2.server as super
    val open_docs : (Lsp_t.DocumentUri.t, open_doc) Hashtbl.t = Hashtbl.create 16
    val analysis_cache : (Lsp_t.DocumentUri.t, cached_doc) Hashtbl.t = Hashtbl.create 16
    val pending_analyses : (Lsp_t.DocumentUri.t, pending) Hashtbl.t = Hashtbl.create 16
    method spawn_query_handler f = Linol_lwt.spawn f

    (* Advertise capabilities *)
    method! config_hover = Some (`Bool true)
    method! config_symbol = Some (`Bool true)
    method! config_inlay_hints = Some (`Bool true)
    method! config_definition = Some (`Bool true)
    method! config_completion = Some (Lsp_t.CompletionOptions.create ~triggerCharacters:[ "." ] ())

    method! config_modify_capabilities c =
      {
        c with
        semanticTokensProvider =
          Some
            (`SemanticTokensOptions
               (Lsp_t.SemanticTokensOptions.create ~full:(`Bool true)
                  ~legend:
                    (Lsp_t.SemanticTokensLegend.create ~tokenTypes:Semantic_tokens.token_types
                       ~tokenModifiers:Semantic_tokens.token_modifiers)
                  ()));
        foldingRangeProvider = Some (`Bool true);
        selectionRangeProvider = Some (`Bool true);
        signatureHelpProvider =
          Some (Lsp_t.SignatureHelpOptions.create ~triggerCharacters:[ "("; "," ] ~retriggerCharacters:[ ")" ] ());
        codeActionProvider =
          Some
            (`CodeActionOptions
               (Lsp_t.CodeActionOptions.create ~codeActionKinds:[ Lsp_t.CodeActionKind.QuickFix ] ()));
      }

    method! config_sync_opts =
      Lsp_t.TextDocumentSyncOptions.create ~change:Lsp_t.TextDocumentSyncKind.Full ~openClose:true
        ~save:(`SaveOptions (Lsp_t.SaveOptions.create ~includeText:false ()))
        ~willSave:false ()

    method private source_overrides () : (string, string) Hashtbl.t =
      let overrides = Hashtbl.create (Hashtbl.length open_docs) in
      Hashtbl.iter
        (fun _ (doc : open_doc) ->
          match doc.file_path with
          | Some file_path -> Hashtbl.replace overrides file_path doc.text
          | None -> ())
        open_docs;
      overrides

    method private publish_diagnostics
        ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
        ~(uri : Lsp_t.DocumentUri.t)
        (diagnostics : Lsp_t.Diagnostic.t list) =
      let open Lwt.Syntax in
      let prior_uri = notify_back#get_uri in
      notify_back#set_uri uri;
      let* () = notify_back#send_diagnostic diagnostics in
      (match prior_uri with
      | Some prior_uri -> notify_back#set_uri prior_uri
      | None -> ());
      Lwt.return_unit

    method private refresh_related_docs
        ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
        ~(project_root : string)
        ~(exclude_uri : Lsp_t.DocumentUri.t option) =
      let related = related_open_docs ~open_docs ~project_root ~exclude_uri in
      Lwt_list.iter_s
        (fun (uri, content) -> self#analyze_and_notify ~notify_back ~refresh_related:false uri content)
        related

    (* Run analysis and push diagnostics — called after debounce or immediately on open *)
    method private analyze_and_notify
        ~(notify_back : Linol_lwt.Jsonrpc2.notify_back)
        ?(refresh_related = true)
        (uri : Lsp_t.DocumentUri.t)
        (content : string) =
      let source_root =
        match Hashtbl.find_opt open_docs uri with
        | Some doc -> choose_known_source_root ~analysis_cache ~uri ~file_path:doc.file_path
        | None ->
            choose_known_source_root ~analysis_cache ~uri
              ~file_path:(Doc_state.file_path_of_file_id (Lsp_t.DocumentUri.to_string uri))
      in
      let source_overrides = self#source_overrides () in
      let analysis, diagnostics =
        try
          let file_id = Lsp_t.DocumentUri.to_string uri in
          let a = Doc_state.analyze_with_file_id ?source_root ~source_overrides ~file_id ~source:content () in
          (Some a, a.diagnostics)
        with exn ->
          let bt = Printexc.get_backtrace () in
          Printf.eprintf "[marmoset-lsp] internal error: %s\n%s%!" (Printexc.to_string exn) bt;
          let file_id = Lsp_t.DocumentUri.to_string uri in
          let diag = Diagnostic.error_no_span ~code:"lsp-internal" ~message:"Internal error during analysis" in
          let lsp_diag = Doc_state.lsp_diagnostic_of_canonical ~source:content ~active_file_id:file_id diag in
          (None, [ lsp_diag ])
      in
      let open Lwt.Syntax in
      (match analysis with
      | Some a ->
          let previous = Hashtbl.find_opt analysis_cache uri in
          Hashtbl.replace analysis_cache uri (update_cached_doc previous a)
      | None -> ());
      let* () = self#publish_diagnostics ~notify_back ~uri diagnostics in
      match (refresh_related, analysis) with
      | true, Some a -> (
          match a.project_root with
          | Some project_root -> self#refresh_related_docs ~notify_back ~project_root ~exclude_uri:(Some uri)
          | None -> Lwt.return_unit)
      | _ -> Lwt.return_unit

    (* Cancel any pending analysis for a URI *)
    method private cancel_pending (uri : Lsp_t.DocumentUri.t) =
      match Hashtbl.find_opt pending_analyses uri with
      | Some p -> (
          match p.task with
          | Some t ->
              Lwt.cancel t;
              p.task <- None
          | None -> ())
      | None -> ()

    (* Schedule analysis after debounce delay, cancelling any previous pending *)
    method private debounce_analyze
        ~(notify_back : Linol_lwt.Jsonrpc2.notify_back) (uri : Lsp_t.DocumentUri.t) (content : string) =
      self#cancel_pending uri;
      let p =
        match Hashtbl.find_opt pending_analyses uri with
        | Some p -> p
        | None ->
            let p = { task = None } in
            Hashtbl.replace pending_analyses uri p;
            p
      in
      let task =
        Lwt.catch
          (fun () ->
            let open Lwt.Syntax in
            let* () = Lwt_unix.sleep debounce_delay in
            p.task <- None;
            self#analyze_and_notify ~notify_back uri content)
          (function
            | Lwt.Canceled -> Lwt.return ()
            | exn -> Lwt.fail exn)
      in
      p.task <- Some task;
      task

    method on_notif_doc_did_open ~notify_back (doc : Lsp_t.TextDocumentItem.t) ~content =
      (* Analyze immediately on open — no debounce *)
      Hashtbl.replace open_docs doc.uri
        { text = content; file_path = Doc_state.file_path_of_file_id (Lsp_t.DocumentUri.to_string doc.uri) };
      self#analyze_and_notify ~notify_back doc.uri content

    method on_notif_doc_did_change
        ~notify_back (doc : Lsp_t.VersionedTextDocumentIdentifier.t) _changes ~old_content:_ ~new_content =
      (* Debounce on change *)
      Hashtbl.replace open_docs doc.uri
        { text = new_content; file_path = Doc_state.file_path_of_file_id (Lsp_t.DocumentUri.to_string doc.uri) };
      self#debounce_analyze ~notify_back doc.uri new_content

    method on_notif_doc_did_close ~notify_back (doc : Lsp_t.TextDocumentIdentifier.t) =
      let closed_root =
        match Hashtbl.find_opt analysis_cache doc.uri with
        | Some cached -> cached_project_root cached
        | None -> None
      in
      self#cancel_pending doc.uri;
      Hashtbl.remove open_docs doc.uri;
      Hashtbl.remove pending_analyses doc.uri;
      Hashtbl.remove analysis_cache doc.uri;
      (* Clear diagnostics for the closed document *)
      let open Lwt.Syntax in
      let* () = self#publish_diagnostics ~notify_back ~uri:doc.uri [] in
      match closed_root with
      | Some project_root -> self#refresh_related_docs ~notify_back ~project_root ~exclude_uri:None
      | None -> Lwt.return_unit

    method! on_req_hover ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_ (_doc_state : Linol_lwt.doc_state) =
      let result =
        match Hashtbl.find_opt analysis_cache uri with
        | None ->
            Printf.eprintf "[marmoset-lsp] hover %s:%d:%d — no analysis cached\n%!"
              (Lsp_t.DocumentUri.to_string uri) pos.line pos.character;
            None
        | Some { latest = analysis; _ } -> (
            match (analysis.program, analysis.type_map, analysis.environment) with
            | Some prog, Some tm, Some env ->
                let r =
                  Hover.hover_at ~source:analysis.source ~program:prog ~type_map:tm ~environment:env
                    ~type_var_user_names:analysis.type_var_user_names ~line:pos.line ~character:pos.character
                in
                (match r with
                | Some hover ->
                    let text =
                      match hover.contents with
                      | `MarkupContent mc -> mc.value
                      | `MarkedString ms -> ms.value
                      | `List items ->
                          String.concat "\n"
                            (List.map
                               (function
                                 | { Lsp_t.MarkedString.value; _ } -> value)
                               items)
                    in
                    Printf.eprintf "[marmoset-lsp] hover %d:%d → %s\n%!" pos.line pos.character
                      (String.escaped text)
                | None -> Printf.eprintf "[marmoset-lsp] hover %d:%d → None\n%!" pos.line pos.character);
                r
            | _ ->
                Printf.eprintf "[marmoset-lsp] hover %d:%d — no type_map (parse/type error?)\n%!" pos.line
                  pos.character;
                None)
      in
      Lwt.return result

    method! on_req_symbol ~notify_back:_ ~id:_ ~uri ~workDoneToken:_ ~partialResultToken:_ () =
      let result =
        match Hashtbl.find_opt analysis_cache uri with
        | None -> None
        | Some { latest = analysis; _ } -> (
            match analysis.program with
            | Some prog ->
                let symbols = Doc_symbols.document_symbols ~source:analysis.source ~program:prog in
                Some (`DocumentSymbol symbols)
            | None -> None)
      in
      Lwt.return result

    method! on_req_inlay_hint ~notify_back:_ ~id:_ ~uri ~range () =
      let result =
        match Hashtbl.find_opt analysis_cache uri with
        | None -> None
        | Some { latest = analysis; _ } -> (
            match (analysis.program, analysis.type_map) with
            | Some prog, Some tm ->
                Some (Inlay_hints.inlay_hints ~source:analysis.source ~program:prog ~type_map:tm ~range)
            | _ -> None)
      in
      Lwt.return result

    method! on_req_code_action ~notify_back:_ ~id:_ (p : Lsp_t.CodeActionParams.t) =
      let uri = p.textDocument.uri in
      let result =
        match Hashtbl.find_opt analysis_cache uri with
        | None -> None
        | Some { latest = analysis; _ } -> (
            match (analysis.program, analysis.type_map) with
            | Some prog, Some tm ->
                let actions =
                  Code_actions.compute ~source:analysis.source ~uri ~program:prog ~type_map:tm
                    ~environment:analysis.environment ~range:p.range
                in
                Some (List.map (fun a -> `CodeAction a) actions)
            | _ -> None)
      in
      Lwt.return result

    method! on_req_completion
        ~notify_back:_
        ~id:_
        ~uri
        ~pos:_
        ~ctx
        ~workDoneToken:_
        ~partialResultToken:_
        (_doc_state : Linol_lwt.doc_state) =
      (* Don't return global completions when trigger char is "." — that should
         be method/field completions which we don't yet support. *)
      let is_dot_trigger =
        match ctx with
        | Some ctx -> (
            match ctx.triggerCharacter with
            | Some "." -> true
            | _ -> false)
        | None -> false
      in
      if is_dot_trigger then
        Lwt.return None
      else
        let env =
          match Hashtbl.find_opt analysis_cache uri with
          | Some { latest = analysis; _ } -> analysis.environment
          | None -> None
        in
        let environment =
          match env with
          | Some e -> e
          | None -> Marmoset.Lib.Infer.empty_env
        in
        let items = Completions.completions ~environment in
        Lwt.return (Some (`List items))

    method! on_req_definition
        ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_ ~partialResultToken:_ (_doc_state : Linol_lwt.doc_state) =
      let result =
        match Hashtbl.find_opt analysis_cache uri with
        | None -> None
        | Some { latest = analysis; _ } ->
            Definition.locations ~analysis
              (Definition.find_definition ~analysis ~line:pos.line ~character:pos.character)
      in
      Lwt.return result

    method! on_request_unhandled : type r. notify_back:_ -> id:_ -> r Linol_lsp.Client_request.t -> r Lwt.t =
      fun ~notify_back ~id req ->
        match req with
        | Linol_lsp.Client_request.SemanticTokensFull p ->
            let uri = p.textDocument.uri in
            let result =
              match Hashtbl.find_opt analysis_cache uri with
              | None -> None
              | Some { latest = analysis; _ } -> (
                  match (analysis.program, analysis.type_map, analysis.environment) with
                  | Some prog, Some tm, Some env ->
                      Semantic_tokens.compute ~source:analysis.source ~program:prog ~type_map:tm ~environment:env
                  | _ -> None)
            in
            Lwt.return result
        | Linol_lsp.Client_request.TextDocumentFoldingRange p ->
            let uri = p.textDocument.uri in
            let result =
              match Hashtbl.find_opt analysis_cache uri with
              | None -> None
              | Some { latest = analysis; _ } -> (
                  match analysis.program with
                  | Some prog -> Some (Folding_ranges.compute ~source:analysis.source ~program:prog)
                  | None -> None)
            in
            Lwt.return result
        | Linol_lsp.Client_request.SelectionRange p ->
            let uri = p.textDocument.uri in
            let result =
              match Hashtbl.find_opt analysis_cache uri with
              | Some { latest = analysis; _ } -> (
                  match analysis.program with
                  | Some prog ->
                      Selection_ranges.compute ~source:analysis.source ~program:prog ~positions:p.positions
                  | None ->
                      (* No program — return a zero-range for each position (LSP requires one per position) *)
                      List.map
                        (fun (pos : Lsp_t.Position.t) ->
                          let range = Lsp_t.Range.create ~start:pos ~end_:pos in
                          Lsp_t.SelectionRange.create ~range ())
                        p.positions)
              | None ->
                  (* No cache — return a zero-range for each position *)
                  List.map
                    (fun (pos : Lsp_t.Position.t) ->
                      let range = Lsp_t.Range.create ~start:pos ~end_:pos in
                      Lsp_t.SelectionRange.create ~range ())
                    p.positions
            in
            Lwt.return result
        | Linol_lsp.Client_request.SignatureHelp p ->
            let uri = p.textDocument.uri in
            let empty = Lsp_t.SignatureHelp.create ~signatures:[] () in
            let result =
              match Hashtbl.find_opt analysis_cache uri with
              | None -> empty
              | Some { latest = analysis; _ } -> (
                  match (analysis.program, analysis.type_map, analysis.environment) with
                  | Some prog, Some tm, Some env -> (
                      match
                        Signature_help.signature_help ~source:analysis.source ~program:prog ~type_map:tm
                          ?compiler_analysis:analysis.compiler_analysis ~environment:env ~line:p.position.line
                          ~character:p.position.character ()
                      with
                      | Some sh -> sh
                      | None -> empty)
                  | _ -> empty)
            in
            Lwt.return result
        | _ -> super#on_request_unhandled ~notify_back ~id req
  end

let run () =
  let server = new marmoset_server in
  let task =
    let rpc = Linol_lwt.Jsonrpc2.create_stdio ~env:() (server :> Linol_lwt.Jsonrpc2.server) in
    let shutdown () = server#get_status = `ReceivedExit in
    Linol_lwt.Jsonrpc2.run ~shutdown rpc
  in
  Linol_lwt.run task

let fake_analysis ?module_id ?source_root ?project_root ?program ?type_map ?environment ?(diagnostics = []) () :
    Doc_state.analysis_result =
  {
    source = "";
    module_id;
    source_root;
    project_root;
    program;
    type_map;
    environment;
    type_var_user_names = [];
    diagnostics;
    compiler_analysis = None;
  }

let%test "update_cached_doc captures a last_good snapshot from typed analyses" =
  let typed =
    fake_analysis ~program:[] ~type_map:(Hashtbl.create 1) ~environment:Marmoset.Lib.Infer.empty_env ()
  in
  match update_cached_doc None typed with
  | { last_good = Some cached; _ } -> cached.program <> None
  | _ -> false

let%test "update_cached_doc preserves last_good across parse-broken latest edits" =
  let typed =
    fake_analysis ~program:[] ~type_map:(Hashtbl.create 1) ~environment:Marmoset.Lib.Infer.empty_env ()
  in
  let stale = update_cached_doc None typed in
  let latest = fake_analysis ~diagnostics:[] () in
  match update_cached_doc (Some stale) latest with
  | { latest = latest_analysis; last_good = Some cached } ->
      latest_analysis.program = None && cached.program <> None
  | _ -> false

let%test "related_open_docs selects sibling file-backed docs under the same project root" =
  let open_docs = Hashtbl.create 3 in
  let main_uri = Lsp_t.DocumentUri.of_path "/tmp/project/main.mr" in
  let math_uri = Lsp_t.DocumentUri.of_path "/tmp/project/math.mr" in
  let other_uri = Lsp_t.DocumentUri.of_path "/tmp/other/util.mr" in
  Hashtbl.replace open_docs main_uri { text = "main"; file_path = Some "/tmp/project/main.mr" };
  Hashtbl.replace open_docs math_uri { text = "math"; file_path = Some "/tmp/project/math.mr" };
  Hashtbl.replace open_docs other_uri { text = "other"; file_path = Some "/tmp/other/util.mr" };
  match related_open_docs ~open_docs ~project_root:"/tmp/project" ~exclude_uri:(Some main_uri) with
  | [ (uri, "math") ] -> Lsp_t.DocumentUri.equal uri math_uri
  | _ -> false
