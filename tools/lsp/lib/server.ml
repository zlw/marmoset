(* LSP server: class + entry point *)

module Lsp_t = Linol_lsp.Types
module Diagnostic = Marmoset.Lib.Diagnostic

(* Per-document cached analysis *)
type cached_doc = { analysis : Doc_state.analysis_result }

(* Per-document pending debounce state *)
type pending = { mutable task : unit Lwt.t option }

(* Debounce delay in seconds *)
let debounce_delay = 0.3

class marmoset_server =
  object (self)
    inherit Linol_lwt.Jsonrpc2.server as super
    val analysis_cache : (Lsp_t.DocumentUri.t, cached_doc) Hashtbl.t = Hashtbl.create 16
    val pending_analyses : (Lsp_t.DocumentUri.t, pending) Hashtbl.t = Hashtbl.create 16
    method spawn_query_handler f = Linol_lwt.spawn f

    (* Advertise capabilities *)
    method! config_hover = Some (`Bool true)
    method! config_symbol = Some (`Bool true)
    method! config_inlay_hints = Some (`Bool true)
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

    (* Run analysis and push diagnostics — called after debounce or immediately on open *)
    method private analyze_and_notify
        ~(notify_back : Linol_lwt.Jsonrpc2.notify_back) (uri : Lsp_t.DocumentUri.t) (content : string) =
      let analysis, diagnostics =
        try
          let file_id = Lsp_t.DocumentUri.to_string uri in
          let a = Doc_state.analyze_with_file_id ~file_id ~source:content in
          (Some a, a.diagnostics)
        with exn ->
          let bt = Printexc.get_backtrace () in
          Printf.eprintf "[marmoset-lsp] internal error: %s\n%s%!" (Printexc.to_string exn) bt;
          let file_id = Lsp_t.DocumentUri.to_string uri in
          let diag =
            Diagnostic.error_no_span ~code:"lsp-internal" ~message:"Internal error during analysis"
          in
          let lsp_diag = Doc_state.lsp_diagnostic_of_canonical ~source:content ~active_file_id:file_id diag in
          (None, [ lsp_diag ])
      in
      (match analysis with
      | Some a -> Hashtbl.replace analysis_cache uri { analysis = a }
      | None -> Hashtbl.remove analysis_cache uri);
      notify_back#send_diagnostic diagnostics

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
      self#analyze_and_notify ~notify_back doc.uri content

    method on_notif_doc_did_change
        ~notify_back (doc : Lsp_t.VersionedTextDocumentIdentifier.t) _changes ~old_content:_ ~new_content =
      (* Debounce on change *)
      self#debounce_analyze ~notify_back doc.uri new_content

    method on_notif_doc_did_close ~notify_back (doc : Lsp_t.TextDocumentIdentifier.t) =
      self#cancel_pending doc.uri;
      Hashtbl.remove pending_analyses doc.uri;
      Hashtbl.remove analysis_cache doc.uri;
      (* Clear diagnostics for the closed document *)
      notify_back#send_diagnostic []

    method! on_req_hover ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_ (_doc_state : Linol_lwt.doc_state) =
      let result =
        match Hashtbl.find_opt analysis_cache uri with
        | None ->
            Printf.eprintf "[marmoset-lsp] hover %s:%d:%d — no analysis cached\n%!"
              (Lsp_t.DocumentUri.to_string uri) pos.line pos.character;
            None
        | Some { analysis } -> (
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
                      | _ -> "?"
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
        | Some { analysis } -> (
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
        | Some { analysis } -> (
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
        | Some { analysis } -> (
            match (analysis.program, analysis.type_map) with
            | Some prog, Some tm ->
                let actions =
                  Code_actions.compute ~source:analysis.source ~uri ~program:prog ~type_map:tm ~range:p.range
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
          | Some { analysis } -> analysis.environment
          | None -> None
        in
        let environment =
          match env with
          | Some e -> e
          | None -> Marmoset.Lib.Infer.empty_env
        in
        let items = Completions.completions ~environment in
        Lwt.return (Some (`List items))

    method! on_request_unhandled : type r. notify_back:_ -> id:_ -> r Linol_lsp.Client_request.t -> r Lwt.t =
      fun ~notify_back ~id req ->
        match req with
        | Linol_lsp.Client_request.SemanticTokensFull p ->
            let uri = p.textDocument.uri in
            let result =
              match Hashtbl.find_opt analysis_cache uri with
              | None -> None
              | Some { analysis } -> (
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
              | Some { analysis } -> (
                  match analysis.program with
                  | Some prog -> Some (Folding_ranges.compute ~source:analysis.source ~program:prog)
                  | None -> None)
            in
            Lwt.return result
        | Linol_lsp.Client_request.SelectionRange p ->
            let uri = p.textDocument.uri in
            let result =
              match Hashtbl.find_opt analysis_cache uri with
              | Some { analysis } -> (
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
              | Some { analysis } -> (
                  match (analysis.program, analysis.type_map, analysis.environment) with
                  | Some prog, Some tm, Some env -> (
                      match
                        Signature_help.signature_help ~source:analysis.source ~program:prog ~type_map:tm
                          ~environment:env ~line:p.position.line ~character:p.position.character
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
