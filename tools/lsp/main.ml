(* Marmoset LSP server entry point *)

module Lsp_t = Lsp.Types

(* Per-document cached analysis *)
type cached_doc = { analysis : Marmoset_lsp.Doc_state.analysis_result }

(* Per-document pending debounce state *)
type pending = { mutable task : unit Lwt.t option }

(* Debounce delay in seconds *)
let debounce_delay = 0.3

class marmoset_server =
  object (self)
    inherit Linol_lwt.Jsonrpc2.server
    val analysis_cache : (Lsp_t.DocumentUri.t, cached_doc) Hashtbl.t = Hashtbl.create 16
    val pending_analyses : (Lsp_t.DocumentUri.t, pending) Hashtbl.t = Hashtbl.create 16
    method spawn_query_handler f = Linol_lwt.spawn f

    (* Advertise capabilities *)
    method! config_hover = Some (`Bool true)
    method! config_symbol = Some (`Bool true)
    method! config_inlay_hints = Some (`Bool true)
    method! config_completion = Some (Lsp_t.CompletionOptions.create ~triggerCharacters:[ "." ] ())

    method! config_sync_opts =
      Lsp_t.TextDocumentSyncOptions.create ~change:Lsp_t.TextDocumentSyncKind.Full ~openClose:true
        ~save:(`SaveOptions (Lsp_t.SaveOptions.create ~includeText:false ()))
        ~willSave:false ()

    (* Run analysis and push diagnostics — called after debounce or immediately on open *)
    method private analyze_and_notify
        ~(notify_back : Linol_lwt.Jsonrpc2.notify_back) (uri : Lsp_t.DocumentUri.t) (content : string) =
      let analysis = Marmoset_lsp.Doc_state.analyze ~source:content in
      Hashtbl.replace analysis_cache uri { analysis };
      notify_back#send_diagnostic analysis.diagnostics

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

    method on_notif_doc_did_close ~notify_back:_ (doc : Lsp_t.TextDocumentIdentifier.t) =
      self#cancel_pending doc.uri;
      Hashtbl.remove pending_analyses doc.uri;
      Hashtbl.remove analysis_cache doc.uri;
      Lwt.return ()

    method! on_req_hover ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_ (_doc_state : Linol_lwt.doc_state) =
      let result =
        match Hashtbl.find_opt analysis_cache uri with
        | None -> None
        | Some { analysis } -> (
            match (analysis.program, analysis.type_map, analysis.environment) with
            | Some prog, Some tm, Some env ->
                Marmoset_lsp.Hover.hover_at ~source:analysis.source ~program:prog ~type_map:tm ~environment:env
                  ~line:pos.line ~character:pos.character
            | _ -> None)
      in
      Lwt.return result

    method! on_req_symbol ~notify_back:_ ~id:_ ~uri ~workDoneToken:_ ~partialResultToken:_ () =
      let result =
        match Hashtbl.find_opt analysis_cache uri with
        | None -> None
        | Some { analysis } -> (
            match analysis.program with
            | Some prog ->
                let symbols = Marmoset_lsp.Doc_symbols.document_symbols ~source:analysis.source ~program:prog in
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
                Some
                  (Marmoset_lsp.Inlay_hints.inlay_hints ~source:analysis.source ~program:prog ~type_map:tm ~range)
            | _ -> None)
      in
      Lwt.return result

    method! on_req_completion
        ~notify_back:_
        ~id:_
        ~uri
        ~pos:_
        ~ctx:_
        ~workDoneToken:_
        ~partialResultToken:_
        (_doc_state : Linol_lwt.doc_state) =
      let result =
        match Hashtbl.find_opt analysis_cache uri with
        | None -> None
        | Some { analysis } -> (
            match analysis.environment with
            | Some env ->
                let items = Marmoset_lsp.Completions.completions ~environment:env in
                Some (`List items)
            | None -> None)
      in
      Lwt.return result
  end

let () =
  let server = new marmoset_server in
  let task =
    let rpc = Linol_lwt.Jsonrpc2.create_stdio ~env:() (server :> Linol_lwt.Jsonrpc2.server) in
    let shutdown () = server#get_status = `ReceivedExit in
    Linol_lwt.Jsonrpc2.run ~shutdown rpc
  in
  Linol_lwt.run task
