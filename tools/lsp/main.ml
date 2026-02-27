(* Marmoset LSP server entry point *)

module Lsp_t = Lsp.Types

(* Per-document cached analysis *)
type cached_doc = { analysis : Marmoset_lsp.Doc_state.analysis_result }

class marmoset_server =
  object (self)
    inherit Linol_lwt.Jsonrpc2.server
    val analysis_cache : (Lsp_t.DocumentUri.t, cached_doc) Hashtbl.t = Hashtbl.create 16
    method spawn_query_handler f = Linol_lwt.spawn f

    (* Advertise capabilities *)
    method! config_hover = Some (`Bool true)
    method! config_symbol = Some (`Bool true)

    method! config_sync_opts =
      Lsp_t.TextDocumentSyncOptions.create ~change:Lsp_t.TextDocumentSyncKind.Full ~openClose:true
        ~save:(`SaveOptions (Lsp_t.SaveOptions.create ~includeText:false ()))
        ~willSave:false ()

    (* Shared handler: analyze document, cache result, push diagnostics *)
    method private analyze_and_notify
        ~(notify_back : Linol_lwt.Jsonrpc2.notify_back) (uri : Lsp_t.DocumentUri.t) (content : string) =
      let analysis = Marmoset_lsp.Doc_state.analyze ~source:content in
      Hashtbl.replace analysis_cache uri { analysis };
      notify_back#send_diagnostic analysis.diagnostics

    method on_notif_doc_did_open ~notify_back (doc : Lsp_t.TextDocumentItem.t) ~content =
      self#analyze_and_notify ~notify_back doc.uri content

    method on_notif_doc_did_change
        ~notify_back (doc : Lsp_t.VersionedTextDocumentIdentifier.t) _changes ~old_content:_ ~new_content =
      self#analyze_and_notify ~notify_back doc.uri new_content

    method on_notif_doc_did_close ~notify_back:_ (doc : Lsp_t.TextDocumentIdentifier.t) =
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
  end

let () =
  let server = new marmoset_server in
  let task =
    let rpc = Linol_lwt.Jsonrpc2.create_stdio ~env:() (server :> Linol_lwt.Jsonrpc2.server) in
    let shutdown () = server#get_status = `ReceivedExit in
    Linol_lwt.Jsonrpc2.run ~shutdown rpc
  in
  Linol_lwt.run task
