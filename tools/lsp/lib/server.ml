(* LSP server: class + entry point *)

module Lsp_t = Linol_lsp.Types
module Compiler = Marmoset.Lib.Frontend_compiler
module Diagnostic = Marmoset.Lib.Diagnostic
module Module_catalog = Frontend.Module_catalog
module StringSet = Set.Make (String)
module Server_request = Linol_lsp.Server_request

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

let analysis_has_completion_semantics (analysis : Doc_state.analysis_result) : bool =
  analysis.compiler_analysis <> None
  && (analysis.surface_program <> None || analysis.program <> None || analysis.environment <> None)

let analysis_has_definition_semantics (analysis : Doc_state.analysis_result) : bool =
  analysis.compiler_analysis <> None && (analysis.surface_program <> None || analysis.program <> None)

let analysis_has_code_lens_semantics (analysis : Doc_state.analysis_result) : bool =
  analysis.compiler_analysis <> None && analysis.surface_program <> None

let update_cached_doc (previous : cached_doc option) (latest : Doc_state.analysis_result) : cached_doc =
  {
    latest;
    last_good =
      (if analysis_has_completion_semantics latest then
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

let completion_items_for_cached_doc
    ~(cached : cached_doc)
    ~(current_source : string option)
    ~(file_id : string)
    ~(source_root : string option)
    ~(line : int)
    ~(character : int)
    ~(trigger_is_dot : bool)
    ~(source_overrides : (string, string) Hashtbl.t) : cached_doc * Lsp_t.CompletionItem.t list option =
  let refreshed =
    match current_source with
    | Some source when not (String.equal source cached.latest.source) ->
        let latest = Doc_state.analyze_with_file_id ?source_root ~source_overrides ~file_id ~source () in
        update_cached_doc (Some cached) latest
    | Some _ | None -> cached
  in
  ( refreshed,
    Completions.complete_at ~latest:refreshed.latest ~last_good:refreshed.last_good ~line ~character
      ~trigger_is_dot ~source_overrides )

let definition_analysis_for_cached_doc
    ~(cached : cached_doc)
    ~(current_source : string option)
    ~(file_id : string)
    ~(source_root : string option)
    ~(source_overrides : (string, string) Hashtbl.t) : cached_doc * Doc_state.analysis_result =
  let refreshed =
    match current_source with
    | Some source when not (String.equal source cached.latest.source) ->
        let latest = Doc_state.analyze_with_file_id ?source_root ~source_overrides ~file_id ~source () in
        update_cached_doc (Some cached) latest
    | Some _ | None -> cached
  in
  let analysis =
    if analysis_has_definition_semantics refreshed.latest then
      refreshed.latest
    else
      Option.value refreshed.last_good ~default:refreshed.latest
  in
  (refreshed, analysis)

let codelens_analysis_for_cached_doc
    ~(cached : cached_doc)
    ~(current_source : string option)
    ~(file_id : string)
    ~(source_root : string option)
    ~(source_overrides : (string, string) Hashtbl.t) : cached_doc * Doc_state.analysis_result =
  let refreshed =
    match current_source with
    | Some source when not (String.equal source cached.latest.source) ->
        let latest = Doc_state.analyze_with_file_id ?source_root ~source_overrides ~file_id ~source () in
        update_cached_doc (Some cached) latest
    | Some _ | None -> cached
  in
  let analysis =
    if analysis_has_code_lens_semantics refreshed.latest then
      refreshed.latest
    else
      Option.value refreshed.last_good ~default:refreshed.latest
  in
  (refreshed, analysis)

let positions_equal (left : Lsp_t.Position.t) (right : Lsp_t.Position.t) : bool =
  left.line = right.line && left.character = right.character

let ranges_equal (left : Lsp_t.Range.t) (right : Lsp_t.Range.t) : bool =
  positions_equal left.start right.start && positions_equal left.end_ right.end_

let export_command_args_of_jsons = function
  | Some [ json ] -> Code_lens.args_of_json json
  | _ -> Error "expected exactly one export-visibility command argument"

let revalidated_export_decl
    ~(source : string) ~(program : Export_edits.Surface.surface_program) ~(args : Code_lens.command_args) :
    Export_edits.exportable_decl option =
  let matching =
    Export_edits.exportable_declarations program
    |> List.filter (fun (decl : Export_edits.exportable_decl) ->
           String.equal decl.surface_name args.surface_name && decl.declaration_kind = args.declaration_kind)
  in
  match
    List.find_opt
      (fun (decl : Export_edits.exportable_decl) ->
        ranges_equal (Export_edits.declaration_range ~source decl) args.original_range)
      matching
  with
  | Some _ as decl -> decl
  | None -> (
      match matching with
      | [ decl ] -> Some decl
      | _ -> None)

let apply_workspace_edit_params_for_export_command
    ~(source : string) ~(program : Export_edits.Surface.surface_program) ~(args : Code_lens.command_args) :
    (Lsp_t.ApplyWorkspaceEditParams.t, string) result =
  match revalidated_export_decl ~source ~program ~args with
  | None ->
      Error
        (Printf.sprintf "unable to revalidate %s declaration '%s'"
           (Code_lens.string_of_declaration_kind args.declaration_kind)
           args.surface_name)
  | Some decl -> (
      match Export_edits.edit_for_visibility ~source ~program ~decl ~visibility:args.visibility with
      | None -> Error (Printf.sprintf "visibility for '%s' already matches requested state" args.surface_name)
      | Some edits ->
          let edit = Lsp_t.WorkspaceEdit.create ~changes:[ (args.uri, edits) ] () in
          Ok
            (Lsp_t.ApplyWorkspaceEditParams.create ~edit
               ~label:(Printf.sprintf "Set export visibility for %s" args.surface_name)
               ()))

let send_apply_workspace_edit
    ~(notify_back : Linol_lwt.Jsonrpc2.notify_back) (params : Lsp_t.ApplyWorkspaceEditParams.t) :
    Yojson.Safe.t Lwt.t =
  let open Lwt.Syntax in
  let waiter, wakener = Lwt.wait () in
  let* _ =
    notify_back#send_request (Server_request.WorkspaceApplyEdit params) (fun result ->
        Lwt.wakeup_later wakener result;
        Lwt.return_unit)
  in
  let* result = waiter in
  match result with
  | Ok (_ : Lsp_t.ApplyWorkspaceEditResult.t) -> Lwt.return `Null
  | Error err ->
      let _ = err in
      Printf.eprintf "[marmoset-lsp] workspace/applyEdit failed\n%!";
      Lwt.return `Null

type decl_identity = {
  file_path : string;
  start_pos : int;
  end_pos : int;
}

let decl_identity_key (identity : decl_identity) : string =
  Printf.sprintf "%s:%d:%d" identity.file_path identity.start_pos identity.end_pos

let decl_identity_of_exportable_decl ~(default_file_path : string) (decl : Export_edits.exportable_decl) :
    decl_identity =
  {
    file_path = Option.value decl.name_ref.file_id ~default:default_file_path;
    start_pos = decl.name_ref.pos;
    end_pos = decl.name_ref.end_pos;
  }

let exportable_decl_keys ~(default_file_path : string) (program : Export_edits.Surface.surface_program) :
    (Export_edits.exportable_decl * string list) list =
  let file_path_of_name_ref (name_ref : Export_edits.Surface.name_ref) =
    Option.value name_ref.file_id ~default:default_file_path
  in
  List.filter_map
    (fun (stmt : Export_edits.Surface.surface_top_stmt) ->
      let with_decl (decl : Export_edits.exportable_decl) =
        let file_path = file_path_of_name_ref decl.name_ref in
        let name_key = decl_identity_key (decl_identity_of_exportable_decl ~default_file_path decl) in
        let stmt_key = decl_identity_key { file_path; start_pos = stmt.std_pos; end_pos = stmt.std_end_pos } in
        Some (decl, [ name_key; stmt_key ])
      in
      match stmt.std_decl with
      | Export_edits.Surface.SLet { name; name_ref; _ } ->
          with_decl { surface_name = name; declaration_kind = Export_edits.Let_decl; name_ref }
      | Export_edits.Surface.SFnDecl { name; name_ref; _ } ->
          with_decl { surface_name = name; declaration_kind = Export_edits.Fn_decl; name_ref }
      | Export_edits.Surface.STypeDef { type_name; type_name_ref; _ } ->
          with_decl
            { surface_name = type_name; declaration_kind = Export_edits.Type_decl; name_ref = type_name_ref }
      | Export_edits.Surface.SShapeDef { shape_name; shape_name_ref; _ } ->
          with_decl
            { surface_name = shape_name; declaration_kind = Export_edits.Shape_decl; name_ref = shape_name_ref }
      | Export_edits.Surface.STraitDef { name; name_ref; _ } ->
          with_decl { surface_name = name; declaration_kind = Export_edits.Trait_decl; name_ref }
      | Export_edits.Surface.SExportDecl _ | Export_edits.Surface.SImportDecl _
      | Export_edits.Surface.SAmbiguousImplDef _ | Export_edits.Surface.SInherentImplDef _
      | Export_edits.Surface.SExpressionStmt _ | Export_edits.Surface.SReturn _ | Export_edits.Surface.SBlock _ ->
          None)
    program

let decl_identity_of_definition_target = function
  | Definition.Span { file_path; start_pos; end_pos } -> Some { file_path; start_pos; end_pos }
  | Definition.File_start _ -> None

let countable_reference = function
  | Cursor_context.Import_alias _ | Cursor_context.Import_path_segment _ | Cursor_context.Declaration_head _ ->
      false
  | _ -> true

let source_for_file ~(source_overrides : (string, string) Hashtbl.t) (file_path : string) : string =
  let file_path = Doc_state.normalize_path file_path in
  match Hashtbl.find_opt source_overrides file_path with
  | Some source -> source
  | None -> Compiler.read_source_file file_path

let dependent_modules_by_decl
    ~(project_root : string)
    ~(active_analysis : Compiler.entry_analysis)
    ~(source_overrides : (string, string) Hashtbl.t)
    ~(active_file_path : string)
    ~(active_module_id : string option)
    ~(program : Export_edits.Surface.surface_program) : (string, StringSet.t) Hashtbl.t =
  let normalized_overrides = Doc_state.normalize_source_overrides source_overrides in
  let catalog =
    Module_catalog.build ~root_dir:project_root ~analysis:active_analysis ~source_overrides:normalized_overrides
      ()
  in
  let target_to_modules = Hashtbl.create 16 in
  let target_to_decl = Hashtbl.create 16 in
  exportable_decl_keys ~default_file_path:active_file_path program
  |> List.iter (fun ((decl : Export_edits.exportable_decl), (keys : string list)) ->
         let canonical =
           decl_identity_key (decl_identity_of_exportable_decl ~default_file_path:active_file_path decl)
         in
         Hashtbl.replace target_to_modules canonical StringSet.empty;
         List.iter (fun key -> Hashtbl.replace target_to_decl key canonical) keys);
  Hashtbl.iter
    (fun module_id (entry : Module_catalog.entry) ->
      let same_file = String.equal entry.file_path active_file_path in
      let same_module =
        match active_module_id with
        | Some active_module_id -> String.equal module_id active_module_id
        | None -> false
      in
      let in_project = path_is_within_root ~root:project_root entry.file_path in
      if in_project && (not same_file) && not same_module then
        let source = source_for_file ~source_overrides:normalized_overrides entry.file_path in
        let module_analysis =
          Doc_state.analyze_with_file_id ~source_root:project_root ~source_overrides:normalized_overrides
            ~file_id:entry.file_path ~source ()
        in
        match (module_analysis.compiler_analysis, module_analysis.surface_program, module_analysis.program) with
        | Some compiler_analysis, Some surface_program, Some lowered_program ->
            let input =
              {
                Cursor_context.surface_program;
                lowered_program;
                scope_index = Cursor_context.build_scope_index surface_program;
              }
            in
            Cursor_context.collect_references ~input
            |> List.iter (fun reference ->
                   if countable_reference reference then
                     match
                       Definition.cursor_reference_target compiler_analysis ~source:module_analysis.source
                         ~surface_program:module_analysis.surface_program ~reference
                     with
                     | Some target -> (
                         match decl_identity_of_definition_target target with
                         | Some identity -> (
                             let key = decl_identity_key identity in
                             match Hashtbl.find_opt target_to_decl key with
                             | Some dependents ->
                                 let prior =
                                   Option.value
                                     (Hashtbl.find_opt target_to_modules dependents)
                                     ~default:StringSet.empty
                                 in
                                 Hashtbl.replace target_to_modules dependents (StringSet.add module_id prior)
                             | None -> ())
                         | None -> ())
                     | None -> ())
        | _ -> ())
    catalog.modules;
  target_to_modules

let dependent_count_for_decl
    ~(counts : (string, StringSet.t) Hashtbl.t) ~(active_file_path : string) (decl : Export_edits.exportable_decl)
    : int =
  let key = decl_identity_key (decl_identity_of_exportable_decl ~default_file_path:active_file_path decl) in
  match Hashtbl.find_opt counts key with
  | Some dependents -> StringSet.cardinal dependents
  | None -> 0

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
    method! config_code_lens_options = Some (Lsp_t.CodeLensOptions.create ())
    method! config_list_commands = [ Code_lens.export_visibility_command ]

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
        ~pos
        ~ctx
        ~workDoneToken:_
        ~partialResultToken:_
        (_doc_state : Linol_lwt.doc_state) =
      let is_dot_trigger =
        match ctx with
        | Some ctx -> (
            match ctx.triggerCharacter with
            | Some "." -> true
            | _ -> false)
        | None -> false
      in
      let result =
        match Hashtbl.find_opt analysis_cache uri with
        | None -> None
        | Some cached ->
            let current_source = Option.map (fun (doc : open_doc) -> doc.text) (Hashtbl.find_opt open_docs uri) in
            let source_root =
              match Hashtbl.find_opt open_docs uri with
              | Some doc -> choose_known_source_root ~analysis_cache ~uri ~file_path:doc.file_path
              | None ->
                  choose_known_source_root ~analysis_cache ~uri
                    ~file_path:(Doc_state.file_path_of_file_id (Lsp_t.DocumentUri.to_string uri))
            in
            let source_overrides = self#source_overrides () in
            let file_id = Lsp_t.DocumentUri.to_string uri in
            let cached, items =
              completion_items_for_cached_doc ~cached ~current_source ~file_id ~source_root ~line:pos.line
                ~character:pos.character ~trigger_is_dot:is_dot_trigger ~source_overrides
            in
            Hashtbl.replace analysis_cache uri cached;
            items
      in
      Lwt.return (Option.map (fun items -> `List items) result)

    method! on_req_definition
        ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_ ~partialResultToken:_ (_doc_state : Linol_lwt.doc_state) =
      let result =
        match Hashtbl.find_opt analysis_cache uri with
        | None -> None
        | Some cached ->
            let current_source = Option.map (fun (doc : open_doc) -> doc.text) (Hashtbl.find_opt open_docs uri) in
            let source_root =
              match Hashtbl.find_opt open_docs uri with
              | Some doc -> choose_known_source_root ~analysis_cache ~uri ~file_path:doc.file_path
              | None ->
                  choose_known_source_root ~analysis_cache ~uri
                    ~file_path:(Doc_state.file_path_of_file_id (Lsp_t.DocumentUri.to_string uri))
            in
            let source_overrides = self#source_overrides () in
            let file_id = Lsp_t.DocumentUri.to_string uri in
            let cached, analysis =
              definition_analysis_for_cached_doc ~cached ~current_source ~file_id ~source_root ~source_overrides
            in
            Hashtbl.replace analysis_cache uri cached;
            Definition.locations ~analysis
              (Definition.find_definition ~analysis ~line:pos.line ~character:pos.character)
      in
      Lwt.return result

    method! on_req_code_lens
        ~notify_back:_ ~id:_ ~uri ~workDoneToken:_ ~partialResultToken:_ (_doc_state : Linol_lwt.doc_state) =
      let result =
        match Hashtbl.find_opt analysis_cache uri with
        | None -> []
        | Some cached -> (
            let current_source = Option.map (fun (doc : open_doc) -> doc.text) (Hashtbl.find_opt open_docs uri) in
            let source_root =
              match Hashtbl.find_opt open_docs uri with
              | Some doc -> choose_known_source_root ~analysis_cache ~uri ~file_path:doc.file_path
              | None ->
                  choose_known_source_root ~analysis_cache ~uri
                    ~file_path:(Doc_state.file_path_of_file_id (Lsp_t.DocumentUri.to_string uri))
            in
            let source_overrides = self#source_overrides () in
            let file_id = Lsp_t.DocumentUri.to_string uri in
            let cached, analysis =
              codelens_analysis_for_cached_doc ~cached ~current_source ~file_id ~source_root ~source_overrides
            in
            Hashtbl.replace analysis_cache uri cached;
            match (analysis.surface_program, analysis.compiler_analysis) with
            | Some program, Some compiler_analysis ->
                let project_root =
                  Option.value analysis.project_root
                    ~default:
                      (Option.value source_root ~default:(Filename.dirname (Doc_state.normalize_path file_id)))
                in
                let counts =
                  dependent_modules_by_decl ~project_root ~active_analysis:compiler_analysis ~source_overrides
                    ~active_file_path:compiler_analysis.active_file.file_path
                    ~active_module_id:compiler_analysis.active_file.module_id ~program
                in
                Code_lens.compute ~source:analysis.source ~uri ~program
                  ~dependent_count:
                    (dependent_count_for_decl ~counts ~active_file_path:compiler_analysis.active_file.file_path)
                  ()
            | None, _ -> []
            | Some _, None -> [])
      in
      Lwt.return result

    method! on_req_execute_command ~notify_back ~id:_ ~workDoneToken:_ command args =
      if not (String.equal command Code_lens.export_visibility_command) then
        Lwt.return `Null
      else
        match export_command_args_of_jsons args with
        | Error message ->
            Printf.eprintf "[marmoset-lsp] executeCommand rejected: %s\n%!" message;
            Lwt.return `Null
        | Ok args -> (
            let uri = args.uri in
            let current_source =
              match Hashtbl.find_opt open_docs uri with
              | Some doc -> Some doc.text
              | None -> Option.map (fun cached -> cached.latest.source) (Hashtbl.find_opt analysis_cache uri)
            in
            let source_root =
              match Hashtbl.find_opt open_docs uri with
              | Some doc -> choose_known_source_root ~analysis_cache ~uri ~file_path:doc.file_path
              | None ->
                  choose_known_source_root ~analysis_cache ~uri
                    ~file_path:(Doc_state.file_path_of_file_id (Lsp_t.DocumentUri.to_string uri))
            in
            match current_source with
            | None ->
                Printf.eprintf "[marmoset-lsp] executeCommand rejected: no source for %s\n%!"
                  (Lsp_t.DocumentUri.to_string uri);
                Lwt.return `Null
            | Some source -> (
                let source_overrides = self#source_overrides () in
                let file_id = Lsp_t.DocumentUri.to_string uri in
                let analysis =
                  Doc_state.analyze_with_file_id ?source_root ~source_overrides ~file_id ~source ()
                in
                let previous = Hashtbl.find_opt analysis_cache uri in
                Hashtbl.replace analysis_cache uri (update_cached_doc previous analysis);
                match analysis.surface_program with
                | None ->
                    Printf.eprintf
                      "[marmoset-lsp] executeCommand rejected: current document does not have a parseable surface program\n%!";
                    Lwt.return `Null
                | Some program -> (
                    match apply_workspace_edit_params_for_export_command ~source ~program ~args with
                    | Error message ->
                        Printf.eprintf "[marmoset-lsp] executeCommand rejected: %s\n%!" message;
                        Lwt.return `Null
                    | Ok params -> send_apply_workspace_edit ~notify_back params)))

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
    surface_program = None;
    program;
    type_map;
    environment;
    type_var_user_names = [];
    diagnostics;
    compiler_analysis = None;
  }

let%test "update_cached_doc captures a last_good snapshot from semantic analyses" =
  let analysis = Doc_state.analyze ~source:"let x = 1\nx\n" in
  match update_cached_doc None analysis with
  | { last_good = Some cached; _ } -> cached.compiler_analysis <> None && cached.program <> None
  | _ -> false

let%test "update_cached_doc preserves last_good across non-semantic latest edits" =
  let semantic = Doc_state.analyze ~source:"let x = 1\nx\n" in
  let stale = update_cached_doc None semantic in
  let latest = fake_analysis ~diagnostics:[] () in
  match update_cached_doc (Some stale) latest with
  | { latest = latest_analysis; last_good = Some cached } ->
      latest_analysis.compiler_analysis = None && cached.compiler_analysis <> None
  | _ -> false

let%test "update_cached_doc retains namespace-navigation snapshots even when typing is incomplete" =
  Doc_state.with_temp_project
    [ ("main.mr", "import math\nmath\n"); ("math.mr", "export add\nfn add(x: Int, y: Int) -> Int = x + y\n") ]
    (fun root ->
      let file_id = Filename.concat root "main.mr" in
      let analysis = Doc_state.analyze_with_file_id ~source_root:root ~file_id ~source:"import math\nmath\n" () in
      match update_cached_doc None analysis with
      | { last_good = Some cached; _ } ->
          cached.compiler_analysis <> None && cached.project_root <> None && cached.environment = None
      | _ -> false)

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

let source_with_cursor (annotated : string) : string * int * int =
  let cursor = String.index annotated '|' in
  let source =
    String.sub annotated 0 cursor ^ String.sub annotated (cursor + 1) (String.length annotated - cursor - 1)
  in
  let pos = Lsp_utils.offset_to_position ~source ~offset:cursor in
  (source, pos.line, pos.character)

let cached_completion_labels
    ?cached_annotated
    ?last_good_annotated
    ?(trigger_is_dot = false)
    ~(files : (string * string) list)
    ~(entry_rel : string)
    (annotated : string) : string list =
  let captured = ref [] in
  let _ =
    Doc_state.with_temp_project files (fun root ->
        let cached_annotated = Option.value cached_annotated ~default:annotated in
        let cached_source, _, _ = source_with_cursor cached_annotated in
        let source, line, character = source_with_cursor annotated in
        let file_id = Filename.concat root entry_rel in
        let latest = Doc_state.analyze_with_file_id ~source_root:root ~file_id ~source:cached_source () in
        let previous =
          Option.map
            (fun annotated ->
              let source, _, _ = source_with_cursor annotated in
              let last_good = Doc_state.analyze_with_file_id ~source_root:root ~file_id ~source () in
              update_cached_doc None last_good)
            last_good_annotated
        in
        let cached = update_cached_doc previous latest in
        let source_overrides = Hashtbl.create 0 in
        let _, items =
          completion_items_for_cached_doc ~cached ~current_source:(Some source) ~file_id ~source_root:(Some root)
            ~line ~character ~trigger_is_dot ~source_overrides
        in
        (captured :=
           match items with
           | Some items -> List.map (fun (item : Lsp_t.CompletionItem.t) -> item.label) items
           | None -> []);
        true)
  in
  !captured

let cached_definition_target
    ?cached_annotated
    ?last_good_annotated
    ~(files : (string * string) list)
    ~(entry_rel : string)
    (annotated : string) : string * Definition.definition_target option =
  let captured = ref ("", None) in
  let _ =
    Doc_state.with_temp_project files (fun root ->
        let cached_annotated = Option.value cached_annotated ~default:annotated in
        let cached_source, _, _ = source_with_cursor cached_annotated in
        let source, line, character = source_with_cursor annotated in
        let file_id = Filename.concat root entry_rel in
        let latest = Doc_state.analyze_with_file_id ~source_root:root ~file_id ~source:cached_source () in
        let previous =
          Option.map
            (fun annotated ->
              let source, _, _ = source_with_cursor annotated in
              let last_good = Doc_state.analyze_with_file_id ~source_root:root ~file_id ~source () in
              update_cached_doc None last_good)
            last_good_annotated
        in
        let cached = update_cached_doc previous latest in
        let source_overrides = Hashtbl.create 0 in
        let _, analysis =
          definition_analysis_for_cached_doc ~cached ~current_source:(Some source) ~file_id
            ~source_root:(Some root) ~source_overrides
        in
        captured := (file_id, Definition.find_definition ~analysis ~line ~character);
        true)
  in
  !captured

let cached_code_lens_titles
    ?cached_annotated
    ?last_good_annotated
    ~(files : (string * string) list)
    ~(entry_rel : string)
    (annotated : string) : string list =
  let captured = ref [] in
  let _ =
    Doc_state.with_temp_project files (fun root ->
        let cached_annotated = Option.value cached_annotated ~default:annotated in
        let cached_source, _, _ = source_with_cursor cached_annotated in
        let source, _, _ = source_with_cursor annotated in
        let file_id = Filename.concat root entry_rel in
        let latest = Doc_state.analyze_with_file_id ~source_root:root ~file_id ~source:cached_source () in
        let previous =
          Option.map
            (fun annotated ->
              let source, _, _ = source_with_cursor annotated in
              let last_good = Doc_state.analyze_with_file_id ~source_root:root ~file_id ~source () in
              update_cached_doc None last_good)
            last_good_annotated
        in
        let cached = update_cached_doc previous latest in
        let source_overrides = Hashtbl.create 0 in
        let _, analysis =
          codelens_analysis_for_cached_doc ~cached ~current_source:(Some source) ~file_id ~source_root:(Some root)
            ~source_overrides
        in
        (captured :=
           match analysis.surface_program with
           | Some program ->
               Code_lens.compute ~source:analysis.source ~uri:(Lsp_t.DocumentUri.of_path file_id) ~program ()
               |> Code_lens.lens_titles
           | None -> []);
        true)
  in
  !captured

let command_args_from_lens (lens : Lsp_t.CodeLens.t) : Code_lens.command_args option =
  Option.bind lens.command (fun (command : Lsp_t.Command.t) ->
      match command.arguments with
      | Some [ json ] -> Result.to_option (Code_lens.args_of_json json)
      | _ -> None)

let first_code_lens_for_source ~(source : string) ~(file_path : string) : Lsp_t.CodeLens.t option =
  match
    Code_lens.compute ~source ~uri:(Lsp_t.DocumentUri.of_path file_path)
      ~program:(Export_edits.parse_surface_program ~source)
      ()
  with
  | lens :: _ -> Some lens
  | [] -> None

let apply_source_for_command ~(source : string) ~(args : Code_lens.command_args) : string option =
  match
    apply_workspace_edit_params_for_export_command ~source
      ~program:(Export_edits.parse_surface_program ~source)
      ~args
  with
  | Ok params -> (
      match params.edit.changes with
      | Some [ (_uri, edits) ] -> Some (Export_edits.apply_text_edits ~source edits)
      | _ -> None)
  | Error _ -> None

let project_code_lenses
    ?source_overrides ~(files : (string * string) list) ~(entry_rel : string) ~(source : string) () :
    Lsp_t.CodeLens.t list =
  let source_overrides = Option.value source_overrides ~default:[] in
  let captured = ref [] in
  let _ =
    Doc_state.with_temp_project files (fun root ->
        let file_id = Filename.concat root entry_rel in
        let overrides = Hashtbl.create (List.length source_overrides) in
        List.iter
          (fun (relative_path, contents) ->
            Hashtbl.replace overrides (Filename.concat root relative_path) contents)
          source_overrides;
        let analysis =
          Doc_state.analyze_with_file_id ~source_root:root ~source_overrides:overrides ~file_id ~source ()
        in
        (captured :=
           match (analysis.compiler_analysis, analysis.surface_program) with
           | Some compiler_analysis, Some program ->
               let counts =
                 dependent_modules_by_decl ~project_root:root ~active_analysis:compiler_analysis
                   ~source_overrides:overrides ~active_file_path:compiler_analysis.active_file.file_path
                   ~active_module_id:compiler_analysis.active_file.module_id ~program
               in
               Code_lens.compute ~source ~uri:(Lsp_t.DocumentUri.of_path file_id) ~program
                 ~dependent_count:
                   (dependent_count_for_decl ~counts ~active_file_path:compiler_analysis.active_file.file_path)
                 ()
           | _ -> []);
        true)
  in
  !captured

let lens_title_for_name ~(name : string) (lenses : Lsp_t.CodeLens.t list) : string option =
  List.find_map
    (fun (lens : Lsp_t.CodeLens.t) ->
      match (lens.command, command_args_from_lens lens) with
      | Some command, Some args when String.equal args.surface_name name -> Some command.title
      | _ -> None)
    lenses

let%test "server definition uses live buffer text instead of stale cached source" =
  let main_source = "type Point = { x: Int }\nlet p: Point = { x: 1 }\np\n" in
  let main_path, actual =
    cached_definition_target ~cached_annotated:"let stale = 1\n|\n"
      ~files:[ ("main.mr", main_source) ]
      ~entry_rel:"main.mr" "type Point = { x: Int }\nlet p: Point| = { x: 1 }\np\n"
  in
  actual = Definition.target_span_of_substring ~file_path:main_path ~source:main_source ~needle:"Point" ()

let%test "server code lens uses live buffer text instead of stale cached source" =
  let titles =
    cached_code_lens_titles ~cached_annotated:"export greet\nfn greet(name: Str) -> Str = name\n|\n"
      ~files:[ ("main.mr", "fn greet(name: Str) -> Str = name\n") ]
      ~entry_rel:"main.mr" "fn greet(name: Str) -> Str = name\n|\n"
  in
  titles = [ "make public" ]

let%test "server code lens falls back to last_good during incomplete edits" =
  let titles =
    cached_code_lens_titles ~cached_annotated:"fn greet(name: Str) -> Str = {|\n"
      ~last_good_annotated:"fn greet(name: Str) -> Str = name|\n"
      ~files:[ ("main.mr", "fn greet(name: Str) -> Str = name\n") ]
      ~entry_rel:"main.mr" "fn greet(name: Str) -> Str = {|\n"
  in
  titles = [ "make public" ]

let%test "server execute command builds export edits against live open-buffer text" =
  let stale_source = "fn greet(name: Str) -> Str = name\n" in
  let live_source = "# heading\n\nfn greet(name: Str) -> Str = name\n" in
  match first_code_lens_for_source ~source:stale_source ~file_path:"/tmp/server_execute_command_test.mr" with
  | None -> false
  | Some lens -> (
      match command_args_from_lens lens with
      | None -> false
      | Some args ->
          apply_source_for_command ~source:live_source ~args
          = Some "# heading\n\nexport greet\n\nfn greet(name: Str) -> Str = name\n")

let%test "server execute command rejects renamed declarations instead of guessing" =
  let source = "fn greet(name: Str) -> Str = name\n" in
  match first_code_lens_for_source ~source ~file_path:"/tmp/server_execute_command_test.mr" with
  | None -> false
  | Some lens -> (
      match command_args_from_lens lens with
      | None -> false
      | Some args -> apply_source_for_command ~source:"fn renamed(name: Str) -> Str = name\n" ~args = None)

let%test "server code lens counts direct import value dependents once" =
  let lenses =
    project_code_lenses ~entry_rel:"math.mr"
      ~files:
        [
          ("math.mr", "export add\nfn add(x: Int, y: Int) -> Int = x + y\n");
          ("main.mr", "import math.add\nadd(1, 2)\nadd(3, 4)\n");
        ]
      ~source:"export add\nfn add(x: Int, y: Int) -> Int = x + y\n" ()
  in
  lens_title_for_name ~name:"add" lenses = Some "make private (used by 1 module)"

let%test "server code lens counts direct import aliases once" =
  let lenses =
    project_code_lenses ~entry_rel:"math.mr"
      ~files:
        [
          ("math.mr", "export add\nfn add(x: Int, y: Int) -> Int = x + y\n");
          ("main.mr", "import math.add as plus\nplus(1, 2)\n");
        ]
      ~source:"export add\nfn add(x: Int, y: Int) -> Int = x + y\n" ()
  in
  lens_title_for_name ~name:"add" lenses = Some "make private (used by 1 module)"

let%test "server code lens counts namespace-qualified value dependents" =
  let lenses =
    project_code_lenses ~entry_rel:"math.mr"
      ~files:
        [
          ("math.mr", "export add\nfn add(x: Int, y: Int) -> Int = x + y\n");
          ("main.mr", "import math\nmath.add(1, 2)\n");
        ]
      ~source:"export add\nfn add(x: Int, y: Int) -> Int = x + y\n" ()
  in
  lens_title_for_name ~name:"add" lenses = Some "make private (used by 1 module)"

let%test "server code lens counts namespace-qualified type dependents" =
  let lenses =
    project_code_lenses ~entry_rel:"types/geo.mr"
      ~files:
        [
          ("types/geo.mr", "export Point\ntype Point = { x: Int, y: Int }\n");
          ("main.mr", "import types.geo\nlet p: geo.Point = { x: 1, y: 2 }\n");
        ]
      ~source:"export Point\ntype Point = { x: Int, y: Int }\n" ()
  in
  lens_title_for_name ~name:"Point" lenses = Some "make private (used by 1 module)"

let%test "server code lens ignores namespace imports without symbol use and same-module refs" =
  let lenses =
    project_code_lenses ~entry_rel:"math.mr"
      ~files:
        [
          ("math.mr", "export add\nfn add(x: Int, y: Int) -> Int = x + y\nlet same = add(1, 2)\n");
          ("main.mr", "import math\nputs(\"unused namespace\")\n");
        ]
      ~source:"export add\nfn add(x: Int, y: Int) -> Int = x + y\nlet same = add(1, 2)\n" ()
  in
  lens_title_for_name ~name:"add" lenses = Some "make private"

let%test "server code lens reflects unsaved dependent overrides in counts" =
  let base_files =
    [
      ("math.mr", "export add\nfn add(x: Int, y: Int) -> Int = x + y\n");
      ("main.mr", "import math.add\nadd(1, 2)\n");
    ]
  in
  let base_lenses =
    project_code_lenses ~entry_rel:"math.mr" ~files:base_files
      ~source:"export add\nfn add(x: Int, y: Int) -> Int = x + y\n" ()
  in
  let override_lenses =
    project_code_lenses ~entry_rel:"math.mr" ~files:base_files
      ~source:"export add\nfn add(x: Int, y: Int) -> Int = x + y\n"
      ~source_overrides:[ ("main.mr", "import math.add\nputs(\"removed\")\n") ]
      ()
  in
  lens_title_for_name ~name:"add" base_lenses = Some "make private (used by 1 module)"
  && lens_title_for_name ~name:"add" override_lenses = Some "make private"

let%test "server definition falls back to last_good during incomplete edits" =
  let main_source = "type Point = { x: Int }\nlet p: Point = { x: 1 }\np\n" in
  let main_path, actual =
    cached_definition_target ~cached_annotated:"type Point = { x: Int }\nlet p: Point| = { x: 1 }\np\n{"
      ~last_good_annotated:"type Point = { x: Int }\nlet p: Point| = { x: 1 }\np\n"
      ~files:[ ("main.mr", main_source) ]
      ~entry_rel:"main.mr" "type Point = { x: Int }\nlet p: Point| = { x: 1 }\np\n{"
  in
  actual = Definition.target_span_of_substring ~file_path:main_path ~source:main_source ~needle:"Point" ()

let%test "server completion uses live buffer text instead of leaking stale cached locals" =
  let labels =
    cached_completion_labels
      ~cached_annotated:"import types.geo\nlet xs = [1]\nlet p = { x: 1, y: 2 }\nlet q = { x: 1 }\n|\n"
      ~files:
        [
          ( "main_namespace_type_positions.mr",
            "import types.geo\nlet xs = [1]\nlet p = { x: 1, y: 2 }\nlet q = { x: 1 }\nimpo|\n" );
          ("types/geo.mr", "export origin\nlet origin = { x: 0, y: 0 }\n");
        ]
      ~entry_rel:"main_namespace_type_positions.mr"
      "import types.geo\nlet xs = [1]\nlet p = { x: 1, y: 2 }\nlet q = { x: 1 }\nimpo|\n"
  in
  List.mem "let" labels
  && (not (List.mem "xs" labels))
  && (not (List.mem "p" labels))
  && (not (List.mem "q" labels))
  && not
       (List.exists
          (fun label ->
            Diagnostics.String_utils.contains_substring ~needle:"u005f" label
            || Diagnostics.String_utils.contains_substring ~needle:"__" label)
          labels)

let%test "server completion supports dot-triggered namespace members" =
  let labels =
    cached_completion_labels ~trigger_is_dot:true ~last_good_annotated:"import math\nmath|\n"
      ~files:
        [
          ("main.mr", "import math\nmath.\n"); ("math.mr", "export add\nfn add(x: Int, y: Int) -> Int = x + y\n");
        ]
      ~entry_rel:"main.mr" "import math\nmath.|\n"
  in
  List.mem "add" labels

let%test "server completion supports dot-triggered imported alias members" =
  let labels =
    cached_completion_labels ~trigger_is_dot:true ~last_good_annotated:"import types.geo\ngeo|\n"
      ~files:
        [
          ("main.mr", "import types.geo\ngeo.\n");
          ( "types/geo.mr",
            "export render_point, Point, HasXY\ntype Point = { x: Int, y: Int }\nshape HasXY = { x: Int, y: Int }\nfn render_point(p: Point) -> Str = \"point\"\n"
          );
        ]
      ~entry_rel:"main.mr" "import types.geo\ngeo.|\n"
  in
  List.mem "render_point" labels && List.mem "Point" labels && List.mem "HasXY" labels

let%test "server completion supports bare-dot imported alias members without trigger character" =
  let labels =
    cached_completion_labels ~cached_annotated:"import types.geo\nlet p = 1\nlet q = 2\ngeo|\n"
      ~files:
        [
          ("main.mr", "import types.geo\nlet p = 1\nlet q = 2\ngeo.\n");
          ( "types/geo.mr",
            "export render_point, Point, HasXY\ntype Point = { x: Int, y: Int }\nshape HasXY = { x: Int, y: Int }\nfn render_point(p: Point) -> Str = \"point\"\n"
          );
        ]
      ~entry_rel:"main.mr" "import types.geo\nlet p = 1\nlet q = 2\ngeo.|\n"
  in
  List.mem "render_point" labels
  && List.mem "Point" labels
  && List.mem "HasXY" labels
  && (not (List.mem "p" labels))
  && (not (List.mem "q" labels))
  && not (List.mem "let" labels)

let%test "server completion on imported module bare dot excludes local names" =
  let source =
    "import fibonacci\n\ntype Point = { x: Int, y: Int }\n\nlet point: Point = { x: 1, y: 2 }\nlet moved = { ...point, x: 10 }\n\nfn get_x(value: Point) -> Int = value.x\n\nfibonacci.\nputs(get_x(moved))\nputs(moved.x + moved.y)\n"
  in
  let labels =
    cached_completion_labels
      ~files:[ ("main.mr", source); ("fibonacci.mr", "export fib\n\nfn fib(n: Int) -> Int = n\n") ]
      ~entry_rel:"main.mr"
      "import fibonacci\n\ntype Point = { x: Int, y: Int }\n\nlet point: Point = { x: 1, y: 2 }\nlet moved = { ...point, x: 10 }\n\nfn get_x(value: Point) -> Int = value.x\n\nfibonacci.|\nputs(get_x(moved))\nputs(moved.x + moved.y)\n"
  in
  List.mem "fib" labels
  && (not (List.mem "get_x" labels))
  && (not (List.mem "moved" labels))
  && (not (List.mem "point" labels))
  && not (List.mem "let" labels)

let%test "server completion uses live buffer text for namespace members when cache is stale" =
  let cached_source =
    "type Point = { x: Int, y: Int }\n\nlet point: Point = { x: 1, y: 2 }\nlet moved = { ...point, x: 10 }\n\nfn get_x(value: Point) -> Int = value.x\n\n|\nputs(get_x(moved))\nputs(moved.x + moved.y)\n"
  in
  let labels =
    cached_completion_labels ~cached_annotated:cached_source
      ~files:
        [
          ( "main.mr",
            "import fibonacci\n\ntype Point = { x: Int, y: Int }\n\nlet point: Point = { x: 1, y: 2 }\nlet moved = { ...point, x: 10 }\n\nfn get_x(value: Point) -> Int = value.x\n\nfibonacci.\nputs(get_x(moved))\nputs(moved.x + moved.y)\n"
          );
          ("fibonacci.mr", "export fib\n\nfn fib(n: Int) -> Int = n\n");
        ]
      ~entry_rel:"main.mr"
      "import fibonacci\n\ntype Point = { x: Int, y: Int }\n\nlet point: Point = { x: 1, y: 2 }\nlet moved = { ...point, x: 10 }\n\nfn get_x(value: Point) -> Int = value.x\n\nfibonacci.|\nputs(get_x(moved))\nputs(moved.x + moved.y)\n"
  in
  List.mem "fib" labels
  && (not (List.mem "get_x" labels))
  && (not (List.mem "moved" labels))
  && (not (List.mem "point" labels))
  && not (List.mem "let" labels)

let%test "server completion supports qualified imported types" =
  let labels =
    cached_completion_labels ~last_good_annotated:"import types.geo\nlet p: geo.Point| = { x: 1, y: 2 }\n"
      ~files:
        [
          ("main.mr", "import types.geo\nlet p: geo.Po = { x: 1, y: 2 }\n");
          ( "types/geo.mr",
            "export render_point, Point, HasXY\ntype Point = { x: Int, y: Int }\nshape HasXY = { x: Int, y: Int }\nfn render_point(p: Point) -> Str = \"point\"\n"
          );
        ]
      ~entry_rel:"main.mr" "import types.geo\nlet p: geo.Po| = { x: 1, y: 2 }\n"
  in
  List.mem "Point" labels && (not (List.mem "HasXY" labels)) && not (List.mem "render_point" labels)

let%test "server completion falls back to last_good for incomplete type annotations" =
  let labels =
    cached_completion_labels ~cached_annotated:"type Point = { x: Int }\nlet value: Po|\n"
      ~last_good_annotated:"type Point = { x: Int }\nlet value: Point| = { x: 1 }\n"
      ~files:[ ("main.mr", "type Point = { x: Int }\nlet value: Po\n") ]
      ~entry_rel:"main.mr" "type Point = { x: Int }\nlet value: Po|\n"
  in
  List.mem "Point" labels
