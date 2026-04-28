module Lsp_t = Linol_lsp.Types
module Export_edits = Export_edits

type command_args = {
  uri : Lsp_t.DocumentUri.t;
  surface_name : string;
  declaration_kind : Export_edits.declaration_kind;
  visibility : Export_edits.visibility;
  original_range : Lsp_t.Range.t;
}

let export_visibility_command = "marmoset.setExportVisibility"

let string_of_declaration_kind = function
  | Export_edits.Let_decl -> "let"
  | Fn_decl -> "fn"
  | Type_decl -> "type"
  | Shape_decl -> "shape"
  | Trait_decl -> "trait"

let declaration_kind_of_string = function
  | "let" -> Ok Export_edits.Let_decl
  | "fn" -> Ok Fn_decl
  | "type" -> Ok Type_decl
  | "shape" -> Ok Shape_decl
  | "trait" -> Ok Trait_decl
  | kind -> Error ("unknown declaration kind: " ^ kind)

let string_of_visibility = function
  | Export_edits.Public -> "public"
  | Private -> "private"

let visibility_of_string = function
  | "public" -> Ok Export_edits.Public
  | "private" -> Ok Private
  | visibility -> Error ("unknown visibility: " ^ visibility)

let json_of_position (position : Lsp_t.Position.t) : Yojson.Safe.t =
  `Assoc [ ("line", `Int position.line); ("character", `Int position.character) ]

let position_of_json = function
  | `Assoc fields -> (
      match (List.assoc_opt "line" fields, List.assoc_opt "character" fields) with
      | Some (`Int line), Some (`Int character) -> Ok (Lsp_t.Position.create ~line ~character)
      | _ -> Error "invalid position")
  | _ -> Error "invalid position"

let json_of_range (range : Lsp_t.Range.t) : Yojson.Safe.t =
  `Assoc [ ("start", json_of_position range.start); ("end", json_of_position range.end_) ]

let range_of_json = function
  | `Assoc fields -> (
      match (List.assoc_opt "start" fields, List.assoc_opt "end" fields) with
      | Some start_json, Some end_json -> (
          match (position_of_json start_json, position_of_json end_json) with
          | Ok start, Ok end_ -> Ok (Lsp_t.Range.create ~start ~end_)
          | Error message, _ | _, Error message -> Error message)
      | _ -> Error "invalid range")
  | _ -> Error "invalid range"

let args_to_json (args : command_args) : Yojson.Safe.t =
  `Assoc
    [
      ("uri", `String (Lsp_t.DocumentUri.to_string args.uri));
      ("surfaceName", `String args.surface_name);
      ("declarationKind", `String (string_of_declaration_kind args.declaration_kind));
      ("visibility", `String (string_of_visibility args.visibility));
      ("originalRange", json_of_range args.original_range);
    ]

let args_of_json = function
  | `Assoc fields -> (
      match
        ( List.assoc_opt "uri" fields,
          List.assoc_opt "surfaceName" fields,
          List.assoc_opt "declarationKind" fields,
          List.assoc_opt "visibility" fields,
          List.assoc_opt "originalRange" fields )
      with
      | ( Some (`String uri),
          Some (`String surface_name),
          Some (`String declaration_kind),
          Some (`String visibility),
          Some range_json ) -> (
          match
            ( declaration_kind_of_string declaration_kind,
              visibility_of_string visibility,
              range_of_json range_json )
          with
          | Ok declaration_kind, Ok visibility, Ok original_range ->
              Ok
                {
                  uri = Lsp_t.DocumentUri.of_string uri;
                  surface_name;
                  declaration_kind;
                  visibility;
                  original_range;
                }
          | Error message, _, _ | _, Error message, _ | _, _, Error message -> Error message)
      | _ -> Error "invalid code lens command arguments")
  | _ -> Error "invalid code lens command arguments"

let make_private_title ~(dependent_count : int) : string =
  if dependent_count <= 0 then
    "make private"
  else
    Printf.sprintf "make private (used by %d module%s)" dependent_count
      (if dependent_count = 1 then
         ""
       else
         "s")

let title_for_decl ~(exported : bool) ~(dependent_count : int) : string =
  if exported then
    make_private_title ~dependent_count
  else
    "make public"

let compute
    ?dependent_count
    ~(source : string)
    ~(uri : Lsp_t.DocumentUri.t)
    ~(program : Export_edits.Surface.surface_program)
    () : Lsp_t.CodeLens.t list =
  let dependent_count = Option.value dependent_count ~default:(fun (_decl : Export_edits.exportable_decl) -> 0) in
  let exports = Export_edits.current_exports program in
  Export_edits.exportable_declarations program
  |> List.map (fun (decl : Export_edits.exportable_decl) ->
         let exported = Export_edits.is_exported ~exports decl in
         let visibility =
           if exported then
             Export_edits.Private
           else
             Export_edits.Public
         in
         let range = Export_edits.declaration_range ~source decl in
         let title = title_for_decl ~exported ~dependent_count:(dependent_count decl) in
         let command =
           Lsp_t.Command.create ~command:export_visibility_command ~title
             ~arguments:
               [
                 args_to_json
                   {
                     uri;
                     surface_name = decl.surface_name;
                     declaration_kind = decl.declaration_kind;
                     visibility;
                     original_range = range;
                   };
               ]
             ()
         in
         Lsp_t.CodeLens.create ~command ~range ())

let lens_titles (lenses : Lsp_t.CodeLens.t list) : string list =
  List.filter_map
    (fun (lens : Lsp_t.CodeLens.t) -> Option.map (fun (command : Lsp_t.Command.t) -> command.title) lens.command)
    lenses

let code_lenses_for_source (source : string) : Lsp_t.CodeLens.t list =
  let program = Export_edits.parse_surface_program ~source in
  compute ~source ~uri:(Lsp_t.DocumentUri.of_path "/tmp/code_lens_test.mr") ~program ()

let%test "code lens computes make private for exported top-level fn" =
  let lenses = code_lenses_for_source "export greet\nfn greet(name: Str) -> Str = name\n" in
  lens_titles lenses = [ "make private" ]

let%test "code lens computes make public for unexported top-level fn" =
  let lenses = code_lenses_for_source "fn greet(name: Str) -> Str = name\n" in
  lens_titles lenses = [ "make public" ]

let%test "code lens includes let, type, shape, and trait declarations" =
  let lenses =
    code_lenses_for_source
      "export top, Point, Named, Show\nlet top = 1\ntype Point = { x: Int }\nshape Named = { name: Str }\ntrait Show[a] = { fn show(self: a) -> Str }\n"
  in
  lens_titles lenses = [ "make private"; "make private"; "make private"; "make private" ]

let%test "code lens excludes local lets, trait methods, and impl methods" =
  let lenses =
    code_lenses_for_source
      "fn greet(name: Str) -> Str = { let local = name\nlocal }\ntrait Show[a] = { fn show(self: a) -> Str }\nimpl Show[Str] = { fn show(self: Str) -> Str = self }\n"
  in
  lens_titles lenses = [ "make public"; "make public" ]

let%test "code lens command arguments round-trip" =
  let lenses = code_lenses_for_source "fn greet(name: Str) -> Str = name\n" in
  match lenses with
  | [ lens ] -> (
      match lens.command with
      | Some { arguments = Some [ json ]; command; title } -> (
          match args_of_json json with
          | Ok args ->
              String.equal command export_visibility_command
              && String.equal title "make public"
              && String.equal args.surface_name "greet"
              && args.declaration_kind = Export_edits.Fn_decl
              && args.visibility = Export_edits.Public
          | Error _ -> false)
      | _ -> false)
  | _ -> false
