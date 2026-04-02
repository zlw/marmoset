module Lsp_t = Linol_lsp.Types

let scenario_source =
  {|
type Option[a] = {
  Some(a),
  None,
}

shape Named = {
  name: Str,
  age: Int,
}

trait Greeter[a] = {
  fn greet(self: a, prefix: Str) -> Str
}

type Monkey = {
  name: Str,
  age: Int,
}

impl Greeter[Monkey] = {
  fn greet(self: Monkey, prefix: Str) -> Str = prefix + self.name
}

impl Monkey = {
  fn rename(self: Monkey, next_name: Str) -> Monkey = self
}

fn display_name[t: Named](x: t) -> Str = x.name

fn print_book_name(book: Map[Str, Str], fallback: Str) => Unit = {
  let title = book["title"]
  puts("#{title}#{fallback}")
}

let book = {
  "title": "Writing A Compiler In Go",
}

let monkey = { name: "George", age: 7, bananas: 3 }
let name = "Curious George"
let promoted = { ...monkey, name: }
let banner = promoted |> display_name

display_name(monkey)
print_book_name(book, "!")
|}

let inlay_scenario_source =
  {|
let render = (x: Int) -> {
  let y = x + 1;
  let add = (z) -> z + y;
  add(2)
};
|}

let nth_substring_offset ~(source : string) ~(needle : string) ~(occurrence : int) : int option =
  let rec scan start remaining =
    if remaining <= 0 then
      None
    else if start + String.length needle > String.length source then
      None
    else if String.sub source start (String.length needle) = needle then
      if remaining = 1 then
        Some start
      else
        scan (start + 1) (remaining - 1)
    else
      scan (start + 1) remaining
  in
  scan 0 occurrence

let position_of_substring ~(source : string) ~(needle : string) ?(occurrence = 1) ?(offset_in_needle = 0) () :
    int * int =
  match nth_substring_offset ~source ~needle ~occurrence with
  | Some base ->
      let pos = Lsp_utils.offset_to_position ~source ~offset:(base + offset_in_needle) in
      (pos.line, pos.character)
  | None -> failwith ("substring not found: " ^ needle)

let get_hover ~(needle : string) ?(occurrence = 1) ?(offset_in_needle = 0) () =
  let line, character = position_of_substring ~source:scenario_source ~needle ~occurrence ~offset_in_needle () in
  Hover.hover_info scenario_source line character

let get_signature ~(needle : string) ?(occurrence = 1) ?(offset_in_needle = 0) () =
  let line, character = position_of_substring ~source:scenario_source ~needle ~occurrence ~offset_in_needle () in
  Signature_help.check_sig scenario_source line character

let get_semantic_tokens source =
  let result = Doc_state.analyze ~source in
  match (result.program, result.type_map, result.environment) with
  | Some prog, Some tm, Some env -> Semantic_tokens.compute ~source ~program:prog ~type_map:tm ~environment:env
  | _ -> None

type decoded_token = {
  line : int;
  character : int;
  length : int;
  token_type : int;
  modifiers : int;
}

let decode_tokens ~(source : string) = function
  | Some (st : Lsp_t.SemanticTokens.t) ->
      let len = Array.length st.data in
      let rec go i prev_line prev_char acc =
        if i + 4 >= len then
          List.rev acc
        else
          let delta_line = st.data.(i) in
          let delta_char = st.data.(i + 1) in
          let length = st.data.(i + 2) in
          let token_type = st.data.(i + 3) in
          let modifiers = st.data.(i + 4) in
          let line = prev_line + delta_line in
          let character =
            if delta_line = 0 then
              prev_char + delta_char
            else
              delta_char
          in
          go (i + 5) line character
            ({ line; character; length; token_type; modifiers } :: acc)
      in
      let _ = source in
      go 0 0 0 []
  | None -> []

let token_under_cursor ~(needle : string) ?(occurrence = 1) ?(offset_in_needle = 0) () =
  let line, character = position_of_substring ~source:scenario_source ~needle ~occurrence ~offset_in_needle () in
  decode_tokens ~source:scenario_source (get_semantic_tokens scenario_source)
  |> List.find_opt (fun token ->
         token.line = line
         && token.character <= character
         && character < token.character + token.length)

let symbol_names symbols = List.map (fun (s : Lsp_t.DocumentSymbol.t) -> s.name) symbols

let get_hints source = Inlay_hints.get_hints source

let hint_labels hints =
  List.map
    (fun (h : Lsp_t.InlayHint.t) ->
      match h.label with
      | `String s -> s
      | _ -> "")
    hints

let hint_at_position ~(source : string) ~(line : int) ~(character : int) =
  get_hints source
  |> List.find_opt (fun (hint : Lsp_t.InlayHint.t) ->
         hint.position.line = line && hint.position.character = character)

let hint_at_substring ~(source : string) ~(needle : string) ?(occurrence = 1) ?offset_in_needle () =
  let offset_in_needle = Option.value offset_in_needle ~default:(String.length needle) in
  let line, character = position_of_substring ~source ~needle ~occurrence ~offset_in_needle () in
  hint_at_position ~source ~line ~character

let%test "editor scenario analyzes cleanly" =
  let result = Doc_state.analyze ~source:scenario_source in
  result.diagnostics = [] && result.program <> None && result.type_map <> None && result.environment <> None

let%test "editor scenario hover keeps top-level second param precise" =
  match get_hover ~needle:"fallback: Str" () with
  | Some h -> h.type_text = "fallback: Str" && h.highlighted = "fallback"
  | None -> false

let%test "editor scenario hover keeps trait impl second param precise" =
  match get_hover ~needle:"prefix: Str) -> Str = prefix + self.name" () with
  | Some h -> h.type_text = "prefix: Str" && h.highlighted = "prefix"
  | None -> false

let%test "editor scenario hover keeps inherent impl return type precise" =
  match get_hover ~needle:"-> Monkey = self" ~offset_in_needle:3 () with
  | Some h -> h.type_text = "Monkey" && h.highlighted = "Monkey"
  | None -> false

let%test "editor scenario hover keeps shape field type precise" =
  match get_hover ~needle:"age: Int" ~offset_in_needle:5 () with
  | Some h -> h.type_text = "Int" && h.highlighted = "Int"
  | None -> false

let%test "editor scenario signature help keeps second active parameter" =
  match get_signature ~needle:"\"!\"" () with
  | Some sh -> (
      match (sh.activeParameter, sh.signatures) with
      | Some (Some 1), sig0 :: _ ->
          Diagnostics.String_utils.contains_substring ~needle:"print_book_name" sig0.label
          && Diagnostics.String_utils.contains_substring ~needle:"fallback: Str" sig0.label
      | _ -> false)
  | None -> false

let%test "editor scenario semantic tokens cover modern declarations and members" =
  let has_decl_mod token = token.modifiers land Semantic_tokens.declaration_mod <> 0 in
  let has_def_mod token = token.modifiers land Semantic_tokens.definition_mod <> 0 in
  match
    ( token_under_cursor ~needle:"Named = {" (),
      token_under_cursor ~needle:"Monkey = {" ~occurrence:1 (),
      token_under_cursor ~needle:"book: Map[Str, Str]" (),
      token_under_cursor ~needle:"fallback: Str" (),
      token_under_cursor ~needle:"greet(self: Monkey" (),
      token_under_cursor ~needle:"self.name" ~offset_in_needle:5 (),
      token_under_cursor ~needle:"bananas: 3" () )
  with
  | Some named, Some monkey, Some book_param, Some fallback_param, Some greet_impl, Some field_access, Some record_field
    ->
      named.token_type = Semantic_tokens.interface_type
      && has_decl_mod named
      && monkey.token_type = Semantic_tokens._type_type
      && has_decl_mod monkey
      && book_param.token_type = Semantic_tokens.parameter_type
      && has_decl_mod book_param
      && fallback_param.token_type = Semantic_tokens.parameter_type
      && has_decl_mod fallback_param
      && greet_impl.token_type = Semantic_tokens.method_type
      && has_def_mod greet_impl
      && field_access.token_type = Semantic_tokens.property_type
      && record_field.token_type = Semantic_tokens.property_type
  | _ -> false

let%test "editor scenario document symbols reflect mixed declaration surface" =
  let symbols = Doc_symbols.get_symbols scenario_source in
  let names = symbol_names symbols in
  List.mem "Option" names
  && List.mem "Named" names
  && List.mem "Greeter" names
  && List.mem "Monkey" names
  && List.mem "impl Greeter[Monkey]" names
  && List.mem "impl Monkey" names
  && List.mem "display_name" names
  && List.mem "print_book_name" names

let%test "editor inlay scenario keeps annotated params quiet while surfacing local inference" =
  match
    ( hint_at_substring ~source:inlay_scenario_source ~needle:"let y = x + 1" ~offset_in_needle:5 (),
      hint_at_substring ~source:inlay_scenario_source ~needle:"(z)" ~offset_in_needle:2 (),
      hint_at_substring ~source:inlay_scenario_source ~needle:"x: Int" ~offset_in_needle:1 (),
      hint_at_substring ~source:inlay_scenario_source ~needle:"(z)" ~offset_in_needle:3 () )
  with
  | Some y_hint, Some z_hint, None, Some add_return_hint ->
      let labels = hint_labels [ y_hint; z_hint; add_return_hint ] in
      List.mem ": Int" labels
      && List.length (List.filter (fun label -> label = ": Int") labels) = 2
      && List.mem " -> Int" labels
  | _ -> false
