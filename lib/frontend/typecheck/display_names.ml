let unescape_internal_component (name : string) : string =
  let buffer = Buffer.create (String.length name) in
  let is_hex = function
    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
    | _ -> false
  in
  let rec go idx =
    if idx >= String.length name then
      ()
    else if
      idx + 5 < String.length name
      && name.[idx] = '_'
      && name.[idx + 1] = 'u'
      && is_hex name.[idx + 2]
      && is_hex name.[idx + 3]
      && is_hex name.[idx + 4]
      && is_hex name.[idx + 5]
    then (
      let hex = String.sub name (idx + 2) 4 in
      match int_of_string_opt ("0x" ^ hex) with
      | Some code when code >= 0 && code <= 255 ->
          Buffer.add_char buffer (Char.chr code);
          go (idx + 6)
      | _ ->
          Buffer.add_string buffer (String.sub name idx 6);
          go (idx + 6))
    else (
      Buffer.add_char buffer name.[idx];
      go (idx + 1))
  in
  go 0;
  Buffer.contents buffer

let display_binding_name (name : string) : string =
  let len = String.length name in
  let rec find_suffix_start idx =
    if idx <= 0 then
      0
    else if name.[idx - 1] = '_' && name.[idx] = '_' then
      idx + 1
    else
      find_suffix_start (idx - 1)
  in
  let suffix_start = find_suffix_start (len - 1) in
  String.sub name suffix_start (len - suffix_start) |> unescape_internal_component

let split_internal_name (name : string) : string list =
  let len = String.length name in
  let rec go start idx acc =
    if idx >= len then
      List.rev (String.sub name start (len - start) :: acc)
    else if idx + 1 < len && name.[idx] = '_' && name.[idx + 1] = '_' then
      let part = String.sub name start (idx - start) in
      go (idx + 2) (idx + 2) (part :: acc)
    else
      go start (idx + 1) acc
  in
  go 0 0 []

let display_qualified_name (name : string) : string =
  split_internal_name name |> List.map unescape_internal_component |> String.concat "."

let display_type_name (name : string) : string =
  match name with
  | "std__prelude__Ordering" -> "Ordering"
  | "std__prelude__Eq" -> "Eq"
  | "std__prelude__Show" -> "Show"
  | "std__prelude__Debug" -> "Debug"
  | "std__prelude__Ord" -> "Ord"
  | "std__prelude__Hash" -> "Hash"
  | "std__prelude__Num" -> "Num"
  | "std__prelude__Rem" -> "Rem"
  | "std__prelude__Neg" -> "Neg"
  | "std__option__Option" -> "Option"
  | "std__result__Result" -> "Result"
  | _ -> display_qualified_name name

let display_trait_name ~(builtin_trait_internal_name : string -> string option) (trait_name : string) : string =
  let base_name = display_binding_name trait_name in
  match builtin_trait_internal_name base_name with
  | Some builtin_name -> builtin_name
  | None -> base_name

let%test "display_binding_name strips module prefixes and unmangles suffixes" =
  display_binding_name "std__result__Result_u0021" = "Result!"

let%test "display_type_name preserves module path and unmangles components" =
  display_type_name "std__io__Error" = "std.io.Error"
  && display_type_name "my_u002dmodule__Result_u0021" = "my-module.Result!"

let%test "display_type_name keeps implicit std core types unqualified" =
  display_type_name "std__result__Result" = "Result"
  && display_type_name "std__prelude__Show" = "Show"

let%test "display_trait_name restores builtin source-facing names" =
  display_trait_name
    ~builtin_trait_internal_name:(function
      | "show" -> Some "Show"
      | _ -> None)
    "show"
  = "Show"
