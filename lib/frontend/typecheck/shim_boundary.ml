open Types

module Diagnostic = Diagnostics.Diagnostic

type enum_identity = {
  enum_name : string;
  enum_type_args : boundary_type list;
}

and extern_type_identity = {
  extern_type_name : string;
  owner_module_id : string;
}

and boundary_type =
  | BUnit
  | BBool
  | BInt
  | BFloat
  | BStr
  | BStdOption of boundary_type
  | BStdResult of boundary_type * boundary_type
  | BOwnerEnum of enum_identity
  | BStdBytes
  | BExternHandle of extern_type_identity

let option_enum_name = "std__option__Option"
let result_enum_name = "std__result__Result"

let rec equal_boundary_type a b =
  match (a, b) with
  | BUnit, BUnit | BBool, BBool | BInt, BInt | BFloat, BFloat | BStr, BStr | BStdBytes, BStdBytes -> true
  | BStdOption a, BStdOption b -> equal_boundary_type a b
  | BStdResult (a_ok, a_err), BStdResult (b_ok, b_err) ->
      equal_boundary_type a_ok b_ok && equal_boundary_type a_err b_err
  | BOwnerEnum a, BOwnerEnum b ->
      String.equal a.enum_name b.enum_name
      && List.length a.enum_type_args = List.length b.enum_type_args
      && List.for_all2 equal_boundary_type a.enum_type_args b.enum_type_args
  | BExternHandle a, BExternHandle b ->
      String.equal a.extern_type_name b.extern_type_name && String.equal a.owner_module_id b.owner_module_id
  | _ -> false

let rec to_string = function
  | BUnit -> "Unit"
  | BBool -> "Bool"
  | BInt -> "Int"
  | BFloat -> "Float"
  | BStr -> "Str"
  | BStdOption inner -> Printf.sprintf "Option[%s]" (to_string inner)
  | BStdResult (ok_type, err_type) -> Printf.sprintf "Result[%s, %s]" (to_string ok_type) (to_string err_type)
  | BOwnerEnum enum ->
      if enum.enum_type_args = [] then
        enum.enum_name
      else
        Printf.sprintf "%s[%s]" enum.enum_name
          (String.concat ", " (List.map to_string enum.enum_type_args))
  | BStdBytes -> "Bytes"
  | BExternHandle handle -> handle.extern_type_name

let rec to_mono_type = function
  | BUnit -> TNull
  | BBool -> TBool
  | BInt -> TInt
  | BFloat -> TFloat
  | BStr -> TString
  | BStdOption inner -> TEnum ("Option", [ to_mono_type inner ])
  | BStdResult (ok_type, err_type) -> TEnum ("Result", [ to_mono_type ok_type; to_mono_type err_type ])
  | BOwnerEnum enum -> TEnum (enum.enum_name, List.map to_mono_type enum.enum_type_args)
  | BStdBytes -> TNamed ("std__bytes__Bytes", [])
  | BExternHandle handle -> TNamed (handle.extern_type_name, [])

let starts_with ~(prefix : string) (s : string) : bool =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len && String.sub s 0 prefix_len = prefix

let owner_internal_prefix (owner_module_id : string) : string =
  String.split_on_char '.' owner_module_id |> String.concat "__"

let is_owner_enum ~(owner_module_id : string) (enum_name : string) : bool =
  starts_with ~prefix:(owner_internal_prefix owner_module_id ^ "__") enum_name

let extern_handle_owner_matches ~(owner_module_id : string) (type_name : string) : bool =
  match Type_registry.extern_type_owner_module_id type_name with
  | Some owner -> String.equal owner owner_module_id
  | None -> false

let unsupported ?source_span ~(typ : mono_type) () =
  let message = Printf.sprintf "unsupported shim boundary type %s" (Types.to_string typ) in
  let code = "type-shim-boundary" in
  match source_span with
  | Some (Diagnostic.Span { file_id; start_pos; end_pos }) ->
      Diagnostic.error_with_span ~code ~message ~file_id ~start_pos ?end_pos ()
  | Some Diagnostic.NoSpan | None -> Diagnostic.error_no_span ~code ~message

let rec classify ?source_span ~(owner_module_id : string) (typ : mono_type) :
    (boundary_type, Diagnostic.t) result =
  let typ = canonicalize_mono_type typ in
  match typ with
  | TNull -> Ok BUnit
  | TBool -> Ok BBool
  | TInt -> Ok BInt
  | TFloat -> Ok BFloat
  | TString -> Ok BStr
  | TEnum (name, [ inner ]) when String.equal name option_enum_name ->
      Result.map (fun classified -> BStdOption classified) (classify ?source_span ~owner_module_id inner)
  | TEnum (name, [ ok_type; err_type ]) when String.equal name result_enum_name ->
      let ( let* ) = Result.bind in
      let* ok_boundary = classify ?source_span ~owner_module_id ok_type in
      let* err_boundary = classify ?source_span ~owner_module_id err_type in
      Ok (BStdResult (ok_boundary, err_boundary))
  | TEnum (name, args) when is_owner_enum ~owner_module_id name ->
      let ( let* ) = Result.bind in
      let rec classify_args acc = function
        | [] -> Ok (List.rev acc)
        | arg :: rest ->
            let* classified = classify ?source_span ~owner_module_id arg in
            classify_args (classified :: acc) rest
      in
      let* enum_type_args = classify_args [] args in
      Ok (BOwnerEnum { enum_name = name; enum_type_args })
  | TNamed (name, []) when Type_registry.is_extern_type_name name ->
      if extern_handle_owner_matches ~owner_module_id name then
        Ok
          (BExternHandle
             {
               extern_type_name = name;
               owner_module_id =
                 Option.value (Type_registry.extern_type_owner_module_id name) ~default:owner_module_id;
             })
      else
        Error (unsupported ?source_span ~typ ())
  | _ -> Error (unsupported ?source_span ~typ ())

let%test "classifies scalar boundary types" =
  classify ~owner_module_id:"test.scalar" TInt = Ok BInt
  && classify ~owner_module_id:"test.scalar" TFloat = Ok BFloat
  && classify ~owner_module_id:"test.scalar" TBool = Ok BBool
  && classify ~owner_module_id:"test.scalar" TString = Ok BStr
  && classify ~owner_module_id:"test.scalar" TNull = Ok BUnit

let%test "classifies canonical option and result identities" =
  match
    classify ~owner_module_id:"test.scalar"
      (TEnum (result_enum_name, [ TEnum (option_enum_name, [ TInt ]); TString ]))
  with
  | Ok (BStdResult (BStdOption BInt, BStr)) -> true
  | _ -> false

let%test "rejects impostor option identity" =
  match classify ~owner_module_id:"test.scalar" (TEnum ("Option", [ TInt ])) with
  | Error diag -> diag.code = "type-shim-boundary"
  | Ok _ -> false
