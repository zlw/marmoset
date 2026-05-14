open Types
module Diagnostic = Diagnostics.Diagnostic

type enum_identity = {
  enum_name : string;
  enum_type_args : boundary_type list;
}

and wrapper_identity = {
  wrapper_name : string;
  wrapper_type_args : boundary_type list;
  wrapper_payloads : boundary_type list;
}

and extern_type_identity = {
  extern_type_name : string;
  owner_module_id : string;
}

and callback_type = {
  callback_params : boundary_type list;
  callback_return : boundary_type;
  callback_effectful : bool;
}

and boundary_type =
  | BUnit
  | BBool
  | BInt
  | BFloat
  | BStr
  | BStdOption of boundary_type
  | BStdResult of boundary_type * boundary_type
  | BList of boundary_type
  | BOwnerEnum of enum_identity
  | BNamedWrapper of wrapper_identity
  | BStdBytes
  | BExternHandle of extern_type_identity
  | BCallback of callback_type

let option_enum_name = "std__option__Option"
let result_enum_name = "std__result__Result"
let std_bytes_type_name = "std__bytes__Bytes"

let rec equal_boundary_type a b =
  match (a, b) with
  | BUnit, BUnit | BBool, BBool | BInt, BInt | BFloat, BFloat | BStr, BStr | BStdBytes, BStdBytes -> true
  | BStdOption a, BStdOption b -> equal_boundary_type a b
  | BStdResult (a_ok, a_err), BStdResult (b_ok, b_err) ->
      equal_boundary_type a_ok b_ok && equal_boundary_type a_err b_err
  | BList a, BList b -> equal_boundary_type a b
  | BOwnerEnum a, BOwnerEnum b ->
      String.equal a.enum_name b.enum_name
      && List.length a.enum_type_args = List.length b.enum_type_args
      && List.for_all2 equal_boundary_type a.enum_type_args b.enum_type_args
  | BNamedWrapper a, BNamedWrapper b ->
      String.equal a.wrapper_name b.wrapper_name
      && List.length a.wrapper_type_args = List.length b.wrapper_type_args
      && List.for_all2 equal_boundary_type a.wrapper_type_args b.wrapper_type_args
      && List.length a.wrapper_payloads = List.length b.wrapper_payloads
      && List.for_all2 equal_boundary_type a.wrapper_payloads b.wrapper_payloads
  | BExternHandle a, BExternHandle b ->
      String.equal a.extern_type_name b.extern_type_name && String.equal a.owner_module_id b.owner_module_id
  | BCallback a, BCallback b ->
      Bool.equal a.callback_effectful b.callback_effectful
      && List.length a.callback_params = List.length b.callback_params
      && List.for_all2 equal_boundary_type a.callback_params b.callback_params
      && equal_boundary_type a.callback_return b.callback_return
  | _ -> false

let rec to_string = function
  | BUnit -> "Unit"
  | BBool -> "Bool"
  | BInt -> "Int"
  | BFloat -> "Float"
  | BStr -> "Str"
  | BStdOption inner -> Printf.sprintf "Option[%s]" (to_string inner)
  | BStdResult (ok_type, err_type) -> Printf.sprintf "Result[%s, %s]" (to_string ok_type) (to_string err_type)
  | BList inner -> Printf.sprintf "List[%s]" (to_string inner)
  | BOwnerEnum enum ->
      if enum.enum_type_args = [] then
        enum.enum_name
      else
        Printf.sprintf "%s[%s]" enum.enum_name (String.concat ", " (List.map to_string enum.enum_type_args))
  | BNamedWrapper wrapper ->
      if wrapper.wrapper_type_args = [] then
        wrapper.wrapper_name
      else
        Printf.sprintf "%s[%s]" wrapper.wrapper_name
          (String.concat ", " (List.map to_string wrapper.wrapper_type_args))
  | BStdBytes -> "Bytes"
  | BExternHandle handle -> handle.extern_type_name
  | BCallback callback ->
      let params =
        match callback.callback_params with
        | [ single ] -> to_string single
        | params -> "(" ^ String.concat ", " (List.map to_string params) ^ ")"
      in
      let arrow =
        if callback.callback_effectful then
          " => "
        else
          " -> "
      in
      params ^ arrow ^ to_string callback.callback_return

let rec to_mono_type = function
  | BUnit -> TNull
  | BBool -> TBool
  | BInt -> TInt
  | BFloat -> TFloat
  | BStr -> TString
  | BStdOption inner -> TEnum ("Option", [ to_mono_type inner ])
  | BStdResult (ok_type, err_type) -> TEnum ("Result", [ to_mono_type ok_type; to_mono_type err_type ])
  | BList inner -> TArray (to_mono_type inner)
  | BOwnerEnum enum -> TEnum (enum.enum_name, List.map to_mono_type enum.enum_type_args)
  | BNamedWrapper wrapper -> TNamed (wrapper.wrapper_name, List.map to_mono_type wrapper.wrapper_type_args)
  | BStdBytes -> TNamed (std_bytes_type_name, [])
  | BExternHandle handle -> TNamed (handle.extern_type_name, [])
  | BCallback callback ->
      let callback_effect = effect_of_bool callback.callback_effectful in
      List.fold_right
        (fun param acc -> TFun (to_mono_type param, acc, callback_effect))
        callback.callback_params
        (to_mono_type callback.callback_return)

let starts_with ~(prefix : string) (s : string) : bool =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len && String.sub s 0 prefix_len = prefix

let owner_internal_prefix (owner_module_id : string) : string =
  String.split_on_char '.' owner_module_id |> String.concat "__"

let is_owner_enum ~(owner_module_id : string) (enum_name : string) : bool =
  starts_with ~prefix:(owner_internal_prefix owner_module_id ^ "__") enum_name

let is_module_qualified_enum (enum_name : string) : bool =
  match Type_registry.owner_module_id_of_internal_name enum_name with
  | Some _ -> Option.is_some (Enum_registry.lookup enum_name)
  | None -> false

let is_named_wrapper (type_name : string) : bool =
  match Type_registry.lookup_named_type type_name with
  | Some { named_type_body = Type_registry.NamedWrapper _; _ } -> true
  | _ -> false

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

let flatten_callback_type (typ : mono_type) : mono_type list * mono_type * bool =
  let rec go acc effectful = function
    | TFun (arg, ret, eff) -> go (arg :: acc) (effectful || effect_is_effectful eff) ret
    | ret -> (List.rev acc, ret, effectful)
  in
  go [] false typ

let rec classify ?source_span ?(allow_callback = false) ~(owner_module_id : string) (typ : mono_type) :
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
  | TArray inner -> Result.map (fun classified -> BList classified) (classify ?source_span ~owner_module_id inner)
  | TEnum (name, args) when is_owner_enum ~owner_module_id name || is_module_qualified_enum name ->
      let ( let* ) = Result.bind in
      let rec classify_args acc = function
        | [] -> Ok (List.rev acc)
        | arg :: rest ->
            let* classified = classify ?source_span ~owner_module_id arg in
            classify_args (classified :: acc) rest
      in
      let* enum_type_args = classify_args [] args in
      Ok (BOwnerEnum { enum_name = name; enum_type_args })
  | TNamed (name, []) when String.equal name std_bytes_type_name -> Ok BStdBytes
  | TNamed (name, args) when is_named_wrapper name -> (
      let ( let* ) = Result.bind in
      let rec classify_types acc = function
        | [] -> Ok (List.rev acc)
        | typ :: rest ->
            let* classified = classify ?source_span ~owner_module_id typ in
            classify_types (classified :: acc) rest
      in
      match Type_registry.instantiate_named_wrapper_representation name args with
      | Some (Ok payload_types) ->
          let* wrapper_type_args = classify_types [] args in
          let* wrapper_payloads = classify_types [] payload_types in
          Ok (BNamedWrapper { wrapper_name = name; wrapper_type_args; wrapper_payloads })
      | Some (Error msg) ->
          let code = "type-shim-boundary" in
          Error (Diagnostic.error_no_span ~code ~message:msg)
      | None -> Error (unsupported ?source_span ~typ ()))
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
  | TFun _ when allow_callback ->
      let ( let* ) = Result.bind in
      let callback_params, callback_return, callback_effectful = flatten_callback_type typ in
      let rec classify_params acc = function
        | [] -> Ok (List.rev acc)
        | param :: rest ->
            let* classified = classify ?source_span ~owner_module_id param in
            classify_params (classified :: acc) rest
      in
      let* callback_params = classify_params [] callback_params in
      let* callback_return = classify ?source_span ~owner_module_id callback_return in
      Ok (BCallback { callback_params; callback_return; callback_effectful })
  | _ -> Error (unsupported ?source_span ~typ ())

let%test "classifies scalar boundary types" =
  classify ~owner_module_id:"std.bytes" TInt = Ok BInt
  && classify ~owner_module_id:"std.bytes" TFloat = Ok BFloat
  && classify ~owner_module_id:"std.bytes" TBool = Ok BBool
  && classify ~owner_module_id:"std.bytes" TString = Ok BStr
  && classify ~owner_module_id:"std.bytes" TNull = Ok BUnit

let%test "classifies canonical option and result identities" =
  match
    classify ~owner_module_id:"std.bytes"
      (TEnum (result_enum_name, [ TEnum (option_enum_name, [ TInt ]); TString ]))
  with
  | Ok (BStdResult (BStdOption BInt, BStr)) -> true
  | _ -> false

let%test "classifies immutable list boundaries" =
  match classify ~owner_module_id:"std.bytes" (TArray (TEnum (option_enum_name, [ TString ]))) with
  | Ok (BList (BStdOption BStr)) -> true
  | _ -> false

let%test "classifies canonical std bytes identity" =
  match classify ~owner_module_id:"std.bytes" (TNamed ("std__bytes__Bytes", [])) with
  | Ok BStdBytes -> true
  | _ -> false

let%test "classifies imported module enum payloads" =
  Enum_registry.register
    {
      Enum_registry.name = "std__bytes__DecodeError";
      source_name = Some "DecodeError";
      type_params = [];
      kind = Enum_registry.ErrorEnum;
      variants = [ { name = "InvalidUtf8"; fields = []; message = Some "Invalid UTF-8" } ];
    };
  match classify ~owner_module_id:"std.file" (TEnum ("std__bytes__DecodeError", [])) with
  | Ok (BOwnerEnum { enum_name = "std__bytes__DecodeError"; enum_type_args = [] }) -> true
  | _ -> false

let%test "classifies direct callback parameters only when allowed" =
  let callback = TFun (TString, TFun (TInt, TString, Pure), Pure) in
  match classify ~owner_module_id:"std.bytes" ~allow_callback:true callback with
  | Ok (BCallback { callback_params = [ BStr; BInt ]; callback_return = BStr; callback_effectful = false }) -> (
      match classify ~owner_module_id:"std.bytes" callback with
      | Error diag -> diag.code = "type-shim-boundary"
      | Ok _ -> false)
  | _ -> false

let%test "rejects impostor option identity" =
  match classify ~owner_module_id:"std.bytes" (TEnum ("Option", [ TInt ])) with
  | Error diag -> diag.code = "type-shim-boundary"
  | Ok _ -> false
