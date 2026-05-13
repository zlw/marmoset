(* Enum Registry - tracks defined enums and their variants *)

open Types

type variant_def = {
  name : string;
  fields : mono_type list;
  message : string option;
}

type enum_kind =
  | OrdinaryEnum
  | ErrorEnum

type enum_def = {
  name : string;
  source_name : string option;
  type_params : string list;
  variants : variant_def list;
  kind : enum_kind;
}

(* Global mutable registry *)
let registry : (string, enum_def) Hashtbl.t = Hashtbl.create 16
let clear () = Hashtbl.clear registry
let register (def : enum_def) : unit = Hashtbl.replace registry def.name def
let lookup (name : string) : enum_def option = Hashtbl.find_opt registry name

let source_name_of_internal_name (name : string) : string =
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
  String.sub name suffix_start (len - suffix_start)

let source_name (def : enum_def) : string = Option.value def.source_name ~default:(source_name_of_internal_name def.name)

let is_error_type_name (name : string) : bool =
  let has_suffix suffix =
    let name_len = String.length name and suffix_len = String.length suffix in
    name_len >= suffix_len && String.sub name (name_len - suffix_len) suffix_len = suffix
  in
  String.equal name "Error"
  || (String.length name > String.length "Error" && has_suffix "Error")

let is_error_enum (name : string) : bool =
  match lookup name with
  | Some { kind = ErrorEnum; _ } -> true
  | _ -> false

let lookup_variant (enum_name : string) (variant_name : string) : variant_def option =
  match lookup enum_name with
  | None -> None
  | Some def -> List.find_opt (fun (v : variant_def) -> v.name = variant_name) def.variants

(* Get constructor type for a variant *)
let variant_type (enum_name : string) (variant_name : string) (type_args : mono_type list) : mono_type option =
  match lookup enum_name with
  | None -> None
  | Some def -> (
      match lookup_variant enum_name variant_name with
      | None -> None
      | Some variant ->
          (* Create substitution from type params to type args *)
          let subst = substitution_of_list (List.combine def.type_params type_args) in

          (* Substitute in variant field types *)
          let result_type = TEnum (enum_name, type_args) in

          if variant.fields = [] then
            (* Nullary: just the enum type *)
            Some result_type
          else
            (* Function from fields to enum *)
            let field_types = List.map (apply_substitution subst) variant.fields in
            let fn_type = List.fold_right (fun field ret -> tfun field ret) field_types result_type in
            Some fn_type)

(* Register builtins *)
let init_builtins () =
  clear ();

  (* Option[a] = Some(a) | None *)
  register
    {
      name = "Option";
      source_name = Some "Option";
      type_params = [ "a" ];
      kind = OrdinaryEnum;
      variants =
        [
          { name = "Some"; fields = [ TVar "a" ]; message = None };
          { name = "None"; fields = []; message = None };
        ];
    };

  (* Result[a, e] = Success(a) | Failure(e) *)
  register
    {
      name = "Result";
      source_name = Some "Result";
      type_params = [ "a"; "e" ];
      kind = OrdinaryEnum;
      variants =
        [
          { name = "Success"; fields = [ TVar "a" ]; message = None };
          { name = "Failure"; fields = [ TVar "e" ]; message = None };
        ];
    }

(* Tests *)

let%test "register and lookup enum" =
  clear ();
  register
    {
      name = "direction";
      source_name = Some "direction";
      type_params = [];
      kind = OrdinaryEnum;
      variants = [ { name = "north"; fields = []; message = None }; { name = "south"; fields = []; message = None } ];
    };
  match lookup "direction" with
  | None -> false
  | Some def -> def.name = "direction" && List.length def.variants = 2

let%test "lookup_variant finds variant" =
  clear ();
  register
    {
      name = "Option";
      source_name = Some "Option";
      type_params = [ "a" ];
      kind = OrdinaryEnum;
      variants =
        [
          { name = "Some"; fields = [ TVar "a" ]; message = None };
          { name = "None"; fields = []; message = None };
        ];
    };
  match lookup_variant "Option" "Some" with
  | None -> false
  | Some v -> v.name = "Some" && List.length v.fields = 1

let%test "lookup_variant returns none for unknown" =
  clear ();
  register
    {
      name = "Option";
      source_name = Some "Option";
      type_params = [ "a" ];
      kind = OrdinaryEnum;
      variants = [ { name = "Some"; fields = [ TVar "a" ]; message = None } ];
    };
  lookup_variant "Option" "None" = None

let%test "lookup_variant does not accept lowercase builtin variant aliases" =
  clear ();
  register
    {
      name = "Ordering";
      source_name = Some "Ordering";
      type_params = [];
      kind = OrdinaryEnum;
      variants =
        [
          { name = "Less"; fields = []; message = None };
          { name = "Equal"; fields = []; message = None };
          { name = "Greater"; fields = []; message = None };
        ];
    };
  lookup_variant "Ordering" "less" = None

let%test "variant_type for nullary constructor" =
  clear ();
  register
    {
      name = "Option";
      source_name = Some "Option";
      type_params = [ "a" ];
      kind = OrdinaryEnum;
      variants = [ { name = "None"; fields = []; message = None } ];
    };
  match variant_type "Option" "None" [ TInt ] with
  | None -> false
  | Some t -> t = TEnum ("Option", [ TInt ])

let%test "variant_type for unary constructor" =
  clear ();
  register
    {
      name = "Option";
      source_name = Some "Option";
      type_params = [ "a" ];
      kind = OrdinaryEnum;
      variants = [ { name = "Some"; fields = [ TVar "a" ]; message = None } ];
    };
  match variant_type "Option" "Some" [ TInt ] with
  | None -> false
  | Some t -> t = tfun TInt (TEnum ("Option", [ TInt ]))

let%test "init_builtins registers option and result" =
  init_builtins ();
  lookup "Option" <> None && lookup "Result" <> None
