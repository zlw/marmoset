(* Enum Registry - tracks defined enums and their variants *)

open Types

type variant_def = {
  name : string;
  fields : mono_type list;
}

type enum_def = {
  name : string;
  type_params : string list;
  variants : variant_def list;
}

(* Global mutable registry *)
let registry : (string, enum_def) Hashtbl.t = Hashtbl.create 16
let clear () = Hashtbl.clear registry
let register (def : enum_def) : unit = Hashtbl.replace registry def.name def
let lookup (name : string) : enum_def option = Hashtbl.find_opt registry name

let lookup_variant (enum_name : string) (variant_name : string) : variant_def option =
  match lookup enum_name with
  | None -> None
  | Some def -> List.find_opt (fun (v : variant_def) -> v.name = variant_name) def.variants

(* Get constructor type for a variant *)
let variant_type (enum_name : string) (variant_name : string) (type_args : mono_type list) : mono_type option =
  match lookup enum_name with
  | None -> None
  | Some def -> (
      match List.find_opt (fun (v : variant_def) -> v.name = variant_name) def.variants with
      | None -> None
      | Some variant ->
          (* Create substitution from type params to type args *)
          let subst = List.combine def.type_params type_args in

          (* Substitute in variant field types *)
          let result_type = TEnum (enum_name, type_args) in

          if variant.fields = [] then
            (* Nullary: just the enum type *)
            Some result_type
          else
            (* Function from fields to enum *)
            let field_types = List.map (apply_substitution subst) variant.fields in
            let fn_type = List.fold_right (fun field ret -> TFun (field, ret)) field_types result_type in
            Some fn_type)

(* Register builtins *)
let init_builtins () =
  clear ();

  (* option[a] = some(a) | none *)
  register
    {
      name = "option";
      type_params = [ "a" ];
      variants = [ { name = "some"; fields = [ TVar "a" ] }; { name = "none"; fields = [] } ];
    };

  (* result[a, e] = success(a) | failure(e) *)
  register
    {
      name = "result";
      type_params = [ "a"; "e" ];
      variants = [ { name = "success"; fields = [ TVar "a" ] }; { name = "failure"; fields = [ TVar "e" ] } ];
    }

(* Tests *)

let%test "register and lookup enum" =
  clear ();
  register
    {
      name = "direction";
      type_params = [];
      variants = [ { name = "north"; fields = [] }; { name = "south"; fields = [] } ];
    };
  match lookup "direction" with
  | None -> false
  | Some def -> def.name = "direction" && List.length def.variants = 2

let%test "lookup_variant finds variant" =
  clear ();
  register
    {
      name = "option";
      type_params = [ "a" ];
      variants = [ { name = "some"; fields = [ TVar "a" ] }; { name = "none"; fields = [] } ];
    };
  match lookup_variant "option" "some" with
  | None -> false
  | Some v -> v.name = "some" && List.length v.fields = 1

let%test "lookup_variant returns none for unknown" =
  clear ();
  register { name = "option"; type_params = [ "a" ]; variants = [ { name = "some"; fields = [ TVar "a" ] } ] };
  lookup_variant "option" "none" = None

let%test "variant_type for nullary constructor" =
  clear ();
  register { name = "option"; type_params = [ "a" ]; variants = [ { name = "none"; fields = [] } ] };
  match variant_type "option" "none" [ TInt ] with
  | None -> false
  | Some t -> t = TEnum ("option", [ TInt ])

let%test "variant_type for unary constructor" =
  clear ();
  register { name = "option"; type_params = [ "a" ]; variants = [ { name = "some"; fields = [ TVar "a" ] } ] };
  match variant_type "option" "some" [ TInt ] with
  | None -> false
  | Some t -> t = TFun (TInt, TEnum ("option", [ TInt ]))

let%test "init_builtins registers option and result" =
  init_builtins ();
  lookup "option" <> None && lookup "result" <> None
