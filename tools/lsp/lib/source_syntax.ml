module Ast = Marmoset.Lib.Ast
module Types = Marmoset.Lib.Types

type type_var_user_name_map = (string * string) list

let canonical_type_name name =
  let capitalize_segment segment =
    if segment = "" then
      ""
    else
      String.uppercase_ascii (String.sub segment 0 1)
      ^ String.sub segment 1 (String.length segment - 1)
  in
  match name with
  | "int" | "Int" -> "Int"
  | "float" | "Float" -> "Float"
  | "bool" | "Bool" -> "Bool"
  | "string" | "String" | "Str" -> "Str"
  | "unit" | "Unit" | "null" | "Null" -> "Unit"
  | "list" | "List" -> "List"
  | "map" | "Map" -> "Map"
  | "option" | "Option" -> "Option"
  | "result" | "Result" -> "Result"
  | _ when name = "" -> name
  | _ when Char.uppercase_ascii name.[0] = name.[0] -> name
  | _ -> String.concat "" (List.map capitalize_segment (String.split_on_char '_' name))

let resolve_var_name ~(type_var_user_names : type_var_user_name_map) (name : string) : string =
  match List.assoc_opt name type_var_user_names with
  | Some user_name -> user_name
  | None -> name

let collapse_purity_union (types : Types.mono_type list) : Types.mono_type list =
  let dominated = Hashtbl.create 8 in
  List.iter
    (fun a ->
      List.iter
        (fun b ->
          match (a, b) with
          | Types.TFun (arg1, ret1, false), Types.TFun (arg2, ret2, true) when arg1 = arg2 && ret1 = ret2 ->
              Hashtbl.replace dominated (Types.to_string a) true
          | _ -> ())
        types)
    types;
  List.filter (fun t -> not (Hashtbl.mem dominated (Types.to_string t))) types

let rec mono_type_to_source ?(type_var_user_names = []) (mono : Types.mono_type) : string =
  match mono with
  | Types.TInt -> "Int"
  | Types.TFloat -> "Float"
  | Types.TBool -> "Bool"
  | Types.TString -> "Str"
  | Types.TNull -> "Unit"
  | Types.TVar name -> resolve_var_name ~type_var_user_names name
  | Types.TRowVar name -> resolve_var_name ~type_var_user_names name
  | Types.TArray element -> "List[" ^ mono_type_to_source ~type_var_user_names element ^ "]"
  | Types.THash (key, value) ->
      "Map[" ^ mono_type_to_source ~type_var_user_names key ^ ", " ^ mono_type_to_source ~type_var_user_names value
      ^ "]"
  | Types.TFun _ ->
      let rec collect_args eff = function
        | Types.TFun (arg, rest, e) ->
            let args, ret, eff' = collect_args (eff || e) rest in
            (arg :: args, ret, eff')
        | t -> ([], t, eff)
      in
      let args, ret, is_eff = collect_args false mono in
      let args_str =
        match args with
        | [ single ] -> "(" ^ mono_type_to_source_parens ~type_var_user_names single ^ ")"
        | _ -> "(" ^ String.concat ", " (List.map (mono_type_to_source ~type_var_user_names) args) ^ ")"
      in
      let arrow =
        if is_eff then
          " => "
        else
          " -> "
      in
      args_str ^ arrow ^ mono_type_to_source ~type_var_user_names ret
  | Types.TRecord (fields, row) ->
      let field_strs =
        List.map
          (fun (f : Types.record_field_type) -> f.name ^ ": " ^ mono_type_to_source ~type_var_user_names f.typ)
          fields
      in
      let row_str =
        match row with
        | None -> ""
        | Some r ->
            if field_strs = [] then
              "..." ^ mono_type_to_source ~type_var_user_names r
            else
              ", ..." ^ mono_type_to_source ~type_var_user_names r
      in
      "{ " ^ String.concat ", " field_strs ^ row_str ^ " }"
  | Types.TUnion types -> (
      match collapse_purity_union types with
      | [ single ] -> mono_type_to_source ~type_var_user_names single
      | many -> String.concat " | " (List.map (mono_type_to_source ~type_var_user_names) many))
  | Types.TTraitObject traits ->
      "Dyn[" ^ String.concat " & " (Types.normalize_trait_object_traits traits) ^ "]"
  | Types.TEnum (name, []) -> canonical_type_name name
  | Types.TEnum (name, args) ->
      canonical_type_name name ^ "["
      ^ String.concat ", " (List.map (mono_type_to_source ~type_var_user_names) args)
      ^ "]"

and mono_type_to_source_parens ?(type_var_user_names = []) = function
  | Types.TFun _ as t -> "(" ^ mono_type_to_source ~type_var_user_names t ^ ")"
  | t -> mono_type_to_source ~type_var_user_names t

let normalize_mono_type_with_user_names ~(type_var_user_names : type_var_user_name_map) (mono : Types.mono_type) :
    Types.mono_type =
  let vars = Types.unique_in_order (Types.collect_vars_in_order mono) in
  let renaming =
    List.mapi
      (fun i old_name ->
        match List.assoc_opt old_name type_var_user_names with
        | Some user_name ->
            if String.equal user_name old_name then
              None
            else
              Some (old_name, Types.TVar user_name)
        | None ->
            let nice = Types.nice_var_name i in
            if String.equal nice old_name then
              None
            else
              Some (old_name, Types.TVar nice))
      vars
  in
  let renaming = Types.substitution_of_list (List.filter_map Fun.id renaming) in
  Types.apply_substitution renaming mono

let format_poly_binding
    ~(type_var_user_names : type_var_user_name_map)
    ~(name : string)
    (Types.Forall (vars, mono)) : string =
  let norm_mono = normalize_mono_type_with_user_names ~type_var_user_names mono in
  let type_str = mono_type_to_source ~type_var_user_names norm_mono in
  let display_vars =
    List.map
      (fun v ->
        match List.assoc_opt v type_var_user_names with
        | Some user_name -> user_name
        | None -> v)
      vars
  in
  match display_vars with
  | [] -> Printf.sprintf "%s: %s" name type_str
  | _ -> Printf.sprintf "%s[%s]: %s" name (String.concat ", " display_vars) type_str

let poly_type_detail (Types.Forall (vars, mono)) : string =
  let norm = Types.normalize mono in
  let type_str = mono_type_to_source norm in
  match vars with
  | [] -> type_str
  | _ ->
      let norm_vars = List.mapi (fun i _ -> Types.nice_var_name i) vars in
      Printf.sprintf "[%s]: %s" (String.concat ", " norm_vars) type_str

let rec type_expr_to_source (te : Ast.AST.type_expr) : string =
  match te with
  | Ast.AST.TVar name -> name
  | Ast.AST.TCon name -> canonical_type_name name
  | Ast.AST.TApp (name, args) ->
      Printf.sprintf "%s[%s]" (canonical_type_name name) (String.concat ", " (List.map type_expr_to_source args))
  | Ast.AST.TArrow (params, ret, effectful) ->
      Printf.sprintf "(%s) %s %s"
        (String.concat ", " (List.map type_expr_to_source params))
        (if effectful then
           "=>"
         else
           "->")
        (type_expr_to_source ret)
  | Ast.AST.TUnion types -> String.concat " | " (List.map type_expr_to_source types)
  | Ast.AST.TTraitObject traits -> "Dyn[" ^ String.concat " & " traits ^ "]"
  | Ast.AST.TRecord (fields, row) ->
      let field_strs =
        List.map
          (fun (f : Ast.AST.record_type_field) -> f.field_name ^ ": " ^ type_expr_to_source f.field_type)
          fields
      in
      let row_str =
        match row with
        | None -> ""
        | Some r ->
            if field_strs = [] then
              "..." ^ type_expr_to_source r
            else
              ", ..." ^ type_expr_to_source r
      in
      "{ " ^ String.concat ", " field_strs ^ row_str ^ " }"

let%test "mono_type_to_source renders Dyn" =
  mono_type_to_source (Types.TTraitObject [ "Show"; "Eq" ]) = "Dyn[Eq & Show]"

let%test "type_expr_to_source renders Dyn" =
  type_expr_to_source (Ast.AST.TTraitObject [ "Show"; "Render" ]) = "Dyn[Show & Render]"
