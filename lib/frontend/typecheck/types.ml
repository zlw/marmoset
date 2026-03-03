(* Type representation for Hindley-Milner type inference *)

(* A monotype - a type without quantifiers (no "forall") *)
type mono_type =
  | TInt (* Int *)
  | TFloat (* Float *)
  | TBool (* Bool *)
  | TString (* String *)
  | TNull (* Null / unit *)
  | TVar of string (* Type variable: 'a, 'b, etc. *)
  | TFun of mono_type * mono_type * bool (* Function: T -> U, bool = is_effectful (=> vs ->) *)
  | TArray of mono_type (* Array: [T] *)
  | THash of mono_type * mono_type (* Hash: {K: V} *)
  | TRecord of record_field_type list * mono_type option
    (* Record: {x: Int, y: String} or open record {x: Int, ...r} *)
  | TRowVar of string (* Row type variable *)
  | TUnion of mono_type list (* Union: Int | String | Bool *)
  | TEnum of string * mono_type list (* Enum: option[Int], result[String, Int] *)

and record_field_type = {
  name : string;
  typ : mono_type;
}

(* Convenience constructors for function types *)
let tfun arg ret = TFun (arg, ret, false)
let tfun_eff arg ret = TFun (arg, ret, true)

(* Canonicalize record fields by name. If duplicates exist, last write wins. *)
let normalize_record_fields (fields : record_field_type list) : record_field_type list =
  let tbl : (string, mono_type) Hashtbl.t = Hashtbl.create (max 16 (List.length fields)) in
  List.iter (fun (f : record_field_type) -> Hashtbl.replace tbl f.name f.typ) fields;
  Hashtbl.to_seq_keys tbl
  |> List.of_seq
  |> List.sort String.compare
  |> List.map (fun name ->
         match Hashtbl.find_opt tbl name with
         | Some typ -> { name; typ }
         | None -> failwith "normalize_record_fields: impossible missing key")

let rec canonicalize_mono_type (mono : mono_type) : mono_type =
  match mono with
  | TInt | TFloat | TBool | TString | TNull | TVar _ | TRowVar _ -> mono
  | TFun (arg, ret, eff) -> TFun (canonicalize_mono_type arg, canonicalize_mono_type ret, eff)
  | TArray element -> TArray (canonicalize_mono_type element)
  | THash (key, value) -> THash (canonicalize_mono_type key, canonicalize_mono_type value)
  | TRecord (fields, row) ->
      let canonical_fields =
        fields
        |> List.map (fun (f : record_field_type) -> { f with typ = canonicalize_mono_type f.typ })
        |> normalize_record_fields
      in
      let canonical_row = Option.map canonicalize_mono_type row in
      TRecord (canonical_fields, canonical_row)
  | TUnion types -> TUnion (List.map canonicalize_mono_type types)
  | TEnum (name, args) -> TEnum (name, List.map canonicalize_mono_type args)

(* A polytype (type scheme) - a type with "forall" quantifiers.
   Example: ∀a. a -> a  is  Forall(["a"], TFun(TVar "a", TVar "a"))
   
   The quantified variables are bound - they can be replaced with any type
   when the scheme is "instantiated" (used). *)
type poly_type = Forall of string list * mono_type

(* Convert type to string for display.
   For functions, we collect all args and display as (A, B, C) -> D
   instead of Haskell-style A -> B -> C -> D *)

(* Helper to wrap function types in parens when needed *)
let rec to_string_parens = function
  | TFun _ as t -> "(" ^ to_string t ^ ")"
  | t -> to_string t

and to_string = function
  | TInt -> "Int"
  | TFloat -> "Float"
  | TBool -> "Bool"
  | TString -> "String"
  | TNull -> "Unit"
  | TVar name -> name
  | TFun _ as t ->
      let rec collect_args eff = function
        | TFun (arg, rest, e) ->
            let args, ret, eff' = collect_args (eff || e) rest in
            (arg :: args, ret, eff')
        | t -> ([], t, eff)
      in
      let args, ret, eff = collect_args false t in
      let args_str =
        match args with
        | [ single ] -> to_string_parens single (* single arg: parens if it's a function *)
        | _ -> "(" ^ String.concat ", " (List.map to_string args) ^ ")"
      in
      let arrow =
        if eff then
          " => "
        else
          " -> "
      in
      args_str ^ arrow ^ to_string ret
  | TArray element -> "[" ^ to_string element ^ "]"
  | THash (key, value) -> "{" ^ to_string key ^ ": " ^ to_string value ^ "}"
  | TRecord (fields, row) ->
      let field_strs = List.map (fun f -> f.name ^ ": " ^ to_string f.typ) fields in
      let row_str =
        match row with
        | None -> ""
        | Some r ->
            if field_strs = [] then
              "..." ^ to_string r
            else
              ", ..." ^ to_string r
      in
      "{ " ^ String.concat ", " field_strs ^ row_str ^ " }"
  | TRowVar name -> name
  | TUnion types -> String.concat " | " (List.map to_string types)
  | TEnum (name, []) -> name
  | TEnum (name, args) -> name ^ "[" ^ String.concat ", " (List.map to_string args) ^ "]"

(* Convert poly_type to string *)
let poly_type_to_string (Forall (vars, mono)) =
  match vars with
  | [] -> to_string mono
  | _ -> "∀" ^ String.concat " " vars ^ ". " ^ to_string mono

(* Create a monomorphic poly_type (no quantified variables) *)
let mono_to_poly mono = Forall ([], mono)

(* ============================================================
   Shared modules for type variable sets and substitution maps
   ============================================================ *)

module TypeVarSet = Set.Make (String)
module SubstMap = Map.Make (String)

(* ============================================================
   Substitution: mapping from type variable names to types
   ============================================================ *)

type substitution = mono_type SubstMap.t

let empty_substitution : substitution = SubstMap.empty

let substitution_of_list (pairs : (string * mono_type) list) : substitution =
  List.fold_left (fun m (k, v) -> SubstMap.add k v m) SubstMap.empty pairs

let substitution_singleton (k : string) (v : mono_type) : substitution = SubstMap.singleton k v

(* Apply substitution to a type - replace TVars according to the map *)
let apply_substitution (subst : substitution) (mono : mono_type) : mono_type =
  let rec go (seen : TypeVarSet.t) (ty : mono_type) : mono_type =
    match ty with
    | TInt | TFloat | TBool | TString | TNull -> ty
    | TVar name -> (
        if TypeVarSet.mem name seen then
          ty
        else
          match SubstMap.find_opt name subst with
          | Some replacement -> go (TypeVarSet.add name seen) replacement
          | None -> ty)
    | TFun (arg, ret, eff) -> TFun (go seen arg, go seen ret, eff)
    | TArray element -> TArray (go seen element)
    | THash (key, value) -> THash (go seen key, go seen value)
    | TRecord (fields, row) ->
        let fields' =
          fields
          |> List.map (fun (f : record_field_type) -> { f with typ = go seen f.typ })
          |> normalize_record_fields
        in
        let row' = Option.map (go seen) row in
        TRecord (fields', row')
    | TRowVar name -> (
        if TypeVarSet.mem name seen then
          ty
        else
          match SubstMap.find_opt name subst with
          | Some replacement -> go (TypeVarSet.add name seen) replacement
          | None -> ty)
    | TUnion types -> TUnion (List.map (go seen) types)
    | TEnum (name, args) -> TEnum (name, List.map (go seen) args)
  in
  go TypeVarSet.empty mono

(* Apply substitution to a poly_type - don't touch quantified variables *)
let apply_substitution_poly (subst : substitution) (Forall (quantified_vars, mono)) : poly_type =
  (* Remove quantified vars from substitution before applying *)
  let filtered_subst =
    let bound = TypeVarSet.of_list quantified_vars in
    SubstMap.filter (fun name _ -> not (TypeVarSet.mem name bound)) subst
  in
  Forall (quantified_vars, apply_substitution filtered_subst mono)

(* Compose two substitutions: apply first_subst first, then second_subst *)
let compose_substitution (first_subst : substitution) (second_subst : substitution) : substitution =
  let first_with_second_applied = SubstMap.map (fun mono -> apply_substitution second_subst mono) first_subst in
  SubstMap.union (fun _k v1 _v2 -> Some v1) first_with_second_applied second_subst

(* ============================================================
   Free Type Variables: type variables not bound by a quantifier
   ============================================================ *)

(* Free type variables in a mono_type *)
let rec free_type_vars (mono : mono_type) : TypeVarSet.t =
  match mono with
  | TInt | TFloat | TBool | TString | TNull -> TypeVarSet.empty
  | TVar name -> TypeVarSet.singleton name
  | TFun (arg, ret, _) -> TypeVarSet.union (free_type_vars arg) (free_type_vars ret)
  | TArray element -> free_type_vars element
  | THash (key, value) -> TypeVarSet.union (free_type_vars key) (free_type_vars value)
  | TRecord (fields, row) ->
      let field_vars =
        List.fold_left (fun acc f -> TypeVarSet.union acc (free_type_vars f.typ)) TypeVarSet.empty fields
      in
      let row_vars =
        match row with
        | None -> TypeVarSet.empty
        | Some r -> free_type_vars r
      in
      TypeVarSet.union field_vars row_vars
  | TRowVar name -> TypeVarSet.singleton name
  | TUnion types -> List.fold_left (fun acc t -> TypeVarSet.union acc (free_type_vars t)) TypeVarSet.empty types
  | TEnum (_, args) -> List.fold_left (fun acc t -> TypeVarSet.union acc (free_type_vars t)) TypeVarSet.empty args

(* Free type variables in a poly_type - quantified vars are NOT free *)
let free_type_vars_poly (Forall (quantified_vars, mono)) : TypeVarSet.t =
  let bound = TypeVarSet.of_list quantified_vars in
  TypeVarSet.diff (free_type_vars mono) bound

(* Check if a type variable occurs in a type (for occurs check) *)
let occurs_in (var_name : string) (mono : mono_type) : bool = TypeVarSet.mem var_name (free_type_vars mono)

(* ============================================================
   Pretty printing with normalized type variable names
   ============================================================
   
   Renames type variables like t0, t1, t2 to a, b, c for nicer display.
   Variables are renamed in order of first appearance.
*)

(* Generate nice variable name from index: 0->a, 1->b, ..., 25->z, 26->a1, etc. *)
let nice_var_name (idx : int) : string =
  if idx < 26 then
    String.make 1 (Char.chr (Char.code 'a' + idx))
  else
    String.make 1 (Char.chr (Char.code 'a' + (idx mod 26))) ^ string_of_int (idx / 26)

(* Collect type variables in order of first appearance (left-to-right, depth-first) *)
let rec collect_vars_in_order (mono : mono_type) : string list =
  match mono with
  | TInt | TFloat | TBool | TString | TNull -> []
  | TVar name -> [ name ]
  | TFun (arg, ret, _) -> collect_vars_in_order arg @ collect_vars_in_order ret
  | TArray element -> collect_vars_in_order element
  | THash (key, value) -> collect_vars_in_order key @ collect_vars_in_order value
  | TRecord (fields, row) ->
      let field_vars = List.concat_map (fun f -> collect_vars_in_order f.typ) fields in
      let row_vars =
        match row with
        | None -> []
        | Some r -> collect_vars_in_order r
      in
      field_vars @ row_vars
  | TRowVar name -> [ name ]
  | TUnion types -> List.concat_map collect_vars_in_order types
  | TEnum (_, args) -> List.concat_map collect_vars_in_order args

(* Remove duplicates while preserving order *)
let unique_in_order (lst : string list) : string list =
  let seen = Hashtbl.create 16 in
  List.filter
    (fun x ->
      if Hashtbl.mem seen x then
        false
      else (
        Hashtbl.add seen x ();
        true))
    lst

(* Normalize a type - rename type variables to a, b, c, ... *)
let normalize (mono : mono_type) : mono_type =
  let vars = unique_in_order (collect_vars_in_order mono) in
  let renaming =
    List.fold_left
      (fun acc (i, old_name) ->
        let new_name = nice_var_name i in
        (* Filter out identity mappings (e.g. "a" -> TVar "a") to avoid infinite
           recursion in apply_substitution, which recursively applies to replacements *)
        if new_name = old_name then
          acc
        else
          SubstMap.add old_name (TVar new_name) acc)
      SubstMap.empty
      (List.mapi (fun i name -> (i, name)) vars)
  in
  apply_substitution renaming mono

(* Convert type to string with normalized variable names *)
let to_string_pretty (mono : mono_type) : string = to_string (normalize mono)

(* ============================================================
   Union Type Normalization
   ============================================================ *)

(* Normalize a union type: flatten, dedupe, sort *)
let normalize_union (types : mono_type list) : mono_type =
  (* Step 1: Flatten nested unions *)
  let rec flatten = function
    | TUnion inner -> List.concat_map flatten inner
    | t -> [ t ]
  in
  let flattened = List.concat_map flatten types in

  (* Step 2: Remove duplicates by sorting and filtering *)
  let sorted = List.sort compare flattened in
  let rec dedup = function
    | [] -> []
    | [ x ] -> [ x ]
    | x :: y :: rest when x = y -> dedup (y :: rest)
    | x :: rest -> x :: dedup rest
  in
  let unique = dedup sorted in

  (* Step 3: Return normalized result *)
  match unique with
  | [] -> failwith "Empty union type"
  | [ single ] -> single (* Single-element union = just the type *)
  | multiple -> TUnion multiple

(* ============================================================
   Tests
   ============================================================ *)

let%test "to_string primitives" = to_string TInt = "Int" && to_string TBool = "Bool" && to_string (TVar "a") = "a"

let%test "to_string single arg function" =
  (* Int -> Bool displays as: Int -> Bool *)
  to_string (tfun TInt TBool) = "Int -> Bool"

let%test "to_string multi arg function" =
  (* Int -> String -> Bool displays as: (Int, String) -> Bool *)
  to_string (tfun TInt (tfun TString TBool)) = "(Int, String) -> Bool"

let%test "to_string nested function" =
  (* (Int -> Bool) -> String displays as: (Int -> Bool) -> String *)
  to_string (tfun (tfun TInt TBool) TString) = "(Int -> Bool) -> String"

let%test "to_string array and hash" =
  to_string (TArray TInt) = "[Int]" && to_string (THash (TString, TInt)) = "{String: Int}"

let%test "to_string closed record" =
  to_string (TRecord ([ { name = "x"; typ = TInt }; { name = "y"; typ = TString } ], None))
  = "{ x: Int, y: String }"

let%test "to_string open record" =
  to_string (TRecord ([ { name = "x"; typ = TInt } ], Some (TRowVar "r"))) = "{ x: Int, ...r }"

let%test "poly_type_to_string monomorphic" =
  (* No quantifiers - just the type *)
  poly_type_to_string (mono_to_poly TInt) = "Int"

let%test "poly_type_to_string polymorphic" =
  (* ∀a. a -> a *)
  let id_type = Forall ([ "a" ], tfun (TVar "a") (TVar "a")) in
  poly_type_to_string id_type = "∀a. a -> a"

let%test "poly_type_to_string multi var" =
  (* ∀a b. (a, b) -> a *)
  let const_type = Forall ([ "a"; "b" ], tfun (TVar "a") (tfun (TVar "b") (TVar "a"))) in
  poly_type_to_string const_type = "∀a b. (a, b) -> a"

let%test "apply_substitution to TVar" =
  let subst = substitution_of_list [ ("a", TInt) ] in
  apply_substitution subst (TVar "a") = TInt

let%test "apply_substitution to unknown TVar" =
  let subst = substitution_of_list [ ("a", TInt) ] in
  apply_substitution subst (TVar "b") = TVar "b"

let%test "apply_substitution to function" =
  let subst = substitution_of_list [ ("a", TInt); ("b", TString) ] in
  apply_substitution subst (tfun (TVar "a") (TVar "b")) = tfun TInt TString

let%test "apply_substitution to record" =
  let subst = substitution_of_list [ ("a", TInt); ("r", TRecord ([ { name = "y"; typ = TString } ], None)) ] in
  let record = TRecord ([ { name = "x"; typ = TVar "a" } ], Some (TRowVar "r")) in
  apply_substitution subst record
  = TRecord ([ { name = "x"; typ = TInt } ], Some (TRecord ([ { name = "y"; typ = TString } ], None)))

let%test "apply_substitution avoids immediate self-cycle" =
  let subst = substitution_of_list [ ("a", TVar "a") ] in
  apply_substitution subst (TVar "a") = TVar "a"

let%test "apply_substitution avoids two-node cycle" =
  let subst = substitution_of_list [ ("a", TVar "b"); ("b", TVar "a") ] in
  apply_substitution subst (TVar "a") = TVar "a"

let%test "apply_substitution avoids var-row cycle" =
  let subst = substitution_of_list [ ("a", TRowVar "a") ] in
  apply_substitution subst (TVar "a") = TRowVar "a"

let%test "apply_substitution_poly respects quantified vars" =
  (* ∀a. a -> b  with {a -> Int, b -> String}  should give  ∀a. a -> String *)
  (* The 'a' is quantified so it should NOT be replaced *)
  let subst = substitution_of_list [ ("a", TInt); ("b", TString) ] in
  let poly = Forall ([ "a" ], tfun (TVar "a") (TVar "b")) in
  apply_substitution_poly subst poly = Forall ([ "a" ], tfun (TVar "a") TString)

let%test "compose_substitution" =
  (* first = {a -> b}, second = {b -> Int} *)
  (* compose should give {a -> Int, b -> Int} *)
  let first = substitution_of_list [ ("a", TVar "b") ] in
  let second = substitution_of_list [ ("b", TInt) ] in
  let composed = compose_substitution first second in
  apply_substitution composed (TVar "a") = TInt && apply_substitution composed (TVar "b") = TInt

let%test "free_type_vars primitives" =
  TypeVarSet.is_empty (free_type_vars TInt) && TypeVarSet.is_empty (free_type_vars TBool)

let%test "free_type_vars TVar" = TypeVarSet.equal (free_type_vars (TVar "a")) (TypeVarSet.singleton "a")

let%test "free_type_vars function" =
  (* a -> b has free vars {a, b} *)
  let free = free_type_vars (tfun (TVar "a") (TVar "b")) in
  TypeVarSet.equal free (TypeVarSet.of_list [ "a"; "b" ])

let%test "free_type_vars record" =
  let free = free_type_vars (TRecord ([ { name = "x"; typ = TVar "a" } ], Some (TRowVar "r"))) in
  TypeVarSet.equal free (TypeVarSet.of_list [ "a"; "r" ])

let%test "free_type_vars_poly quantified" =
  (* ∀a. a -> b  has free vars {b} only (a is bound) *)
  let free = free_type_vars_poly (Forall ([ "a" ], tfun (TVar "a") (TVar "b"))) in
  TypeVarSet.equal free (TypeVarSet.singleton "b")

let%test "occurs_in check" =
  occurs_in "a" (TVar "a")
  && occurs_in "a" (tfun (TVar "a") TInt)
  && (not (occurs_in "a" (tfun (TVar "b") TInt)))
  && not (occurs_in "a" TInt)

let%test "nice_var_name" =
  nice_var_name 0 = "a" && nice_var_name 25 = "z" && nice_var_name 26 = "a1" && nice_var_name 27 = "b1"

let%test "normalize renames vars in order" =
  (* t5 -> t3 becomes a -> b *)
  normalize (tfun (TVar "t5") (TVar "t3")) = tfun (TVar "a") (TVar "b")

let%test "normalize same var keeps same name" =
  (* t2 -> t2 becomes a -> a *)
  normalize (tfun (TVar "t2") (TVar "t2")) = tfun (TVar "a") (TVar "a")

let%test "to_string_pretty" =
  to_string_pretty (tfun (TVar "t99") (TVar "t99")) = "a -> a"
  && to_string_pretty (tfun (TVar "x") (tfun (TVar "y") (TVar "x"))) = "(a, b) -> a"

let%test "to_string effectful function" =
  to_string (tfun_eff TInt TBool) = "Int => Bool"
  && to_string (tfun_eff TInt (tfun_eff TString TBool)) = "(Int, String) => Bool"

(* Union type tests *)

let%test "to_string union" = to_string (TUnion [ TInt; TString ]) = "Int | String"
let%test "to_string multi-member union" = to_string (TUnion [ TInt; TString; TBool ]) = "Int | String | Bool"
let%test "normalize_union dedupes" = normalize_union [ TInt; TString; TInt ] = TUnion [ TInt; TString ]

let%test "normalize_union flattens nested" =
  let nested = TUnion [ TInt; TUnion [ TString; TBool ] ] in
  (* OCaml compare sorts by constructor order: TInt, TFloat, TBool, TString *)
  normalize_union [ nested; TFloat ] = TUnion [ TInt; TFloat; TBool; TString ]

let%test "normalize_union single element" = normalize_union [ TInt ] = TInt

let%test "apply_substitution to union" =
  let subst = substitution_of_list [ ("a", TInt); ("b", TString) ] in
  let union = TUnion [ TVar "a"; TVar "b"; TBool ] in
  apply_substitution subst union = TUnion [ TInt; TString; TBool ]

(* Enum type tests *)

let%test "to_string enum no args" = to_string (TEnum ("direction", [])) = "direction"
let%test "to_string enum with one arg" = to_string (TEnum ("option", [ TInt ])) = "option[Int]"

let%test "to_string enum with multiple args" =
  to_string (TEnum ("result", [ TString; TInt ])) = "result[String, Int]"

let%test "apply_substitution to enum" =
  let subst = substitution_of_list [ ("a", TInt); ("b", TString) ] in
  let enum = TEnum ("result", [ TVar "a"; TVar "b" ]) in
  apply_substitution subst enum = TEnum ("result", [ TInt; TString ])

let%test "normalize_record_fields sorts and keeps last duplicate" =
  let fields =
    [
      { name = "z"; typ = TInt };
      { name = "x"; typ = TBool };
      { name = "x"; typ = TString };
      { name = "y"; typ = TInt };
    ]
  in
  normalize_record_fields fields
  = [ { name = "x"; typ = TString }; { name = "y"; typ = TInt }; { name = "z"; typ = TInt } ]

let%test "canonicalize_mono_type sorts record fields" =
  let record = TRecord ([ { name = "y"; typ = TInt }; { name = "x"; typ = TBool } ], None) in
  canonicalize_mono_type record = TRecord ([ { name = "x"; typ = TBool }; { name = "y"; typ = TInt } ], None)

let%test "free_type_vars in enum" =
  let enum = TEnum ("option", [ TVar "a" ]) in
  TypeVarSet.equal (free_type_vars enum) (TypeVarSet.singleton "a")

let%test "collect_vars_in_order with enum" =
  let enum = TEnum ("result", [ TVar "a"; TVar "b" ]) in
  collect_vars_in_order enum = [ "a"; "b" ]
