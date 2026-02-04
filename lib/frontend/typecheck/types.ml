(* Type representation for Hindley-Milner type inference *)

(* A monotype - a type without quantifiers (no "forall") *)
type mono_type =
  | TInt (* Int *)
  | TFloat (* Float *)
  | TBool (* Bool *)
  | TString (* String *)
  | TNull (* Null / unit *)
  | TVar of string (* Type variable: 'a, 'b, etc. *)
  | TFun of mono_type * mono_type (* Function: T -> U (nested for multi-arg) *)
  | TArray of mono_type (* Array: [T] *)
  | THash of mono_type * mono_type (* Hash: {K: V} *)
  | TUnion of mono_type list (* Union: Int | String | Bool *)
  | TEnum of string * mono_type list (* Enum: option[Int], result[String, Int] *)

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
  | TNull -> "Null"
  | TVar name -> name
  | TFun _ as t ->
      let rec collect_args = function
        | TFun (arg, rest) ->
            let args, ret = collect_args rest in
            (arg :: args, ret)
        | t -> ([], t)
      in
      let args, ret = collect_args t in
      let args_str =
        match args with
        | [ single ] -> to_string_parens single (* single arg: parens if it's a function *)
        | _ -> "(" ^ String.concat ", " (List.map to_string args) ^ ")"
      in
      args_str ^ " -> " ^ to_string ret
  | TArray element -> "[" ^ to_string element ^ "]"
  | THash (key, value) -> "{" ^ to_string key ^ ": " ^ to_string value ^ "}"
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
   Substitution: mapping from type variable names to types
   ============================================================ *)

type substitution = (string * mono_type) list

let empty_substitution : substitution = []

(* Apply substitution to a type - replace TVars according to the map *)
let rec apply_substitution (subst : substitution) (mono : mono_type) : mono_type =
  match mono with
  | TInt | TFloat | TBool | TString | TNull -> mono
  | TVar name -> (
      match List.assoc_opt name subst with
      | Some replacement -> replacement
      | None -> mono)
  | TFun (arg, ret) -> TFun (apply_substitution subst arg, apply_substitution subst ret)
  | TArray element -> TArray (apply_substitution subst element)
  | THash (key, value) -> THash (apply_substitution subst key, apply_substitution subst value)
  | TUnion types -> TUnion (List.map (apply_substitution subst) types)
  | TEnum (name, args) -> TEnum (name, List.map (apply_substitution subst) args)

(* Apply substitution to a poly_type - don't touch quantified variables *)
let apply_substitution_poly (subst : substitution) (Forall (quantified_vars, mono)) : poly_type =
  (* Remove quantified vars from substitution before applying *)
  let filtered_subst = List.filter (fun (name, _) -> not (List.mem name quantified_vars)) subst in
  Forall (quantified_vars, apply_substitution filtered_subst mono)

(* Compose two substitutions: apply first_subst first, then second_subst *)
let compose_substitution (first_subst : substitution) (second_subst : substitution) : substitution =
  let first_with_second_applied =
    List.map (fun (name, mono) -> (name, apply_substitution second_subst mono)) first_subst
  in
  first_with_second_applied @ second_subst

(* ============================================================
   Free Type Variables: type variables not bound by a quantifier
   ============================================================ *)

module TypeVarSet = Set.Make (String)

(* Free type variables in a mono_type *)
let rec free_type_vars (mono : mono_type) : TypeVarSet.t =
  match mono with
  | TInt | TFloat | TBool | TString | TNull -> TypeVarSet.empty
  | TVar name -> TypeVarSet.singleton name
  | TFun (arg, ret) -> TypeVarSet.union (free_type_vars arg) (free_type_vars ret)
  | TArray element -> free_type_vars element
  | THash (key, value) -> TypeVarSet.union (free_type_vars key) (free_type_vars value)
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
  | TFun (arg, ret) -> collect_vars_in_order arg @ collect_vars_in_order ret
  | TArray element -> collect_vars_in_order element
  | THash (key, value) -> collect_vars_in_order key @ collect_vars_in_order value
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
  let renaming = List.mapi (fun i old_name -> (old_name, TVar (nice_var_name i))) vars in
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
  to_string (TFun (TInt, TBool)) = "Int -> Bool"

let%test "to_string multi arg function" =
  (* Int -> String -> Bool displays as: (Int, String) -> Bool *)
  to_string (TFun (TInt, TFun (TString, TBool))) = "(Int, String) -> Bool"

let%test "to_string nested function" =
  (* (Int -> Bool) -> String displays as: (Int -> Bool) -> String *)
  to_string (TFun (TFun (TInt, TBool), TString)) = "(Int -> Bool) -> String"

let%test "to_string array and hash" =
  to_string (TArray TInt) = "[Int]" && to_string (THash (TString, TInt)) = "{String: Int}"

let%test "poly_type_to_string monomorphic" =
  (* No quantifiers - just the type *)
  poly_type_to_string (mono_to_poly TInt) = "Int"

let%test "poly_type_to_string polymorphic" =
  (* ∀a. a -> a *)
  let id_type = Forall ([ "a" ], TFun (TVar "a", TVar "a")) in
  poly_type_to_string id_type = "∀a. a -> a"

let%test "poly_type_to_string multi var" =
  (* ∀a b. (a, b) -> a *)
  let const_type = Forall ([ "a"; "b" ], TFun (TVar "a", TFun (TVar "b", TVar "a"))) in
  poly_type_to_string const_type = "∀a b. (a, b) -> a"

let%test "apply_substitution to TVar" =
  let subst = [ ("a", TInt) ] in
  apply_substitution subst (TVar "a") = TInt

let%test "apply_substitution to unknown TVar" =
  let subst = [ ("a", TInt) ] in
  apply_substitution subst (TVar "b") = TVar "b"

let%test "apply_substitution to function" =
  let subst = [ ("a", TInt); ("b", TString) ] in
  apply_substitution subst (TFun (TVar "a", TVar "b")) = TFun (TInt, TString)

let%test "apply_substitution_poly respects quantified vars" =
  (* ∀a. a -> b  with {a -> Int, b -> String}  should give  ∀a. a -> String *)
  (* The 'a' is quantified so it should NOT be replaced *)
  let subst = [ ("a", TInt); ("b", TString) ] in
  let poly = Forall ([ "a" ], TFun (TVar "a", TVar "b")) in
  apply_substitution_poly subst poly = Forall ([ "a" ], TFun (TVar "a", TString))

let%test "compose_substitution" =
  (* first = {a -> b}, second = {b -> Int} *)
  (* compose should give {a -> Int, b -> Int} *)
  let first = [ ("a", TVar "b") ] in
  let second = [ ("b", TInt) ] in
  let composed = compose_substitution first second in
  apply_substitution composed (TVar "a") = TInt && apply_substitution composed (TVar "b") = TInt

let%test "free_type_vars primitives" =
  TypeVarSet.is_empty (free_type_vars TInt) && TypeVarSet.is_empty (free_type_vars TBool)

let%test "free_type_vars TVar" = TypeVarSet.equal (free_type_vars (TVar "a")) (TypeVarSet.singleton "a")

let%test "free_type_vars function" =
  (* a -> b has free vars {a, b} *)
  let free = free_type_vars (TFun (TVar "a", TVar "b")) in
  TypeVarSet.equal free (TypeVarSet.of_list [ "a"; "b" ])

let%test "free_type_vars_poly quantified" =
  (* ∀a. a -> b  has free vars {b} only (a is bound) *)
  let free = free_type_vars_poly (Forall ([ "a" ], TFun (TVar "a", TVar "b"))) in
  TypeVarSet.equal free (TypeVarSet.singleton "b")

let%test "occurs_in check" =
  occurs_in "a" (TVar "a")
  && occurs_in "a" (TFun (TVar "a", TInt))
  && (not (occurs_in "a" (TFun (TVar "b", TInt))))
  && not (occurs_in "a" TInt)

let%test "nice_var_name" =
  nice_var_name 0 = "a" && nice_var_name 25 = "z" && nice_var_name 26 = "a1" && nice_var_name 27 = "b1"

let%test "normalize renames vars in order" =
  (* t5 -> t3 becomes a -> b *)
  normalize (TFun (TVar "t5", TVar "t3")) = TFun (TVar "a", TVar "b")

let%test "normalize same var keeps same name" =
  (* t2 -> t2 becomes a -> a *)
  normalize (TFun (TVar "t2", TVar "t2")) = TFun (TVar "a", TVar "a")

let%test "to_string_pretty" =
  to_string_pretty (TFun (TVar "t99", TVar "t99")) = "a -> a"
  && to_string_pretty (TFun (TVar "x", TFun (TVar "y", TVar "x"))) = "(a, b) -> a"

(* Union type tests *)

let%test "to_string union" = to_string (TUnion [ TInt; TString ]) = "Int | String"
let%test "to_string multi-member union" = to_string (TUnion [ TInt; TString; TBool ]) = "Int | String | Bool"
let%test "normalize_union dedupes" = normalize_union [ TInt; TString; TInt ] = TUnion [ TInt; TString ]

let%test "normalize_union flattens nested" =
  let nested = TUnion [ TInt; TUnion [ TString; TBool ] ] in
  normalize_union [ nested; TFloat ] = TUnion [ TBool; TFloat; TInt; TString ]

let%test "normalize_union single element" = normalize_union [ TInt ] = TInt

let%test "apply_substitution to union" =
  let subst = [ ("a", TInt); ("b", TString) ] in
  let union = TUnion [ TVar "a"; TVar "b"; TBool ] in
  apply_substitution subst union = TUnion [ TInt; TString; TBool ]

(* Enum type tests *)

let%test "to_string enum no args" = to_string (TEnum ("direction", [])) = "direction"
let%test "to_string enum with one arg" = to_string (TEnum ("option", [ TInt ])) = "option[Int]"

let%test "to_string enum with multiple args" =
  to_string (TEnum ("result", [ TString; TInt ])) = "result[String, Int]"

let%test "apply_substitution to enum" =
  let subst = [ ("a", TInt); ("b", TString) ] in
  let enum = TEnum ("result", [ TVar "a"; TVar "b" ]) in
  apply_substitution subst enum = TEnum ("result", [ TInt; TString ])

let%test "free_type_vars in enum" =
  let enum = TEnum ("option", [ TVar "a" ]) in
  TypeVarSet.equal (free_type_vars enum) (TypeVarSet.singleton "a")

let%test "collect_vars_in_order with enum" =
  let enum = TEnum ("result", [ TVar "a"; TVar "b" ]) in
  collect_vars_in_order enum = [ "a"; "b" ]
