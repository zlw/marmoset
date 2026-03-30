type t =
  | TraitConstraint of string
  | ShapeConstraint of string
[@@deriving show, eq]

let builtin_trait_internal_name (name : string) : string option =
  match name with
  | "eq" | "Eq" -> Some "eq"
  | "show" | "Show" -> Some "show"
  | "debug" | "Debug" -> Some "debug"
  | "ord" | "Ord" -> Some "ord"
  | "hash" | "Hash" -> Some "hash"
  | "num" | "Num" -> Some "num"
  | "neg" | "Neg" -> Some "neg"
  | _ -> None

let canonical_trait_name (name : string) : string =
  match builtin_trait_internal_name name with
  | Some builtin_name -> builtin_name
  | None -> name

let canonicalize = function
  | TraitConstraint name -> TraitConstraint (canonical_trait_name name)
  | ShapeConstraint name -> ShapeConstraint name

let of_name (name : string) : t =
  if Type_registry.is_shape_name name then
    ShapeConstraint name
  else
    TraitConstraint (canonical_trait_name name)

let of_names (names : string list) : t list = List.map of_name names

let name = function
  | TraitConstraint name | ShapeConstraint name -> name

let names (constraints : t list) : string list = List.map name constraints

let is_trait = function
  | TraitConstraint _ -> true
  | ShapeConstraint _ -> false

let is_shape = function
  | TraitConstraint _ -> false
  | ShapeConstraint _ -> true

let trait_name_opt = function
  | TraitConstraint name -> Some name
  | ShapeConstraint _ -> None

let shape_name_opt = function
  | TraitConstraint _ -> None
  | ShapeConstraint name -> Some name

let render_list (constraints : t list) : string = String.concat ", " (names constraints)

let dedupe_preserve_order (constraints : t list) : t list =
  let seen : (string, unit) Hashtbl.t = Hashtbl.create (max 8 (List.length constraints)) in
  let rec go rev_acc = function
    | [] -> List.rev rev_acc
    | constraint_ref :: rest ->
        let key = name constraint_ref in
        if Hashtbl.mem seen key then
          go rev_acc rest
        else (
          Hashtbl.replace seen key ();
          go (constraint_ref :: rev_acc) rest)
  in
  go [] constraints

let%test "of_name canonicalizes builtin trait names" =
  match of_name "Show" with
  | TraitConstraint "show" -> true
  | _ -> false

let%test "of_name preserves registered shapes" =
  Type_registry.clear ();
  Type_registry.register_shape
    {
      Type_registry.shape_name = "Named";
      shape_type_params = [];
      shape_fields = [ { Types.name = "name"; typ = Types.TString } ];
    };
  match of_name "Named" with
  | ShapeConstraint "Named" -> true
  | _ -> false
