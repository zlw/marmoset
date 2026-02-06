(* ============================================================
   Builtin Type Signatures (for type inference)
   ============================================================
   
   These are the types for the prelude - the built-in functions
   available in every Marmoset program.
   
   Note: Some of these are more permissive than ideal because
   we don't have union types or type classes yet. For example,
   `len` works on both String and [a], but we type it as a -> Int.
   We'll tighten these when we add unions/traits.
*)

open Types
module Trait_registry = Trait_registry

(* Helper to create a poly_type with quantified variables *)
let forall vars mono = Forall (vars, mono)

(* Helper for monomorphic types *)
let mono m = Forall ([], m)

(* Builtin type signatures *)
let builtin_types : (string * poly_type) list =
  [
    (* len : a -> Int
       Actually works on String and [a], but we use a -> Int for now.
       TODO: When we have unions: (String | [a]) -> Int *)
    ("len", forall [ "a" ] (TFun (TVar "a", TInt)));
    (* first : [a] -> a
       Returns first element of array, or Null if empty.
       TODO: Should return Option a or a? when we have those *)
    ("first", forall [ "a" ] (TFun (TArray (TVar "a"), TVar "a")));
    (* last : [a] -> a
       Returns last element of array, or Null if empty. *)
    ("last", forall [ "a" ] (TFun (TArray (TVar "a"), TVar "a")));
    (* rest : [a] -> [a]
       Returns all but the first element, or Null if empty.
       TODO: Should return [a] or Null - needs union types *)
    ("rest", forall [ "a" ] (TFun (TArray (TVar "a"), TArray (TVar "a"))));
    (* push : ([a], a) -> [a]
       Appends element to array, returns new array. *)
    ("push", forall [ "a" ] (TFun (TArray (TVar "a"), TFun (TVar "a", TArray (TVar "a")))));
    (* puts : a -> Null
       Prints any value to stdout, returns Null. *)
    ("puts", forall [ "a" ] (TFun (TVar "a", TNull)));
  ]

(* ============================================================
   Builtin Traits
   ============================================================ *)

(* Register the ordering enum used by ord trait *)
let init_builtin_enums () =
  (* enum ordering { less equal greater } *)
  Enum_registry.register
    {
      Enum_registry.name = "ordering";
      type_params = [];
      variants =
        [
          { Enum_registry.name = "less"; fields = [] };
          { Enum_registry.name = "equal"; fields = [] };
          { Enum_registry.name = "greater"; fields = [] };
        ];
    }

(* Register builtin traits in the global registry *)
let init_builtin_traits () =
  (* trait eq[a] { fn eq(x: a, y: a) -> bool } *)
  Trait_registry.register_trait
    {
      trait_name = "eq";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [
          { method_name = "eq"; method_params = [ ("x", TVar "a"); ("y", TVar "a") ]; method_return_type = TBool };
        ];
    };

  (* trait show[a] { fn show(x: a) -> string } *)
  Trait_registry.register_trait
    {
      trait_name = "show";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [ { method_name = "show"; method_params = [ ("x", TVar "a") ]; method_return_type = TString } ];
    };

  (* trait debug[a] { fn debug(x: a) -> string } *)
  Trait_registry.register_trait
    {
      trait_name = "debug";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [ { method_name = "debug"; method_params = [ ("x", TVar "a") ]; method_return_type = TString } ];
    };

  (* trait ord[a]: eq { fn compare(x: a, y: a) -> ordering } *)
  Trait_registry.register_trait
    {
      trait_name = "ord";
      trait_type_param = Some "a";
      trait_supertraits = [ "eq" ];
      trait_methods =
        [
          {
            method_name = "compare";
            method_params = [ ("x", TVar "a"); ("y", TVar "a") ];
            method_return_type = TEnum ("ordering", []);
          };
        ];
    };

  (* trait hash[a] { fn hash(x: a) -> int } *)
  Trait_registry.register_trait
    {
      trait_name = "hash";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods = [ { method_name = "hash"; method_params = [ ("x", TVar "a") ]; method_return_type = TInt } ];
    };

  (* trait num[a] { fn add, sub, mul, div } *)
  Trait_registry.register_trait
    {
      trait_name = "num";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [
          {
            method_name = "add";
            method_params = [ ("x", TVar "a"); ("y", TVar "a") ];
            method_return_type = TVar "a";
          };
          {
            method_name = "sub";
            method_params = [ ("x", TVar "a"); ("y", TVar "a") ];
            method_return_type = TVar "a";
          };
          {
            method_name = "mul";
            method_params = [ ("x", TVar "a"); ("y", TVar "a") ];
            method_return_type = TVar "a";
          };
          {
            method_name = "div";
            method_params = [ ("x", TVar "a"); ("y", TVar "a") ];
            method_return_type = TVar "a";
          };
        ];
    };

  (* trait neg[a] { fn neg(x: a) -> a } *)
  Trait_registry.register_trait
    {
      trait_name = "neg";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [ { method_name = "neg"; method_params = [ ("x", TVar "a") ]; method_return_type = TVar "a" } ];
    }

(* ============================================================
   Builtin Trait Implementations for Primitives
   ============================================================ *)

(* Register builtin impls for primitive types *)
let init_builtin_impls () =
  (* int: show, eq, debug, ord, hash, num, neg *)

  (* impl show for int *)
  Trait_registry.register_impl
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ { method_name = "show"; method_params = [ ("x", TInt) ]; method_return_type = TString } ];
    };

  (* impl eq for int *)
  Trait_registry.register_impl
    {
      impl_trait_name = "eq";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods =
        [ { method_name = "eq"; method_params = [ ("x", TInt); ("y", TInt) ]; method_return_type = TBool } ];
    };

  (* impl debug for int *)
  Trait_registry.register_impl
    {
      impl_trait_name = "debug";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ { method_name = "debug"; method_params = [ ("x", TInt) ]; method_return_type = TString } ];
    };

  (* impl ord for int *)
  Trait_registry.register_impl
    {
      impl_trait_name = "ord";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods =
        [
          {
            method_name = "compare";
            method_params = [ ("x", TInt); ("y", TInt) ];
            method_return_type = TEnum ("ordering", []);
          };
        ];
    };

  (* impl hash for int *)
  Trait_registry.register_impl
    {
      impl_trait_name = "hash";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ { method_name = "hash"; method_params = [ ("x", TInt) ]; method_return_type = TInt } ];
    };

  (* impl num for int *)
  Trait_registry.register_impl
    {
      impl_trait_name = "num";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods =
        [
          { method_name = "add"; method_params = [ ("x", TInt); ("y", TInt) ]; method_return_type = TInt };
          { method_name = "sub"; method_params = [ ("x", TInt); ("y", TInt) ]; method_return_type = TInt };
          { method_name = "mul"; method_params = [ ("x", TInt); ("y", TInt) ]; method_return_type = TInt };
          { method_name = "div"; method_params = [ ("x", TInt); ("y", TInt) ]; method_return_type = TInt };
        ];
    };

  (* impl neg for int *)
  Trait_registry.register_impl
    {
      impl_trait_name = "neg";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ { method_name = "neg"; method_params = [ ("x", TInt) ]; method_return_type = TInt } ];
    };

  (* bool: show, eq, debug, ord, hash *)

  (* impl show for bool *)
  Trait_registry.register_impl
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TBool;
      impl_methods = [ { method_name = "show"; method_params = [ ("x", TBool) ]; method_return_type = TString } ];
    };

  (* impl eq for bool *)
  Trait_registry.register_impl
    {
      impl_trait_name = "eq";
      impl_type_params = [];
      impl_for_type = TBool;
      impl_methods =
        [ { method_name = "eq"; method_params = [ ("x", TBool); ("y", TBool) ]; method_return_type = TBool } ];
    };

  (* impl debug for bool *)
  Trait_registry.register_impl
    {
      impl_trait_name = "debug";
      impl_type_params = [];
      impl_for_type = TBool;
      impl_methods = [ { method_name = "debug"; method_params = [ ("x", TBool) ]; method_return_type = TString } ];
    };

  (* impl ord for bool *)
  Trait_registry.register_impl
    {
      impl_trait_name = "ord";
      impl_type_params = [];
      impl_for_type = TBool;
      impl_methods =
        [
          {
            method_name = "compare";
            method_params = [ ("x", TBool); ("y", TBool) ];
            method_return_type = TEnum ("ordering", []);
          };
        ];
    };

  (* impl hash for bool *)
  Trait_registry.register_impl
    {
      impl_trait_name = "hash";
      impl_type_params = [];
      impl_for_type = TBool;
      impl_methods = [ { method_name = "hash"; method_params = [ ("x", TBool) ]; method_return_type = TInt } ];
    };

  (* string: show, eq, debug, ord, hash *)

  (* impl show for string *)
  Trait_registry.register_impl
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TString;
      impl_methods =
        [ { method_name = "show"; method_params = [ ("x", TString) ]; method_return_type = TString } ];
    };

  (* impl eq for string *)
  Trait_registry.register_impl
    {
      impl_trait_name = "eq";
      impl_type_params = [];
      impl_for_type = TString;
      impl_methods =
        [ { method_name = "eq"; method_params = [ ("x", TString); ("y", TString) ]; method_return_type = TBool } ];
    };

  (* impl debug for string *)
  Trait_registry.register_impl
    {
      impl_trait_name = "debug";
      impl_type_params = [];
      impl_for_type = TString;
      impl_methods =
        [ { method_name = "debug"; method_params = [ ("x", TString) ]; method_return_type = TString } ];
    };

  (* impl ord for string *)
  Trait_registry.register_impl
    {
      impl_trait_name = "ord";
      impl_type_params = [];
      impl_for_type = TString;
      impl_methods =
        [
          {
            method_name = "compare";
            method_params = [ ("x", TString); ("y", TString) ];
            method_return_type = TEnum ("ordering", []);
          };
        ];
    };

  (* impl hash for string *)
  Trait_registry.register_impl
    {
      impl_trait_name = "hash";
      impl_type_params = [];
      impl_for_type = TString;
      impl_methods = [ { method_name = "hash"; method_params = [ ("x", TString) ]; method_return_type = TInt } ];
    };

  (* float: show, eq, debug, ord, num, neg (NO hash) *)

  (* impl show for float *)
  Trait_registry.register_impl
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TFloat;
      impl_methods = [ { method_name = "show"; method_params = [ ("x", TFloat) ]; method_return_type = TString } ];
    };

  (* impl eq for float *)
  Trait_registry.register_impl
    {
      impl_trait_name = "eq";
      impl_type_params = [];
      impl_for_type = TFloat;
      impl_methods =
        [ { method_name = "eq"; method_params = [ ("x", TFloat); ("y", TFloat) ]; method_return_type = TBool } ];
    };

  (* impl debug for float *)
  Trait_registry.register_impl
    {
      impl_trait_name = "debug";
      impl_type_params = [];
      impl_for_type = TFloat;
      impl_methods =
        [ { method_name = "debug"; method_params = [ ("x", TFloat) ]; method_return_type = TString } ];
    };

  (* impl ord for float *)
  Trait_registry.register_impl
    {
      impl_trait_name = "ord";
      impl_type_params = [];
      impl_for_type = TFloat;
      impl_methods =
        [
          {
            method_name = "compare";
            method_params = [ ("x", TFloat); ("y", TFloat) ];
            method_return_type = TEnum ("ordering", []);
          };
        ];
    };

  (* impl num for float *)
  Trait_registry.register_impl
    {
      impl_trait_name = "num";
      impl_type_params = [];
      impl_for_type = TFloat;
      impl_methods =
        [
          { method_name = "add"; method_params = [ ("x", TFloat); ("y", TFloat) ]; method_return_type = TFloat };
          { method_name = "sub"; method_params = [ ("x", TFloat); ("y", TFloat) ]; method_return_type = TFloat };
          { method_name = "mul"; method_params = [ ("x", TFloat); ("y", TFloat) ]; method_return_type = TFloat };
          { method_name = "div"; method_params = [ ("x", TFloat); ("y", TFloat) ]; method_return_type = TFloat };
        ];
    };

  (* impl neg for float *)
  Trait_registry.register_impl
    {
      impl_trait_name = "neg";
      impl_type_params = [];
      impl_for_type = TFloat;
      impl_methods = [ { method_name = "neg"; method_params = [ ("x", TFloat) ]; method_return_type = TFloat } ];
    }

(* Create a type environment with all builtins *)
let prelude_env () : Infer.type_env =
  (* Initialize builtin enums, traits, and impls on first call *)
  init_builtin_enums ();
  init_builtin_traits ();
  init_builtin_impls ();
  List.fold_left (fun env (name, poly) -> Infer.TypeEnv.add name poly env) Infer.empty_env builtin_types

(* ============================================================
   Tests
   ============================================================ *)

let%test "builtin_types has all builtins" =
  let names = List.map fst builtin_types in
  List.for_all (fun name -> List.mem name names) [ "len"; "first"; "last"; "rest"; "push"; "puts" ]

let%test "prelude_env creates valid environment" =
  let env = prelude_env () in
  Infer.TypeEnv.cardinal env = 6

let%test "builtin traits are registered" =
  Trait_registry.clear ();
  init_builtin_traits ();
  let show_exists = Trait_registry.lookup_trait "show" <> None in
  let eq_exists = Trait_registry.lookup_trait "eq" <> None in
  let ord_exists = Trait_registry.lookup_trait "ord" <> None in
  show_exists && eq_exists && ord_exists
