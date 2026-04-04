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
    ("len", forall [ "a" ] (tfun (TVar "a") TInt));
    (* first : [a] -> a
       Returns first element of array, or Null if empty.
       TODO: Should return Option a or a? when we have those *)
    ("first", forall [ "a" ] (tfun (TArray (TVar "a")) (TVar "a")));
    (* last : [a] -> a
       Returns last element of array, or Null if empty. *)
    ("last", forall [ "a" ] (tfun (TArray (TVar "a")) (TVar "a")));
    (* rest : [a] -> [a]
       Returns all but the first element, or Null if empty.
       TODO: Should return [a] or Null - needs union types *)
    ("rest", forall [ "a" ] (tfun (TArray (TVar "a")) (TArray (TVar "a"))));
    (* push : ([a], a) -> [a]
       Appends element to array, returns new array. *)
    ("push", forall [ "a" ] (tfun (TArray (TVar "a")) (tfun (TVar "a") (TArray (TVar "a")))));
    (* puts : a => Null
       Prints any value to stdout, returns Null. Effectful (I/O). *)
    ("puts", forall [ "a" ] (tfun_eff (TVar "a") TNull));
  ]

(* ============================================================
   Builtin Traits
   ============================================================ *)

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
          Trait_registry.mk_method_sig ~name:"eq"
            ~params:[ ("x", TVar "a"); ("y", TVar "a") ]
            ~return_type:TBool ();
        ];
    };

  (* trait show[a] { fn show(x: a) -> string } *)
  Trait_registry.register_trait
    {
      trait_name = "show";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [ Trait_registry.mk_method_sig ~name:"show" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
    };

  (* trait debug[a] { fn debug(x: a) -> string } *)
  Trait_registry.register_trait
    {
      trait_name = "debug";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [ Trait_registry.mk_method_sig ~name:"debug" ~params:[ ("x", TVar "a") ] ~return_type:TString () ];
    };

  (* trait ord[a]: eq { fn compare(x: a, y: a) -> Ordering } *)
  Trait_registry.register_trait
    {
      trait_name = "ord";
      trait_type_param = Some "a";
      trait_supertraits = [ "eq" ];
      trait_methods =
        [
          Trait_registry.mk_method_sig ~name:"compare"
            ~params:[ ("x", TVar "a"); ("y", TVar "a") ]
            ~return_type:(TEnum ("Ordering", []))
            ();
        ];
    };

  (* trait hash[a] { fn hash(x: a) -> int } *)
  Trait_registry.register_trait
    {
      trait_name = "hash";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [ Trait_registry.mk_method_sig ~name:"hash" ~params:[ ("x", TVar "a") ] ~return_type:TInt () ];
    };

  (* trait num[a] { fn add, sub, mul, div } *)
  Trait_registry.register_trait
    {
      trait_name = "num";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [
          Trait_registry.mk_method_sig ~name:"add"
            ~params:[ ("x", TVar "a"); ("y", TVar "a") ]
            ~return_type:(TVar "a") ();
          Trait_registry.mk_method_sig ~name:"sub"
            ~params:[ ("x", TVar "a"); ("y", TVar "a") ]
            ~return_type:(TVar "a") ();
          Trait_registry.mk_method_sig ~name:"mul"
            ~params:[ ("x", TVar "a"); ("y", TVar "a") ]
            ~return_type:(TVar "a") ();
          Trait_registry.mk_method_sig ~name:"div"
            ~params:[ ("x", TVar "a"); ("y", TVar "a") ]
            ~return_type:(TVar "a") ();
        ];
    };

  (* trait rem[a] { fn rem(x: a, y: a) -> a } *)
  Trait_registry.register_trait
    {
      trait_name = "rem";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [
          Trait_registry.mk_method_sig ~name:"rem"
            ~params:[ ("x", TVar "a"); ("y", TVar "a") ]
            ~return_type:(TVar "a") ();
        ];
    };

  (* trait neg[a] { fn neg(x: a) -> a } *)
  Trait_registry.register_trait
    {
      trait_name = "neg";
      trait_type_param = Some "a";
      trait_supertraits = [];
      trait_methods =
        [ Trait_registry.mk_method_sig ~name:"neg" ~params:[ ("x", TVar "a") ] ~return_type:(TVar "a") () ];
    }

(* ============================================================
   Builtin Trait Implementations for Primitives
   ============================================================ *)

(* Register builtin impls for primitive types *)
let init_builtin_impls () =
  (* int: show, eq, debug, ord, hash, num, neg *)

  (* impl show for int *)
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ Trait_registry.mk_method_sig ~name:"show" ~params:[ ("x", TInt) ] ~return_type:TString () ];
    };

  (* impl eq for int *)
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "eq";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods =
        [ Trait_registry.mk_method_sig ~name:"eq" ~params:[ ("x", TInt); ("y", TInt) ] ~return_type:TBool () ];
    };

  (* impl debug for int *)
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "debug";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods =
        [ Trait_registry.mk_method_sig ~name:"debug" ~params:[ ("x", TInt) ] ~return_type:TString () ];
    };

  (* impl ord for int *)
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "ord";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods =
        [
          Trait_registry.mk_method_sig ~name:"compare"
            ~params:[ ("x", TInt); ("y", TInt) ]
            ~return_type:(TEnum ("Ordering", []))
            ();
        ];
    };

  (* impl hash for int *)
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "hash";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ Trait_registry.mk_method_sig ~name:"hash" ~params:[ ("x", TInt) ] ~return_type:TInt () ];
    };

  (* impl num for int *)
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "num";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods =
        [
          Trait_registry.mk_method_sig ~name:"add" ~params:[ ("x", TInt); ("y", TInt) ] ~return_type:TInt ();
          Trait_registry.mk_method_sig ~name:"sub" ~params:[ ("x", TInt); ("y", TInt) ] ~return_type:TInt ();
          Trait_registry.mk_method_sig ~name:"mul" ~params:[ ("x", TInt); ("y", TInt) ] ~return_type:TInt ();
          Trait_registry.mk_method_sig ~name:"div" ~params:[ ("x", TInt); ("y", TInt) ] ~return_type:TInt ();
        ];
    };

  (* impl rem for int *)
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "rem";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods =
        [ Trait_registry.mk_method_sig ~name:"rem" ~params:[ ("x", TInt); ("y", TInt) ] ~return_type:TInt () ];
    };

  (* impl neg for int *)
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "neg";
      impl_type_params = [];
      impl_for_type = TInt;
      impl_methods = [ Trait_registry.mk_method_sig ~name:"neg" ~params:[ ("x", TInt) ] ~return_type:TInt () ];
    };

  (* bool: show, eq, debug, ord, hash *)

  (* impl show for bool *)
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TBool;
      impl_methods =
        [ Trait_registry.mk_method_sig ~name:"show" ~params:[ ("x", TBool) ] ~return_type:TString () ];
    };

  (* impl eq for bool *)
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "eq";
      impl_type_params = [];
      impl_for_type = TBool;
      impl_methods =
        [ Trait_registry.mk_method_sig ~name:"eq" ~params:[ ("x", TBool); ("y", TBool) ] ~return_type:TBool () ];
    };

  (* impl debug for bool *)
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "debug";
      impl_type_params = [];
      impl_for_type = TBool;
      impl_methods =
        [ Trait_registry.mk_method_sig ~name:"debug" ~params:[ ("x", TBool) ] ~return_type:TString () ];
    };

  (* impl ord for bool *)
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "ord";
      impl_type_params = [];
      impl_for_type = TBool;
      impl_methods =
        [
          Trait_registry.mk_method_sig ~name:"compare"
            ~params:[ ("x", TBool); ("y", TBool) ]
            ~return_type:(TEnum ("Ordering", []))
            ();
        ];
    };

  (* impl hash for bool *)
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "hash";
      impl_type_params = [];
      impl_for_type = TBool;
      impl_methods = [ Trait_registry.mk_method_sig ~name:"hash" ~params:[ ("x", TBool) ] ~return_type:TInt () ];
    };

  (* string: show, eq, debug, ord, hash *)

  (* impl show for string *)
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TString;
      impl_methods =
        [ Trait_registry.mk_method_sig ~name:"show" ~params:[ ("x", TString) ] ~return_type:TString () ];
    };

  (* impl eq for string *)
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "eq";
      impl_type_params = [];
      impl_for_type = TString;
      impl_methods =
        [
          Trait_registry.mk_method_sig ~name:"eq" ~params:[ ("x", TString); ("y", TString) ] ~return_type:TBool ();
        ];
    };

  (* impl debug for string *)
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "debug";
      impl_type_params = [];
      impl_for_type = TString;
      impl_methods =
        [ Trait_registry.mk_method_sig ~name:"debug" ~params:[ ("x", TString) ] ~return_type:TString () ];
    };

  (* impl ord for string *)
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "ord";
      impl_type_params = [];
      impl_for_type = TString;
      impl_methods =
        [
          Trait_registry.mk_method_sig ~name:"compare"
            ~params:[ ("x", TString); ("y", TString) ]
            ~return_type:(TEnum ("Ordering", []))
            ();
        ];
    };

  (* impl hash for string *)
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "hash";
      impl_type_params = [];
      impl_for_type = TString;
      impl_methods = [ Trait_registry.mk_method_sig ~name:"hash" ~params:[ ("x", TString) ] ~return_type:TInt () ];
    };

  (* float: show, eq, debug, ord, num, neg (NO hash) *)

  (* impl show for float *)
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "show";
      impl_type_params = [];
      impl_for_type = TFloat;
      impl_methods =
        [ Trait_registry.mk_method_sig ~name:"show" ~params:[ ("x", TFloat) ] ~return_type:TString () ];
    };

  (* impl eq for float *)
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "eq";
      impl_type_params = [];
      impl_for_type = TFloat;
      impl_methods =
        [ Trait_registry.mk_method_sig ~name:"eq" ~params:[ ("x", TFloat); ("y", TFloat) ] ~return_type:TBool () ];
    };

  (* impl debug for float *)
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "debug";
      impl_type_params = [];
      impl_for_type = TFloat;
      impl_methods =
        [ Trait_registry.mk_method_sig ~name:"debug" ~params:[ ("x", TFloat) ] ~return_type:TString () ];
    };

  (* impl ord for float *)
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "ord";
      impl_type_params = [];
      impl_for_type = TFloat;
      impl_methods =
        [
          Trait_registry.mk_method_sig ~name:"compare"
            ~params:[ ("x", TFloat); ("y", TFloat) ]
            ~return_type:(TEnum ("Ordering", []))
            ();
        ];
    };

  (* impl num for float *)
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "num";
      impl_type_params = [];
      impl_for_type = TFloat;
      impl_methods =
        [
          Trait_registry.mk_method_sig ~name:"add" ~params:[ ("x", TFloat); ("y", TFloat) ] ~return_type:TFloat ();
          Trait_registry.mk_method_sig ~name:"sub" ~params:[ ("x", TFloat); ("y", TFloat) ] ~return_type:TFloat ();
          Trait_registry.mk_method_sig ~name:"mul" ~params:[ ("x", TFloat); ("y", TFloat) ] ~return_type:TFloat ();
          Trait_registry.mk_method_sig ~name:"div" ~params:[ ("x", TFloat); ("y", TFloat) ] ~return_type:TFloat ();
        ];
    };

  (* impl neg for float *)
  Trait_registry.register_impl ~builtin:true
    {
      impl_trait_name = "neg";
      impl_type_params = [];
      impl_for_type = TFloat;
      impl_methods = [ Trait_registry.mk_method_sig ~name:"neg" ~params:[ ("x", TFloat) ] ~return_type:TFloat () ];
    }

let seed_builtin_values (env : Infer.type_env) : Infer.type_env =
  List.fold_left (fun env_acc (name, poly) -> Infer.TypeEnv.add name poly env_acc) env builtin_types

let builtin_value_env () : Infer.type_env = seed_builtin_values Infer.empty_env

(* ============================================================
   Tests
   ============================================================ *)

let%test "builtin_types has all builtins" =
  let names = List.map fst builtin_types in
  List.for_all (fun name -> List.mem name names) [ "len"; "first"; "last"; "rest"; "push"; "puts" ]

let%test "builtin_value_env creates valid environment" =
  let env = builtin_value_env () in
  Infer.TypeEnv.cardinal env = 6

let%test "builtin traits are registered" =
  Trait_registry.clear ();
  init_builtin_traits ();
  let show_exists = Trait_registry.lookup_trait "show" <> None in
  let eq_exists = Trait_registry.lookup_trait "eq" <> None in
  let ord_exists = Trait_registry.lookup_trait "ord" <> None in
  let rem_exists = Trait_registry.lookup_trait "rem" <> None in
  show_exists && eq_exists && ord_exists && rem_exists

let%test "puts is effectful, other builtins are pure" =
  let is_effectful = function
    | Types.TFun (_, _, true) -> true
    | _ -> false
  in
  let is_pure = function
    | Types.TFun (_, _, false) -> true
    | _ -> false
  in
  let puts_type = List.assoc "puts" builtin_types in
  let len_type = List.assoc "len" builtin_types in
  let (Types.Forall (_, puts_mono)) = puts_type in
  let (Types.Forall (_, len_mono)) = len_type in
  is_effectful puts_mono && is_pure len_mono
