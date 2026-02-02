type scope =
  | GlobalScope
  | LocalScope
  | BuiltinScope
  | FreeScope
  | FunctionScope

type symbol = {
  name : string;
  scope : scope;
  index : int;
}

type t = {
  store : (string, symbol) Hashtbl.t;
  mutable num_definitions : int;
  outer : t option;
  free_symbols : symbol Dynarray.t; (* tracks free variables captured from outer scopes *)
}

let create () : t =
  { store = Hashtbl.create 16; num_definitions = 0; outer = None; free_symbols = Dynarray.create () }

(* Create an enclosed symbol table with an outer scope *)
let create_enclosed (outer : t) : t =
  { store = Hashtbl.create 16; num_definitions = 0; outer = Some outer; free_symbols = Dynarray.create () }

let define (st : t) (name : string) : symbol =
  let scope =
    match st.outer with
    | None -> GlobalScope
    | Some _ -> LocalScope
  in
  let sym = { name; scope; index = st.num_definitions } in
  Hashtbl.replace st.store name sym;
  st.num_definitions <- st.num_definitions + 1;
  sym

(* Define a free variable (captured from outer scope) *)
let define_free (st : t) (original : symbol) : symbol =
  Dynarray.add_last st.free_symbols original;
  let sym = { name = original.name; scope = FreeScope; index = Dynarray.length st.free_symbols - 1 } in
  Hashtbl.replace st.store original.name sym;
  sym

let rec resolve (st : t) (name : string) : symbol option =
  match Hashtbl.find_opt st.store name with
  | Some sym -> Some sym
  | None -> (
      match st.outer with
      | Some outer -> (
          match resolve outer name with
          | None -> None
          | Some sym -> (
              (* Found in outer scope - check if we need to capture it *)
              match sym.scope with
              | GlobalScope | BuiltinScope ->
                  (* Globals and builtins don't need to be captured *)
                  Some sym
              | LocalScope | FreeScope ->
                  (* Local or already-free variables need to be captured *)
                  Some (define_free st sym)
              | FunctionScope ->
                  (* Function scope references should be returned as-is *)
                  Some sym))
      | None -> None)

(* Define a builtin function at a fixed index *)
let define_builtin (st : t) (index : int) (name : string) : symbol =
  let sym = { name; scope = BuiltinScope; index } in
  Hashtbl.replace st.store name sym;
  (* Note: does NOT increment num_definitions *)
  sym

(* Define a function name for recursive references (always index 0) *)
let define_function_name (st : t) (name : string) : symbol =
  let sym = { name; scope = FunctionScope; index = 0 } in
  Hashtbl.replace st.store name sym;
  (* Note: does NOT increment num_definitions *)
  sym

(* List of builtin names in order of their indices *)
let builtin_names = [ "len"; "puts"; "first"; "last"; "rest"; "push" ]

module Test = struct
  let%test "test_define" =
    let st = create () in
    let a = define st "a" in
    let b = define st "b" in
    a.name = "a" && a.index = 0 && b.name = "b" && b.index = 1

  let%test "test_resolve_global" =
    let st = create () in
    let _ = define st "a" in
    let _ = define st "b" in
    match (resolve st "a", resolve st "b") with
    | Some a, Some b -> a.name = "a" && a.index = 0 && b.name = "b" && b.index = 1
    | _ -> false

  let%test "test_resolve_undefined" =
    let st = create () in
    resolve st "x" = None
end
