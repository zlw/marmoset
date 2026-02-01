type scope = GlobalScope

type symbol = {
  name : string;
  scope : scope;
  index : int;
}

type t = {
  store : (string, symbol) Hashtbl.t;
  mutable num_definitions : int;
}

let create () : t = { store = Hashtbl.create 16; num_definitions = 0 }

let define (st : t) (name : string) : symbol =
  let sym = { name; scope = GlobalScope; index = st.num_definitions } in
  Hashtbl.replace st.store name sym;
  st.num_definitions <- st.num_definitions + 1;
  sym

let resolve (st : t) (name : string) : symbol option = Hashtbl.find_opt st.store name

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
