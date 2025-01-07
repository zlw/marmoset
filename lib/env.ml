type env = {
  store : (string, Value.value) Hashtbl.t;
  outer : env option;
}

let init : env = { store = Hashtbl.create 10; outer = None }

let rec get (e : env) (key : string) : Value.value option =
  match Hashtbl.find_opt e.store key with
  | Some v -> Some v
  | None -> ( match e.outer with Some outer -> get outer key | None -> None)

let set e key val' =
  Hashtbl.replace e.store key val';
  e

let wrap e = { store = Hashtbl.create 10; outer = Some e }
