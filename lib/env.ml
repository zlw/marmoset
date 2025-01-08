open Base

module Hashtbl_printable = struct
  type ('k, 's) t = ('k, 's) Hashtbl.t

  let pp pp_key pp_value ppf values =
    Hashtbl.iteri values ~f:(fun ~key ~data ->
        Stdlib.Format.fprintf ppf "@[<1>%a: %a@]@." pp_key key pp_value data)
end

type 'a env = {
  store : (string, 'a) Hashtbl_printable.t;
  outer : 'a env option;
}
[@@deriving show]

let init () : 'a env = { store = Hashtbl.create (module String) ~size:10; outer = None }

let rec get (e : 'a env) (key : string) : 'a option =
  match Hashtbl.find e.store key with
  | Some v -> Some v
  | None -> ( match e.outer with Some outer -> get outer key | None -> None)

let set e key val' =
  Hashtbl.set e.store ~key ~data:val';
  e

let wrap e = { store = Hashtbl.create (module String) ~size:10; outer = Some e }
