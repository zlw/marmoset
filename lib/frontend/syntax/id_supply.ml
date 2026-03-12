module Id_supply = struct
  type t = { mutable next_id : int }

  let create start = { next_id = start }

  let fresh t =
    let id = t.next_id in
    t.next_id <- t.next_id + 1;
    id
end
