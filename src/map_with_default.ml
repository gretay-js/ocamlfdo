open Core
include Map_with_default_intf

module Make (Key: Key) = struct
  module Key = Key

  module M = Map.Make(Key)

  type +'a t =
    { def: 'a;
      map: 'a M.t
    } [@@deriving sexp]

  let default def = { def; map = M.empty }

  let set { def; map } ~key ~data = { def; map = M.set map ~key ~data }

  let find { def; map } key =
    try M.find_exn map key
    with Not_found_s _ -> def

  let update { def; map } k ~f =
    { def;
      map = M.update map k ~f:(function
        | None -> f def
        | Some v -> f v)
    }

  let merge a b ~f =
    { def = f a.def b.def;
      map = M.merge a.map b.map ~f:(fun ~key:_ v ->
        match v with
        | `Both(a', b') -> Some (f a' b')
        | `Left a' -> Some (f a' b.def)
        | `Right b' -> Some (f a.def b'))
    }

  let map { def; map } ~f = { def = f def; map = M.map map ~f}

  let equal cmp a b =
    M.fold2 a.map b.map ~init:(cmp a.def b.def) ~f:(fun ~key:_ ~data acc ->
      acc && match data with
      | `Both(a', b') -> cmp a' b'
      | `Left a' -> cmp a' b.def
      | `Right b' -> cmp a.def b')
end
