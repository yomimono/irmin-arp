type result = [ `Ok of Macaddr.t | `Timeout ]
type t = 
  | Confirmed of float * Macaddr.t

let to_string = function
  | Confirmed (time, mac) -> Printf.sprintf "%s expiring at %f"
                               (Macaddr.to_string mac) time

let to_json = function
  | Confirmed (time, mac) -> 
    let expiry = Ezjsonm.float time in
    let mac = Ezjsonm.string (Macaddr.to_string mac) in
    Ezjsonm.dict [("expiry", expiry); ("mac", mac)]

let of_json (json : Ezjsonm.value) : t option = match json with
  (* for now, don't try to reflect that we had tried to look up an entry *)
  | `O items -> (
      try let open Ezjsonm in
        let address = Macaddr.of_string_exn 
            (get_string (find (dict items) ["mac"])) in
        let expiry = get_float (find (dict items) ["expiry"]) in
        Some (Confirmed (expiry, address))
      with
      | Not_found -> None
    )
  | `A _ | `Null | `Bool _ | `Float _ | `String _ -> None

let compare p q =
  match (p, q) with
  | Confirmed (p_time, _), Confirmed (q_time, _) -> compare p_time q_time

let make_confirmed f m = Confirmed (f, m)
