type result = [ `Ok of Macaddr.t | `Timeout ]
type t = 
  | Pending of result Lwt.t * result Lwt.u
  | Confirmed of float * Macaddr.t

let pending_str = "Waiting to resolve..."

let to_string = function
  | Pending _ -> pending_str
  | Confirmed (time, mac) -> Printf.sprintf "%s expiring at %f"
                               (Macaddr.to_string mac) time

let to_json = function
  | Confirmed (time, mac) -> 
    let expiry = Ezjsonm.float time in
    let mac = Ezjsonm.string (Macaddr.to_string mac) in
    Ezjsonm.dict [("expiry", expiry); ("mac", mac)]
  | Pending _ ->
    Ezjsonm.string pending_str

let of_json (json : Ezjsonm.value) : t option = match json with
  (* for now, don't try to reflect that we had tried to look up an entry *)
  | `String x when (String.compare x pending_str = 0) -> None
  | `O items -> (
      try let open Ezjsonm in
        let address = Macaddr.of_string_exn 
            (get_string (find (dict items) ["mac"])) in
        let expiry = get_float (find (dict items) ["expiry"]) in
        Some (Confirmed (expiry, address))
      with
      | Not_found -> None
    )
  | `A _ | `Null | `Bool _ | `Float _ -> None
  | `String _ -> None

let is_pending = function | Confirmed _ -> false | Pending _ -> true

(* confirmed trumps pending
   pending trumps absent *)
let compare p q =
  match (p, q) with
  | Pending (p,_),  Pending (q,_) -> compare p q
  | Pending _, Confirmed _ -> -1
  | Confirmed (p_time, _), Confirmed (q_time, _) -> compare p_time q_time
  | Confirmed _, Pending _ -> 1 

let make_pending (thread, waker) = Pending (thread, waker)
let make_confirmed f m = Confirmed (f, m)

