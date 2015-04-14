type result = [ `Ok of Macaddr.t | `Timeout ]
type t = 
  | Pending of result Lwt.t * result Lwt.u
  | Confirmed of float * Macaddr.t

val to_string : t -> string
val to_json : t -> Ezjsonm.value
val of_json : Ezjsonm.value -> t option
val compare : t -> t -> int
val make_confirmed : float -> Macaddr.t -> t
val make_pending : (result Lwt.t * result Lwt.u) -> t
val is_pending : t -> bool
