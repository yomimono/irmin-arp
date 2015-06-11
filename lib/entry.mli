type result = [ `Ok of Macaddr.t | `Timeout ]
type t = 
  | Confirmed of float * Macaddr.t

val to_string : t -> string
val to_json : t -> Ezjsonm.value
val of_json : Ezjsonm.value -> t option
val compare : t -> t -> int
val make_confirmed : float -> Macaddr.t -> t
