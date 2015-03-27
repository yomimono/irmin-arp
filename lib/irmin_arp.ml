module Entry = struct
  type result = [ `Ok of Macaddr.t | `Timeout ]
  type t = 
    | Pending of result Lwt.t * result Lwt.u
    | Confirmed of float * Macaddr.t

  (* confirmed trumps pending
     absent trumps nonpending (with additional timing logic?)
     pending trumps absent *)
  let compare p q =
    match (p, q) with
    | Pending _, Pending _ -> -1 (* arbitrarily; doesn't really matter *)
    | Pending _, Confirmed _ -> -1
    | Confirmed (p_time, _), Confirmed (q_time, _) -> compare p_time q_time
    | Confirmed _, Pending _ -> 1 

  let make_pending () = let t, u = Lwt.task () in Pending (t, u)
  let make_confirmed f m = Confirmed (f, m)
end

module Table(M: Map.S)(P: Irmin.Path.S) : sig
  include Irmin.Contents.S 
end = struct
  module Path = P

  module Ops : sig 
    include Tc.S0 with type t = Entry.t M.t (* map from ip -> entry *)
  end = struct
    type t = Entry.t M.t (* map from ip -> entry *)

    let read buf = M.empty
    let write b buf = Cstruct.create 0
    let size_of _ = 0
    let of_json _ = M.empty
    let to_json _ = Ezjsonm.unit ()
    let hash p = 0
    let compare = M.compare (Entry.compare)
    let equal p q = true
  end

  include Ops

  let to_map t = t

  let merge _path = Irmin.Merge.default (module Tc.Option(Ops))

end

