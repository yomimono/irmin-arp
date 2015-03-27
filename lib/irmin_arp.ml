module Entry = struct
  type result = [ `Ok of Macaddr.t | `Timeout ]
  type entry = 
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

module Table : Irmin.Contents.S = struct
  module Path = Irmin.Path.String_list
  module M = Map.Make(Ipaddr.V4)

  module Ops = struct

    type t = Entry.entry M.t (* map from ip -> entry *)

    (* read the entire map from a cstruct *)
    let read buf = M.empty
    let write b buf = Cstruct.create 0
    let size_of p = 0
    let of_json (t : Ezjsonm.value) = M.empty
    let to_json p = Ezjsonm.unit ()
    let hash p = 0
    let compare p q = M.compare (Entry.compare) p q
    let equal p q = true
  end

  include Ops
  let merge _path = Irmin.Merge.default (module Tc.Option(Ops))

end

