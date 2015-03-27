(* need documentation for what these types are actually supposed to mean/do --
   maybe it's somewhere in irmin.mli or in the wiki tutorials?  
   The example in `custom_merge.ml` is using some predefined stuff which is
   great, maybe we can pull something like that in *)

(* what's the name of the module type signature we're trying to fulfill?
   Contents.S, I suspect? *)

(* It looks like there are combinators in Contents for nicely automaking
   Contents.S from modules that implement AO and define submodules Key and Value; 
   that's probably what we want to do?  Maybe? *)

(* The API for this is internally consistent and avoids repetition, 
   but it's really damn difficult to figure out what's going on here 
   and how to do things *)

(* Like, I want to make a contents store, which means I need to make a module
   with submodules Key and Value which also satisfies module type AO, implying
   that it satisfies module type RO, which requires types key and value 
   (among other stuff) *)

(* just getting the right stubs so this thing compiles is quite the adventure *)

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
    (* huh, this signature makes it clear why we need Path.t to be >1 thing, since
       we get only one argument to merge; presumably the semantics here are "merge
       together all the things in `path`" *)

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

