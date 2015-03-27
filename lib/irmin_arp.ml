(* need types for arp entries *)
type ip = Ipaddr.V4.t
type result = [ `Ok of Macaddr.t | `Timeout ]
type entry = 
  | Pending of result Lwt.t * result Lwt.u
  | Confirmed of float * Macaddr.t

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


module Table : Irmin.Contents.S = struct
  module Path = Irmin.Path.String_list
  module M = Map.Make(Ipaddr.V4)

  module Ops = struct
    (* huh, this signature makes it clear why we need Path.t to be >1 thing, since
       we get only one argument to merge; presumably the semantics here are "merge
       together all the things in `path`" *)

    type t = Ipaddr.V4.t (* map from ip -> entry *)

    (* read the entire map from a cstruct *)
    let read buf = Ipaddr.V4.unspecified

    let write b buf = Cstruct.create 0

    let size_of p = 0

    let of_json (t : Ezjsonm.value) = Ipaddr.V4.unspecified

    let to_json p = Ezjsonm.unit ()

    let hash p = 0

    let compare p q = 0

    let equal p q = true
  end

  include Ops

  (* what are the available Paths?  What do I mean when I'm setting one? *)
  (* stores bind paths to contents, so basically this is a way of denoting what
     thing I'm talking about when I talk about a thing, I guess.  In the
     Git-on-FS model, these are members of a hierarchy in a hierarchical FS,
     with the conceptually-last element being the name of the file itself. *)
  (* In our application, there will be only one element under consideration at
     any given time, so I think this is basically irrelevant -- although perhaps
     instead of modelling this as just one Map, stored in Irmin, we could
     instead label the individual cache entries as if they were elements in an
     FS (since they're uniquely named per-IP), and have just the value of the
     mapping be represented as Contents. *)
  (* It seems like this is actually a fairly natural mapping for something
     that's a key-value store?  Hm, maybe not; the interface expects to have
     lists of "steps" for these -- like directory names in a hierarchical file
     system, presumably? *)
  (* A lot of operations in the examples actually ignore the path, which is
     provided as an argument to the merge function; interesting. *)

  let merge _path = Irmin.Merge.default (module Tc.Option(Ops))

end

