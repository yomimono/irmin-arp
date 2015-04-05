module Key = struct
  (* provide Tc.S0 stuff for Ipaddr.V4.t *)
  include Ipaddr.V4

  let read buf = (Ipaddr.V4.of_string_exn (Mstruct.to_string buf))
  let write k buf = 
    let s  = Ipaddr.V4.to_string k in
    Cstruct.blit_from_string s 0 buf 0 (String.length s);
    Cstruct.sub buf (String.length s) (Cstruct.len buf - (String.length s))
  let size_of k = String.length (Ipaddr.V4.to_string k)
  let to_json k = Ezjsonm.string (Ipaddr.V4.to_string k)
  let of_json = function
    | `String s -> Ipaddr.V4.of_string_exn s
    | `Null | `Bool _ | `O _ | `A _ | `Float _ -> raise (Tc.Read_error "invalid
                                                            json")

  let hash = Hashtbl.hash
  let equal p q = (Ipaddr.V4.compare p q) = 0

end

module Entry = struct

  type result = [ `Ok of Macaddr.t | `Timeout ]
  type t = 
    | Pending of result Lwt.t * result Lwt.u
    | Confirmed of float * Macaddr.t

  let pending_str = "Waiting to resolve..."

  let to_string = function
    | Confirmed (time, mac) -> Printf.sprintf "%s expiring at %f"
                               (Macaddr.to_string mac) time
    | Pending _ -> pending_str

  let to_json = function
    | Confirmed (time, mac) -> 
      let expiry = Ezjsonm.float time in
      let mac = Ezjsonm.string (Macaddr.to_string mac) in
      Ezjsonm.dict [("expiry", expiry); ("mac", mac)]
    | Pending _ ->
      Ezjsonm.string pending_str

  let of_json (json : Ezjsonm.value) : t option = match json with
    | `String x when (String.compare x pending_str = 0) -> 
      (* TODO: create new threads to time out this entry after arp_timeout *)
      let t, u = Lwt.task () in
      Some (Pending (t, u))
    | `O items -> (
        try
          let open Ezjsonm in
        let address = Macaddr.of_string_exn 
            (get_string (find (dict items) ["mac"])) in
        let expiry = get_float (find (dict items) ["expiry"]) in
        Some (Confirmed (expiry, address))
      with
      | Not_found -> None
      )
    | `A _ | `Null | `Bool _ | `Float _ -> None
    | `String _ -> None

  let is_pending = function
    | Confirmed _ -> false
    | Pending _ -> true

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

module Table(M: Map.S with type key = Ipaddr.V4.t)(P: Irmin.Path.S) : sig
  include Irmin.Contents.S 
  val to_map : t -> Entry.t M.t
  val of_map : Entry.t M.t -> t

  module Ops : sig
    include Tc.S0 with type t = Entry.t M.t (* map from ip -> entry *)
  end
end = struct
  module Path = P

  module Ops = struct
    type t = Entry.t M.t (* map from ip -> entry *)

    let hash = Hashtbl.hash
    let compare = M.compare (Entry.compare)
    let equal p q = (compare p q) = 0
    let of_json json = 
      let top_dict = Ezjsonm.get_dict json in
      (* members of top_dict are (name: entry) pairs *)
      let entries = List.map 
          (fun (name, entry) -> (name, Entry.of_json entry)) top_dict
      in
      let mapify map (name, entry) = 
        match (Ipaddr.V4.of_string name), entry with 
        | Some addr, Some entry -> M.add addr entry map
        | _, _ -> map
      in
      List.fold_left mapify M.empty entries

    let to_json map = 
      let add_binding key value json =
        try
          Ezjsonm.update json [(Ipaddr.V4.to_string key)] (Some (Entry.to_json value) )
        with
        | Not_found -> raise (Invalid_argument (Printf.sprintf 
                                                  "Couldn't make json out of key %s and entry %s" 
                                                  (Ipaddr.V4.to_string key) (Entry.to_string value)))
      in
      M.fold add_binding map (Ezjsonm.dict [])

    let read buf = 
      let str = Mstruct.to_string buf in
      let json = 
        try 
          Ezjsonm.unwrap (Ezjsonm.from_string str)
        with Ezjsonm.Parse_error _ -> raise (Tc.Read_error "invalid json")
      in
      of_json json

    (* no regrets *)
        (* why is there no signalling like "uh bro I can't fit in here"? *)
    let write m buf = 
      let s = Ezjsonm.to_string (Ezjsonm.wrap (to_json m)) in
      let to_blit = String.length s in
      Cstruct.blit_from_string s 0 buf 0 to_blit;
      Cstruct.sub buf to_blit ((Cstruct.len buf) - to_blit)

    let size_of m = String.length (Ezjsonm.to_string (Ezjsonm.wrap (to_json m)))
  end

  include Ops

  let to_map t = t
  let of_map = to_map

  let merge _path ~old t1 t2 = 
    let module Map_merge = Irmin.Merge.Map(M)(Key) in

    let merge_maps key val1 val2 =
      let opt_compare v1 v2 =
        match v1, v2 with
        | None, None -> 0
        | Some v1, None -> 1
        | None, Some v2 -> -1
        | Some v1, Some v2 -> Entry.compare v1 v2
      in
      match (opt_compare val1 val2) with
      | 1 | 0 -> val1
      | -1 -> val2
    in

    (* any nodes removed in both t1 and t2 should stay removed, so we don't
       bother merging old vs t1/t2 *)
    Irmin.Merge.OP.ok (M.merge merge_maps t1 t2)

  let merge path = Irmin.Merge.option (module Ops) (merge path)

end

