module Key : sig
  include Tc.S0 with type t = Ipaddr.V4.t
end = struct
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
    | `Null | `Bool _ | `O _ | `A _ | `Float _ -> 
      raise (Tc.Read_error "invalid json")
  let hash = Hashtbl.hash
  let equal p q = (Ipaddr.V4.compare p q) = 0
end

module Entry : sig
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
end = struct

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
    | Pending _, Pending _ -> -1 (* arbitrarily; doesn't really matter *)
    | Pending _, Confirmed _ -> -1
    | Confirmed (p_time, _), Confirmed (q_time, _) -> compare p_time q_time
    | Confirmed _, Pending _ -> 1 

  let make_pending (thread, waker) = Pending (thread, waker)
  let make_confirmed f m = Confirmed (f, m)
end

module Table(P: Irmin.Path.S) : sig
  module M : Map.S with type key = Ipaddr.V4.t
  include Irmin.Contents.S 
  val to_map : t -> Entry.t M.t
  val of_map : Entry.t M.t -> t

  module Ops : sig
    include Tc.S0 with type t = Entry.t M.t (* map from ip -> entry *)
  end
end = struct
  module Path = P
  module M = Map.Make(Ipaddr.V4)

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

  let merge _path ~(old : Entry.t M.t Irmin.Merge.promise) t1 t2 = 
    let open Irmin.Merge.OP in
    old () >>| fun old -> 
    (* TODO: it would be nicer to only wait on the computation of the LCA in the
       case where we actually need it to resolve a merge conflict *)
    let old = match old with None -> M.empty | Some old -> old in
    let merge_maps key val1 val2 =
      let comp_of_operation ~direction key present =
        let operation key new_value =
          match M.mem key old with
          | false -> `Added
          | true -> 
            let old_value = M.find key old in
            if new_value = old_value then `Unchanged else `Modified
        in
        let multiplier = match direction with | `Left -> 1 | `Right -> -1 in
        match operation key present with
        | `Added -> multiplier * 1 (* element added in t1, keep it *)
        | `Unchanged -> multiplier * -1 (* element removed by t2, remove it *)
        | `Modified -> multiplier * 1 (* element modified by t1 and removed by t2.  keep the
                                         modified value *)
      in
      let opt_compare v1 v2 =
        match v1, v2 with
        | None, None -> 0
        | Some v1, Some v2 -> Entry.compare v1 v2
        | Some present, None -> comp_of_operation ~direction:`Left key present
        | None, Some present -> comp_of_operation ~direction:`Right key present
      in
      if (opt_compare val1 val2) < 0 then val2 else val1
    in
    Irmin.Merge.OP.ok (M.merge merge_maps t1 t2)

  let merge path = Irmin.Merge.option (module Ops) (merge path)

end

module Arp = struct
  (* much cribbed from mirage-tcpip/lib/arpv4.ml *)
  module Make (Ethif : V1_LWT.ETHIF) = struct
    type arp = {
      op: [ `Request |`Reply |`Unknown of int ];
      sha: Macaddr.t;
      spa: Ipaddr.V4.t;
      tha: Macaddr.t;
      tpa: Ipaddr.V4.t;
    }

    type t = { 
      ethif: Ethif.t;
      ips: Ipaddr.V4.t list;
    } 

    let is_garp ip buf = true

    let arp_of_cstruct buf = None
    let cstruct_of_arp query = None

    let create ethif = { ethif; ips = [] }
    let add_ip t ip = 
      match List.mem ip (t.ips) with
      | true -> Lwt.return t
      | false -> Lwt.return { t with ips = (ip :: t.ips)}
    let remove_ip t ip =
      match List.mem ip (t.ips) with
      | false -> Lwt.return t
      | true -> 
        let is_not_ip other_ip = ((Ipaddr.V4.compare ip other_ip) <> 0) in
        Lwt.return { t with ips = (List.filter is_not_ip t.ips) }
    let get_ips t = t.ips

    (* construct an arp record representing a gratuitious arp announcement for
       ip *)
    let garp t ip =
      { op = `Reply;
        tha = Macaddr.broadcast;
        sha = Ethif.mac t.ethif;
        tpa = Ipaddr.V4.any;
        spa = ip;
      }

    (* output taken directly from arpv4.ml *)
    let output t arp =
      let open Wire_structs.Arpv4_wire in
      (* Obtain a buffer to write into *)
      let buf = Cstruct.create (Wire_structs.Arpv4_wire.sizeof_arp +
                                Wire_structs.sizeof_ethernet) in
      (* Write the ARP packet *)
      let dmac = Macaddr.to_bytes arp.tha in
      let smac = Macaddr.to_bytes arp.sha in
      let spa = Ipaddr.V4.to_int32 arp.spa in
      let tpa = Ipaddr.V4.to_int32 arp.tpa in
      let op =
        match arp.op with
        |`Request -> 1
        |`Reply -> 2
        |`Unknown n -> n
      in
      set_arp_dst dmac 0 buf;
      set_arp_src smac 0 buf;
      set_arp_ethertype buf 0x0806; (* ARP *)
      set_arp_htype buf 1;
      set_arp_ptype buf 0x0800; (* IPv4 *)
      set_arp_hlen buf 6; (* ethernet mac size *)
      set_arp_plen buf 4; (* ipv4 size *)
      set_arp_op buf op;
      set_arp_sha smac 0 buf;
      set_arp_spa buf spa;
      set_arp_tha dmac 0 buf;
      set_arp_tpa buf tpa;
      (* Resize buffer to sizeof arp packet *)
      let buf = Cstruct.sub buf 0 sizeof_arp in
      Ethif.write t.ethif buf

    let set_ips t ips = 
      (* it would be nice if there were some provision for "uh you really don't
         want to do that, that IP is in the cache already" *)
      List.map (fun ip -> output t (garp t ip)) ips;
      Lwt.return { t with ips = ips }
  end

end
