module Arp = struct
  type arp = {
    op: [ `Request |`Reply |`Unknown of int ];
    sha: Macaddr.t;
    spa: Ipaddr.V4.t;
    tha: Macaddr.t;
    tpa: Ipaddr.V4.t;
  }
  module Parse = struct
    let garp src_mac src_ip =
    { op = `Reply;
      sha = src_mac;
      tha = Macaddr.broadcast;
      spa = src_ip;
      tpa = Ipaddr.V4.any;
    }

    let cstruct_of_arp arp =
    let open Wire_structs.Arpv4_wire in
    (* Obtain a buffer to write into *)
    (* note that sizeof_arp includes sizeof_ethernet by what's currently in
         wire_structs.ml *)
    let buf = Cstruct.create (Wire_structs.Arpv4_wire.sizeof_arp) in

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
    buf

  let arp_of_cstruct buf = 
    let open Wire_structs.Arpv4_wire in
    let unusable buf =
      (* we only know how to deal with ethernet <-> IPv4 *)
      get_arp_htype buf <> 1 || get_arp_ptype buf <> 0x0800 
      || get_arp_hlen buf <> 6 || get_arp_plen buf <> 4
    in
    if (Cstruct.len buf) < sizeof_arp then `Too_short else begin
      if (unusable buf) then `Unusable else begin
        let op = match get_arp_op buf with
          | 1 -> `Request
          | 2 -> `Reply
          | n -> `Unknown n
        in
        let src_mac = copy_arp_sha buf in
        let target_mac = copy_arp_tha buf in
        match (Macaddr.of_bytes src_mac, Macaddr.of_bytes target_mac) with
        | None, Some _ -> `Bad_mac [ src_mac ]
        | Some _, None -> `Bad_mac [ target_mac ]
        | None, None -> `Bad_mac [ src_mac ; target_mac ]
        | Some src_mac, Some target_mac ->
          let src_ip = Ipaddr.V4.of_int32 (get_arp_spa buf) in
          let target_ip = Ipaddr.V4.of_int32 (get_arp_tpa buf) in
          `Ok { op; 
                sha = src_mac; spa = src_ip; 
                tha = target_mac; tpa = target_ip
              }
      end
    end
    let is_garp_for ip buf = match arp_of_cstruct buf with
      | `Ok arp -> arp.op = `Reply && arp.tha = Macaddr.broadcast
      | _ -> false
end


(* much cribbed from mirage-tcpip/lib/arpv4.ml *)
module Make (Ethif : V1_LWT.ETHIF) (Clock: V1.CLOCK) (Maker : Irmin.S_MAKER)
= struct
  module T = Table.Make(Irmin.Path.String_list)
  module I = Irmin.Basic (Maker) (T)
  type arp = {
    op: [ `Request |`Reply |`Unknown of int ];
    sha: Macaddr.t;
    spa: Ipaddr.V4.t;
    tha: Macaddr.t;
    tpa: Ipaddr.V4.t;
  }

  type cache = (string -> ([ `BC ], T.Path.t, T.t) Irmin.t)

  type t = { 
    ethif: Ethif.t;
    bound_ips: Ipaddr.V4.t list;
    cache: cache
  } 
  let arp_timeout = 60. (* age entries out of cache after this many seconds *)
  let probe_repeat_delay = 1.5 (* per rfc5227, 2s >= probe_repeat_delay >= 1s *)
  let probe_num = 3 (* how many probes to send before giving up *)

  let string_of_arp arp = 
    let string_of_op = function
      | `Request -> "Request"
      | `Reply -> "Reply"
      | `Unknown n -> Printf.sprintf "Unknown op: %d" n
    in
    Printf.sprintf "Op %s: %s, %s -> %s %s\n%!" 
      (string_of_op arp.op)
      (Macaddr.to_string arp.sha) (Ipaddr.V4.to_string arp.spa)
      (Macaddr.to_string arp.tha) (Ipaddr.V4.to_string arp.tpa)

  (* TODO: treatment of multicast ethernet address messages differs between
     routers and end hosts; we have no way of knowing which we are without
     taking a setup parameter. *)
  let create ethif config = 
    let open Lwt in
    let store = Irmin.basic (module Maker) (module T) in
    (* currently the only impl of task is Irmin_unix.task; we could write our
       own, I guess *)
    let task str = 
      Irmin.Task.create ~date:(Int64.of_float (Clock.time ())) ~owner:"seal" str in
    Irmin.create store config task >>= fun cache ->
    Irmin.update (cache "Arp.create: Initial empty cache") T.Path.empty T.empty 
    >>= fun () ->
    Lwt.return ({ ethif; bound_ips = []; cache; })
  let add_ip t ip = 
    match List.mem ip (t.bound_ips) with
    | true -> Lwt.return t
    | false -> Lwt.return { t with bound_ips = (ip :: t.bound_ips)}
  let remove_ip t ip =
    match List.mem ip (t.bound_ips) with
    | false -> Lwt.return t
    | true -> 
      let is_not_ip other_ip = ((Ipaddr.V4.compare ip other_ip) <> 0) in
      Lwt.return { t with bound_ips = (List.filter is_not_ip t.bound_ips) }
  let get_ips t = t.bound_ips

  (* construct an arp record representing a gratuitious arp announcement for
     ip *)
  let garp t ip = Parse.garp (Ethif.mac t.ethif) ip

  let notify t ip mac =
    let open Lwt in 
    let open Entry in
    let now = Clock.time () in
    let expire = now +. arp_timeout in
    try
      Irmin.read_exn (t.cache "lookup") T.Path.empty 
      >>= fun table ->
      match T.find ip table with
      | Pending (_, w) ->
        let str = "entry resolved: " ^ Ipaddr.V4.to_string ip ^ " -> " ^
                  Macaddr.to_string mac in
        let updated = T.add ip (Confirmed (expire, mac)) table in
        Irmin.update (t.cache str) T.Path.empty updated >>= fun
          () ->
        Lwt.wakeup w (`Ok mac);
        Lwt.return_unit
      | Confirmed _ ->
        let str = "entry updated: " ^ Ipaddr.V4.to_string ip ^ " -> " ^
                  Macaddr.to_string mac in
        let updated = T.add ip (Confirmed (expire, mac)) table in
        Irmin.update (t.cache str) T.Path.empty updated >>= fun
          () ->
        Lwt.return_unit
    with
    | Not_found ->
      let str = "entry added: " ^ Ipaddr.V4.to_string ip ^ " -> " ^
                Macaddr.to_string mac in
      Irmin.read_exn (t.cache "lookup") T.Path.empty >>= fun table ->
      let updated = T.add ip (Confirmed (expire, mac)) table in
      Irmin.update (t.cache str) T.Path.empty updated >>= fun
        () ->
      Lwt.return_unit

  let output t arp =
    Ethif.write t.ethif (Parse.cstruct_of_arp arp)

  let rec input t frame =
    let open Wire_structs.Arpv4_wire in
    MProf.Trace.label "arpv4.input";
    match Parse.arp_of_cstruct frame with
    | `Too_short | `Unusable | `Bad_mac _ -> Lwt.return_unit
    | `Ok arp ->
      match arp.op with
      | `Request ->
        (* Received ARP request, check if we can satisfy it from
           our own IPv4 list *)
        if List.mem arp.tpa t.bound_ips then 
          output t 
            { op=`Reply; 
              sha = Ethif.mac t.ethif; tha = arp.sha; 
              (* just switch src and dst ips for reply *)
              spa = arp.tpa; tpa = arp.spa 
            }
        else Lwt.return_unit
      | `Reply ->
        (* If we have pending entry, notify the waiters that answer is ready *)
        notify t arp.spa arp.sha
      | n -> (* we don't know what to do for any other request type, so take
                no action, effectively discarding the packet *)
        Lwt.return_unit

  let set_ips t ips = 
    let open Lwt in
    (* it would be nice if there were some provision for "uh you really don't
       want to do that, that IP is in the cache already" *)
    Lwt.join (List.map (fun ip -> output t (garp t ip)) ips) >>= fun () ->
    Lwt.return { t with bound_ips = ips }
end

end
