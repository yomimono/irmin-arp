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
      let open Arpv4_wire in
      (* Obtain a buffer to write into *)
      (* note that sizeof_arp includes sizeof_ethernet by what's currently in
           wire_structs.ml *)
      let buf = Cstruct.create (Arpv4_wire.sizeof_arp) in

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
      let open Arpv4_wire in
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
  module Make (Ethif : V1_LWT.ETHIF) (Clock: V1.CLOCK) (Time: V1_LWT.TIME)
      (Random: V1.RANDOM) (Maker : Irmin.S_MAKER) = struct
    module T = Table.Make(Irmin.Path.String_list)
    module I = Irmin.Basic (Maker) (T)
    type cache = (string -> ([ `BC ], T.Path.t, T.t) Irmin.t)

    type result = [ `Ok of Macaddr.t | `Timeout ]
    type ipaddr = Ipaddr.V4.t
    type macaddr = Macaddr.t
    type buffer = Cstruct.t
    type 'a io = 'a Lwt.t
    type error = [ `Fs | `Network | `Semantics | `Unknown of string ]

    type t = {
      ethif: Ethif.t;
      mutable bound_ips: ipaddr list;
      (* mutable for compability with existing ipv4 code :( *)
      node: T.Path.t;
      owner: string;
      config: Irmin.config;
      store: (T.Path.t, T.t) Irmin.basic;
      pending: (ipaddr, result Lwt.u) Hashtbl.t;
      (* use a hashtable, since everything else in here is mutable :( *)
      cache: cache
    }

    type id = t

    let (>>=) = Lwt.bind

    let dither_wait time =
      (* adjust a minimum wait time by some small amount of ms *)
      let ep = Random.int 10 in
      time +. ((float_of_int ep) *. 0.001)

    let pp fmt t =
      Irmin.read_exn (t.cache "read map for prettyprint") t.node >>= fun map ->
      Format.fprintf fmt "%s" (Ezjsonm.to_string (Ezjsonm.wrap (T.to_json map)));
      Lwt.return_unit

    let disconnect t = Lwt.return_unit (* TODO: kill tick somehow *)

    let arp_timeout = 60. (* age entries out of cache after this many seconds *)
    let expiry_check_interval = 1. (* check for expired entries this often *)
    let probe_repeat_delay = 1.5 (* per rfc5227, 2s >= probe_repeat_delay >= 1s *)
    let probe_num = 3 (* how many probes to send before giving up *)

    let task owner =
      Irmin.Task.create ~date:(Int64.of_float (Clock.time ())) ~owner

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

    let rec tick t () =
      Irmin.head_exn (t.cache "starting expiry") >>= fun head ->
      Irmin.of_head t.store t.config (task t.owner) head >>= fun our_br ->
      Irmin.read_exn (our_br "read for timeouts") t.node >>= fun table ->
      let now = Clock.time () in
      let updated = T.expire table now in
      (* TODO: this could stand to either not be committed if no changes happen,
         or to have a more informative commit message, or both *)
      Irmin.update (our_br "Arp.tick: updating to age out old entries") t.node updated >>= fun () ->
      Irmin.merge_exn "Arp.tick: merge expiry branch" our_br ~into:t.cache >>=
      fun () ->
      Time.sleep (dither_wait expiry_check_interval) >>= fun () -> tick t ()

    (* TODO: treatment of multicast ethernet address messages differs between
       routers and end hosts; we have no way of knowing which we are without
       taking a setup parameter. *)
    let connect ethif config ~pull ~node =
      let seed_cache cache x =
        let process = function
          | `Ok `Ok -> Lwt.return `Ok
          | `Ok `Error -> Lwt.return (`Error `Fs)
          | `Ok `No_head -> Lwt.return (`Error `Semantics)
          | `Conflict s -> failwith s (* conflicts should be precluded by
                                         semantics of the merge function *)
        in
        try
          let x = Irmin.remote_basic x in
          Irmin.pull (cache "Arp.create: Merge seed repository") x `Merge >>= process
        with
          (* TODO: a better serialization method in table.ml will require a change here *)
        | Ezjsonm.Parse_error _ -> Lwt.return (`Error `Semantics)
      in
      let empty_cache cache =
        Irmin.update (cache "Arp.create: Initial empty cache") node T.empty
        >>= fun () -> Lwt.return `Ok
      in
      let check_node node cache =
        Irmin.read (cache "Arp.create: read for node entry") node >>= function
        | Some x -> Lwt.return `Ok
        | None -> Lwt.return (`Error `Semantics)
      in
      let aux cache = function
        | [] -> empty_cache cache
        | l  ->
          let rec init cache = function
            | [] -> check_node node cache (* disallow empty node *)
            | x::xs -> seed_cache cache x >>= function
              | `Ok -> init cache xs
              | `Error s -> Lwt.return (`Error s)
          in
          init cache l
      in
      let store = Irmin.basic (module Maker) (module T) in
      let node = T.Path.create node in
      let owner = String.concat "/" node in
      Random.self_init ();
      Irmin.create store config (task owner) >>= fun cache ->
      aux cache pull >>= function
      | `Error s -> Lwt.return (`Error s)
      | `Ok ->
        let t = { ethif; bound_ips = []; node; owner; config; store; cache;
                  pending = Hashtbl.create 4 } in
        Lwt.async (tick t);
        Lwt.return (`Ok t)

    let push t remote =
      Irmin.push (t.cache "pushing state to remote store") remote

    (* construct an arp record representing a gratuitious arp announcement for
       ip *)
    let garp t ip = Parse.garp (Ethif.mac t.ethif) ip

    let output t arp =
      Ethif.write t.ethif (Parse.cstruct_of_arp arp)

    let set_ips t ips =
      (* it would be nice if there were some provision for "uh you really don't
         want to do that, that IP is in the cache already" *)
      Lwt.join (List.map (fun ip -> output t (garp t ip)) ips) >>= fun () ->
      t.bound_ips <- ips;
      Lwt.return_unit

    let add_ip t ip =
      match List.mem ip (t.bound_ips) with
      | true -> Lwt.return_unit
      | false -> set_ips t (ip :: t.bound_ips)

    let remove_ip t ip =
      match List.mem ip (t.bound_ips) with
      | false -> Lwt.return_unit
      | true ->
        let is_not_ip other_ip = ((Ipaddr.V4.compare ip other_ip) <> 0) in
        set_ips t (List.filter is_not_ip t.bound_ips)

    let get_ips t = t.bound_ips

    let notify t ip mac =
      let merge str new_table our_br head =
        Irmin.update (our_br ("Arp.notify: " ^ str)) t.node new_table >>= fun () ->
        Irmin.merge_exn "Arp.notify: merge notify branch" our_br ~into:t.cache
      in
      Irmin.head_exn (t.cache "starting update") >>= fun head ->
      Irmin.of_head t.store t.config (task t.owner) head >>= fun our_br ->
      Irmin.read_exn (our_br "lookup") t.node >>= fun table ->
      let now = Clock.time () in
      let expire = now +. arp_timeout in
      let updated = T.add ip (Entry.Confirmed (expire, mac)) table in
      match Hashtbl.mem t.pending ip with
      | true ->
        let w = Hashtbl.find t.pending ip in
        let str = "query answered: " ^ Ipaddr.V4.to_string ip ^ " -> " ^
                  Macaddr.to_string mac in
        merge str updated our_br head >>= fun () ->
        Lwt.wakeup w (`Ok mac);
        Lwt.return_unit
      | false ->
        let str = "gratuitous arp: " ^ Ipaddr.V4.to_string ip ^ " -> " ^
                  Macaddr.to_string mac in
        merge str updated our_br head

    let probe t ip =
      let source_ip = match t.bound_ips with
        | hd :: _ -> hd
        | [] -> Ipaddr.V4.any
      in
      { op=`Request; tha = Macaddr.broadcast; sha = (Ethif.mac t.ethif);
        tpa = ip; spa = source_ip; }

    let output_probe t ip = output t (probe t ip)

    let rec input t frame =
      let open Arpv4_wire in
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

    (* Query the cache for an ARP entry, which may result in the sender sleeping
       waiting for a response *)
    let query t ip =
      let (>>=) = Lwt.bind in
      let open Entry in
      Irmin.read_exn (t.cache "Arp.query") t.node
      >>= fun table ->
      try
        match T.find ip table with
        | Confirmed (_, mac) -> Lwt.return (`Ok mac)
      with
      | Not_found ->
        let response, waker = MProf.Trace.named_wait "ARP response" in
        Hashtbl.add (t.pending) ip waker;
        let rec retry n () =
          (* First request, so send a query packet *)
          output_probe t ip >>= fun () ->
          Lwt.choose [ (response >>= fun _ -> Lwt.return `Ok);
                       (Time.sleep (dither_wait probe_repeat_delay)
                        >>= fun () -> Lwt.return `Timeout)
                     ] >>= function
          | `Ok -> Lwt.return_unit
          | `Timeout ->
            (* TODO: track the retry number in Irmin as well *)
            if n < probe_num then begin
              let n = n+1 in
              retry n ()
            end else begin
              Lwt.wakeup waker `Timeout;
              Lwt.return_unit
            end
        in
        Lwt.async (retry 0);
        response
  end

end
