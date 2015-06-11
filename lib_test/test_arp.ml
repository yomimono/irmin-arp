open Lwt
open Common

let first_ip = Ipaddr.V4.of_string_exn "192.168.3.1"
let second_ip = Ipaddr.V4.of_string_exn "192.168.3.10"
let sample_mac = Macaddr.of_string_exn "10:9a:dd:c0:ff:ee"

let send_buf_sleep_then_dc speak_netif listen_netif bufs () =
  Lwt.join (List.map (V.write speak_netif) bufs) >>= fun () ->
  Lwt_unix.sleep 0.1 >>= fun () ->
  V.disconnect listen_netif

let create_is_consistent () =
  (* Arp.create returns something bearing resemblance to an Arp.t *)
  (* possibly assert some qualities of a freshly-created ARP interface -- e.g.
     no bound IPs, empty cache, etc *)
  let root = root ^ "/create_is_consistent" in
  get_arp ~root () >>= fun (config, _, _, _, a) ->
  OUnit.assert_equal [] (A_fs.get_ips a);
  (* cache should contain an empty map at T.Path.empty *)
  let store = Irmin.basic (module Irmin_backend_fs) (module T) in
  Irmin.create store config Irmin_unix.task >>= fun cache ->
  Irmin.read (cache "create_is_consistent checking for empty map") T.Path.empty >>=
  function
  | None -> OUnit.assert_failure "Expected location of the cache was empty"
  | Some map -> OUnit.assert_equal T.empty map; Lwt.return_unit

(* normally I'd test to make sure that we get the exception or error we expect
   if we feed `create` something nonsensible, but I don't see how to actually
   make anything nonsensible to feed to `create`, so I'll call that a test the
   typechecker does for us for free *)

let timeout_or ~timeout ~msg listen_netif do_fn listen_fn =
  (* do something; also set up a listener on listen_netif
     timeout after the specified amount of time with a failure message
  *)
  (* this works best if listen_fn calls V.disconnect on listen_netif after
     getting the information it needs *)
  Lwt.join [
    do_fn ();
    (Lwt.pick [
        V.listen listen_netif (listen_fn ());
      Lwt_unix.sleep timeout >>= fun () -> OUnit.assert_failure msg
    ])
  ]

let set_ips () =
  get_arp ~root:(root ^ "/set_ips") () >>= fun (_, backend, _, _, a) ->
  (* set up a listener on the same backend that will return when it hears a GARP *)
  or_error "backend" V.connect backend >>= fun listen_netif ->
  (* TODO: according to the contract in arpv4.mli, add_ip, set_ip, and remove_ip
   are supposed to emit GARP packets; we should
   generalize this test for use in other functions *)
  let do_fn () =
    A_fs.set_ips a [ first_ip ] >>= fun () ->
    OUnit.assert_equal [ first_ip ] (A_fs.get_ips a);
    Lwt.return_unit
  in
  let listen_fn () =
    (fun buf -> match Irmin_arp.Arp.Parse.is_garp_for first_ip buf with
       | true -> V.disconnect listen_netif
       | false ->
         match Irmin_arp.Arp.Parse.arp_of_cstruct buf with
         | `Ok arp -> OUnit.assert_failure "something ARP but non-GARP sent after set_ips"
         | `Unusable -> OUnit.assert_failure "set_ips seems to have sent
         us something that expects a protocol other than ipv4"
         | `Bad_mac _ -> OUnit.assert_failure "couldn't parse a MAC out of something set_ips sent"
         | `Too_short -> OUnit.assert_failure "got a short packet after set_ips"
    )
  in
  timeout_or ~timeout:0.1 ~msg:"100ms timeout exceeded before listen_fn returned"
    listen_netif do_fn listen_fn >>= fun () ->
  A_fs.set_ips a [] >>= fun () ->
  OUnit.assert_equal [] (A_fs.get_ips a);
  A_fs.set_ips a [ first_ip; Ipaddr.V4.of_string_exn "10.20.1.1" ] >>= fun () ->
  OUnit.assert_equal [ first_ip; Ipaddr.V4.of_string_exn "10.20.1.1" ] (A_fs.get_ips
                                                                       a);
  Lwt.return_unit

let get_remove_ips () =
  get_arp ~root:(root ^ "/remove_ips") () >>= fun (_, backend, _, _, a) ->
  OUnit.assert_equal [] (A_fs.get_ips a);
  A_fs.set_ips a [ first_ip; first_ip ] >>= fun () ->
  let ips = A_fs.get_ips a in
  OUnit.assert_equal true (List.mem first_ip ips);
  OUnit.assert_equal true (List.for_all (fun a -> a = first_ip) ips);
  OUnit.assert_equal true (List.length ips >= 1 && List.length ips <= 2);
  A_fs.remove_ip a first_ip >>= fun () ->
  OUnit.assert_equal [] (A_fs.get_ips a);
  A_fs.remove_ip a first_ip >>= fun () ->
  OUnit.assert_equal [] (A_fs.get_ips a);
  Lwt.return_unit

let input_single_garp () =
  (* use on-disk git fs for cache so we can read it back and check it ourselves *)
  let root = root ^ "/input_single_garp" in
  let backend = B.create () in
  get_arp ~backend ~root:(root ^ "/listener") () >>=
  fun (listen_config, backend, listen_netif, listen_ethif, listen_arp) ->
  get_arp ~backend ~root:(root ^ "/speaker") () >>=
  fun (_, backend, speak_netif, speak_ethif, speak_arp) ->
  (* send a GARP from one side (speak_arp) and make sure it was heard on the
     other *)
  timeout_or ~timeout:0.1 ~msg:"Nothing received by listen_netif when trying to
  do single GARP input test"
    listen_netif (fun () -> A_fs.set_ips speak_arp [ first_ip ] )
    (fun () -> fun buf -> A_fs.input listen_arp buf >>= fun () -> V.disconnect listen_netif)
  >>= fun () ->
  (* load our own representation of the ARP cache of the listener *)
  let store = Irmin.basic (module Irmin_backend_fs) (module T) in
  Irmin.create store listen_config Irmin_unix.task >>= fun store ->
  Irmin.read_exn (store "readback of map") T.Path.empty >>= fun map ->
  try
    let open Entry in
    match T.find first_ip map with
    | Confirmed (time, entry) -> OUnit.assert_equal ~printer:Macaddr.to_string
                                   entry (V.mac speak_netif);
      Lwt.return_unit
  with
    Not_found -> OUnit.assert_failure "Expected cache entry not found in
    listener cache map, as read back from Irmin"

let input_multiple_garp () =
  let root = root ^ "/input_multiple_garp" in
  let strip = Ipaddr.V4.of_string_exn in
  let backend = B.create () in
  get_arp ~backend ~root:(root ^ "/listener") () >>=
  fun (listen_config, backend, listen_netif, listen_ethif, listen_arp) ->
  get_arp ~backend ~root:(root ^ "/speaker1") () >>= fun (_, _, n1, _, speaker1) ->
  get_arp ~backend ~root:(root ^ "/speaker2") () >>= fun (_, _, n2, _, speaker2) ->
  get_arp ~backend ~root:(root ^ "/speaker3") () >>= fun (_, _, n3, _, speaker3) ->
  get_arp ~backend ~root:(root ^ "/speaker4") () >>= fun (_, _, n4, _, speaker4) ->
  get_arp ~backend ~root:(root ^ "/speaker5") () >>= fun (_, _, n5, _, speaker5) ->
  let multiple_ips () = 
    A_fs.set_ips speaker1 [ first_ip ] >>= fun () ->
    A_fs.set_ips speaker2 [ second_ip ] >>= fun () ->
    A_fs.set_ips speaker3 [ (strip "192.168.3.33") ] >>= fun () ->
    A_fs.set_ips speaker4 [ (strip "192.168.3.44") ] >>= fun () ->
    A_fs.set_ips speaker5 [ (strip "192.168.3.255") ] >>= fun () ->
    OS.Time.sleep 0.2 >>= fun () -> 
    V.disconnect listen_netif >>= fun () ->
    Lwt.return_unit
  in
  let listen_fn () = V.listen listen_netif (E.input ~arpv4:(A_fs.input listen_arp)
      ~ipv4:(fun buf -> Lwt.return_unit) ~ipv6:(fun buf -> Lwt.return_unit)
      listen_ethif)
  in
  Lwt.join [ multiple_ips (); listen_fn () ] >>= fun () ->
  (* load our own representation of the ARP cache of the listener *)
  let store = Irmin.basic (module Irmin_backend_fs) (module T) in
  Irmin.create store listen_config Irmin_unix.task >>= fun store ->
  Irmin.read_exn (store "readback of map") T.Path.empty >>= fun imap ->
  let confirm map (ip, mac) =
    try
      let open Entry in
      match T.find ip map with
      | Confirmed (time, entry) -> OUnit.assert_equal ~printer:Macaddr.to_string
                                     entry mac;
        Lwt.return_unit
    with
      Not_found -> OUnit.assert_failure (Printf.sprintf 
                     "Expected cache entry %s not found in listener cache map,
                     as read back from Irmin" (Ipaddr.V4.to_string ip))
  in
  confirm imap (first_ip, (V.mac n1)) >>= fun () ->
  confirm imap (second_ip, (V.mac n2)) >>= fun () ->
  confirm imap (strip "192.168.3.33", (V.mac n3)) >>= fun () ->
  confirm imap (strip "192.168.3.44", (V.mac n4)) >>= fun () ->
  confirm imap (strip "192.168.3.255", (V.mac n5)) >>= fun () ->
  Lwt.return_unit

let input_single_unicast () =
  (* use on-disk git fs for cache so we can read it back and check it ourselves *)
  let root = root ^ "/input_single_unicast" in
  let backend = B.create () in
  get_arp ~backend ~root:(root ^ "/listener") () >>=
  fun (listen_config, backend, listen_netif, listen_ethif, listen_arp) ->
  get_arp ~backend ~root:(root ^ "/speaker") () >>=
  fun (_, backend, speak_netif, speak_ethif, speak_arp) ->
  (* send an ARP reply from one side (speak_arp) and make sure it was heard on the
     other *)
  (* in order to package this properly, we need mac details for each netif *)
  let for_listener = Irmin_arp.Arp.Parse.cstruct_of_arp
      { Irmin_arp.Arp.op = `Reply; 
        sha = (V.mac speak_netif); 
        tha = (V.mac listen_netif); spa = first_ip;
        tpa = second_ip } in
  timeout_or ~timeout:0.1 ~msg:"Nothing received by listen_netif when trying to
  do single unicast reply input test"
    listen_netif (fun () -> E.write speak_ethif for_listener)
    (fun () -> fun buf -> A_fs.input listen_arp buf >>= fun () -> V.disconnect listen_netif)
  >>= fun () ->
  (* listen_config should have the ARP cache history reflecting the updates send
     by speak_arp; a current read should show us first_ip *)
  let store = Irmin.basic (module Irmin_backend_fs) (module T) in
  Irmin.create store listen_config Irmin_unix.task >>= fun store ->
  Irmin.read_exn (store "readback of map") T.Path.empty >>= fun map ->
  (* TODO: iterate over the commit history of IPs *)
  try
    let open Entry in
    match T.find first_ip map with
    | Confirmed (time, entry) -> OUnit.assert_equal entry (V.mac speak_netif);
      Lwt.return_unit
  with
    Not_found -> OUnit.assert_failure "Expected cache entry not found in
    listener cache map, as read back from Irmin"

let input_changed_ip () =
  let root = root ^ "/input_changed_ip" in
  let backend = B.create () in
  get_arp ~backend ~root:(root ^ "/speaker") () >>=
  fun (speak_config, backend, speak_netif, speak_ethif, speak_arp) ->
  get_arp ~backend ~root:(root ^ "/listener") () >>=
  fun (listen_config, backend, listen_netif, listen_ethif, listen_arp) ->
  let multiple_ips () =
    A_fs.set_ips speak_arp [ Ipaddr.V4.of_string_exn "10.23.10.1" ] >>= fun () ->
    A_fs.set_ips speak_arp [ Ipaddr.V4.of_string_exn "10.50.20.22" ] >>= fun () ->
    A_fs.set_ips speak_arp [ Ipaddr.V4.of_string_exn "10.20.254.2" ] >>= fun () ->
    A_fs.set_ips speak_arp [ first_ip ] >>= fun () ->
    Lwt_unix.sleep 0.1 >>= fun () -> V.disconnect listen_netif >>= fun () ->
    Lwt.return_unit
  in
  let listen_fn () = V.listen listen_netif (E.input ~arpv4:(A_fs.input listen_arp)
      ~ipv4:(fun buf -> Lwt.return_unit) ~ipv6:(fun buf -> Lwt.return_unit)
      listen_ethif)
  in
  Lwt.join [ multiple_ips (); listen_fn () ] >>= fun () ->
  (* listen_config should have the ARP cache history reflecting the updates send
     by speak_arp; a current read should show us first_ip *)
  let store = Irmin.basic (module Irmin_backend_fs) (module T) in
  Irmin.create store listen_config Irmin_unix.task >>= fun store ->
  Irmin.read_exn (store "readback of map") T.Path.empty >>= fun map ->
  (* TODO: iterate over the commit history of IPs *)
  try
    let open Entry in
    match T.find first_ip map with
    | Confirmed (time, entry) -> OUnit.assert_equal entry (V.mac speak_netif);
      Lwt.return_unit
  with
    Not_found -> OUnit.assert_failure "Expected cache entry not found in
    listener cache map, as read back from Irmin"

let input_garbage () =
  let open A_fs in
  let root = root ^ "/input_garbage" in
  let backend = B.create () in
  get_arp ~backend ~root:(root ^ "/speaker") () >>= fun (_, backend, speak_netif, _, _) ->
  get_arp ~backend ~root:(root ^ "/listener") () >>=
  fun (listen_config, backend, listen_netif, listen_ethif, listen_arp) ->
  A_fs.set_ips listen_arp [ first_ip ] >>= fun () ->
  let listen_fn () = V.listen listen_netif (E.input ~arpv4:(A_fs.input listen_arp)
      ~ipv4:(fun buf -> Lwt.return_unit) ~ipv6:(fun buf -> Lwt.return_unit)
      listen_ethif)
  in
  let fire_away = send_buf_sleep_then_dc speak_netif listen_netif in
  (* TODO: this is a good candidate for a property test -- `parse` on an arbitrary
     buffer of size less than k always returns `Too_short *)
  let listener_mac = V.mac listen_netif in
  let speaker_mac = V.mac speak_netif in
  (* don't keep entries for unicast replies to someone else *)
  let for_someone_else = Irmin_arp.Arp.Parse.cstruct_of_arp
      { Irmin_arp.Arp.op = `Reply; sha = listener_mac; tha = speaker_mac; spa = first_ip;
        tpa = Ipaddr.V4.of_string_exn "192.168.3.50" } in
  (* don't store cache entries for broadcast either, even if someone claims it *)
  let claiming_broadcast = Irmin_arp.Arp.Parse.cstruct_of_arp
      { Irmin_arp.Arp.op = `Reply; sha = Macaddr.broadcast; tha = listener_mac; spa = first_ip;
        tpa = Ipaddr.V4.of_string_exn "192.168.3.50" } in
  (* TODO: don't set entries for non-unicast MACs if we're a router, but do if
     we're a host (set via some parameter at creation time, presumably) *)
  (* TODO: another decent property test -- if op is something other than reply,
     we never make a cache entry *)
  (* don't believe someone else if they claim one of our IPs *)
  let claiming_ours = Irmin_arp.Arp.Parse.cstruct_of_arp
      { Irmin_arp.Arp.op = `Reply; sha = speaker_mac; tha = listener_mac; spa = first_ip;
        tpa = first_ip } in
  Lwt.join [ listen_fn (); fire_away
               [(Cstruct.create 0); for_someone_else;
                claiming_broadcast; claiming_ours ] () ]
  >>= fun () ->
  (* shouldn't be anything in the cache as a result of all that nonsense *)
  (* TODO: in fact, shouldn't ever have been anything in the cache as a result
     of all that nonsense *)
  let store = Irmin.basic (module Irmin_backend_fs) (module T) in
  Irmin.create store listen_config Irmin_unix.task >>= fun store ->
  Irmin.read_exn (store "readback of map") T.Path.empty >>= fun map ->
  OUnit.assert_equal T.empty map;
  Lwt.return_unit

(* parse responds as expected to nonsense, non-arp buffers *)
(* TODO: this test feels out of place here; I think this yet another
   manifestation of needing a central place/system for parsing arbitrary network
   nonsense according to spec *)
(* TODO: Too_short and Unusable are excellent candidates for property-based tests *)
let parse_zeros () =
  let open A_fs in
  OUnit.assert_equal `Too_short (Irmin_arp.Arp.Parse.arp_of_cstruct (Cstruct.create 0));
  OUnit.assert_equal `Too_short
    (Irmin_arp.Arp.Parse.arp_of_cstruct (Cstruct.create (Arpv4_wire.sizeof_arp - 1)));
  (* I think we actually can't trigger `Bad_mac, since the only condition that
     causes that in the underlying Macaddr implementation is being provided a
     string of insufficient size, which we guard against with `Too_short, ergo
     no test to make sure we return `Bad_mac *)
  let zero_cstruct cs =
    let zero c = Cstruct.set_char c 0 '\000' in
    let i = Cstruct.iter (fun c -> Some 1) zero cs in
    Cstruct.fold (fun b a -> b) i cs
  in
  let all_zero = zero_cstruct (Cstruct.create
                                 (Arpv4_wire.sizeof_arp)) in
  match Irmin_arp.Arp.Parse.arp_of_cstruct all_zero with
  | `Too_short -> OUnit.assert_failure
    "Arp.parse claimed that an appropriate-length zero'd buffer was too short"
  | `Bad_mac l -> let mac_strs = Printf.sprintf "%S" (String.concat ", " l) in
    OUnit.assert_failure ("Arp.parse claimed these were bad MACs: " ^ mac_strs)
  | `Ok all_zero -> OUnit.assert_failure "Arp.parse allowed a 0 protocol"
  | `Unusable -> (* correct! *) Lwt.return_unit

let parse_unparse () =
  let module P = Irmin_arp.Arp.Parse in
  let first_mac = Macaddr.of_string_exn "00:16:3e:00:11:00" in
  let second_mac = Macaddr.of_string_exn "10:9a:dd:c0:ff:ee" in
  let test_arp = { Irmin_arp.Arp.op = `Request;
                   sha = first_mac; spa = first_ip;
                   tha = second_mac; tpa = second_ip; } in
  OUnit.assert_equal (`Ok test_arp) (P.arp_of_cstruct (P.cstruct_of_arp
                                                         test_arp));
  Lwt.return_unit

let query_with_seeded_cache () =
  let root = root ^ "/query_with_seeded_cache" in
  let speak_dir = root ^ "/speaker" in
  let listen_dir = root ^ "/listener" in
  let backend = B.create () in
  get_arp ~backend ~root:speak_dir () >>=
  fun (speak_config, backend, speak_netif, speak_ethif, speak_arp) ->
  A_fs.set_ips speak_arp [ first_ip ] >>= fun () ->
  get_arp ~backend ~root:listen_dir () >>=
  fun (_, backend, listen_netif, listen_ethif, listen_arp) ->
  let store = Irmin.basic (module Irmin_backend_fs) (module T) in
  Irmin.create store speak_config Irmin_unix.task >>= fun store ->
  Irmin.read (store "readback of map") T.Path.empty >>= function
    | None -> OUnit.assert_failure "Couldn't read store from query_for_seeded_map"
    | Some map ->
      OUnit.assert_equal T.empty map;
      let seeded = T.add second_ip (Entry.Confirmed ((Clock.time () +. 60.), sample_mac)) map in
      Irmin.update (store "query_with_seeded_cache: seed cache entry") T.Path.empty
        seeded >>= fun () ->
      (* OK, we've written an entry, so now calling query for that key
         should not emit an ARP query and should return straight away *)
      timeout_or ~timeout:0.5 ~msg:"Query sent for something that was seeded in
        the cache" listen_netif
        (fun () -> A_fs.query speak_arp second_ip >>= function
        | `Ok mac when mac = sample_mac -> (* yay! *)
          V.disconnect listen_netif
        | `Ok mac -> OUnit.assert_failure (Printf.sprintf "pre-seeded query got a
    MAC, but it's the wrong one: %s" (Macaddr.to_string mac))
        | `Timeout -> OUnit.assert_failure "Query timed out for something that was
    seeded in the cache"
        )
        (fun () -> (fun buf -> OUnit.assert_failure "Listener heard a
    packet, but speaker should've had a cache entry")
        )

let query_sent_with_empty_cache () =
  let root = root ^ "/query_sent_with_empty_cache" in
  let speak_dir = root ^ "/speaker" in
  let listen_dir = root ^ "/listener" in
  let backend = B.create () in
  get_arp ~backend ~root:speak_dir () >>=
  fun (_, backend, speak_netif, speak_ethif, speak_arp) ->
  get_arp ~backend ~root:listen_dir () >>=
  fun (_, backend, listen_netif, listen_ethif, listen_arp) ->
  let do_fn () =
    A_fs.query speak_arp first_ip >>= function
    | `Ok mac -> OUnit.assert_failure "query returned a MAC when the cache
    should've been empty and nobody could possibly be responding"
    | `Timeout -> Lwt.return_unit (* we shouldn't get this far, but if
                                     timeout_or has a large value we might and
                                     if that happens, this is indeed the
                                     expected behavior *)
  in
  let listen_fn () =
    fun buf ->
      match Irmin_arp.Arp.Parse.arp_of_cstruct buf with
      | `Too_short | `Unusable | `Bad_mac _ -> OUnit.assert_failure "Attempting
                                                 to produce a probe instead
                                                 resulted in a strange packet"
      | `Ok arp ->
        let expected_arp = { Irmin_arp.Arp.op = `Request;
                             sha = (V.mac speak_netif); spa = Ipaddr.V4.any;
                             tha = Macaddr.broadcast; tpa = first_ip } in
        OUnit.assert_equal expected_arp arp; V.disconnect listen_netif
  in
  timeout_or ~timeout:0.1 ~msg:"ARP probe not sent in response to a query"
    listen_netif do_fn listen_fn

let entries_aged_out () =
  let root = root ^ "/entries_aged_out" in
  let speak_dir = root ^ "/speaker" in
  let backend = B.create () in
  get_arp ~backend ~root:speak_dir () >>=
  fun (speak_config, backend, speak_netif, speak_ethif, speak_arp) ->
  let store = Irmin.basic (module Irmin_backend_fs) (module T) in
  Irmin.create store speak_config Irmin_unix.task >>= fun store ->
  Irmin.clone_force Irmin_unix.task (store "cloning for cache preseeding")
    "entries_aged_out" >>= fun our_branch ->
  Irmin.read_exn (our_branch "readback of map") T.Path.empty >>= fun init_map ->
  let seeded = T.add second_ip (Entry.Confirmed ((Clock.time () -. 9999999.),
                                                 sample_mac)) init_map in
  Irmin.update (our_branch "entries_aged_out: seed cache entry") T.Path.empty
    seeded >>= fun () ->
  Irmin.merge_exn "entries_aged_out: merge seeded ARP entry" our_branch
    ~into:store >>= fun () ->
  Lwt_unix.sleep 65. >>= fun () ->
  Irmin.read_exn (store "readback of map") T.Path.empty >>= fun map ->
  OUnit.assert_raises Not_found (fun () -> T.find second_ip map);
  Lwt.return_unit

let lwt_run f () = Lwt_main.run (f ())

let () =
  let ip_crud = [
    "set_ips", `Slow, lwt_run set_ips;
    "get_remove_ips", `Slow, lwt_run get_remove_ips;
  ] in
  let parse = [
    "parse_zeros", `Quick, lwt_run parse_zeros;
    "parse_unparse", `Quick, lwt_run parse_unparse;
  ] in
  let query = [
    (*
       x if an entry is in the cache, query returns it
       x arp probes are sent for entries not in the cache
       once a response is received, waiting thread returns response immediately
       probes are retried
       entries are aged out
    *)
    "query_with_seeded_cache", `Slow, lwt_run query_with_seeded_cache;
    "query_sent_with_empty_cache", `Slow, lwt_run query_sent_with_empty_cache;
  ] in
  let input = [
    "input_single_garp", `Slow, lwt_run input_single_garp;
    "input_multiple_garp", `Slow, lwt_run input_multiple_garp;
    "input_single_unicast_reply", `Slow, lwt_run input_single_unicast;
    "input_changed_ip", `Slow, lwt_run input_changed_ip ;
    "input_garbage", `Slow, lwt_run input_garbage
  ] in
  let create = [
    "create_is_consistent", `Quick, lwt_run create_is_consistent ;
  ] in
  let aging = [
    "entries_aged_out", `Slow, lwt_run entries_aged_out ;
  ] in
  Alcotest.run "Irmin_arp.Arp" [
    "create", create;
    "ip_CRUD", ip_crud;
    "parse", parse;
    "query", query;
    "input", input;
    "aging", aging
  ]
