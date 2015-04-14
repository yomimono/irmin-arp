open Lwt
module B = Basic_backend.Make
module V = Vnetif.Make(B)
module E = Ethif.Make(V)
module T = Table.Make(Irmin.Path.String_list)
module Irmin_storer = Irmin_git
module Irmin_backend = Irmin_unix.Irmin_git.FS
module I = Irmin.Basic(Irmin_backend)(T)
module A = Irmin_arp.Arp.Make(E)(Clock)(Irmin_backend)

let root = "test_results"

let or_error name fn t =
  fn t >>= function
  | `Error e -> OUnit.assert_failure ("Error starting " ^ name)
  | `Ok t -> return t

let clear_cache config =
  let store = Irmin.basic (module Irmin_backend) (module T) in
  Irmin.create store config Irmin_unix.task >>= fun store ->
  let node = T.Path.empty in
  Irmin.remove (store "removing previous history for new test run") node 

let get_arp ?(backend = B.create ()) ~root () =
  or_error "backend" V.connect backend >>= fun netif ->
  or_error "ethif" E.connect netif >>= fun ethif ->
  let config = Irmin_storer.config ~root () in
  clear_cache config >>= fun () ->
  A.create ethif config >>= fun a ->
  Lwt.return (backend, netif, ethif, a)

(* a few arbitrary addresses for convenience *)
let first_ip = Ipaddr.V4.of_string_exn "192.168.3.1"
let second_ip = Ipaddr.V4.of_string_exn "192.168.3.10"
let first_mac = Macaddr.of_string_exn "00:16:3e:00:11:00"
let second_mac = Macaddr.of_string_exn "10:9a:dd:c0:ff:ee"

let create_is_consistent () =
  (* Arp.create returns something bearing resemblance to an Arp.t *)
  (* possibly assert some qualities of a freshly-created ARP interface -- e.g.
     no bound IPs, empty cache, etc *)
  let root = root ^ "/create_is_consistent" in
  get_arp ~root () >>= fun (_, _, _, a) ->
  OUnit.assert_equal [] (A.get_ips a);
  (* cache should contain an empty map at T.Path.empty *)
  let store = Irmin.basic (module Irmin_backend) (module T) in
  let config = Irmin_storer.config ~root () in
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
  get_arp ~root:(root ^ "/set_ips") () >>= fun (backend, _, _, a) ->
  (* set up a listener that will return when it hears a GARP *)
  or_error "backend" V.connect backend >>= fun listen_netif ->
(* TODO: according to the contract in arpv4.mli, add_ip, set_ip, and remove_ip
   are supposed to emit GARP packets; we should 
   generalize this test for use in other functions *)
  let do_fn () =
    A.set_ips a [ first_ip ] >>= fun a ->
    OUnit.assert_equal [ first_ip ] (A.get_ips a);
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
  A.set_ips a [] >>= fun a ->
  OUnit.assert_equal [] (A.get_ips a);
  A.set_ips a [ first_ip; Ipaddr.V4.of_string_exn "10.20.1.1" ] >>= fun a ->
  OUnit.assert_equal [ first_ip; Ipaddr.V4.of_string_exn "10.20.1.1" ] (A.get_ips
                                                                       a);
  Lwt.return_unit

let get_remove_ips () =
  get_arp ~root:(root ^ "/remove_ips") () >>= fun (backend, _, _, a) ->
  OUnit.assert_equal [] (A.get_ips a);
  A.set_ips a [ first_ip; first_ip ] >>= fun a ->
  let ips = A.get_ips a in
  OUnit.assert_equal true (List.mem first_ip ips);
  OUnit.assert_equal true (List.for_all (fun a -> a = first_ip) ips);
  OUnit.assert_equal true (List.length ips >= 1 && List.length ips <= 2);
  A.remove_ip a first_ip >>= fun a ->
  OUnit.assert_equal [] (A.get_ips a);
  A.remove_ip a first_ip >>= fun a ->
  OUnit.assert_equal [] (A.get_ips a);
  Lwt.return_unit

let input_single_reply () =
  (* use on-disk git fs for cache so we can read it back and check it ourselves *)
  let root = root ^ "/input_single_reply" in
  let listen_config = Irmin_storer.config ~root:(root ^ "/listener") () in
  let backend = B.create () in
  get_arp ~backend ~root:(root ^ "/listener") () >>= 
  fun (backend, listen_netif, listen_ethif, listen_arp) ->
  get_arp ~backend ~root:(root ^ "/speaker") () >>= 
  fun (backend, speak_netif, speak_ethif, speak_arp) ->
  (* send a GARP from one side (speak_arp) and make sure it was heard on the
     other *)
  timeout_or ~timeout:0.1 ~msg:"Nothing received by listen_netif when trying to
  do single reply test"
    listen_netif (fun () -> A.set_ips speak_arp [ first_ip ] >>= fun _a -> Lwt.return_unit)
    (fun () -> fun buf -> A.input listen_arp buf >>= fun () -> V.disconnect listen_netif)
  >>= fun () ->
  (* load our own representation of the ARP cache of the listener *)
  let store = Irmin.basic (module Irmin_backend) (module T) in
  Irmin.create store listen_config Irmin_unix.task >>= fun store ->
  Irmin.read_exn (store "readback of map") T.Path.empty >>= fun map ->
  try
    let open Entry in
    match T.find first_ip map with
    | Confirmed (time, entry) -> OUnit.assert_equal ~printer:Macaddr.to_string 
                                   entry (V.mac speak_netif);
      Lwt.return_unit
    | Pending _ -> OUnit.assert_failure "Pending entry for an entry that had a
  GARP emitted on the same vnetif backend"
  with
    Not_found -> OUnit.assert_failure "Expected cache entry not found in
    listener cache map, as read back from Irmin"

let input_changed_ip () =
  let root = root ^ "/input_changed_ip" in
  let listen_config = Irmin_storer.config ~root:(root ^ "/listener") () in
  let speak_config = Irmin_storer.config ~root:(root ^ "/speaker") () in
  let backend = B.create () in
  or_error "backend" V.connect backend >>= fun speak_netif ->
  or_error "ethif" E.connect speak_netif >>= fun speak_ethif ->
  A.create speak_ethif speak_config >>= fun speak_arp ->
  or_error "backend" V.connect backend >>= fun listen_netif ->
  or_error "ethif" E.connect listen_netif >>= fun listen_ethif ->
  A.create listen_ethif listen_config >>= fun listen_arp ->
  let multiple_ips () =
    A.set_ips speak_arp [ Ipaddr.V4.of_string_exn "10.23.10.1" ] >>= fun speak_arp ->
    A.set_ips speak_arp [ Ipaddr.V4.of_string_exn "10.50.20.22" ] >>= fun speak_arp ->
    A.set_ips speak_arp [ Ipaddr.V4.of_string_exn "10.20.254.2" ] >>= fun speak_arp ->
    A.set_ips speak_arp [ first_ip ] >>= fun speak_arp ->
    Lwt_unix.sleep 0.1 >>= fun () -> V.disconnect listen_netif >>= fun () ->
    Lwt.return_unit
  in
  let listen_fn () = V.listen listen_netif (E.input ~arpv4:(A.input listen_arp)
      ~ipv4:(fun buf -> Lwt.return_unit) ~ipv6:(fun buf -> Lwt.return_unit)
      listen_ethif)
  in
  Lwt.join [ multiple_ips (); listen_fn () ] >>= fun () ->
  (* listen_config should have the ARP cache history reflecting the updates send
     by speak_arp; a current read should show us first_ip *)
  let store = Irmin.basic (module Irmin_backend) (module T) in
  Irmin.create store listen_config Irmin_unix.task >>= fun store ->
  Irmin.read_exn (store "readback of map") T.Path.empty >>= fun map ->
  (* TODO: iterate over the commit history of IPs *)
  try
    let open Entry in
    match T.find first_ip map with
    | Confirmed (time, entry) -> OUnit.assert_equal entry (V.mac speak_netif);
      Lwt.return_unit
    | Pending _ -> OUnit.assert_failure "Pending entry for an entry that had a
  GARP emitted on the same vnetif backend"
  with
    Not_found -> OUnit.assert_failure "Expected cache entry not found in
    listener cache map, as read back from Irmin"

let input_garbage () =
  let open A in
  let root = root ^ "/input_garbage" in
  let backend = B.create () in
  let listen_config = Irmin_storer.config ~root () in
  or_error "backend" V.connect backend >>= fun speak_netif ->
  or_error "backend" V.connect backend >>= fun listen_netif ->
  or_error "ethif" E.connect listen_netif >>= fun listen_ethif ->
  A.create listen_ethif listen_config >>= fun listen_arp ->
  A.set_ips listen_arp [ first_ip ] >>= fun listen_arp ->
  let listen_fn () = V.listen listen_netif (E.input ~arpv4:(A.input listen_arp)
      ~ipv4:(fun buf -> Lwt.return_unit) ~ipv6:(fun buf -> Lwt.return_unit)
      listen_ethif)
  in
  (* TODO: this is a good candidate for a property test -- `parse` on an arbitrary
     buffer of size less than k always returns `Too_short *)
  let send_buf_sleep_then_dc bufs () = 
    Lwt.join (List.map (V.write speak_netif) bufs) >>= fun () ->
    Lwt_unix.sleep 0.1 >>= fun () ->
    V.disconnect listen_netif
  in
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
  Lwt.join [ listen_fn (); send_buf_sleep_then_dc 
               [(Cstruct.create 0); for_someone_else;
                claiming_broadcast; claiming_ours ] () ] 
  >>= fun () ->
  (* shouldn't be anything in the cache as a result of all that nonsense *)
  let store = Irmin.basic (module Irmin_backend) (module T) in
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
  let open A in
  OUnit.assert_equal `Too_short (Irmin_arp.Arp.Parse.arp_of_cstruct (Cstruct.create 0));
  OUnit.assert_equal `Too_short 
    (Irmin_arp.Arp.Parse.arp_of_cstruct (Cstruct.create (Wire_structs.Arpv4_wire.sizeof_arp - 1)));
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
                                 (Wire_structs.Arpv4_wire.sizeof_arp)) in
  match Irmin_arp.Arp.Parse.arp_of_cstruct all_zero with
  | `Too_short -> OUnit.assert_failure 
    "Arp.parse claimed that an appropriate-length zero'd buffer was too short"
  | `Bad_mac l -> let mac_strs = Printf.sprintf "%S" (String.concat ", " l) in
    OUnit.assert_failure ("Arp.parse claimed these were bad MACs: " ^ mac_strs)
  | `Ok all_zero -> OUnit.assert_failure "Arp.parse allowed a 0 protocol"
  | `Unusable -> (* correct! *) Lwt.return_unit

let parse_unparse () =
  let module P = Irmin_arp.Arp.Parse in
  let test_arp = { Irmin_arp.Arp.op = `Request; 
                   sha = first_mac; spa = first_ip; 
                   tha = second_mac; tpa = second_ip; } in
  OUnit.assert_equal (`Ok test_arp) (P.arp_of_cstruct (P.cstruct_of_arp
                                                         test_arp));
  Lwt.return_unit

let query_for_seeded_cache () = 
  let root = root ^ "/query_for_seeded_cache" in
  let speak_dir = root ^ "/speaker" in
  let listen_dir = root ^ "/listener" in
  let speak_config = Irmin_storer.config ~root () in
  let backend = B.create () in
  get_arp ~backend ~root:speak_dir () >>= 
  fun (backend, speak_netif, speak_ethif, speak_arp) ->
  A.set_ips speak_arp [ first_ip ] >>= fun speak_arp ->
  get_arp ~backend ~root:listen_dir () >>= 
  fun (backend, listen_netif, listen_ethif, listen_arp) ->
  let store = Irmin.basic (module Irmin_backend) (module T) in
  Irmin.create store (Irmin_storer.config ~root:speak_dir ()) Irmin_unix.task >>= fun store ->
  Irmin.read (store "readback of map") T.Path.empty >>= function
    | None -> OUnit.assert_failure "Couldn't read store from
    query_for_seeded_map"
    | Some map ->
      OUnit.assert_equal T.empty map;
      let seeded = T.add second_ip (Entry.Confirmed ((Clock.time () +. 60.), second_mac)) map in
      Irmin.update (store "query_for_seeded_cache: seed cache entry") T.Path.empty
        seeded >>= fun () ->
      (* OK, we've written an entry, so now calling query should not emit an ARP
         query and should return before 1 retry interval *)
      timeout_or ~timeout:0.5 ~msg:"Query sent for something that was seeded in
        the cache" listen_netif
        (fun () ->
        A.query speak_arp second_ip >>= function
        | `Ok mac when mac = second_mac -> (* yay! *) 
          V.disconnect listen_netif
        | `Ok mac -> OUnit.assert_failure (Printf.sprintf "pre-seeded query got a
    MAC, but it's the wrong one: %s" (Macaddr.to_string mac))
        | `Timeout -> OUnit.assert_failure "Query timed out for something that was
    seeded in the cache"
        ) 
        (fun () -> (fun buf -> OUnit.assert_failure "Listener heard a
    packet, but speaker should've had a cache entry")
        )

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
       tests from previous arp testing code:
       arp probes are sent for entries not in the cache
       once a response is received, query thread returns response immediately
       probes are retried
       entries are aged out 
       if an entry is in the cache, query returns it
    *)
    (* "query_sent", `Slow, lwt_run query_sent; *)
    "query_for_seeded_cache", `Slow, lwt_run query_for_seeded_cache;
  ] in
  let input = [
    "input_single_reply", `Slow, lwt_run input_single_reply;
    "input_changed_ip", `Slow, lwt_run input_changed_ip ;
    "input_garbage", `Slow, lwt_run input_garbage
  ] in
  let create = [
    "create_is_consistent", `Slow, lwt_run create_is_consistent ;
  ] in
  Alcotest.run "Irmin_arp.Arp" [
    "create", create;
    "ip_CRUD", ip_crud;
    "parse", parse;
    "query", query;
    "input", input
  ]
