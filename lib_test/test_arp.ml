open Lwt
open Common

module Test (I : Irmin.S_MAKER) = struct
  module A = Irmin_arp.Arp.Make(E)(Clock)(OS.Time)(Random)(I)

  type stack = {
    config: Irmin.config;
    node: T.Path.t;
    backend: B.t;
    netif: V.t;
    ethif: E.t;
    arp: A.t;
  }

  let gripe str = OUnit.assert_failure str

  let store = Irmin.basic (module I) (module T)

  let extract_arp etherbuf = Cstruct.shift etherbuf (6+6+2)

  let get_arp ?(backend = blessed_backend) ~(make_fn : unit -> Irmin.config) ~node
      ?(pull=[])() =
    or_error "backend" V.connect backend >>= fun netif ->
    or_error "ethif" E.connect netif >>= fun ethif ->
    let config = make_fn () in
    let node = [node] in
    A.connect ethif config ~node ~pull >>= function
    | `Ok arp -> Lwt.return (`Ok {config; node; backend; netif; ethif; arp;})
    | `Error e -> Lwt.return (`Error e)

  let get_arp_exn ?(backend = blessed_backend) ~(make_fn : unit -> Irmin.config) ~node
      ?(pull=[])() =
    get_arp ~backend ~make_fn ~node ~pull () >>= function
    | `Ok stack -> Lwt.return stack
    | `Error e -> OUnit.assert_failure "Couldn't start ARP :("

  let local_copy stack =
    Irmin.create store stack.config Irmin_unix.task >>= fun cache ->
    A.push stack.arp (cache "make remote") >>= function
    | `Error -> gripe "ARP didn't push cache to us"
    | `Ok -> Lwt.return cache

  let confirm map (ip, mac) =
    try
      match T.find ip map with
      | Entry.Confirmed (time, entry) ->
        OUnit.assert_equal ~printer:Macaddr.to_string entry mac;
        Lwt.return_unit
    with
    | Not_found ->
      let err = (Printf.sprintf
           "Expected cache entry %s not found in listener cache map)"
           (Ipaddr.V4.to_string ip)) in
      OUnit.assert_failure err

  let seed_cache make_fn node map =
    let task =
      let owner = "irmin_arp testing cache seeder bot (tm)" in
      Irmin.Task.create ~date:(Int64.of_float (Clock.time ())) ~owner
    in
    let config = make_fn () in (* for disk backends, this will be literally
                                    the same repo; for in-memory, not so *)
    Irmin.create store config task >>= fun cache ->
    Irmin.update (cache "test_arp: seed_cache") node map >>= fun () ->
    Lwt.return cache


  let first_ip = Ipaddr.V4.of_string_exn "192.168.3.1"
  let second_ip = Ipaddr.V4.of_string_exn "192.168.3.10"
  let sample_mac = Macaddr.of_string_exn "10:9a:dd:c0:ff:ee"

  let send_buf_sleep_then_dc speak_netif listen_netif bufs () =
    Lwt.join (List.map (V.write speak_netif) bufs) >>= fun () ->
    OS.Time.sleep 0.1 >>= fun () ->
    V.disconnect listen_netif

  let create_is_consistent make_fn () =
    (* Arp.create returns something bearing resemblance to an Arp.t *)
    (* possibly assert some qualities of a freshly-created ARP interface -- e.g.
       no bound IPs, empty cache, etc *)
    get_arp_exn ~make_fn ~node:"__root__" () >>= fun stack ->
    OUnit.assert_equal [] (A.get_ips stack.arp);
    local_copy stack >>= fun cache ->
    Irmin.read (cache "create_is_consistent checking for empty map") stack.node >>=
    function
    | None -> gripe "Expected location of the cache was empty"
    | Some map -> OUnit.assert_equal T.empty map; Lwt.return_unit

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
          OS.Time.sleep timeout >>= fun () -> OUnit.assert_failure msg
        ])
    ]

  let set_ips (make_fn : unit -> Irmin.config) () =
    get_arp_exn ~make_fn ~node:"set_ips" () >>= fun stack ->
    (* set up a listener on the same backend that will return when it hears a GARP *)
    or_error "backend" V.connect stack.backend >>= fun listen_netif ->
    (* TODO: according to the contract in arpv4.mli, add_ip and set_ip
       are supposed to emit GARP packets; we should
       generalize this test for use in other functions *)
    let do_fn () =
      A.set_ips stack.arp [ first_ip ] >>= fun () ->
      OUnit.assert_equal [ first_ip ] (A.get_ips stack.arp);
      Lwt.return_unit
    in
    let listen_fn () =
      (fun buf -> match Irmin_arp.Arp.Parse.is_garp_for first_ip (Cstruct.shift
                                                                    buf 14) with
         | true -> V.disconnect listen_netif
         | false ->
           match Irmin_arp.Arp.Parse.arp_of_cstruct buf with
           | `Ok arp -> gripe "something ARP but non-GARP sent after set_ips"
           | `Unusable -> gripe "set_ips seems to have sent
         us something that expects a protocol other than ipv4"
           | `Bad_mac _ -> gripe "couldn't parse a MAC out of something set_ips sent"
           | `Too_short -> gripe "got a short packet after set_ips"
      )
    in
    timeout_or ~timeout:0.1 ~msg:"100ms timeout exceeded before listen_fn returned"
      listen_netif do_fn listen_fn >>= fun () ->
    A.set_ips stack.arp [] >>= fun () ->
    OUnit.assert_equal [] (A.get_ips stack.arp);
    A.set_ips stack.arp [ first_ip; Ipaddr.V4.of_string_exn "10.20.1.1" ] >>= fun () ->
    OUnit.assert_equal [ first_ip; Ipaddr.V4.of_string_exn "10.20.1.1" ] (A.get_ips
                                                                            stack.arp);
    Lwt.return_unit

  let get_remove_ips make_fn () =
    get_arp_exn ~make_fn ~node:"__root__" () >>= fun stack ->
    OUnit.assert_equal [] (A.get_ips stack.arp);
    A.set_ips stack.arp [ first_ip; first_ip ] >>= fun () ->
    let ips = A.get_ips stack.arp in
    OUnit.assert_equal true (List.mem first_ip ips);
    OUnit.assert_equal true (List.for_all (fun a -> a = first_ip) ips);
    OUnit.assert_equal true (List.length ips >= 1 && List.length ips <= 2);
    A.remove_ip stack.arp first_ip >>= fun () ->
    OUnit.assert_equal [] (A.get_ips stack.arp);
    A.remove_ip stack.arp first_ip >>= fun () ->
    OUnit.assert_equal [] (A.get_ips stack.arp);
    Lwt.return_unit

  let input_single_garp make_fn () =
    let backend = blessed_backend in
    get_arp_exn ~backend ~make_fn ~node:"listener" () >>= fun listen ->
    get_arp_exn ~backend ~make_fn ~node:"speaker" () >>= fun speak ->
    (* send a GARP from one side (speak.arp) and make sure it was heard on the
       other *)
    timeout_or ~timeout:0.5 ~msg:"Nothing received by listen.netif when trying to
  do single GARP input test"
      listen.netif (fun () -> A.set_ips speak.arp [ first_ip ] )
      (fun () -> fun buf -> A.input listen.arp (Cstruct.shift buf 14) >>= fun () -> V.disconnect
          listen.netif)
    >>= fun () ->
    (* load our own representation of the ARP cache of the listener *)
    local_copy listen >>= fun cache ->
    Irmin.read_exn (cache "readback of map") listen.node >>= fun map ->
    confirm map (first_ip, (V.mac speak.netif)) >>= fun () ->
    Lwt.return_unit

  let input_single_unicast make_fn () =
    (* use on-disk git fs for cache so we can read it back and check it ourselves *)
    let backend = blessed_backend in
    get_arp_exn ~backend ~make_fn ~node:"listener" () >>= fun listen ->
    get_arp_exn ~backend ~make_fn ~node:"speaker" () >>= fun speak ->
    (* send an ARP reply from one side (speak.arp) and make sure it was heard on the
       other *)
    (* in order to package this properly, we need mac details for each netif *)
    (* we also need to make our own ethernet header, because E.write doesn't slap
       them on for us (as we might assume it does from having experience with
       Udp.write, Tcp.write, and Ip.write -- this is gross and probably the next
       thing to tackle in tcpip... *)
    let arp_reply = 
        { Irmin_arp.Arp.op = `Reply;
          sha = (V.mac speak.netif);
          tha = (V.mac listen.netif); spa = first_ip;
          tpa = second_ip } in
    let arp_cst = Irmin_arp.Arp.Parse.cstruct_of_arp arp_reply in
    let ethernet_cst = Irmin_arp.Arp.Parse.ethernet_of_arp arp_reply in
    timeout_or ~timeout:0.5 ~msg:"Nothing received by listen.netif when trying to
  do single unicast reply input test"
      listen.netif (fun () -> E.writev speak.ethif [ ethernet_cst; arp_cst ])
      (fun () -> fun buf -> A.input listen.arp (Cstruct.shift buf 14) >>= fun () -> V.disconnect
          listen.netif)
    >>= fun () ->
    (* listen.config should have the ARP cache history reflecting the updates send
       by speak.arp; a current read should show us first_ip *)
    local_copy listen >>= fun store ->
    Irmin.read_exn (store "readback of map") listen.node >>= fun map ->
    (* TODO: iterate over the commit history of IPs *)
    try
      let open Entry in
      match T.find first_ip map with
      | Confirmed (time, entry) -> OUnit.assert_equal entry (V.mac speak.netif);
        Lwt.return_unit
    with
      Not_found -> gripe "Expected cache entry not found in
                          listener cache map, as read back from Irmin"

  let input_multiple_garp make_fn () =
    let strip = Ipaddr.V4.of_string_exn in
    let backend = blessed_backend in
    get_arp_exn ~backend ~make_fn ~node:"listener" () >>= fun listen ->
    get_arp_exn ~backend ~make_fn ~node:"speaker1" () >>= fun speaker1 ->
    get_arp_exn ~backend ~make_fn ~node:"speaker2" () >>= fun speaker2 ->
    get_arp_exn ~backend ~make_fn ~node:"speaker3" () >>= fun speaker3 ->
    get_arp_exn ~backend ~make_fn ~node:"speaker4" () >>= fun speaker4 ->
    get_arp_exn ~backend ~make_fn ~node:"speaker5" () >>= fun speaker5 ->
    let multiple_ips () =
      OS.Time.sleep 0.2 >>= fun () ->
      Lwt.join [
        A.set_ips speaker1.arp [ first_ip ];
        A.set_ips speaker2.arp [ second_ip ];
        A.set_ips speaker3.arp [ (strip "192.168.3.33") ];
        A.set_ips speaker4.arp [ (strip "192.168.3.44") ];
        A.set_ips speaker5.arp [ (strip "192.168.3.255") ];
      ] >>= fun () ->
      OS.Time.sleep 0.2 >>= fun () ->
      V.disconnect listen.netif >>= fun () ->
      Lwt.return_unit
    in
    let listen_fn () =
      V.listen listen.netif (E.input ~arpv4:(A.input listen.arp)
         ~ipv4:(fun buf -> Lwt.return_unit) ~ipv6:(fun buf -> Lwt.return_unit)
                                                listen.ethif)
    in
    Lwt.join [ listen_fn () ; multiple_ips () ] >>= fun () ->
    OS.Time.sleep 0.5 >>= fun () ->
    (* load our own representation of the ARP cache of the listener *)
    local_copy listen >>= fun cache ->
    Irmin.read_exn (cache "readback of map") listen.node >>= fun imap ->
    confirm imap (first_ip, (V.mac speaker1.netif)) >>= fun () ->
    confirm imap (second_ip, (V.mac speaker2.netif)) >>= fun () ->
    confirm imap (strip "192.168.3.33", (V.mac speaker3.netif)) >>= fun () ->
    confirm imap (strip "192.168.3.44", (V.mac speaker4.netif)) >>= fun () ->
    confirm imap (strip "192.168.3.255", (V.mac speaker5.netif)) >>= fun () ->
    Lwt.return_unit


  let input_changed_ip make_fn () =
    let backend = blessed_backend in
    get_arp_exn ~backend ~make_fn ~node:"speaker" () >>= fun speak ->
    get_arp_exn ~backend ~make_fn ~node:"listener" () >>= fun listen ->
    let multiple_ips () =
      A.set_ips speak.arp [ Ipaddr.V4.of_string_exn "10.23.10.1" ] >>= fun () ->
      A.set_ips speak.arp [ Ipaddr.V4.of_string_exn "10.50.20.22" ] >>= fun () ->
      A.set_ips speak.arp [ Ipaddr.V4.of_string_exn "10.20.254.2" ] >>= fun () ->
      A.set_ips speak.arp [ first_ip ] >>= fun () ->
      OS.Time.sleep 0.1 >>= fun () -> V.disconnect listen.netif >>= fun () ->
      Lwt.return_unit
    in
    let listen_fn () = V.listen listen.netif (E.input ~arpv4:(A.input listen.arp)
                                                ~ipv4:(fun buf -> Lwt.return_unit) ~ipv6:(fun buf -> Lwt.return_unit)
                                                listen.ethif)
    in
    Lwt.join [ listen_fn () ; multiple_ips () ;] >>= fun () ->
    OS.Time.sleep 0.5 >>= fun () ->
    (* listen.config should have the ARP cache history reflecting the updates send
       by speak.arp; a current read should show us first_ip *)
    local_copy listen >>= fun store ->
    Irmin.read_exn (store "readback of map") listen.node >>= fun map ->
    (* TODO: iterate over the commit history of IPs *)
    try
      let open Entry in
      match T.find first_ip map with
      | Confirmed (time, entry) -> OUnit.assert_equal entry (V.mac speak.netif);
        Lwt.return_unit
    with
      Not_found -> gripe "Expected cache entry not found in
    listener cache map, as read back from Irmin"

  let input_garbage make_fn () =
    let open A in
    let backend = blessed_backend in
    get_arp_exn ~backend ~make_fn ~node:"speaker" () >>= fun speak ->
    get_arp_exn ~backend ~make_fn ~node:"listener" () >>= fun listen ->
    A.set_ips listen.arp [ first_ip ] >>= fun () ->
    let listen_fn () = V.listen listen.netif (E.input ~arpv4:(A.input listen.arp)
                                                ~ipv4:(fun buf -> Lwt.return_unit) ~ipv6:(fun buf -> Lwt.return_unit)
                                                listen.ethif)
    in
    let fire_away = send_buf_sleep_then_dc speak.netif listen.netif in
    (* TODO: this is a good candidate for a property test -- `parse` on an arbitrary
       buffer of size less than k always returns `Too_short *)
    let listener_mac = V.mac listen.netif in
    let speaker_mac = V.mac speak.netif in
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
    local_copy listen >>= fun store ->
    Irmin.read_exn (store "readback of map") listen.node >>= fun map ->
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
      (Irmin_arp.Arp.Parse.arp_of_cstruct (Cstruct.create (Arpv4_wire.sizeof_arp - 1)));
    (* I think we actually can't trigger `Bad_mac, since the only condition that
       causes that in the underlying Macaddr implementation is being provided a
       string of insufficient size, which we guard against with `Too_short, ergo
       no test to make sure we return `Bad_mac *)
    let all_zero = (Cstruct.create (Arpv4_wire.sizeof_arp)) in
    Cstruct.memset all_zero 0;
    match Irmin_arp.Arp.Parse.arp_of_cstruct all_zero with
    | `Too_short -> gripe
                      "Arp.parse claimed that an appropriate-length zero'd buffer was too short"
    | `Bad_mac l -> let mac_strs = Printf.sprintf "%S" (String.concat ", " l) in
       gripe ("Arp.parse claimed these were bad MACs: " ^ mac_strs)
    | `Ok all_zero -> gripe "Arp.parse allowed a 0 protocol"
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

  let query_with_seeded_cache make_fn () =
    let backend = blessed_backend in
    (* gotta make a store with this in it *)
    let make_seeded_cache node (ip, entry) =
      let map = T.add ip entry (T.empty) in
      seed_cache make_fn node map
    in
    let entry = Entry.Confirmed ((Clock.time () +.  60.), sample_mac) in
    let seeded = make_seeded_cache ["speaker"] (second_ip, entry) in
    seeded >>= fun seeded_cache ->
    let poll_remote = seeded_cache "make remote" in
    get_arp_exn ~backend ~make_fn ~node:"listener" ~pull:[] () >>= fun listen ->
    get_arp_exn ~backend ~make_fn ~node:"speaker" ~pull:[poll_remote]() >>= fun speak ->
    A.set_ips speak.arp [ first_ip ] >>= fun () ->
      (* OK, since we seeded the cache, calling query for that key
         should not emit an ARP query and should return straight away *)
      timeout_or ~timeout:0.5 ~msg:"Query sent for something that was seeded in
        the cache" listen.netif
        (fun () -> A.query speak.arp second_ip >>= function
           | `Ok mac when mac = sample_mac -> (* yay! *)
             V.disconnect listen.netif
           | `Ok mac -> gripe ("pre-seeded query got a MAC, but it's the wrong one: "
                        ^ (Macaddr.to_string mac))
           | `Timeout -> gripe "Query timed out for something that was seeded in the cache"
        )
        (fun () -> (fun buf -> OUnit.assert_failure "Listener heard a
    packet, but speaker should've had a cache entry")
        )

  let query_sent_with_empty_cache make_fn () =
    let backend = blessed_backend in
    get_arp_exn ~backend ~make_fn ~node:"speaker" () >>= fun speak ->
    get_arp_exn ~backend ~make_fn ~node:"listener" () >>= fun listen ->
    let do_fn () =
      A.query speak.arp first_ip >>= function
      | `Ok mac -> gripe "query returned a MAC when the cache
    should've been empty and nobody could possibly be responding"
      | `Timeout -> Lwt.return_unit (* we shouldn't get this far, but if
                                       timeout_or has a large value we might and
                                       if that happens, this is indeed the
                                       expected behavior *)
    in
    let listen_fn () =
      fun buf ->
        match Irmin_arp.Arp.Parse.arp_of_cstruct (Cstruct.shift buf 14) with
        | `Too_short | `Unusable | `Bad_mac _ ->
          OUnit.assert_failure "Attempting to produce a probe instead
                                                 resulted in a strange packet"
      | `Ok arp ->
        let expected_arp = { Irmin_arp.Arp.op = `Request;
                             sha = (V.mac speak.netif); spa = Ipaddr.V4.any;
                             tha = Macaddr.broadcast; tpa = first_ip } in
        OUnit.assert_equal expected_arp arp; V.disconnect listen.netif
  in
  timeout_or ~timeout:0.1 ~msg:"ARP probe not sent in response to a query"
    listen.netif do_fn listen_fn

  let entries_aged_out make_fn () =
    let backend = blessed_backend in
    let expiry = Clock.time () +. 1. in
    let seeded_map = T.add first_ip (Entry.Confirmed (expiry, sample_mac)) T.empty in
    seed_cache make_fn ["listener"] seeded_map >>= fun sour_milk ->
    let sour_cream = (sour_milk "make remote") in
    get_arp_exn ~backend ~make_fn ~node:"listener" ~pull:[sour_cream] () >>= fun listen ->
    (* now you see it *)
    local_copy listen >>= fun store ->
    Irmin.read_exn (store "readback of map") listen.node >>= fun map ->
    confirm map (first_ip, sample_mac) >>= fun () ->
    (* nothing up my sleeve *)
    OS.Time.sleep 2. >>= fun () ->
    (* now you don't! *)
    local_copy listen >>= fun store ->
    Irmin.read_exn (store "readback of map") listen.node >>= fun map ->
    OUnit.assert_raises Not_found (fun () -> T.find first_ip map);
    Lwt.return_unit

  let pull_nonsense_data make_fn () =
    let novel_store =
      Irmin.basic (module Irmin_mem.Make) (module Irmin.Contents.String) in
    let novel_config = Irmin_mem.config () in
    Irmin.create novel_store novel_config Irmin_unix.task >>= fun novel ->
    Irmin.update (novel "initial title: The Watchful Listener") ["listener"]
      "Hiding from English summer weather" >>= fun () ->
    let remote_novel = (novel "Let's immortalize this!") in
    get_arp ~make_fn ~node:"listener" ~pull:[remote_novel] () >>= function
    | `Ok stack -> gripe "get_arp claimed success when impossible"
    | `Error `Semantics -> Lwt.return_unit
    | `Error e -> gripe "get_arp: failure reported, but wrong type"

end

let lwt_run f () = Lwt_main.run (f ())

module type IRMIN_ARP = sig
  include V1_LWT.ARP
  val connect : E.t -> Irmin.config -> string list -> [`Ok of t | `Error of
                                                         error ] Lwt.t
end

let () =
  (* let buffer = MProf_unix.mmap_buffer ~size:1000000 "test_arp.ctf" in
  let trace_config = MProf.Trace.Control.make buffer MProf_unix.timestamper in
  MProf.Trace.Control.start trace_config; *)
  (* Log.set_log_level Log.DEBUG;
  Log.color_on ();
  Log.set_output stdout; *)

  let tests make_fn speed tests =
    List.map
      (fun (name, test) -> name, speed, test (make_fn name) |> lwt_run)
      tests
  in
  let ip_crud (module Maker : Irmin.S_MAKER) make_fn =
    let module Test = Test(Maker) in
    tests make_fn `Quick
      [ "set_ips", Test.set_ips;
        "get_remove_ips", Test.get_remove_ips
      ] in
  let parse (module Maker : Irmin.S_MAKER) =
    let module Test = Test(Maker) in
    [
      "parse_zeros", `Quick, Test.parse_zeros |> lwt_run;
      "parse_unparse", `Quick, Test.parse_unparse |> lwt_run;
    ] in
  let query (module Maker : Irmin.S_MAKER) make_fn =
    let module Test = Test(Maker) in
    tests make_fn `Slow
      [
        "query_with_seeded_cache", Test.query_with_seeded_cache;
        "query_sent_with_empty_cache", Test.query_sent_with_empty_cache;
      ] in
  let input (module Maker : Irmin.S_MAKER) make_fn =
    let module Test = Test(Maker) in
    tests make_fn `Slow Test.([
      "input_changed_ip", input_changed_ip ;
      "input_garbage", input_garbage ;
      "input_single_garp", input_single_garp ;
      "input_multiple_garp", input_multiple_garp ;
      "input_single_unicast_reply", input_single_unicast ;
    ]) in
  let create (module Maker : Irmin.S_MAKER) make_fn =
    let module Test = Test(Maker) in
    tests make_fn `Quick [
      "create_is_consistent", Test.create_is_consistent ;
    ] in
  let pulls (module Maker : Irmin.S_MAKER) make_fn =
    let module Test = Test(Maker) in
    tests make_fn `Quick [
      "pull_nonsense_data", Test.pull_nonsense_data ;
    ] in
  let aging (module Maker : Irmin.S_MAKER) make_fn =
    let module Test = Test(Maker) in
    tests make_fn `Slow [
      "entries_aged_out", Test.entries_aged_out ;
    ] in
  let mem_store _ = Irmin_mem.config in
  let git_store subdir = Irmin_git.config ?head:None ~root:(root ^ "/" ^ subdir) ~bare:false in
  let tests = [
    "parse", parse (module Irmin_mem.Make);
    "create_mem", create (module Irmin_mem.Make) mem_store;
    "create_disk", create (module Irmin_backend_fs) git_store;
    "pulls_mem", pulls (module Irmin_mem.Make) mem_store;
    "pulls_disk", pulls (module Irmin_backend_fs) git_store;
    "input_mem", input (module Irmin_mem.Make) mem_store;
    "input_disk", input (module Irmin_backend_fs) git_store;
    "ip_CRUD_mem", ip_crud (module Irmin_mem.Make) mem_store;
    "ip_CRUD_disk", ip_crud (module Irmin_backend_fs) git_store;
    "query_mem", query (module Irmin_mem.Make) mem_store;
    "query_disk", query (module Irmin_backend_fs) git_store;
    "aging_mem", aging (module Irmin_mem.Make) mem_store;
    "aging_disk", aging (module Irmin_backend_fs) git_store;
  ] in
  Alcotest.run "Irmin_arp.Arp" tests
