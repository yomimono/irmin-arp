open Lwt
module B = Basic_backend.Make
module V = Vnetif.Make(B)
module E = Ethif.Make(V)
module T = Irmin_arp.Table(Irmin.Path.String_list)
module Irmin_storer = Irmin_git
module Irmin_backend = Irmin_unix.Irmin_git.FS
module I = Irmin.Basic(Irmin_backend)(T)
module A = Irmin_arp.Arp.Make(E)(Clock)(Irmin_backend)

let or_error name fn t =
  fn t >>= function
  | `Error e -> OUnit.assert_failure ("Error starting " ^ name)
  | `Ok t -> return t

let get_arp () =
  let backend = B.create () in
  or_error "backend" V.connect backend >>= fun netif ->
  or_error "ethif" E.connect netif >>= fun ethif ->
  let config = Irmin_storer.config ~root:"arp_test_results" () in
  A.create ethif config >>= fun a ->
  Lwt.return (backend, a)

let my_ip = Ipaddr.V4.of_string_exn "192.168.3.1"

let create_returns () =
  (* Arp.create returns something bearing resemblance to an Arp.t *)
  (* possibly assert some qualities of a freshly-created ARP interface -- e.g.
     no bound IPs, empty cache, etc *)
  get_arp () >>= fun (_, a) ->
  OUnit.assert_equal [] (A.get_ips a);
  Lwt.return_unit

(* normally I'd test to make sure that we get the exception or error we expect
   if we feed `create` something nonsensible, but I don't see how to actually
   make anything nonsensible to feed to `create`, so I'll call that a test the
   typechecker does for us for free *)

let timeout_or (backend, a) do_fn listen_fn =
  or_error "backend" V.connect backend >>= fun listen_netif ->
  (* set up a listener on listen_netif that fails if it doesn't hear a GARP
     within some small number of seconds *)
  Lwt.join [
    do_fn ;
    (Lwt.pick [
        V.listen listen_netif listen_fn;
      Lwt_unix.sleep 0.1 >>= fun () -> OUnit.assert_failure "100ms timeout
      exceeded before listen_fn returned"
    ])
  ]

let set_ips () =
  get_arp () >>= fun (backend, a) ->
  (* set up a listener that will return when it hears a GARP *)
  or_error "backend" V.connect backend >>= fun listen_netif ->
(* TODO: according to the contract in arpv4.mli, add_ip, set_ip, and remove_ip
   are supposed to emit GARP packets; we should make sure that actually happens
   generalize this test for use in other functions *)
  Lwt.join [
    (A.set_ips a [ my_ip ] >>= fun a ->
    OUnit.assert_equal [ my_ip ] (A.get_ips a);
    Lwt.return_unit) ;
    (Lwt.pick [
      V.listen listen_netif 
        (fun buf -> match A.is_garp my_ip buf with 
           | false -> OUnit.assert_failure "something non-GARP sent after set_ips" 
           | true -> V.disconnect listen_netif
        );
      Lwt_unix.sleep 0.1 >>= fun () -> OUnit.assert_failure "GARP not emitted
      within 100ms of call to set_ips"
    ])
  ] >>= fun () ->
  A.set_ips a [] >>= fun a -> 
  OUnit.assert_equal [] (A.get_ips a);
  A.set_ips a [ my_ip; Ipaddr.V4.of_string_exn "10.20.1.1" ] >>= fun a ->
  OUnit.assert_equal [ my_ip; Ipaddr.V4.of_string_exn "10.20.1.1" ] (A.get_ips
                                                                       a);
  Lwt.return_unit

let get_remove_ips () =
  get_arp () >>= fun (backend, a) ->
  OUnit.assert_equal [] (A.get_ips a);
  A.set_ips a [ my_ip; my_ip ] >>= fun a ->
  let ips = A.get_ips a in
  OUnit.assert_equal true (List.mem my_ip ips);
  OUnit.assert_equal true (List.for_all (fun a -> a = my_ip) ips);
  OUnit.assert_equal true (List.length ips >= 1 && List.length ips <= 2);
  A.remove_ip a my_ip >>= fun a ->
  OUnit.assert_equal [] (A.get_ips a);
  A.remove_ip a my_ip >>= fun a ->
  OUnit.assert_equal [] (A.get_ips a);
  Lwt.return_unit

let input () =
  (* use on-disk git fs for cache so we can read it back and check it ourselves *)
  let listen_config = Irmin_storer.config ~root:"arp_test_results/listener" () in
  let speak_config = Irmin_storer.config ~root:"arp_test_results/speaker" () in
  let backend = B.create () in
  or_error "backend" V.connect backend >>= fun speak_netif ->
  or_error "ethif" E.connect speak_netif >>= fun speak_ethif ->
  A.create speak_ethif speak_config >>= fun speak_arp ->
  or_error "backend" V.connect backend >>= fun listen_netif ->
  or_error "ethif" E.connect listen_netif >>= fun listen_ethif ->
  A.create listen_ethif listen_config >>= fun listen_arp ->
  (* send a GARP from one side (speak_arp) and make sure it was heard on the
     other *)
  Lwt.join [
    (A.set_ips speak_arp [ my_ip ] >>= fun _a -> Lwt.return_unit) ;
    (Lwt.pick [
      V.listen listen_netif 
        (fun buf -> 
           Printf.printf "cool buffer!!\n"; 
           A.input listen_arp buf >>= fun () -> V.disconnect listen_netif
        );
      Lwt_unix.sleep 0.1 >>= fun () -> 
      OUnit.assert_failure "GARP not emitted within 100ms of call to set_ips"
    ])
  ] >>= fun () ->
  (* load our own representation of the ARP cache of the listener *)
  let store = Irmin.basic (module Irmin_backend) (module T) in
  Irmin.create store listen_config Irmin_unix.task >>= fun store ->
  Irmin.read_exn (store "readback of map") T.Path.empty >>= fun map ->
  try
    let open Irmin_arp.Entry in
    match T.find my_ip map with 
    | Confirmed (time, entry) -> OUnit.assert_equal entry (V.mac speak_netif);
      Lwt.return_unit
    | Pending _ -> OUnit.assert_failure "Pending entry for an entry that had a
  GARP emitted on the same vnetif backend"
  with
    Not_found -> OUnit.assert_failure "Expected cache entry not found in
    listener cache map, as read back from Irmin"

let lwt_run f () = Lwt_main.run (f ())

let () =
  let ip_crud = [
    "set_ips", `Slow, lwt_run set_ips;
    "get_remove_ips", `Slow, lwt_run get_remove_ips;
  ] in
  let query = [
    (* TODO: test query output *)
    (* TODO: test pending resolution and timeouts *)
  ] in
  let input = [
    "input", `Slow, lwt_run input
  ] in
  let create : Alcotest.test_case list = [
    "create_returns", `Slow, lwt_run create_returns ;
  ] in
  Alcotest.run "Irmin_arp.Arp" [
    "create", create;
    "ip_CRUD", ip_crud;
    "query", query;
    "input", input
  ]
