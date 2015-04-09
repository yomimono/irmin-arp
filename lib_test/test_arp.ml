open Lwt
module B = Basic_backend.Make
module V = Vnetif.Make(B)
module E = Ethif.Make(V)
module A = Irmin_arp.Arp.Make(E)

let or_error name fn t =
  fn t >>= function
  | `Error e -> OUnit.assert_failure ("Error starting " ^ name)
  | `Ok t -> return t

let get_arp () =
  let backend = B.create () in
  or_error "backend" V.connect backend >>= fun netif ->
  or_error "ethif" E.connect netif >>= fun ethif ->
  Lwt.return (backend, A.create ethif)

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
  (* set up a listener on listen_netif that fails if it doesn't hear a GARP
     within some small number of seconds *)
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

(* TODO: according to the contract in arpv4.mli, add_ip, set_ip, and remove_ip
   are supposed to emit GARP packets; we should make sure that actually happens
   *)
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

let lwt_run f () = Lwt_main.run (f ())

let () =
  let ip_crud = [
    "set_ips", `Slow, lwt_run set_ips;
    "get_remove_ips", `Slow, lwt_run get_remove_ips;
  ] in
  let query = [

  ] in
  let input = [

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
