open Lwt

let or_error name fn t =
  fn t >>= function
  | `Error e -> OUnit.assert_failure ("Error starting " ^ name)
  | `Ok t -> return t

let create_returns () =
  (* Arp.create returns something bearing resemblance to an Arp.t *)
  let module B = Basic_backend.Make in
  let module V = Vnetif.Make(B) in
  let module E = Ethif.Make(V) in
  let module A = Irmin_arp.Arp.Make(E) in

  let backend = B.create () in
  or_error "backend" V.connect backend >>= fun netif ->
  or_error "ethif" E.connect netif >>= fun ethif ->
  let arp = A.create ethif in
  (* possibly assert some qualities of a freshly-created ARP interface -- e.g.
     no bound IPs, empty cache, etc *)
  OUnit.assert_equal 1 1;
  Lwt.return_unit

let () =
  let create = [

  ] in
  let ip_crud = [

  ] in
  let query = [

  ] in
  let input = [

  ] in
  Alcotest.run "Irmin_arp.Arp" [
    "create", create;
    "ip_CRUD", ip_crud;
    "query", query;
    "input", input
  ]
