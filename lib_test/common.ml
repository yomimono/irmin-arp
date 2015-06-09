(* some useful common module definitions and functions *)
open Lwt
module B = Basic_backend.Make
module V = Vnetif.Make(B)
module E = Ethif.Make(V)
module T = Table.Make(Irmin.Path.String_list)
module Irmin_storer_fs = Irmin_git
module Irmin_backend_fs = Irmin_unix.Irmin_git.FS
module I_fs = Irmin.Basic(Irmin_backend_fs)(T)
module A_fs = Irmin_arp.Arp.Make(E)(Clock)(OS.Time)(Irmin_backend_fs)

let root = "test_results"

let or_error name fn t =
  fn t >>= function
  | `Error e -> OUnit.assert_failure ("Error starting " ^ name)
  | `Ok t -> return t

let clear_cache config =
  let store = Irmin.basic (module Irmin_backend_fs) (module T) in
  Irmin.create store config Irmin_unix.task >>= fun store ->
  let node = T.Path.empty in
  Irmin.remove (store "removing previous history for new test run") node

let get_arp ?(backend = B.create ()) ~root () =
  or_error "backend" V.connect backend >>= fun netif ->
  or_error "ethif" E.connect netif >>= fun ethif ->
  let config = Irmin_storer_fs.config ~root () in
  clear_cache config >>= fun () ->
  A_fs.connect ethif config >>= function
  | `Ok a -> Lwt.return (config, backend, netif, ethif, a)
  | `Error e -> OUnit.assert_failure "Couldn't start ARP :("

