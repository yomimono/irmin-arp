(* some useful common module definitions and functions *)
open Lwt
module B = Basic_backend.Make
module V = Vnetif.Make(B)
module E = Ethif.Make(V)
module Entry = Inds_entry.Make(Inds_wrappers.Macaddr_entry)
module T = Inds_table.Make (Inds_wrappers.Ipv4addr_key) (Entry) (Irmin.Path.String_list)
module Irmin_storer_fs = Irmin_git
module Irmin_backend_fs = Irmin_unix.Irmin_git.FS
module I_fs = Irmin.Basic(Irmin_backend_fs)(T)
module I_mem = Irmin.Basic(Irmin_mem.Make)(T)

let root = "test_results"

let blessed_backend = B.create ~use_async_readers:true
    ~yield:(fun() -> Lwt_main.yield () ) ()

let or_error name fn t =
  fn t >>= function
  | `Error e -> OUnit.assert_failure ("Error starting " ^ name)
  | `Ok t -> return t

let clear_cache config node =
  let store = Irmin.basic (module Irmin_backend_fs) (module T) in
  Irmin.create store config Irmin_unix.task >>= fun store ->
  Irmin.remove (store "removing previous history for new test run") [node]

