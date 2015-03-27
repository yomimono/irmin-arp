open Lwt

module Ipv4_map = Map.Make(Ipaddr.V4)
module Entry = Irmin_arp.Entry
module Table = Irmin_arp.Table

let parse ip mac time = (Ipaddr.V4.of_string_exn ip, Macaddr.of_string_exn
                           mac, time)
let confirm time mac = Entry.make_confirmed time mac

let old () =
  let ip1, mac1, time1 = parse "192.168.3.11" "10:9a:dd:00:00:11" 0.0 in
  let ip2, mac2, time2 = parse "192.168.3.22" "00:16:3e:00:00:22" 1.5 in
  let ip3 = Ipaddr.V4.of_string_exn "192.168.3.50" in
  
  let m = Ipv4_map.singleton ip1 (confirm time1 mac1) in
  let m = Ipv4_map.add ip2 (confirm time2 mac2) m in
  let m = Ipv4_map.add ip3 (Entry.make_pending ()) m in
  (* need a way to get a Table.t from a map.  type t isn't exposed by irmin_arp,
     so we can't just make it :( *)
  m

(* make an in-memory new bare irmin thing *)
(* make a simple tree in it [old] *)
(* clone [old]; modify it to get [new1] *)
(* clone [old]; modify it to get [new2] *)
(* merge [new1, new2] on [old] *)

let main () =
  let module P = Irmin.Path.String_list in
  let module T = Table(Ipv4_map)(P) in
  let store = Irmin.basic (module Irmin_mem.Make) (module T) in
  let config = Irmin_mem.config () in
  Irmin.create store config Irmin_unix.task >>= fun t ->
  (* try to store something; make sure we get it back out *)
  let node = T.Path.empty in
  (* update, etc operations need a Table.t ; we know how to make Ipv4_map.t's *)
  let wrapped_old = T.of_map (old ()) in
  Irmin.update (t "initial map") node wrapped_old >>= fun () ->
  Irmin.read_exn (t "readback of initial map") node >>= fun map ->
  OUnit.assert_equal map wrapped_old;
  return_unit

let () =
  Lwt_unix.run (main ())
