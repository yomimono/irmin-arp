open Lwt

module Ipv4_map = Map.Make(Ipaddr.V4)
module Entry = Irmin_arp.Entry
module Table = Irmin_arp.Table
module P = Irmin.Path.String_list
module T = Table(Ipv4_map)(P)

let parse ip mac time = (Ipaddr.V4.of_string_exn ip, Macaddr.of_string_exn
                           mac, time)
let confirm time mac = Entry.make_confirmed time mac

let make_in_memory () =
  let store = Irmin.basic (module Irmin_mem.Make) (module T) in
  let config = Irmin_mem.config () in
  Irmin.create store config Irmin_unix.task

let ip1, mac1, time1 = parse "192.168.3.11" "10:9a:dd:00:00:11" 0.0
let ip2, mac2, time2 = parse "192.168.3.22" "00:16:3e:00:00:22" 1.5
let ip3 = Ipaddr.V4.of_string_exn "192.168.3.50"

let assert_in m k =
  OUnit.assert_equal ~printer:string_of_bool (Ipv4_map.mem k m) true
let assert_resolves m k v =
  assert_in m k;
  OUnit.assert_equal ~printer:Entry.to_string (Ipv4_map.find k m) v
let assert_pending m k =
  assert_in m k;
  OUnit.assert_equal ~printer:string_of_bool
    (Entry.is_pending (Ipv4_map.find k m)) true
let assert_absent m k =
  OUnit.assert_equal ~printer:string_of_bool (Ipv4_map.mem k m) false

let old () =
  let m = Ipv4_map.singleton ip1 (confirm time1 mac1) in
  let m = Ipv4_map.add ip2 (confirm time2 mac2) m in
  let m = Ipv4_map.add ip3 (Entry.make_pending ()) m in
  m

(* make an in-memory new bare irmin thing *)
(* make a simple tree in it [old] *)
(* clone [old]; modify it to get [new1] *)
(* clone [old]; modify it to get [new2] *)
(* merge [new1, new2] on [old] *)

let readback_works _ctx =
  make_in_memory () >>= fun t ->
  (* try to store something; make sure we get it back out *)
  let node = T.Path.empty in
  (* update, etc operations need a Table.t ; we know how to make Ipv4_map.t's *)
  let wrapped_old = T.of_map (old ()) in
  Irmin.update (t "initial map") node wrapped_old >>= fun () ->
  Irmin.read_exn (t "readback of initial map") node >>= fun map ->
  (* returned object at least looks like what we put in *)
  OUnit.assert_equal map wrapped_old;
  (* and it has the right values in it *)
  let m = T.to_map map in
  assert_resolves m ip1 (confirm time1 mac1);
  assert_resolves m ip2 (confirm time2 mac2);
  assert_pending m ip3;
  return_unit

let updates_work _cts =
  let node = T.Path.empty in
  let original = T.of_map (old ()) in
  make_in_memory () >>= fun t ->
  Irmin.update (t "original map") node original >>= fun () ->
  (* clone, branch, update, merge *)
  (* all the examples use `clone_force`, which just overwrites on name
     collisions *)
  Irmin.clone Irmin_unix.task (t "clone original map") "age_out" >>= function
  | `Duplicated_tag ->
    OUnit.assert_failure "tag claimed to be duplicated on a fresh in-memory
    store"
  | `Ok x ->
    (* yay, we have a clone; let's modify it! *)
    Irmin.read_exn (x "get map from clone") node >>= fun m ->
    let m = T.to_map m in
    (* remove the oldest entry *)
    let m = Ipv4_map.remove ip1 m in
    (* store it back on age_out branch *)
    Irmin.update (x "aged out entries") node (T.of_map m) >>= fun updated_branch
    ->
    (* now try to merge age_out back into master *)
    Irmin.merge_exn "Merging age_out into master" x ~into:t >>= fun () ->
    (* x and t should now both be missing the entry we removed for ip1 *)
    Irmin.read_exn (t "get updated map after merge") node >>= fun new_m_from_t
    ->
    let new_m_from_t = T.to_map new_m_from_t in
    (* check contents *)
    assert_resolves new_m_from_t ip2 (confirm time2 mac2);
    assert_pending new_m_from_t ip3;
    assert_absent new_m_from_t ip1;
    (* overall equality test *)
    OUnit.assert_equal new_m_from_t m;
    return_unit

let main () =
  readback_works () >>= fun () ->
  updates_work ()

let () =
  Lwt_unix.run (main ())
