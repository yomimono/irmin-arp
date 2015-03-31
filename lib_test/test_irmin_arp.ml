open Lwt

module Ipv4_map = Map.Make(Ipaddr.V4)
module Entry = Irmin_arp.Entry
module Table = Irmin_arp.Table
module P = Irmin.Path.String_list
module T = Table(Ipv4_map)(P)

let parse ip mac time = (Ipaddr.V4.of_string_exn ip, Macaddr.of_string_exn
                           mac, time)
let confirm time mac = Entry.make_confirmed time mac

let ip1, mac1, time1 = parse "192.168.3.11" "10:9a:dd:00:00:11" 0.0
let ip2, mac2, time2 = parse "192.168.3.22" "00:16:3e:00:00:22" 1.5
let ip3, mac3, time3 = parse "192.168.3.50" "a0:23:4a:00:00:50" 2.0

let make_in_memory () =
  let store = Irmin.basic (module Irmin_mem.Make) (module T) in
  let config = Irmin_mem.config () in
  Irmin.create store config Irmin_unix.task

let assert_in m k =
  OUnit.assert_equal ~msg:"asserting presence of key fails"
    ~printer:string_of_bool true (Ipv4_map.mem k m)
let assert_resolves m k v =
  assert_in m k;
  OUnit.assert_equal ~printer:Entry.to_string v (Ipv4_map.find k m)
let assert_pending m k =
  assert_in m k;
  OUnit.assert_equal ~msg:"asserting pending status of key's value fails" 
    ~printer:string_of_bool true
    (Entry.is_pending (Ipv4_map.find k m)) 
let assert_absent m k =
  OUnit.assert_equal ~msg:"asserting absence of key fails" 
    ~printer:string_of_bool false (Ipv4_map.mem k m)

let old () =
  let m = Ipv4_map.singleton ip1 (confirm time1 mac1) in
  let m = Ipv4_map.add ip2 (confirm time2 mac2) m in
  let m = Ipv4_map.add ip3 (Entry.make_pending ()) m in
  m

let update_and_readback map t ~update_msg ~readback_msg node =
  Irmin.update (t update_msg) node (T.of_map map) >>= fun () ->
  Irmin.read_exn (t readback_msg) node >>= fun map ->
  Lwt.return (T.to_map map)

let clone_update t ~read_msg ~update_msg ~branch_name node fn =
  Irmin.clone Irmin_unix.task (t read_msg) branch_name >>= function
  | `Duplicated_tag -> OUnit.assert_failure "duplicate tag, nonsensibly"
  | `Ok branch ->
    fn branch node >>= fun map ->
    Irmin.update (branch update_msg) node (T.of_map map) >>=
    fun () -> Lwt.return branch

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
  let map = old () in
  update_and_readback map t ~update_msg:"initial map"
      ~readback_msg:"readback of initial map" node >>= fun m ->
  OUnit.assert_equal m map;
  assert_resolves m ip1 (confirm time1 mac1);
  assert_resolves m ip2 (confirm time2 mac2);
  assert_pending m ip3;
  return_unit

let simple_update_works _cts =
  let node = T.Path.empty in
  let original = T.of_map (old ()) in
  make_in_memory () >>= fun t ->
  Irmin.update (t "original map") node original >>= fun () ->
  (* clone, branch, update, merge *)
  (* all the examples use `clone_force`, which just overwrites on name
     collisions, but a name collision would be bizarre enough that we should
     probably just invalidate the test *)
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

(* make sure our facility for automatically solving merge conflicts is working
   as expected *)
let merge_conflicts_solved _ctx = 
  let resolve_pending branch node =
    Irmin.read_exn (branch "read map") node >>= fun map ->
    let map = T.to_map map in
    Lwt.return (Ipv4_map.add ip3 (confirm time3 mac3) map)
  in
  let remove_expired branch node =
    Irmin.read_exn (branch "read map") node >>= fun map ->
    let map = T.to_map map in
    Lwt.return (Ipv4_map.remove ip1 map)
  in
  make_in_memory () >>= fun t ->
  (* initialize data *) 
  let node = T.Path.empty in
  let original = T.of_map (old ()) in
  Irmin.update (t "original map") node original >>= fun () ->
  clone_update t ~read_msg:"clone map" ~update_msg:"resolve arp entry"
    ~branch_name:"pending_resolved" node resolve_pending
  >>= fun pend_branch ->
  clone_update t ~read_msg:"clone map" ~update_msg:"remove expired entries"
    ~branch_name:"expired_removed" node remove_expired
  >>= fun exp_branch ->
  (* both branches (expired_removed, pending_resolved) should now be written
     into Irmin store *)
  (* try merging first one, then the other, into master (t) *)
  Irmin.merge_exn "pending_resolved -> master" pend_branch ~into:t >>= fun () ->
  Irmin.merge_exn "expired_removed -> master" exp_branch ~into:t >>= fun () ->
  (* the tree should have ip3 resolved, ip1 gone, ip2 unchanged, nothing else *)
  Irmin.read_exn (t "final readback") node >>= fun map ->
  let map = T.to_map map in
  assert_absent map ip1;
  assert_resolves map ip2 (confirm time2 mac2);
  assert_resolves map ip3 (confirm time3 mac3);
  OUnit.assert_equal ~printer:string_of_int 2 (Ipv4_map.cardinal map);
  Lwt.return_unit

let check_map_contents map =
  assert_resolves map ip1 (confirm time3 mac1);
  assert_resolves map ip2 (confirm time2 mac2);
  assert_pending map ip3;
  OUnit.assert_equal ~printer:string_of_int 3 (Ipv4_map.cardinal map)

let remove_expired branch node =
  Irmin.read_exn (branch "read map") node >>= fun map ->
  let map = T.to_map map in
  Lwt.return (Ipv4_map.remove ip1 map)

let update_expired branch node =
  Irmin.read_exn (branch "read map") node >>= fun map ->
  let map = T.to_map map in
  Lwt.return (Ipv4_map.add ip1 (confirm time3 mac1) map)

let complex_merge_remove_then_update _ctx =
  make_in_memory () >>= fun t ->
  let node = T.Path.empty in
  let original = T.of_map (old ()) in
  Irmin.update (t "original map") node original >>= fun () ->
  clone_update t ~read_msg:"clone map" ~update_msg:"remove old entries"
    ~branch_name:"remove_expired" node remove_expired
  >>= fun expire_branch ->
  clone_update t ~read_msg:"clone map" ~update_msg:"update cache"
    ~branch_name:"update_entries" node update_expired
  >>= fun update_branch ->
  Irmin.merge_exn "remove_expired -> master" expire_branch ~into:t >>= fun () ->
  Irmin.merge_exn "update_entries -> master" update_branch ~into:t >>= fun () ->
  Irmin.read_exn (t "final readback") node >>= fun map ->
  check_map_contents (T.to_map map);
  Lwt.return_unit

let complex_merge_update_then_remove _ctx =
  make_in_memory () >>= fun t ->
  let node = T.Path.empty in
  let original = T.of_map (old ()) in
  Irmin.update (t "original map") node original >>= fun () ->
  clone_update t ~read_msg:"clone map" ~update_msg:"update cache"
    ~branch_name:"update_entries" node update_expired
  >>= fun update_branch ->
  clone_update t ~read_msg:"clone map" ~update_msg:"remove old entries"
    ~branch_name:"remove_expired" node remove_expired
  >>= fun expire_branch ->
  Irmin.merge_exn "update_entries -> master" update_branch ~into:t >>= fun () ->
  Irmin.merge_exn "remove_expired -> master" expire_branch ~into:t >>= fun () ->
  Irmin.read_exn (t "final readback") node >>= fun map ->
  check_map_contents (T.to_map map);
  Lwt.return_unit

let main () =
  readback_works () >>= fun () -> Printf.printf "readback works!\n";
  simple_update_works () >>= fun () -> Printf.printf "simple update works!\n";
  merge_conflicts_solved () >>= fun () -> Printf.printf "merge conflicts are solved!\n";
  complex_merge_remove_then_update () >>= fun () -> Printf.printf 
    "complex merge (ordering 1) went okay!\n"; 
  complex_merge_update_then_remove () >>= fun () -> Printf.printf 
    "complex merge (ordering 2) went okay!\n";
  Printf.printf "All tests passed.\n";
  Lwt.return_unit

let () =
  Lwt_unix.run (main ())
