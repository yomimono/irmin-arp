open Lwt
open Test_lib

let root = "irmin_test"

let make_in_memory () =
  let store = Irmin.basic (module Irmin_mem.Make) (module T) in
  let config = Irmin_mem.config () in
  Irmin.create store config Irmin_unix.task

let make_on_disk ~root ~bare () = 
  let store = Irmin.basic (module Irmin_unix.Irmin_git.FS) (module T) in
  let config = Irmin_git.config ~root ~bare () in
  Irmin.create store config Irmin_unix.task

let update_and_readback map t ~update_msg ~readback_msg node 
  : Entry.t Ipv4_map.t Lwt.t =
  Irmin.update (t update_msg) node (T.of_map map) >>= fun () ->
  Irmin.read_exn (t readback_msg) node >>= fun map ->
  Lwt.return (T.to_map map)

let clone_update t ~read_msg ~update_msg ~branch_name node fn =
  Irmin.clone_force Irmin_unix.task (t read_msg) branch_name >>= fun branch ->
  fn branch node >>= fun map ->
  Irmin.update (branch update_msg) node (T.of_map map) >>=
  fun () -> Lwt.return branch

let readback_works _ctx =
  make_on_disk ~root ~bare:false ()
  >>= fun t ->
  (* delete previous node contents *)
  let node = T.Path.create [(T.Path.Step.of_hum "readback")] in
  Irmin.remove (t "readback_works: new test begins") node >>= fun () ->
  (* try to store something; make sure we get it back out *)
  let map = sample_table () in
  update_and_readback map t ~update_msg:"readback_works: change a node"
      ~readback_msg:"readback_works: readback of initial map" node >>= fun m ->
  assert_resolves m ip1 (confirm time1 mac1);
  assert_resolves m ip2 (confirm time2 mac2);
  (* TODO: these tests will only pass for in-memory storage backend;
     deserializing destroys the thread references in pending nodes *)
  (* assert_pending m ip3; *)
  (* OUnit.assert_equal m map; *)
  Irmin.remove (t "readback_works: test succeeded; removing data") node >>= fun
    () ->
  return_unit

let simple_update_works _cts =
  let node = T.Path.create [(T.Path.Step.of_hum "simple_update")] in
  let original = T.of_map (sample_table ()) in
  make_on_disk ~root ~bare:false () >>= fun t ->
  Irmin.remove (t "simple_update: new test begins") node >>= fun () ->
  Irmin.update (t "simple_update: set original map") node original >>= fun () ->
  (* clone, branch, update, merge *)
  (* use clone_force, since use of persistent storage means there may well
     actually be a historical "age_out" branch *)
  Irmin.clone_force Irmin_unix.task (t "simple_update: clone original map") "age_out" >>= fun
    x -> 
    (* yay, we have a clone; let's modify it! *)
    Irmin.read_exn (x "simple_update: get map from clone") node >>= fun m ->
    let m = T.to_map m in
    (* remove the sample_tableest entry *)
    let m = Ipv4_map.remove ip1 m in
    (* store it back on age_out branch *)
    Irmin.update (x "simple_update: aged out entries") node (T.of_map m) >>= fun updated_branch
    ->
    (* now try to merge age_out back into master *)
    Irmin.merge_exn "simple_update: Merging age_out into master" x ~into:t >>= fun () ->
    (* x and t should now both be missing the entry we removed for ip1 *)
    Irmin.read_exn (t "simple_update: get updated map after merge") node >>= fun new_m_from_t
    ->
    let new_m_from_t = T.to_map new_m_from_t in
    (* check contents *)
    assert_resolves new_m_from_t ip2 (confirm time2 mac2);
    assert_absent new_m_from_t ip1;
    (* overall equality test and pending node test are invalid when
       deserializing *)
    (* assert_pending new_m_from_t ip3; *)
    (* OUnit.assert_equal new_m_from_t m; *)
    Irmin.remove (t "simple_update: test succeeded; removing data") node >>= fun
      () ->
    return_unit

(* make sure our facility for automatically solving merge conflicts is working
   as expected *)
let merge_conflicts_solved _ctx = 
  let resolve_pending branch node =
    Irmin.read_exn (branch "resolve_pending: read map") node >>= fun map ->
    let map = T.to_map map in
    Lwt.return (Ipv4_map.add ip3 (confirm time3 mac3) map)
  in
  let remove_expired branch node =
    Irmin.read_exn (branch "remove_expired: read map") node >>= fun map ->
    let map = T.to_map map in
    Lwt.return (Ipv4_map.remove ip1 map)
  in
  let node = T.Path.create [(T.Path.Step.of_hum
                               "merge_conflicts_difft_nodes")] in
  let original = T.of_map (sample_table ()) in
  make_on_disk ~root ~bare:false () >>= fun t ->
  Irmin.remove (t "merge_conflicts_difft_nodes: beginning new test") node
  >>= fun () ->
  (* initialize data *) 
  Irmin.update (t "merge_conflicts_difft_nodes: set original map") node original >>= fun () ->
  clone_update t ~read_msg:"clone map" 
    ~update_msg:"merge_conflicts_difft_nodes: resolve arp entry"
    ~branch_name:"pending_resolved" node resolve_pending
  >>= fun pend_branch ->
  clone_update t ~read_msg:"clone map" 
    ~update_msg:"merge_conflicts_difft_nodes: remove expired entries"
    ~branch_name:"expired_removed" node remove_expired
  >>= fun exp_branch ->
  (* both branches (expired_removed, pending_resolved) should now be written
     into Irmin store *)
  (* try merging first one, then the other, into master (t) *)
  Irmin.merge_exn "merge_conflicts_difft_nodes: pending_resolved -> master" 
    pend_branch ~into:t >>= fun () ->
  Irmin.merge_exn "merge_conflicts_difft_nodes: expired_removed -> master" 
    exp_branch ~into:t >>= fun () ->
  (* the tree should have ip3 resolved, ip1 gone, ip2 unchanged, nothing else *)
  Irmin.read_exn (t "merge_conflicts_difft_nodes: final readback") node >>= fun map ->
  let map = T.to_map map in
  assert_absent map ip1;
  assert_resolves map ip2 (confirm time2 mac2);
  assert_resolves map ip3 (confirm time3 mac3);
  OUnit.assert_equal ~printer:string_of_int 2 (Ipv4_map.cardinal map);
  Irmin.remove (t "merge_conflicts_difft_noeds: test succeeded; removing data")
    node >>= fun () ->
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
  let original = T.of_map (sample_table ()) in
  Irmin.update (t "original map") node original >>= fun () ->
  clone_update t ~read_msg:"clone map" ~update_msg:"remove sample_table entries"
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

let complex_merge_pairwise () =
  (* make_on_disk ~root:disk_path ~bare:true () *) (* Hangs indefinitely... *)
  make_in_memory () >>= fun t ->
  let node = T.Path.empty in
  let original = T.of_map (sample_table ()) in
  Irmin.update (t "original map") node original >>= fun () ->
  Irmin.clone_force Irmin_unix.task (t "clone map") "update cache" >>= fun
    update_branch ->
  Irmin.clone_force Irmin_unix.task (t "clone map") "remove expired" >>= fun
    expire_branch ->
  (* update updated branch *)
  update_expired update_branch node >>= fun update_map ->
  Irmin.update (update_branch "update expired") node (T.of_map update_map) >>=
  fun () ->
  (* update removed branch *)
  remove_expired expire_branch node >>= fun expired_map ->
  Irmin.update (expire_branch "remove expired") node (T.of_map expired_map) >>=
  fun () ->
  Irmin.merge_exn "update_entries -> master" update_branch ~into:t >>= fun () ->
  Irmin.merge_exn "remove_expired -> master" expire_branch ~into:t >>= fun () ->
  Irmin.read_exn (t "final readback") node >>= fun map ->
  check_map_contents (T.to_map map);
  Lwt.return_unit

let complex_merge_update_then_remove _ctx =
  make_in_memory () >>= fun t ->
  let node = T.Path.empty in
  let original = T.of_map (sample_table ()) in
  Irmin.update (t "original map") node original >>= fun () ->
  clone_update t ~read_msg:"clone map" ~update_msg:"update cache"
    ~branch_name:"update_entries" node update_expired
  >>= fun update_branch ->
  clone_update t ~read_msg:"clone map" ~update_msg:"remove sample_table entries"
    ~branch_name:"remove_expired" node remove_expired
  >>= fun expire_branch ->
  Irmin.merge_exn "update_entries -> master" update_branch ~into:t >>= fun () ->
  Irmin.merge_exn "remove_expired -> master" expire_branch ~into:t >>= fun () ->
  Irmin.read_exn (t "final readback") node >>= fun map ->
  check_map_contents (T.to_map map);
  Lwt.return_unit

let lwt_run f () = Lwt_main.run (f ())

let () =
  let readback = [
    "readback", `Slow, lwt_run readback_works;
  ] in
  let update = [
    "simple_update", `Slow, lwt_run simple_update_works;
  ] in
  let merge = [
    "merge w/divergent nodes", `Slow, lwt_run merge_conflicts_solved;
    "merge w/conflict; remove then update", `Slow, lwt_run complex_merge_remove_then_update;
    "merge w/conflict; update then remove", `Slow, lwt_run complex_merge_update_then_remove;
    "merge w/conflict; both clones, both updates, both merges", `Slow, lwt_run
      complex_merge_pairwise;
  ] in
  Alcotest.run "Irmin_arp" [
    "readback", readback;
    "update", update;
    "merge", merge;
  ]
