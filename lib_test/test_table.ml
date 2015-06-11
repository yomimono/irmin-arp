open Lwt
open Test_lib

let root = "test_results/table_tests"

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
  let now = int_of_float (Unix.time ()) in
  let name = branch_name (* ^ "-" ^ (string_of_int now) *) in
  Irmin.clone_force Irmin_unix.task (t read_msg) name >>= fun branch ->
  fn branch node >>= fun map ->
  Irmin.update (branch update_msg) node (T.of_map map) >>=
  fun () -> Lwt.return branch

let test_node str = 
  let step = T.Path.Step.of_hum in
  T.Path.create [(step str)]

let readback_works make_fn () =
  make_fn () >>= fun t ->
  (* delete previous node contents *)
  let node = (test_node "readback") in
  Irmin.remove (t "readback_works: new test begins") node >>= fun () ->
  (* try to store something; make sure we get it back out *)
  let map = sample_table () in
  update_and_readback map t ~update_msg:"readback_works: change a node"
      ~readback_msg:"readback_works: readback of initial map" node >>= fun m ->
  assert_resolves m ip1 (confirm time1 mac1);
  assert_resolves m ip2 (confirm time2 mac2);
  Irmin.remove (t "readback_works: test succeeded; removing data") node >>= fun
    () ->
  return_unit

let expire () = 
  let ip = Ipaddr.V4.of_string_exn "10.0.0.1" in
  let mac = Macaddr.of_string_exn "00:16:3e:c0:ff:33" in
  let map = T.add ip (Entry.Confirmed (0.0, mac)) T.empty in
  OUnit.assert_equal (Entry.Confirmed (0.0, mac)) (T.find ip map);
  let expired_map = T.expire map 1337.3030 in
  OUnit.assert_raises Not_found (fun () -> T.find ip expired_map);
  OUnit.assert_equal T.empty expired_map;
  Lwt.return_unit

let simple_update_works make_fn () =
  let node = (test_node "simple_update") in
  let original = T.of_map (sample_table ()) in
  make_fn () >>= fun t ->
  Irmin.remove (t "simple_update: new test begins") node >>= fun () ->
  Irmin.update (t "simple_update: set original map") node original >>= fun () ->
  (* clone, branch, update, merge *)
  (* use clone_force, since use of persistent storage means there may well
     actually be a historical "age_out" branch *)
  let age_out = Printf.sprintf "age_out-%s" (string_of_int (int_of_float
                                                              (Unix.time ()))) in
  Irmin.clone_force Irmin_unix.task (t "simple_update: clone original map")
    age_out >>= fun
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
    Irmin.remove (t "simple_update: test succeeded; removing data") node >>= fun
      () ->
    return_unit

(* make sure our facility for automatically solving merge conflicts is working
   as expected *)
let merge_conflicts_solved make_fn () = 
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
  let node = (test_node "merge_conflicts_separate_entries") in
  let original = T.of_map (sample_table ()) in
  make_on_disk ~root ~bare:false () >>= fun t ->
  Irmin.remove (t "merge_conflicts_separate_entries: beginning new test") node
  >>= fun () ->
  (* initialize data *) 
  Irmin.update (t "merge_conflicts_separate_entries: set original map") node original >>= fun () ->
  clone_update t ~read_msg:"clone map" 
    ~update_msg:"merge_conflicts_separate_entries: resolve arp entry"
    ~branch_name:"pending_resolved" node resolve_pending
  >>= fun pend_branch ->
  clone_update t ~read_msg:"clone map" 
    ~update_msg:"merge_conflicts_separate_entries: remove expired entries"
    ~branch_name:"expired_removed" node remove_expired
  >>= fun exp_branch ->
  (* both branches (expired_removed, pending_resolved) should now be written
     into Irmin store *)
  (* try merging first one, then the other, into primary branch (t) *)
  Irmin.merge_exn "merge_conflicts_separate_entries: pending_resolved -> master" 
    pend_branch ~into:t >>= fun () ->
  Irmin.merge_exn "merge_conflicts_separate_entries: expired_removed -> master" 
    exp_branch ~into:t >>= fun () ->
  (* the tree should have ip3 resolved, ip1 gone, ip2 unchanged, nothing else *)
  Irmin.read_exn (t "merge_conflicts_separate_entries: final readback") node >>= fun map ->
  let map = T.to_map map in
  assert_absent map ip1;
  assert_resolves map ip2 (confirm time2 mac2);
  assert_resolves map ip3 (confirm time3 mac3);
  OUnit.assert_equal ~printer:string_of_int 2 (Ipv4_map.cardinal map);
  Irmin.remove (t "merge_conflicts_separate_entries: test succeeded; removing data")
    node >>= fun () ->
  Lwt.return_unit

let check_map_contents ~serialization map =
  assert_resolves map ip1 (confirm time3 mac1);
  assert_resolves map ip2 (confirm time2 mac2);
  OUnit.assert_equal ~printer:string_of_int 2 (Ipv4_map.cardinal map)

let remove_expired branch node =
  Irmin.read_exn (branch "read map") node >>= fun map ->
  let map = T.to_map map in
  Lwt.return (Ipv4_map.remove ip1 map)

let update_expired branch node =
  Irmin.read_exn (branch "read map") node >>= fun map ->
  let map = T.to_map map in
  Lwt.return (Ipv4_map.add ip1 (confirm time3 mac1) map)

let complex_merge_remove_then_update make_fn () =
  make_in_memory () >>= fun t ->
  let node = test_node "complex_merge_remove_then_update" in
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
  check_map_contents ~serialization:false (T.to_map map);
  Lwt.return_unit

let complex_merge_pairwise make_fn () =
  let node = (test_node "merge_pairwise") in
  let original = T.of_map (sample_table ()) in
  make_on_disk ~root ~bare:false () >>= fun t ->
  Irmin.update (t "merge_pairwise: original map") node original >>= fun () ->
  Irmin.clone_force Irmin_unix.task (t "merge_pairwise: clone map") "update_cache" >>= fun
    update_branch ->
  Irmin.clone_force Irmin_unix.task (t "merge_pairwise: clone map") "remove_expired" >>= fun
    expire_branch ->
  (* update updated branch *)
  update_expired update_branch node >>= fun update_map ->
  Irmin.update (update_branch "merge_pairwise: update expired") node (T.of_map update_map) >>=
  fun () ->
  (* update removed branch *)
  remove_expired expire_branch node >>= fun expired_map ->
  Irmin.update (expire_branch "merge_pairwise: remove expired") node (T.of_map expired_map) >>=
  fun () ->
  Irmin.merge_exn "merge_pairwise: update_entries -> master" update_branch ~into:t >>= fun () ->
  Irmin.merge_exn "merge_pairwise: remove_expired -> master" expire_branch ~into:t >>= fun () ->
  Irmin.read_exn (t "merge_pairwise: final readback") node >>= fun map ->
  check_map_contents ~serialization:true (T.to_map map);
  Irmin.remove (t "merge_pairwise: test succeeded; removing data") node >>= fun () ->
  Lwt.return_unit

let merge_competing_branches make_fn () =
  let rec clone_nicely task cache tag =
    Irmin.clone task cache tag >>= function
    | `Ok branch -> Lwt.return branch
    | `Duplicated_tag ->
      let now = Sys.time () in
      (* this is very ugly, but string_of_float will remove trailing 0's and
         various git tools don't like branch names that end with .'s, so slap
         an arbitrary non-. character on the end of the branch name *)
      let new_tag = tag ^ "-" ^ (string_of_float now) ^ "0" in
      clone_nicely task cache new_tag
  in
  let node = (test_node "merge_competing_branches") in
  let original = T.of_map (sample_table ()) in
  make_on_disk ~root ~bare:false () >>= fun t ->
  Irmin.update (t "merge_competing_branches: original map") node original >>= fun () ->
  clone_nicely Irmin_unix.task (t "merge_competing: first branch")
    "competing" >>= fun t1 ->
  let ip = Ipaddr.V4.of_string_exn "10.0.0.1" in
  let mac = Macaddr.of_string_exn "00:16:3e:11:11:11" in
  let t1_map = T.add ip (Entry.Confirmed (0.5, mac)) T.empty in
  Irmin.update (t1 "merge_competing: first branch") node t1_map >>=
  fun () ->
  clone_nicely Irmin_unix.task (t "merge_competing: second branch")
    "competing" >>= fun t2 ->
  let ip = Ipaddr.V4.of_string_exn "10.0.0.1" in
  let mac = Macaddr.of_string_exn "00:16:3e:22:22:22" in
  let t2_map = T.add ip (Entry.Confirmed (1.0, mac)) T.empty in
  Irmin.update (t2 "merge_competing: second branch") node t2_map >>=
  fun () ->
  (* now try merging t1 into primary *)
  Irmin.merge_exn "merge_competing: merge t1 into primary" t1 ~into:t >>= fun () ->
  (* clone a new branch *)
  clone_nicely Irmin_unix.task (t "merge_competing: third branch")
    "not_competing" >>= fun t3 ->
  Irmin.update (t "merge_competing: update third branch") node T.empty >>= fun
    () ->
  Irmin.merge_exn "merge_competing: merge t3 into primary" t3 ~into:t >>= fun () ->
  Irmin.merge_exn "merge_competing: merge t2 into primary" t2 ~into:t >>= fun () ->
  Irmin.read_exn (t "merge_competing: final readback") node >>= fun map ->
  OUnit.assert_equal (T.to_map t2_map) (T.to_map map);
  Lwt.return_unit

let lwt_run f () = Lwt_main.run (f ())

let () =
  Log.set_log_level Log.DEBUG;
  Log.color_on ();
  Log.set_output stdout;
  let readback make_fn = [
    "readback", `Quick, readback_works make_fn |> lwt_run;
  ] in
  let expire = [
    "expire", `Quick, lwt_run expire;
  ] in
  let update make_fn = [
    "simple_update", `Quick, simple_update_works make_fn |> lwt_run;
  ] in
  let merge make_fn = [
    "merge w/conflict; remove then update", `Quick, 
    complex_merge_remove_then_update make_fn |> lwt_run;
    "merge w/conflict; both clones, both updates, both merges", `Quick,
      complex_merge_pairwise make_fn |> lwt_run;
    "merge w/no conflict", `Quick, merge_conflicts_solved make_fn |> lwt_run;
    "merge w/competing branches", `Quick, merge_competing_branches make_fn |>
                                          lwt_run;

  ] in
  let on_disk = make_on_disk ~root ~bare:false in
  let in_memory = make_in_memory in
  Alcotest.run "Irmin_arp" [
    "expire", expire ;
    "readback (on disk)", readback on_disk;
    "readback (in memory)", readback in_memory;
    "update (on disk)", update on_disk;
    "update (in memory)", update in_memory;
    "merge (on disk)", merge on_disk;
    "merge (in memory)", merge in_memory;
  ]
