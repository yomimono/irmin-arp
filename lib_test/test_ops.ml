open Test_lib

module Ipv4_map = Map.Make(Ipaddr.V4)
module T = Irmin_arp.Table(Ipv4_map)(Irmin.Path.String_list)

(* serialization format is just JSON *)

let write_empty_json () =
  (* empty map -> unit json *)
  let map = Ipv4_map.empty in
  OUnit.assert_equal ~printer:(fun p -> Ezjsonm.to_string (Ezjsonm.wrap p)) 
    (Ezjsonm.dict []) (T.Ops.to_json map)

let write_single_complete_entry () =
  let map = Ipv4_map.empty in
  (* one entry -> dictionary json entry *)
  let populated_map = Ipv4_map.add ip1 (confirm time1 mac1) map in
  let populated_json = T.Ops.to_json populated_map in
  Printf.printf "%s\n" (Ezjsonm.to_string (Ezjsonm.wrap populated_json));
  let dict = Ezjsonm.get_dict populated_json in
  (* there's one (and only one) entry *)
  OUnit.assert_equal ~printer:string_of_int 1 (List.length dict); 
  (* its name is ip1 *)
  OUnit.assert_equal ~printer:string_of_bool 
    true (Ezjsonm.mem populated_json [ip1_str]); 
  let ip1_node = Ezjsonm.find populated_json [ip1_str] in
  (* both expiry and mac are present *)
  OUnit.assert_equal ~printer:string_of_bool true (Ezjsonm.mem ip1_node ["expiry"]);
  OUnit.assert_equal ~printer:string_of_bool true (Ezjsonm.mem ip1_node ["mac"]);
  (* and their entries are correct *)
  OUnit.assert_equal ~printer:string_of_float time1 (Ezjsonm.get_float (Ezjsonm.find ip1_node ["expiry"]));
  OUnit.assert_equal mac1_str (Ezjsonm.get_string (Ezjsonm.find ip1_node
                                                     ["mac"]));
  (* { "192.168.3.50":{ "expiry":0.0, "mac":"00:11:22:33:44:55" } } *)
  ()

let write_populated_map () = 
  let map = sample_table () in
  let json = T.Ops.to_json map in
  let delicious_innards = Ezjsonm.get_dict json in
  (* should have an entry for each ip *)
  OUnit.assert_equal ~printer:string_of_int 3 (List.length delicious_innards);
  OUnit.assert_equal true (Ezjsonm.mem json [ip1_str]);
  OUnit.assert_equal true (Ezjsonm.mem json [ip2_str]);
  OUnit.assert_equal true (Ezjsonm.mem json [ip3_str])

let valify mac time = 
  Ezjsonm.dict [ ("mac", (Ezjsonm.string mac));
                 ("expiry", (Ezjsonm.float time)) ]

let formulate_json name mac time =
  Ezjsonm.dict [ (name, valify mac time) ]


let read_empty_json () =
  let nothin = T.Ops.of_json (Ezjsonm.dict []) in
  OUnit.assert_equal ~printer:string_of_int 0 (Ipv4_map.cardinal nothin)

let read_singleton_map () =
  let singleton_json = formulate_json ip1_str mac1_str time1 in
  let singleton_map = Ipv4_map.singleton ip1 (confirm time1 mac1) in
  OUnit.assert_equal ~printer:string_of_int 1 (Ipv4_map.cardinal (T.Ops.of_json singleton_json));
  OUnit.assert_equal singleton_map (T.Ops.of_json singleton_json)

let read_populated_map () =
  let j = T.Ops.to_json (sample_table ()) in
  let new_table = T.Ops.of_json j in
  assert_in new_table ip1;
  assert_in new_table ip2;
  assert_absent new_table ip3; (* Pending entry not preserved *)
  (* 2, since we don't preserve the Pending entry *)
  OUnit.assert_equal 2 (Ipv4_map.cardinal new_table);
  assert_resolves new_table ip1 (confirm time1 mac1);
  assert_resolves new_table ip2 (confirm time2 mac2)

let empty_maps_equal () =
  OUnit.assert_equal true (T.Ops.equal (Ipv4_map.empty) (Ipv4_map.empty))

let empty_populated_unequal () =
  let p = Ipv4_map.empty in
  let q = sample_table () in
  OUnit.assert_equal false (T.Ops.equal p q)

let differently_populated_unequal () =
  let p = Ipv4_map.singleton ip1 (confirm time1 mac1) in
  let q = sample_table () in
  OUnit.assert_equal false (T.Ops.equal p q)

let smaller () =
  let p = Ipv4_map.singleton ip1 (confirm time1 mac1) in
  let q = sample_table () in
  OUnit.assert_equal (-1) (T.Ops.compare (Ipv4_map.empty) q);
  OUnit.assert_equal (-1) (T.Ops.compare p q)

let larger () =
  let p = Ipv4_map.singleton ip1 (confirm time1 mac1) in
  let q = sample_table () in
  OUnit.assert_equal 1 (T.Ops.compare q (Ipv4_map.empty));
  OUnit.assert_equal 1 (T.Ops.compare q p)

let entry_values () =
  let p = sample_table () in
  (* p will have time == 1.5 *)
  let q = Ipv4_map.add ip2 (confirm 2.0 mac2) p in
  let r = Ipv4_map.add ip2 (Entry.make_pending ()) p in
  OUnit.assert_equal ~printer:string_of_int (-1) (T.Ops.compare p q);
  OUnit.assert_equal ~printer:string_of_int 1 (T.Ops.compare p r)

let equal_things_equal_size () = 
  let p = Ipv4_map.empty in
  OUnit.assert_equal (T.Ops.size_of p) (T.Ops.size_of Ipv4_map.empty);
  let p = Ipv4_map.singleton ip1 (confirm time1 mac1) in
  let q = Ipv4_map.singleton ip1 (confirm time1 mac1) in
  OUnit.assert_equal (T.Ops.size_of p) (T.Ops.size_of q);
  let p = sample_table () in
  let q = sample_table () in
  OUnit.assert_equal (T.Ops.size_of p) (T.Ops.size_of q)

let larger_things_larger_size () =
  let p = Ipv4_map.singleton ip1 (confirm time1 mac1) in
  let q = sample_table () in
  OUnit.assert_equal true ((T.Ops.size_of p) < (T.Ops.size_of q))


let () =
  let read_write_size = [
    (* don't bother testing read and write, since they're just wrappers around
       the json functions *)
    "equal_things_equal_size", `Slow, equal_things_equal_size;
    "larger_things_larger_size", `Slow, larger_things_larger_size
  ] in
  let json = [ 
    "write_empty_map", `Slow, write_empty_json;
    "write_singleton_map", `Slow, write_single_complete_entry;
    "write_populated_map", `Slow, write_populated_map;
    "read_empty_map", `Slow, read_empty_json;
    "read_singleton_map", `Slow, read_singleton_map;
    "read_populated_map", `Slow, read_populated_map
  ] in
  let comp_eq = [
    "empty_maps_equal", `Slow, empty_maps_equal;
    "empty_and_populated_unequal", `Slow, empty_populated_unequal;
    "differently_populated_unequal", `Slow, differently_populated_unequal;
    "smaller_is_recognized_compare", `Slow, smaller;
    "larger_is_recognized_compare", `Slow, larger;
    "different_entry_values_compare", `Slow, entry_values
  ] in
  Alcotest.run "Irmin_arp.Ops" [
    "read_write_size", read_write_size;
    "from_to_json", json;
    "compare_equal", comp_eq
  ]

