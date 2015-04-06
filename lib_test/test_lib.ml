module Entry = Irmin_arp.Entry
module Ipv4_map = Map.Make(Ipaddr.V4)

let parse ip mac time = (Ipaddr.V4.of_string_exn ip, Macaddr.of_string_exn
                           mac, time)
let confirm time mac = Entry.make_confirmed time mac

let ip1_str, mac1_str = "192.168.3.11", "10:9a:dd:00:00:11"
let ip2_str, mac2_str = "192.168.3.22", "00:16:3e:00:00:22"
let ip3_str, mac3_str = "192.168.3.50", "a0:23:4a:00:00:50"

let ip1, mac1, time1 = parse ip1_str mac1_str 0.7
let ip2, mac2, time2 = parse ip2_str mac2_str 1.5
let ip3, mac3, time3 = parse ip3_str mac3_str 2.0

let sample_table () =
  let m = Ipv4_map.singleton ip1 (confirm time1 mac1) in
  let m = Ipv4_map.add ip2 (confirm time2 mac2) m in
  let m = 
    let noop = Lwt.task () in
    Ipv4_map.add ip3 (Entry.make_pending noop) m in
  m

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
