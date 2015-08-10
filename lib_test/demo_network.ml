open Common
open Lwt.Infix

module A_fs = Irmin_arp.Arp.Make(E)(Clock)(OS.Time)(Random)(Irmin_backend_fs)
module A_mem = Irmin_arp.Arp.Make(E)(Clock)(OS.Time)(Random)(Irmin_mem.Make)
module A = A_fs (* change to A_mem for in-memory store! *)
module IPV4 = Ipv4.Make(E)(A)
module TCP = Tcp.Flow.Make(IPV4)(OS.Time)(Clock)(Random)

let netmask = Ipaddr.V4.of_string_exn "255.255.255.0"

let server_1_ip = Ipaddr.V4.of_string_exn "192.168.252.1"
let server_2_ip = Ipaddr.V4.of_string_exn "192.168.252.2"

let echo_port = 443

let pester_interval = 1.0
let crosstalk_interval = 5.0

let root = "demo_results"

let strip = Ipaddr.V4.to_string

let ignore_errors fn = function
  | `Ok q -> fn q
  | `Error _ -> Log.warn "DEMO: error reading or writing from flow"; Lwt.return_unit
  | `Eof -> Log.warn "DEMO: EOF reading or writing from flow"; Lwt.return_unit

let get_arp ?(backend = blessed_backend) ~root ~node ?(pull=[]) () =
  or_error "backend" V.connect backend >>= fun netif ->
  or_error "ethif" E.connect netif >>= fun ethif ->
  let config = Irmin_storer_fs.config ~root () in
  A.connect ethif config ~node:[node] ~pull >>= function
  | `Ok arp -> Lwt.return (netif, ethif, arp)
  | `Error _ -> Lwt.fail (failwith "Arp.connect failed!")

let arp_and_tcp_listeners netif ethif arp ip tcp () =
        V.listen netif (E.input
                          ~ipv6:(fun buf -> Lwt.return_unit)
                          ~arpv4:(fun buf -> A.input arp buf)
                          ~ipv4:(
                            IPV4.input
                              ~tcp:(TCP.input tcp ~listeners:(fun _ -> None))
                              ~udp:(fun ~src ~dst _buf -> Lwt.return_unit)
                              ~default:(fun ~proto ~src ~dst _ -> Lwt.return_unit)
                              ip
                          )
                          ethif )


let start_ip ip_addr (netif, ethif, arp) =
  IPV4.connect ethif arp >>= function
  | `Error e -> OUnit.assert_failure (Printf.sprintf "error starting ip %s"
                                        (Ipaddr.V4.to_string ip_addr))
  | `Ok i ->
    IPV4.set_ip i ip_addr >>= fun () -> IPV4.set_ip_netmask i netmask >>= fun () ->
    Lwt.return (netif, ethif, arp, i)

let spawn_listeners (netif, ethif, arp, ip, tcp) =
  (* TODO: an async_hook for error reporting would be nice *)
  Lwt.async (arp_and_tcp_listeners netif ethif arp ip tcp);
  Lwt.return (netif, ethif, arp, ip, tcp)

let start_tcp_listener ~port ~fn (netif, ethif, arp, ip) =
  let chooser = function | n when n = port -> Some fn | _ -> None in
  or_error "tcp" TCP.connect ip >>= fun tcp ->
  let listener = (
        V.listen netif (E.input
                    ~ipv6:(fun buf -> Lwt.return_unit)
                    ~arpv4:(fun buf -> A.input arp buf)
                    ~ipv4:(
                      IPV4.input
                        ~tcp:(TCP.input tcp ~listeners:chooser)
                        ~udp:(fun ~src ~dst _buf -> Lwt.return_unit)
                        ~default:(fun ~proto ~src ~dst _ -> Lwt.return_unit)
                        ip
                    )
                    ethif ) 
  ) in
  Lwt.return (netif, ethif, arp, ip, tcp, listener)

let clients ~backend lownum highnum =
  let rec intlist s e acc =
    if s = e then List.rev (s::acc) else intlist (s+1) e (s::acc) in
  let base = Ipaddr.V4.to_int32 (Ipaddr.V4.of_string_exn "192.168.252.0") in
  let our_ip base offset =
      Ipaddr.V4.of_int32 Int32.(add base (of_int offset))
  in
  let ips = List.map (fun number -> our_ip base number) (intlist lownum highnum []) in
  let name_repo ip = Printf.sprintf "client_%s" (Ipaddr.V4.to_string ip) in
  let start_tcp (n, e, a, ip) =
    or_error "tcp" TCP.connect ip >>= fun tcp -> Lwt.return (n, e, a, ip, tcp)
  in
  Lwt_list.map_p
      (fun ip ->
         get_arp ~backend ~root ~node:(name_repo ip) () >>=
         start_ip ip >>= start_tcp >>= spawn_listeners
      ) ips

let servers ~backend =
  let rec echo flow =
    let ignore_errors fn = function
      | `Ok q -> fn q
      | `Error _ | `Eof ->
        Log.warn "DEMO: connection terminated!";
        Lwt.return_unit
    in
    (* try echoing, but don't really mind if we fail *)
    TCP.read flow >>= ignore_errors (fun buf ->
        TCP.write flow buf >>= fun _ -> Lwt.return_unit
      ) >>= fun () -> echo flow
  in
  let start_server ~root ~node ~ip =
    get_arp ~backend ~root ~node ()
    >>= start_ip ip
    >>= start_tcp_listener ~port:echo_port ~fn:echo
  in
  start_server ~root ~node:"server_1" ~ip:server_1_ip >>= fun s1 ->
  start_server ~root ~node:"server_2" ~ip:server_2_ip >>= fun s2 ->
  Lwt.return (s1, s2)

let converse
    (_, _, _, server_ip, _, _)
    (_, _, client_arp, client_ip, client_tcp)
    =
  (* every second, bother the other end and see whether they have anything to
     say back to us *)
  let dest = List.hd (IPV4.get_ip server_ip) in
  let src = List.hd (IPV4.get_ip client_ip) in
  Log.warn "DEMO: trying connection from %s to %s on port %d"
    (Ipaddr.V4.to_string src) (Ipaddr.V4.to_string dest) echo_port;
  TCP.create_connection client_tcp (List.hd (IPV4.get_ip server_ip), echo_port)
  >>= function
  | `Error _ -> Lwt.fail (failwith "couldn't establish connection between client and server")
  | `Ok flow ->
    let rec pester flow =
      let important_content = Cstruct.of_string "please acknowledge my existence" in
      TCP.write flow important_content >>= ignore_errors
        (
          Log.warn "%s -> %s: %s" (strip src) (strip dest) (Cstruct.to_string
                                                             important_content);
          fun () -> TCP.read flow >>= ignore_errors (fun buf ->
          Log.warn "%s -> %s: %s" (strip dest) (strip src) (Cstruct.to_string buf);
          Lwt.return_unit )
        ) >>= fun () ->
      OS.Time.sleep pester_interval >>= fun () -> pester flow
    in
    let strip ip = IPV4.get_ip ip |> List.hd |> Ipaddr.V4.to_string in
    Log.warn "DEMO: connection established between %s and %s!" (strip client_ip)
      (strip server_ip);
    pester flow

let crosstalk ((_, _, _, left_ip, _),
    (_, _, _, right_ip, _)) : unit Lwt.t =
  let rec gossip dst = 
    let (frame, len) = IPV4.allocate_frame left_ip ~dst ~proto:`UDP in
    (* this is a broken packet -- no udp header *)
    let app_data = (Cstruct.shift frame len) in
    let secrets = "CONFIDENTIAL GOSSIP" in
    Cstruct.blit_from_string secrets 0 app_data 0 (String.length secrets);
    IPV4.write left_ip frame app_data >>= fun () ->
    Log.warn "%s -> %s: %s" (strip (List.hd (IPV4.get_ip left_ip))) (strip dst)
      secrets;
    OS.Time.sleep crosstalk_interval >>= fun () -> gossip dst
  in
  let dst = List.hd (IPV4.get_ip right_ip) in
  gossip dst

let ok_go () =
  Log.set_log_level Log.WARN;
  Log.color_on ();
  Log.set_output stdout;
  let backend = B.create ~yield:(fun () -> Lwt_main.yield ()) ~use_async_readers:true () in
  let get_listener (_, _, _, _, _, listener) = listener in
  servers ~backend >>= fun (s1, s2) ->
  Log.warn "DEMO: servers started";
  (* last entry of servers is a listener for inclusion in Lwt.join *)
  (* some clients talk only to s1, some to only s2 *)
  clients ~backend 40 41 >>= fun s1_clients ->
  Log.warn "DEMO: clients contacting s1 online";
  OS.Time.sleep 0.01 >>= fun () ->
  clients ~backend 50 51 >>= fun s2_clients ->
  Log.warn "DEMO: clients contacting s2 online";
  OS.Time.sleep 0.01 >>= fun () ->
  (* "left" and "right" clients periodically send one another IP messages *)
  clients ~backend 10 11 >>= fun left_clients ->
  Log.warn "DEMO: crosstalk left-hand clients online";
  OS.Time.sleep 0.01 >>= fun () ->
  clients ~backend 20 21 >>= fun right_clients ->
  Log.warn "DEMO: crosstalk right-hand clients online";
  Log.warn "Starting demo in %s with %d nodes" root (2
                                                     + (List.length s1_clients)
                                                     + (List.length s2_clients)
                                                     + (List.length left_clients)
                                                     + (List.length right_clients));
                                                       
  (* clients are now up and running ARP listeners *)
  Lwt.choose [
    get_listener s1;
    get_listener s2;
    Lwt_list.iter_p (converse s1) s1_clients;
    Lwt_list.iter_p (converse s2) s2_clients;
    Lwt_list.iter_p crosstalk (List.combine left_clients right_clients);
  ] >>= fun _ -> Lwt.return_unit

let () =
  Lwt_main.run (ok_go ())
