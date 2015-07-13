open Common
open Lwt.Infix

module A_fs = Irmin_arp.Arp.Make(E)(Clock)(OS.Time)(Irmin_backend_fs)
module IPV4 = Ipv4.Make(E)(A_fs)
module TCP = Tcp.Flow.Make(IPV4)(OS.Time)(Clock)(Random)

let netmask = Ipaddr.V4.of_string_exn "255.255.255.0"

let server_1_ip = Ipaddr.V4.of_string_exn "192.168.252.1"
let server_2_ip = Ipaddr.V4.of_string_exn "192.168.252.2"

let echo_port = 443

let root = "demo_results"

(* clients are 3..60, inclusive *)
(* clients whose ips end in 0 make requests to 192.168.252.1 *)
(* clients whose ips end in 5 make requests to 192.168.252.2 *)

let strip = Ipaddr.V4.to_string

let get_arp ?(backend = blessed_backend) ~root ~node ?(pull=[]) () =
  or_error "backend" V.connect backend >>= fun netif ->
  or_error "ethif" E.connect netif >>= fun ethif ->
  let config = Irmin_storer_fs.config ~root () in
  clear_cache config node >>= fun () ->
  A_fs.connect ethif config ~node:[node] ~pull >>= function
  | `Ok arp -> Lwt.return (netif, ethif, arp)
  | `Error _ -> Lwt.fail (failwith "Arp.connect failed!")

let arp_only_listener netif ethif arp () =
  let noop = fun _ -> Lwt.return_unit in
  V.listen netif (E.input 
                    ~ipv6:noop ~ipv4:noop
                    ~arpv4:(fun buf -> A_fs.input arp buf)
                    ethif)

let start_ip ip_addr (netif, ethif, arp) =
  IPV4.connect ethif arp >>= function
  | `Error e -> OUnit.assert_failure (Printf.sprintf "error starting ip %s"
                                        (Ipaddr.V4.to_string ip_addr))
  | `Ok i ->
    IPV4.set_ip i ip_addr >>= fun () -> IPV4.set_ip_netmask i netmask >>= fun () ->
    Lwt.return (netif, ethif, arp, i)

let spawn_arp_listener (netif, ethif, arp, i) =
  Lwt.async (arp_only_listener netif ethif arp);
  Lwt.return (netif, ethif, arp, i)

let start_tcp_listener ~port ~fn (netif, ethif, arp, ip) =
  let chooser = function | n when n = port -> Some fn | _ -> None in
  let name = Printf.sprintf "tcp listener %s" (strip (List.hd (IPV4.get_ip ip))) in
  or_error "tcp" TCP.connect ip >>= fun tcp ->
  let listener = (
        MProf.Trace.label name;
        V.listen netif (E.input 
                    ~ipv6:(fun buf -> Lwt.return_unit)
                    ~arpv4:(fun buf -> A_fs.input arp buf)
                    ~ipv4:(
                      IPV4.input 
                        ~tcp:(TCP.input tcp ~listeners:chooser)
                        ~udp:(fun ~src ~dst _buf -> Lwt.return_unit)
                        ~default:(fun ~proto ~src ~dst _ -> Lwt.return_unit)
                        ip
                    )
                    ethif ) ) in
  Lwt.return (netif, ethif, arp, ip, tcp, listener)

let clients ~backend = 
  let rec intlist s e acc = 
    if s = e then List.rev (s::acc) else intlist (s+1) e (s::acc) in
  let base = Ipaddr.V4.to_int32 (Ipaddr.V4.of_string_exn "192.168.252.0") in
  let our_ip base offset = 
      Ipaddr.V4.of_int32 Int32.(add base (of_int offset))
  in
  let ips = List.map (fun number -> our_ip base number) (intlist 3 60 []) in
  let name_repo ip = Printf.sprintf "client_%s" (Ipaddr.V4.to_string ip) in
  let start_tcp (n, e, a, ip) =
    or_error "tcp" TCP.connect ip >>= fun tcp -> Lwt.return (n, e, a, ip, tcp)
  in
  let arps = Lwt_list.map_p
      (fun ip -> 
         get_arp ~backend ~root ~node:(name_repo ip) () >>= 
         start_ip ip >>= spawn_arp_listener >>= start_tcp
      ) ips
  in
  arps

let servers ~backend = 
  let echo flow =
    let ignore_errors fn = function
      | `Ok q -> fn q
      | `Error _ | `Eof -> Lwt.return_unit
    in
    (* try echoing, but don't really mind if we fail *)
    TCP.read flow >>= ignore_errors (fun buf -> 
        TCP.write flow buf >>= fun _ -> Lwt.return_unit
      )
  in
  let start_server ~root ~node ~ip =
    get_arp ~backend ~root ~node () 
    >>= start_ip ip
    >>= start_tcp_listener ~port:echo_port ~fn:echo
  in
  start_server ~root ~node:"server_1" ~ip:server_1_ip >>= fun s1 ->
  start_server ~root ~node:"server_2" ~ip:server_2_ip >>= fun s2 ->
  Lwt.return (s1, s2)

let converse (_, _, _, client_ip, client_tcp) (_, _, _, server_ip, _, _) =
  (* every second, bother the other end and see whether they have anything to
     say back to us *)
  let important_content = Cstruct.of_string "hi I love you I missed you" in
  let rec pester flow =
    TCP.write flow important_content >>= fun _ -> 
    TCP.read flow >>= fun _ -> 
    OS.Time.sleep 1.0 >>= fun () -> pester flow
  in
  let dest = List.hd (IPV4.get_ip server_ip) in
  let src = List.hd (IPV4.get_ip client_ip) in
  Printf.printf "trying connection from %s to %s on port %d\n%!"
    (Ipaddr.V4.to_string src) (Ipaddr.V4.to_string dest) echo_port;

  TCP.create_connection client_tcp (List.hd (IPV4.get_ip server_ip), echo_port)
  >>= function
  | `Error _ -> Lwt.fail (failwith "couldn't establish connection between client and server")
  | `Ok flow -> pester flow

let ok_go () =
  let buffer = MProf_unix.mmap_buffer ~size:1000000 "demo_network_trace.ctf" in
  let trace_config = MProf.Trace.Control.make buffer MProf_unix.timestamper in
  MProf.Trace.Control.start trace_config;
  let backend = B.create ~yield:(fun () -> Lwt_main.yield ()) ~use_async_readers:true () in
  let get_listener (_, _, _, _, _, listener) = listener in
  servers ~backend >>= fun (s1, s2) ->
  (* last entry of servers is a listener for inclusion in Lwt.join *)
  clients ~backend >>= fun client_list ->
  (* clients are now up and running ARP listeners *)
  Lwt.choose [
    get_listener s1;
    get_listener s2;
    converse (List.hd client_list) s1 
  ] >>= fun _ -> Lwt.return_unit

let () = 
  Lwt_main.run (ok_go ())
