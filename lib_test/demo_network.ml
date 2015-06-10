open Common
open Lwt.Infix

module IPV4 = Ipv4.Make(E)(A_fs)
module TCP = Tcp.Flow.Make(IPV4)(OS.Time)(Clock)(Random)

let netmask = Ipaddr.V4.of_string_exn "255.255.255.128"

let server_1_ip = Ipaddr.V4.of_string_exn "192.168.252.1"
let server_2_ip = Ipaddr.V4.of_string_exn "192.168.252.2"

let root = "demo_results"

(* clients are 3..60, inclusive *)
(* clients whose ips end in 0 make requests to 192.168.252.1 *)
(* clients whose ips end in 5 make requests to 192.168.252.2 *)

(* server 1 runs a service on port 443; server 2 runs a service on port 587 *)

let start_ip ip_addr (_c, _b, netif, ethif, arp) =
  IPV4.connect ethif arp >>= function
  | `Error e -> OUnit.assert_failure (Printf.sprintf "error starting ip %s"
                                        (Ipaddr.V4.to_string ip_addr))
  | `Ok i ->
    IPV4.set_ip i ip_addr >>= fun () -> IPV4.set_ip_netmask i netmask >>= fun () ->
    Lwt.return (netif, ethif, arp, i)

let start_tcp_listener ~port ~fn (netif, ethif, arp, ip) =
  let chooser = function | n when n = port -> Some fn | _ -> None in
  (* make tcp *)
  or_error "tcp" TCP.connect ip >>= fun tcp ->
  Lwt.async (fun () -> V.listen netif (E.input 
                    ~ipv6:(fun buf -> Lwt.return_unit)
                    ~arpv4:(fun buf -> A_fs.input arp buf)
                    ~ipv4:(
                      IPV4.input 
                        ~tcp:(TCP.input tcp ~listeners:chooser)
                        ~udp:(fun ~src ~dst _buf -> Lwt.return_unit)
                        ~default:(fun ~proto ~src ~dst _ -> Lwt.return_unit)
                        ip
                    )
                    ethif
                                      ));
  Lwt.return (netif, ethif, arp, ip, tcp)

let clients ~backend = 
  let rec intlist s e acc = 
    if s = e then List.rev (s::acc) else intlist (s+1) e (s::acc) in
  let base = Ipaddr.V4.to_int32 (Ipaddr.V4.of_string_exn "192.168.252.0") in
  let ips = List.map 
      (fun number -> Ipaddr.V4.of_int32 (Int32.add base (Int32.of_int number))) (intlist 3 60 []) in
  let name_repo ip = Printf.sprintf "%s/client_%s" root (Ipaddr.V4.to_string ip) in
  let arps = List.map 
      (fun ip -> 
         get_arp ~backend ~root:(name_repo ip) () >>= start_ip ip
      ) ips
  in
  arps

let servers ~backend = 
  let ignore_errors fn = function
    | `Ok q -> fn q
    | `Error e -> Lwt.return_unit
    | `Eof -> Lwt.return_unit
  in
  let s1_listener flow =
    (* try echoing, but don't really mind if we fail *)
    TCP.read flow >>= ignore_errors (fun buf -> 
        TCP.write flow buf >>= fun _ -> Lwt.return_unit
      )
  in
  let s1 = 
    get_arp ~backend ~root:(root ^ "/server_1") () >>= start_ip server_1_ip
  in
  let s2 = 
    get_arp ~backend ~root:(root ^ "/server_2") () >>= start_ip server_2_ip 
  in
  (s1, s2)

let () =
  let backend = B.create () in
  let arps = clients ~backend in
  ()
