(* make an in-memory new bare irmin thing *)
(* make a simple tree in it [old] *)
(* clone [old]; modify it to get [new1] *)
(* clone [old]; modify it to get [new2] *)
(* merge [new1, new2] on [old] *)

let main () =
  let store = Irmin.basic (module Irmin_mem.Make) (module Irmin_arp.Table) in
  let config = Irmin_mem.config ~bare:true () in
  Irmin.create store config task >>= fun t ->
  return_unit

let () =
  Lwt_unix.run (main ())
