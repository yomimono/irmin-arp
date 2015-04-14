(* provide Tc.S0 stuff for Ipaddr.V4.t *)
include Ipaddr.V4

let read buf = (Ipaddr.V4.of_string_exn (Mstruct.to_string buf))
let write k buf = 
  let s  = Ipaddr.V4.to_string k in
  Cstruct.blit_from_string s 0 buf 0 (String.length s);
  Cstruct.sub buf (String.length s) (Cstruct.len buf - (String.length s))
let size_of k = String.length (Ipaddr.V4.to_string k)
let to_json k = Ezjsonm.string (Ipaddr.V4.to_string k)
let of_json = function
  | `String s -> Ipaddr.V4.of_string_exn s
  | `Null | `Bool _ | `O _ | `A _ | `Float _ -> 
    raise (Tc.Read_error "invalid json")
let hash = Hashtbl.hash
let equal p q = (Ipaddr.V4.compare p q) = 0
