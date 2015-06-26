module Arp : sig
  type arp = {
    op: [ `Request |`Reply |`Unknown of int ];
    sha: Macaddr.t;
    spa: Ipaddr.V4.t;
    tha: Macaddr.t;
    tpa: Ipaddr.V4.t;
  }
  module Parse : sig
    val garp : Macaddr.t -> Ipaddr.V4.t -> arp
    val is_garp_for : Ipaddr.V4.t -> Cstruct.t -> bool
    val cstruct_of_arp : arp -> Cstruct.t
    val arp_of_cstruct : Cstruct.t -> [ `Ok of arp
                                      | `Too_short
                                      | `Unusable
                                      | `Bad_mac of string list ]
  end
  module Make (Ethif : V1_LWT.ETHIF) (Clock: V1.CLOCK) (Time: V1_LWT.TIME) 
      (Maker : Irmin.S_MAKER) :
  sig
    include V1_LWT.ARP
    val connect : Ethif.t -> Irmin.config -> string list -> [> `Ok of t | `Error of error ] io
  end
end
