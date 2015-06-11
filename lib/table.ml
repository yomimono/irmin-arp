module Make(P: Irmin.Path.S) = struct 
  module Path = P
  module Ops = struct
    module M = Map.Make(Ipaddr.V4)
    type t = Entry.t M.t (* map from ip -> entry *)

    let hash = Hashtbl.hash
    let compare = M.compare (Entry.compare)
    let equal p q = (compare p q) = 0
    let of_json json = 
      let top_dict = Ezjsonm.get_dict json in
      (* members of top_dict are (name: entry) pairs *)
      let entries = List.map 
          (fun (name, entry) -> (name, Entry.of_json entry)) top_dict
      in
      let mapify map (name, entry) = 
        match (Ipaddr.V4.of_string name), entry with 
        | Some addr, Some entry -> M.add addr entry map
        | _, _ -> map
      in
      List.fold_left mapify M.empty entries

    let to_json map = 
      let add_binding key value json =
        try
          Ezjsonm.update json [(Ipaddr.V4.to_string key)] (Some (Entry.to_json value) )
        with
        | Not_found -> raise (Invalid_argument (Printf.sprintf 
                                                  "Couldn't make json out of key %s and entry %s" 
                                                  (Ipaddr.V4.to_string key) (Entry.to_string value)))
      in
      M.fold add_binding map (Ezjsonm.dict [])

    let read buf = 
      let str = Mstruct.to_string buf in
      let json = 
        try 
          Ezjsonm.unwrap (Ezjsonm.from_string str)
        with Ezjsonm.Parse_error _ -> raise (Tc.Read_error "invalid json")
      in
      of_json json

    (* no regrets *)
    (* why is there no signalling like "uh bro I can't fit in here"? *)
    let write m buf = 
      let s = Ezjsonm.to_string (Ezjsonm.wrap (to_json m)) in
      let to_blit = String.length s in
      Cstruct.blit_from_string s 0 buf 0 to_blit;
      Cstruct.sub buf to_blit ((Cstruct.len buf) - to_blit)

    let size_of m = String.length (Ezjsonm.to_string (Ezjsonm.wrap (to_json m)))
  end
  include Ops

  let to_map t = t
  let of_map = to_map
  let add = M.add
  let remove = M.remove
  let find = M.find
  let empty = M.empty
  let expire t now =
    M.filter (fun _ip entry -> match entry with
        | Entry.Confirmed (time, _) ->
          Printf.printf "time %f > current time %f: %s\n%!" time now
            (string_of_bool (time > now));
          time > now) t

  let merge _path ~(old : Entry.t M.t Irmin.Merge.promise) t1 t2 = 
    let open Irmin.Merge.OP in
    old () >>| fun old -> 
    (* TODO: it would be nicer to only wait on the computation of the LCA in the
       case where we actually need it to resolve a merge conflict *)
    let old = match old with None -> M.empty | Some old -> old in
    let merge_maps key val1 val2 =
      let comp_of_operation ~direction key present =
        let operation key new_value =
          match M.mem key old with
          | false -> `Added
          | true -> 
            let old_value = M.find key old in
            if new_value = old_value then `Unchanged else `Modified
        in
        let multiplier = match direction with | `Left -> 1 | `Right -> -1 in
        match operation key present with
        | `Added -> multiplier * 1 (* element added in t1, keep it *)
        | `Unchanged -> multiplier * -1 (* element removed by t2, remove it *)
        | `Modified -> multiplier * 1 (* element modified by t1 and removed by t2.  keep the
                                         modified value *)
      in
      let opt_compare v1 v2 =
        match v1, v2 with
        | None, None -> 0
        | Some v1, Some v2 -> Entry.compare v1 v2
        | Some present, None -> comp_of_operation ~direction:`Left key present
        | None, Some present -> comp_of_operation ~direction:`Right key present
      in
      if (opt_compare val1 val2) < 0 then val2 else val1
    in
    Irmin.Merge.OP.ok (M.merge merge_maps t1 t2)

  let merge path = Irmin.Merge.option (module Ops) (merge path)

end

