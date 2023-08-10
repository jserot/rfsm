type 'a t = (Ident.t * 'a) list

let apply phi id = List.assoc id phi

let pp pp_v fmt phi = Misc.pp_assoc (Ident.pp,pp_v) fmt phi (** For debug only *)
