type 'a t = (Ident.t * 'a) list

let apply phi id = List.assoc id phi
