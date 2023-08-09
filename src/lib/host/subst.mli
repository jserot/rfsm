type 'a t = (Ident.t * 'a) list

val apply: 'a t -> Ident.t -> 'a
