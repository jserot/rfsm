type 'a t = (Ident.t * 'a) list

val apply: 'a t -> Ident.t -> 'a

val pp: (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit   (* For debug *)
