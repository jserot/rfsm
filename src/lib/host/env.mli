type ident = string

type 'a t

val empty: 'a t

val is_empty: 'a t -> bool

val dom: 'a t -> string list

val bindings: 'a t -> (string * 'a) list

val init: (string * 'a) list -> 'a t
  
val add : ident -> 'a -> 'a t -> 'a t

val union: 'a t -> 'a t -> 'a t 

val mem : ident -> 'a t -> bool

val find : ident -> 'a t -> 'a

val upd : ident -> 'a -> 'a t -> 'a t

val filter : (string -> 'a -> bool) -> 'a t -> 'a t

val fold : (string -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val pp: ?sep:string -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
