type 'a t

val empty: 'a t

val is_empty: 'a t -> bool

val dom: 'a t -> Ident.t list

val bindings: 'a t -> (Ident.t * 'a) list

val init: (Ident.t * 'a) list -> 'a t
  
val add : Ident.t -> 'a -> 'a t -> 'a t

val union: 'a t -> 'a t -> 'a t 

val mem : Ident.t -> 'a t -> bool

val find : Ident.t -> 'a t -> 'a

val upd : Ident.t -> 'a -> 'a t -> 'a t

val filter : (Ident.t -> 'a -> bool) -> 'a t -> 'a t

val fold : (Ident.t -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val iter : (Ident.t -> 'a -> unit) -> 'a t -> unit

val map : ('a -> 'b) -> 'a t -> 'b t

val pp: ?sep:string -> ?vlayout:bool -> ?qual:bool -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

val pp_dom: pp_ident:(Format.formatter -> Ident.t -> unit) -> Format.formatter -> 'a t -> unit
