type scope = Local | Global
                   
type t = {
    scope: scope;
    id: string
    }

val mk: ?scope:scope -> string -> t

val mk_global: t -> t
val mk_local: t -> t

val upd_id: (string -> string) -> t -> t

val pp: Format.formatter -> t -> unit
val pp_qual: Format.formatter -> t -> unit

val to_string: t -> string

val compare: t -> t -> int
