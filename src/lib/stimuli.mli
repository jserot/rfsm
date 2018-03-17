(** Events and stimuli *)

type event = Types.date * Expr.value option
  (** Single event at time [t]. Value is [None] for pure events *)

type stimuli = Types.date * (Ident.t * Expr.value option) list
  (** Set of named events at time [t] *)

val mk_spor_event : Types.date list -> (Types.date * Expr.value option) list
val mk_per_event : Types.date -> Types.date -> Types.date -> (Types.date * Expr.value option) list
val mk_val_changes : (Types.date * Expr.value) list -> (Types.date * Expr.value option) list

val mk_stimuli : string -> Types.date * Expr.value option -> Types.date * (Ident.t * Expr.value option) list

val merge_stimuli : stimuli list list -> stimuli list

(** {2 Printers} *)

val string_of_event : event -> string
val string_of_events : event list -> string
val string_of_stimuli : stimuli -> string 

                                                           
(* val string_of_event : int * Expr.value option -> string
 * val string_of_events : (int * Expr.value option) list -> string
 * val string_of_stimuli : int * (Ident.t * Expr.value option) list -> string *)
