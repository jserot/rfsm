(**********************************************************************)
(*                                                                    *)
(*              This file is part of the RFSM package                 *)
(*                                                                    *)
(*  Copyright (c) 2018-present, Jocelyn SEROT.  All rights reserved.  *)
(*                                                                    *)
(*  This source code is licensed under the license found in the       *)
(*  LICENSE file in the root directory of this source tree.           *)
(*                                                                    *)
(**********************************************************************)

(** Events and stimuli *)

type event = Types.date * Expr.value
  (** Single event at time [t]. Value is [Val_none] for pure events *)

type stimuli = Types.date * (Ident.t * Expr.value) list
  (** Set of named events at time [t] *)

val mk_spor_event : Types.date list -> (Types.date * Expr.value) list
val mk_per_event : Types.date -> Types.date -> Types.date -> (Types.date * Expr.value) list
val mk_val_changes : (Types.date * Expr.value) list -> (Types.date * Expr.value) list

val mk_stimuli : string -> Types.date * Expr.value -> Types.date * (Ident.t * Expr.value) list

val merge_stimuli : stimuli list list -> stimuli list

val events_of : Global.stim_desc -> event list
  
(** {2 Printers} *)

val string_of_event : event -> string
val string_of_events : event list -> string
val string_of_stimuli : stimuli -> string 
