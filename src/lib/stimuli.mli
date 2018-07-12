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

type event = Types.date * Expr.e_val
  (** Single event at time [t]. Value is [Val_none] for pure events *)

type stimuli = Types.date * (Ident.t * Expr.e_val) list
  (** Set of named events at time [t] *)

val mk_spor_event : Types.date list -> (Types.date * Expr.e_val) list
val mk_per_event : Types.date -> Types.date -> Types.date -> (Types.date * Expr.e_val) list
val mk_val_changes : (Types.date * Expr.e_val) list -> (Types.date * Expr.e_val) list

val mk_stimuli : string -> Types.date * Expr.e_val -> Types.date * (Ident.t * Expr.e_val) list

val merge_stimuli : stimuli list list -> stimuli list

(** {2 Printers} *)

val string_of_event : event -> string
val string_of_events : event list -> string
val string_of_stimuli : stimuli -> string 
