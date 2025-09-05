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

(**{1 Socket-based server} *)

val start: socket:string -> fn:(in_channel -> out_channel -> unit) -> unit
(** Start the service function [fn], reading input and writing output on Unix socket [socket]  *)
