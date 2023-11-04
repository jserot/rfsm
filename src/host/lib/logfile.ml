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

(**{1 Compiler log files} *)

let fname = "rfsm.output"
let channel = ref None

let stop () = match !channel with
  Some oc -> close_out oc
| None -> ()
  
let start () = 
  stop ();
  let oc = open_out fname in
  channel := Some oc

let write fname =
    Printf.printf "Wrote file %s\n" fname;
    match !channel with
      Some oc -> Printf.fprintf oc "%s\n" fname
    | None -> ()
