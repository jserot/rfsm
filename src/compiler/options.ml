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

type target = Dot | Sim | CTask | SystemC | Vhdl 
let main_prefix = ref "main"
let target = ref None
let use_old_syntax = ref false
let transl_syntax = ref false
let target_dir = ref "."
let main_name = ref ""
let print_version = ref false
let do_run = ref false
let dump_static = ref false
let dot_captions = ref true

let set_main_prefix p = main_prefix := p
let set_old_syntax () = use_old_syntax := true
let set_transl_syntax () = use_old_syntax := true; transl_syntax := true
let set_sim () = target := Some Sim
let set_dot () = target := Some Dot
let set_ctask () = target := Some CTask
let set_systemc () = target := Some SystemC
let set_vhdl () = target := Some Vhdl
let set_print_version () = print_version := true
let set_dump_static () = dump_static := true
let set_lib_dir d =
  Systemc.cfg.Systemc.sc_lib_dir <- d;
  Vhdl.cfg.Vhdl.vhdl_lib_dir <- d
let set_target_dir name = target_dir := name
let set_main_name name = main_name := name

let set_systemc_time_unit u = Systemc.cfg.Systemc.sc_time_unit <- u

let set_vhdl_time_unit u = Vhdl.cfg.Vhdl.vhdl_time_unit <- u
let set_vhdl_ev_duration d = Vhdl.cfg.Vhdl.vhdl_ev_duration <- d
let set_vhdl_rst_duration d = Vhdl.cfg.Vhdl.vhdl_reset_duration <- d
let set_vhdl_use_numeric_std () = Vhdl.cfg.Vhdl.vhdl_use_numeric_std <- true
let set_vhdl_bool_as_bool () = Vhdl.cfg.Vhdl.vhdl_bool_as_bool <- true

let set_stop_time d =
  Systemc.cfg.Systemc.sc_stop_time <- d;
  Vhdl.cfg.Vhdl.vhdl_stop_time <- d

let set_synchronous_actions () = Fsm.cfg.Fsm.act_sem <- Fsm.Synchronous
let set_sequential_actions () = Fsm.cfg.Fsm.act_sem <- Fsm.Sequential
let set_trace level = Trace.level := level
let set_sc_trace () = Systemc.cfg.Systemc.sc_trace <- true
let set_sc_double_float () = Systemc.cfg.Systemc.sc_double_float <- true
let set_vhdl_trace () = Vhdl.cfg.Vhdl.vhdl_trace <- true
let set_vhdl_dump_ghw () = Vhdl.cfg.Vhdl.vhdl_dump_format <- Vhdl.Ghw

let set_dot_no_captions () = dot_captions := false
let set_dot_actions_nl () = Fsm.cfg.Fsm.act_sep <- "\\n"
