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

(** Compiler options *)

type target = Dot | Sim | CTask | SystemC | Vhdl 
let main_prefix = ref "main"
let target = ref None
let use_old_syntax = ref false
let target_dir = ref "."
let main_name = ref ""
let print_version = ref false
let do_run = ref false
let dump_tenv = ref false
let dump_typed = ref false
let dump_static = ref false
let dot_abbrev_types = ref false
let normalize = ref false
let dump_backtrace = ref false
let gui = ref false

let set_main_prefix p = main_prefix := p
let set_old_syntax () = use_old_syntax := true
let set_sim () = target := Some Sim
let set_dot () = target := Some Dot
let set_ctask () = target := Some CTask
let set_systemc () = target := Some SystemC
let set_vhdl () = target := Some Vhdl
let set_print_version () = print_version := true
let set_dump_typed () = dump_typed := true
let set_dump_tenv () = dump_tenv := true
let set_dump_static () = dump_static := true
let set_target_dir name = target_dir := name
let set_main_name name = main_name := name
let set_normalize () = normalize := true
let set_gui () = gui := true

let set_synchronous_actions () =
  Dynamic.cfg.act_semantics <- Misc.Synchronous;
  Systemc.cfg.sc_act_semantics <- Misc.Synchronous;
  Vhdl.cfg.vhdl_act_semantics <- Misc.Synchronous
let set_sequential_actions () =
  Dynamic.cfg.act_semantics <- Misc.Sequential;
  Systemc.cfg.sc_act_semantics <- Misc.Sequential;
  Vhdl.cfg.vhdl_act_semantics <- Misc.Sequential
let set_sim_trace level = Dynamic.cfg.verbose_level <- level

let set_dot_no_captions () = Dot.cfg.show_captions <- false
let set_dot_short_trans () =  Dot.cfg.Dot.trans_vlayout <- false
let set_show_models () =
  Dot.cfg.show_models <- true;
  Ctask.cfg.show_models <- true;
  Systemc.cfg.show_models <- true
let set_dot_abbrev_types () =  Dot.cfg.Dot.abbrev_types <- true
let set_dot_qual_ids () = Dot.cfg.Dot.qual_ids <- true
let set_dot_boxed () = Dot.cfg.Dot.boxed <- true

let set_vcd_default_int_size s = Vcd.cfg.Vcd.default_int_size <- s

let set_lib_dir d =
  Systemc.cfg.Systemc.sc_lib_dir <- d;
  Vhdl.cfg.Vhdl.vhdl_lib_dir <- d

let set_stop_time d =
  Systemc.cfg.Systemc.sc_stop_time <- d;
  Vhdl.cfg.Vhdl.vhdl_stop_time <- d

let set_systemc_time_unit u = Systemc.cfg.Systemc.sc_time_unit <- u
let set_sc_trace () = Systemc.cfg.Systemc.sc_trace <- true
let set_sc_double_float () = Systemc.cfg.Systemc.sc_double_float <- true

let set_vhdl_time_unit u = Vhdl.cfg.Vhdl.vhdl_time_unit <- u
let set_vhdl_ev_duration d = Vhdl.cfg.Vhdl.vhdl_ev_duration <- d
let set_vhdl_rst_duration d = Vhdl.cfg.Vhdl.vhdl_reset_duration <- d
let set_vhdl_use_numeric_std () = Vhdl_types.cfg.vhdl_use_numeric_std <- true
let set_vhdl_bool_as_bool () = Vhdl_types.cfg.vhdl_bool_as_bool <- true
let set_vhdl_trace () = Vhdl.cfg.Vhdl.vhdl_trace <- true
let set_vhdl_dump_ghw () = Vhdl.cfg.Vhdl.vhdl_dump_format <- Vhdl.Ghw

