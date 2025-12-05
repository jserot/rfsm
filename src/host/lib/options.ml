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
(* let gui = ref false *)
let server_mode = ref false
let verbose = ref false
let socket_port = ref 12345

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
(* let set_gui () = gui := true *)
let set_server_mode () = server_mode := true
let set_verbose () = verbose := true
let set_socket_port path = socket_port := path

let set_synchronous_actions () =
  Dynamic.cfg.act_semantics <- Misc.Synchronous;
  Systemc.cfg.sc_act_semantics <- Misc.Synchronous;
  Vhdl.cfg.vhdl_act_semantics <- Misc.Synchronous
let set_sequential_actions () =
  Dynamic.cfg.act_semantics <- Misc.Sequential;
  Systemc.cfg.sc_act_semantics <- Misc.Sequential;
  Vhdl.cfg.vhdl_act_semantics <- Misc.Sequential
let set_sim_trace level = Dynamic.cfg.verbose_level <- level

let set_dot_captions () = Dot.cfg.show_captions <- true
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


let spec = [
"-main", Arg.String set_main_prefix, "set prefix for the generated main files";
"-dump_typed", Arg.Unit set_dump_typed, "dump typed representation of model(s)/program to stdout";
"-dump_static", Arg.Unit set_dump_static, "dump static representation of model(s)/program to stdout";
"-target_dir", Arg.String set_target_dir, "set target directory (default: .)";
"-lib", Arg.String set_lib_dir, "set location of the support library (default: <opam_prefix>/share/rfsm)";
"-dot", Arg.Unit set_dot, "generate .dot representation of model(s)/program";
"-sim", Arg.Unit set_sim, "run simulation (generating .vcd file)";
"-ctask", Arg.Unit set_ctask, "generate CTask code";
"-systemc", Arg.Unit set_systemc, "generate SystemC code";
"-vhdl", Arg.Unit set_vhdl, "generate VHDL code";
"-version", Arg.Unit set_print_version, "print version of the compiler and quit";
"-show_models", Arg.Unit set_show_models, "generate separate representations for uninstanciated FSM models ";
"-server_mode", Arg.Unit set_server_mode, "run the compiler in server mode (to be used by the grasp app)";
"-verbose", Arg.Unit set_verbose, "run the compiler in verbose mode (including server)";
"-socket_port", Arg.Int set_socket_port, "set socket port when running in server mode";
"-dot_qual_ids", Arg.Unit set_dot_qual_ids, "print qualified identifiers in DOT representations";
"-dot_captions", Arg.Unit set_dot_captions, "Add captions in .dot representation(s)";
"-dot_short_trans", Arg.Unit set_dot_short_trans, "Print single-line transition labels (default is multi-lines)";
"-dot_abbrev_types", Arg.Unit set_dot_abbrev_types, "Print abbreviated types (default is to print definitions)";
"-dot_boxed", Arg.Unit set_dot_boxed, "Draw FSM instances in boxes";
"-sim_trace", Arg.Int set_sim_trace, "set trace level for simulation (default: 0)";
"-vcd_int_size", Arg.Int set_vcd_default_int_size, "set default int size for VCD traces (default: 8)";
"-synchronous_actions", Arg.Unit set_synchronous_actions, "interpret actions synchronously";
"-sc_time_unit", Arg.String set_systemc_time_unit, "set time unit for the SystemC test-bench (default: SC_NS)";
"-sc_trace", Arg.Unit set_sc_trace, "set trace mode for SystemC backend (default: false)";
"-stop_time", Arg.Int set_stop_time, "set stop time for the SystemC and VHDL test-bench (default: 100)";
"-sc_double_float", Arg.Unit set_sc_double_float, "implement float type as C++ double instead of float (default: false)";
"-vhdl_trace", Arg.Unit set_vhdl_trace, "set trace mode for VHDL backend (default: false)";
"-vhdl_time_unit", Arg.String set_vhdl_time_unit, "set time unit for the VHDL test-bench";
"-vhdl_ev_duration", Arg.Int set_vhdl_ev_duration, "set duration of event signals (default: 1 ns)";
"-vhdl_rst_duration", Arg.Int set_vhdl_ev_duration, "set duration of reset signals (default: 1 ns)";
"-vhdl_numeric_std", Arg.Unit set_vhdl_use_numeric_std, "translate integers as numeric_std [un]signed (default: false)";
"-vhdl_bool_as_bool", Arg.Unit set_vhdl_bool_as_bool, "translate all booleans as boolean (default: false)";
"-vhdl_dump_ghw", Arg.Unit set_vhdl_dump_ghw, "make GHDL generate trace files in .ghw format instead of .vcd";
];
