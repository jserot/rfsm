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

(* VHDL backend *)

type dump_format = Vcd | Ghw

type vhdl_config = {
  mutable vhdl_lib_name: string;
  mutable vhdl_lib_dir: string;
  mutable vhdl_inpmod_prefix: string;
  mutable vhdl_tb_prefix: string;
  mutable vhdl_globals_name: string;
  mutable vhdl_state_var: string;
  mutable vhdl_stop_time: int;
  mutable vhdl_time_unit: string;
  mutable vhdl_reset_sig: string;
  mutable vhdl_reset_duration: int;
  mutable vhdl_ev_duration: int;
  mutable vhdl_bool_as_bool: bool;
  mutable vhdl_support_package: string;
  mutable vhdl_trace: bool;
  mutable vhdl_dump_format: dump_format;
  mutable vhdl_trace_state_var: string;
  mutable vhdl_act_semantics: Misc.act_semantics;
  }

let cfg = {
  vhdl_lib_name = "rfsm";
  vhdl_lib_dir = ".";
  vhdl_inpmod_prefix = "inp_";
  vhdl_tb_prefix = "main";
  vhdl_globals_name = "globals";
  vhdl_state_var = "state";
  vhdl_stop_time = 100;
  vhdl_reset_sig = "rst";
  vhdl_time_unit = "ns";
  vhdl_reset_duration = 1;
  vhdl_ev_duration = 1;
  vhdl_bool_as_bool = false;
  vhdl_support_package = "rfsm";
  vhdl_trace = false;
  vhdl_dump_format = Vcd;
  vhdl_trace_state_var = "st";
  vhdl_act_semantics = Misc.Sequential;
  }

module type VHDL = sig
  
  module Static: Static.T
  module G: Guest.VHDL

  exception Invalid_output_assign of string * Location.t

  val output: dir:string -> ?pfx:string -> Static.t -> string list 
end

module Make (Static: Static.T)
            (Guest: Guest.VHDL with module Syntax = Static.Syntax.Guest and type value = Static.Value.t)
       : VHDL with module Static = Static =
struct

  module Static = Static
  module G = Guest

  module Cmodel = Cmodel.Make(Static)

  exception Invalid_output_assign of string * Location.t
                   
  let output_files = ref ([] : string list)

  open Format

  let need_globals s = s.Static.types <> [] || s.Static.fns <> [] || s.Static.csts <> [] 

  let pp_ident = Ident.pp

  let vhdl_type_of t = 
    match t with
    | Some t -> G.vhdl_type_of t
    | None -> Misc.fatal_error "Host.Vhdl.vhdl_type_of_opt"
               
  let pp_action fmt tab m a =
    let open Static in
    let open Cmodel in
    let asn id =
      if List.mem_assoc id m.c_vars && cfg.vhdl_act_semantics = Misc.Sequential then ":=" else "<=" in
    let pp fmt a = match a.Annot.desc  with
      | Syntax.Emit id ->
         fprintf fmt "notify_ev(%a,%d %s)" Ident.pp id cfg.vhdl_ev_duration cfg.vhdl_time_unit
      | Syntax.Assign (lhs, expr) ->
         let id = G.Syntax.lhs_base_name lhs in
         let pp_expr = G.pp_expr in
         let pp_lhs = G.pp_lhs in
         if G.Syntax.is_simple_lhs lhs then
           fprintf fmt "%a %s %a" Ident.pp id (asn id) pp_expr expr
         else (* Non-scalar LHS *)
           begin
             if List.mem_assoc id m.c_vars
             then fprintf fmt "%a %s %a" pp_lhs lhs (asn id) pp_expr expr
             else raise (Invalid_output_assign (Ident.to_string id, a.Annot.loc))
           end in
    fprintf fmt "%s%a;\n" tab pp a

  let pp_transition fmt tab src clk m (is_first,needs_endif) (_,{Annot.desc=_,guards;_},acts,q',_) =
    match guards with
    | [] ->
       List.iter (pp_action fmt tab m) acts;
       fprintf fmt "%s%s <= %a;\n" tab cfg.vhdl_state_var Ident.pp q';
       (false,false)
    | guards -> 
       fprintf fmt "%s%s ( %a ) then\n"
         tab
         (if is_first then "if" else "elsif ")
         (Misc.pp_list_h ~sep:" and " G.pp_expr) guards;
       List.iter (pp_action fmt (tab^"  ") m) acts;
       if q' <> src then fprintf fmt "%s  %s <= %a;\n" tab cfg.vhdl_state_var Ident.pp q';
       (false,true)

  let pp_sync_transitions fmt src after clk m ts =
    let tab = "        " in
    let (_,needs_endif) =
      List.fold_left
        (pp_transition fmt tab src clk m)
        (true,false)
        ts in
    if needs_endif then Format.fprintf fmt "        end if;\n"
    
  let pp_state fmt clk m Cmodel.{ st_src=q; st_sensibility_list=evs; st_transitions=tss } =
    match tss with
      [ev,ts] -> pp_sync_transitions fmt q false clk m ts
    | [] -> Misc.fatal_error ("VHDL: state " ^ Ident.to_string q ^ " has no output transition")
    | _ -> Misc.not_implemented "VHDL: transitions involving multiple events"

  let pp_state_case fmt clk m c =
    Format.fprintf fmt "      when %a =>\n" pp_ident c.Cmodel.st_src;
    pp_state fmt clk m c

  let pp_output_valuation fmt (o,e) = 
    fprintf fmt "      %a <= %a;\n" Ident.pp o G.pp_expr e

  let pp_state_outputs fmt (s,ovs) = 
    Format.fprintf fmt "    when %a =>\n" Ident.pp s;
    List.iter (pp_output_valuation fmt) ovs

  let pp_abbr_type fmt t = G.pp_typ ~type_mark:Vhdl_types.TM_Abbr fmt t
  let pp_abbr_type_expr fmt te = match te.Annot.typ with
    | Some t -> pp_abbr_type fmt t
    | None -> Misc.fatal_error "Host.Vhdl.pp_abbr_type_expr"

  let pp_typed_symbol fmt (id,t) =
    fprintf fmt "%a: %a" pp_ident id pp_abbr_type_expr t
    
  let dump_module_arch ocf m =
    let open Cmodel in
    let open Format in
    let modname = String.capitalize_ascii (Ident.to_string m.Cmodel.c_name) in
    let is_event_type ty = Static.Syntax.Guest.is_event_type ty in
    let pp_state_id fmt (q,_) = Format.fprintf fmt "%a" pp_ident q in
    let clk_sig = match List.filter (function (_, ty) when is_event_type ty -> true | _ -> false) m.c_inps with
        [] -> Misc.fatal_error "Vhdl.dump_module_arch: no event input for model" (* Should not happen *)
      | [h,_] -> Ident.to_string h
      | _ -> Misc.not_implemented (Ident.to_string m.c_name ^ ": translation to VHDL of FSM with more than one input events") in
    fprintf ocf "architecture RTL of %s is\n" modname;
    fprintf ocf "  type t_%s is ( %a );\n" cfg.vhdl_state_var (Misc.pp_list_h ~sep:", " pp_state_id) m.c_states;
    fprintf ocf "  signal %s: t_state;\n" cfg.vhdl_state_var;
    if cfg.vhdl_act_semantics = Misc.Synchronous then 
      List.iter
        (fun (id,ty) -> fprintf ocf "  signal %a: %a;\n" pp_ident id pp_abbr_type_expr ty)
        m.c_vars;
    fprintf ocf "begin\n";
    fprintf ocf "  process(%s, %s)\n" cfg.vhdl_reset_sig clk_sig;
    if cfg.vhdl_act_semantics = Misc.Sequential then 
      List.iter
        (fun (id,ty) -> fprintf ocf "  variable %a: %a;\n" pp_ident id pp_abbr_type_expr ty)
        m.c_vars;
    fprintf ocf "  begin\n";
    fprintf ocf "    if ( %s='1' ) then\n" cfg.vhdl_reset_sig;
    fprintf ocf "      %s <= %a;\n" cfg.vhdl_state_var pp_ident (fst m.c_init);
    List.iter (pp_action ocf "      " m) (snd m.c_init);
    fprintf ocf "    elsif rising_edge(%s) then \n" clk_sig;
    begin match m.c_body with
      [] -> () (* should not happen *)
    | [q] -> pp_state ocf clk_sig m q 
    | qs -> 
       fprintf ocf "      case %s is\n" cfg.vhdl_state_var;
       List.iter (pp_state_case ocf clk_sig m) m.c_body;
       fprintf ocf "    end case;\n"
    end;
    fprintf ocf "    end if;\n";
    fprintf ocf "  end process;\n";
    if List.exists (fun (s,ovs) -> ovs <> []) m.c_states then begin
        fprintf ocf "  process(%s)\n" cfg.vhdl_state_var;
        fprintf ocf "  begin\n";
        fprintf ocf "    case %s is\n" cfg.vhdl_state_var;
        List.iter (pp_state_outputs ocf) m.c_states;
        fprintf ocf "    end case;\n";
        fprintf ocf "  end process;\n";
      end;
    if cfg.vhdl_trace then begin
        let pp_state_int fmt (q,i) = Format.fprintf fmt "%d when %s = %a" i cfg.vhdl_state_var pp_ident q in
        fprintf ocf "  %s <= %a;\n"
          cfg.vhdl_trace_state_var
          (Misc.pp_list_h ~sep:" else " pp_state_int) (List.mapi (fun i (s,_) -> s,i) m.Cmodel.c_states)
      end;
    fprintf ocf "end architecture;\n"

  let dump_module_intf kind ocf m = 
    let open Cmodel in
    let modname = String.capitalize_ascii (Ident.to_string m.Cmodel.c_name) in
    let pp_param fmt (id,(ty,v)) = pp_typed_symbol fmt (id,ty) in
    fprintf ocf "%s %s %s\n" kind modname (if kind = "entity" then "is" else "");
    if m.c_consts <> [] then 
      fprintf ocf "  generic (%a);\n" (Misc.pp_list_h ~sep:";" pp_param) m.c_consts;
    fprintf ocf "  port(\n";
    List.iter (fun (id,ty) -> fprintf ocf "        %a: in %a;\n" Ident.pp id pp_abbr_type_expr ty) m.c_inps;
    List.iter (fun (id,ty) -> fprintf ocf "        %a: out %a;\n" Ident.pp id pp_abbr_type_expr ty) m.c_outps;
    List.iter (fun (id,ty) -> fprintf ocf "        %a: inout %a;\n" Ident.pp id pp_abbr_type_expr ty) m.c_inouts;
    fprintf ocf "        %s: in std_logic" cfg.vhdl_reset_sig;
    if cfg.vhdl_trace then fprintf ocf ";\n        %s: out integer\n" cfg.vhdl_trace_state_var else fprintf ocf "\n";
    fprintf ocf "        );\n";
    fprintf ocf "end %s;\n" kind

  (* Dumping input generator processes *)

  let pp_time fmt t = Format.fprintf fmt "%d %s" t cfg.vhdl_time_unit
  
  let dump_sporadic_inp_process ocf id ts =
    fprintf ocf "    type t_dates is array ( 0 to %d ) of time;\n" (List.length ts-1);
    fprintf ocf "    constant dates : t_dates := ( %a );\n" (Misc.pp_list_h ~sep:", " pp_time) ts;
    fprintf ocf "    variable i : natural := 0;\n";
    fprintf ocf "    variable t : time := 0 %s;\n" cfg.vhdl_time_unit;
    fprintf ocf "    begin\n";
    fprintf ocf "      %a <= '0';\n" pp_ident id;
    fprintf ocf "      for i in 0 to %d loop\n" (List.length ts-1);
    fprintf ocf "        wait for dates(i)-t;\n";
    fprintf ocf "        notify_ev(%a,%d %s);\n" pp_ident id cfg.vhdl_ev_duration cfg.vhdl_time_unit;
    fprintf ocf "        t := dates(i);\n";
    fprintf ocf "      end loop;\n";
    fprintf ocf "      wait;\n"
  
  let dump_periodic_inp_process ocf id (p,t1,t2) =
    fprintf ocf "    type t_periodic is record period: time; t1: time; t2: time; end record;\n";
    fprintf ocf "    constant periodic : t_periodic := ( %a, %a, %a );\n" pp_time p pp_time t1 pp_time t2;
    fprintf ocf "    variable t : time := 0 %s;\n" cfg.vhdl_time_unit;
    fprintf ocf "    begin\n";
    fprintf ocf "      %a <= '0';\n" pp_ident id;
    fprintf ocf "      wait for periodic.t1;\n";
    fprintf ocf "      t := t + periodic.t1;\n";
    fprintf ocf "      while ( t < periodic.t2 ) loop\n";
    fprintf ocf "        %a <= '1';\n" pp_ident id;
    fprintf ocf "        wait for periodic.period/2;\n";
    fprintf ocf "        %a <= '0';\n" pp_ident id;
    fprintf ocf "        wait for periodic.period/2;\n";
    fprintf ocf "        t := t + periodic.period;\n";
    fprintf ocf "      end loop;\n";
    fprintf ocf "      wait;\n"
    
  let dump_vc_inp_process ocf ty id vcs =
    let pp_vc fmt (t,v) = fprintf fmt "(%a,%a)" pp_time t G.pp_expr v in
    fprintf ocf "    type t_vc is record date: time; val: %a; end record;\n" pp_abbr_type ty;
    fprintf ocf "    type t_vcs is array ( 0 to %d ) of t_vc;\n" (List.length vcs-1);
    fprintf ocf "    constant vcs : t_vcs := ( %s%a );\n"
      (if List.length vcs = 1 then "others => " else "")  (* GHDL complains when initializing a 1-array *)
      (Misc.pp_list_h ~sep:", " pp_vc) vcs;
    fprintf ocf "    variable i : natural := 0;\n";
    fprintf ocf "    variable t : time := 0 %s;\n" cfg.vhdl_time_unit;
    fprintf ocf "    begin\n";
    fprintf ocf "      for i in 0 to %d loop\n" (List.length vcs-1);
    fprintf ocf "        wait for vcs(i).date-t;\n";
    fprintf ocf "        %a <= vcs(i).val;\n" pp_ident id;
    fprintf ocf "        t := vcs(i).date;\n";
    fprintf ocf "      end loop;\n";
    fprintf ocf "      wait;\n"
  
  let dump_input_process ocf (id,cc) =
    let open Static in
    fprintf ocf "  inp_%a: process\n" pp_ident id;
    begin match cc.Static.ct_stim with
    | Some (Syntax.Sporadic ts) -> dump_sporadic_inp_process ocf id ts
    | Some (Syntax.Periodic (p,t1,t2)) -> dump_periodic_inp_process ocf id (p,t1,t2)
    | Some (Syntax.Value_change []) -> ()
    | Some (Syntax.Value_change vcs) -> dump_vc_inp_process ocf cc.ct_typ id vcs
    | None -> () (* TODO: emit warning ? *)
    end;
    fprintf ocf "  end process;\n"

  (* Dumping toplevel module *)

  let dump_libraries ocf =
    fprintf ocf "library ieee;\n";
    fprintf ocf "use ieee.std_logic_1164.all;	   \n";
    if Vhdl_types.cfg.vhdl_use_numeric_std then fprintf ocf "use ieee.numeric_std.all;\n";
    fprintf ocf "\n"
  
  let dump_toplevel_intf prefix kind ocf s =
    let top_name = prefix ^ "_top" in
    fprintf ocf "%s %s is\n" kind top_name;
    fprintf ocf "  port(\n";
    let open Static in
    List.iter
      (function (id,cc) -> fprintf ocf "        %a: in %a;\n" pp_ident id pp_abbr_type cc.ct_typ)
      s.ctx.inputs;
    List.iter
      (function (id,cc) -> fprintf ocf "        %a: out %a;\n" pp_ident id pp_abbr_type cc.ct_typ)
      s.ctx.outputs;
    if cfg.vhdl_trace then
      List.iter
        (function f -> fprintf ocf "        %s: out integer;\n" (Ident.to_string f.name ^ "_state"))
        s.fsms;  
    fprintf ocf "        %s: in std_logic" cfg.vhdl_reset_sig;
    fprintf ocf "        );\n";
    fprintf ocf "end %s;\n" kind;
    fprintf ocf "\n"
    
  let dump_toplevel_impl prefix fname s =
    let oc,ocf = Misc.open_file fname in
    let top_name = prefix ^ "_top" in
    let open Static in
    let modname n = String.capitalize_ascii (Ident.to_string n) in
    dump_libraries ocf;
    if need_globals s then fprintf ocf "use work.%s.all;\n" cfg.vhdl_globals_name;
    dump_toplevel_intf prefix "entity" ocf s;
    fprintf ocf "architecture struct of %s is\n" top_name;
    fprintf ocf "\n";
    (* FSMs *)
    List.iter
      (fun f -> dump_module_intf "component" ocf (Cmodel.of_fsm_inst s f))
      s.fsms;
    fprintf ocf "\n";
    (* Shared signals *)
    List.iter
      (function (id,cc) -> fprintf ocf "signal %a: %a;\n" pp_ident id pp_abbr_type cc.ct_typ)
      s.ctx.shared;
    fprintf ocf "\n";
    fprintf ocf "begin\n";
    (* Instanciated components *)
    let pp_param fmt (id,(t,v)) = fprintf fmt "%a" G.pp_value (v, vhdl_type_of t.Annot.typ) in 
    let pp_params fmt ps = 
      match ps with
      | [] -> ()
      | _ -> fprintf fmt " generic map (%a)" (Misc.pp_list_h ~sep:"," pp_param) ps in
    List.iteri
      (fun i f ->
         let m = Cmodel.of_fsm_inst s f in
         fprintf ocf "  %s%d: %s%a port map(%a,%s%s);\n"
           (modname f.name)
          i
           (modname f.name)
           pp_params m.c_consts 
           (Misc.pp_list_h ~sep:"," Ident.pp) (List.map fst (m.c_inps @ m.c_outps @ m.c_inouts))
           cfg.vhdl_reset_sig
           (if cfg.vhdl_trace then "," ^ Ident.to_string f.name ^ "_state" else ""))
      s.fsms;
    fprintf ocf "end architecture;\n";
    Misc.close_file (oc,ocf);
    output_files := fname :: !output_files
  
  let dump_toplevel ?(name="") ?(dir="./vhdl") s =
    let prefix = match name with "" -> cfg.vhdl_tb_prefix | p -> p in
    dump_toplevel_impl prefix (dir ^ "/" ^ prefix ^ "_top.vhd") s

   (* Dumping the testbench *)
  
  let dump_testbench_impl prefix fname s =
    let oc,ocf = Misc.open_file fname in
    let tb_name = prefix ^ "_tb" in
    let top_name = prefix ^ "_top" in
    let open Static in
    dump_libraries ocf;
    if need_globals s then fprintf ocf "use work.%s.all;\n" cfg.vhdl_globals_name;
    fprintf ocf "entity %s is\n" tb_name;
    fprintf ocf "end entity;\n";
    fprintf ocf "\n";
    fprintf ocf "architecture struct of %s is\n" tb_name;
    fprintf ocf "\n";
    dump_toplevel_intf prefix "component" ocf s;
    (* Signals *)
    List.iter
      (function (id,cc) -> fprintf ocf "signal %a: %a;\n" pp_ident id pp_abbr_type cc.ct_typ)
      (s.ctx.inputs @ s.ctx.outputs);
    fprintf ocf "signal %s: std_logic;\n" cfg.vhdl_reset_sig;
    if cfg.vhdl_trace then
      List.iter
        (function f -> fprintf ocf "signal %a_state: integer;\n" pp_ident f.name)
        s.fsms;  
    fprintf ocf "\n";
    fprintf ocf "begin\n";
    fprintf ocf "\n";
    (* Input generators *)
    List.iter (dump_input_process ocf) s.ctx.inputs;
    (* Reset generator *)
    fprintf ocf "  reset: process\n";
    fprintf ocf "  begin\n";
    fprintf ocf "    %s <= '1';\n" cfg.vhdl_reset_sig;
    fprintf ocf "    wait for %d %s;\n" cfg.vhdl_reset_duration cfg.vhdl_time_unit;
    fprintf ocf "    %s <= '0';\n" cfg.vhdl_reset_sig;
    fprintf ocf "    wait for %d %s;\n" cfg.vhdl_stop_time cfg.vhdl_time_unit;
    fprintf ocf "    wait;\n";
    fprintf ocf "  end process;\n";
    fprintf ocf "\n";
    let pp_trace ocf fs = 
      let pp_state ocf f = fprintf ocf "%a_state" pp_ident f.name in
      if cfg.vhdl_trace then fprintf ocf ",%a" (Misc.pp_list_h ~sep:"," pp_state) fs
      else fprintf ocf "" in
    (* Toplevel instanciation  *)
    fprintf ocf "  Top: %s port map(%a%a,%s);\n"
      top_name
      (Misc.pp_list_h ~sep:"," pp_ident) (List.map fst (s.ctx.inputs @  s.ctx.outputs))
      pp_trace s.fsms
      cfg.vhdl_reset_sig;
    fprintf ocf "\n";
    fprintf ocf "end architecture;\n";
    Misc.close_file (oc,ocf);
    output_files := fname :: !output_files
  
  let dump_testbench ?(name="") ?(dir="./vhdl") m =
    let prefix = match name with "" -> cfg.vhdl_tb_prefix | p -> p in
    dump_testbench_impl prefix (dir ^ "/" ^ prefix ^ "_tb.vhd") m 

   (* Dumping models *)
  
  let dump_fsm ?(dir="./vhdl") need_globals m =
    let prefix = Ident.to_string m.Cmodel.c_name in
    let fname = dir ^ "/" ^ prefix ^ ".vhd" in
    let oc,ocf = Misc.open_file fname in
    fprintf ocf "library ieee;\n";
    fprintf ocf "use ieee.std_logic_1164.all;\n";
    if Vhdl_types.cfg.vhdl_use_numeric_std then fprintf ocf "use ieee.numeric_std.all;\n";
    fprintf ocf "use work.%s.all;\n" cfg.vhdl_support_package;
    if need_globals then fprintf ocf "use work.%s.all;\n" cfg.vhdl_globals_name;
    fprintf ocf "\n";
    dump_module_intf "entity" ocf m;
    fprintf ocf "\n";
    dump_module_arch ocf m;
    Misc.close_file (oc,ocf);
    output_files := fname :: !output_files
  
  let dump_fsm_model ?(dir="./vhdl") s fm =
    dump_fsm ~dir (need_globals s) (Cmodel.of_fsm_model fm)

  let dump_fsm_inst ?(dir="./vhdl") s fi =
    dump_fsm ~dir (need_globals s) (Cmodel.of_fsm_inst s fi)

  (* Dumping global type definitions, functions and constants *)

  let dump_fun_decl ?(for_impl=false) ocf fd = 
    let open Static.Syntax in
    let f = fd.Annot.desc in
    let pp_typ fmt t = G.pp_type_expr ~type_mark:Vhdl_types.TM_None fmt t in (* No type marks in function args and result *)
    let pp_farg fmt (n,t) = fprintf fmt "%a: %a" pp_ident n pp_typ t in
    fprintf ocf "  function %a(%a) return %a%s\n"
         pp_ident f.ff_name
         (Misc.pp_list_h ~sep:"; " pp_farg) f.ff_args
         pp_typ f.ff_res
         (if for_impl then " is" else ";")

  let dump_cst_decl ocf cd = 
    let open Static.Syntax in
    let c = cd.Annot.desc in
    fprintf ocf "  constant %a : %a := %a;\n"
         pp_ident c.cc_name 
         pp_abbr_type_expr c.cc_typ
         G.pp_expr c.cc_val

  let dump_globals_intf ocf package_name s =
    fprintf ocf "package %s is\n" package_name;
    List.iter (G.pp_type_decl ocf) s.Static.types;
    List.iter (G.pp_type_fns_intf ocf) s.Static.types;
    List.iter (dump_fun_decl ocf) s.Static.fns;
    List.iter (dump_cst_decl ocf) s.Static.csts;
    fprintf ocf "end %s;\n" package_name

  and dump_fun_impl ocf fd =
    dump_fun_decl ~for_impl:true ocf fd;
    fprintf ocf "  begin\n";
    fprintf ocf "    return %a;\n" G.pp_expr fd.Annot.desc.ff_body;
    fprintf ocf "  end;\n\n"
  
  let dump_globals_impl ocf package_name m =
    fprintf ocf "package body %s is\n" package_name;
    List.iter (G.pp_type_fns_impl ocf) m.Static.types;
    List.iter (dump_fun_impl ocf) m.Static.fns;
    fprintf ocf "end %s;\n" package_name

  let dump_globals ?(name="") ?(dir="./vhdl") s =
    let prefix = match name with "" -> cfg.vhdl_globals_name | p -> p in
    let fname = dir ^ "/" ^ prefix ^ ".vhd" in
    let oc,ocf = Misc.open_file fname in
    fprintf ocf "library ieee;\n";
    fprintf ocf "use ieee.std_logic_1164.all;\n";
    if Vhdl_types.cfg.vhdl_use_numeric_std then fprintf ocf "use ieee.numeric_std.all;\n";
    fprintf ocf "use work.%s.all;\n\n" cfg.vhdl_support_package;
    dump_globals_intf ocf prefix s; 
    fprintf ocf "\n";
    dump_globals_impl ocf prefix s;
    Misc.close_file (oc,ocf);
    output_files := fname :: !output_files

  (* Dumping Makefile *)

  let dump_makefile ?(name="") ?(dir="./vhdl") m =
    let templ_fname = cfg.vhdl_lib_dir ^ "/templates/Makefile.vhdl.templ" in
    if Sys.file_exists templ_fname then begin
        let prefix = match name with "" -> cfg.vhdl_tb_prefix | p -> p in
        let tb_name = prefix ^ "_tb" in
        let top_name = prefix ^ "_top" in
        let fname = dir ^ "/" ^ "Makefile" in
        let oc,ocf = Misc.open_file fname in
        Printf.fprintf oc "LIBDIR=%s\n\n" cfg.vhdl_lib_dir;
        Printf.fprintf oc "\n";
        let ic = open_in templ_fname in
        let dump_opt, dump_fmt =
          begin match cfg.vhdl_dump_format with
          | Vcd -> "--vcd", "vcd"
          | Ghw -> "--wave", "ghw"
          end in
        Misc.copy_with_subst ["%%MAIN%%", tb_name; "%%DUMPOPT%%", dump_opt; "%%DUMPFMT%%", dump_fmt] ic oc;
        close_in ic;
        fprintf ocf "\n";
        let pp_modname suff fmt f = fprintf fmt "%a.%s" pp_ident f.Static.name suff in
        let open Static in
        fprintf ocf "%s: %s %s %a %s.vhd\n"
          tb_name
          (cfg.vhdl_lib_name ^ ".vhd")
          (if need_globals m then cfg.vhdl_globals_name ^ ".vhd" else "")
          (Misc.pp_list_h ~sep:" " (pp_modname "vhd")) m.fsms
          tb_name;
        fprintf ocf "\t$(GHDL) -a $(GHDLOPTS) %s.vhd\n" cfg.vhdl_lib_name;
        if need_globals m then 
          fprintf ocf "\t$(GHDL) -a $(GHDLOPTS) %s.vhd\n" cfg.vhdl_globals_name;
        List.iter
          (function f -> fprintf ocf "\t$(GHDL) -a $(GHDLOPTS) %a\n" (pp_modname "vhd") f)
          m.fsms;
        fprintf ocf "\t$(GHDL) -a $(GHDLOPTS) %s.vhd\n" top_name;
        fprintf ocf "\t$(GHDL) -a $(GHDLOPTS) %s.vhd\n" tb_name;
        fprintf ocf "\t$(GHDL) -e $(GHDLOPTS) %s\n" tb_name;
        Misc.close_file (oc,ocf);
        output_files := fname :: !output_files
      end
    else
      Misc.warning (Printf.sprintf "No file %s. No Makefile generated." templ_fname)

  (* Check whether a model can be translated *)

  let check_allowed_fsm_inst (f: Static.fsm) = 
    let m = f.model.Annot.desc in
    let name = Ident.to_string f.name in
    let has_event_type (_,t) =  Static.Syntax.Guest.is_event_type t in
    begin match List.filter has_event_type m.inps with
      | [_] -> ()
      | _ -> Misc.not_implemented ("Vhdl: FSM " ^ name ^ " has more than one input event")
    end;
    begin match List.filter has_event_type m.outps with
      | [] -> ()
      | _ -> Misc.not_implemented ("Vhdl: FSM " ^ name ^ " has output event(s)")
    end;
    if cfg.vhdl_act_semantics = Misc.Synchronous && not (Static.is_rtl f) then
        Misc.warning ("Vhdl: FSM " ^ name ^ " has non-RTL transition(s). This may cause incorrect behavior when using the synchronous interpretation of actions.") 
  
  let check_allowed_system s = 
    let open Static in 
    let valid_shared (id,cc) = 
      if not (G.allowed_shared_type cc.ct_typ) then begin
        let pp fmt (id,ty) = Format.fprintf fmt "VHDL: shared variable %a with type %a" pp_ident id pp_abbr_type cc.ct_typ in
        Misc.not_implemented (Misc.to_string pp (id,cc.ct_typ))
        end;
      if List.length cc.ct_wrs > 1 then  begin
        let pp fmt id = Format.fprintf fmt "VHDL: shared variable %a with multiple writer" pp_ident id in
        Misc.not_implemented (Misc.to_string pp id)
        end in
    List.iter valid_shared s.ctx.shared;
    List.iter check_allowed_fsm_inst s.fsms

  module TypeExprSet = Set.Make(struct type t = Static.Syntax.type_expr let compare = compare end)

  let collect_array_types_in_fn acc fd =
    let f = fd.Annot.desc in
    let open Static.Syntax in
    List.fold_left
      (fun acc t -> TypeExprSet.add t acc)
      acc
      (List.filter Guest.is_array_type (f.ff_res :: List.map snd f.ff_args))

  let collect_array_types_in_cst acc cd =
    let c = cd.Annot.desc in
    let open Static.Syntax in
    List.fold_left
      (fun acc t -> TypeExprSet.add t acc)
      acc
      (List.filter Guest.is_array_type [c.cc_typ])

  let collect_array_types_in_fsm acc (f: Static.fsm) =
    let open Static.Syntax in
    let m = f.model.Annot.desc in
    List.fold_left
      (fun acc t -> TypeExprSet.add t acc)
      acc
      (List.filter Guest.is_array_type (List.map snd (m.inps @ m.outps @ m.vars @ m.params)))

  let collect_array_types (s: Static.t) =
    (* Arrays are not "first class" in VHDL. They have to be declared before being used.
       You cannot write, for ex:
         variable z: array (0 to 3) of integer
       but have to declare the specific array type before (in the architecture or in a global package), e.g :
         type array_4_of_integer is array (0 to 3) of integer
         ...
         variable z: array_4_of_integer;
       To cope with this, we first find all array type instances in the model and treat them "as if" they have
       been declared as type aliases. *)
    let res =
         TypeExprSet.empty
      |> Misc.fold_left collect_array_types_in_fn s.Static.fns 
      |> Misc.fold_left collect_array_types_in_cst s.Static.csts
      |> Misc.fold_left collect_array_types_in_fsm s.Static.fsms in
    TypeExprSet.elements res

  let output ~dir ?(pfx="") s =
    output_files := [];
    check_allowed_system s;
    if s.Static.fsms <> [] then
      begin
        let array_type_decls =
          List.map
            (fun t -> Static.Syntax.Guest.mk_alias_type_decl (Ident.mk ~scope:Global @@ Misc.to_string pp_abbr_type_expr t) t)
            (collect_array_types s) in 
        let s' = { s with types = s.types @ array_type_decls } in
        if need_globals s' then dump_globals ~dir s';
        List.iter (dump_fsm_inst ~dir s') s'.Static.fsms;
        dump_toplevel ~name:pfx ~dir s';
        dump_testbench ~name:pfx ~dir s';
        dump_makefile ~name:pfx ~dir s'
      end
    else (* No instances, dump only models *)
      List.iter (dump_fsm_model ~dir s) s.Static.models;
    !output_files
    
end
