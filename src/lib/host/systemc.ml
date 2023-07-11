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

(* SystemC backend *)

(* exception Error of string * string  (\* where, msg *\) *)

type sc_config = {
  mutable sc_lib_name: string;
  mutable sc_lib_dir: string;
  mutable sc_inpmod_prefix: string;
  mutable sc_tb_name: string;
  mutable sc_globals_name: string;
  mutable sc_state_var: string;
  mutable sc_proc_name: string;
  mutable sc_inp_proc_name: string;
  mutable sc_clk_step_proc_name: string;
  mutable sc_time_unit: string;
  mutable sc_stop_time: int;
  mutable sc_trace: bool;
  mutable sc_trace_state_var: string;
  mutable sc_double_float: bool;
  }

let cfg = {
  sc_lib_name = "rfsm";
  sc_lib_dir = ".";
  sc_inpmod_prefix = "inp_";
  sc_tb_name = "tb";
  sc_globals_name = "globals";
  sc_state_var = "state";
  sc_proc_name = "react";
  sc_inp_proc_name = "gen";
  sc_clk_step_proc_name = "step";
  sc_time_unit = "SC_NS";
  sc_stop_time = 100;
  sc_trace = false;
  sc_trace_state_var = "st";
  sc_double_float = false;
  }

module type SYSTEMC = sig
  
  module Static: Static.T
  module G: Guest.SYSTEMC

  (* exception Error of string * string  (\* where, msg *\) *)
  exception Invalid_output_assign of string * Location.t

  val output: dir:string -> ?pfx:string -> Static.t -> string list 
end
                  
module Make (Static: Static.T)
            (Guest: Guest.SYSTEMC with module Syntax = Static.Syntax.Guest and type value = Static.Value.t)
       : SYSTEMC with module Static = Static =
struct

  module Static = Static
  module G = Guest

  module Cmodel = Cmodel.Make(Static)

  exception Invalid_output_assign of string * Location.t
                   
  let output_files = ref ([] : string list)

  open Format

  let need_globals m = m.Static.types <> [] || m.Static.fns <> [] || m.Static.csts <> [] (* Idem CTask **)

  let scope modname id = modname ^ "::" ^ id

  let pp_action tab m fmt a =
    let open Static in
    let open Cmodel in
    let pp fmt a = match a.Annot.desc  with
      | Syntax.Emit id -> fprintf fmt "notify_ev(%s,\"%s\")" id id
      | Syntax.Assign (lhs, expr) ->
         let id = G.Syntax.lhs_base_name lhs in
         let pp_expr = G.pp_expr ~inps:(List.map fst m.c_inps) in
         let pp_lhs = G.pp_lhs ~inps:(List.map fst m.c_inps) in
         if G.Syntax.is_simple_lhs lhs then
           begin
             if List.mem_assoc id m.c_vars
             then fprintf fmt "%s = %a" id pp_expr expr
             else fprintf fmt "%s.write(%a)" id pp_expr expr
           end
         else (* Non-scalar LHS *)
           begin
             if List.mem_assoc id m.c_vars
             then fprintf fmt "%a = %a" pp_lhs lhs pp_expr expr
             else raise (Invalid_output_assign (id, a.Annot.loc))
           end in
    fprintf fmt "%s%a;\n" tab pp a
  
  let pp_transition tab is_first src m fmt (_,{Annot.desc=_,guards;_},acts,q',_) =
    match guards with
    | [] ->
       List.iter (pp_action tab m fmt) acts;
       if q' <> src then fprintf fmt "%s%s = %s;\n" tab cfg.sc_state_var q'
    | guards -> 
       (* for _=0 to m.c_ddepth-1 do *   fprintf fmt "%swait(SC_ZERO_TIME);\n" tab * done; *)
       fprintf fmt "%s%sif ( %a ) {\n"
         tab
         (if is_first then "" else "else ")
         (Misc.pp_list_h ~sep:" && " (G.pp_expr ~inps:(List.map fst m.c_inps))) guards;
       List.iter (pp_action (tab^"  ") m fmt) acts;
       if q' <> src then fprintf fmt "%s  %s = %s;\n" tab cfg.sc_state_var q';
       fprintf fmt "%s  }\n" tab
  
  let pp_ev_transition tab is_first src m fmt (ev,ts) = 
    fprintf fmt "%s%sif ( %s.read() ) {\n" tab (if is_first then "" else "else ") ev;
    Misc.list_iter_fst (fun is_first t -> pp_transition (tab^"  ") is_first src m fmt t) ts;
    fprintf fmt "%s  }\n" tab
  
  let pp_sysc_ev fmt e = Format.fprintf fmt "%s.posedge_event()" e
  (* Events are implemented as boolean signals because it is not possible to wait on multiple [sc_event]s 
     and telling afterwards which one occurred in SystemC 2.3.0 !! *)
  
  let pp_transitions src after evs m fmt tss =
    let open Cmodel in
    if after then fprintf fmt "      else {\n";
    let tab = if after then "        " else "      " in
    begin match tss with 
      [] -> ()  (* no wait in this case *)
    | [ev,ts] ->
       fprintf fmt "%swait(%a);\n" tab pp_sysc_ev ev;
       for _=0 to m.c_ddepth-1 do 
         fprintf fmt "%swait(SC_ZERO_TIME);\n" tab
       done;
       Misc.list_iter_fst (fun is_first t -> pp_transition tab is_first src m fmt t) ts
    | _ ->
       fprintf fmt "%swait(%a);\n" tab (Misc.pp_list_h ~sep:" | " pp_sysc_ev) evs;
       Misc.list_iter_fst (fun is_first t -> pp_ev_transition tab is_first src m fmt t) tss
    end;
    fprintf fmt "%swait(SC_ZERO_TIME);\n" tab;
    (* Waiting at least one delta cycle so that 
          - events notified at delta=0 cannot be caught after
          - variables modified at delta=0 can be viewed at delta>0 *)
    if after then fprintf fmt "      }\n"
    
  let pp_output_valuation m fmt (o,e) = 
    let pp_expr = G.pp_expr ~inps:(List.map fst m.Cmodel.c_inps) in
    fprintf fmt "      %s.write(%a);\n" o pp_expr e
  
  let pp_state_case m fmt Cmodel.{ st_src=q; st_sensibility_list=evs; st_transitions=tss } =
    fprintf fmt "    case %s:\n" q;
    List.iter (pp_output_valuation m fmt) (List.assoc q m.Cmodel.c_states);
    pp_transitions q false evs m fmt tss;
    fprintf fmt "      break;\n"
  
  let pp_state m fmt Cmodel.{ st_src=q; st_sensibility_list=evs; st_transitions=tss } =
    pp_transitions q false evs m fmt tss
  
  let dump_module_impl with_globals fname m =
    let open Cmodel in
    let oc,ocf = Misc.open_file fname in
    let modname = String.capitalize_ascii m.Cmodel.c_name in
    fprintf ocf "#include \"%s.h\"\n" m.c_name;
    fprintf ocf "#include \"%s.h\"\n" cfg.sc_lib_name;
    if with_globals then fprintf ocf "#include \"%s.h\"\n" cfg.sc_globals_name;
    fprintf ocf "\n";
    List.iter
      (fun (id,(ty,v)) ->
        fprintf ocf "const %a = %a;\n" (G.pp_typed_symbol) (scope modname id,ty) G.pp_value v)
      m.Cmodel.c_consts;
    fprintf ocf "\n";
    if m.c_params <> [] then 
      fprintf ocf "template <%a>\n" (Misc.pp_list_h ~sep:"," G.pp_typed_symbol) m.c_params;
    fprintf ocf "void %s::%s()\n" modname cfg.sc_proc_name;
    fprintf ocf "{\n";
    fprintf ocf "  %s = %s;\n" cfg.sc_state_var (fst m.c_init);
    List.iter (pp_action "  " m ocf) (snd m.c_init);
    fprintf ocf "  while ( 1 ) {\n";
    if cfg.sc_trace then fprintf ocf "    %s = %s;\n" cfg.sc_trace_state_var cfg.sc_state_var;
    begin match m.c_body with
      [] -> () (* should not happen *)
    | [q] -> pp_state m ocf q 
    | qs -> 
       fprintf ocf "    switch ( %s ) {\n" cfg.sc_state_var;
       List.iter (pp_state_case m ocf) m.c_body;
       fprintf ocf "    }\n"
    end;
    fprintf ocf "  }\n";
    fprintf ocf "};\n";
    Misc.close_file (oc,ocf);
    output_files := fname :: !output_files
  
  let dump_module_intf with_globals fname m = 
    let open Cmodel in
    let oc,ocf = Misc.open_file fname in
    let modname = String.capitalize_ascii m.c_name in
    fprintf ocf "#include \"systemc.h\"\n";
    if with_globals then fprintf ocf "#include \"%s.h\"\n" cfg.sc_globals_name;
    fprintf ocf "\n";
    if m.c_params <> [] then 
      fprintf ocf "template <%a>\n" (Misc.pp_list_h ~sep:"," G.pp_typed_symbol) m.c_params;
    fprintf ocf "SC_MODULE(%s)\n" modname;
    fprintf ocf "{\n";
    fprintf ocf "  // Types\n";
    (* if List.length m.c_states > 1 then *)
    fprintf ocf "  typedef enum { %a } t_%s;\n"
      (Misc.pp_list_h ~sep:"," pp_print_string) (List.map fst m.c_states)
      cfg.sc_state_var;
    fprintf ocf "  // IOs\n";
    List.iter (fun (id,ty) -> fprintf ocf "  sc_in<%a> %s;\n" G.pp_type_expr ty id) m.c_inps;
    List.iter (fun (id,ty) -> fprintf ocf "  sc_out<%a> %s;\n" G.pp_type_expr ty id) m.c_outps;
    (* List.iter (fun (id,ty) -> fprintf ocf "  sc_inout<%a> %s;\n" G.pp_typ ty id) m.c_inouts; *)
    if cfg.sc_trace then fprintf ocf "  sc_out<int> %s;\n" cfg.sc_trace_state_var;
    fprintf ocf "  // Constants\n";
    List.iter (fun (id,(ty,_)) -> fprintf ocf "  static const %a;\n" G.pp_typed_symbol (id,ty)) m.c_consts;
    fprintf ocf "  // Local variables\n";
    fprintf ocf "  t_%s %s;\n" cfg.sc_state_var cfg.sc_state_var;
    List.iter (fun (id,ty) -> fprintf ocf "  %a;\n" G.pp_typed_symbol (id,ty)) m.c_vars;
    fprintf ocf "\n";
    fprintf ocf "  void %s();\n" cfg.sc_proc_name;
    fprintf ocf "\n";
    fprintf ocf "  SC_CTOR(%s) {\n" modname;
    fprintf ocf "    SC_THREAD(%s);\n" cfg.sc_proc_name;
    (* fprintf ocf "    sensitive %a;\n"
     *   (Misc.pp_list_h ~sep:"" (fun fmt (e,_) -> fprintf ocf " << %s.pos()" e))
     *   (List.filter (fun (_,t) -> G.Syntax.is_event_type t) m.c_inps); *)
    fprintf ocf "    }\n";
    fprintf ocf "};\n";
    Misc.close_file (oc,ocf);
    output_files := fname :: !output_files
  
  let dump_inp_module_impl fname (id,cc) = 
    let oc,ocf = Misc.open_file fname in
    let name = cfg.sc_inpmod_prefix ^ id in
    let modname = String.capitalize_ascii name in
    fprintf ocf "#include \"%s.h\"\n" name;
    fprintf ocf "#include \"%s.h\"\n" cfg.sc_lib_name;
    fprintf ocf "\n";
    let open Static in
    let pp_date ocf t = fprintf ocf "%d" t in
    let pp_expr = G.pp_expr ~inps:[] in
    let pp_vc ocf (t,v) = fprintf ocf "{%d,%a}"  t pp_expr v in
    begin match cc.Static.ct_stim with
    | Some (Syntax.Sporadic ts) ->
       fprintf ocf "static int _dates[%d] = { %a };\n" (List.length ts) (Misc.pp_list_h ~sep:", " pp_date) ts; 
    | Some (Syntax.Periodic (p,t1,t2)) ->
       fprintf ocf "typedef struct { int period; int t1; int t2; } _periodic_t;\n\n";
       fprintf ocf "static _periodic_t _clk = { %d, %d, %d };\n" p t1 t2
    | Some (Syntax.Value_change []) ->
       ()
    | Some (Syntax.Value_change vcs) ->
       fprintf ocf "typedef struct { int date; %a val; } _vc_t;\n" G.pp_typ cc.Static.ct_typ;
       fprintf ocf "static _vc_t _vcs[%d] = { %a };\n" (List.length vcs) (Misc.pp_list_h ~sep:", " pp_vc) vcs
    | None ->
       () (* TODO: emit warning ? *)
    end;
    fprintf ocf "\n";
    begin match cc.Static.ct_stim with
    | Some (Syntax.Sporadic ts) ->
       fprintf ocf "void %s::%s()\n" modname cfg.sc_inp_proc_name;
       fprintf ocf "{\n";
       fprintf ocf "  int _i=0, _t=0;\n";
       fprintf ocf "  while ( _i < %d ) {\n" (List.length ts);
       fprintf ocf "    wait(_dates[_i]-_t, SC_NS);\n";
       fprintf ocf "    notify_ev(%s,\"%s\");\n" id id;
       fprintf ocf "    _t = _dates[_i];\n";
       fprintf ocf "    _i++;\n";
       fprintf ocf "    }\n";
       fprintf ocf "};\n";
    | Some (Syntax.Periodic (p,t1,t2)) ->
       fprintf ocf "void %s::%s(void)\n" modname cfg.sc_inp_proc_name;
       fprintf ocf "{\n";
       fprintf ocf "  t=0;\n";
       fprintf ocf "  %s.write(0);\n" id;
       fprintf ocf "  wait(_clk.t1, SC_NS);\n";
       fprintf ocf "  t += _clk.t1;\n";
       fprintf ocf "  while ( t <= _clk.t2 ) {\n";
       fprintf ocf "    %s();\n" cfg.sc_clk_step_proc_name;
       fprintf ocf "    }\n";
       fprintf ocf "};\n\n";
       fprintf ocf "void %s::%s(void)\n" modname cfg.sc_clk_step_proc_name;
       fprintf ocf "{\n";
       fprintf ocf "  %s.write(1);\n" id;
       fprintf ocf "  wait(_clk.period/2.0, SC_NS);\n";
       fprintf ocf "  %s.write(0);\n" id;
       fprintf ocf "  wait(_clk.period/2.0, SC_NS);\n";
       fprintf ocf "  t += _clk.period;\n";
       fprintf ocf "};\n"
    | Some (Value_change []) ->
       ()
    | Some (Value_change vcs) ->
       fprintf ocf "void %s::%s()\n" modname cfg.sc_inp_proc_name;
       fprintf ocf "{\n";
       fprintf ocf "  int _i=0, _t=0;\n";
       fprintf ocf "  while ( _i < %d ) {\n" (List.length vcs);
       fprintf ocf "    wait(_vcs[_i].date-_t, SC_NS);\n";
       fprintf ocf "    %s = _vcs[_i].val;\n" id;
       fprintf ocf "    _t = _vcs[_i].date;\n";
       if cfg.sc_trace then
         fprintf ocf "    cout << \"%s: t=\" << _vcs[_i].date << \": wrote \" << _vcs[_i].val << endl;\n" modname;
       fprintf ocf "    _i++;\n";
       fprintf ocf "    }\n";
       fprintf ocf "};\n";
    | None ->
       () (* TODO: emit warning ? *)
    end;
    Misc.close_file (oc,ocf);
    output_files := fname :: !output_files

  let dump_inp_module_intf with_globals fname (id,cc) = 
    let oc,ocf = Misc.open_file fname in
    let modname = String.capitalize_ascii (cfg.sc_inpmod_prefix ^ id) in
    fprintf ocf "#include \"systemc.h\"\n";
    if with_globals then fprintf ocf "#include \"%s.h\"\n" cfg.sc_globals_name;
    fprintf ocf "\n";
    fprintf ocf "SC_MODULE(%s)\n" modname;
    fprintf ocf "{\n";
    fprintf ocf "  // Output\n";
    fprintf ocf "  sc_out<%a> %s;\n" G.pp_typ cc.Static.ct_typ id;
    fprintf ocf "\n";
    begin match cc.Static.ct_stim with
    | Some (Static.Syntax.Periodic _) -> (* For clock processes *)
       fprintf ocf "  void %s();\n" cfg.sc_clk_step_proc_name;
       fprintf ocf "  int t;\n"
    | _ -> () 
    end;
    fprintf ocf "  void %s(void);\n" cfg.sc_inp_proc_name;
    fprintf ocf "\n";
    fprintf ocf "  SC_CTOR(%s) {\n" modname;
    fprintf ocf "    SC_THREAD(%s);\n" cfg.sc_inp_proc_name;
    fprintf ocf "    }\n";
    fprintf ocf "};\n";
    Misc.close_file (oc,ocf);
    output_files := fname :: !output_files

  (* Dumping global type declarations, functions and constants *)

  let dump_fun_decl fmt { Annot.desc = f; _ } = (* Idem CTask *)
    let open Static.Syntax in
    let pp_f_arg fmt (n,t) = fprintf fmt "%a %s" G.pp_type_expr t n in
    Format.fprintf fmt "%a %s(%a);\n" 
      G.pp_type_expr f.ff_res 
      f.ff_name
      (Misc.pp_list_h ~sep:"," pp_f_arg) f.ff_args

  let dump_fun_impl fmt { Annot.desc = f; _ } = (* Idem CTask *)
    let open Static.Syntax in
    let pp_f_arg fmt (n,t) = fprintf fmt "%a %s" G.pp_type_expr t n in
    let pp_expr = G.pp_expr ~inps:[] in
    Format.fprintf fmt "%a %s(%a) { return %a; }\n" 
      G.pp_type_expr f.ff_res 
      f.ff_name
      (Misc.pp_list_h ~sep:"," pp_f_arg) f.ff_args
      pp_expr f.ff_body

  let dump_cst_decl fmt { Annot.desc = c; _ } = (* Idem CTask *)
    let open Static.Syntax in
    G.pp_cst_decl fmt c.cc_name c.cc_typ  (* Cannot do more due to the idiosyncrasies of C type declarations.. *)

  let dump_cst_impl fmt { Annot.desc = c; _ } =
    let open Static.Syntax in
    G.pp_cst_impl fmt c.cc_name c.cc_typ c.cc_val  (* Cannot do more due to the idiosyncrasies of C type declarations.. *)

  let dump_type_impl fmt td =
    G.pp_type_impl fmt td 

  let dump_globals_intf dir prefix s =
    let open Static in
    let fname = dir ^ "/" ^ prefix ^ ".h" in
    let oc, ocf = Misc.open_file fname in
    fprintf ocf "#ifndef _%s_h\n" cfg.sc_globals_name;
    fprintf ocf "#define _%s_h\n\n" cfg.sc_globals_name;
    fprintf ocf "#include \"systemc.h\"\n\n";
    List.iter (fun td -> Format.fprintf ocf "%a\n" G.pp_type_decl td) s.types;
    List.iter (dump_fun_decl ocf) s.fns;
    List.iter (dump_cst_decl ocf) s.csts;
    fprintf ocf "#endif@.";
    Misc.close_file (oc,ocf);
    output_files := fname :: !output_files

  let dump_globals_impl dir prefix s = 
    let fname = dir ^ "/" ^ prefix ^ ".cpp" in
    let oc,ocf = Misc.open_file fname in
    fprintf ocf "#include \"%s.h\"\n\n" prefix;
    List.iter (dump_fun_impl ocf) s.Static.fns;
    List.iter (dump_cst_impl ocf) s.Static.csts;
    List.iter (dump_type_impl ocf) s.Static.types;
    Misc.close_file (oc,ocf);
    output_files := fname :: !output_files

  let dump_globals ?(name="") ?(dir="./systemc") m =
    let prefix = match name with "" -> cfg.sc_globals_name | p -> p in
    dump_globals_intf dir prefix m;
    dump_globals_impl dir prefix m

  (* Dumping the testbench *)

  let dump_stimulus ocf (id,v) =
    let pp_expr = G.pp_expr ~inps:[] in
    match v with 
      None -> fprintf ocf "  notify_ev(%s);\n" id
    | Some v' -> fprintf ocf "  %s = %a;\n" id pp_expr v'

  let dump_testbench_impl tb_name fname m = 
    let oc,ocf = Misc.open_file fname in
    let open Static in
    let modname n = String.capitalize_ascii n in
    fprintf ocf "#include \"systemc.h\"\n";
    fprintf ocf "#include \"%s.h\"\n" cfg.sc_lib_name;
    List.iter (function (id,_) -> fprintf ocf "#include \"%s%s.h\"\n" cfg.sc_inpmod_prefix id) m.ctx.inputs;
    List.iter (function f -> fprintf ocf "#include \"%s.h\"\n" f.name) m.fsms;
    fprintf ocf "\n";
    fprintf ocf "int sc_main(int argc, char *argv[])\n";
    fprintf ocf "{\n";
    (* Signals *)
    List.iter
      (function (id,cc) -> fprintf ocf "  sc_signal<%a> %s;\n" G.pp_typ cc.Static.ct_typ id)
      (m.ctx.inputs @ m.ctx.outputs @ m.ctx.shared); (* TO FIX !!!! - see below *)
    (* List.iter
     *   (function
     *    | id,(ty,MShared (wrs,_)) ->
     *       if List.length wrs > 1 then fprintf ocf "  sc_signal<%a,SC_MANY_WRITERS> %s;\n" G.pp_typ typ id
     *       else fprintf ocf "  sc_signal<%a> %s;\n" G.pp_typ ty id
     *    | _ -> ())
     *   m.ctx.shared; *) (* TO FIX ! *)
    if cfg.sc_trace then
      List.iter
        (function f -> fprintf ocf "  sc_signal<int> %s;\n" (f.name ^ "_state"))
        m.fsms;
    (* Trace file *)
    fprintf ocf "  sc_trace_file *trace_file;\n";
    fprintf ocf "  trace_file = sc_create_vcd_trace_file (\"%s\");\n" tb_name;
    fprintf ocf "  sc_write_comment(trace_file, \"Generated by RFSM v%s\");\n" Version.version;
    List.iter
      (function (id,_) -> fprintf ocf "  sc_trace(trace_file, %s, \"%s\");\n" id id)
      (m.ctx.inputs @ m.ctx.outputs @ m.ctx.shared);
    if cfg.sc_trace then
      List.iter
        (function f -> let id = f.name ^ "_state" in fprintf ocf "  sc_trace(trace_file, %s, \"%s\");\n" id id)
        m.fsms;  
    fprintf ocf "\n";
    (* Input modules *)
    List.iter
      (function (id, _) ->
         let modname = String.capitalize_ascii (cfg.sc_inpmod_prefix ^ id) in
         fprintf ocf "  %s %s(\"%s\");\n" modname modname modname;
         fprintf ocf "  %s(%s);\n" modname id)
      m.ctx.inputs;
    fprintf ocf "\n";
    (* FSM modules *)
    List.iter
      (function f ->
         let m = Cmodel.of_fsm_model f.model in
         (* let actual_name (id,_) = f.f_l2g id in *)
         fprintf ocf "  %s %s(\"%s\");\n" (modname f.name) f.name f.name;
         fprintf ocf "  %s(%a%s);\n"
           f.name
           (Misc.pp_list_h ~sep:"," pp_print_string) (List.map fst (m.c_inps @ m.c_outps @ m.c_inouts))
           (if cfg.sc_trace then "," ^ f.name ^ "_state" else ""))
      m.fsms;
    fprintf ocf "\n";
    (* Start *)
    fprintf ocf "  sc_start(%d, %s);\n" cfg.sc_stop_time cfg.sc_time_unit;
    fprintf ocf "\n";
    fprintf ocf "  sc_close_vcd_trace_file (trace_file);\n";
    fprintf ocf "\n";
    fprintf ocf "  return EXIT_SUCCESS;\n";
    fprintf ocf "}\n";
    Misc.close_file (oc,ocf);
    output_files := fname :: !output_files

  (* Dumping Makefile *)

  let dump_makefile ?(name="") ?(dir="./systemc") m =
    let templ_fname = cfg.sc_lib_dir ^ "/templates/Makefile.systemc.templ" in
    if Sys.file_exists templ_fname then begin
        let tb_name = match name with "" -> cfg.sc_tb_name | p -> p in
        let fname = dir ^ "/" ^ "Makefile" in
        let oc,ocf = Misc.open_file fname in
        Printf.fprintf oc "LIBDIR=%s\n\n" cfg.sc_lib_dir;
        Printf.fprintf oc "\n";
        let ic = open_in templ_fname in
        Misc.copy_with_subst ["%%MAIN%%", tb_name] ic oc;
        close_in ic;
        let modname suff f = f.Static.name ^ suff in
        let imodname suff (id,_) = cfg.sc_inpmod_prefix ^ id ^ suff in
        let open Static in
        let globals suffix = if need_globals m then cfg.sc_globals_name ^ suffix else "" in
        (* fprintf ocf "%s.o: %s.h %s.cpp\n" cfg.sc_lib_name cfg.sc_lib_name cfg.sc_lib_name; *)
        List.iter
          (function f -> fprintf ocf "%s.o: %s.h %s.cpp %s\n" f.Static.name f.Static.name f.Static.name (globals ".h"))
          m.fsms;
        List.iter
          (function inp -> let name = imodname "" inp in fprintf ocf "%s.o: %s.h %s.cpp\n" name name name)
          m.ctx.inputs;
        let pp_mod suff ocf f = fprintf ocf "%s" (modname suff f) in 
        let pp_imod suff ocf f = fprintf ocf "%s" (imodname suff f) in 
        fprintf ocf "%s.o: %a %a %s.cpp\n"
          tb_name
          (Misc.pp_list_h ~sep:" " (pp_mod ".h")) m.fsms
          (Misc.pp_list_h ~sep:" " (pp_imod ".h")) m.ctx.inputs
          tb_name;
        let pp_objs ocf () =
          fprintf ocf "%s.o %s %a %a %s.o"
            cfg.sc_lib_name
            (globals ".o")
            (Misc.pp_list_h ~sep:" " (pp_mod ".o")) m.fsms
            (Misc.pp_list_h ~sep:" " (pp_imod ".o")) m.ctx.inputs
            tb_name in
        fprintf ocf "%s: %a\n" tb_name pp_objs ();
        fprintf ocf "\t$(LD) $(LDFLAGS) -o %s %a -lsystemc  2>&1 | c++filt\n" tb_name pp_objs ();
        Misc.close_file (oc,ocf);
        output_files := fname :: !output_files
      end
    else
      Misc.warning (Printf.sprintf "No file %s. No Makefile generated." templ_fname)

  let dump_fsm_model ?(prefix="") ?(dir="./systemc") fm =
    let f = Cmodel.of_fsm_model fm in
    let prefix = match prefix with "" -> f.c_name | p -> p in
    dump_module_intf false (dir ^ "/" ^ prefix ^ ".h") f;
    dump_module_impl false (dir ^ "/" ^ prefix ^ ".cpp") f

  let dump_fsm_inst ?(dir="./systemc") m fi =
    let f = Cmodel.of_fsm_inst fi in
    dump_module_intf (need_globals m) (dir ^ "/" ^ fi.name ^ ".h") f;
    dump_module_impl (need_globals m) (dir ^ "/" ^ fi.name ^ ".cpp") f

  let dump_input ?(prefix="") ?(dir="./systemc") m ((id,_) as inp) =
    let prefix = match prefix with "" -> cfg.sc_inpmod_prefix ^ id | p -> p in
    dump_inp_module_intf (need_globals m) (dir ^ "/" ^ prefix ^ ".h") inp;
    dump_inp_module_impl (dir ^ "/" ^ prefix ^ ".cpp") inp

  let dump_testbench ?(name="") ?(dir="./systemc") m =
    let tb_name = match name with "" -> cfg.sc_tb_name | p -> p in
    dump_testbench_impl tb_name (dir ^ "/" ^ tb_name ^ ".cpp") m

  let output ~dir ?(pfx="") s =
    output_files := [];
    if s.Static.fsms <> [] then
      begin
        List.iter (dump_input ~dir s) s.Static.ctx.inputs;
        if need_globals s then dump_globals ~dir s;
        List.iter (dump_fsm_inst ~dir s) s.Static.fsms;
        dump_testbench ~name:pfx ~dir s;
        dump_makefile ~name:pfx ~dir s
      end
    else (* No instances, dump only models *)
      List.iter (dump_fsm_model ~dir) s.Static.models;
    !output_files

end
