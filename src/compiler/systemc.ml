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

open Utils
open Types
open Printf
open Cmodel

exception Error of string * string  (* where, msg *)

type sc_config = {
  mutable sc_lib_name: string;
  mutable sc_lib_dir: string;
  mutable sc_inpmod_prefix: string;
  mutable sc_tb_name: string;
  mutable sc_globals_name: string;
  mutable sc_state_var: string;
  mutable sc_proc_name: string;
  mutable sc_inp_proc_name: string;
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
  sc_globals_name = "global_fns";
  sc_state_var = "state";
  sc_proc_name = "react";
  sc_inp_proc_name = "gen";
  sc_time_unit = "SC_NS";
  sc_stop_time = 100;
  sc_trace = false;
  sc_trace_state_var = "st";
  sc_double_float = false;
  }

type profil = {
    mutable has_globals: bool;
  }

let profil = {
  has_globals = false;
  }

let rec bit_size n = if n=0 then 0 else 1 + bit_size (n/2)

let rec string_of_type t = match t with 
  | TyEvent -> "bool"
  | TyBool -> "bool"
  | TyEnum cs -> "enum {" ^ ListExt.to_string (function c -> c) "," cs ^ "}"
  | TyInt (Some (TiConst lo,TiConst hi)) ->
     if lo < 0 then "sc_int<" ^ string_of_int (bit_size (max (-lo) hi)) ^ "> "
     else "sc_uint<" ^ string_of_int (bit_size hi) ^ "> "
  | TyInt _ -> "int"
  | TyFloat -> if cfg.sc_double_float then "double" else "float"
  | _ -> raise (Error ("string_of_type", "unsupported type"))

let string_of_value v = match v with
  Expr.Val_int i -> string_of_int i
| Expr.Val_float i -> string_of_float i
| Expr.Val_bool i -> string_of_bool i
| Expr.Val_enum s -> s
| Expr.Val_fn _ -> "<fun>"

exception Type_of_value
        
let type_of_value v = match v with
  Expr.Val_int _ -> "int"
| Expr.Val_float _ -> "float"
| Expr.Val_bool _ -> "bool"
| Expr.Val_enum _ -> raise Type_of_value
| Expr.Val_fn _ -> raise Type_of_value

let string_of_ival = function
    None -> ""
  | Some v -> " = " ^ string_of_value v

let string_of_op = function
    "=" -> "=="
  | "mod" -> "%"
  | "+." -> "+" 
  | "-." -> "-" 
  | "*." -> "*" 
  | "/." -> "/" 
  | op ->  op

let string_of_expr m e =
  let paren level s = if level > 0 then "(" ^ s ^ ")" else s in
  let rec string_of level e =
    match e with
      Expr.EInt c -> string_of_int c
    | Expr.EFloat c -> string_of_float c
    | Expr.EBool c -> string_of_bool c
    | Expr.EEnum c -> c
    | Expr.EVar n -> if List.mem_assoc n (m.c_inps @ m.c_inouts) then n ^ ".read()" else n
    | Expr.EBinop (op,e1,e2) -> paren level (string_of (level+1) e1 ^ string_of_op op ^ string_of (level+1) e2)
    | Expr.ECond (e1,e2,e3) -> paren level (string_of (level+1) e1 ^ "?" ^ string_of (level+1) e2 ^ ":" ^ string_of (level+1) e3)
    | Expr.EFapp (("~-"|"~-."),[e]) -> "-" ^ "(" ^ string_of level e ^ ")"
    | Expr.EFapp (f,es) -> f ^ "(" ^ ListExt.to_string (string_of level) "," es ^ ")"
  in
  string_of 0 e

let string_of_guard m e = string_of_expr m e

let string_of_action m a = match a with
  | Action.Assign (id, expr) ->
       if List.mem_assoc id m.c_outps then id ^ ".write(" ^ string_of_expr m expr ^ ")"
       else id ^ "=" ^ string_of_expr m expr
    | Action.Emit id -> "notify_ev(" ^ id ^ ",\"" ^ id ^ "\")"
    | Action.StateMove (id,s,s') -> "" (* should not happen *)

let dump_action oc tab m a = fprintf oc "%s%s;\n" tab (string_of_action m a)

let dump_transition oc tab is_first src m (q',(cond,acts,_,_)) =
  match cond with
  | [ev], [] ->
       List.iter (dump_action oc tab m) acts;
       if q' <> src then fprintf oc "%s%s = %s;\n" tab cfg.sc_state_var q'
  | [ev], guards -> 
       (* for i=0 to m.c_ddepth-1 do 
        *   fprintf oc "%swait(SC_ZERO_TIME);\n" tab
        * done; *)
       fprintf oc "%s%sif ( %s ) {\n"
        tab
        (if is_first then "" else "else ")
        (ListExt.to_string (string_of_guard m) " && " guards);
       List.iter (dump_action oc (tab^"  ") m) acts;
       if q' <> src then fprintf oc "%s  %s = %s;\n" tab cfg.sc_state_var q';
       fprintf oc "%s  }\n" tab
  | _, _ ->
       failwith "Systemc.dump_transition"

let dump_ev_transition oc tab is_first src m (ev,ts) = 
  fprintf oc "%s%sif ( %s.read() ) {\n" tab (if is_first then "" else "else ") ev;
  ListExt.iter_fst (fun is_first t -> dump_transition oc (tab^"  ") is_first src m t) ts;
  fprintf oc "%s  }\n" tab

let sysc_event e = e ^ ".posedge_event()" 
  (* Events are implemented as boolean signals because it is not possible to wait on multiple [sc_event]s 
     and telling afterwards which one occurred in SystemC 2.3.0 !! *)

let dump_transitions oc g src after evs m tss =
   if after then fprintf oc "      else {\n";
   let tab = if after then "        " else "      " in
   begin match tss with 
      [] -> ()  (* no wait in this case *)
    | [ev,ts] ->
       fprintf oc "%swait(%s);\n" tab (sysc_event ev);
       for i=0 to m.c_ddepth-1 do 
         fprintf oc "%swait(SC_ZERO_TIME);\n" tab
       done;
       ListExt.iter_fst (fun is_first t -> dump_transition oc tab is_first src m t) ts;
    | _ ->
       fprintf oc "%swait(%s);\n" tab (ListExt.to_string sysc_event " | " evs);
       ListExt.iter_fst (fun is_first t -> dump_ev_transition oc tab is_first src m t) tss
   end;
   fprintf oc "%swait(SC_ZERO_TIME);\n" tab;
       (* Waiting at least one delta cycle so that 
          - events notified at delta=0 cannot be caught after
          - variables modified at delta=0 can be viewed at delta>0 *)
   if after then fprintf oc "      }\n"
     
let dump_state oc g m { st_src=q; st_sensibility_list=evs; st_transitions=tss } =
   dump_transitions oc g q false evs m tss

let dump_state_case oc g m { st_src=q; st_sensibility_list=evs; st_transitions=tss } =
  fprintf oc "    case %s:\n" q;
  dump_transitions oc g q false evs m tss;
  fprintf oc "      break;\n"

let dump_module_impl g fname m =
  let oc = open_out fname in
  let modname = String.capitalize_ascii m.c_name in
  fprintf oc "#include \"%s.h\"\n" m.c_name;
  fprintf oc "#include \"%s.h\"\n" cfg.sc_lib_name;
  if profil.has_globals then fprintf oc "#include \"%s.h\"\n" cfg.sc_globals_name;
  fprintf oc "\n";
  fprintf oc "void %s::%s()\n" modname cfg.sc_proc_name;
  fprintf oc "{\n";
  fprintf oc "  %s = %s;\n" cfg.sc_state_var (fst m.c_init);
  List.iter (dump_action oc "  " m) (snd m.c_init);
  fprintf oc "  while ( 1 ) {\n";
  if cfg.sc_trace then fprintf oc "    %s = %s;\n" cfg.sc_trace_state_var cfg.sc_state_var;
  begin match m.c_body with
    [] -> () (* should not happen *)
  | [q] -> dump_state oc g m q 
  | qs -> 
      fprintf oc "    switch ( %s ) {\n" cfg.sc_state_var;
      List.iter (dump_state_case oc g m) m.c_body;
      fprintf oc "    }\n"
  end;
  fprintf oc "  }\n";
  fprintf oc "};\n";
  Logfile.write fname;
  close_out oc

let dump_module_intf fname m = 
  let oc = open_out fname in
  let modname = String.capitalize_ascii m.c_name in
  fprintf oc "#include \"systemc.h\"\n";
  fprintf oc "\n";
  fprintf oc "SC_MODULE(%s)\n" modname;
  fprintf oc "{\n";
  fprintf oc "  // Types\n";
  (* if List.length m.c_states > 1 then  *)
  fprintf oc "  typedef enum { %s } t_%s;\n" (ListExt.to_string (function s -> s) ", " m.c_states) cfg.sc_state_var;
  fprintf oc "  // IOs\n";
  List.iter (fun (id,ty) -> fprintf oc "  sc_in<%s> %s;\n" (string_of_type ty) id) m.c_inps;
  List.iter (fun (id,ty) -> fprintf oc "  sc_out<%s> %s;\n" (string_of_type ty) id) m.c_outps;
  List.iter (fun (id,ty) -> fprintf oc "  sc_inout<%s> %s;\n" (string_of_type ty) id) m.c_inouts;
  if cfg.sc_trace then fprintf oc "  sc_out<int> %s;\n" cfg.sc_trace_state_var;
  fprintf oc "  // Constants\n";
  List.iter (fun (id,(ty,v)) -> fprintf oc "  static const %s %s = %s;\n" (string_of_type ty) id (string_of_value v)) m.c_consts;
  fprintf oc "  // Local variables\n";
  fprintf oc "  t_%s %s;\n" cfg.sc_state_var cfg.sc_state_var;
  List.iter (fun (id,(ty,iv)) -> fprintf oc "  %s %s%s;\n" (string_of_type ty) id (string_of_ival iv)) m.c_vars;
  fprintf oc "\n";
  fprintf oc "  void %s();\n" cfg.sc_proc_name;
  fprintf oc "\n";
  fprintf oc "  SC_CTOR(%s) {\n" modname;
  fprintf oc "    SC_THREAD(%s);\n" cfg.sc_proc_name;
  (* let inp_events = List.filter (function (_, TyEvent) -> true | _ -> false) m.c_inps in *)
  (* fprintf oc "    sensitive %s;\n" (ListExt.to_string (function (e,_) -> (" << " ^ e ^ ".pos()")) "" inp_events); *)
  fprintf oc "    }\n";
  fprintf oc "};\n";
  Logfile.write fname;
  close_out oc

let dump_inp_module_impl fname (id,(ty,desc)) = 
  let oc = open_out fname in
  let name = cfg.sc_inpmod_prefix ^ id in
  let modname = String.capitalize_ascii name in
  fprintf oc "#include \"%s.h\"\n" name;
  fprintf oc "#include \"%s.h\"\n" cfg.sc_lib_name;
  fprintf oc "\n";
  let open Sysm in
  begin match desc with
    | MInp ({sd_comprehension=Sporadic ts}, _) ->
       fprintf oc "static int _dates[%d] = { %s };\n" (List.length ts) (ListExt.to_string string_of_int ", " ts)
    | MInp ({sd_comprehension=Periodic (p,t1,t2)}, _) ->
       fprintf oc "typedef struct { int period; int t1; int t2; } _periodic_t;\n\n";
       fprintf oc "static _periodic_t _clk = { %d, %d, %d };\n" p t1 t2
    | MInp ({sd_comprehension=ValueChange []}, _) ->
       ()
    | MInp ({sd_comprehension=ValueChange vcs}, _) ->
       let ty =
         try type_of_value (snd (List.hd vcs))
         with Type_of_value -> Error.not_implemented "SystemC input generator with non-int values" in
       let string_of_vc (t,v) = "{" ^ string_of_int t ^ "," ^ string_of_value v ^ "}" in
       fprintf oc "typedef struct { int date; %s val; } _vc_t;\n" ty;
       fprintf oc "static _vc_t _vcs[%d] = { %s };\n" (List.length vcs) (ListExt.to_string string_of_vc ", " vcs)
    | _ ->
       failwith "Systemc.dump_inp_module_impl" (* should not happen *)
  end;
  fprintf oc "\n";
  fprintf oc "void %s::%s()\n" modname cfg.sc_inp_proc_name;
  fprintf oc "{\n";
  begin match desc with
    | MInp ({sd_comprehension=Sporadic ts}, _) ->
       fprintf oc "  int _i=0, _t=0;\n";
       fprintf oc "  while ( _i < %d ) {\n" (List.length ts);
       fprintf oc "    wait(_dates[_i]-_t, SC_NS);\n";
       fprintf oc "    notify_ev(%s,\"%s\");\n" id id;
       fprintf oc "    _t = _dates[_i];\n";
       fprintf oc "    _i++;\n";
       fprintf oc "    }\n";
    | MInp ({sd_comprehension=Periodic (p,t1,t2)}, _) ->
       fprintf oc "  int _t=0;\n";
       fprintf oc "  wait(_clk.t1, SC_NS);\n";
       fprintf oc "  notify_ev(%s,\"%s\");\n" id id;
       fprintf oc "  _t = _clk.t1;\n";
       fprintf oc "  while ( _t <= _clk.t2 ) {\n";
       fprintf oc "    wait(_clk.period, SC_NS);\n";
       fprintf oc "    notify_ev(%s,\"%s\");\n" id id;
       fprintf oc "    _t += _clk.period;\n";
       fprintf oc "    }\n";
    | MInp ({sd_comprehension=ValueChange []}, _) ->
       ()
    | MInp ({sd_comprehension=ValueChange vcs}, _) ->
       fprintf oc "  int _i=0, _t=0;\n";
       fprintf oc "  while ( _i < %d ) {\n" (List.length vcs);
       fprintf oc "    wait(_vcs[_i].date-_t, SC_NS);\n";
       fprintf oc "    %s = _vcs[_i].val;\n" id;
       fprintf oc "    _t = _vcs[_i].date;\n";
       fprintf oc "    _i++;\n";
       fprintf oc "    }\n";
    | _ ->
       failwith "Systemc.dump_inp_module_impl" (* should not happen *)
  end;
  fprintf oc "};\n";
  Logfile.write fname;
  close_out oc

let dump_inp_module_intf fname (id,(ty,desc)) = 
  let oc = open_out fname in
  let modname = String.capitalize_ascii (cfg.sc_inpmod_prefix ^ id) in
  fprintf oc "#include \"systemc.h\"\n";
  fprintf oc "\n";
  fprintf oc "SC_MODULE(%s)\n" modname;
  fprintf oc "{\n";
  fprintf oc "  // Output\n";
  fprintf oc "  sc_out<%s> %s;\n" (string_of_type ty) id;
  fprintf oc "\n";
  fprintf oc "  void %s();\n" cfg.sc_inp_proc_name;
  fprintf oc "\n";
  fprintf oc "  SC_CTOR(%s) {\n" modname;
  fprintf oc "    SC_THREAD(%s);\n" cfg.sc_inp_proc_name;
  fprintf oc "    }\n";
  fprintf oc "};\n";
  Logfile.write fname;
  close_out oc

(* Dumping global functions *)

let dump_global_fn_intf oc (id,(ty,gd)) = match gd, ty with
| Sysm.MFun (args, body), Types.TyArrow(TyProduct ts, tr) -> 
    fprintf oc "%s %s (%s);\n"
      (string_of_type tr)
      id 
      (ListExt.to_string (function (a,t) -> string_of_type t ^ " " ^ a) "," (List.combine args ts)) 
| _ -> ()

let dump_global_fn_impl oc (id,(ty,gd)) = match gd, ty with
| Sysm.MFun (args, body), Types.TyArrow(TyProduct ts, tr) -> 
    fprintf oc "%s %s (%s) { return %s; }\n"
      (string_of_type tr)
      id 
      (ListExt.to_string (function (a,t) -> string_of_type t ^ " " ^ a) "," (List.combine args ts)) 
      (string_of_expr Cmodel.empty body)
| _ -> ()

let dump_global_fns_intf dir prefix fs =
  let fname = dir ^ "/" ^ prefix ^ ".h" in
  profil.has_globals <- true;
  let oc = open_out fname in
  Printf.fprintf oc "#ifndef _%s_h\n" cfg.sc_globals_name;
  Printf.fprintf oc "#define _%s_h\n\n" cfg.sc_globals_name;
  List.iter (dump_global_fn_intf oc) fs;
  Printf.fprintf oc "\n#endif\n";
  Logfile.write fname;
  close_out oc

let dump_global_fns_impl dir prefix fs =
  let fname = dir ^ "/" ^ prefix ^ ".cpp" in
  let oc = open_out fname in
  Printf.fprintf oc "#include \"%s.h\"\n\n" prefix;
  List.iter (dump_global_fn_impl oc) fs;
  Logfile.write fname;
  close_out oc

let dump_globals ?(name="") ?(dir="./systemc") m =
  let prefix = match name with "" -> cfg.sc_globals_name | p -> p in
  dump_global_fns_intf dir prefix m.Sysm.m_fns;
  dump_global_fns_impl dir prefix m.Sysm.m_fns

(* Dumping the testbench *)

let dump_stimulus oc (id,v) = match v with 
    None -> fprintf oc "  notify_ev(%s);\n" id
  | Some v' -> fprintf oc "  %s = %s;\n" id (string_of_value v')

let dump_testbench_impl fname m = 
  let oc = open_out fname in
  let open Sysm in
  let modname n = String.capitalize_ascii n in
  fprintf oc "#include \"systemc.h\"\n";
  fprintf oc "#include \"%s.h\"\n" cfg.sc_lib_name;
  List.iter (function (id,_) -> fprintf oc "#include \"%s%s.h\"\n" cfg.sc_inpmod_prefix id) m.m_inputs;
  List.iter (function f -> fprintf oc "#include \"%s.h\"\n" f.Fsm.f_name) m.m_fsms;
  fprintf oc "\n";
  fprintf oc "int sc_main(int argc, char *argv[])\n";
  fprintf oc "{\n";
  (* Signals *)
  List.iter
   (function (id,(ty,_)) -> fprintf oc "  sc_signal<%s> %s;\n" (string_of_type ty) id)
   (m.m_inputs @ m.m_outputs);
  List.iter
    (function
     | id,(ty,MShared (wrs,_)) ->
        if List.length wrs > 1 then fprintf oc "  sc_signal<%s,SC_MANY_WRITERS> %s;\n" (string_of_type ty) id
        else fprintf oc "  sc_signal<%s> %s;\n" (string_of_type ty) id
     | _ -> ())
   m.m_shared;
  if cfg.sc_trace then
    List.iter
      (function f -> fprintf oc "  sc_signal<int> %s;\n" (f.Fsm.f_name ^ "_state"))
      m.m_fsms;  
  (* Trace file *)
  fprintf oc "  sc_trace_file *trace_file;\n";
  fprintf oc "  trace_file = sc_create_vcd_trace_file (\"%s\");\n" cfg.sc_tb_name;
  List.iter
   (function (id,(ty,_)) -> fprintf oc "  sc_trace(trace_file, %s, \"%s\");\n" id id)
   (m.m_inputs @ m.m_outputs @ m.m_shared);
  if cfg.sc_trace then
    List.iter
      (function f -> let id = f.Fsm.f_name ^ "_state" in fprintf oc "  sc_trace(trace_file, %s, \"%s\");\n" id id)
      m.m_fsms;  
  fprintf oc "\n";
  (* Input modules *)
  List.iter
    (function (id, _) ->
      let modname = String.capitalize_ascii (cfg.sc_inpmod_prefix ^ id) in
      fprintf oc "  %s %s(\"%s\");\n" modname modname modname;
      fprintf oc "  %s(%s);\n" modname id)
    m.m_inputs;
  fprintf oc "\n";
  (* FSM modules *)
  List.iter
    (function f ->
      let m = Cmodel.c_model_of_fsm m f in
      let actual_name (id,_) = f.f_l2g id in
      fprintf oc "  %s %s(\"%s\");\n" (modname f.f_name) f.f_name f.f_name;
      fprintf oc "  %s(%s%s);\n"
              f.f_name
              (ListExt.to_string actual_name "," (m.c_inps @ m.c_outps @ m.c_inouts))
              (if cfg.sc_trace then "," ^ f.f_name ^ "_state" else ""))
    m.m_fsms;
  fprintf oc "\n";
  (* Start *)
  fprintf oc "  sc_start(%d, %s);\n" cfg.sc_stop_time cfg.sc_time_unit;
  fprintf oc "\n";
  fprintf oc "  sc_close_vcd_trace_file (trace_file);\n";
  fprintf oc "\n";
  fprintf oc "  return EXIT_SUCCESS;\n";
  fprintf oc "}\n";
  Logfile.write fname;
  close_out oc

(* Dumping Makefile *)

let dump_makefile ?(dir="./systemc") m =
  let fname = dir ^ "/" ^ "Makefile" in
  let oc = open_out fname in
  let modname suff f = f.Fsm.f_name ^ suff in
  let imodname suff (id,_) = cfg.sc_inpmod_prefix ^ id ^ suff in
  let open Sysm in
  fprintf oc "include %s/etc/Makefile.systemc\n\n" cfg.sc_lib_dir;
  let globals suffix = if profil.has_globals then cfg.sc_globals_name ^ suffix else "" in
  (* fprintf oc "%s.o: %s.h %s.cpp\n" cfg.sc_lib_name cfg.sc_lib_name cfg.sc_lib_name; *)
  List.iter
    (function f -> fprintf oc "%s.o: %s.h %s.cpp %s\n" f.Fsm.f_name f.Fsm.f_name f.Fsm.f_name (globals ".h"))
    m.m_fsms;
  List.iter
    (function inp -> let name = imodname "" inp in fprintf oc "%s.o: %s.h %s.cpp\n" name name name)
    m.m_inputs;
  fprintf oc "%s.o: %s %s %s.cpp\n"
          cfg.sc_tb_name
          (ListExt.to_string (modname ".h")  " " m.m_fsms)
          (ListExt.to_string (imodname ".h")  " " m.m_inputs)
          cfg.sc_tb_name;
  let objs = sprintf "%s.o %s %s %s %s.o"
            (cfg.sc_lib_name |> Filename.concat "systemc" |> Filename.concat cfg.sc_lib_dir)
            (globals ".o")
            (ListExt.to_string (modname ".o")  " " m.m_fsms)
            (ListExt.to_string (imodname ".o")  " " m.m_inputs)
            cfg.sc_tb_name in
  fprintf oc "%s: %s\n" cfg.sc_tb_name  objs;
  fprintf oc "\t$(LD) $(LDFLAGS) -o %s %s -lsystemc  2>&1 | c++filt\n" cfg.sc_tb_name objs;
  Logfile.write fname;
  close_out oc

(* Dumping the support library *)

(* let dump_lib_impl fname = *)
(*   let oc = open_out fname in *)
(*   fprintf oc "#include \"systemc.h\"\n"; *)
(*   fprintf oc "\n"; *)
(*   fprintf oc "void notify_ev(sc_out<bool> &s)\n"; *)
(*   fprintf oc "{\n"; *)
(*   fprintf oc "  s = 1;\n"; *)
(*   fprintf oc "  wait(%s,%s);\n" (fst cfg.sc_ev_duration) (snd cfg.sc_ev_duration); *)
(*   fprintf oc "  wait(%s,%s);\n" (fst cfg.sc_ev_duration) (snd cfg.sc_ev_duration); *)
(*   fprintf oc "  s = 0;\n";  *)
(*   fprintf oc "}\n"; *)
(*   fprintf oc "void notify_ev(sc_signal<bool> &s)\n"; *)
(*   fprintf oc "{\n"; *)
(*   fprintf oc "  s.write(1);\n"; *)
(*   if cfg.sc_trace then  *)
(*     fprintf oc " cout << \"notify_ev(\" << s.name() << \") @ t=\" << sc_time_stamp() << endl;\n"; *)
(*   (\* fprintf oc "  sc_start(%s,%s);\n" (fst cfg.sc_ev_duration) (snd cfg.sc_ev_duration); *\) *)
(*   fprintf oc "  sc_start(0,SC_NS);\n"; *)
(*   fprintf oc "  s = 0;\n"; *)
(*   fprintf oc "  sc_start(0,SC_NS);\n"; *)
(*   fprintf oc "}\n"; *)
  (* Logfile.write fname; *)
(*   close_out oc *)

(* let dump_lib_intf fname = *)
(*   let oc = open_out fname in *)
(*   fprintf oc "void notify_ev(sc_out<bool> &s);\n"; *)
(*   fprintf oc "void notify_ev(sc_signal<bool> &s);\n"; *)
  (* Logfile.write fname; *)
(*   close_out oc *)

let dump_fsm m ?(prefix="") ?(dir="./systemc") fsm =
  let f = Cmodel.c_model_of_fsm m fsm in
  let prefix = match prefix with "" -> fsm.Fsm.f_name | p -> p in
  dump_module_intf (dir ^ "/" ^ prefix ^ ".h") f;
  dump_module_impl m (dir ^ "/" ^ prefix ^ ".cpp") f

let dump_input ?(prefix="") ?(dir="./systemc") ((id,_) as inp) =
  let prefix = match prefix with "" -> cfg.sc_inpmod_prefix ^ id | p -> p in
  dump_inp_module_intf (dir ^ "/" ^ prefix ^ ".h") inp;
  dump_inp_module_impl (dir ^ "/" ^ prefix ^ ".cpp") inp

let dump_model ?(dir="./systemc") m = 
  List.iter (dump_input ~dir:dir) m.Sysm.m_inputs;
  List.iter (dump_fsm ~dir:dir m) m.Sysm.m_fsms

let dump_testbench ?(name="") ?(dir="./systemc") m =
  let prefix = match name with "" -> cfg.sc_tb_name | p -> p in
  dump_testbench_impl (dir ^ "/" ^ prefix ^ ".cpp") m 

(* Check whether a model can be translated *)

let check_allowed m =
  match Fsm.cfg.Fsm.act_sem with
  | Fsm.Sequential -> ()
  | Fsm.Synchronous -> Error.not_implemented "SystemC: synchronous actions"

(* let dump_lib dir = *)
(*   dump_lib_intf (dir ^ "/" ^ cfg.sc_lib_name ^ ".h"); *)
(*   dump_lib_impl (dir ^ "/" ^ cfg.sc_lib_name ^ ".cpp") *)
