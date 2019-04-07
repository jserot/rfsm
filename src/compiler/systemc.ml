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

let record_access f = ".repr." ^ f 

let need_globals m = m.Static.m_types <> [] || m.Static.m_fns <> [] || m.Static.m_consts <> [] 

let rec string_of_type t = match t with 
  | TyEvent -> "bool"
  | TyBool -> "bool"
  | TyChar -> "char"
  | TyEnum (nm, _) when Types.is_lit_name nm -> Types.string_of_name nm
  | TyEnum (_, cs) -> "enum {" ^ Utils.ListExt.to_string (function c -> c) "," cs ^ "}"
  | TyInt (SzExpr1 (TiConst sz)) -> "sc_uint<" ^ string_of_int sz ^ "> "
  | TyInt (SzExpr2  _) -> "int"  (* range annotations ignored here *)
  | TyInt _ -> "int"
  | TyFloat -> if cfg.sc_double_float then "double" else "float"
  | TyRecord (nm, _) when Types.is_lit_name nm -> Types.string_of_name nm
  | TyRecord (_, fs) -> raise (Error ("string_of_type", "anonymous record"))
  | _ -> raise (Error ("string_of_type", "unsupported type"))

let string_of_array_size sz = match sz with
  | Types.Index.TiConst n -> string_of_int n
  | _ -> Misc.fatal_error "Systemc.string_of_array_size"
                                  
let string_of_typed_item ?(scope="") (id,ty) =
  let id' = if scope = "" then id else scope ^ "::" ^ id in
  match ty with 
  | TyArray (sz,ty') -> string_of_type ty' ^ " " ^ id' ^ "[" ^ string_of_array_size sz ^ "]"
  | _ -> string_of_type ty ^ " " ^ id'

let string_of_char c = "'" ^ String.make 1 c ^ "'"

let rec string_of_value v = match v.Expr.v_desc, Types.real_type v.Expr.v_typ with
  Expr.Val_int i, _ -> string_of_int i
| Expr.Val_float i, _-> string_of_float i
| Expr.Val_bool i, _ -> string_of_bool i
| Expr.Val_char c, _ -> string_of_char c
| Expr.Val_enum c, Types.TyEnum (name, _) -> string_of_enum_value (Types.string_of_name name) c
| Expr.Val_fn _, _ -> "<fun>"
| Expr.Val_unknown, _ -> "<unknown>"
| Expr.Val_none, _ -> "<none>"
| Expr.Val_array vs, _ -> "{" ^ Utils.ListExt.to_string string_of_value "," (Array.to_list vs) ^ "}"
| Expr.Val_record fs, Types.TyRecord (name, _) -> string_of_record_value (Types.string_of_name name) fs
| _, _ -> Misc.fatal_error "Systemc.string_of_value"

and string_of_enum_value tyname c = tyname ^ "::" ^ c

and string_of_record_value tyname fs = tyname ^ "(" ^ Utils.ListExt.to_string string_of_value "," (List.map snd fs) ^ ")"

let string_of_op = function
    "=" -> "=="
  | "mod" -> "%"
  | "+." -> "+" 
  | "-." -> "-" 
  | "*." -> "*" 
  | "/." -> "/" 
  | op ->  op

let string_of_int_range a hi lo = a ^ ".range(" ^ hi ^ "," ^ lo ^ ")"

let string_of_expr m e =
  let paren level s = if level > 0 then "(" ^ s ^ ")" else s in
  let access id = if List.mem_assoc id (m.c_inps @ m.c_inouts) then id ^ ".read()" else id in
  let rec string_of level e =
    match e.Expr.e_desc, e.Expr.e_typ with
      Expr.EInt c, _ -> string_of_int c
    | Expr.EFloat c, _ -> string_of_float c
    | Expr.EChar c, _ -> string_of_char c
    | Expr.EBool c, _ -> string_of_bool c
    | Expr.EEnum c, Types.TyEnum (n, _) -> string_of_enum_value (Types.string_of_name n) c
    | Expr.EEnum c, _ -> Misc.fatal_error "Systemc.string_of_expr"
    | Expr.EVar n, _ -> access n
    | Expr.EBinop (op,e1,e2), _ -> paren level (string_of (level+1) e1 ^ string_of_op op ^ string_of (level+1) e2)
    | Expr.ECond (e1,e2,e3), _ -> paren level (string_of (level+1) e1 ^ "?" ^ string_of (level+1) e2 ^ ":" ^ string_of (level+1) e3)
    | Expr.EFapp (("~-"|"~-."),[e]), _ -> "-" ^ "(" ^ string_of level e ^ ")"
    | Expr.EFapp (f,es), _ -> f ^ "(" ^ Utils.ListExt.to_string (string_of level) "," es ^ ")"
    | Expr.EArrExt es, _ -> "{" ^ Utils.ListExt.to_string (string_of level) "," es ^ "}"
    | Expr.EArr (a,idx), _ -> a ^ "[" ^ string_of level idx ^ "]" (* [a] is always a local var *)
    | Expr.ERecord (n,f), _ -> access n ^ record_access f
    | Expr.EBit (a,idx), _ -> let i = string_of level idx in string_of_int_range (access a) i i
    | Expr.EBitrange (a,hi,lo), _ -> string_of_int_range (access a) (string_of level hi) (string_of level lo)
    | Expr.ECast (e,te), _ -> "(" ^ string_of_type te.te_typ ^ ")(" ^ string_of level e ^ ")"
  in
  string_of 0 e

let string_of_guard m e = string_of_expr m e

let string_of_action m a = match a with
  | Action.Assign ({l_desc=Action.LhsVar id}, expr) ->
     if List.mem_assoc id m.c_outps
     then id ^ ".write(" ^ string_of_expr m expr ^ ")"
     else id ^ "=" ^ string_of_expr m expr
  | Action.Assign ({l_desc=Action.LhsArrInd (id,idx)}, expr) ->
     if List.mem_assoc id m.c_outps
     then Misc.fatal_error "Systemc.string_of_action: assignation of a non-scalar output"
     else id ^ "[" ^ string_of_expr m idx ^ "]" ^ "=" ^ string_of_expr m expr
  | Action.Assign ({l_desc=Action.LhsArrRange (id,idx1,idx2)}, expr) ->
     if List.mem_assoc id m.c_outps
     then Misc.fatal_error "Systemc.string_of_action: assignation of a non-scalar output"
     else
       let hi = string_of_expr m idx1 in 
       let lo = string_of_expr m idx2 in 
       string_of_int_range id hi lo ^ "=" ^ string_of_expr m expr
  | Action.Assign ({l_desc=Action.LhsRField (id,f)}, expr) ->
     if List.mem_assoc id m.c_outps
     then Misc.fatal_error "Systemc.string_of_action: assignation of a non-scalar output"
     else id ^ record_access f ^ "=" ^ string_of_expr m expr
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
        (Utils.ListExt.to_string (string_of_guard m) " && " guards);
       List.iter (dump_action oc (tab^"  ") m) acts;
       if q' <> src then fprintf oc "%s  %s = %s;\n" tab cfg.sc_state_var q';
       fprintf oc "%s  }\n" tab
  | _, _ ->
       Misc.fatal_error "Systemc.dump_transition"

let dump_ev_transition oc tab is_first src m (ev,ts) = 
  fprintf oc "%s%sif ( %s.read() ) {\n" tab (if is_first then "" else "else ") ev;
  Utils.ListExt.iter_fst (fun is_first t -> dump_transition oc (tab^"  ") is_first src m t) ts;
  fprintf oc "%s  }\n" tab

let sysc_event e = e ^ ".posedge_event()" 
  (* Events are implemented as boolean signals because it is not possible to wait on multiple [sc_event]s 
     and telling afterwards which one occurred in SystemC 2.3.0 !! *)

let dump_transitions oc src after evs m tss =
   if after then fprintf oc "      else {\n";
   let tab = if after then "        " else "      " in
   begin match tss with 
      [] -> ()  (* no wait in this case *)
    | [ev,ts] ->
       fprintf oc "%swait(%s);\n" tab (sysc_event ev);
       for i=0 to m.c_ddepth-1 do 
         fprintf oc "%swait(SC_ZERO_TIME);\n" tab
       done;
       Utils.ListExt.iter_fst (fun is_first t -> dump_transition oc tab is_first src m t) ts;
    | _ ->
       fprintf oc "%swait(%s);\n" tab (Utils.ListExt.to_string sysc_event " | " evs);
       Utils.ListExt.iter_fst (fun is_first t -> dump_ev_transition oc tab is_first src m t) tss
   end;
   fprintf oc "%swait(SC_ZERO_TIME);\n" tab;
       (* Waiting at least one delta cycle so that 
          - events notified at delta=0 cannot be caught after
          - variables modified at delta=0 can be viewed at delta>0 *)
   if after then fprintf oc "      }\n"
     
let dump_state oc m { st_src=q; st_sensibility_list=evs; st_transitions=tss } =
   dump_transitions oc q false evs m tss

let dump_state_case oc m { st_src=q; st_sensibility_list=evs; st_transitions=tss } =
  fprintf oc "    case %s:\n" q;
  dump_transitions oc q false evs m tss;
  fprintf oc "      break;\n"

let dump_module_impl with_globals fname m =
  let oc = open_out fname in
  let modname = String.capitalize_ascii m.c_name in
  fprintf oc "#include \"%s.h\"\n" m.c_name;
  fprintf oc "#include \"%s.h\"\n" cfg.sc_lib_name;
  if with_globals then fprintf oc "#include \"%s.h\"\n" cfg.sc_globals_name;
  fprintf oc "\n";
  (* List.iter (fun (id,(ty,v)) -> fprintf oc "  static const %s = %s;\n" (string_of_typed_item (id,ty)) (string_of_value v)) m.c_consts; *)
  List.iter
    (fun (id,(ty,v)) ->
      fprintf oc "const %s = %s;\n" (string_of_typed_item ~scope:modname (id,ty)) (string_of_value v))
    m.c_consts;
  fprintf oc "\n";
  if m.c_params <> [] then 
    fprintf oc "template <%s>\n" (Utils.ListExt.to_string string_of_typed_item ", " m.c_params);
  fprintf oc "void %s::%s()\n" modname cfg.sc_proc_name;
  fprintf oc "{\n";
  fprintf oc "  %s = %s;\n" cfg.sc_state_var (fst m.c_init);
  List.iter (dump_action oc "  " m) (snd m.c_init);
  fprintf oc "  while ( 1 ) {\n";
  if cfg.sc_trace then fprintf oc "    %s = %s;\n" cfg.sc_trace_state_var cfg.sc_state_var;
  begin match m.c_body with
    [] -> () (* should not happen *)
  | [q] -> dump_state oc m q 
  | qs -> 
      fprintf oc "    switch ( %s ) {\n" cfg.sc_state_var;
      List.iter (dump_state_case oc m) m.c_body;
      fprintf oc "    }\n"
  end;
  fprintf oc "  }\n";
  fprintf oc "};\n";
  Logfile.write fname;
  close_out oc

let dump_module_intf with_globals fname m = 
  let oc = open_out fname in
  let modname = String.capitalize_ascii m.c_name in
  (* let string_of_ival v =
   *   let open Expr in
   *   match v.v_desc with
   *   | Val_unknown -> ""
   *   | Val_array vs when List.for_all (function {v_desc=Val_unknown} -> true | _ -> false) (Array.to_list vs) -> ""
   *   | Val_record fs when List.for_all (function {v_desc=Val_unknown} -> true | _ -> false) (List.map snd fs) -> ""
   *   | _ -> " = " ^ string_of_value v in *)
  fprintf oc "#include \"systemc.h\"\n";
  if with_globals then fprintf oc "#include \"%s.h\"\n" cfg.sc_globals_name;
  fprintf oc "\n";
  if m.c_params <> [] then 
    fprintf oc "template <%s>\n" (Utils.ListExt.to_string string_of_typed_item ", " m.c_params);
  fprintf oc "SC_MODULE(%s)\n" modname;
  fprintf oc "{\n";
  fprintf oc "  // Types\n";
  (* if List.length m.c_states > 1 then  *)
  fprintf oc "  typedef enum { %s } t_%s;\n" (Utils.ListExt.to_string (function s -> s) ", " m.c_states) cfg.sc_state_var;
  fprintf oc "  // IOs\n";
  List.iter (fun (id,ty) -> fprintf oc "  sc_in<%s> %s;\n" (string_of_type ty) id) m.c_inps;
  List.iter (fun (id,ty) -> fprintf oc "  sc_out<%s> %s;\n" (string_of_type ty) id) m.c_outps;
  List.iter (fun (id,ty) -> fprintf oc "  sc_inout<%s> %s;\n" (string_of_type ty) id) m.c_inouts;
  if cfg.sc_trace then fprintf oc "  sc_out<int> %s;\n" cfg.sc_trace_state_var;
  fprintf oc "  // Constants\n";
  List.iter (fun (id,(ty,v)) -> fprintf oc "  static const %s;\n" (string_of_typed_item (id,ty))) m.c_consts;
  fprintf oc "  // Local variables\n";
  fprintf oc "  t_%s %s;\n" cfg.sc_state_var cfg.sc_state_var;
  (* List.iter (fun (id,(ty,iv)) -> fprintf oc "  %s%s;\n" (string_of_typed_item (id,ty)) (string_of_ival iv)) m.c_vars; *)
  List.iter (fun (id,ty) -> fprintf oc "  %s;\n" (string_of_typed_item (id,ty))) m.c_vars;
  fprintf oc "\n";
  fprintf oc "  void %s();\n" cfg.sc_proc_name;
  fprintf oc "\n";
  fprintf oc "  SC_CTOR(%s) {\n" modname;
  fprintf oc "    SC_THREAD(%s);\n" cfg.sc_proc_name;
  (* let inp_events = List.filter (function (_, TyEvent) -> true | _ -> false) m.c_inps in *)
  (* fprintf oc "    sensitive %s;\n" (Utils.ListExt.to_string (function (e,_) -> (" << " ^ e ^ ".pos()")) "" inp_events); *)
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
  let open Static in
  begin match desc with
    (* | MInp ({sd_comprehension=Sporadic ts}, _) -> *)
    | MInp (Sporadic ts, _) ->
       fprintf oc "static int _dates[%d] = { %s };\n" (List.length ts) (Utils.ListExt.to_string string_of_int ", " ts)
    (* | MInp ({sd_comprehension=Periodic (p,t1,t2)}, _) -> *)
    | MInp (Periodic (p,t1,t2), _) ->
       fprintf oc "typedef struct { int period; int t1; int t2; } _periodic_t;\n\n";
       fprintf oc "static _periodic_t _clk = { %d, %d, %d };\n" p t1 t2
    (* | MInp ({sd_comprehension=ValueChange []}, _) -> *)
    | MInp (ValueChange [], _) ->
       ()
    (* | MInp ({sd_comprehension=ValueChange vcs}, _) -> *)
    | MInp (ValueChange vcs, _) ->
       (* let ty =
        *   try type_of_value (snd (List.hd vcs))
        *   with Type_of_value -> Error.not_implemented "SystemC input generator with non-int values" in *)
       let string_of_vc (t,v) = "{" ^ string_of_int t ^ "," ^ string_of_value v ^ "}" in
       fprintf oc "typedef struct { int date; %s val; } _vc_t;\n" (string_of_type ty);
       fprintf oc "static _vc_t _vcs[%d] = { %s };\n" (List.length vcs) (Utils.ListExt.to_string string_of_vc ", " vcs)
    | _ ->
       Misc.fatal_error "Systemc.dump_inp_module_impl" (* should not happen *)
  end;
  fprintf oc "\n";
  begin match desc with
    (* | MInp ({sd_comprehension=Sporadic ts}, _) -> *)
    | MInp (Sporadic ts, _) ->
       fprintf oc "void %s::%s()\n" modname cfg.sc_inp_proc_name;
       fprintf oc "{\n";
       fprintf oc "  int _i=0, _t=0;\n";
       fprintf oc "  while ( _i < %d ) {\n" (List.length ts);
       fprintf oc "    wait(_dates[_i]-_t, SC_NS);\n";
       fprintf oc "    notify_ev(%s,\"%s\");\n" id id;
       fprintf oc "    _t = _dates[_i];\n";
       fprintf oc "    _i++;\n";
       fprintf oc "    }\n";
       fprintf oc "};\n";
    (* | MInp ({sd_comprehension=Periodic (p,t1,t2)}, _) -> *)
    | MInp (Periodic (p,t1,t2), _) ->
       fprintf oc "void %s::%s(void)\n" modname cfg.sc_inp_proc_name;
       fprintf oc "{\n";
       fprintf oc "  t=0;\n";
       fprintf oc "  %s.write(0);\n" id;
       fprintf oc "  wait(_clk.t1, SC_NS);\n";
       fprintf oc "  t += _clk.t1;\n";
       fprintf oc "  while ( t <= _clk.t2 ) {\n";
       fprintf oc "    %s();\n" cfg.sc_clk_step_proc_name;
       fprintf oc "    }\n";
       fprintf oc "};\n\n";
       fprintf oc "void %s::%s(void)\n" modname cfg.sc_clk_step_proc_name;
       fprintf oc "{\n";
       fprintf oc "  %s.write(1);\n" id;
       fprintf oc "  wait(_clk.period/2.0, SC_NS);\n";
       fprintf oc "  %s.write(0);\n" id;
       fprintf oc "  wait(_clk.period/2.0, SC_NS);\n";
       fprintf oc "  t += _clk.period;\n";
       fprintf oc "};\n"
    (* | MInp ({sd_comprehension=ValueChange []}, _) -> *)
    | MInp (ValueChange [], _) ->
       ()
    (* | MInp ({sd_comprehension=ValueChange vcs}, _) -> *)
    | MInp (ValueChange vcs, _) ->
       fprintf oc "void %s::%s()\n" modname cfg.sc_inp_proc_name;
       fprintf oc "{\n";
       fprintf oc "  int _i=0, _t=0;\n";
       fprintf oc "  while ( _i < %d ) {\n" (List.length vcs);
       fprintf oc "    wait(_vcs[_i].date-_t, SC_NS);\n";
       fprintf oc "    %s = _vcs[_i].val;\n" id;
       fprintf oc "    _t = _vcs[_i].date;\n";
       if cfg.sc_trace then
         fprintf oc "    cout << \"%s: t=\" << _vcs[_i].date << \": wrote \" << _vcs[_i].val << endl;\n" modname;
       fprintf oc "    _i++;\n";
       fprintf oc "    }\n";
       fprintf oc "};\n";
    | _ ->
       Misc.fatal_error "Systemc.dump_inp_module_impl" (* should not happen *)
  end;
  Logfile.write fname;
  close_out oc

let dump_inp_module_intf with_globals fname (id,(ty,desc)) = 
  let oc = open_out fname in
  let modname = String.capitalize_ascii (cfg.sc_inpmod_prefix ^ id) in
  fprintf oc "#include \"systemc.h\"\n";
  if with_globals then fprintf oc "#include \"%s.h\"\n" cfg.sc_globals_name;
  fprintf oc "\n";
  fprintf oc "SC_MODULE(%s)\n" modname;
  fprintf oc "{\n";
  fprintf oc "  // Output\n";
  fprintf oc "  sc_out<%s> %s;\n" (string_of_type ty) id;
  fprintf oc "\n";
  begin match desc with
  (* | Static.MInp ({sd_comprehension=Periodic _}, _) -> (\* For clock processes *\) *)
  | Static.MInp (Periodic _, _) -> (* For clock processes *)
     fprintf oc "  void %s();\n" cfg.sc_clk_step_proc_name;
     fprintf oc "  int t;\n"
  | _ -> ()
  end;
  fprintf oc "  void %s(void);\n" cfg.sc_inp_proc_name;
  fprintf oc "\n";
  fprintf oc "  SC_CTOR(%s) {\n" modname;
  fprintf oc "    SC_THREAD(%s);\n" cfg.sc_inp_proc_name;
  fprintf oc "    }\n";
  fprintf oc "};\n";
  Logfile.write fname;
  close_out oc

(* Dumping global type declarations, functions and constants *)

let dump_record_type_defn oc name fields = 
  let mk f sep fs = Utils.ListExt.to_string f sep fs in
  fprintf oc "class %s {\n" name;
  fprintf oc "public:\n";
  fprintf oc "  struct { %s } repr;\n"
    (mk (function (n,t) -> string_of_type t ^ " " ^ n ^ ";") " " fields);
  fprintf oc "  %s() { };\n" name;
  fprintf oc "  %s(%s) { %s };\n"
    name
    (mk (function (n,t) -> string_of_type t ^ " " ^ n) ", " fields)
    (mk (function (n,_) -> "repr." ^ n ^ "=" ^ n ^ ";") " " fields);
  fprintf oc "  inline bool friend operator == ( const %s& v1, const %s& v2) { return %s; }\n" name name
    (mk (function (n,_) -> "v1.repr." ^ n ^ "==v2.repr." ^ n) " && " fields);
  fprintf oc "  inline %s& operator = (const %s& v) { repr = v.repr; return *this; }\n" name name;
  fprintf oc "  inline friend ::std::ostream& operator << ( ::std::ostream& os, const %s& v) {\n" name;
  fprintf oc "    os << \"{\" << %s << \"} \";\n"
    (mk (function (n,_) -> "\"" ^ n ^ "=\"" ^ " << v.repr." ^ n) " << \",\" << " fields); 
  fprintf oc "    return os;\n";
  fprintf oc "    }\n";
  fprintf oc "  inline friend void sc_trace(sc_trace_file *tf, const %s& v, const std::string& n) {\n" name;
  List.iter
    (function (n,_) -> fprintf oc "    sc_trace(tf,v.repr.%s, n+\".%s\");\n" n n)
    fields;
  fprintf oc "  }\n";
  fprintf oc "};\n"

let dump_enum_type_defn oc name vs = 
  fprintf oc "class %s {\n" name;
  fprintf oc "public:\n";
  fprintf oc "  enum values { %s };\n"
    (Utils.ListExt.to_string (function (v,i) -> v ^ "=" ^ string_of_int i) "," (List.mapi (fun i v -> v,i) vs));
  fprintf oc "  int repr; // SystemC 2.3 does not allow tracing of enumerated values :(\n";
  fprintf oc "  static const char* names[%d];\n" (List.length vs);
  fprintf oc "  %s() { };\n" name;
  fprintf oc "  %s(int r) { repr=r; };\n" name;
  fprintf oc "  inline friend bool operator == ( const %s& v1, const %s& v2) { return v1.repr == v2.repr; }\n" name name;
  fprintf oc "  inline %s& operator = (const %s& v) { repr = v.repr; return *this; }\n" name name;
  fprintf oc "  inline friend ::std::ostream& operator << ( ::std::ostream& os, const %s& v) {\n" name;
  fprintf oc "     os << names[v.repr];\n";
  fprintf oc "     return os;\n";
  fprintf oc "     }\n";
  fprintf oc "  inline friend void sc_trace(sc_trace_file *tf, const %s& v, const std::string& n) {\n" name;
  fprintf oc "     sc_trace(tf, v.repr, n);\n";
  fprintf oc "     }\n";
  fprintf oc "};\n\n"

let dump_global_type_defn oc (name,ty) = match ty with
  | TyEnum (nm,cs) -> dump_enum_type_defn oc (Types.string_of_name nm) cs
  | TyRecord (nm,fs) -> dump_record_type_defn oc (Types.string_of_name nm) fs
  | _ -> ()

let dump_global_fn_intf oc (id,(ty,gd)) = match gd, ty with
| Static.MConst _, TyArray(sz,ty') ->
     fprintf oc "extern %s %s[%s];\n" (string_of_type ty') id (string_of_array_size sz)
| Static.MConst _, _ ->
     fprintf oc "%s %s;\n" (string_of_type ty) id 
| Static.MFun (args, body), Types.TyArrow(TyProduct ts, tr) -> 
    fprintf oc "%s %s (%s);\n"
      (string_of_type tr)
      id 
      (Utils.ListExt.to_string (function (a,t) -> string_of_type t ^ " " ^ a) "," (List.combine args ts)) 
| _ -> ()

let dump_global_fn_impl oc (id,(ty,gd)) = match gd, ty with
| Static.MConst v, TyArray(sz,ty') ->
     fprintf oc "%s %s[%s] = %s;\n" (string_of_type ty') id (string_of_array_size sz) (string_of_value v)
| Static.MConst v, _ -> 
    fprintf oc "%s %s = %s;\n" (string_of_type ty) id (string_of_value v)
| Static.MFun (args, body), Types.TyArrow(TyProduct ts, tr) -> 
    fprintf oc "%s %s (%s) { return %s; }\n"
      (string_of_type tr)
      id 
      (Utils.ListExt.to_string (function (a,t) -> string_of_type t ^ " " ^ a) "," (List.combine args ts)) 
      (string_of_expr Cmodel.empty body)
| _ -> ()

let dump_globals_intf dir prefix m =
  let fname = dir ^ "/" ^ prefix ^ ".h" in
  let oc = open_out fname in
  Printf.fprintf oc "#ifndef _%s_h\n" cfg.sc_globals_name;
  Printf.fprintf oc "#define _%s_h\n\n" cfg.sc_globals_name;
  Printf.fprintf oc "#include \"systemc.h\"\n\n";
  List.iter (dump_global_type_defn oc) m.Static.m_types; 
  List.iter (dump_global_fn_intf oc) (m.Static.m_consts @ m.Static.m_fns);
  Printf.fprintf oc "\n#endif\n";
  Logfile.write fname;
  close_out oc

let dump_enum_type_names oc = function
  | _ , Types.TyEnum (nm,cs) ->
     fprintf oc "const char* %s::names[%d] = { %s };\n" 
       (Types.string_of_name nm)
       (List.length cs)
       (Utils.ListExt.to_string (function c -> "\"" ^ c ^ "\"") ", " cs)
  | _ -> ()

let dump_globals_impl dir prefix m =
  let fname = dir ^ "/" ^ prefix ^ ".cpp" in
  let oc = open_out fname in
  Printf.fprintf oc "#include \"%s.h\"\n\n" prefix;
  List.iter (dump_global_fn_impl oc) (m.Static.m_consts @ m.Static.m_fns);
  List.iter (dump_enum_type_names oc) m.Static.m_types;
  Logfile.write fname;
  close_out oc

let dump_globals ?(name="") ?(dir="./systemc") m =
  let prefix = match name with "" -> cfg.sc_globals_name | p -> p in
  dump_globals_intf dir prefix m;
  dump_globals_impl dir prefix m

(* Dumping the testbench *)

let dump_stimulus oc (id,v) = match v with 
    None -> fprintf oc "  notify_ev(%s);\n" id
  | Some v' -> fprintf oc "  %s = %s;\n" id (string_of_value v')

let dump_testbench_impl tb_name fname m = 
  let oc = open_out fname in
  let open Static in
  let modname n = String.capitalize_ascii n in
  let comment = sprintf "Generated by RFSM v%s from model %s.fsm" Version.version m.m_name in
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
  fprintf oc "  trace_file = sc_create_vcd_trace_file (\"%s\");\n" tb_name;
  fprintf oc "  sc_write_comment(trace_file, \"%s\");\n" comment;
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
      let m = Cmodel.c_model_of_fsm_inst m f in
      let actual_name (id,_) = f.f_l2g id in
      fprintf oc "  %s %s(\"%s\");\n" (modname f.f_name) f.f_name f.f_name;
      fprintf oc "  %s(%s%s);\n"
              f.f_name
              (Utils.ListExt.to_string actual_name "," (m.c_inps @ m.c_outps @ m.c_inouts))
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

let dump_makefile ?(name="") ?(dir="./systemc") m =
  let templ_fname = cfg.sc_lib_dir ^ "/etc/Makefile.systemc.templ" in
  if Sys.file_exists templ_fname then begin
      let tb_name = match name with "" -> cfg.sc_tb_name | p -> p in
      let fname = dir ^ "/" ^ "Makefile" in
      let oc = open_out fname in
      fprintf oc "LIBDIR=%s\n\n" cfg.sc_lib_dir;
      let ic = open_in templ_fname in
      Misc.copy_with_subst ["%%MAIN%%", tb_name] ic oc;
      close_in ic;
      fprintf oc "\n";
      let modname suff f = f.Fsm.f_name ^ suff in
      let imodname suff (id,_) = cfg.sc_inpmod_prefix ^ id ^ suff in
      let open Static in
      let globals suffix = if need_globals m then cfg.sc_globals_name ^ suffix else "" in
      (* fprintf oc "%s.o: %s.h %s.cpp\n" cfg.sc_lib_name cfg.sc_lib_name cfg.sc_lib_name; *)
      List.iter
        (function f -> fprintf oc "%s.o: %s.h %s.cpp %s\n" f.Fsm.f_name f.Fsm.f_name f.Fsm.f_name (globals ".h"))
        m.m_fsms;
      List.iter
        (function inp -> let name = imodname "" inp in fprintf oc "%s.o: %s.h %s.cpp\n" name name name)
        m.m_inputs;
      fprintf oc "%s.o: %s %s %s.cpp\n"
        tb_name
        (Utils.ListExt.to_string (modname ".h")  " " m.m_fsms)
        (Utils.ListExt.to_string (imodname ".h")  " " m.m_inputs)
        tb_name;
      let objs = sprintf "$(LIBDIR)/systemc/%s.o %s %s %s %s.o"
                   cfg.sc_lib_name
                   (globals ".o")
                   (Utils.ListExt.to_string (modname ".o")  " " m.m_fsms)
                   (Utils.ListExt.to_string (imodname ".o")  " " m.m_inputs)
                   tb_name in
      fprintf oc "%s: %s\n" tb_name  objs;
      fprintf oc "\t$(LD) $(LDFLAGS) -o %s %s -lsystemc  2>&1 | c++filt\n" tb_name objs;
      Logfile.write fname;
      close_out oc
    end
  else
    Misc.warning (Printf.sprintf "No file %s. No Makefile generated." templ_fname)

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

let dump_fsm_model ?(prefix="") ?(dir="./systemc") fsm =
  let f = Cmodel.c_model_of_fsm_model fsm in
  let prefix = match prefix with "" -> fsm.Fsm.fm_name | p -> p in
  dump_module_intf false (dir ^ "/" ^ prefix ^ ".h") f;
  dump_module_impl false (dir ^ "/" ^ prefix ^ ".cpp") f

let dump_fsm_inst ?(prefix="") ?(dir="./systemc") m fsm =
  let f = Cmodel.c_model_of_fsm_inst m fsm in
  let prefix = match prefix with "" -> fsm.Fsm.f_name | p -> p in
  dump_module_intf (need_globals m) (dir ^ "/" ^ prefix ^ ".h") f;
  dump_module_impl (need_globals m) (dir ^ "/" ^ prefix ^ ".cpp") f

let dump_input ?(prefix="") ?(dir="./systemc") m ((id,_) as inp) =
  let prefix = match prefix with "" -> cfg.sc_inpmod_prefix ^ id | p -> p in
  dump_inp_module_intf (need_globals m) (dir ^ "/" ^ prefix ^ ".h") inp;
  dump_inp_module_impl (dir ^ "/" ^ prefix ^ ".cpp") inp

(* let dump_model ?(dir="./systemc") m = 
 *   List.iter (dump_input ~dir:dir m) m.Static.m_inputs;
 *   List.iter (dump_fsm_inst ~dir:dir m) m.Static.m_fsms *)

let dump_testbench ?(name="") ?(dir="./systemc") m =
  let tb_name = match name with "" -> cfg.sc_tb_name | p -> p in
  dump_testbench_impl tb_name (dir ^ "/" ^ tb_name ^ ".cpp") m 

(* Check whether a model can be translated *)

let check_allowed m =
  match Fsm.cfg.Fsm.act_sem with
  | Fsm.Sequential -> ()
  | Fsm.Synchronous -> Misc.not_implemented "SystemC: synchronous actions"

(* let dump_lib dir = *)
(*   dump_lib_intf (dir ^ "/" ^ cfg.sc_lib_name ^ ".h"); *)
(*   dump_lib_impl (dir ^ "/" ^ cfg.sc_lib_name ^ ".cpp") *)
