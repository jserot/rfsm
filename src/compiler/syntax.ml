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

(* Abstract syntax of programs *)

open Utils

(* Type expressions *)

type type_expression = {
  te_desc: Type_expr.t;
  te_loc: Location.location;
  }

(* Expressions *)
             
type expression = {
  e_desc: Expr.t;
  e_loc: Location.location;
  }

(* Type declarations *)

type type_declaration = {
  td_desc: type_decl;
  td_loc: Location.location;
  }

and type_decl =
  TD_Enum of string * string list  (* Name, constructors *)
| TD_Record of string * record_field list (* Name, field descrs *)
| TD_Alias of string * Type_expr.t   (* Name, abbreviated type expr *)

and record_field = string * Type_expr.t 

(* Function declarations *)

type fn_declaration = {
  fd_desc: fn_decl;
  fd_loc: Location.location;
  }

and fn_decl = {
  ff_name: string;
  ff_args: (string * type_expression) list;
  ff_res: type_expression;
  ff_body: expression;
  }

(* Constant declarations *)

type cst_declaration = {
  cst_desc: cst_decl;
  cst_loc: Location.location;
  }

and cst_decl = {
  cc_name: string;
  cc_typ: type_expression;
  cc_val: Expr.value;
  }
            
(* FSM declarations *)

type fsm_model = {
  fsm_desc: fsm_desc;
  fsm_loc: Location.location;
  }

and fsm_desc = { 
  fd_name: string;
  fd_states: string list;
  fd_params: (string * type_expression) list;
  fd_ios: (Types.dir * (string * type_expression)) list;
  fd_vars: (string * type_expression) list;
  fd_trans: (string * condition * action list * string * bool) list;
  fd_itrans: string * action list;
  }

and condition = {
  cond_desc: Condition.t;
  cond_loc: Location.location;
  }

and action = {
  act_desc: Action.t;
  act_loc: Location.location;
  }

(* STIMULI *)

type stimuli = {
  stim_desc: stim_desc;
  stim_loc: Location.location;
  }

and stim_desc = 
  Periodic of int * int * int             (* Period, start time, end time *)
| Sporadic of int list                    (* Dates *)
| ValueChange of (int * Expr.value) list  (* (Date,value)s *)

(* Global (IOs and channels) declarations *)
              
type global_decl = {
  g_desc: g_desc;
  g_loc: Location.location;
  }

and g_desc = {
    gd_name: string;
    gd_type: type_expression;
    gd_desc: gd_desc
  }
      
and gd_desc =
  | GInp of stimuli
  | GOutp
  | GShared 
    
(* FSM instanciations *)
               
type fsm_inst = {
  fi_desc: fsm_inst_desc;
  fi_loc: Location.location;
  }

and fsm_inst_desc = {
  fi_name: string;
  fi_model: string;
  fi_params: Expr.value list;
  fi_args: string list;
  }

(* Programs *)

type program = {
  p_type_decls: type_declaration list;
  p_fn_decls: fn_declaration list;
  p_cst_decls: cst_declaration list;
  p_fsm_models: fsm_model list;
  p_globals: global_decl list;
  p_fsm_insts: fsm_inst list
  }

let empty={ p_type_decls=[]; p_fn_decls=[]; p_cst_decls=[]; p_fsm_models=[]; p_globals=[]; p_fsm_insts=[] }

(* Printing *)

let string_of_value_change (t,v) = string_of_int t ^ ":" ^ Expr.string_of_value v

let string_of_stim = function
 | Periodic (x,y,z) -> "Periodic(" ^ ListExt.to_string string_of_int "," [x;y;z] ^ ")"
 | Sporadic ts -> "Sporadic(" ^ ListExt.to_string string_of_int "," ts ^ ")"
 | ValueChange vs -> "ValueChange(" ^ ListExt.to_string string_of_value_change "," vs ^ ")"

let string_of_stimuli st = string_of_stim st.stim_desc
   
let string_of_type_expression t = Type_expr.string_of_type_expr t.te_desc

let string_of_io_tag = function Types.IO_In -> "in" | Types.IO_Out -> "out" | Types.IO_Inout -> "inout"
                                                                            
let string_of_io (tag,(id,ty)) = string_of_io_tag tag ^ " " ^ id ^ ": " ^ string_of_type_expression ty

and string_of_field (n,ty) = n ^ ":" ^ Type_expr.string_of_type_expr ty

let dump_type_decl oc { td_desc=d } = match d with
    TD_Alias (name,te) ->
     Printf.printf "TYPE %s = %s\n" name (Type_expr.string_of_type_expr te)
  | TD_Enum (name, cs) ->
     Printf.printf "TYPE %s = { %s }\n" name (ListExt.to_string (function c -> c) "," cs)
  | TD_Record (name, fs) ->
     Printf.printf "TYPE %s = { %s }\n" name (ListExt.to_string string_of_field "," fs)

let string_of_fn_arg (id,ty) = id ^ ":" ^ string_of_type_expression ty

let string_of_expression e = Expr.to_string e.e_desc
                           
let dump_fn_decl oc { fd_desc=d } =
  Printf.printf "FUNCTION %s (%s) : %s { return %s }\n"
    d.ff_name
    (ListExt.to_string string_of_fn_arg "," d.ff_args)
    (string_of_type_expression d.ff_res)
    (string_of_expression d.ff_body)

let dump_fsm_model oc { fsm_desc=m } =
  let of_list f xs = ListExt.to_string f ", " xs in
  let string_of_comp_t (id,ty) = id ^ ": " ^ string_of_type_expression ty in
  let string_of_acts = ListExt.to_string (function a -> Action.to_string a.act_desc) "; " in
  Printf.fprintf oc "FSM %s <%s> (%s) {\n"
                 m.fd_name
                 (of_list string_of_comp_t m.fd_params)
                 (of_list string_of_io m.fd_ios);
  Printf.fprintf oc "  STATES = { %s }\n" (of_list (function n -> n) m.fd_states);
  Printf.fprintf oc "  VARS = { %s }\n" (of_list string_of_comp_t m.fd_vars);
  Printf.fprintf oc "  TRANS = {\n";
  List.iter 
    (fun (q,cond,acts,q',p) ->
      Printf.fprintf oc "    { %s%s {%s} {%s} %s }\n"
        (if p then "*" else "") q (Condition.to_string cond.cond_desc) (string_of_acts acts) q')
    (m.fd_trans);
  Printf.fprintf oc "    }\n";
  let (q,iacts) = m.fd_itrans in
  Printf.fprintf oc "  ITRANS = { %s \"%s\" }\n" q (string_of_acts iacts);
  Printf.fprintf oc "  }\n"

let dump_global oc {g_desc=g} = match g.gd_desc with
  | GInp st -> Printf.fprintf oc "INPUT %s : %s = %s\n" g.gd_name (string_of_type_expression g.gd_type) (string_of_stimuli st)
  | GOutp -> Printf.fprintf oc "OUTPUT %s : %s\n" g.gd_name (string_of_type_expression g.gd_type)
  | GShared -> Printf.fprintf oc "SHARED %s : %s\n" g.gd_name (string_of_type_expression g.gd_type)

let dump_fsm_inst oc {fi_desc=f} =
  Printf.fprintf oc "FSM %s = %s<%s>(%s)\n"
    f.fi_name
    f.fi_model
    (ListExt.to_string Expr.string_of_value "," f.fi_params)
    (ListExt.to_string Misc.id "," f.fi_args)
  
let dump_program p =   (* for debug only *)
  List.iter (dump_type_decl stdout) p.p_type_decls;
  List.iter (dump_fn_decl stdout) p.p_fn_decls;
  List.iter (dump_fsm_model stdout) p.p_fsm_models;
  List.iter (dump_global stdout) p.p_globals;
  List.iter (dump_fsm_inst stdout) p.p_fsm_insts
