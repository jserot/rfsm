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

(* Type expressions *)

type type_expression = {
  te_desc: Type_expr.t;
  te_loc: Location.location;
  te_typ: Types.typ;
  }

(* Expressions *)
             
type expression = {
  e_desc: Expr.t;
  e_loc: Location.location;
  e_typ: Types.typ;
  }

(* Type declarations *)

type type_declaration = {
  td_desc: type_decl;
  td_loc: Location.location;
  td_typ: Types.typ;
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
  fd_typ: Types.typ;
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
  stim_desc: Global.stim_desc;
  stim_loc: Location.location;
  }

(* and stim_desc = 
 *   Periodic of int * int * int             (\* Period, start time, end time *\)
 * | Sporadic of int list                    (\* Dates *\)
 * | ValueChange of (int * Expr.value) list  (\* (Date,value)s *\) *)

(* Global (IOs and channels) declarations *)
              
type global_mdecl = {
  mg_dsc: mg_dsc;
  mg_loc: Location.location;
  }

and mg_dsc = {
    mg_names: string list;
    mg_type: type_expression;
    mg_desc: gd_desc
  }

and gd_desc =
  | GInp of stimuli
  | GOutp
  | GShared 
    
type global_decl = {
  g_desc: g_desc;
  g_loc: Location.location;
  }

and g_desc = {
    gd_name: string;
    gd_type: type_expression;
    gd_desc: gd_desc
  }
      
let split_global_mdecl (md:global_mdecl) = 
  List.map 
    (function id ->
       { g_desc = { gd_name=id; gd_type=md.mg_dsc.mg_type; gd_desc=md.mg_dsc.mg_desc };
         g_loc = md.mg_loc })
    md.mg_dsc.mg_names
  
(* FSM instanciations *)
               
type fsm_inst = {
  fi_desc: fsm_inst_desc;
  fi_loc: Location.location;
  }

and fsm_inst_desc = {
  fi_name: string;
  fi_model: string;
  fi_params: Expr.t list;
  fi_args: string list;
  }

(* Declaration *)

type decl =
  | TypeDecl of type_declaration
  | FnDecl of fn_declaration
  | CstDecl of cst_declaration
  | FsmModelDecl of fsm_model
  | FsmInstDecl of fsm_inst
  | GlobalDecl of global_mdecl
              
(* Programs *)

type program = {
  p_type_decls: type_declaration list;
  p_fn_decls: fn_declaration list;
  p_cst_decls: cst_declaration list;
  p_fsm_models: fsm_model list;
  p_globals: global_decl list;
  p_fsm_insts: fsm_inst list
  }

let empty_program = { p_type_decls=[]; p_fn_decls=[]; p_cst_decls=[]; p_fsm_models=[]; p_globals=[]; p_fsm_insts=[] }

let add_program p1 p2 = { (* TODO : Flag redefinitions ? *)
    p_type_decls= p1.p_type_decls @ p2.p_type_decls;
    p_fn_decls= p1.p_fn_decls @ p2.p_fn_decls;
    p_cst_decls= p1.p_cst_decls @ p2.p_cst_decls;
    p_fsm_models= p1.p_fsm_models @ p2.p_fsm_models;
    p_fsm_insts= p1.p_fsm_insts @ p2.p_fsm_insts;
    p_globals= p1.p_globals @ p2.p_globals;
  }

(* Printing *)

(* let string_of_value_change (t,v) = string_of_int t ^ ":" ^ Expr.string_of_value v
 * 
 * let string_of_stim = function
 *  | Periodic (x,y,z) -> "Periodic(" ^ ListExt.to_string string_of_int "," [x;y;z] ^ ")"
 *  | Sporadic ts -> "Sporadic(" ^ ListExt.to_string string_of_int "," ts ^ ")"
 *  | ValueChange vs -> "ValueChange(" ^ ListExt.to_string string_of_value_change "," vs ^ ")" *)

type syntax_mode = Syntax_old | Syntax_new

let string_of_stimuli st = Global.string_of_stim st.stim_desc
   
let string_of_type_expression t = Type_expr.string_of_type_expr t.te_desc

let string_of_io_tag = function Types.IO_In -> "in" | Types.IO_Out -> "out" | Types.IO_Inout -> "inout"
                                                                            
let string_of_io pfx (tag,(id,ty)) = pfx ^ string_of_io_tag tag ^ " " ^ id ^ ": " ^ string_of_type_expression ty

and string_of_field (n,ty) = "  " ^ n ^ ": " ^ Type_expr.string_of_type_expr ty

let dump_type_decl mode oc { td_desc=d } = match d with
    TD_Alias (name,te) ->
     Printf.fprintf oc "type %s = %s\n" name (Type_expr.string_of_type_expr te)
  | TD_Enum (name, cs) ->
     Printf.fprintf oc "type %s = { %s }\n" name (Utils.ListExt.to_string (function c -> c) ", " cs)
  | TD_Record (name, fs) ->
     Printf.fprintf oc "type %s = record {\n%s\n}\n" name (Utils.ListExt.to_string string_of_field ",\n" fs)

let string_of_fn_arg (id,ty) = id ^ ":" ^ string_of_type_expression ty

let string_of_expression e = Expr.to_string e.e_desc
                           
let dump_fn_decl mode oc { fd_desc=d } =
  Printf.fprintf oc "function %s (%s) : %s { return %s }\n"
    d.ff_name
    (Utils.ListExt.to_string string_of_fn_arg "," d.ff_args)
    (string_of_type_expression d.ff_res)
    (string_of_expression d.ff_body)

let string_of_opt f left right l = match l with [] -> "" | _ -> left ^ f l ^ right

let string_of_actions sep acts = Utils.ListExt.to_string (fun a -> Action.to_string a.act_desc) sep acts

let string_of_transition mode (q,cond,acts,q',p) =
  match mode, cond.cond_desc with
  | Syntax_old, _ ->
      Printf.sprintf "    %s%s -- %s %s %s"
        (if p then "*" else "")
        q
        (Condition.to_string cond.cond_desc)
        (string_of_opt (string_of_actions "; ") "| " "" acts)
        q'
  | Syntax_new, ([ev],guards) ->
      Printf.sprintf "  %s %s -> %s on %s%s%s"
        (if p then "!" else "|")
        q
        q'
        ev
        (string_of_opt Condition.string_of_guards " when " "" guards)
        (string_of_opt (string_of_actions ", ") " with " "" acts)
  | _, _ ->
     Misc.fatal_error "Syntax.string_of_transition"

let string_of_transitions mode ts =
  match mode with
  | Syntax_old -> Utils.ListExt.to_string (string_of_transition mode) ",\n" ts
  | Syntax_new -> Utils.ListExt.to_string (string_of_transition mode) "\n" ts


let string_of_itransition mode (q,iacts) =
  match mode with
  | Syntax_old -> Printf.sprintf "  | %s -> %s" (string_of_actions ";" iacts) q
  | Syntax_new -> Printf.sprintf "  | -> %s %s" q (string_of_opt (string_of_actions ",") "with " "" iacts)

let dump_fsm_model mode oc { fsm_desc=m } =
  let string_of_comp_t (id,ty) = id ^ ": " ^ string_of_type_expression ty in
  Printf.fprintf oc "fsm model %s%s(\n%s)\n{\n"
                 m.fd_name
                 (string_of_opt (Utils.ListExt.to_string string_of_comp_t ", ") "<" ">" m.fd_params)
                 (Utils.ListExt.to_string (string_of_io "  ") ",\n" m.fd_ios);
  Printf.fprintf oc "  states: %s;\n" (Utils.ListExt.to_string Misc.id ", " m.fd_states);
  if m.fd_vars <> [] then 
    Printf.fprintf oc "  vars: %s;\n" (Utils.ListExt.to_string string_of_comp_t ", " m.fd_vars);
  Printf.fprintf oc "  trans:\n%s;\n" (string_of_transitions mode m.fd_trans);
  Printf.fprintf oc "  itrans:\n%s;\n" (string_of_itransition mode m.fd_itrans);
  Printf.fprintf oc "}\n"

let dump_global mode oc {g_desc=g} = match g.gd_desc with
  | GInp st -> Printf.fprintf oc "input %s : %s = %s\n" g.gd_name (string_of_type_expression g.gd_type) (string_of_stimuli st)
  | GOutp -> Printf.fprintf oc "output %s : %s\n" g.gd_name (string_of_type_expression g.gd_type)
  | GShared -> Printf.fprintf oc "shared %s : %s\n" g.gd_name (string_of_type_expression g.gd_type)

let dump_fsm_inst mode oc {fi_desc=f} =
  Printf.fprintf oc "fsm %s = %s%s(%s)\n"
    f.fi_name
    f.fi_model
    (string_of_opt (Utils.ListExt.to_string Expr.to_string ",") "<" ">" f.fi_params)
    (Utils.ListExt.to_string Misc.id "," f.fi_args)
  
let dump_program oc mode p =
  List.iter (dump_type_decl mode oc) p.p_type_decls;
  Printf.fprintf oc "\n";
  List.iter (dump_fn_decl mode oc) p.p_fn_decls;
  Printf.fprintf oc "\n";
  List.iter (dump_fsm_model mode oc) p.p_fsm_models;
  Printf.fprintf oc "\n";
  List.iter (dump_global mode oc) p.p_globals;
  Printf.fprintf oc "\n";
  List.iter (dump_fsm_inst mode oc) p.p_fsm_insts
