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

(* Abstract syntax -> internal models *)

open Syntax
   
(* Types from type expressions *)

exception Unbound_expr_index of string * type_index_expr 

let type_index_of_index_expr e =
  let rec type_index_of = function 
    TEConst c -> Types.Index.TiConst c
  | TEVar v -> Types.Index.TiVar v
  | TEBinop (op,e1,e2) -> Types.Index.TiBinop (op, type_index_of e1, type_index_of e2) in
  type_index_of e.ti_desc
    
exception Unbound_type_ctor of string
                             
let rec type_of_type_expr defns params ~strict:strict te = match te with
  | TEBool -> Types.TyBool
  | TEInt None -> Types.TyInt None
  | TEFloat -> Types.TyFloat
  | TEInt (Some (lo,hi)) -> Types.TyInt (Some (type_index_of_index_expr lo, type_index_of_index_expr hi))
  | TEEvent -> Types.TyEvent
  | TEName n ->
     try List.assoc n defns
     with Not_found -> raise (Unbound_type_ctor n)

and type_of_type_expression defns params ~strict:strict te =
  type_of_type_expr defns params ~strict:strict te.te_desc

let mk_fsm_model (tdefns,tctors) { fsm_desc = f; fsm_loc = loc } = 
  let mk_typed what (id,te) =
    if List.mem id tctors
    then Error.warning (Printf.sprintf "declaration of %s %s in FSM model %s shadows enum value" what id f.fd_name);
    id, type_of_type_expression tdefns [] ~strict:true te in 
  let mk_cond c = match c.cond_desc with
      [ev],guards -> ev, guards
    | _ -> Error.fatal_error "Intern.mk_fsm_model" in
  let mk_prio p = if p then 1 else 0 in
  let mk_act a = a.act_desc in
  Fsm.build_model
    ~name:f.fd_name
    ~states:f.fd_states
    ~params:(List.map (mk_typed "parameter") f.fd_params)
    ~ios:(List.map (function (dir,desc) -> let id,ty = mk_typed "input/output" desc in dir,id,ty) f.fd_ios)
    ~vars:(List.map (mk_typed "variable") f.fd_vars)
    ~trans:(List.map (function (s,cond,acts,s',p) -> s, mk_cond cond, List.map mk_act acts, s', mk_prio p) f.fd_trans)
    ~itrans:(let q0,acts = f.fd_itrans in q0, List.map mk_act acts)

let mk_fsm_inst tdefns models globals { fi_desc=f; fi_loc=loc } = 
  let model =
    try List.find (function m -> m.Fsm.fm_name = f.fi_model) models
    with Not_found -> Error.unbound_fsm loc f.fi_model in  
  let params =
    try List.map2 (fun (p,ty) v -> p, v) model.Fsm.fm_params f.fi_params
    with Invalid_argument _ -> Error.fsm_mismatch "parameter(s)" loc f.fi_name in
  let mk_global id =
    try List.assoc id globals
    with Not_found -> Error.unbound_global loc id in
  Fsm.build_instance
      ~name:f.fi_name
      ~model:model
      ~params:params
      ~ios:(List.map mk_global f.fi_args)

let mk_stim_desc = function
  | Periodic (p,t1,t2) -> Fsm.Periodic (p,t1,t2)
  | Sporadic ts -> Fsm.Sporadic ts
  | ValueChange vcs -> Fsm.ValueChange vcs
                     
let mk_global tdefns { g_desc = g; g_loc = loc } = 
  let ty = type_of_type_expression tdefns [] ~strict:true g.gd_type in
  match g.gd_desc with
  | GInp stim -> g.gd_name, Fsm.GInp (g.gd_name, ty, mk_stim_desc stim.stim_desc)
  | GOutp -> g.gd_name, Fsm.GOutp (g.gd_name, ty)
  | GShared -> g.gd_name, Fsm.GShared (g.gd_name, ty)

let mk_type_defn (defns,ctors) { td_desc = d; td_loc = loc } =
    match d with
    | TD_Alias (id, te) -> (id, type_of_type_expr defns [] ~strict:true te) :: defns, ctors
    | TD_Enum (id, cs) -> (id, TyEnum cs) :: defns, ctors @ cs
                      
let build_system name p = 
  let (tdefns,tctors) = List.fold_left mk_type_defn ([],[]) p.p_type_decls in
  let models = List.map (mk_fsm_model (tdefns,tctors)) p.p_fsm_models in
  let globals = List.map (mk_global tdefns) p.p_globals in
  let fsms = List.map (mk_fsm_inst tdefns models globals) p.p_fsm_insts in
  Sysm.build name fsms
