(* Abstract syntax -> internal models *)

open Syntax
   
let mk_fsm_model { fsm_desc = f; fsm_loc = loc } = 
  let mk_typed (id,te) = id, Typing.type_of_type_expression [] ~strict:true te in 
  let mk_cond c = match c.cond_desc with
      [ev],guards -> ev, guards
    | _ -> Error.fatal_error "Intern.mk_fsm_model" in
  let mk_act a = a.act_desc in
  Fsm.build_model
    ~name:f.fd_name
    ~states:f.fd_states
    ~params:(List.map mk_typed f.fd_params)
    ~ios:(List.map (function (dir,desc) -> let id,ty = mk_typed desc in dir,id,ty) f.fd_ios)
    ~vars:(List.map mk_typed f.fd_vars)
    ~trans:(List.map (function (s,cond,acts,s') -> s, mk_cond cond, List.map mk_act acts, s') f.fd_trans)
    ~itrans:(let q0,acts = f.fd_itrans in q0, List.map mk_act acts)

let mk_fsm_inst models globals { fi_desc=f; fi_loc=loc } = 
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
                     
let mk_global { g_desc = g; g_loc = loc } = 
  let ty = Typing.type_of_type_expression [] ~strict:true g.gd_type in
  match g.gd_desc with
  | GInp stim -> g.gd_name, Fsm.GInp (g.gd_name, ty, mk_stim_desc stim.stim_desc)
  | GOutp -> g.gd_name, Fsm.GOutp (g.gd_name, ty)
  | GShared -> g.gd_name, Fsm.GShared (g.gd_name, ty)

let build_composite name p = 
  let models = List.map mk_fsm_model p.p_fsm_models in
  let globals = List.map mk_global p.p_globals in
  let fsms = List.map (mk_fsm_inst models globals) p.p_fsm_insts in
  Comp.build_composite name fsms
