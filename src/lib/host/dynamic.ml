(** Dynamic semantics *)


type act_semantics =
  | Sequential
  | Synchronous
  
type cfg = {
    mutable act_semantics: act_semantics;
    mutable verbose_level: int;
  }
         
let cfg = {
    act_semantics = Sequential;
    verbose_level = 2;
  }

module type DYNAMIC = sig
  module Syntax: Syntax.SYNTAX
  module Static: Static.STATIC 
  module Eval: Guest.EVAL 
  module Seq: Seq.SEQ 

  exception Illegal_stimulus_value of Location.t
  exception Non_deterministic_transition of string * int * Syntax.transition list (** FSM name, date, transitions *)

  val run: verbose_level:int -> Syntax.program -> Static.t -> Seq.t
end

module Make
         (Syntax: Syntax.SYNTAX)
         (Static: Static.STATIC with module Syntax = Syntax)
         (Eval: Guest.EVAL with module Syntax = Syntax.Guest and module Value = Static.Value)
     : DYNAMIC with module Syntax = Syntax
          and module Static = Static
          and module Eval = Eval =
struct

  module Syntax = Syntax
  module Static = Static
  module Eval = Eval
  module Event = Event.Make(Syntax.Guest)(Static.Value)
  module Evset = Evset.Make(Event)
  module Seq = Seq.Make(Evset)

  module Trace = struct  (* Execution traces *)
    type t = { mutable contents: Seq.t }
    let create  () = { contents = [] }
    let reset t = t.contents <- []
    let add e t = if not (Evset.is_empty e) then t.contents <- e::t.contents
    let events t = List.rev t.contents
  end
  
  exception Illegal_stimulus_value of Location.t
  exception Non_deterministic_transition of string * int * Syntax.transition list 
  
  let trace = Trace.create () (* Global trace *)
          
  let var_name m x = m.Static.name ^ "." ^ x
  let state_name m = var_name m "state"
  
  let mk_event t e = Evset.mk t [e]
    
  let is_fireable env m e = (* \Delta(M,q,e) *)
    let open Static in
    List.filter
      (fun {Annot.desc=(q,{Annot.desc=e',gs; _},_,_); _} ->  
        q = m.Static.q
        && e = Event.Ev e'
        && List.for_all (fun g -> Eval.eval_bool env g = true) gs)
     m.model.desc.trans
  
  let fireable env m evs = (* \Delta(M,q,{e1,...,en} *)
    List.map (is_fireable env m) evs |> List.concat
  
  let choose_transition (f,t,trs) = (* Function CHOICE *)
    raise (Non_deterministic_transition (f,t,trs))
  
  let r_act sd (vars,env) ({Annot.desc=act; _},t) =  (* Rules ActUpdL, ActUpdG, ActEmitL and ActEmitG *)
  (* \Nu, \Gamma -- act,t | \rho_e --> \Nu', \Gamma' *)
    match act with
    | Syntax.Emit e ->
       Trace.add (Evset.mk t [Event.Ev e]) trace;
       if List.mem_assoc e sd.Static.shared then  (* ActEmitL *)
         (vars, env),
         (Evset.mk t [Ev e])
       else                                   (* ActEmitG *)
         (vars, env),
         (Evset.empty t)
    | Syntax.Assign (lhs,expr) ->
       let genv = Env.union vars env in
       let x = Syntax.Guest.lhs_name lhs in
       let upd env =
         let v = Eval.eval_expr genv expr in
         Trace.add (mk_event t (Event.Upd (lhs,v))) trace; 
         Eval.upd_env lhs v env in
       if Env.mem x vars then (* ActUpdL *)
         (upd vars, env),
         Evset.empty t
       else if Env.mem x env then (* ActUpdG *)
         (vars, upd env),
         Evset.empty t
       else (* Should not happen *)
         Misc.fatal_error "Dynamic.r_act"
  
  let r_acts sd (vars,env) (acts,t) = (* Rule ACTS *)
  (* \Nu, \Gamma -- <acts,t> | \rho_e --> \Nu', \Gamma' *)
    let (vars',env'), rs =
      List.fold_left_map
        (r_act sd)
        (vars,env)
        (List.map (fun act -> (act,t)) acts) in
    vars', env', Evset.union_all t rs
  
  let r_trans sd (m,env) ({Annot.desc=q,_,acts,q'; _},t) =  (* Rule TRANS *)
    (* \mu, \Gamma -- \tau,t | \rho_e --> \mu', \Gamma' *)
    let vars', env', r_e = r_acts sd (m.Static.vars,env) (acts,t) in
    let m' = { m with q=q'; vars = vars' } in
    Trace.add (mk_event t (Event.StateMove (state_name m,q'))) trace;
    m', env', r_e
  
  let r_reaction sd (m,env) s_e = (* Rules React1, React0 and ReactN *)
    (*  \mu, \Gamma -- \sigma_e | \rho_e --> \mu', \Gamma' *)
    let t = Evset.date s_e in
    match fireable (Env.union env m.Static.vars) m (Evset.events s_e) with
    | [tr] -> r_trans sd (m,env) (tr,t)   (* REACT_1 *)
    | [] -> m, env, Evset.empty t (* REACT_0 *)
    | trs -> r_trans sd (m,env) (choose_transition (m.name,t,trs), t) (* REACT_N *)
  
  let r_react_upd sd (m,env) evs = (* Rule ReactUpd *)
    (* M, \Gamma -- \sigma_v --> M', \Gamma' *)
    (* TODO: use a GADT to remove dynamic checking of event kind ? *)
    let upd env e = match e with
      | Event.Upd (l,v') ->
         let x = Syntax.Guest.lhs_name l in
         Env.upd x v' env
      | _ -> Misc.fatal_error "Simul.r_react_upd"  (* Should not happen *) in
    Trace.add evs trace; 
    let env' = List.fold_left upd env (Evset.events evs) in
    m, env'
  
  let dump1 level fmt f arg = 
    if cfg.verbose_level >= level then  Format.fprintf Format.std_formatter fmt f arg
  
  let dump2 level fmt f1 arg1 f2 arg2 =
    if cfg.verbose_level >= level then  Format.fprintf Format.std_formatter fmt f1 arg1 f2 arg2
  
  let dump3 level fmt f1 arg1 f2 arg2 f3 arg3 =
    if cfg.verbose_level >= level then  Format.fprintf Format.std_formatter fmt f1 arg1 f2 arg2 f3 arg3
  
  let dump4 level fmt f1 arg1 f2 arg2 f3 arg3 f4 arg4 =
    if cfg.verbose_level >= level then  Format.fprintf Format.std_formatter fmt f1 arg1 f2 arg2 f3 arg3 f4 arg4
  
  let r_react_ev sd (m,env) s_e = (* Rule ReactEv *)
    (* M, \Gamma -- \sigma_e | \rho --> M', \Gamma' *)
    let ms = Static.dep_sort m in 
    dump1 1 ">> >> REACT_EV: using order:  %a\n" (Misc.pp_list_v (Static.pp_fsm ~verbose_level:0)) ms;
    Trace.add s_e trace; 
    let (env',_,rho), ms' =
      List.fold_left_map
        (fun (env,s,r) mu ->
          let mu', env', r' = r_reaction sd (mu,env) s in
          ((env',Evset.union s r',Evset.union r r'), mu'))
        (env,s_e,Evset.empty (Evset.date s_e))
        ms in
    ms', env', rho
  
  let r_react sd (m,env) st =  (* Rule REACT *) 
    let pp_env = Env.pp Eval.Value.pp in
    let pp_fsms = Misc.pp_list_v (Static.pp_fsm ~verbose_level:1) in
    let t = Evset.date st in
    (* M, \Gamma -- \sigma | \rho_e --> M', \Gamma' *)
    dump4 1 ">> t=%a: REACT: M=%a G=%a -- %a --> ...\n" Format.pp_print_int t pp_fsms m pp_env env Evset.pp st; 
    let st_e, st_v = Evset.partition ~f:Event.is_pure_event st in
    let _, env_v = r_react_upd sd (m,env) st_v in
    dump1 2 ">> >> REACT_UPD: G_v=%a\n" pp_env env_v;
    let m', env', r_e = r_react_ev sd (m,env_v) st_e in
    dump3 2 ">> >> REACT_EV: M'=%a G'=%a r_e=%a\n" pp_fsms m' pp_env env' Evset.pp r_e;
    dump4 1 ">> t=%a: REACT: ... -- [%a] -> M'=%a G'=%a\n"
      Format.pp_print_int t Evset.pp r_e pp_fsms m' pp_env env';
    (m',env'), r_e
  
  let is_event_type ty = Static.Typing.Types.is_type_constr0 "event" ty

  let r_init sd m = (* Rule INIT *)
    (* M --> M_0, \Gamma_0 *)
    let env0 =
      Env.init
        (List.map
           (fun (v,_) -> v, Eval.Value.default_value None)
           (List.filter
              (fun (_,ty) -> not (is_event_type ty))
              (sd.Static.inputs @ sd.Static.outputs @ sd.Static.shared))) in
    let m', env' =
      List.fold_left_map 
        (fun env mu ->
          let nu = mu.Static.vars in
          let { Annot.desc=q0,acts; _ } = mu.Static.model.Annot.desc.itrans in
          let nu',env', r_e = r_acts sd (nu, env) (acts,0) in
          Trace.add (mk_event 0 (Event.StateMove (state_name mu,q0))) trace; 
          Trace.add r_e trace; 
          env', { mu with Static.q=q0 ; Static.vars = Env.union nu' mu.Static.params })
        env0
        m in
    m', env'
      
  let is_input ctx evs =
    let check ev = match ev with
      | Event.Ev x ->
         if List.mem_assoc x ctx.Static.inputs && is_event_type (List.assoc x ctx.Static.inputs) then
           ()
         else
           Misc.fatal_error ("Dynamic.is_input: " ^ x ^ " is not an event typed input")
      | Event.Upd (l,_) ->
         let x = Syntax.Guest.lhs_name l in
         if List.mem_assoc x ctx.Static.inputs && not (is_event_type (List.assoc x ctx.Static.inputs)) then
           ()
         else
           Misc.fatal_error ("Dynamic.is_input: " ^ x ^ " is an event typed input")
      | Event.StateMove _ -> () in
    List.iter check (Evset.events evs)
    
  let r_exec (h: Static.t) (sts: Evset.t list) = (* Rule EXEC *)
    (* H=<M,C> -- \sigma_1, ..., \sigma_n | \rho_1, ..., \rho_n --> M_n, \Gamma_n *)
    (* First check that all stimuli refer to input signals listed in H *)
    List.iter (is_input h.Static.ctx) sts;
    Trace.reset trace;
    let env0, m0 = r_init h.ctx h.fsms in
    let _, _ = List.fold_left_map (r_react h.ctx) (m0,env0) sts in
    Trace.events trace
  
    let extract_stimuli p = 
      let eval (t,expr) = 
        try t, Eval.eval_expr Env.empty expr 
        with _ -> raise (Illegal_stimulus_value expr.Annot.loc) in
      let expand id st =
        (* let ty = match st.Annot.typ with
         *   | Some ty -> ty
         *   | None -> Misc.fatal_error "Host.extract_stimuli: cannot recover type" in *)
        match st.Annot.desc with
        | Syntax.Periodic (p,t1,t2) -> Seq.mk_periodic id p t1 t2
        | Syntax.Sporadic ts -> Seq.mk_sporadic id ts
        | Syntax.Value_change vcs -> Seq.mk_changes id (List.map eval vcs) in
      let sts =
        List.fold_left
          (fun acc io ->
            match io.Annot.desc with
            | id, Syntax.Input, _, Some st -> expand id st :: acc
            | _ -> acc)
          []
          p.Syntax.ios in
      Seq.merge_all sts

   let run ~verbose_level (p: Syntax.program) (s: Static.t) =
     cfg.verbose_level <- verbose_level;
     let sts = extract_stimuli p in
     match cfg.act_semantics with
     | Sequential -> r_exec s sts
     | Synchronous -> Misc.not_implemented "Dynamic.run with synchronous semantics for actions"
   
end