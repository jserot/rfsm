(** Dynamic semantics *)

type cfg = {
    mutable act_semantics: Misc.act_semantics;
    mutable verbose_level: int;
  }
         
let cfg = {
    act_semantics = Misc.Sequential;
    verbose_level = 0;
  }

module type DYNAMIC = sig
  module Syntax: Syntax.SYNTAX
  module Static: Static.T 
  module Eval: Guest.EVAL 
  module EvSeq: Evseq.EVSEQ

  exception Illegal_stimulus_value of Location.t
  exception Non_deterministic_transition of string * int * Syntax.transition list (** FSM name, date, transitions *)

  val run: Syntax.program -> Static.t -> EvSeq.t
end

module Make
         (Syntax: Syntax.SYNTAX)
         (Static: Static.T with module Syntax = Syntax)
         (Eval: Guest.EVAL with module Syntax = Syntax.Guest and module Value = Static.Value)
     : DYNAMIC with module Syntax = Syntax
          and module Static = Static
          and module Eval = Eval =
struct

  module Syntax = Syntax
  module Static = Static
  module Types = Syntax.Guest.Types
  module Eval = Eval
  module Event = Event.Make(Syntax.Guest)(Static.Value)
  module Evset = Evset.Make(Event)
  module EvSeq = Evseq.Make(Evset)

  module Trace = struct  (* Execution traces *)
    type t = { mutable contents: EvSeq.t }
    let create  () = { contents = [] }
    let reset t = t.contents <- []
    let add e t = if not (Evset.is_empty e) then t.contents <- e::t.contents
    let events t = List.rev t.contents
  end
  
  exception Illegal_stimulus_value of Location.t
  exception Non_deterministic_transition of string * int * Syntax.transition list 
  
  let trace = Trace.create () (* Global trace *)
          
  let var_name m x = Ident.to_string m.Static.name ^ "." ^ x  (* TO FIX ? Prefix should already be in the ident *)
  let state_name m = var_name m "state"
  
  let mk_event t e = Evset.mk t [e]
    
  let is_fireable env m e = (* \Delta(M,q,e) *)
    let open Static in
    List.filter
      (fun {Annot.desc=(q,{Annot.desc=e',gs; _},_,_,_); _} ->  
        q = m.Static.q
        && e = Event.Ev e'
        && List.for_all (fun g -> Eval.eval_bool env g = true) gs)
     m.model.desc.trans
  
  let fireable env m evs = (* \Delta(M,q,{e1,...,en} *)
    List.map (is_fireable env m) evs |> List.concat
  
  let choose_transition (f,t,trs) = (* Function CHOICE *)
    match
      trs 
      |> Misc.list_scatter (function { Annot.desc = _,_,_,_,p; _ } -> p) (* Transitions, scattered by priority levels ... *)
      |> List.sort (fun (p1,_) (p2,_) -> Stdlib.compare p1 p2) (* ... then sorted by descending order of priority level ...*)
      |> List.hd |> snd (* ... gives all transitions with the highest priority level *)
    with 
    | [] -> Misc.fatal_error "Dynamic.choose_transition" (* Should not happen *)
    | [tr] -> tr 
    | trs' -> raise (Non_deterministic_transition (f,t,trs'))
  
  let r_act ~f sd (vars,env) ({Annot.desc=act; _},t) =  (* Rules ActUpdL, ActUpdG, ActEmitL and ActEmitG *)
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
       let x = Syntax.Guest.lhs_base_name lhs in
       let pfx p lhs = if p = "" then lhs else Syntax.Guest.lhs_prefix p lhs in 
       let upd ?(prefix="") env =
         let v = Eval.eval_expr genv expr in
         Trace.add (mk_event t (Event.Upd (pfx prefix lhs, v))) trace; (* Using prefixes in traces allow VCD scoping *)
         Eval.upd_env lhs v env in
       if Env.mem x vars then (* ActUpdL *)
         (upd ~prefix:(Ident.to_string f) vars, env), (* For local updates, prefix target variable *)
         Evset.empty t
       else if Env.mem x env then (* ActUpdG *)
         (vars, upd env), (* No prefix for updates of globals *)
         Evset.empty t
       else (* Should not happen *)
         Misc.fatal_error "Dynamic.r_act"
  
  let r_acts ~f sd (vars,env) (acts,t) = (* Rule ACTS *)
  (* \Nu, \Gamma -- <acts,t> | \rho_e --> \Nu', \Gamma' *)
    let (vars',env'), rs =
      List.fold_left_map
        (r_act ~f sd)
        (vars,env)
        (List.map (fun act -> (act,t)) acts) in
    vars', env', Evset.union_all t rs
  
  let r_trans sd (m,env) ({Annot.desc=q,_,acts,q',_; _},t) =  (* Rule TRANS *)
    (* \mu, \Gamma -- \tau,t | \rho_e --> \mu', \Gamma' *)
    let vars', env', r_e = r_acts ~f:m.Static.name sd (m.Static.vars,env) (acts,t) in
    let m' = { m with q=q'; vars = vars' } in
    Trace.add (mk_event t (Event.StateMove (state_name m,Ident.to_string q'))) trace;
    m', env', r_e
  
  let r_reaction sd (m,env) s_e = (* Rules React1, React0 and ReactN *)
    (*  \mu, \Gamma -- \sigma_e | \rho_e --> \mu', \Gamma' *)
    let t = Evset.date s_e in
    let env' = List.fold_left Env.union Env.empty [m.Static.vars; m.Static.params; env] in
      (* The folding order in the previous defn forces the _local_ definitions (vars and params) to
         shadow the global ones. *)
    match fireable env' m (Evset.events s_e) with
    | [tr] -> r_trans sd (m,env) (tr,t)   (* REACT_1 *)
    | [] -> m, env, Evset.empty t (* REACT_0 *)
    | trs -> r_trans sd (m,env) (choose_transition (Ident.to_string m.name,t,trs), t) (* REACT_N *)
  
  let r_react_upd sd (m,env) evs = (* Rule ReactUpd *)
    (* M, \Gamma -- \sigma_v --> M', \Gamma' *)
    (* TODO: use a GADT to remove dynamic checking of event kind ? *)
    let upd env e = match e with
      | Event.Upd (l,v') -> Eval.upd_env l v' env
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

  module FsmNode = 
    struct
      type t = Static.fsm
      type context = unit
      let name_of f = f.Static.name
      let depends_on _ m m' = 
        (* Return true if FSM m' (in state q') depends on FSM m (in state q), i.e. if
         - at least one transition starting from q' is triggered by a (shared) event emitted by at least one transition
           starting from q
         - at least one transition starting from q' is guarded by a condition refering to a (shared) variable modified by
           an action of a transition starting from q
         In other words, if we write
           - [evs'] the set of triggering (shared) events associated to state q' in m'
           - [rvs'] the set of (shared variables) used by the conditions associated to state q'
           - [evs] the set (shared) events emitted from state q in m
           - [wvs]  the set of (shared variables) modified by the actions modified from state q
         then m' depends on m iff [evs' \inter \evs] or [rvs' \inter wvs] is not empty *)
        let module S = Set.Make(Ident) in
        let inter l1 l2 = not (S.is_empty (S.inter (S.of_list l1) (S.of_list l2))) in
        let evs', rvs', _, _ = Syntax.state_ios m'.Static.model m'.q in
        let _, _, evs, wvs = Syntax.state_ios m.Static.model m.q in (* TODO ? Filter out non shared events / vars ? *)
        let r = inter evs' evs || inter rvs' wvs in
        (* let open Format in
         *  fprintf std_formatter "*** *** *** depends_on %s %s: evs'=[%a] rvs'=[%a] evs=[%a] wvs=[%a] r=%b\n"
         *   m.name m'.name
         *   (Misc.pp_list pp_print_string) evs'
         *   (Misc.pp_list pp_print_string) rvs'
         *   (Misc.pp_list pp_print_string) evs
         *   (Misc.pp_list pp_print_string) wvs
         *   r; *)
        r
    end
  
  let dep_sort fsms =
    (* Sort FSMs using the ($\leq$) relation of the dynamic semantics.
       The resulting order is used by [r_react_ev] to sequence the reactions of FSMs at a given instant. *)
    (* TODO ?: this could be pre-computed statically for each ((M,q),(M',q')) pair ? *)
    let module D = Depg.Make(FsmNode) in
    D.dep_sort () fsms
  
  let r_react_ev sd (m,env) s_e = (* Rule ReactEv *)
    (* M, \Gamma -- \sigma_e | \rho --> M', \Gamma' *)
    let ms = dep_sort m in 
    dump1 2 ">> >> REACT_EV: using order: %a\n" (Misc.pp_list_h ~sep:"; " (Static.pp_fsm ~verbose_level:0)) ms;
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
    dump4 1 ">> REACT t=%a@.   M=%a@.   G=%a@,  -- %a --> ...@."
      Format.pp_print_int t pp_fsms m pp_env env Evset.pp st; 
    let st_e, st_v = Evset.partition ~f:Event.is_pure_event st in
    let _, env_v = r_react_upd sd (m,env) st_v in
    dump1 2 ">> >> REACT_UPD: G_v=%a\n" pp_env env_v;
    let m', env', r_e = r_react_ev sd (m,env_v) st_e in
    dump3 2 ">> >> REACT_EV:@.       M'=%a@.       G'=%a@.       r_e=%a@." pp_fsms m' pp_env env' Evset.pp r_e;
    dump3 1 ">> REACT ... -- [%a] ->@.   M'=%a@.   G'=%a@." Evset.pp r_e pp_fsms m' pp_env env';
    (m',env'), r_e
  
  let is_event_type (ty: Types.typ) = Types.is_type_constr0 "event" ty

  let default_value (ty: Types.typ) = Static.Value.default_value (Some ty)

  let r_init sd m = (* Rule INIT *)
    (* M --> M_0, \Gamma_0 *)
    let env0 =
      List.fold_left
        (fun env (v,cc) ->
          let ty = cc.Static.ct_typ in 
          if not (is_event_type ty)
          then Env.add v (Static.Value.default_value (Some ty)) env
          else env)
        Env.empty
        (sd.Static.inputs @ sd.Static.outputs @ sd.Static.shared) in
    let env', m' =
      List.fold_left_map 
        (fun env mu ->
          let nu = mu.Static.vars in
          let { Annot.desc=q0,acts; _ } = mu.Static.model.Annot.desc.itrans in
          let nu',env', r_e = r_acts ~f:mu.name sd (nu, env) (acts,0) in
          Trace.add (mk_event 0 (Event.StateMove (state_name mu,Ident.to_string q0))) trace; 
          Trace.add r_e trace; 
          env', { mu with Static.q=q0 ; Static.vars = Env.union nu' mu.Static.params })
        env0
        m in
    m', env'
      
  let is_input ctx evs =
    let check ev = match ev with
      | Event.Ev x ->
         if List.mem_assoc x ctx.Static.inputs && is_event_type ((List.assoc x ctx.Static.inputs).ct_typ) then
           ()
         else
           Misc.fatal_error ("Dynamic.is_input: " ^ Ident.to_string x ^ " is not an event typed input")
      | Event.Upd (l,_) ->
         let x = Syntax.Guest.lhs_base_name l in
         if List.mem_assoc x ctx.Static.inputs && not (is_event_type ((List.assoc x ctx.Static.inputs).ct_typ)) then
           ()
         else
           Misc.fatal_error ("Dynamic.is_input: " ^ Ident.to_string x ^ " is an event typed input")
      | Event.StateMove _ -> () in
    List.iter check (Evset.events evs)
    
  let r_exec (h: Static.t) (sts: Evset.t list) = (* Rule EXEC *)
    (* H=<M,C> -- \sigma_1, ..., \sigma_n | \rho_1, ..., \rho_n --> M_n, \Gamma_n *)
    (* First check that all stimuli refer to input signals listed in H *)
    List.iter (is_input h.Static.ctx) sts;
    Trace.reset trace;
    let m0, env0 = r_init h.ctx h.fsms in
    let env1 = Env.union env0 h.globals in
    let _, _ = List.fold_left_map (r_react h.ctx) (m0,env1) sts in
    Trace.events trace
  
    let extract_stimuli p = 
      let eval (t,expr) = 
        try t, Eval.eval_expr Env.empty expr 
        with _ -> raise (Illegal_stimulus_value expr.Annot.loc) in
      let expand id st =
        match st.Annot.desc with
        | Syntax.Periodic (p,t1,t2) -> EvSeq.mk_periodic id p t1 t2
        | Syntax.Sporadic ts -> EvSeq.mk_sporadic id ts
        | Syntax.Value_change vcs -> EvSeq.mk_changes id (List.map eval vcs) in
      let sts =
        List.fold_left
          (fun acc io ->
            match io.Annot.desc with
            | id, Syntax.Input, _, Some st -> expand id st :: acc
            | _ -> acc)
          []
          p.Syntax.globals in
      EvSeq.merge_all sts

   let run (p: Syntax.program) (s: Static.t) =
     let sts = extract_stimuli p in
     match cfg.act_semantics with
     | Misc.Sequential -> r_exec s sts
     | Misc.Synchronous -> Misc.not_implemented "Dynamic.run with synchronous semantics for actions"
   
end
