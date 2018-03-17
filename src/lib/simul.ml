(* The simulator *)

open Utils

exception Error of string

type config = {
  max_micro_reactions: int;
  }

let cfg = {
  max_micro_reactions = 32;
  }

type stimulus = Ident.t * Expr.value option  (** name, value (None for pure events) *)

type response = Ident.t * Expr.value option   (* name, value (None for pure events) *)

type reaction = Types.date * string * Stimuli.stimuli list * response list * string

type context = {  (* The simulator state *)
  c_date: Types.date;
  c_inputs: (string * (Types.typ * Expr.value option)) list;   (* Global inputs *)
  c_outputs: (string * (Types.typ * Expr.value option)) list;  (* Globals outputs *)
  c_vars: (string * (Types.typ * Expr.value option)) list;     (* Shared variables *)
  c_evs: (string * (Types.typ * Expr.value option)) list;      (* Shared events *)
  c_fsms: Fsm.inst list * Fsm.inst list;                       (* FSMs, partitioned into active and inactive subsets *)
  }

let update_ctx ctx = function
  | Ident.Global id', v' ->
     let update_io ((id,(ty,v)) as x) =
       if id=id' then (id,(ty,if Types.is_event_type ty then Expr.set_event else v')) else x in
     let update_var ((id,(ty,v)) as x) = if id=id' then (id,(ty,v')) else x in
     let update_ev ((id,(ty,v)) as x) = if id=id' then (id,(ty,Expr.set_event)) else x in
     { ctx with
       c_inputs = List.map update_io ctx.c_inputs;
       c_vars = List.map update_var ctx.c_vars;
       c_evs = List.map update_ev ctx.c_evs; }
  | Ident.Local _, _ ->
     ctx

exception OverReaction of Types.date
   (* Raised when the number of micro-reactions at the given instant exceeds [cfg.max_micro_reactions] *)

let global_updates resps = 
  List.fold_left
        (fun acc (id,v) ->
          match id with
          | Ident.Global i -> (i,v)::acc
          | Ident.Local _ -> acc)
        []
        resps

let erase_type (id,(ty,v)) = id, v

let rec react t (ctx:context) (stimuli:(Ident.t * Expr.value option) list) =
  (* Compute a global reaction in [ctx] at time [t] given a set of stimuli [stimuli],
     producing an updated context [ctx'] and a set of responses [resps].
     The (operational) semantics is that of StateCharts (in turn similar to that of the delta concept
     used un DE formalisms. A reaction is viewed as a (finite) sequence of "micro-reactions".
     Each micro-reaction can generate stimuli which can trigger another micro-reaction (the related
     stimuli are here called "reentrant". However, a given FSM can only react once during a sequence of 
     micro-reactions (this is implemented by partitionning FSMs into active/inactive subsets during a reaction.
     A reaction ends when all the micro-reactions have taken place, i.e. when the last one did not produce
     any further re-entrant stimulus. *)
  let open Fsm in
  let is_reentrant =     (* A reentrant stimulus is one which can trigger a micro-reaction *)
    function
    | Ident.Global n, v -> 
       begin
         match List.mem_assoc n ctx.c_evs, List.mem_assoc n ctx.c_vars, v with
         true, _, Some _ -> true   (* shared event, currently set *)
       | false, true, _ -> true    (* shared variable, regardless of its value *)
       | _, _, _ -> false
       end
    | Ident.Local _, _ -> false in
  let still_active f = not f.f_has_reacted in
  let micro_react ctx stimuli =
    let ctx' = List.fold_left update_ctx ctx stimuli in 
    let genv = List.map erase_type (ctx'.c_inputs @ ctx'.c_vars @ ctx'.c_evs) in
    let fsms', resps = List.split (List.map (Fsm.react t genv) (fst ctx'.c_fsms)) in (* Only active FSMs play here.. *)
    (* TODO : check coherency for this set of resps (for ex that no global var is assigned diff value.. ) *)
    let resps' = List.concat resps in
    let ctx'' = List.fold_left update_ctx ctx' resps' in
    let fsms'', fsms''' = List.partition still_active fsms' in
    let ctx''' = { ctx'' with c_date=t; c_fsms=(fsms'', snd ctx'.c_fsms @ fsms''' ) } in
    ctx''', resps' in
  let erase_inst_event ((id,(ty,v)) as i) = match ty with
    | Types.TyEvent -> (id,(ty,Expr.unset_event))
    | _ -> i in
  let rec iter n ctx resps stimuli = match stimuli with
    [] -> ctx, resps   (* Done *)
  | evs ->
      if n > cfg.max_micro_reactions then raise (OverReaction t) else
      let ctx', resps' = micro_react ctx evs in
      match List.filter is_reentrant resps' with
      | [] ->                        (* Ok, all's quiet now. End of macro-reaction *)
         let ctx'' =
           { ctx' with c_inputs = List.map erase_inst_event ctx'.c_inputs;  (* Erase all events *)
                       c_evs = List.map erase_inst_event ctx'.c_evs;
                       c_fsms = fst ctx'.c_fsms @ snd ctx'.c_fsms, []; (* All FSMs return to potetntially active status *) } in
         ctx'', resps @ resps'
      | evs' ->                      (* Play it again time, Sam .. *)
         iter (n+1) ctx' (resps @ resps') evs' in
  iter 0 ctx [] stimuli

(* RUN *)

let run ?(ctx=None) m =
  let open Comp in
  let extract_shared (vars,evs) (name,(ty,desc)) = match desc, ty with
    | MShared _, Types.TyEvent -> vars, (name,(ty,None))::evs  (* Initially not set *)
    | MShared _, _ -> (name, (ty,None))::vars, evs (* Uninitialized *)
    | _, _ -> vars, evs in
  let rec step (ctxs,resps) stims =
    match stims with
      [] -> List.rev ctxs, List.rev resps (* End of simulation *)
    | (t,evs)::stims' ->
        let ctx = List.hd ctxs in  (* The last (current) context is at head *)
        let ctx', resps' = react t ctx evs in
        step (ctx' :: ctxs, (t, evs @ resps') :: resps) stims' in
          (* The events [evs] causing the reaction are included in the responses [resps] for tracing facilities .. *)
  let mk_ival (id,(ty,desc)) = id, (ty, None) in
  let ctx0, resps0 = match ctx with 
    Some ctx -> ctx, []
  | None ->
      let shared_vars, shared_evs = List.fold_left extract_shared ([],[]) m.m_shared in
      let init_ctx = {
          c_date = 0;
          c_inputs = List.map mk_ival m.m_inputs; 
          c_outputs = List.map mk_ival m.m_outputs; 
          c_vars = shared_vars;
          c_evs = shared_evs;
          c_fsms = m.m_fsms, [] } in
      let init_env = List.map erase_type (init_ctx.c_inputs @ init_ctx.c_vars @ init_ctx.c_evs) in
      let fsms', resps = List.split (List.map (Fsm.init_fsm init_env) (fst init_ctx.c_fsms)) in 
      let resps' = List.concat resps in
      let ctx' = List.fold_left update_ctx init_ctx resps' in
      {ctx' with c_fsms=fsms',[]}, [0, resps' (*@ resps''*)] in
  let ctxs, reacts = step ([ctx0],resps0) m.m_stimuli in
  (* TODO: post-processing ? *)
  ctxs, reacts

(* Printing *)

let string_of_comp (id,(ty,v)) = id  ^ "=" ^ Expr.string_of_opt_value v

let string_of_fsm f = (* short version for context printing *)
  let string_of_var (id,(ty,_)) = id  ^ ":" ^ Types.string_of_type ty in
  let open Fsm in
  match f.f_vars with
    [] -> f.f_name ^ ".st=" ^ f.f_state
  | vs -> f.f_name ^ "={st=" ^ f.f_state ^ ";vars=" ^ ListExt.to_string string_of_var "," f.f_vars ^ "}"

let dump_context c = 
  Printf.printf "t=%4d: active_fsms=[%s] inactive_fsms=[%s] inps=[%s] outps=[%s] shared_vars=[%s] shared_evs=[%s]\n" 
    c.c_date
    (ListExt.to_string string_of_fsm " " (fst c.c_fsms))
    (ListExt.to_string string_of_fsm " " (snd c.c_fsms))
    (ListExt.to_string string_of_comp " " c.c_inputs)
    (ListExt.to_string string_of_comp " " c.c_outputs)
    (ListExt.to_string string_of_comp " " c.c_vars)
    (ListExt.to_string string_of_comp " " c.c_evs)
    
let string_of_event (id,v) = match v with
  None -> Ident.to_string id
| Some v -> Ident.to_string id ^ ":=" ^ Expr.string_of_value v

let rec dump_reaction (t,evs) =
  Printf.printf "t=%4d: %s\n" t (ListExt.to_string string_of_event " " evs)

