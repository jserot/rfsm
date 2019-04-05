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

(* The simulator *)

exception Error of string

type config = {
  mutable max_micro_reactions: int;
  }

let cfg = {
  max_micro_reactions = 32;
  }

type stimulus = Dynamic.loc * Expr.value

type response = Dynamic.loc * Expr.value 

type reaction = Types.date * string * Stimuli.stimuli list * response list * string

type context = {  (* The simulator state *)
  c_date: Types.date;
  c_inputs: (string * (Types.typ * Expr.value)) list;       (* Global inputs *)
  c_outputs: (string * (Types.typ * Expr.value)) list;      (* Global outputs *)
  c_fns: (string * (Types.typ * Expr.value)) list;          (* Global functions *)
  c_csts: (string * (Types.typ * Expr.value)) list;         (* Global constants *)
  c_vars: (string * (Types.typ * Expr.value)) list;         (* Shared variables *)
  c_evs: (string * (Types.typ * Expr.value)) list;          (* Shared events *)
  c_fsms: Dynamic.fsm list * Dynamic.fsm list;              (* FSMs, partitioned into active and inactive subsets *)
  }

let update_ctx ctx =
  let open Dynamic in
  function
  | LVar (Ident.Global id'), v' ->
     let update_io ((id,(ty,v)) as x) =
       if id=id' then (id,(ty,if Types.is_event_type ty then Expr.set_event else v')) else x in
     let update_var ((id,(ty,v)) as x) = if id=id' then (id,(ty,v')) else x in
     let update_ev ((id,(ty,v)) as x) = if id=id' then (id,(ty,Expr.set_event)) else x in
     { ctx with
       c_inputs = List.map update_io ctx.c_inputs;
       c_vars = List.map update_var ctx.c_vars;
       c_evs = List.map update_ev ctx.c_evs; }
  | LVar (Ident.Local _), _
  | LArrInd (Ident.Local _, _), _ 
  | LRField (Ident.Local _, _), _ ->
     ctx
  | LArrInd (_,_), _
  | LRField (_,_), _ ->
     failwith "Simul.update_ctx: non scalar value in context"
     (* ctx  *)

let string_of_context c = 
  let open Dynamic in
  let string_of_comp (id,(ty,v)) = id  ^ "=" ^ Expr.string_of_value v in
  let string_of_fsm f = f.f_static.f_name ^ ".st=" ^ f.f_state in
  Printf.sprintf "{fsms=[%s / %s] inps=[%s] outps=[%s] shared=[%s]}"
    (Utils.ListExt.to_string string_of_fsm "," (fst c.c_fsms))
    (Utils.ListExt.to_string string_of_fsm "," (snd c.c_fsms))
    (Utils.ListExt.to_string string_of_comp "," c.c_inputs)
    (Utils.ListExt.to_string string_of_comp "," c.c_outputs)
    (Utils.ListExt.to_string string_of_comp "," (c.c_vars @ c.c_evs))

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

let string_of_event (lhs,v) =
  let open Dynamic in
  match lhs with
  | LVar id -> Ident.to_string id ^ ":=" ^ Expr.string_of_value v
  | LArrInd (id,k) -> Ident.to_string id ^ "[" ^ string_of_int k ^ "]" ^ ":=" ^ Expr.string_of_value v
  | LRField (id,f) -> Ident.to_string id ^ "." ^ f

let string_of_events evs = "[" ^ Utils.ListExt.to_string string_of_event "," evs ^ "]"

let mk_fsm_env ctx =
  let open Dynamic in
  { fe_inputs = ctx.c_inputs;
    fe_csts = ctx.c_csts;
    fe_fns = ctx.c_fns;
    fe_vars = ctx.c_vars;
    fe_evs = ctx.c_evs }

let rec react t ctx stimuli =
  let open Dynamic in
  let is_reentrant =     (* A reentrant stimulus is one which can trigger a micro-reaction *)
    function
    | LVar (Ident.Global n), v -> 
       begin
         match List.mem_assoc n ctx.c_evs, List.mem_assoc n ctx.c_vars, v with
         true, _, { Expr.v_desc=Expr.Val_bool true } -> true   (* shared event, currently set *)
       | false, true, _ -> true                                (* shared variable, regardless of its value *)
       | _, _, _ -> false
       end
    | LVar (Ident.Local _), _
    | LArrInd (Ident.Local _, _), _
    | LRField (Ident.Local _, _), _ -> false
    | LArrInd _, _ 
    | LRField _, _ -> failwith "Simul.react: non scalar value" (* false *) in
  let still_active f = not f.f_has_reacted in
  let micro_react ctx stimuli =
    let ctx' = List.fold_left update_ctx ctx stimuli in 
    let genv = mk_fsm_env ctx' in
    let fsms', resps =
      List.split (List.map (react t genv) (fst ctx'.c_fsms)) in (* Only active FSMs play here.. *)
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
  | [] ->   (* Done *)
       Trace.msg0 2 "reaction completed\n";
       ctx, resps
  | evs ->
      if n > cfg.max_micro_reactions then raise (OverReaction t) else
      let ctx', resps' = micro_react ctx evs in
      Trace.msg3 2 "t=%d.%d:  resps=%s\n" t n (string_of_events resps');
      match List.filter is_reentrant resps' with
      | [] ->                        (* Ok, all's quiet now. End of macro-reaction *)
         let ctx'' =
           { ctx' with c_inputs = List.map erase_inst_event ctx'.c_inputs;  (* Erase all events *)
                       c_evs = List.map erase_inst_event ctx'.c_evs;
                       c_fsms = fst ctx'.c_fsms @ snd ctx'.c_fsms, []; (* All FSMs return to potentially active status *) } in
         ctx'', resps @ resps'
      | evs' ->                      (* Play it again time, Sam .. *)
         iter (n+1) ctx' (resps @ resps') evs' in
  iter 0 ctx [] stimuli

(* RUN *)

let run m =
  let open Static in
  let extract_shared (vars,evs) (name,(ty,desc)) = match desc, ty with
    | MShared _, Types.TyEvent -> vars, (name,(ty,Expr.unset_event))::evs  (* Initially not set *)
    | MShared _, _ -> (name, (ty, Expr.mk_val ty Expr.Val_unknown))::vars, evs (* Uninitialized *)
    | _, _ -> vars, evs in
  let rec step (ctx,resps) stims =
    match stims with
      [] -> ctx, List.rev resps (* End of simulation *)
    | (t,evs)::stims' ->
        begin match !Trace.level with
        | 0 -> ()
        | 1 -> Trace.msg2 1 "t=%d: evs=%s ==> " t (string_of_events evs)
        | n -> Trace.msg3 1 "t=%d: ctx=%s evs=%s ...\n" t (string_of_context ctx) (string_of_events evs)
        end;
        let ctx', resps' = react t ctx evs in
        begin match !Trace.level with
        | 0 -> ()
        | 1 -> Trace.msg1 1 "%s\n" (string_of_events resps')
        | n -> Trace.msg1 1 "==> %s\n" (string_of_events resps')
        end;
        step (ctx', (t, evs @ resps') :: resps) stims' in
          (* The events [evs] causing the reaction are included in the responses [resps] for tracing facilities .. *)
  let mk_ival (id,(ty,desc)) = id, (ty, Expr.mk_val ty Expr.Val_unknown) in
  let mk_gfun (id,(ty,desc)) = match desc with
      MFun (args,body) -> id, (ty, Expr.mk_val ty (Expr.Val_fn (args, body)))
    | _ -> failwith "Simul.run: cannot build global function description" in
  let mk_gcst (id,(ty,desc)) = match desc with
      MConst v -> id, (ty, v)
    | _ -> failwith "Simul.run: cannot build global constant description" in
  let ctx0, resps0 =
      let shared_vars, shared_evs = List.fold_left extract_shared ([],[]) m.m_shared in
      let init_ctx = {
          c_date = 0;
          c_inputs = List.map mk_ival m.m_inputs; 
          c_outputs = List.map mk_ival m.m_outputs; 
          c_fns = List.map mk_gfun m.m_fns; 
          c_csts = List.map mk_gcst m.m_consts; 
          c_vars = shared_vars;
          c_evs = shared_evs;
          c_fsms = List.map Dynamic.make_fsm m.m_fsms, [] } in
      let init_env = mk_fsm_env init_ctx in
      let fsms', resps =
        List.split (List.map (Dynamic.init init_env) (fst init_ctx.c_fsms)) in 
      let resps' = List.concat resps in
      let ctx' = List.fold_left update_ctx init_ctx resps' in
      {ctx' with c_fsms=fsms',[]}, [0, resps' (*@ resps''*)] in
  let inp_stimuli = function
      name, (ty, MInp (sd, _)) -> List.map (Stimuli.mk_stimuli name) (Stimuli.events_of sd)
    | _ -> Misc.fatal_error "Simul.run(inp_stimuli)" (* should not happen *) in
  let stimuli = Stimuli.merge_stimuli (List.map inp_stimuli m.m_inputs) in
  let wrap_stimuli (t,evs) =  t, List.map (function (id,v) -> Dynamic.LVar id, v) evs in
  let ctx, resps = step (ctx0,resps0) (List.map wrap_stimuli stimuli) in
  (* TODO: post-processing ? *)
  ctx, resps

(* Printing *)

let string_of_comp (id,(ty,v)) = id  ^ "=" ^ Expr.string_of_value v

let string_of_fsm f = (* short version for context printing *)
  let string_of_val v = match v.Expr.v_desc with
      Expr.Val_none -> "<none>"
    | Expr.Val_unknown -> "<unknown>"
    | _ -> Expr.string_of_value v in
  let string_of_var (id,(ty,v)) = id  ^ "=" ^ string_of_val v in
  let open Dynamic in
  let sf = f.f_static in
  match f.f_vars with
    [] -> sf.f_name ^ ".st=" ^ f.f_state
  | vs -> sf.f_name ^ "={st=" ^ f.f_state ^ ";vars=" ^ Utils.ListExt.to_string string_of_var "," f.f_vars ^ "}"

let dump_context c = 
  Printf.printf "t=%4d: active_fsms=[%s] inactive_fsms=[%s] inps=[%s] outps=[%s] csts=[%s] fns=[%s] shared_vars=[%s] shared_evs=[%s]\n" 
    c.c_date
    (Utils.ListExt.to_string string_of_fsm " " (fst c.c_fsms))
    (Utils.ListExt.to_string string_of_fsm " " (snd c.c_fsms))
    (Utils.ListExt.to_string string_of_comp " " c.c_inputs)
    (Utils.ListExt.to_string string_of_comp " " c.c_outputs)
    (Utils.ListExt.to_string string_of_comp " " c.c_csts)
    (Utils.ListExt.to_string string_of_comp " " c.c_fns)
    (Utils.ListExt.to_string string_of_comp " " c.c_vars)
    (Utils.ListExt.to_string string_of_comp " " c.c_evs)
    
let rec dump_reaction (t,evs) =
  Printf.printf "t=%4d: %s\n" t (Utils.ListExt.to_string string_of_event " " evs)

