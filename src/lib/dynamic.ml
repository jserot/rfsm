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

type fsm = { 
    f_static: Fsm.inst;                                       (** Static representation *)
    f_vars: (string * (Types.typ * Expr.value)) list;         (** name, (type, value) *)
    f_state: string;                                          (** Current state *)
    f_has_reacted: bool;                                      (** true when implied in the last reaction *)
  }
          
type lenv = (string * Expr.value) list

type genv = {
    fe_inputs: (string * (Types.typ * Expr.value)) list;   (** Global inputs *)
    fe_csts: (string * (Types.typ * Expr.value)) list;     (** Global constants *)
    fe_fns: (string * (Types.typ * Expr.value)) list;      (** Global functions *)
    fe_vars: (string * (Types.typ * Expr.value)) list;     (** Shared variables *)
    fe_evs: (string * (Types.typ * Expr.value)) list;      (** Shared events *)
  }

type event = loc * Expr.value  (** Event location, new value *)

and loc =
  | LVar of Ident.t             (** Scalar *)
  | LArrInd of Ident.t * int    (** 1D array location *)
  | LRField of Ident.t * string (** Record field *)

exception IllegalTrans of fsm * string
exception Undeterminate of fsm * string * Types.date
exception NonDetTrans of fsm * Fsm.transition list * Types.date
exception NonAtomicIoWrite of fsm * Action.t 

let rec mk_ival ty = match ty with
  | Types.TyArray(TiConst sz, ty') ->
     Expr.mk_array (Utils.ListExt.range (function i -> Expr.mk_val ty' Expr.Val_unknown) 1 sz)
  | Types.TyArray(_, _) -> Misc.fatal_error "Fsm.Dynamic.mk_ival"
  | Types.TyRecord (n,fs) -> 
     Expr.mk_record n (List.map (function (n,ty) -> n, ty, Expr.mk_val ty Expr.Val_unknown) fs)
  | _ -> Expr.mk_val ty Expr.Val_unknown

let mk_var (v,ty) = (v,(ty, mk_ival ty))
                  
let make_fsm sf = {
    f_static = sf;
    f_vars = List.map mk_var sf.Fsm.f_vars;
    f_state = ""; (* undefined *)
    f_has_reacted = false
  }

let rec replace_assoc' k v env =
  (* This is a variation on [Utils.ListExt.replace_assoc], where [v=(_,v')] and only [v'] is replaced *)
  let rec repl = function
      [] -> []
    | (k',(x,v'))::rest -> if k=k' then (k,(x,v)) :: repl rest else (k',(x,v')) :: repl rest in
  repl env

exception IllegalAction of fsm * Action.t
                         
let do_action (f,resps,resps',env) act =
  (* Make FSM [f] perform action [act] in (local) environment [env], returning an updated FSM [f'],
     a list of responses [resps], and an updated (local) environment [env']. *)
  let open Expr in
  let s = f.f_static in
  let array_upd id idx v = 
    match List.assoc id env, Eval.eval env idx with
    | { v_desc=Val_array vs} as a, { v_desc=Val_int i } -> { a with v_desc = Val_array (array_update id vs i v) }, i
    | _, _ -> raise (IllegalAction (f,act)) in
  let record_upd id fd v = 
    match List.assoc id env with
    | { v_desc=Val_record vs } as r -> { r with v_desc = Val_record (record_update id vs fd v) }
    | _ -> raise (IllegalAction (f,act)) in
  let set_bits id idx1 idx2 v = 
    match List.assoc id env, Eval.eval env idx1, Eval.eval env idx2, v with
    | { v_desc=Val_int x } as u, { v_desc=Val_int hi }, { v_desc=Val_int lo }, { v_desc=Val_int b } ->
       { u with v_desc = Val_int (Intbits.set_bits hi lo x b) }
    | _, _, _, _ -> raise (IllegalAction (f,act)) in
  match act with
    Action.Assign (lhs, expr) ->
     let id = Action.lhs_name lhs in
     let v = Eval.eval env expr in
     if List.mem_assoc id s.f_vars then (* Local variable *)
       begin match Fsm.cfg.act_sem with
       | Sequential ->
          (* In the sequential interpretation, updates of local variables are performed immediately
             and reflected in the environment (so that actions sequences such as "c:=c+1;s=c" are interpreted correctly. *)
          begin match lhs.l_desc with
          | Action.LhsVar id ->
             let v' = Eval.eval env expr in
             { f with f_vars = replace_assoc' id v' f.f_vars },
             resps @ [LVar (Ident.Local (s.f_name, id)), v'],
             resps',
             Utils.ListExt.replace_assoc id v' env
          | Action.LhsArrInd (id, idx) ->
             let v', i = array_upd id idx v in
             { f with f_vars = replace_assoc' id v' f.f_vars },
             resps @ [LArrInd (Ident.Local (s.f_name, id), i), v],
             resps',
             Utils.ListExt.replace_assoc id v' env
          | Action.LhsArrRange (id,idx1,idx2) ->
             let v' = set_bits id idx1 idx2 v in
             { f with f_vars = replace_assoc' id v' f.f_vars },
             resps @ [LVar (Ident.Local (s.f_name, id)), v'],
             resps',
             Utils.ListExt.replace_assoc id v' env
          | Action.LhsRField (id, fd) ->
             let v' = record_upd id fd v in
             { f with f_vars = replace_assoc' id v' f.f_vars },
             resps @ [LVar (Ident.Local (s.f_name, id)), v'],
             resps',
             Utils.ListExt.replace_assoc id v' env
          end
       | Synchronous ->
          (* In the synchronous interpretation, updates of local variables are not performed immediately
             nor reflected in the environment, but only reported in [resps] so that they can be performed at the end of the
             reaction. *)
          begin match lhs.l_desc with
          | Action.LhsVar id ->
             let v' = Eval.eval env expr in
             f,
             resps,
             resps' @ [LVar (Ident.Local (s.f_name, id)), v'],
             env
          | Action.LhsArrInd (id, idx) ->
             let v', i = array_upd id idx v in
             f,
             resps,
             resps' @ [LArrInd (Ident.Local (s.f_name, id), i), v],
             env
          | Action.LhsArrRange (id, idx1, idx2) ->
             let v' = set_bits id idx1 idx2 v in
             f,
             resps,
             resps' @ [LVar (Ident.Local (s.f_name, id)), v'],
             env
          | Action.LhsRField (id, fd) ->
             let v' = record_upd id fd v in
             f,
             resps,
             resps' @ [LRField (Ident.Local (s.f_name, id), fd), v'],
             env
          end
       end
     else  (* Global IO or shared value. Updates are never performed immediately *)
       begin match lhs.l_desc with
       | Action.LhsVar id ->
          let v' = Eval.eval env expr in
          f,
          resps @ [LVar (Ident.Global (s.f_l2g id)), v'],
          resps',
          env
       | Action.LhsArrInd (id, _)
       | Action.LhsArrRange (id, _, _)
       | Action.LhsRField (id, _) ->
          raise (NonAtomicIoWrite (f,act))
       end
  | Action.Emit id ->
     f,
     resps @ [LVar (Ident.Global (s.f_l2g id)), Expr.set_event],
     resps',
     env
  | Action.StateMove (id,q,q') ->
     { f with f_state = q' },
     resps @ [LVar (Ident.Local (s.f_name, "state")), { v_desc=Val_enum q'; v_typ=TyEnum (Types.new_name_var(),[]) }],
     resps',
     env

let perform_delayed_action (f,env) (lhs,v) = 
  let open Expr in
  let id, v' = match lhs with 
    | LVar (Ident.Local (_, id)) -> id, v
    | LArrInd (Ident.Local (_, id), i) ->
       id,
       begin match List.assoc id env with
       | { v_desc=Val_array vs } as a -> { a with v_desc = Val_array (array_update id vs i v) }
       | _ -> Misc.fatal_error "Fsm_dyn.perform_delayed_action" (* should not happen *)
       end
    | _ -> Misc.fatal_error "Fsm_dyn.perform_delayed_action" (* should not happen *) in
  { f with f_vars = replace_assoc' id v' f.f_vars },
  Utils.ListExt.replace_assoc id v' env
  
let string_of_actions resps =
  Utils.ListExt.to_string (function (id,v) -> Ident.to_string id ^ ":=" ^ (Expr.string_of_opt_value v)) "," resps

let do_actions env f acts = 
  let f', resps', resps'', env' = List.fold_left do_action (f,[],[],env) acts in
  (* Printf.printf "do_actions: resps'=[%s] resps''=[%s]\n" (string_of_actions resps') (string_of_actions resps''); *)
  match Fsm.cfg.act_sem with
  | Sequential ->
     f', resps'
  | Synchronous ->
     let f'', _ = List.fold_left perform_delayed_action (f',env') resps'' in
     f'', resps' @ resps''

let mk_local_env f genv = 
  let s = f.f_static in
  let erase_type (id,(ty,v)) = id, v in
  let get_value id =
    try snd  (List.assoc (s.f_l2g id) (genv.fe_inputs @ genv.fe_vars @ genv.fe_evs))
    with Not_found -> Misc.fatal_error "Fsm_dyn.mk_local_env" in (* should not happen *)
  (* let extract_global_fns acc (id, v) = match v with
   *     Expr.Val_fn _ -> (id,v) :: acc
   *   | _ -> acc in *)
  List.map (function (id,ty) -> id, get_value id) (s.f_inps @ s.f_inouts)
  @ List.map erase_type f.f_vars
  @ List.map erase_type (genv.fe_csts @ genv.fe_fns)

let rec react t genv f =
  (* Compute the reaction, at time [t] of FSM [f] in a global environment [genv].
     The global environment contains the values of global inputs, shared objects and global functions/constants.
     Return an updated fsm and list of responses consisting of
     - updates to global outputs or shared objects
     - updates to local variables (including state move) *)
  let sf = f.f_static in
  let lenv = mk_local_env f genv in
  let cross_transition (s,(cond,acts,_,_),s') =
    let acts' = if s <> s' then Action.StateMove(sf.f_name,s,s')::acts else acts in
    let f', resps' = do_actions lenv f acts'  in   (* .. perform associated actions .. *)
    { f' with f_has_reacted=true }, resps' in
  let ts = List.filter (fireable f lenv) (Fsm.transitions_of_inst sf) in
  match ts with
    [] ->                                                                 (* No transition found *)
     ({f with f_has_reacted=false}, [])
  | [t1] ->                                                               (* One found *)
     cross_transition t1
  | ts ->                                                                 (* Several found *)
     let priority_of (_,(_,_,p,_),_) = p in
     let compare_priority t1 t2 = Pervasives.compare (priority_of t2) (priority_of t1) in (* reverse order *)
     begin match List.sort compare_priority ts with
       t1::t2::_ ->
        if priority_of t1 > priority_of t2 then begin
            Printf.printf "Non deterministic transitions found for FSM %s at t=%d: {%s}; chose %s\n"
              sf.f_name
              t
              (Utils.ListExt.to_string Fsm.string_of_transition "," ts)
              (Fsm.string_of_transition t1);
            cross_transition t1
          end
        else
          raise (NonDetTrans (f,ts,t))
     | _ -> Misc.fatal_error "Fsm_dyn.react"
     end

and fireable f env (s,(cond,acts,_,_),s') =
  f.f_state = s && check_cond f env cond 

and check_cond f env (evs,guards) =
  List.for_all (is_event_set env) evs && Condition.eval_guards env guards

and is_event_set env e = match List.assoc e env with
    { Expr.v_desc=Val_bool true } -> true
  | _ -> false
  | exception Not_found -> false

let init genv f =
  let sf = f.f_static in
  match Fsm.Repr.itransitions sf.f_repr with
  | [(([],[]),acts,_,_), s] ->
     let env = mk_local_env f genv in
     do_actions env f (Action.StateMove (sf.f_name,"",s) :: acts)
  | [_] ->
     raise (IllegalTrans (f, "illegal initial transition"))
  | _ ->
     raise (IllegalTrans (f, "muliple initial transitions"))

