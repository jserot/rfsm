(* The type checker for the host language. *)

module type TYPING = sig
  module HostSyntax: Syntax.SYNTAX
  type env
  val mk_env: unit -> env
  val type_program: env -> HostSyntax.program -> unit
  val pp_env: Format.formatter -> env -> unit

  exception Undefined_symbol of Location.t * string
  exception Invalid_state of Location.t * string
  exception Duplicate_state of Location.t * string
  exception No_event_input of Location.t
  exception Illegal_inst of Location.t
  exception Illegal_state_output of Location.t * string * string
end

module Make
         (HS: Syntax.SYNTAX)
         (GT: Guest.TYPING with module Syntax = HS.Guest and type Types.typ = HS.typ ) =
struct
  module HostSyntax = HS
  module GuestTyping = GT 

  type env = GuestTyping.env

  exception Undefined_symbol of Location.t * string
  exception Invalid_state of Location.t * string
  exception Duplicate_state of Location.t * string
  exception No_event_input of Location.t
  exception Illegal_inst of Location.t
  exception Illegal_state_output of Location.t * string * string

  let mk_env () = GuestTyping.mk_env ()

  (* Typing FSM models *)

  let type_fsm_action env m act =
    (* TO FIX: the type of an action should always be "unit" ? *)
    let loc = act.Annot.loc in
    let ty = match act.Annot.desc with 
      | HostSyntax.Emit s ->
         let t = GuestTyping.Types.mk_type_constr0 "event" in 
         let t' = GuestTyping.lookup_var ~loc:act.Annot.loc s env in
         GuestTyping.type_check ~loc t t';
         t
      | HostSyntax.Assign (lhs,expr) -> 
         let t = GuestTyping.type_lhs env lhs in
         let t' = GuestTyping.type_expression env expr in
         GuestTyping.type_check ~loc t t';
         t in
    act.Annot.typ <- Some ty

  let type_fsm_event ~loc t env ev =
    GuestTyping.type_check
      ~loc 
      (GuestTyping.Types.mk_type_constr0 "event")
      (GuestTyping.lookup_var ~loc ev env)

  let type_fsm_guard t env gexp =
    GuestTyping.type_check
      ~loc:gexp.Annot.loc
      (GuestTyping.Types.mk_type_constr0 "bool")
      (GuestTyping.type_expression env gexp)

  let type_fsm_condition t env { Annot.desc=ev,gs; Annot.loc=loc; _ } =
    type_fsm_event ~loc t env ev;
    List.iter (type_fsm_guard t env) gs 

  let check_fsm_state ~loc { Annot.desc=m; _} q = 
    let states = List.map (function s -> s.Annot.desc) m.HostSyntax.states in 
    if not (List.mem_assoc q states) then raise (Invalid_state (loc, q))

  let type_fsm_transition env m ({ Annot.desc= q,cond,acts,q',_; Annot.loc=loc; _ } as t) =
    (* For each transition [q -> cond / acts -> q'] check that
     *    - [q] and [q'] are listed as states in the model declaration
     *    - [cond] has form [e.[guard1]...[guardn]] where [e] has type [event] and each [guardi] type [bool]
     *    - for each [act]=[lhs:=rhs] in [acts], [type(rhs)=type(lhs)] *)
    check_fsm_state ~loc m q;
    check_fsm_state ~loc m q';
    type_fsm_condition t env cond;
    List.iter (type_fsm_action env m) acts

  let type_fsm_itransition env m { Annot.desc=q,acts; Annot.loc=loc; _ } =
    (* For the initial transition [/ acts -> q] check that
     *    - [q] is listed as state in model declaration
     *    - for each [act]=[v:=exp] in [acts], [type(v)=type(exp)] *)
    check_fsm_state ~loc m q;
    List.iter (type_fsm_action env m) acts

  let type_fsm_state_valuation env (m:HostSyntax.model) q (o,expr) = 
    let te =
      try List.assoc o m.Annot.desc.HostSyntax.outps
      with Not_found -> raise (Illegal_state_output (expr.Annot.loc, q, o)) in
    GuestTyping.type_check
      ~loc:expr.Annot.loc 
      (GuestTyping.type_of_type_expr env te)
      (GuestTyping.type_expression env expr)

  let type_fsm_state env m { Annot.desc = q,ovs; _ } = 
    List.iter (type_fsm_state_valuation env m q) ovs

  let type_fsm_states env m = 
    let states = m.Annot.desc.HostSyntax.states in
    let rec check_dupl states = match states with
      | [] -> ()
      | { Annot.desc = q, _; Annot.loc = loc; _ } :: rest ->
         if List.exists (function { Annot.desc = q', _; _ } ->  q=q' ) rest 
         then raise (Duplicate_state (loc,q))
         else check_dupl rest in
    check_dupl states;
    List.iter (type_fsm_state env m) states

  let types_of_fsm_model env { Annot.desc = m; _ } = 
    (* Computes the "local" typing environment associated to an FSM model, containing
       the types of parameters, inputs, outputs and local variables *)
    List.map
      (function (id, te) -> id, GuestTyping.type_of_type_expr env te)
      (m.HostSyntax.params @ m.HostSyntax.inps @ m.HostSyntax.outps @ m.HostSyntax.vars)

  let type_fsm_ios env { Annot.desc = m; Annot.loc = loc; _ } =
    (* Check that there's exactly one input with type event *)
    let is_event_type (_,te) =
      match te.Annot.typ with
      | Some ty -> GuestTyping.Types.is_type_constr0 "event" ty
      | _ -> false in
    match List.filter is_event_type m.HostSyntax.inps with
    | [] -> raise (No_event_input loc)
    | _ -> ()
         
  let type_fsm_model env m =
    type_fsm_states env m;
    let env' = List.fold_left GuestTyping.add_var env (types_of_fsm_model env m) in
    type_fsm_ios env' m;
    List.iter (type_fsm_transition env' m) m.Annot.desc.trans;
    type_fsm_itransition env' m m.Annot.desc.itrans

  (* Typing FSM instances *)

  let type_fsm_inst env p { Annot.desc=name,model,params,args; Annot.loc=loc; _ } =
    let open HostSyntax in
    let lookup_model name =
      try List.find (fun { Annot.desc = m; _ } -> m.name = name) p.models
      with Not_found -> raise (Undefined_symbol (loc,name)) in
    let lookup_io name =
      try List.find (fun { Annot.desc = (id,_,_,_); _ } -> id = name) p.ios
      with Not_found -> raise (Undefined_symbol (loc,name)) in
    let unify_cat cat cat' = match cat, cat' with
      (* Check that an Input (resp. Output) is not plugged on an Output (resp. Input) *)
      | Input, Output -> raise (Illegal_inst loc)
      | Output, Input -> raise (Illegal_inst loc)
      | _, _ -> () in
    let type_of te = match te.Annot.typ with
      | Some ty -> ty
      | None -> Misc.fatal_error "Typing.type_check_fsm_inst" in
    let m = (lookup_model model).Annot.desc in
    let m_inps = List.map (fun (id,te) -> id, Input, type_of te) m.inps in
    let m_outps = List.map (fun (id,te) -> id, Output, type_of te) m.outps in
    let m_params = List.map (fun (id,te) -> id, type_of te) m.params in
    let bind_arg (id,cat,ty) id' =
      let _,cat',te',_ = (lookup_io id').Annot.desc in
      unify_cat cat cat';
      GuestTyping.type_check ~loc ty (type_of te') in
    let bind_param (id,ty) e =
      GuestTyping.type_check
        ~loc:loc
        ty
        (GuestTyping.type_expression env e) in
    try
      List.iter2 bind_param m_params params;
      List.iter2 bind_arg (m_inps @ m_outps) args;
    with Invalid_argument _ -> raise (Illegal_inst loc)

  (* Typing IOs *)
                             
  let type_stimulus env id ty st =
    let check t = GuestTyping.type_check ~loc:st.Annot.loc ty t in 
    st.Annot.typ <- Some ty;
    match st.Annot.desc with
    | HostSyntax.Periodic _ -> check (GuestTyping.Types.mk_type_constr0 "event")
    | HostSyntax.Sporadic _ -> check (GuestTyping.Types.mk_type_constr0 "event")
    | HostSyntax.Value_change vcs ->
       List.iter
         check
         (List.map (function (_,e) -> GuestTyping.type_expression env e) vcs)

  let type_io env  ({ Annot.desc = id,cat,te,st; _ } as io) = 
    let ty = GuestTyping.type_of_type_expr env te in
    let _ = match st with 
      | Some st -> type_stimulus env id ty st
      | None -> () in
    io.Annot.typ <- Some ty

  (* Typing function declarations *)

  let type_fun_decl env ({ Annot.desc = fd; Annot.loc = loc; _ } as f) = 
    let open HostSyntax in
    let ty_args =
      List.map
        (function (id,te) -> id, GuestTyping.type_of_type_expr env te)
        fd.ff_args in
    let env' = List.fold_left GuestTyping.add_var env ty_args in
    let ty_body = GuestTyping.type_expression env' fd.ff_body  in
    let ty_result = GuestTyping.type_of_type_expr env fd.ff_res in
    GuestTyping.type_check ~loc:loc ty_body ty_result;
    let ty = GuestTyping.Types.mk_type_fun (List.map snd ty_args) ty_result in
    f.Annot.typ <- Some ty;
    GuestTyping.add_var env (fd.ff_name, ty)

  (* Typing constant declarations *)

  let type_cst_decl env ({ Annot.desc = cd; Annot.loc = loc; _ } as c) = 
    let open HostSyntax in
    let ty = GuestTyping.type_of_type_expr env cd.cc_typ in
    let ty' = GuestTyping.type_expression env cd.cc_val in
    GuestTyping.type_check ~loc:loc ty ty';
    c.Annot.typ <- Some ty;
    GuestTyping.add_var env (cd.cc_name, ty)

  (* Typing programs *)

  let type_program env0 p = 
    let open HostSyntax in
    let env1 = List.fold_left GuestTyping.type_type_decl env0 p.type_decls in
    let env2 = List.fold_left type_fun_decl env1 p.fun_decls in
    let env = List.fold_left type_cst_decl env2 p.cst_decls in
    List.iter (type_fsm_model env) p.models;
    List.iter (type_io env) p.ios;
    List.iter (type_fsm_inst env p) p.insts

  let pp_env fmt env = GuestTyping.pp_env fmt env

end
