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

(**{1 Abstract syntax of the host language} *)

module type SYNTAX = sig

  module Guest: Guest.SYNTAX
  
  type typ = Guest.Types.typ
  type expr = Guest.expr
  type type_expr = Guest.type_expr
  (* type type_expr_desc = Guest.type_expr_desc *)
  type lval = Guest.lval

  type type_decl = Guest.type_decl

  type program = {
      type_decls: type_decl list;
      fun_decls: fun_decl list;
      cst_decls: cst_decl list;
      models: model list;
      globals: global list;
      insts: inst list;
    }

  and model = (model_desc,typ) Annot.t
  and model_desc = {
      name: Ident.t;
      states: state list;
      params: (Ident.t * type_expr) list;
      ios: (Ident.t * (io_cat * type_expr)) list;
      inps: (Ident.t * type_expr) list;
      outps: (Ident.t * type_expr) list;
      inouts: (Ident.t * type_expr) list;
      vars: (Ident.t * type_expr) list;
      trans: transition list;
      itrans: itransition 
    }

  and io_cat = In| Out | InOut | Var (* [Var] is only used for program fragments - TO FIX *)
  
  and state = (state_desc,unit) Annot.t
  and state_desc = Ident.t * (Ident.t * expr) list (* Name, output valuations *)

  and cond = (cond_desc,typ) Annot.t
  and cond_desc = Ident.t * expr list (* event, guards *)
  
  and action = (action_desc,typ) Annot.t
  and action_desc =
    Emit of Ident.t
  | Assign of lval * expr
            
  and transition = (transition_desc,typ) Annot.t
  and transition_desc = Ident.t * cond * action list * Ident.t * int  (* source state, condition, actions, destination state, priority *)

  and itransition = (itransition_desc,typ) Annot.t
  and itransition_desc = Ident.t * action list  (* state, actions *)
                 
  and global = (global_desc,typ) Annot.t
  and global_desc = Ident.t * global_cat * type_expr * stimulus option

  and global_cat = Input | Output | Shared
                              
  and stimulus = (stimulus_desc,typ) Annot.t
  and stimulus_desc = 
    | Periodic of int * int * int (* Period, start date, end date *)
    | Sporadic of int list (* Dates *)
    | Value_change of (int * expr) list  (* Changes *)

  and inst = (inst_desc,typ) Annot.t
  and inst_desc =
      Ident.t  (* Name *)
    * Ident.t  (* Model *)
    * expr list (* Actual parameters *)
    * Ident.t list (* Args *)

  and fun_decl = (fun_decl_desc,typ) Annot.t
  and fun_decl_desc = {
    ff_name: Ident.t;
    ff_args: (Ident.t * type_expr) list;
    ff_res: type_expr;
    ff_body: expr;
    }

  and cst_decl = (cst_decl_desc,typ) Annot.t
  and cst_decl_desc = {
      cc_name: Ident.t;
      cc_typ: type_expr;
      cc_val: expr;
      }

  exception Invalid_symbol of Ident.t * Location.t * string 

  type fragment = { (* Program fragment for syntax and type checking of guards, actions and state valuations. Server mode only *)
    pf_inps: (Ident.t * type_expr) list;
    pf_outps: (Ident.t * type_expr) list;
    pf_vars: (Ident.t * type_expr) list;
    pf_obj: fragment_obj;
  }
  
  and fragment_obj =
    | Guard of expr
    | Action of action
    | SVal of Ident.t * expr  (* Output, value *)
      
  val empty_program: program
  val add_program: program -> program -> program

  val subst_model_io: phi:Ident.t Subst.t -> model -> model (* IO substitution *)
  val subst_model_param: phi:expr Subst.t -> model -> model (* Parameter substitution *)
    
  val state_ios: model -> Ident.t -> Ident.t list * Ident.t list * Ident.t list * Ident.t list
    (* [state_ios m q] is [l1,l2,l3,l4] where
        - [l1] is the list of events triggering an exit from state [q] 
        - [l2] is the list of variables occuring in guards when exiting from state [q] 
        - [l3] is the list of events possibly emitted by [m] when exiting state [q]
        - [l4] is the list of variables possibly modified by [m] when exiting state [q] *)

  val mk_basic_type_expr: string -> type_expr
    
  val normalize_model: model -> model

  val check_fragment: fragment -> unit
  val ppr_program: program -> program
  (* val ppr_fragment_obj: fragment_obj -> fragment_obj *)
  val ppr_fragment: fragment -> fragment
    
  val pp_typ: Format.formatter -> typ -> unit
  val pp_expr: Format.formatter -> expr -> unit
  (* val pp_type_expr_desc: Format.formatter -> type_expr_desc -> unit *)
  val pp_type_expr: Format.formatter -> type_expr -> unit
  val pp_cond_desc: Format.formatter -> cond_desc -> unit
  val pp_cond: Format.formatter -> cond -> unit (* Abstract syntax *)
  val ppf_cond: Format.formatter -> cond -> unit (* Concrete syntax *)
  val pp_action: Format.formatter -> action -> unit
  val pp_action_desc: Format.formatter -> action_desc -> unit
  val pp_transition: Format.formatter -> transition -> unit (* Abstract syntax *)
  val ppf_transition: Format.formatter -> transition -> unit (* Concrete syntax *)
  val pp_transition_desc: Format.formatter -> transition_desc -> unit
  val pp_itransition: Format.formatter -> itransition -> unit
  val pp_itransition_desc: Format.formatter -> itransition_desc -> unit
  val pp_stimulus: Format.formatter -> stimulus -> unit
  val pp_stimulus_desc: Format.formatter -> stimulus_desc -> unit
  val pp_state: Format.formatter -> state -> unit
  val pp_type_decl: Format.formatter -> Guest.type_decl -> unit
  val pp_model: Format.formatter -> model -> unit
  val pp_model_desc: Format.formatter -> model_desc -> unit
  val pp_model_name: Format.formatter -> model -> unit
  val pp_global: Format.formatter -> global -> unit
  val pp_cst_decl: Format.formatter -> cst_decl -> unit
  val pp_fun_decl: Format.formatter -> fun_decl -> unit
  val pp_program: Format.formatter -> program -> unit
  (* val pp_pf_obj: Format.formatter -> fragment_obj -> unit *)
  val pp_fragment: Format.formatter -> fragment -> unit

end

module Make(G: Guest.SYNTAX) : SYNTAX with module Guest=G =
struct
  module Guest = G
               
  type typ = Guest.Types.typ
  type expr = Guest.expr
  type type_expr = Guest.type_expr
  (* type type_expr_desc = Guest.type_expr_desc *)
  type lval = Guest.lval

  let mk_basic_type_expr = Guest.mk_basic_type_expr

  type type_decl = Guest.type_decl

  let pp_typ = Guest.Types.pp_typ ~abbrev:false
  (* let pp_type_expr fmt te = Guest.Types.pp_typ ~abbrev:false fmt te.Annot.typ *)
  let pp_type_expr fmt te = Guest.pp_type_expr fmt te
  (* let pp_type_expr_desc fmt te = Guest.pp_type_expr_desc fmt te *)
  let pp_expr = Guest.pp_expr
  let pp_lval = Guest.pp_lval

  type inst_desc =
      Ident.t  
    * Ident.t 
    * expr list
    * Ident.t list [@@deriving show {with_path=false}]
  type inst = (inst_desc,typ) Annot.t
  let pp_inst fmt i = pp_inst_desc fmt i.Annot.desc

  type global_cat = Input | Output | Shared [@@deriving show {with_path=false}]

  type stimulus_desc = 
    | Periodic of int * int * int (* Period, start date, end date *)
    | Sporadic of int list (* Dates *)
    | Value_change of (int * expr) list  (* Changes *)
    [@@deriving show {with_path=false}]
  type stimulus = (stimulus_desc,typ) Annot.t
  let pp_stimulus fmt s = pp_stimulus_desc fmt s.Annot.desc

  type global_desc = Ident.t * global_cat * type_expr * stimulus option [@@deriving show {with_path=false}]
  type global = (global_desc,typ) Annot.t
  let pp_global fmt i = pp_global_desc fmt i.Annot.desc

  type cond_desc = Ident.t * expr list [@@deriving show {with_path=false}]
  type cond = (cond_desc,typ) Annot.t
  let pp_cond fmt i = pp_cond_desc fmt i.Annot.desc
  let ppf_cond fmt { Annot.desc = (ev,guards); _ } = 
    match guards with
    | [] -> Format.fprintf fmt "on %a" Ident.pp ev
    | _ -> Format.fprintf fmt "on %a when %a" Ident.pp ev (Ext.List.pp_h ~sep:"." Guest.pp_expr) guards

  type action_desc =
    Emit of Ident.t
  | Assign of lval * expr [@@deriving show {with_path=false}]
  type action = (action_desc,typ) Annot.t
  let pp_action fmt a = pp_action_desc fmt a.Annot.desc
            
  type transition_desc = Ident.t * cond * action list * Ident.t * int [@@deriving show {with_path=false}]
  type transition = (transition_desc,typ) Annot.t 
  let pp_transition fmt t = pp_transition_desc fmt t.Annot.desc

  let ppf_transition fmt { Annot.desc = (src,cond,_,dst,_); _ } = 
   Format.fprintf fmt "%a -> %a %a" Ident.pp src Ident.pp dst ppf_cond cond
                                                                   
  type itransition_desc = Ident.t * action list [@@deriving show {with_path=false}]
  type itransition = (itransition_desc,typ) Annot.t
  let pp_itransition fmt t = pp_itransition_desc fmt t.Annot.desc

  type state_desc = (Ident.t * (Ident.t * expr) list) [@@deriving show {with_path=false}]
  type state = (state_desc,unit) Annot.t
  let pp_state fmt s = pp_state_desc fmt s.Annot.desc

  type io_cat = In| Out | InOut | Var

  type model_desc = {
      name: Ident.t;
      states: state list;
      params: (Ident.t * type_expr) list;
      ios: (Ident.t * (io_cat * type_expr)) list;
      (* Note: we must keep the unsorted IO specs to perform the formal/actual substitution when instanciating the model *)
      inps: (Ident.t * type_expr) list;
      outps: (Ident.t * type_expr) list;
      inouts: (Ident.t * type_expr) list;
      vars: (Ident.t * type_expr) list;
      trans: transition list;
      itrans: itransition 
    } (*[@@deriving show {with_path=false}]*)
  type model = (model_desc,typ) Annot.t

  type fun_decl_desc = {
    ff_name: Ident.t;
    ff_args: (Ident.t * type_expr) list;
    ff_res: type_expr;
    ff_body: expr;
    } [@@deriving show {with_path=false}]
  type fun_decl = (fun_decl_desc,typ) Annot.t
  let pp_fun_decl fmt cd = pp_fun_decl_desc fmt cd.Annot.desc

  type cst_decl_desc = {
      cc_name: Ident.t;
      cc_typ: type_expr;
      cc_val: expr;
      } [@@deriving show {with_path=false}]
  type cst_decl = (cst_decl_desc,typ) Annot.t
  let pp_cst_decl fmt cd = pp_cst_decl_desc fmt cd.Annot.desc

  type program = {
      type_decls: Guest.type_decl list;
      fun_decls: fun_decl list;
      cst_decls: cst_decl list;
      models: model list;
      globals: global list;
      insts: inst list;
    } 

  let empty_program = {
      type_decls=[];
      cst_decls=[];
      fun_decls=[];
      models=[];
      globals=[];
      insts=[]
    }

  let add_program p1 p2 = { (* TODO : Flag redefinitions ? *)
      type_decls= p1.type_decls @ p2.type_decls;
      cst_decls= p1.cst_decls @ p2.cst_decls;
      fun_decls= p1.fun_decls @ p2.fun_decls;
      models= p1.models @ p2.models;
      globals= p1.globals @ p2.globals;
      insts= p1.insts @ p2.insts;
    }

  let pp_model_name fmt m = Format.fprintf fmt "%a" Ident.pp m.Annot.desc.name
    
  (* IO substitutions *)

  let subst_var phi i = 
    try Subst.apply phi i 
    with Not_found -> i 

  let local_prefix x = "l_" ^ x

  let subst_io_cond phi c = match c.Annot.desc with 
    | (ev,guards) -> { c with desc = subst_var phi ev, List.map (Guest.subst_expr phi) guards }

  let subst_io_iov phi (id,ty) = subst_var phi id, ty
                                  
  let subst_io_action phi act = match act.Annot.desc with
    | Emit ev -> { act with desc = Emit (subst_var phi ev) }
    | Assign (lval,expr) -> { act with desc = Assign (Guest.subst_lval phi lval, Guest.subst_expr phi expr) }
                        
  let subst_io_transition phi ({Annot.desc=(q,cond,acts,q',p); _} as t)  =
   { t with desc = (q, subst_io_cond phi cond, List.map (subst_io_action phi) acts, q', p) }

  let subst_io_itransition phi ({Annot.desc=(q,acts); _} as t)  =
    { t with desc = (q, List.map (subst_io_action phi) acts) }

  let subst_model_io ~phi m =
    let mm = m.Annot.desc in
    let phi_r = List.map Ext.Base.swap phi in
    let captured_vars = 
      List.filter
        (fun (id,_) -> List.mem_assoc (Ident.mk_global id) phi_r)
        mm.vars |> List.map fst in
    if captured_vars <> [] then begin
      let pp_msg fmt vs =
        Format.fprintf fmt "The following variables are captured, and hence renamed, when instantiating model %a: %a"
          Ident.pp mm.name
          (Ext.List.pp_h ~sep:" " Ident.pp) vs in
      Misc.warning (Ext.Format.to_string pp_msg captured_vars)
      end;
    let phi' = List.map (fun id -> (id, Ident.upd_id local_prefix id)) captured_vars in
    Annot.map
    (fun m -> 
    { m with 
      inps = List.map (subst_io_iov phi) m.inps;
      outps = List.map (subst_io_iov phi) m.outps;
      inouts = List.map (subst_io_iov phi) m.inouts;
      vars = m.vars |> List.map (subst_io_iov phi') |> List.map (subst_io_iov phi);
      trans = m.trans |> List.map (subst_io_transition phi') |> List.map (subst_io_transition phi);
      itrans = m.itrans |> subst_io_itransition phi' |> subst_io_itransition phi })
    (Fun.id)
    m

  (* Parameter substitution *)

  let subst_param_cond phi c = match c.Annot.desc with 
    | (ev,guards) ->
       let c' = { c with desc = ev, List.map (Guest.subst_param_expr phi) guards } in
       c'
  
  let subst_param_iov phi (id,ty) = id, Guest.subst_param_type_expr phi ty
                                  
  let subst_param_action phi act = match act.Annot.desc with
    | Emit _ -> act
    | Assign (lval,expr) -> { act with desc = Assign (lval, Guest.subst_param_expr phi expr) }
                        
  let subst_param_transition phi ({Annot.desc=(q,cond,acts,q',p); _} as t)  =
   { t with desc = (q, subst_param_cond phi cond, List.map (subst_param_action phi) acts, q', p) }
  
  let subst_param_itransition phi ({Annot.desc=(q,acts); _} as t)  =
    { t with desc = (q, List.map (subst_param_action phi) acts) }

  let subst_model_param ~phi m =
    Annot.map
    (fun m -> 
    { m with 
      inps = List.map (subst_param_iov phi) m.inps;
      outps = List.map (subst_param_iov phi) m.outps;
      inouts = List.map (subst_param_iov phi) m.inouts;
      vars = List.map (subst_param_iov phi)  m.vars;
      trans = List.map (subst_param_transition phi) m.trans;
      itrans = subst_param_itransition phi m.itrans })
    (Fun.id)
    m

  (* Moore-Mealy conversion *)

  let normalize_outp m (o,_) = (* Remove output [o] from valuations and update transitions accordingly *)
    let updated_states = (* The list of states having [o] in their attached valuation ... *)
      List.fold_left 
        (fun acc { Annot.desc = q,ovs; _ } ->
          match List.assoc_opt o ovs with
          | Some e -> (q,e)::acc
          | None -> acc)
        []
        m.states in
    let remove_output_valuation ({ Annot.desc=q, ovs; _} as s) = { s with Annot.desc = q, List.remove_assoc o ovs } in
    let add_act e acts =
      Annot.{desc=Assign(Guest.mk_simple_lval o, e); typ=Guest.Types.no_type; loc=Location.no_location} :: acts in 
    let add_output_assignation ({ Annot.desc=q,conds,acts,q',p; _ } as t) =
      match List.assoc_opt q' updated_states with
      | Some e -> { t with Annot.desc = q,conds,add_act e acts,q',p }
      | None -> t in
    let add_output_iassignation ({ Annot.desc=q',acts; _ } as t) =
      match List.assoc_opt q' updated_states with
      | Some e -> { t with Annot.desc = q',add_act e acts }
      | None -> t in
    { m with states = List.map remove_output_valuation m.states;
             trans = List.map add_output_assignation m.trans;
             itrans = add_output_iassignation m.itrans }
    
  let normalize_model m = 
    let md = m.Annot.desc in 
    { m with Annot.desc = List.fold_left normalize_outp md md.outps }

  (* Pre-processing *)

  let type_of ~loc env v =
      (* Since pre-processing is carried out _before_ typing, the only type-related available information
         is given by the type expressions assigned to identifiers in the enclosing model *)
      try Env.find v env
      with Not_found -> raise (Ident.Undefined ("symbol",loc,v)) 

  let rec ppr_model m = { m with Annot.desc = ppr_model_desc m.Annot.desc }
  and ppr_model_desc m =
    let env = Env.init (m.inps @ m.outps @ m.inouts @ m.vars) in
    { m with states = List.map (ppr_state env) m.states;
             trans = List.map (ppr_transition env) m.trans;
             itrans = ppr_itransition env m.itrans }

  and ppr_state env s = { s with Annot.desc = ppr_state_desc ~loc:s.Annot.loc env s.Annot.desc }
  and ppr_state_desc ~loc env (q,ovs) = q, List.map (ppr_ov ~loc env) ovs

  and ppr_ov ~loc env (o,expr) = 
    let typ = type_of ~loc env o in
    if Guest.is_bool_type typ 
    then (o, Guest.ppr_expr env ~expected_type:(Some typ) expr)
    else (o, expr)

  and ppr_transition env t = { t with Annot.desc = ppr_trans_desc env t.Annot.desc }
  and ppr_trans_desc env (q,cond,acts,q',p) = (q, ppr_cond env cond, List.map (ppr_action env) acts, q', p)

  and ppr_itransition env t = { t with Annot.desc = ppr_itrans_desc env t.Annot.desc }
  and ppr_itrans_desc env (q,acts) = (q, List.map (ppr_action env) acts)

  and ppr_cond env c = { c with Annot.desc = ppr_cond_desc env c.Annot.desc }
  and ppr_cond_desc env (ev,exprs) = (ev, List.map (Guest.ppr_expr env) exprs)

  and ppr_action env a = { a with Annot.desc = ppr_action_desc env a.Annot.desc }
  and ppr_action_desc env act =
    match act with
    | Emit _ -> act
    | Assign (lval, expr) ->
       let typ = type_of ~loc:lval.Annot.loc env (Guest.lval_base_name lval) in
       let expr' = Guest.ppr_expr env expr in
       if Guest.is_bool_type typ 
       then Assign (lval, Guest.ppr_expr env ~expected_type:(Some typ) expr')
       else Assign (Guest.ppr_lval env lval, expr') (* In case pre-processing should be carried out _inside_ l-value sub-exprs *)

  let rec ppr_global gl = { gl with Annot.desc = ppr_global_desc gl.Annot.desc }
  and ppr_global_desc ((id,cat,te,stim) as gl) = 
    match stim with 
    | None -> gl
    | Some st -> (id, cat, te, Some (ppr_stim te st))

  and ppr_stim te st = { st with Annot.desc = ppr_stim_desc te st.Annot.desc }
  and ppr_stim_desc te st = 
    match st with 
    | Value_change vcs -> Value_change (List.map (function (t,expr) -> t, Guest.ppr_expr Env.empty ~expected_type:(Some te) expr) vcs)
    | _ -> st

  let ppr_program p =
    { p with models = List.map ppr_model p.models;
             globals = List.map ppr_global p.globals }

  module S = Set.Make(Ident)

  let rvars_of_action a =
    match a.Annot.desc with
    | Emit _ -> S.empty
    | Assign (_,expr) -> S.of_list (Guest.vars_of_expr expr)

  let wvars_of_action a =
    match a.Annot.desc with
    | Emit _ -> S.empty
    | Assign (lval,_) -> S.of_list (Guest.vars_of_lval lval)

  let events_of_action a =
    match a.Annot.desc with
    | Emit e -> S.singleton e
    | Assign _ -> S.empty

  let triggering_event { Annot.desc=(_,{Annot.desc=(e,_);_},_,_,_); _} = S.singleton e 

  let read_vars { Annot.desc=(_,{Annot.desc=(_,guards);_},acts,_,_); _} = 
    S.union
      (List.fold_left (fun acc e -> S.union (S.of_list (Guest.vars_of_expr e)) acc) S.empty guards)
      (List.fold_left (fun acc a -> S.union (rvars_of_action a) acc) S.empty acts)

  let emitted_events { Annot.desc=(_,_,acts,_,_); _ } =
    List.fold_left
      (fun acc a -> S.union (events_of_action a) acc)
      S.empty
      acts

  let written_vars { Annot.desc=(_,_,acts,_,_); _ } =
    List.fold_left
      (fun acc g -> S.union (wvars_of_action g) acc)
      S.empty
      acts
                    
  let state_ios { Annot.desc=m; _ } q =
    let ts = List.filter (fun { Annot.desc=q',_,_,_,_; _ } -> q'=q) m.trans in
    let accum f ts =
      List.fold_left
        (fun acc t -> S.union (f t) acc)
        S.empty
        ts
      |> S.elements in
    accum triggering_event ts,
    accum read_vars ts,
    accum emitted_events ts,
    accum written_vars ts

  let pp_type_decl = Guest.pp_type_decl 
  let pp_model_desc fmt p = 
    let open Format in
    let pp_iov fmt (x,t) = fprintf fmt "%a:%a" Ident.pp x pp_type_expr t in
    let pp_ov fmt (o,e) = fprintf fmt "%a=%a" Ident.pp o pp_expr e in
    let pp_ovs fmt ovs = match ovs with [] -> () | _ -> fprintf fmt "{%a}" (Ext.List.pp_h pp_ov) ovs in
    let pp_state fmt { Annot.desc=x,ovs; _ } = fprintf fmt "%a%a" Ident.pp x pp_ovs ovs in
    fprintf fmt "@[<v>{@,name=%a@,params=[%a]@,inps=%a@,outps=%a@,inouts=%a@,states=[%a]@,vars=%a@,trans=%a@,itrans=%a@,}@]"
      Ident.pp p.name
      (Ext.List.pp_h ~sep:"," pp_iov) p.params
      (Ext.List.pp_v pp_iov) p.inps
      (Ext.List.pp_v pp_iov) p.outps
      (Ext.List.pp_v pp_iov) p.inouts
      (Ext.List.pp_h ~sep:"," pp_state) p.states
      (Ext.List.pp_v pp_iov) p.vars
      (Ext.List.pp_v pp_transition) p.trans
      pp_itransition p.itrans
  let pp_model fmt m = pp_model_desc fmt m.Annot.desc
      
  let pp_program fmt p = 
    let open Format in
    fprintf fmt "@[<v>{@,csts=%a@,fns=%a@,types=%a@,models=%a@,globals=%a@,insts=%a@,}@]@."
      (Ext.List.pp_v pp_cst_decl) p.cst_decls
      (Ext.List.pp_v pp_fun_decl) p.fun_decls
      (Ext.List.pp_v Guest.pp_type_decl) p.type_decls
      (Ext.List.pp_v pp_model) p.models
      (Ext.List.pp_v pp_global) p.globals
      (Ext.List.pp_v pp_inst) p.insts

  exception Invalid_symbol of Ident.t * Location.t * string 

  (* Program fragments - since 2.1 *)
      
  type fragment_obj =
    | Guard of expr
    | Action of action
    | SVal of Ident.t * expr  (* Output, value *)
  
  type fragment_iov = Ident.t * type_expr 
                      
  type fragment = {
    pf_inps: fragment_iov list;
    pf_outps: fragment_iov list;
    pf_vars: fragment_iov list;
    pf_obj: fragment_obj;
    }

  let pp_pf_iov fmt (id,t) = 
    Format.fprintf fmt "%a: %a" Ident.pp id pp_type_expr t
    
  let pp_pf_obj fmt o = 
    let open Format in
    match o with
    | Guard e -> fprintf fmt "guard \"%a\"" pp_expr e
    | Action a -> fprintf fmt "action \"%a\"" pp_action a
    | SVal (id,e) -> fprintf fmt "sval \"%a=%a\"" Ident.pp id pp_expr e 

  let pp_fragment fmt p = 
    let open Format in
    fprintf fmt "@[<v>{@,inps = %a@,outps = %a@,vars = %a@,obj = %a@]@."
      (Ext.List.pp_v pp_pf_iov) p.pf_inps
      (Ext.List.pp_v pp_pf_iov) p.pf_outps
      (Ext.List.pp_v pp_pf_iov) p.pf_vars
      pp_pf_obj p.pf_obj

  let check_fragment p = (* Basic sanity checking *)
    let check_var ~loc ~msg ~src v =
      if not @@ List.mem_assoc v src
      then raise (Invalid_symbol (v, loc, msg)) in
    let check_inp_or_var ~loc v =
      check_var ~src:(p.pf_inps @ p.pf_vars) ~loc ~msg:"is not an input, nor a local variable" v in
    let check_outp_or_var ~loc v =
      check_var ~src:(p.pf_outps @ p.pf_vars) ~loc ~msg:"is not an output, nor a local variable" v in
    let check_outp ~loc v =
      check_var ~src:p.pf_outps ~loc ~msg:"is not an output" v in
    let check obj =
      match obj with
      | Guard e ->
        (* Check that all symbols occuring in [e] are defined as input or variable *)
        List.iter
          (check_inp_or_var ~loc:e.Annot.loc)
          (Guest.vars_of_expr e) 
      | Action a ->
        (* Check that all symbols occuring in RHS are defined as input or variable
           and that all symbols occuring in LHS are defined as output or variable *)
        S.iter
          (check_inp_or_var ~loc:a.Annot.loc)
          (rvars_of_action a);
        S.iter
          (check_outp_or_var ~loc:a.Annot.loc)
          (wvars_of_action a);
      | SVal (o,e) ->
        (* Check that all symbols occuring in RHS are defined as input or variable
           and that [o] is defined as output *)
        List.iter
          (check_inp_or_var ~loc:e.Annot.loc)
          (Guest.vars_of_expr e); 
        check_outp ~loc:e.Annot.loc o in 
    check p.pf_obj
    
  let ppr_fragment_obj pf obj =
    let env = Env.init (pf.pf_inps @ pf.pf_vars @ pf.pf_outps) in
    match obj with
    | Guard e -> Guard (Guest.ppr_expr env e)
    | Action a -> Action (ppr_action env a)
    | SVal (o,e) ->
        let _, e' = ppr_ov ~loc:Location.no_location env (o,e) in
        SVal (o, e')

  let ppr_fragment pf = { pf with pf_obj = ppr_fragment_obj pf pf.pf_obj }

end
