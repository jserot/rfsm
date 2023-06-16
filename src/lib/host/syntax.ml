(* The abstract syntax of the host language. *)

module type SYNTAX = sig

  module Guest: Guest.SYNTAX
  
  type typ = Guest.Types.typ
  type expr = Guest.expr
  type type_expr = Guest.type_expr
  type lhs = Guest.lhs

  type program = {
      type_decls: Guest.type_decl list;
      fun_decls: fun_decl list;
      cst_decls: cst_decl list;
      models: model list;
      ios: io list;
      insts: inst list;
    }

  and model = (model_desc,typ) Annot.t
  and model_desc = {
      name: string;
      states: state list;
      params: (string * type_expr) list;
      inps: (string * type_expr) list;
      outps: (string * type_expr) list;
      vars: (string * type_expr) list;
      trans: transition list;
      itrans: itransition 
    }
  
  and state = (state_desc,unit) Annot.t
  and state_desc = string * (string * expr) list (* Name, output valuations *)

  and cond = (cond_desc,typ) Annot.t
  and cond_desc = string * expr list (** event, guards *)
  
  and action = (action_desc,typ) Annot.t
  and action_desc =
    Emit of string
  | Assign of lhs * expr
            
  and transition = (transition_desc,typ) Annot.t
  and transition_desc = string * cond * action list * string  (** source state, condition, actions, destination state *)

  and itransition = (itransition_desc,typ) Annot.t
  and itransition_desc = string * action list  (** state, actions *)
                 
  and io = (io_desc,typ) Annot.t
  and io_desc = string * io_cat * type_expr * stimulus option

  and io_cat = Input | Output | Shared
                              
  and stimulus = (stimulus_desc,typ) Annot.t
  and stimulus_desc = 
    | Periodic of int * int * int (** Period, start date, end date *)
    | Sporadic of int list (** Dates *)
    | Value_change of (int * expr) list  (** Changes *)

  and inst = (inst_desc,typ) Annot.t
  and inst_desc =
      string  (** Name *)
    * string  (** Model *)
    * expr list (** Actual parameters *)
    * string list (** Args *)

  and fun_decl = (fun_decl_desc,typ) Annot.t
  and fun_decl_desc = {
    ff_name: string;
    ff_args: (string * type_expr) list;
    ff_res: type_expr;
    ff_body: expr;
    }

  and cst_decl = (cst_decl_desc,typ) Annot.t
  and cst_decl_desc = {
      cc_name: string;
      cc_typ: type_expr;
      cc_val: expr;
      }

  val empty_program: program
  val add_program: program -> program -> program

  val subst_model: phi:(string * string) list -> model -> model (** Name substitution *)
    
  val state_ios: model -> string -> string list * string list * string list * string list
    (** [state_ios m q] is [l1,l2,l3,l4] where
        - [l1] is the list of events triggering an exit from state [q] 
        - [l2] is the list of variables occuring in guards when exiting from state [q] 
        - [l3] is the list of events possibly emitted by [m] when exiting state [q]
        - [l4] is the list of variables possibly modified by [m] when exiting state [q] *)

  val normalize_model: model -> model

  val ppr_program: program -> program
  exception Undefined_symbol of Location.t * string
    
  val pp_action: Format.formatter -> action -> unit
  val pp_transition: Format.formatter -> transition -> unit
  val pp_model: Format.formatter -> model -> unit
  val pp_model_name: Format.formatter -> model -> unit
  val pp_program: Format.formatter -> program -> unit
end

module Make(B: Guest.SYNTAX) : SYNTAX with module Guest=B =
struct
  module Guest = B
               
  type typ = Guest.Types.typ
  type expr = Guest.expr
  type type_expr = Guest.type_expr
  type lhs = Guest.lhs

  let pp_type_expr = Guest.pp_type_expr
  let pp_expr = Guest.pp_expr
  let pp_lhs = Guest.pp_lhs

  type inst_desc =
      string  
    * string 
    * expr list
    * string list [@@deriving show {with_path=false}]
  type inst = (inst_desc,typ) Annot.t
  let pp_inst fmt i = pp_inst_desc fmt i.Annot.desc

  type io_cat = Input | Output | Shared [@@deriving show {with_path=false}]

  type stimulus_desc = 
    | Periodic of int * int * int (** Period, start date, end date *)
    | Sporadic of int list (** Dates *)
    | Value_change of (int * expr) list  (** Changes *)
    [@@deriving show {with_path=false}]
  type stimulus = (stimulus_desc,typ) Annot.t
  let pp_stimulus fmt s = pp_stimulus_desc fmt s.Annot.desc

  type io_desc = string * io_cat * type_expr * stimulus option [@@deriving show {with_path=false}]
  type io = (io_desc,typ) Annot.t
  let pp_io fmt i = pp_io_desc fmt i.Annot.desc

  type cond_desc = string * expr list [@@deriving show {with_path=false}]
  type cond = (cond_desc,typ) Annot.t
  let pp_cond fmt i = pp_cond_desc fmt i.Annot.desc

  type action_desc =
    Emit of string
  | Assign of lhs * expr [@@deriving show {with_path=false}]
  type action = (action_desc,typ) Annot.t
  let pp_action fmt a = pp_action_desc fmt a.Annot.desc
            
  type transition_desc = string * cond * action list * string [@@deriving show {with_path=false}]
  type transition = (transition_desc,typ) Annot.t 
  let pp_transition fmt t = pp_transition_desc fmt t.Annot.desc

  type itransition_desc = string * action list [@@deriving show {with_path=false}]
  type itransition = (itransition_desc,typ) Annot.t
  let pp_itransition fmt t = pp_itransition_desc fmt t.Annot.desc

  type state_desc = (string * (string * expr) list)
  type state = (state_desc,unit) Annot.t

  type model_desc = {
      name: string;
      states: state list;
      params: (string * type_expr) list;
      inps: (string * type_expr) list;
      outps: (string * type_expr) list;
      vars: (string * type_expr) list;
      trans: transition list;
      itrans: itransition 
    } (*[@@deriving show {with_path=false}]*)
  type model = (model_desc,typ) Annot.t

  type fun_decl_desc = {
    ff_name: string;
    ff_args: (string * type_expr) list;
    ff_res: type_expr;
    ff_body: expr;
    } [@@deriving show {with_path=false}]
  type fun_decl = (fun_decl_desc,typ) Annot.t
  let pp_fun_decl fmt cd = pp_fun_decl_desc fmt cd.Annot.desc

  type cst_decl_desc = {
      cc_name: string;
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
      ios: io list;
      insts: inst list;
    } 

  let empty_program = {
      type_decls=[];
      cst_decls=[];
      fun_decls=[];
      models=[];
      ios=[];
      insts=[]
    }

  let add_program p1 p2 = { (* TODO : Flag redefinitions ? *)
      type_decls= p1.type_decls @ p2.type_decls;
      cst_decls= p1.cst_decls @ p2.cst_decls;
      fun_decls= p1.fun_decls @ p2.fun_decls;
      models= p1.models @ p2.models;
      ios= p1.ios @ p2.ios;
      insts= p1.insts @ p2.insts;
    }

  let pp_model_name fmt m = Format.fprintf fmt "%s" m.Annot.desc.name
    
  (* Substitutions *)

  let subst_cond phi c = match c.Annot.desc with 
    | (ev,guards) -> { c with desc = Misc.subst_id phi ev, List.map (Guest.subst_expr phi) guards }

  let subst_iov phi (id,ty) = Misc.subst_id phi id, ty
                                  
  let subst_action phi act = match act.Annot.desc with
    | Emit ev -> { act with desc = Emit (Misc.subst_id phi ev) }
    | Assign (lhs,expr) -> { act with desc = Assign (Guest.subst_lhs phi lhs, Guest.subst_expr phi expr) }
                        
  let subst_transition phi ({Annot.desc=(q,cond,acts,q'); _} as t)  =
   { t with desc = (q, subst_cond phi cond, List.map (subst_action phi) acts, q') }

  let subst_itransition phi ({Annot.desc=(q,acts); _} as t)  =
    { t with desc = (q, List.map (subst_action phi) acts) }

  let subst_model ~phi m =
    Annot.map
    (fun m -> 
    { m with 
      inps = List.map (subst_iov phi) m.inps;
      outps = List.map (subst_iov phi) m.outps;
      vars = List.map (subst_iov phi) m.vars;
      trans = List.map (subst_transition phi) m.trans;
      itrans = subst_itransition phi m.itrans })
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
    let add_act e acts = Annot.make (Assign(Guest.mk_simple_lhs o, e)) :: acts in 
    let add_output_assignation ({ Annot.desc=q,conds,acts,q'; _ } as t) =
      match List.assoc_opt q' updated_states with
      | Some e -> { t with Annot.desc = q,conds,add_act e acts,q' }
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

  exception Undefined_symbol of Location.t * string

  let type_of ~loc env v =
      (* Since pre-processing is carried out _before_ typing, the only type-related available information
         is given by the type expressions assigned to identifiers in the enclosing model *)
      try List.assoc v env
      with Not_found -> raise (Undefined_symbol (loc,v)) 

  let rec ppr_model m = { m with Annot.desc = ppr_model_desc m.Annot.desc }
  and ppr_model_desc m =
    let env = m.inps @ m.outps @ m.vars in
    { m with states = List.map (ppr_state env) m.states;
             trans = List.map (ppr_transition env) m.trans;
             itrans = ppr_itransition env m.itrans }

  and ppr_state env s = { s with Annot.desc = ppr_state_desc ~loc:s.Annot.loc env s.Annot.desc }
  and ppr_state_desc ~loc env (q,ovs) = q, List.map (ppr_ov ~loc env) ovs

  and ppr_ov ~loc env (o,expr) = 
    let typ = type_of ~loc env o in
    if Guest.is_bool_type typ 
    then (o, Guest.mk_bool_expr typ expr)
    else (o, expr)

  and ppr_transition env t = { t with Annot.desc = ppr_trans_desc env t.Annot.desc }
  and ppr_trans_desc env (q,cond,acts,q') = (q, ppr_cond env cond, List.map (ppr_action env) acts, q')

  and ppr_itransition env t = { t with Annot.desc = ppr_itrans_desc env t.Annot.desc }
  and ppr_itrans_desc env (q,acts) = (q, List.map (ppr_action env) acts)

  and ppr_cond env c = { c with Annot.desc = ppr_cond_desc env c.Annot.desc }
  and ppr_cond_desc env (ev,exprs) = (ev, List.map (Guest.ppr_expr env) exprs)

  and ppr_action env a = { a with Annot.desc = ppr_action_desc env a.Annot.desc }
  and ppr_action_desc env act =
    (* Replace all assignations [v:=0/1], where [v:bool] by [v:=false/true] *)
    match act with
    | Emit _ -> act
    | Assign (lhs, expr) ->
       let typ = type_of ~loc:lhs.Annot.loc env (Guest.lhs_name lhs) in
        if Guest.is_bool_type typ 
        then Assign (lhs, Guest.mk_bool_expr typ expr)
        else Assign (Guest.ppr_lhs env lhs, expr) (* In case pre-processing should be carried out _inside_ LHS sub-exprs *)

  let rec ppr_io io = { io with Annot.desc = ppr_io_desc io.Annot.desc }
  and ppr_io_desc ((id,cat,te,stim) as io) = 
    match stim with 
    | None -> io
    | Some st -> (id, cat, te, Some (ppr_stim te st))

  and ppr_stim te st = { st with Annot.desc = ppr_stim_desc te st.Annot.desc }
  and ppr_stim_desc te st = 
    match st with 
    | Value_change vcs -> Value_change (List.map (function (t,expr) -> t, Guest.mk_bool_expr te expr) vcs)
    | _ -> st

  let ppr_program p =
    { p with models = List.map ppr_model p.models;
             ios = List.map ppr_io p.ios }

  module S = Set.Make(String)

  let rvars_of_action a =
    match a.Annot.desc with
    | Emit _ -> S.empty
    | Assign (_,expr) -> S.of_list (Guest.vars_of_expr expr)

  let wvars_of_action a =
    match a.Annot.desc with
    | Emit _ -> S.empty
    | Assign (lhs,_) -> S.of_list (Guest.vars_of_lhs lhs)

  let events_of_action a =
    match a.Annot.desc with
    | Emit e -> S.singleton e
    | Assign _ -> S.empty

  let triggering_event { Annot.desc=(_,{Annot.desc=(e,_);_},_,_); _} = S.singleton e 

  let read_vars { Annot.desc=(_,{Annot.desc=(_,guards);_},acts,_); _} = 
    S.union
      (List.fold_left (fun acc e -> S.union (S.of_list (Guest.vars_of_expr e)) acc) S.empty guards)
      (List.fold_left (fun acc a -> S.union (rvars_of_action a) acc) S.empty acts)

  let emitted_events { Annot.desc=(_,_,acts,_); _ } =
    List.fold_left
      (fun acc a -> S.union (events_of_action a) acc)
      S.empty
      acts

  let written_vars { Annot.desc=(_,_,acts,_); _ } =
    List.fold_left
      (fun acc g -> S.union (wvars_of_action g) acc)
      S.empty
      acts
                    
  let state_ios { Annot.desc=m; _ } q =
    let ts = List.filter (fun { Annot.desc=q',_,_,_; _ } -> q'=q) m.trans in
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

  let pp_model_desc fmt p = 
    let open Format in
    let pp_iov fmt (x,t) = fprintf fmt "%s:%a" x pp_type_expr t in
    let pp_ov fmt (o,e) = fprintf fmt "%s=%a" o (pp_expr ~with_type:false) e in
    let pp_ovs fmt ovs = match ovs with [] -> () | _ -> fprintf fmt "{%a}" (Misc.pp_list_h pp_ov) ovs in
    let pp_state fmt { Annot.desc=x,ovs; _ } = fprintf fmt "%s%a" x pp_ovs ovs in
    fprintf fmt "@[<v>[@,name=%s@,params=[%a]@,inps=%a@,outps=%a@,states=[%a]@,vars=%a@,trans=%a@,itrans=%a@,]@]"
      p.name
      (Misc.pp_list_h ~sep:"," pp_iov) p.params
      (Misc.pp_list_v pp_iov) p.inps
      (Misc.pp_list_v pp_iov) p.outps
      (Misc.pp_list_h ~sep:"," pp_state) p.states
      (Misc.pp_list_v pp_iov) p.vars
      (Misc.pp_list_v pp_transition) p.trans
      pp_itransition p.itrans
  let pp_model fmt m = pp_model_desc fmt m.Annot.desc
      
  let pp_program fmt p = 
    let open Format in
    fprintf fmt "@[<v>[@,csts=%a@,fns=%a@,types=%a@,models=%a@,ios=%a@,insts=%a@,]@]@."
      (Misc.pp_list_v pp_cst_decl) p.cst_decls
      (Misc.pp_list_v pp_fun_decl) p.fun_decls
      (Misc.pp_list_v Guest.pp_type_decl) p.type_decls
      (Misc.pp_list_v pp_model) p.models
      (Misc.pp_list_v pp_io) p.ios
      (Misc.pp_list_v pp_inst) p.insts
end
