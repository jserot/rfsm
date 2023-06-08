(* The abstract syntax of the host language. *)

module type SYNTAX = sig

  module Guest: Guest.SYNTAX
  
  type state = string
  type event = string
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
      inps: (string * type_expr) list;
      outps: (string * type_expr) list;
      vars: (string * type_expr) list;
      trans: transition list;
      itrans: itransition 
    }
  
  and cond = (cond_desc,typ) Annot.t
  and cond_desc = event * expr list
  
  and action = (action_desc,typ) Annot.t
  and action_desc =
    Emit of event
  | Assign of lhs * expr
            
  and transition = (transition_desc,typ) Annot.t
  and transition_desc = state * cond * action list * state

  and itransition = (itransition_desc,typ) Annot.t
  and itransition_desc = state * action list
                 
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
    
  val state_ios: model -> state -> string list * string list * string list * string list
    (** [state_ios m q] is [l1,l2,l3,l4] where
        - [l1] is the list of events triggering an exit from state [q] 
        - [l2] is the list of variables occuring in guards when exiting from state [q] 
        - [l3] is the list of events possibly emitted by [m] when exiting state [q]
        - [l4] is the list of variables possibly modified by [m] when exiting state [q] *)

  val pp_transition: Format.formatter -> transition -> unit
  val pp_model: Format.formatter -> model -> unit
  val pp_model_name: Format.formatter -> model -> unit
  val pp_program: Format.formatter -> program -> unit
end

module Make(B: Guest.SYNTAX) : SYNTAX with module Guest=B =
struct
  module Guest = B
               
  type state = string [@@deriving show {with_path=false}]
  type event = string [@@deriving show {with_path=false}]

  type typ = Guest.Types.typ
  type expr = Guest.expr
  type type_expr = Guest.type_expr
  type lhs = Guest.lhs

  let pp_type_expr = Guest.pp_type_expr
  let pp_expr = Guest.pp_expr
  let pp_lhs = Guest.pp_lhs

  type inst_desc = string * string * string list [@@deriving show {with_path=false}]
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

  type cond_desc = event * expr list [@@deriving show {with_path=false}]
  type cond = (cond_desc,typ) Annot.t
  let pp_cond fmt i = pp_cond_desc fmt i.Annot.desc

  type action_desc =
    Emit of event
  | Assign of lhs * expr [@@deriving show {with_path=false}]
  type action = (action_desc,typ) Annot.t
  let pp_action fmt a = pp_action_desc fmt a.Annot.desc
            
  type transition_desc = state * cond * action list * state [@@deriving show {with_path=false}]
  type transition = (transition_desc,typ) Annot.t 
  let pp_transition fmt t = pp_transition_desc fmt t.Annot.desc

  type itransition_desc = state * action list [@@deriving show {with_path=false}]
  type itransition = (itransition_desc,typ) Annot.t
  let pp_itransition fmt t = pp_itransition_desc fmt t.Annot.desc

  type model_desc = {
      name: string;
      states: state list;
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
    fprintf fmt "@[<v>[@,name=%s@,inps=%a@,outps=%a@,vars=%a@,trans=%a@,itrans=%a@,]@]"
      p.name
      (Misc.pp_list_v pp_iov) p.inps
      (Misc.pp_list_v pp_iov) p.outps
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
