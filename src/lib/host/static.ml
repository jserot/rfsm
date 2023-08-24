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

(** Static elaboration *)

(** This step takes a typed program consisting of 
    - types, functions and constant declarations
    - global IO declarations 
    - typed FSM instances
    and produce a representation, consisting of
    - the same types, functions and constant declarations
    - a elaborated set of FSM instances
    In the latter
    i) the formal IOs of the corresponding model have been bound to the global IOs;
      for ex, if the input program is like
        [
        fsm model f (in x: bool, out y: int, ...) ... rules | q -> q' when h.(x=1) with y:=0 ...
        ...
        input X: bool
        output Y: int
        ...
        fsm f0 = f(X,Y,...)
        ]
      then, the model describing [f0] in the result representation will be like 
        [
        fsm model f (in X: bool, out X: int, ...) ... rules | q -> q' when h.(X=1) with Y:=0 ...
        ]
      Note: the compatibility between formal and actual IOs has already been checked by the typing phase.
    ii) the (generic) parameters have been replaced by their actual value
      for ex, if the input program is like
        [
        fsm model f <n: int> (in x: int<n>, ...) ... vars z: int ... rules | q -> q' when h.(x=1).(z<n) ...
        ...
        input X: int<8>
        ...
        fsm f8 = f<8>(X,,...)
        ]
      then, the model describing [f8] in the result representation will be like 
        [
        fsm model f (in X: int<8>, ...) ... vars z: int ... rules | q -> q' when h.(X=1).(z<8)  ...
        ]
      Note: the compatibility between formal and actual parameters has already been checked by the typing phase.
      iii) Moore-style descriptions (with output assignations attached to states) have been turned to
      to Mealy-style ones (with output assignations attached to transitions). 
      for ex, if the input program is like
        [
        fsm model f  (out o: int, ...) states { ..., q' with o=1 } ... rules | q -> q' ...
        ...
        fsm f0 = f(...)
        ]
      then, the model describing [f0] in the result representation will be like 
        [
        fsm model f (out o: int, ...) states { ..., q } ... rules | q -> q' with o:=1 ...
        ]
   
      The elaboration step also computes the dependency order induced by shared variables
      between FSMs. An FSM [f] depends on another FSM [f'] if [f] reads a variable that is written by [f'].
      Note that this order is here purely static because all rules are considered for reading and writing,
      independentely of the actual FSM state. The resulting order will used by the SystemC (and possibly other)
      backend to implement instantaneous broadcast using delta cycles.
      *) 

module type T = sig
  
  module Syntax: Syntax.SYNTAX
  module Typing: Typing.TYPING with module HostSyntax = Syntax
  module Value: Guest.VALUE with type typ = Syntax.typ

  (** FSM instances *)
  type fsm = {
      name: Ident.t;
      model: Syntax.model;    (** Normalized, type-refined model *)
      q: Ident.t;
      vars: Value.t Env.t;
    }
  
  type ctx_comp = {
    ct_typ: Syntax.typ;
    ct_stim: Syntax.stimulus_desc option;  (** For inputs only *)
    ct_rds: Ident.t list; (** Readers, for inputs and shareds *)
    ct_wrs: Ident.t list; (** Writers, for outputs and shareds *)
    }

  type ctx = { 
    inputs: (Ident.t * ctx_comp) list;
    outputs: (Ident.t * ctx_comp) list;
    shared: (Ident.t * ctx_comp) list;
    }

  type t = {
    ctx: ctx;
    models: Syntax.model list; (** Original, un-type-refined and un-normalized models. Not used any longer *)
    fsms: fsm list;
    globals: Value.t Env.t; (** Functions and constants *)
    fns: Syntax.fun_decl list; (** Functions *)
    csts: Syntax.cst_decl list; (** Constants *)
    types: Syntax.Guest.type_decl list; (** User-defined types *)
    dep_order: (Ident.t * int) list; 
    }

  val build: Typing.typed_program -> Syntax.program -> t

  val pp: verbose_level:int -> Format.formatter -> t -> unit
  val pp_fsm: verbose_level:int -> Format.formatter -> fsm -> unit

  val is_rtl: fsm -> bool

end

module Make
         (HS: Syntax.SYNTAX)
         (HT: Typing.TYPING with module HostSyntax = HS)
         (GV: Guest.VALUE with type typ = HS.typ)
         (GS: Guest.STATIC with type expr = HS.expr and type value = GV.t)
  : T with module Syntax = HS
       and module Typing = HT
       and module Value = GV =
struct

  module Syntax = HS
  module Typing = HT
  module Value = GV

  type fsm = {
      name: Ident.t;
      model: Syntax.model;
      q: Ident.t;
      vars: Value.t Env.t;
    } [@@deriving show {with_path=false}]

  let pp_fsm ~verbose_level fmt f  = 
    let open Format in
    match verbose_level with
    | 0 -> Format.fprintf fmt "%a" Ident.pp f.name  (* Name only *)
    | 1 -> (* With model name, state and vars only *)
       fprintf fmt "@[<h>{@,name=%a,model=%a@,q=%a,@,vars=%a}@]"
         Ident.pp f.name
         Ident.pp f.model.Annot.desc.name
         Ident.pp f.q
         (Env.pp Value.pp) f.vars
    | _ -> (* Full *)
       fprintf fmt "@[<v>{@,name=%a@,model=%a@,q=%a@,vars=%a}@]"
         Ident.pp f.name
         Syntax.pp_model f.model
         Ident.pp f.q
         (Env.pp Value.pp) f.vars

  type ctx_comp = {
    ct_typ: Syntax.typ [@printer fun fmt -> fprintf fmt "%a" (Syntax.Guest.Types.pp_typ ~abbrev:false)];
    ct_stim: Syntax.stimulus_desc option;  (* Attached stimuli, for inputs only *)
    ct_rds: Ident.t list; (* Readers, for inputs and shareds *)
    ct_wrs: Ident.t list; (* Writers, for outputs and shareds *)
    } [@@deriving show {with_path=false}]

  type ctx = {  
    inputs: (Ident.t * ctx_comp) list;
    outputs: (Ident.t * ctx_comp) list;
    shared: (Ident.t * ctx_comp) list;
    }

  let pp_ctx fmt ctx =
    let open Format in
    let pp_io fmt (id,cc) = Format.fprintf fmt "%a: %a" Ident.pp id pp_ctx_comp cc in
    fprintf fmt "@[<v>{inputs=%a@,outputs=%a@,shared=%a}@]"
    (Misc.pp_list_v pp_io) ctx.inputs
    (Misc.pp_list_v pp_io) ctx.outputs
    (Misc.pp_list_v pp_io) ctx.shared

  type t = {
    ctx: ctx;
    models: Syntax.model list;
    fsms: fsm list;
    globals: Value.t Env.t;
    fns: Syntax.fun_decl list;
    csts: Syntax.cst_decl list;
    types: Syntax.Guest.type_decl list;
    dep_order: (Ident.t * int) list; 
    }

  let pp ~verbose_level fmt s =
    let open Format in
    fprintf fmt "@[<v>{ctx=%a@,fsms=%a@,globals=%a@,types=%a@,dep_order=%a@,}@."
    pp_ctx s.ctx
    (Misc.pp_list_v (pp_fsm ~verbose_level)) s.fsms
    (Env.pp Value.pp) s.globals
    (Misc.pp_list_v Syntax.pp_type_decl) s.types
    (Misc.pp_list_h ~sep:"," (fun fmt (f,d) -> fprintf fmt "%a:%d" Ident.pp f d)) s.dep_order

  (* Rules *)
         
  let r_inst tp senv_i { Annot.desc=name,model,params,args; Annot.loc=loc; _ } =
    let open Syntax in
    (* Find the corresponding instanciated model *)
    let mm = 
      try List.assoc name tp.Typing.tp_insts
      with Not_found -> Misc.fatal_error "Static.r_inst"  in (* should not happen after TC *)
    let m = mm.Annot.desc in
    (* Build the parameter substitution *)
    let bind_param (id,_) e = (id, e) in
      (* Note: parameters are _not_ evaluated here. They will be syntactically substituted  *)
    let phi_p =
      try List.map2 bind_param m.params params
      with Invalid_argument _ ->  Misc.fatal_error "Static.r_inst" in  (* should not happen after TC *)
    (* Build the IO substitution *)
    let bind_arg (l_id,_) g_id = (l_id,g_id) in
    let phi_io = 
      try List.map2 bind_arg m.ios args
      with Invalid_argument _ ->  Misc.fatal_error "Static.r_inst" in  (* should not happen after TC *)
    let erase_params m = { m with Annot.desc = { m.Annot.desc with params = [] } } in
    (* Format.printf "Static.phi_p = %a\n" (Subst.pp Syntax.pp_expr) phi_p; *)
    let mm' =
         mm
         |> normalize_model
         |> subst_model_param ~phi:phi_p
         |> subst_model_io ~phi:phi_io
         |> erase_params in
    let collect cats =
      List.fold_left2 
        (fun acc (id,(cat,_)) arg -> if List.mem cat cats then  arg::acc else acc)
        []
        m.ios
        args in
    let m' = mm'.Annot.desc in
    let rds = collect [Syntax.In; Syntax.InOut] in
    let wrs = collect [Syntax.Out; Syntax.InOut] in
    let f = {
        name = name;
        model = mm';
        q = fst m'.itrans.Annot.desc;
        vars =
          List.fold_left
            (fun env (id,te) -> Env.add id (Value.default_value te.Annot.typ) env)
            Env.empty
            m'.vars
      } in
    f, (name, (rds,wrs))

  let r_insts tp senv_i insts = List.map (r_inst tp senv_i) insts

  let r_global env { Annot.desc=id,cat,ty,st; _ } = 
    Env.add id (cat,ty,st) env
    
  let r_globals gls = List.fold_left r_global Env.empty gls
                
  let build_ctx rws senv_i =  (* \mathcal{L} *)
    let open Syntax in
    let type_of te = te.Annot.typ in
    let collect_rds i = List.fold_left (fun acc (name,(rds,_)) -> if List.mem i rds then name::acc else acc) [] rws in
    let collect_wrs o = List.fold_left (fun acc (name,(_,wrs)) -> if List.mem o wrs then name::acc else acc) [] rws in
    let extract cat acc senv =
      Env.fold 
      (fun id (cat',te,st) acc ->
        let t = type_of te in
        match cat, cat', st with
        | Input, Input, Some { Annot.desc=st'; _ } ->
           let cc = { ct_typ=t; ct_stim=Some st'; ct_rds=collect_rds id; ct_wrs=[] } in
           (id, cc)::acc 
        | Output, Output, _ ->
           let cc = { ct_typ=t; ct_stim=None; ct_rds=[]; ct_wrs=collect_wrs id } in
           (id, cc)::acc 
        | Shared, Shared, _ ->
           let cc = { ct_typ=t; ct_stim=None; ct_rds=collect_rds id; ct_wrs=collect_wrs id } in
           (id, cc)::acc 
        | _, _, _ ->
           acc)
      senv
      [] in
    { inputs = extract Input [] senv_i;
      outputs = extract Output [] senv_i;
      shared = extract Shared [] senv_i; }

  let r_program tp p =
    let senv_i = r_globals p.Syntax.globals in
    let m, rws = List.split @@ r_insts tp senv_i p.Syntax.insts in
    let c = build_ctx rws senv_i in
    m, c
    
  let r_const env { Annot.desc = c; _ } = 
    let open Syntax in
     Env.add c.cc_name (GS.eval c.cc_val) env  (* TODO: fold env so that a defn can depend on a previous one *)

  let r_fun env { Annot.desc = f; _ } = 
    let open Syntax in
    let args = f.ff_args |> List.map fst |> List.map Ident.to_string in
    Env.add f.ff_name (GS.eval_fn args f.ff_body) env 

  let r_globals p = 
    let env_c = List.fold_left r_const Env.empty p.Syntax.cst_decls in
    let env_f = List.fold_left r_fun env_c p.Syntax.fun_decls in
    Env.union env_c env_f

  (* Computation of the static dependency order induced by shared variables *)

  module FsmNode = 
    struct
      type t = fsm
      type context = ctx
      let name_of f = f.name
      let depends_on ctx f f' = 
        (* [f'] (statically) depends on [f] iff [f'] reads a _shared_ variable of [ctx] which is written by [f], 
         i.e. if there's at least one variable [v] of [ctx.shared] such that [f] appears in [v.ct_wrs] and 
         [f'] appears in [v.ct_rds] *)
        List.exists
          (fun (v,cc) -> List.mem f.name cc.ct_wrs && List.mem f'.name cc.ct_rds)
          ctx.shared
    end

  let dep_sort ctx fsms =
    (* Sort FSMs using the [FsmStatNode.depends_on] relation.
       The resulting order will used by the SystemC (and possibly other) backend to implement instantaneous broadcast
       using delta cycles *)
    let module D = Depg.Make(FsmNode) in
    D.dep_sort ctx fsms

  (* Main function *)

  let build tp p =
    let m, c = r_program tp p in
    { ctx = c;
      models = p.Syntax.models;
      fsms = m;
      globals = r_globals p;
      fns = p.Syntax.fun_decls;
      csts = p.Syntax.cst_decls;
      types = p.Syntax.type_decls;
      dep_order = m |> dep_sort c |> List.mapi (fun i f -> f.name, i) }

  let is_rtl acts = (* Is the sequence of actions [acts] RTL ? *)
    let module S = Set.Make(Ident) in
    let wvars_of_action a = (* Note: this function is copied from [Syntax] *)
      let open Syntax in
      match a.Annot.desc with
      | Emit _ -> S.empty
      | Assign (lhs,_) -> S.of_list (Guest.vars_of_lhs lhs) in
    let rec scan acc acts = match acts with
        [] -> true
      | a::rest ->
         let wvs = wvars_of_action a in (* The set of variables written by [a] *)
         if S.is_empty (S.inter acc wvs) 
         then scan (S.union acc wvs) rest  (* Ok. None has already be written .. *)
         else false in
    scan S.empty acts
              
  let is_rtl_model { Annot.desc = m; _ } =
    let is_rtl_transition { Annot.desc = _,_,acts,_,_; _ } = is_rtl acts in
    let is_rtl_itransition { Annot.desc = _,acts; _ } = is_rtl acts in
       List.for_all is_rtl_transition m.Syntax.trans
    && is_rtl_itransition m.Syntax.itrans

  let is_rtl f = is_rtl_model f.model
end
