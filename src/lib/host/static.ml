(** Dependency graph between FSM instances *)

module type T = sig
  
  module Syntax: Syntax.SYNTAX
  module Value: Guest.VALUE with type typ = Syntax.typ

  (** FSM instances *)
  type fsm = {
      name: string;
      model: Syntax.model;    (* Normalized model (without output valuations) *)
      params: Value.t Env.t;
      q: string;
      vars: Value.t Env.t;
    }
  
  type ctx_comp = {
    ct_typ: Syntax.typ;
    ct_stim: Syntax.stimulus_desc option;  (* For inputs only *)
    ct_rds: string list; (* Readers, for inputs and shareds *)
    ct_wrs: string list; (* Writers, for outputs and shareds *)
    }

  type ctx = {  (* TODO : update formal semantics accordingly *)
    inputs: (string * ctx_comp) list;
    outputs: (string * ctx_comp) list;
    shared: (string * ctx_comp) list;
    }

  type t = {
    ctx: ctx;
    models: Syntax.model list; (** Original, un-normalized models *)
    fsms: fsm list;
    globals: Value.t Env.t; (** Functions and constants *)
    fns: Syntax.fun_decl list; (** Functions *)
    csts: Syntax.cst_decl list; (** Constants *)
    types: Syntax.Guest.type_decl list; (** User-defined types *)
    dep_order: (string * int) list; 
    }

  val build: Syntax.program -> t

  val pp: ?verbose_level:int -> Format.formatter -> t -> unit
  val pp_fsm: ?verbose_level:int -> Format.formatter -> fsm -> unit

end

module Make
         (HS: Syntax.SYNTAX)
         (GV: Guest.VALUE with type typ = HS.typ)
         (GS: Guest.STATIC with type expr = HS.expr and type value = GV.t)
  : T with module Syntax = HS
       and module Value = GV =
struct

  module Syntax = HS
  module Value = GV

  type fsm = {
      name: string;
      model: Syntax.model;
      params: Value.t Env.t;
      q: string;
      vars: Value.t Env.t;
    } [@@deriving show {with_path=false}]

  let pp_fsm ?(verbose_level=2) fmt f  = 
    let open Format in
    match verbose_level with
    | 0 -> Format.fprintf fmt "%s" f.name  (* Name only *)
    | 1 -> (* With model name, state and vars only *)
       fprintf fmt "@[<h>{@,name=%s,@,q=%s,@,vars=%a}@]"
         f.name
         f.q
         (Env.pp Value.pp) f.vars
    | _ -> (* Full *)
       fprintf fmt "@[<v>{@,name=%s@,model=%s@,params=%a@,q=%s@,vars=%a}@]"
         f.name
         f.model.Annot.desc.name
         (Env.pp Value.pp) f.params
         f.q
         (Env.pp Value.pp) f.vars

  type ctx_comp = {
    ct_typ: Syntax.typ [@printer fun fmt -> fprintf fmt "%a" (Syntax.Guest.Types.pp_typ ~abbrev:false)];
    ct_stim: Syntax.stimulus_desc option;  (* Attached stimuli, for inputs only *)
    ct_rds: string list; (* Readers, for inputs and shareds *)
    ct_wrs: string list; (* Writers, for outputs and shareds *)
    } [@@deriving show {with_path=false}]

  type ctx = {  (* TODO : update formal semantics accordingly *)
    inputs: (string * ctx_comp) list;
    outputs: (string * ctx_comp) list;
    shared: (string * ctx_comp) list;
    }

  let pp_ctx fmt ctx =
    let open Format in
    let pp_io fmt (id,cc) = Format.fprintf fmt "%s: %a" id pp_ctx_comp cc in
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
    dep_order: (string * int) list; 
    }

  let pp ?(verbose_level=1) fmt s =
    let open Format in
    fprintf fmt "@[<v>{ctx=%a@,models=%a@,fsms=%a@,globals=%a@,dep_order=%a@,}@."
    pp_ctx s.ctx
    (Misc.pp_list_v Syntax.pp_model) s.models
    (Misc.pp_list_v (pp_fsm ~verbose_level)) s.fsms
    (Env.pp Value.pp) s.globals
    (Misc.pp_list_h ~sep:"," (fun fmt (f,d) -> fprintf fmt "%s:%d" f d)) s.dep_order

  (* Rules *)
         
  let r_inst (senv_m,senv_i) { Annot.desc=name,model,params,args; Annot.loc=loc; _ } =
    let open Syntax in
    let mm = 
      try Env.find model senv_m
      with Not_found -> Misc.fatal_error "Static.r_inst"  in (* should not happen after TC *)
    let m = mm.Annot.desc in
    let phi = 
      try List.map2 (fun (io',_) io -> io', io) m.ios args
      with Invalid_argument _ -> Misc.fatal_error "Static.r_inst" in  (* should not happen after TC *)
    let collect cats =
      List.fold_left2 
        (fun acc (id,(cat,_)) arg -> if List.mem cat cats then  arg::acc else acc)
        []
        m.ios
        args in
    let rds = collect [Syntax.In; Syntax.InOut] in
    let wrs = collect [Syntax.Out; Syntax.InOut] in
    let m = {
        name = name;
        params =
          (try List.fold_left2 (fun env (id,_) expr -> Env.add id (GS.eval expr) env) Env.empty m.params params
           with Invalid_argument _ -> Misc.fatal_error "Static.r_inst");  (* should not happen after TC *)
        model = mm |> normalize_model |> subst_model ~phi;
        q = fst m.itrans.Annot.desc;
        vars = List.fold_left (fun env (id,te) -> Env.add id (Value.default_value te.Annot.typ) env) Env.empty m.vars
      } in
    m, (name, (rds,wrs))

  let r_insts (senv_m,senv_i) insts = List.map (r_inst (senv_m,senv_i)) insts

  let r_global env { Annot.desc=id,cat,ty,st; _ } = 
    Env.add id (cat,ty,st) env
    
  let r_globals gls = List.fold_left r_global Env.empty gls
                
  let r_model env m =
    Env.add m.Annot.desc.Syntax.name m env
    
  let r_models models = List.fold_left r_model Env.empty models

  let build_ctx rws senv_i =  (* \mathcal{L} *)
    let open Syntax in
    let type_of te =
      match te.Annot.typ with
      | Some ty -> ty
      | _ -> Misc.fatal_error "Static.build_ctx" in
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

  let r_program p =
    let senv_m = r_models p.Syntax.models in
    let senv_i = r_globals p.Syntax.globals in
    let m, rws = List.split @@ r_insts (senv_m, senv_i) p.Syntax.insts in
    let c = build_ctx rws senv_i in
    m, c
    
  let r_const env { Annot.desc = c; _ } = 
    let open Syntax in
     Env.add c.cc_name (GS.eval c.cc_val) env  (* TODO: fold env so that a defn can depend on a previous one *)

  let r_fun env { Annot.desc = f; _ } = 
    let open Syntax in
    Env.add f.ff_name (GS.eval_fn (List.map fst f.ff_args) f.ff_body) env 

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

  let build p =
    let m, c = r_program p in
    { ctx = c;
      models = p.Syntax.models;
      fsms = m;
      globals = r_globals p;
      fns = p.Syntax.fun_decls;
      csts = p.Syntax.cst_decls;
      types = p.Syntax.type_decls;
      dep_order = m |> dep_sort c |> List.mapi (fun i f -> f.name, i) }
              
end
