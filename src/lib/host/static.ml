module type STATIC = sig
  
  module Syntax: Syntax.SYNTAX
  module Typing: Guest.TYPING
  module Value: Guest.VALUE 

  type fsm = {
      name: string;
      model: Syntax.model;
      q: string;
      vars: Value.value Env.t;
    }
  
  type ctx = {  (* TODO : update formal semantics accordingly *)
    inputs: (string * Typing.Types.typ) list;  
    outputs: (string * Typing.Types.typ) list;  
    shared: (string * Typing.Types.typ) list;  
    }

  type t = {
    ctx: ctx;
    models: Syntax.model list;
    fsms: fsm list;
    }

  val build: Syntax.program -> t

  val dep_sort: fsm list -> fsm list (** Dependency-based sorting *)

  val pp: Format.formatter -> t -> unit
  val pp_fsm: ?verbose_level:int -> Format.formatter -> fsm -> unit

end

module Make
         (HS: Syntax.SYNTAX)
         (GT: Guest.TYPING with type Types.typ = HS.typ)
         (GV: Guest.VALUE with type typ = HS.typ) =
struct

  module Syntax = HS
  module Value = GV
  module Typing = GT

  type fsm = {
      name: string;
      model: Syntax.model;
      q: string;
      vars: Value.value Env.t;
    } [@@deriving show {with_path=false}]

  let pp_fsm ?(verbose_level=2) fmt f = 
    let open Format in
    match verbose_level with
    | 0 -> Format.fprintf fmt "%s" f.name  (* Name only *)
    | 1 -> (* With model name only *)
       Format.fprintf fmt "{name=%s; model=%s; q=%s; vars=%a}"
         f.name
         f.model.Annot.desc.name
         f.q
         (Env.pp Value.pp_value) f.vars
    | _ -> (* Full *)
       fprintf fmt "@[<v>[@,name=%s@,model=%a@,q=%s@,vars=%a@,]@]@."
         f.name
         Syntax.pp_model f.model
         f.q
         (Env.pp Value.pp_value) f.vars

  type ctx = {
    inputs: (string * Typing.Types.typ) list;  
    outputs: (string * Typing.Types.typ) list;  
    shared: (string * Typing.Types.typ) list;  
    } 

  let pp_ctx fmt ctx =
    let open Format in
    let pp_io fmt (id,ty) = Format.fprintf fmt "%s: %a" id Typing.Types.pp_typ ty in
    fprintf fmt "@[<v>[inputs=[%a]@,outputs=[%a]@,shared=[%a]]@]"
    (Misc.pp_list_h ~sep:", " pp_io) ctx.inputs
    (Misc.pp_list_h ~sep:", " pp_io) ctx.outputs
    (Misc.pp_list_h ~sep:", " pp_io) ctx.shared

  type t = {
    ctx: ctx;
    models: Syntax.model list;
    fsms: fsm list;
    }

  let pp fmt s = 
    let open Format in
    fprintf fmt "@[<v>[ctx=%a@,models=[%a]@,fsms=[%a]@]@."
    pp_ctx s.ctx
    (Misc.pp_list_h Syntax.pp_model_name) s.models
    (Misc.pp_list_v (pp_fsm ~verbose_level:1)) s.fsms

  (* Rules *)
         
  let r_inst (senv_m,senv_i) { Annot.desc=name,model,args; Annot.loc=loc; _ } =
    let open Syntax in
    let mm = 
      try Env.find model senv_m
      with Not_found -> Misc.fatal_error "Static.r_inst"  in (* should not happen after TC *)
    let m = mm.Annot.desc in
    let phi = 
      try List.map2 (fun (io',_) io -> io', io) (m.inps @ m.outps) args
      with Invalid_argument _ -> Misc.fatal_error "Static.r_inst" in  (* should not happen after TC *)
    { name = name;
      model = subst_model ~phi mm;
      q = fst m.itrans.Annot.desc; (* q0 *)
      vars = List.fold_left (fun env (id,te) -> Env.add id (Value.default_value te.Annot.typ) env) Env.empty m.vars
    }

  let r_insts (senv_m,senv_i) insts = List.map (r_inst (senv_m,senv_i)) insts

  let r_io env { Annot.desc=id,cat,ty,_; _ } = 
    Env.add id (cat,ty) env
    
  let r_ios ios = List.fold_left r_io Env.empty ios
                
  let r_model env m =
    Env.add m.Annot.desc.Syntax.name m env
    
  let r_models models = List.fold_left r_model Env.empty models

  let build_ctx senv_i =  (* \mathcal{L} *)
    let open Syntax in
    (* let f cat h senv = senv |> Env.filter (fun id v -> match v with cat', t -> cat'=cat && h t) |> Env.dom in *)
    let type_of te =
      match te.Annot.typ with
      | Some ty -> ty
      | _ -> Misc.fatal_error "Static.build_ctx" in
    let extract cat acc senv =
      Env.fold 
      (fun id (cat',te) acc ->
        if cat'= cat then (id,type_of te)::acc 
        else acc)
      senv
      [] in
    { inputs = extract Input [] senv_i;
      outputs = extract Output [] senv_i;
      shared = extract Shared [] senv_i; }

  let r_program p =
    let senv_m = r_models p.Syntax.models in
    let senv_i = r_ios p.Syntax.ios in
    let m = r_insts (senv_m, senv_i) p.Syntax.insts in
    let c = build_ctx senv_i in
    m, c
    
  let build p =
    let m, c = r_program p in
    { ctx = c; models = p.Syntax.models; fsms = m }
              
  (* Dependency-based sorting *)
    
  module G = Graph.Imperative.Digraph.Abstract(String) (* Vertices are FSM names *)
  module M = Map.Make(String) (* For mapping FSM names to vertices and FSM descriptions *)
  module TS = Graph.Topological.Make(G)

  let depends_on m m' =
    let module S = Set.Make(String) in
    let inter l1 l2 = not (S.is_empty (S.inter (S.of_list l1) (S.of_list l2))) in
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
    let evs', rvs', _, _ = Syntax.state_ios m'.model m'.q in
    let _, _, evs, wvs = Syntax.state_ios m.model m.q in (* TODO ? Filter out non shared events / vars ? *)
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

  let dep_sort fsms =
    (* Sort FSMs using the [depends_on] ($\leq$) relation.
       The resulting order is used by [Dynamic.ReactEv] to sequence the reactions of FSMs at a given instant. *)
    (* TODO: this could be pre-computed statically for each ((M,q),(M',q')) pair ? *)
    let g = G.create () in (* The graph of dependencies *)
    let vs, fs = 
      (* [vs] is the table of vertices, indexed by FSM names,
         [fs] is the table of FSMs, also indexed by FSM names *)
    List.fold_left
      (fun (acc,acc') f ->
        let v = G.V.create f.name in
        G.add_vertex g v; (* Add vertices to the graph *)
        M.add f.name v acc,
        M.add f.name f acc')
      (M.empty, M.empty)
      fsms in
    List.iter (* Add edges *)
      (fun (m,m') ->
        if m.name <> m'.name && depends_on (M.find m.name fs) (M.find m'.name fs) then
          (* Add an edge m->m' in the graph iff m' depends on m. Omit self-dependencies *)
          G.add_edge g (M.find m.name vs) (M.find m'.name vs))
      (Misc.list_cart_prod2 fsms fsms);
    let fs' = TS.fold (fun v acc -> M.find (G.V.label v) fs :: acc) g [] in
    (* The result order is then simply obtained by a topological sort of the graph *)
    List.rev fs'

end
