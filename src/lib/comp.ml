(* Internal representation of programs as a composition of FSMs *)

open Utils
(* open Syntax *)

module DepG =
  Graph.Imperative.Digraph.AbstractLabeled
    (struct type t = string end)  (* Only abstract vertices allow imperative marking .. *)
    (struct
      type t = string
      let compare = Pervasives.compare
      let default = ""
    end)

type t = {
  m_name: string;
  (* m_fsm_models: Fsm.model list; *)
  m_fsms: Fsm.inst list;
  m_inputs: (string * global) list; 
  m_outputs: (string * global) list; 
  m_shared: (string * global) list; 
  m_stimuli: Stimuli.stimuli list;
  m_deps: dependencies;
  }

and global = Types.typ * mg_desc 
      
and mg_desc =
  | MInp of istim_desc * string list     (** stimuli desc, reader(s) *)
  | MOutp of string list                 (** writer(s) *)
  | MShared of string list * string list (** writer(s), reader(s) *)

and istim_desc = {
  sd_comprehension: Fsm.stim_desc;
  sd_extension: Stimuli.event list
  }
               
and dependencies = {
    md_graph: DepG.t;
    md_node: string -> DepG.V.t;
    }

(* exception Unknown_global of string *)
                          
(* let type_of_global gls id =
 *   try fst (List.assoc id gls)
 *   with Not_found -> raise (Unknown_global id) *)
                  
(* let bind_param loc (p,te) v =
 *   match Types.type_of_type_expression [] ~strict:true te, v with
 *     (\* Note: the type of a parameter cannot involve another parameter *\)
 *     Types.TyInt _ as ty, Expr.Val_int c -> p, (ty,v)
 *   | _, _ -> Error.invalid_param_value loc p v
 *   
 * let build_fsm_model { fsm_desc=f; fsm_loc=loc } =
 *   let open Fsm in
 *   let mk_cond { Syntax.cond_desc = e, guards } = (e, guards) in
 *   let mk_act a = a.Syntax.act_desc in
 *   let mk_trans (q,cond,acts,q') = q, (mk_cond cond, List.map mk_act acts, false), q' in
 *   let q0, iacts = fst f.fd_itrans, List.map mk_act (snd f.fd_itrans) in
 *   let mk_ios dir =
 *        f.fd_ios
 *     |> List.filter (function (dir', _) when dir=dir' -> true | _ -> false)
 *     |> List.map (function (_,(id,te)) -> id, Types.type_of_type_expression [] ~strict:false te) in
 *   let mk_var (id,te) =
 *       let ty = Types.type_of_type_expression [] ~strict:false te in
 *       (id,(ty,None)) in
 *   let r = {
 *         f_name = f.fd_name;
 *         f_model = "";
 *         f_repr =
 *           begin try Repr.create
 *                    ~states:f.fd_states
 *                    ~itrans:[(([],[]),iacts,false),q0]
 *                    ~trans:(List.map mk_trans f.fd_trans)
 *           with Repr.Invalid_state s ->
 *                Error.invalid_state f.fd_name s end;
 *         f_params =
 *           List.map
 *             (function (id,te) -> id, (Types.type_of_type_expression [] ~strict:false te, Expr.Val_int 0))
 *             f.fd_params;
 *         f_inps = mk_ios IO_In;
 *         f_outps = mk_ios IO_Out;
 *         f_inouts = mk_ios IO_InOut;
 *         f_vars = List.map mk_var f.fd_vars;
 *         f_l2g = (function _ -> "");
 *         f_resolve = None; (\* TO FIX *\)
 *         f_state = "";  (\* current state is not defined until the initial transition has been carried out *\)
 *         f_has_reacted = false;
 *       } in
 *   sanity_check r;
 *   r
 * 
 * let build_fsm_instance (inputs,outputs,shared) p =
 *   let open Fsm in
 *   function ({ fi_desc=f; fi_loc=loc } as fi) ->
 *     let m =
 *       (try List.find (function m' -> m'.fsm_desc.fd_name = f.fi_model) p.p_fsm_models
 *       with Not_found -> Error.unbound_fsm loc f.fi_model).fsm_desc in
 *     let params =
 *       try List.map2 (bind_param loc) m.fd_params f.fi_params
 *       with Invalid_argument _ -> Error.fsm_mismatch "parameter(s)" fi in
 *     let params' = List.map (function id,(ty,v) -> id, v) params in
 *     let subst_guard (e1,op,e2) = Expr.subst params' e1, op, Expr.subst params' e2 in
 *     let subst_cond { Syntax.cond_desc = e, guards } = (e, List.map subst_guard  guards) in
 *     let subst_act act = match act.Syntax.act_desc with
 *       | Action.Assign (i,e) -> Action.Assign (i, Expr.subst params' e)
 *       | act -> act in
 *     let subst_acts acts = List.map subst_act acts in
 *     let mk_trans (q,cond,acts,q') = q, (subst_cond cond, subst_acts acts, false), q' in (\* not initial condition *\)
 *     let q0, iacts = fst m.fd_itrans, subst_acts (snd m.fd_itrans) in
 *     let type_of_global id =
 *       try fst (List.assoc id (inputs @ outputs @ shared))
 *       with Not_found -> Error.unbound_global loc id in
 *     let bind_io (is,os,ios) (tag,(id,te)) id' =
 *       let ty = Types.type_of_type_expression params' ~strict:true te in
 *       let ty' = type_of_global id' in
 *       let type_check what id =
 *         if not (Types.type_equal ty ty') 
 *         then Error.type_mismatch loc what id (Types.string_of_type ty) (Types.string_of_type ty') in
 *       begin match tag with
 *       | IO_In ->
 *          if List.mem_assoc id' (inputs @ shared) then (type_check "input" id; ((id,ty)::is, os, ios))
 *          else Error.io_mismatch loc "input" id
 *       | IO_Out ->
 *          if List.mem_assoc id' (outputs @ shared) then (type_check "output" id; (is, (id,ty)::os, ios))
 *          else Error.io_mismatch loc "output" id
 *       | IO_InOut ->
 *          if List.mem_assoc id' shared then (type_check "inout" id; (is, os, (id,ty)::ios))
 *          else Error.io_mismatch loc "shared" id
 *       end in
 *     let inps, outps, inouts =
 *       try List.fold_left2 bind_io ([],[],[]) m.fd_ios f.fi_args 
 *       with Invalid_argument _ -> Error.fsm_mismatch "input/output(s)" fi in
 *     let mk_var (id,te) =
 *       let ty = Types.type_of_type_expression params' ~strict:true te in
 *       (id,(ty,None)) in
 *     let r = {
 *         f_name = f.fi_name;
 *         f_model = f.fi_model;
 *         f_repr =
 *           begin try Repr.create
 *                    ~states:m.fd_states
 *                    ~itrans:[(([],[]),iacts,false),q0]
 *                    ~trans:(List.map mk_trans m.fd_trans)
 *           with Repr.Invalid_state s ->
 *                Error.invalid_state f.fi_name s end;
 *         f_params = params;
 *         f_inps = inps;
 *         f_outps = outps;
 *         f_inouts = inouts;
 *         f_vars = List.map mk_var m.fd_vars;
 *         f_l2g =
 *           Fsm.mk_bindings
 *             ~local_names:(List.map (function (_,(id,_)) -> id) m.fd_ios)
 *             ~global_names:f.fi_args;
 *         f_resolve = None; (\* TO FIX *\)
 *         f_state = "";  (\* current state is not defined until the initial transition has been carried out *\)
 *         f_has_reacted = false;
 *       } in
 *   sanity_check r;
 *   r *)

let build_stim st =
  let mk_ext = function
    | Fsm.Periodic (per,t1,t2) -> Stimuli.mk_per_event per t1 t2
    | Fsm.Sporadic ts -> Stimuli.mk_spor_event ts
    | Fsm.ValueChange vs -> Stimuli.mk_val_changes vs in
 { sd_comprehension = st; sd_extension = mk_ext st }

(* let extract_globals (inputs,outputs,shared) { g_desc=g } = match g.gd_desc with
 *     | GInp st ->
 *        (g.gd_name, (Types.type_of_type_expression [] ~strict:false g.gd_type, MInp (build_stim g.gd_name st))) :: inputs,
 *        outputs,
 *        shared
 *     | GOutp ->
 *        inputs,
 *        (g.gd_name, (Types.type_of_type_expression [] ~strict:false g.gd_type, MOutp)) :: outputs,
 *        shared
 *     | GShared ->
 *        inputs,
 *        outputs,
 *        (g.gd_name, (Types.type_of_type_expression [] ~strict:false g.gd_type, MShared ([],[]))) :: shared *)


let fold_left f l acc = List.fold_left f acc l
  (* This variant allows [fold_left]s to be chained with [|>] *)
          
let extract_globals (inputs,outputs,shared) f =
  let add_reader (ty,mg) id = match mg with
      MInp (sd, rdrs) -> ty, MInp (sd, id::rdrs)
    | MShared (wrs, rdrs) -> ty, MShared (wrs, id::rdrs)
    | m -> ty, m in
  let add_writer (ty,mg) id = match mg with
      MOutp wrs -> ty, MOutp (id::wrs)
    | MShared (wrs, rdrs) -> ty, MShared (id::wrs, rdrs)
    | m -> ty, m in
  let add_reader_writer (ty,mg) id = match mg with
    | MShared (wrs, rdrs) -> ty, MShared (id::wrs, id::rdrs)
    | m -> ty, m in
  (inputs,outputs,shared)
  |> fold_left
      (fun (inps,outps,shrds) (_,(_,gl)) -> match gl with
       | Fsm.GInp (id,ty,sd) ->
          if List.mem_assoc id inps
          then ListExt.update_assoc add_reader id f.Fsm.f_name inps, outps, shrds
          else (id, (ty, MInp (build_stim sd, [f.Fsm.f_name]))) :: inps, outps, shrds
       | Fsm.GShared (id,ty) ->
          if List.mem_assoc id shrds
          then inps, outps, ListExt.update_assoc add_reader id f.Fsm.f_name shrds
          else inps, outps, (id, (ty, MShared ([], [f.Fsm.f_name]))) :: shrds
       | _ -> inps, outps, shrds)
      f.f_inps
  |> fold_left
      (fun (inps,outps,shrds) (_,(_,gl)) -> match gl with
       | Fsm.GOutp (id,ty) ->
          if List.mem_assoc id outps
          then inps, ListExt.update_assoc add_writer id f.Fsm.f_name outps, shrds
          else inps, (id, (ty, MOutp [f.Fsm.f_name])) :: outps, shrds
       | Fsm.GShared (id,ty) ->
          if List.mem_assoc id shrds
          then inps, outps, ListExt.update_assoc add_writer id f.Fsm.f_name shrds
          else inps, outps, (id, (ty, MShared ([f.Fsm.f_name],[]))) :: shrds
       | _ -> inps, outps, shrds)
      f.f_outps
  |> fold_left
      (fun (inps,outps,shrds) (_,(_,gl)) -> match gl with
       | Fsm.GShared (id,ty) ->
          if List.mem_assoc id shrds
          then inps, outps, ListExt.update_assoc add_reader_writer id f.Fsm.f_name shrds
          else inps, outps, (id, (ty, MShared ([f.Fsm.f_name],[]))) :: shrds
       | _ -> inps, outps, shrds)
      f.f_inouts 

let build_dependencies fsms shared =
  let g = DepG.create () in
  let nodes = ref [] in
  let lookup n = try List.assoc n !nodes with Not_found -> failwith "Model.build_dependencies.lookup" in
  List.iter
    (function f ->
       let v = DepG.V.create f.Fsm.f_name in
       nodes := (f.Fsm.f_name, v) :: !nodes;
       DepG.add_vertex g v)
    fsms;
  List.iter   (* Compute dependency graph *)
    (function
       id,(ty,MShared (wrs,rrs)) ->
         List.iter
           (function (s,d) ->
              if s <> d then let e = DepG.E.create (lookup s) id (lookup d) in DepG.add_edge_e g e)
           (* Self-dependencies are deliberately not taken into account *)
           (ListExt.cart_prod2 wrs rrs)
     | _ -> failwith "Model.build_dependencies" (* should not happen *))
  shared;
  let update_dep_depth n =
    match DepG.pred g n with
      [] -> ()
    | preds ->
       let m = List.fold_left (fun z n' -> max z (DepG.Mark.get n')) 0  preds in
       DepG.Mark.set n (m+1) in
  let module T = Graph.Topological.Make(DepG) in
  DepG.Mark.clear g;
  T.iter update_dep_depth g; (* Set dependency depths *)
  { md_graph = g;
    md_node = function n -> try List.assoc n !nodes with Not_found -> failwith "Model.md_node " }

let build_composite ~name ~fsm_insts =
  let inputs, outputs, shared = List.fold_left extract_globals ([],[],[]) fsm_insts in
  let mk_stimuli = function
      name, (ty, MInp ({sd_extension=evs}, _)) -> List.map (Stimuli.mk_stimuli name) evs
    | _ -> failwith "Model.mk_stimuli" (* should not happen *) in
  let stimuli = List.map mk_stimuli inputs in
  (* let get_writers, get_readers =
   *   let get ios id = 
   *     List.fold_left
   *       (fun acc f ->
   *         let global_id (id,_) = f.Fsm.f_l2g id in
   *         if List.mem id (List.map global_id (ios f)) then Expr.VarSet.add f.Fsm.f_name acc else acc)
   *       Expr.VarSet.empty
   *       fsm_insts in
   *   (function id -> Expr.VarSet.elements (get (function f -> f.Fsm.f_outps @ f.Fsm.f_inouts) id)),
   *   (function id -> Expr.VarSet.elements (get (function f -> f.Fsm.f_inps @ f.Fsm.f_inouts) id)) in
   * let annotate_shared = function
   *   | id, (ty,MShared _) -> id, (ty, MShared (get_writers id, get_readers id))
   *   | _ -> failwith "Model.annotated_shared" (\* should not happen *\) in
   * let shared' = List.map annotate_shared shared in *)
  { m_name = name;
    (* m_fsm_models = fsm_models; *)
    m_fsms = fsm_insts;
    m_inputs = inputs;
    m_outputs = outputs;
    (* m_shared = shared'; *)
    m_shared = shared;
    m_stimuli = Stimuli.merge_stimuli stimuli;
    m_deps = build_dependencies fsm_insts shared;
   }

(* let build_model name (p: Syntax.program) =
 *   let inputs, outputs, shared = List.fold_left extract_globals ([],[],[]) p.p_globals in
 *   let mk_stimuli = function
 *       name, (ty, MInp { sd_extension = evs }) -> List.map (Stimuli.mk_stimuli name) evs
 *     | _ -> failwith "Model.mk_stimuli" (\* should not happen *\) in
 *   let stimuli = List.map mk_stimuli inputs in
 *   let fsm_models = List.map build_fsm_model p.p_fsm_models in
 *   let fsm_insts = List.map (build_fsm_instance (inputs,outputs,shared) p) p.p_fsm_insts in
 *   let get_writers, get_readers =
 *     let get ios id = 
 *       List.fold_left
 *         (fun acc f ->
 *           let global_id (id,_) = f.Fsm.f_l2g id in
 *           if List.mem id (List.map global_id (ios f)) then Expr.VarSet.add f.Fsm.f_name acc else acc)
 *         Expr.VarSet.empty
 *         fsm_insts in
 *     (function id -> Expr.VarSet.elements (get (function f -> f.Fsm.f_outps @ f.Fsm.f_inouts) id)),
 *     (function id -> Expr.VarSet.elements (get (function f -> f.Fsm.f_inps @ f.Fsm.f_inouts) id)) in
 *   let annotate_shared = function
 *     | id, (ty,MShared _) -> id, (ty, MShared (get_writers id, get_readers id))
 *     | _ -> failwith "Model.annotated_shared" (\* should not happen *\) in
 *   let shared' = List.map annotate_shared shared in
 *   { m_name = name;
 *     m_fsm_models = fsm_models;
 *     m_fsm_insts = fsm_insts;
 *     m_inputs = inputs;
 *     m_outputs = outputs;
 *     m_shared = shared';
 *     m_stimuli = Stimuli.merge_stimuli stimuli;
 *     m_deps = build_dependencies fsm_insts shared';
 *    } *)

(* DOT output *)

let string_of_shared (name, (ty, desc)) =
  let pfx = match desc with MInp _ -> "input" | MOutp _ -> "output" | MShared _ -> "shared" in
  pfx ^ " " ^ name ^ ":" ^ Types.string_of_type ty

let dot_output dir ?(dot_options=[]) ?(fsm_options=[]) ?(with_insts=false) ?(with_models=false) m =
  let rankdir = if List.mem Utils.Dot.RankdirLR dot_options then "LR" else "UD" in
  let layout, mindist = if List.mem Ltsa.Circular dot_options then "circo", 1.5 else "dot", 1.0 in
  let dump_header oc name =
     Printf.fprintf oc "digraph %s {\nlayout = %s;\nrankdir = %s;\nsize = \"8.5,11\";\nlabel = \"\"\n center = 1;\n nodesep = \"0.350000\"\n ranksep = \"0.400000\"\n fontsize = 14;\nmindist=\"%1.1f\"\n" name layout rankdir mindist in
  if with_insts then 
    List.iter
      (Fsm.dot_output ~dot_options:dot_options ~options:(GlobalNames::fsm_options) ~dir:dir)
      m.m_fsms;
  if with_models then 
    List.iter
      (function f -> Fsm.dot_output_model ~dot_options:dot_options ~options:fsm_options ~dir:dir f.Fsm.f_model)
      m.m_fsms;
  let fname = Filename.concat dir (m.m_name ^ "_top.dot") in
  let oc = open_out fname in
  dump_header oc m.m_name;
  List.iter
    (Fsm.dot_output_oc oc ~dot_options:(Utils.Dot.SubGraph::dot_options) ~options:(GlobalNames::fsm_options))
    m.m_fsms;
  let caption = StringExt.concat_sep "\\r" 
            [ListExt.to_string string_of_shared "\\r" m.m_inputs;
             ListExt.to_string string_of_shared "\\r" m.m_outputs;
             ListExt.to_string string_of_shared "\\r" m.m_shared] in
  Printf.fprintf oc "%s_globals [label=\"%s\", shape=rect, style=rounded]\n" m.m_name caption;
  Printf.fprintf oc "}\n";
  Logfile.write fname;
  close_out oc
  
(* Printing *)

let dump_global oc (name,(ty,g_desc)) = match g_desc with
  | MInp ({sd_extension=evs},rdrs) ->
     Printf.fprintf oc "INPUT %s : %s = %s [-> %s]\n"
       name
       (Types.string_of_type ty)
       (Stimuli.string_of_events evs)
       (ListExt.to_string Misc.id "," rdrs)
  | MOutp wrs ->
     Printf.fprintf oc "OUTPUT %s : %s [<- %s]\n"
       name
       (Types.string_of_type ty)
       (ListExt.to_string Misc.id "," wrs)
  | MShared (wrs,rrs) ->
     Printf.fprintf oc "SHARED %s : %s [<- %s] [-> %s])\n"
       name
       (Types.string_of_type ty)
       (ListExt.to_string Misc.id "," wrs)
       (ListExt.to_string Misc.id "," rrs)

let dump_stimuli oc st =
  Printf.fprintf oc "%s\n" (Stimuli.string_of_stimuli st)

let dump_dependencies m =
  let module G = struct
      include DepG
      let edge_attributes e = [`Label (DepG.E.label e)]
      let default_edge_attributes _ = []
      let vertex_attributes v = [`Label (DepG.V.label v ^ "\\n" ^ (string_of_int (DepG.Mark.get v)))]
      let default_vertex_attributes _ = []
      let vertex_name v = DepG.V.label v
      let graph_attributes _ = []
      let get_subgraph _ = None
    end in
  let module D = Graph.Graphviz.Dot(G) in
  let fname = Filename.concat "." (m.m_name ^ "_deps.dot") in
  let oc = open_out fname in
  D.output_graph oc m.m_deps.md_graph;
  Printf.printf "Wrote file %s\n" fname;  (* Do not add to rfsm.output *)
  close_out oc
  
let dump oc m =
  (* List.iter (Fsm.dump_model stdout) m.m_fsm_models; *)
  List.iter (Fsm.dump_inst oc) m.m_fsms;
  List.iter (dump_global oc) m.m_inputs;
  List.iter (dump_global oc) m.m_outputs;
  List.iter (dump_global oc) m.m_shared;
  List.iter (dump_stimuli oc) m.m_stimuli;
  dump_dependencies m
  
