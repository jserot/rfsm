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

open Utils

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

let build_stim st =
  let mk_ext = function
    | Fsm.Periodic (per,t1,t2) -> Stimuli.mk_per_event per t1 t2
    | Fsm.Sporadic ts -> Stimuli.mk_spor_event ts
    | Fsm.ValueChange vs -> Stimuli.mk_val_changes vs in
 { sd_comprehension = st; sd_extension = mk_ext st }

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

let build ~name ~fsm_insts =
  let inputs, outputs, shared = List.fold_left extract_globals ([],[],[]) fsm_insts in
  let mk_stimuli = function
      name, (ty, MInp ({sd_extension=evs}, _)) -> List.map (Stimuli.mk_stimuli name) evs
    | _ -> failwith "Model.mk_stimuli" (* should not happen *) in
  let stimuli = List.map mk_stimuli inputs in
  { m_name = name;
    (* m_fsm_models = fsm_models; *)
    m_fsms = fsm_insts;
    m_inputs = inputs;
    m_outputs = outputs;
    m_shared = shared;
    m_stimuli = Stimuli.merge_stimuli stimuli;
    m_deps = build_dependencies fsm_insts shared;
   }

(* DOT output *)

let string_of_shared (name, (ty, desc)) =
  let pfx = match desc with MInp _ -> "input" | MOutp _ -> "output" | MShared _ -> "shared" in
  pfx ^ " " ^ name ^ ":" ^ Types.string_of_type ty

let dot_output dir ?(dot_options=[]) ?(fsm_options=[]) ?(with_insts=false) ?(with_models=false) m =
  let rankdir = if List.mem Utils.Dot.RankdirLR dot_options then "LR" else "UD" in
  let layout, mindist = if List.mem Lascar.Ltsa.Circular dot_options then "circo", 1.5 else "dot", 1.0 in
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
  
