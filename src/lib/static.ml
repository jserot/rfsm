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
  m_models : Fsm.model list;
  m_fsms: Fsm.inst list;
  m_inputs: (string * global) list; 
  m_outputs: (string * global) list; 
  m_types: (string * Types.typ) list; 
  m_fns: (string * global) list; 
  m_consts: (string * global) list; 
  m_shared: (string * global) list; 
  m_deps: dependencies;
  }

and global = Types.typ * mg_desc 
      
and mg_desc =
  | MInp of Global.stim_desc * string list     (** stimuli desc, reader(s) *)
  | MOutp of string list                 (** writer(s) *)
  | MFun of string list * Expr.t         (** args, body *)
  | MConst of Expr.value                 (** value *)
  | MShared of string list * string list (** writer(s), reader(s) *)

and dependencies = {
    md_graph: DepG.t;
    md_node: string -> DepG.V.t;
    }

let extract_globals (inputs,outputs,shared) f =
  let name = f.Fsm.f_name in
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
  |> Misc.fold_left
      (fun (inps,outps,shrds) (_,(_,gl)) -> match gl with
       | Global.GInp (id,ty,sd) ->
          if List.mem_assoc id inps
          then Utils.ListExt.update_assoc add_reader id name inps, outps, shrds
          (* else (id, (ty, MInp (build_stim sd, [name]))) :: inps, outps, shrds *)
          else (id, (ty, MInp (sd, [name]))) :: inps, outps, shrds
       | Global.GShared (id,ty) ->
          if List.mem_assoc id shrds
          then inps, outps, Utils.ListExt.update_assoc add_reader id name shrds
          else inps, outps, (id, (ty, MShared ([], [name]))) :: shrds
       | _ -> inps, outps, shrds)
      f.f_inps
  |> Misc.fold_left
      (fun (inps,outps,shrds) (_,(_,gl)) -> match gl with
       | Global.GOutp (id,ty) ->
          if List.mem_assoc id outps
          then inps, Utils.ListExt.update_assoc add_writer id name outps, shrds
          else inps, (id, (ty, MOutp [name])) :: outps, shrds
       | Global.GShared (id,ty) ->
          if List.mem_assoc id shrds
          then inps, outps, Utils.ListExt.update_assoc add_writer id name shrds
          else inps, outps, (id, (ty, MShared ([name],[]))) :: shrds
       | _ -> inps, outps, shrds)
      f.f_outps
  |> Misc.fold_left
      (fun (inps,outps,shrds) (_,(_,gl)) -> match gl with
       | Global.GShared (id,ty) ->
          if List.mem_assoc id shrds
          then inps, outps, Utils.ListExt.update_assoc add_reader_writer id name shrds
          else inps, outps, (id, (ty, MShared ([name],[]))) :: shrds
       | _ -> inps, outps, shrds)
      f.f_inouts 

let build_dependencies fsms shared =
  let g = DepG.create () in
  let nodes = ref [] in
  let lookup n = try List.assoc n !nodes with Not_found -> Misc.fatal_error "Sysm.build_dependencie.lookup" in
  List.iter
    (function f ->
       let name = f.Fsm.f_name in
       let v = DepG.V.create name in
       nodes := (name, v) :: !nodes;
       DepG.add_vertex g v)
    fsms;
  List.iter   (* Compute dependency graph *)
    (function
       id,(ty,MShared (wrs,rrs)) ->
         List.iter
           (function (s,d) ->
              if s <> d then let e = DepG.E.create (lookup s) id (lookup d) in DepG.add_edge_e g e)
           (* Self-dependencies are deliberately not taken into account *)
           (Utils.ListExt.cart_prod2 wrs rrs)
     | _ -> Misc.fatal_error "Sysm.build_dependencies" (* should not happen *))
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
    md_node = function n -> try List.assoc n !nodes with Not_found -> Misc.fatal_error "Sysm.build_dependencies.md_node " }

exception Illegal_const_expr of Expr.t

let add_global_type tenv (name,ty) = 
  let open Typing in
  match ty with
  | Types.TyEnum (_, cs) ->
     { tenv with te_defns = (name, ty) :: tenv.te_defns;
                 te_ctors = List.map (function c -> c, ty) cs @ tenv.te_ctors }
  | Types.TyRecord (_, fs) ->
     { tenv with te_defns = (name, ty) :: tenv.te_defns;
                 te_rfields = List.map (function (n,_) -> n, ty) fs @ tenv.te_rfields }
  | _ ->
     { tenv with te_defns = (name, ty) :: tenv.te_defns }

let add_global_value tenv (name, (ty, d)) =
  let open Typing in
  match d with 
  | MConst _
  | MFun _ -> { tenv with te_vars = (name, ty) :: tenv.te_vars }
  | _ -> tenv

let build ~name ?(gtyps=[]) ?(gfns=[]) ?(gcsts=[]) models fsms =
  let tenv =
       Typing.builtin_tenv
    |> Misc.fold_left add_global_type gtyps
    |> Misc.fold_left add_global_value (gcsts @ gfns) in
  let inputs, outputs, shared = List.fold_left extract_globals ([],[],[]) fsms in
  (* Type check FSMs *)
  List.iter (Typing.type_fsm_inst tenv) fsms;
  (* Type check inputs *)
  List.iter (function
    | name, (ty, MInp (sd, _)) -> Typing.type_check_stim tenv name ty sd
    | _, _ -> ())
    inputs;
  { m_name = name;
    m_models = models;
    m_fsms = fsms;
    m_inputs = inputs;
    m_outputs = outputs;
    m_types = gtyps;
    m_fns = gfns;
    m_consts = gcsts;
    m_shared = shared;
    m_deps = build_dependencies fsms shared;
   }

(* DOT output *)

let string_of_global (name, (ty, desc)) =
  let pfx = match desc with
    | MInp _ -> "input"
    | MOutp _ -> "output"
    | MFun _ -> "function"
    | MConst _ -> "const"
    | MShared _ -> "shared" in
  pfx ^ " " ^ name ^ ":" ^ Types.string_of_type ty

let dot_output dir ?(dot_options=[]) ?(fsm_options=[]) m =
  let open Utils in
  let rankdir = if List.mem Utils.Dot.RankdirLR dot_options then "LR" else "UD" in
  let layout, mindist = if List.mem Lascar.Ltsa.Circular dot_options then "circo", 1.5 else "dot", 1.0 in
  let dump_header oc name =
     Printf.fprintf oc "digraph %s {\nlayout = %s;\nrankdir = %s;\nsize = \"8.5,11\";\nlabel = \"\"\n center = 1;\n nodesep = \"0.350000\"\n ranksep = \"0.400000\"\n fontsize = 14;\nmindist=\"%1.1f\"\n" name layout rankdir mindist in
  let fnames = (* Dump all FSM models *)
    List.map
      (Fsm.dot_output_model ~dot_options:dot_options ~options:fsm_options ~dir:dir)
      m.m_models in
  match m.m_fsms with
  | [] -> (* No instance, that's all folks *)
     fnames
  | _ -> (* System, with FSM instances, globals, etc.. *)
     let fname = Filename.concat dir (m.m_name ^ ".dot") in
     let oc = open_out fname in
     dump_header oc m.m_name;
     List.iter
       (Fsm.dot_output_oc oc ~dot_options:(Utils.Dot.SubGraph::dot_options) ~options:(GlobalNames::fsm_options))
       m.m_fsms;
     let caption = StringExt.concat_sep "\\r" 
            [ListExt.to_string string_of_global "\\r" m.m_inputs;
             ListExt.to_string string_of_global "\\r" m.m_outputs;
             ListExt.to_string string_of_global "\\r" m.m_consts;
             ListExt.to_string string_of_global "\\r" m.m_fns;
             ListExt.to_string string_of_global "\\r" m.m_shared] in
     Printf.fprintf oc "%s_globals [label=\"%s\", shape=rect, style=rounded]\n" m.m_name caption;
     Printf.fprintf oc "}\n";
     close_out oc;
     fname :: fnames
  
(* Printing *)

let dump_global oc (name,(ty,g_desc)) =
  let open Utils in
  match g_desc with
  | MInp (sd,rdrs) ->
     Printf.fprintf oc "INPUT %s : %s = %s [-> %s]\n"
       name
       (Types.string_of_type ty)
       (Global.string_of_stim sd)
       (ListExt.to_string Misc.id "," rdrs)
  | MOutp wrs ->
     Printf.fprintf oc "OUTPUT %s : %s [<- %s]\n"
       name
       (Types.string_of_type ty)
       (ListExt.to_string Misc.id "," wrs)
  | MFun (args,body) ->
     Printf.fprintf oc "FUNCTION %s : %s\n"
       name
       (Types.string_of_type ty)
  | MConst v ->
     Printf.fprintf oc "CONST %s : %s\n"
       name
       (Types.string_of_type ty)
  | MShared (wrs,rrs) ->
     Printf.fprintf oc "SHARED %s : %s [<- %s] [-> %s])\n"
       name
       (Types.string_of_type ty)
       (ListExt.to_string Misc.id "," wrs)
       (ListExt.to_string Misc.id "," rrs)

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
  List.iter (Fsm.dump_inst oc) m.m_fsms;
  List.iter (dump_global oc) m.m_inputs;
  List.iter (dump_global oc) m.m_outputs;
  List.iter (dump_global oc) m.m_fns;
  List.iter (dump_global oc) m.m_shared;
  dump_dependencies m
  
