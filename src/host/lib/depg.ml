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

(**{1 Dependency graphs} *)

module G = Graph.Imperative.Digraph.Abstract(Ident)
  (** Graphs for which vertices are simple names *)
module M = Map.Make(Ident)
  (** For mapping names to vertices and to full descriptors *)
module TS = Graph.Topological.Make(G)
   (** For topological sorting *)

(** The signature for graph nodes *)

module type NODE = sig  
  type t
  type context
  val name_of: t -> Ident.t
    (** [name_of n] should return a unique name for node [n] *)
  val depends_on: context -> t -> t -> bool
    (** [depends_on c n n'] returns [true] if node [n] depends on node [n'] in context [c], [false] otherwise. *)
end

(** The signature of the module performing dependency sorting of a graph of nodes *)

module type T = sig
  type node
  type context
  val dep_sort: context -> node list -> node list
end

(** The functor building such a module *)

module Make (N: NODE) : T with type node = N.t and type context = N.context =
struct
    type node = N.t
    type context = N.context

    let init nodes = 
      let g = G.create () in
      let vs, ns = 
        (* [vs] is the table of graph vertices, indexed by [node] names,
           [fs] is the table of [nodes], also indexed by [node] names *)
        List.fold_left
          (fun (acc,acc') n ->
            let nm = N.name_of n in
            let v = G.V.create nm in
            G.add_vertex g v;
            M.add nm v acc,
            M.add nm n acc')
          (M.empty, M.empty)
          nodes in
      g, vs, ns

   let add_edges ctx g vs nodes = 
    List.iter 
      (fun (n,n') ->
        let nm = N.name_of n 
        and nm' = N.name_of n' in
        if nm <> nm' && N.depends_on ctx n n' then
          (* Add an edge nm->nm' in the graph iff n' depends on n. Omit self-dependencies *)
          G.add_edge g (M.find nm vs) (M.find nm' vs))
      (Ext.List.cart_prod nodes nodes)

   let topo_sort g fs = 
     TS.fold (fun v acc -> M.find (G.V.label v) fs :: acc) g []

   let dep_sort ctx nodes =
     let g, vs, fs = init nodes in
     let _ = add_edges ctx g vs nodes in
     let nodes' = topo_sort g fs in
     List.rev nodes'
end
