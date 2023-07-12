(** Dependency graphs *)

module G =
  Graph.Imperative.Digraph.AbstractLabeled
    (struct type t = string end)  (* Only abstract vertices allow imperative marking .. *)
    (struct
      type t = string
      let compare = Stdlib.compare
      let default = ""
    end)

type t = G.t
module V = G.V
module E = G.E
module Mark = G.Mark

let create = G.create
let add_vertex = G.add_vertex
let add_edge = G.add_edge
let add_edge_e = G.add_edge_e
let pred = G.pred
let iter_vertex = G.iter_vertex
let iter_succ = G.iter_succ
