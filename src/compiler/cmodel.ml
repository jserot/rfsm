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

(* Target-independent C model *)

type c_type_defn = 
    CTyEnum of string list

type c_model = {
  c_name: string;
  c_states: string list;
  c_types: (string * c_type_defn) list;
  c_params: (string * Types.typ) list;
  c_consts: (string * (Types.typ * Expr.value)) list;
  c_inps: (string * Types.typ) list;
  c_outps: (string * Types.typ) list;
  c_inouts: (string * Types.typ) list;
  (* c_vars: (string * (Types.typ * Expr.value)) list;   *)
  c_vars: (string * Types.typ) list;  
  c_init: Fsm.state * Action.t list;
  c_body: c_state_case list;
     (* c_body = [case_1;...;case_n]
       means
        "while ( 1 ) { switch ( [state] ) { [case_1]; ...; [case_n] } }" *)
  c_ddepth: int  (* depth in the dependency graph *)
  }

and c_state_case = {
  st_src: Fsm.state;
  st_sensibility_list: string list;
  st_transitions: (Condition.event * c_transition list) list;  (* transitions, sorted by triggering event *)
  }

and c_transition = Fsm.state * Fsm.TransLabel.t  (* destination state, transition label *)

exception Error of string * string   (* where, message *)

let update_assoc k v l =
  (* If key [k] does not belong to assoc list [l], add it, with associated value [[v]].
     Else add [v] to the associated value *)
  let rec h = function
    [] -> [k,[v]]
  | (k',vs)::l -> if k=k' then (k,v::vs) :: l else (k',vs) :: h l in
  h l

let mk_state_case succs q = 
  let module EventSet = Set.Make(String) in
  let ts = succs q in 
  let tss = List.fold_left
    (fun acc ((_,((evs,_),_,_,_)) as t) -> 
      match evs with
        [] -> acc
      | [ev] -> update_assoc ev t acc
      | _ -> Misc.not_implemented "Cmodel: transitions with multiple triggering events")
    []
    ts in
  { st_src = q;
    st_sensibility_list = List.map fst tss;
    st_transitions = tss }

let mk_init m ts =
  match ts with
      [] -> raise (Error (m, "No initial transition"))
    | [(([],[]),acts,_,_),q] -> q, acts
    | [_] -> Misc.fatal_error ("Cmodel.mk_init: illegal initial transition for FSM " ^ m) (* should not happen *)
    | _ -> raise (Error (m, "Multiple initial transitions"))

let c_model_of_fsm_model f = 
  let open Fsm in
  let filter_ios kind ios =
    ios |> List.filter (fun (_, (k, _)) -> k=kind) |> List.map (fun (id, (_,ty)) -> id, ty) in
  let states = Repr.states' f.fm_repr in
  { c_name = f.fm_name;
    c_states = states;
    c_types = [];
    c_params = f.fm_params;
    c_consts = [];
    c_inps = f.fm_ios |> filter_ios Types.IO_In;
    c_outps = f.fm_ios |> filter_ios Types.IO_Out;
    c_inouts = f.fm_ios |> filter_ios Types.IO_Inout;
    c_vars = f.fm_vars;
    c_init = mk_init f.fm_name (Repr.itransitions f.fm_repr);
    c_body = List.map (mk_state_case (Fsm.Repr.succs' f.fm_repr)) (List.rev states);
    c_ddepth = 0;
    }

let c_model_of_fsm_inst m f = 
  let states = Fsm.states_of_inst f in
  let open Static in
  let open Fsm in
  { c_name = f.f_name;
    c_states = states;
    c_types = [];
    c_params = [];
    c_consts = f.f_params;
    c_inps = List.map (function (id, (ty,_)) -> id, ty) f.f_inps;
    c_outps = List.map (function (id, (ty,_)) -> id, ty) f.f_outps;
    c_inouts = List.map (function (id, (ty,_)) -> id, ty) f.f_inouts;
    c_vars = f.f_vars;
    c_init = mk_init f.f_name (Fsm.itransitions_of_inst f);
    c_body = List.map (mk_state_case (Fsm.Repr.succs' f.f_repr)) (List.rev states);
    c_ddepth = Static.DepG.Mark.get (m.m_deps.md_node f.f_name);
    }

let empty = 
  { c_name = "";
    c_states = [];
    c_types = [];
    c_params = [];
    c_consts = [];
    c_inps = [];
    c_outps = [];
    c_inouts = [];
    c_vars = [];
    c_init = "",[];
    c_body = [];
    c_ddepth = 0; 
    }
