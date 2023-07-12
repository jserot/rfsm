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

(* Target-independent C-like model *)

module type CMODEL = sig

  module Static : Static.T
       
  type typ = Static.Syntax.typ
  type type_expr = Static.Syntax.type_expr
  type expr = Static.Syntax.Guest.expr
     
  type c_type_defn = 
    CTyEnum of string list

  type t = {
      c_mname: string; (* Model name *)
      c_name: string;  (* Instance name *)
      c_states: c_state list;
      (* c_types: (string * c_type_defn) list; *) (* TODO: GET RID ? *)
      c_params: (string * type_expr) list;
      c_consts: (string * (type_expr * Static.Value.t)) list;
      c_inps: (string * type_expr) list;
      c_outps: (string * type_expr) list;
      c_inouts: (string * type_expr) list;
      c_vars: (string * type_expr) list;  
      c_init: Static.Syntax.itransition_desc;
      c_body: c_state_case list;
      (* c_body = [case_1;...;case_n]
       means
        "while ( 1 ) { switch ( [state] ) { [case_1]; ...; [case_n] } }" *)
      c_ddepth: int  (* depth in the dependency graph *)
    }

  and c_state = string * (string * expr) list (* name, output valuations *)

  and c_state_case = {
      st_src: string;
      st_sensibility_list: string list;
      st_transitions: (string * Static.Syntax.transition_desc list) list;  (* transitions, sorted by triggering event *)
    }

  val pp: Format.formatter -> t -> unit
    
  val of_fsm_model: Static.Syntax.model -> t
  val of_fsm_inst: Static.fsm -> t

  exception Error of string * string   (* where, message *)

end

module Make (Static: Static.T)
       : CMODEL with module Static = Static and type typ = Static.Syntax.typ (*and type value = Static.Value.t*) =
struct

  module Static = Static
  module Syntax = Static.Syntax
  module Types = Syntax.Guest.Types
                
  type typ = Syntax.typ
  type expr = Syntax.Guest.expr
  type type_expr = Syntax.type_expr
     
  let pp_typ = Types.pp_typ ~abbrev:true
  let pp_expr = Static.Syntax.pp_expr
  let pp_type_expr = Static.Syntax.pp_type_expr
  let pp_value = Static.Value.pp
             
  type c_type_defn = 
    CTyEnum of string list
    [@@deriving show {with_path=false}]

  type t = {
      c_mname: string; 
      c_name: string; 
      c_states: c_state list;
      (* c_types: (string * c_type_defn) list; *)
      c_params: (string * type_expr) list;
      c_consts: (string * (type_expr * Static.Value.t)) list;
      c_inps: (string * type_expr) list;
      c_outps: (string * type_expr) list;
      c_inouts: (string * type_expr) list;
      c_vars: (string * type_expr) list;  
      c_init: Static.Syntax.itransition_desc;
      c_body: c_state_case list;
      c_ddepth: int  (* depth in the dependency graph *)
    } [@@deriving show {with_path=false}]

  and c_state = string * (string * expr) list  [@@deriving show {with_path=false}]

  and c_state_case = {
      st_src: string;
      st_sensibility_list: string list;
      st_transitions: (string * Static.Syntax.transition_desc list) list;  (* transitions, grouped by triggering event *)
    } [@@deriving show {with_path=false}]

  exception Error of string * string   (* where, message *)

  let mk_state_case m { Annot.desc = q,_; _ } = 
    let open Static.Syntax in
    (* Find all transitions starting from [q] and sort them according to the trigerring event *)
    let ts =
      List.fold_left
        (fun acc ({ Annot.desc = (q',{Annot.desc=e,_; _},_,_,_) as t; _ }) ->
          if q = q' then Misc.update_list_assoc e t acc else acc)
        []
        m.trans in
    { st_src = q;
      st_sensibility_list = List.map fst ts;
      st_transitions = ts }
  
  let of_fsm_model { Annot.desc = m; _ } =
    let open Static.Syntax in 
    { c_mname = m.name;
      c_name = m.name;
      c_states = List.map (fun { Annot.desc=d; _ } -> d) m.states;
      (* c_types = []; *)
      c_params = m.params;
      c_consts = [];
      c_inps = m.inps;
      c_outps = m.outps;
      c_inouts = m.inouts;
      c_vars = m.vars;
      c_init = m.itrans.Annot.desc;
      c_body = List.map (mk_state_case m) m.states;
      c_ddepth = 0;
    }

  let of_fsm_inst f = 
    let open Static in 
    let m = f.model.Annot.desc in 
    { c_mname = m.name;
      c_name = f.name;
      c_states = List.map (fun { Annot.desc=d; _ } -> d) m.states;
      (* c_types = []; *)
      c_params = [];
      c_consts = List.map2 (fun (id,ty) (_,v) -> (id,(ty,v))) m.params (Env.bindings f.params);
      c_inps = m.inps;
      c_outps = m.outps;
      c_inouts = m.inouts;
      c_vars = m.vars; 
      c_init = m.itrans.Annot.desc;
      c_body = List.map (mk_state_case m) m.states;
      c_ddepth = 0 (* TO FIX : Static.DepG.Mark.get (m.m_deps.md_node f.f_name); *)
    }

end
