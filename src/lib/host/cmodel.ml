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

(** Target-independent C-oriented model *)

module type CMODEL = sig

  module Static : Static.T
       
  type typ = Static.Syntax.typ
  type type_expr = Static.Syntax.type_expr
  type expr = Static.Syntax.Guest.expr
     
  type c_type_defn = 
    CTyEnum of string list

  type t = {
      c_mname: Ident.t; (* Model name *)
      c_name: Ident.t;  (* Instance name *)
      c_states: c_state list;
      c_params: (Ident.t * type_expr) list;
      c_inps: (Ident.t * type_expr) list;
      c_outps: (Ident.t * type_expr) list;
      c_inouts: (Ident.t * type_expr) list;
      c_vars: (Ident.t * type_expr) list;  
      c_init: Static.Syntax.itransition_desc;
      c_body: c_state_case list;
      (* c_body = [case_1;...;case_n] means "while ( 1 ) { switch ( [state] ) { [case_1]; ...; [case_n] } }" *)
      c_ddepth: int  (* depth in the static dependency graph induced by shared variables *)
    }

  and c_state = Ident.t * (Ident.t * expr) list (* name, output valuations *)

  and c_state_case = {
      st_src: Ident.t;
      st_sensibility_list: Ident.t list;
      st_transitions: (Ident.t * Static.Syntax.transition_desc list) list;  (* transitions, sorted by triggering event *)
    }

  val pp: Format.formatter -> t -> unit
    
  val of_fsm_model: Static.Syntax.model -> t
  val of_fsm_inst: Static.t -> Static.fsm -> t

  exception Error of string * string   (* where, message *)

end

module Make (Static: Static.T)
       : CMODEL with module Static = Static and type typ = Static.Syntax.typ =
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
      c_mname: Ident.t; 
      c_name: Ident.t; 
      c_states: c_state list;
      c_params: (Ident.t * type_expr) list;
      c_inps: (Ident.t * type_expr) list;
      c_outps: (Ident.t * type_expr) list;
      c_inouts: (Ident.t * type_expr) list;
      c_vars: (Ident.t * type_expr) list;  
      c_init: Static.Syntax.itransition_desc;
      c_body: c_state_case list;
      c_ddepth: int 
    } [@@deriving show {with_path=false}]

  and c_state = Ident.t * (Ident.t * expr) list  [@@deriving show {with_path=false}]

  and c_state_case = {
      st_src: Ident.t;
      st_sensibility_list: Ident.t list;
      st_transitions: (Ident.t * Static.Syntax.transition_desc list) list;  (* transitions, grouped by triggering event *)
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
  
  let of_model ?(dep_order=0) ~mname ~name m  =
    let open Static.Syntax in 
    { c_mname = name;
      c_name = name;
      c_states = List.map (fun { Annot.desc=d; _ } -> d) m.states;
      c_params = m.params;
      c_inps = m.inps;
      c_outps = m.outps;
      c_inouts = m.inouts;
      c_vars = m.vars;
      c_init = m.itrans.Annot.desc;
      c_body = List.map (mk_state_case m) m.states;
      c_ddepth = dep_order
    }

  let of_fsm_model { Annot.desc = m; _ } =
    let open Syntax in
    of_model ~mname:m.name ~name:m.name m 

  let of_fsm_inst s f = 
    let open Static in
    let m = f.model.Annot.desc in 
    of_model ~dep_order:(List.assoc f.name s.dep_order ) ~mname:m.name ~name:f.name m

end
