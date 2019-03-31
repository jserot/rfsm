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

open Lascar

type act_semantics =  (** Interpretation of actions associated to transitions *)
  | Sequential        (** sequential (ex: [x:=x+1;y:=x] with [x=1] gives [x=2,y=2]) *)
  | Synchronous       (** synchronous (ex: [x:=x+1:y=x] with [x=1] gives [x=2,y=1]) *)
  
type fsm_config = {
    mutable act_sep: string;         (** Default value: [" "] *)
    mutable act_sem: act_semantics;  (** Default value: [Sequential] *)
  }

let cfg = {
    act_sep = " ";
    act_sem = Sequential;
    }

exception Undef_symbol of string * string * string (** FSM, kind, name *)
exception Invalid_state of string * string (** FSM, id *)
exception Typing_error of string * string * Types.typ * Types.typ (** what, where, type, type *)

module TransLabel = struct
  type t = Condition.t * Action.t list * int * bool
   (* Cond will be ([],[]) for initial transitions,
      [int] is the priority level (used to resolve non-deterministic transitions)
      [bool] is true for "implicit" transitions *)
  let compare = Pervasives.compare
  let to_string (cond,acts,_,_) =  (* This function is used for .dot representations *)
    let s1 = Condition.to_string cond in
    let sep = match cfg.act_sem with
      | Sequential -> ";" ^ cfg.act_sep 
      | Synchronous -> "," ^ cfg.act_sep in
    let length_of a = String.length (Action.to_string a) in
    let s2 = Utils.ListExt.to_string Action.to_string sep acts in
    let l2 = match cfg.act_sep, acts with
      | "\\n", a::rest -> List.fold_left (fun acc a -> max acc (length_of a)) (length_of a) rest
      | _, _ -> String.length s2 in
    let l = String.make (Utils.Misc.max (String.length s1) l2) '_' in
    if s2 <> ""
    then Printf.sprintf "%s\\n%s\\n%s" s1 l s2 
    else Printf.sprintf "%s" s1
  let rename f (cond,acts,prio,is_impl) = (Condition.rename f cond, List.map (Action.rename f) acts, prio, is_impl)
  let subst env (cond,acts,prio,is_impl) = (Condition.subst env cond, List.map (Action.subst env) acts, prio, is_impl)
  let is_rtl (_,acts,_,_) = 
      let rec scan acc acts = match acts with
          [] -> true
        | a::rest ->
           let wrs = snd (Action.vars_of a) in          (* The set of variables written by [a] *)
           if Expr.VarSet.is_empty (Expr.VarSet.inter acc wrs) 
           then scan (Expr.VarSet.union acc wrs) rest  (* Ok. None has already be written .. *)
           else false in
      scan Expr.VarSet.empty acts
end

module State = struct
  type t = string
  let compare = Pervasives.compare
  let to_string s = s
end

module Repr = Lts.Make(State)(TransLabel)

type state = State.t
type transition = State.t * TransLabel.t * State.t
type itransition = TransLabel.t * State.t

let string_of_state s = State.to_string s
                      
let string_of_transition (s,(cond,acts,prio,_),s') =
  let lbl = 
    let s1 = Condition.to_string cond in
    let s2 = Utils.ListExt.to_string Action.to_string "; " acts in
    if s2 <> ""
    then s1 ^ "|" ^ s2 
    else s1 in
  State.to_string s ^ "--" ^ lbl ^ "->" ^ State.to_string s' ^ "[" ^ string_of_int prio ^ "]"
  
type model = { 
    fm_name: string;
    fm_params: (string * Types.typ) list;                      (** name, type *)
    fm_ios : (string * (Types.dir * Types.typ)) list;          (** i/os *)
    fm_vars: (string * Types.typ) list;                        (** name, type *)
    fm_repr: Repr.t;                                           (** Static representation as a LTS *)
    (* mutable fm_typ : Types.typ; *)
  }

type inst = { 
    f_name: string;
    f_model: model;
    f_params: (string * (Types.typ * Expr.value)) list;       (** name, type, actual value *)
    f_inps: (string * (Types.typ * Global.global)) list;      (** local name, (type, global) *)
    f_outps: (string * (Types.typ * Global.global)) list;     (** local name, (type, global) *)
    f_inouts: (string * (Types.typ * Global.global)) list;    (** local name, (type, global) *)
    f_vars: (string * Types.typ) list;                        (** name, type *)
    f_repr: Repr.t;                                           (** Static representation as a LTS (with _local_ names) *)
    f_l2g: string -> string;                                  (** local -> global name *)
  }

(* Inspectors *)

let states_of_inst m = Repr.states' m.f_repr
let istate_of_inst m = match Repr.istates' m.f_repr with [] -> None | qs -> Some (List.hd qs)
let transitions_of_inst m = Repr.transitions m.f_repr
let itransitions_of_inst m = Repr.itransitions m.f_repr

let succs_inst m q = Repr.succs' m.f_repr q

let input_events_of_inst, output_events_of_inst =
  let extract l = List.fold_left (fun acc (id,(ty,_)) -> match ty with Types.TyEvent -> id::acc | _ -> acc) [] l in
  (function f -> extract f.f_inps),
  (function f -> extract f.f_outps)

let is_rtl_inst f =
  List.for_all (function (_,t,_) -> TransLabel.is_rtl t) (transitions_of_inst f)
  && List.for_all (function (t,_) -> TransLabel.is_rtl t) (itransitions_of_inst f)

let states_of_model m = Repr.states' m.fm_repr
let istate_of_model m = match Repr.istates' m.fm_repr with [] -> None | qs -> Some (List.hd qs)
let transitions_of_model m = Repr.transitions m.fm_repr
let itransitions_of_model m = Repr.itransitions m.fm_repr

let succs_model m q = Repr.succs' m.fm_repr q

let input_events_of_model, output_events_of_model =
  let extract dir l = List.fold_left (fun acc (id,(d,ty)) -> match ty with Types.TyEvent when d=dir -> id::acc | _ -> acc) [] l in
  (function f -> extract Types.IO_In f.fm_ios),
  (function f -> extract Types.IO_Out f.fm_ios)

let is_rtl_model f =
  List.for_all (function (_,t,_) -> TransLabel.is_rtl t) (transitions_of_model f)
  && List.for_all (function (t,_) -> TransLabel.is_rtl t) (itransitions_of_model f)

exception Binding_mismatch of string * string * string  (** FSM, kind, id *)
exception Invalid_parameter of string * string (** FSM, name *)
exception Uninstanciated_type_vars of string * string * string * string list (* FSM, kind, id, vars *)

(* Builders *)

let mk_bindings ~local_names:ls ~global_names:gs =
  let l2g  =
    try List.combine ls gs
    with Invalid_argument _ -> Misc.fatal_error ("Fsm.Static.mk_bindings") (* should not happen *) in
  (function id -> try List.assoc id l2g with Not_found -> id)

let sanity_check_model consts f =
  (* Check that
       - each input symbol occuring in transition rules is declared as input, local variable, parameter or global const
       - each output symbol occuring in transition rules is declared as output or local variable *)
  let isymbols, osymbols =
    List.fold_left
      (fun (ivs,ovs) (_,(cond,acts,_,_),_) ->
        let ivs' = Expr.VarSet.union ivs (Condition.vars_of cond) in
        List.fold_left
          (fun (ivs,ovs) act ->
            let ivs',ovs' = Action.vars_of act in
            Expr.VarSet.union ivs ivs', Expr.VarSet.union ovs ovs')
          (ivs',ovs)
          acts)
      (Expr.VarSet.empty, Expr.VarSet.empty)
      (transitions_of_inst f) in
  let check_symbols kind ss ss' =
    Expr.VarSet.iter
      (function s -> if not (List.mem_assoc s ss') then raise (Undef_symbol(f.f_name,kind,s)))
      ss in
  let get l = List.map (function (id,(ty,_)) -> id, ty) l in
  check_symbols "input or local variable"
    isymbols
    (get f.f_inps @ get f.f_inouts @ f.f_vars @ get f.f_params @ consts);
  check_symbols "output or local variable"
    osymbols
    (get f.f_outps @ get f.f_inouts @ f.f_vars)

let build_model ~name ~states ~params ~ios ~vars ~trans ~itrans = 
  (* Build a FSM model from a syntax level description. *)
  let mk_trans (s,(ev,gds),acts,s',p) = s, (([ev],gds),acts,p,false), s' in
  let m = {
      fm_name = name;
      fm_params = params;
      fm_ios = List.map (function (dir,id,ty) -> (id, (dir,ty))) ios;
      fm_vars = vars;
      fm_repr =
        begin
          try Repr.create
                ~states:states
                ~trans:(List.map mk_trans trans)
                ~itrans:(let q0,iacts = itrans in [(([],[]),iacts,0,false),q0])
          with
            Repr.Invalid_state s -> raise (Invalid_state (name, s))
        end;
      (* fm_typ = Types.no_type; (\* unknown *\) *)
    } in
  (* sanity_check_model [] m; *)
  m


let type_check what where ty ty'  =
  try
    Types.unify ty ty'
  with
    Types.TypeConflict _ -> raise (Typing_error (what, where, ty, ty'))

let build_instance (*~tenv*) ~name ~model ~params ~ios =
  (* Builds an FSM instance from a model *)
  let bind_param vs (p,ty) =
    let rec compat ty v = match ty, v.Expr.v_desc with
        Types.TyInt _, Expr.Val_int _ -> true
      | Types.TyFloat, Expr.Val_float _ -> true
      | Types.TyBool, Expr.Val_bool _ -> true
      | Types.TyChar, Expr.Val_char _ -> true
      | Types.TyArray (TiConst n,ty'), Expr.Val_array vs ->
         let n' = Array.length vs in
         n > 0 && n = n' && compat ty' vs.(0)
      | _, _ -> false in
    let v =
      begin
        try List.assoc p vs
        with Not_found -> raise (Binding_mismatch (name, "parameters", p))
      end in
    if compat ty v then p, (ty,v) else raise (Invalid_parameter (name, p)) in
  let bound_params = List.map (bind_param params) model.fm_params in
  let ienv = 
    List.fold_left
      (fun acc (id,(ty,v)) -> match v.Expr.v_desc with
                              | Expr.Val_int x -> (id,x) :: acc  (* Only int parameters can be used as indices *)
                              | _ -> acc)
      []
      bound_params in
  let bind_io (lid,(dir,lty)) gl =
    let open Global in
    (* Bind FSM IOs to global objects, checking type compatibility *)
    let ty' = Types.subst_indexes ienv lty in
    match dir, gl with 
    | Types.IO_In, GInp (gid,ty,st) ->
       type_check ("input " ^ gid) ("FSM " ^ name) ty ty';
       (lid,gid,Types.IO_In,ty,gl)
    | Types.IO_In, GShared (gid,ty) ->
       type_check ("input " ^ gid) ("FSM " ^ name) ty ty';
       (lid,gid,Types.IO_In,ty,gl)
    | Types.IO_In, _ ->
       raise (Binding_mismatch (name, "input", lid))
    | Types.IO_Out, GOutp (gid,ty)
      | Types.IO_Out, GShared (gid,ty) ->
       type_check ("output " ^ gid) ("FSM " ^ name) ty ty';
       (lid,gid,Types.IO_Out,ty,gl)
    | Types.IO_Out, _ ->
       raise (Binding_mismatch (name, "output", lid))
    | Types.IO_Inout, GShared (gid,ty) ->
       type_check ("inout " ^ gid) ("FSM " ^ name) ty ty';
       (lid,gid,Types.IO_Inout,ty,gl)
    | Types.IO_Inout, _ ->
       raise (Binding_mismatch (name, "inout", lid)) in
  let bound_ios =
    try List.map2 bind_io model.fm_ios ios
    with Invalid_argument _ -> raise (Binding_mismatch (name, "IOs", "")) in
  let filter_ios kind bound_ios =
    bound_ios
    |> List.filter (function (_,_,k,_,_) -> k=kind)
    |> List.map (function (lid,_,_,ty,gl) -> lid, (ty,gl)) in
  let mk_var (id,ty) = id, Types.subst_indexes ienv ty in
  let r =
    { f_name = name;
      f_model = model;
      f_repr =
        (let subst_env = List.map (function id,(ty,v) -> id, v) bound_params in
         Repr.map_label (TransLabel.subst subst_env) model.fm_repr);
      f_params = bound_params;
      f_inps = filter_ios Types.IO_In bound_ios;
      f_outps = filter_ios Types.IO_Out bound_ios;
      f_inouts = filter_ios Types.IO_Inout bound_ios;
      f_vars = List.map mk_var model.fm_vars;
      f_l2g =
        mk_bindings
          ~local_names:(List.map (function (lid,_,_,_,_) -> lid) bound_ios)
          ~global_names:(List.map (function (_,gid,_,_,_) -> gid) bound_ios);
    } in
  r

(* DOT OUTPUT *)

let string_of_io f (id,(ty,gl)) = id ^ ": " ^ Types.string_of_type ty ^ " (->" ^ Global.global_id gl ^ ")"
let string_of_param (id,(ty,v)) = id  ^ "=" ^ Expr.string_of_value v 
let string_of_var (id,ty) = id  ^ ":" ^ Types.string_of_type ty

type dot_options =
  OmitImplicitTransitions
| GlobalNames
| NoCaption

let dot_output_oc oc ?(dot_options=[]) ?(options=[]) f =
  let string_of_var (id,ty) = "var " ^ id  ^ ":" ^ Types.string_of_type ty in
  (* let caption = StringExt.concat_sep "\\r" 
   *    [Utils.ListExt.to_string (string_of_io "inp") "\\r" f.f_inps;
   *     Utils.ListExt.to_string (string_of_io "out") "\\r" f.f_outps;
   *     Utils.ListExt.to_string (string_of_io "inout") "\\r" f.f_inouts;
   *     Utils.ListExt.to_string string_of_var "\\r" f.f_vars] in *)
  let caption = Utils.ListExt.to_string string_of_var "\\r" f.f_vars in
  let caption_style = {Utils.Dot.node_shape="rect"; Utils.Dot.node_style="rounded"} in
  let impl_ts f opts =
    if List.mem OmitImplicitTransitions opts
    then List.filter (function (_,(_,_,_,is_impl),_) -> is_impl) (Repr.transitions f.f_repr)
    else [] in
  Repr.dot_output_oc
    f.f_name
    oc
    ~options:dot_options
    ~extra_nodes:(if caption <> "" && not (List.mem NoCaption options) then [caption,caption_style] else [])
    ~implicit_transitions:(impl_ts f options)
    (if List.mem GlobalNames options then Repr.map_label (TransLabel.rename f.f_l2g) f.f_repr else f.f_repr)

let dot_output ?(fname="") ?(dot_options=[]) ?(options=[]) ~dir f =
  let fname = if fname = "" then Filename.concat dir (f.f_name ^ ".dot") else fname in
  let oc = open_out fname in
  dot_output_oc oc ~dot_options:dot_options ~options:options f;
  close_out oc;
  fname

let dot_output_model ?(fname="") ?(dot_options=[]) ?(options=[]) ~dir f =
  let string_of_var (id,ty) = "var " ^ id  ^ ":" ^ Types.string_of_type ty in
  let fname = if fname = "" then Filename.concat dir (f.fm_name ^ ".dot") else fname in
  let oc = open_out fname in
  let caption = Utils.ListExt.to_string string_of_var "\\r" f.fm_vars in
  let caption_style = {Utils.Dot.node_shape="rect"; Utils.Dot.node_style="rounded"} in
  let impl_ts f opts =
    if List.mem OmitImplicitTransitions opts
    then List.filter (function (_,(_,_,_,is_impl),_) -> is_impl) (Repr.transitions f.fm_repr)
    else [] in
  Repr.dot_output_oc
    f.fm_name
    oc
    ~options:dot_options
    ~extra_nodes:(if caption <> "" && not (List.mem NoCaption options) then [caption,caption_style] else [])
    ~implicit_transitions:(impl_ts f options)
    f.fm_repr;
  close_out oc;
  fname

(* TXT OUTPUT *)

let inputs_of m = List.filter (function (_,(Types.IO_In,_)) -> true | _ -> false) m.fm_ios
let outputs_of m = List.filter (function (_,(Types.IO_Out,_)) -> true | _ -> false) m.fm_ios
let inouts_of m = List.filter (function (_,(Types.IO_Inout,_)) -> true | _ -> false) m.fm_ios

let dump_model oc f =
  let of_list f xs = Utils.ListExt.to_string f ", " xs in
  let string_of_io (id,(_,ty)) = id  ^ ":" ^ Types.string_of_type ty in
  let string_of_var (id,ty) = id  ^ ":" ^ Types.string_of_type ty in
  let string_of_acts = Utils.ListExt.to_string Action.to_string "; " in
  Printf.fprintf oc "FSM MODEL %s{\n" f.fm_name;
  if f.fm_params <> []  then
    Printf.fprintf oc "  PARAMS = { %s }\n" (of_list string_of_var f.fm_params);
  Printf.fprintf oc "  STATES = { %s }\n" (of_list (function n -> n) (Repr.states' f.fm_repr));
  Printf.fprintf oc "  INPS = { %s }\n" (of_list string_of_io (inputs_of f));
  Printf.fprintf oc "  OUTPS = { %s }\n" (of_list string_of_io (outputs_of f));
  Printf.fprintf oc "  INOUTS = { %s }\n" (of_list string_of_io (inouts_of f));
  Printf.fprintf oc "  VARS = { %s }\n" (of_list string_of_var f.fm_vars);
  Printf.fprintf oc "  TRANS = {\n";
  List.iter 
    (fun (q,(cond,acts,p,_),q') ->
      Printf.fprintf oc "    { %s {%s} {%s} %s [%d]}\n" q (Condition.to_string cond) (string_of_acts acts) q' p)
    (Repr.transitions f.fm_repr);
  Printf.fprintf oc "    }\n";
  let (_,iacts,_,_),iq = List.hd (Repr.itransitions f.fm_repr) in
  Printf.fprintf oc "  ITRANS = { %s \"%s\" }\n" iq (string_of_acts iacts);
  Printf.fprintf oc "  }\n"

let dump_inst oc f =
  let of_list f xs = Utils.ListExt.to_string f ", " xs in
  let string_of_acts = Utils.ListExt.to_string Action.to_string "; " in
  Printf.fprintf oc "FSM %s{\n" f.f_name;
  Printf.fprintf oc "  MODEL = %s\n" f.f_model.fm_name;
  if f.f_params <> []  then
    Printf.fprintf oc "  PARAMS = { %s }\n" (of_list string_of_param f.f_params);
  Printf.fprintf oc "  STATES = { %s }\n" (of_list (function n -> n) (states_of_inst f));
  Printf.fprintf oc "  INPS = { %s }\n" (of_list (string_of_io f) f.f_inps);
  Printf.fprintf oc "  OUTPS = { %s }\n" (of_list (string_of_io f) f.f_outps);
  (* Printf.fprintf oc "  INOUTS = { %s }\n" (of_list (string_of_io f) f.f_inouts); *)
  Printf.fprintf oc "  VARS = { %s }\n" (of_list string_of_var f.f_vars);
  (* Printf.fprintf oc "  TENV =\n"; Typing.dump_tenv oc f.f_tenv; Printf.fprintf oc "\n"; *)
  Printf.fprintf oc "  TRANS = {\n";
  List.iter 
    (fun (q,(cond,acts,p,_),q') ->
      Printf.fprintf oc "    { %s {%s} {%s} %s [%d]}\n" q (Condition.to_string cond) (string_of_acts acts) q' p)
    (transitions_of_inst f);
  Printf.fprintf oc "    }\n";
  let (_,iacts,_,_),iq = List.hd (itransitions_of_inst f) in
  Printf.fprintf oc "  ITRANS = { %s \"%s\" }\n" iq (string_of_acts iacts);
  Printf.fprintf oc "  }\n"
