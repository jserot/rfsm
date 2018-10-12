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
open Lascar

type act_semantics = Sequential | Synchronous 

type fsm_config = {
  mutable act_sem: act_semantics;
  mutable act_sep: string;
  }

let cfg = {
  act_sem = Sequential;
  act_sep = " ";
  }
   
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
    let s2 = ListExt.to_string Action.to_string sep acts in
    let l2 = match cfg.act_sep, acts with
      | "\\n", a::rest -> List.fold_left (fun acc a -> max acc (length_of a)) (length_of a) rest
      | _, _ -> String.length s2 in
    let l = String.make (Misc.max (String.length s1) l2) '_' in
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
    let s2 = ListExt.to_string Action.to_string "; " acts in
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
  }

type inst = { 
  f_name: string;
  f_model: model;
  f_params: (string * (Types.typ * Expr.e_val)) list;       (** name, type, actual value *)
  f_inps: (string * (Types.typ * global)) list;             (** local name, (type, global) *)
  f_outps: (string * (Types.typ * global)) list;            (** local name, (type, global) *)
  f_inouts: (string * (Types.typ * global)) list;           (** local name, (type, global) *)
  f_vars: (string * (Types.typ * Expr.e_val)) list;         (** name, (type, value) *)
  f_repr: Repr.t;                                           (** Static representation as a LTS (with _local_ names) *)
  f_l2g: string -> string;                                  (** local -> global name *)
  f_state: string;                                          (** current state *)
  f_has_reacted: bool;                                      (** true when implied in the last reaction *)
  }

and global =
  GInp of string * Types.typ * stim_desc                    (** name, type, stimuli desc *)
| GOutp of string * Types.typ                               (** name, type *)
| GShared of string * Types.typ                             (** name, type *)                 

and stim_desc = 
  Periodic of int * int * int             (** Period, start time, end time *)
| Sporadic of int list                    (** Dates *)
| ValueChange of (int * Expr.e_val) list  (** (Date,value)s *)

(* Inspectors *)

let states_of m = Repr.states' m.f_repr
let istate_of m = match Repr.istates' m.f_repr with [] -> None | qs -> Some (List.hd qs)
let transitions_of m = Repr.transitions m.f_repr
let itransitions_of m = Repr.itransitions m.f_repr

let succs m q = Repr.succs' m.f_repr q

let erase_type (id,(ty,v)) = id, v
  
let input_events_of, output_events_of =
  let extract l = List.fold_left (fun acc (id,(ty,_)) -> match ty with Types.TyEvent -> id::acc | _ -> acc) [] l in
  (function f -> extract f.f_inps),
  (function f -> extract f.f_outps)

let global_id = function
    GInp (id, _, _) -> id
  | GOutp (id, _) -> id
  | GShared (id, _) -> id
    
exception Undef_symbol of string * string * string (** FSM, kind, name *)
exception Internal_error of string (** where *)
exception Invalid_state of string * string (** FSM, id *)
exception Binding_mismatch of string * string * string  (** FSM, kind, id *)
exception Invalid_parameter of string * string (** FSM, name *)

exception Uninstanciated_type_vars of string * string * string * string list (* FSM, kind, id, vars *)

let is_rtl f =
   List.for_all (function (_,t,_) -> TransLabel.is_rtl t) (transitions_of f)
&& List.for_all (function (t,_) -> TransLabel.is_rtl t) (itransitions_of f)

(* Builders *)

let mk_bindings ~local_names:ls ~global_names:gs =
  let l2g  =
    try List.combine ls gs
    with Invalid_argument _ -> raise (Internal_error "Fsm.mk_bindings") (* should not happen *) in
  (function id -> try List.assoc id l2g with Not_found -> id)

let build_model ~name ~states ~params ~ios ~vars ~trans ~itrans = 
  (* Build a FSM model from a syntax level description.
     No type checking here. It will be performed after instanciation *)
  let mk_trans (s,(ev,gds),acts,s',p) = s, (([ev],gds),acts,p,false), s' in
  let r = {
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
      (* fm_resolve = None; (\* TO FIX *\) *)
    } in
  r

let type_check ~strict what where ty ty'  =
  if not (Types.type_equal ~strict ty ty') 
  then raise (Typing.Type_error (what, where, ty, ty'))

let type_check_stim fsm id ty st = match st with
  | ValueChange vcs ->
     List.iter
       (type_check ~strict:false "stimuli" ("input \"" ^ id ^ "\"") ty) 
       (List.map (function (_,v) -> Expr.type_of_value v) vcs)
  | _ ->
     ()

let type_check_instance tenv f =
  (* Type checks an FSM instance.
     Note: this function must called _after_ substitution of local parameters *)
  let local_types = 
            List.map (function (id, (ty,_)) -> id, ty) f.f_params
          @ List.map (function (id, (ty,_)) -> id, ty) (f.f_inps @ f.f_outps @ f.f_inouts)
          @ List.map (function (id, (ty,_)) -> id, ty) f.f_vars in
  let local_ctors = 
    let add acc cs =
      List.fold_left
        (fun acc (c,ty) -> if List.mem_assoc c acc then acc else (c,ty)::acc)
        acc
        cs in
    List.fold_left
      (fun acc (_,ty) -> add acc (Types.enums_of ty))
      []
      local_types in
  let tenv = 
    { tenv with
        Typing.te_vars = tenv.Typing.te_vars @ local_types;
        Typing.te_ctors = tenv.Typing.te_ctors @ local_ctors } in
  let type_check_guard gexp =
    try type_check
          ~strict:true ("guard \"" ^ (Condition.string_of_guard gexp) ^ "\"") ("FSM \"" ^ f.f_name ^ "\"")
          (Typing.type_expression tenv gexp) Types.TyBool
    with
    | Typing.Typing_error (expr, ty, ty') ->
       raise (Typing.Type_error ("guard expression \"" ^ Expr.to_string expr ^ "\"", "FSM \"" ^ f.f_name ^ "\"", ty, ty'))
    | Typing.Unbound_id (kind, id) -> raise (Undef_symbol (f.f_name, kind, id)) in 
  let type_check_index_expression exp =
    try type_check
          ~strict:false ("index expression \"" ^ (Expr.to_string exp) ^ "\"") ("FSM \"" ^ f.f_name ^ "\"")
          (Typing.type_expression tenv exp) (Types.TyInt Types.Int_none)
    with
    | Typing.Typing_error (expr, ty, ty') ->
       raise (Typing.Type_error ("index expression \"" ^ Expr.to_string expr ^ "\"", "FSM \"" ^ f.f_name ^ "\"", ty, ty'))
    | Typing.Unbound_id (kind, id) -> raise (Undef_symbol (f.f_name, kind, id)) in 
  let type_check_condition (_,gs) = List.iter type_check_guard gs in
  let type_check_action act = match act with 
    | Action.Assign (lhs, exp) -> 
       let t =
         begin match lhs.l_desc with
         | Var0 v -> List.assoc v tenv.te_vars
         | Var1 (a,i) ->
            type_check_index_expression i;
            begin match List.assoc a tenv.te_vars with
            | TyInt _ -> (* Special case *)
               lhs.l_desc <- Var2 (a,i,i);  (* This is a hack *)
               TyInt (Int_size (TiConst 1))
            | ty ->  (* Should be an array *)
               Types.subtype_of ty
            end
         | Var2 (a,i1,i2) ->
            type_check_index_expression i1;
            type_check_index_expression i2;
            begin match List.assoc a tenv.te_vars with
            | TyInt _ -> (* Special case *)
               TyInt Int_none
            | ty -> 
               raise (Typing.Type_error ("action \"" ^ Action.to_string act ^ "\"", "FSM \"" ^ f.f_name ^ "\"", ty, TyInt Int_none))
            end
         | exception _ -> raise (Internal_error "Fsm.type_check_action")
         end in
       let t' =
         try Typing.type_expression tenv exp
         with Typing.Typing_error (expr, ty, ty') ->
           raise (Typing.Type_error ("expression \"" ^ Expr.to_string expr ^ "\"", "FSM \"" ^ f.f_name ^ "\"", ty, ty')) in
       (* Printf.printf "Fsm.type_check_action: %s:%s <- %s:%s\n"
        *   (Action.string_of_lhs lhs) (Types.string_of_type t) (Expr.string_of_expr exp.e_desc) (Types.string_of_type t');
        * flush stdout; *)
       begin
         try type_check
               ~strict:false
               ("action \"" ^ Action.to_string act ^ "\"")
               ("FSM \"" ^ f.f_name ^ "\"")
               (* [strict=false] here to accept actions like [v:=1] where [v:int<lo..hi>] or [v:bool] *)
               t' t
         with
         | Typing.Typing_error (expr, ty, ty') ->
            raise (Typing.Type_error ("action \"" ^ Expr.to_string expr ^ "\"", "FSM \"" ^ f.f_name ^ "\"", ty, ty'))
         | Typing.Unbound_id (kind, id) -> raise (Undef_symbol (f.f_name, kind, id)) 
       end
    | Action.Emit s ->
       let t = try List.assoc s tenv.te_vars with Not_found -> raise (Internal_error "Fsm.type_check_action") in
       type_check ~strict:true ("action \"" ^ Action.to_string act ^ "\"") ("FSM \"" ^ f.f_name ^ "\"") t TyEvent
    | _ -> () in
  let type_check_transition (_,(cond,acts,_,_),_) =
    type_check_condition cond;
    List.iter type_check_action acts in
  let type_check_itransition ((_,acts,_,_),_) =
    List.iter type_check_action acts in
  (* Check that all type indexes have been instanciated for IOs and local vars *)
  let check_type kind (id,ty) = 
    match Types.ivars_of ty with
    | [] -> ()
    | vs -> raise (Uninstanciated_type_vars (f.f_name, kind, id, vs)) in
  List.iter (check_type "input") (List.map (function (id, (ty,_)) -> id,ty) f.f_inps);
  List.iter (check_type "output") (List.map (function (id, (ty,_)) -> id,ty) f.f_outps);
  List.iter (check_type "inout") (List.map (function (id, (ty,_)) -> id,ty) f.f_inouts);
  List.iter (check_type "variable") (List.map (function (id, (ty,_)) -> id,ty) f.f_vars);
  (* Type check conditions and actions on transitions *)
  List.iter type_check_transition (Repr.transitions f.f_repr);
  List.iter type_check_itransition (Repr.itransitions f.f_repr)

let sanity_check f =
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
      (transitions_of f) in
  let check_symbols kind ss ss' =
    Expr.VarSet.iter
      (function s -> if not (List.mem_assoc s ss') then raise (Undef_symbol(f.f_name,kind,s)))
      ss in
  let get l = List.map (function (id,(ty,_)) -> id, ty) l in
  (* Check that each input symbol occuring in transition rules is declared as input or local variable *)
  check_symbols "input or local variable"
    isymbols
    (get f.f_inps @ get f.f_inouts @ get f.f_vars @ get f.f_params);
  (* Check that each output symbol occuring in transition rules is declared as output or local variable *)
  check_symbols "output or local variable"
    osymbols
    (get f.f_outps @ get f.f_inouts @ get f.f_vars)

let build_instance ~tenv ~name ~model ~params ~ios =
    (* Builds an FSM instance from a model *)
    let bind_param vs (p,ty) =
      let rec compat ty v = match ty, v with
        Types.TyInt _, Expr.Val_int _ -> true
      | Types.TyFloat, Expr.Val_float _ -> true
      | Types.TyBool, Expr.Val_bool _ -> true
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
        (fun acc (id,(ty,v)) -> match v with
         | Expr.Val_int x -> (id,x) :: acc  (* Only int parameters can be used as indices *)
         | _ -> acc)
      []
      bound_params in
    let bind_io (lid,(dir,lty)) gl =
      (* Bind FSM IOs to global objects, checking type compatibility *)
      let ty' = Types.subst_indexes ienv lty in
      match dir, gl with 
      | Types.IO_In, GInp (gid,ty,st) ->
         type_check_stim name gid ty st;
         type_check ~strict:true ("input " ^ gid) ("FSM " ^ name) ty ty';
         (lid,gid,Types.IO_In,ty,gl)
      | Types.IO_In, GShared (gid,ty) ->
         type_check ~strict:true ("input " ^ gid) ("FSM " ^ name) ty ty';
         (lid,gid,Types.IO_In,ty,gl)
      | Types.IO_In, _ ->
         raise (Binding_mismatch (name, "input", lid))
      | Types.IO_Out, GOutp (gid,ty)
        | Types.IO_Out, GShared (gid,ty) ->
         type_check ~strict:true ("output " ^ gid) ("FSM " ^ name) ty ty';
         (lid,gid,Types.IO_Out,ty,gl)
      | Types.IO_Out, _ ->
         raise (Binding_mismatch (name, "output", lid))
      | Types.IO_Inout, GShared (gid,ty) ->
         type_check ~strict:true ("inout " ^ gid) ("FSM " ^ name) ty ty';
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
    let mk_ival ty = match ty with
      | Types.TyArray(TiConst sz, _) -> Expr.Val_array (Array.make sz Expr.Val_unknown)
      | Types.TyArray(_, _) -> failwith "Fsm.build_instance.mk_ival"
      | _ -> Expr.Val_unknown in
    let mk_var (id,ty) =
      let ty' = Types.subst_indexes ienv ty in
      id, (ty',mk_ival ty') in
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
        f_state = "";  (* current state is not defined until the initial transition has been carried out *)
        f_has_reacted = false;
      } in
    sanity_check r;
    type_check_instance tenv r;
    r

(* Dynamic behavior (reactive semantics) *)

exception IllegalTrans of inst * string
exception Undeterminate of inst * string * Types.date
exception NonDetTrans of inst * transition list * Types.date

type lenv = (string * Expr.e_val) list
type genv = (Ident.t * Expr.e_val) list

(* type response = Ident.t * Expr.e_val *)
type response = lhs * Expr.e_val

and lhs =
  | Var0 of Ident.t         (* Scalar *)
  | Var1 of Ident.t * int   (* 1D array location *)

let rec replace_assoc' k v env =
  (* This is a variation on [ListExt.replace_assoc], where [v=(_,v')] and only [v'] is replaced *)
  let rec repl = function
    [] -> []
  | (k',(x,v'))::rest -> if k=k' then (k,(x,v)) :: repl rest else (k',(x,v')) :: repl rest in
  repl env

(* FSM environment *)
  
type fsm_env = (string * Expr.e_val) list

let do_action (f,resps,resps',env) act =
  (* Make FSM [f] perform action [act] in (local) environment [env], returning an updated FSM [f'],
     a list of responses [resps], and an updated (local) environment [env']. *)
  let array_upd id idx v = 
    match List.assoc id env, Eval.eval env idx, v with
    | Expr.Val_array vs, Expr.Val_int i, _ -> Expr.Val_array (Expr.array_update id vs i v), i
    | _, _, _ -> failwith "Fsm.do_action.array_upd" in
  let set_bits id idx1 idx2 v = 
    match List.assoc id env, Eval.eval env idx1, Eval.eval env idx2, v with
    | Expr.Val_int x, Expr.Val_int hi, Expr.Val_int lo, Expr.Val_int b -> Expr.Val_int (Intbits.set_bits hi lo x b)
    | _, _, _, _ -> failwith "Fsm.do_action.set_bits" in
  match act with
    Action.Assign (lhs, expr) ->
      let id = Action.lhs_name lhs in
      let v = Eval.eval env expr in
      if List.mem_assoc id f.f_vars then (* Local variable *)
        begin match cfg.act_sem with
        | Sequential ->
          (* In the sequential interpretation, updates of local variables are performed immediately
             and reflected in the environment (so that actions sequences such as "c:=c+1;s=c" are interpreted correctly. *)
           begin match lhs.l_desc with
           | Action.Var0 id ->
              let v' = Eval.eval env expr in
              { f with f_vars = replace_assoc' id v' f.f_vars },
              resps @ [Var0 (Ident.Local (f.f_name, id)), v'],
              resps',
              ListExt.replace_assoc id v' env
           | Action.Var1 (id, idx) ->
              let v', i = array_upd id idx v in
              { f with f_vars = replace_assoc' id v' f.f_vars },
              resps @ [Var1 (Ident.Local (f.f_name, id), i), v],
              resps',
              ListExt.replace_assoc id v' env
           | Action.Var2 (id,idx1,idx2) ->
              let v' = set_bits id idx1 idx2 v in
              { f with f_vars = replace_assoc' id v' f.f_vars },
              resps @ [Var0 (Ident.Local (f.f_name, id)), v'],
              resps',
              ListExt.replace_assoc id v' env
           end
        | Synchronous ->
          (* In the synchronous interpretation, updates of local variables are not performed immediately
             nor reflected in the environment, but only reported in [resps] so that they can be performed at the end of the
             reaction. *)
           begin match lhs.l_desc with
           | Action.Var0 id ->
              let v' = Eval.eval env expr in
              f,
              resps,
              resps' @ [Var0 (Ident.Local (f.f_name, id)), v'],
              env
           | Action.Var1 (id, idx) ->
              let v', i = array_upd id idx v in
              f,
              resps,
              resps' @ [Var1 (Ident.Local (f.f_name, id), i), v],
              env
           | Action.Var2 (id, idx1, idx2) ->
              let v' = set_bits id idx1 idx2 v in
              f,
              resps,
              resps' @ [Var0 (Ident.Local (f.f_name, id)), v'],
              env
           end
        end
      else  (* Global IO or shared value. Updates are never performed immediately *)
        begin match lhs.l_desc with
        | Action.Var0 id ->
           let v' = Eval.eval env expr in
           f,
           resps @ [Var0 (Ident.Global (f.f_l2g id)), v'],
           resps',
           env
        | Action.Var1 (id, _)
        | Action.Var2 (id, _, _) ->
           failwith "Fsm.do_action: not implemented: global IO or shared value with array/bitset type"
        end
  | Action.Emit id ->
     f,
     resps @ [Var0 (Ident.Global (f.f_l2g id)), Expr.set_event],
     resps',
     env
  | Action.StateMove (id,s,s') ->
     { f with f_state = s' },
     resps @ [Var0 (Ident.Local (f.f_name, "state")), Expr.Val_enum s'],
     resps',
     env

let perform_delayed_action (f,env) (lhs,v) = 
  let id, v' = match lhs with 
    | Var0 (Ident.Local (_, id)) -> id, v
    | Var1 (Ident.Local (_, id), i) ->
       id,
       begin match List.assoc id env with
       | Expr.Val_array vs -> Expr.Val_array (Expr.array_update id vs i v)
       | _ -> raise (Internal_error "Fsm.perform_delayed_action") (* should not happen *)
       end
    | _ ->
       raise (Internal_error "Fsm.perform_delayed_action") (* should not happen *) in
  { f with f_vars = replace_assoc' id v' f.f_vars },
  ListExt.replace_assoc id v' env
  
let string_of_actions resps = ListExt.to_string (function (id,v) -> Ident.to_string id ^ ":=" ^ (Expr.string_of_opt_value v)) "," resps

let do_actions env f acts = 
  let f', resps', resps'', env' = List.fold_left do_action (f,[],[],env) acts in
  (* Printf.printf "do_actions: resps'=[%s] resps''=[%s]\n" (string_of_actions resps') (string_of_actions resps''); *)
  match cfg.act_sem with
  | Sequential ->
      f', resps'
  | Synchronous ->
      let f'', _ = List.fold_left perform_delayed_action (f',env') resps'' in
      f'', resps' @ resps''

let mk_local_env f genv = 
  let get_value id =
      try List.assoc (f.f_l2g id) genv
      with Not_found -> raise (Internal_error "Fsm.mk_local_env") in (* should not happen *)
  let extract_global_fns acc (id, v) = match v with
      Expr.Val_fn _ -> (id,v) :: acc
    | _ -> acc in
  List.map (function (id,ty) -> id, get_value id) (f.f_inps @ f.f_inouts)
  @ List.map erase_type f.f_vars
  @ List.fold_left extract_global_fns [] genv 

let rec react t genv f =
  (* Compute the reaction, at time [t] of FSM [f] in a global environment [genv].
     The global environment contains the values of global inputs, shared objects and global functions.
     Return an updated fsm and list of responses consisting of
     - updates to global outputs or shared objects
     - updates to local variables (including state move) *)
  let lenv = mk_local_env f genv in
  let cross_transition (s,(cond,acts,_,_),s') =
    let acts' = if s <> s' then Action.StateMove(f.f_name,s,s')::acts else acts in
    let f', resps' = do_actions lenv f acts'  in   (* .. perform associated actions .. *)
    { f' with f_has_reacted=true }, resps' in
  let ts = List.filter (fireable f lenv) (transitions_of f) in
  match ts with
    [] ->                                                                 (* No transition found *)
    ({f with f_has_reacted=false}, [])
  | [t1] ->                                                               (* One found *)
     cross_transition t1
  | ts ->                                                                 (* Several found *)
     let priority_of (_,(_,_,p,_),_) = p in
     let compare_priority t1 t2 = Pervasives.compare (priority_of t2) (priority_of t1) in (* reverse order *)
     begin match List.sort compare_priority ts with
       t1::t2::_ ->
        if priority_of t1 > priority_of t2 then begin
            Printf.printf "Non deterministic transitions found for FSM %s at t=%d: {%s}; chose %s\n"
              f.f_name
              t
              (ListExt.to_string string_of_transition "," ts)
              (string_of_transition t1);
            cross_transition t1
          end
        else
          raise (NonDetTrans (f,ts,t))
     | _ -> raise (Internal_error "Fsm.react")
     end

and fireable f env (s,(cond,acts,_,_),s') =
  f.f_state = s && check_cond f env cond 

and check_cond f env (evs,guards) =
  List.for_all (is_event_set env) evs && Condition.eval_guards env guards

and is_event_set env e = match List.assoc e env with
    Val_bool true -> true
  | _ -> false
  | exception Not_found -> false

let init_fsm genv f = match Repr.itransitions f.f_repr with
  | [(([],[]),acts,_,_), s] ->
     let env = mk_local_env f genv in
     do_actions env f (Action.StateMove (f.f_name,"",s) :: acts)
  | [_] ->
     raise (IllegalTrans (f, "illegal initial transition"))
  | _ ->
     raise (IllegalTrans (f, "muliple initial transitions"))

(* DOT OUTPUT *)

let string_of_io f (id,(ty,gl)) = id ^ ": " ^ Types.string_of_type ty ^ " (->" ^ global_id gl ^ ")"
let string_of_param (id,(ty,v)) = id  ^ "=" ^ Expr.string_of_value v 
let string_of_var (id,(ty,_)) = id  ^ ":" ^ Types.string_of_type ty

type dot_options =
    OmitImplicitTransitions
  | GlobalNames
  | NoCaption

let dot_output_oc oc ?(dot_options=[]) ?(options=[]) f =
  let string_of_var (id,(ty,_)) = "var " ^ id  ^ ":" ^ Types.string_of_type ty in
  (* let caption = StringExt.concat_sep "\\r" 
   *    [ListExt.to_string (string_of_io "inp") "\\r" f.f_inps;
   *     ListExt.to_string (string_of_io "out") "\\r" f.f_outps;
   *     ListExt.to_string (string_of_io "inout") "\\r" f.f_inouts;
   *     ListExt.to_string string_of_var "\\r" f.f_vars] in *)
  let caption = ListExt.to_string string_of_var "\\r" f.f_vars in
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
  Logfile.write fname;
  close_out oc

let dot_output_model ?(fname="") ?(dot_options=[]) ?(options=[]) ~dir f =
  let string_of_var (id,ty) = "var " ^ id  ^ ":" ^ Types.string_of_type ty in
  let fname = if fname = "" then Filename.concat dir (f.fm_name ^ ".dot") else fname in
  let oc = open_out fname in
  let caption = ListExt.to_string string_of_var "\\r" f.fm_vars in
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
  Logfile.write fname;
  close_out oc

(* TXT OUTPUT *)

let inputs_of m = List.filter (function (_,(Types.IO_In,_)) -> true | _ -> false) m.fm_ios
let outputs_of m = List.filter (function (_,(Types.IO_Out,_)) -> true | _ -> false) m.fm_ios
let inouts_of m = List.filter (function (_,(Types.IO_Inout,_)) -> true | _ -> false) m.fm_ios

let dump_model oc f =
  let of_list f xs = ListExt.to_string f ", " xs in
  let string_of_io (id,(_,ty)) = id  ^ ":" ^ Types.string_of_type ty in
  let string_of_var (id,ty) = id  ^ ":" ^ Types.string_of_type ty in
  let string_of_acts = ListExt.to_string Action.to_string "; " in
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
  let of_list f xs = ListExt.to_string f ", " xs in
  let string_of_acts = ListExt.to_string Action.to_string "; " in
  Printf.fprintf oc "FSM %s{\n" f.f_name;
  Printf.fprintf oc "  MODEL = %s\n" f.f_model.fm_name;
  if f.f_params <> []  then
    Printf.fprintf oc "  PARAMS = { %s }\n" (of_list string_of_param f.f_params);
  Printf.fprintf oc "  STATES = { %s }\n" (of_list (function n -> n) (states_of f));
  Printf.fprintf oc "  INPS = { %s }\n" (of_list (string_of_io f) f.f_inps);
  Printf.fprintf oc "  OUTPS = { %s }\n" (of_list (string_of_io f) f.f_outps);
  (* Printf.fprintf oc "  INOUTS = { %s }\n" (of_list (string_of_io f) f.f_inouts); *)
  Printf.fprintf oc "  VARS = { %s }\n" (of_list string_of_var f.f_vars);
  (* Printf.fprintf oc "  TENV =\n"; Typing.dump_tenv oc f.f_tenv; Printf.fprintf oc "\n"; *)
  Printf.fprintf oc "  TRANS = {\n";
  List.iter 
    (fun (q,(cond,acts,p,_),q') ->
      Printf.fprintf oc "    { %s {%s} {%s} %s [%d]}\n" q (Condition.to_string cond) (string_of_acts acts) q' p)
    (transitions_of f);
  Printf.fprintf oc "    }\n";
  let (_,iacts,_,_),iq = List.hd (itransitions_of f) in
  Printf.fprintf oc "  ITRANS = { %s \"%s\" }\n" iq (string_of_acts iacts);
  Printf.fprintf oc "  }\n"
