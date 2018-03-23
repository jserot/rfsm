open Utils
   
module TransLabel = struct
  type t = Condition.t * Action.t list * bool
   (* Cond will be ([],[]) for initial transitions, [bool] is true for "implicit" transitions *)
  let compare = Pervasives.compare
  let to_string (cond,acts,_) =
    let s1 = Condition.to_string cond in
    let s2 = ListExt.to_string Action.to_string "; " acts in
    let l = String.make (Misc.max (String.length s1) (String.length s2)) '_' in
    if s2 <> ""
    then Printf.sprintf "%s\\n%s\\n%s" s1 l s2 
    else Printf.sprintf "%s" s1
  let rename f (cond,acts,is_impl) = (Condition.rename f cond, List.map (Action.rename f) acts, is_impl)
  let subst env (cond,acts,is_impl) = (Condition.subst env cond, List.map (Action.subst env) acts, is_impl)
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

type model = { 
  fm_name: string;
  fm_params: (string * Types.typ) list;                      (** name, type *)
  fm_ios : (string * (Types.dir * Types.typ)) list;          (** i/os *)
  fm_vars: (string * Types.typ) list;                        (** name, type *)
  fm_repr: Repr.t;                                           (** Static representation as a LTS *)
  fm_resolve: (transition list -> transition) option;
  }

type inst = { 
  f_name: string;
  f_model: model;
  f_params: (string * (Types.typ * Expr.value)) list;       (** name, type, actual value *)
  f_inps: (string * (Types.typ * global)) list;             (** local name, (type, global) *)
  f_outps: (string * (Types.typ * global)) list;            (** local name, (type, global) *)
  f_inouts: (string * (Types.typ * global)) list;           (** local name, (type, global) *)
  f_vars: (string * (Types.typ * Expr.value option)) list;  (** name, (type, value) *)
  f_repr: Repr.t;                                           (** Static representation as a LTS (with _local_ names) *)
  (* f_enums: (string * Types.typ);                            (\** Locally used enums, with their associated type *\) *)
  f_l2g: string -> string;                                  (** local -> global name *)
  f_resolve: (transition list -> transition) option;
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
| ValueChange of (int * Expr.value) list  (** (Date,value)s *)

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
exception Type_mismatch of string * string * string * Types.typ * Types.typ (** FSM, kind, id, type, type *)
exception Type_error of string * string * string * Types.typ * Types.typ (** FSM, what, id, type, type *)

exception Uninstanciated_type_vars of string * string * string * string list (* FSM, kind, id, vars *)

(* Builders *)

let mk_bindings ~local_names:ls ~global_names:gs =
  let l2g  =
    try List.combine ls gs
    with Invalid_argument _ -> raise (Internal_error "Fsm.mk_bindings") (* should not happen *) in
  (function id -> try List.assoc id l2g with Not_found -> id)

let build_model ~name ~states ~params ~ios ~vars ~trans ~itrans = 
  let r = {
        fm_name = name;
        fm_params = params;
        fm_ios = List.map (function (dir,id,ty) -> (id, (dir,ty))) ios;
        fm_vars = vars;
        fm_repr =
          begin
            try Repr.create
                   ~states:states
                   ~trans:(List.map (function (s,(ev,gds),acts,s') -> s, (([ev],gds),acts,false), s') trans)
                   ~itrans:(let q0,iacts = itrans in [(([],[]),iacts,false),q0])
            with
              Repr.Invalid_state s -> raise (Invalid_state (name, s))
          end;
        fm_resolve = None; (* TO FIX *)
      } in
  r

let type_check ~strict fsm what where ty ty'  =
  if not (Types.type_equal ~strict ty ty') 
  then raise (Type_mismatch (fsm, what, where, ty, ty'))

let type_check_stim fsm id ty st = match st with
  | ValueChange vcs ->
     List.iter
       (type_check ~strict:false fsm "stimuli" id ty) 
       (List.map (function (_,v) -> Types.type_of_value v) vcs)
  | _ ->
     ()

let sanity_check tenv f =
  let isymbols, osymbols =
    List.fold_left
      (fun (ivs,ovs) (_,(cond,acts,_),_) ->
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
  let check_type kind (id,ty) = 
    match Types.vars_of ty with
    | [] -> ()
    | vs -> raise (Uninstanciated_type_vars (f.f_name, kind, id, vs)) in
  let get l = List.map (function (id,(ty,_)) -> id, ty) l in
  (* Check that each input symbol occuring in transition rules is declared as input or local variable *)
  check_symbols "input or local variable"
    isymbols
    (get f.f_inps @ get f.f_inouts @ get f.f_vars @ get f.f_params);
  (* Check that each output symbol occuring in transition rules is declared as output or local variable *)
  check_symbols "output or local variable"
    osymbols
    (get f.f_outps @ get f.f_inouts @ get f.f_vars);
  (* Check that all type indexes have been instanciated *)
  List.iter (check_type "input") (List.map (function (id, (ty,_)) -> id,ty) f.f_inps);
  List.iter (check_type "output") (List.map (function (id, (ty,_)) -> id,ty) f.f_outps);
  List.iter (check_type "inout") (List.map (function (id, (ty,_)) -> id,ty) f.f_inouts);
  List.iter (check_type "variable") (List.map (function (id, (ty,_)) -> id,ty) f.f_vars);
  (* Type checking *)
  (* let type_check what item ty ty' =
   *   if not (Types.type_equal [] ty ty')
   *     (\* Checking is here carried out with an empty index env since all indexes are supposed to have been instanciated *\)
   *   then raise (Type_error (f.f_name, what, item, ty, ty')) in *)
  let type_check_guard ((e1,op,e2) as g) =
    try type_check
          ~strict:true f.f_name "guard" (Condition.string_of_guard g)
          (Types.type_expression tenv (Expr.EBinop (op,e1,e2))) Types.TyBool
    with
      Types.Typing_error (expr, ty, ty') -> raise (Type_error (f.f_name, "guard", Expr.to_string expr, ty, ty')) in
  let type_check_condition (_,gs) = List.iter type_check_guard gs in
  let type_check_action act = match act with 
    | Action.Assign (v, exp) -> 
       let t = try List.assoc v tenv.te_vars with Not_found -> raise (Internal_error "Fsm.type_check_action") in
       begin
         try type_check
               ~strict:false f.f_name "action"
               (* [strict=false] here to accept actions like [v:=1] where [v:int<lo..hi>] *)
               (Action.to_string act) (Types.type_expression tenv exp) t
         with
           Types.Typing_error (expr, ty, ty') -> raise (Type_error (f.f_name, "action", Expr.to_string expr, ty, ty'))
       end
    | Action.Emit s ->
       let t = try List.assoc s tenv.te_vars with Not_found -> raise (Internal_error "Fsm.type_check_action") in
       type_check ~strict:true f.f_name "action" (Action.to_string act) t TyEvent
    | _ -> () in
  let type_check_transition (_,(cond,acts,_),_) =
    type_check_condition cond;
    List.iter type_check_action acts in
  List.iter type_check_transition (Repr.transitions f.f_repr)

let build_instance ~name ~model ~params ~ios =
    let bind_param vs (p,ty) =
      match ty, List.assoc p vs with 
        Types.TyInt _, (Expr.Val_int c as v) -> p, (ty,v)
      | _, _ -> raise (Invalid_parameter (name, p))
      | exception Not_found -> raise (Binding_mismatch (name, "parameters", p)) in
    let bound_params = List.map (bind_param params) model.fm_params in
    let ienv = List.map (function id,(ty,v) -> id, v) bound_params in
    let bind_io (lid,(dir,lty)) gl =
      let ty' = Types.subst_indexes ienv lty in
      match dir, gl with 
      | Types.IO_In, GInp (gid,ty,st) ->
         type_check_stim name gid ty st;
         type_check ~strict:true name "input" gid ty ty';
         (lid,gid,Types.IO_In,ty,gl)
      | Types.IO_In, GShared (gid,ty) ->
         type_check ~strict:true name "input" gid ty ty';
         (lid,gid,Types.IO_In,ty,gl)
      | Types.IO_In, _ ->
         raise (Binding_mismatch (name, "input", lid))
      | Types.IO_Out, GOutp (gid,ty)
        | Types.IO_Out, GShared (gid,ty) ->
         type_check ~strict:true name "output" gid ty ty';
         (lid,gid,Types.IO_Out,ty,gl)
      | Types.IO_Out, _ ->
         raise (Binding_mismatch (name, "output", lid))
      | Types.IO_Inout, GShared (gid,ty) ->
         type_check ~strict:true name "inout" gid ty ty';
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
    let r =
      { f_name = name;
        f_model = model;
        f_repr = Repr.map_label (TransLabel.subst ienv) model.fm_repr;
        (* f_enums = collect_ty_ctors (; *)
        f_params = bound_params;
        f_inps = filter_ios Types.IO_In bound_ios;
        f_outps = filter_ios Types.IO_Out bound_ios;
        f_inouts = filter_ios Types.IO_Inout bound_ios;
        f_vars = List.map (function (id,ty) -> (id, (Types.subst_indexes ienv ty, None))) model.fm_vars;
        f_l2g =
          mk_bindings
            ~local_names:(List.map (function (lid,_,_,_,_) -> lid) bound_ios)
            ~global_names:(List.map (function (_,gid,_,_,_) -> gid) bound_ios);
        f_resolve = None; (* TO FIX *)
        f_state = "";  (* current state is not defined until the initial transition has been carried out *)
        f_has_reacted = false;
      } in
    let tenv =
      let vars = 
            List.map (function (id, (ty,_)) -> id, ty) r.f_params
          @ List.map (function (id, _, _, ty, _) -> id, ty) bound_ios
          @ List.map (function (id, (ty,_)) -> id, ty) r.f_vars in
      let local_ctors = 
          let add acc cs =
            List.fold_left
              (fun acc (c,ty) -> if List.mem_assoc c acc then acc else (c,ty)::acc)
              acc
              cs in
          List.fold_left
            (fun acc (_,ty) -> add acc (Types.tycons_of ty))
            []
            vars in
      { Types.builtin_tenv with
        Types.te_vars = vars;
        Types.te_ctors = Types.builtin_tenv.te_ctors @ local_ctors } in
    sanity_check tenv r;
    r

(* Dynamic behavior (reactive semantics) *)

exception IllegalTrans of inst * string
exception Undeterminate of inst * string * Types.date
exception NonDetTrans of inst * transition list * Types.date

type response = string * Expr.value option   (* name, value (None for pure events) *)

let rec replace_assoc' k v env =
  (* This is a variation on [Ext.List.replace_assoc], where [v=(_,v')] and only [v'] is replaced *)
  let rec repl = function
    [] -> []
  | (k',(x,v'))::rest -> if k=k' then (k,(x,v)) :: repl rest else (k',(x,v')) :: repl rest in
  repl env

(* FSM environment *)
  
type fsm_env = (string * Expr.value option) list

let do_action (f,resps,env) act =
  (* Make FSM [f] perform action [act] in (local) environment [env], returning an updated FSM [f'],
     a list of responses [resps], and an updated (local) environment [env'].
     Updates of local variables are performed immediately and reported in [resps] (for tracing) 
     and reflected in the environment (so that actions sequences such as "c:=c+1;s=c" are interpreted correctly.
     Updates in the environment are reported in [resps] but are not performed immediately, only at the end
     of each simulation step *)
  match act with
    Action.Assign (id, expr) ->
      let v = Expr.eval env expr in
      if List.mem_assoc id f.f_vars then (* Local variable *)
        { f with f_vars = replace_assoc' id (Some v) f.f_vars },
        resps @ [Ident.Local (f.f_name, id), Some v],
        ListExt.replace_assoc id (Some v) env
      else
        f, resps @ [Ident.Global (f.f_l2g id), Some v], env
  | Action.Emit id ->
        f, resps @ [Ident.Global (f.f_l2g id), Expr.set_event], env
  | Action.StateMove (id,s,s') ->
     { f with f_state = s' },
     resps @ [Ident.Local (f.f_name, "state"), Some (Expr.Val_enum s')],
     env

let do_actions env f acts = 
  let f', resps', _ = List.fold_left do_action (f,[],env) acts in
  f', resps'


let mk_local_env f genv = 
  let get_value id =
      try List.assoc (f.f_l2g id) genv
      with Not_found -> raise (Internal_error "Fsm.mk_local_env") in (* should not happen *)
  List.map (function (id,ty) -> id, get_value id) (f.f_inps @ f.f_inouts)
  @ List.map erase_type f.f_vars

let rec react t genv f =
  (* Compute the reaction, at time [t] of FSM [f] in a global environment [genv].
     The global environment contains the values of global inputs and shared objects.
     Return an updated fsm and list of responses consisting of
     - updates to global outputs or shared objects
     - updates to local variables (including state move) *)
  let env = mk_local_env f genv in
  let cross_transition (s,(cond,acts,_),s') =
    let acts' = if s <> s' then Action.StateMove(f.f_name,s,s')::acts else acts in
    let f', resps' = do_actions env f acts'  in   (* .. perform associated actions .. *)
    { f' with f_has_reacted=true }, resps' in
  let ts = List.filter (fireable f env) (transitions_of f) in
  match ts with
    [] ->                                                                 (* No transition found *)
    ({f with f_has_reacted=false}, [])
  | [t1] ->                                                               (* One found *)
     cross_transition t1
  | ts ->                                                                 (* Several found *)
     begin match f.f_resolve with
       None -> raise (NonDetTrans (f,ts,t))
     | Some select ->
        let t1 = select ts in
        Printf.printf "Non deterministic transitions found for FSM %s at t=%d: {%s}; chose %s\n"
                      f.f_name
                      t
                      (ListExt.to_string Repr.string_of_transition "," ts)
                      (Repr.string_of_transition t1);
        cross_transition (select ts)
     end

and fireable f env (s,(cond,acts,_),s') =
  f.f_state = s && check_cond f env cond 

and check_cond f env (evs,guards) =
  List.for_all (is_event_set env) evs && Condition.eval_guards env guards

and is_event_set evs e = match List.assoc e evs with
    Some _ -> true
  | None -> false
  | exception Not_found -> false

let init_fsm genv f = match Repr.itransitions f.f_repr with
  | [(([],[]),acts,_), s] ->
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
    then List.filter (function (_,(_,_,is_impl),_) -> is_impl) (Repr.transitions f.f_repr)
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
    then List.filter (function (_,(_,_,is_impl),_) -> is_impl) (Repr.transitions f.fm_repr)
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
    (fun (q,(cond,acts,_),q') ->
      Printf.fprintf oc "    { %s {%s} {%s} %s }\n" q (Condition.to_string cond) (string_of_acts acts) q')
    (Repr.transitions f.fm_repr);
  Printf.fprintf oc "    }\n";
  let (_,iacts,_),iq = List.hd (Repr.itransitions f.fm_repr) in
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
  Printf.fprintf oc "  TRANS = {\n";
  List.iter 
    (fun (q,(cond,acts,_),q') ->
      Printf.fprintf oc "    { %s {%s} {%s} %s }\n" q (Condition.to_string cond) (string_of_acts acts) q')
    (transitions_of f);
  Printf.fprintf oc "    }\n";
  let (_,iacts,_),iq = List.hd (itransitions_of f) in
  Printf.fprintf oc "  ITRANS = { %s \"%s\" }\n" iq (string_of_acts iacts);
  Printf.fprintf oc "  }\n"
