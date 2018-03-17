(* open Stim *)
open Types
open Error

module State = struct
  type t = string
  let compare = Pervasives.compare
  let to_string s = s
end

module Condition = struct
  type t = 
    | Test of Ident.t * string * Expr.t    (* var, op, expr *)
    | IsPresent of Ident.t                 (* event *)
  let to_string' ?(paren=false) c =
    let opt_paren t s = if t then "(" ^ s ^ ")" else s in
    match c with
  | Test (v,op,e) -> opt_paren paren (Ident.to_string v ^ op ^ Expr.to_string e)
  | IsPresent id -> Ident.to_string id
  let to_string c = to_string' c
  (* BNF : <cond>  ::= ID
                     | ID <relop> <exp>
           <relop> ::= '=' | '>' | '<' | '<=' | '>=' | '!='  *)
  let rec p_cond csts = parser
      [< 'Genlex.Ident e1; rest >] -> p_cond1 csts e1 rest
  and p_cond1 csts e1 = parser 
      [< 'Genlex.Kwd op when List.mem_assoc op Expr.builtin_relops; e2 = Expr.parse csts >] -> Test (Ident.mk e1, op, e2)
    | [< >] -> IsPresent (Ident.mk e1)
  let parse csts s = p_cond csts s
  exception Unknown_op of string
  let eval env texp = match texp with
  | Test (id, op, exp) -> (Expr.lookup_prim op Expr.builtin_relops) (Expr.lookup env id) (Expr.eval env exp)
  | IsPresent _ -> failwith "Fsm.Condition.eval"  (* should not happen *)
end

module Action = struct
  type t = 
    | Assign of Ident.t * Expr.t        (* variable, value *)
    | Emit of Ident.t                   (* event *)
    | StateMove of string * string               (* old state, new state *)
  let to_string a = match a with
    | Assign (id, expr) -> Ident.to_string id ^ ":=" ^ Expr.to_string expr
    | Emit id -> Ident.to_string id
    | StateMove (s,s') -> s ^ "->" ^ s' (* should not happen *)
   (* BNF : <act>   ::= ID ':=' <exp> *)
  let rec p_act csts = parser
      [< 'Genlex.Ident e1; rest >] -> p_act1 csts e1 rest
  and p_act1 csts e1 = parser
      [< 'Genlex.Kwd ":="; e2=Expr.parse csts >] -> Assign (Ident.mk e1, e2)
    | [< >] -> Emit (Ident.mk e1)
  let parse csts s = p_act csts s
end

module TransLabel = struct
  type t = Condition.t list * Action.t list * bool (* [true] for "implicit" transitions *)
  let compare = Pervasives.compare
  let to_string (conds,acts,_) =
    let s1 = begin match conds with
      [c] -> Condition.to_string c
    |  _  -> Ext.List.to_string (Condition.to_string' ~paren:true) "." conds
    end in
    let s2 = Ext.List.to_string Action.to_string "; " acts in
    let l = String.make (Misc.max (String.length s1) (String.length s2)) '_' in
    if s2 <> ""
    then Printf.sprintf "%s\\n%s\\n%s" s1 l s2 
    else Printf.sprintf "%s" s1
  let conds_of_string csts s = Ext.List.parse ";" (Condition.parse csts) (Expr.lexer s)
  let acts_of_string csts s = Ext.List.parse ";" (Action.parse csts) (Expr.lexer s)
end

module Repr = Lts.Make(State)(TransLabel)

type transition = State.t * TransLabel.t * State.t
type itransition = TransLabel.t * State.t

type fsm = { 
  m_name: string;
  m_params: (Ident.t * (Types.typ * Expr.value option)) list;
  m_inps: (Ident.t * (Types.typ * Expr.value option)) list;
  m_outps: (Ident.t * (Types.typ * Expr.value option)) list;
  m_vars: (Ident.t * (Types.typ * Expr.value option)) list;    (* local (internal, non shared) variables *)
  m_repr: Lts.Make(State)(TransLabel).t;                       (* Static representation as a LTS *)
  m_resolve: (transition list -> transition) option;
  m_state: string;                                             (* current state *)
  }

(* Inspectors *)

let states_of m = Repr.states' m.m_repr
let istate_of m = match Repr.istates' m.m_repr with [] -> None | qs -> Some (List.hd qs)
let transitions_of m = Repr.transitions m.m_repr
let itransitions_of m = Repr.itransitions m.m_repr

let succs m q = Repr.succs' m.m_repr q

let is_event_io ios e = match List.assoc e ios with
  TyEvent, _ -> true
| TyMemEvent, _ -> true
| _ -> false
| exception Not_found -> false

let is_scalar_io ios e = match List.assoc e ios with
  TyEvent, _ -> false
| TyMemEvent, _ -> false
| _ -> true
| exception Not_found -> false

let is_event_input m e = is_event_io m.m_inps e
let is_event_output m e = is_event_io m.m_outps e
let is_scalar_input m e = is_scalar_io m.m_inps e
let is_scalar_output m e = is_scalar_io m.m_outps e
let is_local_var m v = List.mem_assoc v m.m_vars

(* Modifiers *)

let sanity_check_transition m (q,(conds,acts,_),q') = 
  (* Check that [q] and [q'] are declared in [m] *)
  let check_state q = 
    if not (Repr.is_state m.m_repr q)
    then warning ("State " ^ m.m_name  ^ "." ^ (State.to_string q) ^ " is not declared") in
  (* Check that all IOs occuring in [conds] and [acts] are declared in [m] *)
  let check_cond = function
      Condition.Test (v, op, exp) ->  (* TODO: should also check [exp]...*)
        if not (is_scalar_input m v || is_local_var m v)
        then warning ("Variable " ^ (Ident.to_string v) ^ ", used as a condition in some transition(s), is not declared as input nor variable")
    | Condition.IsPresent e ->
        if not (is_event_input m e)
        then warning ("Event " ^ (Ident.to_string e) ^ ", used as in some transition(s), is not declared in inputs") in
  let check_act = function
      Action.Assign (v, exp) -> 
        if not (is_scalar_output m v || is_local_var m v) 
        then warning ("Variable " ^ (Ident.to_string v) ^ ", assigned in some transition(s), is not declared as output nor variable")
    | Action.Emit e -> 
        if not (is_event_output m e)
        then warning ("Event " ^ (Ident.to_string e) ^ ", emitted in some transition(s), is not declared in outputs")
    | _ -> () in
  begin match q with Some q -> check_state q | None -> () end;
  check_state q';
  List.iter check_cond conds;
  List.iter check_act acts

let add_state s m = { m with m_repr = Repr.add_state s m.m_repr }
let add_transition (s,(conds,acts),s') m =
  let t = TransLabel.conds_of_string [] conds, TransLabel.acts_of_string [] acts, false in
  sanity_check_transition m (Some s,t,s');
  { m with m_repr = Repr.add_transition (s,t,s') m.m_repr }
let add_itransition (acts,s) m =
  let t = [], TransLabel.acts_of_string [] acts, false in
  sanity_check_transition m (None,t,s);
  { m with m_repr = Repr.add_itransition (t,s) m.m_repr }
let remove_state s m = { m with m_repr = Repr.remove_state s m.m_repr }

(* Building *)

let sanity_check m = 
  Repr.iter_transitions (fun (s,t,s') -> sanity_check_transition m (Some s,t,s')) m.m_repr
  (* TODO: other sanity checks.. *)

let create ~name:name ~params:params
    ~states:states ~inps:inps ~outps:outps ~vars:vars ~trans:trans ?(resolve=None) ~itrans:(s0,acts) =
  (* Note that parameters, inputs and outputs are bound to _global_ idents (w/o prefix).
     This will both ease sharing (i.e. no need to duplicate input stimuli, such as [A1.H=.., A2.H=...]
     and improve legibility of output traces. 
     OTOH, this means two really distinct inputs (resp. outputs) cannot have the same name and must be
     explicitely distinguished [by writting [A1.o1] and [A2.o2] for ex.
     Local variables, by contrast, will be refered to using prefixed idents. *)
  let is_scalar = function (_, TyEvent) -> false | _ -> true in
  let type_of_val = function Expr.Val_int _ -> TyInt None | Expr.Val_enum _ -> TyEnum [] in
  let mk_ival (i,v) = (Ident.mk i, (type_of_val v, Some v)) in
  let mk_ival' (i,ty) = (Ident.mk i, (ty, None)) in
  let ts =
    List.map
      (function (e1,conds,acts,e2) ->
        (e1, (TransLabel.conds_of_string params conds, TransLabel.acts_of_string params acts, false), e2))
      trans in
  let iacts = TransLabel.acts_of_string params acts in
  let r = {
    m_name = name;
    m_repr = Repr.create ~states:states ~itrans:[([],iacts,false),s0] ~trans:ts;
    m_resolve = resolve;
    m_params = List.map mk_ival params;
    m_state = "";  (* current state is not defined until the initial transition has been carried out *) 
    m_inps = List.map mk_ival' inps;
    m_outps = List.map mk_ival' outps;
    m_vars = Ext.List.filter_map is_scalar mk_ival' vars } in
  sanity_check r;
  r

(* Defactorizing *)

exception Unknown_var of string * string
exception Invalid_domain of string * string

let var_type m v = 
    try fst (List.assoc v m.m_vars)
    with Not_found -> raise (Unknown_var (m.m_name, Ident.to_string v))

let var_domain m v = 
    match var_type m v with
    | Types.TyBool -> [v, Expr.Val_int 0; v, Expr.Val_int 1]
    | Types.TyEnum cs -> List.map (function c -> v, Expr.Val_enum c) cs
    | Types.TyInt (Some (lo,hi)) -> Ext.List.range (function x -> v, Expr.Val_int x) lo hi
    | _ -> raise (Invalid_domain (m.m_name, Ident.to_string v))

let defactorize (*?(clean=true)*) ?(rename_outps="") ?(istate="") name' vars' m =
  let vars = List.map Ident.mk vars' in
  let mk_st (s,vvs) =  s ^ Ext.List.to_string (function _, v -> Expr.string_of_value v) "" vvs in
  let d = Ext.List.cart_prodn (List.map (var_domain m) vars) in
    (* [D = D_v1 X D_v2 X ... D_Vn] lists all possible valuations of local vars [v1], ..., [vn] *)
  let dd = Ext.List.cart_prod2 d d in
    (* DD = D X D *)
  let states' = Ext.List.cart_prod2 (states_of m) d in
  let add_states mm = List.fold_left (fun m (s, vvs) -> Repr.add_state (mk_st (s,vvs)) m) mm states' in
  let mk_env vvals = List.map (function (name,value) -> name, (var_type m name, Some value)) vvals @ m.m_params in
  let check_cond vvals cond = match cond with
    | Condition.Test (v,_,_) as t when List.mem_assoc v vvals -> Condition.eval (mk_env vvals) t
    | _ -> true in
  let is_not_var_cond vars cond = match cond with
    | Condition.Test (v,_,_) when List.mem_assoc v vars -> false
    | _ -> true in
  let do_action vvals acc act = match act with
      Action.Assign (name, expr) when List.mem name vars ->
        let value = Expr.eval (mk_env vvals) expr in
        (name,value) :: acc
    | _ -> acc in
  let match_var_value vvals vvals'' (name,value) =
     if List.mem_assoc name vvals'' then List.assoc name vvals'' = value
     else if List.mem_assoc name vvals then List.assoc name vvals = value
     else true in  (* If [name] is not assigned in [vvals''], nor listed in [vvals] then it implicitely matches *)
  let rewrite_act vvals' acc act = match act with
    | Action.Assign (v,exp) ->
        if List.mem_assoc v vvals'
        then acc                                                                 (* suppress *)
        else Action.Assign (v, Expr.subst_vars vvals' exp) :: acc                (* rewrite *)
    | Action.Emit _
    | Action.StateMove _ -> act :: acc in                                        (* keep *)
  let defact_transition (s,(conds,acts,is_impl),s') =
    let dd_c =
      (* [DD_c] is the restriction of [DD] to the pairs [(vvals,vvals')] such that [conds], evaluated
         in the environment [vvals] gives [true].
         For ex: If [vars={c}] and [domain(c)={0,1}]
                 then [D={c=0,c=1}], [DD={(c=0,c=0),(c=0,c=1),(c=1,c=0),(c=1,c=1)}] and
                 [DD_{c<1}={(c=0,c=0),(c=0,c=1)}] *)
      List.filter
        (function (vvals,vvals') -> List.for_all (check_cond vvals) conds)
        dd in
    let dd_a =
      (* [DD_a] is the restriction of [DD_c] to the pairs [(vvals,vvals')] such that the value associated
         to variable [v] in [vvals'] is either
         - that obtained by evaluating [acts] in the environment [vvals] (when [v] is assigned by [acts])
         - or that associated to [v] in [vvals] (when [v] is not modified by [acts].
         For ex, under the same assumptions of the example above : [DD_{c:=c+1}={(c=0,c=1)}] *)
      List.filter
        (function (vvals,vvals') ->
           let vvals'' = List.fold_left (do_action vvals) [] acts in
           List.for_all (match_var_value vvals vvals'') vvals')
        dd_c in
     List.map
       (function (vvals,vvals') ->
          let conds' = List.filter (is_not_var_cond vvals) conds in
          (* [conds'] is the restriction of [conds] to the conditions not involving local variables *)
          let acts' = List.fold_left (rewrite_act vvals') [] acts in
          (* [acts'] is the rewriting of [acts] in which all assignations to local variables have been removed
             and all occurences of local variables in RHS of expressions have been replaced by the value
             associated to this variable in [vvals'] (i.e. the "target" value) *)
          (mk_st (s,vvals), (conds', List.rev acts', is_impl), mk_st (s',vvals')))  (* [rev] is mandatory to preserve the order .. *)
       dd_a in
  let add_transitions mm =
    Repr.fold_transitions
      (fun t m ->
         let ts' = defact_transition t in 
         List.fold_left (fun m' t' -> Repr.add_transition t' m') m ts')
      m.m_repr
      mm in
  let defact_itransition ((_,acts,is_impl),s) =
    let d_a =
      (* [D_a] is the restriction of [D] to the valuations [vvals] such that the values associated
         to the variables in [vvals] match those obtained by evaluating the initial actions [acts] *)
      List.filter
        (function vvals ->
           let vvals' = List.fold_left (do_action []) [] acts in  (* Initial actions are evaluated in an empty env *)
           List.for_all (match_var_value [] vvals') vvals)
        d in
     List.map
       (function vvals ->
          let acts' = List.fold_left (rewrite_act vvals) [] acts in
          (* [acts'] is the restriction of [acts] to the actions not involving local variables *)
          (([], acts',is_impl), mk_st (s,vvals)))
       d_a in
  let add_itransitions mm =
    Repr.fold_itransitions
      (fun t m ->
         let ts' = defact_itransition t in 
         match istate with
         | "" ->
             List.fold_left (fun m' t' -> Repr.add_itransition t' m') m ts'
         | s ->
             let t' = List.find (function (_,s') -> s'=s) ts' in
             Repr.add_itransition t' m)
      m.m_repr
      mm in
  let repr' = 
    Repr.empty |> add_states |> add_transitions |> add_itransitions in
  { m with m_name = name';
           m_repr = Repr.clean repr';
           m_outps = List.map (function (n, (ty,v)) -> Ident.add_prefix rename_outps n, (ty,v)) m.m_outps;
           m_vars = List.fold_left (fun vs v -> List.remove_assoc v vs) m.m_vars vars }

(* Add implicit transitions *)

let add_implicit_transitions ?(name="") m = 
  (* If an input event [e] does not appear in any transition coming from state [q], then this means that
     if [e] occurs when the machine is in state [q], then it has no effect (other than being memorized if 
     this is a buffered event). Adding an explicit transition is required for giving the "right" interpretation
     to the product of two FSMs as a synchronized product of the underlying LTS. *)
  let add_transitions r =
    let add_trans q r =
       let occurs_in_outc_trans q e = (* check whether an event [e] occurs in the transitions coming out of state [q] *)
         let occurs_in_cond e = function Condition.IsPresent e' when e=e' -> true | _ -> false in
         let check_conds e conds = List.exists (fun cond -> occurs_in_cond e cond) conds in
         Repr.fold_succs r q (fun q' (conds,_,_) z -> z || check_conds e conds) false in
       List.fold_left
         (fun r' (i,_) ->
            if is_event_input m i && not (occurs_in_outc_trans q i)
            then Repr.add_transition (q,([Condition.IsPresent i],[],true),q) r'
            else r')
         r
         m.m_inps in
    Repr.fold_states (fun q r' -> add_trans q r') r r in
  { m with
      m_name = if name <> "" then name else m.m_name ^ "_expl";
      m_repr = add_transitions m.m_repr } 

(* Product *)

let is_global n = Ident.prefix n = ""
let is_local n = Ident.prefix n <> "" || Ident.suffix n = "state"

type event_sync_mode = 
    Blocking     (* emission and reception are blocking *)
  | NonBlocking  (* emission in non blocking; the corresponding event may be lost *)

let product ?(rename_outps="") event_sync_mode name m1 m2 =
  let module M = struct
    type state = State.t
    type label = TransLabel.t
    let merge_state (s1,s2) = s1 ^ s2
    let merge_label = function
      | None, None -> [], [], false
      | Some t, None -> t
      | None, Some t -> t
      | Some (conds1,acts1,is_impl1), Some (conds2,acts2,is_impl2) -> conds1@conds2, acts1@acts2, is_impl1&&is_impl2
  end in
  let module P = Lts.IProduct(Repr)(M) in
  let shared_events = 
    (* Shared events are those listed both as inputs of [m1] and outputs of [m2] or vice versa *)
     let scan inps outps = 
       List.fold_left
         (fun acc e -> match e with 
             e, (TyEvent,_) when List.mem_assoc e outps -> e::acc 
           | e, (TyMemEvent, _) ->
               failwith ("Event " ^ (Ident.to_string e) ^ " is buffered.\
                         The product of two FSMs is not (yet) defined in this situation..")
           | _, _ -> acc) 
       [] inps in
     scan m1.m_inps m2.m_outps @ scan m2.m_inps m1.m_outps  in
  let shared_events_of_conds conds = 
    let scan acc cond = match cond with
        Condition.IsPresent e when List.mem e shared_events -> e :: acc
      | _ -> acc in
    List.fold_left scan [] conds in
  let shared_events_of_acts acts = 
    let scan acc act = match act with
        Action.Emit e when List.mem e shared_events -> e :: acc
      | _ -> acc in
    List.fold_left scan [] acts in
  let shared_events_of_trans (conds,acts,_) = shared_events_of_conds conds, shared_events_of_acts acts in
  let sync = function 
      | None, None -> true
      | Some t, None ->
          let z =
            begin match event_sync_mode, shared_events_of_trans t with
              Blocking, ([],[]) -> true
            | NonBlocking, ([],_) -> true
                (* In non blocking mode, shared events are allowed in the [acts] part of a single-sided transition *)
            | _, _ -> false
            end in
          Printf.printf "** sync(%s, .) = %b\n" (TransLabel.to_string t) z;
          z
      | None, Some t ->
          let z =
            begin match event_sync_mode, shared_events_of_trans t with
              Blocking, ([],[]) -> true
            | NonBlocking, ([],_) -> true
                (* In non blocking mode, shared events are allowed in the [acts] part of a single-sided transition *)
            | _, _ -> false
            end in
          Printf.printf "** sync(., %s) = %b\n" (TransLabel.to_string t) z;
          z
      | Some ((conds1,acts1,is_impl1) as t1), Some ((conds2,acts2,is_impl2) as t2) -> 
          let z = 
          begin match shared_events_of_trans t1, shared_events_of_trans t2 with
            ([],[]), ([],[]) -> true
               (* No shared events involved *)
          | (evs1,evs1'), (evs2,evs2') ->
                 List.for_all (function e -> List.mem e evs1') evs2
              && List.for_all (function e -> List.mem e evs2') evs1
               (* All shared events awaited in [conds2] are emitted in [acts1]
                  and all shared events awaited in [conds1] are emitted in [acts2] *)
              && (event_sync_mode = NonBlocking || List.for_all (function e -> List.mem e evs2) evs1')
              && (event_sync_mode = NonBlocking || List.for_all (function e -> List.mem e evs1) evs2')
               (* All shared events emitted in [acts1] are awaited in [conds2]
                  and all shared events emitted in [acts2] are awaited in [conds1] *)
               (* These two last conditions are removed if non-blocking emission of events is allowed. *)
          end in
          Printf.printf "** sync(%s, %s) = %b\n" (TransLabel.to_string t1) (TransLabel.to_string t2) z;
          z in
  let mm1 = add_implicit_transitions m1 in
  let mm2 = add_implicit_transitions m2 in
  let p = P.synch_product sync mm1.m_repr mm2.m_repr in
  let remove_shared_evs vars = List.fold_left (fun acc ev -> List.remove_assoc ev acc) vars shared_events in
  { m_name = name;
    m_repr = p;
    m_resolve = None; (* TOFIX *)
    m_params = Ext.List.union m1.m_params m2.m_params;
    m_state = M.merge_state (m1.m_state, m2.m_state);
    m_inps = Ext.List.union m1.m_inps m2.m_inps;
    m_outps = List.map
      (function (n, (ty,v)) -> Ident.add_prefix rename_outps n, (ty,v))
      (Ext.List.union m1.m_outps m2.m_outps);
    m_vars = remove_shared_evs (Ext.List.union m1.m_vars m2.m_vars) }

(* Printing *)

let rec string_of_fsm m = 
  Printf.sprintf "%s=<st=%s,inps=[%s],vars=[%s],outps=[%s]>" 
    m.m_name
    m.m_state
    (Ext.List.to_string string_of_comp ", " m.m_inps)
    (Ext.List.to_string string_of_comp ", " m.m_vars)
    (Ext.List.to_string string_of_comp ", " m.m_outps)
    
and string_of_comp (id,(ty,v)) = (Ident.to_string id) ^ ": " ^ string_of_type ty ^ " = " ^ Expr.string_of_opt_value v
and string_of_comp_t (id,(ty,_)) = (Ident.to_string id) ^ ": " ^ string_of_type ty
and string_of_comp_v (id,(_,v)) = (Ident.to_string id)  ^ "=" ^ Expr.string_of_opt_value v

and string_of_type t = match t with 
  | TyEvent -> "event"
  | TyMemEvent -> "event*"
  | TyBool -> "bool"
  | TyEnum cs -> " enum {" ^ Ext.List.to_string (function c -> c) "," cs ^ "}"
  | TyInt None -> "int"
  | TyInt (Some (lo,hi)) -> "int<" ^ string_of_int lo ^ ".." ^ string_of_int hi ^ ">"

(* DOT OUTPUT *)

type options = 
    OmitImplicitTransitions

let dot_output, dot_view =
  let fsm_params m = match m.m_params with
    [] -> []
  | ps -> [Ext.List.to_string string_of_comp_v "\\n" m.m_params, {Dot.node_shape="rect"; Dot.node_style="solid"}] in
  let fsm_vars m = match m.m_vars with 
    [] -> []
  | vs -> [Ext.List.to_string string_of_comp_t "\\n" m.m_vars, {Dot.node_shape="rect"; Dot.node_style="rounded"}] in
  let impl_ts m opts = 
    if List.mem OmitImplicitTransitions opts
    then List.filter (function (_,(_,_,is_impl),_) -> is_impl) (Repr.transitions m.m_repr) 
    else [] in
  (fun dir ?(dot_options=[]) ?(options=[]) m ->
    let fname = Filename.concat dir (m.m_name ^ ".dot") in
    Repr.dot_output m.m_name fname
      ~options:dot_options ~extra_nodes:(fsm_params m @ fsm_vars m) ~implicit_transitions:(impl_ts m options) m.m_repr;
    Printf.printf "Wrote file %s\n" fname),
  (fun ?(cmd="open -F -a Graphviz") ?(dir="/tmp") ?(dot_options=[]) ?(options=[]) m ->
    Repr.dot_view m.m_name ~cmd:cmd ~dir:dir
       ~options:dot_options ~extra_nodes:(fsm_params m @ fsm_vars m) ~implicit_transitions:(impl_ts m options) m.m_repr)

(* TXT OUTPUT *)

let text_output m =
  let fname = m.m_name ^ ".fsm" in
  let oc = open_out fname in
  let of_list f xs = Ext.List.to_string f ", " xs in
  let string_of_conds = Ext.List.to_string Condition.to_string "; " in
  let string_of_acts = Ext.List.to_string Action.to_string "; " in
  Printf.fprintf oc "FSM %s {\n" m.m_name;
  if m.m_params <> []  then
    Printf.fprintf oc "  PARAMS = { %s }\n" (of_list string_of_comp_v m.m_params);
  Printf.fprintf oc "  STATES = { %s }\n" (of_list (function n -> n) (states_of m));
  Printf.fprintf oc "  INPS = { %s }\n" (of_list string_of_comp_t m.m_inps);
  Printf.fprintf oc "  OUTPS = { %s }\n" (of_list string_of_comp_t m.m_outps);
  Printf.fprintf oc "  VARS = { %s }\n" (of_list string_of_comp_t m.m_vars);
  Printf.fprintf oc "  TRANS = {\n";
  List.iter 
    (fun (q,(conds,acts,_),q') ->
      Printf.fprintf oc "    { %s \"%s\" \"%s\" %s }\n" q (string_of_conds conds) (string_of_acts acts) q')
    (transitions_of m);
  Printf.fprintf oc "    }\n";
  let (_,iacts,_),iq = List.hd (itransitions_of m) in
  Printf.fprintf oc "  ITRANS = { %s \"%s\" }\n" iq (string_of_acts iacts);
  Printf.fprintf oc "  }\n"
