(* The type checker for the host language. *)

module type TYPING = sig
  module HostSyntax: Syntax.SYNTAX
  type env
  (* type typed_inst = {
   *     name: Ident.t; 
   *     params: (Ident.t * HostSyntax.typ * HostSyntax.Guest.expr) list;
   *     args: (Ident.t * HostSyntax.typ) list;
   *     model: HostSyntax.model_desc;  (\* Typed model *\)
   *     }
   * type typed_program = {
   *     type_decls: HostSyntax.type_decl list;  
   *     fun_decls: HostSyntax.fun_decl list;
   *     cst_decls: HostSyntax.cst_decl list;
   *     globals: HostSyntax.global list;
   *     insts: typed_inst list;
   *     } *)
  type typed_program = {
      tp_models: HostSyntax.model list;  (* Un-instanciated models *)
      tp_insts: (Ident.t * HostSyntax.model) list;  (* Model instances *)
      } [@@deriving show {with_path=false}]

  val mk_env: unit -> env
  val type_program: env -> HostSyntax.program -> typed_program
  val pp_env: Format.formatter -> env -> unit
  val pp_typed_program: Format.formatter -> typed_program -> unit

  exception Undefined_symbol of Location.t * Ident.t
  exception Duplicate_symbol of Location.t * Ident.t
  exception Invalid_state of Location.t * Ident.t
  exception Duplicate_state of Location.t * Ident.t
  exception No_event_input of Location.t
  exception Illegal_inst of Location.t
  exception Illegal_state_output of Location.t * Ident.t * Ident.t
end

module Make
         (HS: Syntax.SYNTAX)
         (GT: Guest.TYPING with module Syntax = HS.Guest and type Types.typ = HS.typ )
         (GS: Guest.STATIC with type expr = HS.expr) =
struct
  module HostSyntax = HS
  module GuestTyping = GT 
  module A = Annot 

  type env = GuestTyping.env

  (* type typed_inst = {
   *     name: Ident.t; 
   *     params: (Ident.t * HostSyntax.typ * HostSyntax.Guest.expr) list;
   *     args: (Ident.t * HostSyntax.typ) list;
   *     model: HostSyntax.model_desc;  (\* Typed model *\)
   *     } [@@deriving show {with_path=false}] *)
  
  type typed_program = {
      tp_models: HostSyntax.model list;  
      tp_insts: (Ident.t * HostSyntax.model) list;  
      } [@@deriving show {with_path=false}]

  exception Undefined_symbol of Location.t * Ident.t
  exception Duplicate_symbol of Location.t * Ident.t
  exception Invalid_state of Location.t * Ident.t
  exception Duplicate_state of Location.t * Ident.t
  exception No_event_input of Location.t
  exception Illegal_inst of Location.t
  exception Illegal_state_output of Location.t * Ident.t * Ident.t

  let mk_env () = GuestTyping.mk_env ()

  (* Typing FSM models *)

  let type_fsm_action env m act =
    (* TO FIX: the type of an action should always be "unit" ? *)
    let loc = act.A.loc in
    let ty = match act.A.desc with 
      | HostSyntax.Emit s ->
         let t = GuestTyping.Types.mk_type_constr0 "event" in 
         let t' = GuestTyping.lookup_var ~loc:act.A.loc s env in
         GuestTyping.type_check ~loc t t';
         t
      | HostSyntax.Assign (lhs,expr) -> 
         let t = GuestTyping.type_lhs env lhs in
         let t' = GuestTyping.type_expression env expr in
         (* let pp_typ = GuestTyping.Syntax.Types.pp_typ ~abbrev:false in
          * Format.printf "Host.type_fsm_action %a: %a <- %a\n" HostSyntax.pp_action act pp_typ t pp_typ t'; *)
         GuestTyping.type_check ~loc t t';
         t in
    act.A.typ <- ty

  let type_fsm_event ~loc t env ev =
    GuestTyping.type_check
      ~loc 
      (GuestTyping.Types.mk_type_constr0 "event")
      (GuestTyping.lookup_var ~loc ev env)

  let type_fsm_guard t env gexp =
    GuestTyping.type_check
      ~loc:gexp.A.loc
      (GuestTyping.Types.mk_type_constr0 "bool")
      (GuestTyping.type_expression env gexp)

  let type_fsm_condition t env A.{ desc=ev,gs; loc=loc; _ } =
    type_fsm_event ~loc t env ev;
    List.iter (type_fsm_guard t env) gs 

  let check_fsm_state ~loc A.{ desc=m; _} q = 
    let states = List.map (function s -> s.A.desc) m.HostSyntax.states in 
    if not (List.mem_assoc q states) then raise (Invalid_state (loc, q))

  let type_fsm_transition env m (A.{ desc= q,cond,acts,q',_; loc=loc; _ } as t) =
    (* For each transition [q -> cond / acts -> q'] check that
     *    - [q] and [q'] are listed as states in the model declaration
     *    - [cond] has form [e.[guard1]...[guardn]] where [e] has type [event] and each [guardi] type [bool]
     *    - for each [act]=[lhs:=rhs] in [acts], [type(rhs)=type(lhs)] *)
    check_fsm_state ~loc m q;
    check_fsm_state ~loc m q';
    type_fsm_condition t env cond;
    List.iter (type_fsm_action env m) acts

  let type_fsm_itransition env m A.{ desc=q,acts; loc=loc; _ } =
    (* For the initial transition [/ acts -> q] check that
     *    - [q] is listed as state in model declaration
     *    - for each [act]=[v:=exp] in [acts], [type(v)=type(exp)] *)
    check_fsm_state ~loc m q;
    List.iter (type_fsm_action env m) acts

  let type_fsm_state_valuation env (m:HostSyntax.model) q (o,expr) = 
    let te =
      try List.assoc o m.A.desc.HostSyntax.outps
      with Not_found -> raise (Illegal_state_output (expr.A.loc, q, o)) in
    GuestTyping.type_check
      ~loc:expr.A.loc 
      (GuestTyping.type_of_type_expr env te)
      (GuestTyping.type_expression env expr)

  let type_fsm_state env m { A.desc = q,ovs; _ } = 
    List.iter (type_fsm_state_valuation env m q) ovs

  let type_fsm_states env m = 
    let states = m.A.desc.HostSyntax.states in
    let rec check_dupl states = match states with
      | [] -> ()
      | { A.desc = q, _; A.loc = loc; _ } :: rest ->
         if List.exists (function { A.desc = q', _; _ } ->  q=q' ) rest 
         then raise (Duplicate_state (loc,q))
         else check_dupl rest in
    check_dupl states;
    List.iter (type_fsm_state env m) states

  let types_of_fsm_model env A.{ desc = m; loc = loc; _ } = 
    (* Computes the "local" typing environment associated to an FSM model, containing
       the types of parameters, inputs, outputs and local variables *)
    List.fold_left
      (fun acc (id,te) ->
        if List.mem_assoc id acc
        then raise (Duplicate_symbol (loc, id)) 
        else (id, GuestTyping.type_of_type_expr env te) :: acc)
      []
      (m.HostSyntax.params @ m.HostSyntax.inps @ m.HostSyntax.outps @ m.HostSyntax.inouts @  m.HostSyntax.vars)

  let type_fsm_ios env A.{ desc = m; loc = loc; _ } =
    (* Check that there's exactly one input with type event *)
    let is_event_type (_,te) = GuestTyping.Types.is_type_constr0 "event" te.A.typ in
    match List.filter is_event_type m.HostSyntax.inps with
    | [] -> raise (No_event_input loc)
    | _ -> ()
         
  let type_fsm_model (env,params) m =
    type_fsm_states env m;
    let type_indexes =
      List.fold_left
      (fun acc (id,ty,e) ->
        if GuestTyping.Types.is_index_type ty then (id,GS.eval_index e)::acc
        else acc)
      []
      params in
    (* Format.printf "** Host.Typing.type_fsm_model: indexes=[%a]\n"
     *   (Misc.pp_list_h ~sep:"," (fun fmt (i,v) -> Format.fprintf fmt "%a=%d" Ident.pp i v)) type_indexes; *)
    let env' = List.fold_left GuestTyping.add_index env type_indexes in
    let env'' = List.fold_left GuestTyping.add_var env' (types_of_fsm_model env' m) in
    type_fsm_ios env'' m;
    List.iter (type_fsm_transition env'' m) m.A.desc.trans;
    type_fsm_itransition env'' m m.A.desc.itrans;
    m

  (* Typing FSM instances *)

  let type_fsm_inst env p A.{ desc=name,model,params,args; loc=loc; _ } =
    let open HostSyntax in
    let lookup_model name =
      try List.find (fun { A.desc = m; _ } -> m.name = name) p.models
      with Not_found -> raise (Undefined_symbol (loc, name)) in
    let lookup_io name =
      try List.find (fun { A.desc = (id,_,_,_); _ } -> id = name) p.globals
      with Not_found -> raise (Undefined_symbol (loc,name)) in
    let unify_cat cat cat' = match cat, cat' with
      (* Check that an Input (resp. Output) is not plugged on an Output (resp. Input) *)
      | Input, Output -> raise (Illegal_inst loc)
      | Output, Input -> raise (Illegal_inst loc)
      | _, _ -> () in
    (* Get associated model *)
    let mm = Misc.clone @@ lookup_model model in
      (* Note: we need a _deep copy_ of the model so that destructive updates performed by the
         type-checking are applied to fresh copies *)
    let m = mm.A.desc in
    (* Type-check and (statically) evaluate parameters *)
    let bind_param (id,te) e =
      let ty = GuestTyping.type_of_type_expr env te in
      GuestTyping.type_check
        ~loc:loc
        ty
        (GuestTyping.type_expression env e);
      (id,ty,e) in
    let i_params =
      try List.map2 bind_param m.params params
      with Invalid_argument _ -> raise (Illegal_inst loc) in
    (* Now type the instanciated model *)
    let _ = type_fsm_model (env,i_params) mm in
    let m_inps = List.map (fun (id,te) -> id, Input, te.A.typ) m.inps in
    let m_outps = List.map (fun (id,te) -> id, Output, te.A.typ) m.outps in
    let m_inouts = List.map (fun (id,te) -> id, Shared, te.A.typ) m.inouts in
    let bind_arg (id,cat,ty) id' =
      let _,cat',te',_ = (lookup_io id').A.desc in
      unify_cat cat cat';
      GuestTyping.type_check ~loc ty te'.A.typ;
      (id',ty) in
    let _ = 
      try List.map2 bind_arg (m_inps @ m_outps @ m_inouts) args;
      with Invalid_argument _ -> raise (Illegal_inst loc) in
    (name, mm)
    (* { name = name;
     *   params = i_params;
     *   args = i_args;
     *   model = m } *)

  (* Typing globals *)
                             
  let type_stimulus env id ty st =
    let check t = GuestTyping.type_check ~loc:st.A.loc ty t in 
    st.A.typ <- ty;
    match st.A.desc with
    | HostSyntax.Periodic _ -> check (GuestTyping.Types.mk_type_constr0 "event")
    | HostSyntax.Sporadic _ -> check (GuestTyping.Types.mk_type_constr0 "event")
    | HostSyntax.Value_change vcs ->
       List.iter
         check
         (List.map (function (_,e) -> GuestTyping.type_expression env e) vcs)

  let type_global env  ({ A.desc = id,cat,te,st; _ } as gl) = 
    let ty = GuestTyping.type_of_type_expr env te in
    let _ = match st with 
      | Some st -> type_stimulus env id ty st
      | None -> () in
    gl.A.typ <- ty

  (* Typing function declarations *)

  let type_fun_decl env (A.{ desc = fd; loc = loc; _ } as f) = 
    let open HostSyntax in
    let ty_args =
      List.map
        (function (id,te) -> id, GuestTyping.type_of_type_expr env te)
        fd.ff_args in
    let env' = List.fold_left GuestTyping.add_var env ty_args in
    let ty_body = GuestTyping.type_expression env' fd.ff_body  in
    let ty_result = GuestTyping.type_of_type_expr env fd.ff_res in
    GuestTyping.type_check ~loc:loc ty_body ty_result;
    let ty = GuestTyping.Types.mk_type_fun (List.map snd ty_args) ty_result in
    f.A.typ <- ty;
    GuestTyping.add_var env (fd.ff_name, ty)

  (* Typing constant declarations *)

  let type_cst_decl env (A.{ desc = cd; loc = loc; _ } as c) = 
    let open HostSyntax in
    let ty = GuestTyping.type_of_type_expr env cd.cc_typ in
    let ty' = GuestTyping.type_expression env cd.cc_val in
    GuestTyping.type_check ~loc:loc ty ty';
    c.A.typ <- ty;
    GuestTyping.add_var env (cd.cc_name, ty)

  let pp_env fmt env = GuestTyping.pp_env fmt env

  (* Typing programs *)

  let type_program env0 p = 
    let open HostSyntax in
    let env1 = List.fold_left GuestTyping.type_type_decl env0 p.type_decls in
    let env2 = List.fold_left type_fun_decl env1 p.fun_decls in
    let env = List.fold_left type_cst_decl env2 p.cst_decls in
    (* List.iter (type_fsm_model env) p.models; *)
    (* Note v2-full4: we cannot check the models alone in the presence of type parameters.
       For ex:
         [fsm model f (n:int) (in i:int<n>, ...) vars z:int<8> ... trans: ... with z:=i ...]
       can only be checked when the model [f] is instanciated (as [fsm f1 = f<8>(...)] for example)
       because of the assignation [z:=i] ! *)
    List.iter (type_global env) p.globals;
    { tp_models = List.map (type_fsm_model (env,[])) p.models;
      tp_insts =  List.map (type_fsm_inst env p) p.insts }
    (* { type_decls = p.type_decls; 
     *   fun_decls = p.fun_decls;
     *   cst_decls = p.cst_decls;
     *   globals = p.globals;
     *   insts = List.map (type_fsm_inst env p) p.insts } *)

end
