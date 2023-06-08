module Syntax = Syntax
module Value = Value

module Annot = Rfsm.Annot
module Env = Rfsm.Env
module Location = Rfsm.Location

open Value

type env = value Env.t

exception Uninitialized of Location.t
exception Out_of_bound of Location.t * int

let mk_env () = Env.empty 

let lookup ~loc v env = 
  match Rfsm.Env.find v env with
  | Val_unknown -> raise (Uninitialized loc)
  | v -> v
  | exception Not_found -> Rfsm.Misc.fatal_error "Guest.Eval.lookup" (* Should not occur after TC *)

let rec eval_expr env e = match e.Annot.desc with
  | Syntax.EVar v -> lookup ~loc:e.Annot.loc v env
  | Syntax.EInt i -> Val_int i 
  | Syntax.EBool i -> Val_bool i 
  | Syntax.EBinop (op,e1,e2) -> 
     let f = Builtins.lookup op Builtins.eval_env in
     f [eval_arg env e1; eval_arg env e2]
  | Syntax.EArr (a,idx) ->
     begin match lookup ~loc:e.Annot.loc a env with
     | Val_array vs ->
        let i = eval_array_index a (Array.length vs) env idx in
        vs.(i)
     | _ -> Rfsm.Misc.fatal_error "Guest.Eval.eval_expr" (* Should not occur after TC *)
     end
  | Syntax.ECon0 c ->  Val_enum c

and eval_arg env e = match eval_expr env e with
    | Val_unknown -> raise (Uninitialized e.Annot.loc)
    | v -> v

and eval_array_index a sz env idx = 
  match eval_arg env idx with
  | Val_int i ->
     if i >= 0 && i < sz then i
     else raise (Out_of_bound (idx.Annot.loc, i))
  | _ -> raise (Rfsm.Misc.fatal_error "Guest.Eval.eval_array_index") (* Should not occur after TC *)

let upd_env lhs v env = 
  match lhs.Annot.desc with
  | Syntax.LhsVar x -> Env.upd x v env
  | Syntax.LhsArrInd (x,idx) ->
        begin match lookup ~loc:lhs.Annot.loc x env with
        | Val_array vs ->
           let i = eval_array_index x (Array.length vs) env idx in
           vs.(i) <- v;
           env (* In-place update *)
        | _ -> Rfsm.Misc.fatal_error "Guest.Eval.upd_env"
        end

let eval_bool env e = 
  match eval_expr env e with
  | Val_bool b -> b
  | _ -> Rfsm.Misc.fatal_error "Guest.eval_bool" (* Should not occur after TC *)

let pp_env fmt env = 
  Format.fprintf fmt "%a\n" (Env.pp ~sep:"=" Value.pp_value) env
