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

module Syntax = Syntax
module Value = Value

module Annot = Rfsm.Annot
module Env = Rfsm.Env
module Location = Rfsm.Location

open Value

type env = Value.t Env.t

let mk_env () = Env.empty 

let pp_env fmt env = 
  Format.fprintf fmt "%a\n" (Env.pp ~sep:"=" Value.pp) env

exception Uninitialized of string * Location.t
exception Out_of_bound of Location.t * int * int * int
exception Illegal_application of Syntax.expr

let lookup ~loc x env = 
  match Rfsm.Env.find x env with
  | Val_unknown -> raise (Uninitialized (Rfsm.Ident.to_string x,loc))
  | v -> v
  | exception Not_found ->
     raise (Rfsm.Misc.Fatal_error "Full.Eval.lookup") (* Should not occur after TC *)

let rec eval_expr env e = match e.Annot.desc with
  | Syntax.EVar v -> lookup ~loc:e.Annot.loc v env
  | Syntax.EInt i -> Val_int (i,Types.size_of_type e.Annot.typ) 
  | Syntax.EBool i -> Val_bool i 
  | Syntax.EFloat f -> Val_float f 
  | Syntax.EChar c -> Val_char c 
  | Syntax.EBinop (op,e1,e2) -> 
     let f = Builtins.lookup op Builtins.eval_env in
     f [eval_arg env e1; eval_arg env e2]
  | Syntax.ECon0 c ->  Val_enum c.Rfsm.Ident.id
  | Syntax.EIndexed (a,idx) ->
     begin match lookup ~loc:e.Annot.loc a env with
     | Val_array vs ->
        let i = eval_expr_index a ~bounds:(0,Array.length vs-1) env idx in
        vs.(i)
     | Val_int (x,sz) -> 
        let bounds = eval_bounds sz in
        let i = eval_expr_index a ~bounds env idx in  
        Val_int (Rfsm.Bits.get_bits ~hi:i ~lo:i x, [1])
     | _ -> Rfsm.Misc.fatal_error "Full.Eval.eval_expr: EIndexed" (* Should not occur after TC *)
     end
  | Syntax.ERanged (a,idx1,idx2) ->
     begin
       match lookup ~loc:e.Annot.loc a env with
       | Val_int (x,sz) ->
          let bounds = eval_bounds sz in
          let hi = eval_expr_index a ~bounds env idx1 in
          let lo = eval_expr_index a ~bounds env idx2 in
          (* Val_int (Rfsm.Bits.get_bits ~hi ~lo x, [hi-lo+1]) *)
          Val_int (Rfsm.Bits.get_bits ~hi ~lo x, sz)
       | _ ->
          Rfsm.Misc.fatal_error "Full.Eval.eval_expr: ERanged" (* Should not occur after TC *)
     end
  | Syntax.EArrExt es -> Val_array (Array.of_list (List.map (eval_expr env) es))
  | Syntax.ECond (e1, e2, e3) ->
     begin match eval_expr env e1 with
       | Val_bool true -> eval_expr env e2
       | Val_bool false -> eval_expr env e3
       | _ -> Rfsm.Misc.fatal_error "Full.Eval.eval_expr.ECond"
     end
  | Syntax.ECast (e,te) ->
     let v = eval_expr env e in
     let t = te.Annot.typ in
     eval_cast ~loc:e.Annot.loc t v
   | Syntax.EFapp (f, es) ->
       begin 
         match lookup ~loc:e.Annot.loc f env with
         | Val_fn (args, body) -> 
            let env' = List.map2 (fun arg e -> Syntax.mk_ident arg, eval_expr env e) args es |> Env.init in
            let r = eval_expr env' body in
            r
         | _ -> raise (Illegal_application e)
       end
    | Syntax.ERecord (r,f) ->
       begin 
         match lookup ~loc:e.Annot.loc r env with
         | Val_record vs -> 
            begin
              try List.assoc f vs
              with Not_found -> Rfsm.Misc.fatal_error "Full.Eval.eval_expr: ERrecord"
            end
         | v ->
            Rfsm.Misc.fatal_error "Full.Eval.eval_expr: ERrecord"
       end
    | Syntax.ERecordExt fs ->
       Val_record (List.map (fun (n,e) -> n, eval_expr env e) fs)

and eval_arg env e = match eval_expr env e with
    | Val_unknown -> raise (Uninitialized (Rfsm.Misc.to_string Syntax.pp_expr e, e.Annot.loc))
    | v -> v

and eval_expr_index a ~bounds:(lo,hi) env idx = 
  match eval_arg env idx with
  | Val_int (i,_) ->
     if i >= lo && i <= hi then i
     else raise (Out_of_bound (idx.Annot.loc, i, lo, hi))
  | _ -> raise (Rfsm.Misc.fatal_error "Full.Eval.eval_array_index") (* Should not occur after TC *)

and eval_bounds sz = 
  match sz with 
  | [s] -> 0, s-1 
  | _ -> min_int, max_int

and eval_cast ~loc ty v =
  (* let mk v = Annot.{ desc=v; typ=Some ty; loc=loc } in *)
  let mk v = v in
  match v, ty with
  | Val_int (x,_), Types.TyConstr ("int", _, _) -> Val_int (x, Types.size_of_type ty)
  | Val_int (x,_), Types.TyConstr ("bool", _, _) -> mk (Val_bool (x <> 0))
  | Val_int (x,_), Types.TyConstr ("float", _, _) -> mk (Val_float (float_of_int x))
  (* | Val_int x, Types.TyConstr ("char", _, _) -> mk (Val_char (char_of_int x)) *)
  | Val_bool _, Types.TyConstr ("bool", _, _) -> v
  | Val_bool b, Types.TyConstr ("int", _, _) -> mk (Val_int ((if b then 1 else 0), Types.size_of_type ty))
  | Val_float _, Types.TyConstr ("float", _, _) -> v
  | Val_float x, Types.TyConstr ("int", _, _) -> mk (Val_int (int_of_float x, Types.size_of_type ty))
  (* | Val_char x, Types.TyConstr ("int", _, _) -> mk (Val_int (int_of_char x)) *)
  | _, _ -> Rfsm.Misc.fatal_error "Eval.eval_cast" (* should not happen *)

let eval_bool env e = 
  match eval_expr env e with
  | Val_bool b -> b
  | _ -> Rfsm.Misc.fatal_error "Full.Eval.eval_bool" (* Should not occur after TC *)

let upd_env lhs v env = 
  (* Note: bound checking (for arrays, ranged and sized integers) should take place here.
     For example, we should reject :
     - [a[i]:=v] if [i=10] and [a] has type [int array[10]] -> Ok
     - [r:=10] if [r] has type [int<0:9>]
     - [u[8:4]:=0] if [u] has type [int<8>] *)
  let env' = match lhs.Annot.desc with
  | Syntax.LhsVar x ->
     Env.upd x v env
  | Syntax.LhsIndex (x,idx) ->
     begin match lookup ~loc:lhs.Annot.loc x env, v with
     | Val_array vs, _ ->
        let i = eval_expr_index x ~bounds:(0,Array.length vs-1) env idx in
        vs.(i) <- v;
        env (* In-place update ! *)
     | Val_int (dst,sz), Val_int (v',_) ->
        let bounds = eval_bounds sz in
        let i = eval_expr_index x ~bounds env idx in
        Env.upd x (Val_int (Rfsm.Bits.set_bits ~hi:i ~lo:i ~dst v', sz)) env
     | _ -> Rfsm.Misc.fatal_error "Full.Eval.upd_env"
     end
  | Syntax.LhsRange (x,idx1,idx2) ->
     begin
       match lookup ~loc:lhs.Annot.loc x env, v with
       | Val_int (dst,sz), Val_int (v',_) ->
          let bounds = eval_bounds sz in
          let hi = eval_expr_index x ~bounds env idx1 in
          let lo = eval_expr_index x ~bounds env idx2 in
          Env.upd x (Val_int (Rfsm.Bits.set_bits ~hi ~lo ~dst v',sz)) env
       | _ -> Rfsm.Misc.fatal_error "Full.Eval.upd_env"
     end
  | Syntax.LhsRField (r,f) ->
     begin match lookup ~loc:lhs.Annot.loc r env with
     | Val_record fs ->
        Env.upd r (Val_record (Rfsm.Misc.replace_assoc f v fs)) env
     | _ -> Rfsm.Misc.fatal_error "Full.Eval.upd_env"
     end
    in
    env'
