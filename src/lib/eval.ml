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

type env = (string * Expr.value) list

open Expr

exception Unknown_id of string
exception Illegal_expr of Expr.t
exception Illegal_application of Expr.t
exception Illegal_array_access of Expr.t
exception Illegal_bit_range_access of Expr.t
exception Invalid_array_access of string * int (* array name, index value *)
exception Illegal_record_access of Expr.t
exception Non_static_expr of Expr.t * Expr.t

let lookup env id = 
  try List.assoc id env 
  with Not_found -> raise (Unknown_id id)

let string_of_env env = Utils.ListExt.to_string (function (id,v) -> id ^ "=" ^ Expr.string_of_opt_value v) "," env
                      
let rec subst vs expr = match expr.e_desc with
  (* Substitute, in [expr], each occurence of a variable listed in [vs] by its value *)
  | EVar v when List.mem_assoc v vs -> { expr with e_desc = Expr.of_value (List.assoc v vs) }
  | EBinop (op,e1,e2) ->
     begin match Builtins.lookup_val op, subst vs e1, subst vs e2 with
       f, {e_desc=EInt c1}, { e_desc=EInt c2} ->   (* Immediate reduction *)
        begin match f [Val_int c1;Val_int c2] with
          Val_int v -> { expr with e_desc = EInt v }
        | Val_bool v -> { expr with e_desc = EBool v }
        | _ -> raise (Illegal_expr expr)
        end
     | f, {e_desc=EFloat c1}, {e_desc=EFloat c2} ->   (* Immediate reduction *)
        begin match f [Val_float c1;Val_float c2] with
          Val_float v -> { expr with e_desc = EFloat v }
        | Val_bool v -> { expr with e_desc = EBool v }
        | _ -> raise (Illegal_expr expr)
        end
     | _, e1', e2' -> { expr with e_desc = EBinop (op, e1', e2')  }
     end
  | ECond (e1,e2,e3) -> { expr with e_desc = ECond (subst vs e1, subst vs e2, subst vs e3) }
  | EFapp (f, es) -> { expr with e_desc = EFapp (f, List.map (subst vs) es) }
  | EArr (a,idx) ->
     let eval_index idx =
       begin
         try eval [] idx (* No env here; dynamic indexes are not allowed in (statically) substituted exprs *)
         with Unknown_id id -> raise (Non_static_expr (expr,idx))
       end in
     if List.mem_assoc a vs then
       begin
         match List.assoc a vs, eval_index idx with
         | { v_desc=Val_array vs}, {v_desc=Val_int i} when i >= 0 && i < Array.length vs ->
            { expr with e_desc = Expr.of_value (vs.(i)) }
         | _, _ ->
            { expr with e_desc = EArr (a, subst vs idx) }
       end
     else
       { expr with e_desc = EArr (a, subst vs idx) }
  | _ -> expr
               
and eval env exp = 
  let mk v = { v_desc=v; v_typ=exp.e_typ } in
  let r = match exp.e_desc with
    EInt v -> mk (Val_int v)
  | EFloat v -> mk (Val_float v)
  | EChar v -> mk (Val_char v)
  | EBool v -> mk (Val_bool v)
  | EEnum c -> mk (Val_enum c)
  | EVar id -> lookup env id
  | EBinop (op, exp1, exp2) ->
     let f = Builtins.lookup_val op in
     mk (f [(eval env exp1).v_desc; (eval env exp2).v_desc])
  | ECond (e1, e2, e3) ->
     begin match eval env e1 with
       { v_desc=Val_bool true } -> eval env e2
     | { v_desc=Val_bool false } -> eval env e3
     | _ -> raise (Illegal_expr exp)
     end
   | EFapp (f, exps) ->
     begin match lookup env f with
     | { v_desc=Val_fn (args, body) } -> 
        let env' = List.map2 (fun arg exp -> arg, eval env exp) args exps in
        eval (env'@env) body
     | _ -> raise (Illegal_application exp)
     end
   | EArrExt exps ->
      mk (Val_array (Array.of_list (List.map (eval env) exps)))
   | EArr (a,idx) ->
     begin
       match lookup env a, eval env idx with
       | { v_desc=Val_array vs }, { v_desc=Val_int i } -> 
          if i >= 0 && i < Array.length vs then vs.(i)
          else raise (Invalid_array_access (a,i))
       | _ -> raise (Illegal_array_access exp)
     end
   | EBit (a,idx) ->
     begin
       match lookup env a, eval env idx with
       | { v_desc=Val_int x }, { v_desc=Val_int i } -> 
          mk (Val_int (Intbits.get_bits i i x))
       | _ -> raise (Illegal_bit_range_access exp)
     end
  | EBitrange (a,idx1,idx2) ->
     begin
       match lookup env a, eval env idx1, eval env idx2 with
       | { v_desc=Val_int x}, { v_desc=Val_int hi }, { v_desc=Val_int lo } -> 
          mk (Val_int (Intbits.get_bits hi lo x))
       | _ -> raise (Illegal_bit_range_access exp)
     end
  | ERecord (a,f) ->
     begin
       match lookup env a with
       | { v_desc=Val_record vs } -> 
          begin
            try List.assoc f vs
            with Not_found -> raise (Illegal_record_access exp)
          end
       | _ -> raise (Illegal_record_access exp)
     end
  | ECast (e,te) ->
     let v = eval env e in
     eval_cast te.te_typ v
  in
  (* Printf.printf "Eval.eval [%s] (%s) -> %s\n" (string_of_env env) (Expr.to_string exp) (string_of_value r); *)
  r

and eval_cast ty v =
  let mk v = { v_desc=v; v_typ=ty } in
  match v.v_desc, ty with
  | Val_int _, Types.TyInt _ -> v
  | Val_int x, Types.TyBool -> mk (Val_bool (x <> 0))
  | Val_int x, Types.TyFloat -> mk (Val_float (float_of_int x))
  | Val_int x, Types.TyChar -> mk (Val_char (char_of_int x))
  | Val_bool _, Types.TyBool -> v
  | Val_bool b, Types.TyInt _ -> mk (Val_int (if b then 1 else 0))
  | Val_float _, Types.TyFloat -> v
  | Val_float x, Types.TyInt _ -> mk (Val_int (int_of_float x))
  | Val_char x, Types.TyInt _ -> mk (Val_int (int_of_char x))
  | _, _ -> Misc.fatal_error "Eval.eval_cast" (* should not happen *)
