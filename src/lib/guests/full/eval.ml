module Syntax = Syntax
module Value = Value

module Annot = Rfsm.Annot
module Env = Rfsm.Env
module Location = Rfsm.Location

open Value

type env = Value.t Env.t

let mk_env () = Env.empty 

exception Uninitialized of Location.t
exception Out_of_bound of Location.t * int
exception Illegal_application of Syntax.expr

let lookup ~loc v env = 
  match Rfsm.Env.find v env with
  | Val_unknown -> raise (Uninitialized loc)
  | v -> v
  | exception Not_found ->
     raise (Rfsm.Misc.Fatal_error "Full.Eval.lookup") (* Should not occur after TC *)

let rec eval_expr env e = match e.Annot.desc with
  | Syntax.EVar v -> lookup ~loc:e.Annot.loc v env
  | Syntax.EInt i -> Val_int i 
  | Syntax.EBool i -> Val_bool i 
  | Syntax.EFloat f -> Val_float f 
  | Syntax.EBinop (op,e1,e2) -> 
     let f = Builtins.lookup op Builtins.eval_env in
     f [eval_arg env e1; eval_arg env e2]
  | Syntax.ECon0 c ->  Val_enum c
  | Syntax.EArr (a,idx) ->
     begin match lookup ~loc:e.Annot.loc a env with
     | Val_array vs ->
        let i = eval_array_index a (Array.length vs) env idx in
        vs.(i)
     | _ -> Rfsm.Misc.fatal_error "Full.Eval.eval_expr: EArr" (* Should not occur after TC *)
     end
  | Syntax.EBit (a,idx) ->
     begin
       match lookup ~loc:e.Annot.loc a env, eval_expr env idx with
         | Val_int x, Val_int i -> Val_int (Rfsm.Bits.get_bits i i x)
         | _, _ -> Rfsm.Misc.fatal_error "Full.Eval.eval_expr: EBit" (* Should not occur after TC *)
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
     let t = begin match te.Annot.typ with
             | Some t -> t 
             | None -> Rfsm.Misc.fatal_error "Full.Eval.eval_expr: cast" end in
     eval_cast ~loc:e.Annot.loc t v
   | Syntax.EFapp (f, es) ->
       begin 
         match lookup ~loc:e.Annot.loc f env with
         | Val_fn (args, body) -> 
            let env' = List.map2 (fun arg e -> arg, eval_expr env e) args es |> Env.init in
            let r = eval_expr env' body in
            (* Format.printf "** Full.Eval.eval_expr {env'=%a}: %a -> %a\n" (Env.pp Value.pp) env (Env.pp Value.pp) env' Syntax.pp_expr body Value.pp r ;*)
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
         | _ -> Rfsm.Misc.fatal_error "Full.Eval.eval_expr: ERrecord"
       end
    | Syntax.ERecordExt fs ->
       Val_record (List.map (fun (n,e) -> n, eval_expr env e) fs)

and eval_arg env e = match eval_expr env e with
    | Val_unknown -> raise (Uninitialized e.Annot.loc)
    | v -> v

and eval_array_index a sz env idx = 
  match eval_arg env idx with
  | Val_int i ->
     if i >= 0 && i < sz then i
     else raise (Out_of_bound (idx.Annot.loc, i))
  | _ -> raise (Rfsm.Misc.fatal_error "Full.Eval.eval_array_index") (* Should not occur after TC *)

and eval_cast ~loc ty v =
  (* let mk v = Annot.{ desc=v; typ=Some ty; loc=loc } in *)
  let mk v = v in
  match v, ty with
  | Val_int _, Types.TyConstr ("int", _, _) -> v
  | Val_int x, Types.TyConstr ("bool", _, _) -> mk (Val_bool (x <> 0))
  | Val_int x, Types.TyConstr ("float", _, _) -> mk (Val_float (float_of_int x))
  (* | Val_int x, Types.TyConstr ("char", _, _) -> mk (Val_char (char_of_int x)) *)
  | Val_bool _, Types.TyConstr ("bool", _, _) -> v
  | Val_bool b, Types.TyConstr ("int", _, _) -> mk (Val_int (if b then 1 else 0))
  | Val_float _, Types.TyConstr ("float", _, _) -> v
  | Val_float x, Types.TyConstr ("int", _, _) -> mk (Val_int (int_of_float x))
  (* | Val_char x, Types.TyConstr ("int", _, _) -> mk (Val_int (int_of_char x)) *)
  | _, _ -> Rfsm.Misc.fatal_error "Eval.eval_cast" (* should not happen *)

let eval_bool env e = 
  match eval_expr env e with
  | Val_bool b -> b
  | _ -> Rfsm.Misc.fatal_error "Full.Eval.eval_bool" (* Should not occur after TC *)

let upd_env lhs v env = 
  match lhs.Annot.desc with
  | Syntax.LhsVar x -> Env.upd x v env
  | Syntax.LhsArrInd (x,idx) ->
        begin match lookup ~loc:lhs.Annot.loc x env with
        | Val_array vs ->
           let i = eval_array_index x (Array.length vs) env idx in
           vs.(i) <- v;
           env (* In-place update *)
        | _ -> Rfsm.Misc.fatal_error "Full.Eval.upd_env"
        end
  | Syntax.LhsRField (r,f) ->
     begin match lookup ~loc:lhs.Annot.loc r env with
     | Val_record fs ->
        Env.upd r (Val_record (Rfsm.Misc.replace_assoc f v fs)) env
     | _ -> Rfsm.Misc.fatal_error "Full.Eval.upd_env"
     end


let pp_env fmt env = 
  Format.fprintf fmt "%a\n" (Env.pp ~sep:"=" Value.pp) env
