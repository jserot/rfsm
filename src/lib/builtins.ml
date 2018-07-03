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

open Types
open Expr

let type_arithm2 () = 
  { ts_params=[]; ts_body=TyArrow (TyProduct [TyInt None; TyInt None], TyInt None) }

let type_compar () = 
  let tv = Types.mk_type_var () in
  { ts_params = [tv]; ts_body=TyArrow (TyProduct [TyVar tv; TyVar tv], TyBool) }

let type_farithm2 () = 
  { ts_params=[]; ts_body=TyArrow (TyProduct [TyFloat; TyFloat], TyFloat) }

exception Internal_error of string

let fatal_error msg = raise (Internal_error msg)
                
let encode_int n =
    Val_int n
let rec decode_int = function
  | Val_int n -> n
  | _ -> fatal_error "Builtins.decode_int" (* should not happen *)
let encode_bool b =
    Val_bool b
let rec decode_bool = function
  | Val_bool b -> b
  | _ -> fatal_error "Builtins.decode bool" (* should not happen *)
let encode_float n =
    Val_float n
let rec decode_float = function
  | Val_float n -> n
  | _ -> fatal_error "Builtins.decode_float" (* should not happen *)

let prim2 encode op decode =
  function
   | [v1;v2] ->
       encode (op (decode v1) (decode v2))
   | _ -> fatal_error "Builtins.prim2"

let tprim2 op =
  let decode v = v  in
  function
   | [v1;v2] -> encode_bool (op (decode v1) (decode v2))
   | _ -> fatal_error "Builtins.tprim2"

type prim = Expr.e_val list -> Expr.e_val
          
type desc = Types.typ_scheme * prim  (** type, value *)

type env = (string * desc) list

let env = [
    "+", (type_arithm2 (), prim2 encode_int  ( + ) decode_int);
    "-", (type_arithm2 (), prim2 encode_int  ( - ) decode_int);
    "*", (type_arithm2 (), prim2 encode_int  ( * ) decode_int);
    "/", (type_arithm2 (), prim2 encode_int  ( / ) decode_int);
    "mod", (type_arithm2 (), prim2 encode_int  ( mod ) decode_int);
    "+.", (type_farithm2 (), prim2 encode_float  ( +. ) decode_float);
    "-.", (type_farithm2 (), prim2 encode_float  ( -. ) decode_float);
    "*.", (type_farithm2 (), prim2 encode_float  ( *. ) decode_float);
    "/.", (type_farithm2 (), prim2 encode_float  ( /. ) decode_float);
    "=", (type_compar () , tprim2 ( = ));
    "!=", (type_compar (), tprim2 ( <> ));
    "<", (type_compar (), tprim2 ( < ));
    ">", (type_compar (), tprim2 ( > ));
    "<=", (type_compar (), tprim2 ( <= ));
    ">=", (type_compar (), tprim2 ( >= ))
]

exception Unbound_id of string
        
let lookup id =
  try List.assoc id env
  with Not_found -> raise (Unbound_id id)

let lookup_val id = snd (lookup id)
let lookup_typ id = fst (lookup id)
