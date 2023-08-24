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

type typ_scheme = Types.typ_scheme
                
let type_arithm () = 
  { ts_params=[]; ts_body=type_arrow (type_pair (type_int ()) (type_int ())) (type_int ()) }

let type_compar () = 
  let tv = mk_type_var () in
  { ts_params = [tv]; ts_body=type_arrow (type_pair (TyVar tv) (TyVar tv)) (type_bool ()) }

exception Unknown_value
        
let encode_int n =
    Value.Val_int n
let decode_int = function
  | Value.Val_int n -> n
  | Value.Val_unknown -> raise Unknown_value
  | _ -> Rfsm.Misc.fatal_error "Builtins.decode_int" (* Should not occur after TC *)

let encode_bool n =
    Value.Val_bool n
let decode_bool = function
  | Value.Val_bool n -> n
  | Value.Val_unknown -> raise Unknown_value
  | _ -> Rfsm.Misc.fatal_error "Builtins.decode_bool" (* Should not occur after TC *)

let prim2 encode op decode =
  function
   | [v1;v2] ->
      begin
        try encode (op (decode v1) (decode v2))
        with Unknown_value -> Value.Val_unknown
      end
   | _ -> Rfsm.Misc.fatal_error "Builtins.prim2"

let tprim2 op =
  let decode v = v  in
  function
  | [v1;v2] ->
      begin
        try encode_bool (op (decode v1) (decode v2))
        with Unknown_value -> Val_unknown
      end
   | _ -> Rfsm.Misc.fatal_error "Builtins.tprim2"

type prim = Value.t list -> Value.t
          
type desc = Types.typ_scheme * prim  (** type, value *)

type env = (string * desc) list

let env = [
    "+", (type_arithm (), prim2 encode_int  ( + ) decode_int);
    "-", (type_arithm (), prim2 encode_int  ( - ) decode_int);
    "*", (type_arithm (), prim2 encode_int  ( * ) decode_int);
    "/", (type_arithm (), prim2 encode_int  ( / ) decode_int);
    "=", (type_compar () , tprim2 ( = ));
    "!=", (type_compar (), tprim2 ( <> ));
    "<", (type_compar (), tprim2 ( < ));
    ">", (type_compar (), tprim2 ( > ));
]

type typing_env = {
    tycons: (Rfsm.Ident.t * int) list; (* name, arity (always 0 here) *)
    ctors: (Rfsm.Ident.t * Types.typ) list; (* name, target type *)
    prims: (Rfsm.Ident.t * Types.typ_scheme) list; (* name, type scheme *)
  }

let mk_ident s = Rfsm.Ident.(mk ~scope:Global s)

let typing_env =
  { tycons = [
      mk_ident "int", 0;
      mk_ident "bool", 0;
      mk_ident "event", 0;
      ];
    ctors = [
      mk_ident "true", Types.type_bool ();
      mk_ident "false", Types.type_bool ();
      ];
    prims =
      List.map (fun (id, desc) -> mk_ident id, fst desc) env
  }
let eval_env = List.map (fun (id, desc) -> mk_ident id, snd desc) env

let lookup id env =
  try List.assoc id env
  with Not_found -> Rfsm.Misc.fatal_error "Builtins.lookup" (* Should not occur after TC *)
