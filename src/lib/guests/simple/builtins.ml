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

let type_arithm2 () = 
  { ts_params = [];
    ts_body =
      type_arrow
        (type_product [type_int (); type_int ()])
        (type_int ()) }

let type_arithm1 () = 
  { ts_params = [];
    ts_body =
      type_arrow
        (type_int ())
        (type_int ()) }

let type_compar () = 
  let tv = Types.make_var () in
  { ts_params = [tv];
    ts_body =
      type_arrow
        (type_product [TyVar tv; TyVar tv])
        (type_bool ()) }

let type_farithm2 () = 
  { ts_params=[];
    ts_body=
      type_arrow
        (type_product [type_float (); type_float ()])
        (type_float ()) }

let type_farithm1 () = 
  { ts_params=[];
    ts_body= type_arrow (type_float ()) (type_float ()) }

exception Unknown_value
        
let encode_int n =
    Value.Val_int n
let decode_int = function
  | Value.Val_int n -> n
  | Val_unknown -> raise Unknown_value
  | _ -> Rfsm.Misc.fatal_error "Builtins.decode_int" (* Should not occur after TC *)
let encode_bool b =
    Value.Val_bool b
let decode_bool = function
  | Value.Val_bool b -> b
  | Value.Val_unknown -> raise Unknown_value
  | _ -> Rfsm.Misc.fatal_error "Builtins.decode_bool" (* Should not occur after TC *)
let encode_float n =
    Value.Val_float n
let decode_float = function
  | Value.Val_float n -> n
  | Value.Val_unknown -> raise Unknown_value
  | _ -> Rfsm.Misc.fatal_error "Builtins.decode_float" (* Should not occur after TC *)

let prim2 encode op decode =
  function
   | [v1;v2] ->
      begin
        try encode (op (decode v1) (decode v2))
        with Unknown_value -> Value.Val_unknown
      end
   | _ -> Rfsm.Misc.fatal_error "Builtins.prim2"

let prim1 encode op decode =
  function
   | [v] ->
      begin
        try encode (op (decode v))
        with Unknown_value -> Value.Val_unknown
      end
   | _ -> Rfsm.Misc.fatal_error "Builtins.prim1"

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
    "+", (type_arithm2 (), prim2 encode_int  ( + ) decode_int);
    "-", (type_arithm2 (), prim2 encode_int  ( - ) decode_int);
    "*", (type_arithm2 (), prim2 encode_int  ( * ) decode_int);
    "/", (type_arithm2 (), prim2 encode_int  ( / ) decode_int);
    "~-", (type_arithm1 (), prim1 encode_int  ( ~- ) decode_int);
    "%", (type_arithm2 (), prim2 encode_int  ( mod ) decode_int);
    ">>", (type_arithm2 (), prim2 encode_int  ( lsr ) decode_int);
    "<<", (type_arithm2 (), prim2 encode_int  ( lsl ) decode_int);
    "&", (type_arithm2 (), prim2 encode_int  ( land ) decode_int);
    "|", (type_arithm2 (), prim2 encode_int  ( lor ) decode_int);
    "^", (type_arithm2 (), prim2 encode_int  ( lxor ) decode_int);
    "+.", (type_farithm2 (), prim2 encode_float  ( +. ) decode_float);
    "-.", (type_farithm2 (), prim2 encode_float  ( -. ) decode_float);
    "*.", (type_farithm2 (), prim2 encode_float  ( *. ) decode_float);
    "/.", (type_farithm2 (), prim2 encode_float  ( /. ) decode_float);
    "~-.", (type_farithm1 (), prim1 encode_float  ( ~-. ) decode_float);
    "=", (type_compar () , tprim2 ( = ));
    "!=", (type_compar (), tprim2 ( <> ));
    "<", (type_compar (), tprim2 ( < ));
    ">", (type_compar (), tprim2 ( > ));
    "<=", (type_compar (), tprim2 ( <= ));
    ">=", (type_compar (), tprim2 ( >= ))
]

type typing_env = {
    tycons: (Rfsm.Ident.t * (int * Types.typ)) list; (* name, arity *)
    ctors: (Rfsm.Ident.t * Types.typ) list; (* name, target type *)
    prims: (Rfsm.Ident.t * Types.typ_scheme) list; (* name, type scheme *)
  }

let mk_ident s = Rfsm.Ident.(mk ~scope:Global s)

let typing_env =
  { tycons = [
      mk_ident "event", (0, Types.type_event ());
      mk_ident "int", (0, Types.type_int ());
      mk_ident "bool", (0, Types.type_bool ());
      mk_ident "float", (0, Types.type_float ());
      mk_ident "char", (0, Types.type_char ());
      (* Note: the type ctor "array" is handled specifically in this version *)
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
