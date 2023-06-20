open Types

type typ_scheme = Types.typ_scheme

let type_arithm2 () = 
  let sz = Types.make_var () in
  { ts_tparams = [];
    ts_sparams = [sz];
    ts_body =
      type_arrow
        (type_product [type_int (SzVar sz); type_int (SzVar sz)])
        (type_int (SzVar sz)) }

let type_arithm1 () = 
  let sz = Types.make_var () in
  { ts_tparams = [];
    ts_sparams = [sz];
    ts_body =
      type_arrow
        (type_int (SzVar sz))
        (type_int (SzVar sz)) }

let type_compar () = 
  let tv = Types.make_var () in
  { ts_tparams = [tv];
    ts_sparams = [];
    ts_body =
      type_arrow
        (type_product [TyVar tv; TyVar tv])
        (type_bool ()) }

let type_farithm2 () = 
  { ts_tparams=[];
    ts_sparams=[];
    ts_body=
      type_arrow
        (type_product [type_float (); type_float ()])
        (type_float ()) }

let type_farithm1 () = 
  { ts_tparams=[];
    ts_sparams=[];
    ts_body= type_arrow (type_float ()) (type_float ()) }

exception Unknown_value
        
let encode_int n =
    Value.Val_int n
let decode_int = function
  | Value.Val_int n -> n
  | Val_unknown -> raise Unknown_value
  | _ -> Rfsm.Misc.fatal_error "Full.Builtins.decode_int" (* Should not occur after TC *)
let encode_bool b =
    Value.Val_bool b
let decode_bool = function
  | Value.Val_bool b -> b
  | Value.Val_unknown -> raise Unknown_value
  | _ -> Rfsm.Misc.fatal_error "Full.Builtins.decode_bool" (* Should not occur after TC *)
(* let encode_float n =
 *     Value.Val_float n
 * let decode_float = function
 *   | Value.Val_float n -> n
 *   | Value.Val_unknown -> raise Unknown_value
 *   | _ -> Rfsm.Misc.fatal_error "Full.Builtins.decode_float" (\* Should not occur after TC *\) *)

let prim2 encode op decode =
  function
   | [v1;v2] ->
      begin
        try encode (op (decode v1) (decode v2))
        with Unknown_value -> Value.Val_unknown
      end
   | _ -> Rfsm.Misc.fatal_error "Full.Builtins.prim2"

let prim1 encode op decode =
  function
   | [v] ->
      begin
        try encode (op (decode v))
        with Unknown_value -> Value.Val_unknown
      end
   | _ -> Rfsm.Misc.fatal_error "Full.Builtins.prim1"

let tprim2 op =
  let decode v = v  in
  function
  | [v1;v2] ->
      begin
        try encode_bool (op (decode v1) (decode v2))
        with Unknown_value -> Val_unknown
      end
   | _ -> Rfsm.Misc.fatal_error "Full.Builtins.tprim2"

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
    (* "+.", (type_farithm2 (), prim2 encode_float  ( +. ) decode_float);
     * "-.", (type_farithm2 (), prim2 encode_float  ( -. ) decode_float);
     * "*.", (type_farithm2 (), prim2 encode_float  ( *. ) decode_float);
     * "/.", (type_farithm2 (), prim2 encode_float  ( /. ) decode_float);
     * "~-.", (type_farithm1 (), prim1 encode_float  ( ~-. ) decode_float); *)
    "=", (type_compar () , tprim2 ( = ));
    "!=", (type_compar (), tprim2 ( <> ));
    "<", (type_compar (), tprim2 ( < ));
    ">", (type_compar (), tprim2 ( > ));
    "<=", (type_compar (), tprim2 ( <= ));
    ">=", (type_compar (), tprim2 ( >= ))
]

type typing_env = {
    tycons: (string * int) list; (* name, arity *)
    ctors: (string * Types.typ) list; (* name, target type *)
    prims: (string * Types.typ_scheme) list; (* name, type scheme *)
  }

let typing_env =
  { tycons = [
      "int", 0;
      "bool", 0;
      "float", 0;
      "array", 1;
      ];
    ctors = [
      "true", Types.type_bool ();
      "false", Types.type_bool ();
      ];
    prims =
      List.map (fun (id, desc) -> id, fst desc) env
  }

let eval_env = List.map (fun (id, desc) -> id, snd desc) env

let lookup id env =
  try List.assoc id env
  with Not_found -> Rfsm.Misc.fatal_error "Full.Builtins.lookup" (* Should not occur after TC *)

(* let lookup_val id = snd (lookup id)
 * let lookup_typ id = fst (lookup id) *)
