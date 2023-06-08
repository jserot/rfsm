open Types

type typ_scheme = Types.typ_scheme
type value = Value.value
                
let type_arithm () = 
  { ts_params=[]; ts_body=type_arrow (type_pair (ty_int ()) (ty_int ())) (ty_int ()) }

let type_compar () = 
  let tv = mk_type_var () in
  { ts_params = [tv]; ts_body=type_arrow (type_pair (TyVar tv) (TyVar tv)) (ty_bool ()) }

exception Unknown_value
        
let encode_int n =
    Value.Val_int n
let decode_int = function
  | Value.Val_int n -> n
  | Value.Val_unknown -> raise Unknown_value
  | _ -> Msic.Misc.fatal_error "Builtins.encode_int" (* Should not occur after TC *)
let encode_bool b =
    Value.Val_bool b
let decode_bool = function
  | Value.Val_bool b -> b
  | Value.Val_unknown -> raise Unknown_value
  | _ -> Msic.Misc.fatal_error "Builtins.decode bool" (* should not happen *)

let prim2 encode op decode =
  function
   | [v1;v2] ->
      begin
        try encode (op (decode v1) (decode v2))
        with Unknown_value -> Value.Val_unknown
      end
   | _ -> Msic.Misc.fatal_error "Builtins.prim2"

let tprim2 op =
  let decode v = v  in
  function
  | [v1;v2] ->
      begin
        try encode_bool (op (decode v1) (decode v2))
        with Unknown_value -> Val_unknown
      end
   | _ -> Msic.Misc.fatal_error "Builtins.tprim2"

type prim = Value.value list -> Value.value
          
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
    tycons: (string * int) list; (* name, arity *)
    ctors: (string * Types.typ) list; (* name, target type *)
    prims: (string * Types.typ_scheme) list; (* name, type scheme *)
  }

let typing_env =
  { tycons = [
      "int", 0;
      "bool", 0;
      "array", 1;
      ];
    ctors = [
      "true", Types.ty_bool ();
      "false", Types.ty_bool ();
      ];
    prims =
      List.map (fun (id, desc) -> id, fst desc) env
  }

let eval_env = List.map (fun (id, desc) -> id, snd desc) env

let lookup id env =
  try List.assoc id env
  with Not_found -> Msic.Misc.fatal_error "Builtins.lookup" (* Should not occur after TC *)
