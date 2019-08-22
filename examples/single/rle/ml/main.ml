open Rfsm
open Expr
open Types
open Action

let rle_t = TyRecord (NmLit "rle_t", ["val", TyChar; "cnt", type_int []])

let mk_binop (op,e1,e2) = mk_expr (EBinop(op, e1, e2))
let mk_asn (l,e) = Assign (l, e)
let mk_int n = mk_expr (EInt n)
let mk_rlhs (r,f) = { l_desc = LhsRField(r,f) }
let mk_rec (r,f) = mk_expr (ERecord (r,f))
  
let rle = Fsm.build_model
  ~name:"rle"
  ~states:["E0"; "E1"]
  ~params:[]
  ~ios:[
    IO_In, "h", TyEvent;
    IO_In, "e", TyChar;
    IO_Out, "s", rle_t
    ]
  ~vars:["r", rle_t]
  ~trans:[
    ("E0",
     ("h",[]),
     [mk_asn (mk_rlhs ("r","val"), mk_var "e"); mk_asn (mk_rlhs ("r", "cnt"), mk_int 1)],
     "E1", 0);
    ("E1",
     ("h",[mk_binop ("=", mk_var "e", mk_rec ("r", "val"))]),
     [mk_asn (mk_rlhs ("r","cnt"), mk_binop ("+", mk_rec ("r", "cnt"), mk_int 1))],
     "E1", 0);
    ("E1",
     ("h",[mk_binop ("!=", mk_var "e", mk_rec ("r", "val"))]),
     [mk_asn (mk_lhs "s", mk_var "r"); mk_asn (mk_rlhs ("r", "val"), mk_var "e"); mk_asn (mk_rlhs ("r","cnt"), mk_int 1)],
     "E1", 0)
    ]
  ~itrans:("E0",[])

let h = Global.GInp ("H", TyEvent, Periodic (10,10,100))
let inp = Global.GInp ("Inp", TyChar, ValueChange [5, mk_char 'A'; 15, mk_char 'A'; 25, mk_char 'A'; 35, mk_char 'B';
45, mk_char 'B'; 55, mk_char '+'; 65, mk_char 'C'; 75, mk_char 'C'; 85, mk_char 'C'; 95, mk_char 'z'])
let outp = Global.GOutp ("S", rle_t)

let r1 = Fsm.build_instance ~name:"r1" ~model:rle ~params:[] ~ios:[h;inp;outp]

let s = Static.build ~name:"rle" [rle] [r1]

let _ = Static.dump stdout s
let _ = Static.dot_output "./dot" s

(* let c, rs = Simul.run s
 * let _ = List.iter Simul.dump_reaction rs *)

