open Rfsm
open Expr
open Types
open Action
  
let t_bit = type_int [0;1]
           
(* A few syntax helpers.. *)

let mk_binop (op,e1,e2) = mk_expr (EBinop(op, e1, e2))
let mk_asn (v,e) = Assign (mk_lhs v, e)
let mk_int n = mk_expr (EInt n)

let gensig = Fsm.build_model
  ~name:"gensig"
  ~states:["E0"; "E1"]
  ~params:["n",type_int []]
  ~ios:[
    IO_In, "h",TyEvent;
    IO_In, "e",t_bit;
    IO_Out, "s", t_bit
    ]
  ~vars:["k", TyInt (SzExpr2 (TiConst 0, TiVar "n"))] 
  ~trans:[
    ("E0", ("h",[mk_binop("=", mk_var "e", mk_int 1)]), [mk_asn("k", mk_int 1); mk_asn("s", mk_int 1)], "E1", 0);
    ("E1", ("h",[mk_binop("<", mk_var "k", mk_var "n")]), [mk_asn("k", mk_binop ("+", mk_var "k", mk_int 1))], "E1", 0);
    ("E1", ("h",[mk_binop("=", mk_var "k", mk_var "n")]), [mk_asn("s", mk_int 0)], "E0", 0)
    ]
  ~itrans:("E0",[mk_asn("s", mk_int 0)])

let h = Global.GInp ("H", TyEvent, Periodic (10,0,80))
let e = Global.GInp ("E", t_bit, ValueChange [0,Expr.mk_int 0; 25,Expr.mk_int 1; 35,Expr.mk_int 0])
let s = Global.GOutp ("S", t_bit)

let g = Fsm.build_instance ~name:"g" ~model:gensig ~params:["n",Expr.mk_int 4] ~ios:[h;e;s]

(* let _ = Fsm.dump_inst stdout g *)
      
let m = Static.build ~name:"gensig" [gensig] [g]

let _ = Sys.command "mkdir -p dot" 
let _ = Static.dot_output "./dot" m

(* let c, rs = Simul.run p
 * let _ = List.iter Simul.dump_reaction rs *)
