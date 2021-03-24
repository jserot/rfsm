open Rfsm
open Expr
open Types
open Action

let mk_binop (op,e1,e2) = mk_expr (EBinop(op, e1, e2))
let mk_asn (v,e) = Assign (mk_lhs v, e)
let mk_int n = mk_expr (EInt n)
let mk_float n = mk_expr (EFloat n)
let mk_bool b = mk_expr (EBool b)
let mk_fneg e = mk_expr (EFapp ("~-.", [e]))
  
let f_abs = 
  TyArrow (TyProduct [TyFloat], TyFloat),
  Static.MFun (["x"],
              mk_expr (ECond (mk_binop ("<", mk_var "x", mk_float 0.0), mk_fneg (mk_var "x"), mk_var "x")))

let heron = Fsm.build_model
  ~name:"Heron"
  ~states:["Idle", ["rdy", mk_bool true]; "Iter", ["rdy", mk_bool false]]
  ~params:["eps", TyFloat]
  ~ios:[
    IO_In, "h", TyEvent;
    IO_In, "start", TyBool;
    IO_In, "u", TyFloat;
    IO_Out, "rdy", TyBool;
    IO_Out, "niter", type_int [];
    IO_Out, "r", TyFloat
    ]
  ~vars:["a", TyFloat; "x", TyFloat; "n", type_int []]
  ~trans:[
    ("Idle",
     ("h",[mk_binop ("=", mk_var "start", mk_bool true)]),
     [mk_asn ("a", mk_var "u"); mk_asn ("x", mk_var "u"); mk_asn ("n", mk_int 0)],
     "Iter", 0);
    ("Iter",
     ("h",[mk_binop (">=", mk_expr (EFapp ("f_abs", [mk_binop ("-.", mk_binop("*.", mk_var "x", mk_var "x"), mk_var "a")])), mk_var "eps")]),
     [mk_asn ("x", mk_binop ("/.", mk_binop ("+.", mk_var "x", mk_binop("/.", mk_var "a", mk_var "x")), mk_float 2.0));
      mk_asn ("n", mk_binop ("+", mk_var "n", mk_int 1))],
     "Iter", 0);
    ("Iter",
     ("h",[mk_binop ("<", mk_expr (EFapp ("f_abs", [mk_binop ("-.", mk_binop("*.", mk_var "x", mk_var "x"), mk_var "a")])), mk_var "eps")]),
     [mk_asn ("r", mk_var "x"); mk_asn ("niter", mk_var "n")], 
     "Idle", 0);
    ]
  ~itrans:("Idle",[])

let h = Global.GInp ("H", TyEvent, Periodic (10,10,200))
let u = Global.GInp ("U", TyFloat, ValueChange [5, Expr.mk_float 2.0])
let start = Global.GInp ("Start", TyBool, ValueChange [0, Expr.mk_bool false; 25, Expr.mk_bool true; 35, Expr.mk_bool false])
let rdy = Global.GOutp ("Rdy", TyBool)
let r = Global.GOutp ("R", TyFloat)
let niter = Global.GOutp ("niter", type_int [])

let h1 = Fsm.build_instance
           ~name:"h1"
           ~model:heron
           ~params:["eps", Expr.mk_float 0.0000001]
           ~ios:[h; start; u; rdy; niter; r]

let s = Static.build ~name:"heron" ~gfns:["f_abs", f_abs] [heron] [h1]

let _ = Static.dump stdout s
let _ = Sys.command "mkdir -p dot" 
let _ = Static.dot_output "./dot" s

let c, rs = Simul.run s
let _ = List.iter Simul.dump_reaction rs

