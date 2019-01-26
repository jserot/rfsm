open Rfsm
open Expr
open Types
open Action

let mk_binop (op,e1,e2) = mk_expr (EBinop(op, e1, e2))
let mk_asn (v,e) = Assign (mk_lhs v, e)
let mk_int n = mk_expr (EInt n)
  
let chrono = Fsm.build_model
  ~name:"chrono"
  ~states:["Stopped"; "Running"]
  ~params:[]
  ~ios:[
    IO_In, "h", TyEvent;
    IO_In, "startstop", TyEvent;
    IO_Out, "aff", type_int []
    ]
  ~vars:["ctr", type_int []]
  ~trans:[
    ("Stopped",
     ("startstop",[]),
     [mk_asn ("ctr", mk_int 0); mk_asn ("aff", mk_int 0)],
     "Running", 0);
    ("Running",
     ("h",[]),
     [mk_asn ("ctr", mk_binop("+", mk_var "ctr", mk_int 1)); mk_asn ("aff", mk_var "ctr")],
     "Running", 0);
    ("Running", ("startstop",[]), [], "Stopped", 0)
    ]
  ~itrans:("Stopped",[])

let h = Global.GInp ("H", TyEvent, Periodic (10,10,110))
let startstop = Global.GInp ("StartStop", TyEvent, Sporadic [25; 75])
let aff = Global.GOutp ("Aff", type_int [])

let c1 = Fsm.build_instance ~name:"c1" ~model:chrono ~params:[] ~ios:[h;startstop;aff]

let _ = Fsm.dot_output "./dot" c1

let s = Sysm.build ~name:"chrono" [c1]

let _ = Sysm.dot_output ~with_insts:true ~with_models:true "./dot" s

(* let c, rs = Simul.run s
 * let _ = List.iter Simul.dump_reaction rs *)

