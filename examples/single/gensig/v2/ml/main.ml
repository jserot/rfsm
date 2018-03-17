open Rfsm
open Expr
open Types
open Action
  
let gensig = Fsm.build_model
  ~name:"gensig"
  ~states:["E0"; "E1"]
  ~params:["n",TyInt None]
  ~inps:["h",TyEvent; "e",TyBool]
  ~outps:["s", TyBool]
  ~vars:["k", TyInt (Some (TiConst 0, TiVar "n"))] 
  ~trans:[
    ("E0", ("h",[EVar "e", "=", EConst 1]), [Assign ("k",EConst 1); Assign ("s",EConst 1)], "E1");
    ("E1", ("h",[EVar "k", "<", EVar "n"]), [Assign ("k",EBinop ("+",EVar "k",EConst 1))], "E1");
    ("E1", ("h",[EVar "k", "=", EVar "n"]), [Assign ("s",EConst 0)], "E0");
    ]
  ~itrans:("E0",[Assign ("s", EConst 0)])

let h = Fsm.GInp ("H", TyEvent, Periodic (10,0,80))
let e = Fsm.GInp ("E", TyBool, ValueChange [0,Val_int 0; 25,Val_int 1; 35,Val_int 0])
let s = Fsm.GOutp ("S", TyBool)

let g = Fsm.build_instance ~name:"g" ~model:gensig ~params:["n",Val_int 4] ~inps:[h;e] ~outps:[s]

let _ = Fsm.dump_inst stdout g
      

let p = Comp.build_composite "gensig" [g]

let _ = Comp.dot_output "./dot" p

let c, rs = Simul.run p
let _ = List.iter Simul.dump_reaction rs
