open Rfsm
open Expr
open Types
open Action
  
let t_bit = TyInt (Some (TiConst 0, TiConst 1))
           
let gensig = Fsm.build_model
  ~name:"gensig"
  ~states:["E0"; "E1"]
  ~params:["n",TyInt None]
  ~ios:[
    IO_In, "h",TyEvent;
    IO_In, "e",t_bit;
    IO_Out, "s", t_bit
    ]
  ~vars:["k", TyInt (Some (TiConst 0, TiVar "n"))] 
  ~trans:[
    ("E0", ("h",[EVar "e", "=", EInt 1]), [Assign ("k",EInt 1); Assign ("s",EInt 1)], "E1", 0);
    ("E1", ("h",[EVar "k", "<", EVar "n"]), [Assign ("k",EBinop ("+",EVar "k",EInt 1))], "E1", 0);
    ("E1", ("h",[EVar "k", "=", EVar "n"]), [Assign ("s",EInt 0)], "E0", 0);
    ]
  ~itrans:("E0",[Assign ("s", EInt 0)])

let h = Fsm.GInp ("H", TyEvent, Periodic (10,0,80))
let e = Fsm.GInp ("E", t_bit, ValueChange [0,Val_int 0; 25,Val_int 1; 35,Val_int 0])
let s = Fsm.GOutp ("S", t_bit)

let g = Fsm.build_instance ~name:"g" ~model:gensig ~params:["n",Val_int 4] ~ios:[h;e;s]

(* let _ = Fsm.dump_inst stdout g *)
      
let p = Comp.build_composite "gensig" [g]

let _ = Comp.dot_output "./dot" p

let c, rs = Simul.run p
let _ = List.iter Simul.dump_reaction rs
