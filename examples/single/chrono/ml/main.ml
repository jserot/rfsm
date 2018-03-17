open Rfsm
open Expr
open Types
open Action
  
let chrono = Fsm.build_model
  ~name:"chrono"
  ~states:["Stopped"; "Running"]
  ~params:[]
  ~inps:["h",TyEvent; "startstop",TyEvent]
  ~outps:["aff", TyInt None]
  ~vars:["ctr", TyInt None]
  ~trans:[
    ("Stopped", ("startstop",[]), [Assign ("ctr",EConst 0); Assign ("aff",EConst 0)], "Running");
    ("Running", ("h",[]), [Assign ("ctr",EBinop("+",EVar "ctr",EConst 1)); Assign ("aff",EVar "ctr")], "Running");
    ("Running", ("startstop",[]), [], "Stopped")
    ]
  ~itrans:("Stopped",[])

(* let _ = Fsm.dump_model stdout chrono *)
(* let _ = Fsm.dot_output_model "./dot" chrono *)

let h = Fsm.GInp ("H", TyEvent, Periodic (10,10,110))
let startstop = Fsm.GInp ("StartStop", TyEvent, Sporadic [25; 75])
let aff = Fsm.GOutp ("Aff", TyInt None)

let c1 = Fsm.build_instance ~name:"c1" ~model:chrono ~params:[] ~inps:[h;startstop] ~outps:[aff]

(* let _ = Fsm.dump_inst stdout c1 *)
(* let _ = Fsm.dot_output "./dot" c1 *)

let p = Comp.build_composite "chrono" [c1]

(* let _ = Comp.dump stdout p *)
let _ = Comp.dot_output "./dot" p

let c, rs = Simul.run p
let _ = List.iter Simul.dump_reaction rs

