(* Emet "Gagne" a chaque occurrence de trois "Pile" consecutifs *)

open Rfsm
open Expr
open Types
open Action
  
let pf = Fsm.build_model
  ~name:"pf"
  ~states:["P0"; "P1"; "P2"]
  ~params:[]
  ~ios:[
    IO_In, "Pile", TyEvent;
    IO_In, "Face", TyEvent;
    IO_Out, "Gagne", TyEvent
    ]
  ~vars:[]
  ~trans:[
    ("P0", ("Face",[]), [], "P0", 0); 
    ("P0", ("Pile",[]), [], "P1", 0); 
    ("P1", ("Face",[]), [], "P0", 0); 
    ("P1", ("Pile",[]), [], "P2", 0); 
    ("P2", ("Face",[]), [], "P0", 0); 
    ("P2", ("Pile",[]), [Emit "Gagne"], "P2", 0); 
    ]
  ~itrans:("P0",[])

let pile = Global.GInp ("Pile", TyEvent, Sporadic [20; 40; 50; 70; 80; 90; 110; 120; 130; 140])
let face = Global.GInp ("Face", TyEvent, Sporadic [10; 30; 60; 150])
let gagne = Global.GOutp ("Gagne", TyEvent)

let pf = Fsm.build_instance ~name:"pf" ~model:pf ~params:[] ~ios:[pile;face;gagne]

let p = Sysm.build "game" [pf]

let _ = Sysm.dot_output "./dot" p

(* let c, rs = Simul.run p
 * let _ = List.iter Simul.dump_reaction rs *)
