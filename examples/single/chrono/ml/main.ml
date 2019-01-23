open Rfsm
open Expr
open Types
open Action
  
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
    ("Stopped", ("startstop",[]),
     [Assign (mk_lhs "ctr", mk_expr (EInt 0));
      (* Assign (mk_lhs "aff", mk_expr(EBool false))], (\* err *\) *)
      Assign (mk_lhs "aff", mk_expr(EInt 0))],
     "Running",
     0);
    ("Running", ("h",[]),
     [Assign (mk_lhs "ctr", mk_expr(EBinop("+", mk_expr(EVar "ctr"), mk_expr(EInt 1))));
      Assign (mk_lhs "aff", mk_expr(EVar "ctr"))],
     "Running",
     0);
    ("Running", ("startstop",[]), [], "Stopped", 0)
    ]
  ~itrans:("Stopped",[])

let t = Typing.type_fsm_model Typing.builtin_tenv chrono
let _ = Printf.printf "type(chrono)=%s\n" (Types.string_of_type t)
      
let _ = Fsm.dot_output_model "./dot" chrono

let h = Global.GInp ("H", TyEvent, Periodic (10,10,110))
let startstop = Global.GInp ("StartStop", TyEvent, Sporadic [25; 75])
let aff = Global.GOutp ("Aff", type_int [])

let c1 = Fsm.build_instance ~name:"c1" ~model:chrono ~params:[] ~ios:[h;startstop;aff]
let t1 = Typing.type_fsm_inst Typing.builtin_tenv c1
let _ = Printf.printf "type(chrono)=%s\n" (Types.string_of_type t1)

let _ = Fsm.dot_output "./dot" c1

let s = Sysm.build ~name:"chrono" [c1]

let _ = Sysm.dot_output ~with_insts:true ~with_models:true "./dot" s

(* let c, rs = Simul.run s
 * let _ = List.iter Simul.dump_reaction rs *)

