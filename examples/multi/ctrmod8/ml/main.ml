open Rfsm
open Expr
open Types
open Action
  
let t_bit = type_int [0; 1]
          
let ctrmod2 = Fsm.build_model
  ~name:"ctrmod2"
  ~states:["E0"; "E1"]
  ~params:[]
  ~ios:[
    IO_In, "h", TyEvent;
    IO_Out, "s", t_bit;
    IO_Out, "r", TyEvent
    ]
  ~vars:[]
  ~trans:[
    ("E0", ("h",[]), [Assign (mk_lhs "s", mk_expr (EInt 1))], "E1", 0);
    ("E1", ("h",[]), [Emit "r"; Assign (mk_lhs "s", mk_expr (EInt 0))], "E0", 0);
    ]
  ~itrans:("E0",[Assign (mk_lhs "s", mk_expr (EInt 0))])

let rec pow2 n = if n=0 then 1 else 2 * pow2 (n-1)
           
let build_counter n = 
  let h = Global.GInp ("H", TyEvent, Periodic (10,10,100)) in
  let s = Array.init n (function i -> Global.GOutp ("S" ^ string_of_int i, t_bit)) in
  let r = Array.init n (function i -> Global.GShared ("R" ^ string_of_int i, TyEvent)) in
  let c = Array.init n
          (function i -> Fsm.build_instance
                     ~name:("c"^string_of_int i)
                     ~model:ctrmod2
                     ~params:[]
                     ~ios:[if i=0 then h else r.(i-1); s.(i); r.(i)]) in
  Sysm.build ~name:("ctrmod" ^ string_of_int (pow2 n)) ~fsms:(Array.to_list c)

let p = build_counter 3

let _ = Sysm.dot_output "./dot" p

let c, rs = Simul.run p
let _ = List.iter Simul.dump_reaction rs

