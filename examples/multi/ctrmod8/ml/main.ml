open Rfsm
open Expr
open Types
open Action
  
let t_bit = TyInt (Some (0,1))
          
let ctrmod2 = Fsm.build_model
  ~name:"ctrmod2"
  ~states:["E0"; "E1"]
  ~params:[]
  ~inps:["h",TyEvent]
  ~outps:["s", t_bit; "r", TyEvent]
  ~vars:[]
  ~trans:[
    ("E0", ("h",[]), [Assign ("s",EConst 1)], "E1");
    ("E1", ("h",[]), [Emit "r"; Assign ("s",EConst 0)], "E0");
    ]
  ~itrans:("E0",[Assign ("s",EConst 0)])

let rec pow2 n = if n=0 then 1 else 2 * pow2 (n-1)
           
let build_counter n = 
  let h = Fsm.GInp ("H", TyEvent, Periodic (10,10,100)) in
  let s = Array.init n (function i -> Fsm.GOutp ("S" ^ string_of_int i, t_bit)) in
  let r = Array.init n (function i -> Fsm.GShared ("R" ^ string_of_int i, TyEvent)) in
  let c = Array.init n
          (function i -> Fsm.build_instance
                     ~name:("c"^string_of_int i)
                     ~model:ctrmod2
                     ~params:[]
                     ~inps:[if i=0 then h else r.(i-1)]
                     ~outps:[s.(i);r.(i)]) in
  Comp.build_composite ("ctrmod" ^ string_of_int (pow2 n)) (Array.to_list c)

let p = build_counter 3

let _ = Comp.dot_output "./dot" p

let c, rs = Simul.run p
let _ = List.iter Simul.dump_reaction rs

