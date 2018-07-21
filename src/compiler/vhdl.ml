(**********************************************************************)
(*                                                                    *)
(*              This file is part of the RFSM package                 *)
(*                                                                    *)
(*  Copyright (c) 2018-present, Jocelyn SEROT.  All rights reserved.  *)
(*                                                                    *)
(*  This source code is licensed under the license found in the       *)
(*  LICENSE file in the root directory of this source tree.           *)
(*                                                                    *)
(**********************************************************************)

(* VHDL backend *)

open Utils
open Fsm
open Types
open Printf
open Cmodel

exception Vhdl_error of string * string  (* where, msg *)

type vhdl_config = {
  mutable vhdl_lib_name: string;
  mutable vhdl_lib_dir: string;
  mutable vhdl_inpmod_prefix: string;
  mutable vhdl_tb_name: string;
  mutable vhdl_globals_name: string;
  mutable vhdl_state_var: string;
  mutable vhdl_stop_time: int;
  mutable vhdl_time_unit: string;
  mutable vhdl_reset_sig: string;
  mutable vhdl_reset_duration: int;
  mutable vhdl_ev_duration: int;
  mutable vhdl_use_numeric_std: bool;
  mutable vhdl_support_library: string;
  mutable vhdl_support_package: string;
  mutable vhdl_trace: bool;
  mutable vhdl_trace_state_var: string
  }

let cfg = {
  vhdl_lib_name = "rfsm";
  vhdl_lib_dir = ".";
  vhdl_inpmod_prefix = "inp_";
  vhdl_tb_name = "tb";
  vhdl_globals_name = "globals";
  vhdl_state_var = "state";
  vhdl_stop_time = 100;
  vhdl_reset_sig = "rst";
  vhdl_time_unit = "ns";
  vhdl_reset_duration = 1;
  vhdl_ev_duration = 1;
  vhdl_use_numeric_std = false;
  vhdl_support_library = "rfsm";
  vhdl_support_package = "core";
  vhdl_trace = false;
  vhdl_trace_state_var = "st";
  }

type profil = {
    mutable has_globals: bool;
  }

let profil = {
  has_globals = false;
  }

type vhdl_type = 
    Std_logic 
  | Unsigned of int
  | Signed of int
  | Integer of int_range option
  | Real
  | Boolean
  | Array of int * vhdl_type
  | Unknown

and int_range = int * int

let rec vhdl_type_of t = match t with 
  | TyEvent -> Std_logic
  | TyBool -> Boolean
  | TyFloat -> Real
  | TyEnum cs -> Error.not_implemented "VHDL translation of enumerated type"
  | TyInt (Some (TiConst lo,TiConst hi)) ->
      if cfg.vhdl_use_numeric_std then
        if lo < 0 then Signed (Systemc.bit_size (max (-lo) hi)) else Unsigned (Systemc.bit_size hi)
      else
        Integer (Some (lo,hi))
  | TyInt _ -> Integer None
  | TyArray (Types.Index.TiConst sz,t') -> Array (sz, vhdl_type_of t')
  | _ -> (Printf.printf "** warning: vhdl_type_of(%s)=Unknown\n" (string_of_type t); flush stdout; Unknown)

type type_mark = TM_Full | TM_Abbr | TM_None
                                   
let rec string_of_vhdl_type ?(type_marks=TM_Full) t = match t, type_marks with 
  | Std_logic, _ -> "std_logic"
  | Unsigned n, TM_Full -> Printf.sprintf "unsigned(%d downto 0)" (n-1)
  | Unsigned n, TM_Abbr -> Printf.sprintf "unsigned%d" n
  | Unsigned n, TM_None -> "unsigned"
  | Signed n, TM_Full -> Printf.sprintf "signed(%d downto 0)" (n-1)
  | Signed n, TM_Abbr -> Printf.sprintf "signed%d" n
  | Signed n, TM_None -> "signed"
  | Integer (Some (lo,hi)), TM_Full -> Printf.sprintf "integer range %d to %d" lo hi
  | Integer _, _ -> "integer"
  | Real, _ -> "real"
  | Boolean, _ -> "boolean"
  | Array (n,t'), _ -> string_of_vhdl_array_type n t'
  | Unknown, _ -> "<unknown>" (* failwith "Vhdl.string_of_vhdl_type" *)

and string_of_vhdl_array_type n t = "array_" ^ string_of_int n ^ "_" ^ string_of_vhdl_type ~type_marks:TM_Abbr t

let string_of_type ?(type_marks=TM_Full) t =
  string_of_vhdl_type ~type_marks:type_marks (vhdl_type_of t)

(* let global_types = ref ( [] : (string * Types.typ) list ) *)

let lookup_type tenv id = 
  try List.assoc id tenv 
  with Not_found -> failwith ("Vhdl.lookup_type(" ^ id ^ ")")

let type_error where what item ty1 ty2 = 
  raise (Vhdl_error(
     where,
     Printf.sprintf "incompatible types for %s \"%s\": %s and %s"
       what item (string_of_type ty1) (string_of_type ty2)))

(* let add_type (id,ty) =
 *   try
 *     let ty' = List.assoc id !global_types in
 *     if ty' <> ty then type_error "" "id" id ty ty'
 *   with Not_found ->
 *     (\* Printf.printf "** Adding %s (%s) for id %s to global types\n" (string_of_type ty) (Types.string_of_type ty) id; *\)
 *     global_types := (id,ty) :: !global_types *)

(* let reset_types () = global_types := [] *)

let vhdl_string_of_float x =
  Printf.sprintf "%#E" x
  (* let s = string_of_float x in
   * if String.get s (String.length s - 1) = '.' then s ^ "0" else s *)
  
let string_of_value ?(ty=None) v = match v, ty with
  Expr.Val_int i, Some (Unsigned n) -> Printf.sprintf "to_unsigned(%d,%d)" i n
| Expr.Val_int i, Some (Signed n) -> Printf.sprintf "to_signed(%d,%d)" i n
| Expr.Val_int i, Some Std_logic -> Printf.sprintf "'%d'" i
| Expr.Val_int i, Some (Integer _) -> Printf.sprintf "%d" i
| Expr.Val_int i, Some Boolean -> if i > 0 then "true" else "false"
| Expr.Val_int i, Some Real -> Printf.sprintf "%d.0" i
| Expr.Val_int i, None -> Printf.sprintf "%d" i
| Expr.Val_int i, _ -> failwith "Vhdl.string_of_value"
| Expr.Val_float f, _ -> vhdl_string_of_float f
| Expr.Val_bool b, _ -> string_of_bool b
| Expr.Val_enum s, _ -> Error.not_implemented "VHDL translation of enumerated value"
| Expr.Val_fn _, _ -> Error.not_implemented "VHDL translation of function value"
| Expr.Val_unknown, _ -> "<unknown>"
| Expr.Val_none, _ -> "<none>"
| Expr.Val_array _, _ -> Error.not_implemented "VHDL translation of array value"

let string_of_ival ?(ty=None) = function
    None -> ""
  | Some v -> " = " ^ string_of_value ~ty:ty v

let rec type_of_expr e = vhdl_type_of e.Expr.e_typ
  (* match e with
   *   Expr.EInt c -> Some Integer
   * | Expr.EFloat c -> Some (vhdl_type_of TyFloat)
   * | Expr.EBool c -> Some (vhdl_type_of TyBool)
   * | Expr.EEnum c -> None
   * | Expr.EVar n -> lookup_type n 
   * | Expr.EBinop (op,e1,e2) ->
   *     begin match type_of_expr e1, type_of_expr e2 with
   *       None, None -> None
   *     | Some t1, None -> Some t1
   *     | None, Some t2 -> Some t2
   *     | Some t1, Some t2 -> 
   *         if t1 = t2 then Some t1
   *         else type_error "" "binary operation" op t1 t2
   *     end
   * | Expr.ECond (e1,e2,e3) ->   (\* TO FIX ? *\)
   *     begin match type_of_expr e1, type_of_expr e2, type_of_expr e3 with
   *       _, None, None -> None
   *     | _, Some t1, None -> Some t1
   *     | _, None, Some t2 -> Some t2
   *     | _, Some t1, Some t2 -> 
   *         if t1 = t2 then Some t1
   *         else type_error "" "ternary conditioal" "" t1 t2
   *     end
   * | Expr.EFapp (f,es) -> None (\* TO FIX ? *\)
   * | Expr.EArr (a,idx) -> None (\* TO FIX ? *\) *)

let string_of_op = function
    "=" -> " = "
  | "!=" -> " /= "
  | "mod" -> " mod "
  | "+." -> "+" 
  | "-." -> "-" 
  | "*." -> "*" 
  | "/." -> "/" 
  | op ->  op

let string_of_expr e =
  let paren level s = if level > 0 then "(" ^ s ^ ")" else s in
  let rec string_of level e =
    Printf.printf "level=%d string_of(%s) : %s\n" level (Expr.to_string e) (string_of_type e.Expr.e_typ); flush stdout;
    match e.Expr.e_desc, vhdl_type_of e.Expr.e_typ  with
    | Expr.EInt n, Unsigned s -> Printf.sprintf "to_unsigned(%d,%d)" n s
    | Expr.EInt n, Signed s -> Printf.sprintf "to_signed(%d,%d)" n s
    | Expr.EInt n, Boolean -> if n > 0 then "true" else "false"
    | Expr.EInt n, Std_logic -> if n > 0 then "'1'" else "'0'"
    | Expr.EInt n, _ -> string_of_int n
    | Expr.EFloat c, _ -> vhdl_string_of_float c
    | Expr.EBool c, _ -> string_of_bool c
    | Expr.EEnum c, _ -> c
    | Expr.EVar n, _ ->  n
    | Expr.EBinop (op,e1,e2), _ -> 
       let s1 = string_of (level+1) e1 
       and s2 = string_of (level+1) e2 in 
       begin match op, type_of_expr e1, type_of_expr e2 with
       | "*", Signed _, _
       | "*", Unsigned _, _
       | "*", _, Unsigned _
       | "*", _, Signed _ ->  "mul(" ^ s1 ^ "," ^ s2 ^ ")"
       | _, _, _ -> paren level (s1 ^ string_of_op op ^ s2)
       end
    | Expr.ECond (e1,e2,e3), _ -> sprintf "cond(%s,%s,%s)" (string_of level e1) (string_of level e2) (string_of level e3)
    | Expr.EFapp (("~-"|"~-."),[e]), _ -> "-" ^ "(" ^ string_of level e ^ ")"
    | Expr.EFapp (f,es), _ -> f ^ "(" ^ ListExt.to_string (string_of level) "," es ^ ")"
    | Expr.EArr (a,idx), _ -> a ^ "(" ^ string_of level idx ^ ")"
  in
  string_of 0 e

let string_of_action ?(lvars=[]) m a =
  let tenv = List.map (function (id,(ty,_)) -> id, ty) m.c_vars @ m.c_outps @ m.c_inouts in
    match a with
  | Action.Assign (Action.Var0 id, expr) ->
     let asn = if List.mem_assoc id lvars && Fsm.cfg.Fsm.act_sem = Sequential then " := " else " <= " in
     let ty = lookup_type tenv id in
     expr.Expr.e_typ <- ty;  (* To handle situations like [v:=1] when [v:unsigned(2 downto 0)] *) 
     id ^ asn ^ string_of_expr expr
  | Action.Assign (Action.Var1 (id,idx), expr) ->
     let asn = if List.mem_assoc id lvars && Fsm.cfg.Fsm.act_sem = Sequential then " := " else " <= " in
     let ty = Types.subtype_of (lookup_type tenv id) in
     expr.Expr.e_typ <- ty;  (* To handle situations like  [v[0]:=1] when [v:array of unsigned(2 downto 0)] *) 
     if List.mem_assoc id lvars
     then id ^ "(" ^ string_of_expr idx ^ ")" ^ asn ^ string_of_expr expr
     else failwith "Vhdl.string_of_action: assignation of a non-scalar output"
  | Action.Emit id -> "notify_ev(" ^ id ^ "," ^ (string_of_int cfg.vhdl_ev_duration) ^ " " ^ cfg.vhdl_time_unit ^ ")"
  | Action.StateMove (id,s,s') -> "" (* should not happen *)

let string_of_condition (e,cs) =  
  let string_of_guard gexp  = string_of_expr gexp in
  match cs with
    [] -> failwith "Vhdl.string.of_condition"
  | _ -> ListExt.to_string string_of_guard " and " cs

let dump_action ?(lvars=[]) oc tab m a = fprintf oc "%s%s;\n" tab (string_of_action ~lvars:lvars m a)

let dump_transition ?(lvars=[]) oc tab src clk m (is_first,needs_endif) (q',(cond,acts,_,_)) =
  match cond with
    _, [] -> 
       List.iter (dump_action ~lvars:lvars oc tab m) acts;
       fprintf oc "%s%s <= %s;\n" tab cfg.vhdl_state_var q';
       (false,false)
  | _, _ ->
       fprintf oc "%s%s ( %s ) then\n" tab (if is_first then "if" else "elsif ") (string_of_condition cond);
       List.iter (dump_action ~lvars:lvars oc (tab ^ "  ") m) acts;
       if q' <> src then fprintf oc "%s  %s <= %s;\n" tab cfg.vhdl_state_var q';
       (false,true)

let dump_sync_transitions ?(lvars=[]) oc src after clk m ts =
   let tab = "        " in
   let (_,needs_endif) = List.fold_left (dump_transition ~lvars:lvars oc tab src clk m) (true,false) ts in
   if needs_endif then fprintf oc "        end if;\n"
     
let dump_state oc clk m { st_src=q; st_sensibility_list=evs; st_transitions=tss } =
  match tss with
    [ev,ts] -> dump_sync_transitions ~lvars:m.c_vars oc q false clk m ts
  | _ -> Error.not_implemented "VHDL: transitions involving multiple events"

let dump_state_case oc clk m c =
    fprintf oc "      when %s =>\n" c.st_src;
    dump_state oc clk m c

let dump_array_types oc m =
  let array_types =
    List.fold_left
      (fun acc (_,(ty,_)) ->
        match ty with
        | TyArray (Types.Index.TiConst _, _) when not (List.mem ty acc) -> ty::acc
        | TyArray (_, _) -> failwith "Vhdl.dump_array_types"
        | _ -> acc)
      []
      m.c_vars in
  List.iter 
    (function
     | TyArray(Types.Index.TiConst sz,ty') as ty ->
        fprintf oc "  type %s is array (0 to %d) of %s;\n" (string_of_type ~type_marks:TM_Abbr ty) (sz-1) (string_of_type ty')
     | _ -> ())
      array_types

let dump_module_arch oc s fsm =
  let m = Cmodel.c_model_of_fsm s fsm in
  let modname = m.c_name in
  (* let _ = reset_types () in
   * List.iter (function (id,ty) -> add_type (id, ty)) m.c_inps;
   * List.iter (function (id,ty) -> add_type (id, ty)) m.c_outps;
   * List.iter (function (id,ty) -> add_type (id, ty)) m.c_inouts;
   * List.iter (function (id,(ty,_)) -> add_type (id, ty)) m.c_vars; *)
  let clk_sig = match List.filter (function (_, TyEvent) -> true | _ -> false) m.c_inps with
    [] -> raise (Vhdl_error (m.c_name, "no input event, hence no possible clock"))
  | [h,_] -> h
  | _ -> Error.not_implemented (m.c_name ^ ": translation to VHDL of FSM with more than one input events") in
  fprintf oc "architecture RTL of %s is\n" modname;
  fprintf oc "  type t_%s is ( %s );\n" cfg.vhdl_state_var (ListExt.to_string (function s -> s) ", " m.c_states);
  fprintf oc "  signal %s: t_state;\n" cfg.vhdl_state_var;
  dump_array_types oc m;
  if Fsm.cfg.Fsm.act_sem = Fsm.Synchronous then 
    List.iter
      (fun (id,(ty,iv)) -> fprintf oc "  signal %s: %s;\n" id (string_of_type ~type_marks:TM_Abbr ty))
      m.c_vars;
  fprintf oc "begin\n";
  fprintf oc "  process(%s, %s)\n" cfg.vhdl_reset_sig clk_sig;
  if Fsm.cfg.Fsm.act_sem = Fsm.Sequential then 
    List.iter
      (fun (id,(ty,iv)) -> fprintf oc "    variable %s: %s;\n" id (string_of_type ty))
      m.c_vars;
  fprintf oc "  begin\n";
  fprintf oc "    if ( %s='1' ) then\n" cfg.vhdl_reset_sig;
  fprintf oc "      %s <= %s;\n" cfg.vhdl_state_var (fst m.c_init);
  List.iter (dump_action ~lvars:m.c_vars oc "      " m) (snd m.c_init);
  fprintf oc "    elsif rising_edge(%s) then \n" clk_sig;
  begin match m.c_body with
    [] -> () (* should not happen *)
  | [q] -> dump_state oc clk_sig m q 
  | qs -> 
      fprintf oc "      case %s is\n" cfg.vhdl_state_var;
      List.iter (dump_state_case oc clk_sig m) m.c_body;
      fprintf oc "    end case;\n"
  end;
  fprintf oc "    end if;\n";
  fprintf oc "  end process;\n";
  if cfg.vhdl_trace then begin
    let int_of_vhdl_state m =
      ListExt.to_string
        (function (s,i) -> string_of_int i ^ " when " ^ cfg.vhdl_state_var ^ "=" ^ s)
        " else "
        (List.mapi (fun i s -> s,i) m.c_states) in
    fprintf oc "  %s <= %s;\n" cfg.vhdl_trace_state_var (int_of_vhdl_state m)
    end;
  fprintf oc "end RTL;\n"

let dump_module_intf kind oc m fsm = 
  let m = Cmodel.c_model_of_fsm m fsm in
  let modname = m.c_name in
  fprintf oc "%s %s %s\n" kind modname (if kind = "entity" then "is" else "");
  fprintf oc "  port(\n";
  List.iter (fun (id,ty) -> fprintf oc "        %s: in %s;\n" id (string_of_type ty)) m.c_inps;
  List.iter (fun (id,ty) -> fprintf oc "        %s: out %s;\n" id (string_of_type ty)) m.c_outps;
  List.iter (fun (id,ty) -> fprintf oc "        %s: inout %s;\n" id (string_of_type ty)) m.c_inouts;
  fprintf oc "        %s: in std_logic" cfg.vhdl_reset_sig;
  if cfg.vhdl_trace then fprintf oc ";\n        %s: out integer\n" cfg.vhdl_trace_state_var else fprintf oc "\n";
  fprintf oc "        );\n";
  fprintf oc "end %s;\n" (if kind = "entity" then modname else kind)

(* Dumping input generator processes *)

let string_of_time t = string_of_int t ^ " " ^ cfg.vhdl_time_unit

let dump_sporadic_inp_process oc id ts =
       fprintf oc "    type t_dates is array ( 0 to %d ) of time;\n" (List.length ts-1);
       fprintf oc "    constant dates : t_dates := ( %s );\n" (ListExt.to_string string_of_time ", " ts);
       fprintf oc "    variable i : natural := 0;\n";
       fprintf oc "    variable t : time := 0 %s;\n" cfg.vhdl_time_unit;
       fprintf oc "    begin\n";
       fprintf oc "      %s <= '0';\n" id;
       fprintf oc "      for i in 0 to %d loop\n" (List.length ts-1);
       fprintf oc "        wait for dates(i)-t;\n";
       fprintf oc "        notify_ev(%s,%d %s);\n" id cfg.vhdl_ev_duration cfg.vhdl_time_unit;
       fprintf oc "        t := dates(i);\n";
       fprintf oc "      end loop;\n";
       fprintf oc "      wait;\n"

let dump_periodic_inp_process oc id (p,t1,t2) =
       fprintf oc "    type t_periodic is record period: time; t1: time; t2: time; end record;\n";
       fprintf oc "    constant periodic : t_periodic := ( %s, %s, %s );\n"
               (string_of_time (p-cfg.vhdl_ev_duration))
               (string_of_time t1)
               (string_of_time t2);
       fprintf oc "    variable t : time := 0 %s;\n" cfg.vhdl_time_unit;
       fprintf oc "    begin\n";
       fprintf oc "      %s <= '0';\n" id;
       fprintf oc "      wait for periodic.t1;\n";
       fprintf oc "      notify_ev(%s,%d %s);\n" id cfg.vhdl_ev_duration cfg.vhdl_time_unit;
       fprintf oc "      while ( t < periodic.t2 ) loop\n";
       fprintf oc "        wait for periodic.period;\n";
       fprintf oc "        notify_ev(%s,%d %s);\n" id cfg.vhdl_ev_duration cfg.vhdl_time_unit;
       fprintf oc "        t := t + periodic.period;\n";
       fprintf oc "      end loop;\n";
       fprintf oc "      wait;\n"
  
let dump_vc_inp_process oc ty id vcs =
       let ty = vhdl_type_of ty in
       let string_of_vc (t,v) = "(" ^ string_of_int t ^ " " ^ cfg.vhdl_time_unit ^ "," ^ string_of_value ~ty:(Some ty) v ^ ")" in
       fprintf oc "    type t_vc is record date: time; val: %s; end record;\n" (string_of_vhdl_type ty);
       fprintf oc "    type t_vcs is array ( 0 to %d ) of t_vc;\n" (List.length vcs-1);
       fprintf oc "    constant vcs : t_vcs := ( %s%s );\n"
               (if List.length vcs = 1 then "others => " else "")  (* GHDL complains when initializing a 1-array *)
               (ListExt.to_string string_of_vc ", " vcs);
       fprintf oc "    variable i : natural := 0;\n";
       fprintf oc "    variable t : time := 0 %s;\n" cfg.vhdl_time_unit;
       fprintf oc "    begin\n";
       fprintf oc "      for i in 0 to %d loop\n" (List.length vcs-1);
       fprintf oc "        wait for vcs(i).date-t;\n";
       fprintf oc "        %s <= vcs(i).val;\n" id;
       fprintf oc "        t := vcs(i).date;\n";
       fprintf oc "      end loop;\n";
       fprintf oc "      wait;\n"

let dump_input_process oc (id,(ty,desc)) =
  let open Sysm in
  fprintf oc "  inp_%s: process\n" id;
  begin match desc with
    | MInp ({sd_comprehension=Sporadic ts}, _) -> dump_sporadic_inp_process oc id ts
    | MInp ({sd_comprehension=Periodic (p,t1,t2)}, _) -> dump_periodic_inp_process oc id (p,t1,t2)
    | MInp ({sd_comprehension=ValueChange []}, _) -> ()
    | MInp ({sd_comprehension=ValueChange vcs}, _) -> dump_vc_inp_process oc ty id vcs 
    | _ -> failwith "Vhdl.dump_inp_module_arch" (* should not happen *) end;
  fprintf oc "  end process;\n"

(* Dumping the testbench *)

let tb_name s = s

let dump_testbench_impl fname m =
  let oc = open_out fname in
  let open Sysm in
  let modname n = String.capitalize_ascii n in
  fprintf oc "library ieee;\n";
  fprintf oc "use ieee.std_logic_1164.all;	   \n";
  if cfg.vhdl_use_numeric_std then fprintf oc "use ieee.numeric_std.all;\n";
  fprintf oc "library %s;\n" cfg.vhdl_support_library;
  fprintf oc "use %s.%s.all;\n" cfg.vhdl_support_library cfg.vhdl_support_package;
  fprintf oc "\n";
  fprintf oc "entity tb is\n";
  fprintf oc "end tb;\n";
  fprintf oc "\n";
  fprintf oc "architecture Bench of tb is\n";
  fprintf oc "\n";
  List.iter (dump_module_intf "component" oc m) m.m_fsms;
  fprintf oc "\n";
  (* reset_types (); *)
  (* Signals *)
  List.iter
   (function (id,(ty,_)) ->
     fprintf oc "signal %s: %s;\n" (tb_name id) (string_of_type ty))
     (* add_type (id, ty)) *)
   (m.m_inputs @ m.m_outputs @ m.m_shared);
  fprintf oc "signal %s: std_logic;\n" (tb_name cfg.vhdl_reset_sig);
  if cfg.vhdl_trace then
    List.iter
      (function f -> fprintf oc "  signal %s: integer;\n" (f.f_name ^ "_state"))
      m.m_fsms;  
  fprintf oc "\n";
  fprintf oc "begin\n";
  fprintf oc "\n";
  (* Input generators *)
  List.iter (dump_input_process oc) m.m_inputs;
  fprintf oc "\n";
  (* Instanciated components *)
  List.iteri
    (fun i f ->
      let m = Cmodel.c_model_of_fsm m f in
      let actual_name (id,_) = f.f_l2g id in
      fprintf oc "  U%d: %s port map(%s%s);\n"
        i
        (modname f.f_name)
        (ListExt.to_string tb_name ","
           (List.map actual_name (m.c_inps @  m.c_outps @ m.c_inouts) @ [cfg.vhdl_reset_sig]))
        (if cfg.vhdl_trace then "," ^ f.f_name ^ "_state" else ""))
    m.m_fsms;
  (* Main process *)
  fprintf oc "\n";
  fprintf oc "  process\n";
  fprintf oc "\n";
  fprintf oc "  begin\n";
  fprintf oc "    %s <= '1';\n" cfg.vhdl_reset_sig;
  fprintf oc "    wait for %d %s;\n" cfg.vhdl_reset_duration cfg.vhdl_time_unit;
  fprintf oc "    %s <= '0';\n" cfg.vhdl_reset_sig;
  fprintf oc "    wait for %d %s;\n" cfg.vhdl_stop_time cfg.vhdl_time_unit;
  fprintf oc "    wait;\n";
  fprintf oc "\n";
  fprintf oc "  end process;\n";
  fprintf oc "end Bench;\n";
  Logfile.write fname;
  close_out oc

let dump_fsm ?(prefix="") ?(dir="./vhdl") m fsm =
  let prefix = match prefix with "" -> fsm.Fsm.f_name | p -> p in
  let fname = dir ^ "/" ^ prefix ^ ".vhd" in
  let oc = open_out fname in
  fprintf oc "library ieee;\n";
  fprintf oc "use ieee.std_logic_1164.all;\n";
  if cfg.vhdl_use_numeric_std then fprintf oc "use ieee.numeric_std.all;\n";
  fprintf oc "library %s;\n" cfg.vhdl_support_library;
  fprintf oc "use %s.%s.all;\n" cfg.vhdl_support_library cfg.vhdl_support_package;
  if profil.has_globals then fprintf oc "use work.%s.all;\n" cfg.vhdl_globals_name;
  fprintf oc "\n";
  dump_module_intf "entity" oc m fsm;
  fprintf oc "\n";
  dump_module_arch oc m fsm;
  Logfile.write fname;
  close_out oc

let dump_testbench ?(name="") ?(dir="./vhdl") m =
  let prefix = match name with "" -> cfg.vhdl_tb_name | p -> p in
  dump_testbench_impl (dir ^ "/" ^ prefix ^ ".vhd") m

let dump_model ?(dir="./vhdl") m =
  List.iter (dump_fsm ~dir:dir m) m.Sysm.m_fsms

(* Dumping global functions *)

let rec dump_globals ?(name="") ?(dir="./systemc") m =
  let prefix = match name with "" -> cfg.vhdl_globals_name | p -> p in
  let fname = dir ^ "/" ^ prefix ^ ".vhd" in
  let oc = open_out fname in
  profil.has_globals <- true;
  fprintf oc "library ieee;\n";
  fprintf oc "use ieee.std_logic_1164.all;\n";
  if cfg.vhdl_use_numeric_std then fprintf oc "use ieee.numeric_std.all;\n";
  fprintf oc "library %s;\n" cfg.vhdl_support_library;
  fprintf oc "use %s.%s.all;\n\n" cfg.vhdl_support_library cfg.vhdl_support_package;
  dump_globals_intf oc prefix m.Sysm.m_fns;
  fprintf oc "\n";
  dump_globals_impl oc prefix m.Sysm.m_fns;
  Logfile.write fname;
  close_out oc

and dump_globals_intf oc package_name fns =
  fprintf oc "package %s is\n" package_name;
  List.iter (dump_global_fn_sig oc) fns;
  fprintf oc "end %s;\n" package_name

and dump_global_fn_sig oc (id,(ty,gd)) = match gd, ty with
| Sysm.MFun (args, body), Types.TyArrow(TyProduct ts, tr) -> 
    fprintf oc "  function %s(%s) return %s;\n"
      id
      (ListExt.to_string string_of_fn_arg "; "  (List.combine args ts))
      (string_of_type ~type_marks:TM_None tr) 
| _ -> ()

and string_of_fn_arg (id,ty) = id ^ ":" ^ (string_of_type ty)

and dump_globals_impl oc package_name fns =
  fprintf oc "package body %s is\n" package_name;
  List.iter (dump_global_fn_impl oc) fns;
  fprintf oc "end %s;\n" package_name

and dump_global_fn_impl oc (id,(ty,gd)) = match gd, ty with
| Sysm.MFun (args, body), Types.TyArrow(TyProduct ts, tr) -> 
    fprintf oc "function %s(%s) return %s is\n"
      id
      (ListExt.to_string string_of_fn_arg "; "  (List.combine args ts))
      (string_of_type ~type_marks:TM_None tr) ;
    fprintf oc "  begin\n";
    fprintf oc "    return %s;\n" (string_of_expr body);
    fprintf oc "  end %s;\n" id
| _ -> ()

(* Dumping Makefile *)

let dump_makefile ?(dir="./vhdl") m =
  let fname = dir ^ "/" ^ "Makefile" in
  let oc = open_out fname in
  let modname suff f = f.f_name ^ suff in
  let open Sysm in
  fprintf oc "include %s/etc/Makefile.vhdl\n\n" cfg.vhdl_lib_dir;
  fprintf oc "%s: %s %s %s.vhd\n"
          cfg.vhdl_tb_name
          (if profil.has_globals then cfg.vhdl_globals_name ^ ".vhd" else "")
          (ListExt.to_string (modname ".vhd") " " m.m_fsms)
          cfg.vhdl_tb_name;
  if profil.has_globals then 
    fprintf oc "\t$(GHDL) -a $(GHDLOPTS) %s.vhd\n" cfg.vhdl_globals_name;
  List.iter
    (function f -> fprintf oc "\t$(GHDL) -a $(GHDLOPTS) %s\n" (modname ".vhd" f))
    m.m_fsms;
  fprintf oc "\t$(GHDL) -a $(GHDLOPTS) %s.vhd\n" cfg.vhdl_tb_name;
  fprintf oc "\t$(GHDL) -e $(GHDLOPTS) %s\n" cfg.vhdl_tb_name;
  Logfile.write fname;
  close_out oc

(* Check whether a model can be translated *)

let check_allowed m =
  let open Sysm in 
  let is_mono_sync f = match Fsm.input_events_of f with
    | [_] -> ()
    | _ -> Error.not_implemented "Vhdl: FSM with more than one input event" in
  let no_outp_event f = match Fsm.output_events_of f with
    | [] -> ()
    | _ -> Error.not_implemented "Vhdl: FSM with output event(s)" in
  if List.length m.m_fsms > 1 then Error.not_implemented "Vhdl: multi-FSMs model";
  List.iter is_mono_sync m.m_fsms;
  List.iter no_outp_event m.m_fsms;
  if Fsm.cfg.Fsm.act_sem = Fsm.Synchronous && List.exists (function f -> not (Fsm.is_rtl f)) m.m_fsms then
     Error.warning "Vhdl: Some FSM(s) have non-RTL transitions. This may cause incorrect behavior when using the synchronous interpretation of actions."
   
