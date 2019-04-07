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

open Fsm
open Types
open Printf

exception Error of string * string  (* where, msg *)

type dump_format = Vcd | Ghw

type vhdl_config = {
  mutable vhdl_lib_name: string;
  mutable vhdl_lib_dir: string;
  mutable vhdl_inpmod_prefix: string;
  mutable vhdl_tb_prefix: string;
  mutable vhdl_globals_name: string;
  mutable vhdl_enum_prefix: string;
  mutable vhdl_state_var: string;
  mutable vhdl_stop_time: int;
  mutable vhdl_time_unit: string;
  mutable vhdl_reset_sig: string;
  mutable vhdl_reset_duration: int;
  mutable vhdl_ev_duration: int;
  mutable vhdl_use_numeric_std: bool;
  mutable vhdl_bool_as_bool: bool;
  mutable vhdl_support_library: string;
  mutable vhdl_support_package: string;
  mutable vhdl_trace: bool;
  mutable vhdl_dump_format: dump_format;
  mutable vhdl_trace_state_var: string
  }

let cfg = {
  vhdl_lib_name = "rfsm";
  vhdl_lib_dir = ".";
  vhdl_inpmod_prefix = "inp_";
  vhdl_tb_prefix = "main";
  vhdl_globals_name = "globals";
  vhdl_enum_prefix = "E_";
  vhdl_state_var = "state";
  vhdl_stop_time = 100;
  vhdl_reset_sig = "rst";
  vhdl_time_unit = "ns";
  vhdl_reset_duration = 1;
  vhdl_ev_duration = 1;
  vhdl_use_numeric_std = false;
  vhdl_bool_as_bool = false;
  vhdl_support_library = "rfsm";
  vhdl_support_package = "core";
  vhdl_trace = false;
  vhdl_dump_format = Vcd;
  vhdl_trace_state_var = "st";
  }

let need_globals m = m.Static.m_types <> [] || m.Static.m_fns <> [] || m.Static.m_consts <> [] 

let enum_id id = cfg.vhdl_enum_prefix ^ id
               
type vhdl_type = 
    Std_logic 
  | Unsigned of int
  | Signed of int
  | Integer of int_range option
  | Real
  | Boolean
  | Char
  | Array of int * vhdl_type
  | Enum of string * string list
  | Record of string * (string * vhdl_type) list
  | Unknown

and int_range = int * int

let rec vhdl_type_of t = match Types.real_type t with 
  | TyEvent -> Std_logic
  | TyBool -> if cfg.vhdl_bool_as_bool then Boolean else Std_logic
  | TyFloat -> Real
  | TyChar -> Char
  | TyEnum (nm,cs) when Types.is_lit_name nm -> Enum (Types.string_of_name nm,cs)
  | TyInt (SzExpr1 (TiConst sz)) ->
      if cfg.vhdl_use_numeric_std then Unsigned sz
      else Integer (Some (0, 1 lsl sz - 1))
  | TyInt (SzExpr2 (TiConst lo, TiConst hi)) ->
      if cfg.vhdl_use_numeric_std then
        if lo < 0 then Signed (Intbits.bit_size (max (-lo) hi)) else Unsigned (Intbits.bit_size hi)
      else
        Integer (Some (lo,hi))
  | TyInt _ -> Integer None
  | TyArray (Types.Index.TiConst sz,t') -> Array (sz, vhdl_type_of t')
  | TyRecord (nm, fs) when Types.is_lit_name nm ->
     Record (Types.string_of_name nm, List.map (function (n,ty) -> n, vhdl_type_of ty) fs)
  | ty -> Misc.fatal_error ("Vhdl.vhdl_type_of (" ^ Types.string_of_type ty ^ ")")

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
  | Char, _ -> "character"
  | Array (n,t'), _ -> string_of_vhdl_array_type n t'
  | Enum (n,_), _ -> n
  | Record (n,_), _ -> n
  | Unknown, _ -> "<unknown>" 

and string_of_vhdl_array_type n t = "array_" ^ string_of_int n ^ "_" ^ string_of_vhdl_type ~type_marks:TM_Abbr t

let string_of_type ?(type_marks=TM_Full) t =
  string_of_vhdl_type ~type_marks:type_marks (vhdl_type_of t)

let lookup_type tenv id = 
  try List.assoc id tenv 
  with Not_found -> Misc.fatal_error ("Vhdl.lookup_type(" ^ id ^ ")")

let type_error where what item ty1 ty2 = 
  raise (Error(
     where,
     Printf.sprintf "incompatible types for %s \"%s\": %s and %s"
       what item (string_of_type ty1) (string_of_type ty2)))

let vhdl_string_of_float x =
  Printf.sprintf "%#E" x
  (* let s = string_of_float x in
   * if String.get s (String.length s - 1) = '.' then s ^ "0" else s *)

let vhdl_string_of_bool b = match cfg.vhdl_bool_as_bool, b with
  | true, _ -> string_of_bool b
  | false, true -> "'1'"
  | false, false -> "'0'"
  
let vhdl_string_of_char c = "'" ^ String.make 1 c ^ "'"

let rec string_of_value v =
  match v.Expr.v_desc, vhdl_type_of v.Expr.v_typ with
  Expr.Val_int i, Unsigned n -> Printf.sprintf "to_unsigned(%d,%d)" i n
| Expr.Val_int i, Signed n -> Printf.sprintf "to_signed(%d,%d)" i n
| Expr.Val_int i, Std_logic -> Printf.sprintf "'%d'" i
| Expr.Val_int i, Integer _ -> Printf.sprintf "%d" i
| Expr.Val_int i, Boolean -> vhdl_string_of_bool (i > 0)
| Expr.Val_int i, Real -> Printf.sprintf "%d.0" i
| Expr.Val_int i, _ -> Printf.sprintf "%d" i
| Expr.Val_float f, _ -> vhdl_string_of_float f
| Expr.Val_char f, _ -> vhdl_string_of_char f
| Expr.Val_bool b, _ -> vhdl_string_of_bool b
| Expr.Val_enum s, _ -> enum_id s
| Expr.Val_fn _, _ -> Misc.not_implemented "VHDL translation of function value"
| Expr.Val_unknown, _ -> "<unknown>"
| Expr.Val_none, _ -> "<none>"
| Expr.Val_array vs, _ ->
   "(" ^ Utils.ListExt.to_string string_of_value "," (Array.to_list vs) ^")"
| Expr.Val_record fs, _ ->
   "(" ^ Utils.ListExt.to_string (function (n,v) -> string_of_value v) "," fs ^")"

let string_of_ival = function
    None -> ""
  | Some v -> " = " ^ string_of_value v

let rec type_of_expr e = vhdl_type_of e.Expr.e_typ

let string_of_op = function
    "=" -> " = "
  | "!=" -> " /= "
  | "mod" -> " mod "
  | "+." -> "+" 
  | "-." -> "-" 
  | "*." -> "*" 
  | "/." -> "/" 
  | "&" -> " and " 
  | "|" -> " or " 
  | "^" -> " xor " 
  | op ->  op

let string_of_cast t_exp t_ty e = match t_exp, t_ty with
  | Integer _, Unsigned n -> sprintf "conv_unsigned(%s,%d)" e n
  | Signed n, Unsigned n' when n=n' -> sprintf "conv_unsigned(%s,%d)" e n
  | Boolean, Unsigned n ->  sprintf "conv_unsigned(%s,%d)" e n
  | Unsigned n', Unsigned n when n<>n' ->  sprintf "resize(%s,%d)" e n
  | Integer _, Signed n -> sprintf "conv_signed(%s,%d)" e n
  | Signed n, Signed n' when n=n' -> sprintf "conv_signed(%s,%d)" e n
  | Boolean, Signed n ->  sprintf "conv_signed(%s,%d)" e n
  | Signed n', Signed n when n<>n' -> sprintf "resize(%s,%d)" e n
  | Integer _, Boolean -> sprintf "to_bool(%s)" e
  | Unsigned _, Boolean -> sprintf "to_bool(%s)" e
  | Signed _, Boolean -> sprintf "to_bool(%s)" e
  | Signed _, Integer _ -> sprintf "to_integer(%s)" e
  | Integer _, Integer _ -> e
  | Integer _, Char -> sprintf "to_char(%s)" e
  | Unsigned _, Char -> sprintf "to_char(%s)" e
  | Char, Integer _ -> sprintf "to_integer(%s)" e
  | Char, Unsigned n -> sprintf "conv_unsigned(%s,%d)" e n
  | t, t' when t=t' -> e
  | _, _ -> Misc.fatal_error "Vhdl.string_of_cast" (* should  not happen *)

let rec string_of_expr e =
  let paren level s = if level > 0 then "(" ^ s ^ ")" else s in
  let rec string_of level e =
    match e.Expr.e_desc, vhdl_type_of e.Expr.e_typ  with
    | Expr.EInt n, Unsigned s -> Printf.sprintf "to_unsigned(%d,%d)" n s
    | Expr.EInt n, Signed s -> Printf.sprintf "to_signed(%d,%d)" n s
    | Expr.EInt n, Boolean -> vhdl_string_of_bool (n > 0)
    | Expr.EInt n, Std_logic -> vhdl_string_of_bool (n > 0)
    | Expr.EInt n, _ -> string_of_int n
    | Expr.EFloat c, _ -> vhdl_string_of_float c
    | Expr.EChar c, _ -> vhdl_string_of_char c
    | Expr.EBool c, _ -> vhdl_string_of_bool c
    | Expr.EEnum c, _ -> enum_id c
    | Expr.EVar n, _ ->  n
    | Expr.EBinop (">>",e1,e2), _ -> "shift_right(" ^ string_of level e1 ^ "," ^ string_of_int_expr  e2 ^ ")"
    | Expr.EBinop ("<<",e1,e2), _ -> "shift_left(" ^ string_of level e1 ^ "," ^ string_of_int_expr e2 ^ ")"
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
    | Expr.EFapp (f,es), _ -> f ^ "(" ^ Utils.ListExt.to_string (string_of level) "," es ^ ")"
    | Expr.EArrExt es, _ -> "(" ^ Utils.ListExt.to_string (string_of level) "," es ^ ")"
    | Expr.EArr (a,idx), _ -> a ^ "(" ^ string_of level idx ^ ")"
    | Expr.EBit (a,idx), _ -> string_of_range a idx idx
    | Expr.EBitrange (a,hi,lo), _ -> string_of_range a hi lo
    | Expr.ERecord (r,f), _ -> r ^ "." ^ f
    | Expr.ECast (e,te), ty_e -> string_of_cast (vhdl_type_of e.e_typ) (vhdl_type_of te.te_typ) (string_of level e)
  in
  string_of 0 e

and string_of_int_expr e = match e.Expr.e_desc, vhdl_type_of (e.Expr.e_typ) with
    Expr.EInt n, _ -> string_of_int n
  | _, Integer _ -> string_of_expr e
  | _, _ -> "to_integer(" ^ string_of_expr e ^ ")"

and string_of_range id hi lo = id ^ "(" ^ string_of_int_expr hi ^ " downto " ^ string_of_int_expr lo ^ ")"

let string_of_action m a =
  let asn id = if List.mem_assoc id m.Cmodel.c_vars && Fsm.cfg.Fsm.act_sem = Sequential then " := " else " <= " in
  let invalid_lhs () = raise (Error (m.Cmodel.c_name, "invalid LHS for action \"" ^ Action.to_string a ^ "\"")) in
  let open Types in
  match a with
  | Action.Assign ({l_desc=Action.LhsVar id}, expr) ->
     id ^ asn id ^ string_of_expr expr
  | Action.Assign ({l_desc=Action.LhsArrInd (id,idx)}, expr) ->
     if List.mem_assoc id m.c_vars
     then id ^ "(" ^ string_of_expr idx ^ ")" ^ asn id ^ string_of_expr expr
     else invalid_lhs ()
  | Action.Assign ({l_desc=Action.LhsArrRange (id,idx1,idx2)}, expr) ->
     if List.mem_assoc id m.c_vars
     then string_of_range id idx1 idx2 ^ asn id ^ string_of_expr expr
     else invalid_lhs ()
  | Action.Assign ({l_desc=Action.LhsRField (id,fd)}, expr) ->
     if List.mem_assoc id m.c_vars
     then id ^ "." ^ fd ^ asn id ^ string_of_expr expr
     else invalid_lhs ()
  | Action.Emit id -> "notify_ev(" ^ id ^ "," ^ (string_of_int cfg.vhdl_ev_duration) ^ " " ^ cfg.vhdl_time_unit ^ ")"
  | Action.StateMove (id,s,s') -> "" (* should not happen *)

let string_of_condition (e,cs) =  
  let string_of_guard gexp  = string_of_expr gexp in
  match cs with
    [] -> Misc.fatal_error "Vhdl.string.of_condition"
  | _ -> Utils.ListExt.to_string string_of_guard " and " cs

let dump_action oc tab m a = fprintf oc "%s%s;\n" tab (string_of_action m a)

let dump_transition oc tab src clk m (is_first,needs_endif) (q',(cond,acts,_,_)) =
  match cond with
    _, [] -> 
       List.iter (dump_action oc tab m) acts;
       fprintf oc "%s%s <= %s;\n" tab cfg.vhdl_state_var q';
       (false,false)
  | _, _ ->
       fprintf oc "%s%s ( %s ) then\n" tab (if is_first then "if" else "elsif ") (string_of_condition cond);
       List.iter (dump_action oc (tab ^ "  ") m) acts;
       if q' <> src then fprintf oc "%s  %s <= %s;\n" tab cfg.vhdl_state_var q';
       (false,true)

let dump_sync_transitions oc src after clk m ts =
   let tab = "        " in
   let (_,needs_endif) = List.fold_left (dump_transition oc tab src clk m) (true,false) ts in
   if needs_endif then fprintf oc "        end if;\n"
     
let dump_state oc clk m { Cmodel.st_src=q; Cmodel.st_sensibility_list=evs; Cmodel.st_transitions=tss } =
  match tss with
    [ev,ts] -> dump_sync_transitions oc q false clk m ts
  | [] -> Misc.fatal_error ("VHDL: state " ^ q ^ " has no output transition")
  | _ -> Misc.not_implemented "VHDL: transitions involving multiple events"

let dump_state_case oc clk m c =
    fprintf oc "      when %s =>\n" c.Cmodel.st_src;
    dump_state oc clk m c

let dump_array_types oc vs =
  let array_types =
    List.fold_left
      (fun acc (_,ty) ->
        match ty with
        | TyArray (Types.Index.TiConst _, _) -> if not (List.mem ty acc) then ty::acc else acc
        | TyArray (_, _) -> Misc.fatal_error ("Vhdl.dump_array_types: " ^ Types.string_of_type ty)
        | _ -> acc)
      []
      vs in
  List.iter 
    (function
     | TyArray(Types.Index.TiConst sz,ty') as ty ->
        fprintf oc "  type %s is array (0 to %d) of %s;\n" (string_of_type ~type_marks:TM_Abbr ty) (sz-1) (string_of_type ty')
     | _ -> ())
      array_types

let dump_module_arch oc m =
  let open Cmodel in
  let modname = m.c_name in
  let clk_sig = match List.filter (function (_, TyEvent) -> true | _ -> false) m.c_inps with
    [] -> raise (Error (m.c_name, "no input event, hence no possible clock"))
  | [h,_] -> h
  | _ -> Misc.not_implemented (m.c_name ^ ": translation to VHDL of FSM with more than one input events") in
  fprintf oc "architecture RTL of %s is\n" modname;
  fprintf oc "  type t_%s is ( %s );\n" cfg.vhdl_state_var (Utils.ListExt.to_string (function s -> s) ", " m.c_states);
  fprintf oc "  signal %s: t_state;\n" cfg.vhdl_state_var;
  dump_array_types oc m.c_vars;
  if Fsm.cfg.Fsm.act_sem = Fsm.Synchronous then 
    List.iter
      (fun (id,ty) -> fprintf oc "  signal %s: %s;\n" id (string_of_type ~type_marks:TM_Abbr ty))
      m.c_vars;
  fprintf oc "begin\n";
  fprintf oc "  process(%s, %s)\n" cfg.vhdl_reset_sig clk_sig;
  if Fsm.cfg.Fsm.act_sem = Fsm.Sequential then 
    List.iter
      (fun (id,ty) -> fprintf oc "    variable %s: %s;\n" id (string_of_type ty))
      m.c_vars;
  fprintf oc "  begin\n";
  fprintf oc "    if ( %s='1' ) then\n" cfg.vhdl_reset_sig;
  fprintf oc "      %s <= %s;\n" cfg.vhdl_state_var (fst m.c_init);
  List.iter (dump_action oc "      " m) (snd m.c_init);
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
      Utils.ListExt.to_string
        (function (s,i) -> string_of_int i ^ " when " ^ cfg.vhdl_state_var ^ "=" ^ s)
        " else "
        (List.mapi (fun i s -> s,i) m.Cmodel.c_states) in
    fprintf oc "  %s <= %s;\n" cfg.vhdl_trace_state_var (int_of_vhdl_state m)
    end;
  fprintf oc "end architecture;\n"

let dump_module_intf kind oc m = 
  let open Cmodel in
  let modname = m.c_name in
  fprintf oc "%s %s %s\n" kind modname (if kind = "entity" then "is" else "");
  if m.c_params <> [] then begin
    let string_of_typed_item (id,ty) = id ^ ": " ^ string_of_type ty in
    fprintf oc "  generic (%s);\n" (Utils.ListExt.to_string string_of_typed_item "; " m.c_params)
    end;
  fprintf oc "  port(\n";
  List.iter (fun (id,ty) -> fprintf oc "        %s: in %s;\n" id (string_of_type ty)) m.c_inps;
  List.iter (fun (id,ty) -> fprintf oc "        %s: out %s;\n" id (string_of_type ty)) m.c_outps;
  List.iter (fun (id,ty) -> fprintf oc "        %s: inout %s;\n" id (string_of_type ty)) m.c_inouts;
  fprintf oc "        %s: in std_logic" cfg.vhdl_reset_sig;
  if cfg.vhdl_trace then fprintf oc ";\n        %s: out integer\n" cfg.vhdl_trace_state_var else fprintf oc "\n";
  fprintf oc "        );\n";
  fprintf oc "end %s;\n" kind

(* Dumping input generator processes *)

let string_of_time t = string_of_int t ^ " " ^ cfg.vhdl_time_unit

let dump_sporadic_inp_process oc id ts =
       fprintf oc "    type t_dates is array ( 0 to %d ) of time;\n" (List.length ts-1);
       fprintf oc "    constant dates : t_dates := ( %s );\n" (Utils.ListExt.to_string string_of_time ", " ts);
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
         (string_of_time p)
         (string_of_time t1)
               (string_of_time t2);
       fprintf oc "    variable t : time := 0 %s;\n" cfg.vhdl_time_unit;
       fprintf oc "    begin\n";
       fprintf oc "      %s <= '0';\n" id;
       fprintf oc "      wait for periodic.t1;\n";
       fprintf oc "      t := t + periodic.t1;\n";
       fprintf oc "      while ( t < periodic.t2 ) loop\n";
       fprintf oc "        %s <= '1';\n" id;
       fprintf oc "        wait for periodic.period/2;\n";
       fprintf oc "        %s <= '0';\n" id;
       fprintf oc "        wait for periodic.period/2;\n";
       fprintf oc "        t := t + periodic.period;\n";
       fprintf oc "      end loop;\n";
       fprintf oc "      wait;\n"
  
let dump_vc_inp_process oc ty id vcs =
       let ty = vhdl_type_of ty in
       let string_of_vc (t,v) = "(" ^ string_of_int t ^ " " ^ cfg.vhdl_time_unit ^ "," ^ string_of_value v ^ ")" in
       fprintf oc "    type t_vc is record date: time; val: %s; end record;\n" (string_of_vhdl_type ty);
       fprintf oc "    type t_vcs is array ( 0 to %d ) of t_vc;\n" (List.length vcs-1);
       fprintf oc "    constant vcs : t_vcs := ( %s%s );\n"
               (if List.length vcs = 1 then "others => " else "")  (* GHDL complains when initializing a 1-array *)
               (Utils.ListExt.to_string string_of_vc ", " vcs);
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
  let open Static in
  fprintf oc "  inp_%s: process\n" id;
  begin match desc with
    (* | MInp ({sd_comprehension=Sporadic ts}, _) -> dump_sporadic_inp_process oc id ts
     * | MInp ({sd_comprehension=Periodic (p,t1,t2)}, _) -> dump_periodic_inp_process oc id (p,t1,t2)
     * | MInp ({sd_comprehension=ValueChange []}, _) -> ()
     * | MInp ({sd_comprehension=ValueChange vcs}, _) -> dump_vc_inp_process oc ty id vcs  *)
    | MInp (Sporadic ts, _) -> dump_sporadic_inp_process oc id ts
    | MInp (Periodic (p,t1,t2), _) -> dump_periodic_inp_process oc id (p,t1,t2)
    | MInp (ValueChange [], _) -> ()
    | MInp (ValueChange vcs, _) -> dump_vc_inp_process oc ty id vcs 
    | _ -> Misc.fatal_error "Vhdl.dump_inp_module_arch" (* should not happen *) end;
  fprintf oc "  end process;\n"

(* Dumping toplevel module *)

let dump_libraries oc =
  fprintf oc "library ieee;\n";
  fprintf oc "use ieee.std_logic_1164.all;	   \n";
  if cfg.vhdl_use_numeric_std then fprintf oc "use ieee.numeric_std.all;\n";
  fprintf oc "library %s;\n" cfg.vhdl_support_library;
  fprintf oc "use %s.%s.all;\n" cfg.vhdl_support_library cfg.vhdl_support_package;
  fprintf oc "\n"

let dump_toplevel_intf prefix kind oc m =
  let top_name = prefix ^ "_top" in
  fprintf oc "%s %s is\n" kind top_name;
  fprintf oc "  port(\n";
  let open Static in
  List.iter
   (function (id,(ty,_)) ->
     fprintf oc "        %s: in %s;\n" id (string_of_type ty))
   m.m_inputs;
  List.iter
   (function (id,(ty,_)) ->
     fprintf oc "        %s: out %s;\n" id (string_of_type ty))
   m.m_outputs;
  if cfg.vhdl_trace then
    List.iter
      (function f -> fprintf oc "        %s: out integer;\n" (f.Fsm.f_name ^ "_state"))
      m.m_fsms;  
  fprintf oc "        %s: in std_logic\n" cfg.vhdl_reset_sig;
  fprintf oc "        );\n";
  fprintf oc "end %s;\n" kind;
  fprintf oc "\n"
  
let dump_toplevel_impl prefix fname m =
  let oc = open_out fname in
  let top_name = prefix ^ "_top" in
  let open Static in
  let modname n = n in
  dump_libraries oc;
  if need_globals m then fprintf oc "use work.%s.all;\n" cfg.vhdl_globals_name;
  dump_toplevel_intf prefix "entity" oc m;
  fprintf oc "architecture struct of %s is\n" top_name;
  fprintf oc "\n";
  (* FSMs *)
  List.iter
    (fun f -> dump_module_intf "component" oc (Cmodel.c_model_of_fsm_inst m f))
    m.m_fsms;
  fprintf oc "\n";
  (* Shared signals *)
  List.iter
   (function (id,(ty,_)) ->
     fprintf oc "signal %s: %s;\n" id (string_of_type ty))
   m.m_shared;
  fprintf oc "\n";
  fprintf oc "begin\n";
  (* Instanciated components *)
  List.iteri
    (fun i f ->
      let m = Cmodel.c_model_of_fsm_inst m f in
      let n = modname f.Fsm.f_name in
      let actual_name (id,_) = f.f_l2g id in
      fprintf oc "  %s%d: %s port map(%s%s);\n"
        m.c_name
        (i+1)
        n
        (Utils.ListExt.to_string (fun id -> id) ","
           (List.map actual_name (m.c_inps @  m.c_outps @ m.c_inouts) @ [cfg.vhdl_reset_sig]))
        (if cfg.vhdl_trace then "," ^ f.Fsm.f_name ^ "_state" else ""))
    m.m_fsms;
  fprintf oc "end architecture;\n";
  Logfile.write fname;
  close_out oc

let dump_toplevel ?(name="") ?(dir="./vhdl") m =
  let prefix = match name with "" -> cfg.vhdl_tb_prefix | p -> p in
  dump_toplevel_impl prefix (dir ^ "/" ^ prefix ^ "_top.vhd") m

(* Dumping the testbench *)

let dump_testbench_impl prefix fname m =
  let oc = open_out fname in
  let tb_name = prefix ^ "_tb" in
  let top_name = prefix ^ "_top" in
  let open Static in
  dump_libraries oc;
  if need_globals m then fprintf oc "use work.%s.all;\n" cfg.vhdl_globals_name;
  fprintf oc "entity %s is\n" tb_name;
  fprintf oc "end entity;\n";
  fprintf oc "\n";
  fprintf oc "architecture struct of %s is\n" tb_name;
  fprintf oc "\n";
  dump_toplevel_intf prefix "component" oc m;
  (* Signals *)
  List.iter
   (function (id,(ty,_)) ->
     fprintf oc "signal %s: %s;\n" id (string_of_type ty))
   (m.m_inputs @ m.m_outputs);
  fprintf oc "signal %s: std_logic;\n" cfg.vhdl_reset_sig;
  if cfg.vhdl_trace then
    List.iter
      (function f -> fprintf oc "signal %s: integer;\n" (f.Fsm.f_name ^ "_state"))
      m.m_fsms;  
  fprintf oc "\n";
  fprintf oc "begin\n";
  fprintf oc "\n";
  (* Input generators *)
  List.iter (dump_input_process oc) m.m_inputs;
  (* Reset generator *)
  fprintf oc "  reset: process\n";
  fprintf oc "  begin\n";
  fprintf oc "    %s <= '1';\n" cfg.vhdl_reset_sig;
  fprintf oc "    wait for %d %s;\n" cfg.vhdl_reset_duration cfg.vhdl_time_unit;
  fprintf oc "    %s <= '0';\n" cfg.vhdl_reset_sig;
  fprintf oc "    wait for %d %s;\n" cfg.vhdl_stop_time cfg.vhdl_time_unit;
  fprintf oc "    wait;\n";
  fprintf oc "  end process;\n";
  fprintf oc "\n";
  (* Toplevel instanciation  *)
  fprintf oc "  Top: %s port map(%s%s,%s);\n"
    top_name
    (Utils.ListExt.to_string (fun id -> id) "," (List.map fst (m.m_inputs @  m.m_outputs)))
    (if cfg.vhdl_trace then "," ^ Utils.ListExt.to_string (function f -> f.Fsm.f_name ^ "_state") "," m.m_fsms else "")
    cfg.vhdl_reset_sig;
  fprintf oc "\n";
  fprintf oc "end architecture;\n";
  Logfile.write fname;
  close_out oc

let dump_testbench ?(name="") ?(dir="./vhdl") m =
  let prefix = match name with "" -> cfg.vhdl_tb_prefix | p -> p in
  dump_testbench_impl prefix (dir ^ "/" ^ prefix ^ "_tb.vhd") m

(* Dumping models *)

let dump_fsm_model ?(prefix="") ?(dir="./vhdl") fsm =
  let f = Cmodel.c_model_of_fsm_model fsm in
  let prefix = match prefix with "" -> fsm.Fsm.fm_name | p -> p in
  let fname = dir ^ "/" ^ prefix ^ ".vhd" in
  let oc = open_out fname in
  fprintf oc "library ieee;\n";
  fprintf oc "use ieee.std_logic_1164.all;\n";
  if cfg.vhdl_use_numeric_std then fprintf oc "use ieee.numeric_std.all;\n";
  fprintf oc "library %s;\n" cfg.vhdl_support_library;
  fprintf oc "use %s.%s.all;\n" cfg.vhdl_support_library cfg.vhdl_support_package;
  fprintf oc "\n";
  dump_module_intf "entity" oc f;
  fprintf oc "\n";
  dump_module_arch oc f;
  Logfile.write fname;
  close_out oc

let dump_fsm_inst ?(prefix="") ?(dir="./vhdl") m fsm =
  let f = Cmodel.c_model_of_fsm_inst m fsm in
  let prefix = match prefix with "" -> fsm.Fsm.f_name | p -> p in
  let fname = dir ^ "/" ^ prefix ^ ".vhd" in
  let oc = open_out fname in
  fprintf oc "library ieee;\n";
  fprintf oc "use ieee.std_logic_1164.all;\n";
  if cfg.vhdl_use_numeric_std then fprintf oc "use ieee.numeric_std.all;\n";
  fprintf oc "library %s;\n" cfg.vhdl_support_library;
  fprintf oc "use %s.%s.all;\n" cfg.vhdl_support_library cfg.vhdl_support_package;
  if need_globals m then fprintf oc "use work.%s.all;\n" cfg.vhdl_globals_name;
  fprintf oc "\n";
  dump_module_intf "entity" oc f;
  fprintf oc "\n";
  dump_module_arch oc f;
  Logfile.write fname;
  close_out oc

let dump_model ?(dir="./vhdl") m =
  List.iter (dump_fsm_inst ~dir:dir m) m.Static.m_fsms;
  dump_toplevel ~dir:dir m

(* Dumping global type definitions, functions and constants *)

let dump_record_type_defn oc name fields = 
  fprintf oc "  type %s is record\n" name;
  List.iter
    (function (n,t) -> fprintf oc "    %s: %s;\n" n (string_of_type t))
    fields;
  fprintf oc "  end record;\n"

let dump_enum_type_defn oc name vs = 
  fprintf oc "  type %s is (%s);\n" name (Utils.ListExt.to_string enum_id "," vs)

let dump_global_type_defn oc (name,ty) =
  let dump_cond_fn_decl oc name =
    fprintf oc "  function cond(e1: boolean; e2: %s; e3: %s) return %s;\n" name name name in
  match ty with
  | TyEnum (nm,cs) ->
     dump_enum_type_defn oc (Types.string_of_name nm) cs;
     dump_cond_fn_decl oc (Types.string_of_name nm)
  | TyRecord (nm,fs) ->
     dump_record_type_defn oc (Types.string_of_name nm) fs;
     dump_cond_fn_decl oc (Types.string_of_name nm)
  | _ -> ()

let rec dump_globals ?(name="") ?(dir="./vhdl") m =
  let prefix = match name with "" -> cfg.vhdl_globals_name | p -> p in
  let fname = dir ^ "/" ^ prefix ^ ".vhd" in
  let oc = open_out fname in
  fprintf oc "library ieee;\n";
  fprintf oc "use ieee.std_logic_1164.all;\n";
  if cfg.vhdl_use_numeric_std then fprintf oc "use ieee.numeric_std.all;\n";
  fprintf oc "library %s;\n" cfg.vhdl_support_library;
  fprintf oc "use %s.%s.all;\n\n" cfg.vhdl_support_library cfg.vhdl_support_package;
  dump_globals_intf oc prefix m; 
  fprintf oc "\n";
  dump_globals_impl oc prefix m;
  Logfile.write fname;
  close_out oc

and dump_globals_intf oc package_name m =
  fprintf oc "package %s is\n" package_name;
  List.iter (dump_global_type_defn oc) m.Static.m_types; 
  dump_array_types oc (List.map (fun (id,(ty,_)) -> id,ty) (m.Static.m_consts @ m.Static.m_fns));
  List.iter (dump_global_sig oc) (m.Static.m_consts @ m.Static.m_fns);
  fprintf oc "end %s;\n" package_name

and dump_global_sig oc (id,(ty,gd)) = match gd, ty with
| Static.MConst v, _ -> 
    fprintf oc "  constant %s : %s := %s;\n"
      id
      (string_of_type ty) 
      (string_of_value v) 
| Static.MFun (args, body), Types.TyArrow(TyProduct ts, tr) -> 
    fprintf oc "  function %s(%s) return %s;\n"
      id
      (Utils.ListExt.to_string string_of_fn_arg "; "  (List.combine args ts))
      (string_of_type ~type_marks:TM_None tr) 
| _ -> ()

and string_of_fn_arg (id,ty) = id ^ ":" ^ (string_of_type ty)

and dump_globals_impl oc package_name m =
  fprintf oc "package body %s is\n" package_name;
  List.iter (dump_global_type_fns oc) m.Static.m_types;
  List.iter (dump_global_fn_impl oc) m.Static.m_fns;
  fprintf oc "end %s;\n" package_name

and dump_global_fn_impl oc (id,(ty,gd)) = match gd, ty with
| Static.MFun (args, body), Types.TyArrow(TyProduct ts, tr) -> 
    fprintf oc "function %s(%s) return %s is\n"
      id
      (Utils.ListExt.to_string string_of_fn_arg "; "  (List.combine args ts))
      (string_of_type ~type_marks:TM_None tr) ;
    fprintf oc "  begin\n";
    fprintf oc "    return %s;\n" (string_of_expr body);
    fprintf oc "  end;\n\n"
| _ -> ()

and dump_global_type_fns oc (name,ty) =
  match ty with
  | TyEnum (nm,_) 
  | TyRecord (nm,_) ->
      let name = Types.string_of_name nm in
      fprintf oc "function cond(e1: boolean; e2: %s; e3: %s) return %s is\n" name name name;
      fprintf oc "  begin\n";
      fprintf oc "    if e1 then return e2; else return e3; end if;\n";
      fprintf oc "  end;\n"
  | _ -> ()

(* Dumping Makefile *)

let dump_makefile ?(name="") ?(dir="./vhdl") m =
  let templ_fname = cfg.vhdl_lib_dir ^ "/etc/Makefile.vhdl.templ" in
  if Sys.file_exists templ_fname then begin
      let prefix = match name with "" -> cfg.vhdl_tb_prefix | p -> p in
      let tb_name = prefix ^ "_tb" in
      let top_name = prefix ^ "_top" in
      let fname = dir ^ "/" ^ "Makefile" in
      let oc = open_out fname in
      fprintf oc "LIBDIR=%s\n\n" cfg.vhdl_lib_dir;
      let ic = open_in templ_fname in
      let dump_opt, dump_fmt =
        begin match cfg.vhdl_dump_format with
        | Vcd -> "--vcd", "vcd"
        | Ghw -> "--wave", "ghw"
      end in
      Misc.copy_with_subst ["%%MAIN%%", tb_name; "%%DUMPOPT%%", dump_opt; "%%DUMPFMT%%", dump_fmt] ic oc;
      close_in ic;
      fprintf oc "\n";
      let modname suff f = f.Fsm.f_name ^ suff in
      let open Static in
      fprintf oc "%s: %s %s %s.vhd\n"
        tb_name
        (if need_globals m then cfg.vhdl_globals_name ^ ".vhd" else "")
        (Utils.ListExt.to_string (modname ".vhd") " " m.m_fsms)
        tb_name;
      if need_globals m then 
        fprintf oc "\t$(GHDL) -a $(GHDLOPTS) %s.vhd\n" cfg.vhdl_globals_name;
      List.iter
        (function f -> fprintf oc "\t$(GHDL) -a $(GHDLOPTS) %s\n" (modname ".vhd" f))
        m.m_fsms;
      fprintf oc "\t$(GHDL) -a $(GHDLOPTS) %s.vhd\n" top_name;
      fprintf oc "\t$(GHDL) -a $(GHDLOPTS) %s.vhd\n" tb_name;
      fprintf oc "\t$(GHDL) -e $(GHDLOPTS) %s\n" tb_name;
      Logfile.write fname;
      close_out oc
    end
  else
    Misc.warning (Printf.sprintf "No file %s. No Makefile generated." templ_fname)

(* Check whether a model can be translated *)

let check_allowed_fsm_model, check_allowed_fsm_inst = 
  let check_inputs name = function
    | [_] -> ()
    | _ -> Misc.not_implemented ("Vhdl: FSM " ^ name ^ " has more than one input event") in
  let check_outputs name = function
    | [] -> ()
    | _ -> Misc.not_implemented ("Vhdl: FSM " ^ name ^ " has output event(s)") in
  let check_rtl name is_rtl = 
    if Fsm.cfg.Fsm.act_sem = Fsm.Synchronous && not is_rtl then
      Misc.warning ("Vhdl: FSM " ^ name ^ " has non-RTL transition(s). This may cause incorrect behavior when using the synchronous interpretation of actions.") in
  (fun f -> check_inputs f.fm_name (Fsm.input_events_of_model f); 
            check_outputs f.fm_name (Fsm.output_events_of_model f); 
            check_rtl f.fm_name (Fsm.is_rtl_model f)), 
  (fun f -> check_inputs f.f_name (Fsm.input_events_of_inst f); 
            check_outputs f.f_name (Fsm.output_events_of_inst f); 
            check_rtl f.f_name (Fsm.is_rtl_inst f))

let check_allowed_models ms =
  List.iter check_allowed_fsm_model ms

let check_allowed_system s = 
  let open Static in 
  let valid_shared (id, (ty, desc)) = match desc with
      MShared ([_], _) ->
       begin match ty with
       | TyInt _ | TyBool | TyFloat | TyChar | TyRecord _ -> ()
       | _ ->  Misc.not_implemented ("Vhdl: " ^ id ^ ": shared signal with type=" ^ Types.string_of_type ty)
       end
    | MShared (_,_)  -> Misc.not_implemented ("Vhdl: " ^ id ^ ": shared signal with more than one writer")
    | _ -> Misc.not_implemented ("Vhdl: " ^ id ^ ": unsupported kind of shared signal") in
  List.iter valid_shared s.m_shared;
  List.iter check_allowed_fsm_inst s.m_fsms
    
