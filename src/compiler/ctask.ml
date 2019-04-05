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

(* CTask backend *)

open Fsm
open Types
open Printf
open Cmodel

type ctsk_config = {
  state_var_name: string;
  recvd_ev_name: string;
  globals_name: string;
  }

let cfg = {
  state_var_name = "state";
  recvd_ev_name = "received";
  globals_name = "globals";
  }

exception Ctask_error of string * string  (* where, msg *)

let need_globals m = m.Static.m_types <> [] || m.Static.m_fns <> [] || m.Static.m_consts <> [] 

let string_of_type t = match t with 
  | TyEvent -> "event"
  | TyBool -> "bool"
  | TyEnum (nm, _) when Types.is_lit_name nm -> Types.string_of_name nm
  | TyEnum (_, cs) -> "enum {" ^ Utils.ListExt.to_string (function c -> c) "," cs ^ "}"
  | TyInt _ -> "int"
  | TyFloat -> "float"
  | TyChar -> "char"
  | TyRecord (nm, _) when Types.is_lit_name nm ->  Types.string_of_name nm
  | TyRecord (_, fs) -> "struct {" ^ Utils.ListExt.to_string (function (f,t) -> f ^ ":" ^ string_of_type t) ";" fs ^ "}"
  | _ -> Misc.fatal_error "Ctask.string_of_type"

let string_of_array_size sz = match sz with
  | Types.Index.TiConst n -> string_of_int n
  | _ -> Misc.fatal_error "Systemc.string_of_array_size"

let string_of_typed_item (id,ty) = match ty with 
  | TyArray (sz,ty') -> string_of_type ty' ^ " " ^ id ^ "[" ^ string_of_array_size sz ^ "]"
  | _ -> string_of_type ty ^ " " ^ id

let string_of_char c = "'" ^ String.make 1 c ^ "'"

let rec string_of_value v = match v.Expr.v_desc with
  Expr.Val_int i -> string_of_int i
| Expr.Val_float b -> string_of_float b
| Expr.Val_char b -> string_of_char b
| Expr.Val_bool b -> string_of_bool b
| Expr.Val_enum e -> e
| Expr.Val_fn _ -> "<fun>"
| Expr.Val_unknown -> "<unknown>"
| Expr.Val_none -> "<none>"
| Expr.Val_array vs -> "{" ^ Utils.ListExt.to_string string_of_value "," (Array.to_list vs) ^ "}"
| Expr.Val_record fs -> "{" ^ Utils.ListExt.to_string string_of_field "," fs ^ "}"

and string_of_field (n,v) = string_of_value v

(*
<case> ::= [ <async_transitions> ] [ sync_transitions ] "break"
<async_transitions> ::=
           "if" <async_conds_1> "{" <async_acts_1> "}"
      "else if" <async_conds_2> "{" <async_acts_2> "}"
      ...
      "else if" <async_conds_m> "{" <async_acts_m> "}"
      "else"
<sync_transitions> ::=
      "wait_ev(ev)" | "r = wait_evs(evs)" 
           "if" <sync_conds_1> "{" <sync_acts_1> "}"
      "else if" <sync_conds_2> "{" <sync_acts_2> "}"
      ...
      "else if" <sync_conds_m> "{" <sync_acts_m> "}"
*)
 
let rec string_of_expr e =
  let paren level s = if level > 0 then "(" ^ s ^ ")" else s in
  let rec string_of level e = match e.Expr.e_desc with
    Expr.EInt c -> string_of_int c
  | Expr.EFloat c -> string_of_float c
  | Expr.EChar c -> string_of_char c
  | Expr.EBool c -> string_of_bool c
  | Expr.EEnum c -> c
  | Expr.EVar n -> n
  | Expr.EBinop (op,e1,e2) -> paren level (string_of (level+1) e1 ^ string_of_op op ^ string_of (level+1) e2)
  | Expr.ECond (e1,e2,e3) ->
     paren level (string_of (level+1) e1 ^ " ? " ^ string_of (level+1) e2 ^ " : " ^ string_of (level+1) e3)
  | Expr.EFapp (f,es) -> f ^ "(" ^ Utils.ListExt.to_string (string_of level) "," es ^ ")"
  | Expr.EArrExt es -> "{" ^ Utils.ListExt.to_string (string_of level) "," es ^ "}"
  | Expr.EArr (a,idx) -> a ^ "[" ^ string_of level idx ^ "]"
  | Expr.ERecord (a,f) -> a ^ "." ^ f
  | Expr.EBit (a,idx) -> a ^ "[" ^ string_of level idx ^ "]"
  | Expr.EBitrange (a,hi,lo) -> a ^ "[" ^ string_of level hi ^ ":" ^ string_of level lo ^ "]" 
  | Expr.ECast (e,te) -> "(" ^ string_of_type te.te_typ ^ ")(" ^ string_of level e ^ ")" in
  string_of 0 e

and string_of_op = function
    "=" -> "=="
  | "mod" -> "%"
  | "+." -> "+" 
  | "-." -> "-" 
  | "*." -> "*" 
  | "/." -> "/" 
  | op ->  op

let string_of_guard exp = string_of_expr exp

let string_of_lhs l = match l.Action.l_desc with
  | Action.LhsVar id -> id
  | Action.LhsArrInd (id,idx) -> id ^ "[" ^ string_of_expr idx ^ "]"
  | Action.LhsArrRange (id,hi,lo) -> id ^ "[" ^ string_of_expr hi ^ ":" ^ string_of_expr lo ^ "]"
  | Action.LhsRField (id,f) -> id ^ "." ^ f
                          
let string_of_action a = match a with
    | Action.Assign (lhs, expr) -> string_of_lhs lhs ^ "=" ^ string_of_expr expr
    | Action.Emit id -> "notify_ev(" ^ id ^ ")"
    | Action.StateMove (id,s,s') -> "" (* should not happen *)

let dump_action oc tab a = fprintf oc "%s%s;\n" tab (string_of_action a)

let dump_transition oc tab is_first src (q',((evs,guards),acts,_,_)) =
  match guards with
  | [] ->
       List.iter (dump_action oc tab) acts;
       if q' <> src then fprintf oc "%s%s = %s;\n" tab cfg.state_var_name q'
  | _  -> 
       fprintf oc "%s%sif ( %s ) {\n"
        tab
        (if is_first then "" else "else ")
        (Utils.ListExt.to_string string_of_guard " && " guards);
       List.iter (dump_action oc (tab ^ "  ")) acts;
       if q' <> src then fprintf oc "%s  %s = %s;\n" tab cfg.state_var_name q';
       fprintf oc "%s  }\n" tab

let dump_ev_transition oc tab src (ev,ts) = 
  fprintf oc "%scase %s:\n" tab ev;
  Utils.ListExt.iter_fst (fun is_first t -> dump_transition oc (tab^"  ") is_first src t) ts;
  fprintf oc "%s  break;\n" tab

let dump_transitions oc src after evs tss =
   if after then fprintf oc "      else {\n";
   let tab = if after then "        " else "      " in
   begin match tss with 
      [] -> ()  (* no wait in this case *)
    | [ev,ts] ->
       fprintf oc "%swait_ev(%s);\n" tab ev;
       Utils.ListExt.iter_fst (fun is_first t -> dump_transition oc tab is_first src t) ts;
    | _ ->
       fprintf oc "%s%s = wait_evs(%s);\n" tab cfg.recvd_ev_name (Utils.ListExt.to_string (function id -> id) "," evs);
       fprintf oc "%sswitch ( %s ) {\n" tab cfg.recvd_ev_name;
       List.iter (dump_ev_transition oc (tab^"  ") src) tss;
       fprintf oc "%s  }\n" tab
   end;
   if after then fprintf oc "      }\n"
     
let dump_state_case oc m { st_src=q; st_sensibility_list=evs; st_transitions=tss } =
  fprintf oc "    case %s:\n" q;
  dump_transitions oc q false evs tss;
  fprintf oc "      break;\n"

let dump_state oc m { st_src=q; st_sensibility_list=evs; st_transitions=tss } =
  dump_transitions oc q false evs tss

let dump_model fname m =
  let oc = open_out fname in
  let modname = String.capitalize_ascii m.c_name in
  (* let string_of_ival v =
   *   let open Expr in
   *   match v.v_desc with
   *   | Val_unknown -> ""
   *   | Val_array vs when List.for_all (function {v_desc=Val_unknown} -> true | _ -> false) (Array.to_list vs) -> ""
   *   | Val_record fs when List.for_all (function {v_desc=Val_unknown} -> true | _ -> false) (List.map snd fs) -> ""
   *   | _ -> " = " ^ string_of_value v in *)
  fprintf oc "task %s%s(\n"
    modname
    (match m.c_params with [] -> "" | ps -> "<" ^ Utils.ListExt.to_string string_of_typed_item "," ps ^ ">");
  List.iter (fun (id,ty) -> fprintf oc "  in %s;\n" (string_of_typed_item (id,ty))) m.c_inps;
  List.iter (fun (id,ty) -> fprintf oc " out %s;\n" (string_of_typed_item (id,ty))) m.c_outps;
  List.iter (fun (id,ty) -> fprintf oc " inout %s;\n" (string_of_typed_item (id,ty))) m.c_inouts;
  fprintf oc "  )\n";
  fprintf oc "{\n";
  (* List.iter (fun (id,(ty,iv)) -> fprintf oc "  %s%s;\n" (string_of_typed_item (id,ty)) (string_of_ival iv)) m.c_vars; *)
  List.iter (fun (id,ty) -> fprintf oc "  %s;\n" (string_of_typed_item (id,ty))) m.c_vars;
  if List.length m.c_states > 1 then 
    fprintf oc "  %s %s = %s;\n"
      (string_of_type (Types.TyEnum (Types.new_name_var(),m.c_states)))
      cfg.state_var_name
      (fst m.c_init);
  if List.exists (function c -> List.length c.st_sensibility_list > 1) m.c_body then
      fprintf oc "  event %s;\n" cfg.recvd_ev_name;
  List.iter (dump_action oc "  ") (snd m.c_init);
  fprintf oc "  while ( 1 ) {\n";
  begin match m.c_body with
    [] -> () (* should not happen *)
  | [q] -> dump_state_case oc m q 
  | qs -> 
      fprintf oc "    switch ( %s ) {\n" cfg.state_var_name;
      List.iter (dump_state_case oc m) m.c_body;
      fprintf oc "    }\n"
  end;
  fprintf oc "  }\n";
  fprintf oc "};\n";
  Logfile.write fname;
  close_out oc

let dump_fsm_model ?(prefix="") ?(dir="./ctask") m f =
  let prefix = match prefix with "" -> f.Fsm.fm_name | p -> p in
  dump_model (dir ^ "/" ^ prefix ^ ".c") (Cmodel.c_model_of_fsm_model f)

let dump_fsm_inst ?(prefix="") ?(dir="./ctask") m f =
  let prefix = match prefix with "" -> f.Fsm.f_name | p -> p in
  dump_model (dir ^ "/" ^ prefix ^ ".c") (Cmodel.c_model_of_fsm_inst m f)

(* Dumping global type declarations, functions and constants *)

let dump_record_type_defn oc name fields = 
  let mk f sep fs = Utils.ListExt.to_string f sep fs in
  fprintf oc "typedef struct { %s } %s;\n"
    (mk (function (n,t) -> string_of_type t ^ " " ^ n ^ ";") " " fields)
    name

let dump_enum_type_defn oc name vs = 
  fprintf oc "typedef enum { %s } %s;\n"
    (Utils.ListExt.to_string Misc.id "," vs)
    name

let dump_global_type_defn oc (name,ty) = match ty with
  | TyEnum (nm,cs) -> dump_enum_type_defn oc (Types.string_of_name nm) cs
  | TyRecord (nm,fs) -> dump_record_type_defn oc (Types.string_of_name nm) fs
  | _ -> ()

let dump_global_fn_intf oc (id,(ty,gd)) = match gd, ty with
| Static.MConst _, TyArray(sz,ty') ->
     fprintf oc "extern %s %s[%s];\n" (string_of_type ty') id (string_of_array_size sz)
| Static.MConst _, _ ->
     fprintf oc "%s %s;\n" (string_of_type ty) id 
| Static.MFun (args, body), Types.TyArrow(TyProduct ts, tr) -> 
    fprintf oc "%s %s (%s);\n"
      (string_of_type tr)
      id 
      (Utils.ListExt.to_string (function (a,t) -> string_of_type t ^ " " ^ a) "," (List.combine args ts)) 
| _ -> ()

let dump_global_fn_impl oc (id,(ty,gd)) = match gd, ty with
| Static.MConst v, TyArray(sz,ty') ->
     fprintf oc "%s %s[%s] = %s;\n" (string_of_type ty') id (string_of_array_size sz) (string_of_value v)
| Static.MConst v, _ -> 
    fprintf oc "%s %s = %s;\n" (string_of_type ty) id (string_of_value v)
| Static.MFun (args, body), Types.TyArrow(TyProduct ts, tr) -> 
    fprintf oc "%s %s (%s) { return %s; }\n"
      (string_of_type tr)
      id 
      (Utils.ListExt.to_string (function (a,t) -> string_of_type t ^ " " ^ a) "," (List.combine args ts)) 
      (string_of_expr body)
| _ -> ()

let dump_globals_intf dir prefix m =
  let fname = dir ^ "/" ^ prefix ^ ".h" in
  let oc = open_out fname in
  Printf.fprintf oc "#ifndef _%s_h\n" cfg.globals_name;
  Printf.fprintf oc "#define _%s_h\n\n" cfg.globals_name;
  List.iter (dump_global_type_defn oc) m.Static.m_types; 
  List.iter (dump_global_fn_intf oc) (m.Static.m_consts @ m.Static.m_fns);
  Printf.fprintf oc "\n#endif\n";
  Logfile.write fname;
  close_out oc

let dump_globals_impl dir prefix m =
  let fname = dir ^ "/" ^ prefix ^ ".c" in
  let oc = open_out fname in
  Printf.fprintf oc "#include \"%s.h\"\n\n" prefix;
  List.iter (dump_global_fn_impl oc) (m.Static.m_consts @ m.Static.m_fns);
  Logfile.write fname;
  close_out oc

let dump_globals ?(name="") ?(dir="./ctask") m =
  let prefix = match name with "" -> cfg.globals_name | p -> p in
  dump_globals_intf dir prefix m;
  if m.Static.m_fns <> [] || m.Static.m_consts <> [] then dump_globals_impl dir prefix m

(* Check whether a model can be translated *)

let check_allowed m =
  match Fsm.cfg.Fsm.act_sem with
  | Fsm.Sequential -> ()
  | Fsm.Synchronous -> Misc.not_implemented "CTask: synchronous actions"
