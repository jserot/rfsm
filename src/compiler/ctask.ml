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

open Utils
open Fsm
open Types
open Printf
open Cmodel

type ctsk_config = {
  state_var_name: string;
  recvd_ev_name: string;
  }

let cfg = {
  state_var_name = "state";
  recvd_ev_name = "received";
  }

let string_of_type t = match t with 
  | TyEvent -> "event"
  | TyBool -> "bool"
  | TyEnum cs -> "enum {" ^ ListExt.to_string (function c -> c) "," cs ^ "}"
  | TyInt _ -> "int"
  | TyFloat -> "float"
  | _ -> Error.fatal_error "Ctask.string_of_type"

let string_of_array_size sz = match sz with
  | Types.Index.TiConst n -> string_of_int n
  | _ -> failwith "Systemc.string_of_array_size"

let string_of_typed_item (id,ty) = match ty with 
  | TyArray (sz,ty') -> string_of_type ty' ^ " " ^ id ^ "[" ^ string_of_array_size sz ^ "]"
  | _ -> string_of_type ty ^ " " ^ id

let rec string_of_value v = match v with
  Expr.Val_int i -> string_of_int i
| Expr.Val_float b -> string_of_float b
| Expr.Val_bool b -> string_of_bool b
| Expr.Val_enum s -> s
| Expr.Val_fn _ -> "<fun>"
| Expr.Val_unknown -> "<unknown>"
| Expr.Val_none -> "<none>"
| Expr.Val_array vs -> "{" ^ ListExt.to_string string_of_value "," (Array.to_list vs) ^ "}"

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
  let rec string_of level e = match e with
    Expr.EInt c -> string_of_int c
  | Expr.EFloat c -> string_of_float c
  | Expr.EBool c -> string_of_bool c
  | Expr.EEnum c -> c
  | Expr.EVar n -> n
  | Expr.EBinop (op,e1,e2) -> paren level (string_of (level+1) e1 ^ string_of_op op ^ string_of (level+1) e2)
  | Expr.ECond (e1,e2,e3) ->
     paren level (string_of (level+1) e1 ^ " ? " ^ string_of (level+1) e2 ^ " : " ^ string_of (level+1) e3)
  | Expr.EFapp (f,es) -> f ^ "(" ^ ListExt.to_string (string_of level) "," es ^ ")"
  | Expr.EArr (a,idx) -> a ^ "[" ^ string_of level idx ^ "]" in
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

let string_of_lhs = function
  | Action.Var0 id -> id
  | Action.Var1 (id,idx) -> id ^ "[" ^ string_of_expr idx ^ "]"
                          
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
        (ListExt.to_string string_of_guard " && " guards);
       List.iter (dump_action oc (tab ^ "  ")) acts;
       if q' <> src then fprintf oc "%s  %s = %s;\n" tab cfg.state_var_name q';
       fprintf oc "%s  }\n" tab

let dump_ev_transition oc tab src (ev,ts) = 
  fprintf oc "%scase %s:\n" tab ev;
  ListExt.iter_fst (fun is_first t -> dump_transition oc (tab^"  ") is_first src t) ts;
  fprintf oc "%s  break;\n" tab

let dump_transitions oc src after evs tss =
   if after then fprintf oc "      else {\n";
   let tab = if after then "        " else "      " in
   begin match tss with 
      [] -> ()  (* no wait in this case *)
    | [ev,ts] ->
       fprintf oc "%swait_ev(%s);\n" tab ev;
       ListExt.iter_fst (fun is_first t -> dump_transition oc tab is_first src t) ts;
    | _ ->
       fprintf oc "%s%s = wait_evs(%s);\n" tab cfg.recvd_ev_name (ListExt.to_string (function id -> id) "," evs);
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

let dump_module_impl m fname fsm =
  let m = Cmodel.c_model_of_fsm m fsm in
  let oc = open_out fname in
  let modname = String.capitalize_ascii fsm.f_name in
  let string_of_ival = function
    | Expr.Val_none -> ""
    | Expr.Val_array vs when List.for_all (function Expr.Val_unknown -> true | _ -> false) (Array.to_list vs) -> ""
    | v -> " = " ^ string_of_value v in
  fprintf oc "task %s(\n" modname;
  List.iter (fun (id,ty) -> fprintf oc "  in %s;\n" (string_of_typed_item (id,ty))) m.c_inps;
  List.iter (fun (id,ty) -> fprintf oc " out %s;\n" (string_of_typed_item (id,ty))) m.c_outps;
  List.iter (fun (id,ty) -> fprintf oc " inout %s;\n" (string_of_typed_item (id,ty))) m.c_inouts;
  fprintf oc "  )\n";
  fprintf oc "{\n";
  List.iter (fun (id,(ty,iv)) -> fprintf oc "  %s%s;\n" (string_of_typed_item (id,ty)) (string_of_ival iv)) m.c_vars;
  if List.length m.c_states > 1 then 
    fprintf oc "  %s %s = %s;\n" (string_of_type (Types.TyEnum m.c_states)) cfg.state_var_name (fst m.c_init);
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

let dump_fsm ?(prefix="") ?(dir="./ctask") m f =
  let prefix = match prefix with "" -> f.f_name | p -> p in
  dump_module_impl m (dir ^ "/" ^ prefix ^ ".c") f

let dump_fn oc (id,(ty,gd)) = match gd, ty with
| Sysm.MFun (args, body), Types.TyArrow(TyProduct ts, tr) -> 
    fprintf oc "%s %s (%s) { return %s; }\n"
      (string_of_type tr)
      id 
      (ListExt.to_string (function (a,t) -> a ^ ":" ^ string_of_type t) ";" (List.combine args ts)) 
      (string_of_expr body)
| _ -> ()

let dump_fns ?(prefix="") ?(dir="./ctask") m =
  let prefix = match prefix with "" -> "global_fns" | p -> p in
  let fname = dir ^ "/" ^ prefix ^ ".c" in
  let oc = open_out fname in
  List.iter (dump_fn oc) m.Sysm.m_fns;
  Logfile.write fname;
  close_out oc

(* Check whether a model can be translated *)

let check_allowed m =
  match Fsm.cfg.Fsm.act_sem with
  | Fsm.Sequential -> ()
  | Fsm.Synchronous -> Error.not_implemented "CTask: synchronous actions"
