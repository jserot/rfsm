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
  | _ -> Error.fatal_error "Ctask.string_of_type"

let string_of_value v = match v with
  Expr.Val_int i -> string_of_int i
| Expr.Val_bool b -> string_of_bool b
| Expr.Val_enum s -> s

let string_of_ival = function
    None -> ""
  | Some v -> " = " ^ string_of_value v

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
 
let rec string_of_expr e = match e with
    Expr.EInt c -> string_of_int c
  | Expr.EBool c -> string_of_bool c
  | Expr.EEnum c -> c
  | Expr.EVar n -> n
  | Expr.EBinop (op,e1,e2) -> string_of_expr e1 ^ string_of_op op ^ string_of_expr e2 (* TODO : add parens *)

and string_of_op = function "=" -> "==" | op ->  op

let string_of_guard (e1, op, e2) = 
  string_of_expr e1 ^ string_of_op op ^ string_of_expr e2 (* TODO : add parens *)

let string_of_action a = match a with
    | Action.Assign (id, expr) -> id ^ "=" ^ string_of_expr expr
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
  fprintf oc "task %s(\n" modname;
  List.iter (fun (id,ty) -> fprintf oc "  in %s %s;\n" (string_of_type ty) id) m.c_inps;
  List.iter (fun (id,ty) -> fprintf oc " out %s %s;\n" (string_of_type ty) id) m.c_outps;
  List.iter (fun (id,ty) -> fprintf oc " inout %s %s;\n" (string_of_type ty) id) m.c_inouts;
  fprintf oc "  )\n";
  fprintf oc "{\n";
  List.iter (fun (id,(ty,iv)) -> fprintf oc "  %s %s%s;\n" (string_of_type ty) id (string_of_ival iv)) m.c_vars;
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
