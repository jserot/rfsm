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

open Format 

(* CTask backend *)

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

module type CTASK = sig
  
  module Static: Static.T
  module G: Guest.CTASK
       
  exception Error of string * string  (* where, msg *)

  val output: dir:string -> Static.t -> string list 
end
                  
module Make (Static: Static.T)
            (Guest: Guest.CTASK with module Syntax = Static.Syntax.Guest)
       : CTASK with module Static = Static =
struct

  module Static = Static
  module G = Guest
  module Cmodel = Cmodel.Make(Static)

  exception Error of string * string  (* where, msg *)
                   
  let output_files = ref ([] : string list)
                   
  let need_globals m = m.Static.types <> [] || m.Static.fns <> [] || m.Static.csts <> []

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

  let pp_action tab fmt a =
    let open Static.Syntax in
    match a.Annot.desc with
    | Emit id -> fprintf fmt "%snotify_ev(%s);\n" tab id
    | Assign (lhs, expr) -> fprintf fmt "%s%a = %a;\n" tab Guest.pp_lhs lhs G.pp_expr expr

  let pp_transition tab is_first src fmt (_,{Annot.desc=ev,guards;_},acts,q',_) =
    match guards with
    | [] ->
       List.iter (pp_action tab fmt) acts;
       if q' <> src then fprintf fmt "%s%s = %s;\n" tab cfg.state_var_name q'
    | _  -> 
       fprintf fmt "%s%sif ( %a ) {\n"
         tab
         (if is_first then "" else "else ")
         (Misc.pp_list_h ~sep:" && " G.pp_expr) guards;
       List.iter (pp_action (tab ^ "  ") fmt) acts;
       if q' <> src then fprintf fmt "%s  %s = %s;\n" tab cfg.state_var_name q';
       fprintf fmt "%s  }\n" tab

  let pp_ev_transition tab src fmt (ev,ts) = 
    fprintf fmt "%scase %s:\n" tab ev;
    Misc.list_iter_fst (fun is_first t -> pp_transition (tab^"  ") is_first src fmt t) ts;
    fprintf fmt "%sbreak;@." (tab^"  ")

  let pp_transitions src after evs fmt tss =
    if after then fprintf fmt "      else {\n";
    let tab = if after then "        " else "      " in
    begin match tss with 
      [] -> ()  (* no wait in this case *)
    | [ev,ts] ->
       fprintf fmt "%swait_ev(%s);\n" tab ev;
       Misc.list_iter_fst (fun is_first t -> pp_transition tab is_first src fmt t) ts
    | _ ->
       fprintf fmt "%s%s = wait_evs(%a);\n" tab cfg.recvd_ev_name (Misc.pp_list_h ~sep:"," pp_print_string) evs;
       fprintf fmt "%sswitch ( %s ) {\n" tab cfg.recvd_ev_name;
       List.iter (pp_ev_transition (tab^"  ") src fmt) tss;
       fprintf fmt "%s  }\n" tab
    end;
    if after then fprintf fmt "      }\n"
    
  let pp_output_valuation fmt (o,e) = 
    fprintf fmt "      %s = %a;\n" o G.pp_expr e
    
  let pp_state_case m fmt Cmodel.{ st_src=q; st_sensibility_list=evs; st_transitions=tss } =
    fprintf fmt "    case %s:\n" q;
    List.iter (pp_output_valuation fmt) (List.assoc q m.Cmodel.c_states);
    pp_transitions q false evs fmt tss;
    fprintf fmt "      break;\n"

  let pp_state m fmt Cmodel.{ st_src=q; st_sensibility_list=evs; st_transitions=tss } =
    pp_transitions q false evs fmt tss

  let dump_model fname m =
    let open Cmodel in
    let oc = open_out fname in
    let ocf = formatter_of_out_channel oc in
    let modname = String.capitalize_ascii m.c_name in
    fprintf ocf "task %s%a(\n"
      modname
      (Misc.pp_opt_list ~lr:("<",">") ~sep:"," G.pp_typed_symbol) m.c_params;
    List.iter (fun io -> fprintf ocf "  in %a;\n" G.pp_typed_symbol io) m.c_inps;
    List.iter (fun io -> fprintf ocf " out %a;\n" G.pp_typed_symbol io) m.c_outps;
    (* List.iter (fun (id,ty) -> fprintf ocf " inout %s;\n" (G.pp_typed_symbol ocf (id,ty))) m.inouts; *)
    fprintf ocf "  )\n";
    fprintf ocf "{\n";
    List.iter (fun io -> fprintf ocf "  %a;\n" G.pp_typed_symbol io) m.c_vars;
    if List.length m.c_states > 1 then 
      fprintf ocf "  enum { %a } %s = %s;\n"
        (Misc.pp_list_h ~sep:"," pp_print_string) (List.map fst m.c_states)
        cfg.state_var_name
        (fst m.c_init);
    if List.exists (function c -> List.length c.st_sensibility_list > 1) m.c_body then
      fprintf ocf "  event %s;\n" cfg.recvd_ev_name;
    List.iter (pp_action "  " ocf) (snd m.c_init);
    fprintf ocf "  while ( 1 ) {\n";
    begin match m.c_body with
      [] -> () (* should not happen *)
    | [q] -> pp_state_case m ocf q 
    | qs -> 
       fprintf ocf "    switch ( %s ) {\n" cfg.state_var_name;
       List.iter (pp_state_case m ocf) m.c_body;
       fprintf ocf "    }\n"
    end;
    fprintf ocf "  }\n";
    fprintf ocf "};@.";
    output_files := fname :: !output_files;
    close_out oc

  let dump_fsm_model ?(prefix="") ?(dir="./ctask") m =
    let c = Cmodel.c_model_of_fsm_model m in
    let prefix = match prefix with "" -> c.Cmodel.c_name | p -> p in
    dump_model (dir ^ "/" ^ prefix ^ ".c") c

  (* let dump_fsm_inst ?(prefix="") ?(dir="./ctask") m f =
   *   let prefix = match prefix with "" -> f.Fsm.f_name | p -> p in
   *   dump_model (dir ^ "/" ^ prefix ^ ".c") (Cmodel.c_model_of_fsm_inst m f)  *)

  (* Dumping global type declarations, functions and constants *)

  let dump_fun_decl fmt { Annot.desc = f; _ } =
    let open Static.Syntax in
    let pp_f_arg fmt (n,t) = fprintf fmt "%a %s" G.pp_type_expr t n in
    Format.fprintf fmt "%a %s(%a);\n" 
      G.pp_type_expr f.ff_res 
      f.ff_name
      (Misc.pp_list_h ~sep:"," pp_f_arg) f.ff_args

  let dump_fun_impl fmt { Annot.desc = f; _ } =
    let open Static.Syntax in
    let pp_f_arg fmt (n,t) = fprintf fmt "%a %s" G.pp_type_expr t n in
    Format.fprintf fmt "%a %s(%a) { return %a; }\n" 
      G.pp_type_expr f.ff_res 
      f.ff_name
      (Misc.pp_list_h ~sep:"," pp_f_arg) f.ff_args
      G.pp_expr f.ff_body

  let dump_cst_decl fmt { Annot.desc = c; _ } =
    let open Static.Syntax in
    G.pp_cst_decl fmt c.cc_name c.cc_typ  (* Cannot do more due to the idiosyncrasies of C type declarations.. *)

  let dump_cst_impl fmt { Annot.desc = c; _ } =
    let open Static.Syntax in
    G.pp_cst_impl fmt c.cc_name c.cc_typ c.cc_val  (* Cannot do more due to the idiosyncrasies of C type declarations.. *)

  let dump_globals_intf dir prefix s =
    let open Static in
    let fname = dir ^ "/" ^ prefix ^ ".h" in
    let oc = open_out fname in
    let ocf = formatter_of_out_channel oc in
    fprintf ocf "#ifndef _%s_h\n" cfg.globals_name;
    fprintf ocf "#define _%s_h\n\n" cfg.globals_name;
    List.iter (fun td -> Format.fprintf ocf "%a\n" G.pp_type_decl td) s.types;
    List.iter (dump_fun_decl ocf) s.fns;
    List.iter (dump_cst_decl ocf) s.csts;
    fprintf ocf "#endif@.";
    output_files := fname :: !output_files;
    close_out oc

  let dump_globals_impl dir prefix s =
    let open Static in
    let fname = dir ^ "/" ^ prefix ^ ".c" in
    let oc = open_out fname in
    let ocf = formatter_of_out_channel oc in
    fprintf ocf "#include \"%s.h\"\n\n" prefix;
    List.iter (dump_fun_impl ocf) s.fns;
    List.iter (dump_cst_impl ocf) s.csts;
    fprintf ocf "@.";
    output_files := fname :: !output_files;
    close_out oc

  let dump_globals ?(name="") ?(dir="./ctask") s =
    let open Static in
    let prefix = match name with "" -> cfg.globals_name | p -> p in
    dump_globals_intf dir prefix s;
    if s.fns <> [] || s.csts <> [] then dump_globals_impl dir prefix s

  let output ~dir s =
    output_files := [];
    if need_globals s then dump_globals ~dir s;
    List.iter (dump_fsm_model ~dir) s.Static.models;
    !output_files

end
