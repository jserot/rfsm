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

(** Handling of guest specific errors *)

module Location = Rfsm.Location

let handle e =
  let open Format in
  let open Rfsm.Error in
  match e with
  | Builtins.Unknown_value -> 
      eprintf "Cannot operate on undefined values\n"; exit 2
  | Types.Type_circularity (loc,ty,ty')
  | Types.Type_conflict (loc,ty,ty') ->
      eprintf "%aTyping error: cannot unify types %a and %a\n"
        pp_loc loc (Types.pp_typ ~abbrev:false) ty (Types.pp_typ ~abbrev:false) ty'; exit 2
  | Typing.Illegal_cast e -> 
      eprintf "%aIllegal cast\n" pp_loc e.Rfsm.Annot.loc; exit 2
  | Typing.Illegal_expr (loc,what) -> 
      eprintf "%aIllegal expression: %s\n" pp_loc loc what; exit 2
  | Syntax.Invalid_parameter (loc,p) -> 
      eprintf "%aInvalid type parameter : %a. Should be an constant integer expression\n" pp_loc loc Rfsm.Ident.pp p; exit 2
  | Eval.Uninitialized (what,loc) -> 
     if loc = Location.no_location then eprintf "Uninitialized value: %s\n" what
     else eprintf "%aUninitialized value: %s\n" pp_loc loc what; 
     exit 2
  | Eval.Out_of_bound (loc,i,lo,hi) -> 
      eprintf "%aOut of bound array access (%d not in [%d,%d])\n" pp_loc loc i lo hi; exit 2
  | Eval.Illegal_application e -> 
      eprintf "%aIllegal application\n" pp_loc e.Rfsm.Annot.loc; exit 2
  | Ctask.Unsupported_type t ->
      eprintf "CTask backend: unsupported type: %a\n" (Types.pp_typ ~abbrev:false) t; exit 2
  | Systemc.Unsupported_type t ->
      eprintf "SystemC backend: unsupported type: %a\n" (Types.pp_typ ~abbrev:false) t; exit 2
  | Systemc.Unsupported_expr e ->
      eprintf "%aSystemC backend: unsupported expression\n" pp_loc e.Rfsm.Annot.loc; exit 2
  | Systemc.Unsupported_value v ->
      eprintf "SystemC backend: unsupported value: %a\n" Value.pp v; exit 2
  | Vhdl.Unsupported_type t ->
      eprintf "VHDL backend: unsupported type: %a\n" (Types.pp_typ ~abbrev:false) t; exit 2
  | Vhdl.Unsupported_expr e ->
      eprintf "%aVHDL backend: unsupported expression\n" pp_loc e.Rfsm.Annot.loc; exit 2
  | Vhdl.Illegal_cast e ->
      eprintf "%aVHDL backend: illegal cast\n" pp_loc e.Rfsm.Annot.loc; exit 2
  | e ->
     eprintf "Internal error: %s.\n" (Printexc.to_string e);
     flush stderr; exit 100
