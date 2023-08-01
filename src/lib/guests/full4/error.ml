(** Handling of guest specific errors *)

module Location = Rfsm.Location
                
let handle e =
  let open Format in
  let pp_loc = Location.pp_location in
  match e with
  | Builtins.Unknown_value -> 
      eprintf "Cannot operate o undefined values\n"; exit 2
  | Types.Type_circularity (loc,ty,ty')
  | Types.Type_conflict (loc,ty,ty') ->
      eprintf "%aTyping error: cannot unify types %a and %a\n"
        pp_loc loc (Types.pp_typ ~abbrev:false) ty (Types.pp_typ ~abbrev:false) ty'; exit 2
  | Typing.Undefined (what,loc,s) -> 
      eprintf "%aUndefined %s: %a\n" pp_loc loc what Rfsm.Ident.pp s; exit 2
  | Typing.Duplicate (what,loc,x) -> 
      eprintf "%aDuplicate %s: %a\n" pp_loc loc what Rfsm.Ident.pp x; exit 2
  | Typing.Illegal_cast e -> 
      eprintf "%aIllegal cast\n" pp_loc e.Rfsm.Annot.loc; exit 2
  | Typing.Illegal_expr (loc,what) -> 
      eprintf "%aIllegal expression: %s\n" pp_loc loc what; exit 2
  | Eval.Uninitialized (what,loc) -> 
     if loc = Location.no_location then eprintf "Uninitialized value: %s\n" what
     else eprintf "%aUninitialized value: %s\n" pp_loc loc what; 
     exit 2
  | Eval.Out_of_bound (loc,i,lo,hi) -> 
      eprintf "%aOut of bound array access (%d not in [%d,%d])\n" pp_loc loc i lo hi; exit 2
  | Eval.Illegal_application e -> 
      eprintf "%aIllegal application\n" pp_loc e.Rfsm.Annot.loc; exit 2
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
