(** Handling of guest specific errors *)

module Location = Rfsm.Location
                
let handle e =
  let open Format in
  let pp_loc = Location.pp_location in
  match e with
  | Builtins.Unknown_value -> 
      eprintf "Cannot operate o undefined values\n"; exit 2
  | Types.Index.Illegal_type_index i -> 
     eprintf "Illegal type index: %a\n" Types.Index.pp i; exit 2
  | Types.Index.Unbound_type_index v -> 
     eprintf "Unbound type index: %s\n" v; exit 2
  | Types.Index.Illegal_op op -> 
     eprintf "Illegal operation on type index: %s\n" op; exit 2
  | Types.Type_circularity (loc,ty,ty')
  | Types.Type_conflict (loc,ty,ty') ->
      eprintf "%aTyping error: cannot unify types %a and %a\n" pp_loc loc Types.pp_typ ty Types.pp_typ ty'; exit 2
  | Typing.Undefined (what,loc,s) -> 
      eprintf "%aUndefined %s: %s\n" pp_loc loc what s; exit 2
  | Typing.Duplicate (what,loc,x) -> 
      eprintf "%aDuplicate %s: %s\n" pp_loc loc what x; exit 2
  | Typing.Illegal_cast e -> 
      eprintf "%aIllegal cast\n" pp_loc e.Rfsm.Annot.loc; exit 2
  | Typing.Illegal_expr (loc,what) -> 
      eprintf "%aIllegal expression: %s\n" pp_loc loc what; exit 2
  | Eval.Uninitialized loc -> 
      eprintf "%aUninitialized value\n" pp_loc loc; exit 2
  | Eval.Out_of_bound (loc,i) -> 
      eprintf "%aOut of bound array access (%d)\n" pp_loc loc i; exit 2
  | Eval.Illegal_application e -> 
      eprintf "%aIllegal application\n" pp_loc e.Rfsm.Annot.loc; exit 2
  | Value.Unsupported_vcd v ->
      eprintf "No VCD conversion for value %a\n" Value.pp v; exit 2
  | e ->
     eprintf "Internal error: %s.\n" (Printexc.to_string e);
     flush stderr; exit 100
