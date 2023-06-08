(** Handling of guest specific errors *)

module Location = Rfsm.Location
                
let handle e =
  let open Format in
  let pp_loc = Location.pp_location in
  match e with
  | Typing.Undefined (what,loc,s) -> 
      eprintf "%aUndefined %s: %s\n" pp_loc loc what s; exit 2
  | Types.Type_conflict (loc,ty,ty') ->
      eprintf "%aTyping error: cannot unify types %a and %a\n" pp_loc loc Types.pp_typ ty Types.pp_typ ty'; exit 2
  | Eval.Uninitialized loc -> 
      eprintf "%aUninitialized value\n" pp_loc loc; exit 2
  | Eval.Out_of_bound (loc,i) -> 
      eprintf "%aOut of bound array access (%d)\n" pp_loc loc i; exit 2
  | Value.Unsupported_vcd v ->
      eprintf "No VCD conversion for value %a\n" Value.pp_value v; exit 2
  | e ->
     eprintf "Internal error: %s.\n" (Printexc.to_string e);
     flush stderr; exit 100
