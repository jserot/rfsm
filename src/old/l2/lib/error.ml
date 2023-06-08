module Location = Msic.Location

let handle e =
  let open Format in
  let pp_loc = Location.pp_location in
  match e with
  | Typing.Undefined (what,loc,s) -> 
      eprintf "%aUndefined %s: %s\n" pp_loc loc what s; exit 2
  | Types.Type_conflict (loc,ty,ty')
  | Typing.Typing_error (loc,ty,ty') -> 
      eprintf "%aTyping error: cannot unify types %a and %a\n" pp_loc loc Types.pp_typ ty Types.pp_typ ty'; exit 2
  | Typing.Duplicate (what,loc,x) -> 
      eprintf "%aDuplicate %s: %s\n" pp_loc loc what x; exit 2
  | Eval.Uninitialized loc -> 
      eprintf "%aUninitialized value\n" pp_loc loc; exit 2
  | Eval.Eval_error loc -> 
      eprintf "%aError when evaluating this expression\n" pp_loc loc; exit 2
  | Eval.Out_of_bound (loc,i) -> 
      eprintf "%aOut of bound array access (%d)\n" pp_loc loc i; exit 2
  | Sys_error msg ->
     eprintf "Input/output error: %s.\n" msg; flush stderr; exit 21
  | Sys.Break -> flush stderr; exit 20
  | End_of_file -> exit 0
  | e ->
     eprintf "Internal error: %s.\n" (Printexc.to_string e);
     flush stderr; exit 100
