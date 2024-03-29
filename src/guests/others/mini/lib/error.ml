(* The implementation defined in this file should match the signature [Guest.ERROR]  specified in ../../../host/lib/guest.ml *)

module Location = Rfsm.Location
                
let handle e =
  let open Format in
  let pp_loc = Location.pp_location in
  let pp_typ = Types.pp_typ ~abbrev:false in
  match e with
  | Typing.Type_conflict (loc,ty,ty') ->
      eprintf "%aTyping error: cannot unify types %a and %a\n"
        pp_loc loc pp_typ ty pp_typ ty'; exit 2
  | Eval.Uninitialized loc -> 
      eprintf "%aUninitialized value\n" pp_loc loc; exit 2
  | Vhdl.Unsupported_type t ->
      eprintf "VHDL backend: unsupported type: %a\n" pp_typ t; exit 2
  | Vhdl.Unsupported_value v ->
      eprintf "VHDL backend: unsupported value: %a\n" Value.pp v; exit 2
  | e ->
     eprintf "Internal error: %s.\n" (Printexc.to_string e);
     flush stderr; exit 100
