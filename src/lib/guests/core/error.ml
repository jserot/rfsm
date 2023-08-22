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
  | Eval.Uninitialized loc -> 
      eprintf "%aUninitialized value\n" pp_loc loc; exit 2
  | Value.Unsupported_vcd v ->
      eprintf "No VCD conversion for value %a\n" Value.pp v; exit 2
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
  | Vhdl.Unsupported_value v ->
      eprintf "VHDL backend: unsupported value: %a\n" Value.pp v; exit 2
  | e ->
     eprintf "Internal error: %s.\n" (Printexc.to_string e);
     flush stderr; exit 100
