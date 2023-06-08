(** Guest-specific CL options *)

let set_print_types () = Syntax.print_types := true

let specs = [
"-print_types", Arg.Unit (set_print_types), "print types";
];
