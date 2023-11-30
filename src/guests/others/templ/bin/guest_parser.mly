(* The parser for the guest language *)
(* See file ../../../../host/lib/host_parser.mly for the list of keywords already defined by the host language *)

(* TO BE COMPLETED. Define here the guest-specific tokens (with precedence and associativities) *

%{
(* TO BE COMPLETED. Add here OCaml code prelude for the parser rules *)
(* This should at least include 
     [open <Guest>.Top.Syntax]
   where [<Guest>] is the name of the guest language *)
%}

%%

(* TO BE COMPLETED. Write here the guest-specific parser rules *)
(* This include, at least, rules for the following non-terminals :
   - type_decl
   - type_expr 
   - lval
   - param_value
   - scalar_const
   - const
   - stim_const
   All these NTs must be annotated with the Menhir [%public] attribute, since they will be referenced
   by the host language parser. *)
