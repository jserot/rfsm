(* The language itself (syntax, type-checker, evaluator) *)

(* Note: this functor application has to be put in a separated module to be referenced by the parser
   w/o creating a dependency cycle... *)

module L = Msic.Host.Make(Guest2.Top)

