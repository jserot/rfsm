(* The language itself (syntax, type-checker, evaluator) *)

(* Note: this functor application has to be put in a separate module to be referenced by the parser
   w/o creating a dependency cycle... *)

module L = Rfsm.Host.Make(Szdints.Top)

