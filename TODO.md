COMPILER

- remove abbrev arg to Guest.Types.pp_typ (or make it optional)
- Use macro to build located syntax nodes in parsers (esp. for _guest_ parsers, which should really 
  not have to write things like `mk ~loc:($symbolstartofs,$endofs) ... }`)
- remove `pp_xxx` fns from `Guest.vhdl`: `vhdl_type_of` should be enough (?)
- rename `subst_expr` and `subst_type_expr` in `Guest.Syntax` to make explicit they deal with _parameters_
- separate src/host/lib/misc.ml in misc+utils
- true modular parsing in Menhir ?
- support for synchronous actions (allow `examples/full/single/pgcd:sim` for ex.)
- allow shared signals with multiple writers in VHDL
- allow arrays as parameters (ex: `fsm model fir <c: int array[3],...)`)
- display enums as string in SystemC generated VCDs (currently not supported by SystemC 2.3) 
- rewrite parsers with `$sloc+%inline` (cf OCaml 4.08 srcs)
- clarify mechanism for delta-waits insertion in SystemC backend
- VHDL implementation of models with multiple input events
- VHDL implementation of event and variable synchronized models
- FreeRTOS (or other RTOS) specialized C-backend
- SCXML backend (www.w3.org/TR/scxml, Yakindu, ...)
- SMV backend (with predicated states for connection to model-checking tools)
- foreign function interfacing
- defactorisation wrt. variable(s) ?
- communication via FIFOs (message queues) ?

EXAMPLES

DIST
- use Dune `install` stanza to install documentation, etcs and examples
- KNOWN-BUGS -> Issues

DOC
- add a section (at least paragraph) on sized types and paramaterized types
- fully automatize generation of doc/um/{grammar.tex,grammar-defns.sty}
- fix automatic numbering of Appendices
- add a section describing how to build a new language by providing a guest lib+bin
- integrate Notes.md to documentation
