COMPILER

- document lib/host/guest.ml
- support for synchronous actions (and allow examples/full/single/pgcd:sim)
- dual error reporting mechanism (CLC / RfsmLight)
- check that .sav/.gtkw files are saved in GH repo
- support shared signals with multiple writers in VHDL (see examples/multi/sync_vp/ex{2,3,4}) ??
- display enums as string in SystemC generated VCDs (currently not supported by SystemC 2.3) 
- rewrite parsers with $sloc+%inline (cf OCaml 4.08 srcs)
- allow _unsized_ arrays as parameters (ex: `fsm model m <t: int array[], sz: int> (...)`)
- check for coherency in reaction responses (see src/lib/simul.ml)
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

- KNOWN-BUGS -> Issues

DOC
- fully automatize generation of doc/um/{grammar.tex,grammar-defns.sty}
- man page for rfsmc
- parameterize the formal semantics on that of the guest language (ex: the concrete `int`, `bool`
  types and values should not show any longer in the description of "host" semantics)
- add a section describing how to build a new language by providing a guest lib+bin
