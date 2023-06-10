COMPILER

- bool -> int implicit conv
- Value.value -> Value.t
- check exn catching (both at host and guest level)
- move typing env defn from guest to host ?
- support shared signals with multiple writers in VHDL (see examples/multi/sync_vp/ex{2,3,4}) ??
- rewrite parsers with $sloc+%inline (cf OCaml 4.08 srcs)
- support for synchronous actions
- allow arrays as parameters (ex: [fsm model m <t: int array[4]])
- allow _unsized_ arrays as parameters (ex: [fsm model m <t: int array[], sz: int> (...)])
- bound check for ranged integers
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
- add a section describing how to build a new language by providing a guest lib+bin
