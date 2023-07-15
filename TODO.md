COMPILER

- option to show / unshow ident qualifiers in dot output
- options in do_test(s) to compare VCD outputs btw sim and systemc
- replace all string's by Ident.t's (?)
- VHDL backend
- support for synchronous actions
- document lib/host/guest.ml
- banner
- VCD hierarchy (gtkwave format ?)
- dual error reporting mechanism (CLC / RfsmLight)
- support shared signals with multiple writers in VHDL (see examples/multi/sync_vp/ex{2,3,4}) ??
- rewrite parsers with $sloc+%inline (cf OCaml 4.08 srcs)
- allow _unsized_ arrays as parameters (ex: [fsm model m <t: int array[], sz: int> (...)])
- bound check for arrays, ranged integers and bit ranges
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
