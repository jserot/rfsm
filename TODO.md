COMPILER

- create src/host/bin
- dual error reporting mechanism (CLC / RfsmLight)
- support for synchronous actions (and allow examples/full/single/pgcd:sim)
- document lib/host/guest.ml
- check that .sav/.gtkw files are saved in GH repo
- allow shared signals with multiple writers in VHDL (see examples/multi/sync_vp/ex{2,3,4}) ??
- allow arrays as parameters (ex: `fsm model fir <c: int array[3],...)`)
- display enums as string in SystemC generated VCDs (currently not supported by SystemC 2.3) 
- rewrite parsers with $sloc+%inline (cf OCaml 4.08 srcs)
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
- add a section describing how to build a new language by providing a guest lib+bin
