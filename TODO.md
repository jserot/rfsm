COMPILER

- fix bugs in `examples/std/single/fir/vhdl` and `examples/std/single/div8/systemc`
- support for synchronous actions (allow `examples/full/single/pgcd:sim` for ex.)
- allow shared signals with multiple writers in VHDL
- allow arrays as parameters (ex: `fsm model fir <c: int array[3],...)`)
- display enums as string in SystemC generated VCDs (currently not supported by SystemC 2.3) 
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

TOOLS
- syntax checkers for guards, actions and state valuations, to be used by `rfsm-light`

DIST
- opam install `etc/lib/{systemc,vhdl}`  and `src/host/lib/options_spec.txt` (to be used when building `rfsm-light`)

DOC
- clean .tex files
