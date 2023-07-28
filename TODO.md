COMPILER

- size vars. I.e.
   - by default, an `int` has type `'a int` where `'a` is a _size variable_ (just as a list has type `a list`). 
   - type unification also unifies sizes. For example, unifying `8 int` and `_v12 int` (where `_v12`
     is a size variable creates the indirection `_v12 -> 8`
   - one advantage is that if we write, for ex, `z:=0`, where `z` has been declared with type
     `int<8>`, le RHS expression (`0`) will automatically be assigned type `int<8>`. There was no
     way to "refine" the type of constants appearing in RHS up to now. This would make bound
     checking more tractable.
   - size variables are similar to type variables (in terms of unification) but handled separately
     because they "ground" values are _integers_ and not _types_ (one could use the classical
     type-encoded values trick - e.g. `type _sz8; ...; int<8> = _sz8 int` - but this is really a
     hack
   - in the current version, one cannot do _computations_ on int sizes (e.g. `int<n+1>`)
   - the same mechanism is applied to array sizes
   - this _not_ dependent types because the actual sizes will always be supplied litteraly and not
     given by arguments (but maybe by parameters ?)
- sized and ranged int bound checking
- refine translation of parameterized types (ex: int<n> when [n] is a model parameter) in SystemC and VHDL backends
- `CModel.c_consts` = `c_params` ? 
- support for synchronous actions (and allow examples/full/single/pgcd:sim)
- document lib/host/guest.ml
- banner
- move VCD fns in guest language from Syntax module to a separate Vcd module
- replace all string's by Ident.t's (?)
- dual error reporting mechanism (CLC / RfsmLight)
- support shared signals with multiple writers in VHDL (see examples/multi/sync_vp/ex{2,3,4}) ??
- display enums as string in SystemC generated VCDs (currently not supported by SystemC 2.3) 
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
