RFSM 
====

RFSM is a toolset for describing and simulating StateChart-like state diagrams.
The toolset is composed of

* an Ocaml library

* a command-line compiler (`rfsmc`) 

RFSM tools take

* a description of a system as a set of StateChart-like state diagrams

* a description of stimuli to be used as input for this system

and generate

* a graphical representation of the system (to be viewed with [Graphviz](http://www.graphviz.org) for example)

* execution traces as `.vcd` files (to be viewed with [Gtkwave](http://gtkwave.sourceforge.net) for example)

Additionnaly, dedicated backends can generate system descriptions in

* `CTask` (a C dialect with primitives for describing tasks and event-based synchronisation)

* `SystemC`

* `VHDL` 

for simulation of implementation. 

The `rfmsc` compiler is intended to be used from the command line. Graphical User Interfaces are
provided separately:

* [Rfsm](http://dream.ispr-ip.fr/RFSM)

* [RfsmLight](http://github.com/jserot/rfsm-light) 

DOCUMENTATION
-------------

The compiler user manual can be found [here](http://github.com/jserot/RFSM/doc/rfsm.pdf)

The library API is documented [here](https://jserot.github.io/rfsmc/index.html)

INSTALLATION
------------

The latest stable version is provided as a ready-to-install OPAM
[package](https://opam.ocaml.org/packages/rfsm). Just type 

`opam install rfsm`
