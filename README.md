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

Documentation
-------------

The compiler user manual can be found [here](http://jserot.github.io/rfsm-docs/rfsm.pdf)

The library API is documented [here](https://jserot.github.io/rfsm/index.html)

Installation
------------

The latest stable version is provided as a ready-to-install OPAM
[package](https://opam.ocaml.org/packages/rfsm). Just type 

`opam install rfsm`

Compiling and running examples
------------------------------

Some examples are provided in the directories [single](https://github.com/jserot/rfsm/tree/master/examples/single) and
[multi](https://github.com/jserot/rfsm/tree/master/examples/multi) (the former concerns systems
built from a single state diagram, the latter systems built from several diagrams).

For compiling and running an example

* clone the corresponding directory somewhere on your file system (ex: `/tmp/rfsm-examples/single/chrono`)

* cd to the cloned directory (`cd /tmp/rfsm-examples/single/chrono`)

* build the top `Makefile` by invoking: `rfsmmake main.pro` 
  (this supposes that the `rfsm` package has been properly installed; in particular that the `rfsmc`
  and `rfsmmake` executables are available on your path)
  
* the generated `Makefile` contains a set of rules to 

  - generate and view the graphical representation of the system (`make dot`)

  - simulate the behavior the system and view the execution traces (`make sim`)

  - generate code describing the system in C (`make ctask.code`), SystemC (`make systemc.code`) and
    VHDL (`make vhdl.code`)

Viewing the graphical representations (`.dot` files) and the execution traces (`.vcd` files) is
carried out by calling external programs called `$DOTVIEWER` and `$VCDVIEWER` in the Makefile.
Default values are provided in the file `<opam_prefix>/share/rfsm/platform`, where `<opam_prefix>`
is the root of the `opam` tree where the `rfsm` package has been installed. These values will
probably to be adjusted according to your system.

The generated SystemC (resp. VHDL) code is written in
sub-directory `./systemc` (resp. `./vhdl`). Also generated in these directories is a dedicated
`Makefile` for compiling and running the generated code and viewing the results. This `Makefile` is derived from a template
located in directory `<opam_prefix>/share/rfsm/templates/`. These templates will also probably have to be
adjusted to suit your local `SystemC` or `VHDL` installation.

Some examples also contain a `./ml` sub-directory. The `ocaml` source code located in these
sub-directories illustrates the use of the `rfsm` library. The code can be compiled and run by simply typing
`make` (provided, again, that the `rfsm` package has been properly installed). 
