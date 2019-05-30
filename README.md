RFSM 
====

RFSM is a toolset for describing and simulating StateChart-like state diagrams.
The toolset is composed of

* an Ocaml library

* a command-line compiler (`rfsmc`) 

* a Qt-based standalone application (`Rfsm.app` or `rfsm.exe`)

RFSM tools take

* a description of a system as a set of StateChart-like state diagrams

* a description of stimuli to be used as input for this system

and generate

* a graphical representation of the system (to be viewed with [Graphviz][graphviz] for example)

* execution traces as `.vcd` files (to be viewed with [Gtkwave][gtkwave] for example)

Additionnaly, dedicated backends can generate system descriptions in

* `CTask` (a C dialect with primitives for describing tasks and event-based synchronisation)

* `SystemC`

* `VHDL` 

for simulation of implementation. 

[graphviz]: http://www.graphviz.org
[gtkwave]: http://gtkwave.sourceforge.net

DOCUMENTATION
-------------

The project web page (including links for downloading the tools) is 
[here][web].

The user manual can be found [here][usermanual]

[usermanual]: http://dream.ispr-ip.fr/RFSM/doc/rfsm.pdf

[web]: http://dream.ispr-ip.fr/RFSM

The library API is documented [here][libapi].

[libapi]: http://dream.ispr-ip.fr/RFSM/doc/lib/index.html

INSTALLATION
------------

Prebuilt Windows and MacOS versions can be downloaded from the [project webpage][web].

Source code is available via [github][] (`git clone https://github.com/jserot/rfsm`).

The library and the compiler are available as an [opam][] package.

[github]: https://github.com/jserot/rfsm
[opam]: https://opam.ocaml.org/packages/rfsm
