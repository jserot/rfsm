## DESCRIPTION 

RFSM is a framework for describing, simulating and generating code from _reactive finite state
machines_. A reactive finite state machine is a finite state machine (FSM) for which transitions
between states are always triggered by a single _event_. When the triggering event occurs
a set of boolean expressions, called _guards_, may also be used to decide whether the transition
is taken or not and _actions_, such as updating an output or local variable or emitting an event, can
also be performed.

As a simple example, consider the following model of a calibrated pulse generator. This model has
two inputs (`h`, of type `event`, and `e`, of type `bool`) and one output (`s`, of type
`bool`). Input `h` is supposed to be periodic, with period `T`. Input `e` is sampled at each
occurrence of the `h` event and when a `1` is read, the `s` output is set to `1` for a duration
`D=n.T`. 

![](https://github.com/jserot/rfsm/blob/master/doc/figs/gensig-model-moore.png "")

The model has two states (named `E0` and `E1`). The initial state is `E0`. 
The `s` output is `0` when the machine is in state `E0` and `1` when it is in state `E1`.
A local variable, `k`, of type `int` is used to count the occurrences of the event `h` when in state
`E1`. Each transition has a label of the form 
   
```
<ev>.<conds> / <acts>
```

where 
- `<ev>` is the event triggering the transition (always `h` here)
- `<conds>` is a (possibly empty) list of enabling boolean expressions ("guards") involving the inputs and local variables
- `<acts>` is a (possibly empty) list of actions involving the outputs and local variables

For example, the transition from state `E0` to state `E1`, labeled `h.(e=1)/k:=1` is taken whenever
input `e` is equal to `1` when `h` occurs and, when this happens, local variable `k` is set to 1. 

Here's a formulation of this model in RFSM :

```
fsm model gensig <n: int> (
  in h: event,
  in e: bool,
  out s: bool)
{
  states: E0 where s=0, E1 where s=1;
  vars: k: int<1:n>;
  trans:
  | E0 -> E1 on h when e=1 with k:=1
  | E1 -> E1 on h when k<n with k:=k+1
  | E1 -> E0 on h when k=n;
  itrans:
  | -> E0;
}
```

Within the model body (`{ ... }`), sections `states`, `vars`, `trans` and `itrans` respectively
describe the states, the local variables, the transitions and the initial transition. For each
transition, the triggering event, the enabling guards and associated actions are respectively
specified after the `on`, `when` and `with` keywords. The duration `n` of the pulse is here specified as
a parameter of the model. 

This model can be simulated by writing, for example, the following RFSM program :

```
input H : event = periodic (10,10,80)
input E : bool = value_changes (0:0, 25:1, 35:0)
output S : bool 

fsm g = gensig<3>(H,E,S)
```

This program declares two global inputs, `H` and `E` and one global output `S`, attaches stimuli
to the input and instanciates the previously declared `gensig` model (setting the value of the `n`
parameter to 3 here and binding the global IOs to the model IOs). 

Invoking the `rfmsc` compiler with the `-sim` option produces `.vcd` file, which can 
be viewed with the [Gtkwave](http://gtkwave.sourceforge.net) trace viewer for example, as shown
below

![](https://github.com/jserot/rfsm/blob/master/doc/figs/gensig-chrono.png "")

Invoking the `rfmsc` compiler with the `-ctask`, `-systemc` and `-vhdl` option can generate C,
SystemC and VHDL code for the model (and the testbench). Here's for example the VHDL code generated
for the `gensig` model. All the generated code can  can be viewed
[here](https://github.com/jserot/rfsm/tree/master/doc/code/gensig).

RFSM can also be used to describe multi-FSM models. Here's a description of a simple modulo-8
counter as three concurrent modulo-2 counters :

```
fsm model cntmod2(
  in h: event,
  out s: bool,
  out r: event)
{
  states: E0 where s=0, E1 where s=1;
  trans:
  | E0 -> E1 on h
  | E1 -> E0 on h with r;
  itrans:
  | -> E0;
}

input H: event = periodic(10,10,100)
output S0, S1, S2: bool
output R2: event

shared R0, R1: event

fsm C0 = cntmod2(H,S0,R0) 
fsm C1 = cntmod2(R0,S1,R1) 
fsm C2 = cntmod2(R1,S2,R2) 
```

We here have three instances of the `cntmod2` model. These instances are synchronized using two
internal, shared events named `R0` and `R1`.

A graphical view of global model (obtained by invoking the `rfsmc` compiler with the `-dot` option) is
given below

![](https://github.com/jserot/rfsm/blob/master/doc/figs/ctrmod8-top.png "")

Simulation of this model produces the following trace :

![](https://github.com/jserot/rfsm/blob/master/doc/figs/ctrmod8-chrono.png "")

## PACKAGE DESCRIPTION

RFSM actually is a framework composed of

* an Ocaml library defining a _host_ language for describing _generic_ reactive finite state
  machines, _i.e._ state machines which are _parameterized_ on the kind of expressions and actions attached to the their
  transitions

* a set of _guest_ languages, obtained by parameterizing this host language, each implemented as a
  command-line compiler

As exemplified above, RFSM compilers can

* generate graphical representation of the system (to be viewed with [Graphviz](http://www.graphviz.org) for example)

* simulate the behavior of the system when reacting to input stimuli (generating execution traces as
  `.vcd` files, to be viewed with [Gtkwave](http://gtkwave.sourceforge.net) for example)

* generate implementation of the described system in

  - `CTask` (a C dialect with primitives for describing tasks and event-based synchronisation)
  - `SystemC`
  - `VHDL` 

for simulation or implementation on a target platform (micro-controlers or FPGAs for instance). 

## Documentation

The user and reference manuals can be found
[here](https://github.com/jserot/rfsm/tree/master/doc/user_manual/rfsm_um.pdf) and
[here](https://github.com/jserot/rfsm/tree/master/doc/ref_manual/rfsm_rm.pdf).

The "host" library API is documented
[here](https://github.com/jserot/rfsm/tree/master/doc/lib/index.html). 


## Installation

The latest stable version is provided as a ready-to-install OPAM
[package](https://opam.ocaml.org/packages/rfsm). Just type 

`opam install rfsm`

The package includes
- the RFSM library itself (under `./host/lib`)
- the so-called _standard_ RFSM compiler, described in the user manual (under `./guests/std`)
- a bunch of variant compilers, corresponding to distinct parameterizations of the host language
  (under `./guests/others`)
- a program `rfsmmake` generating Makefiles for compiling RFSM programs and viewing and compiling
  results
- dedicated `SystemC` and `VHDL` libraries to be used with generated code

### Compiling and running examples

Some examples are provided in
[examples/single](https://github.com/jserot/rfsm/tree/master/examples/std/single) and
[examples/multi](https://github.com/jserot/rfsm/tree/master/examples/std/multi) (the former concerns systems
built from a single state diagram, the latter systems built from several diagrams).

For compiling and running an example

* get a copy of the source tree and go to the selected example directory
  * `git clone https://github.com/jserot/rfsm`
  * `cd rfsm/examples/std/single/gensig` (for example)
  
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

## BUILDING COMPILERS

The implementation of RFSM relies on the _host+guest_ design pattern described
[here](https://github.com/jserot/modlang). It is fairly easy to build one's own compiler 
implementing a variant of the "standard" RFSM language and supporting dedicated expression
sub-languages and type systems. The process is described in the reference manual. Several examples
of variant languages are provided in the distribution (under directorty `guests/others`).


## RELATED TOOLS

A Graphical User Interface to the RFSM compiler - but restricted to mono-FSM models
is provided by the [RfsmLight](http://github.com/jserot/rfsm-light) application.
