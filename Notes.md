Size vars (in `full3` and `full4`)
==================================
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

More formally :
-------------

\tau ::=                                    types
       | ...
       | \alpha                             type variable
       | \khi (\tau_1,...,\tau_m) \sigma    constructed type (constructor, arguments, size)

\sigma ::=                                  type sizes
       | \empty                             none (for bool type for ex)
       | \beta                              size variable
       | \iota                              litteral (int size or array dimension)
       | \iota \times \iota                 pair of litterals (range for int's, dimensions for 2D arrays)
       | \iota                              parameter name (to be removed by the elaboration step)

\iota ::=                                  size litteral
       | int
       | ident                             parameter name (temporary; should be replaced by int during elaboration) 

Examples
--------

type expression                   associated type
int<8>                            int([],8)
int                               int([],'a)
bool array[8]                     array([bool([],\empty)],8)
int<0:7>                          int([],(0,7))
int<n>                            int([],"n")
int<0:n>                          int([],(0,"n"))

Note that 
- there's no type expression for unsized arrays (it is therefore not possible to declare such an
  array in a program); this could change in future extensions (?)
- a type like [int([],(0,'a)] will never occur; this explains why size variables can occur in type
  size position but not in size litteral position

Parameterized types (in `full4`)
================================

The goal is to be able to write pgms like, for ex
```
fsm model f (n: int) (in i:int<n>, out o: int<n>) ... vars z: int<n> ... 
...
input i8: int<8>
output o8: int<8>
input i16: int<16>
output o16: int<16>
fsm f8<8>(i8,o8)
fsm f16<16>(i16,o16)
```
where the parameter `n` (of type `int`) is used to _refine_ the type of some IOs and local variables
of a model.
This can be viewed as a limited form of dependent typing (here the _type_ of the model `f` depends on
the _value_ of its parameter). 

A typical example is given by the `mul8/mul16` example.

This is distinct from (orthogonal to) the concept of size variable described above (the distinction
was unclear in `rfsm-v1` and the recrafting / reimplementation in `v2` helped clarifying this).

Implementation issues:

- the type of a model is no longer unique; there are possibly has many possible types as distinct
  instanciations of this model in the program; in practice, this means that each instance must now
  have its own copy of the (typed) associated model
  
Compilation flow
----------------

Raw AST (from parsing) : models + instance defns
         |
         |
   [SHALLOW TYPING] 
         |
         V
AST with typed type/fun/cst decls and fsm instanciations
         |
         V
   [ELABORATION] (FSM instanciation)
         |
         V
Collection of instanciated models (in which parameter occurrences have been substituted in types and
         expressions and local ios substituted by bound global ios)
         |
         | 
   [DEEP TYPING] (instanciated model defns)
         |
         V
     typed program
         |
         | 
   [BACKENDS] 
     | ... |
     V     V

In this view, typing and type-checking is performed in two separate steps:
1. during the elaboration phase
   - type, function and constant declarations and type-checked (and added to the typing environment)
   - each model instanciation is type-checked; i.e. we check that
     i) the type of the actual parameters supplied for the instance match the type of the formal
     parameters declared in the model. for example, if model [f] has been declared as
       [fsm model f (n: int) (...) ...]
     then we should flag an instanciation like
       [fsm f1 = f(true) (...)]
     ii) the type of the actual arguments supplied for the instance match the type of the the formal
     arguments declated in the model; but type matching must adopt a more "relaxed" interpretation
     because the type of formal arguments may involve parameters; for example, if model [f] has been declared as
       [fsm model f (n: int) (i: int<n>, ...) ...]
     then we should accept an instanciation like
       [fsm f1 = f(8) (x,...)] where [x] has been declared as [input x: int<8>]
     but also an instanciation like
       [fsm f1 = f(8) (x,...)] where [x] has been declared as [input x: int<4>]
     Of course, we should reject an instanciation like
       [fsm f1 = f(8) (x,...)] where [x] has been declared as [input x: bool]
     ** Q: why can't we type-check "strictly" such instanciations ?
2. after the elaboration step id completed, the FSM models themselves (variables, rules, ...) are
   type-checked; this step is called "deep typing"

This organisation is required because _parameterized_ models cannot be (deeply) typed before instanciation.
For example, in this pgm :
```
fsm model f (n:int) (in i:int<n>, ...)
  vars z:int<8> ...
  trans: 
  | ... with z:=i ...
  
...
input x8: int<8>
...
fsm f8 = f<8>(x8,...)
```
the _model_ `f` cannot be type-checked "in isolation" (un-instanciated), because
there's no way to tell whether the assignation `z:=i` is well-typed "per se". 
Only the _instanciated model_ `f8` can be type-checked, i.e. the FSM
```
fsm f8 (in i:int<8>, ...)
  vars z:int<8> ...
  trans: 
  | ... with z:=i ...
```
  
** REM : "deep" typing _could_ be carried out for each instanciation if we keep, in the environment,
the value of each parameter, couldn't it ?

This static elaboration phase can in fact remove _all occurrence_ of parameters in the model. 
In the previous example, a rule 
```
q -> q' when h.z<n-1 with ...
```
in `f` would be (syntactically) rewritten as
```
q -> q' when h.z<8-1 with ...
```
in `f8`.
This way, we don't need generic parameters in generated CTask, SystemC and VHDL models. 

Static elaboration can even be pushed a little further. If an expression containing a parameter
contains only litteral constants and parameters, it can be (statically) evaluated and the resulting
value -- turned back into a constant expression, because we are dealing with syntactic objects at
this level -- replace this expression.
In the previous example, this mean that resulting rule would be rewritten as
```
q -> q' when h.z<8-1 with ...
```
in `f8`.

With is approach, the parameter mechanism is really viewed as a "macro" or "template" mechanism
(performing substitions in the "source" code) before it is actually "compiled".
This clearly simplifies the compilation process. In particular, parameters do not appear only longer
in the dynamic semantics. OTOH, it can make simulation or error reporting more ambiguous (?)




