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
  have its own copy of the (typed) associated model (see Sec. Compilation flow below)
  
Compilation flow
----------------

Raw AST (from parsing) : models + instance defns
         |
         |
     [TYPING] 
         |
         V
   [ELABORATION]
         |
         V
  static representation
         |
         V 
   [SIMUL / BACKENDS] 
     | ... |  ... |
     V     V      V

The flow is classical. The main main issue is typing in case of type systems supporting
_parameterized types_ (e.g. `int<n>`). In this case, the FSM models cannot be type-checked "in
isolation", because the _value_ of parameters, given at the instanciation point, is required.
For example, the following model:
```
fsm model f (n:int) (in i:int<n>, ...)
  vars z:int<8>, k:int ...
  trans: 
  | ... when k=n-1 ... with z:=i ...
  
```
the _model_ `f` cannot be type-checked without knowing the value of `n`.
But an _instanciation_ of this model can be type-checked, like in
```
...
input x8: int<8>
...
fsm f8 = f<8>(x8,...)
```

As a side-effect, typing a model instanciation can _remove_ all parameters from this model, replacing
them by their actual values. In the previous example, the following (instanciated) model will be
attached to FSM `f8`:
```
fsm f8 (in i:int<8>, ...)
  vars z:int<8> ...
  trans: 
  | ... when k=8-1 ... with z:=i ...
```

The models submitted to the static elaboration step (and, further, to the backends) are then 
parameter-less. In this view, the parameter mechanism can be viewed as (typed) "macro" or "template"
system.

Parameter substitution can in fact be pushed a little further. If an expression containing a parameter
contains only litteral constants and parameters, it can be _evaluated_ at typing time and the resulting
value -- turned back into a constant expression, because we are dealing with syntactic objects at
this level -- replace this expression.
In the previous example, this means that rule in the `trans` section could actually be rewritten as:
```
  | ... when k=7 ... with z:=i ...
```
in instanciated model `f8`.





