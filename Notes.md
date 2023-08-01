Size vars (in `full3`)
---------
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

Parameterized types (in `full4`)
-------------------

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
  
