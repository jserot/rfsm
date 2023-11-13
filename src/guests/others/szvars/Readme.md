Extension of `../simple` with size variables with size unification for `int`s and `array`s)

Description
-----------

By default, an `int` now has type `'a int` where `'a` is a _size variable_ (just as a list has type `a list`). 

Type unification also unifies sizes; for example, unifying `8 int` and `_v12 int` (where `_v12`
is a size variable, creates the indirection `_v12 -> 8`.

We now can write, for ex, `z:=0`, where `z` has been declared with type
`int<8>`, the RHS expression (`0`) will automatically be assigned type `int<8>`. There was no
way to "refine" the type of litteral constants appearing in RHS in the language ``szdints` for ex. and this 
made bound checking not always tractable.

Size variables are similar to type variables (in terms of unification) but handled separately
because 
- their "ground" values are _integers_ and not _types_
- there are always generalized (there's no such thing as a free size variable)

Note that is not possible to do _computations_ on int sizes (e.g. `int<n+1>`).

More formally :
-------------

```
\tau ::=                                    types
       | ...
       | \alpha                             type variable
       | \khi (\tau_1,...,\tau_m) \sigma    constructed type (constructor, arguments, size)

\sigma ::=                                  type sizes
       | \empty                             none (for bool type for ex)
       | \beta                              size variable
       | int                                litteral (int size or array dimension)
       | int \times int                     pair of litterals (range for int's, dimensions for 2D arrays)
```

Examples
--------

| type expression | Associated type            |
| --------------- |:--------------------------:|
| int<0:7>        | int([],(0,7))              |
| int<8>          | int([],8)                  |
| int             | int([],'a)                 |
| bool array[8]   | array([bool([],\empty)],8) |

Note that 
- there's no type expression for unsized arrays (it is therefore not possible to declare such an
  array in a program); this could change in future extensions (?)
- a type like `int([],(0,'a)]`will never occur; this explains why size variables can occur in type
  size position but not in size litteral position

Global vs. local sizes
----------------------

Size variables are handled differently depending on whether they occur on global constants or local
variables.

When occuring in global constants, they are always generalized. For example, if we declare 

`constant n: int`

which is actually a shorthand for

`constant n: int<'s>`

where `'s` is a _size variable_`,

`n` can be used with distinct values for `s` in the same program, as in, for example 

```
fsm f4 (in i: int<4>, ...) ... rules q -> q' when i=n ... -- n:int<4> here
...
fsm f8 (in i: int<8>, ...) ... rules q -> q' when i=n ... -- n:int<8> here
```

When occuring in local variables, size variables are _not_ generalized. For example, if we declare
FSM `f` as 

```
fsm f (...)
...
var z: int  -- i.e. int<'s>
...
```

`'s` cannot be instanciated with distinct values in the body (rules) of `f`. For example,
we cannot write:

```
fsm f (in i: int<8>, out o: int<4>, ...)
...
var z: int;
rules 
...
| q -> q' when i=z with o:=z
...
```

Because type-checking the condition `i=z` will have unified types `int<8>` and `int<'s>`, and hence
set `'s` to `8`, so that type-checking the action `o:=z` will raise an error, trying to unify
`int<4>` and `int<8>`. 

This also applies to FSM IOs.


**Warning**. The above applies to FSM _instances_. The type of a local variable (and of an IO) can
of course vary between different instances of the same FSM model. E.g.

```
fsm model f (in i: int, ...)
...
var z: int
rules 
...
| q -> q' when i=z ...
...

input i4: int<4>
input i8: int<8>
...
fsm f4 = f(i4)
fsm f8 = f(i8)
```

The type of `i` (resp. `z`) is `int<4>` in `f4` and `int<8>` in `f8`.
This is because the instanciation of the FSM model `f` will effectively create a _copy_ of its types
(and, consequently, of the contained size variables).

Last but not least, from a typing point of view, model parameters should be considered as global
constants. And, therefore, have their type generalized when retrieved. 

Arrays
------

The mechanism described for integers also applies to _array sizes_.

