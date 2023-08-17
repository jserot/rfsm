Extension of `../simple` with size variables with size unification for `int`s and `array`s)

Description
-----------
- by default, an `int` now has type `'a int` where `'a` is a _size variable_ (just as a list has type `a list`). 
- type unification also unifies sizes; for example, unifying `8 int` and `_v12 int` (where `_v12`
is a size variable, creates the indirection `_v12 -> 8`
- one advantage is that if we write, for ex, `z:=0`, where `z` has been declared with type
`int<8>`, the RHS expression (`0`) will automatically be assigned type `int<8>`. There was no
way to "refine" the type of litteral constants appearing in RHS in the language ``szdints` for ex and this 
made bound checking not always tractable.
- size variables are similar to type variables (in terms of unification) but handled separately
because they "ground" values are _integers_ and not _types_ (one could use the classical
type-encoded values trick - e.g. `type _sz8; ...; int<8> = _sz8 int` - but this is really a
hack
- in the current version, one cannot do _computations_ on int sizes (e.g. `int<n+1>`)
- the same mechanism is applied to array sizes

More formally :
-------------

\tau ::=                                    types
       | ...
       | \alpha                             type variable
       | \khi (\tau_1,...,\tau_m) \sigma    constructed type (constructor, arguments, size)

\sigma ::=                                  type sizes
       | \empty                             none (for bool type for ex)
       | \beta                              size variable
       | int                                litteral (int size or array dimension)
       | int \times int                     pair of litterals (range for int's, dimensions for 2D arrays)

Examples
--------

type expression                   associated type
---------------                   ---------------
int<8>                            int([],8)
int                               int([],'a)
bool array[8]                     array([bool([],\empty)],8)
int<0:7>                          int([],(0,7))

Note that 
- there's no type expression for unsized arrays (it is therefore not possible to declare such an
  array in a program); this could change in future extensions (?)
- a type like [int([],(0,'a)] will never occur; this explains why size variables can occur in type
  size position but not in size litteral position

