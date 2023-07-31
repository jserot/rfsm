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
