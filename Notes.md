Host vs. guest languages
=========================

- Motivations
- Inplementation using functors, issues (interface delimitation)
  - modular impl (a la Leroy)
  - allow easy, non intrusive experimentations with new guest lgs (esp with type system)
  - allow addition of user-defined guests


Polymorphism, types, type variables and type schemes
====================================================

This is an issue related to the _guest_ language since at the host language level, types -- assigned
to constants, functions, model IOs and variables -- are essentially opaques.

Even the `core` guest language needs a polymorphic type system, in order
to accomodate primitives such as `=`, `<`, _etc_, which must have type : `t * t -> bool` for any
possible type `t`.

Classicaly, this is supported by using _type schemes_. The type scheme assigned to `=` for ex is 
`for all 'a. 'a * 'a -> bool` and any time this primitive is referenced in the program, this type
scheme is _instanciated_ to a new type `'v * 'v -> bool` (where `'v` is a fresh type variable).

Note that because this type of polymorphism only occurs for _builtin primitives_. There's no way to
declare a constant, function, global IO, model IO or local variable with a polymorphic type (i.e. a
type containing a _type variable_).

But there's another kind of polymorphism. For example, in a language supporting _sized_ integers
(such as `szvars` or `typarams`), if we declare 

`constant n: int`

which is a shorthand for

`constant n: int<'s>`

where `'s` is a _size variable_`,

it is clear that we should be able to use `n` with distint values of `'s` in the same
program, e.g.

```
fsm f4 (in i: int<4>, ...) ... rules q -> q' when i=n ... -- n:int<4> here
...
fsm f8 (in i: int<8>, ...) ... rules q -> q' when i=n ... -- n:int<8> here
```

Hence, the type of global constants, just like that of primitives, must be generalized when
retrieved from the environment. But generalization is here applied to _size variables_ instead of
type variables. And, again, it is unconditionnal (there's no such thing as free size variables).

Note that this does _not_ apply to size variables attached to _local variables_. For example, in

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

In other words, contrary to global values, the type of local variables -- and IOs - is never
generalized. 

**Warning**. The above applies to FSM _instances_. The type of a local variable (and of an IO) can
of course vary btw different instances of the same FSM model. E.g.

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

Implementation
--------------

It is clear, then, that we need to distinguish _types_ from _type schemes_. But the
generalisation/instanciation operations are simpler than in the "classical" HM type system since it
either involves _all_ or _none_ of the type variables occuring in a type. The list of variables to
be generalized in a type can be computed once for all (since it does not depend of the context of
the expression in which this type occurs). Generalisation extracts all type and size variables and
put them in the list(s) associated to the resulting type scheme. Instanciation systematically
replaces all the variables of these lists by fresh copies.




