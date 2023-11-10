This directory contains the definition of "minimal" guest language. 
This is the language used in App. F of the user manual to illustrate how to define a new RFSM variant.

This language has only two types, `event` and `bool` :

```
<ty> := event | bool
```

and expressions, used in guards or assignations, are limited to boolean constants and variables :

```
<expr> := <var> | <const>

<const> := true | false
```


