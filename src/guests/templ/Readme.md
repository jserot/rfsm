This directory contains templates for building a RFSM language variant using a new guest language.

To use it

1. Make sure the `rfsm` library is available (`cd src/host/lib; make`)
2. Make a copy of this directory in `src/guests` and rename it (`myguest` for example)
3. Go to the `myguest/lib` subdir
  - edit the files `{info,types,syntax,typing,value,static,eval,ctask,error,options,systemc,vhdl}.ml`
  - check that everything compiles by typing `make`
4. go to the `myguest/bin` subdir
  - edit the files `guest_kw.mll`, `guest_tokens.mll`, `guest_open.mly`, `guest_rules.mly` and `guest_tokens.mly`
  - check that everything compiles by typing `make`
5. go to `myguest` dir and type `make`

The resulting compiler should be in `<root>/_default/src/guests/myguest/bin/rfsmc.{bc,exe}`. 

