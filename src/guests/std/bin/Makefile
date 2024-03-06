all: native 

byte:
	dune build rfsmc.bc

native:
	dune build rfsmc.exe

test:
	dune exec -- ./rfsmc.bc -dump_typed -dump_static -sim ./tests/gensig/main.fsm

clean:
	dune clean

clobber: clean
	@rm -f *~
