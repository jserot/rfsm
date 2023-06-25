all: byte 

byte:
	dune build rfsmc.bc

test:
	dune exec -- ./rfsmc.bc -dump_typed -dump_static -sim ./tests/gensig/main.fsm

clean:
	dune clean
	@rm *.vcd

clobber: clean
	@rm -f *~
