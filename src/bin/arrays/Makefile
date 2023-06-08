all: byte 

byte:
	dune build main.bc

test:
	dune exec -- ./main.bc -dump_typed -dump_static -sim ./tests/fir/main.fsm

clean:
	dune clean
	@rm *.vcd

clobber: clean
	@rm -f *~
