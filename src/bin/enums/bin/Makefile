all: byte 

byte:
	dune build main.bc

test:
	dune exec -- ./main.bc -dump_parsed -dump_typed -dump_static -sim ./tests/simple/main.fsm

clean:
	dune clean
	@rm *.vcd

clobber: clean
	@rm -f *~
