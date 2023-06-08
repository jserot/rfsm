all: byte 

byte:
	dune build rfsmcc.bc

test:
	dune exec -- ./rfsmcc.bc -dump_typed -dump_static -sim ./tests/gensig/main.fsm

clean:
	dune clean
	@rm *.vcd

clobber: clean
	@rm -f *~
