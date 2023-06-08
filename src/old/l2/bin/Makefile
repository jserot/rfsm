all: byte 

byte:
	dune build main.exe

run:
	dune exec -- ./main.exe -dump_tenv -dump_typed -dump_eenv ./tests/p1.pgm
	dune exec -- ./main.exe -dump_tenv -dump_typed -dump_eenv ./tests/p2.pgm

clean:
	dune clean

clobber: clean
	@rm -f *~
