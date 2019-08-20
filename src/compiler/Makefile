all: byte native

byte:
	dune build rfsmc.bc

native:
	dune build rfsmc.exe

clean:
	dune clean

clobber: clean 
	\rm -f *~
	\rm -f rfsm.output
