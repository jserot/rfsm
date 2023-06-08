all: byte native

byte:
	dune build rfsm.cma

native:
	dune build rfsm.cmxa

doc:
	dune build @doc

clean:
	dune clean

clobber: clean
	@rm -f *~
