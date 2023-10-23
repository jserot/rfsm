all: options.ml byte native

options.ml: options_spec.txt options.cppo.ml
	./build_options_spec options_spec.txt options.cppo.ml options.ml
# Note: this should really be in the [dune] file (see)
# But for obscure reasons, this does not work when building under Windows :(

byte:
	dune build rfsm.cma

native:
	dune build rfsm.cmxa

doc:
	dune build @doc

clean:
	dune clean
	@rm -f options.ml

clobber: clean
	@rm -f *~
