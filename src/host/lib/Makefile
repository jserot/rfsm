all: byte native

byte:
	dune build rfsm.cma
native:
	dune build rfsm.cmxa

doc:
	dune build @doc

ifeq ($(OS),Windows_NT)
clean:
	cmd /c "if exist _build rmdir /s /q _build"
else
clean:
	dune clean
endif

clobber: clean
	@rm -f *~
