.PHONY: doc tests

all: host guests doc

host:
	dune build src/lib/host/rfsm.cma
#	dune build src/lib/host/rfsm.cmxa

guests: core simple szdints szvars typarams

core:
	dune build src/bin/core/rfsmc.bc

simple:
	dune build src/bin/simple/rfsmc.bc

szdints:
	dune build src/bin/szdints/rfsmc.bc

szvars:
	dune build src/bin/szvars/rfsmc.bc

typarams:
	dune build src/bin/typarams/rfsmc.bc

install:
	dune build @install

INSTALL_DOCDIR=`opam var doc`

doc.view:
	open -a Safari _build/default/_doc/_html/index.html

doc:
	dune build @doc
	(cd ./doc; make)
	rm -rf ./docs/*
	cp -r _build/default/_doc/_html/* ./docs
	cp doc/rfsm.pdf ./docs


html: README.md
	pandoc -t html -o README.html README.md
	pandoc -t html -o CHANGES.html CHANGES.md

toplevel:
	dune exec ./src/bin/rfsmtop.exe

tests:
	(cd examples; ./do_tests dot)
	(cd examples; ./do_tests sim)

clean:
	dune clean
	(cd examples; make clean)
	\rm -f README.html CHANGES.html

clobber: clean
	\rm -f *~
