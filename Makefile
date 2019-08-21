.PHONY: doc

all: build doc

build:
	dune build src/lib/rfsm.cma
	dune build src/lib/rfsm.cmxa
	dune build src/bin/rfsmc.bc
	dune build src/bin/rfsmc.exe

utop:
	dune utop src

install:
	dune build @install

INSTALL_DOCDIR=`opam config var doc`

opam.install: 
	opam install .
	rm -rf $(INSTALL_DOCDIR)/rfsm
	cp -r _build/default/_doc/_html/ $(INSTALL_DOCDIR)/rfsm

opam.remove:
	opam remove .
	rm -rf $(INSTALL_DOCDIR)/rfsm

opam.show:
	opam info rfsm

doc.view:
	open -a Safari _build/default/_doc/_html/index.html

doc:
	dune build @doc

html: README.md
	pandoc -t html -o README.html README.md
	pandoc -t html -o CHANGES.html CHANGES.md

test:
	(cd examples; make)

clean:
	dune clean
	(cd examples; make clean)
	\rm -f README.html

clobber: clean
	\rm -f *~


