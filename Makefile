.PHONY: doc

all: build doc

build:
	dune build src/lib/rfsm.cma
	dune build src/lib/rfsm.cmxa
	dune build src/bin/rfsmc.bc
	dune build src/bin/rfsmc.exe

install:
	dune build @install

INSTALL_DOCDIR=`opam config var doc`

doc.view:
	open -a Safari _build/default/_doc/_html/index.html

doc:
	dune build @doc
	(cd ./doc; make)
	rm -rf ../../github.io/rfsm
	cp -r _build/default/_doc/_html ../../github.io/rfsm
	cp doc/rfsm.pdf ../../github.io/rfsm


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
