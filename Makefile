.PHONY: doc tests

all: host guests # doc

host:
	(cd src/host; make)

guests: full others

full:
	(cd src/guests/full; make)

others:
	for i in src/guests/others/{core,simple,szdints,szvars}; do (cd $$i; make); done

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

clean:
	dune clean
	(cd examples; make clean)
	\rm -f README.html CHANGES.html

clobber: clean
	\rm -f *~
