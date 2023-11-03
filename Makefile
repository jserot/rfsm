.PHONY: doc.html doc.pdf tests

all: host guest_full guest_core # doc

host:
	(cd src/host; make)

guests: full others

guest_full:
	(cd src/guests/full; make)

guest_core:
	(cd src/guests/core; make)

other_guests:
	for i in src/guests/others/{simple,szdints,szvars}; do (cd $$i; make); done

install:
	dune build @install

INSTALL_DOCDIR=`opam var doc`

doc.view:
	open -a Safari _build/default/_doc/_html/index.html

doc.html:
	dune build @doc

doc.pdf:
	(cd ./doc; make)
#	rm -rf ./docs/*
#	cp -r _build/default/_doc/_html/* ./docs
#	cp doc/rfsm.pdf ./docs

html: README.md
	pandoc -t html -o README.html README.md
	pandoc -t html -o CHANGES.html CHANGES.md

clean:
	dune clean
	(cd examples; make clean)

clobber: clean
	rm -f *~
