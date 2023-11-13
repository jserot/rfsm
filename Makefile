.PHONY: doc.html doc.pdf tests

all: host std_guest doc

host:
	(cd src/host; make)

all_guests: std_guest other_guests

std_guest:
	(cd src/guests/std; make)

other_guests:
	for i in src/guests/others/{core,mini,simple,szdints,szvars}; do (cd $$i; make); done

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
