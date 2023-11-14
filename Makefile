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

doc: doc.html doc.pdf

doc.view:
	open -a Safari _build/default/_doc/_html/index.html

doc.html:
	dune build @doc
	cp -r -p _build/default/_doc/_html/* ./docs

doc.pdf:
	(cd ./docs/user_manual; make)
	(cd ./docs/ref_manual; make)

clean:
	dune clean
	(cd examples; make clean)

clobber: clean
	rm -f *~
