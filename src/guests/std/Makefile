all:
	dune build ./lib/full.cma
	dune build ./lib/full.cmxa
	dune build ./bin/rfsmc.bc
	dune build ./bin/rfsmc.exe

clean:
	(cd lib; make clean)
	(cd bin; make clean)
