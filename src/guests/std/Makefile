all:
	dune build ./lib/std.cma
	dune build ./lib/std.cmxa
	dune build ./bin/rfsmc.exe

clean:
	(cd lib; make clean)
	(cd bin; make clean)
