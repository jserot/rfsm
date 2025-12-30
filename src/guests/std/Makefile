all:
	dune build ./lib/std.cma
	dune build ./lib/std.cmxa
	dune build ./bin/rfsmc.exe

clean:
	make -C lib clean
	make -C bin clean
