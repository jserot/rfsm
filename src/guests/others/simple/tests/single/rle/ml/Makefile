all: build run view.all

build:
	dune build ./main.bc

run:
	dune exec ./main.bc

view.all:
	for i in ./dot/*.dot; do open -a Graphviz $$i; done

view:
	open -a Graphviz ./dot/heron.dot

.PHONY: tags

clean:
	\rm -f dot/* ctask/* systemc/*

clobber: clean 
	\rm -f *~
