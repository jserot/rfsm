OCB_FLAGS=-use-ocamlfind -no-hygiene -package ocamlgraph -package lascar
OCB = ocamlbuild $(OCB_FLAGS)

all: byte 

run:
	./main.byte

view.all:
	for i in ./dot/*.dot; do open -a Graphviz $$i; done

view:
	open -a Graphviz ./dot/chrono_top.dot

.PHONY: tags

tags:
	otags *.ml *.mli

clean:
			$(OCB) -clean
			\rm -f dot/* ctask/* systemc/*

clobber: clean 
			\rm -f *~

byte: main.ml
			$(OCB) main.byte

debug: 
			$(OCB) -tag debug main.byte

.PHONY: all clean byte native profile debug test doc
