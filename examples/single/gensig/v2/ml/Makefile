OCB_FLAGS=-use-ocamlfind -no-hygiene -package ocamlgraph -package lascar
OCB = 		ocamlbuild $(OCB_FLAGS)

all: 		byte 

run:
	./main.byte
	for i in ./dot/*.dot; do open -a Graphviz $$i; done

.PHONY: tags

tags:
	otags *.ml *.mli

clean:
			$(OCB) -clean
			\rm -f dot/* ctask/* systemc/*

clobber: clean 
			\rm -f *~

byte: 		
			$(OCB) main.byte

debug: 
			$(OCB) -tag debug main.byte

.PHONY: all clean byte native profile debug test doc
