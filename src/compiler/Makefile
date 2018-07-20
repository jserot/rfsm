OCB_FLAGS=-use-ocamlfind -use-menhir -I lib -no-hygiene -package ocamlgraph -package lascar 
OPAMLIBDIR=`opam config var lib`
OBELISK=obelisk

OCB = 		ocamlbuild $(OCB_FLAGS)

all: 		byte native # profile debug

byte: 
			$(OCB) main.byte

native:
			$(OCB) main.native

tags:
	otags *.ml *.mli

clean:
			$(OCB) -clean

clobber: clean 
			\rm -f *~

profile:
			$(OCB) -tag profile main.native

debug: 
			$(OCB) -tag debug main.byte

# options_spec.ml: options_spec.txt
# 	echo "let options_spec = [" > options_spec.ml
# 	awk -F ";" '{ printf("\"%s\", %s (Options.%s), \"%s\";\n", $$3, $$4, $$5, $$6) }' < options_spec.txt >> options_spec.ml
# 	echo "];" >> options_spec.ml

gen_tex_option = \
	awk -F ";" '{ printf("%s & %s\\\\\n", $$3, $$6) }' < $(1) | sed 's/_/\\_/g' > $(2)

options.tex: options_spec.txt
	$(call gen_tex_option,options_spec.txt,../../doc/um/rfsmc-options.tex)

grammar.tex:
	$(OBELISK) latex -i -prefix rfsm -o grammar.tex -package grammar-defns main_parser.mly

.PHONY: all clean byte native profile debug tags doc options.tex grammar.tex


