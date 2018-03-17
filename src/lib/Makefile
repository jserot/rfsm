OCB_FLAGS=-use-ocamlfind -use-menhir -no-hygiene -package ocamlgraph -package lascar 
OPAMLIBDIR=`opam config var lib`
OCDOC_INCLS=-I $(OPAMLIBDIR)/lascar -I $(OPAMLIBDIR)/ocamlgraph

OCB = 		ocamlbuild $(OCB_FLAGS)

all: 		byte native # profile debug

.PHONY: tags

tags:
	otags *.ml *.mli

clean:
			$(OCB) -clean

clobber: clean 
			\rm -f *~

byte:
			$(OCB) rfsm.cma
			\rm -f ../../lib/ml/*
			cp _build/rfsm.cma ../../lib/ml
			cp _build/*.{cmo,cmi} ../../lib/ml
			cp *.mli ../../lib/ml

native:
			$(OCB) rfsm.cmxa
			cp _build/rfsm.{cmxa,a}  ../../lib/ml
			cp _build/*.{cmo,cmi}  ../../lib/ml
			cp *.mli ../../lib/ml

top:
			$(OCB) rfsm.top

profile:
			$(OCB) -tag profile rfsm.cmxa

debug: 
			$(OCB) -tag debug rfsm.cma

DOCSRCS=*.mli *.ml

doc: html

html:
	$(OCB) rfsm.docdir/index.html

deps.dot:
	$(OCB) rfsm.docdir/rfsm.dot
	sed -i '' '/rotate=/d'  rfsm.docdir/rfsm.dot
#	open -a Graphviz rfsm.docdir/rfsm.dot
#	ocamldoc -I ./_build $(OCDOC_INCLS) -short-functors -d ./doc -html -colorize-code $(DOCSRCS)

.PHONY: all clean byte native profile debug doc deps.dot
