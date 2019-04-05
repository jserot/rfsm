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
			if [ -d ../../lib/ml ]; then rm -f ../../lib/ml/*; else mkdir ../../lib/ml; fi
			cp _build/rfsm.cma ../../lib/ml
			cp _build/*.cmo _build/*.cmi ../../lib/ml
			cp *.mli ../../lib/ml

native:
			$(OCB) rfsm.cmxa
			if [ -d ../../lib/ml ]; then rm -f ../../lib/ml/*; else mkdir ../../lib/ml; fi
			cp _build/rfsm.cma _build/rfsm.cmxa _build/rfsm.a  ../../lib/ml
			cp _build/*.cmx _build/*.cmi ../../lib/ml
			cp *.mli ../../lib/ml

top:
			$(OCB) rfsm.top

profile:
			$(OCB) -tag profile rfsm.cmxa

debug: _build/main.ml
			$(OCB) -tag debug rfsm.cma
			echo "all:\n" > _build/Makefile
			echo "\tocamlc -g  -c main.cmo main.ml" >> _build/Makefile
			echo "\tocamlc -g  -I ~/.opam/4.06.0/lib/lascar -I ~/.opam/4.06.0/lib/ocamlgraph -o main.byte utils.cma ltsa.cma graph.cma rfsm.cmo main.cmo" >> _build/Makefile
			(cd _build; make)
#			\rm -f ../../lib/ml/*      # Note 2018-03-22: debugging with rfsm.cma crashes ocamldebug :(
#			cp _build/* ../../lib/ml

DOCSRCS=*.mli *.ml

doc: html

html:
	$(OCB) rfsm.docdir/index.html

deps.dot:
	$(OCB) rfsm.docdir/rfsm.dot
	sed -i '' '/rotate=/d'  _build/rfsm.docdir/rfsm.dot
#	open -a Graphviz rfsm.docdir/rfsm.dot
#	ocamldoc -I ./_build $(OCDOC_INCLS) -short-functors -d ./doc -html -colorize-code $(DOCSRCS)

.PHONY: all clean byte native profile debug doc deps.dot
