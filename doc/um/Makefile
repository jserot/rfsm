DOC=rfsm

all: $(DOC).pdf

$(DOC).pdf: 
	pdflatex $(DOC).tex > pdflatex.log
	pdflatex $(DOC).tex > pdflatex.log

clean:
	\rm -f *.dvi *.log *.rel *.aux *.toc

.PHONY: grammar.tex

grammar.tex:
	@read -p "** About to backup ./grammar.tex and to overwrite it with ../../src/compiler/grammar.tex. Are you sure ? " r;\
	if [ $$r = "yes" ]; then (cp ./grammar.tex grammar.sav.tex; cp ../../src/compiler/grammar.tex .; echo "** Done. Now diff-edit grammar.sav.tex and grammar.tex to integrate changes"); fi

realclean: clobber

clobber: clean
	\rm -f $(DOC).pdf *~ 
