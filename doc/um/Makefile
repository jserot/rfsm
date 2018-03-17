DOC=rfsm

all: $(DOC).pdf

$(DOC).pdf: 
	pdflatex $(DOC).tex > pdflatex.log
	pdflatex $(DOC).tex > pdflatex.log

clean:
	\rm -f *.dvi *.log *.rel *.aux *.toc

.PHONY: 

realclean: clobber

clobber: clean
	\rm -f $(DOC).pdf *~ 
