DOC=rfsm_um

SRCS= ex1-c.tex \
	ex1-systemc.tex \
	ex1-vhdl.tex \
	introduction.tex \
	overview.tex \
	language.tex \
	rfsm_um.tex

all: $(DOC).pdf

$(DOC).pdf: $(SRCS) $(GEN_SRCS)
	pdflatex $(DOC).tex > pdflatex.log
	pdflatex $(DOC).tex > pdflatex.log

clean:
	\rm -f *.dvi *.log *.rel *.aux *.toc

realclean: clobber

clobber: clean
	\rm -f $(DOC).pdf *~ 
