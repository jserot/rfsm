all:
	(cd systemc; make)
	(cd vhdl; make)

clean:
	(cd systemc; make clean)
	(cd vhdl; make clean)

clobber: clean
	(cd systemc; make clobber)
	(cd vhdl; make clobber)
	\rm -f ml/*
	\rm -f etc/*~

