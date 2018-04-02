GHDL=ghdl
GHDLOPTS=-fno-color-diagnostics

all: lib

lib: rfsm.vhd
	$(GHDL) -a --work=rfsm --workdir=. $(GHDLOPTS) rfsm.vhd

clean:
	\rm -f *.cf *.o

realclean: clean
	\rm -f *~

clobber: realclean
