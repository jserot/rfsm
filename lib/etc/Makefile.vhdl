GHDL=ghdl
GHDLOPTS=-P/Users/jserot/Dev/ml/rfsm/working/lib/vhdl -fno-color-diagnostics
GTKWAVE=PATH=/Applications/gtkwave.app/Contents/MacOS:$(PATH) gtkwave-bin

all: run

run: tb
	$(GHDL) -r $(GHDLOPTS) tb --vcd=tb.vcd

view: run
	$(GTKWAVE) -f tb.vcd -a tb.sav > /tmp/gtkwave.log 2>&1; echo $$?

clean:
	\rm -f work*.cf
	\rm -f *.o
	\rm -f tb
	\rm -f tb.vcd

clobber: clean
	\rm -f *~
	\rm -rf html
