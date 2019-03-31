GHDL=ghdl
GHDLOPTS=-P/Users/jserot/Dev/ml/rfsm/working/lib/vhdl -fno-color-diagnostics
GTKWAVE=PATH=/Applications/gtkwave.app/Contents/MacOS:$(PATH) gtkwave-bin

all: run

run: $(TB)
#	$(GHDL) -r $(GHDLOPTS) $(TB) --vcd=$(TB).vcd
	$(GHDL) -r $(GHDLOPTS) $(TB) --wave=$(TB).ghw

view: run
#	$(GTKWAVE) -f $(TB).vcd -a $(TB).sav > /tmp/gtkwave.log 2>&1; echo $$?
	$(GTKWAVE) -f $(TB).ghw -a $(TB).sav > /tmp/gtkwave.log 2>&1; echo $$?

clean:
	\rm -f work*.cf
	\rm -f *.o
	\rm -f $(TB)
	\rm -f $(TB).vcd

clobber: clean
	\rm -f *~
	\rm -rf html
