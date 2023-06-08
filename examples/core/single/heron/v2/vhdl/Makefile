LIBDIR=/Users/jserot/.opam/4.10.0/share/rfsm

include $(LIBDIR)/platform

GHDL=ghdl
GHDLOPTS=-fno-color-diagnostics -P$(LIBDIR)/lib/vhdl

all: run

run: main_tb
	$(GHDL) -r $(GHDLOPTS) main_tb --vcd=main_tb.vcd

view: run
	$(VCDVIEWER) main_tb.vcd main_tb.gtkw > /tmp/gtkwave.log 2>&1; echo $$?

clean:
	\rm -f work*.cf
	\rm -f *.o
	\rm -f rfsm.vhd
	\rm -f main_tb
	\rm -f main_tb.vcd
	\rm -f main_tb.ghw

clobber: clean
	\rm -f *~
	\rm -rf html

rfsm.vhd: $(LIBDIR)/lib/vhdl/rfsm.vhd
	cp $< $@

main_tb: rfsm.vhd globals.vhd heron.vhd main_tb.vhd
	$(GHDL) -a $(GHDLOPTS) rfsm.vhd
	$(GHDL) -a $(GHDLOPTS) globals.vhd
	$(GHDL) -a $(GHDLOPTS) heron.vhd
	$(GHDL) -a $(GHDLOPTS) main_top.vhd
	$(GHDL) -a $(GHDLOPTS) main_tb.vhd
	$(GHDL) -e $(GHDLOPTS) main_tb
