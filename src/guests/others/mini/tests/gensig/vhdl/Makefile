LIBDIR=/Users/jserot/Dev/ml/rfsm/etc


include $(LIBDIR)/platform

GHDL=ghdl
GHDLOPTS=-fno-color-diagnostics -P$(LIBDIR)/lib/vhdl

all: run

run: main_tb
	$(GHDL) -r $(GHDLOPTS) main_tb --vcd=main_tb.vcd

view: run
	if [[ -f main_tb.ghw ]]; then \
		$(VCDVIEWER) main_tb.ghw main_tb.gtkw > /tmp/gtkwave.log 2>&1; echo $$?; \
	else \
		$(VCDVIEWER) main_tb.vcd main_tb.sav > /tmp/gtkwave.log 2>&1; echo $$?; \
	fi; \

clean:
	\rm -f work*.cf
	\rm -f *.o
	\rm -f main_tb
	\rm -f main_tb.vcd
	\rm -f main_tb.ghw

clobber: clean
	\rm -f *.vhd
	\rm -f *~
	\rm -f Makefile  # Generated
	\rm -rf html

rfsm.vhd: $(LIBDIR)/lib/vhdl/rfsm.vhd
	cp $< $@

main_tb: rfsm.vhd  g.vhd main_tb.vhd
	$(GHDL) -a $(GHDLOPTS) rfsm.vhd
	$(GHDL) -a $(GHDLOPTS) g.vhd
	$(GHDL) -a $(GHDLOPTS) main_top.vhd
	$(GHDL) -a $(GHDLOPTS) main_tb.vhd
	$(GHDL) -e $(GHDLOPTS) main_tb

