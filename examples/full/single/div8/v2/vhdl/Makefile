include /Users/jserot/Dev/ml/rfsm/working/lib/etc/Makefile.vhdl

tb:  d8.vhd tb.vhd
	$(GHDL) -a $(GHDLOPTS) d8.vhd
	$(GHDL) -a $(GHDLOPTS) top.vhd
	$(GHDL) -a $(GHDLOPTS) tb.vhd
	$(GHDL) -e $(GHDLOPTS) tb
