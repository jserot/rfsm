SELF_DIR := $(dir $(lastword $(MAKEFILE_LIST)))

include $(SELF_DIR)/config

.PHONY: dot sim ctask systemc vhdl clean clobber test

VCDVIEWER=PATH=/Applications/gtkwave.app/Contents/MacOS:$(PATH) gtkwave-bin
TXTVIEWER=open

.phony: dot sim ctask systemc vhdl clean clobber test

dot:
	$(RFSMC) -dot -target_dir ./dot $(DOT_OPTS) $(SRCS)
	@$(eval files=`cat ./rfsm.output`)
	@for f in $(files); do \
		if [[ $$f = *.dot ]]; then \
			$(DOTVIEWER) $$f; \
		fi; \
	done

dot.run:
	$(RFSMC) -dot -target_dir ./dot $(DOT_OPTS) $(SRCS)

dot.test: dot.run

all: dot sim ctask systemc vhdl

all.test: dot.test sim.test ctask.test systemc.test vhdl.test

sim.run:
	$(RFSMC) $(SIM_OPTS) -sim -vcd "./sim/$(TB).vcd" $(SRCS)

sim.view:
	$(VCDVIEWER) -f ./sim/$(TB).vcd -a ./sim/$(TB).gtkw > /tmp/gtkwave.log 2>&1; echo $$?

sim: sim.run sim.view

sim.test: sim.run

ctask: ctask.code
ctask.run: ctask.code

ctask.code:
	$(RFSMC) -ctask -target_dir ./ctask $(CTASK_OPTS) $(SRCS)

ctask.view:
	@$(eval files=`cat ./rfsm.output`)
	@for f in $(files); do \
		if [[ $$f = *.c ]]; then \
			$(TXTVIEWER) $$f; \
		fi; \
	done

ctask: ctask.run ctask.view

ctask.test: ctask.code

systemc.code:
	$(RFSMC) -systemc -target_dir ./systemc -lib $(LIBDIR) $(SYSTEMC_OPTS) $(SRCS)

systemc.run: systemc.code
	if [ -d ./systemc ]; then cd ./systemc; make; make run; fi

systemc.view:
	if [ -d ./systemc ]; then cd ./systemc; make view; fi

systemc: systemc.run systemc.view

systemc.test: systemc.run

vhdl.code:
	$(RFSMC) -vhdl -target_dir ./vhdl -lib $(LIBDIR) $(VHDL_OPTS) $(SRCS)

vhdl.run: vhdl.code
	if [ -d ./vhdl ]; then cd ./vhdl; make; make run; fi

vhdl.view: 
	if [ -d ./vhdl ]; then cd ./vhdl; make view; fi

vhdl: vhdl.run vhdl.view

vhdl.test:
	if [ -d ./vhdl ]; then make vhdl.run; fi

new_syntax:
	for f in *.fsm ; do \
		$(RFSMC) -transl_syntax $$f; \
	done

test:
	if [ -d ./vhdl ]; then make test3; else make test2; fi

test2:
	make dot.run
	make sim.run
	make systemc.run

test3:
	make dot.run
	make sim.run
	make systemc.run
	make vhdl.run

view:
	if [ -d ./vhdl ]; then make view3; else make view2; fi

view2:
	make sim.run
	make systemc.run
	$(VCDVIEWER) -f ./sim/$(TB).vcd -a ./sim/$(TB).gtkw > /tmp/gtkwave.log 2>&1 &
	(cd ./systemc; make view) 

view3:
	make sim.run
	make systemc.run
	make vhdl.run
	$(VCDVIEWER) -f ./sim/$(TB).vcd -a ./sim/$(TB).gtkw > /tmp/gtkwave.log 2>&1 &
	(cd ./systemc; make view) &
	(cd ./vhdl; make view) 

clean:
	\rm -f dot/*
	\rm -f ctask/*
	\rm -f systemc/{*.o,tb,core,*.vcd}
	\rm -f vhdl/{work*.cf,*.o,tb,*.vcd}
	\rm -f sim/*.vcd
	\rm -f *_deps.dot
	\rm -f *.output *.vcd

clobber: clean
	\rm -f systemc/{*.cpp,*.h,Makefile,*~}
	\rm -f vhdl/{*.vhd,Makefile,*~}
	\rm -f *~
