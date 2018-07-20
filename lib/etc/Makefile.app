SELF_DIR := $(dir $(lastword $(MAKEFILE_LIST)))

include $(SELF_DIR)/config

.PHONY: dot sim ctask systemc vhdl clean clobber test

VCDVIEWER=PATH=/Applications/gtkwave.app/Contents/MacOS:$(PATH) gtkwave-bin
TXTVIEWER=open

.phony: dot sim ctask systemc vhdl clean clobber test

dot:
	$(RFSMC) -dot -target_dir ./dot $(DOT_OPTS) $(APP).fsm
	@$(eval files=`cat ./rfsm.output`)
	@for f in $(files); do \
		if [[ $$f = *.dot ]]; then \
			$(DOTVIEWER) $$f; \
		fi; \
	done

dot.run:
	$(RFSMC) -dot -target_dir ./dot $(DOT_OPTS) $(APP).fsm

all: dot sim ctask systemc vhdl

sim.run:
	$(RFSMC) $(SIM_OPTS) -sim -vcd "./sim/$(APP).vcd" $(APP).fsm

sim.view:
	$(VCDVIEWER) -f ./sim/$(APP).vcd -a ./sim/$(APP).gtkw > /tmp/gtkwave.log 2>&1; echo $$?

sim: sim.run sim.view

ctask: ctask.code

ctask.code:
	$(RFSMC) -ctask -target_dir ./ctask $(CTASK_OPTS) $(APP).fsm

ctask.view:
	@$(eval files=`cat ./rfsm.output`)
	@for f in $(files); do \
		if [[ $$f = *.c ]]; then \
			$(TXTVIEWER) $$f; \
		fi; \
	done

ctask: ctask.run ctask.view

systemc.code:
	$(RFSMC) -systemc -target_dir ./systemc -lib $(LIBDIR) $(SYSTEMC_OPTS) $(APP).fsm

systemc.run: systemc.code
	if [ -d ./systemc ]; then cd ./systemc; make; make run; fi

systemc.view:
	if [ -d ./systemc ]; then cd ./systemc; make view; fi

systemc: systemc.run systemc.view

vhdl.code:
	$(RFSMC) -vhdl -target_dir ./vhdl -lib $(LIBDIR) $(VHDL_OPTS) $(APP).fsm

vhdl.run: vhdl.code
	if [ -d ./vhdl ]; then cd ./vhdl; make; make run; fi

vhdl.view: 
	if [ -d ./vhdl ]; then cd ./vhdl; make view; fi

vhdl: vhdl.run vhdl.view

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
	$(VCDVIEWER) -f ./sim/$(APP).vcd -a ./sim/$(APP).gtkw > /tmp/gtkwave.log 2>&1 &
	(cd ./systemc; make view) 

view3:
	make sim.run
	make systemc.run
	make vhdl.run
	$(VCDVIEWER) -f ./sim/$(APP).vcd -a ./sim/$(APP).gtkw > /tmp/gtkwave.log 2>&1 &
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
