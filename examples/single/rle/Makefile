RFSM=/Users/jserot/Dev/ml/rfsm/working

SRCS=rle.fsm main.fsm
DOT_OPTS=#-dot_actions_nl
SIM_OPTS=#-trace 1
SYSTEMC_OPTS=-sc_time_unit "SC_NS" -stop_time 100 -sc_trace
VHDL_OPTS=-vhdl_time_unit "ns" -stop_time 100 -vhdl_numeric_std

include $(RFSM)/lib/etc/Makefile.app
