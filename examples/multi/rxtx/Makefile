SRCS=types.fsm txd.fsm rxd.fsm main.fsm
APP=main
DOT_OPTS=#-dot_fsm_models
SIM_OPTS=
SYSTEMC_OPTS=-sc_time_unit "SC_NS" -stop_time 550
VHDL_OPTS=-vhdl_time_unit "ns" -vhdl_numeric_std -stop_time 550 -vhdl_trace

include ../../../lib/etc/Makefile.app
