SRCS=emetteur.fsm recepteur.fsm main.fsm
DOT_OPTS=
SIM_OPTS=
SYSTEMC_OPTS=-sc_time_unit "SC_NS" -stop_time 400 -sc_trace
VHDL_OPTS=-vhdl_time_unit "ns" -vhdl_numeric_std -stop_time 400 -vhdl_trace
