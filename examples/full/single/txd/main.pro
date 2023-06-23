SRCS=txd.fsm main.fsm
DOT_OPTS=-dot_vert_trans
SIM_OPTS=
SYSTEMC_OPTS=-sc_time_unit "SC_NS" -stop_time 550 -sc_trace
VHDL_OPTS=-vhdl_time_unit "ns" -vhdl_numeric_std -stop_time 550
