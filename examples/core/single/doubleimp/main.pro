SRCS=dimp.fsm main.fsm
DOT_OPTS=-dot_show_models -dot_vert_trans
SIM_OPTS=
SYSTEMC_OPTS=-sc_time_unit "SC_NS" -stop_time 280 -sc_trace
VHDL_OPTS=-vhdl_time_unit "ns" -stop_time 280 -vhdl_trace
