SRCS=main.fsm
DOT_OPTS=-dot_no_captions -dot_short_trans -dot_boxed
SIM_OPTS=#-sim_trace 1
SYSTEMC_OPTS=-sc_time_unit "SC_NS" -sc_trace
VHDL_OPTS=-vhdl_time_unit "ns"
