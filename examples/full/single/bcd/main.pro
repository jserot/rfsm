SRCS=convert.fsm main.fsm
DOT_OPTS=-dot_vert_trans
SIM_OPTS=#-sim_trace 1
SYSTEMC_OPTS=-sc_time_unit "SC_NS" -stop_time 200 
VHDL_OPTS=-vhdl_time_unit "ns" -stop_time 200 -vhdl_numeric_std -vhdl_dump_ghw
