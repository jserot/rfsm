SRCS=dimp.fsm main.fsm
GEN_OPTS=-show_models
SIM_OPTS=
SYSTEMC_OPTS=-sc_time_unit "SC_NS" -stop_time 280 -sc_trace
VHDL_OPTS=-vhdl_time_unit "ns" -stop_time 280 -vhdl_trace
