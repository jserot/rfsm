SRCS=mul.fsm main.fsm
GEN_OPTS=-show_models
DOT_OPTS=
SIM_OPTS=
SYSTEMC_OPTS=-sc_time_unit "SC_NS" -stop_time 140 # -trace
VHDL_OPTS=-vhdl_time_unit "ns" -stop_time 140 -vhdl_numeric_std
