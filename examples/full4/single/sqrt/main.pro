SRCS=main.fsm
DOT_OPTS=#-dot_show_models
SYSTEMC_OPTS=-sc_time_unit "SC_NS" -stop_time 1000 -sc_trace
SIM_OPTS=-vcd_int_size 16
VHDL_OPTS=-vhdl_bool_as_bool
