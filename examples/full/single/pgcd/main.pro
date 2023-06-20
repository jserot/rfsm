SRCS=main.fsm
DOT_OPTS=-dot_vert_trans #-dot_show_models
SIM_OPTS=-synchronous_actions
SYSTEMC_OPTS=-sc_time_unit "SC_NS" -stop_time 120 -sc_trace
