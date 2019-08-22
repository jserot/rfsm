#ifndef _rfsm_h
#define _rfsm_h

#define SYSTEMC_EVENT_DURATION_UNIT SC_PS
#define SYSTEMC_EVENT_DURATION 1

void notify_ev(sc_out<bool> &s, const char *name);

#endif
