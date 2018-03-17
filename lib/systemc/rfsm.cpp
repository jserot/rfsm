#include "systemc.h"
#include "rfsm.h"

void notify_ev(sc_out<bool> &s, const char* name)
{
  s.write(1);
#ifdef SYSTEMC_TRACE_EVENTS
  cout << "rsfm.cpp: notify_ev(" << name << ") @ t=" << sc_time_stamp() << " (delta=" << sc_delta_count() << ")" << endl;
#endif
  wait(SYSTEMC_EVENT_DURATION,SYSTEMC_EVENT_DURATION_UNIT);
  s.write(0);
}

// void notify_ev(sc_signal<bool> &s)
// {
//   s.write(1);
// #ifdef SYSTEMC_TRACE_EVENTS
//   cout << "rsfm.cpp: notify_ev(" << s.name() << ") @ t=" << sc_time_stamp() << " (delta=" << sc_delta_count() << ")" << endl;
// #endif
//   //wait(SYSTEMC_EVENT_DURATION,SYSTEMC_EVENT_DURATION_UNIT);
//   sc_start(SYSTEMC_EVENT_DURATION,SYSTEMC_EVENT_DURATION_UNIT);
//   s.write(0);
// }
