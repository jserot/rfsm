#include "a1.h"
#include "rfsm.h"


void A1::react()
{
  state = A;
  while ( 1 ) {
    st = state;
    switch ( state ) {
    case C:
      wait(h.posedge_event());
      state = A;
      wait(SC_ZERO_TIME);
      break;
    case B:
      wait(h.posedge_event());
      state = C;
      wait(SC_ZERO_TIME);
      break;
    case A:
      wait(h.posedge_event());
      notify_ev(e,"e");
      state = B;
      wait(SC_ZERO_TIME);
      break;
    }
  }
};
