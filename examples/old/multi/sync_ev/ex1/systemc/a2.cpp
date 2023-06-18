#include "a2.h"
#include "rfsm.h"


void A2::react()
{
  state = A;
  while ( 1 ) {
    st = state;
    switch ( state ) {
    case B:
      wait(h.posedge_event());
      wait(SC_ZERO_TIME);
      state = A;
      wait(SC_ZERO_TIME);
      break;
    case A:
      wait(e.posedge_event());
      wait(SC_ZERO_TIME);
      state = B;
      wait(SC_ZERO_TIME);
      break;
    }
  }
};
