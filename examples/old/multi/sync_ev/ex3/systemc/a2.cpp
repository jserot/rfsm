#include "a2.h"
#include "rfsm.h"


void A2::react()
{
  state = D;
  while ( 1 ) {
    switch ( state ) {
    case F:
      wait(e.posedge_event());
      wait(SC_ZERO_TIME);
      state = D;
      wait(SC_ZERO_TIME);
      break;
    case E:
      wait(h.posedge_event());
      wait(SC_ZERO_TIME);
      state = F;
      wait(SC_ZERO_TIME);
      break;
    case D:
      wait(e.posedge_event());
      wait(SC_ZERO_TIME);
      state = E;
      wait(SC_ZERO_TIME);
      break;
    }
  }
};
