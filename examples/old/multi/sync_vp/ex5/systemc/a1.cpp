#include "a1.h"
#include "rfsm.h"


void A1::react()
{
  state = S1;
  v.write(false);
  while ( 1 ) {
    switch ( state ) {
    case S2:
      wait(h.posedge_event());
      state = S1;
      wait(SC_ZERO_TIME);
      break;
    case S1:
      wait(h.posedge_event());
      v.write(true);
      state = S2;
      wait(SC_ZERO_TIME);
      break;
    }
  }
};
