#include "a3.h"
#include "rfsm.h"


void A3::react()
{
  state = S1;
  while ( 1 ) {
    switch ( state ) {
    case S2:
      wait(h.posedge_event());
      wait(SC_ZERO_TIME);
      wait(SC_ZERO_TIME);
      v=false;
      state = S1;
      wait(SC_ZERO_TIME);
      break;
    case S1:
      wait(h.posedge_event());
      wait(SC_ZERO_TIME);
      wait(SC_ZERO_TIME);
      if ( v.read()==true ) {
        state = S2;
        }
      wait(SC_ZERO_TIME);
      break;
    }
  }
};
