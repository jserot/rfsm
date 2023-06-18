#include "a2.h"
#include "rfsm.h"


void A2::react()
{
  state = S1;
  v2.write(false);
  while ( 1 ) {
    switch ( state ) {
    case S2:
      wait(h.posedge_event());
      wait(SC_ZERO_TIME);
      v1=false;
      state = S1;
      wait(SC_ZERO_TIME);
      break;
    case S1:
      wait(h.posedge_event());
      wait(SC_ZERO_TIME);
      if ( v1.read()==true ) {
        v2.write(true);
        state = S2;
        }
      wait(SC_ZERO_TIME);
      break;
    }
  }
};
