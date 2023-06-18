#include "a2.h"
#include "rfsm.h"


void A2::react()
{
  state = S1;
  while ( 1 ) {
    switch ( state ) {
    case S3:
      wait(h.posedge_event());
      wait(SC_ZERO_TIME);
      state = S1;
      wait(SC_ZERO_TIME);
      break;
    case S2:
      wait(h.posedge_event());
      wait(SC_ZERO_TIME);
      if ( v.read()==true ) {
        v=false;
        state = S3;
        }
      wait(SC_ZERO_TIME);
      break;
    case S1:
      wait(h.posedge_event());
      wait(SC_ZERO_TIME);
      state = S2;
      wait(SC_ZERO_TIME);
      break;
    }
  }
};
