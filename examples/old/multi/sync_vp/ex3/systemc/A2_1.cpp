#include "A2_1.h"
#include "rfsm.h"


void A2_1::react()
{
  state = S3;
  while ( 1 ) {
    switch ( state ) {
    case S4:
      wait(h.posedge_event());
      wait(SC_ZERO_TIME);
      v=0;
      state = S3;
      wait(SC_ZERO_TIME);
      break;
    case S3:
      wait(h.posedge_event());
      wait(SC_ZERO_TIME);
      if ( v.read()==1 ) {
        state = S4;
        }
      wait(SC_ZERO_TIME);
      break;
    }
  }
};
