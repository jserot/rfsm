#include "e.h"
#include "rfsm.h"


void E::react()
{
  state = E1;
  cnt=1;
  while ( 1 ) {
    st = state;
    switch ( state ) {
    case E2:
      e_rdy.write(true);
      wait(h.posedge_event());
      wait(SC_ZERO_TIME);
      if ( r_rdy.read()==false ) {
        state = E1;
        }
      wait(SC_ZERO_TIME);
      break;
    case E1:
      e_rdy.write(false);
      wait(h.posedge_event());
      wait(SC_ZERO_TIME);
      if ( r_rdy.read()==true ) {
        data.write(cnt);
        cnt=cnt+1;
        state = E2;
        }
      wait(SC_ZERO_TIME);
      break;
    }
  }
};
