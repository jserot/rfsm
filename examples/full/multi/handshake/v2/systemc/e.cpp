#include "e.h"
#include "rfsm.h"

const int E::delai = 3;

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
        t=0;
        state = Att;
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
    case Att:
      e_rdy.write(false);
      wait(h.posedge_event());
      wait(SC_ZERO_TIME);
      if ( t<3 ) {
        t=t+1;
        }
      else if ( t==3 ) {
        state = E1;
        }
      wait(SC_ZERO_TIME);
      break;
    }
  }
};
