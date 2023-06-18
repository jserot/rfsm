#include "r.h"
#include "rfsm.h"

const int R::delai = 1;

void R::react()
{
  state = R1;
  while ( 1 ) {
    st = state;
    switch ( state ) {
    case R2:
      r_rdy.write(false);
      wait(h.posedge_event());
      wait(SC_ZERO_TIME);
      wait(SC_ZERO_TIME);
      if ( e_rdy.read()==false ) {
        state = R1;
        }
      wait(SC_ZERO_TIME);
      break;
    case R1:
      r_rdy.write(true);
      wait(h.posedge_event());
      wait(SC_ZERO_TIME);
      wait(SC_ZERO_TIME);
      if ( e_rdy.read()==true ) {
        dout.write(data.read());
        t=0;
        state = Att;
        }
      wait(SC_ZERO_TIME);
      break;
    case Att:
      wait(h.posedge_event());
      wait(SC_ZERO_TIME);
      wait(SC_ZERO_TIME);
      if ( t<1 ) {
        t=t+1;
        }
      else if ( t==1 ) {
        state = R2;
        }
      wait(SC_ZERO_TIME);
      break;
    }
  }
};
