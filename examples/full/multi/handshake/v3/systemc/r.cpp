#include "r.h"
#include "rfsm.h"


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
      wait(SC_ZERO_TIME);
      if ( e_rdy.read()==true ) {
        dout.write(data.read());
        state = R2;
        }
      wait(SC_ZERO_TIME);
      break;
    }
  }
};
