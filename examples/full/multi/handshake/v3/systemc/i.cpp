#include "i.h"
#include "rfsm.h"

const int I::delai = 5;

void I::react()
{
  state = R1;
  r_rdy1.write(true);
  e_rdy2.write(false);
  while ( 1 ) {
    st = state;
    switch ( state ) {
    case U:
      wait(h.posedge_event());
      wait(SC_ZERO_TIME);
      wait(SC_ZERO_TIME);
      if ( t<5 ) {
        t=t+1;
        }
      else if ( t==5 ) {
        state = E1;
        }
      wait(SC_ZERO_TIME);
      break;
    case R2:
      wait(h.posedge_event());
      wait(SC_ZERO_TIME);
      wait(SC_ZERO_TIME);
      if ( e_rdy1.read()==false ) {
        r_rdy1.write(true);
        t=0;
        state = U;
        }
      wait(SC_ZERO_TIME);
      break;
    case R1:
      wait(h.posedge_event());
      wait(SC_ZERO_TIME);
      wait(SC_ZERO_TIME);
      if ( e_rdy1.read()==true ) {
        data=din.read();
        r_rdy1.write(false);
        state = R2;
        }
      wait(SC_ZERO_TIME);
      break;
    case E2:
      wait(h.posedge_event());
      wait(SC_ZERO_TIME);
      wait(SC_ZERO_TIME);
      if ( r_rdy2.read()==false ) {
        e_rdy2.write(false);
        state = R1;
        }
      wait(SC_ZERO_TIME);
      break;
    case E1:
      wait(h.posedge_event());
      wait(SC_ZERO_TIME);
      wait(SC_ZERO_TIME);
      if ( r_rdy2.read()==true ) {
        dout.write(data);
        e_rdy2.write(true);
        state = E2;
        }
      wait(SC_ZERO_TIME);
      break;
    }
  }
};
