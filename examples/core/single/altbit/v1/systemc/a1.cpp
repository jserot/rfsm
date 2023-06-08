#include "a1.h"
#include "rfsm.h"


void A1::react()
{
  state = Init;
  while ( 1 ) {
    st = state;
    switch ( state ) {
    case R:
      wait(h.posedge_event());
      if ( last!=e.read() ) {
        last=e.read();
        }
      else if ( last==e.read() ) {
        notify_ev(err,"err");
        }
      wait(SC_ZERO_TIME);
      break;
    case Init:
      wait(h.posedge_event());
      if ( e.read()==false ) {
        last=false;
        state = R;
        }
      else if ( e.read()==true ) {
        last=true;
        state = R;
        }
      wait(SC_ZERO_TIME);
      break;
    }
  }
};
