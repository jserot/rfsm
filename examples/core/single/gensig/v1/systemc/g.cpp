#include "g.h"
#include "rfsm.h"


void G::react()
{
  state = E0;
  while ( 1 ) {
    switch ( state ) {
    case E3:
      s.write(true);
      wait(h.posedge_event());
      state = E0;
      wait(SC_ZERO_TIME);
      break;
    case E2:
      s.write(true);
      wait(h.posedge_event());
      state = E3;
      wait(SC_ZERO_TIME);
      break;
    case E1:
      s.write(true);
      wait(h.posedge_event());
      state = E2;
      wait(SC_ZERO_TIME);
      break;
    case E0:
      s.write(false);
      wait(h.posedge_event());
      if ( e.read()==true ) {
        state = E1;
        }
      wait(SC_ZERO_TIME);
      break;
    }
  }
};
