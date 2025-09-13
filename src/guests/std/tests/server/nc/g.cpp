#include "g.h"
#include "rfsm.h"


void G::react()
{
  state = E0;
  s1.write(false);
  while ( 1 ) {
    switch ( state ) {
    case E0:
      wait(H.posedge_event());
      if ( e.read()==true ) {
        s1.write(true);
        state = E1;
        }
      wait(SC_ZERO_TIME);
      break;
    case E1:
      wait(H.posedge_event());
      s1.write(true);
      state = E2;
      wait(SC_ZERO_TIME);
      break;
    case E2:
      wait(H.posedge_event());
      s1.write(true);
      state = E3;
      wait(SC_ZERO_TIME);
      break;
    case E3:
      wait(H.posedge_event());
      s1.write(false);
      state = E0;
      wait(SC_ZERO_TIME);
      break;
    }
  }
};

