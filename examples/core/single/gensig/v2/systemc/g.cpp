#include "g.h"
#include "rfsm.h"

const int G::n = 4;

void G::react()
{
  state = E0;
  s.write(false);
  while ( 1 ) {
    switch ( state ) {
    case E1:
      wait(h.posedge_event());
      if ( k<4 ) {
        s.write(true);
        k=k+1;
        }
      else if ( k==4 ) {
        s.write(false);
        state = E0;
        }
      wait(SC_ZERO_TIME);
      break;
    case E0:
      wait(h.posedge_event());
      if ( e.read()==true ) {
        s.write(true);
        k=1;
        state = E1;
        }
      wait(SC_ZERO_TIME);
      break;
    }
  }
};
