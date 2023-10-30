#include "g.h"
#include "rfsm.h"


void G::react()
{
  state = E0;
  S.write(false);
  while ( 1 ) {
    switch ( state ) {
    case E0:
      wait(H.posedge_event());
      if ( E.read()==true ) {
        S.write(true);
        k = 1;
        state = E1;
        }
      wait(SC_ZERO_TIME);
      break;
    case E1:
      wait(H.posedge_event());
      if ( k==3 ) {
        S.write(false);
        state = E0;
        }
      else if ( k<3 ) {
        S.write(true);
        k = k+1;
        }
      wait(SC_ZERO_TIME);
      break;
    }
  }
};

