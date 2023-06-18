#include "a2.h"
#include "rfsm.h"


void A2::react()
{
  state = C;
  while ( 1 ) {
    switch ( state ) {
    case D:
      wait(h.posedge_event() | e.posedge_event());
      if ( h.read() ) {
        state = C;
        }
      else if ( e.read() ) {
        }
      wait(SC_ZERO_TIME);
      break;
    case C:
      wait(h.posedge_event() | e.posedge_event());
      if ( h.read() ) {
        }
      else if ( e.read() ) {
        state = D;
        }
      wait(SC_ZERO_TIME);
      break;
    }
  }
};
