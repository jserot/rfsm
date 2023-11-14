#include "gensig.h"
#include "rfsm.h"


void Gensig::react()
{
  state = E0;
  s.write(false);
  while ( 1 ) {
    switch ( state ) {
    case E0:
      wait(h.posedge_event());
      if ( e ) {
        s.write(true);
        state = E1;
        }
      wait(SC_ZERO_TIME);
      break;
    case E1:
      wait(h.posedge_event());
      state = E2;
      wait(SC_ZERO_TIME);
      break;
    case E2:
      wait(h.posedge_event());
      state = E3;
      wait(SC_ZERO_TIME);
      break;
    case E3:
      wait(h.posedge_event());
      s.write(false);
      state = E0;
      wait(SC_ZERO_TIME);
      break;
    }
  }
};

