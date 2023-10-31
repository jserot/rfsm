#include "gensig.h"
#include "rfsm.h"


template <int n>
void Gensig::react()
{
  state = E0;
  while ( 1 ) {
    switch ( state ) {
    case E0:
      s.write(false);
      wait(h.posedge_event());
      if ( e==true ) {
        k = 1;
        state = E1;
        }
      wait(SC_ZERO_TIME);
      break;
    case E1:
      s.write(true);
      wait(h.posedge_event());
      if ( k==n ) {
        state = E0;
        }
      else if ( k<n ) {
        k = k+1;
        }
      wait(SC_ZERO_TIME);
      break;
    }
  }
};

