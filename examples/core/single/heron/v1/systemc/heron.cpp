#include "heron.h"
#include "rfsm.h"

const int Heron::niter = 8;

void Heron::react()
{
  state = Idle;
  while ( 1 ) {
    switch ( state ) {
    case Iter:
      rdy.write(false);
      wait(h.posedge_event());
      if ( n<8 ) {
        x=(x+(a/x))/2.;
        n=n+1;
        }
      else if ( n==8 ) {
        r.write(x);
        state = Idle;
        }
      wait(SC_ZERO_TIME);
      break;
    case Idle:
      rdy.write(true);
      wait(h.posedge_event());
      if ( start.read()==true ) {
        a=u.read();
        x=u.read();
        n=0;
        state = Iter;
        }
      wait(SC_ZERO_TIME);
      break;
    }
  }
};
