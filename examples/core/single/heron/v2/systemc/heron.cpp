#include "heron.h"
#include "rfsm.h"
#include "globals.h"

const double Heron::eps = 1e-08;

void Heron::react()
{
  state = Idle;
  while ( 1 ) {
    switch ( state ) {
    case Iter:
      rdy.write(false);
      wait(h.posedge_event());
      if ( f_abs(((x*x)-a))<1e-08 ) {
        r.write(x);
        niter.write(n);
        state = Idle;
        }
      else if ( f_abs(((x*x)-a))>=1e-08 ) {
        x=(x+(a/x))/2.;
        n=n+1;
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
