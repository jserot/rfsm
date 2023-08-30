#include "d8.h"
#include "rfsm.h"


void D8::react()
{
  state = Repos;
  rdy.write(true);
  while ( 1 ) {
    switch ( state ) {
    case Repos:
      wait(h.posedge_event());
      if ( start.read()==true ) {
        a=(sc_uint<16> )(n1.read());
        b=(sc_uint<16> )(n2.read())*128;
        c=0;
        rdy.write(false);
        i=0;
        state = Calcul;
        }
      wait(SC_ZERO_TIME);
      break;
    case Calcul:
      wait(h.posedge_event());
      if ( i<8 && a<b ) {
        c=c*2;
        i=i+1;
        b=b/2;
        }
      else if ( i<8 && a>=b ) {
        a=a-b;
        c=(c*2)+1;
        i=i+1;
        b=b/2;
        }
      else if ( i==8 ) {
        r.write(c);
        rdy.write(true);
        state = Repos;
        }
      wait(SC_ZERO_TIME);
      break;
    }
  }
};
