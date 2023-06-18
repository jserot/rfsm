#include "inp_H2.h"
#include "rfsm.h"

static int _dates[1] = { 45 };

void Inp_H2::gen()
{
  int _i=0, _t=0;
  while ( _i < 1 ) {
    wait(_dates[_i]-_t, SC_NS);
    notify_ev(H2,"H2");
    _t = _dates[_i];
    _i++;
    }
};
