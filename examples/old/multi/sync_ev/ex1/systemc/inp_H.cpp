#include "inp_H.h"
#include "rfsm.h"

static int _dates[2] = { 10, 20 };

void Inp_H::gen()
{
  int _i=0, _t=0;
  while ( _i < 2 ) {
    wait(_dates[_i]-_t, SC_NS);
    notify_ev(H,"H");
    _t = _dates[_i];
    _i++;
    }
};
