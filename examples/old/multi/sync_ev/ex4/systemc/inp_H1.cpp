#include "inp_H1.h"
#include "rfsm.h"

static int _dates[7] = { 10, 20, 30, 40, 50, 60, 70 };

void Inp_H1::gen()
{
  int _i=0, _t=0;
  while ( _i < 7 ) {
    wait(_dates[_i]-_t, SC_NS);
    notify_ev(H1,"H1");
    _t = _dates[_i];
    _i++;
    }
};
