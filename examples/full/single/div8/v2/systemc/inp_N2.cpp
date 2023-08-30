#include "inp_N2.h"
#include "rfsm.h"

typedef struct { int date; int val; } _vc_t;
static _vc_t _vcs[2] = { {5,25}, {45,0} };

void Inp_N2::gen()
{
  int _i=0, _t=0;
  while ( _i < 2 ) {
    wait(_vcs[_i].date-_t, SC_NS);
    N2 = _vcs[_i].val;
    _t = _vcs[_i].date;
    _i++;
    }
};
