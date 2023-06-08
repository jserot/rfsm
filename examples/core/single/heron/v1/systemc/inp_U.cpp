#include "inp_U.h"
#include "rfsm.h"

typedef struct { int date; float val; } _vc_t;
static _vc_t _vcs[1] = { {5,2.} };

void Inp_U::gen()
{
  int _i=0, _t=0;
  while ( _i < 1 ) {
    wait(_vcs[_i].date-_t, SC_NS);
    U = _vcs[_i].val;
    _t = _vcs[_i].date;
    _i++;
    }
};
