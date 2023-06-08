#include "inp_e.h"
#include "rfsm.h"

typedef struct { int date; bool val; } _vc_t;
static _vc_t _vcs[3] = { {0,false}, {25,true}, {35,false} };

void Inp_e::gen()
{
  int _i=0, _t=0;
  while ( _i < 3 ) {
    wait(_vcs[_i].date-_t, SC_NS);
    e = _vcs[_i].val;
    _t = _vcs[_i].date;
    _i++;
    }
};
