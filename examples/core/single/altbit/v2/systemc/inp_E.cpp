#include "inp_E.h"
#include "rfsm.h"

typedef struct { int date; bool val; } _vc_t;
static _vc_t _vcs[11] = { {5,false}, {15,true}, {25,false}, {35,true}, {45,true}, {55,false}, {65,true}, {75,false}, {85,false}, {95,false}, {105,true} };

void Inp_E::gen()
{
  int _i=0, _t=0;
  while ( _i < 11 ) {
    wait(_vcs[_i].date-_t, SC_NS);
    E = _vcs[_i].val;
    _t = _vcs[_i].date;
    cout << "Inp_E: t=" << _vcs[_i].date << ": wrote " << _vcs[_i].val << endl;
    _i++;
    }
};
