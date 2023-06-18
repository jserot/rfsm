#include "inp_He.h"
#include "rfsm.h"

typedef struct { int period; int t1; int t2; } _periodic_t;

static _periodic_t _clk = { 10, 10, 300 };

void Inp_He::gen(void)
{
  t=0;
  He.write(0);
  wait(_clk.t1, SC_NS);
  t += _clk.t1;
  while ( t <= _clk.t2 ) {
    step();
    }
};

void Inp_He::step(void)
{
  He.write(1);
  wait(_clk.period/2.0, SC_NS);
  He.write(0);
  wait(_clk.period/2.0, SC_NS);
  t += _clk.period;
};
