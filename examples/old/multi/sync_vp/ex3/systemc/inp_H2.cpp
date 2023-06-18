#include "inp_H2.h"
#include "rfsm.h"

typedef struct { int period; int t1; int t2; } _periodic_t;

static _periodic_t _clk = { 10, 15, 100 };

void Inp_H2::gen(void)
{
  t=0;
  H2.write(0);
  wait(_clk.t1, SC_NS);
  t += _clk.t1;
  while ( t <= _clk.t2 ) {
    step();
    }
};

void Inp_H2::step(void)
{
  H2.write(1);
  wait(_clk.period/2, SC_NS);
  H2.write(0);
  wait(_clk.period/2, SC_NS);
  t += _clk.period;
};
